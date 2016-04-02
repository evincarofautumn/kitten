{-# LANGUAGE OverloadedStrings #-}

module Interact
  ( run
  ) where

import Control.Exception (catch, try)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (partition, sort, stripPrefix)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Kitten (runKitten)
import Kitten.Dictionary (Dictionary)
import Kitten.Entry (Entry)
import Kitten.Entry.Parameter (Parameter(..))
import Kitten.Infer (typeFromSignature)
import Kitten.Informer (checkpoint)
import Kitten.Instantiated (Instantiated(Instantiated))
import Kitten.Interpret (Failure, interpret)
import Kitten.Kind (Kind(..))
import Kitten.Name
import Report
import System.Exit (exitFailure)
import System.IO (hFlush, hPutStrLn, stdin, stdout, stderr)
import System.IO.Error (isEOFError)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import Text.Printf (printf)
import qualified Data.Text as Text
import qualified Kitten
import qualified Kitten.Definition as Definition
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Enter as Enter
import qualified Kitten.Entry as Entry
import qualified Kitten.IO as IO
import qualified Kitten.Instantiated as Instantiated
import qualified Kitten.Name as Name
import qualified Kitten.Origin as Origin
import qualified Kitten.Parse as Parse
import qualified Kitten.Pretty as Pretty
import qualified Kitten.Report as Report
import qualified Kitten.Resolve as Resolve
import qualified Kitten.Signature as Signature
import qualified Kitten.Term as Term
import qualified Kitten.TypeEnv as TypeEnv
import qualified Kitten.Unify as Unify
import qualified Kitten.Vocabulary as Vocabulary
import qualified Text.PrettyPrint as Pretty

run :: IO ()
run = do
  let commonPath = "common.ktn"
  commonSource <- IO.readFileUtf8 commonPath
  commonDictionary <- runKitten $ do
    fragment <- Kitten.fragmentFromSource
      [QualifiedName $ Qualified Vocabulary.global "IO"]
      (Just $ Qualified Vocabulary.global "main")
      1 commonPath commonSource
    Enter.fragment fragment Dictionary.empty
  dictionaryRef <- newIORef =<< case commonDictionary of
    Left reports -> do
      reportAll reports
      exitFailure
    Right result -> return result
  lineNumberRef <- newIORef (1 :: Int)
  stackRef <- newIORef []
  putStrLn "Welcome to Kitten! Type //help for help or //quit to quit"
  let
    loop = do
      lineNumber <- readIORef lineNumberRef
      let currentOrigin = Origin.point "<interactive>" lineNumber 1
      mLine <- try $ getEntry lineNumber
      case mLine of
        Left e -> if isEOFError e then putStrLn "" >> bye else ioError e
        Right (line, lineNumber') -> case line of
          "//dict" -> do
            renderDictionary dictionaryRef
            loop
          "//help" -> do
            showHelp
            loop
          "//stack" -> do
            renderStack stackRef
            loop
          "//quit" -> bye
          _
            | "//" `Text.isPrefixOf` line
            -> case Text.break (== ' ') $ Text.drop 2 line of
              ("info", name) -> nameCommand lineNumber dictionaryRef name loop
                $ \ _name' entry -> do
                  putStrLn $ Pretty.render $ pPrint entry
              ("list", name) -> nameCommand lineNumber dictionaryRef name loop
                $ \ name' entry -> case entry of
                  Entry.Word _ _ _ _ _ (Just body) -> do
                    putStrLn $ Pretty.render $ pPrint body
                  _ -> hPutStrLn stderr $ Pretty.render $ Pretty.hsep
                    [ "I can't find a word entry called"
                    , Pretty.quote name'
                    , "with a body to list"
                    ]
              (command, _) -> do
                hPutStrLn stderr $ Pretty.render $ Pretty.hsep
                  [ "I don't know the command"
                  , Pretty.quotes $ Pretty.text $ Text.unpack command
                  ]
                loop
          _ -> do
            dictionary <- readIORef dictionaryRef
            let
              entryNameUnqualified = Text.pack $ "entry" ++ show lineNumber
              entryName = Qualified
                (Qualifier [Vocabulary.globalName, "interactive"])
                $ Unqualified entryNameUnqualified
            mResults <- runKitten $ do
              -- Each entry gets its own definition in the dictionary, so it can
              -- be executed individually, and later conveniently referred to.
              fragment <- Kitten.fragmentFromSource
                [QualifiedName $ Qualified Vocabulary.global "IO"]
                (Just entryName)
                lineNumber "<interactive>" line
              dictionary' <- Enter.fragment fragment dictionary
              checkpoint
              callFragment <- Kitten.fragmentFromSource
                [QualifiedName $ Qualified Vocabulary.global "IO"]
                Nothing lineNumber "<interactive>"
                -- TODO: Avoid stringly typing.
                (Text.pack $ Pretty.render $ pPrint entryName)
              dictionary'' <- Enter.fragment callFragment dictionary'
              checkpoint
              let tenv = TypeEnv.empty
              (mainScheme, mainBody) <- case Dictionary.lookup
                (Instantiated Definition.mainName []) dictionary'' of
                Just (Entry.Word _ _ _ _ (Just signature) (Just body))
                  -> do
                    type_ <- typeFromSignature tenv signature
                    return (type_, body)
                _ -> error "cannot get entry scheme"
              stackScheme <- typeFromSignature tenv $ Signature.Quantified
                [ Parameter currentOrigin "R" Stack
                , Parameter currentOrigin "E" Permission
                ]
                (Signature.StackFunction
                  (Signature.Bottom currentOrigin) []
                  (Signature.Variable "R" currentOrigin) []
                  ["E"] currentOrigin)
                currentOrigin
              -- Checking that the main definition is able to operate on an
              -- empty stack is the same as verifying the last entry against the
              -- current stack state, as long as the state was modified
              -- correctly by the interpreter.
              _ <- Unify.type_ tenv stackScheme mainScheme
              checkpoint
              return (dictionary'', mainBody)
            case mResults of
              Left reports -> do
                reportAll reports
                loop
              Right (dictionary', mainBody) -> do
                writeIORef dictionaryRef dictionary'
                writeIORef lineNumberRef lineNumber'
                -- HACK: Get the last entry from the main body so we have the
                -- right generic args.
                let lastEntry = last $ Term.decompose mainBody
                case lastEntry of
                  (Term.Word _ _ _ args _) -> catch
                    (do
                      stack <- interpret dictionary'
                        (Just entryName) args
                        stdin stdout stderr
                        =<< readIORef stackRef
                      writeIORef stackRef stack
                      renderStack stackRef)
                    $ \ e -> hPutStrLn stderr
                      $ show (e :: Failure)
                  _ -> error $ show lastEntry
                loop

  loop
  where
  bye = do
    liftIO $ putStrLn "Bye!"
    return ()

renderDictionary :: IORef Dictionary -> IO ()
renderDictionary dictionaryRef = do
  names <- sort . map (Name.toParts . Instantiated.name . fst) . Dictionary.toList
    <$> readIORef dictionaryRef
  let
    loop :: Int -> [[Text]] -> IO ()
    loop depth acc = case foldr0 commonPrefix [] acc of
      [] -> mapM_ (putStrLn . prettyName depth) acc
      prefix -> let
        stripped = map (fromJust . stripPrefix prefix) acc
        (leaves, branches) = partition ((== 1) . length) stripped
        in do
          putStrLn $ prettyName depth prefix
          loop (depth + 4) branches
          mapM_ (putStrLn . prettyName (depth + 4)) leaves
  loop 0 names
  where
  -- TODO: Don't rely on name of global vocabulary.
  prettyName depth = Pretty.render . Pretty.nest depth . Pretty.text
    . Text.unpack . Text.intercalate "::"
    . (\x -> if x == [""] then ["_"] else x)

foldr0 :: (a -> a -> a) -> a -> [a] -> a
foldr0 _ x [] = x
foldr0 f _ xs = foldr1 f xs

commonPrefix :: (Eq a) => [a] -> [a] -> [a]
commonPrefix (x : xs) (y : ys)
  | x == y = x : commonPrefix xs ys
  | otherwise = []
commonPrefix xs@[] _ = xs
commonPrefix _ ys@[] = ys

nameCommand
  :: Int
  -> IORef Dictionary
  -> Text
  -> IO ()
  -> (Qualified -> Entry -> IO ())
  -> IO ()
nameCommand lineNumber dictionaryRef name loop action = do
  result <- runKitten $ Parse.generalName
    lineNumber "<interactive>" name
  let currentOrigin = Origin.point "<interactive>" lineNumber 1
  case result of
    Right unresolved -> do
      dictionary <- readIORef dictionaryRef
      mResolved <- runKitten $ Resolve.run $ Resolve.generalName
        dictionary
        -- TODO: Use 'WordOrTypeName' or something as the category.
        Report.WordName
        (\ _ index -> return $ LocalName index)
        (\ name -> Instantiated name [] `Dictionary.member` dictionary)
        -- TODO: Keep a notion of current vocabulary?
        Vocabulary.global
        unresolved
        -- TODO: Get this from the parser.
        currentOrigin
      case mResolved of
        Left reports -> do
          reportAll reports
          loop
        Right (QualifiedName resolved)
          | Just entry <- Dictionary.lookup
            (Instantiated resolved []) dictionary
          -> do
            action resolved entry
            loop
        Right resolved -> do
          hPutStrLn stderr $ Pretty.render $ Pretty.hsep
            [ "I can't find an entry in the dictionary for"
            , Pretty.quote resolved
            ]
          loop
    Left reports -> do
      reportAll reports
      loop

renderStack :: (Pretty a) => IORef [a] -> IO ()
renderStack stackRef = do
  stack <- readIORef stackRef
  case stack of
    [] -> return ()
    _ -> putStrLn $ Pretty.render $ Pretty.vcat $ map pPrint stack

showHelp :: IO ()
showHelp = putStrLn "\
\\n\
\//help         - Show this help.\n\
\//quit         - Quit interactive mode.\n\
\\n\
\//dict         - Show the contents of the dictionary.\n\
\//info <name>  - Show information about <name>.\n\
\//list <name>  - Show the desugared source of <name>.\n\
\//stack        - Show the state of the stack.\n\
\\&"

data InString = Inside | Outside

getEntry :: Int -> IO (Text, Int)
getEntry lineNumber0 = do
  printf "\n% 4d: " lineNumber0
  hFlush stdout
  line <- getLine
  (result, lineNumber') <- check lineNumber0 line Nothing
  return (Text.pack result, lineNumber' + 1)
  where

  check :: Int -> String -> Maybe String -> IO (String, Int)
  check lineNumber line acc
    | matched acc' = return (acc', lineNumber)
    | otherwise = continue (succ lineNumber) $ Just acc'
    where
    acc' = case acc of
      Just previous -> concat [previous, "\n", line]
      Nothing -> line

  continue :: Int -> Maybe String -> IO (String, Int)
  continue lineNumber acc = do
    printf "\n% 4d| " lineNumber
    hFlush stdout
    line <- getLine
    check lineNumber line acc

  matched :: String -> Bool
  matched = go Outside (0::Int)
    where

    go :: InString -> Int -> String -> Bool
    go q n ('\\':x:xs)
      | x `elem` ("'\"" :: String) = go q n xs
      | otherwise = go q n xs
    go q n ('"':xs) = go (case q of Inside -> Outside; Outside -> Inside) n xs
    go Inside n (_:xs) = go Inside n xs
    go Inside _ [] = True
    go Outside n (x:xs)
      | isOpen x = go Outside (succ n) xs
      | isClose x = n <= 0 || go Outside (pred n) xs
      | otherwise = go Outside n xs
    go Outside n [] = n == 0
    isOpen = (`elem` ("([{" :: String))
    isClose = (`elem` ("}])" :: String))
