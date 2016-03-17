{-# LANGUAGE OverloadedStrings #-}

module Interact
  ( run
  ) where

import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (partition, sort, stripPrefix)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Kitten (runKitten)
import Kitten.Dictionary (Dictionary)
import Kitten.Entry (Entry)
import Kitten.Informer (checkpoint)
import Kitten.Interpret (interpret)
import Kitten.Name (GeneralName(..), Qualified(..))
import Report
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isEOFError)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import Text.Printf (printf)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Kitten
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Enter as Enter
import qualified Kitten.Entry as Entry
import qualified Kitten.Name as Name
import qualified Kitten.Origin as Origin
import qualified Kitten.Parse as Parse
import qualified Kitten.Pretty as Pretty
import qualified Kitten.Report as Report
import qualified Kitten.Resolve as Resolve
import qualified Kitten.Vocabulary as Vocabulary
import qualified Text.PrettyPrint as Pretty

run :: IO ()
run = do
  commonDictionary <- runKitten $ do
    fragment <- Kitten.fragmentFromSource
      [QualifiedName $ Qualified Vocabulary.global "IO"]
        1 "<interactive>" commonSource
    Enter.fragment fragment Dictionary.empty
  dictionaryRef <- newIORef =<< case commonDictionary of
    Left reports -> do
      reportAll reports
      exitFailure
    Right result -> return result
  lineNumberRef <- newIORef (1 :: Int)
  putStrLn "Welcome to Kitten! Type //help for help or //quit to quit."
  let
    loop = do
      lineNumber <- readIORef lineNumberRef
      printf "% 4d: " lineNumber
      hFlush stdout
      mLine <- try Text.getLine
      case mLine of
        Left e -> if isEOFError e then putStrLn "" >> bye else ioError e
        Right line -> case line of
          "//dict" -> do
            renderDictionary dictionaryRef
            loop
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
          "//quit" -> bye
          _ -> do
            dictionary <- readIORef dictionaryRef
            mDictionary' <- runKitten $ do
              fragment <- Kitten.fragmentFromSource
                [QualifiedName $ Qualified Vocabulary.global "IO"]
                lineNumber "<interactive>" line
              dictionary' <- Enter.fragment fragment dictionary
              checkpoint
              return dictionary'
            case mDictionary' of
              Left reports -> do
                reportAll reports
                loop
              Right dictionary' -> do
                putStrLn "Okay."
                writeIORef dictionaryRef dictionary'
                modifyIORef' lineNumberRef (+ 1)
                interpret dictionary'
                loop
  loop
  where
  bye = do
    liftIO $ putStrLn "Bye!"
    return ()

commonSource :: Text
commonSource = "\
\permission IO<R..., S..., +E> (R..., (R... -> S... +IO +E) -> S... +E):\n\
\  with (+IO)\n\
\\&"

renderDictionary :: IORef Dictionary -> IO ()
renderDictionary dictionaryRef = do
  names <- sort . map (Name.toParts . fst) . Dictionary.toList
    <$> readIORef dictionaryRef
  let
    loop :: Int -> [[Text]] -> IO ()
    loop depth acc = case foldr0 commonPrefix [] acc of
      [] -> return ()
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
  case result of
    Right unresolved -> do
      dictionary <- readIORef dictionaryRef
      mResolved <- runKitten $ Resolve.run $ Resolve.generalName
        dictionary
        -- TODO: Use 'WordOrTypeName' or something as the category.
        Report.WordName
        (\ _ index -> return $ LocalName index)
        (`Dictionary.member` dictionary)
        -- TODO: Keep a notion of current vocabulary?
        Vocabulary.global
        unresolved
        -- TODO: Get this from the parser.
        (Origin.point "<interactive>" lineNumber 1)
      case mResolved of
        Left reports -> do
          reportAll reports
          loop
        Right (QualifiedName resolved)
          | Just entry <- Dictionary.lookup resolved dictionary
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
