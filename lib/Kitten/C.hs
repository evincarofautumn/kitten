{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.C
  ( toC
  ) where

import Control.Applicative
import Control.Monad.Trans.State.Strict
import Data.IntMap (IntMap)
import Data.List
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.IntMap as I
import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.Id
import Kitten.IdMap (LabelIdMap)
import Kitten.IR
import Kitten.Types
import Kitten.Util.Maybe
import Kitten.Util.Text (showText)
import Kitten.Util.Tuple

import qualified Kitten.IdMap as Id
import qualified Kitten.Util.Vector as V

data Env = Env
  { envIdGen :: !(IdGen LabelSpace)
  , envNames :: !(IntMap DefId)
  , envOffsets :: !(LabelIdMap Int)
  }

newLabel :: Int -> State Env LabelId
newLabel offset = do
  idGen <- gets envIdGen
  let (label, idGen') = genId idGen
  modify $ \e -> e
    { envIdGen = idGen'
    , envOffsets = Id.insert label offset (envOffsets e)
    }
  return label

advance :: Int -> State Env Text
advance index = do
  offsets <- Id.toList <$> gets envOffsets
  let (labels, remaining) = foldl' go ([], []) offsets
  modify $ \e -> e { envOffsets = Id.fromList remaining }
  mGlobal <- gets (I.lookup index . envNames)
  return . T.unlines
    $ ((<> ":") . global <$> mGlobal)
    `consMaybe` map ((<> ":") . local) labels
  where
  go (names, rest) (name, 0) = (name : names, rest)
  go (names, rest) (name, offset) = (names, (name, pred offset) : rest)

toC :: FlattenedProgram -> Vector Text
toC FlattenedProgram{..} = V.concat
  [ V.singleton begin
  , evalState (V.imapM go flattenedBlock) Env
    { envIdGen = mkIdGen
    , envNames = I.fromList . map swap $ Id.toList flattenedNames
    , envOffsets = Id.empty
    }
  , V.singleton end
  ]

  where
  begin = "\
    \#include \"kitten.h\"\n\
    \int main(int argc, char** argv) {\n\
      \k_runtime_init();\n\
      \K_IN_CALL(" <> global entryId <> ", exit);"
  end = "\
    \exit:\n\
      \k_runtime_quit();\n\
      \return 0;\n\
    \}"

  go :: Int -> IrInstruction -> State Env Text
  go ip instruction = (<>) <$> advance ip <*> case instruction of
    IrAct label names _ -> return $ "K_IN_ACT(" <> global label <> ", "
      <> T.intercalate ", " (showText (V.length names)
        : map closedName (V.toList names)) <> ");"
      where
      closedName (ClosedName index) = "K_CLOSED, " <> showText index
      closedName (ReclosedName index) = "K_RECLOSED, " <> showText index
    IrCall label -> do
      next <- newLabel 0
      return $ "K_IN_CALL(" <> global label <> ", " <> local next <> ");"
    IrClosure index -> return
      $ "k_data_push(k_object_retain(k_closure_get(" <> showText index <> ")));"
    IrComment text -> return $ "/* " <> text <> "*/"
    IrConstruct index size -> return $ T.concat
      ["k_in_construct(", showText index, ", ", showText size, ");"]
    IrEnter -> return "k_locals_push(k_data_pop());"
    IrIntrinsic intrinsic -> toCIntrinsic intrinsic
    IrLeave locals -> return $ "k_locals_drop(" <> showText locals <> ");"
    IrLocal index -> return
      $ "k_data_push(k_object_retain(k_locals_get(" <> showText index <> ")));"
    IrMakeVector size -> return $ "k_in_make_vector(" <> showText size <> ");"
    IrMatch cases mDefault -> do
      next <- newLabel 0
      return $ T.concat
        [ "K_IN_CALL(*k_in_match("
        , T.intercalate ", "
          $ showText (V.length cases)
          : maybe "NULL" (("&&" <>) . global) mDefault
          : concatMap
            (\ (IrCase index label)
              -> ["(k_cell_t)" <> showText index, "&&" <> global label])
            (V.toList cases)
        , "), "
        , local next
        , ");"
        ]
    IrPush x -> return $ "k_data_push(" <> toCValue x <> ");"
    IrReturn locals -> return $ "K_IN_RETURN(" <> showText locals <> ");"
    IrTailApply locals -> return
      $ "K_IN_TAIL_APPLY(" <> showText locals <> ");"
    IrTailCall locals label -> return $ T.concat
      [ "K_IN_TAIL_CALL("
      , showText locals
      , ", "
      , global label
      , ");"
      ]

toCValue :: IrValue -> Text
toCValue value = case value of
  IrBool x -> "k_bool_new(" <> showText (fromEnum x :: Int) <> ")"
  IrChar x -> "k_char_new(" <> showText (fromEnum x :: Int) <> ")"
  IrChoice False x -> "k_left_new(" <> toCValue x <> ")"
  IrChoice True x -> "k_right_new(" <> toCValue x <> ")"
  IrFloat x -> "k_float_new(" <> showText x <> ")"
  IrInt x -> "k_int_new(" <> showText x <> ")"
  IrOption Nothing -> "k_none_new()"
  IrOption (Just x) -> "k_some_new(" <> toCValue x <> ")"
  IrPair x y -> "k_pair_new(" <> toCValue x <> ", " <> toCValue y <> ")"
  IrString x -> "k_vector("
    <> T.intercalate ", " (showText (T.length x) : map char (T.unpack x))
    <> ")"
    where char c = "k_char_new(" <> showText c <> ")"

global :: DefId -> Text
global (Id label) = "global" <> showText label

local :: LabelId -> Text
local (Id label) = "local" <> showText label

toCIntrinsic :: Intrinsic -> State Env Text
toCIntrinsic intrinsic = case intrinsic of
  InAddFloat -> binary "float" "+"
  InAddInt -> binary "int" "+"
  InAddVector -> return "k_in_add_vector();"
  InAndBool -> relational "int" "&&"
  InAndInt -> binary "int" "&"
  InApply -> do
    next <- newLabel 0
    return $ "K_IN_APPLY(" <> local next <> ");"
  InCharToInt -> return "k_in_char_to_int();"
  InChoice -> return "K_IN_CHOICE();"
  InChoiceElse -> return "K_IN_CHOICE_ELSE();"
  InClose -> return "k_in_close();"
  InDivFloat -> binary "float" "/"
  InDivInt -> binary "int" "/"
  InEqFloat -> relational "float" "=="
  InEqInt -> relational "int" "=="
  InExit -> return "exit(k_data[0].data.as_int);"
  InFirst -> return "k_in_first();"
  InFromLeft -> return "k_in_from_box(K_LEFT);"
  InFromRight -> return "k_in_from_box(K_RIGHT);"
  InFromSome -> return "k_in_from_box(K_SOME);"
  InGeFloat -> relational "float" ">="
  InGeInt -> relational "int" ">="
  InGet -> return "k_in_get();"
  InGetLine -> return "k_in_get_line();"
  InGtFloat -> relational "float" ">"
  InGtInt -> relational "int" ">"
  InIf -> return "K_IN_IF();"
  InIfElse -> return "K_IN_IF_ELSE();"
  InInit -> return "k_in_init();"
  InIntToChar -> return "k_in_int_to_char();"
  InLeFloat -> relational "float" "<="
  InLeInt -> relational "int" "<="
  InLeft -> return "k_in_left();"
  InLength -> return "k_in_length();"
  InLtFloat -> relational "float" "<"
  InLtInt -> relational "int" "<"
  InModFloat -> return "k_in_mod_float();"
  InModInt -> binary "int" "%"
  InMulFloat -> binary "float" "*"
  InMulInt -> binary "int" "*"
  InNeFloat -> relational "float" "!="
  InNeInt -> relational "int" "!="
  InNegFloat -> unary "float" "-"
  InNegInt -> unary "int" "-"
  InNone -> return "k_data_push(k_none_new());"
  InNotBool -> unary "int" "!"
  InNotInt -> unary "int" "~"
  InOption -> return "K_IN_OPTION();"
  InOptionElse -> return "K_IN_OPTION_ELSE();"
  InOrBool -> relational "int" "||"
  InOrInt -> binary "int" "|"
  InPair -> return "k_in_pair();"
  InPrint -> return "k_in_print();"
  InRest -> return "k_in_rest();"
  InRight -> return "k_in_right();"
  InSet -> return "k_in_set();"
  InShowFloat -> return "k_in_show_float();"
  InShowInt -> return "k_in_show_int();"
  InSome -> return "k_in_some();"
  InStderr -> return "k_data_push(k_handle_new(stderr));"
  InStdin -> return "k_data_push(k_handle_new(stdin));"
  InStdout -> return "k_data_push(k_handle_new(stdout));"
  InSubFloat -> binary "float" "-"
  InSubInt -> binary "int" "-"
  InTail -> return "k_in_tail();"
  InXorBool -> relational "int" "!="
  InXorInt -> binary "int" "^"

  InOpenIn -> return "assert(!\"TODO stdio __open_in\");"
  InOpenOut -> return "assert(!\"TODO stdio __open_out\");"

  where

  -- a a -> a
  binary :: (Monad m) => Text -> Text -> m Text
  binary type_ operation = return
    $ "K_IN_BINARY(" <> type_ <> ", " <> operation <> ");"

  -- a a -> Bool
  relational :: (Monad m) => Text -> Text -> m Text
  relational type_ operation = return
    $ "K_IN_RELATIONAL(" <> type_ <> ", " <> operation <> ");"

  -- a -> a
  unary :: (Monad m) => Text -> Text -> m Text
  unary type_ operation = return
    $ "K_IN_UNARY(" <> type_ <> ", " <> operation <> ");"
