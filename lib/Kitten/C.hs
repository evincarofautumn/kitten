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
      \k_push_return(((KR){ .address = &&exit, .closure = 0 }));\n\
      \goto " <> global entryId <> ";"

  end = "\
    \exit:\n\
      \k_runtime_quit();\n\
      \return 0;\n\
    \}"

  go :: Int -> IrInstruction -> State Env Text
  go ip instruction = (<>) <$> advance ip <*> case instruction of
    IrAct label names _ -> return $ "K_ACT(" <> global label <> ", "
      <> T.intercalate ", " (showText (V.length names)
        : map closedName (V.toList names)) <> ");"
      where
      closedName (ClosedName index) = "K_CLOSED, " <> showText index
      closedName (ReclosedName index) = "K_RECLOSED, " <> showText index
    IrCall label -> do
      next <- newLabel 0
      return $ "K_CALL(" <> global label <> ", " <> local next <> ");"
    IrClosure index -> return
      $ "k_push_data(k_retain(k_get_closure(" <> showText index <> ")));"
    IrComment text -> return $ "/* " <> text <> "*/"
    IrEnter -> return "k_push_locals(k_pop_data());"
    IrIntrinsic intrinsic -> toCIntrinsic intrinsic
    IrLeave -> return "k_drop_locals();"
    IrLocal index -> return
      $ "k_push_data(k_retain(k_get_local(" <> showText index <> ")));"
    IrMakeVector size -> return $ "k_make_vector(" <> showText size <> ");"
    IrPush x -> return $ "k_push_data(" <> toCValue x <> ");"
    IrReturn -> return "K_RETURN();"
    IrTailCall label -> return $ "K_TAIL_CALL(" <> global label <> ");"

toCValue :: IrValue -> Text
toCValue value = case value of
  IrBool x -> "k_new_bool(" <> showText (fromEnum x :: Int) <> ")"
  IrChar x -> "k_new_char(" <> showText (fromEnum x :: Int) <> ")"
  IrChoice False x -> "k_new_left(" <> toCValue x <> ")"
  IrChoice True x -> "k_new_right(" <> toCValue x <> ")"
  IrFloat x -> "k_new_float(" <> showText x <> ")"
  IrInt x -> "k_new_int(" <> showText x <> ")"
  IrOption Nothing -> "k_new_none()"
  IrOption (Just x) -> "k_new_some(" <> toCValue x <> ")"
  IrPair x y -> "k_new_pair(" <> toCValue x <> ", " <> toCValue y <> ")"
  IrString x -> "k_vector("
    <> T.intercalate ", " (showText (T.length x) : map char (T.unpack x))
    <> ")"
    where char c = "k_new_char(" <> showText c <> ")"

global :: DefId -> Text
global (Id label) = "global" <> showText label

local :: LabelId -> Text
local (Id label) = "local" <> showText label

toCIntrinsic :: Intrinsic -> State Env Text
toCIntrinsic intrinsic = case intrinsic of
  InAddFloat -> binary "float" "+"
  InAddInt -> binary "int" "+"
  InAddVector -> return "k_add_vector();"
  InAndBool -> relational "int" "&&"
  InAndInt -> binary "int" "&"
  InApply -> do
    next <- newLabel 0
    return $ "K_APPLY(" <> local next <> ");"
  InCharToInt -> return "/* __char_to_int */"
  InChoice -> return "K_CHOICE();"
  InChoiceElse -> return "K_CHOICE_ELSE();"
  InClose -> return "k_close();"
  InDivFloat -> binary "float" "/"
  InDivInt -> binary "int" "/"
  InEqFloat -> relational "float" "=="
  InEqInt -> relational "int" "=="
  InExit -> return "exit(k_data[0].data);"
  InFirst -> return "k_first();"
  InFromLeft -> return "k_from_box(K_LEFT);"
  InFromRight -> return "k_from_box(K_RIGHT);"
  InFromSome -> return "k_from_box(K_SOME);"
  InGeFloat -> relational "float" ">="
  InGeInt -> relational "int" ">="
  InGet -> return "k_get();"
  InGetLine -> return "k_get_line();"
  InGtFloat -> relational "float" ">"
  InGtInt -> relational "int" ">"
  InIf -> return "K_IF();"
  InIfElse -> return "K_IF_ELSE();"
  InInit -> return "k_init();"
  InIntToChar -> return "/* __int_to_char */"
  InLeFloat -> relational "float" "<="
  InLeInt -> relational "int" "<="
  InLeft -> return "k_left();"
  InLength -> return "k_length();"
  InLtFloat -> relational "float" "<"
  InLtInt -> relational "int" "<"
  InModFloat -> return "k_mod_float();"
  InModInt -> binary "int" "%"
  InMulFloat -> binary "float" "*"
  InMulInt -> binary "int" "*"
  InNeFloat -> relational "float" "!="
  InNeInt -> relational "int" "!="
  InNegFloat -> unary "float" "-"
  InNegInt -> unary "int" "-"
  InNone -> return "k_push_data(k_new_none());"
  InNotBool -> unary "int" "!"
  InNotInt -> unary "int" "~"
  InOption -> return "K_OPTION();"
  InOptionElse -> return "K_OPTION_ELSE();"
  InOrBool -> relational "int" "||"
  InOrInt -> binary "int" "|"
  InPair -> return "k_pair();"
  InPrint -> return "k_print();"
  InRest -> return "k_rest();"
  InRight -> return "k_right();"
  InSet -> return "k_set();"
  InShowFloat -> return "k_show_float();"
  InShowInt -> return "k_show_int();"
  InSome -> return "k_some();"
  InStderr -> return "k_push_data(k_new_handle(stderr));"
  InStdin -> return "k_push_data(k_new_handle(stdin));"
  InStdout -> return "k_push_data(k_new_handle(stdout));"
  InSubFloat -> binary "float" "-"
  InSubInt -> binary "int" "-"
  InTail -> return "k_tail();"
  InXorBool -> relational "int" "!="
  InXorInt -> binary "int" "^"

  InOpenIn -> return "assert(!\"TODO stdio __open_in\");"
  InOpenOut -> return "assert(!\"TODO stdio __open_out\");"

  where

  -- a a -> a
  binary :: (Monad m) => Text -> Text -> m Text
  binary type_ operation = return $ "K_BINARY(" <> type_ <> ", " <> operation <> ");"

  -- a a -> Bool
  relational :: (Monad m) => Text -> Text -> m Text
  relational type_ operation = return
    $ "K_RELATIONAL(" <> type_ <> ", " <> operation <> ");"

  -- a -> a
  unary :: (Monad m) => Text -> Text -> m Text
  unary type_ operation = return $ "K_UNARY(" <> type_ <> ", " <> operation <> ");"
