{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Kitten.Intrinsic where

import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.Util.Text (ToText(..))
import Kitten.Util.Tuple

data Intrinsic
  = InAddFloat
  | InAddInt
  | InAddVector
  | InAndBool
  | InAndInt
  | InApply
  | InCharToInt
  | InChoice
  | InChoiceElse
  | InClose
  | InDivFloat
  | InDivInt
  | InEqFloat
  | InEqInt
  | InExit
  | InFirst
  | InFromLeft
  | InFromRight
  | InFromSome
  | InGeFloat
  | InGeInt
  | InGet
  | InGetLine
  | InGtFloat
  | InGtInt
  | InIf
  | InIfElse
  | InInit
  | InIntToChar
  | InLeFloat
  | InLeInt
  | InLeft
  | InLength
  | InLtFloat
  | InLtInt
  | InModFloat
  | InModInt
  | InMulFloat
  | InMulInt
  | InNeFloat
  | InNeInt
  | InNegFloat
  | InNegInt
  | InNone
  | InNotBool
  | InNotInt
  | InOpenIn
  | InOpenOut
  | InOption
  | InOptionElse
  | InOrBool
  | InOrInt
  | InPair
  | InPrint
  | InRest
  | InRight
  | InSet
  | InShowFloat
  | InShowInt
  | InSome
  | InStderr
  | InStdin
  | InStdout
  | InSubFloat
  | InSubInt
  | InTail
  | InXorBool
  | InXorInt
  deriving (Eq, Generic, Ord)

instance Hashable Intrinsic

instance ToText Intrinsic where
  toText = fromJust . (`H.lookup` intrinsicToTextMap)

instance Show Intrinsic where
  show = T.unpack . toText

intrinsicFromText :: Text -> Maybe Intrinsic
intrinsicFromText = (`H.lookup` intrinsicFromTextMap)

intrinsicToTextMap :: HashMap Intrinsic Text
intrinsicToTextMap = H.fromList (V.toList intrinsicToTextTable)

intrinsicFromTextMap :: HashMap Text Intrinsic
intrinsicFromTextMap = H.fromList (V.toList intrinsicFromTextTable)

intrinsicNames :: Vector Text
intrinsicNames = V.map fst intrinsicFromTextTable

intrinsicFromTextTable :: Vector (Text, Intrinsic)
intrinsicFromTextTable = V.fromList
  [ (,) "__add_float"       InAddFloat
  , (,) "__add_int"         InAddInt
  , (,) "__add_vector"      InAddVector
  , (,) "__and_bool"        InAndBool
  , (,) "__and_int"         InAndInt
  , (,) "__apply"           InApply
  , (,) "__char_to_int"     InCharToInt
  , (,) "__choice"          InChoice
  , (,) "__choice_else"     InChoiceElse
  , (,) "__close"           InClose
  , (,) "__div_float"       InDivFloat
  , (,) "__div_int"         InDivInt
  , (,) "__eq_float"        InEqFloat
  , (,) "__eq_int"          InEqInt
  , (,) "__exit"            InExit
  , (,) "__first"           InFirst
  , (,) "__from_left"       InFromLeft
  , (,) "__from_right"      InFromRight
  , (,) "__from_some"       InFromSome
  , (,) "__ge_float"        InGeFloat
  , (,) "__ge_int"          InGeInt
  , (,) "__get"             InGet
  , (,) "__get_line"        InGetLine
  , (,) "__gt_float"        InGtFloat
  , (,) "__gt_int"          InGtInt
  , (,) "__if"              InIf
  , (,) "__if_else"         InIfElse
  , (,) "__init"            InInit
  , (,) "__int_to_char"     InIntToChar
  , (,) "__le_float"        InLeFloat
  , (,) "__le_int"          InLeInt
  , (,) "__left"            InLeft
  , (,) "__length"          InLength
  , (,) "__lt_float"        InLtFloat
  , (,) "__lt_int"          InLtInt
  , (,) "__mod_float"       InModFloat
  , (,) "__mod_int"         InModInt
  , (,) "__mul_float"       InMulFloat
  , (,) "__mul_int"         InMulInt
  , (,) "__ne_float"        InNeFloat
  , (,) "__ne_int"          InNeInt
  , (,) "__neg_float"       InNegFloat
  , (,) "__neg_int"         InNegInt
  , (,) "__none"            InNone
  , (,) "__not_bool"        InNotBool
  , (,) "__not_int"         InNotInt
  , (,) "__open_in"         InOpenIn
  , (,) "__open_out"        InOpenOut
  , (,) "__option"          InOption
  , (,) "__option_else"     InOptionElse
  , (,) "__or_bool"         InOrBool
  , (,) "__or_int"          InOrInt
  , (,) "__pair"            InPair
  , (,) "__print"           InPrint
  , (,) "__rest"            InRest
  , (,) "__right"           InRight
  , (,) "__set"             InSet
  , (,) "__show_float"      InShowFloat
  , (,) "__show_int"        InShowInt
  , (,) "__some"            InSome
  , (,) "__stderr"          InStderr
  , (,) "__stdin"           InStdin
  , (,) "__stdout"          InStdout
  , (,) "__sub_float"       InSubFloat
  , (,) "__sub_int"         InSubInt
  , (,) "__tail"            InTail
  , (,) "__xor_bool"        InXorBool
  , (,) "__xor_int"         InXorInt
  ]

intrinsicToTextTable :: Vector (Intrinsic, Text)
intrinsicToTextTable = V.map swap intrinsicFromTextTable
