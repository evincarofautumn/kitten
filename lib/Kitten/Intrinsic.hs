{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Kitten.Intrinsic where

import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.Vector (Vector)
import GHC.Generics (Generic)

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.Name
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
  | InFloatToInt
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
  | InIntToFloat
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
  | InPowFloat
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
  toText = toText . fromJust . (`H.lookup` intrinsicToNameMap)

instance Show Intrinsic where
  show = T.unpack . toText

intrinsicFromName :: Name -> Maybe Intrinsic
intrinsicFromName = (`H.lookup` intrinsicFromNameMap)

intrinsicToNameMap :: HashMap Intrinsic Name
intrinsicToNameMap = H.fromList (V.toList intrinsicToNameTable)

intrinsicFromNameMap :: HashMap Name Intrinsic
intrinsicFromNameMap = H.fromList (V.toList intrinsicFromNameTable)

intrinsicNames :: Vector Name
intrinsicNames = V.map fst intrinsicFromNameTable

intrinsicFromNameTable :: Vector (Name, Intrinsic)
intrinsicFromNameTable = V.fromList
  [ entry "addFloat"    InAddFloat
  , entry "addInt"      InAddInt
  , entry "addVector"   InAddVector
  , entry "andBool"     InAndBool
  , entry "andInt"      InAndInt
  , entry "apply"       InApply
  , entry "charToInt"   InCharToInt
  , entry "choice"      InChoice
  , entry "choiceElse"  InChoiceElse
  , entry "close"       InClose
  , entry "divFloat"    InDivFloat
  , entry "divInt"      InDivInt
  , entry "eqFloat"     InEqFloat
  , entry "eqInt"       InEqInt
  , entry "exit"        InExit
  , entry "first"       InFirst
  , entry "floatToInt"  InFloatToInt
  , entry "fromLeft"    InFromLeft
  , entry "fromRight"   InFromRight
  , entry "fromSome"    InFromSome
  , entry "geFloat"     InGeFloat
  , entry "geInt"       InGeInt
  , entry "get"         InGet
  , entry "getLine"     InGetLine
  , entry "gtFloat"     InGtFloat
  , entry "gtInt"       InGtInt
  , entry "if"          InIf
  , entry "ifElse"      InIfElse
  , entry "init"        InInit
  , entry "intToChar"   InIntToChar
  , entry "intToFloat"  InIntToFloat
  , entry "leFloat"     InLeFloat
  , entry "leInt"       InLeInt
  , entry "left"        InLeft
  , entry "length"      InLength
  , entry "ltFloat"     InLtFloat
  , entry "ltInt"       InLtInt
  , entry "modFloat"    InModFloat
  , entry "modInt"      InModInt
  , entry "mulFloat"    InMulFloat
  , entry "mulInt"      InMulInt
  , entry "neFloat"     InNeFloat
  , entry "neInt"       InNeInt
  , entry "negFloat"    InNegFloat
  , entry "negInt"      InNegInt
  , entry "none"        InNone
  , entry "notBool"     InNotBool
  , entry "notInt"      InNotInt
  , entry "openIn"      InOpenIn
  , entry "openOut"     InOpenOut
  , entry "option"      InOption
  , entry "optionElse"  InOptionElse
  , entry "orBool"      InOrBool
  , entry "orInt"       InOrInt
  , entry "pair"        InPair
  , entry "powFloat"    InPowFloat
  , entry "print"       InPrint
  , entry "rest"        InRest
  , entry "right"       InRight
  , entry "set"         InSet
  , entry "showFloat"   InShowFloat
  , entry "showInt"     InShowInt
  , entry "some"        InSome
  , entry "stderr"      InStderr
  , entry "stdin"       InStdin
  , entry "stdout"      InStdout
  , entry "subFloat"    InSubFloat
  , entry "subInt"      InSubInt
  , entry "tail"        InTail
  , entry "xorBool"     InXorBool
  , entry "xorInt"      InXorInt
  ]
  where
  entry name intrinsic =
    ( Qualified (Qualifier (V.fromList ["kitten", "intrinsic"])) name
    , intrinsic
    )

intrinsicToNameTable :: Vector (Intrinsic, Name)
intrinsicToNameTable = V.map swap intrinsicFromNameTable
