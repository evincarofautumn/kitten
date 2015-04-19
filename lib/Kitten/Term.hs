{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Kitten.Term where

import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.ClosedName
import Kitten.Definition
import Kitten.Intrinsic
import Kitten.Kind
import Kitten.Location
import Kitten.Name
import Kitten.Operator
import Kitten.Type
import Kitten.Util.Text (ToText(..), showText)

data TrTerm a
  = TrIntrinsic !Intrinsic !a
  | TrCall !Fixity !Name !a
  | TrCompose !StackHint !(Vector (TrTerm a)) !a
  | TrConstruct !Name !Name !Int !a
  | TrLambda !Name !Location !(TrTerm a) !a
  | TrMakePair !(TrTerm a) !(TrTerm a) !a
  | TrMakeVector !(Vector (TrTerm a)) !a
  | TrMatch !(Vector (TrCase a)) !(Maybe (TrValue a)) !a
  | TrPush !(TrValue a) !a

data TrCase a = TrCase !Name !(TrValue a) !a

instance (Eq a) => Eq (TrTerm a) where
  -- Calls are equal regardless of 'Fixity'.
  TrCall _ a b == TrCall _ c d = (a, b) == (c, d)
  -- Minor hack to make 'Compose's less in the way.

  TrCompose _ a _ == b | V.length a == 1 = V.head a == b
  a == TrCompose _ b _ | V.length b == 1 = a == V.head b
  -- 'Compose's are equal regardless of 'StackHint'.
  TrCompose _ a b == TrCompose _ c d = (a, b) == (c, d)
  TrIntrinsic a b == TrIntrinsic c d = (a, b) == (c, d)
  TrLambda a _ b c == TrLambda d _ e f = (a, b, c) == (d, e, f)
  TrMakePair a b c == TrMakePair d e f = (a, b, c) == (d, e, f)
  TrPush a b == TrPush c d = (a, b) == (c, d)
  TrMakeVector a b == TrMakeVector c d = (a, b) == (c, d)
  _ == _ = False

instance ToText (TrTerm a) where
  toText = \case
    TrCall _ label _ -> toText label
    TrCompose _ terms _ -> T.concat
      ["(", T.intercalate " " (V.toList (V.map toText terms)), ")"]
    TrConstruct name ctor size _ -> T.concat
      ["new ", toText name, ".", toText ctor, "(", showText size, ")"]
    TrIntrinsic intrinsic _ -> toText intrinsic
    TrLambda name _ term _ -> T.concat ["(\\", toText name, " ", toText term, ")"]
    TrMakePair a b _ -> T.concat ["(", toText a, ", ", toText b, ")"]
    TrMakeVector terms _ -> T.concat
      ["[", T.intercalate ", " (V.toList (V.map toText terms)), "]"]
    TrMatch cases mDefault _ -> T.unwords $ concat
      [ ["match", "{"]
      , V.toList $ V.map toText cases
      , case mDefault of
        Just body -> ["default", "{", toText body, "}"]
        Nothing -> []
      , ["}"]
      ]
    TrPush value _ -> toText value

instance Show (TrTerm a) where
  show = T.unpack . toText

instance ToText (TrCase a) where
  toText (TrCase name body _) = T.unwords ["case", toText name, "{", toText body, "}"]

data TrValue a
  = TrBool !Bool !a
  | TrChar !Char !a
  | TrClosed !Int !a  -- resolved
  | TrClosure !(Vector ClosedName) !(TrTerm a) !a  -- resolved
  | TrFloat !Double !a
  | TrInt !Int !a
  | TrLocal !Int !a  -- resolved
  | TrQuotation !(TrTerm a) !a
  | TrText !Text !a
  deriving (Eq)

instance ToText (TrValue a) where
  toText = \case
    TrBool value _ -> if value then "true" else "false"
    TrChar value _ -> showText value
    TrClosed index _ -> "closure" <> showText index
    TrClosure{} -> "<closure>"
    TrFloat value _ -> showText value
    TrInt value _ -> showText value
    TrLocal index _ -> "local" <> showText index
    TrQuotation term _ -> T.concat ["{", toText term, "}"]
    TrText value _ -> toText value

instance Show (TrValue a) where
  show = T.unpack . toText

type ParsedTerm = TrTerm Location
type ParsedValue = TrValue Location

type ResolvedTerm = TrTerm Location
type ResolvedValue = TrValue Location

type TypedTerm = TrTerm (Location, Type 'Scalar)
type TypedValue = TrValue (Location, Type 'Scalar)

defTypeScheme :: Def TypedTerm -> TypeScheme
defTypeScheme def = type_ <$ defTerm def
  where
  type_ = typedType $ unscheme (defTerm def)

termMetadata :: TrTerm a -> a
termMetadata = \case
  TrCall _ _ x -> x
  TrCompose _ _ x -> x
  TrConstruct _ _ _ x -> x
  TrIntrinsic _ x -> x
  TrLambda _ _ _ x -> x
  TrMakePair _ _ x -> x
  TrMakeVector _ x -> x
  TrMatch _ _ x -> x
  TrPush _ x -> x

typedLocation :: TypedTerm -> Location
typedLocation = fst . termMetadata

typedType :: TypedTerm -> Type 'Scalar
typedType = snd . termMetadata

parsedLocation :: ParsedTerm -> Location
parsedLocation = termMetadata
