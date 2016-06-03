{-# LANGUAGE OverloadedStrings #-}

module Kitten.Signature
  ( Signature(..)
  , origin
  ) where

import Kitten.Entry.Parameter (Parameter(Parameter))
import Kitten.Kind (Kind(..))
import Kitten.Name (GeneralName)
import Kitten.Origin (Origin)
import Kitten.Type (Type)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Kitten.Pretty as Pretty
import qualified Kitten.Type as Type
import qualified Text.PrettyPrint as Pretty

data Signature
  = Application Signature Signature !Origin
  | Bottom !Origin
  | Function [Signature] [Signature] [GeneralName] !Origin
  | Quantified [Parameter] !Signature !Origin
  | Variable !GeneralName !Origin
  | StackFunction
    Signature [Signature]
    Signature [Signature]
    [GeneralName]
    !Origin
  -- Produced when generating signatures for lifted quotations after
  -- typechecking.
  | Type !Type
  deriving (Show)

-- Signatures are compared regardless of origin.
instance Eq Signature where
  Application a b _ == Application c d _ = (a, b) == (c, d)
  Function a b c _ == Function d e f _ = (a, b, c) == (d, e, f)
  Quantified a b _ == Quantified c d _ = (a, b) == (c, d)
  Variable a _ == Variable b _ = a == b
  StackFunction a b c d e _ == StackFunction f g h i j _
    = (a, b, c, d, e) == (f, g, h, i, j)
  _ == _ = False

origin :: Signature -> Origin
origin signature = case signature of
  Application _ _ o -> o
  Bottom o -> o
  Function _ _ _ o -> o
  Quantified _ _ o -> o
  Variable _ o -> o
  StackFunction _ _ _ _ _ o -> o
  Type t -> Type.origin t

instance Pretty Signature where
  pPrint signature = case signature of
    Application a b _ -> Pretty.hcat
      [pPrint a, Pretty.angles $ pPrint b]
    Bottom _ -> "<bottom>"
    Function as bs es _ -> Pretty.parens $ Pretty.hsep $
      [ Pretty.list $ map pPrint as
      , "->"
      , Pretty.list $ map pPrint bs
      ] ++ map ((Pretty.char '+' Pretty.<>) . pPrint) es
    Quantified names type_ _ -> Pretty.hsep
      [ Pretty.angles $ Pretty.list $ map prettyVar names
      , pPrint type_
      ]
      where

      prettyVar :: Parameter -> Pretty.Doc
      prettyVar (Parameter _ name kind) = case kind of
        Value -> pPrint name
        Stack -> pPrint name Pretty.<> "..."
        Permission -> Pretty.char '+' Pretty.<> pPrint name
        _ -> error "quantified signature contains variable of invalid kind"

    Variable name _ -> pPrint name
    StackFunction r as s bs es _ -> Pretty.parens $ Pretty.hsep
      $ (pPrint r Pretty.<> "...")
      : map pPrint as ++ ["->"]
      ++ ((pPrint s Pretty.<> "...") : map pPrint bs)
      ++ map ((Pretty.char '+' Pretty.<>) . pPrint) es
    Type t -> pPrint t
