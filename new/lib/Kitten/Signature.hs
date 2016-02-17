{-# LANGUAGE OverloadedStrings #-}

module Kitten.Signature
  ( Signature(..)
  , origin
  ) where

import Kitten.Kind (Kind(..))
import Kitten.Name (GeneralName, Unqualified)
import Kitten.Origin (Origin)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Kitten.Pretty as Pretty
import qualified Text.PrettyPrint as Pretty

data Signature
  = Application Signature Signature !Origin
  | Function [Signature] [Signature] [GeneralName] !Origin
  | Quantified [(Unqualified, Kind, Origin)] !Signature !Origin
  | Variable !GeneralName !Origin
  | StackFunction
    !Unqualified [Signature] !Unqualified [Signature] [GeneralName] !Origin
  deriving (Show)

origin :: Signature -> Origin
origin signature = case signature of
  Application _ _ o -> o
  Function _ _ _ o -> o
  Quantified _ _ o -> o
  Variable _ o -> o
  StackFunction _ _ _ _ _ o -> o

instance Pretty Signature where
  pPrint signature = case signature of
    Application a b _ -> Pretty.hcat
      [pPrint a, Pretty.angles $ pPrint b]
    Function as bs es _ -> Pretty.parens $ Pretty.hsep $
      [ Pretty.list $ map pPrint as
      , "->"
      , Pretty.list $ map pPrint bs
      ] ++ map ((Pretty.char '+' Pretty.<>) . pPrint) es
    Quantified names type_ _
      -> (Pretty.angles $ Pretty.list $ map prettyVar names)
        Pretty.<+> pPrint type_
      where

      prettyVar :: (Unqualified, Kind, Origin) -> Pretty.Doc
      prettyVar (name, kind, _) = case kind of
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
