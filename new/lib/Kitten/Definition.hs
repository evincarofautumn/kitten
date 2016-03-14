{-# LANGUAGE OverloadedStrings #-}

module Kitten.Definition
  ( Definition(..)
  , isMain
  , main
  ) where

import Kitten.Entry.Category (Category)
import Kitten.Entry.Merge (Merge)
import Kitten.Entry.Parameter (Parameter(..))
import Kitten.Kind (Kind(..))
import Kitten.Name (GeneralName(..), Qualified(..))
import Kitten.Operator (Fixity)
import Kitten.Origin (Origin)
import Kitten.Signature (Signature)
import Kitten.Term (Term)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Kitten.Entry.Category as Category
import qualified Kitten.Entry.Merge as Merge
import qualified Kitten.Operator as Operator
import qualified Kitten.Pretty as Pretty
import qualified Kitten.Signature as Signature
import qualified Kitten.Term as Term
import qualified Kitten.Token as Token
import qualified Kitten.Vocabulary as Vocabulary

data Definition a = Definition
  { body :: !(Term a)
  , category :: !Category
  , fixity :: !Fixity
  , merge :: !Merge
  , name :: !Qualified
  , origin :: !Origin
  , signature :: !Signature
  } deriving (Show)

instance Pretty (Definition a) where
  pPrint definition = Pretty.asDefinition
    (pPrint $ name definition)
    (pPrint $ signature definition)
    (pPrint $ body definition)
    (pPrint Token.Define)

-- The main definition, created implicitly from top-level code in program
-- fragments. The list of permissions are those granted by the runtime.

main :: [GeneralName] -> Term a -> Definition a
main permissions term = Definition
  { body = term
  , category = Category.Word
  , fixity = Operator.Postfix
  , merge = Merge.Compose
  , name = Qualified Vocabulary.global "main"
  , origin = o
  , signature = Signature.Quantified
    [Parameter o "R" Stack, Parameter o "S" Stack]
    (Signature.StackFunction "R" [] "S" []
      permissions o) o
  }
  where o = Term.origin term

isMain :: Definition a -> Bool
isMain = (== Qualified Vocabulary.global "main") . name
