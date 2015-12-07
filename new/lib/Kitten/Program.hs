module Kitten.Program
  ( Program(..)
  , empty
  ) where

import Data.HashMap.Strict (HashMap)
import Kitten.Name (Qualified)
import Kitten.Term (Term)
import Kitten.Type (Type)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.HashMap.Strict as HashMap
import qualified Kitten.Token as Token
import qualified Kitten.Pretty as Pretty

-- A program consists of a set of definitions.

data Program = Program
  { definitions :: HashMap (Qualified, Type) Term
  , traits :: HashMap Qualified Type
  }

empty :: Program
empty = Program
  { definitions = HashMap.empty
  , traits = HashMap.empty
  }

instance Pretty Program where
  pPrint program = Pretty.vsep
    $ map (\ ((name, type_), term) -> Pretty.asDefinition
      (pPrint name) (pPrint type_) (pPrint term)
      (pPrint Token.Define))
    $ HashMap.toList $ definitions program
