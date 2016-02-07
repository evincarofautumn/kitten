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

data Program a = Program
  { definitions :: HashMap (Qualified, Type) (Term a)
  , traits :: HashMap Qualified Type
  }

empty :: Program a
empty = Program
  { definitions = HashMap.empty
  , traits = HashMap.empty
  }

instance Pretty (Program a) where
  pPrint program = Pretty.vsep
    $ map (\ ((name, type_), term) -> Pretty.asDefinition
      (pPrint name) (pPrint type_) (pPrint term)
      (pPrint Token.Define))
    $ HashMap.toList $ definitions program
