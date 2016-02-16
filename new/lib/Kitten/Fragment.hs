module Kitten.Fragment
  ( Fragment(..)
  ) where

import Kitten.Definition (Definition)
import Kitten.Operator (Operator)
import Kitten.Synonym (Synonym)
import Kitten.Trait (Trait)
import Kitten.TypeDefinition (TypeDefinition)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Kitten.Pretty as Pretty

data Fragment a = Fragment
  { definitions :: [Definition a]
  , operators :: [Operator]
  , synonyms :: [Synonym]
  , traits :: [Trait]
  , types :: [TypeDefinition]
  } deriving (Show)

instance Monoid (Fragment a) where
  mempty = Fragment
    { definitions = []
    , operators = []
    , synonyms = []
    , traits = []
    , types = []
    }
  mappend a b = Fragment
    { definitions = definitions a ++ definitions b
    , operators = operators a ++ operators b
    , synonyms = synonyms a ++ synonyms b
    , traits = traits a ++ traits b
    , types = types a ++ types b
    }

instance Pretty (Fragment a) where
  pPrint fragment = Pretty.vsep $ concat
    [ map pPrint $ definitions fragment
    , map pPrint $ operators fragment
    , map pPrint $ synonyms fragment
    , map pPrint $ types fragment
    ]
