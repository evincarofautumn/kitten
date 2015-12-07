module Kitten.Fragment
  ( Fragment(..)
  ) where

import Kitten.DataDefinition (DataDefinition)
import Kitten.Definition (Definition)
import Kitten.Operator (Operator)
import Kitten.Synonym (Synonym)
import Kitten.Trait (Trait)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Kitten.Pretty as Pretty

data Fragment = Fragment
  { dataDefinitions :: [DataDefinition]
  , definitions :: [Definition]
  , operators :: [Operator]
  , synonyms :: [Synonym]
  , traits :: [Trait]
  } deriving (Show)

instance Monoid Fragment where
  mempty = Fragment
    { dataDefinitions = []
    , definitions = []
    , operators = []
    , synonyms = []
    , traits = []
    }
  mappend a b = Fragment
    { dataDefinitions = dataDefinitions a ++ dataDefinitions b
    , definitions = definitions a ++ definitions b
    , operators = operators a ++ operators b
    , synonyms = synonyms a ++ synonyms b
    , traits = traits a ++ traits b
    }

instance Pretty Fragment where
  pPrint fragment = Pretty.vsep $ concat
    [ map pPrint $ dataDefinitions fragment
    , map pPrint $ definitions fragment
    , map pPrint $ operators fragment
    , map pPrint $ synonyms fragment
    ]
