module Kitten.Fragment
  ( Fragment(..)
  ) where

import Kitten.Declaration (Declaration)
import Kitten.Definition (Definition)
import Kitten.Metadata (Metadata)
import Kitten.Operator (Operator)
import Kitten.Synonym (Synonym)
import Kitten.TypeDefinition (TypeDefinition)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Kitten.Pretty as Pretty

data Fragment a = Fragment
  { declarations :: [Declaration]
  , definitions :: [Definition a]
  , metadata :: [Metadata]
  , operators :: [Operator]
  , synonyms :: [Synonym]
  , types :: [TypeDefinition]
  } deriving (Show)

instance Monoid (Fragment a) where
  mempty = Fragment
    { declarations = []
    , definitions = []
    , metadata = []
    , operators = []
    , synonyms = []
    , types = []
    }
  mappend a b = Fragment
    { declarations = declarations a ++ declarations b
    , definitions = definitions a ++ definitions b
    , metadata = metadata a ++ metadata b
    , operators = operators a ++ operators b
    , synonyms = synonyms a ++ synonyms b
    , types = types a ++ types b
    }

instance Pretty (Fragment a) where
  pPrint fragment = Pretty.vsep $ concat
    [ map pPrint $ definitions fragment
    , map pPrint $ metadata fragment
    , map pPrint $ operators fragment
    , map pPrint $ synonyms fragment
    , map pPrint $ types fragment
    ]
