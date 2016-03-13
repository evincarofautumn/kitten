module Kitten.Definition
  ( Definition(..)
  ) where

import Kitten.Entry.Category (Category)
import Kitten.Entry.Merge (Merge)
import Kitten.Name (Qualified)
import Kitten.Operator (Fixity)
import Kitten.Origin (Origin)
import Kitten.Signature (Signature)
import Kitten.Term (Term)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Kitten.Pretty as Pretty
import qualified Kitten.Token as Token

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
