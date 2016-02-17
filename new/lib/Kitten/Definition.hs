module Kitten.Definition
  ( Definition(..)
  , Category(..)
  ) where

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
  , name :: !Qualified
  , origin :: !Origin
  , signature :: !Signature
  } deriving (Show)

data Category
  = Instance
  | Permission
  | Word
  deriving (Eq, Show)

instance Pretty (Definition a) where
  pPrint definition = Pretty.asDefinition
    (pPrint $ name definition)
    (pPrint $ signature definition)
    (pPrint $ body definition)
    (pPrint Token.Define)
