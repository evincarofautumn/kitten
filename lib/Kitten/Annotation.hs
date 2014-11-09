module Kitten.Annotation where

import Data.Text (Text)
import Data.Vector (Vector)

import Kitten.Location

data Anno = Anno AnType Location | TestAnno
  deriving (Show)

instance Eq Anno where
  TestAnno == _ = True
  _ == TestAnno = True
  Anno type1 loc1 == Anno type2 loc2 = (type1, loc1) == (type2, loc2)

data AnType
  = AnApp !AnType !(Vector AnType)
  | AnFunction !(Vector AnType) !(Vector AnType)
  | AnChoice !AnType !AnType
  | AnOption !AnType
  | AnPair !AnType !AnType
  | AnQuantified !(Vector Text) !(Vector Text) !AnType
  | AnStackFunction !Text !(Vector AnType) !Text !(Vector AnType)
  | AnVar !Text
  | AnVector !AnType
  deriving (Eq, Show)
