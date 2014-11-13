module Kitten.Annotation where

import Data.Vector (Vector)

import Kitten.Location
import Kitten.Name

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
  | AnQuantified !(Vector Name) !(Vector Name) !AnType
  | AnStackFunction !Name !(Vector AnType) !Name !(Vector AnType)
  | AnVar !Name
  | AnVector !AnType
  deriving (Eq, Show)
