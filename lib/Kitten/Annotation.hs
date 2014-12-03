module Kitten.Annotation where

import Data.Vector (Vector)

import Kitten.Location
import Kitten.Name

data Anno = Anno AnType | TestAnno
  deriving (Show)

instance Eq Anno where
  TestAnno == _ = True
  _ == TestAnno = True
  Anno type1 == Anno type2 = type1 == type2

data AnType
  = AnApp !AnType !(Vector AnType) !Location
  | AnFunction !(Vector AnType) !(Vector AnType) !Location
  | AnChoice !AnType !AnType !Location
  | AnOption !AnType !Location
  | AnPair !AnType !AnType !Location
  | AnQuantified
    !(Vector (Name, Location))
    !(Vector (Name, Location))
    !AnType
    !Location
  | AnStackFunction !Name !(Vector AnType) !Name !(Vector AnType) !Location
  | AnVar !Name !Location
  | AnVector !AnType !Location
  deriving (Eq, Show)
