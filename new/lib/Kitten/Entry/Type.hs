module Kitten.Entry.Type
  ( Entry(..)
  ) where

import Data.HashMap.Strict (HashMap)
import Kitten.Kind (Kind)
import Kitten.Name (Qualified, Unqualified)
import Kitten.Origin (Origin)
import Kitten.Term (Term)

data Entry = Entry

-- The names of the constructors of this type.

  { constructors :: [Qualified]

-- Whether this type is visible outside its vocabulary.

  , export :: !Bool

-- User-defined metadata.

  , metadata :: !(HashMap Unqualified (Term ()))

-- Source location.

  , origin :: !Origin

-- Type parameters, for a generic definition.

  , parameters :: [(Unqualified, Kind, Origin)]

  } deriving (Show)
