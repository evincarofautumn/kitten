module Kitten.Anno
  ( Anno(..)
  ) where

import Data.Text (Text)

import Kitten.Type

data Anno = Anno
  { annoName :: !Text
  , annoType :: TypeScheme
  }
