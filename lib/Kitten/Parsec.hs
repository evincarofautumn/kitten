module Kitten.Parsec
  ( module Parsec
  ) where

import Text.Parsec as Parsec hiding
  ( (<|>)
  , Empty
  , many
  , newline
  , optional
  , parse
  , satisfy
  , token
  , tokens
  )
