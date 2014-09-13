module Kitten.Parsec
  ( module Parsec
  ) where

import Text.Parsec as Parsec hiding
  ( (<|>)
  , many
  , newline
  , optional
  , parse
  , satisfy
  , token
  , tokens
  )
