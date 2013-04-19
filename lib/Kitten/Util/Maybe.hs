module Kitten.Util.Maybe
  ( justIf
  ) where

justIf :: Bool -> a -> Maybe a
justIf condition value = if condition then Just value else Nothing
