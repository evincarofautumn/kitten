module Kitten.Util.Show
  ( showWords
  ) where

showWords :: (Show a) => [a] -> String
showWords = unwords . map show
