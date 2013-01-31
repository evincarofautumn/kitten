module Kitten.Embed
  ( embed
  ) where

import Language.Haskell.TH.Syntax

embed :: FilePath -> Q Exp
embed path = do
  qAddDependentFile path
  string <- runIO (readFile path)
  return . LitE $ StringL string
