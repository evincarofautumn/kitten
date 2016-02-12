{-# LANGUAGE OverloadedStrings #-}

module Kitten.Vocabulary
  ( globalVocabulary
  , globalVocabularyName
  ) where

import Data.Text (Text)
import Kitten.Name (Qualifier(..))

-- FIXME: Remove Hungarian.
globalVocabulary :: Qualifier
globalVocabulary = Qualifier [globalVocabularyName]

globalVocabularyName :: Text
globalVocabularyName = ""
