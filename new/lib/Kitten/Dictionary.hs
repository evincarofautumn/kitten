{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Kitten.Dictionary
  ( Dictionary(..)
  , empty
  , lookup
  , operatorMetadata
  , signatures
  , typeNames
  , wordNames
  ) where

import Data.Char (isLetter)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (mapMaybe)
import Kitten.Entry (Entry)
import Kitten.Name
import Kitten.Operator (Operator(Operator))
import Kitten.Signature (Signature)
import Prelude hiding (lookup)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Kitten.Entry as Entry
import qualified Kitten.Entry.Category as Category
import qualified Kitten.Operator as Operator
import qualified Kitten.Term as Term

data Dictionary = Dictionary
  { entries :: !(HashMap Qualified Entry)
  } deriving (Show)

empty :: Dictionary
empty = Dictionary
  { entries = HashMap.empty
  }

lookup :: Qualified -> Dictionary -> Maybe Entry
lookup name = HashMap.lookup name . entries

-- The dictionary should generally be monotonically increasing in size and
-- specificity. We never remove definitions and we never remove data from
-- definitions.
--
-- words:
--   makeOperator -> + precedence associativity, fail if not operator name
--   declareTrait = declareWord + category
--   defineDefault = defineWord (define default instance of trait)
--   defineInstance = defineWord + trait
--   declareInstance = declareWord + trait
--   addMetadata = + metadata
--   declareConstructor = declareWord + type
--   defineConstructor = defineWord
--   declarePermission = declareWord + category
--   definePermission = defineWord
--   export = + export
--
-- types:
--   declareType = + origin parameters
--   export = + export

operatorMetadata :: Dictionary -> HashMap Qualified Operator
operatorMetadata dictionary = HashMap.fromList $ map getMetadata
  $ filter isOperatorName $ wordNames dictionary
  where

  getMetadata :: Qualified -> (Qualified, Operator)
  getMetadata name = let
    key = Qualified (qualifierFromName name) (Unqualified "operator")
    in case HashMap.lookup key $ entries dictionary of
      Just (Entry.Metadata _ term)
        -- TODO: Report invalid metadata.
        | [Term.Word _ _
            (UnqualifiedName (Unqualified associativityName)) _ origin]
          <- Term.decompose term
        -> case associativityName of
          -- TODO: Use actual precedences.
          "left" -> (name, Operator
            { Operator.associativity = Operator.Leftward
            , Operator.name = name
            , Operator.precedence = Operator.Precedence 6
            })
          "right" -> (name, Operator
            { Operator.associativity = Operator.Rightward
            , Operator.name = name
            , Operator.precedence = Operator.Precedence 6
            })
      -- If unspecified, use default associativity and precedence.
      _ -> (name, Operator
        { Operator.associativity = Operator.Nonassociative
        , Operator.name = name
        , Operator.precedence = Operator.Precedence 6
        })

signatures :: Dictionary -> [(Qualified, Signature)]
signatures = mapMaybe getSignature . HashMap.toList . entries
  where
  getSignature :: (Qualified, Entry) -> Maybe (Qualified, Signature)
  getSignature (name, Entry.Word _ _ _ _ (Just signature) _)
    = Just (name, signature)
  getSignature (name, Entry.Trait _ signature)
    = Just (name, signature)
  getSignature _ = Nothing

typeNames :: Dictionary -> [Qualified]
typeNames = mapMaybe typeName . HashMap.toList . entries
  where
  typeName (name, Entry.Word Category.Permission _ _ _ _ _) = Just name
  typeName (name, Entry.Type{}) = Just name
  typeName _ = Nothing

wordNames :: Dictionary -> [Qualified]
wordNames = mapMaybe wordName . HashMap.toList . entries
  where
  wordName (name, Entry.Word{}) = Just name
  wordName _ = Nothing
