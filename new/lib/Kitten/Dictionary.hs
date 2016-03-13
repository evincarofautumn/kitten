{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Kitten.Dictionary
  ( Dictionary
  , empty
  , fromList
  , insert
  , lookup
  , operatorMetadata
  , signatures
  , toList
  , typeNames
  , wordNames
  ) where

import Control.Applicative (liftA2)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (mapMaybe)
import Kitten.Entry (Entry)
import Kitten.Name
import Kitten.Operator (Operator(Operator))
import Kitten.Signature (Signature)
import Prelude hiding (lookup)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.HashMap.Strict as HashMap
import qualified Kitten.Entry as Entry
import qualified Kitten.Entry.Category as Category
import qualified Kitten.Operator as Operator
import qualified Kitten.Term as Term

data Dictionary = Dictionary
  { entries :: !(HashMap Qualified Entry)
  } deriving (Show)

instance Pretty Dictionary where
  pPrint = pPrint . HashMap.keys . entries

empty :: Dictionary
empty = Dictionary
  { entries = HashMap.empty
  }

fromList :: [(Qualified, Entry)] -> Dictionary
fromList = Dictionary . HashMap.fromList

-- Directly inserts into the dictionary. This is somewhat unsafe, as it can lead
-- to an invalid dictionary state.

insert ::  Qualified -> Entry -> Dictionary -> Dictionary
insert name entry dictionary = dictionary
  { entries = HashMap.insert name entry $ entries dictionary }

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
      -- TODO: Report invalid metadata.
      -- TODO: Avoid redundant decomposition.
      Just (Entry.Metadata _ term)

        -- Just associativity.
        | [Term.Word _ _ (UnqualifiedName (Unqualified assoc)) _ _]
          <- Term.decompose term
        , Just associativity <- associativityFromName assoc
        -> yield associativity defaultPrecedence

        -- Just precedence.
        | [Term.Push _ (Term.Integer prec) _]
          <- Term.decompose term
        , validPrecedence prec
        -> yield defaultAssociativity
          $ Operator.Precedence $ fromInteger prec

        -- Associativity and precedence.
        | [ Term.Word _ _ (UnqualifiedName (Unqualified assoc)) _ _
          , Term.Push _ (Term.Integer prec) _
          ] <- Term.decompose term
        , Just associativity <- associativityFromName assoc
        , validPrecedence prec
        -> yield associativity
          $ Operator.Precedence $ fromInteger prec

        -- FIXME: Generate real report.
        | otherwise -> error "invalid operator metadata"

      _ -> yield defaultAssociativity defaultPrecedence

      where

      associativityFromName "left" = Just Operator.Leftward
      associativityFromName "right" = Just Operator.Rightward
      associativityFromName _ = Nothing

      validPrecedence = liftA2 (&&) (>= 0) (<= 9)

      defaultPrecedence = Operator.Precedence 6
      defaultAssociativity = Operator.Nonassociative

      yield associativity precedence = (name, Operator
        { Operator.associativity = associativity
        , Operator.name = name
        , Operator.precedence = precedence
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

toList :: Dictionary -> [(Qualified, Entry)]
toList = HashMap.toList . entries

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
