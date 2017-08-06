{-|
Module      : Kitten.Dictionary
Description : Program database
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Kitten.Dictionary
  ( Dictionary
  , empty
  , fromList
  , insert
  , lookup
  , member
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
import Kitten.Informer (Informer(..))
import Kitten.Instantiated (Instantiated(Instantiated))
import Kitten.Literal (IntegerLiteral(IntegerLiteral))
import Kitten.Name
import Kitten.Operator (Operator(Operator))
import Kitten.Origin (getOrigin)
import Kitten.Signature (Signature)
import Prelude hiding (lookup)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.HashMap.Strict as HashMap
import qualified Kitten.Entry as Entry
import qualified Kitten.Entry.Category as Category
import qualified Kitten.Operator as Operator
import qualified Kitten.Report as Report
import qualified Kitten.Term as Term

-- | A key-value store mapping an 'Instantiated' name to a dictionary 'Entry'.

data Dictionary = Dictionary
  { entries :: !(HashMap Instantiated Entry)
  } deriving (Show)

instance Pretty Dictionary where
  pPrint = pPrint . HashMap.keys . entries

empty :: Dictionary
empty = Dictionary
  { entries = HashMap.empty
  }

fromList :: [(Instantiated, Entry)] -> Dictionary
fromList = Dictionary . HashMap.fromList

-- | Directly inserts into the dictionary. This is somewhat unsafe, as it can
-- lead to an invalid dictionary state.

insert :: Instantiated -> Entry -> Dictionary -> Dictionary
insert name entry dictionary = dictionary
  { entries = HashMap.insert name entry $ entries dictionary }

lookup :: Instantiated -> Dictionary -> Maybe Entry
lookup name = HashMap.lookup name . entries

-- | Whether a name is present in the dictionary.

member :: Instantiated -> Dictionary -> Bool
member name = (name `HashMap.member`) . entries

-- | Compiles all operator metadata for infix desugaring.

operatorMetadata
  :: (Informer m) => Dictionary -> m (HashMap Qualified Operator)
operatorMetadata dictionary = HashMap.fromList <$> mapM getMetadata
  (filter isOperatorName $ wordNames dictionary)
  where

  getMetadata :: (Informer m) => Qualified -> m (Qualified, Operator)
  getMetadata name = let
    key = Qualified (qualifierFromName name) (Unqualified "operator")
    in case HashMap.lookup (Instantiated key []) $ entries dictionary of
      -- TODO: Report invalid metadata.
      -- TODO: Avoid redundant decomposition.
      Just (Entry.Metadata _ term)

        -- Just associativity.
        | [Term.SWord _ _ _ (UnqualifiedName (Unqualified assoc)) _]
          <- Term.decomposed term
        , Just associativity <- associativityFromName assoc
        -> yield associativity defaultPrecedence

        -- Just precedence.
        | [Term.SInteger _ _ (IntegerLiteral prec _base _bits)]
          <- Term.decomposed term
        , validPrecedence prec
        -> yield defaultAssociativity
          $ Operator.Precedence $ fromInteger prec

        -- Associativity and precedence.
        | [ Term.SWord _ _ _ (UnqualifiedName (Unqualified assoc)) _
          , Term.SInteger _ _ (IntegerLiteral prec _base _bits)
          ] <- Term.decomposed term
        , Just associativity <- associativityFromName assoc
        , validPrecedence prec
        -> yield associativity
          $ Operator.Precedence $ fromInteger prec

        | otherwise -> do
          report $ Report.InvalidOperatorMetadata (getOrigin term) name term
          yield defaultAssociativity defaultPrecedence

      _ -> yield defaultAssociativity defaultPrecedence

      where

      associativityFromName "left" = Just Operator.Leftward
      associativityFromName "right" = Just Operator.Rightward
      associativityFromName _ = Nothing

      validPrecedence = liftA2 (&&) (>= 0) (<= 9)

      defaultPrecedence = Operator.Precedence 6
      defaultAssociativity = Operator.Nonassociative

      yield associativity precedence = return (name, Operator
        { Operator.associativity = associativity
        , Operator.name = name
        , Operator.precedence = precedence
        })

-- | All type signatures (for words or traits) in the dictionary.

signatures :: Dictionary -> [(Qualified, Signature)]
signatures = mapMaybe getSignature . HashMap.toList . entries
  where
  getSignature :: (Instantiated, Entry) -> Maybe (Qualified, Signature)
  getSignature (Instantiated name [], Entry.Word _ _ _ _ (Just signature) _)
    = Just (name, signature)
  getSignature (Instantiated name [], Entry.Trait _ signature)
    = Just (name, signature)
  getSignature _ = Nothing

toList :: Dictionary -> [(Instantiated, Entry)]
toList = HashMap.toList . entries

-- | All type names (for data types or permissions) in the dictionary.

typeNames :: Dictionary -> [Qualified]
typeNames = mapMaybe typeName . HashMap.toList . entries
  where
  typeName (Instantiated name _, Entry.Word Category.Permission _ _ _ _ _)
    = Just name
  typeName (Instantiated name _, Entry.Type{}) = Just name
  typeName _ = Nothing

-- | All word names (for words or traits) in the dictionary.

wordNames :: Dictionary -> [Qualified]
wordNames = mapMaybe wordName . HashMap.toList . entries
  where
  wordName (Instantiated name [], Entry.Word{}) = Just name
  -- TODO: Figure out how to get mangled names out of this...
  wordName (Instantiated name _, Entry.Trait{}) = Just name
  wordName _ = Nothing
