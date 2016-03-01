module Kitten.Dictionary
  ( Dictionary(..)
  , empty
  ) where

import Data.HashMap.Strict (HashMap)
import Kitten.Monad (K)
import Kitten.Name (Qualified)
import Kitten.Origin (Origin)
import Kitten.Signature (Signature)
import Kitten.Term (Term)
import Kitten.Type (Type)
import Prelude hiding (words)
import qualified Data.HashMap.Strict as HashMap
import qualified Kitten.Definition as Definition
import qualified Kitten.Entry.Category as Category
import qualified Kitten.Entry.Type as Type
import qualified Kitten.Entry.Word as Word

data Dictionary = Dictionary
  { words :: !(HashMap Qualified Word.Entry)
  , types :: !(HashMap Qualified Type.Entry)
  } deriving (Show)

empty :: Dictionary
empty = Dictionary
  { words = HashMap.empty
  , types = HashMap.empty
  }

declareWord
  :: Qualified -> Signature -> Origin -> Dictionary -> Maybe Dictionary
declareWord name signature origin dictionary
  = case HashMap.lookup name $ words dictionary of
    -- Not previously declared or defined.
    Nothing -> let
      entry = Word.Entry
        { Word.associativity = Nothing
        , Word.body = Nothing
        , Word.category = Category.Word
        , Word.export = False
        , Word.metadata = HashMap.empty
        , Word.origin = origin
        , Word.precedence = Nothing
        , Word.signature = signature
        , Word.parent = Nothing
        }
      in Just dictionary
        { words = HashMap.insert name entry $ words dictionary }
    -- Already declared with the same signature.
    Just existing | Word.signature existing == signature
      -> Just dictionary
    -- Already declared or defined with a different signature.
    Just{} -> Nothing

defineWord
  :: Qualified -> Signature -> Term a -> Origin
  -> Dictionary -> K (Maybe Dictionary)
defineWord name signature body origin dictionary = do
  body' <- typecheck body dictionary
  case HashMap.lookup name $ words dictionary of
    -- Previously declared but not defined.
    Just existing@Word.Entry{ Word.body = Nothing } -> return $ Just dictionary
      { words = HashMap.insert name existing { Word.body = Just body' }
        $ words dictionary }
    -- Not previously declared.
    Nothing -> return Nothing
    -- Already defined.
    Just{} -> return Nothing

typecheck :: a
typecheck = error "TODO: typecheck"

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
