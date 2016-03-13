{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Kitten
  ( Enter.fragmentFromSource
  , compile
  , runKitten
  , tokenize
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text)
import Kitten.Dictionary (Dictionary)
import Kitten.Monad (K, runKitten)
import Kitten.Tokenize (tokenize)
import Text.Parsec.Text ()
import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Text
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Enter as Enter

-- The compiler is a straightforward pipeline. At each stage, errors and
-- warnings ("reports") are accumulated, and reported to the programmer at the
-- next checkpoint.

compile :: [FilePath] -> K Dictionary
compile paths = do

-- Source files must be encoded in UTF-8.

  sources <- liftIO $ mapM readFileUtf8 paths
  parsed <- mconcat <$> zipWithM Enter.fragmentFromSource paths sources
  Enter.fragment parsed Dictionary.empty
  where

  readFileUtf8 :: FilePath -> IO Text
  readFileUtf8 = fmap Text.decodeUtf8 . ByteString.readFile

{-

    definitions :: [Definition a]
    metadata :: [Metadata]
    operators :: [Operator]
    synonyms :: [Synonym]
    traits :: [Trait]
    types :: [TypeDefinition]

->

define f (...) { ... }
define g (...) { ... }
about f { k1 { v1 } ... kn { vn } }
define + (...) { ... }
trait * (...)
instance * (...) { ... }
infix ... *
type t { case c1 (...) ... cn }

=>

// declare f (...)
PUT /words/f
category=word&signature=...

// declare g (...)
PUT /words/g
category=word&signature=...

// declare + (...)
PUT /words/%2B
category=operator&signature=...

// trait * (...)
PUT /words/%2A
category=trait&signature=...

// instance * (...) { ... }
PUT /words/<mangled>
category=instance&signature=...&body=...

// declare type t<...>
PUT /types/t
parameters=...

// declare constructor c1 for t
PUT /words/c1
category=constructor&type=t&signature=...

// about f { ... }
POST /words/f
k1=v1&...&kn=vn

// infix right 5 +
POST /words/%2B
associativity=right&precedence=5

data Request

  = DeclareWord
  -- associativity = Nothing
  -- category = Word
  -- body = Nothing
  -- export = ...
  -- origin = ...
  -- precedence = Nothing
  -- signature = ...
  -- trait = Nothing
  -- type_ = Nothing

  | DeclareOperator
  -- associativity = ...
  -- category = Operator
  -- body = Nothing
  -- export = ...
  -- origin = ...
  -- precedence = ...
  -- signature = ...
  -- trait = Nothing
  -- type_ = Nothing

  | DeclareInstance
  -- associativity = Nothing
  -- category = Instance
  -- body = Nothing
  -- export = ...
  -- origin = ...
  -- precedence = ...?
  -- signature = ...
  -- trait = Just ...
  -- type_ = Nothing

  | DeclarePermission
  -- associativity = Nothing
  -- category = Permission
  -- body = Nothing
  -- export = ...
  -- origin = ...
  -- precedence = Nothing
  -- signature = ...
  -- trait = Nothing
  -- type_ = Nothing

  | DeclareConstructor
  -- associativity = Nothing
  -- category = Constructor
  -- body = Nothing
  -- export = ...
  -- origin = ...
  -- precedence = Nothing
  -- signature = ...
  -- trait = Nothing
  -- type_ = Just ...

  | DeclareTrait
  -- associativity = Nothing
  -- category = Trait
  -- body = Nothing
  -- export = ...
  -- origin = ...
  -- precedence = ...?
  -- signature = ...
  -- trait = Nothing
  -- type_ = Nothing

  | AddDefinition
  -- body = Just ...

  | AddMetadata
  -- metadata = ...

  | DeclareType
  -- export = ...
  -- origin = ...

-}
