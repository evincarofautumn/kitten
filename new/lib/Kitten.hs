{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Kitten
  ( compile
  , desugarFragment
  , fragmentFromSource
  , requestsFromFragment
  , runKitten
  , tokenize
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable (foldlM)
import Data.Text (Text)
import Kitten.CollectInstantiations (collectInstantiations)
import Kitten.Dictionary (Dictionary)
import Kitten.Fragment (Fragment)
import Kitten.Infer (inferTypes)
import Kitten.Informer (Informer(..))
import Kitten.Layout (layout)
import Kitten.Linearize (linearize)
import Kitten.Monad (K, runKitten)
import Kitten.Parse (parse)
import Kitten.Program (Program)
import Kitten.Request (Request, submit)
import Kitten.Resolve (resolveNames)
import Kitten.Scope (scope)
import Kitten.Tokenize (tokenize)
import Kitten.Type (Type)
import Text.Parsec.Text ()
import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Text
import qualified Kitten.Desugar.Data as Data
import qualified Kitten.Desugar.Infix as Infix
import qualified Kitten.Desugar.Quotations as Quotations
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Quantify as Quantify
import qualified Kitten.TypeEnv as TypeEnv

-- The compiler is a straightforward pipeline. At each stage, errors and
-- warnings ("reports") are accumulated, and reported to the programmer at the
-- next checkpoint.

compile :: [FilePath] -> K Dictionary
compile paths = do

-- Source files must be encoded in UTF-8.

  sources <- liftIO $ mapM readFileUtf8 paths
  parsed <- mconcat <$> zipWithM fragmentFromSource paths sources
  desugared <- desugarFragment parsed
  requests <- requestsFromFragment desugared
  foldlM (flip submit) Dictionary.empty (requests :: [Request])

  where

  readFileUtf8 :: FilePath -> IO Text
  readFileUtf8 = fmap Text.decodeUtf8 . ByteString.readFile

fragmentFromSource :: FilePath -> Text -> K (Fragment ())
fragmentFromSource path source = do

-- Sources are lexed into a stream of tokens.

  tokenized <- tokenize path source
  checkpoint

-- Next, the layout rule is applied to desugar indentation-based syntax, so that
-- the parser can find the ends of blocks without checking the indentation of
-- tokens.

  laidout <- layout path tokenized
  checkpoint

-- We then parse the token stream as a series of top-level program elements.

  parsed <- parse path laidout
  checkpoint

  return parsed

desugarFragment :: Fragment () -> K (Fragment ())
desugarFragment fragment = do

-- Datatype definitions are desugared into regular definitions, so that name
-- resolution can find their names.

  dataDesugared <- Data.desugar fragment

-- Name resolution rewrites unqualified names into fully qualified names, so
-- that it's evident from a name which program element it refers to.

  resolved <- resolveNames dataDesugared
  checkpoint

-- After names have been resolved, the precedences of operators are known, so
-- infix operators can be desugared into postfix syntax.

  postfix <- Infix.desugar resolved
  checkpoint

-- In addition, now that we know which names refer to local variables,
-- quotations can be rewritten into closures that explicitly capture the
-- variables they use from the enclosing scope.

  return $ scope postfix

-- A fragment must be lowered from a high-level set of program elements into a
-- low-level list of requests suitable for issuing to the dictionary.

requestsFromFragment :: Fragment () -> K [Request]
requestsFromFragment fragment = do
  error "TODO requestsFromFragment"

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
