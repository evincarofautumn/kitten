{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Kitten
  ( compile
  , runKitten
  , tokenize
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text)
import Kitten.CollectInstantiations (collectInstantiations)
import Kitten.Infer (inferTypes)
import Kitten.Informer (Informer(..))
import Kitten.Layout (layout)
import Kitten.Linearize (linearize)
import Kitten.Monad (K, runKitten)
import Kitten.Parse (parse)
import Kitten.Program (Program)
import Kitten.Resolve (resolveNames)
import Kitten.Scope (scope)
import Kitten.Tokenize (tokenize)
import Text.Parsec.Text ()
import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Text
import qualified Kitten.Desugar.Data as Data
import qualified Kitten.Desugar.Infix as Infix
import qualified Kitten.Desugar.Quotations as Quotations
import qualified Kitten.Quantify as Quantify
import qualified Kitten.TypeEnv as TypeEnv

-- The compiler is a straightforward pipeline. At each stage, errors and
-- warnings ("reports") are accumulated, and reported to the programmer at the
-- next checkpoint.

compile :: [FilePath] -> K Program
compile paths = do

-- Source files must be encoded in UTF-8.

  sources <- liftIO $ mapM readFileUtf8 paths

-- They are lexed into a stream of tokens.

  tokenized <- zipWithM tokenize paths sources
  checkpoint

-- Next, the layout rule is applied to desugar indentation-based syntax, so that
-- the parser can find the ends of blocks without checking the indentation of
-- tokens.

  laidout <- zipWithM layout paths tokenized
  checkpoint

-- We parse the token stream as a series of top-level program elements.

  parsed <- mconcat <$> zipWithM parse paths laidout
  checkpoint

-- Datatype definitions are desugared into regular definitions, so that name
-- resolution can find their names.

  dataDesugared <- Data.desugar parsed

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

  let scoped = scope postfix

-- Finally we can infer and check the types of all definitions.

  inferred <- inferTypes scoped
  checkpoint

-- Knowing the inferred types of all quotations in the program, we can now lift
-- them into top-level definitions.

  desugared <- Quotations.desugar inferred

-- With fully desugared definitions, we can now make generic definitions
-- explicitly indicate the scalar type variables that must be instantiated when
-- generating specializations.

  let quantified = Quantify.program desugared

-- Also, we now have enough type information to make calls to the destructors
-- and copy constructors of locals explicit.

  let linear = linearize quantified

-- Now we can go through all definitions in the program, starting from the main
-- entry point, and collect specializations of generic definitions.

  instantiated <- collectInstantiations TypeEnv.empty linear
  -- generate instance declarations
  --   (so inference can know what's available in a precompiled module)
  -- generate code
  return instantiated

  where

  readFileUtf8 :: FilePath -> IO Text
  readFileUtf8 = fmap Text.decodeUtf8 . ByteString.readFile
