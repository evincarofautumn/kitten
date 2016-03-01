module Kitten.Request
  ( Request(..)
  , submit
  ) where

import Data.HashMap.Strict (HashMap)
import Kitten.Dictionary (Dictionary)
import Kitten.Entry.Category (Category)
import Kitten.Entry.Parent (Parent)
import Kitten.Kind (Kind)
import Kitten.Monad (K)
import Kitten.Name (Qualified, Unqualified)
import Kitten.Operator (Associativity, Precedence)
import Kitten.Origin (Origin)
import Kitten.Signature (Signature)
import Kitten.Term (Term)
import qualified Data.HashMap.Strict as HashMap
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Entry.Word as Word

data Request
  = DeclareWord !Qualified !Category {-export-}!Bool !Origin !Signature !(Maybe Parent)
  | MakeOperator !Qualified !(Maybe Associativity) !Precedence
  | DefineWord !Qualified !(Term ())
  | AboutWord !Qualified !(HashMap Unqualified (Term ()))
  | AboutType !Qualified !(HashMap Unqualified (Term ()))
  | DeclareType !Qualified {-export-}!Bool !Origin {-params-}[(Unqualified, Kind, Origin)]

submit :: Request -> Dictionary -> K Dictionary
submit request dictionary = case request of
  DeclareWord name category export origin signature parent -> do
    case HashMap.lookup name $ Dictionary.words dictionary of
      Just{} -> fail "existing"
      Nothing -> let
        entry = Word.Entry
          { Word.associativity = Nothing
          , Word.category = category
          , Word.body = Nothing
          , Word.export = export
          , Word.metadata = HashMap.empty
          , Word.origin = origin
          , Word.precedence = Nothing
          , Word.signature = signature
          , Word.parent = parent
          }
        in return dictionary
          { Dictionary.words = HashMap.insert name entry
            $ Dictionary.words dictionary }
  _ -> fail "TODO: submit"

{-

-- Finally we can infer and check the types of all definitions.

-- Typechecking is done only when actually issuing a request, because
-- declarations need to have been added to the dictionary.

  inferred <- inferTypes fragment
  checkpoint

-- Knowing the inferred types of all quotations in the program, we can now lift
-- them into top-level definitions.

  flattened <- Quotations.desugar inferred

-- With fully desugared definitions, we can now make generic definitions
-- explicitly indicate the scalar type variables that must be instantiated when
-- generating specializations.

  let quantified = Quantify.program flattened

-- Also, we now have enough type information to make calls to the destructors
-- and copy constructors of locals explicit.

  let linear = linearize quantified

-- Now we can go through all definitions in the program, starting from the main
-- entry point, and collect specializations of generic definitions.

  collectInstantiations TypeEnv.empty linear
-}

--------------------------------------------------------------------------------

--   | AddDefinition
--   -- body = Just ...
-- 
--   | AddMetadata
--   -- metadata = ...
-- 
--   | DeclareType
--   -- export = ...
--   -- origin = ...

