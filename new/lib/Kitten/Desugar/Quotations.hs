module Kitten.Desugar.Quotations
  ( desugar
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, gets, modify, runStateT)
import Data.Traversable (forM)
import Kitten.Dictionary (Dictionary)
import Kitten.Infer (typecheck)
import Kitten.Monad (K)
import Kitten.Name (Closed(..), Qualified(..), Qualifier, Unqualified(..))
import Kitten.Term (Case(..), Else(..), Term(..), Value(..))
import Kitten.Type (Type(..), Var(..))
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Entry as Entry
import qualified Kitten.Entry.Category as Category
import qualified Kitten.Entry.Merge as Merge
import qualified Kitten.Free as Free
import qualified Kitten.Signature as Signature
import qualified Kitten.Term as Term

newtype LambdaIndex = LambdaIndex Int

desugar
  :: Dictionary
  -> Qualifier
  -> Term Type
  -> K (Term Type, Dictionary)
desugar dictionary qualifier term0 = do
  (term', (_, dictionary')) <- flip runStateT (LambdaIndex 0, dictionary)
    $ go term0
  return (term', dictionary')
  where

  go
    :: Term Type
    -> StateT (LambdaIndex, Dictionary) K (Term Type)
  go term = case term of
    Call{} -> done
    Compose type_ a b -> do
      a' <- go a
      b' <- go b
      return $ Compose type_ a' b'
    Drop{} -> done
    Generic type_ a origin -> do
      a' <- go a
      return $ Generic type_ a' origin
    Group{} -> error "group should not appear after infix desugaring"
    Identity{} -> done
    If type_ a b origin -> do
      a' <- go a
      b' <- go b
      return $ If type_ a' b' origin
    Lambda type_ name varType a origin -> do
      a' <- go a
      return $ Lambda type_ name varType a' origin
    Match type_ cases mElse origin -> do
      cases' <- forM cases
        $ \ (Case name a caseOrigin) -> do
          a' <- go a
          return $ Case name a' caseOrigin
      mElse' <- case mElse of
        Just (Else a elseOrigin) -> do
          a' <- go a
          return $ Just $ Else a' elseOrigin
        Nothing -> return Nothing
      return $ Match type_ cases' mElse' origin
    New{} -> done
    NewClosure{} -> done
    NewVector{} -> done
    Push _type (Capture closed a) origin -> do
      a' <- go a
      LambdaIndex index <- gets fst
      let
        name = Qualified qualifier
          $ Unqualified $ Text.pack $ "lambda" ++ show index
      modify $ \ (_, d) -> (LambdaIndex $ succ index, d)
      let
        deducedType = Term.type_ a
        type_ = foldr (uncurry ((Forall origin .) . Var)) deducedType
          $ Map.toList $ Free.tvks deducedType
      modify $ \ (l, d) -> let
        entry = Entry.Word
          Category.Word
          Merge.Deny
          (Term.origin a')
          Nothing
          (Just (Signature.Type type_))
          (Just a')
        in (l, Dictionary.insert name entry d)
      dict <- gets snd
      (typechecked, _) <- lift $ typecheck dict Nothing
        $ Term.compose () origin $ map pushClosed closed ++
          [ Push () (Name name) origin
          , NewClosure () (length closed) origin
          ]
      return typechecked
      where

      pushClosed :: Closed -> Term ()
      pushClosed name = Push () (case name of
        ClosedLocal index -> Local index
        ClosedClosure index -> Closed index) origin

    Push{} -> done
    Swap{} -> done
    With{} -> done
    Word{} -> done
    where
    done = return term
