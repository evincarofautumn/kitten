module Kitten.Desugar.Quotations
  ( desugar
  ) where

import Control.Monad (mapAndUnzipM)
import Control.Monad.Trans.State (StateT, evalStateT, get, put)
import Kitten.Monad (K)
import Kitten.Name (Closed(..), Qualified(..), Qualifier, Unqualified(..), qualifierFromName)
import Kitten.Program (Program)
import Kitten.Term (Case(..), Else(..), Term(..), Value(..))
import Kitten.Type (Type(..), Var(..))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Kitten.Free as Free
import qualified Kitten.Program as Program
import qualified Kitten.Term as Term

newtype LambdaIndex = LambdaIndex Int

desugar :: Program Type -> K (Program Type)
desugar program = do
  definitions <- HashMap.fromList . concat <$> mapM (uncurry desugarDefinition)
    (HashMap.toList $ Program.definitions program)
  return program { Program.definitions = definitions }
  where

  desugarDefinition
    :: (Qualified, Type) -> Term Type -> K [((Qualified, Type), Term Type)]
  desugarDefinition (name, type_) body = do
    (body', lifted) <- desugarTerm (qualifierFromName name) body
    return $ ((name, type_), body') : lifted

  desugarTerm :: Qualifier -> Term Type -> K (Term Type, [((Qualified, Type), Term Type)])
  desugarTerm qualifier = flip evalStateT (LambdaIndex 0) . go
    where

    go :: Term Type -> StateT LambdaIndex K (Term Type, [((Qualified, Type), Term Type)])
    go term = case term of
      Call{} -> done
      Compose type_ a b -> do
        (a', as) <- go a
        (b', bs) <- go b
        return (Compose type_ a' b', as ++ bs)
      Drop{} -> done
      Generic{} -> error
        "generic expression should not appear before desugaring"
      Group{} -> error "group should not appear after infix desugaring"
      Identity{} -> done
      If type_ a b origin -> do
        (a', as) <- go a
        (b', bs) <- go b
        return (If type_ a' b' origin, as ++ bs)
      Intrinsic{} -> done
      Lambda type_ name varType a origin -> do
        (a', as) <- go a
        return (Lambda type_ name varType a' origin, as)
      Match type_ cases mElse origin -> do
        (cases', as) <- flip mapAndUnzipM cases
          $ \ (Case name a caseOrigin) -> do
            (a', xs) <- go a
            return (Case name a' caseOrigin, xs)
        (mElse', bs) <- case mElse of
          Just (Else a elseOrigin) -> do
            (a', xs) <- go a
            return (Just (Else a' elseOrigin), xs)
          Nothing -> return (Nothing, [])
        return (Match type_ cases' mElse' origin, concat as ++ bs)
      New{} -> done
      NewClosure{} -> done
      NewVector{} -> done
      -- FIXME: Should be Closure, not Quotation.
      Push _type (Closure closed a) origin -> do
        (a', as) <- go a
        LambdaIndex index <- get
        let
          name = Qualified qualifier
            $ Unqualified $ Text.pack $ "lambda" ++ show index
        put $ LambdaIndex $ succ index
        let
          deducedType = Term.type_ a
          type_ = foldr (uncurry ((Forall origin .) . Var)) deducedType
            $ Map.toList $ Free.tvks deducedType
        return
          ( Term.compose todoTyped origin $ map pushClosed closed ++
            -- FIXME: What type should be used here?
            [ Push todoTyped (Name name) origin
            , NewClosure todoTyped (length closed) origin
            ]
          , ((name, type_), a') : as
          )
        where

        pushClosed :: Closed -> Term Type
        pushClosed name = Push todoTyped (case name of
          ClosedLocal index -> Local index
          ClosedClosure index -> Closed index) origin

      Push{} -> done
      Swap{} -> done
      With{} -> done
      Word{} -> done
      where
      done = return (term, [])

todoTyped :: a
todoTyped = error "TODO: generate typed terms"
