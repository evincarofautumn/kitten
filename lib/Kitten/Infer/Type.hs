module Kitten.Infer.Type
  ( fromAnno
  ) where

import Control.Applicative
import Control.Monad.Trans.State
import Data.List

import qualified Data.Set as Set

import Kitten.Anno (Anno(Anno))
import Kitten.Infer.Monad
import Kitten.Name
import Kitten.Type

import qualified Kitten.Anno as Anno

fromAnno :: Anno -> Inferred Scheme
fromAnno (Anno annoType _) = do
  Name startIndex <- getsEnv envNext
  let
    (count, type_) = fromAnnoType startIndex annoType
    scheme = Forall
      (Set.fromList (map (Name . (startIndex +)) [0 .. pred count]))
      type_
  modifyEnv $ \ env -> env
    { envNext = Name (nameIndex (envNext env) + count) }
  return scheme

fromAnnoType :: Int -> Anno.Type -> (Int, Type)
fromAnnoType startIndex annoType
  = let (type_, names) = flip runState [] $ fromAnnoType' annoType
  in (length names, type_)

  where
  fromAnnoType' :: Anno.Type -> State [String] Type
  fromAnnoType' type_ = case type_ of

    Anno.Function a b p -> FunctionType
      <$> mapM fromAnnoType' a
      <*> mapM fromAnnoType' b
      <*> pure p

    Anno.Bool -> return BoolType

    Anno.Char -> return CharType

    Anno.Float -> return FloatType

    Anno.Handle -> return HandleType

    Anno.Int -> return IntType

    Anno.Pair a b -> (:&) <$> fromAnnoType' a <*> fromAnnoType' b

    Anno.Unit -> return UnitType

    Anno.Var name -> do
      mExisting <- gets $ elemIndex name
      case mExisting of
        Just existing -> return $ TypeVar (Name (startIndex + existing))
        Nothing -> do
          index <- gets length
          modify (++ [name])
          return $ TypeVar (Name (startIndex + index))

    Anno.Vector a -> VectorType <$> fromAnnoType' a
