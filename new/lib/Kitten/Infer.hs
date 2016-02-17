{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Kitten.Infer
  ( inferTypes
  ) where

import Control.Monad (forM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, get, gets, modify, put, runStateT)
import Data.Foldable (foldrM)
import Data.List (find, foldl')
import Data.Map (Map)
import Kitten.Fragment (Fragment)
import Kitten.Informer (Informer(..))
import Kitten.InstanceCheck (instanceCheck)
import Kitten.Intrinsic (Intrinsic)
import Kitten.Kind (Kind(..))
import Kitten.Monad (K)
import Kitten.Name (Closed(..), ClosureIndex(..), GeneralName(..), LocalIndex(..), Qualified(..), Unqualified(..))
import Kitten.Origin (Origin)
import Kitten.Program (Program(Program))
import Kitten.Regeneralize (regeneralize)
import Kitten.Signature (Signature)
import Kitten.Term (Case(..), Else(..), Term(..), Value(..))
import Kitten.Type (Constructor(..), Type(..), Var(..), funType, joinType, prodType)
import Kitten.TypeEnv (TypeEnv, freshTypeId)
import Kitten.Vocabulary (globalVocabulary)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Kitten.Definition as Definition
import qualified Kitten.Fragment as Fragment
import qualified Kitten.Instantiate as Instantiate
import qualified Kitten.Intrinsic as Intrinsic
import qualified Kitten.Mangle as Mangle
import qualified Kitten.Operator as Operator
import qualified Kitten.Pretty as Pretty
import qualified Kitten.Program as Program
import qualified Kitten.Report as Report
import qualified Kitten.Signature as Signature
import qualified Kitten.Term as Term
import qualified Kitten.Trait as Trait
import qualified Kitten.Type as Type
import qualified Kitten.TypeEnv as TypeEnv
import qualified Kitten.Unify as Unify
import qualified Kitten.Zonk as Zonk
import qualified Text.PrettyPrint as Pretty

-- Type inference takes a program fragment and produces a program with every
-- term annotated with its inferred type. It's polymorphic in the annotation
-- type of its input so that it can't depend on those annotations.

inferTypes :: Fragment a -> K (Program Type)
inferTypes fragment = do
  let tenv0 = TypeEnv.empty

-- We begin by converting instance definitions into ordinary word definitions.
-- With their mangled names already in the global vocabulary, they will be found
-- first when checking for instantiations during instance collection.

  definitions <- forM (Fragment.definitions fragment) $ \ definition -> do
    type_ <- typeFromSignature tenv0 $ Definition.signature definition
    let name = Definition.name definition
    name' <- case Definition.category definition of
      Definition.Instance -> let
        mTrait = find ((name ==) . Trait.name) $ Fragment.traits fragment
        in case mTrait of
          Nothing -> error "instance refers to a nonexistent trait"
          Just trait -> do
            instanceType <- typeFromSignature tenv0
              $ Definition.signature definition
            traitType <- typeFromSignature tenv0 $ Trait.signature trait
            instanceCheck "trait" traitType "instance" instanceType
            (traitType', args, tenv1) <- Instantiate.prenex tenv0 traitType
            tenv2 <- Unify.type_ tenv1 instanceType traitType'
            let
              args' = valueKinded $ map (Zonk.type_ tenv2) args
              mangled = Mangle.name name args'
            return $ Qualified globalVocabulary $ Unqualified mangled
      Definition.Permission -> return name
      Definition.Word -> return name
    return ((name', type_), Definition.body definition)
  traits <- forM (Fragment.traits fragment) $ \ trait -> do
    type_ <- typeFromSignature tenv0 $ Trait.signature trait
    return (Trait.name trait, type_)
  let
    declaredTypes = Map.union
      (Map.fromList (map fst definitions))
      (Map.fromList traits)
    infer = inferType0 declaredTypes

    go :: (Qualified, Type) -> Term a -> K ((Qualified, Type), Term Type)
    go (name, declaredScheme) term = do
      (term', inferredScheme) <- infer name declaredScheme term
      case find ((name ==) . Trait.name) $ Fragment.traits fragment of
        Just trait -> do
          traitScheme <- typeFromSignature tenv0 $ Trait.signature trait
          instanceCheck "generic" traitScheme "declared" declaredScheme
        Nothing -> return ()
      -- FIXME: Should this be the inferred or declared scheme?
      return ((name, inferredScheme), term')
  definitions' <- mapM (uncurry go) definitions
  return Program
    { Program.traits = HashMap.fromList traits
    , Program.definitions = HashMap.fromList definitions'
    }

-- Since type variables can be generalized if they do not depend on the initial
-- state of the typing environment, the type of a single definition is inferred
-- in an empty environment so that it can be trivially generalized. It is then
-- regeneralized to increase stack polymorphism.

inferType0
  :: Map Qualified Type -> Qualified -> Type -> Term a -> K (Term Type, Type)
inferType0 sigs _name declared term = while context (Term.origin term) $ do
  rec
    (term', t, tenvFinal) <- inferType tenvFinal'
      TypeEnv.empty { TypeEnv.sigs = sigs } term
    tenvFinal' <- Unify.type_ tenvFinal t declared
  let zonked = Zonk.type_ tenvFinal' t
  let regeneralized = regeneralize tenvFinal' zonked
  instanceCheck "inferred" regeneralized "declared" declared
  return (Zonk.term tenvFinal' term', regeneralized)
  where

  context :: Pretty.Doc
  context = Pretty.hsep ["inferring the type of", Pretty.quote term]


-- We infer the type of a term and annotate each terminal with the inferred type
-- as we go. We ignore any existing annotations because a definition may need to
-- be re-inferred and re-annotated if a program is updated with a new
-- implementation of an existing definition.

inferType :: TypeEnv -> TypeEnv -> Term a -> K (Term Type, Type, TypeEnv)
inferType tenvFinal tenv0 term0
  = while context (Term.origin term0) $ case term0 of

-- The type of the composition of two expressions is the composition of the
-- types of those expressions.

    Compose _ term1 term2 -> do
      (term1', t1, tenv1) <- inferType' tenv0 term1
      (term2', t2, tenv2) <- inferType' tenv1 term2
      (a, b, e1, tenv3) <- Unify.function tenv2 t1
      (c, d, e2, tenv4) <- Unify.function tenv3 t2
      tenv5 <- Unify.type_ tenv4 b c
      tenv6 <- Unify.type_ tenv5 e1 e2
      -- FIXME: Use range origin over whole composition?
      let origin = Term.origin term1
      let type_ = funType origin a d e1
      let type' = Zonk.type_ tenvFinal type_
      return (Compose type' term1' term2', type_, tenv6)

    Drop _ origin -> do
      [a, b, e] <- fresh origin [Stack, Value, Permission]
      let type_ = funType origin (prodType origin a b) a e
      let type' = Zonk.type_ tenvFinal type_
      return (Drop type' origin, type_, tenv0)

    Generic{} -> error
      "generic expresison should not appear during type inference"
    Group{} -> error
      "group expression should not appear during type inference"

-- The empty program is the identity function on stacks.

    Identity _ origin -> do
      [a, e] <- fresh origin [Stack, Permission]
      let type_ = funType origin a a e
      let type' = Zonk.type_ tenvFinal type_
      return (Identity type' origin, type_, tenv0)

-- A conditional expression consumes a Boolean and applies one of its two
-- branches to the remainder of the stack. Note that an 'if' without an 'else'
-- is sugar for an 'if' with an empty (i.e., identity) 'else' branch, and this
-- works out neatly in the types.

    If _ true false origin -> do
      [a, b, e] <- fresh origin [Stack, Stack, Permission]
      (true', t1, tenv1) <- while "checking true branch" origin
        $ inferType' tenv0 true
      (false', t2, tenv2) <- while "checking false branch" origin
        $ inferType' tenv1 false
      tenv3 <- Unify.type_ tenv2 t1 $ funType origin a b e
      tenv4 <- Unify.type_ tenv3 t2 $ funType origin a b e
      let
        type_ = funType origin
          (prodType origin a $ TypeConstructor origin "bool") b e
        type' = Zonk.type_ tenvFinal type_
      return (If type' true' false' origin, type_, tenv4)

    Intrinsic _ name origin -> inferIntrinsic tenvFinal tenv0 name origin

-- A local variable binding in Kitten is in fact a lambda term in the ordinary
-- lambda-calculus sense. We infer the type of its body in the

    Lambda _ name _ term origin -> do
      a <- TypeEnv.freshTv tenv0 origin Value
      let oldLocals = TypeEnv.vs tenv0
      let localEnv = tenv0 { TypeEnv.vs = a : TypeEnv.vs tenv0 }
      (term', t1, tenv1) <- inferType tenvFinal localEnv term
      let tenv2 = tenv1 { TypeEnv.vs = oldLocals }
      (b, c, e, tenv3) <- Unify.function tenv2 t1
      let
        type_ = funType origin (prodType origin b a) c e
        type' = Zonk.type_ tenvFinal type_
        varType' = Zonk.type_ tenvFinal a
      return
        ( Lambda type' name varType' term' origin
        , type_
        , tenv3
        )

    Match _ cases mElse origin -> do
      (cases', caseTypes, tenv1) <- foldrM inferCase' ([], [], tenv0) cases
      (mElse', elseType, tenv2) <- case mElse of
        Just (Else body elseOrigin) -> do
          (body', bodyType, tenv') <- inferType' tenv1 body
          return (Just (Else body' elseOrigin), bodyType, tenv')
        Nothing -> do
          [a, b, e] <- fresh origin [Stack, Stack, Permission]
          let permission = joinType origin (TypeConstructor origin "fail") e
          return (Nothing, funType origin a b permission, tenv1)
      tenv3 <- foldrM (\ type_ tenv -> Unify.type_ tenv elseType type_)
        tenv2 caseTypes
      let type_ = Type.setOrigin origin elseType
      let type' = Zonk.type_ tenvFinal type_
      return (Match type' cases' mElse' origin, type_, tenv3)

      where
      inferCase' case_ (cases', types, tenv) = do
        (case', type_, tenv') <- inferCase tenvFinal tenv case_
        return (case' : cases', type_ : types, tenv')

-- A 'new' expression simply tags some fields on the stack, so the most
-- straightforward way to type it is as an unsafe cast. For now, we can rely on
-- the type signature of the desugared data constructor definition to make this
-- type-safe, since only the compiler can generate 'new' expressions.

    New _ constructor origin -> do
      [a, b, e] <- fresh origin [Stack, Stack, Permission]
      let type_ = funType origin a b e
      let type' = Zonk.type_ tenvFinal type_
      return (New type' constructor origin, type_, tenv0)

-- Unlike with 'new', we cannot simply type a 'new closure' expression as an
-- unsafe cast because we need to know its effect on the stack within the body
-- of a definition. So we type a 'new.closure.x' expression as:
--
--     ∀ρστα̂. ρ × α₀ × … × αₓ × (σ → τ) → ρ × (σ → τ)
--

    NewClosure _ size origin -> do
      [a, b, c, d, e1, e2] <- fresh origin
        [Stack, Value, Stack, Stack, Permission, Permission]
      let
        f = funType origin c d e1
        type_ = funType origin
          (foldl' (prodType origin) a (replicate size b ++ [f]))
          (prodType origin a f)
          e2
        type' = Zonk.type_ tenvFinal type_
      return (NewClosure type' size origin, type_, tenv0)

-- This is similar for 'new vector' expressions, which we type as:
--
--     ∀ρα. ρ × α₀ × … × αₓ → ρ × vector<α>
--

    NewVector _ size origin -> do
      [a, b, e] <- fresh origin [Stack, Value, Permission]
      let
        type_ = funType origin
          (foldl' (prodType origin) a (replicate size b))
          (prodType origin a (TypeConstructor origin "vector" :@ b))
          e
        type' = Zonk.type_ tenvFinal type_
      return (NewVector type' size origin, type_, tenv0)

-- Pushing a value results in a stack with that value on top.

    Push _ value origin -> do
      [a, e] <- fresh origin [Stack, Permission]
      (value', t, tenv1) <- inferValue tenvFinal tenv0 origin value
      let type_ = funType origin a (prodType origin a t) e
      let type' = Zonk.type_ tenvFinal type_
      return (Push type' value' origin, type_, tenv1)

    Swap _ origin -> do
      [a, b, c, e] <- fresh origin [Stack, Value, Value, Permission]
      let
        type_ = funType origin
          (prodType origin (prodType origin a b) c)
          (prodType origin (prodType origin a c) b)
          e
      let type' = Zonk.type_ tenvFinal type_
      return (Swap type' origin, type_, tenv0)

    -- FIXME: Should generic parameters be restricted to none?
    Word _ _fixity name _ origin -> inferCall tenvFinal tenv0 name origin

  where
  inferType' = inferType tenvFinal
  fresh origin = foldrM (\k ts -> (: ts) <$> TypeEnv.freshTv tenv0 origin k) []

  context :: Pretty.Doc
  context = Pretty.hsep ["inferring the type of", Pretty.quote term0]

-- A case in a 'match' expression is simply the inverse of a constructor:
-- whereas a constructor takes some fields from the stack and produces
-- an instance of a data type, a 'case' deconstructs an instance of a data type
-- and produces the fields on the stack for the body of the case to consume.

inferCase :: TypeEnv -> TypeEnv -> Case a -> K (Case Type, Type, TypeEnv)
inferCase tenvFinal tenv0 (Case qualified@(QualifiedName name) body origin) = do
  (body', bodyType, tenv1) <- inferType tenvFinal tenv0 body
  (a1, b1, e1, tenv2) <- Unify.function tenv1 bodyType
  case Map.lookup name $ TypeEnv.sigs tenv2 of
    Just signature -> do
      (a2, b2, e2, tenv3) <- Unify.function tenv2 signature
      -- Note that we swap the consumption and production of the constructor
      -- to get the type of the deconstructor. The body consumes the fields.
      tenv4 <- Unify.type_ tenv3 a1 a2
      tenv5 <- Unify.type_ tenv4 e1 e2
      let type_ = funType origin b2 b1 e1
      -- FIXME: Should a case be annotated with a type?
      -- let type' = Zonk.type_ tenvFinal type_
      return (Case qualified body' origin, type_, tenv5)
    Nothing -> error
      "case constructor missing signature after name resolution"
inferCase _ _ _ = error "case of non-qualified name after name resolution"

inferValue :: TypeEnv -> TypeEnv -> Origin -> Value a -> K (Value Type, Type, TypeEnv)
inferValue tenvFinal tenv0 origin value = case value of
  Character x -> return (Character x, TypeConstructor origin "char", tenv0)
  Closed (ClosureIndex index) -> return
    (Closed $ ClosureIndex index, TypeEnv.closure tenv0 !! index, tenv0)
  Closure names term -> do
    let types = map (getClosed tenv0) names
    let oldClosure = TypeEnv.closure tenv0
    let localEnv = tenv0 { TypeEnv.closure = types }
    (term', t1, tenv1) <- inferType tenvFinal localEnv term
    let tenv2 = tenv1 { TypeEnv.closure = oldClosure }
    return (Closure names term', t1, tenv2)
  Float x -> return (Float x, TypeConstructor origin "float", tenv0)
  Integer x -> return (Integer x, TypeConstructor origin "int", tenv0)
  Local (LocalIndex index) -> return
    (Local $ LocalIndex index, TypeEnv.vs tenv0 !! index, tenv0)
  Quotation{} -> error "quotation should not appear during type inference"
  Name{} -> error "raw name should not appear during type inference"
  Text x -> return
    ( Text x
    , TypeConstructor origin "vector" :@ TypeConstructor origin "char"
    , tenv0
    )
  where

  getClosed :: TypeEnv -> Closed -> Type
  getClosed tenv name = case name of
    ClosedLocal (LocalIndex index) -> TypeEnv.vs tenv !! index
    ClosedClosure (ClosureIndex index) -> TypeEnv.closure tenv !! index

inferCall
  :: TypeEnv -> TypeEnv -> GeneralName -> Origin -> K (Term Type, Type, TypeEnv)
inferCall tenvFinal tenv0 (QualifiedName name) origin
  = case Map.lookup name $ TypeEnv.sigs tenv0 of
    Just t@Forall{} -> do
      (type_, params, tenv1) <- Instantiate.prenex tenv0 t
      let
        type' = Type.setOrigin origin type_
        params' = valueKinded params
        type'' = Zonk.type_ tenvFinal type'
        params'' = map (Zonk.type_ tenvFinal) params'
      return
        ( Word type'' Operator.Postfix (QualifiedName name) params'' origin
        , type'
        , tenv1
        )
    Just{} -> error "what is a non-quantified type doing as a type signature?"
    Nothing -> do
      report $ Report.MissingTypeSignature origin name
      halt

inferCall tenvFinal tenv0 name@(IntrinsicName intrinsic) origin
  = case intrinsic of
    Intrinsic.Add -> do
      a <- TypeEnv.freshTv tenv0 origin Stack
      e <- TypeEnv.freshTv tenv0 origin Permission
      let
        type_ = funType origin
          (prodType origin
            (prodType origin a (TypeConstructor origin "int"))
            (TypeConstructor origin "int"))
          (prodType origin a (TypeConstructor origin "int"))
          e
        type' = Zonk.type_ tenvFinal type_
      returnCall type_ type'
    Intrinsic.Magic -> do
      a <- TypeEnv.freshTv tenv0 origin Stack
      b <- TypeEnv.freshTv tenv0 origin Stack
      e <- TypeEnv.freshTv tenv0 origin Permission
      let
        type_ = funType origin a b e
        type' = Zonk.type_ tenvFinal type_
      returnCall type_ type'

  where

  -- FIXME: Generate instantiation.
  returnCall type_ type' = return
    (Word type' Operator.Postfix name [] origin, type_, tenv0)


inferCall _ _ _ _ = error "cannot infer type of non-qualified name"

inferIntrinsic
  :: TypeEnv -> TypeEnv -> Intrinsic -> Origin -> K (Term a, Type, TypeEnv)
inferIntrinsic _ _ _ _ = return $ error "TODO: infer intrinsic"

-- Here we desugar a parsed signature into an actual type. We resolve whether
-- names refer to quantified type variables or data definitions, and make stack
-- polymorphism explicit.

typeFromSignature :: TypeEnv -> Signature -> K Type
typeFromSignature tenv signature0 = do
  (type_, env) <- flip runStateT SignatureEnv
    { sigEnvAnonymous = []
    , sigEnvVars = Map.empty
    } $ go signature0
  let
    forallAnonymous = Forall (Signature.origin signature0)
    forallVar (var, origin) = Forall origin var
  return
    $ foldr forallAnonymous
      (foldr forallVar type_ $ Map.elems $ sigEnvVars env)
    $ sigEnvAnonymous env
  where

  go :: Signature -> StateT SignatureEnv K Type
  go signature = case signature of
    Signature.Application a b _ -> (:@) <$> go a <*> go b
    Signature.Function as bs es origin -> do
      r <- lift $ freshTypeId tenv
      let var = Var r Stack
      let typeVar = TypeVar origin var
      es' <- mapM (fromVar origin) es
      (me, es'') <- lift $ permissionVar origin es'
      Forall origin var <$> makeFunction origin typeVar as typeVar bs es'' me
    Signature.Quantified vars a origin -> do
      original <- get
      (envVars, vars') <- foldrM ((lift .) . declare)
        (sigEnvVars original, []) vars
      modify $ \ env -> env { sigEnvVars = envVars }
      a' <- go a
      let result = foldr (Forall origin) a' vars'
      put original
      return result
      where

      declare
        :: (Unqualified, Kind, Origin)
        -> (Map Unqualified (Var, Origin), [Var])
        -> K (Map Unqualified (Var, Origin), [Var])
      declare (name, kind, varOrigin) (envVars, freshVars) = do
        x <- freshTypeId tenv
        let var = Var x kind
        return (Map.insert name (var, varOrigin) envVars, var : freshVars)

    Signature.Variable name origin -> fromVar origin name
    Signature.StackFunction r as s bs es origin -> do
      let var = fromVar origin
      r' <- var $ UnqualifiedName r
      s' <- var $ UnqualifiedName s
      es' <- mapM var es
      (me, es'') <- lift $ permissionVar origin es'
      makeFunction origin r' as s' bs es'' me

  permissionVar :: Origin -> [Type] -> K (Maybe Type, [Type])
  permissionVar origin types = case splitFind isTypeVar types of
    Just (preceding, type_, following) -> case find isTypeVar following of
      Nothing -> return (Just type_, preceding ++ following)
      Just type' -> do
        report $ Report.MultiplePermissionVariables origin type_ type'
        halt
    Nothing -> return (Nothing, types)
    where
    isTypeVar TypeVar{} = True
    isTypeVar _ = False

  fromVar :: Origin -> GeneralName -> StateT SignatureEnv K Type
  fromVar origin (UnqualifiedName name) = do
    existing <- gets $ Map.lookup name . sigEnvVars
    case existing of
      Just (var, varOrigin) -> return $ TypeVar varOrigin var
      Nothing -> lift $ do
        report $ Report.CannotResolveType origin $ UnqualifiedName name
        halt
  fromVar origin (QualifiedName name)
    = return $ TypeConstructor origin $ Constructor name
  fromVar _ name = error
    $ "incorrectly resolved name in signature: " ++ show name

  makeFunction
    :: Origin
    -> Type -> [Signature] -> Type -> [Signature] -> [Type] -> Maybe Type
    -> StateT SignatureEnv K Type
  makeFunction origin r as s bs es me = do
    as' <- mapM go as
    bs' <- mapM go bs
    e <- case me of
      Just e -> return e
      Nothing -> do
        ex <- lift $ freshTypeId tenv
        let var = Var ex Permission
        modify $ \ env -> env { sigEnvAnonymous = var : sigEnvAnonymous env }
        return $ TypeVar origin var
    return $ funType origin (stack r as') (stack s bs')
      $ foldr (joinType origin) e es
    where

    stack :: Type -> [Type] -> Type
    stack = foldl' $ prodType origin

splitFind :: (Eq a) => (a -> Bool) -> [a] -> Maybe ([a], a, [a])
splitFind f = go []
  where
  go acc (x : xs)
    | f x = Just (reverse acc, x, xs)
    | otherwise = go (x : acc) xs
  go _ [] = Nothing

data SignatureEnv = SignatureEnv
  { sigEnvAnonymous :: [Var]
  , sigEnvVars :: !(Map Unqualified (Var, Origin))
  }

valueKinded :: [Type] -> [Type]
valueKinded = filter $ (Value ==) . typeKind

typeKind :: Type -> Kind
typeKind t = case t of
  -- TODON'T: hard-code these.
  TypeConstructor _origin constructor -> case constructor of
    Constructor (Qualified qualifier unqualified)
      | qualifier == globalVocabulary -> case unqualified of
        "int" -> Value
        "bool" -> Value
        "char" -> Value
        "float" -> Value
        "text" -> Value
        "fun" -> Stack :-> Stack :-> Permission :-> Value
        "prod" -> Stack :-> Value :-> Stack
        "vec" -> Value :-> Value
        "ptr" -> Value :-> Value
        "unsafe" -> Label
        "pure" -> Label
        "io" -> Label
        "fail" -> Label
        "join" -> Label :-> Permission :-> Permission
        _ -> error "can't infer kind of constructor"
    _ -> error "TODO: infer kinds properly"
  TypeVar _origin (Var _ k) -> k
  TypeConstant _origin (Var _ k) -> k
  Forall _origin _ t' -> typeKind t'
  a :@ _b -> let
    ka = typeKind a
    in case ka of
      _ :-> k -> k
      _ -> error "applying non-constructor to type"
