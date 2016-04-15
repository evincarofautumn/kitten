{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Kitten.Infer
  ( dataType
  , inferType0
  , mangleInstance
  , typeFromSignature
  , typeKind
  , typeSize
  , typecheck
  ) where

import Control.Monad (filterM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, get, gets, modify, put, runStateT)
import Data.Either (partitionEithers)
import Data.Foldable (foldrM)
import Data.List (find, foldl')
import Data.Map (Map)
import Kitten.Bits
import Kitten.DataConstructor (DataConstructor)
import Kitten.Dictionary (Dictionary)
import Kitten.Entry.Parameter (Parameter(Parameter))
import Kitten.Informer (Informer(..))
import Kitten.InstanceCheck (instanceCheck)
import Kitten.Instantiated (Instantiated(Instantiated))
import Kitten.Kind (Kind(..))
import Kitten.Monad (K)
import Kitten.Name (Closed(..), ClosureIndex(..), GeneralName(..), LocalIndex(..), Qualified(..), Unqualified(..))
import Kitten.Origin (Origin)
import Kitten.Regeneralize (regeneralize)
import Kitten.Signature (Signature)
import Kitten.Term (Case(..), Else(..), Term(..), Value(..))
import Kitten.Type (Constructor(..), Type(..), Var(..))
import Kitten.TypeEnv (TypeEnv, freshTypeId)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.Map as Map
import qualified Kitten.DataConstructor as DataConstructor
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Entry as Entry
import qualified Kitten.Instantiate as Instantiate
import qualified Kitten.Mangle as Mangle
import qualified Kitten.Operator as Operator
import qualified Kitten.Pretty as Pretty
import qualified Kitten.Report as Report
import qualified Kitten.Resolve as Resolve
import qualified Kitten.Signature as Signature
import qualified Kitten.Substitute as Substitute
import qualified Kitten.Term as Term
import qualified Kitten.Type as Type
import qualified Kitten.TypeEnv as TypeEnv
import qualified Kitten.Unify as Unify
import qualified Kitten.Vocabulary as Vocabulary
import qualified Kitten.Zonk as Zonk
import qualified Text.PrettyPrint as Pretty

-- Type inference takes a program fragment and produces a program with every
-- term annotated with its inferred type. It's polymorphic in the annotation
-- type of its input so that it can't depend on those annotations.

typecheck
  :: Dictionary
  -> Maybe Signature
  -> Term a
  -> K (Term Type, Type)
typecheck dictionary mDeclaredSignature term = do

-- We begin by converting instance definitions into ordinary word definitions.
-- With their mangled names already in the global vocabulary, they will be found
-- first when checking for instantiations during instance collection.

{-
  definitions <- forM (Fragment.definitions fragment) $ \ definition -> do
    type_ <- typeFromSignature tenv0 $ Definition.signature definition
    let name = Definition.name definition
    name' <- case Definition.category definition of
      Category.Constructor -> return name
      Category.Instance -> let
      Category.Permission -> return name
      Category.Word -> return name
    return ((name', type_), Definition.body definition)
  traits <- forM (Fragment.traits fragment) $ \ trait -> do
    type_ <- typeFromSignature tenv0 $ Trait.signature trait
    return (Trait.name trait, type_)
-}

  let tenv0 = TypeEnv.empty
  declaredType <- traverse (typeFromSignature tenv0) mDeclaredSignature
  declaredTypes <- mapM
    (\ (name, signature) -> (,) name <$> typeFromSignature tenv0 signature)
    $ Dictionary.signatures dictionary
  let
    tenv1 = tenv0
      { TypeEnv.sigs = Map.union (Map.fromList declaredTypes)
        $ TypeEnv.sigs tenv0 }
  inferType0 dictionary tenv1 declaredType term
  
  -- declaredTypes

{-
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
-}

{-
  definitions' <- mapM (uncurry go) definitions
  return fragment
    { Fragment.definitions = HashMap.fromList definitions'
    }
-}

mangleInstance
  :: Dictionary -> Qualified -> Signature -> Signature -> K Instantiated
mangleInstance dictionary name instanceSignature traitSignature = do
  let tenv0 = TypeEnv.empty
  instanceType <- typeFromSignature tenv0 instanceSignature
  traitType <- typeFromSignature tenv0 traitSignature
  instanceCheck "trait" traitType "instance" instanceType
  (traitType', args, tenv1) <- Instantiate.prenex tenv0 traitType
  tenv2 <- Unify.type_ tenv1 instanceType traitType'
  args' <- valueKinded dictionary $ map (Zonk.type_ tenv2) args
  return $ Instantiated name args'

-- Since type variables can be generalized if they do not depend on the initial
-- state of the typing environment, the type of a single definition is inferred
-- in an empty environment so that it can be trivially generalized. It is then
-- regeneralized to increase stack polymorphism.

inferType0
  :: Dictionary
  -> TypeEnv
  -> Maybe Type
  -> Term a
  -> K (Term Type, Type)
inferType0 dictionary tenv mDeclared term
  = while (Term.origin term) context $ do
    rec
      (term', t, tenvFinal) <- inferType dictionary tenvFinal' tenv term
      tenvFinal' <- maybe (return tenvFinal) (Unify.type_ tenvFinal t) mDeclared
    let zonked = Zonk.type_ tenvFinal' t
    let regeneralized = regeneralize tenvFinal' zonked
    case mDeclared of
      Just declared -> instanceCheck
        "declared" declared "inferred" regeneralized
      Nothing -> return ()
    return (Zonk.term tenvFinal' term', regeneralized)
  where

  context :: Pretty.Doc
  context = Pretty.hsep ["inferring the type of", Pretty.quote term]


-- We infer the type of a term and annotate each terminal with the inferred type
-- as we go. We ignore any existing annotations because a definition may need to
-- be re-inferred and re-annotated if a program is updated with a new
-- implementation of an existing definition.

inferType
  :: Dictionary
  -> TypeEnv
  -> TypeEnv
  -> Term a
  -> K (Term Type, Type, TypeEnv)
inferType dictionary tenvFinal tenv0 term0
  = while (Term.origin term0) context $ case term0 of

-- The call operator denotes modus ponens: if we have some program state A, a
-- permission +E, and a closure (A → B +E) as evidence that we can convert A to
-- B given +E, then we can call the closure to produce B.

    Call _ origin -> do
      [a, b, e] <- fresh origin [Stack, Stack, Permission]
      let type_ = Type.fun origin (Type.prod origin a (Type.fun origin a b e)) b e
      let type' = Zonk.type_ tenvFinal type_
      return (Call type' origin, type_, tenv0)

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
      let type_ = Type.fun origin a d e1
      let type' = Zonk.type_ tenvFinal type_
      return (Compose type' term1' term2', type_, tenv6)

    Drop _ origin -> do
      [a, b, e] <- fresh origin [Stack, Value, Permission]
      let type_ = Type.fun origin (Type.prod origin a b) a e
      let type' = Zonk.type_ tenvFinal type_
      return (Drop type' origin, type_, tenv0)

    -- TODO: Verify that this is correct.
    Generic _ t _ -> inferType' tenv0 t
    Group{} -> error
      "group expression should not appear during type inference"

-- The empty program is the identity function on stacks.

    Identity _ origin -> do
      [a, e] <- fresh origin [Stack, Permission]
      let type_ = Type.fun origin a a e
      let type' = Zonk.type_ tenvFinal type_
      return (Identity type' origin, type_, tenv0)

-- A conditional expression consumes a Boolean and applies one of its two
-- branches to the remainder of the stack. Note that an 'if' without an 'else'
-- is sugar for an 'if' with an empty (i.e., identity) 'else' branch, and this
-- works out neatly in the types.

    If _ true false origin -> do
      [a, b, e] <- fresh origin [Stack, Stack, Permission]
      (true', t1, tenv1) <- while origin "checking true branch"
        $ inferType' tenv0 true
      (false', t2, tenv2) <- while origin "checking false branch"
        $ inferType' tenv1 false
      tenv3 <- Unify.type_ tenv2 t1 $ Type.fun origin a b e
      tenv4 <- Unify.type_ tenv3 t2 $ Type.fun origin a b e
      let
        type_ = Type.fun origin
          (Type.prod origin a $ TypeConstructor origin "Bool") b e
        type' = Zonk.type_ tenvFinal type_
      return (If type' true' false' origin, type_, tenv4)

-- A local variable binding in Kitten is in fact a lambda term in the ordinary
-- lambda-calculus sense. We infer the type of its body in the

    Lambda _ name _ term origin -> do
      a <- TypeEnv.freshTv tenv0 origin Value
      let oldLocals = TypeEnv.vs tenv0
      let localEnv = tenv0 { TypeEnv.vs = a : TypeEnv.vs tenv0 }
      (term', t1, tenv1) <- inferType' localEnv term
      let tenv2 = tenv1 { TypeEnv.vs = oldLocals }
      (b, c, e, tenv3) <- Unify.function tenv2 t1
      let
        type_ = Type.fun origin (Type.prod origin b a) c e
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
          let permission = Type.join origin (TypeConstructor origin "Fail") e
          return (Nothing, Type.fun origin a b permission, tenv1)
      tenv3 <- foldrM (\ type_ tenv -> Unify.type_ tenv elseType type_)
        tenv2 caseTypes
      let type_ = Type.setOrigin origin elseType
      let type' = Zonk.type_ tenvFinal type_
      return (Match type' cases' mElse' origin, type_, tenv3)

      where
      inferCase' case_ (cases', types, tenv) = do
        (case', type_, tenv') <- inferCase dictionary tenvFinal tenv case_
        return (case' : cases', type_ : types, tenv')

-- A 'new' expression simply tags some fields on the stack, so the most
-- straightforward way to type it is as an unsafe cast. For now, we can rely on
-- the type signature of the desugared data constructor definition to make this
-- type-safe, since only the compiler can generate 'new' expressions.

    New _ constructor size origin -> do
      [a, b, e] <- fresh origin [Stack, Stack, Permission]
      let type_ = Type.fun origin a b e
      let type' = Zonk.type_ tenvFinal type_
      return (New type' constructor size origin, type_, tenv0)

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
        f = Type.fun origin c d e1
        type_ = Type.fun origin
          (foldl' (Type.prod origin) a (replicate size b ++ [f]))
          (Type.prod origin a f)
          e2
        type' = Zonk.type_ tenvFinal type_
      return (NewClosure type' size origin, type_, tenv0)

-- This is similar for 'new vector' expressions, which we type as:
--
--     ∀ρα. ρ × α₀ × … × αₓ → ρ × vector<α>
--

    NewVector _ size _ origin -> do
      [a, b, e] <- fresh origin [Stack, Value, Permission]
      let
        type_ = Type.fun origin
          (foldl' (Type.prod origin) a (replicate size b))
          (Type.prod origin a (TypeConstructor origin "List" :@ b))
          e
        type' = Zonk.type_ tenvFinal type_
        b' = Zonk.type_ tenvFinal b
      return (NewVector type' size b' origin, type_, tenv0)

-- Pushing a value results in a stack with that value on top.

    Push _ value origin -> do
      [a, e] <- fresh origin [Stack, Permission]
      (value', t, tenv1) <- inferValue dictionary tenvFinal tenv0 origin value
      let type_ = Type.fun origin a (Type.prod origin a t) e
      let type' = Zonk.type_ tenvFinal type_
      return (Push type' value' origin, type_, tenv1)

    Swap _ origin -> do
      [a, b, c, e] <- fresh origin [Stack, Value, Value, Permission]
      let
        type_ = Type.fun origin
          (Type.prod origin (Type.prod origin a b) c)
          (Type.prod origin (Type.prod origin a c) b)
          e
      let type' = Zonk.type_ tenvFinal type_
      return (Swap type' origin, type_, tenv0)

-- The 'with' operator evaluates a closure with altered permissions. It can be
-- used safely to add permissions, enabling a restricted function to be used in
-- a more permissive context:
--
--     define call_optional<R..., S..., +E>
--       (R..., optional<(R... -> S... +E)> -> S... +fail +E):
--       match:
--         case none:
--           abort
--         case some -> f:
--           f with (-fail)
--
-- Here, 'call_optional' has the '+fail' permission, but we want to restrict 'f'
-- from gaining access to that permission, so we remove it using 'with (-fail)'.
--
-- 'with' can also be used unsafely, to grant permissions that the caller
-- doesn't necessarily have. This is typically used inside permission handlers,
-- to remove the permission when it's been successfully evaluated:
--
--     permission io<R..., S..., +E> (R..., (R... -> S... +io +E) -> S... +E):
--       with (+io)
--
-- FIXME: We should probably issue a warning when a permission is granted
-- outside its permission handler, because that's a Highly Suspicious Thing.

    With _ permits origin -> do
      [a, b, e] <- fresh origin [Stack, Stack, Permission]
      let
        (es, es') = partitionEithers $ map (\p -> let
          name = case Term.permitName p of
            QualifiedName qualified -> qualified
            _ -> error
              "unqualified name should not appear after name resolution"
          in (if Term.permitted p then Left else Right)
            $ TypeConstructor origin $ Constructor name)
          permits
      let
        type_ = Type.fun origin
          (Type.prod origin a
            (Type.fun origin a b (foldr (Type.join origin) e es)))
          b
          (foldr (Type.join origin) e es')
        type' = Zonk.type_ tenvFinal type_
      return (With type' permits origin, type_, tenv0)

    -- FIXME: Should generic parameters be restricted to none?
    Word _ _fixity name _ origin -> inferCall dictionary tenvFinal tenv0 name origin

  where
  inferType' = inferType dictionary tenvFinal
  fresh origin = foldrM (\k ts -> (: ts) <$> TypeEnv.freshTv tenv0 origin k) []

  context :: Pretty.Doc
  context = Pretty.hsep ["inferring the type of", Pretty.quote term0]

-- A case in a 'match' expression is simply the inverse of a constructor:
-- whereas a constructor takes some fields from the stack and produces
-- an instance of a data type, a 'case' deconstructs an instance of a data type
-- and produces the fields on the stack for the body of the case to consume.

inferCase
  :: Dictionary
  -> TypeEnv
  -> TypeEnv
  -> Case a
  -> K (Case Type, Type, TypeEnv)
inferCase dictionary tenvFinal tenv0
  (Case qualified@(QualifiedName name) body origin) = do
  (body', bodyType, tenv1) <- inferType dictionary tenvFinal tenv0 body
  (a1, b1, e1, tenv2) <- Unify.function tenv1 bodyType
  case Map.lookup name $ TypeEnv.sigs tenv2 of
    Just signature -> do
      (a2, b2, e2, tenv3) <- Unify.function tenv2 signature
      -- Note that we swap the consumption and production of the constructor
      -- to get the type of the deconstructor. The body consumes the fields.
      tenv4 <- Unify.type_ tenv3 a1 a2
      tenv5 <- Unify.type_ tenv4 e1 e2
      let type_ = Type.fun origin b2 b1 e1
      -- FIXME: Should a case be annotated with a type?
      -- let type' = Zonk.type_ tenvFinal type_
      return (Case qualified body' origin, type_, tenv5)
    Nothing -> error
      "case constructor missing signature after name resolution"
inferCase _ _ _ _ = error "case of non-qualified name after name resolution"

inferValue
  :: Dictionary
  -> TypeEnv
  -> TypeEnv
  -> Origin
  -> Value a
  -> K (Value Type, Type, TypeEnv)
inferValue dictionary tenvFinal tenv0 origin value = case value of
  Algebraic{} -> error "adt should not appear before runtime"
  Array{} -> error "array should not appear before runtime"
  Capture names term -> do
    let types = map (TypeEnv.getClosed tenv0) names
    let oldClosure = TypeEnv.closure tenv0
    let localEnv = tenv0 { TypeEnv.closure = types }
    (term', t1, tenv1) <- inferType dictionary tenvFinal localEnv term
    let tenv2 = tenv1 { TypeEnv.closure = oldClosure }
    return (Capture names term', t1, tenv2)
  Character x -> return (Character x, TypeConstructor origin "Char", tenv0)
  Closed (ClosureIndex index) -> return
    (Closed $ ClosureIndex index, TypeEnv.closure tenv0 !! index, tenv0)
  Closure{} -> error "closure should not appear before runtime"
  Float x bits -> let
    ctor = case bits of
      Float32 -> "Float32"
      Float64 -> "Float64"
    in return (Float x bits, TypeConstructor origin ctor, tenv0)
  Integer x bits -> let
    ctor = case bits of
      Signed8 -> "Int8"
      Signed16 -> "Int16"
      Signed32 -> "Int32"
      Signed64 -> "Int64"
      Unsigned8 -> "UInt8"
      Unsigned16 -> "UInt16"
      Unsigned32 -> "UInt32"
      Unsigned64 -> "UInt64"
    in return (Integer x bits, TypeConstructor origin ctor, tenv0)
  Local (LocalIndex index) -> return
    (Local $ LocalIndex index, TypeEnv.vs tenv0 !! index, tenv0)
  Quotation{} -> error "quotation should not appear during type inference"
  Name name -> case Dictionary.lookup (Instantiated name []) dictionary of
    Just (Entry.Word _ _ _ _ (Just signature) _) -> do
      type_ <- typeFromSignature tenv0 signature
      return (Name name, type_, tenv0)
    _ -> error $ Pretty.render $ Pretty.hsep
      [ "unbound word name"
      , Pretty.quote name
      , "found during type inference"
      ]
  Text x -> return
    ( Text x
    , TypeConstructor origin "List" :@ TypeConstructor origin "Char"
    , tenv0
    )

inferCall
  :: Dictionary
  -> TypeEnv
  -> TypeEnv
  -> GeneralName
  -> Origin
  -> K (Term Type, Type, TypeEnv)
inferCall dictionary tenvFinal tenv0 (QualifiedName name) origin
  = case Map.lookup name $ TypeEnv.sigs tenv0 of
    Just t@Forall{} -> do
      (type_, params, tenv1) <- Instantiate.prenex tenv0 t
      let
        type' = Type.setOrigin origin type_
      params' <- valueKinded dictionary params
      let
        type'' = Zonk.type_ tenvFinal type'
        params'' = map (Zonk.type_ tenvFinal) params'
      let
        mangled = QualifiedName name {- $ case params'' of
          [] -> name
          _ -> Qualified Vocabulary.global
            $ Unqualified $ Mangle.name name params'' -}
      return
        ( Word type'' Operator.Postfix mangled
          params'' origin
        , type'
        , tenv1
        )
    Just{} -> error "what is a non-quantified type doing as a type signature?"
    Nothing -> do
      report $ Report.MissingTypeSignature origin name
      halt

inferCall _dictionary _tenvFinal _tenv0 name origin
  = error $ Pretty.render $ Pretty.hsep
    ["cannot infer type of non-qualified name", Pretty.quote name]

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
    Signature.Bottom origin -> return $ Type.bottom origin
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
        :: Parameter
        -> (Map Unqualified (Var, Origin), [Var])
        -> K (Map Unqualified (Var, Origin), [Var])
      declare (Parameter varOrigin name kind) (envVars, freshVars) = do
        x <- freshTypeId tenv
        let var = Var x kind
        return (Map.insert name (var, varOrigin) envVars, var : freshVars)

    Signature.Variable name origin -> fromVar origin name
    Signature.StackFunction r as s bs es origin -> do
      let var = fromVar origin
      r' <- go r
      s' <- go s
      es' <- mapM var es
      (me, es'') <- lift $ permissionVar origin es'
      makeFunction origin r' as s' bs es'' me
    -- TODO: Verify that the type contains no free variables.
    Signature.Type type_ -> return type_

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
    return $ Type.fun origin (stack r as') (stack s bs')
      $ foldr (Type.join origin) e es
    where

    stack :: Type -> [Type] -> Type
    stack = foldl' $ Type.prod origin

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

valueKinded :: Dictionary -> [Type] -> K [Type]
valueKinded dictionary = filterM
  $ fmap (Value ==) . typeKind dictionary

typeKind :: Dictionary -> Type -> K Kind
typeKind dictionary = go
  where
  go :: Type -> K Kind
  go t = case t of
    TypeConstructor _origin (Constructor qualified)
      -> case Dictionary.lookup (Instantiated qualified []) dictionary of
      Just (Entry.Type _origin parameters _ctors) -> case parameters of
        [] -> return Value
        _ -> return $ foldr (:->) Value
          $ map (\ (Parameter _ _ k) -> k) parameters
      _ -> case qualified of
        Qualified qualifier unqualified
          | qualifier == Vocabulary.global -> case unqualified of
            "Bottom" -> return Stack
            "Fun" -> return $ Stack :-> Stack :-> Permission :-> Value
            "Prod" -> return $ Stack :-> Value :-> Stack
            "Sum" -> return $ Value :-> Value :-> Value
            "Unsafe" -> return Label
            "Void" -> return Value
            "IO" -> return Label
            "Fail" -> return Label
            "Join" -> return $ Label :-> Permission :-> Permission
            "List" -> return $ Value :-> Value
            _ -> error $ Pretty.render $ Pretty.hsep
              [ "can't infer kind of constructor"
              , Pretty.quote qualified
              , "in dictionary"
              , pPrint dictionary
              ]
        -- TODO: Better error reporting.
        _ -> error $ Pretty.render $ Pretty.hsep
          [ "can't infer kind of constructor"
          , Pretty.quote qualified
          , "in dictionary"
          , pPrint dictionary
          ]
    TypeVar _origin (Var _ k) -> return k
    TypeConstant _origin (Var _ k) -> return k
    Forall _origin _ t' -> go t'
    a :@ b -> do
      ka <- go a
      case ka of
        _ :-> k -> return k
        -- TODO: Better error reporting.
        _ -> error $ Pretty.render $ Pretty.hsep
          [ "applying type"
          , Pretty.quote a
          , "of non-constructor kind"
          , Pretty.quote ka
          , "to type"
          , Pretty.quote b
          ]

typeSize :: Dictionary -> Type -> K Type
typeSize dictionary = eval
  where
  eval :: Type -> K Type
  eval (a :@ b) = do
    a' <- eval a
    b' <- eval b
    apply a' b'
  eval (Type.TypeConstructor origin "Unit") = return $ Type.TypeValue origin 0
  eval (Type.TypeConstructor origin "Void") = return $ Type.TypeValue origin 0
  eval (Type.TypeConstructor origin "Int8") = return $ Type.TypeValue origin 1
  eval (Type.TypeConstructor origin "Int16") = return $ Type.TypeValue origin 1
  eval (Type.TypeConstructor origin "Int32") = return $ Type.TypeValue origin 1
  eval (Type.TypeConstructor origin "Int64") = return $ Type.TypeValue origin 1
  eval (Type.TypeConstructor origin "UInt8") = return $ Type.TypeValue origin 1
  eval (Type.TypeConstructor origin "UInt16") = return $ Type.TypeValue origin 1
  eval (Type.TypeConstructor origin "UInt32") = return $ Type.TypeValue origin 1
  eval (Type.TypeConstructor origin "UInt64") = return $ Type.TypeValue origin 1
  eval (Type.TypeConstructor origin "Float32") = return $ Type.TypeValue origin 1
  eval (Type.TypeConstructor origin "Float64") = return $ Type.TypeValue origin 1
  eval t@(Type.TypeConstructor _ (Type.Constructor name))
    = case Dictionary.lookup (Instantiated name []) dictionary of
      Just (Entry.Type origin params ctors) -> do
        type_ <- dataType origin params ctors dictionary
        eval type_
      _ -> return t {- error $ Pretty.render $ Pretty.hsep
        ["I could not find a type entry for", Pretty.quote name] -}
  eval t@Type.TypeValue{} = return t
  eval t@Type.TypeVar{} = return t
  eval t@Type.TypeConstant{} = return t
  eval t@Type.Forall{} = return t

  apply (Type.Forall _ (Var typeId _) body) arg = do
    body' <- Substitute.type_ TypeEnv.empty typeId arg body
    eval body'
  apply (Type.TypeConstructor origin "Sum" :@ TypeValue _ a) (TypeValue _ b)
    = return $ Type.TypeValue origin $ max a b
  apply (Type.TypeConstructor origin "Product" :@ TypeValue _ a) (TypeValue _ b)
    = return $ Type.TypeValue origin $ a + b
  apply (Type.TypeConstructor origin "Fun" :@ _ :@ _) _
    = return $ Type.TypeValue origin 1
  apply (Type.TypeConstructor origin "List") _
    = return $ Type.TypeValue origin 3 -- begin+end+capacity
  apply t arg = return $ t :@ arg {- error $ Pretty.render $ Pretty.hsep
    ["cannot apply type", pPrint t, "to argument", pPrint arg] -}

dataType :: Origin -> [Parameter] -> [DataConstructor] -> Dictionary -> K Type
dataType origin params ctors dictionary = let
  tag = case ctors of
    [] -> unary "Void"   -- 0 -> void layout, no tag
    [_] -> unary "Void"  -- 1 -> struct layout, no tag
    _ -> unary "UInt64"  -- n -> union layout, tag
  -- Product of tag and sum of products of fields.
  sig = Signature.Quantified params
    (binary "Product" tag
      $ foldr (binary "Sum") (unary "Void")
      $ map (foldr (binary "Product") (unary "Unit"))
      $ map DataConstructor.fields ctors) origin
  binary name a b = Signature.Application
    (Signature.Application (unary name) a origin) b origin
  unary name = (Signature.Variable
    (QualifiedName (Qualified Vocabulary.global name)) origin)
  -- FIXME: Use correct vocabulary.
  in typeFromSignature TypeEnv.empty
    =<< Resolve.run (Resolve.signature dictionary Vocabulary.global sig)
