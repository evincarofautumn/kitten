{-|
Module      : Kitten.Infer
Description : Type inference
Copyright   : (c) Jon Purdy, 2016
License     : MIT
Maintainer  : evincarofautumn@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
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
import Data.Foldable (foldlM, foldrM)
import Data.Function (on)
import Data.List (find, foldl', partition)
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Text (Text)
import Kitten.Bits
import Kitten.DataConstructor (DataConstructor)
import Kitten.Dictionary (Dictionary)
import Kitten.Entry.Parameter (Parameter(Parameter))
import Kitten.Informer (Informer(..))
import Kitten.InstanceCheck (instanceCheck)
import Kitten.Instantiated (Instantiated(Instantiated))
import Kitten.Kind (Kind(..))
import Kitten.Monad (K)
import Kitten.Name (ClosureIndex(..), GeneralName(..), LocalIndex(..), Qualified(..), Unqualified(..))
import Kitten.Origin (Origin, getOrigin)
import Kitten.Phase (Phase(..))
import Kitten.Regeneralize (regeneralize)
import Kitten.Signature (Signature)
import Kitten.Term (Sweet(..))
import Kitten.Type (Constructor(..), Type(..), Var(..))
import Kitten.TypeEnv (TypeEnv, freshTypeId)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Kitten.DataConstructor as DataConstructor
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Entry as Entry
import qualified Kitten.Entry.Parent as Parent
import qualified Kitten.Instantiate as Instantiate
import qualified Kitten.Literal as Literal
import qualified Kitten.Operator as Operator
import qualified Kitten.Origin as Origin
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

-- | Type inference takes a program fragment and produces a program with every
-- term annotated with its inferred type. It's polymorphic in the annotation
-- type of its input so that it can't depend on those annotations.

typecheck
  :: Dictionary
  -- ^ Current dictionary, for context.
  -> Maybe Signature
  -- ^ Optional signature to check inferred type against.
  -> Sweet 'Scoped
  -- ^ Term to infer.
  -> K (Sweet 'Typed, Type)
  -- ^ Type-annotated term and its inferred type.
typecheck dictionary mDeclaredSignature term = do
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


-- | Mangles an instance name according to its trait signature.

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

-- | Since type variables can be generalized if they do not depend on the
-- initial state of the typing environment, the type of a single definition is
-- inferred in an empty environment so that it can be trivially generalized. It
-- is then regeneralized to increase stack polymorphism.

inferType0
  :: Dictionary
  -- ^ Current dictionary, for context.
  -> TypeEnv
  -- ^ Current typing environment.
  -> Maybe Type
  -- ^ Optional type to check inferred type against.
  -> Sweet 'Scoped
  -- ^ Term to infer.
  -> K (Sweet 'Typed, Type)
  -- ^ Type-annotated term and its inferred type.
inferType0 dictionary tenv mDeclared term = do
    rec
      (term', t, tenvFinal) <- inferType dictionary tenvFinal' tenv term
      tenvFinal' <- maybe (return tenvFinal) (Unify.type_ tenvFinal t) mDeclared
    let zonked = Zonk.type_ tenvFinal' t
    let regeneralized = regeneralize tenvFinal' zonked
    case mDeclared of
      -- The inferred type must be at least as polymorphic as the declared type.
      Just declared -> instanceCheck
        "inferred" regeneralized "declared" declared
      Nothing -> return ()
    return (Zonk.term tenvFinal' term', regeneralized)

-- We infer the type of a term and annotate each terminal with the inferred type
-- as we go. We ignore any existing annotations because a definition may need to
-- be re-inferred and re-annotated if a program is updated with a new
-- implementation of an existing definition.

inferType
  :: Dictionary
  -> TypeEnv
  -> TypeEnv
  -> Sweet 'Scoped
  -> K (Sweet 'Typed, Type, TypeEnv)
inferType dictionary tenvFinal tenv0 term0 = case term0 of

  -- Unboxed array literals @[|a, b, c, ...|]@ have type @Array<T, N>@ where @T@
  -- is the type of the elements and @N@ is the length. See 'inferList'.

  SArray _ origin items -> while origin context $ do
    (items', itemType, tenv1)
      <- inferList dictionary tenvFinal tenv0 origin items
    [stackType, permission] <- fresh origin
      [ ("S", Stack)
      , ("P", Permission)
      ]
    let
      -- S... -> S..., Array<Item, N> +P
      type_ = Type.fun origin stackType
        (Type.prod origin stackType
          (TypeConstructor origin "Array"
            :@ itemType
            :@ TypeValue origin (length items)))
        permission
      type' = Zonk.type_ tenvFinal type_
    pure (SArray type' origin items', type_, tenv1)

  -- @as (T1, T2, ...)@ expressions are the identity function specialized to a
  -- particular stack type @<S..., +P> (S..., T1, T2, ... -> S..., T1, T2 +P)@.
  --
  -- This is used for documentation and to disambiguate trait instances in
  -- expressions such as @read as (Int32) show@. Without the @as@, the type to
  -- read would be ambiguous because it's not constrained by @show@.

  SAs _ origin signatures -> do
    types <- mapM (typeFromSignature tenv0) signatures
    [stackType, permission] <- fresh origin
      [ ("S", Stack)
      , ("P", Permission)
      ]
    let
      stack = foldl' (Type.prod origin) stackType types
      type_ = Type.fun origin stack stack permission
      type' = Zonk.type_ tenvFinal type_
    pure (SAs type' origin signatures, type_, tenv0)

  -- A character literal has type @S... -> S..., Char +P@.

  SCharacter _ origin text -> inferChar tenv0 SCharacter origin text

  -- The type of the composition of two expressions is the composition of the
  -- types of those expressions, that is, @A... -> B... +P1@ unifies with
  -- @C... -> D... +P2@ as @A -> D... +P1@ if @B...@ unifies with @C...@
  -- (equality) and @+P1@ unifies with @+P2@ (union).

  SCompose _ term1 term2 -> do
    (term1', type1, tenv1) <- inferType' tenv0 term1
    (term2', type2, tenv2) <- inferType' tenv1 term2
    (a, b, p1, tenv3) <- Unify.function tenv2 type1
    (c, d, p2, tenv4) <- Unify.function tenv3 type2
    tenv5 <- Unify.type_ tenv4 b c
    tenv6 <- Unify.type_ tenv5 p1 p2
    let
      origin = (Origin.merge `on` getOrigin) term1 term2
      type_ = Type.fun origin a d p1
      type' = Zonk.type_ tenvFinal type_
    pure (SCompose type' term1' term2', type_, tenv6)

  -- A @do@ expression is syntactic sugar for the common case where a function
  -- is called with a block or list as its topmost argument; @do (f) { x y z }@
  -- and @do (f) [x, y, z]@ are sugar for @{ x y z } f@ and @[x, y, z] f@,
  -- respectively. So we just infer it as a normal composition.

  SDo _ origin function argument -> while origin context $ do
    (SCompose type' argument' function', type_, tenv1)
      <- inferType' tenv0 $ SCompose () argument function
    pure (SDo type' origin function' argument', type_, tenv1)

  -- An "escape" expression @\x@ is syntactic sugar for a quotation with a
  -- single element, so we just infer it as a quotation.

  SEscape _ origin term -> do
    (SQuotation type' _origin term', type_, tenv1)
      <- inferType' tenv0 $ SQuotation () origin term
    pure (SQuotation type' origin term', type_, tenv1)

  -- A floating-point literal has type @S... -> S..., FloatN +P@ where @N@ is
  -- the number of bits.

  SFloat _ origin literal -> do
    let
      type_ = TypeConstructor origin $ case Literal.floatBits literal of
        Float32 -> "Float32"
        Float64 -> "Float64"
      type' = Zonk.type_ tenvFinal type_
    pure (SFloat type' origin literal, type_, tenv0)

  SGeneric{} -> error "generic terms should not appear during type inference"

  -- A group expression @(x)@ only exists to override operator precedence and
  -- improve readability. We just infer the type of the body.

  SGroup _ origin body -> do
    (body', type_, tenv1) <- inferType' tenv0 body
    let type' = Zonk.type_ tenvFinal type_
    pure (SGroup type' origin body', type_, tenv1)

  -- The empty program represents the identity function.

  SIdentity _ origin -> do
    [stack, permission] <- fresh origin
      [ ("S", Stack)
      , ("P", Permission)
      ]
    let type_ = Type.fun origin stack stack permission
    let type' = Zonk.type_ tenvFinal type_
    return (SIdentity type' origin, type_, tenv0)

  -- The type of an @if@ expression is the unified type of all the branches.

  SIf _ origin mCondition true elifs mElse -> while origin context
    $ inferIf dictionary tenvFinal tenv0 origin mCondition true elifs mElse

  SInfix _ origin left op right -> error "TODO: infer infix"

  SInteger _ origin literal -> do
    [stackType, permission] <- fresh origin
      [ ("S", Stack)
      , ("P", Permission)
      ]
    let
      ctor = TypeConstructor origin $ case Literal.integerBits literal of
        Signed8 -> "Int8"
        Signed16 -> "Int16"
        Signed32 -> "Int32"
        Signed64 -> "Int64"
        Unsigned8 -> "UInt8"
        Unsigned16 -> "UInt16"
        Unsigned32 -> "UInt32"
        Unsigned64 -> "UInt64"
      type_ = Type.fun origin stackType
        (Type.prod origin stackType ctor) permission
      type' = Zonk.type_ tenvFinal type_
    pure (SInteger type' origin literal, type_, tenv0)

  SJump _ origin -> error "TODO: infer jump"

-- A local variable binding in Kitten is in fact a lambda term in the ordinary
-- lambda-calculus sense. We infer the type of its body in the environment
-- extended with a fresh local bound to a fresh type variable, and produce a
-- type of the form 'R..., A -> S... +P'.

  SLambda _ origin vars body -> while origin context $ do

    -- Make a map from local variables to their types. We also track the types
    -- of ignored values, because we need to know their sizes in order to
    -- generate the appropriate 'drop' calls.
    varTypes <- fmap (reverse . snd) $ foldlM
      (\ (ignoreCount, types) (origin, mName, _) -> case mName of
        Just name@(Unqualified unqualified) -> do
          let typeName = Unqualified $ "Local" <> capitalize unqualified
          type_ <- TypeEnv.freshTv tenv0 typeName origin Value
          pure (ignoreCount, (name, type_) : types)
        Nothing -> do
          let name = Unqualified $ "_." <> Text.pack (show ignoreCount)
          let typeName = Unqualified $ "Ignore" <> Text.pack (show ignoreCount)
          type_ <- TypeEnv.freshTv tenv0 typeName origin Value
          pure (ignoreCount + 1, (name, type_) : types))
      (0 :: Int, mempty) vars

    -- Infer the body in the current environment extended with those locals.
    let
      oldLocals = TypeEnv.vs tenv0
      localEnv = tenv0
        { TypeEnv.vs = TypeEnv.vs tenv0 <> HashMap.fromList varTypes }
    (body', t1, tenv1) <- inferType' localEnv body
    let tenv2 = tenv1 { TypeEnv.vs = oldLocals }
    (b, c, p, tenv3) <- Unify.function tenv2 t1

    -- Annotate the variables and ignores with their types.
    let
      type_ = Type.fun origin
        (foldl' (Type.prod origin) b (map snd varTypes))
        c p
      type' = Zonk.type_ tenvFinal type_
      vars' = reverse $ snd $ foldl'
        (\ (ignoreCount, acc) (origin, mName, _) -> case mName of
          Just name -> case lookup name varTypes of
            Just varType ->
              ( ignoreCount
              , (origin, mName, Zonk.type_ tenvFinal varType) : acc
              )
            Nothing -> error "unknown local variable"
          Nothing -> let
            name = Unqualified $ "_." <> Text.pack (show ignoreCount)
            in case lookup name varTypes of
              Just ignoreType ->
                ( ignoreCount + 1
                , (origin, mName, Zonk.type_ tenvFinal ignoreType) : acc
                )
              Nothing -> error "unknown ignored local")
        (0 :: Int, []) vars

    return
      ( SLambda type' origin vars' body'
      , type_
      , tenv3
      )

  SList _ origin items -> while origin context $ do
    (items', itemType, tenv1)
      <- inferList dictionary tenvFinal tenv0 origin items
    [stackType, permission] <- fresh origin
      [ ("S", Stack)
      , ("P", Permission)
      ]
    let
      -- S... -> S..., List<Item> +P
      type_ = Type.fun origin stackType
        (Type.prod origin stackType
          (TypeConstructor origin "List" :@ itemType))
        permission
      type' = Zonk.type_ tenvFinal type_
    pure (SList type' origin items', type_, tenv1)

  SLocal _ origin name -> do
    [stackType, permission] <- fresh origin
      [ ("S", Stack)
      , ("P", Permission)
      ]
    let
      varType = case HashMap.lookup name $ TypeEnv.vs tenv0 of
        Just type_ -> type_
        Nothing -> error "unknown local variable appeared during inference"
    let
      type_ = Type.fun origin stackType
        (Type.prod origin stackType varType)
        permission
      type' = Zonk.type_ tenvFinal type_
    pure (SLocal type' origin name, type_, tenv0)

  SLoop _ origin -> error "TODO: infer loop"

  SMatch _ origin mScrutinee cases mElse -> while origin context
    $ error "TODO: infer match"

  SNestableCharacter _ origin text -> inferChar tenv0 SNestableCharacter origin text

  SNestableText _ origin text -> inferText tenv0 SNestableText origin text

  SParagraph _ origin text -> inferText tenv0 SParagraph origin text

  STag _ origin size index -> error "TODO: infer tag"

  SText _ origin text -> inferText tenv0 SText origin text

  SQuotation _ origin body -> while origin context
    $ error "TODO: infer quotation"

  SReturn _ origin -> error "TODO: infer return"

  SSection _ origin name swap operand -> error "TODO: infer section"

  STodo _ origin -> error "TODO: infer todo"

  SUnboxedQuotation _ origin body -> while origin context
    $ error "TODO: infer unboxed quotation"

  SWith _ origin permits -> error "TODO: infer with"

  SWord _ origin fixity name typeArgs -> error "TODO: infer word"

  where
    inferType' = inferType dictionary tenvFinal
    fresh origin = foldrM
      (\(name, k) ts -> (: ts) <$> TypeEnv.freshTv tenv0 name origin k)
      []

    context :: Pretty.Doc
    context = Pretty.hsep ["inferring the type of", Pretty.quote term0]

-- | Infers the type of a list or array literal from the types of its elements.
-- This ensures that each element is a term that takes no inputs and produces
-- one output, and that all the items in the list have the same type. It also
-- accounts for permissions, to enable effects when constructing a list, e.g.,
-- @[get_line, get_line]@.

inferList
  :: Dictionary
  -> TypeEnv
  -> TypeEnv
  -> Origin
  -> [Sweet 'Scoped]
  -> K ([Sweet 'Typed], Type, TypeEnv)
inferList dictionary tenvFinal tenv0 origin items = do

  -- We infer the type of each item individually from left to right.
  (itemsAndTypes, tenv1) <- foldlM inferItem ([], tenv0) items

  -- Next, we unpack the (reversed) accumulator into the annotated items and
  -- their individual types and permissions.
  let
    (items', itemTypes, itemPermissions) = let
      (typedItems, typesAndPermissions) = unzip $ reverse itemsAndTypes
      (types, permissions) = unzip typesAndPermissions
      in (typedItems, types, permissions)

  -- Finally, we unify the types and permissions of all the items, and return
  -- the annotated items, their single unified type, and the updated type
  -- environment.

  itemType <- TypeEnv.freshTv tenv1 "Item" origin Value
  tenv2 <- unifyAll tenv1 itemType itemTypes

  itemPermission <- TypeEnv.freshTv tenv0 "P" origin Permission
  tenv3 <- unifyAll tenv2 itemPermission itemPermissions

  pure (items', itemType, tenv3)

  where

    unifyAll tenv x xs = foldlM (\ tenv' -> Unify.type_ tenv' x) tenv xs

    inferItem (acc, tenv) item = do
      (item', itemTermType, tenv') <- inferType dictionary tenvFinal tenv item

      -- We ensure each item takes no inputs and produces a single output by
      -- checking it against this signature:
      --
      --   <S..., Item, +P> (S... -> S..., Item +P).
      --
      -- We can't just unify it with (S... -> S..., T +P) for fresh type variables
      -- S..., T, and +P, because that would allow an item like @(+ 1) 2@, which
      -- wouldn't change the type of the stack below the item it adds, but would
      -- change the value.
      --
      singleOutput <- typeFromSignature tenv'
        $ Signature.Quantified
          [ Parameter origin "S" Stack
          , Parameter origin "Item" Value
          ]
          (Signature.StackFunction
            (Signature.Variable (UnqualifiedName "S") origin)
            []
            (Signature.Variable (UnqualifiedName "S") origin)
            [Signature.Variable (UnqualifiedName "Item") origin]
            [UnqualifiedName "P"]
            origin) origin
      instanceCheck "item" itemTermType "single-output" singleOutput

      -- Like all terms, the item term type is a function type. Now that we've
      -- checked it against the signature, we can extract the actual item type.

      (itemStackIn, itemStackOut, itemPermission, tenv'')
        <- Unify.function tenv' itemTermType
      itemType <- TypeEnv.freshTv tenv' "Item" origin Value
      tenv'' <- Unify.type_ tenv itemStackOut
        $ Type.prod origin itemStackIn itemType

      -- We accumulate the annotated item term, its type, and its permissions.
      pure ((item', (itemType, itemPermission)) : acc, tenv')

-- Infers the type of a character literal. This accepts the constructor and
-- fields separately, rather than a partially applied constructor, because it
-- needs the origin and could eventually depend on the contents of the literal
-- to determine its type.

inferChar
  :: TypeEnv
  -> (Type -> Origin -> Text -> Sweet 'Typed)
  -> Origin
  -> Text
  -> K (Sweet 'Typed, Type, TypeEnv)
inferChar tenv0 constructor origin text = do
  stackType <- TypeEnv.freshTv tenv0 "S" origin Stack
  permission <- TypeEnv.freshTv tenv0 "P" origin Permission
  let
    type_ = Type.fun origin stackType
      (Type.prod origin stackType (TypeConstructor origin "Char"))
      permission
    type' = Zonk.type_ tenv0 type_
  pure (constructor type' origin text, type_, tenv0)

inferText
  :: TypeEnv
  -> (Type -> Origin -> Text -> Sweet 'Typed)
  -> Origin
  -> Text
  -> K (Sweet 'Typed, Type, TypeEnv)
inferText tenv0 constructor origin text = do
  stackType <- TypeEnv.freshTv tenv0 "S" origin Stack
  permission <- TypeEnv.freshTv tenv0 "P" origin Permission
  let
    type_ = Type.fun origin stackType
      (Type.prod origin stackType
        (TypeConstructor origin "List" :@ TypeConstructor origin "Char"))
      permission
    type' = Zonk.type_ tenv0 type_
  pure (constructor type' origin text, type_, tenv0)

data SimplifiedIf (p :: Phase)
  = Condition (Term.Annotation p) !(Sweet p) !(SimplifiedIf p)
  | IfElse (Term.Annotation p) !(Sweet p) !(SimplifiedIf p)
  | JustElse !(Sweet p)

-- When inferring the type of an @if@ expression, we need to account for the
-- following things:
--
--  • If the condition is specified, it must have type @Bool@.
--
--  • If the condition is not specified, the @if@ expression takes an additional
--    @Bool@ input atop the stack.
--
--  • The permissions of the @if@ condition, the true branch body, @elif@
--    conditions and bodies, and the @else@ body, must all unify.
--
--  • The conditions of @elif@ branches may take elements from the stack.
--

inferIf
  :: Dictionary
  -> TypeEnv
  -> TypeEnv
  -> Origin
  -> Maybe (Sweet 'Scoped)
  -> Sweet 'Scoped
  -> [(Origin, Sweet 'Scoped, Sweet 'Scoped)]
  -> Maybe (Sweet 'Scoped)
  -> K (Sweet 'Typed, Type, TypeEnv)
inferIf dictionary tenvFinal tenv0 origin mCondition true elifs mElse = do

  -- To infer the type of a complex if-elif-else expression, first we simplify
  -- it into a chain of nested if-else expressions, where the conditions are all
  -- drawn from the stack and the else branches are all specified. For example,
  -- this expression:
  --
  --     if (a) { b } elif (c) { d } else { e }
  --
  -- Becomes this:
  --
  --     a if { b } else { c if { d } else { e } }
  --
  let
    elseTerm = case mElse of
      Just else_ -> else_
      Nothing -> SIdentity () origin

    elifChain = foldr (\ (elifOrigin, condition, body) acc
      -> Condition () condition (IfElse () body acc))
      (JustElse elseTerm)
      elifs

    simplified = case mCondition of
      Just condition -> Condition () condition elifChain
      Nothing -> elifChain

  -- We infer the type of the simplified expression.
  (simplified', type_, tenv1)
    <- inferSimplifiedIf dictionary tenvFinal tenv0 simplified

  -- Then we convert the simplified expression back into a term.
  -- TODO: Return a term with the same structure as the original input.
  let term' = termFromSimplifiedIf simplified'

  pure (term', type_, tenv1)

-- TODO
termFromSimplifiedIf
  :: SimplifiedIf p
  -> Sweet p
termFromSimplifiedIf simplified = case simplified of

  Condition type_ condition (IfElse _ true false)
    -> SIf type_ (getOrigin condition) (Just condition) true []
      (Just (termFromSimplifiedIf false))

  -- Probably shouldn't happen, but treat it like a composition.
  Condition type_ condition other
    -> SCompose type_ condition (termFromSimplifiedIf other)

  IfElse type_ true false
    -> SIf type_ (getOrigin true) Nothing true [] (Just (termFromSimplifiedIf false))

  JustElse false
    -> false

inferSimplifiedIf
  :: Dictionary
  -> TypeEnv
  -> TypeEnv
  -> SimplifiedIf 'Scoped
  -> K (SimplifiedIf 'Typed, Type, TypeEnv)
inferSimplifiedIf dictionary tenvFinal tenv0 simplified = case simplified of

  -- A condition is inferred in the same way as a composition, except that the
  -- second term is a simplified if, not an ordinary term.
  Condition _ condition rest -> do
    (condition', conditionType, tenv1) <- inferType' tenv0 condition
    (rest', restType, tenv2) <- inferSimplifiedIf' tenv1 rest
    (a, b, p1, tenv3) <- Unify.function tenv2 conditionType
    (c, d, p2, tenv4) <- Unify.function tenv3 restType
    tenv5 <- Unify.type_ tenv4 b c
    tenv6 <- Unify.type_ tenv5 p1 p2
    let
      origin = getOrigin condition
      type_ = Type.fun origin a d p1
      type' = Zonk.type_ tenvFinal type_
    pure (Condition type' condition' rest', type_, tenv6)

  -- To infer an if-else, we infer the types of both branches and unify them,
  -- adding an extra @Bool@ input to the resulting type. If the else branch is
  -- at the end of a chain, we infer it as a term; if it's from a nested elif,
  -- we infer it as a simplified if.
  IfElse _ true false -> do
    (true', trueType, tenv1) <- inferType' tenv0 true
    (false', falseType, tenv2) <- inferSimplifiedIf' tenv1 false
    tenv3 <- Unify.type_ tenv2 trueType falseType
    (a, b, p, tenv4) <- Unify.function tenv3 trueType
    let
      origin = getOrigin true
      type_ = Type.fun origin
        (Type.prod origin a (TypeConstructor origin "Bool")) b p
      type' = Zonk.type_ tenvFinal type_
    pure (IfElse type' true' false', type_, tenv4)

  JustElse false -> do
    (false', falseType, tenv1) <- inferType' tenv0 false
    pure (JustElse false', falseType, tenv1)

  where
    inferType' = inferType dictionary tenvFinal
    inferSimplifiedIf' = inferSimplifiedIf dictionary tenvFinal

{-
 case term0 of

-- A coercion is a typed no-op.
--
-- An identity coercion is the identity function on stacks. The empty program is
-- an identity coercion.
--
-- A type coercion is the identity function specialised to a certain type. It
-- may be specialised to the identity function on stacks; to the unary identity
-- function on a particular type, in order to constrain the type of a value atop
-- the stack; or to an arbitrary type, in order to unsafely reinterpret-cast
-- between types, e.g., to grant or revoke permissions.

    Coercion hint@Term.IdentityCoercion _ origin
      -> while (getOrigin term0) context $ do
      [a, p] <- fresh origin
        [ ("S", Stack)
        , ("P", Permission)
        ]
      let type_ = Type.fun origin a a p
      let type' = Zonk.type_ tenvFinal type_
      return (Coercion hint type' origin, type_, tenv0)
    Coercion hint@(Term.AnyCoercion sig) _ origin
      -> while (getOrigin term0) context $ do
      type_ <- typeFromSignature tenv0 sig
      let type' = Zonk.type_ tenvFinal type_
      return (Coercion hint type' origin, type_, tenv0)

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
      let origin = getOrigin term1
      let type_ = Type.fun origin a d e1
      let type' = Zonk.type_ tenvFinal type_
      return (Compose type' term1' term2', type_, tenv6)

    -- TODO: Verify that this is correct.
    Generic _name _ t _ -> inferType' tenv0 t
    Group{} -> error
      "group expression should not appear during type inference"

-- A local variable binding in Kitten is in fact a lambda term in the ordinary
-- lambda-calculus sense. We infer the type of its body in the environment
-- extended with a fresh local bound to a fresh type variable, and produce a
-- type of the form 'R..., A -> S... +P'.

    Lambda _ name@(Unqualified unqualified) _ term origin -> do
      let varTypeName = Unqualified ("Local" <> capitalize unqualified)
      a <- TypeEnv.freshTv tenv0 varTypeName origin Value
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

-- A match expression consumes an instance of a data type, pushing its fields
-- onto the stack, and testing its tag to determine which case to apply, if
-- any. Note that 'if' is sugar for 'match' on Booleans. An 'if' without an
-- 'else' is sugar for a 'match' with a present-but-empty (i.e., identity,
-- modulo permissions) 'else' branch, and this works out neatly in the types. A
-- 'match' without an else branch raises 'abort', causing the 'match' to require
-- the +Fail permission.

    Match hint _ cases else_ origin -> while (getOrigin term0) context $ do
      let
        constructors = case cases of
          -- Curiously, because an empty match works on any type, no
          -- constructors are actually permitted.
          [] -> []
          Case (QualifiedName ctorName) _ _ : _
            -> case Dictionary.lookup (Instantiated ctorName []) dictionary of
              Just (Entry.Word _ _ _ (Just (Parent.Type typeName)) _ _)
                -> case Dictionary.lookup (Instantiated typeName []) dictionary of
                  Just (Entry.Type _ _ ctors) -> ctors
                  -- TODO: Check whether this can happen if a non-constructor
                  -- word is erroneously used in a case; if this is possible, we
                  -- should generate a report rather than an error.
                  _ -> error "constructor not linked to type"
              _ -> error "constructor not found after name resolution"
          _ -> error "unqualified constructor after name resolution"
      (cases', caseTypes, constructors', tenv1)
        <- foldlM inferCase' ([], [], constructors, tenv0) cases
      -- Checkpoint to halt after redundant cases are reported.
      checkpoint
      (else', elseType, tenv2) <- case else_ of
        Else body elseOrigin -> do
          (body', bodyType, tenv') <- inferType' tenv1 body
          -- The type of a match is the union of the types of the cases, and
          -- since the cases consume the scrutinee, the 'else' branch must have
          -- a dummy (fully polymorphic) type for the scrutinee. This may be
          -- easier to see when considering the type of an expression like:
          --
          --     match { else { ... } }
          --
          -- Which consumes a value of any type and always executes the 'else'
          -- branch.
          --
          -- TODO: This should be considered a drop.
          unusedScrutinee <- TypeEnv.freshTv tenv1 "MatchUnused" origin Value
          (a, b, e, tenv'') <- Unify.function tenv' bodyType
          let
            elseType = Type.fun elseOrigin
              (Type.prod elseOrigin a unusedScrutinee) b e
          return (Else body' elseOrigin, elseType, tenv'')
      (type_, tenv3) <- case constructors' of
        -- FIXME: Assumes caseTypes is non-empty.
        [] -> do
          let firstCase : remainingCases = caseTypes
          tenv' <- foldrM
            (\ type_ tenv -> Unify.type_ tenv firstCase type_)
            tenv2 remainingCases
          return (Type.setOrigin origin firstCase, tenv')
        -- Only include 'else' branch if there are unhandled cases.
        _ -> do
          tenv' <- foldrM (\ type_ tenv -> Unify.type_ tenv elseType type_)
            tenv2 caseTypes
          return (Type.setOrigin origin elseType, tenv')
      let type' = Zonk.type_ tenvFinal type_
      return (Match hint type' cases' else' origin, type_, tenv3)

      where
      inferCase' (cases', types, remaining, tenv) case_ = do
        (case', type_, remaining', tenv')
          <- inferCase dictionary tenvFinal tenv remaining case_
        return (case' : cases', type_ : types, remaining', tenv')

-- A 'new' expression simply tags some fields on the stack, so the most
-- straightforward way to type it is as an unsafe cast. For now, we can rely on
-- the type signature of the desugared data constructor definition to make this
-- type-safe, since only the compiler can generate 'new' expressions.

    New _ constructor size origin
      -> while (getOrigin term0) context $ do
      [a, b, e] <- fresh origin
        [ ("R", Stack)
        , ("S", Stack)
        , ("P", Permission)
        ]
      let type_ = Type.fun origin a b e
      let type' = Zonk.type_ tenvFinal type_
      return (New type' constructor size origin, type_, tenv0)

-- Unlike with 'new', we cannot simply type a 'new closure' expression as an
-- unsafe cast because we need to know its effect on the stack within the body
-- of a definition. So we type a 'new.closure.x' expression as:
--
--     ∀ρστα̂. ρ × α₀ × … × αₓ × (σ → τ) → ρ × (σ → τ)
--

    NewClosure _ size origin
      -> while (getOrigin term0) context $ do
      as <- fresh origin
        $ zip
          [ Unqualified ("Capture" <> Text.pack (show i))
          | i <- [1 :: Int ..]
          ]
        $ replicate size Value
      [r, s, t, p1, p2] <- fresh origin
        [ ("R", Stack)
        , ("ClosureIn", Stack)
        , ("ClosureOut", Stack)
        , ("ClosurePermissions", Permission)
        , ("P", Permission)
        ]
      let
        f = Type.fun origin s t p1
        type_ = Type.fun origin
          (foldl' (Type.prod origin) r (as ++ [f]))
          (Type.prod origin r f)
          p2
        type' = Zonk.type_ tenvFinal type_
      return (NewClosure type' size origin, type_, tenv0)

-- This is similar for 'new vector' expressions, which we type as:
--
--     ∀ρα. ρ × α₀ × … × αₓ → ρ × vector<α>
--

    NewVector _ size _ origin
      -> while (getOrigin term0) context $ do
      [a, b, e] <- fresh origin
        [ ("R", Stack)
        , ("Item", Value)
        , ("P", Permission)
        ]
      let
        type_ = Type.fun origin
          (foldl' (Type.prod origin) a (replicate size b))
          (Type.prod origin a (TypeConstructor origin "List" :@ b))
          e
        type' = Zonk.type_ tenvFinal type_
        b' = Zonk.type_ tenvFinal b
      return (NewVector type' size b' origin, type_, tenv0)

-- Pushing a value results in a stack with that value on top.

    Push _ value origin
      -> while (getOrigin term0) context $ do
      [a, e] <- fresh origin
        [ ("S", Stack)
        , ("P", Permission)
        ]
      (value', t, tenv1) <- inferValue dictionary tenvFinal tenv0 origin value
      let type_ = Type.fun origin a (Type.prod origin a t) e
      let type' = Zonk.type_ tenvFinal type_
      return (Push type' value' origin, type_, tenv1)

    -- FIXME: Should generic parameters be restricted to none?
    Word _ _fixity name _ origin
      -> while (getOrigin term0) context
      $ inferCall dictionary tenvFinal tenv0 name origin
-}

-- A case in a 'match' expression is simply the inverse of a constructor:
-- whereas a constructor takes some fields from the stack and produces
-- an instance of a data type, a 'case' deconstructs an instance of a data type
-- and produces the fields on the stack for the body of the case to consume.

{-
inferCase
  :: Dictionary
  -> TypeEnv
  -> TypeEnv
  -> [DataConstructor]
  -> Case a
  -> K (Case Type, Type, [DataConstructor], TypeEnv)
inferCase dictionary tenvFinal tenv0 dataConstructors
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
      let matching ctor = DataConstructor.name ctor == unqualifiedName name
      dataConstructors' <- case partition matching dataConstructors of
        ([], remaining) -> do
          report $ Report.RedundantCase origin
          return remaining
        (_covered, remaining) -> return remaining
      return (Case qualified body' origin, type_, dataConstructors', tenv5)
    Nothing -> error
      "case constructor missing signature after name resolution"
inferCase _ _ _ _ _ = error "case of non-qualified name after name resolution"

inferValue
  :: Dictionary
  -> TypeEnv
  -> TypeEnv
  -> Origin
  -> Value a
  -> K (Value Type, Type, TypeEnv)
inferValue dictionary tenvFinal tenv0 origin value = case value of
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
  Float x -> let
    ctor = case Literal.floatBits x of
      Float32 -> "Float32"
      Float64 -> "Float64"
    in return (Float x, TypeConstructor origin ctor, tenv0)
  Integer x -> let
    ctor = case Literal.integerBits x of
      Signed8 -> "Int8"
      Signed16 -> "Int16"
      Signed32 -> "Int32"
      Signed64 -> "Int64"
      Unsigned8 -> "UInt8"
      Unsigned16 -> "UInt16"
      Unsigned32 -> "UInt32"
      Unsigned64 -> "UInt64"
    in return (Integer x, TypeConstructor origin ctor, tenv0)
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
        mangled = QualifiedName name
        -- case params'' of
        --   [] -> name
        --   _ -> Qualified Vocabulary.global
        --     $ Unqualified $ Mangle.name name params''
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
  -- FIXME: Use proper reporting. (Internal error?)
  = error $ Pretty.render $ Pretty.hsep
    ["cannot infer type of non-qualified name", Pretty.quote name]

-}

-- | Desugars a parsed signature into an actual type. We resolve whether names
-- refer to quantified type variables or data definitions, and make stack
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
      let var = Var "R" r Stack
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
        let var = Var name x kind
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
        let var = Var "P" ex Permission
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

-- | Infers the kind of a type.

typeKind :: Dictionary -> Type -> K Kind
typeKind dictionary = go
  where
  go :: Type -> K Kind
  go t = case t of
    TypeConstructor _origin (Constructor qualified)
      -> case Dictionary.lookup (Instantiated qualified []) dictionary of
      Just (Entry.Type _origin parameters _ctors) -> case parameters of
        [] -> return Value
        _ -> return $ foldr
          ((:->) . (\ (Parameter _ _ k) -> k)) Value parameters
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
    TypeValue{} -> error "TODO: infer kind of type value"
    TypeVar _origin (Var _name _ k) -> return k
    TypeConstant _origin (Var _name _ k) -> return k
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

-- | Calculates the size of a type.

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

  apply (Type.Forall _ (Var _name typeId _) body) arg = do
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

-- | Converts a data type definition into a generic sum of products, for
-- 'typeSize' calculation.

dataType :: Origin -> [Parameter] -> [DataConstructor] -> Dictionary -> K Type
dataType origin params ctors dictionary = let
  tag = case ctors of
    [] -> unary "Void"   -- 0 -> void layout, no tag
    [_] -> unary "Void"  -- 1 -> struct layout, no tag
    _ -> unary "UInt64"  -- n -> union layout, tag
  -- Product of tag and sum of products of fields.
  sig = Signature.Quantified params
    (binary "Product" tag $ foldr
      (binary "Sum"
        . foldr (binary "Product") (unary "Unit")
        . DataConstructor.fields)
      (unary "Void")
      ctors) origin
  binary name a b = Signature.Application
    (Signature.Application (unary name) a origin) b origin
  unary name = Signature.Variable
    (QualifiedName (Qualified Vocabulary.global name)) origin
  -- FIXME: Use correct vocabulary.
  in typeFromSignature TypeEnv.empty
    =<< Resolve.run (Resolve.signature dictionary Vocabulary.global sig)

capitalize :: Text -> Text
capitalize x
  | Text.null x = x
  | otherwise
  = Text.toUpper (Text.singleton (Text.head x)) <> Text.tail x
