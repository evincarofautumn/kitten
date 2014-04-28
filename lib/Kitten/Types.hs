{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Kitten.Types where

import Control.Applicative
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Foldable (Foldable)
import Data.Function
import Data.Functor.Identity
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import Data.Traversable (Traversable)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Numeric
import Text.Parsec.Pos

import qualified Data.Char as Char
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.Error
import Kitten.Id
import Kitten.IdMap (DefIdMap, TypeIdMap)
import Kitten.Location
import Kitten.Util.FailWriter
import Kitten.Util.Text (ToText(..), showText)
import Kitten.Util.Tuple

import qualified Kitten.IdMap as Id

data Program = Program
  { programBlocks :: !(DefIdMap IrBlock)
  , programDefIdGen :: !DefIdGen
  , programFixities :: !(HashMap Text Fixity)
  , programOperators :: [Operator]
  , programTypeIdGen :: !TypeIdGen
  , programSymbols :: !(HashMap Text DefId)

  , inferenceClosure :: !(Vector (Type Scalar))
  , inferenceDefs :: !(HashMap Text TypeScheme)
  , inferenceDecls :: !(HashMap Text TypeScheme)
  , inferenceLocals :: [Type Scalar]
  , inferenceOrigin :: !Origin
  , inferenceScalars :: !(TypeIdMap (Type Scalar))
  , inferenceStacks :: !(TypeIdMap (Type Stack))
  }

instance Show Program where
  show Program{..} = concat
    [ "{ symbols: "
    , show (H.keys programSymbols)
    , ", operators: "
    , show programOperators
    , " }"
    ]

data ClosedName
  = ClosedName !Int
  | ReclosedName !Int
  deriving (Eq, Show)

newtype KT m a = KT
  { unwrapKT :: FailWriterT [ErrorGroup] (StateT Program m) a }
  deriving (Applicative, Functor, Monad, MonadFix)

instance MonadTrans KT where
  lift = KT . lift . lift

type K = KT Identity

getProgram :: K Program
getProgram = liftState get

getsProgram :: (Program -> a) -> K a
getsProgram = liftState . gets

liftState :: (Monad m) => StateT Program m a -> KT m a
liftState = KT . lift

modifyProgram :: (Program -> Program) -> K ()
modifyProgram = liftState . modify

putProgram :: Program -> K ()
putProgram = liftState . put

runKT :: (Monad m) => Program -> KT m a -> m (Either [ErrorGroup] a, Program)
runKT program = flip runStateT program . runFailWriterT null . unwrapKT

runK :: Program -> K a -> (Either [ErrorGroup] a, Program)
runK program = runIdentity . runKT program

liftFailWriter
  :: (Monad m) => FailWriterT [ErrorGroup] (StateT Program m) a -> KT m a
liftFailWriter = KT

emptyProgram :: Program
emptyProgram = Program
  { programBlocks = Id.singleton entryId V.empty
  , programDefIdGen = mkIdGenFrom (succ entryId)
  , programFixities = H.empty
  , programOperators = []
  , programTypeIdGen = mkIdGen
  , programSymbols = H.empty
  , inferenceClosure = V.empty
  , inferenceDefs = H.empty
  , inferenceDecls = H.empty
  , inferenceLocals = []
  , inferenceOrigin = Origin HiNone Location
    { locationStart = initialPos "<unknown>"
    , locationIndent = 0
    }
  , inferenceScalars = Id.empty
  , inferenceStacks = Id.empty
  }

entryId :: DefId
entryId = Id 0

freshDefId :: Program -> (DefId, Program)
freshDefId program = let
  (i, gen') = genId (programDefIdGen program)
  in (i, program { programDefIdGen = gen' })

freshDefIdM :: K (DefId)
freshDefIdM = liftState $ state freshDefId

freshTypeId :: Program -> (TypeId, Program)
freshTypeId program = let
  (i, gen') = genId (programTypeIdGen program)
  in (i, program { programTypeIdGen = gen' })

freshTypeIdM :: K (TypeId)
freshTypeIdM = liftState $ state freshTypeId

freshKindedId :: Program -> (KindedId a, Program)
freshKindedId program = let
  (i, program') = freshTypeId program
  in (KindedId i, program')

freshKindedIdM :: forall (a :: Kind). K (KindedId a)
freshKindedIdM = liftState $ state freshKindedId

freshM
  :: forall a (b :: Kind -> *)
  . (Origin -> Program -> (b a, Program))
  -> K (b a)
freshM action = do
  Origin hint loc <- getsProgram inferenceOrigin
  liftState $ state (action (Origin hint loc))

--------------------------------------------------------------------------------
-- Intermediate Representation

data IrInstruction
  = IrAct !DefId !(Vector ClosedName) !(Type Scalar)
  | IrIntrinsic !Intrinsic
  | IrCall !(DefId)
  | IrClosure !Int
  | IrComment !Text
  | IrEnter
  | IrLeave
  | IrLocal !Int
  | IrMakeVector !Int
  | IrPush !IrValue
  | IrReturn

type IrBlock = Vector IrInstruction

data IrValue
  = IrBool !Bool
  | IrChar !Char
  | IrChoice !Bool !IrValue
  | IrFloat !Double
  | IrInt !Int
  | IrOption !(Maybe IrValue)
  | IrPair !IrValue !IrValue
  | IrString !Text

instance Show IrInstruction where
  show instruction = T.unpack . T.unwords $ case instruction of
    IrAct target names _
      -> "act" : showText target : map showClosedName (V.toList names)
      where
      showClosedName :: ClosedName -> Text
      showClosedName (ClosedName index) = "local:" <> showText index
      showClosedName (ReclosedName index) = "closure:" <> showText index

    IrIntrinsic intrinsic -> ["intrinsic", toText intrinsic]
    IrCall target -> ["call", showText target]
    IrClosure index -> ["closure", showText index]
    IrComment comment -> ["\n;", comment]
    IrEnter -> ["enter"]
    IrLeave -> ["leave"]
    IrLocal index -> ["local", showText index]
    IrMakeVector size -> ["vector", showText size]
    IrPush value -> ["push", showText value]
    IrReturn -> ["ret"]

instance Show IrValue where
  show = T.unpack . toText

instance ToText IrValue where
  toText = T.unwords . \case
    IrBool value -> ["bool", if value then "1" else "0"]
    IrChar value -> ["char", showText (Char.ord value)]
    IrChoice which value
      -> [if which then "right" else "left", toText value]
    IrFloat value -> ["float", showText value]
    IrInt value -> ["int", showText value]
    IrOption Nothing -> ["none"]
    IrOption (Just value) -> ["some", toText value]
    IrPair a b -> ["pair", toText a, toText b]
    IrString value
      -> "vector"
      : showText (T.length value)
      : map (\c -> "char " <> showText (Char.ord c))
        (T.unpack value)

--------------------------------------------------------------------------------
-- Intrinsics

data Intrinsic
  = InAddFloat
  | InAddInt
  | InAddVector
  | InAndBool
  | InAndInt
  | InApply
  | InCharToInt
  | InChoice
  | InChoiceElse
  | InClose
  | InDivFloat
  | InDivInt
  | InEqFloat
  | InEqInt
  | InExit
  | InFirst
  | InFromLeft
  | InFromRight
  | InFromSome
  | InGeFloat
  | InGeInt
  | InGet
  | InGetLine
  | InGtFloat
  | InGtInt
  | InIf
  | InIfElse
  | InInit
  | InIntToChar
  | InLeFloat
  | InLeInt
  | InLeft
  | InLength
  | InLtFloat
  | InLtInt
  | InModFloat
  | InModInt
  | InMulFloat
  | InMulInt
  | InNeFloat
  | InNeInt
  | InNegFloat
  | InNegInt
  | InNone
  | InNotBool
  | InNotInt
  | InOpenIn
  | InOpenOut
  | InOption
  | InOptionElse
  | InOrBool
  | InOrInt
  | InPair
  | InPrint
  | InRest
  | InRight
  | InSet
  | InShowFloat
  | InShowInt
  | InSome
  | InStderr
  | InStdin
  | InStdout
  | InSubFloat
  | InSubInt
  | InTail
  | InXorBool
  | InXorInt
  deriving (Eq, Generic, Ord)

instance Hashable Intrinsic

instance ToText Intrinsic where
  toText = fromJust . (`H.lookup` intrinsicToTextMap)

instance Show Intrinsic where
  show = T.unpack . toText

intrinsicFromText :: Text -> Maybe Intrinsic
intrinsicFromText = (`H.lookup` intrinsicFromTextMap)

intrinsicToTextMap :: HashMap Intrinsic Text
intrinsicToTextMap = H.fromList (V.toList intrinsicToTextTable)

intrinsicFromTextMap :: HashMap Text Intrinsic
intrinsicFromTextMap = H.fromList (V.toList intrinsicFromTextTable)

intrinsicNames :: Vector Text
intrinsicNames = V.map fst intrinsicFromTextTable

intrinsicFromTextTable :: Vector (Text, Intrinsic)
intrinsicFromTextTable = V.fromList
  [ (,) "__add_float"       InAddFloat
  , (,) "__add_int"         InAddInt
  , (,) "__add_vector"      InAddVector
  , (,) "__and_bool"        InAndBool
  , (,) "__and_int"         InAndInt
  , (,) "__apply"           InApply
  , (,) "__char_to_int"     InCharToInt
  , (,) "__choice"          InChoice
  , (,) "__choice_else"     InChoiceElse
  , (,) "__close"           InClose
  , (,) "__div_float"       InDivFloat
  , (,) "__div_int"         InDivInt
  , (,) "__eq_float"        InEqFloat
  , (,) "__eq_int"          InEqInt
  , (,) "__exit"            InExit
  , (,) "__first"           InFirst
  , (,) "__from_left"       InFromLeft
  , (,) "__from_right"      InFromRight
  , (,) "__from_some"       InFromSome
  , (,) "__ge_float"        InGeFloat
  , (,) "__ge_int"          InGeInt
  , (,) "__get"             InGet
  , (,) "__get_line"        InGetLine
  , (,) "__gt_float"        InGtFloat
  , (,) "__gt_int"          InGtInt
  , (,) "__if"              InIf
  , (,) "__if_else"         InIfElse
  , (,) "__init"            InInit
  , (,) "__int_to_char"     InIntToChar
  , (,) "__le_float"        InLeFloat
  , (,) "__le_int"          InLeInt
  , (,) "__left"            InLeft
  , (,) "__length"          InLength
  , (,) "__lt_float"        InLtFloat
  , (,) "__lt_int"          InLtInt
  , (,) "__mod_float"       InModFloat
  , (,) "__mod_int"         InModInt
  , (,) "__mul_float"       InMulFloat
  , (,) "__mul_int"         InMulInt
  , (,) "__ne_float"        InNeFloat
  , (,) "__ne_int"          InNeInt
  , (,) "__neg_float"       InNegFloat
  , (,) "__neg_int"         InNegInt
  , (,) "__none"            InNone
  , (,) "__not_bool"        InNotBool
  , (,) "__not_int"         InNotInt
  , (,) "__open_in"         InOpenIn
  , (,) "__open_out"        InOpenOut
  , (,) "__option"          InOption
  , (,) "__option_else"     InOptionElse
  , (,) "__or_bool"         InOrBool
  , (,) "__or_int"          InOrInt
  , (,) "__pair"            InPair
  , (,) "__print"           InPrint
  , (,) "__rest"            InRest
  , (,) "__right"           InRight
  , (,) "__set"             InSet
  , (,) "__show_float"      InShowFloat
  , (,) "__show_int"        InShowInt
  , (,) "__some"            InSome
  , (,) "__stderr"          InStderr
  , (,) "__stdin"           InStdin
  , (,) "__stdout"          InStdout
  , (,) "__sub_float"       InSubFloat
  , (,) "__sub_int"         InSubInt
  , (,) "__tail"            InTail
  , (,) "__xor_bool"        InXorBool
  , (,) "__xor_int"         InXorInt
  ]

intrinsicToTextTable :: Vector (Intrinsic, Text)
intrinsicToTextTable = V.map swap intrinsicFromTextTable

--------------------------------------------------------------------------------
-- Kinds

data Kind = Scalar | Stack

-- | A helper data type for reification of a kind type,
-- better demonstrated than explained:
--
-- > toText (KindProxy :: KindProxy a)
--
data KindProxy (a :: Kind) = KindProxy

class ReifyKind (a :: Kind) where
  reifyKind :: KindProxy a -> Kind

instance ReifyKind Stack where
  reifyKind _ = Stack

instance ReifyKind Scalar where
  reifyKind _ = Scalar

instance ToText Kind where
  toText Stack = "stack"
  toText Scalar = "scalar"

--------------------------------------------------------------------------------
-- Typing

data Type (a :: Kind) where
  (:&) :: !(Type Scalar) -> !(Type Scalar) -> Type Scalar
  (:.) :: !(Type Stack) -> !(Type Scalar) -> Type Stack
  (:?) :: !(Type Scalar) -> Type Scalar
  (:|) :: !(Type Scalar) -> !(Type Scalar) -> Type Scalar
  TyConst :: !(KindedId a) -> !Origin -> Type a
  TyCtor :: !Ctor -> !Origin -> Type Scalar
  TyEmpty :: !Origin -> Type Stack
  TyFunction :: !(Type Stack) -> !(Type Stack) -> !Origin -> Type Scalar
  TyQuantified :: !TypeScheme -> !Origin -> Type Scalar
  TyVar :: !(KindedId a) -> !Origin -> Type a
  TyVector :: !(Type Scalar) -> !Origin -> Type Scalar

data Ctor
  = CtorBool
  | CtorChar
  | CtorFloat
  | CtorHandle
  | CtorInt
  deriving (Eq)

tyBool, tyChar, tyFloat, tyHandle, tyInt :: Origin -> Type Scalar
tyBool = TyCtor CtorBool
tyChar = TyCtor CtorChar
tyFloat = TyCtor CtorFloat
tyHandle = TyCtor CtorHandle
tyInt = TyCtor CtorInt

instance ToText Ctor where
  toText = \case
    CtorBool -> "bool"
    CtorChar -> "char"
    CtorFloat -> "float"
    CtorHandle -> "handle"
    CtorInt -> "int"

instance Show Ctor where
  show = T.unpack . toText

instance Eq (Type a) where
  (a :& b) == (c :& d) = (a, b) == (c, d)
  (a :. b) == (c :. d) = (a, b) == (c, d)
  (:?) a == (:?) b = a == b
  (a :| b) == (c :| d) = (a, b) == (c, d)
  TyConst a _ == TyConst b _ = a == b
  TyCtor a _ == TyCtor b _ = a == b
  TyEmpty{} == TyEmpty{} = True
  TyFunction a b _ == TyFunction c d _ = (a, b) == (c, d)
  TyQuantified a _ == TyQuantified b _ = a == b
  TyVar a _ == TyVar b _ = a == b
  TyVector a _ == TyVector b _ = a == b
  _ == _ = False

instance Show (Type Scalar) where
  show = T.unpack . toText

instance Show (Type Stack) where
  show = T.unpack . toText

-- TODO showsPrec
instance ToText (Type Scalar) where
  toText = \case
    t1 :& t2 -> T.concat ["(", toText t1, " & ", toText t2, ")"]
    (:?) t -> toText t <> "?"
    t1 :| t2 -> T.concat ["(", toText t1, " | ", toText t2, ")"]
    TyConst (KindedId (Id i)) o
      -> "t" <> showText i <> suffix o  -- TODO Show differently?
    TyCtor name o -> toText name <> suffix o
    TyFunction r1 r2 o -> T.concat
      [ "(", T.unwords [toText r1, "->", toText r2], ")"
      , suffix o
      ]
    TyQuantified scheme _ -> toText scheme
    TyVar (KindedId (Id i)) o
      -> "t" <> showText i <> suffix o
    TyVector t o -> T.concat ["[", toText t, "]", suffix o]

instance ToText (Type Stack) where
  toText = \case
    t1 :. t2 -> T.unwords [toText t1, toText t2]
    TyConst (KindedId (Id i)) o
      -> ".s" <> showText i <> suffix o  -- TODO Show differently?
    TyEmpty o -> "<empty>" <> suffix o
    TyVar (KindedId (Id i)) o
      -> ".s" <> showText i <> suffix o

suffix :: Origin -> Text
suffix (Origin hint _) = case hint of
  HiLocal name -> " (type of " <> name <> ")"
  HiType annotated
    -> " (type of " <> toText annotated <> ")"
  HiVar _ annotated
    -> " (from " <> toText annotated <> ")"
  HiFunctionInput annotated
    -> " (input to " <> toText annotated <> ")"
  HiFunctionOutput annotated
    -> " (output of " <> toText annotated <> ")"
  HiNone -> ""

newtype KindedId (a :: Kind) = KindedId { unkinded :: TypeId }
  deriving (Eq, Ord)

instance Show (KindedId a) where
  show = T.unpack . toText

instance ToText (KindedId a) where
  toText = toText . unkinded

data Annotated
  = AnDef !Text
  | AnIntrinsic !Intrinsic

instance Show Annotated where
  show = T.unpack . toText

instance ToText Annotated where
  toText (AnDef defName) = defName
  toText (AnIntrinsic intrinsic) = toText intrinsic

data Origin = Origin
  { originHint :: !Hint
  , originLocation :: !Location
  } deriving (Show)

data Hint
  = HiLocal !Text
  -- ^ Name introduced by a 'Scoped' term.

  | HiType !Annotated
  -- ^ Explicit type annotation.

  | HiVar !Text !Annotated
  -- ^ Type variable in a type annotation.

  | HiFunctionInput !Annotated
  | HiFunctionOutput !Annotated

  | HiNone
  deriving (Show)

-- | Picks the most helpful hint.  'mappend' is commutative.
instance Monoid Hint where
  mempty = HiNone

  -- Note: patterns are ordered by preference.
  x@HiLocal{} `mappend` _ = x
  _ `mappend` x@HiLocal{} = x
  x@HiType{} `mappend` _ = x
  _ `mappend` x@HiType{} = x
  x@HiVar{} `mappend` _ = x
  _ `mappend` x@HiVar{} = x
  x@HiFunctionInput{} `mappend` _ = x
  _ `mappend` x@HiFunctionInput{} = x
  x@HiFunctionOutput{} `mappend` _ = x
  _ `mappend` x@HiFunctionOutput{} = x
  x@HiNone `mappend` _ = x

data StackHint
  = StackAny
  | Stack0
  | Stack1

-- FIXME Derived 'Eq' instance may be too restrictive.
data Scheme a = Forall
  (Set (KindedId Stack))
  (Set (KindedId Scalar))
  a
  deriving (Eq, Foldable, Functor, Traversable)

instance (ToText a) => Show (Scheme a) where
  show = T.unpack . toText

instance (ToText a) => ToText (Scheme a) where
  toText (Forall stacks scalars type_) = T.unwords
    $ (if null variables then id else (("forall" : variables ++ ["."]) ++))
    [toText type_]

    where
    variables :: [Text]
    variables = wordSetText stacks ++ wordSetText scalars

    wordSetText :: Set (KindedId a) -> [Text]
    wordSetText = map toText . S.toList

type TypeScheme = Scheme (Type Scalar)

infix 6 :&
infix 6 :|
infixl 5 :.
infix 4 -->

-- | Creates a 'Function' scalar type, inferring hints from
-- the given 'Origin'.
hintedFunction :: Type Stack -> Type Stack -> Origin -> Type Scalar
hintedFunction inputs outputs origin
  = TyFunction inputs' outputs' origin
  where
  inputs', outputs' :: Type Stack
  (inputs', outputs') = case origin of
    Origin (HiType anno) _ ->
      ( inputs `addStackHint` HiFunctionInput anno
      , outputs `addStackHint` HiFunctionOutput anno
      )
    _ -> (inputs, outputs)

(-->) :: Type Stack -> Type Stack -> Origin -> Type Scalar
(a --> b) origin = hintedFunction a b origin

addHint :: Type a -> Hint -> Type a
addHint type_ hint = case type_ of
  _ :& _ -> type_
  _ :. _ -> type_
  (:?) _ -> type_
  _ :| _ -> type_
  TyEmpty o -> TyEmpty (f o)
  TyConst i o -> TyConst i (f o)
  TyCtor ctor o -> TyCtor ctor (f o)
  TyFunction r1 r2 o -> TyFunction r1 r2 (f o)
  TyQuantified scheme o -> TyQuantified scheme o
  TyVar i o -> TyVar i (f o)
  TyVector t o -> TyVector t (f o)
  where
  f :: Origin -> Origin
  f (Origin _hint loc) = Origin hint loc

-- | Adds a 'Hint' to each 'Type Scalar' along a 'Type Stack'.
-- Shallow in scalars, deep in stacks.
--
-- TODO(strager): mapStack
addStackHint :: Type Stack -> Hint -> Type Stack
addStackHint type_ hint = case type_ of
  r :. t -> addStackHint r hint :. (t `addHint` hint)
  _ -> type_

-- | Gets the bottommost element of a stack type.
bottommost :: Type Stack -> Type Stack
bottommost type_ = case type_ of
  (a :. _) -> bottommost a
  _ -> type_

mono :: a -> Scheme a
mono = Forall S.empty S.empty

stackVar :: TypeId -> KindedId Stack
stackVar = KindedId

scalarVar :: TypeId -> KindedId Scalar
scalarVar = KindedId

unscheme :: Scheme a -> a
unscheme (Forall _ _ x) = x

--------------------------------------------------------------------------------
-- Tokens

data BlockTypeHint
  = NormalBlockHint
  | LayoutBlockHint

data BaseHint
  = BinaryHint
  | OctalHint
  | DecimalHint
  | HexadecimalHint

data Token
  = TkArrow
  | TkBlockBegin !BlockTypeHint
  | TkBlockEnd
  | TkBool Bool
  | TkChar Char
  | TkComma
  | TkDef
  | TkDo
  | TkElse
  | TkGroupBegin
  | TkGroupEnd
  | TkFloat !Double
  | TkIgnore
  | TkIntrinsic !Intrinsic
  | TkImport
  | TkInfix
  | TkInfixLeft
  | TkInfixRight
  | TkInt !Int !BaseHint
  | TkLayout
  | TkOperator !Text
  | TkSemicolon
  | TkText !Text
  | TkType
  | TkVectorBegin
  | TkVectorEnd
  | TkWord !Text

instance Eq Token where
  TkArrow        == TkArrow        = True
  -- TkBlockBegins are equal regardless of BlockTypeHint.
  TkBlockBegin{} == TkBlockBegin{} = True
  TkBlockEnd     == TkBlockEnd     = True
  TkBool a       == TkBool b       = a == b
  TkChar a       == TkChar b       = a == b
  TkComma        == TkComma        = True
  TkDef          == TkDef          = True
  TkDo           == TkDo           = True
  TkElse         == TkElse         = True
  TkGroupBegin   == TkGroupBegin   = True
  TkGroupEnd     == TkGroupEnd     = True
  TkFloat a      == TkFloat b      = a == b
  TkIgnore       == TkIgnore       = True
  TkInfix        == TkInfix        = True
  TkInfixLeft    == TkInfixLeft    = True
  TkInfixRight   == TkInfixRight   = True
  TkIntrinsic a  == TkIntrinsic b  = a == b
  TkImport       == TkImport       = True
  -- TkInts are equal regardless of BaseHint.
  TkInt a _      == TkInt b _      = a == b
  TkLayout       == TkLayout       = True
  TkOperator a   == TkOperator b   = a == b
  TkSemicolon    == TkSemicolon    = True
  TkText a       == TkText b       = a == b
  TkType         == TkType         = True
  TkVectorBegin  == TkVectorBegin  = True
  TkVectorEnd    == TkVectorEnd    = True
  TkWord a       == TkWord b       = a == b
  _              == _              = False

instance Show Token where
  show = \case
    TkArrow -> "->"
    TkBlockBegin NormalBlockHint -> "{"
    TkBlockBegin LayoutBlockHint -> ":"
    TkBlockEnd -> "}"
    TkBool value -> if value then "true" else "false"
    TkChar value -> show value
    TkComma -> ","
    TkDef -> "def"
    TkDo -> "\\"
    TkElse -> "else"
    TkIgnore -> "_"
    TkInfix -> "infix"
    TkInfixLeft -> "infix_left"
    TkInfixRight -> "infix_right"
    TkIntrinsic name -> T.unpack (toText name)
    TkGroupBegin -> "("
    TkGroupEnd -> ")"
    TkFloat value -> show value
    TkImport -> "import"
    TkInt value hint -> if value < 0 then '-' : shown else shown
      where
      shown = prefix ++ showIntAtBase base (digits !!) (abs value) ""
      (base, prefix, digits) = case hint of
        BinaryHint -> (2, "0b", "01")
        OctalHint -> (8, "0o", ['0'..'7'])
        DecimalHint -> (10, "", ['0'..'9'])
        HexadecimalHint -> (16, "0x", ['0'..'9'] ++ ['A'..'F'])
    TkLayout -> ":"
    TkOperator word -> T.unpack word
    TkSemicolon -> ";"
    TkText value -> show value
    TkType -> "type"
    TkVectorBegin -> "["
    TkVectorEnd -> "]"
    TkWord word -> T.unpack word

data Located = Located
  { locatedToken :: Token
  , locatedLocation :: Location
  }

instance Show Located where
  show Located{..} = show locatedToken

--------------------------------------------------------------------------------
-- Definitions

data Def a = Def
  { defAnno :: !Anno
  , defFixity :: !Fixity
  , defLocation :: !Location
  , defName :: !Text
  , defTerm :: !(Scheme a)
  } deriving (Eq, Foldable, Functor, Show, Traversable)

--------------------------------------------------------------------------------
-- Program Fragments

data Fragment a = Fragment
  { fragmentDefs :: !(HashMap Text (Def a))
  , fragmentImports :: [Import]
  , fragmentOperators :: [Operator]
  , fragmentTerm :: !a
  } deriving (Eq, Show)

termFragment :: a -> Fragment a
termFragment term = Fragment
  { fragmentDefs = H.empty
  , fragmentImports = []
  , fragmentOperators = []
  , fragmentTerm = term
  }

--------------------------------------------------------------------------------
-- Type Annotations

data Anno = Anno AnType Location | TestAnno
  deriving (Show)

instance Eq Anno where
  TestAnno == _ = True
  _ == TestAnno = True
  Anno type1 loc1 == Anno type2 loc2 = (type1, loc1) == (type2, loc2)

data AnType
  = AnFunction !(Vector AnType) !(Vector AnType)
  | AnChoice !AnType !AnType
  | AnOption !AnType
  | AnPair !AnType !AnType
  | AnQuantified !(Vector Text) !(Vector Text) !AnType
  | AnStackFunction !Text !(Vector AnType) !Text !(Vector AnType)
  | AnVar !Text
  | AnVector !AnType
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Imports

data Import = Import
  { importName :: Text
  , importLocation :: Location
  } deriving (Show)

instance Eq Import where
  (==) = (==) `on` importName

--------------------------------------------------------------------------------
-- Operators

-- | The associativity of an infix operator.
data Associativity
  = NonAssociative
  | LeftAssociative
  | RightAssociative
  deriving (Eq, Show)

-- | Whether a call is to a word that was originally postfix or infix.
data Fixity = Infix | Postfix
  deriving (Eq)

instance ToText Fixity where
  toText Infix = "infix"
  toText Postfix = "postfix"

instance Show Fixity where
  show = T.unpack . toText

data Operator = Operator
  { operatorAssociativity :: !Associativity
  , operatorPrecedence :: !Precedence
  , operatorName :: !Text
  } deriving (Eq)

instance Show Operator where
  show (Operator fixity precedence name) = unwords
    [ case fixity of
      NonAssociative -> "infix"
      LeftAssociative -> "infix_left"
      RightAssociative -> "infix_right"
    , show precedence
    , T.unpack name
    ]

type Precedence = Int

--------------------------------------------------------------------------------
-- Syntax Tree

data TrTerm a
  = TrIntrinsic !Intrinsic !a
  | TrCall !Fixity !Text !a
  | TrCompose !StackHint !(Vector (TrTerm a)) !a
  | TrLambda !Text !(TrTerm a) !a
  | TrMakePair !(TrTerm a) !(TrTerm a) !a
  | TrPush !(TrValue a) !a
  | TrMakeVector !(Vector (TrTerm a)) !a

instance (Eq a) => Eq (TrTerm a) where
  -- Calls are equal regardless of 'Fixity'.
  TrCall _ a b == TrCall _ c d = (a, b) == (c, d)
  -- Minor hack to make 'Compose's less in the way.

  TrCompose _ a _ == b | V.length a == 1 = V.head a == b
  a == TrCompose _ b _ | V.length b == 1 = a == V.head b
  -- 'Compose's are equal regardless of 'StackHint'.
  TrCompose _ a b == TrCompose _ c d = (a, b) == (c, d)
  TrIntrinsic a b == TrIntrinsic c d = (a, b) == (c, d)
  TrLambda a b c == TrLambda d e f = (a, b, c) == (d, e, f)
  TrMakePair a b c == TrMakePair d e f = (a, b, c) == (d, e, f)
  TrPush a b == TrPush c d = (a, b) == (c, d)
  TrMakeVector a b == TrMakeVector c d = (a, b) == (c, d)
  _ == _ = False

instance ToText (TrTerm a) where
  toText = \case
    TrCall _ label _ -> label
    TrCompose _ terms _ -> T.concat
      ["(", T.intercalate " " (V.toList (V.map toText terms)), ")"]
    TrIntrinsic intrinsic _ -> toText intrinsic
    TrLambda name term _ -> T.concat ["(\\", name, " ", toText term, ")"]
    TrMakePair a b _ -> T.concat ["(", toText a, ", ", toText b, ")"]
    TrPush value _ -> toText value
    TrMakeVector terms _ -> T.concat
      ["[", T.intercalate ", " (V.toList (V.map toText terms)), "]"]

instance Show (TrTerm a) where
  show = T.unpack . toText

data TrValue a
  = TrBool !Bool !a
  | TrChar !Char !a
  | TrClosed !Int !a  -- resolved
  | TrClosure !(Vector ClosedName) !(TrTerm a) !a  -- resolved
  | TrFloat !Double !a
  | TrInt !Int !a
  | TrLocal !Int !a  -- resolved
  | TrQuotation !(TrTerm a) !a
  | TrText !Text !a
  deriving (Eq)

instance ToText (TrValue a) where
  toText = \case
    TrBool value _ -> if value then "true" else "false"
    TrChar value _ -> showText value
    TrClosed index _ -> "closure" <> showText index
    TrClosure{} -> "<closure>"
    TrFloat value _ -> showText value
    TrInt value _ -> showText value
    TrLocal index _ -> "local" <> showText index
    TrQuotation term _ -> T.concat ["{", toText term, "}"]
    TrText value _ -> toText value

instance Show (TrValue a) where
  show = T.unpack . toText

type ParsedTerm = TrTerm Location
type ParsedValue = TrValue Location

type ResolvedTerm = TrTerm Location
type ResolvedValue = TrValue Location

type TypedTerm = TrTerm (Location, Type Scalar)
type TypedValue = TrValue (Location, Type Scalar)

defTypeScheme :: Def TypedTerm -> TypeScheme
defTypeScheme def = type_ <$ defTerm def
  where
  type_ = typedType $ unscheme (defTerm def)

termMetadata :: TrTerm a -> a
termMetadata = \case
  TrCall _ _ x -> x
  TrCompose _ _ x -> x
  TrIntrinsic _ x -> x
  TrLambda _ _ x -> x
  TrMakePair _ _ x -> x
  TrPush _ x -> x
  TrMakeVector _ x -> x

typedLocation :: TypedTerm -> Location
typedLocation = fst . termMetadata

typedType :: TypedTerm -> Type Scalar
typedType = snd . termMetadata
