{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kitten.Program where 

import Control.Applicative
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Data.Functor.Identity
import Data.HashMap.Strict (HashMap)
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector)
import Text.Parsec.Pos

import qualified Data.Char as Char
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.ClosedName
import Kitten.Config
import Kitten.Error
import Kitten.Id
import Kitten.IdMap (DefIdMap, TypeIdMap)
import Kitten.Intrinsic
import Kitten.Kind
import Kitten.KindedId
import Kitten.Location
import Kitten.Name
import Kitten.Operator
import Kitten.Type
import Kitten.TypeDefinition
import Kitten.Util.FailWriter
import Kitten.Util.Text (ToText(..), showText)

import qualified Kitten.IdMap as I

data Program = Program
  { programBlocks :: !(DefIdMap IrBlock)
  , programDefIdGen :: !DefIdGen
  , programFixities :: !(HashMap Name Fixity)
  , programOperators :: [Operator]
  , programScalarIdGen :: !(KindedGen Scalar)
  , programStackIdGen :: !(KindedGen Stack)
  , programSymbols :: !(HashMap Name DefId)
  , programTypes :: !(HashMap Name TypeDef)

  , inferenceContext :: [Name]
  , inferenceClosure :: !(Vector (Type Scalar))
  , inferenceDefs :: !(HashMap Name TypeScheme)
  , inferenceDecls :: !(HashMap Name TypeScheme)
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
    , ", blocks: "
    , show . map fst $ I.toList programBlocks
    , " }"
    ]

emptyProgram :: Program
emptyProgram = Program
  { programBlocks = I.singleton entryId V.empty
  , programDefIdGen = mkIdGenFrom (succ entryId)
  , programFixities = H.empty
  , programOperators = []
  , programScalarIdGen = mkKindedGen
  , programStackIdGen = mkKindedGen
  , programSymbols = H.empty
  , programTypes = H.empty

  , inferenceContext = []
  , inferenceClosure = V.empty
  , inferenceDefs = H.empty
  , inferenceDecls = H.empty
  , inferenceLocals = []
  , inferenceOrigin = Origin HiNone Location
    { locationStart = initialPos "<unknown>"
    , locationIndent = 0
    }
  , inferenceScalars = I.empty
  , inferenceStacks = I.empty
  }

entryId :: DefId
entryId = Id 0

freshDefId :: Program -> (DefId, Program)
freshDefId program = let
  (i, gen') = genId (programDefIdGen program)
  in (i, program { programDefIdGen = gen' })

freshDefIdM :: K (DefId)
freshDefIdM = liftState $ state freshDefId

freshScalarId :: Program -> (KindedId Scalar, Program)
freshScalarId program = let
  (i, gen') = genKinded (programScalarIdGen program)
  in (i, program { programScalarIdGen = gen' })

freshScalarIdM :: K (KindedId Scalar)
freshScalarIdM = liftState $ state freshScalarId

freshStackId :: Program -> (KindedId Stack, Program)
freshStackId program = let
  (i, gen') = genKinded (programStackIdGen program)
  in (i, program { programStackIdGen = gen' })

freshStackIdM :: K (KindedId Stack)
freshStackIdM = liftState $ state freshStackId

freshM
  :: forall a (b :: Kind -> *)
  . (Origin -> Program -> (b a, Program))
  -> K (b a)
freshM action = do
  Origin hint loc <- getsProgram inferenceOrigin
  liftState $ state (action (Origin hint loc))

data IrInstruction
  = IrAct !IrAct
  | IrIntrinsic !Intrinsic
  | IrCall !DefId
  | IrClosure !Int
  | IrConstruct !Int !Int !(Type Scalar)
  | IrDrop !Int
  | IrEnter !Int
  | IrLabel !DefId
  | IrLeave !Int
  | IrLocal !Int
  | IrMakeVector !Int
  | IrMatch !(Vector IrCase) !(Maybe IrAct)
  | IrPush !IrValue
  | IrReturn !Int
  | IrTailApply !Int
  | IrTailCall !Int !DefId
  deriving (Eq)

type IrAct = (DefId, Vector ClosedName, Type Scalar)

data IrCase = IrCase !Int !IrAct
  deriving (Eq)

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
  deriving (Eq)

instance Show IrInstruction where
  show = T.unpack . toText

instance ToText IrInstruction where
  toText instruction = T.unwords $ case instruction of
    IrAct act -> ["act", actToText act]
    IrIntrinsic intrinsic -> ["intrinsic", toText intrinsic]
    IrCall target -> ["call", toText target]
    IrClosure index -> ["closure", showText index]
    IrConstruct name size _ -> ["construct", showText name, showText size]
    IrDrop size -> ["drop", showText size]
    IrLabel target -> ["label", toText target]
    IrEnter locals -> ["enter", showText locals]
    IrLeave locals -> ["leave", showText locals]
    IrLocal index -> ["local", showText index]
    IrMakeVector size -> ["vector", showText size]
    IrMatch cases mDefault -> (:[]) . T.unwords $ concat
      [ "match" : map toText (V.toList cases)
      , maybe [] (\target -> ["default", actToText target]) mDefault
      ]
    IrPush value -> ["push", showText value]
    IrReturn locals -> ["ret", showText locals]
    IrTailApply locals -> ["tailapply", showText locals]
    IrTailCall locals target -> ["tailcall", showText locals, showText target]

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
      -> "string" : showText (T.length value)
      : map (showText . Char.ord) (T.unpack value)

instance ToText IrCase where
  toText (IrCase index act) = T.unwords
    [showText index, actToText act]

actToText :: IrAct -> Text
actToText (target, names, _) = T.unwords
  $ showText target : map showClosedName (V.toList names)

showClosedName :: ClosedName -> Text
showClosedName (ClosedName index) = "local:" <> showText index
showClosedName (ReclosedName index) = "closure:" <> showText index

instance Show IrCase where
  show = T.unpack . toText

newtype KT m a = KT
  { unwrapKT :: FailWriterT [ErrorGroup] (ReaderT Config (StateT Program m)) a }
  deriving (Applicative, Functor, Monad, MonadFix)

instance MonadTrans KT where
  lift = KT . lift . lift . lift

type K = KT Identity

asksConfig :: (Config -> a) -> K a
asksConfig = KT . lift . asks

getProgram :: K Program
getProgram = liftState get

getsProgram :: (Program -> a) -> K a
getsProgram = liftState . gets

liftState :: (Monad m) => StateT Program m a -> KT m a
liftState = KT . lift . lift

modifyProgram :: (Program -> Program) -> K ()
modifyProgram = liftState . modify

putProgram :: Program -> K ()
putProgram = liftState . put

runKT
  :: (Monad m)
  => Program -> Config -> KT m a -> m (Either [ErrorGroup] a, Program)
runKT program config = flip runStateT program
  . flip runReaderT config . runFailWriterT null . unwrapKT

runK :: Program -> Config -> K a -> (Either [ErrorGroup] a, Program)
runK program config = runIdentity . runKT program config

liftFailWriter
  :: (Monad m)
  => FailWriterT [ErrorGroup] (ReaderT Config (StateT Program m)) a -> KT m a
liftFailWriter = KT
