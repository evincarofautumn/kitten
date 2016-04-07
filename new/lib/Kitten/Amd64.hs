{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Kitten.Amd64
  ( amd64
  ) where

import Control.Arrow ((&&&))
import Control.Monad (replicateM_)
import Control.Monad.Trans.State (State, gets, modify, runState, withState)
import Data.Text (Text)
import Kitten.Dictionary (Dictionary)
import Kitten.Entry (Entry)
import Kitten.Instantiated (Instantiated(Instantiated))
import Kitten.Name
import Kitten.Origin (Origin)
import Kitten.Platform (Platform(..))
import Kitten.Term (Term)
import Kitten.Type (Constructor(..), Type(..))
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Data.Text as Text
import qualified Kitten.Definition as Definition
import qualified Kitten.Dictionary as Dictionary
import qualified Kitten.Entry as Entry
import qualified Kitten.Mangle as Mangle
import qualified Kitten.Pretty as Pretty
import qualified Kitten.Term as Term
import qualified Kitten.Type as Type
import qualified Kitten.Vocabulary as Vocabulary
import qualified Text.PrettyPrint as Pretty

amd64 :: Dictionary -> Platform -> Text
amd64 dictionary platform = case platform of
  OSX -> runCodeGen $ do
    directive ".section __DATA, __data"
    line ["stack:\t.space ", dataStackSize]
    directive ".section __TEXT, __text"
    directive ".globl _main"
    label "_main"
    prologue
    initStack
    kittenCall $ Mangle.name $ Instantiated Definition.mainName []
    exitCall
    mapM_ definition $ Dictionary.toList dictionary

dataStackSize :: Text
dataStackSize = Text.pack $ show (1048576 :: Int)

kittenCall :: Text -> CodeGen ()
kittenCall name = line ["call ", name]

definition :: (Instantiated, Entry) -> CodeGen ()
definition (name, Entry.Word _category _ _ _ _ (Just body))
  | concrete body
  = do
    label $ Mangle.name name
    prologue
    (oldLocals, oldSizes) <- CodeGen $ gets $ genLocalSizes &&& genSizes
    CodeGen $ modify (\ state -> state
      { genLocalSizes = []
      , genSizes = [{- TODO -}]
      })
    term body
    CodeGen $ modify (\ state -> state
      { genLocalSizes = oldLocals
      , genSizes = oldSizes
      })
    epilogue
    line ["ret"]
  where

  concrete :: Term Type -> Bool
  concrete (Term.Generic{}) = False
  concrete _ = True

definition _ = return ()

term :: Term Type -> CodeGen ()
term t = case t of
  Term.Call{} -> comment ["call"]
  Term.Compose _ a b -> do
    term a
    term b
  Term.Drop{} -> comment ["drop"]
  Term.Generic{} -> error
    "uninstantiated term appeared during code generation"
  Term.Group a -> term a
  Term.Identity{} -> return ()
  Term.If _ a b _ -> do
    comment ["if"]
    indented $ do
      term a
      comment ["else"]
      term b
      comment ["then"]
  Term.Lambda _ _ type_ a _ -> do
    -- TODO: Use actual size class.
    let size = Large type_
    CodeGen $ modify $ \ state -> state
      { genLocalSizes = size : genLocalSizes state }
    term a
    CodeGen $ modify $ \ state -> state
      { genLocalSizes = tail $ genLocalSizes state }
  Term.Match _ _cases _mElse _ -> comment ["match"]
  Term.New _ (ConstructorIndex index) _size _
    -> comment ["new.", Text.pack $ show index]
  Term.NewClosure _ size _
    -> comment ["new.closure.", Text.pack $ show size]
  Term.NewVector _ size elemType _ -> do
    comment ["new.vec.", Text.pack $ show size]
    indented $ do

      -- FIXME: Assumes elements are small.
      comment ["flush TOS cache"]
      line ["subq $8, %rdi"]
      line ["movq %rsi, (%rdi)"]

      -- rbx = data stack pointer
      line ["movq %rdi, %rbx"]

      comment ["align system stack"]
      line ["subq $8, %rsp"]

      comment ["cache data stack"]
      line ["pushq %rdi"]

      comment ["malloc vec"]
      -- TODO: Include element size.
      let memorySize = 8 * size
      line ["movq $", Text.pack $ show memorySize, ", %rdi"]
      line ["call _malloc"]

      -- rdx = malloc pointer
      line ["movq %rax, %rdx"]
      line ["movq %rax, %rdi"]

      comment ["copy data"]
      line ["movq %rbx, %rsi"]
      line ["movq $", Text.pack $ show size, ", %rcx"]
      line ["addq $", Text.pack $ show memorySize, ", %rsi"]
      line ["subq $8, %rsi"]

      loopLabel <- freshLabel "loop"
      line ["std"]
      line ["lodsq"]
      line ["cld"]
      line ["stosq"]
      line ["loop ", loopLabel]

      comment ["restore data stack"]
      line ["popq %rdi"]

      comment ["align system stack"]
      line ["addq $8, %rsp"]

      -- Deallocate elements from stack.
      line ["addq $", Text.pack $ show memorySize, ", %rdi"]

      -- Not necessary to refill TOS because it's now large.

      -- Allocate vector on stack.
      -- No tag is necessary because vector has struct layout.
      -- 24=begin(8)+end(8)+capacity(8)
      line ["subq $24, %rdi"]
      -- begin
      line ["movq %rdx, (%rdi)"]
      line ["addq $", Text.pack $ show $ 8 * size, ", %rdx"]
      -- end
      line ["movq %rdx, 8(%rdi)"]
      -- capacity
      line ["movq %rdx, 16(%rdi)"]

      CodeGen $ modify $ \ state -> state
        { genSizes = Large elemType : drop size (genSizes state) }

  Term.Push _ v origin -> value origin v
  Term.Swap{} -> comment ["swap"]
  Term.With{} -> return ()
  Term.Word _ _ (QualifiedName name) args _
    -> kittenCall $ Mangle.name $ Instantiated name args
  Term.Word _ _ (UnqualifiedName (Unqualified name)) _ _
    -> case name of

      "_K_N06kitten10add__int16_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten10add__int32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten10add__int64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten10add__uint8_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten10div__int16_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten10div__int32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten10div__int64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten10div__uint8_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten10mod__int16_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten10mod__int32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten10mod__int64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten10mod__uint8_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten10mul__int16_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten10mul__int32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten10mul__int64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten10mul__uint8_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten10neg__int16_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten10neg__int32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten10neg__int64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten10neg__uint8_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten10show__int8_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten10sub__int16_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten10sub__int32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten10sub__int64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten10sub__uint8_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11add__uint16_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11add__uint32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11add__uint64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11div__uint16_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11div__uint32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11div__uint64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11eq__float32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11eq__float64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11ge__float32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11ge__float64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11gt__float32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11gt__float64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11le__float32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11le__float64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11lt__float32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11lt__float64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11mod__uint16_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11mod__uint32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11mod__uint64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11mul__uint16_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11mul__uint32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11mul__uint64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11ne__float32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11ne__float64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11neg__uint16_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11neg__uint32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11neg__uint64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11show__int16_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11show__int32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11show__int64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11show__uint8_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11sub__uint16_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11sub__uint32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten11sub__uint64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten12add__float32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten12add__float64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten12div__float32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten12div__float64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten12mod__float32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten12mod__float64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten12mul__float32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten12mul__float64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten12neg__float32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten12neg__float64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten12show__uint16_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten12show__uint32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten12show__uint64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten12sub__float32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten12sub__float64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten4draw_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten4head_E_I_A_L_N04RGBA_E_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten4head_E_I_I4_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten4head_E_I_N04RGBA_E_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten4tail_E_I_A_L_N04RGBA_E_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten4tail_E_I_I4_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten4tail_E_I_N04RGBA_E_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten5abort_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten5print_E" -> do
        line ["pushq %rdi"]

        line ["movq (%rdi), %rbx"]
        line ["movq 8(%rdi), %rcx"]
        line ["subq %rbx, %rcx"]
        line ["shrq $3, %rcx"]

        loopLabel <- freshLabel "loop"
        line ["movq (%rbx), %rdi"]
        line ["addq $8, %rbx"]
        -- rcx is caller-saved
        line ["pushq %rcx"]
        -- align stack
        line ["call _putchar"]
        line ["popq %rcx"]
        line ["loop ", loopLabel]

        line ["popq %rdi"]

        -- sizeof vec
        line ["addq $24, %rdi"]

        -- TODO: Free vector contents.

        CodeGen $ modify $ \ state -> state
          { genSizes = tail $ genSizes state }
        -- TODO: Repopulate TOS cache.

      "_K_N06kitten6append_E_I_A_L_N04RGBA_E_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten6append_E_I_N04RGBA_E_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten7prepend_E_I_A_L_N04RGBA_E_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten7prepend_E_I_N04RGBA_E_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten8eq__int8_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten8ge__int8_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten8gt__int8_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten8le__int8_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten8lt__int8_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten8ne__int8_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten9add__int8_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten9div__int8_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten9eq__int16_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten9eq__int32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten9eq__int64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten9ge__int16_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten9ge__int32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten9ge__int64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten9gt__int16_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten9gt__int32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten9gt__int64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten9le__int16_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten9le__int32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten9le__int64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten9lt__int16_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten9lt__int32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten9lt__int64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten9mod__int8_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten9mul__int8_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten9ne__int16_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten9ne__int32_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten9ne__int64_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten9neg__int8_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      "_K_N06kitten9sub__int8_E" -> do
        comment ["TODO: intrinsic ", Text.pack $ show name]
        debugger

      _ -> kittenCall name

  Term.Word _ _ name args _ -> error $ Pretty.render $ Pretty.hsep
    ["cannot generate call to name", Pretty.text $ show name]

value :: Origin -> Term.Value Type -> CodeGen ()
value origin v = case v of
  Term.Algebraic{} -> error
    "algebraic data type appeared during code generation"
  Term.Array{} -> error
    "array appeared during code generation"
  Term.Capture{} -> error
    "capture appeared during code generation"
  Term.Character c -> do
    comment ["push char ", Text.pack $ show c]
    indented $ pushSmall $ fromIntegral $ fromEnum c
  Term.Closed (ClosureIndex index) -> do
    comment ["closure.", Text.pack $ show index]
  Term.Closure{} -> error
    "closure appeared during code generation"
  Term.Float f _bits -> do
    comment ["push float ", Text.pack $ show f]
    -- TODO: Use FPU stack.
    indented $ pushSmall $ fromIntegral $ floor f
  Term.Integer i _bits -> do
    comment ["push int ", Text.pack $ show i]
    -- TODO: Handle different bit sizes.
    indented $ pushSmall $ fromIntegral i
  Term.Local (LocalIndex index) -> do
    localSizes <- CodeGen $ gets genLocalSizes
    let size = localSizes !! index
    comment ["local.", Text.pack $ show index]
    CodeGen $ modify $ \ state -> state
      { genSizes = size : genSizes state }
  Term.Name name -> do
    comment ["\\", Text.pack $ Pretty.render $ pPrint name]
    CodeGen $ modify $ \ state -> state
      { genSizes = Small : genSizes state }
  Term.Quotation{} -> error
    "quotation appeared during code generation"
  -- FIXME: Text should be lifted to read-only data segment.
  Term.Text text -> do
    let size = Text.length text
    comment ["push text ", Text.pack $ show text]
    indented $ do
      mapM_ (value origin . Term.Character) $ Text.unpack text
      term $ Term.NewVector
        (error "TODO: Generate typed terms.")
        size
        (TypeConstructor origin $ Constructor
          $ Qualified Vocabulary.global $ Unqualified "Char")
        origin

pushSmall :: Word -> CodeGen ()
pushSmall x = do
  comment ["push small"]
  indented $ do
    sizes <- CodeGen $ gets genSizes
    case sizes of
      -- If there is already a small value in the cache, flush it to the stack.
      Small : _ -> do
        comment ["flush TOS cache"]
        line ["subq $8, %rdi"]
        line ["movq %rsi, (%rdi)"]
      _ -> return ()
    comment ["push"]
    line ["movq $", Text.pack $ show x, ", %rsi"]
    CodeGen $ modify $ \ state -> state
      { genSizes = Small : genSizes state }

newtype CodeGen a = CodeGen (State CodeGenState a)
  deriving (Applicative, Functor, Monad)

runCodeGen :: CodeGen a -> Text
runCodeGen (CodeGen action) = let
  state0 = CodeGenState
    { genLabel = 0
    , genLocalSizes = []
    , genLogIndent = 0
    , genLogging = True
    , genSizes = []
    , genText = []
    }
  (_, state1) = runState action state0
  in Text.concat $ reverse $ genText state1

data CodeGenState = CodeGenState
  { genLabel :: !Int
  , genLocalSizes :: [SizeClass]
  , genLogIndent :: !Int
  , genLogging :: !Bool
  , genSizes :: [SizeClass]
  , genText :: [Text]
  }

data SizeClass = Small | Large !Type
  deriving (Eq)

emit :: Text -> CodeGen ()
emit text = CodeGen $ modify $ \ state -> state
  { genText = text : genText state }

line :: [Text] -> CodeGen ()
line text = emit $ Text.concat $ "\t" : text ++ ["\n"]

comment :: [Text] -> CodeGen ()
comment text = do
  (indent, logging) <- CodeGen $ gets $ genLogIndent &&& genLogging
  if logging
    then line ["// ", Text.replicate indent " ", Text.concat text]
    else return ()

indented :: CodeGen a -> CodeGen a
indented action = do
  oldIndent <- CodeGen $ gets genLogIndent
  CodeGen $ modify $ \ state -> state
    { genLogIndent = 2 + genLogIndent state }
  result <- action
  CodeGen $ modify $ \ state -> state
    { genLogIndent = oldIndent }
  return result

directive :: Text -> CodeGen ()
directive text = do
  emit text
  emit "\n"

label :: Text -> CodeGen ()
label name = do
  emit name
  emit ":\n"

freshLabel :: Text -> CodeGen Text
freshLabel name = do
  index <- CodeGen $ gets genLabel
  CodeGen $ modify $ \ state -> state
    { genLabel = succ $ genLabel state }
  let labelName = Text.concat [name, Text.pack $ show index]
  emit labelName
  emit ":\n"
  return labelName

exitCall :: CodeGen ()
exitCall = do
  comment ["exit call"]
  indented $ do
    comment ["syscall 1 with offset 0x2000000"]
    line ["movl $0x2000001, %eax"]
    comment ["exit code 0"]
    line ["movq $0, %rdi"]
    line ["syscall"]

prologue :: CodeGen ()
prologue = do
  line ["pushq %rbp"]
  line ["movq %rsp, %rbp"]

epilogue :: CodeGen ()
epilogue = line ["popq %rbp"]

initStack :: CodeGen ()
initStack = do
  line ["movq stack@GOTPCREL(%rip), %rdi"]
  line ["addq $", dataStackSize, ", %rdi"]

pop :: CodeGen ()
pop = do
  comment ["pop"]
  indented $ do
    sizes <- CodeGen $ gets genSizes
    case sizes of
      Large type_ : _ -> do
        comment ["pop large value"]
        -- TODO: sizeOf type_
        let size = 4
        line ["addq $", Text.pack $ show $ 8 * size, ", %rdi"]
      _ -> return ()
    let sizes' = tail sizes
    CodeGen $ modify $ \ state -> state { genSizes = sizes' }
    case sizes' of
      Small : _ -> do
        comment ["repopulate cache with small value"]
        line ["mov (%rdi), %rsi"]
        line ["addq $8, %rdi"]
      _ -> return ()

debugger :: CodeGen ()
debugger = line ["int $3"]
