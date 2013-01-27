module Kitten.Type.Substitute
  ( substChain
  , substDef
  , substFragment
  , substTerm
  , substType
  , substValue
  ) where

import qualified Data.Vector as Vector

import Kitten.Def
import Kitten.Fragment
import Kitten.Type
import Kitten.Type.Inference

-- | Simplifies a chain of transitive equality constraints.
substChain :: Env -> Type -> Type
substChain env (Var var)
  | Right type_ <- findType env var = substChain env type_
substChain _ type_ = type_

substDef :: Env -> Def Typed -> Def Typed
substDef env (Def name term) = Def name $ substTerm env term

substFragment :: Fragment Typed -> Env -> Fragment Typed
substFragment (Fragment defs term) env
  = Fragment (Vector.map (substDef env) defs) (substTerm env term)

substTerm :: Env -> Typed -> Typed
substTerm env typed = case typed of
  Value value -> Value $ substValue env value
  Builtin builtin type_ -> Builtin builtin $ substType env type_
  Scoped term type_ -> Scoped (substTerm env term) (substType env type_)
  Local index type_ -> Local index $ substType env type_
  Compose down top type_
    -> Compose (substTerm env down) (substTerm env top) (substType env type_)
  Empty type_ -> Empty $ substType env type_

substType :: Env -> Type -> Type
substType env (a :> b) = substType env a :> substType env b
substType env (a :. b) = substType env a :. substType env b
substType env (Var var)
  | Right type_ <- findType env var = substType env type_
substType _ type_ = type_

substValue :: Env -> Value -> Value
substValue env v = case v of
  Word index type_ -> Word index $ substType env type_
  Int value type_ -> Int value $ substType env type_
  Bool value type_ -> Bool value $ substType env type_
  Text value type_ -> Text value $ substType env type_
  Vec values type_ -> Vec
    (Vector.map (substValue env) values) (substType env type_)
  Tuple values type_ -> Tuple
    (Vector.map (substValue env) values) (substType env type_)
  Fun body type_ -> Fun (substTerm env body) (substType env type_)
