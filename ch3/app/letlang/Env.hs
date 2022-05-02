module Env where

import Expr

type Env = [(Identifier,ExpVal)]

empty_env :: Env
empty_env = []

apply_env :: Env -> Identifier -> ExpVal
apply_env env x =
  case [ v | (y,v) <- env, x==y ] of
    []    -> error (x ++ " is not found.")
    (v:_) -> v

extend_env :: Identifier -> ExpVal -> Env -> Env
extend_env x v env = (x,v):env


