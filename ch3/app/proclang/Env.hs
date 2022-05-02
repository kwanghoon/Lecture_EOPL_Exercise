module Env where

import Expr (Identifier,Exp)

-- Environment
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


-- Expressed values
data ExpVal =
    Num_Val {expval_num :: Int}
  | Bool_Val {expval_bool :: Bool}
  | Proc_Val {expval_proc :: Proc}

instance Show ExpVal where
  show (Num_Val num)   = show num
  show (Bool_Val bool) = show bool
  show (Proc_Val proc) = show "<proc>"

-- Denoted values
type DenVal = ExpVal   

-- Procedure values : data structures
data Proc = Procedure {var :: Identifier, body :: Exp, saved_env :: Env}

procedure :: Identifier -> Exp -> Env -> Proc
procedure var body env = Procedure var body env

-- In Interp.hs
-- apply_procedure :: Proc -> ExpVal -> ExpVal
  
