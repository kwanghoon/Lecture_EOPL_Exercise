module Env where

import Expr (Identifier,Exp)

-- Environment
data Env =
    Empty_env
  | Extend_env Identifier ExpVal Env  -- let x = v in ...
  | Extend_env_rec Identifier Identifier Exp Env
  -- Add Extend_env_rec

empty_env :: Env
empty_env = Empty_env

apply_env :: Env -> Identifier -> ExpVal
apply_env Empty_env search_var = error (search_var ++ " is not found.")
apply_env (Extend_env saved_var saved_val saved_env) search_var
  | search_var==saved_var = saved_val
  | otherwise             = apply_env saved_env search_var
apply_env (Extend_env_rec p_name b_var p_body saved_env) search_var
  error "TODO: implement an apply_env function for Extend_env_rec"

extend_env :: Identifier -> ExpVal -> Env -> Env
extend_env x v env = Extend_env x v env

extend_env_rec :: Identifier -> Identifier -> Exp -> Env -> Env
extend_env_rec f x exp env = Extend_env_rec f x exp env

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

-- Procedure values : data structures (closure : (code: x & exp, env))
data Proc = Procedure {var :: Identifier, body :: Exp, saved_env :: Env}

procedure :: Identifier -> Exp -> Env -> Proc
procedure var body env = Procedure var body env

-- In Interp.hs
-- apply_procedure :: Proc -> ExpVal -> ExpVal
  
