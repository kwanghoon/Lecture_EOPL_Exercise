module Env where

import Expr

data Env = Empty_Env 
  | Extend_Env Identifier ExpVal Env
    deriving (Show)


empty_env :: Env
empty_env = Empty_Env

extend_env :: Identifier -> ExpVal -> Env -> Env
extend_env = Extend_Env

apply_env :: Env -> Identifier -> ExpVal
apply_env Empty_Env x = error (x ++ " is not found.")
apply_env (Extend_Env y v env) x
  | x == y   = v
  | otherwise = apply_env env x

data ExpVal =
    Num_Val {expval_num :: Int}
  | Bool_Val {expval_bool :: Bool}

instance Show ExpVal where
  show (Num_Val num)   = show num
  show (Bool_Val bool) = show bool

type DenVal = ExpVal   




