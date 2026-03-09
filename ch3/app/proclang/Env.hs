{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}
module Env where

import Expr (Identifier,Exp)

-- Environment
-- [TODO] Complete data Env.
--   data Env = 
--     ...
data Env = DummyEnv
  deriving (Show)


empty_env :: Env
empty_env = error "TODO: implement empty_env function"

extend_env :: Identifier -> ExpVal -> Env -> Env
extend_env x v env = error "TODO: implement extend_env function"

apply_env :: Env -> Identifier -> ExpVal
apply_env env x = error "TODO: implement apply_env function"


-- Expressed values
-- [TODO] Complete data ExpVal.
--   data ExpVal = 
--     ...
data ExpVal = DummyExpVal
   
instance Show ExpVal where
   show DummyExpVal   = "DummyExpVal"
--   show (Num_Val num)   = show num
--   show (Bool_Val bool) = show bool
--   show (Proc_Val proc) = show "<proc>"

-- Denoted values
type DenVal = ExpVal   

-- Procedure values : data structures
data Proc = Procedure {var :: Identifier, body :: Exp, saved_env :: Env}

-- procedure :: Identifier -> Exp -> Env -> Proc
-- procedure var body env = Procedure var body env

-- In Interp.hs
-- apply_procedure :: Proc -> ExpVal -> ExpVal
  
