{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Env where

import Expr

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

-- [TODO] Complete data ExpVal.
--   data ExpVal = 
--     ...
data ExpVal = DummyExpVal
   
instance Show ExpVal where
   show DummyExpVal   = "DummyExpVal"
--   show (Num_Val num)   = show num
--   show (Bool_Val bool) = show bool

type DenVal = ExpVal   




