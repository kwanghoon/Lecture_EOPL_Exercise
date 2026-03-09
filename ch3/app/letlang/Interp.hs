{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Interp where

import Expr
import Env

--
value_of :: Exp -> Env -> ExpVal

value_of exp env = 
  error "TODO: implement a value_of function"
    

--
value_of_program :: Exp -> ExpVal

value_of_program exp = error "TODO: implement a value_of_program function"

-- [TODO] Complete initEnv.
--   { x |-> 10, v |-> 5, i |-> 1 }
initEnv :: Env
initEnv = undefined

          

