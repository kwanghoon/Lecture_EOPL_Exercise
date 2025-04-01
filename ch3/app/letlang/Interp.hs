module Interp where

import Expr
import Env

--
value_of :: Exp -> Env -> ExpVal

value_of (Const_Exp n) env =
  error "TODO: implement a value_of function"

value_of (Var_Exp var) env =
  error "TODO: implement a value_of function"

value_of (Diff_Exp exp1 exp2) env =
  error "TODO: implement a value_of function"
  
value_of (IsZero_Exp exp) env =
  error "TODO: implement a value_of function"

value_of (If_Exp exp1 exp2 exp3) env =
  error "TODO: implement a value_of function"

value_of (Let_Exp var exp1 body) env =
  error "TODO: implement a value_of function"
    

--
value_of_program :: Exp -> ExpVal

value_of_program exp = error "TODO: implement a value_of_program function"

initEnv :: Env
initEnv = extend_env "i" (Num_Val 1) 
            (extend_env "v" (Num_Val 5)
              (extend_env "x" (Num_Val 10) empty_env))

          

