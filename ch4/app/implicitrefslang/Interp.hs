module Interp where

import Expr
import EnvStore

--
value_of :: Exp -> Env -> Store -> (ExpVal, Store)   -- Sec 4.2.1: Store-passing specifications

value_of (Const_Exp n) env store = 
  error "TODO: implement a value_of function"

value_of (Var_Exp var) env store = 
  error "TODO: implement a value_of function"

value_of (Diff_Exp exp1 exp2) env store =
  error "TODO: implement a value_of function"
  
value_of (IsZero_Exp exp) env store =
  error "TODO: implement a value_of function"

value_of (If_Exp exp1 exp2 exp3) env store =
  error "TODO: implement a value_of function"

value_of (Let_Exp var exp1 body) env store =
  error "TODO: implement a value_of function"

value_of (Letrec_Exp letbindings letrec_body) env store =
  error "TODO: implement a value_of function"

value_of (Proc_Exp var body) env store =
  error "TODO: implement a value_of function"

value_of (Call_Exp rator rand) env store =
  error "TODO: implement a value_of function"

value_of (Block_Exp [exp]) env store =
  error "TODO: implement a value_of function"

value_of (Block_Exp (exp:expList)) env store =
  error "TODO: implement a value_of function"

value_of (Set_Exp x exp) env store =
  error "TODO: implement a value_of function"

--
value_of_program :: Exp -> ExpVal

value_of_program exp =
  error "TODO: implement a value_of_program function"


--
initEnv = empty_env

--
apply_procedure :: Proc -> ExpVal -> Store -> (ExpVal,Store)
apply_procedure proc arg store =
  error "TODO: implement an apply_procedure function"
