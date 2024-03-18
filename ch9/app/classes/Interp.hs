module Interp where

import Expr
import EnvStore

--
value_of :: ClassEnv -> Exp -> Env -> Store -> (ExpVal, Store)   

value_of class_env (Const_Exp n) env store = 
  error "TODO: implement a value_of function"

value_of class_env (Var_Exp var) env store = 
  error "TODO: implement a value_of function"

value_of class_env (Diff_Exp exp1 exp2) env store =
  error "TODO: implement a value_of function"

value_of class_env (Sum_Exp exp1 exp2) env store =
  error "TODO: implement a value_of function"
  
value_of class_env (IsZero_Exp exp) env store =
  error "TODO: implement a value_of function"

value_of class_env (If_Exp exp1 exp2 exp3) env store =
  error "TODO: implement a value_of function"

value_of class_env (Let_Exp varExpList body) env store =
  error "TODO: implement a value_of function"

value_of class_env (Letrec_Exp letbindings letrec_body) env store =
  error "TODO: implement a value_of function"

value_of class_env (Proc_Exp var body) env store =
  error "TODO: implement a value_of function"

value_of class_env (Call_Exp rator rands) env store =
  error "TODO: implement a value_of function"

value_of class_env (Block_Exp exps) env store =
  error "TODO: implement a value_of function"

value_of class_env (Set_Exp x exp) env store =
  error "TODO: implement a value_of function" -- The dummy value, 23, comes from the EOPL book. :)

value_of class_env (List_Exp exps) env store =
  error "TODO: implement a value_of function"

-- New kinds of expressions in classes

value_of class_env (Self_Exp) env store = 
  error "TODO: implement a value_of function"

value_of class_env (Method_Call_Exp obj_exp method_name args) env store =
  error "TODO: implement a value_of function"

value_of class_env (Super_Call_Exp method_name args) env store =
  error "TODO: implement a value_of function"

value_of class_env (New_Object_Exp class_name args) env store =
  error "TODO: implement a value_of function"

-- Helper function for evaluating args
-- value_of_arg class_env env (vals,store) arg =
--     let (val,store') = value_of class_env arg env store
--     in  (vals++[val],store')
--
value_of_program :: Program -> ExpVal
value_of_program (Program classDecls body) =
  error "TODO: implement a value_of_program function"


--
init_env = empty_env

--
-- initStore in EnvStore.hs

--
apply_procedure :: Proc -> [ExpVal] -> ClassEnv -> Store -> (ExpVal,Store)
apply_procedure proc args class_env store = 
  error "TODO: implement an apply_procedure function"

apply_method :: Method -> Object -> [ExpVal] -> ClassEnv -> Store -> (ExpVal, Store)
apply_method (AMethod vars body super_name field_names) self args class_env store = 
    error "TODO: implement an apply_method function"
