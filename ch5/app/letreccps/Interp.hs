module Interp where

import Expr
import Env

type FinalAnswer = ExpVal 

-- Continuation

data Cont =
    End_Cont
  | Zero1_Cont Cont
  | Let_Exp_Cont Identifier Exp Env Cont
  | If_Test_Cont Exp Exp Env Cont
  | Diff1_Cont Exp Env Cont
  | Diff2_Cont ExpVal Cont
  | Rator_Cont Exp Env Cont
  | Rand_Cont ExpVal Cont

apply_cont :: Cont -> ExpVal -> FinalAnswer
apply_cont End_Cont v = error "TODO: implement a apply_cont function"

apply_cont (Zero1_Cont cont) num1 = error "TODO: implement a apply_cont function"
    
apply_cont (Let_Exp_Cont var body env cont) val1 = error "TODO: implement a apply_cont function"

apply_cont (If_Test_Cont exp2 exp3 env cont) v = error "TODO: implement a apply_cont function"
  
apply_cont (Diff1_Cont exp2 env cont) val1 = error "TODO: implement a apply_cont function"

apply_cont (Diff2_Cont val1 cont) val2 = error "TODO: implement a apply_cont function"

apply_cont (Rator_Cont rand env cont) ratorVal = error "TODO: implement a apply_cont function"

apply_cont (Rand_Cont ratorVal cont) randVal = error "TODO: implement a apply_cont function"


--
value_of_k :: Exp -> Env -> Cont -> FinalAnswer

value_of_k (Const_Exp n) env cont = error "TODO: implement a value_of_k function"

value_of_k (Var_Exp var) env cont = error "TODO: implement a value_of_k function"

value_of_k (Diff_Exp exp1 exp2) env cont = error "TODO: implement a value_of_k function"
  
value_of_k (IsZero_Exp exp) env cont = error "TODO: implement a value_of_k function"

value_of_k (If_Exp exp1 exp2 exp3) env cont = error "TODO: implement a value_of_k function"

value_of_k (Let_Exp var exp1 body) env cont = error "TODO: implement a value_of_k function"

value_of_k (Letrec_Exp proc_name bound_var proc_body letrec_body) env cont =
  error "TODO: implement a value_of_k function"

value_of_k (Proc_Exp var body) env cont = error "TODO: implement a value_of_k function"

value_of_k (Call_Exp rator rand) env cont = error "TODO: implement a value_of_k function"
  

--
value_of_program :: Exp -> ExpVal

value_of_program exp = error "TODO: implement a value_of_program function"


--
apply_procedure_k :: Proc -> ExpVal -> Cont -> FinalAnswer
apply_procedure_k proc arg cont =
  error "TODO: implement a apply_procedure_k function"
