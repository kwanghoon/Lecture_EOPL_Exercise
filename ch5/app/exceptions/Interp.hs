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
  | Unop_Arg_Cont UnaryOp Cont
  | Try_Cont Identifier Exp Env Cont
  | Raise1_Cont Cont

apply_cont :: Cont -> ExpVal -> FinalAnswer
apply_cont End_Cont v = error "TODO: implement apply_cont"

apply_cont (Zero1_Cont cont) num1 = error "TODO: implement apply_cont"
    
apply_cont (Let_Exp_Cont var body env cont) val1 = error "TODO: implement apply_cont"

apply_cont (If_Test_Cont exp2 exp3 env cont) v = error "TODO: implement apply_cont"
  
apply_cont (Diff1_Cont exp2 env cont) val1 = error "TODO: implement apply_cont"

apply_cont (Diff2_Cont val1 cont) val2 = error "TODO: implement apply_cont"

apply_cont (Unop_Arg_Cont op cont) val = error "TODO: implement apply_cont"

apply_cont (Rator_Cont rand env cont) ratorVal = error "TODO: implement apply_cont"

apply_cont (Rand_Cont ratorVal cont) randVal = error "TODO: implement apply_cont"

apply_cont (Try_Cont var handler_exp env cont) val = error "TODO: implement apply_cont"
                           
apply_cont (Raise1_Cont cont) val = error "TODO: implement apply_cont"



apply_handler :: ExpVal -> Cont -> FinalAnswer
apply_handler val (Try_Cont var handler_exp env saved_cont) = error "TODO: implement apply_handler"

apply_handler val (End_Cont) = error "TODO: implement apply_handler"

apply_handler val (Zero1_Cont cont) = error "TODO: implement apply_handler"

apply_handler val (Let_Exp_Cont x body env cont) = error "TODO: implement apply_handler"

apply_handler val (If_Test_Cont exp2 exp3 env cont) = error "TODO: implement apply_handler"

apply_handler val (Diff1_Cont exp env cont) = error "TODO: implement apply_handler"

apply_handler val (Diff2_Cont val1 cont) = error "TODO: implement apply_handler"

apply_handler val (Unop_Arg_Cont op cont) = error "TODO: implement apply_handler"

apply_handler val (Rator_Cont exp env cont) = error "TODO: implement apply_handler"

apply_handler val (Rand_Cont val1 cont) = error "TODO: implement apply_handler"


apply_unop :: UnaryOp -> ExpVal -> ExpVal 
apply_unop IsZero (Num_Val num) = error "TODO: implement apply_unop"
apply_unop IsNull (List_Val [])  = error "TODO: implement apply_unop"
apply_unop IsNull (List_Val _)   = error "TODO: implement apply_unop"
apply_unop Car (List_Val (x:_))  = error "TODO: implement apply_unop"
apply_unop Cdr (List_Val (_:xs)) = error "TODO: implement apply_unop"

--
value_of_k :: Exp -> Env -> Cont -> FinalAnswer

value_of_k (Const_Exp n) env cont = error "TODO: implement value_of_k"

value_of_k (Const_List_Exp nums) env cont = error "TODO: implement value_of_k"

value_of_k (Var_Exp var) env cont = error "TODO: implement value_of_k"

value_of_k (Diff_Exp exp1 exp2) env cont = error "TODO: implement value_of_k"

value_of_k (Unary_Exp op exp1) env cont = error "TODO: implement value_of_k"
  
value_of_k (If_Exp exp1 exp2 exp3) env cont = error "TODO: implement value_of_k"

value_of_k (Let_Exp var exp1 body) env cont = error "TODO: implement value_of_k"

value_of_k (Letrec_Exp proc_name bound_var proc_body letrec_body) env cont =
  error "TODO: implement value_of_k"

value_of_k (Proc_Exp var body) env cont = error "TODO: implement value_of_k"

value_of_k (Call_Exp rator rand) env cont = error "TODO: implement value_of_k"
  
value_of_k (Try_Exp exp var handler_exp) env cont = error "TODO: implement value_of_k"

value_of_k (Raise_Exp exp) env cont = error "TODO: implement value_of_k"

--
value_of_program :: Exp -> ExpVal

value_of_program exp = error "TODO: implement value_of_k"


--
apply_procedure_k :: Proc -> ExpVal -> Cont -> FinalAnswer
apply_procedure_k proc arg cont = error "TODO: implement an apply_procedure_k"
