
-- The syntax is based on the implicitrefs language, and
-- the semantics is based on the one for the continuation-based language.

module Interp where

import Expr
import EnvStore
import Semaphores
import Scheduler

import Debug.Trace

-- Continuation

data Cont =
    End_Main_Thread_Cont
  | Zero1_Cont Cont
  | Let_Exp_Cont Identifier Exp Env Cont
  | If_Test_Cont Exp Exp Env Cont
  | Diff1_Cont Exp Env Cont
  | Diff2_Cont ExpVal Cont
  | Rator_Cont Exp Env Cont
  | Rand_Cont ExpVal Cont
  | Unop_Arg_Cont UnaryOp Cont
  | Set_Rhs_Cont Location Cont
  | Spawn_Cont Cont
  | Wait_Cont Cont
  | Signal_Cont Cont
  | End_Subthread_Cont

apply_cont :: Cont -> ExpVal -> Store -> SchedState -> (FinalAnswer, Store)
apply_cont cont val store sched =
  if time_expired sched
  then
    let sched' = error "TODO: implement place this on the ready queue"
    in  run_next_thread store sched'
    
  else
    let sched' = error "TODO: implement the decrement of the timer" 
    in  apply_cont' cont val store sched'
    
  where
    apply_cont' End_Main_Thread_Cont v store sched =
      error "TODO: implement an apply_cont' function"

    apply_cont' (Zero1_Cont cont) num1 store sched =
      error "TODO: implement an apply_cont' function"

    apply_cont' (Let_Exp_Cont var body env cont) val1 store sched =
      error "TODO: implement an apply_cont' function"

    apply_cont' (If_Test_Cont exp2 exp3 env cont) v store sched =
      error "TODO: implement an apply_cont' function"

    apply_cont' (Diff1_Cont exp2 env cont) val1 store sched =
      error "TODO: implement an apply_cont' function"

    apply_cont' (Diff2_Cont val1 cont) val2 store sched =
      error "TODO: implement an apply_cont' function"

    apply_cont' (Unop_Arg_Cont op cont) val store sched =
      error "TODO: implement an apply_cont' function"

    apply_cont' (Rator_Cont rand env cont) ratorVal store sched =
      error "TODO: implement an apply_cont' function"

    apply_cont' (Rand_Cont ratorVal cont) randVal store sched =
      error "TODO: implement an apply_cont' function"

    apply_cont' (Set_Rhs_Cont loc cont) val store sched =
      error "TODO: implement an apply_cont' function"

    apply_cont' (Spawn_Cont saved_cont) val store sched =
      error "TODO: implement an apply_cont' function"

    apply_cont' (Wait_Cont saved_cont) val store sched =
      error "TODO: implement an apply_cont' function"

    apply_cont' (Signal_Cont saved_cont) val store sched =
      error "TODO: implement an apply_cont' function"

    apply_cont' (End_Subthread_Cont) val store sched =
      error "TODO: implement an apply_cont' function"

      
-- Todo: Introduce exceptions and define apply_handler to see how complex it is!
-- Todo: Use the monadic style to hide as many global parameters as possible.

apply_unop :: UnaryOp -> ExpVal -> ExpVal

apply_unop IsZero (Num_Val num)  = error "TODO: implement an apply_unop function"
apply_unop IsNull (List_Val [])  = error "TODO: implement an apply_unop function"
apply_unop IsNull (List_Val _)   = error "TODO: implement an apply_unop function"
apply_unop Car (List_Val (x:_))  = error "TODO: implement an apply_unop function"
apply_unop Cdr (List_Val (_:xs)) = error "TODO: implement an apply_unop function"
apply_unop Print v = trace (show v) $ List_Val []  -- Use this code instead of writing a monadic one.

--
value_of_k :: Exp -> Env -> Cont -> Store -> SchedState -> (FinalAnswer, Store)

value_of_k (Const_Exp n) env cont store sched =
  error "TODO: implement a value_of_k function"  

value_of_k (Const_List_Exp nums) env cont store sched =
  error "TODO: implement a value_of_k function"  

value_of_k (Var_Exp var) env cont store sched =
  error "TODO: implement a value_of_k function"  

value_of_k (Diff_Exp exp1 exp2) env cont store sched =
  error "TODO: implement a value_of_k function"  

value_of_k (Unary_Exp op exp1) env cont store sched =
  error "TODO: implement a value_of_k function"  
  
value_of_k (If_Exp exp1 exp2 exp3) env cont store sched =
  error "TODO: implement a value_of_k function"  

value_of_k (Let_Exp var exp1 body) env cont store sched =
  error "TODO: implement a value_of_k function"  

value_of_k (Letrec_Exp nameArgBodyList letrec_body) env cont store sched =
  error "TODO: implement a value_of_k function"  

value_of_k (Proc_Exp var body) env cont store sched =
  error "TODO: implement a value_of_k function"  

value_of_k (Call_Exp rator rand) env cont store sched =
  error "TODO: implement a value_of_k function"  
  
value_of_k (Block_Exp [exp]) env cont store sched =
  error "TODO: implement a value_of_k function"  

value_of_k (Block_Exp (exp:exps)) env cont store sched =
  error "TODO: implement a value_of_k function"  

value_of_k (Set_Exp x exp) env cont store sched =
  error "TODO: implement a value_of_k function"  

value_of_k (Spawn_Exp exp) env cont store sched =
  error "TODO: implement a value_of_k function"  

value_of_k (Yield_Exp) env cont store sched =
  error "TODO: implement a value_of_k function"  

value_of_k (Mutex_Exp) env cont store sched =
  error "TODO: implement a value_of_k function"  

value_of_k (Wait_Exp exp) env cont store sched =
  error "TODO: implement a value_of_k function"  

value_of_k (Signal_Exp exp) env cont store sched =
  error "TODO: implement a value_of_k function"  


--
value_of_program :: Exp -> Integer -> ExpVal

value_of_program exp timeslice =
  error "TODO: implement a value_of_program function"  


--
initEnv = empty_env

--
apply_procedure_k :: Proc -> ExpVal -> Store -> SchedState -> Cont -> (FinalAnswer, Store)
apply_procedure_k proc arg store sched cont =
  error "TODO: implement an apply_procedure_k function"  
