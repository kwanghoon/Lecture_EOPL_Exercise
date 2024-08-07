module EnvStore where

import Expr (Identifier,Exp)
import Data.List(intersperse)
import Data.Maybe
import Queue

-- Environment
data Env =
    Empty_env
  | Extend_env Identifier DenVal Env
  | Extend_env_rec [(Identifier,Identifier,Exp)] Env

empty_env :: Env
empty_env = Empty_env

apply_env :: Env -> Store -> Identifier -> (DenVal, Store)
apply_env Empty_env store search_var = error (search_var ++ " is not found.")
apply_env (Extend_env saved_var saved_val saved_env) store search_var
  | search_var==saved_var = (saved_val,store)
  | otherwise             = apply_env saved_env store search_var
apply_env (Extend_env_rec idIdExpList saved_env) store search_var
  | isIn      = newref store procVal
  | otherwise = apply_env saved_env store search_var
  where isIn      = or [ p_name==search_var | (p_name,b_var,p_body) <- idIdExpList ]
        procVal = head [ Proc_Val (procedure b_var p_body (Extend_env_rec idIdExpList saved_env)) 
                       | (p_name,b_var,p_body) <- idIdExpList, p_name==search_var ]

extend_env :: Identifier -> DenVal -> Env -> Env
extend_env x v env = Extend_env x v env

extend_env_rec :: [(Identifier,Identifier,Exp)] -> Env -> Env
extend_env_rec idIdExpList env = Extend_env_rec idIdExpList env

-- Expressed values
data ExpVal =
    Num_Val   {expval_num  :: Int}
  | Bool_Val  {expval_bool :: Bool}
  | Proc_Val  {expval_proc :: Proc}
  | List_Val  {expval_list :: [ExpVal]}
  | Mutex_Val {expval_mutex :: Mutex } -- Mutex {Loc to Bool, Loc to Queue Thread}
  | Queue_Val {expval_queue :: Queue Thread}  -- (newref queue); newref takes an Expval arg!

instance Show ExpVal where
  show (Num_Val num)   = show num
  show (Bool_Val bool) = show bool
  show (Proc_Val proc) = show "<proc>"
  show (List_Val nums) = show "[" ++ concat (intersperse "," (map show nums)) ++ show "]"
  show (Mutex_Val mutex) = show mutex

type FinalAnswer = ExpVal 

-- Location
type Location = Integer

-- Denoted values   
type DenVal = Location

-- Procedure values : data structures
data Proc = Procedure {var :: Identifier, body :: Exp, saved_env :: Env}

procedure :: Identifier -> Exp -> Env -> Proc
procedure var body env = Procedure var body env

-- Mutex values : boolean and thread queue
data Mutex = Mutex Location Location -- binary semaphores: Loc to Bool, Loc to (Queue Thread)
             deriving Show

-- Threads
type Thread = Store -> SchedState -> (FinalAnswer, Store)

-- Scheduler states
data SchedState =
  SchedState {
   the_ready_queue :: Queue Thread,
   the_final_answer :: Maybe FinalAnswer,
   the_max_time_slice :: Integer,
   the_time_remaining :: Integer
  }

-- In Interp.hs
-- apply_procedure :: Proc -> ExpVal -> ExpVal
  
type Store = (Location, [(Location,ExpVal)]) -- Next new location

newref :: Store -> ExpVal -> (Location,Store)
newref store@(next,s) v = (next,(next+1,(next,v):s))

deref :: Store -> Location -> ExpVal
deref store@(next,s) loc =
  case [v | (loc',v) <- s, loc==loc'] of
    (v:_) -> v
    _     -> error ("Location not found: " ++ show loc)

setref :: Store -> Location -> ExpVal -> Store
setref store@(next,s) loc v = (next,update s)
  where update [] = error ("Invalid reference: " ++ show loc)
        update ((loc',w):s')
          | loc==loc' = (loc,v):s'
          | otherwise = (loc',w):update s'

initStore :: Store
initStore = (1,[])

