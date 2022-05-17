module Expr(Program,Exp(..),Identifier) where

type Program = Exp
  
data Exp =
    Const_Exp  Int
  | Diff_Exp   Exp Exp
  | IsZero_Exp Exp
  | If_Exp     Exp Exp Exp
  | Var_Exp    Identifier
  | Let_Exp    Identifier Exp Exp            -- let x = exp in exp 
  | Letrec_Exp Identifier Identifier Exp Exp -- letrec f(arg) = ... recusive expr ... in ... f ... 
  | Proc_Exp   Identifier Exp                -- proc (arg) exp 
  | Call_Exp   Exp Exp                       -- call
  deriving Show

type Identifier = String

