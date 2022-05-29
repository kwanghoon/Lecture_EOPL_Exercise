module Expr(Program,Exp(..),Identifier) where

type Program = Exp
  
data Exp =
    Const_Exp  Int
  | Diff_Exp   Exp Exp
  | IsZero_Exp Exp
  | If_Exp     Exp Exp Exp
  | Var_Exp    Identifier
  | Let_Exp    Identifier Exp Exp
  | Letrec_Exp Identifier Identifier Exp Exp -- letrec f(x) = ... recusive expr ...
  | Proc_Exp   Identifier Exp                -- proc
  | Call_Exp   Exp Exp                       -- call
  deriving Show

type Identifier = String

