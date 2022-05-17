module Expr(Program,Exp(..),Identifier,PET(..),fromExp,fromExpList,fromIdIdExpList) where

type Program = Exp
  
data Exp =
    Const_Exp  Int
  | Diff_Exp   Exp Exp
  | IsZero_Exp Exp
  | If_Exp     Exp Exp Exp
  | Var_Exp    Identifier
  | Let_Exp    Identifier Exp Exp
  | Letrec_Exp [(Identifier, Identifier, Exp)] Exp -- letrec f1(x1) = expr1; ... ; fk(xk) = exprk in expr
  | Proc_Exp   Identifier Exp                      -- proc
  | Call_Exp   Exp Exp                             -- call
  | Block_Exp  [Exp]
  | Newref_Exp Exp
  | Deref_Exp  Exp
  | Setref_Exp Exp Exp
  deriving Show

type Identifier = String

--- Parsed Expression Tree

data PET =
    PET_IdIdExpList {idIdExpListFrom :: [(Identifier, Identifier, Exp)] }
  | PET_ExpList {expListFrom :: [Exp] }
  | PET_Exp {expFrom :: Exp}
  deriving Show

fromExp exp                 = PET_Exp exp
fromExpList expList         = PET_ExpList expList
fromIdIdExpList idIdExpList = PET_IdIdExpList idIdExpList
