module Expr where

type Program = Exp
  
data Exp =
    Const_Exp  Int
  | Diff_Exp   Exp Exp
  | IsZero_Exp Exp
  | If_Exp     Exp Exp Exp
  | Var_Exp    Identifier
  | Let_Exp    Identifier Exp Exp
  deriving Show

type Identifier = String

data ExpVal =
    Num_Val {expval_num :: Int}
  | Bool_Val {expval_bool :: Bool}

instance Show ExpVal where
  show (Num_Val num)   = show num
  show (Bool_Val bool) = show bool

type DenVal = ExpVal   
