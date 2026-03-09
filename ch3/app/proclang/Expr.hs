{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Expr where

type Program = Exp
  
-- [TODO] Complete data Exp. 
-- data Exp = 
--       ... 
data Exp = DummyExp
  deriving Show

type Identifier = String

const_exp n = error "TODO: implement a const_exp function" 
diff_exp e1 e2 = error "TODO: implement a diff_exp function"

iszero_exp e = error "TODO: implement a iszero_exp function" 

if_exp e1 e2 e3 = error "TODO: implement a if_exp function" 

var_exp s = error "TODO: implement a var_exp function" 

let_exp x e1 e2 = error "TODO: implement a let_exp function" 

proc_exp x e = error "TODO: implement a proc_exp function" 

call_exp e1 e2 = error "TODO: implement a call_exp function" 


