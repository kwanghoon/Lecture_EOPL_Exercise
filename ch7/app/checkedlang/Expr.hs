module Expr where

import Data.Maybe

-- for abstract syntax tree
type Program = Exp
  
data Exp =
    Const_Exp  Int
  | Diff_Exp   Exp Exp
  | IsZero_Exp Exp
  | If_Exp     Exp Exp Exp
  | Var_Exp    Identifier
  | Let_Exp    Identifier Exp Exp
  | Letrec_Exp Type Identifier Identifier Type Exp Exp -- letrec f(x) = ... recusive expr ...
  | Proc_Exp   Identifier Type Exp           -- proc
  | Call_Exp   Exp Exp                       -- call
  deriving Show

type Identifier = String

data Type =
    TyInt
  | TyBool
  | TyFun Type Type
  deriving Show


-- for parser
data AST =
    ASTExp { fromASTExp :: Exp }
  | ASTType { fromASTType :: Type }
  deriving Show

toASTExp exp = ASTExp exp

toASTType ty = ASTType ty


-- for testing the type checker
type TestCaseName = String
type ExprText = String

data TypeDeclTestCase = TDTC TestCaseName ExprText (Maybe Type)

data TypeDeclTestSuite = TypeDeclTestSuite [ TypeDeclTestCase ]


