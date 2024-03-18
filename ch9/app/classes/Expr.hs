module Expr(Program(..),ClassDecl(..),MethodDecl(..),Exp(..),Identifier,
            PET(..),
            fromExp,fromExpList,fromIdExpList,fromIdIdListExpList,fromIdList,
            fromClassDecl,fromClassDeclList,fromMethodDecl,fromMethodDeclList,
            fromProgram) where

-- Untyped class-based expression language

data Program = Program [ClassDecl] Exp
  deriving Show
  
-- Class_Decl: class-name, super-class-name, field-names, method-decls
data ClassDecl = Class_Decl Identifier Identifier [ Identifier ] [ MethodDecl ]
  deriving Show

-- Method_Decl: method-name, parameter-names, body
data MethodDecl = Method_Decl Identifier [ Identifier ] Exp  
  deriving Show

data Exp =
    Const_Exp  Int
  | Diff_Exp   Exp Exp
  | Sum_Exp    Exp Exp
  | IsZero_Exp Exp
  | If_Exp     Exp Exp Exp
  | Var_Exp    Identifier
  | Let_Exp    LetBindings Exp    -- let bindings in expr
  | Letrec_Exp LetRecBindings Exp -- letrec rec_bindings in expr
  | Proc_Exp   [Identifier] Exp   -- proc
  | Call_Exp   Exp [Exp]          -- e1 e2 ... en (n >= 1)
  | Block_Exp  [Exp]
  | Set_Exp    Identifier Exp
  | List_Exp   [Exp]              -- list(e1, ..., en)

  -- New kinds of expressions in classes
  | New_Object_Exp Identifier [Exp]      -- new Identifier ( Exp1, ..., Expn )
  | Method_Call_Exp Exp Identifier [Exp] -- Exp.Identifier ( Exp1, ..., Expn )
  | Super_Call_Exp Identifier [Exp]      -- super.Identifier ( Exp1, ..., Expn )
  | Self_Exp                             -- self
  deriving Show  

type Identifier = String

-- x1 = expr1  ...  xn = exprn
type LetBindings = [ (Identifier, Exp) ]  

-- f1(x11,...,xn1) = expr1  ...  fk(xk1,...,xkn) = exprk
type LetRecBindings = [(Identifier, [Identifier], Exp)] 

--- Parsed Expression Tree

data PET =
    PET_IdIdListExpList {idIdListExpListFrom :: [(Identifier, [Identifier], Exp)] }
  | PET_IdExpList {idExpListFrom :: [(Identifier, Exp)] }
  | PET_ExpList {expListFrom :: [Exp] }
  | PET_Exp {expFrom :: Exp}
  | PET_IdList {idListFrom :: [Identifier] }
  | PET_MethodDecl {methodDeclFrom :: MethodDecl}
  | PET_MethodDeclList {methodDeclListFrom :: [MethodDecl]}
  | PET_ClassDecl {classDeclFrom :: ClassDecl}
  | PET_ClassDeclList {classDeclListFrom :: [ClassDecl]}
  | PET_Program {programFrom :: Program}
  deriving Show

fromExp exp                 = PET_Exp exp
fromExpList expList         = PET_ExpList expList
fromIdExpList idExpList     = PET_IdExpList idExpList
fromIdIdListExpList idIdListExpList 
                            = PET_IdIdListExpList idIdListExpList
fromIdList idList           = PET_IdList idList
fromMethodDecl methodDecl   = PET_MethodDecl methodDecl
fromClassDecl classDecl     = PET_ClassDecl classDecl
fromMethodDeclList methodDeclList = PET_MethodDeclList methodDeclList
fromClassDeclList classDeclList   = PET_ClassDeclList classDeclList
fromProgram program         = PET_Program program
