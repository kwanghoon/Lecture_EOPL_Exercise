module TypeCheck where

import qualified Data.Map as Map
import Expr

--
typeCheck :: Exp -> IO (Either String Type)
typeCheck exp = return (type_of_program exp )
     
--
type_of_program :: Exp -> Either String Type
type_of_program exp = error "TODO: implement a type_of_program function"

--
type TyEnv = Map.Map Identifier Type

type_of :: Exp -> TyEnv -> Either String Type

type_of (Const_Exp n) tyenv = error "TODO: implement a type_of function"

type_of (Var_Exp var) tyenv =
  error "TODO: implement a type_of function"

type_of (Diff_Exp exp1 exp2) tyenv =
  error "TODO: implement a type_of function"

type_of (IsZero_Exp exp1) tyenv =
  error "TODO: implement a type_of function"

type_of exp@(If_Exp exp1 exp2 exp3) tyenv =
  error "TODO: implement a type_of function"

type_of (Let_Exp var exp1 body) tyenv =
  error "TODO: implement a type_of function"

type_of (Letrec_Exp ty proc_name bound_var bvar_ty proc_body letrec_body) tyenv  =
  error "TODO: implement a type_of function"

type_of (Proc_Exp var argTy body) tyenv =
  error "TODO: implement a type_of function"

type_of (Call_Exp rator rand) tyenv =
  error "TODO: implement a type_of function"

         
-- Utilities
apply_tyenv :: TyEnv -> Identifier -> Either String Type 
apply_tyenv tyenv var =
  case Map.lookup var tyenv of
    Just ty -> Right ty
    Nothing -> Left $ "Variable not found: " ++ var

empty_tyenv :: TyEnv 
empty_tyenv = Map.empty 

extend_tyenv :: Identifier -> Type -> TyEnv -> TyEnv
extend_tyenv var ty tyenv = Map.insert var ty tyenv

expectedButErr :: Type -> Type -> Exp -> Either String Type
expectedButErr expectedTy gotTy exp =
  Left $ "Expected " ++ show expectedTy ++ " but got " ++ show gotTy ++ " in " ++ show exp

expectedFuntyButErr :: Type -> Exp -> Either String Type
expectedFuntyButErr gotTy exp =
  Left $ "Expected function type but got " ++ show gotTy ++ " in " ++ show exp

inequalIfBranchTyErr :: Type -> Type -> Exp -> Exp -> Either String Type
inequalIfBranchTyErr thenTy elseTy exp2 exp3 =
  Left $ "Type mismatch: \n"
          ++ "\t" ++ show thenTy ++ " in " ++ show exp2
          ++ "\t" ++ show elseTy ++ " in " ++ show exp3

inequalArgtyErr :: Type -> Type -> Exp -> Exp -> Either String Type
inequalArgtyErr argTy1 argTy2 funexp argexp =
  Left $ "Type mismatch: \n"
          ++ "\t" ++ show argTy1 ++ " for the arugment of " ++ show funexp
          ++ "\t" ++ show argTy2 ++ " in " ++ show argexp

equalType :: Type -> Type -> Bool
equalType TyInt  TyInt  = True
equalType TyBool TyBool = True
equalType (TyFun ty1 ty1') (TyFun ty2 ty2') =
  equalType ty1 ty2 && equalType ty1' ty2'
equalType _ _ = False

