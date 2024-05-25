module TypeCheck where

import qualified Data.Map as Map
import Expr

--
typeCheck :: Exp -> IO (Either String Type)
typeCheck exp = return (type_of_program exp )
     
--
type_of_program :: Exp -> Either String Type
type_of_program exp = error "TODO: implement a type_of_program function"
     
initTyEnv :: TyEnv     
initTyEnv = Map.empty 

--
type TyEnv = Map.Map Identifier Type

type_of :: TyEnv -> Exp -> Either String Type

type_of tyenv exp@(Const_Exp n) = error "TODO: implement a type_of function"

type_of tyenv exp@(Var_Exp var) =
  error "TODO: implement a type_of function"

type_of tyenv exp@(Diff_Exp exp1 exp2) =
  error "TODO: implement a type_of function"

type_of tyenv exp@(IsZero_Exp exp1) =
  error "TODO: implement a type_of function"

type_of tyenv exp@(If_Exp exp1 exp2 exp3) =
  error "TODO: implement a type_of function"

type_of tyenv exp@(Let_Exp var exp1 body) =
  error "TODO: implement a type_of function"

type_of tyenv exp@(Letrec_Exp ty proc_name bound_var bvar_ty proc_body letrec_body) =
  error "TODO: implement a type_of function"

type_of tyenv exp@(Proc_Exp var argTy body) =
  error "TODO: implement a type_of function"

type_of tyenv exp@(Call_Exp rator rand) =
  error "TODO: implement a type_of function"

         
-- Utilities
apply_tyenv :: TyEnv -> Identifier -> Either String Type 
apply_tyenv tyenv var =
  case Map.lookup var tyenv of
    Just ty -> Right ty
    Nothing -> Left $ "Variable not found: " ++ var

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

