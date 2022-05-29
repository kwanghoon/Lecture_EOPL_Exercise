module Parser where

import CommonParserUtil
import Token
import Expr

-- | Utility
rule prodRule action              = (prodRule, action, Nothing  )
ruleWithPrec prodRule action prec = (prodRule, action, Just prec)

--
parserSpec :: ParserSpec Token Exp IO ()
parserSpec = ParserSpec
  {
    startSymbol = "Expression'",

    tokenPrecAssoc = [],

    parserSpecList =
    [
      rule "Expression' -> Expression" (\rhs -> return $ get rhs 1),

      rule "Expression -> integer_number"
        (\rhs -> return $ Const_Exp (read (getText rhs 1) :: Int)),
      
      rule "Expression -> - integer_number"
        (\rhs -> return $ Const_Exp (-(read (getText rhs 2) :: Int))),
      
      rule "Expression -> - ( Expression , Expression )"
        (\rhs -> return $ Diff_Exp (get rhs 3) (get rhs 5)),

      rule "Expression -> if Expression then Expression else Expression"
        (\rhs -> return $ If_Exp (get rhs 2) (get rhs 4) (get rhs 6)),

      rule "Expression -> identifier" (\rhs -> return $ Var_Exp (getText rhs 1)),
      
      rule "Expression -> let identifier = Expression in Expression"
        (\rhs -> return $ Let_Exp (getText rhs 2) (get rhs 4) (get rhs 6)),

      rule "Expression -> letrec identifier ( identifier ) = Expression in Expression"
        (\rhs -> return $ Letrec_Exp (getText rhs 2) (getText rhs 4) (get rhs 7) (get rhs 9)),

      rule "Expression -> proc ( identifier ) Expression"
        (\rhs -> return $ Proc_Exp (getText rhs 3) (get rhs 5)),

      rule "Expression -> ( Expression Expression )"
        (\rhs -> return $ Call_Exp (get rhs 2) (get rhs 3)),

      rule "Expression -> list ( NumberList )"
        (\rhs -> return $ get rhs 3),

      rule "NumberList -> integer_number"
        (\rhs -> return $ Const_List_Exp [read (getText rhs 1) :: Int]),

      rule "NumberList -> integer_number , NumberList"
        (\rhs ->
           let num           = read (getText rhs 1) :: Int
               Const_List_Exp nums = get rhs 3 
           in  return $ Const_List_Exp (num : nums)),
      
      rule "Expression -> zero? ( Expression )"
        (\rhs -> return $ Unary_Exp IsZero (get rhs 3)),
      
      rule "Expression -> null? ( Expression )"
        (\rhs -> return $ Unary_Exp IsNull (get rhs 3)),

      rule "Expression -> car ( Expression )"
        (\rhs -> return $ Unary_Exp Car (get rhs 3)),

      rule "Expression -> cdr ( Expression )"
        (\rhs -> return $ Unary_Exp Cdr (get rhs 3)),

      rule "Expression -> try Expression catch ( identifier ) Expression"
        (\rhs -> return $ Try_Exp (get rhs 2) (getText rhs 5) (get rhs 7)),

      rule "Expression -> raise Expression"
        (\rhs -> return $ Raise_Exp (get rhs 2))
    ],
    
    baseDir        = "./",
    actionTblFile  = "action_table_letreclang.txt",
    gotoTblFile    = "goto_table_letreclang.txt",
    grammarFile    = "prod_rules_letreclang.txt",
    parserSpecFile = "mygrammar_letreclang.grm",
    genparserexe   = "yapb-exe"
  }


