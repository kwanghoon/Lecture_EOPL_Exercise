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
        (\rhs -> return $ const_exp (read (getText rhs 1) :: Int)),
      
      rule "Expression -> - integer_number"
        (\rhs -> return $ const_exp (-(read (getText rhs 2) :: Int))),
      
      rule "Expression -> - ( Expression , Expression )"
        (\rhs -> return $ diff_exp (get rhs 3) (get rhs 5)),

      rule "Expression -> zero? ( Expression )"
        (\rhs -> return $ iszero_exp (get rhs 3)),
      
      rule "Expression -> if Expression then Expression else Expression"
        (\rhs -> return $ if_exp (get rhs 2) (get rhs 4) (get rhs 6)),

      rule "Expression -> identifier" (\rhs -> return $ var_exp (getText rhs 1)),
      
      rule "Expression -> let identifier = Expression in Expression"
        (\rhs -> return $ let_exp (getText rhs 2) (get rhs 4) (get rhs 6)),

      rule "Expression -> letrec identifier ( identifier ) = Expression in Expression"
        (\rhs -> return $ letrec_exp (getText rhs 2) (getText rhs 4) (get rhs 7) (get rhs 9)),

      rule "Expression -> proc ( identifier ) Expression"
        (\rhs -> return $ proc_exp (getText rhs 3) (get rhs 5)),

      rule "Expression -> ( Expression Expression )"
        (\rhs -> return $ call_exp (get rhs 2) (get rhs 3))
    ],
    
    baseDir        = "./",
    actionTblFile  = "action_table_letreclang.txt",
    gotoTblFile    = "goto_table_letreclang.txt",
    grammarFile    = "prod_rules_letreclang.txt",
    parserSpecFile = "mygrammar_letreclang.grm",
    genparserexe   = "yapb-exe"
  }


