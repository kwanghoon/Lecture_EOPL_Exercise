
-- The syntax is based on the implicitrefs language, and
-- the semantics is based on the one for the continuation-based language.

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

      rule "Expression -> letrec ArbiNumberOfUnaryProcs in Expression"
        (\rhs -> let Letrec_Exp recbinds _ = get rhs 2 in
                   return $ Letrec_Exp recbinds (get rhs 4)),

      rule "ArbiNumberOfUnaryProcs -> identifier ( identifier ) = Expression"
        (\rhs -> return $ Letrec_Exp [ (getText rhs 1, getText rhs 3, get rhs 6) ] undefined),

      rule "ArbiNumberOfUnaryProcs -> identifier ( identifier ) = Expression ArbiNumberOfUnaryProcs"
        (\rhs -> let recbind = (getText rhs 1, getText rhs 3, get rhs 6)
                     Letrec_Exp theRest body = get rhs 7
                 in  return $ Letrec_Exp (recbind:theRest) body),
      
      rule "Expression -> proc ( identifier ) Expression"
        (\rhs -> return $ Proc_Exp (getText rhs 3) (get rhs 5)),

      rule "Expression -> ( Expression Expression )"
        (\rhs -> return $ Call_Exp (get rhs 2) (get rhs 3)),

      rule "Expression -> begin ExpressionList end"
        (\rhs -> return $ get rhs 2),

      rule "ExpressionList -> Expression"
        (\rhs -> return $ Block_Exp [ get rhs 1 ]),

      rule "ExpressionList -> Expression ; ExpressionList"
        (\rhs -> return $
                   case get rhs 3 of
                     Block_Exp exprs -> Block_Exp (get rhs 1 : exprs)),
        
      rule "Expression -> set identifier = Expression"
        (\rhs -> return $ Set_Exp (getText rhs 2) (get rhs 4)),

      rule "Expression -> spawn ( Expression )"    -- key features
        (\rhs -> return $ Spawn_Exp (get rhs 3)),

      rule "Expression -> yield ( )"               -- key features
        (\rhs -> return $ Yield_Exp),
      
      rule "Expression -> mutex ( )"               -- key features
        (\rhs -> return $ Mutex_Exp),
      
      rule "Expression -> wait ( Expression )"     -- key features
        (\rhs -> return $ Wait_Exp (get rhs 3)),
      
      rule "Expression -> signal ( Expression )"   -- key features
        (\rhs -> return $ Signal_Exp (get rhs 3)),
      
      rule "Expression -> [ NumberList ]"          -- change: lists => [ ... ]
        (\rhs -> return $ get rhs 2),

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

      rule "Expression -> print ( Expression )"
        (\rhs -> return $ Unary_Exp Print (get rhs 3))
    ],
    
    baseDir        = "./",
    actionTblFile  = "action_table_threadslang.txt",
    gotoTblFile    = "goto_table_threadslang.txt",
    grammarFile    = "prod_rules_threadslang.txt",
    parserSpecFile = "mygrammar_threadslang.grm",
    genparserexe   = "yapb-exe"
  }


