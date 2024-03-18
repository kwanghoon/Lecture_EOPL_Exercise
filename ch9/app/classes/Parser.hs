module Parser where

import CommonParserUtil
import Token
import Expr

-- | Utility
rule prodRule action              = (prodRule, action, Nothing  )
ruleWithPrec prodRule action prec = (prodRule, action, Just prec)

--
parserSpec :: ParserSpec Token PET IO ()
parserSpec = ParserSpec
  {
    startSymbol = "Program'",

    tokenPrecAssoc = [],

    parserSpecList =
    [
      {- Program : classes and an expression -}

      rule "Program' -> Program" (\rhs -> return $ get rhs 1),

      rule "Program -> ZeroMoreClassDecl Expression" 
        (\rhs -> return $ fromProgram $ 
                  Program (classDeclListFrom (get rhs 1)) (expFrom (get rhs 2))),

      rule "ZeroMoreClassDecl -> ClassDecl ZeroMoreClassDecl" 
        (\rhs -> return $ fromClassDeclList $ 
                  classDeclFrom (get rhs 1) : classDeclListFrom (get rhs 2)),

      rule "ZeroMoreClassDecl -> " (\rhs -> return $ fromClassDeclList []),

      rule "ClassDecl -> class identifier extends identifier ZeroMoreFieldDecl ZeroMoreMethodDecl"
        (\rhs -> return $ fromClassDecl $ 
                  Class_Decl (getText rhs 2) (getText rhs 4) 
                    (idListFrom (get rhs 5)) (methodDeclListFrom (get rhs 6))),

      rule "ZeroMoreFieldDecl -> field identifier ZeroMoreFieldDecl" 
        (\rhs -> return $ fromIdList $ getText rhs 2 : idListFrom (get rhs 3)),

      rule "ZeroMoreFieldDecl -> " (\rhs -> return $ fromIdList []),

      rule "ZeroMoreMethodDecl -> MethodDecl ZeroMoreMethodDecl" 
        (\rhs -> return $ fromMethodDeclList $ 
                  methodDeclFrom (get rhs 1) : methodDeclListFrom (get rhs 2)),

      rule "ZeroMoreMethodDecl -> " (\rhs -> return $ fromMethodDeclList []),

      rule "MethodDecl -> method identifier ( ZeroMoreIdentifier ) Expression"
        (\rhs -> return $ fromMethodDecl $ 
                  Method_Decl (getText rhs 2) (idListFrom (get rhs 4)) (expFrom (get rhs 6))),

      -- ZeroMoreIdentifier :: PET_IdList
      rule "ZeroMoreIdentifier -> OneMoreIdentifier" (\rhs -> return $ get rhs 1),

      rule "ZeroMoreIdentifier -> " (\rhs -> return $ fromIdList []),

      rule "OneMoreIdentifier -> identifier , OneMoreIdentifier" 
        (\rhs -> return $ fromIdList $ getText rhs 1 : idListFrom (get rhs 3)),

      rule "OneMoreIdentifier -> identifier" (\rhs -> return $ fromIdList [ getText rhs 1 ]),
  

      {- Multiple expressions -}

      -- Exp1, Exp2, ... , Expk  (k >= 0)
      -- ZeroMoreExpression :: PET_ExpList
      rule "ZeroMoreExpression -> OneMoreExpression" (\rhs -> return $ get rhs 1),

      rule "ZeroMoreExpression -> " (\rhs -> return $ fromExpList []),

      -- Exp1, Exp2, ... , Expk  (k >= 1)  (separated by a comma)
      -- OneMoreExpression :: PET_ExpList  where length >= 1
      rule "OneMoreExpression -> Expression , OneMoreExpression" 
        (\rhs -> return $ fromExpList $ 
                  expFrom (get rhs 1) : expListFrom (get rhs 3)),

      rule "OneMoreExpression -> Expression" 
        (\rhs -> return $ fromExpList [ expFrom (get rhs 1) ]),

      -- Exp1 ; Exp2 ; ... ; Expk (k >= 0)   (separated by a semicolon)
      -- ExpressionList :: PET_ExpList
      rule "ExpressionList -> Expression"
        (\rhs -> return $ fromExpList $ [ expFrom (get rhs 1) ]),

      rule "ExpressionList -> Expression ; ExpressionList"
        (\rhs -> return $ fromExpList $ 
                  (expFrom (get rhs 1) : expListFrom (get rhs 3))),
      
      -- Exp1 Exp2 ... Expk  (separated by a space)
      -- ExpressionListSpace : PET_ExpList
      rule "ExpressionListSpace -> Expression"
        (\rhs -> return $ fromExpList $ [ expFrom (get rhs 1) ]),

      rule "ExpressionListSpace -> Expression ExpressionListSpace"
        (\rhs -> return $ fromExpList $ 
                  (expFrom (get rhs 1) : expListFrom (get rhs 3))),


      {- Single expression -}

      -- Expression :: PET_Exp 
      rule "Expression -> new identifier ( ZeroMoreExpression )"
        (\rhs -> return $ fromExp $ 
                    New_Object_Exp (getText rhs 2) (expListFrom (get rhs 4))),

      rule "Expression -> self" (\rhs -> return $ fromExp $ Self_Exp),

      rule "Expression -> send Expression identifier ( ZeroMoreExpression )"
        (\rhs -> return $ fromExp $ 
                    Method_Call_Exp (expFrom (get rhs 2)) 
                      (getText rhs 3) (expListFrom (get rhs 5)) ),

      rule "Expression -> super identifier ( ZeroMoreExpression )"
        (\rhs -> return $ fromExp $ 
                    Super_Call_Exp (getText rhs 2)  (expListFrom (get rhs 4)) ),

      rule "Expression -> integer_number"
        (\rhs -> return $ fromExp $ Const_Exp (read (getText rhs 1) :: Int)),
      
      rule "Expression -> - integer_number"
        (\rhs -> return $ fromExp $ Const_Exp (-(read (getText rhs 2) :: Int))),
      
      rule "Expression -> - ( Expression , Expression )"
        (\rhs -> return $ fromExp $ 
                  Diff_Exp (expFrom (get rhs 3)) (expFrom (get rhs 5))),

      rule "Expression -> + ( Expression , Expression )"
        (\rhs -> return $ fromExp $ 
                  Sum_Exp (expFrom (get rhs 3)) (expFrom (get rhs 5))),
  
      rule "Expression -> zero? ( Expression )"
        (\rhs -> return $ fromExp $ IsZero_Exp (expFrom (get rhs 3))),
      
      rule "Expression -> if Expression then Expression else Expression"
        (\rhs -> return $ fromExp $ 
                  If_Exp (expFrom (get rhs 2)) 
                    (expFrom (get rhs 4)) 
                    (expFrom (get rhs 6))),

      rule "Expression -> identifier" 
        (\rhs -> return $ fromExp $ Var_Exp (getText rhs 1)),
      
      rule "Expression -> let LetBindings in Expression"
        (\rhs -> return $ fromExp $ 
                  Let_Exp (idExpListFrom (get rhs 2)) (expFrom (get rhs 4))),

      rule "Expression -> letrec LetRecBindings in Expression"
        (\rhs -> return $ fromExp $ 
                  Letrec_Exp (idIdListExpListFrom (get rhs 2)) (expFrom (get rhs 4))),

      rule "Expression -> proc ( ZeroMoreIdentifier ) Expression"
        (\rhs -> return $ fromExp $ 
                  Proc_Exp (idListFrom (get rhs 3)) (expFrom (get rhs 5))),

      rule "Expression -> ( Expression ExpressionListSpace )"
        (\rhs -> return $ fromExp $ 
                  Call_Exp (expFrom (get rhs 2)) (expListFrom (get rhs 3))),

      rule "Expression -> begin ExpressionList end"
        (\rhs -> return $ fromExp $ 
                  Block_Exp (expListFrom (get rhs 2))),

      rule "Expression -> set identifier = Expression"
        (\rhs -> return $ fromExp $ 
                  Set_Exp (getText rhs 2) (expFrom (get rhs 4))),

      rule "Expression -> list ( ZeroMoreExpression )"
        (\rhs -> return $ fromExp $ 
                  List_Exp (expListFrom (get rhs 3))),

      {- Let bindings -}
      rule "LetBindings -> "
      (\rhs -> return $ fromIdExpList []),

      rule "LetBindings -> identifier = Expression LetBindings"
        (\rhs -> return $ fromIdExpList
                  ((getText rhs 1, expFrom (get rhs 3)) : idExpListFrom (get rhs 4))),

      {- Letrec bindings -}
      rule "LetRecBindings -> identifier ( ZeroMoreIdentifier ) = Expression"
        (\rhs -> return $ fromIdIdListExpList 
                  [(getText rhs 1, idListFrom (get rhs 3), expFrom (get rhs 6))]),

      rule "LetRecBindings -> identifier ( ZeroMoreIdentifier ) = Expression LetRecBindings"
        (\rhs -> return $ fromIdIdListExpList 
                  ((getText rhs 1, idListFrom (get rhs 3), expFrom (get rhs 6)) 
                      : idIdListExpListFrom (get rhs 7)))
    ],
    
    baseDir        = "./",
    actionTblFile  = "action_table_classes.txt",
    gotoTblFile    = "goto_table_classes.txt",
    grammarFile    = "prod_rules_classes.txt",
    parserSpecFile = "mygrammar_classes.grm",
    genparserexe   = "yapb-exe"
  }


