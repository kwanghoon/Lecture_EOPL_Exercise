module Main where

import Expr
import TypeCheck
import TypeCheckerTest
  
import CommonParserUtil
import TokenInterface
import Lexer
import Parser

import Test.Hspec
import Control.Exception(try, throw, SomeException)

main :: IO ()
main = 
  hspec $ do 
    describe "exceptions" $ do
      let atdir f = "checkedlang:" ++ f
      let TypeDeclTestSuite typechecker_tests' = typechecker_tests

      mapM_ 
       (\tdtcArg@(TDTC tcname _ maybeResult) -> 
          (it(atdir(tcname)) $ do
             result <- try (doTest tdtcArg) :: IO (Either SomeException ())
             case result of
               Left exn -> throw exn `shouldBe` maybeResult
               Right () -> putStr ""
          )
       )
       typechecker_tests'
  
doTest (TDTC tcname expr_text maybeResult) =
  do  -- putStr $ tcname ++ " : "

      expressionAst <-
          parsing False
              parserSpec ((), 1, 1, expr_text)
              (aLexer lexerSpec)
              (fromToken (endOfToken lexerSpec))

      let expression = fromASTExp expressionAst

      -- Just to add the type of x!
      case maybeResult of 
        Just ty' ->
          do eitherTyOrErr <- typeCheck (Let_Exp "x" (Const_Exp 1) expression)
             case eitherTyOrErr of
              Left errMsg ->
                putStrLn ("Expected " ++ show ty' ++ " but got " ++ errMsg ++ " in " ++ show expression)
              Right ty -> 
                if equalType ty ty'
                    then putStr "" -- putStrLn "Successfully typechecked."
                    else putStrLn ("Expected " ++ show ty' ++ " but got " ++ show ty ++ " in " ++ show expression)
        Nothing ->
          do eitherTyOrErr <- typeCheck (Let_Exp "x" (Const_Exp 1) expression)
             case eitherTyOrErr of
              Left errMsg -> putStr "" -- putStrLn "Successfully type-unchecked." -- Is it the same error?
              Right ty -> putStr "" -- putStrLn "Should not be typechecked."
