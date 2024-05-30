module Main where

import MainUtil

import Expr
import Test.Hspec
import System.IO (readFile)
import Control.Exception (evaluate)

spec = hspec $ do
  describe "explicitrefslang" $ do
    let atdir f = "./app/explicitrefslang/examples/" ++ f

    mapM_
      (\(name, maybeResStr) -> 
           (it (name) $
              do text <- readFile name
                 
                 case maybeResStr of
                   Just resultStr -> do result <- runProg text False
                                        show result `shouldBe` resultStr
                   Nothing -> (do result <- runProg text False; putStrLn (show result)) `shouldThrow` anyException))
      [ (atdir name, maybeResStr) | (name,maybeResStr) <- testcases ]
         
main = spec

testcases = 
  [
    -- simple arithmetic
    ("positive_const.let", Just "11"),
    ("negative_const.let", Just "-33"),
    ("simple_arith_1.let", Just "11"),
    
    -- nested arithmetic
    ("nested_arith_left.let", Just "-11"),
    ("nested_arith_right.let", Just "44"),

    -- simple variables
    ("test_var_1.let", Just "10"),
    ("test_var_2.let", Just "9"),
    ("test_var_3.let", Just"-9"),

    -- simple unbound variables
    ("test_unbound_var_1.let", Nothing),
    ("test_unbound_var_2.let", Nothing),

    -- simple conditionals
    ("if_true.let", Just "3"),
    ("if_false.let", Just "4"),

    -- test dynamic typechecking
    ("no_bool_to_diff_1.let", Nothing),
    ("no_bool_to_diff_2.let", Nothing),
    ("no_int_to_if.let", Nothing),

    -- make sure that the test and both arms get evaluated properly
    ("if_eval_test_true.let", Just "3"),
    ("if_eval_test_false.let", Just "4"),

    -- and make sure the other arm doesn't get evaluated
    ("if_eval_test_true_2.let", Just "3"),
    ("if_eval_test_false_2.let", Just "4"),

    -- simple let
    ("simple_let_1.let", Just "3"),

    -- make sure the body and rhs get evaluated
    ("eval_let_body.let", Just "2"),
    ("eval_let_rhs.let", Just "2"),

    -- check nested let and shadowing
    ("simple_nested_let.let", Just "-1"),
    ("check_shadowing_in_body.let", Just "4"),
    ("check_shadowing_in_rhs.let", Just "2"),

    -- simple applications
    ("simple_app_1.proc", Just "29"),
    ("simple_app_2.proc", Just "29"),
    ("simple_app_3.proc", Just "29"),

    ("nested_procs_1.proc", Just "-1"),
    ("nested_procs_2.proc", Just "-1"),

    -- y combinator
    ("y_combinator_1.proc", Just "12"),


    -- simple letrecs
    ("simple_letrec_1.letrec", Just "32"),
    ("simple_letrec_2.letrec", Just "8"),
    ("simple_letrec_3.letrec", Just "20"),

    -- 
    ("ho_nested_letrecs.letrec", Just "1"),

    --begin
    ("begin_test_1.expref", Just "3"),

    --gensym test
    ("gensym_test_1.expref", Just "-1"),
    ("gensym_test_2.expref", Just "-1"),

    --simple store
    ("simple_store_test_1.expref", Just "17"),

    --assginment
    ("assignment_test_1.expref", Just "27"),

    --even odd via set
    ("even_odd_via_set_1.expref", Just "1"),

    --show allocation
    ("show_allocation_1.expref", Just "11"),

    --chains
    ("chains_1.expref", Just "11")

  ]
