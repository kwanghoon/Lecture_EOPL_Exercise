module TypeCheckerTest where

import Expr

typechecker_tests :: TypeDeclTestSuite
typechecker_tests =
  TypeDeclTestSuite
   [
     -- simple arithmetic
     
     TDTC "positive-const" "11" (Just TyInt),
     TDTC "negative-const" "-33" (Just TyInt),
     TDTC "simple-arith-1" "-(44,33)" (Just TyInt),
  
     -- nested arithmetic
     TDTC "nested-arith-left" "-(-(44,33),22)" (Just TyInt),
     TDTC "nested-arith-right" "-(55, -(22,11))" (Just TyInt),
  
     -- simple variables
     TDTC "test-var-1" "x" (Just TyInt),
     TDTC "test-var-2" "-(x,1)" (Just TyInt),
     TDTC "test-var-3" "-(1,x)" (Just TyInt),

     TDTC "zero-test-1" "zero?(-(3,2))" (Just TyBool),
     TDTC "zero-test-2" "-(2,zero?(0))" Nothing,
      
     -- simple unbound variables
     TDTC "test-unbound-var-1" "foo" Nothing,
     TDTC "test-unbound-var-2" "-(x,foo)" Nothing,
  
     -- simple conditionals
     TDTC "if-true" "if zero?(1) then 3 else 4" (Just TyInt),
     TDTC "if-false" "if zero?(0) then 3 else 4" (Just TyInt),

     -- make sure that the test and both arms get evaluated
     -- properly. 
     TDTC "if-eval-test-true" "if zero?(-(11,12)) then 3 else 4" (Just TyInt),
     TDTC "if-eval-test-false" "if zero?(-(11, 11)) then 3 else 4" (Just TyInt),
     TDTC "if-eval-then" "if zero?(1) then -(22,1) else -(22,2)" (Just TyInt),
     TDTC "if-eval-else" "if zero?(0) then -(22,1) else -(22,2)" (Just TyInt),
      
     -- make sure types of arms agree (new for lang5-1)
     TDTC "if-compare-arms" "if zero?(0) then 1 else zero?(1)" Nothing,
     TDTC "if-check-test-is-boolean" "if 1 then 11 else 12" Nothing,

     -- simple let
     TDTC "simple-let-1" "let x = 3 in x" (Just TyInt),

     -- make sure the body and rhs get evaluated
     TDTC "eval-let-body" "let x = 3 in -(x,1)" (Just TyInt),
     TDTC "eval-let-rhs" "let x = -(4,1) in -(x,1)" (Just TyInt),

     -- check nested let and shadowing
     TDTC "simple-nested-let" "let x = 3 in let y = 4 in -(x,y)" (Just TyInt),
     TDTC "check-shadowing-in-body" "let x = 3 in let x = 4 in x" (Just TyInt),
     TDTC "check-shadowing-in-rhs" "let x = 3 in let x = -(x,1) in x" (Just TyInt),

     -- simple applications
     TDTC "apply-proc-in-rator-pos" "(proc(x : int) -(x,1)  30)" (Just TyInt),
     TDTC "checker-doesnt-ignore-type-info-in-proc"
        "(proc(x : (int -> int)) -(x,1)  30)"
        Nothing,
      
     TDTC "apply-simple-proc" "let f = proc (x : int) -(x,1) in (f 30)" (Just TyInt),
     TDTC "let-to-proc-1" "(proc(f : (int -> int))(f 30)  proc(x : int)-(x,1))" (Just TyInt),


     TDTC "nested-procs" "((proc (x : int) proc (y : int) -(x,y)  5) 6)" (Just TyInt),
     TDTC "nested-procs2"
        "let f = proc (x : int) proc (y : int) -(x,y) in ((f -(10,5)) 3)"
        (Just TyInt),
      
     -- simple letrecs
     TDTC "simple-letrec-1" "letrec int f(x : int) = -(x,1) in (f 33)" (Just TyInt),
     TDTC "simple-letrec-2"
        "letrec int f(x : int) = if zero?(x) then 0 else -((f -(x,1)), -2) in (f 4)"
        (Just TyInt),

     TDTC "simple-letrec-3"
        "let m = -5 \
           \ in letrec int f(x : int) = if zero?(x) then -((f -(x,1)), m) else 0 in (f 4)"
        (Just TyInt),

     TDTC "double-it" "letrec int double (n : int) = if zero?(n) then 0 \
                                \ else -( (double -(n,1)), -2) \
                                \ in (double 3)"
        (Just TyInt),

     -- tests of expressions that produce procedures
     TDTC "build-a-proc-typed" "proc (x : int) -(x,1)" (Just (TyFun TyInt TyInt)),

     TDTC "build-a-proc-typed-2" "proc (x : int) zero?(-(x,1))" (Just (TyFun TyInt TyBool)),
      
     TDTC "bind-a-proc-typed"
        "let f = proc (x : int) -(x,1) in (f 4)"
        (Just TyInt),

     TDTC "bind-a-proc-return-proc"
        "let f = proc (x : int) -(x,1) in f"
        (Just (TyFun TyInt TyInt)),

     TDTC "type-a-ho-proc-1"
        "proc(f : (int -> bool)) (f 3)"
        (Just (TyFun (TyFun TyInt TyBool) TyBool)),

     TDTC "type-a-ho-proc-2"
        "proc(f : (bool -> bool)) (f 3)"
        Nothing,

     TDTC "apply-a-ho-proc"
        "proc (x : int) proc (f : (int -> bool)) (f x)"
        (Just (TyFun TyInt (TyFun (TyFun TyInt TyBool) TyBool))),

     TDTC "apply-a-ho-proc-2"
        "proc (x : int) proc (f : (int -> (int -> bool))) (f x)"
        (Just (TyFun TyInt (TyFun (TyFun TyInt (TyFun TyInt TyBool)) (TyFun TyInt TyBool)))),

     TDTC "apply-a-ho-proc-3"
        "proc (x : int) proc (f : (int -> (int -> bool))) (f zero?(x))"
        Nothing,

     TDTC "apply-curried-proc"
        "((proc(x : int) proc (y : int)-(x,y)  4) 3)"
        (Just TyInt),

     TDTC "apply-a-proc-2-typed"
        "(proc (x : int) -(x,1) 4)" 
        (Just TyInt),

     TDTC "apply-a-letrec" " \
             \ letrec int f(x : int) = -(x,1) \
             \ in (f 40)"
                     (Just TyInt),

     TDTC "letrec-non-shadowing"
                "(proc (x : int) \
                   \ letrec bool loop(x : bool) =(loop x) \ 
                   \  in x \
                   \ 1)"
        (Just TyInt),

      
     TDTC "letrec-return-fact" " \
               \ let times = proc (x : int) proc (y : int) -(x,y) \   
               \ in letrec \
               \      int fact(x : int) = if zero?(x) then 1 else ((times x) (fact -(x,1))) \
               \    in fact"
        (Just (TyFun TyInt TyInt)),

     TDTC "letrec-apply-fact" " \
          \ let times = proc (x : int) proc (y : int) -(x,y) \
          \ in letrec \
          \     int fact(x : int) = if zero?(x) then 1 else ((times x) (fact -(x,1))) \
          \   in (fact 4)"
        (Just TyInt)

   ]
