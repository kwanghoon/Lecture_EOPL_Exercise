Chapter 1. Inductive Sets of Data

[Summary]

 - Section 1.1 and 1.2

   * introduce methods for inductiviely specifying data structures and 

   * show how such specifications may be used to guide the
     construction of recursive programs

 - Section 1.3

   * shows how to extend these techniques to more complex problems.


> module Main where

> import Lib
> import Data.Array

> main :: IO ()
> main = putStrLn "1. Inductive Sets of Data"



[Section 1.1 Recursively Specified Data]

- Inductive specification

   * a method of specifying a set of values.

   * Three styles: a top-down definition, a bottome-up definition, rules of inference

- Example 1: S

   * Let us define S = {0, 3, 6, 9, 12, ... }  which is a subset of N = {0, 1, 2, ... }.

- Def. 1.1.1 Top-down definition

   * A natural number n is in S if and only if

      1. n = 0 or

      2. n - 3 is in S


   * We can use this definition to write a procedure to decide
      whether a natural number n is in S.

> in_S :: Int -> Bool
> in_S n = error "TODO: implement an in_s function"

- Def. 1.1.2 Bottom-up definition

   * Define the set S to be the smallest contained in N and
      satisfying the following two properties:

      1. 0 is in S, and

      2. if n is in S then n + 3 is in S.


- Rules of inference:

   (1) -------------
         0 is in S


          n is in S
   (2) -----------------
         (n+3) is in S


- Example 2: list of integers

   * Definition 1.1.3 (Top-down)

   * Definition 1.1.4 (Bottom-up)

   * Definition 1.1.5 (rules of inference)


- Defining sets using grammars

   * ListOfInt ::= ()
               ::= (Int . ListOfInt)

   * ListofInt ::= ({Int}*)


     cf. Kleen star

          {-}* : repeat zero or more
          {-}+ : repeat one or more
          {-}*(c) : repeat zero or more separated by c


> data ListOfInt = Nil | Cons Int ListOfInt


- Example 3: s-list (also called s-exp)

   * Definition 1.1.6

      S-list ::= ({S-exp}*)

      S-exp  ::= Symbol | S-list


> data S_List = S_List [S_Exp]
> 
> type Symbol = String
> 
> data S_Exp  = S_Exp_Symbol Symbol | S_Exp_S_List S_List


- Example 4: binary tree

   * Definition 1.1.7

      Bintree ::= Int
              ::= Symbol Bintree Bintree


> data BinTree = Leaf Int | Node Symbol BinTree BinTree


- Example 5: lambda expression

   * Definition 1.1.8

      LCExp ::= Identifier
            ::= (lambda (Identifier) LcExp)
            ::= (LcExp LcExp)

> type Identifier = String
> 
> data LcExp = Var Identifier
>            | Lam Identifier LcExp
>            | App LcExp LcExp


- Induction

  * Having described set inductively, we can use the inductive
     definitions in two ways:

   (1) to prove theorems about members of the set and

   (2) to write programs that manipulate them.


[Section 1.2 Deriving Recursive Programs]

- Principle 1: The smaller-subproblem principle

  * If we can reduce a problem to a smaller subproblem, we can call
     the procedure that solves the problem to solve the subproblem.


> list_length :: [a] -> Int
> list_length = error "TODO: implement an list_length function"


  *   list_length [1,2,3]
    = list_length (1:(2:(3:[])))
    = 1 + list_length (2:(3:[]))
    = 1 + 1 + list_length (3:[])
    = 1 + 1 + 1 + list_length []
    = 1 + 1 + 1 + 0
    = 3

> nth_element :: [a] -> Int -> a
> nth_element = error "TODO: implement an nth_element function" []
>
> report_list_too_short :: Int -> a
> report_list_too_short n = error ("List too short by " ++ show (n+1) ++ " elements.")


  *   nth_element ['a','b','c','d','e'] 3
    = nth_element ['b','c','d','e'] 2
    = nth_element ['c','d','e'] 1
    = nth_element ['d','e'] 0
    = 'd'


> remove_first :: a -> [a] -> [a]
> remove_first x list = error "implement remove_first!"


> occurs_free :: Identifier -> LcExp -> Bool
> occurs_free x exp = error "implement occurs_free"


> subst :: Symbol -> Symbol -> S_List -> S_List
> subst new old slist = error "implement subst"

> substInS_Exp :: Symbol -> Symbol -> S_Exp -> S_Exp
> substInS_Exp new old sexp = error "implement substInS_Exp"



- Principle 2: Follow the Grammar!

  * When defining a procedure that operates on inductively defined
     data, the structure of the program should be patterned after the
     structure of the data.


  * Write one procedure for each nonterminal in the grammar.

  * In each procedure, write one alternative for each production
     corresponding to that nonterminal.


[Section 1.3 Auxiliary Procedures and Context Arguments]


- The Follow-the-Grammar recipe is powerful, but sometimes it is not
   sufficient.

  *  number_elements :: [a] -> [(Int,a)]
     number_elements [v0, v1, v2, ...] = [(0,v0), (1,v1), (2,v2), ...]

  => There is no obvious way

      to build number_elements (x:xs)

      from number_elements xs!

- To solve this problem, we need to generalize the problem. We write a
   new procedure number_elements_from that takes an additional
   argument n that specifies the number to start from.

> number_elements_from :: [a] -> Int -> [(Int,a)]
> number_elements_from lst n = error "implement number_elements_from"
>
> number_elements :: [a] -> [(Int,a)]
> number_elements lst = number_elements_from lst 0


  * In number_elements_from, the first argument lst is the list we are
      working on and it gets smaller at every recursive call.

    The second argument, however, is a context argument (or called
       inherited attribute) where we are working.


- Example 1: list_sum

> list_sum :: [Int] -> Int
> list_sum = error "implement a list_sum function"


- Example 2: vector_sum (or array_sum)

   Learn Haskell array before attempting to solve the following exercise.

> partial_array_sum :: Array Int Int -> Int -> Int
> partial_array_sum = error "implement a partial_array_sum function"
>
> -- cf. arr ! i is an indexing operation as arr[i]
> 
> array_sum :: Array Int Int -> Int
> array_sum arr = partial_array_sum arr (len_array arr)
>
> len_array arr = let (lower,upper) = bounds arr in upper - lower
> 
> -- cf. bounds arr should be the form of (0,n) starting from 0!

