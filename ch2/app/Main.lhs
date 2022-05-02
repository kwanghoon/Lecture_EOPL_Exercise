Chapter 2. Data Abstraction


> module Main where

> import Lib
> import Data.Maybe(isJust) -- for parser
> import Text.Read(readMaybe)  -- for parser

> main :: IO ()
> main = putStrLn "Chapter 2. Data Abstraction"


[Summary]

 - Section 2.1

    * Abstract data type

 - Section 2.2

    * Examples of abstract data type for Environments

      (1) The environment interface

      (2) Data structure representation

      (3) Procedural representation

 - Section 2.3

    * Examples of abstract data type for lambda calculus expressions

 - Section 2.4

    * A tool for defining recursive data types (in Scheme)

      => In Haskell, the language supports this tool, and no extra tool is required!

 - Section 2.5

    * Concrete syntax vs. Abstract syntax

    * Parse : Text in the concrete syntax ===> Abstract syntax tree

      Unparse : Abstarct syntax tree ===> Text in the concrete syntax


[Section 2.1 Specifying Data via Interfaces]


- Abstract data type

  : The interface tells us

      what the data of the type represents,

      what the operations on the data are, and

      what properties these operations may be relied on to have.

  : The implementation provides

      a specific representation of the data and

      code for the operations that make use of that data representation


- Benefits of abstract data type

  : A client code becomes representation-independent it it only uses the interface.


- A simple example: the abstract data type of natural numbers


  : zero             (to create the zero value)

  : is-zero?         (to test if a given value is zero)

  : successor        (to create the successor of a given value)

  : predecessor      (to create the predecessor of a given value)


  (1) Haskell type class interface


> class NaturalNum a where
>   zero        :: a
>   is_zero     :: a -> Bool
>   successor   :: a -> a
>   predecessor :: a -> a


      What the data of the type represents : a
      What the operations on the data are  : zero, is_zero, successor, predecessor
      What properties these operations may be relied on to have :

        is_zero n == true  if n is zero
                  == false if n is not zero

        successor n == n+1  (n>=0)

        predecessor (n+1) == n  (n>=0)


- Many possible representations of the interface


  (1) Unary list representation

>
> data UnaryList = UnaryNil | UnaryCons () UnaryList
> 
> instance NaturalNum UnaryList where
>   zero        = zero_list
>   is_zero     = is_zero_list
>   successor   = successor_list
>   predecessor = predecessor_list
>
>
> 
> zero_list :: UnaryList
> zero_list = UnaryNil
>
> is_zero_list :: UnaryList -> Bool
> is_zero_list UnaryNil         = True
> is_zero_list (UnaryCons x xs) = False
>
> successor_list :: UnaryList -> UnaryList
> successor_list xs = UnaryCons () xs
>
> predecessor_list :: UnaryList -> UnaryList
> predecessor_list (UnaryCons x xs) = xs
> predecessor_list _                = undefined


    ghci> is_zero (successor zero :: UnaryList)
    False

    ghci> is_zero (zero :: UnaryList)
    True

    ghci> is_zero (predecessor (successor zero) :: UnaryList)
    True



  (2) Haskell number representation

> instance NaturalNum Int where
>   zero        = zero_int
>   is_zero     = is_zero_int
>   successor   = successor_int
>   predecessor = predecessor_int


> zero_int :: Int
> zero_int = 0
>
> is_zero_int :: Int -> Bool
> is_zero_int 0 = True
> is_zero_int n = False
>
> successor_int n 
>   | n>=0      = n+1
>   | otherwise = undefined
>
> predecessor_int n
>   | n-1 >= 0  = n-1
>   | otherwise = undefined

     ghci> is_zero (successor zero :: Int)
     False

     ghci> is_zero (zero :: Int)
     True

     ghci> is_zero (predecessor (successor zero) :: Int)
     True

     Q. Can you explain why is_zero zero causes a type error in an intuitive manner?


  (3) Bignum representation

> data Bignum16 = Bignum16Nil | Bignum16Cons Int Bignum16
>
> instance NaturalNum Bignum16 where
>   zero        = zero_bignum16
>   is_zero     = is_zero_bignum16
>   successor   = successor_bignum16
>   predecessor = predecessor_bignum16
> 
>
> zero_bignum16 :: Bignum16
> zero_bignum16 = Bignum16Nil
>
> is_zero_bignum16 :: Bignum16 -> Bool
> is_zero_bignum16 Bignum16Nil = True
> is_zero_bignum16 _           = False
>
> successor_bignum16 :: Bignum16 -> Bignum16
> successor_bignum16 xs = do_implement
> 
> predecessor_bignum16 :: Bignum16 -> Bignum16
> predecessor_bignum16 xs = do_implement
>
> do_implement = error "please do implement!"
>

   ghci> is_zero (zero :: Bignum16)
   True

   ghci> :type is_zero (successor zero :: Bignum16)
   is_zero (successor zero :: Bignum16) :: Bool

   ghci> is_zero (successor zero :: Bignum16)
   False

   ghci> :type is_zero (predecessor (successor zero) :: Bignum16)
   is_zero (predecessor (successor zero) :: Bignum16) :: Bool

   ghci> is_zero (predecessor (successor zero) :: Bignum16)
   True


   Q. In the Haskell implementations, the type information such as
        Int, UnaryList, and Bignum16 was exposed, which is
        undesirable.  As a Haskell extension, the existential types
        can hide such type information as well.

        Please self-study what is the feature of the existential types
         and how it can be applied for our purpose.

        Hint: Goolgling with "haskell existential types"


[Section 2.2 Representation Strategies for Data Type]

- Topics

  : The environment interface

  : Data structure representation

  : Procedural representation


- What is environments?

   : In an interpreter, an environment associates each variable with a value.

      env = { (var1, val1), ..., (vark, valk) }

      e.g., env1 = { (x, 123), (i, 0), (s, "xyz"), (l, [1,2,3]), (c, 'K') }

   : In a compiler, an environment associates each variable with a type.

      env = { (var1, type1), ..., (vark, typek) }

      e.g., env1 = { (x, Int), (i, Int), (s, String), (l, [Int]), (c, Char) }


   Note tha variables may be presented in any way so long as we can
    check two variables for equality.

- The abstract data type of environments

  : empty-env         (to create the empty environment)

  : apply-env f var

      (to get the bound value of the variable var in the environment f)

  : extend-env var v f

      (to extend the environment f with a pair of variable and value, (var,v))



  (1) Haskell type class interface

> type Var = String
>
> data Value = IntValue Int | StrValue String | CharValue Char deriving Show
>
> class Environment env where
>    empty_env   :: env
>    apply_env   :: env -> Var -> Value
>    extend_env  :: Var -> Value -> env -> env
>


- Two possible representations of the interface

  (1) Data structure

> data EnvDS = EnvDS_Nil | EnvDS_Cons Var Value EnvDS
>
> instance Environment EnvDS where
>   empty_env  = empty_env_ds
>   apply_env  = apply_env_ds
>   extend_env = extend_env_ds
>
> 
> empty_env_ds :: EnvDS
> empty_env_ds = error "TODO: implement an empty_env_ds function"
>
> apply_env_ds :: EnvDS -> Var -> Value
> apply_env_ds = error "TODO: implement an apply_env_ds function"
>
> extend_env_ds :: Var -> Value -> EnvDS -> EnvDS
> extend_env_ds = error "TODO: implement an extend_env_ds function"
> 

     ghci> apply_env (extend_env "x" (IntValue 123) empty_env :: EnvDS) "x"
     IntValue 123

     ghci> apply_env (extend_env "x" (IntValue 123) empty_env :: EnvDS) "y"
     *** Exception: y is not found.


  (2) Procedural representation

> data EnvFun = EnvFun (Var -> Value)
>
> instance Environment EnvFun where
>   empty_env  = empty_env_fun
>   apply_env  = apply_env_fun
>   extend_env = extend_env_fun
>
> empty_env_fun :: EnvFun
> empty_env_fun = error "TODO: implement an empty_env_fun function"
>
> apply_env_fun :: EnvFun -> Var -> Value
> apply_env_fun (EnvFun envfun) x = error "TODO: implement an apply_env_fun function"
> 
> extend_env_fun :: Var -> Value -> EnvFun -> EnvFun
> extend_env_fun x v (EnvFun envfun) =
>   error "TODO: implement an extend_env_fun function"


    ghci> apply_env (extend_env "x" (IntValue 123) empty_env :: EnvFun) "x"
    IntValue 123

    ghci> apply_env (extend_env "x" (IntValue 123) empty_env :: EnvFun) "y"
    *** Exception: x is not found.



[Section 2.3 Interfaces for Recursive Data Types]


- A recursive data type for lambda calculus expressions

> type Identifier = String
> 
> data Lc_exp = Var_exp    Identifier
>             | Lambda_exp Identifier Lc_exp
>             | App_exp    Lc_exp     Lc_exp
>             deriving Show
>

- Examples

  (1) x                    in Haskell

    Var_exp "x"                                   


  (2) \x -> x              in Haskell

    Lambda_exp "x" (Var_exp "x")                  


  (3) (\x -> x) (\y -> y)  in Haskell

    App_exp (Lambda_exp "x" (Var_exp "x"))        
            (Lambda_exp "y" (Var_exp "y"))



- Example: A procedure occur_free to compute a set of free variables
   in a given lambda expression.

 
[Section 2.4 A Tool for Defining Recursive Data Types]

- A tool for automatically constructing and implementing such
   interfaces one discussed in Scheme.

    (define-datatype lc-exp lc-exp?
        (var-exp (var identifier?))
        (lambda-exp (bound-var identifier?))
        (app-exp (rator lc-exp?) (rand lc-exp?)))


  ==> In Haskell, it is already supported by the data declaration that
    was shown previously. So, there is nothing to explain on the section. :=)


[Section 2.5 Abstract Syntax and Its Representation]

- Concrete syntax (defined by a grammar) vs. Abstract syntax (by data declaration)

  : The concrete syntax is an external representation for humans.

  : The abstract syntax is an internal one for computers.

  : See Figure 2.2 for a comparison.


- Parsing is a task to deriving the corresponding abstract syntax tree
   from the concrete syntax which is a sequence of characters:

  : Parser

  : parse_expression : String -> LcExp


- The reverse task of parsing is called unparsing or "pretty-printing"

  : Unparser

  : unparse_lc_exp : LcExp -> String


  SchemeVal : S-expressions (either a symbol or a list of S-expressions)

> data SchemeVal = S String | L [SchemeVal]  deriving Show
>
> type Env = [(String,SchemeVal)]
>
> tokenize :: String -> [String]
> tokenize str = words $ concat $ map (\ch -> if ch=='(' || ch==')' then " " ++ [ch] ++ " " else [ch]) str

> parse :: String -> SchemeVal
> parse program = fst $ read_from_tokens $ tokenize $ program

> read_from_tokens :: [String] -> (SchemeVal, [String])
> read_from_tokens [] = error "Syntax error: unexpected EOF while reading"
> read_from_tokens ("(":tokens) = 
>     let build list [] = (list, [])
>         build list (token:tokens) =
>             if token /= ")"
>             then let (list',tokens') = read_from_tokens (token:tokens)
>                  in  build (list ++ [list']) tokens'
>             else (list, tokens)
>         (list'', tokens'') = build [] tokens
>     in (L list'', tokens'')
> read_from_tokens (")":tokens) = error "Syntax error: unexpected )"
> read_from_tokens (token:tokens) = (atom token, tokens)
> 
> atom :: String -> SchemeVal
> atom token =  S token
>
> unparse :: SchemeVal -> String
> unparse (S s) = s
> unparse (L []) = "()"
> unparse (L [obj]) = "(" ++ unparse obj  ++  ")"
> unparse (L (obj:objs)) = "(" ++ concat (unparse obj : map ((" "++) . unparse) objs)  ++  ")"


Exercise 2.27

  ;;   ((lambda (a) (a b)) c)
  
  ;;   (lambda (x)
  ;;     (lambda (y)
  ;;       ((lambda (x)
  ;;          (x y))
  ;;        x)))


  ghci> :type tokenize
  tokenize :: String -> [String]

  ghci> tokenize "((lambda (a) (a b)) c)"
  ["(","(","lambda","(","a",")","(","a","b",")",")","c",")"]


  ghci> read_from_tokens (tokenize "((lambda (a) (a b)) c)")
  (L [L [S "lambda",L [S "a"],L [S "a",S "b"]],S "c"],[])

  ghci> read_from_tokens (tokenize "(lambda (x) (lambda (y) ((lambda (x) (x y)) x)))")
  (L [S "lambda",L [S "x"],L [S "lambda",L [S "y"],L [L [S "lambda",L [S "x"],L [S "x",S "y"]],S "x"]]],[])



> parse_expression :: SchemeVal -> Lc_exp
> parse_expression (S v) = Var_exp v
> parse_expression (L (S "lambda":(L [S v]):[body])) =  -- No multiple arguments are supported!!
>  Lambda_exp v (parse_expression body)
> parse_expression (L [f,arg]) =
>  App_exp (parse_expression f) (parse_expression arg)
> parse_expression obj = error ("invalid concrete syntax: " ++ show obj)


  ghci> parse_expression (fst (read_from_tokens (tokenize "((lambda (a) (a b)) c)")))
  App_exp (Lambda_exp "a" (App_exp (Var_exp "a") (Var_exp "b"))) (Var_exp "c")


  ghci> parse_expression (fst (read_from_tokens (tokenize "(lambda (x) (lambda (y) ((lambda (x) (x y)) x)))")))
  Lambda_exp "x" (Lambda_exp "y" (App_exp (Lambda_exp "x" (App_exp (Var_exp "x") (Var_exp "y"))) (Var_exp "x")))


> unparse_lc_exp :: Lc_exp -> SchemeVal
> unparse_lc_exp (Var_exp v) = S v
> unparse_lc_exp (Lambda_exp v body) =
>   L [S "lambda", L [S v], unparse_lc_exp body]
> unparse_lc_exp (App_exp fun arg) =
>   L [unparse_lc_exp fun, unparse_lc_exp arg]
>


   ghci> unparse (unparse_lc_exp (parse_expression (fst (read_from_tokens (tokenize "((lambda (a) (a b)) c)")))))

   "((lambda (a) (a b)) c)"


   ghci> unparse (unparse_lc_exp (parse_expression (fst (read_from_tokens (tokenize "(lambda (x) (lambda (y) ((lambda (x) (x y)) x)))")))))
   "(lambda (x) (lambda (y) ((lambda (x) (x y)) x)))"

