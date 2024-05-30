module MainUtil where

import CommonParserUtil

import TokenInterface
import Lexer
import Terminal
import Parser
import Expr
import Interp
import Scheduler(timeslice)

import Control.Monad (when)
import System.IO
import System.Environment (getArgs, withArgs)


parser text = do
    parsing False                            -- parser converting a text-based program
       parserSpec ((), 1, 1, text)           -- into a program in abstract syntax tree (Expr)
       (aLexer lexerSpec)
       (fromToken (endOfToken lexerSpec))

runProg text bool = do 
  expression <- parser text

  if bool then putStrLn (show expression) else return ()
  
  let val = value_of_program expression timeslice     -- interpreter
  putStrLn (show val)
  
run text = runProg text True
