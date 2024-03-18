module Token(Token(..), keywords) where

import Prelude hiding(EQ)
import TokenInterface

data Token =
    END_OF_TOKEN
    
  | INTEGER_NUMBER              -- number
  
  | SUB                         -- - ( expr1, expr2 )
  | PLUS                        -- + ( expr1, expr2 )
  | OPEN_PAREN  | CLOSE_PAREN
  | COMMA

  | ISZERO                      -- zero? ( expr )

  | IF                          -- if expr1 then expr2 else expr3
  | THEN
  | ELSE
  
  | LET                         -- let identifier = expr1 in expr2
  | IN                            
  | EQ
  
  | LETREC                      -- letrec identifier ( identifier )= expr1 in expr2

  | PROC                        -- proc ( identifier ) expr
                                -- (expr1 expr2)

  | IDENTIFIER                  -- identifier
  
  | BEGIN                       -- begin ..;..;.. end
  | END
  | SEMICOLON

  | SET                         -- set
  | LIST                         -- list

  -- new tokens in classes

  | CLASS  -- class
  | EXTENDS  -- extends
  | METHOD -- method
  | FIELD  -- field
  | NEW    -- new
  | SEND   -- send
  | SELF   -- self
  | SUPER  -- super
  deriving (Eq, Show)

tokenStrList :: [(Token,String)]
tokenStrList =
  [ (END_OF_TOKEN, "$"),
    
    (INTEGER_NUMBER, "integer_number"),
    
    (SUB, "-"),
    (PLUS, "+"),
    (OPEN_PAREN, "("),
    (CLOSE_PAREN, ")"),
    (COMMA, ","),
    (ISZERO, "zero?"),
    (IDENTIFIER, "identifier"), 
    (EQ, "="),
    (SEMICOLON, ";")
  ] ++ keywords

keywords :: [(Token, String)]
keywords =
  [
    (LET, "let"), 
    (IN, "in"), 
    (LETREC, "letrec"),
  
    (IF, "if"), 
    (THEN, "then"), 
    (ELSE, "else"), 

    (PROC, "proc"),
    
    (BEGIN, "begin"),
    (END, "end"),
    (SET, "set"),
    (LIST, "list"),

    -- classes language
    (CLASS, "class"),
    (EXTENDS, "extends"),
    (METHOD, "method"),
    (FIELD, "field"),
    (NEW, "new"),
    (SEND, "send"),
    (SELF, "self"),
    (SUPER, "super")
  ]

findTok tok [] = Nothing
findTok tok ((tok_,str):list)
  | tok == tok_ = Just str
  | otherwise   = findTok tok list

findStr str [] = Nothing
findStr str ((tok,str_):list)
  | str == str_ = Just tok
  | otherwise   = findStr str list

instance TokenInterface Token where
  -- toToken str   =
  --   case findStr str tokenStrList of
  --     Nothing  -> error ("toToken: " ++ str)
  --     Just tok -> tok
  fromToken tok =
    case findTok tok tokenStrList of
      Nothing  -> error ("fromToken: " ++ show tok)
      Just str -> str
  

  isEOT END_OF_TOKEN = True
  isEOT _            = False  
