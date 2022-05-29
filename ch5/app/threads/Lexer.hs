module Lexer(lexerSpec) where

import Prelude hiding (EQ)
import CommonParserUtil
import Token

mkFn :: Token -> LexAction Token IO ()
mkFn tok = \text -> return $ Just tok

skip :: LexAction Token IO ()
skip = \text -> return $ Nothing

lexerSpec :: LexerSpec Token IO ()
lexerSpec = LexerSpec
  {
    endOfToken    = END_OF_TOKEN,
    lexerSpecList = 
      [ ("[ \t\n]" , skip),
        
        ("[0-9]+"  , mkFn INTEGER_NUMBER),
        
        ("\\-"     , mkFn SUB),
        ("\\("     , mkFn OPEN_PAREN),
        ("\\)"     , mkFn CLOSE_PAREN),
        ("\\,"     , mkFn COMMA),
        
        ("\\="     , mkFn EQ),
        
        (";"       , mkFn SEMICOLON),

        ("\\["     , mkFn OPEN_BRACKET),
        ("\\]"     , mkFn CLOSE_BRACKET),

        -- identifiers ending with a symbol
        ("zero\\?" , mkFn ISZERO),
        ("null\\?"   , mkFn ISNULL),
        
        ("[a-zA-Z][a-zA-Z0-9]*"    , keywordOrIdentifier)
      ]
  } 

keywordOrIdentifier text =
  case lookup text keywords of
    Nothing  -> mkFn IDENTIFIER text
    Just tok -> mkFn tok text

keywords =
  [ ("if",     IF)
  , ("then",   THEN)
  , ("else",   ELSE)
  , ("letrec", LETREC)
  , ("let",    LET)
  , ("proc",   PROC)
  , ("begin",  BEGIN)
  , ("end",    END)
  , ("set",    SET)
  , ("spawn",  SPAWN)
  , ("yield",  YIELD)
  , ("mutex",  MUTEX)
  , ("wait",   WAIT)
  , ("signal", SIGNAL)
  , ("car",    CAR)
  , ("cdr",    CDR)
  , ("print",  PRINT)
  , ("in",     IN)
  ]
  
