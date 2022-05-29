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
        
        ("zero\\?" , mkFn ISZERO),

        ("if"      , mkFn IF),
        ("then"    , mkFn THEN),
        ("else"    , mkFn ELSE),
        
        ("letrec"  , mkFn LETREC),

        ("let"     , mkFn LET),
        ("\\="     , mkFn EQ),
        
        ("proc"    , mkFn PROC),

        ("list"    , mkFn LIST),
        ("null\\?"   , mkFn ISNULL),
        ("car"     , mkFn CAR),
        ("cdr"     , mkFn CDR),

        ("try"     , mkFn TRY),
        ("catch"   , mkFn CATCH),
        ("raise"   , mkFn RAISE),
        
        ("in[ \t\n]"      , mkFn IN),
        
        ("[a-zA-Z][a-zA-Z0-9]*"    , mkFn IDENTIFIER)
      ]
  } 
