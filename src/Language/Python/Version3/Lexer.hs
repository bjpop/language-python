{-# OPTIONS  #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Python.Version3.Lexer
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- A lexer and set of tokens for Python version 3 programs. 
-- See: <http://www.python.org/doc/3.0/reference/lexical_analysis.html>.
-----------------------------------------------------------------------------

module Language.Python.Version3.Lexer (
   -- * Lexical analysis
   lex, 
   lexOneToken,
   -- * Parse errors
   ParseError) where

import Prelude hiding (lex)
import Language.Python.Version3.Parser.Lexer (lexToken, initStartCodeStack)
import Language.Python.Common.Token as Token 
import Language.Python.Common.SrcLocation (initialSrcLocation)
import Language.Python.Common.ParserMonad 
       (ParseState (input), P, runParser, execParser, ParseError, initialState)

-- | Parse a string into a list of Python Tokens, or return an error. 
lex :: String -- ^ The input stream (python source code). 
    -> String -- ^ The name of the python source (filename or input device).
    -> Either ParseError [Token] -- ^ An error or a list of tokens.
lex input srcName =
   execParser lexer state
   where
   initLoc = initialSrcLocation srcName
   state = initialState initLoc input initStartCodeStack

-- | Try to lex the first token in an input string. Return either a parse error
-- or a pair containing the next token and the rest of the input after the token.
lexOneToken :: String -- ^ The input stream (python source code).
         -> String -- ^ The name of the python source (filename or input device).
         -> Either ParseError (Token, String) -- ^ An error or the next token and the rest of the input after the token. 
lexOneToken source srcName =
   case runParser lexToken state of
      Left err -> Left err
      Right (tok, state) -> Right (tok, input state)
   where
   initLoc = initialSrcLocation srcName
   state = initialState initLoc source initStartCodeStack

lexer :: P [Token]
lexer = loop []
   where
   loop toks = do
      tok <- lexToken
      case tok of
         EOF {} -> return (reverse toks)
         other -> loop (tok:toks)
