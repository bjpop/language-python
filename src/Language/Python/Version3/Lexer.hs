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
-- See: <http://docs.python.org/dev/3.0/reference/lexical_analysis.html>.
-----------------------------------------------------------------------------

module Language.Python.Version3.Lexer (
   -- * Lexical analysis
   lex, 
   -- * Tokens
   Token(..), 
   -- * Parse errors
   ParseError(ParseError)) where

import Prelude hiding (lex)
import Language.Python.Version3.Parser.Lexer (lexToken, initStartCodeStack)
import Language.Python.Version3.Parser.Token (Token (..))
import Language.Python.Data.SrcLocation (initialSrcLocation)
import Language.Python.Version3.Parser.ParserMonad (P, execParser, ParseError(ParseError), initialState)

-- | Parse a string into a list of Python Tokens, or return an error. 
lex :: String -- ^ The input stream (python source code). 
    -> String -- ^ The name of the python source (filename or input device).
    -> Either ParseError [Token] -- ^ An error or a list of tokens.
lex input srcName =
   execParser lexer state
   where
   initLoc = initialSrcLocation srcName
   state = initialState initLoc input initStartCodeStack

lexer :: P [Token]
lexer = loop []
   where
   loop toks = do
      tok <- lexToken
      if tok == EOF
         then return (reverse toks)
         else loop (tok:toks)
