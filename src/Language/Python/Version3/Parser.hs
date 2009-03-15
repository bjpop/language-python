{-# OPTIONS  #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Python.Version3.Parser
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- A parser for Python version 3 programs. Parsers are provided for 
-- modules, statements, and expressions. 
--
-- See: 
--
-- * <http://docs.python.org/dev/3.0/reference/index.html> for an overview of the language. 
--
-- * <http://docs.python.org/dev/3.0/reference/grammar.html> for the full grammar.
-- 
-- * <http://docs.python.org/dev/3.0/reference/toplevel_components.html> for a description of 
-- the various Python top-levels, which correspond to the parsers provided here.
-----------------------------------------------------------------------------

module Language.Python.Version3.Parser (
   -- * Parsing modules
   parseModule, 
   -- * Parsing statements
   parseStmt, 
   -- * Parsing expressions
   parseExpr, 
   -- * Parse errors
   ParseError(ParseError)) where

import Language.Python.Version3.Parser.Parser (parseFileInput, parseSingleInput, parseEval)
import Language.Python.Version3.Syntax.AST (Module, Statement, Expr)
import Language.Python.Data.SrcLocation (initialSrcLocation)
import Language.Python.Version3.Parser.ParserMonad (execParser, ParseError(ParseError), initialState)
import Language.Python.Version3.Parser.Lexer (initStartCodeStack)

-- | Parse a whole Python source file.
parseModule :: String -- ^ The input stream (python module source code). 
      -> String -- ^ The name of the python source (filename or input device). 
      -> Either ParseError Module -- ^ An error or the abstract syntax tree (AST) of the python module. 
parseModule input srcName = 
   execParser parseFileInput state 
   where
   initLoc = initialSrcLocation srcName
   state = initialState initLoc input initStartCodeStack

-- | Parse one compound statement, or a sequence of simple statements. Generally used for interactive input, such as from the command line of an interpreter.
parseStmt :: String -- ^ The input stream (python statement source code). 
      -> String -- ^ The name of the python source (filename or input device). 
      -> Either ParseError [Statement] -- ^ An error or maybe the abstract syntax tree (AST) of zero or more python statements. 
parseStmt input srcName = 
   execParser parseSingleInput state 
   where
   initLoc = initialSrcLocation srcName
   state = initialState initLoc input initStartCodeStack

-- | Parse an expression. Generally used as input for the \'eval\' primitive. 
parseExpr :: String -- ^ The input stream (python statement source code). 
      -> String -- ^ The name of the python source (filename or input device). 
      -> Either ParseError Expr -- ^ An error or maybe the abstract syntax tree (AST) of the python expression. 
parseExpr input srcName = 
   execParser parseEval state 
   where
   initLoc = initialSrcLocation srcName
   state = initialState initLoc input initStartCodeStack
