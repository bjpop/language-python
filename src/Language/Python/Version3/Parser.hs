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
-- A parser for Python version 3.x programs. Parsers are provided for 
-- modules, statements, and expressions. The parsers produce comment tokens
-- in addition to the abstract syntax tree.
--
-- See: 
--
-- * <http://www.python.org/doc/3.1/reference/index.html> for an overview of the language. 
--
-- * <http://www.python.org/doc/3.1/reference/grammar.html> for the full grammar.
-- 
-- * <http://www.python.org/doc/3.1/reference/toplevel_components.html> for a description of 
-- the various Python top-levels, which correspond to the parsers provided here.
-----------------------------------------------------------------------------

module Language.Python.Version3.Parser (
   -- * Parsing modules
   parseModule,
   -- * Parsing statements
   parseStmt,
   -- * Parsing expressions
   parseExpr) where

import Language.Python.Version3.Parser.Parser (parseFileInput, parseSingleInput, parseEval)
import Language.Python.Version3.Parser.Lexer (initStartCodeStack)
import Language.Python.Common.AST (ModuleSpan, StatementSpan, ExprSpan)
import Language.Python.Common.Token (Token)
import Language.Python.Common.SrcLocation (initialSrcLocation)
import Language.Python.Common.ParserMonad (execParser, execParserKeepComments, ParseError, initialState)

-- | Parse a whole Python source file. Return comments in addition to the parsed module.
parseModule :: String -- ^ The input stream (python module source code). 
      -> String -- ^ The name of the python source (filename or input device). 
      -> Either ParseError (ModuleSpan, [Token]) -- ^ An error or the abstract syntax tree (AST) of the python module and comment tokens.
parseModule input srcName = 
   execParserKeepComments parseFileInput state 
   where
   initLoc = initialSrcLocation srcName
   state = initialState initLoc input initStartCodeStack

-- | Parse one compound statement, or a sequence of simple statements. Generally used for interactive input, such as from the command line of an interpreter. Return comments in addition to the parsed statements.
parseStmt :: String -- ^ The input stream (python statement source code). 
      -> String -- ^ The name of the python source (filename or input device). 
      -> Either ParseError ([StatementSpan], [Token]) -- ^ An error or maybe the abstract syntax tree (AST) of zero or more python statements, plus comments.
parseStmt input srcName = 
   execParserKeepComments parseSingleInput state 
   where
   initLoc = initialSrcLocation srcName
   state = initialState initLoc input initStartCodeStack

-- | Parse an expression. Generally used as input for the \'eval\' primitive. Return comments in addition to the parsed expression.
parseExpr :: String -- ^ The input stream (python statement source code). 
      -> String -- ^ The name of the python source (filename or input device). 
      -> Either ParseError (ExprSpan, [Token]) -- ^ An error or maybe the abstract syntax tree (AST) of the python expression, plus comment tokens.
parseExpr input srcName = 
   execParserKeepComments parseEval state 
   where
   initLoc = initialSrcLocation srcName
   state = initialState initLoc input initStartCodeStack
