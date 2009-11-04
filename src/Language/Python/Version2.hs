-----------------------------------------------------------------------------
-- |
-- Module      : Language.Python.Version2
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- A convenient re-export of the parser and lexer for version 2.x of Python. 
--
-- See: 
--
-- * <http://www.python.org/doc/2.6/reference/index.html> for an overview of the language. 
--
-- * <http://www.python.org/doc/2.6/reference/grammar.html> for the full grammar.
-- 
-- * <http://www.python.org/doc/2.6/reference/toplevel_components.html> for a description of 
-- the various Python top-levels, which correspond to the parsers provided here.
-----------------------------------------------------------------------------

module Language.Python.Version2 (
   -- * The parser
   module Language.Python.Version2.Parser, 
   -- * The lexer
   module Language.Python.Version2.Lexer 
   ) where

import Language.Python.Version2.Parser 
import Language.Python.Version2.Lexer 
