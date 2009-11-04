-----------------------------------------------------------------------------
-- |
-- Module      : Language.Python.Common.ParseError
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Error values for the lexer and parser. 
-----------------------------------------------------------------------------

module Language.Python.Common.ParseError ( ParseError (..) ) where

import Language.Python.Common.Pretty
import Language.Python.Common.SrcLocation (SrcLocation)
import Language.Python.Common.Token (Token)
import Control.Monad.Error.Class

data ParseError  
   = UnexpectedToken Token           -- ^ An error from the parser. Token found where it should not be. Note: tokens contain their own source span.
   | UnexpectedChar Char SrcLocation -- ^ An error from the lexer. Character found where it should not be.
   | StrError String                 -- ^ A generic error containing a string message. No source location.
   deriving (Eq, Ord, Show)

instance Error ParseError where
   noMsg = StrError ""
   strMsg = StrError 
