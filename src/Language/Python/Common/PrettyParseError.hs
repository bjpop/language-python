-----------------------------------------------------------------------------
-- |
-- Module      : Language.Python.Common.PrettyParseError
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Pretty printing of parse errors. 
-----------------------------------------------------------------------------

module Language.Python.Common.PrettyParseError where

import Language.Python.Common.Pretty
import Language.Python.Common.ParseError (ParseError (..))
import Language.Python.Common.SrcLocation 
import Language.Python.Common.PrettyToken

instance Pretty ParseError where
    pretty (UnexpectedToken t) = pretty (getSpan t) <+> text "unexpected token:" <+> pretty t
    pretty (UnexpectedChar c loc) = pretty loc <+> text "unexpected characer:" <+> char c
    pretty (StrError str) = text str
