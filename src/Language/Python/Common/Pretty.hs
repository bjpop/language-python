{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Python.Common.Pretty
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Convenience class for pretty printing combinators.
-----------------------------------------------------------------------------

module Language.Python.Common.Pretty (module TextPP, module Language.Python.Common.Pretty) where

import Text.PrettyPrint as TextPP

--------------------------------------------------------------------------------

-- | All types which can be transformed into a 'Doc'.
class Pretty a where
   pretty :: a -> Doc

-- | Transform values into strings.
prettyText :: Pretty a => a -> String
prettyText = render . pretty

-- | Print just the prefix of something
prettyPrefix :: Pretty a => Int -> a -> Doc
prettyPrefix maxLen x
   | length fullText <= maxLen = pretty fullText
   | otherwise = pretty (take maxLen fullText) <+> text "..." 
   where
   fullText = prettyText x 

instance Pretty String where
   pretty s = text s

-- | Conditionally wrap parentheses around an item.
parensIf :: Pretty a => (a -> Bool) -> a -> Doc
parensIf test x = if test x then parens $ pretty x else pretty x 

perhaps :: Pretty a => Maybe a -> Doc -> Doc
perhaps Nothing doc = empty
perhaps (Just {}) doc = doc 

-- | A list of things separated by commas.
commaList :: Pretty a => [a] -> Doc
commaList = hsep . punctuate comma . map pretty 

instance Pretty Int where
  pretty = int

instance Pretty Integer where
  pretty = integer

instance Pretty Double where
   pretty = double

instance Pretty Bool where
  pretty True = text "True"
  pretty False = text "False"

instance Pretty a => Pretty (Maybe a) where
   pretty Nothing = empty
   pretty (Just x) = pretty x
