{-# LANGUAGE CPP, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Python.Common.SrcLocation 
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Source location information for the Python parser. 
-- XXX We will probably move to source spans at some point.
-----------------------------------------------------------------------------

module Language.Python.Common.SrcLocation (
  -- * Projection of locations from values 
  Location (..),
  -- * Construction 
  SrcLocation (..),
  SrcSpan (..),
  Span (..),
  spanning,
  mkSrcSpan,
  mkSrcSpanPoint,
  combineSrcSpans,
  initialSrcLocation,
  spanStartPoint,
  -- * Modification
  incColumn, 
  decColumn,
  incLine,
  incTab,
  endCol
) where

import Language.Python.Common.PrettyClass
import Data.Data

-- | A location for a syntactic entity from the source code.
-- The location is specified by its filename, and starting row
-- and column. 
data SrcLocation = 
   Sloc { sloc_filename :: !String
        , sloc_row :: {-# UNPACK #-} !Int
        , sloc_column :: {-# UNPACK #-} !Int 
        } 
   | NoLocation
   deriving (Eq,Ord,Show,Typeable,Data)

-- | Types which have a source location.
class Location a where
   -- | Project the location from a value.
   location :: a -> SrcLocation 
   -- | By default a value has no location. 
   location x = NoLocation

class Span a where
   getSpan :: a -> SrcSpan
   getSpan x = SpanEmpty

spanning :: (Span a, Span b) => a -> b -> SrcSpan
spanning x y = combineSrcSpans (getSpan x) (getSpan y)

instance Span a => Span [a] where
   getSpan [] = SpanEmpty
   getSpan [x] = getSpan x 
   getSpan list@(x:xs) = combineSrcSpans (getSpan x) (getSpan (last list))

instance Span a => Span (Maybe a) where
   getSpan Nothing = SpanEmpty
   getSpan (Just x) = getSpan x

instance (Span a, Span b) => Span (Either a b) where
   getSpan (Left x) = getSpan x
   getSpan (Right x) = getSpan x

instance (Span a, Span b) => Span (a, b) where
   getSpan (x,y) = spanning x y

instance Span SrcSpan where
   getSpan = id

-- | Construct the initial source location for a file.
initialSrcLocation :: String -> SrcLocation
initialSrcLocation filename 
    = Sloc 
      { sloc_filename = filename
      , sloc_row = 1
      , sloc_column = 1
      }

-- | Decrement the column of a location, only if they are on the same row.
decColumn :: Int -> SrcLocation -> SrcLocation
decColumn n loc
   | n < col = loc { sloc_column = col - n }
   | otherwise = loc 
   where
   col = sloc_column loc

-- | Increment the column of a location. 
incColumn :: Int -> SrcLocation -> SrcLocation
incColumn n loc@(Sloc { sloc_column = col })
   = loc { sloc_column = col + n }

-- | Increment the column of a location by one tab stop.
incTab :: SrcLocation -> SrcLocation
incTab loc@(Sloc { sloc_column = col })
   = loc { sloc_column = newCol } 
   where
   newCol = col + 8 - (col - 1) `mod` 8

-- | Increment the line number (row) of a location by one.
incLine :: Int -> SrcLocation -> SrcLocation
incLine n loc@(Sloc { sloc_row = row }) 
   = loc { sloc_column = 1, sloc_row = row + n }

{-
Inspired heavily by compiler/basicTypes/SrcLoc.lhs 
A SrcSpan delimits a portion of a text file.  
-}

data SrcSpan
  = SpanCoLinear
    { span_filename     :: !String
    , span_row          :: {-# UNPACK #-} !Int
    , span_start_column :: {-# UNPACK #-} !Int
    , span_end_column   :: {-# UNPACK #-} !Int
    }
  | SpanMultiLine
    { span_filename     :: !String
    , span_start_row    :: {-# UNPACK #-} !Int
    , span_start_column :: {-# UNPACK #-} !Int
    , span_end_row      :: {-# UNPACK #-} !Int
    , span_end_column   :: {-# UNPACK #-} !Int
    }
  | SpanPoint
    { span_filename :: !String
    , span_row      :: {-# UNPACK #-} !Int
    , span_column   :: {-# UNPACK #-} !Int
    }
  | SpanEmpty 
   deriving (Eq,Ord,Show,Typeable,Data)

instance Pretty SrcSpan where
   pretty span@(SpanCoLinear {}) = prettyMultiSpan span
   pretty span@(SpanMultiLine {}) = prettyMultiSpan span
   pretty span@(SpanPoint {})
      = text (span_filename span) <> colon <+>
        parens (pretty (span_row span) <> comma <> pretty (span_column span))
   pretty SpanEmpty = empty

prettyMultiSpan :: SrcSpan -> Doc 
prettyMultiSpan span 
  = text (span_filename span) <> colon <+>
    parens (pretty (startRow span) <> comma <> pretty (startCol span)) <> char '-' <>
    parens (pretty (endRow span) <> comma <> pretty (endCol span))

mkSrcSpanPoint :: SrcLocation -> SrcSpan
mkSrcSpanPoint loc@(Sloc {})
   = SpanPoint 
     { span_filename = sloc_filename loc
     , span_row = sloc_row loc
     , span_column = sloc_column loc
     }
mkSrcSpanPoint NoLocation = error "attempt to convert an empty location to a span"

-- make a point span from the start of a span
spanStartPoint :: SrcSpan -> SrcSpan
spanStartPoint SpanEmpty = SpanEmpty
spanStartPoint span = 
   SpanPoint 
   { span_filename = span_filename span
   , span_row = startRow span
   , span_column = startCol span
   }

mkSrcSpan :: SrcLocation -> SrcLocation -> SrcSpan
mkSrcSpan NoLocation _ = SpanEmpty
mkSrcSpan _ NoLocation = SpanEmpty 
mkSrcSpan loc1 loc2
  | line1 == line2 = 
       if col1 == col2
          then SpanPoint file line1 col1
          else SpanCoLinear file line1 col1 col2
  | otherwise = 
       SpanMultiLine file line1 col1 line2 col2
  where
  line1 = sloc_row loc1
  line2 = sloc_row loc2
  col1 = sloc_column loc1
  col2 = sloc_column loc2
  file = sloc_filename loc1

-- | Combines two 'SrcSpan' into one that spans at least all the characters
-- within both spans. Assumes the "file" part is the same in both inputs
combineSrcSpans :: SrcSpan -> SrcSpan -> SrcSpan
combineSrcSpans SpanEmpty r = r -- this seems more useful
combineSrcSpans l SpanEmpty = l
combineSrcSpans start end
 = case row1 `compare` row2 of
     EQ -> case col1 `compare` col2 of
                EQ -> SpanPoint file row1 col1
                LT -> SpanCoLinear file row1 col1 col2
                GT -> SpanCoLinear file row1 col2 col1
     LT -> SpanMultiLine file row1 col1 row2 col2
     GT -> SpanMultiLine file row2 col2 row1 col1
  where
  row1 = startRow start
  col1 = startCol start
  row2 = endRow end
  col2 = endCol end
  file = span_filename start

startRow :: SrcSpan -> Int
startRow (SpanCoLinear { span_row = row }) = row
startRow (SpanMultiLine { span_start_row = row }) = row
startRow (SpanPoint { span_row = row }) = row
startRow SpanEmpty = error "startRow called on empty span"

endRow :: SrcSpan -> Int
endRow (SpanCoLinear { span_row = row }) = row
endRow (SpanMultiLine { span_end_row = row }) = row
endRow (SpanPoint { span_row = row }) = row
endRow SpanEmpty = error "endRow called on empty span"

startCol :: SrcSpan -> Int
startCol (SpanCoLinear { span_start_column = col }) = col 
startCol (SpanMultiLine { span_start_column = col }) = col 
startCol (SpanPoint { span_column = col }) = col 
startCol SpanEmpty = error "startCol called on empty span"

endCol :: SrcSpan -> Int
endCol (SpanCoLinear { span_end_column = col }) = col 
endCol (SpanMultiLine { span_end_column = col }) = col 
endCol (SpanPoint { span_column = col }) = col 
endCol SpanEmpty = error "endCol called on empty span"
