{-# OPTIONS  #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Python.Data.SrcLocation 
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Source location information for the Python parser. 
-- XXX We will probably move to source spans at some point.
-----------------------------------------------------------------------------

module Language.Python.Data.SrcLocation (
  -- * Projection of locations from values 
  Location (..),
  -- * Construction 
  SrcLocation (..),
  initialSrcLocation,
  -- * Modification
  incColumn, 
  incLine,
  incTab 
) where

-- | A location for a syntactic entity from the source code.
-- The location is specified by its filename, and starting row
-- and column. 
data SrcLocation = 
   Sloc { sloc_filename :: !String
        , sloc_row :: {-# UNPACK #-} !Int
        , sloc_column :: {-# UNPACK #-} !Int 
        } 
   | NoLocation
   deriving (Eq, Ord, Show)

-- | Types which have a source location.
class Location a where
   -- | Project the location from a value.
   location :: a -> SrcLocation 
   -- | By default a value has no location. 
   location x = NoLocation

-- | Construct the initial source location for a file.
initialSrcLocation :: String -> SrcLocation
initialSrcLocation filename 
    = Sloc 
      { sloc_filename = filename
      , sloc_row = 1
      , sloc_column = 1
      }

-- | Increment the column of a location by one. 
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

{- |
Taken from compiler/basicTypes/SrcLoc.lhs in ghc (inluding comments).

A SrcSpan delimits a portion of a text file.  It could be represented
by a pair of (line,column) coordinates, but in fact we optimise
slightly by using more compact representations for single-line and
zero-length spans, both of which are quite common.

The end position is defined to be the column /after/ the end of the
span.  That is, a span of (1,1)-(1,2) is one character long, and a
span of (1,1)-(1,1) is zero characters long.
-}
data SrcSpan
  = SpanCoLinear
    { span_file         :: !String
    , span_row          :: {-# UNPACK #-} !Int
    , span_start_column :: {-# UNPACK #-} !Int
    , span_end_column   :: {-# UNPACK #-} !Int
    }
  | SpanMultiLine
    { span_file         :: !String
    , span_start_row    :: {-# UNPACK #-} !Int
    , span_start_column :: {-# UNPACK #-} !Int
    , span_end_row      :: {-# UNPACK #-} !Int
    , span_end_column   :: {-# UNPACK #-} !Int
    }
  | SpanPoint
    { span_file     :: !String
    , span_row      :: {-# UNPACK #-} !Int
    , span_column   :: {-# UNPACK #-} !Int
    }
  | SpanEmpty 
  deriving (Eq,Ord,Show)

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
  file = span_file start

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
