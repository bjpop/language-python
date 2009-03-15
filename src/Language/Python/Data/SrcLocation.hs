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
   Sloc { sloc_filename :: String
        , sloc_row :: !Int
        , sloc_column :: !Int 
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
