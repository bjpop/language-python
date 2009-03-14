module Language.Python.Data.SrcLocation (
  SrcLocation (..),
  Location (..),
  initialSrcLocation,
  incColumn, 
  incLine,
  incTab 
) where

data SrcLocation = 
   Sloc { sloc_filename :: String
        , sloc_row :: !Int
        , sloc_column :: !Int 
        } 
   | NoLocation
   deriving (Eq, Ord, Show)

class Location a where
   location :: a -> SrcLocation 
   -- default declaration
   location x = NoLocation

initialSrcLocation :: String -> SrcLocation
initialSrcLocation filename 
    = Sloc 
      { sloc_filename = filename
      , sloc_row = 1
      , sloc_column = 1
      }

incColumn :: Int -> SrcLocation -> SrcLocation
incColumn n loc@(Sloc { sloc_column = col })
   = loc { sloc_column = col + n }

incTab :: SrcLocation -> SrcLocation
incTab loc@(Sloc { sloc_column = col })
   = loc { sloc_column = newCol } 
   where
   newCol = col + 8 - (col - 1) `mod` 8

incLine :: Int -> SrcLocation -> SrcLocation
incLine n loc@(Sloc { sloc_row = row }) 
   = loc { sloc_column = 1, sloc_row = row + n }
