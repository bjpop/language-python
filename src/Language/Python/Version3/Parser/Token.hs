module Language.Python.Version3.Parser.Token 
   ( Token (..)
   , Ident (..)
   )
where

import Language.Python.Data.SrcLocation (SrcLocation (..), Location (location))
import qualified Data.ByteString.Char8 as BS (ByteString)

newtype Ident = Ident String
   deriving (Eq, Show, Ord)

data Token 
   = Identifier SrcLocation !String
   | String SrcLocation !String 
   | ByteString SrcLocation !BS.ByteString
   | Integer SrcLocation !Integer
   | Float SrcLocation !Double 
   | Imaginary SrcLocation !Double
   | Assign SrcLocation  
   | PlusAssign SrcLocation
   | MinusAssign SrcLocation
   | MultAssign SrcLocation
   | DivAssign  SrcLocation
   | ModAssign SrcLocation
   | PowAssign SrcLocation
   | BinAndAssign SrcLocation
   | BinOrAssign SrcLocation
   | BinXorAssign SrcLocation
   | LeftShiftAssign SrcLocation
   | RightShiftAssign SrcLocation
   | FloorDivAssign SrcLocation
   | LeftRoundBracket  SrcLocation
   | RightRoundBracket  SrcLocation
   | LeftSquareBracket  SrcLocation
   | RightSquareBracket  SrcLocation
   | LeftBrace  SrcLocation
   | RightBrace  SrcLocation
   | RightArrow SrcLocation
   | Dot SrcLocation
   | Ellipsis SrcLocation
   | Comma  SrcLocation
   | SemiColon  SrcLocation
   | Colon  SrcLocation
   | Def  SrcLocation
   | While  SrcLocation
   | If  SrcLocation
   | True SrcLocation
   | False SrcLocation
   | Return SrcLocation
   | Indent SrcLocation
   | Dedent SrcLocation
   | Newline SrcLocation
   | Try SrcLocation
   | Except SrcLocation
   | Raise SrcLocation
   | Plus  SrcLocation
   | Minus  SrcLocation
   | Mult  SrcLocation
   | Div  SrcLocation
   | GreaterThan SrcLocation
   | LessThan  SrcLocation
   | Equality  SrcLocation
   | GreaterThanEquals  SrcLocation
   | LessThanEquals SrcLocation
   | And SrcLocation
   | Or  SrcLocation
   | Exponent SrcLocation
   | Pass SrcLocation
   | Break SrcLocation
   | Continue SrcLocation
   | Delete SrcLocation
   | Else SrcLocation
   | Not SrcLocation
   | BinaryOr SrcLocation
   | Xor SrcLocation
   | BinaryAnd SrcLocation
   | ShiftLeft SrcLocation
   | ShiftRight SrcLocation
   | Modulo SrcLocation
   | FloorDiv SrcLocation
   | Tilde SrcLocation
   | NotEquals SrcLocation
   | In SrcLocation
   | Is SrcLocation
   | Lambda SrcLocation
   | Class SrcLocation
   | Finally SrcLocation
   | None SrcLocation
   | For SrcLocation
   | From SrcLocation
   | NonLocal SrcLocation
   | Global SrcLocation
   | With SrcLocation
   | As SrcLocation
   | Elif SrcLocation
   | Yield SrcLocation
   | Assert SrcLocation
   | Import SrcLocation
   | At SrcLocation
   | EOF  -- does not have src location on purpose.
   deriving (Show, Eq, Ord)

-- XXX fixme
instance Location Token where
   location x = NoLocation

