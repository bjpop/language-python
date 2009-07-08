{-# OPTIONS  #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Python.Version3.Parser.Token 
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Lexical tokens for the Python version 3 lexer. 
-- See: <http://www.python.org/doc/3.0/reference/lexical_analysis.html>
-----------------------------------------------------------------------------

module Language.Python.Version3.Parser.Token 
   ( Token (..)
   , Ident (..)
   )
where

import Language.Python.Data.SrcLocation (SrcLocation (..), Location (location))
import qualified Data.ByteString.Char8 as BS (ByteString)

-- | Identifier.
newtype Ident = Ident String
   deriving (Eq, Show, Ord)

-- | Lexical tokens.
data Token 
   -- Whitespace
   = Indent SrcLocation                       -- ^ Indentation: increase.
   | Dedent SrcLocation                       -- ^ Indentation: decrease.
   | Newline SrcLocation                      -- ^ Newline.

   -- Identifiers 
   | Identifier SrcLocation !String           -- ^ Identifier.

   -- Literals
   | String SrcLocation !String               -- ^ Literal: string.
   | ByteString SrcLocation !BS.ByteString    -- ^ Literal: byte string.
   | Integer SrcLocation !Integer             -- ^ Literal: integer.
   | Float SrcLocation !Double                -- ^ Literal: floating point.
   | Imaginary SrcLocation !Double            -- ^ Literal: imaginary number.

   -- Keywords
   | Def SrcLocation                          -- ^ Keyword: \'def\'. 
   | While SrcLocation                        -- ^ Keyword: \'while\'.
   | If SrcLocation                           -- ^ Keyword: \'if\'.
   | True SrcLocation                         -- ^ Keyword: \'True\'.
   | False SrcLocation                        -- ^ Keyword: \'False\'.
   | Return SrcLocation                       -- ^ Keyword: \'Return\'.
   | Try SrcLocation                          -- ^ Keyword: \'try\'.
   | Except SrcLocation                       -- ^ Keyword: \'except\'.
   | Raise SrcLocation                        -- ^ Keyword: \'raise\'.
   | In SrcLocation                           -- ^ Keyword: \'in\'.
   | Is SrcLocation                           -- ^ Keyword: \'is\'.
   | Lambda SrcLocation                       -- ^ Keyword: \'lambda\'.
   | Class SrcLocation                        -- ^ Keyword: \'class\'.
   | Finally SrcLocation                      -- ^ Keyword: \'finally\'.
   | None SrcLocation                         -- ^ Keyword: \'None\'
   | For SrcLocation                          -- ^ Keyword: \'for\'.
   | From SrcLocation                         -- ^ Keyword: \'from\'.
   | NonLocal SrcLocation                     -- ^ Keyword: \'nonlocal\'.
   | Global SrcLocation                       -- ^ Keyword: \'global\'.
   | With SrcLocation                         -- ^ Keyword: \'with\'.
   | As SrcLocation                           -- ^ Keyword: \'as\'.
   | Elif SrcLocation                         -- ^ Keyword: \'elif\'.
   | Yield SrcLocation                        -- ^ Keyword: \'yield\'.
   | Assert SrcLocation                       -- ^ Keyword: \'assert\'.
   | Import SrcLocation                       -- ^ Keyword: \'import\'.
   | Pass SrcLocation                         -- ^ Keyword: \'pass\'.
   | Break SrcLocation                        -- ^ Keyword: \'break\'.
   | Continue SrcLocation                     -- ^ Keyword: \'continue\'.
   | Delete SrcLocation                       -- ^ Keyword: \'del\'.
   | Else SrcLocation                         -- ^ Keyword: \'else\'.
   | Not SrcLocation                          -- ^ Keyword: \'not\'.
   | And SrcLocation                          -- ^ Keyword: boolean conjunction \'and\'.
   | Or SrcLocation                           -- ^ Keyword: boolean disjunction \'or\'.

   -- Delimiters
   | At SrcLocation                           -- ^ Delimiter: at sign \'\@\'. 
   | LeftRoundBracket SrcLocation             -- ^ Delimiter: left round bracket \'(\'.
   | RightRoundBracket SrcLocation            -- ^ Delimiter: right round bracket \')\'.
   | LeftSquareBracket SrcLocation            -- ^ Delimiter: left square bracket \'[\'.
   | RightSquareBracket SrcLocation           -- ^ Delimiter: right square bracket \']\'.
   | LeftBrace SrcLocation                    -- ^ Delimiter: left curly bracket \'{\'.
   | RightBrace SrcLocation                   -- ^ Delimiter: right curly bracket \'}\'.
   | Dot SrcLocation                          -- ^ Delimiter: dot (full stop) \'.\'.
   | Comma SrcLocation                        -- ^ Delimiter: comma \',\'.
   | SemiColon SrcLocation                    -- ^ Delimiter: semicolon \';\'.
   | Colon SrcLocation                        -- ^ Delimiter: colon \':\'.
   | Ellipsis SrcLocation                     -- ^ Delimiter: ellipses (three dots) \'...\'.
   | RightArrow SrcLocation                   -- ^ Delimiter: right facing arrow \'->\'.
   | Assign SrcLocation                       -- ^ Delimiter: assignment \'=\'.
   | PlusAssign SrcLocation                   -- ^ Delimiter: plus assignment \'+=\'.
   | MinusAssign SrcLocation                  -- ^ Delimiter: minus assignment \'-=\'.
   | MultAssign SrcLocation                   -- ^ Delimiter: multiply assignment \'*=\'
   | DivAssign SrcLocation                    -- ^ Delimiter: divide assignment \'/=\'.
   | ModAssign SrcLocation                    -- ^ Delimiter: modulus assignment \'%=\'.
   | PowAssign SrcLocation                    -- ^ Delimiter: power assignment \'**=\'.
   | BinAndAssign SrcLocation                 -- ^ Delimiter: binary-and assignment \'&=\'.
   | BinOrAssign SrcLocation                  -- ^ Delimiter: binary-or assignment \'|=\'.
   | BinXorAssign SrcLocation                 -- ^ Delimiter: binary-xor assignment \'^=\'.
   | LeftShiftAssign SrcLocation              -- ^ Delimiter: binary-left-shift assignment \'<<=\'.
   | RightShiftAssign SrcLocation             -- ^ Delimiter: binary-right-shift assignment \'>>=\'.
   | FloorDivAssign SrcLocation               -- ^ Delimiter: floor-divide assignment \'//=\'.

   -- Operators
   | Plus SrcLocation                         -- ^ Operator: plus \'+\'.
   | Minus SrcLocation                        -- ^ Operator: minus: \'-\'.
   | Mult SrcLocation                         -- ^ Operator: multiply \'*\'.
   | Div SrcLocation                          -- ^ Operator: divide \'/\'.
   | GreaterThan SrcLocation                  -- ^ Operator: greater-than \'>\'.
   | LessThan SrcLocation                     -- ^ Operator: less-than \'<\'.
   | Equality SrcLocation                     -- ^ Operator: equals \'==\'.
   | GreaterThanEquals SrcLocation            -- ^ Operator: greater-than-or-equals \'>=\'.
   | LessThanEquals SrcLocation               -- ^ Operator: less-than-or-equals \'<=\'.
   | Exponent SrcLocation                     -- ^ Operator: exponential \'**\'.
   | BinaryOr SrcLocation                     -- ^ Operator: binary-or \'|\'.
   | Xor SrcLocation                          -- ^ Operator: binary-xor \'^\'.
   | BinaryAnd SrcLocation                    -- ^ Operator: binary-and \'&\'.
   | ShiftLeft SrcLocation                    -- ^ Operator: binary-shift-left \'<<\'.
   | ShiftRight SrcLocation                   -- ^ Operator: binary-shift-right \'>>\'.
   | Modulo SrcLocation                       -- ^ Operator: modulus \'%\'.
   | FloorDiv SrcLocation                     -- ^ Operator: floor-divide \'//\'.
   | Tilde SrcLocation                        -- ^ Operator: tilde \'~\'.
   | NotEquals SrcLocation                    -- ^ Operator: not-equals \'!=\'.

   -- Special cases
   | EOF                                      -- ^ End of file (no source location). 
   deriving (Show, Eq, Ord)

-- XXX fixme
instance Location Token where
   location x = NoLocation

