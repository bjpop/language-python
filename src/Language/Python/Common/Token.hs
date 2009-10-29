{-# LANGUAGE CPP, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Python.Common.Token 
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Lexical tokens for the Python lexer. 
-- See: <http://www.python.org/doc/3.0/reference/lexical_analysis.html>
-----------------------------------------------------------------------------

module Language.Python.Common.Token ( Token (..), prettyToken) where

import Language.Python.Common.PrettyClass
import Language.Python.Common.SrcLocation (SrcSpan (..), SrcLocation (..), Location (location), Span(getSpan))
import qualified Data.ByteString.Char8 as BS (ByteString, unpack)

#ifdef BASE4
import Data.Data
#else
import Data.Generics (Data(..),Typeable(..))
#endif

-- | Lexical tokens.
data Token 
   -- Whitespace
   = Indent { token_span :: !SrcSpan }                       -- ^ Indentation: increase.
   | Dedent { token_span :: !SrcSpan }                       -- ^ Indentation: decrease.
   | Newline { token_span :: !SrcSpan }                      -- ^ Newline.

   -- Comment
   | Comment { token_span :: !SrcSpan, token_comment :: !String } -- ^ Comment.

   -- Identifiers 
   | Identifier { token_span :: !SrcSpan, token_identifier :: !String }            -- ^ Identifier.

   -- Literals
   | String { token_span :: !SrcSpan, token_string :: !String }                    -- ^ Literal: string.
   | ByteString { token_span :: !SrcSpan, token_byte_string :: !BS.ByteString }    -- ^ Literal: byte string.
   | Integer { token_span :: !SrcSpan, token_integer :: !Integer }                 -- ^ Literal: integer.
   | Float { token_span :: !SrcSpan, token_double :: !Double }                     -- ^ Literal: floating point.
   | Imaginary { token_span :: !SrcSpan, token_double :: !Double }                 -- ^ Literal: imaginary number.

   -- Keywords
   | Def { token_span :: !SrcSpan }                          -- ^ Keyword: \'def\'. 
   | While { token_span :: !SrcSpan }                        -- ^ Keyword: \'while\'.
   | If { token_span :: !SrcSpan }                           -- ^ Keyword: \'if\'.
   | True { token_span :: !SrcSpan }                         -- ^ Keyword: \'True\'.
   | False { token_span :: !SrcSpan }                        -- ^ Keyword: \'False\'.
   | Return { token_span :: !SrcSpan }                       -- ^ Keyword: \'Return\'.
   | Try { token_span :: !SrcSpan }                          -- ^ Keyword: \'try\'.
   | Except { token_span :: !SrcSpan }                       -- ^ Keyword: \'except\'.
   | Raise { token_span :: !SrcSpan }                        -- ^ Keyword: \'raise\'.
   | In { token_span :: !SrcSpan }                           -- ^ Keyword: \'in\'.
   | Is { token_span :: !SrcSpan }                           -- ^ Keyword: \'is\'.
   | Lambda { token_span :: !SrcSpan }                       -- ^ Keyword: \'lambda\'.
   | Class { token_span :: !SrcSpan }                        -- ^ Keyword: \'class\'.
   | Finally { token_span :: !SrcSpan }                      -- ^ Keyword: \'finally\'.
   | None { token_span :: !SrcSpan }                         -- ^ Keyword: \'None\'
   | For { token_span :: !SrcSpan }                          -- ^ Keyword: \'for\'.
   | From { token_span :: !SrcSpan }                         -- ^ Keyword: \'from\'.
   | Global { token_span :: !SrcSpan }                       -- ^ Keyword: \'global\'.
   | With { token_span :: !SrcSpan }                         -- ^ Keyword: \'with\'.
   | As { token_span :: !SrcSpan }                           -- ^ Keyword: \'as\'.
   | Elif { token_span :: !SrcSpan }                         -- ^ Keyword: \'elif\'.
   | Yield { token_span :: !SrcSpan }                        -- ^ Keyword: \'yield\'.
   | Assert { token_span :: !SrcSpan }                       -- ^ Keyword: \'assert\'.
   | Import { token_span :: !SrcSpan }                       -- ^ Keyword: \'import\'.
   | Pass { token_span :: !SrcSpan }                         -- ^ Keyword: \'pass\'.
   | Break { token_span :: !SrcSpan }                        -- ^ Keyword: \'break\'.
   | Continue { token_span :: !SrcSpan }                     -- ^ Keyword: \'continue\'.
   | Delete { token_span :: !SrcSpan }                       -- ^ Keyword: \'del\'.
   | Else { token_span :: !SrcSpan }                         -- ^ Keyword: \'else\'.
   | Not { token_span :: !SrcSpan }                          -- ^ Keyword: \'not\'.
   | And { token_span :: !SrcSpan }                          -- ^ Keyword: boolean conjunction \'and\'.
   | Or { token_span :: !SrcSpan }                           -- ^ Keyword: boolean disjunction \'or\'.
   -- Version 3.x only:
   | NonLocal { token_span :: !SrcSpan }                     -- ^ Keyword: \'nonlocal\' (Python 2.x only)
   -- Version 2.x only:
   | Print { token_span :: !SrcSpan }                        -- ^ Keyword: \'print\'. (Python 3.x only)
   | Exec { token_span :: !SrcSpan }                         -- ^ Keyword: \'exec\'. (Python 3.x only)

   -- Delimiters
   | At { token_span :: !SrcSpan }                           -- ^ Delimiter: at sign \'\@\'. 
   | LeftRoundBracket { token_span :: !SrcSpan }             -- ^ Delimiter: left round bracket \'(\'.
   | RightRoundBracket { token_span :: !SrcSpan }            -- ^ Delimiter: right round bracket \')\'.
   | LeftSquareBracket { token_span :: !SrcSpan }            -- ^ Delimiter: left square bracket \'[\'.
   | RightSquareBracket { token_span :: !SrcSpan }           -- ^ Delimiter: right square bracket \']\'.
   | LeftBrace { token_span :: !SrcSpan }                    -- ^ Delimiter: left curly bracket \'{\'.
   | RightBrace { token_span :: !SrcSpan }                   -- ^ Delimiter: right curly bracket \'}\'.
   | Dot { token_span :: !SrcSpan }                          -- ^ Delimiter: dot (full stop) \'.\'.
   | Comma { token_span :: !SrcSpan }                        -- ^ Delimiter: comma \',\'.
   | SemiColon { token_span :: !SrcSpan }                    -- ^ Delimiter: semicolon \';\'.
   | Colon { token_span :: !SrcSpan }                        -- ^ Delimiter: colon \':\'.
   | Ellipsis { token_span :: !SrcSpan }                     -- ^ Delimiter: ellipses (three dots) \'...\'.
   | RightArrow { token_span :: !SrcSpan }                   -- ^ Delimiter: right facing arrow \'->\'.
   | Assign { token_span :: !SrcSpan }                       -- ^ Delimiter: assignment \'=\'.
   | PlusAssign { token_span :: !SrcSpan }                   -- ^ Delimiter: plus assignment \'+=\'.
   | MinusAssign { token_span :: !SrcSpan }                  -- ^ Delimiter: minus assignment \'-=\'.
   | MultAssign { token_span :: !SrcSpan }                   -- ^ Delimiter: multiply assignment \'*=\'
   | DivAssign { token_span :: !SrcSpan }                    -- ^ Delimiter: divide assignment \'/=\'.
   | ModAssign { token_span :: !SrcSpan }                    -- ^ Delimiter: modulus assignment \'%=\'.
   | PowAssign { token_span :: !SrcSpan }                    -- ^ Delimiter: power assignment \'**=\'.
   | BinAndAssign { token_span :: !SrcSpan }                 -- ^ Delimiter: binary-and assignment \'&=\'.
   | BinOrAssign { token_span :: !SrcSpan }                  -- ^ Delimiter: binary-or assignment \'|=\'.
   | BinXorAssign { token_span :: !SrcSpan }                 -- ^ Delimiter: binary-xor assignment \'^=\'.
   | LeftShiftAssign { token_span :: !SrcSpan }              -- ^ Delimiter: binary-left-shift assignment \'<<=\'.
   | RightShiftAssign { token_span :: !SrcSpan }             -- ^ Delimiter: binary-right-shift assignment \'>>=\'.
   | FloorDivAssign { token_span :: !SrcSpan }               -- ^ Delimiter: floor-divide assignment \'//=\'.
   | BackQuote { token_span :: !SrcSpan }                    -- ^ Delimiter: back quote character \'`\'.

   -- Operators
   | Plus { token_span :: !SrcSpan }                         -- ^ Operator: plus \'+\'.
   | Minus { token_span :: !SrcSpan }                        -- ^ Operator: minus: \'-\'.
   | Mult { token_span :: !SrcSpan }                         -- ^ Operator: multiply \'*\'.
   | Div { token_span :: !SrcSpan }                          -- ^ Operator: divide \'/\'.
   | GreaterThan { token_span :: !SrcSpan }                  -- ^ Operator: greater-than \'>\'.
   | LessThan { token_span :: !SrcSpan }                     -- ^ Operator: less-than \'<\'.
   | Equality { token_span :: !SrcSpan }                     -- ^ Operator: equals \'==\'.
   | GreaterThanEquals { token_span :: !SrcSpan }            -- ^ Operator: greater-than-or-equals \'>=\'.
   | LessThanEquals { token_span :: !SrcSpan }               -- ^ Operator: less-than-or-equals \'<=\'.
   | Exponent { token_span :: !SrcSpan }                     -- ^ Operator: exponential \'**\'.
   | BinaryOr { token_span :: !SrcSpan }                     -- ^ Operator: binary-or \'|\'.
   | Xor { token_span :: !SrcSpan }                          -- ^ Operator: binary-xor \'^\'.
   | BinaryAnd { token_span :: !SrcSpan }                    -- ^ Operator: binary-and \'&\'.
   | ShiftLeft { token_span :: !SrcSpan }                    -- ^ Operator: binary-shift-left \'<<\'.
   | ShiftRight { token_span :: !SrcSpan }                   -- ^ Operator: binary-shift-right \'>>\'.
   | Modulo { token_span :: !SrcSpan }                       -- ^ Operator: modulus \'%\'.
   | FloorDiv { token_span :: !SrcSpan }                     -- ^ Operator: floor-divide \'//\'.
   | Tilde { token_span :: !SrcSpan }                        -- ^ Operator: tilde \'~\'.
   | NotEquals { token_span :: !SrcSpan }                    -- ^ Operator: not-equals \'!=\'.

   -- Special cases
   | EOF { token_span :: !SrcSpan }                          -- ^ End of file 
   deriving (Eq,Ord,Show,Typeable,Data)

instance Span Token where
  getSpan = token_span 

instance Pretty Token where
   pretty token = 
      text (show $ toConstr token) <+> pretty (token_span token) <+>
         case token of
            Comment {}    -> quotes $ text $ token_comment token 
            Identifier {} -> text $ token_identifier token
            String {}     -> quotes $ text $ token_string token
            ByteString {} -> quotes $ text $ BS.unpack $ token_byte_string token 
            Integer {}    -> pretty $ token_integer token 
            Float {}      -> pretty $ token_double token
            Imaginary {}  -> pretty $ token_double token
            other         -> empty
   
prettyToken :: Token -> String
prettyToken = prettyText 
