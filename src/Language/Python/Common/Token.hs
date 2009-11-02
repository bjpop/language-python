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

module Language.Python.Common.Token ( Token (..), debugTokenStr) where

import Language.Python.Common.Pretty
import Language.Python.Common.SrcLocation (SrcSpan (..), SrcLocation (..), Location (location), Span(getSpan))
import qualified Data.ByteString.Char8 as BS (ByteString, unpack)
import Data.Data

-- | Lexical tokens.
data Token 
   -- Whitespace
   = IndentToken { token_span :: !SrcSpan }                       -- ^ Indentation: increase.
   | DedentToken { token_span :: !SrcSpan }                       -- ^ Indentation: decrease.
   | NewlineToken { token_span :: !SrcSpan }                      -- ^ Newline.

   -- Comment
   | CommentToken { token_span :: !SrcSpan, token_comment :: !String } -- ^ Single line comment.

   -- Identifiers 
   | IdentifierToken { token_span :: !SrcSpan, token_identifier :: !String }            -- ^ Identifier.

   -- Literals
   | StringToken { token_span :: !SrcSpan, token_string :: !String }                    -- ^ Literal: string.
   | ByteStringToken { token_span :: !SrcSpan, token_byte_string :: !BS.ByteString }    -- ^ Literal: byte string.
   | IntegerToken { token_span :: !SrcSpan, token_integer :: !Integer }                 -- ^ Literal: integer.
   | LongIntegerToken { token_span :: !SrcSpan, token_integer :: !Integer }             -- ^ Literal: long integer. Version 2 only.
   | FloatToken { token_span :: !SrcSpan, token_double :: !Double }                     -- ^ Literal: floating point.
   | ImaginaryToken { token_span :: !SrcSpan, token_double :: !Double }                 -- ^ Literal: imaginary number.

   -- Keywords
   | DefToken { token_span :: !SrcSpan }                          -- ^ Keyword: \'def\'. 
   | WhileToken { token_span :: !SrcSpan }                        -- ^ Keyword: \'while\'.
   | IfToken { token_span :: !SrcSpan }                           -- ^ Keyword: \'if\'.
   | TrueToken { token_span :: !SrcSpan }                         -- ^ Keyword: \'True\'.
   | FalseToken { token_span :: !SrcSpan }                        -- ^ Keyword: \'False\'.
   | ReturnToken { token_span :: !SrcSpan }                       -- ^ Keyword: \'Return\'.
   | TryToken { token_span :: !SrcSpan }                          -- ^ Keyword: \'try\'.
   | ExceptToken { token_span :: !SrcSpan }                       -- ^ Keyword: \'except\'.
   | RaiseToken { token_span :: !SrcSpan }                        -- ^ Keyword: \'raise\'.
   | InToken { token_span :: !SrcSpan }                           -- ^ Keyword: \'in\'.
   | IsToken { token_span :: !SrcSpan }                           -- ^ Keyword: \'is\'.
   | LambdaToken { token_span :: !SrcSpan }                       -- ^ Keyword: \'lambda\'.
   | ClassToken { token_span :: !SrcSpan }                        -- ^ Keyword: \'class\'.
   | FinallyToken { token_span :: !SrcSpan }                      -- ^ Keyword: \'finally\'.
   | NoneToken { token_span :: !SrcSpan }                         -- ^ Keyword: \'None\'
   | ForToken { token_span :: !SrcSpan }                          -- ^ Keyword: \'for\'.
   | FromToken { token_span :: !SrcSpan }                         -- ^ Keyword: \'from\'.
   | GlobalToken { token_span :: !SrcSpan }                       -- ^ Keyword: \'global\'.
   | WithToken { token_span :: !SrcSpan }                         -- ^ Keyword: \'with\'.
   | AsToken { token_span :: !SrcSpan }                           -- ^ Keyword: \'as\'.
   | ElifToken { token_span :: !SrcSpan }                         -- ^ Keyword: \'elif\'.
   | YieldToken { token_span :: !SrcSpan }                        -- ^ Keyword: \'yield\'.
   | AssertToken { token_span :: !SrcSpan }                       -- ^ Keyword: \'assert\'.
   | ImportToken { token_span :: !SrcSpan }                       -- ^ Keyword: \'import\'.
   | PassToken { token_span :: !SrcSpan }                         -- ^ Keyword: \'pass\'.
   | BreakToken { token_span :: !SrcSpan }                        -- ^ Keyword: \'break\'.
   | ContinueToken { token_span :: !SrcSpan }                     -- ^ Keyword: \'continue\'.
   | DeleteToken { token_span :: !SrcSpan }                       -- ^ Keyword: \'del\'.
   | ElseToken { token_span :: !SrcSpan }                         -- ^ Keyword: \'else\'.
   | NotToken { token_span :: !SrcSpan }                          -- ^ Keyword: \'not\'.
   | AndToken { token_span :: !SrcSpan }                          -- ^ Keyword: boolean conjunction \'and\'.
   | OrToken { token_span :: !SrcSpan }                           -- ^ Keyword: boolean disjunction \'or\'.
   -- Version 3.x only:
   | NonLocalToken { token_span :: !SrcSpan }                     -- ^ Keyword: \'nonlocal\' (Python 2.x only)
   -- Version 2.x only:
   | PrintToken { token_span :: !SrcSpan }                        -- ^ Keyword: \'print\'. (Python 3.x only)
   | ExecToken { token_span :: !SrcSpan }                         -- ^ Keyword: \'exec\'. (Python 3.x only)

   -- Delimiters
   | AtToken { token_span :: !SrcSpan }                           -- ^ Delimiter: at sign \'\@\'. 
   | LeftRoundBracketToken { token_span :: !SrcSpan }             -- ^ Delimiter: left round bracket \'(\'.
   | RightRoundBracketToken { token_span :: !SrcSpan }            -- ^ Delimiter: right round bracket \')\'.
   | LeftSquareBracketToken { token_span :: !SrcSpan }            -- ^ Delimiter: left square bracket \'[\'.
   | RightSquareBracketToken { token_span :: !SrcSpan }           -- ^ Delimiter: right square bracket \']\'.
   | LeftBraceToken { token_span :: !SrcSpan }                    -- ^ Delimiter: left curly bracket \'{\'.
   | RightBraceToken { token_span :: !SrcSpan }                   -- ^ Delimiter: right curly bracket \'}\'.
   | DotToken { token_span :: !SrcSpan }                          -- ^ Delimiter: dot (full stop) \'.\'.
   | CommaToken { token_span :: !SrcSpan }                        -- ^ Delimiter: comma \',\'.
   | SemiColonToken { token_span :: !SrcSpan }                    -- ^ Delimiter: semicolon \';\'.
   | ColonToken { token_span :: !SrcSpan }                        -- ^ Delimiter: colon \':\'.
   | EllipsisToken { token_span :: !SrcSpan }                     -- ^ Delimiter: ellipses (three dots) \'...\'.
   | RightArrowToken { token_span :: !SrcSpan }                   -- ^ Delimiter: right facing arrow \'->\'.
   | AssignToken { token_span :: !SrcSpan }                       -- ^ Delimiter: assignment \'=\'.
   | PlusAssignToken { token_span :: !SrcSpan }                   -- ^ Delimiter: plus assignment \'+=\'.
   | MinusAssignToken { token_span :: !SrcSpan }                  -- ^ Delimiter: minus assignment \'-=\'.
   | MultAssignToken { token_span :: !SrcSpan }                   -- ^ Delimiter: multiply assignment \'*=\'
   | DivAssignToken { token_span :: !SrcSpan }                    -- ^ Delimiter: divide assignment \'/=\'.
   | ModAssignToken { token_span :: !SrcSpan }                    -- ^ Delimiter: modulus assignment \'%=\'.
   | PowAssignToken { token_span :: !SrcSpan }                    -- ^ Delimiter: power assignment \'**=\'.
   | BinAndAssignToken { token_span :: !SrcSpan }                 -- ^ Delimiter: binary-and assignment \'&=\'.
   | BinOrAssignToken { token_span :: !SrcSpan }                  -- ^ Delimiter: binary-or assignment \'|=\'.
   | BinXorAssignToken { token_span :: !SrcSpan }                 -- ^ Delimiter: binary-xor assignment \'^=\'.
   | LeftShiftAssignToken { token_span :: !SrcSpan }              -- ^ Delimiter: binary-left-shift assignment \'<<=\'.
   | RightShiftAssignToken { token_span :: !SrcSpan }             -- ^ Delimiter: binary-right-shift assignment \'>>=\'.
   | FloorDivAssignToken { token_span :: !SrcSpan }               -- ^ Delimiter: floor-divide assignment \'//=\'.
   | BackQuoteToken { token_span :: !SrcSpan }                    -- ^ Delimiter: back quote character \'`\'.

   -- Operators
   | PlusToken { token_span :: !SrcSpan }                         -- ^ Operator: plus \'+\'.
   | MinusToken { token_span :: !SrcSpan }                        -- ^ Operator: minus: \'-\'.
   | MultToken { token_span :: !SrcSpan }                         -- ^ Operator: multiply \'*\'.
   | DivToken { token_span :: !SrcSpan }                          -- ^ Operator: divide \'/\'.
   | GreaterThanToken { token_span :: !SrcSpan }                  -- ^ Operator: greater-than \'>\'.
   | LessThanToken { token_span :: !SrcSpan }                     -- ^ Operator: less-than \'<\'.
   | EqualityToken { token_span :: !SrcSpan }                     -- ^ Operator: equals \'==\'.
   | GreaterThanEqualsToken { token_span :: !SrcSpan }            -- ^ Operator: greater-than-or-equals \'>=\'.
   | LessThanEqualsToken { token_span :: !SrcSpan }               -- ^ Operator: less-than-or-equals \'<=\'.
   | ExponentToken { token_span :: !SrcSpan }                     -- ^ Operator: exponential \'**\'.
   | BinaryOrToken { token_span :: !SrcSpan }                     -- ^ Operator: binary-or \'|\'.
   | XorToken { token_span :: !SrcSpan }                          -- ^ Operator: binary-xor \'^\'.
   | BinaryAndToken { token_span :: !SrcSpan }                    -- ^ Operator: binary-and \'&\'.
   | ShiftLeftToken { token_span :: !SrcSpan }                    -- ^ Operator: binary-shift-left \'<<\'.
   | ShiftRightToken { token_span :: !SrcSpan }                   -- ^ Operator: binary-shift-right \'>>\'.
   | ModuloToken { token_span :: !SrcSpan }                       -- ^ Operator: modulus \'%\'.
   | FloorDivToken { token_span :: !SrcSpan }                     -- ^ Operator: floor-divide \'//\'.
   | TildeToken { token_span :: !SrcSpan }                        -- ^ Operator: tilde \'~\'.
   | NotEqualsToken { token_span :: !SrcSpan }                    -- ^ Operator: not-equals \'!=\'.
   | NotEqualsV2Token { token_span :: !SrcSpan }                  -- ^ Operator: not-equals \'<>\'. Version 2 only.

   -- Special cases
   | EOFToken { token_span :: !SrcSpan }                          -- ^ End of file 
   deriving (Eq,Ord,Show,Typeable,Data)

instance Span Token where
  getSpan = token_span 
   
debugTokenStr :: Token -> String
debugTokenStr token =
   render (text (show $ toConstr token) <+> pretty (token_span token) <+>
      case token of
         CommentToken {}    -> quotes $ text $ token_comment token 
         IdentifierToken {} -> text $ token_identifier token
         StringToken {}     -> quotes $ text $ token_string token
         ByteStringToken {} -> quotes $ text $ BS.unpack $ token_byte_string token 
         IntegerToken {}    -> pretty $ token_integer token 
         LongIntegerToken {} -> pretty (token_integer token) <> char 'L'
         FloatToken {}      -> pretty $ token_double token
         ImaginaryToken  {} -> pretty $ token_double token
         other              -> empty)
