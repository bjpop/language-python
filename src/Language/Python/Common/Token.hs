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

module Language.Python.Common.Token ( Token (..), prettyToken, debugTokenStr) where

import Language.Python.Common.PrettyClass
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
   
prettyToken :: Token -> String
prettyToken = prettyText 

debugTokenStr :: Token -> String
debugTokenStr token =
   render (text (show $ toConstr token) <+> pretty (token_span token) <+>
      case token of
         CommentToken {}    -> quotes $ text $ token_comment token 
         IdentifierToken {} -> text $ token_identifier token
         StringToken {}     -> quotes $ text $ token_string token
         ByteStringToken {} -> quotes $ text $ BS.unpack $ token_byte_string token 
         IntegerToken {}    -> pretty $ token_integer token 
         FloatToken {}      -> pretty $ token_double token
         ImaginaryToken  {} -> pretty $ token_double token
         other              -> empty)

instance Pretty Token where
   pretty tok = 
      case tok of
        IndentToken {} -> text "indentation"
        DedentToken {} -> text "dedentation"
        NewlineToken {} -> text "end of line" 
        CommentToken { token_comment = str } -> 
           text "comment:" <+> prettyPrefix 5 str
        IdentifierToken { token_identifier = str } ->
           text "identifier:" <+> text str 
        StringToken { token_string = str } -> 
           text "string:" <+> quotes (prettyPrefix 5 str)
        ByteStringToken { token_byte_string = str } ->
           text "byte string:" <+> quotes (prettyPrefix 5 (BS.unpack $ str))
        IntegerToken { token_integer = i } ->
           text "integer:" <+> pretty i
        FloatToken { token_double = d } ->
           text "floating point number:" <+> pretty d
        ImaginaryToken { token_double = d } ->
           text "imaginary number:" <+> pretty d
        DefToken {} -> text "def" 
        WhileToken {} -> text "while"
        IfToken {} -> text "if"
        TrueToken {} -> text "True"
        FalseToken {} -> text "False"
        ReturnToken {} -> text "return"
        TryToken {} -> text "try"
        ExceptToken {} -> text "except"
        RaiseToken {} -> text "raise"
        InToken {} -> text "in" 
        IsToken {} -> text "is" 
        LambdaToken {} -> text "lambda" 
        ClassToken {} -> text "class"                       
        FinallyToken {} -> text "finally"                     
        NoneToken {} -> text "None"
        ForToken {} -> text "for"
        FromToken {} -> text "from"
        GlobalToken {} -> text "global"
        WithToken {} -> text "with"
        AsToken {} -> text "as"
        ElifToken {} -> text "elif"
        YieldToken {} -> text "yield"
        AssertToken {} -> text "assert"
        ImportToken {} -> text "import"
        PassToken {} -> text "pass"
        BreakToken {} -> text "break"
        ContinueToken {} -> text "continue"
        DeleteToken {} -> text "delete"
        ElseToken {} -> text "else"
        NotToken {} -> text "not"
        AndToken {} -> text "and"
        OrToken {} -> text "or"
        NonLocalToken {} -> text "nonlocal"
        PrintToken {} -> text "print"
        ExecToken {} -> text "exec"
        AtToken {} -> text "at"
        LeftRoundBracketToken {} -> text "("
        RightRoundBracketToken {} -> text ")"
        LeftSquareBracketToken {} -> text "["
        RightSquareBracketToken {} -> text "]"
        LeftBraceToken {} -> text "{"
        RightBraceToken {} -> text "}"
        DotToken {} -> text "." 
        CommaToken {} -> text ","
        SemiColonToken {} -> text ";"
        ColonToken {} -> text ":"
        EllipsisToken {} -> text "..."
        RightArrowToken {} -> text "->"
        AssignToken {} -> text "="
        PlusAssignToken {} -> text "+="
        MinusAssignToken {} -> text "-="
        MultAssignToken {} -> text "*="
        DivAssignToken {} -> text "/="
        ModAssignToken {} -> text "%="
        PowAssignToken {} -> text "**="
        BinAndAssignToken {} -> text "&="
        BinOrAssignToken {} -> text "|="
        BinXorAssignToken {} -> text "^="
        LeftShiftAssignToken {} -> text "<<="
        RightShiftAssignToken {} -> text ">>="
        FloorDivAssignToken {} -> text "//="
        BackQuoteToken {} -> text "` (back quote)"
        PlusToken {} -> text "+"
        MinusToken {} -> text "-"
        MultToken {} -> text "*" 
        DivToken {} -> text "/"
        GreaterThanToken {} -> text ">"
        LessThanToken {} -> text "<"
        EqualityToken {} -> text "=="
        GreaterThanEqualsToken {} -> text ">="
        LessThanEqualsToken {} -> text "<="
        ExponentToken {} -> text "**" 
        BinaryOrToken {} -> text "|"
        XorToken {} -> text "^"
        BinaryAndToken {} -> text "&"
        ShiftLeftToken {} -> text "<<"
        ShiftRightToken {} -> text ">>"
        ModuloToken {} -> text "%" 
        FloorDivToken {} -> text "//"
        TildeToken {} -> text "~"
        NotEqualsToken {} -> text "!="
        NotEqualsV2Token {} -> text "<>"
        EOFToken {} -> text "end of input"
