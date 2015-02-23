-----------------------------------------------------------------------------
-- |
-- Module      : Language.Python.Common.PrettyToken 
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Pretty printing of tokens. Note the output is intended for displaying in
-- messages to the user, and may not be valid Python syntax. For instance
-- the pretty printing is useful for displaying parser error messages, but
-- not useful for producing concrete Python source.
-----------------------------------------------------------------------------

module Language.Python.Common.PrettyToken where

import Language.Python.Common.Token
import Language.Python.Common.Pretty

instance Pretty Token where
   pretty tok = 
      case tok of
        IndentToken {} -> text "indentation"
        DedentToken {} -> text "dedentation"
        NewlineToken {} -> text "end of line" 
        LineJoinToken {} -> text "line join"
        CommentToken { token_literal = str } -> 
           text "comment:" <+> prettyPrefix 10 str
        IdentifierToken { token_literal = str } ->
           text "identifier:" <+> text str 
        StringToken { token_literal = str } -> 
           text "string:" <+> prettyPrefix 10 str
        ByteStringToken { token_literal = str } ->
           text "byte string:" <+> prettyPrefix 10 str
        UnicodeStringToken { token_literal = str } ->
           text "unicode string:" <+> prettyPrefix 10 str
        IntegerToken { token_literal = str } ->
           text "integer:" <+> text str 
        LongIntegerToken { token_literal = str } ->
           text "long integer:" <+> text str 
        FloatToken { token_literal = str } ->
           text "floating point number:" <+> text str 
        ImaginaryToken { token_literal = str } ->
           text "imaginary number:" <+> text str 
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
