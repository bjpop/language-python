{
module Language.Python.Version3.Parser.Lexer 
   (initStartCodeStack, lexToken, endOfFileToken, lexCont) where

import Language.Python.Version3.Parser.Token hiding (True, False)
import qualified Language.Python.Version3.Parser.Token 
import Language.Python.Version3.Parser.ParserMonad
import Language.Python.SrcLocation
import qualified Data.Map as Map
import Monad (liftM)
import Data.List (foldl')
import Numeric (readHex, readOct)
import qualified Data.ByteString.Char8 as BS (pack)
}

-- character sets
$lf = \n  -- line feed
$cr = \r  -- carriage return
$eol_char = [$lf $cr] -- any end of line character
$not_eol_char = ~$eol_char -- anything but an end of line character
$white_char   = [\ \n\r\f\v\t]
$white_no_nl = $white_char # $eol_char
$ident_letter = [a-zA-Z_]
$digit    = 0-9
$non_zero_digit = 1-9
$oct_digit = 0-7
$hex_digit = [$digit a-fA-F]
$bin_digit = 0-1 
$short_str_char = [^ \n \r ' \" \\]
$long_str_char = [. \n] # [' \"]
$short_byte_str_char = \0-\127 # [\n \r ' \" \\]
$long_byte_str_char = \0-\127 # [' \"]
$not_single_quote = [. \n] # '
$not_double_quote = [. \n] # \"

-- macro definitions
@exponent = (e | E) (\+ | \-)? $digit+ 
@fraction = \. $digit+
@int_part = $digit+
@point_float = (@int_part? @fraction) | @int_part \.
@exponent_float = (@int_part | @point_float) @exponent
@float_number = @point_float | @exponent_float
@eol_pattern = $lf | $cr $lf | $cr $lf  
@one_single_quote = ' $not_single_quote
@two_single_quotes = '' $not_single_quote
@one_double_quote = \" $not_double_quote
@two_double_quotes = \"\" $not_double_quote
@byte_str_prefix = b | B
@raw_str_prefix = r | R
@raw_byte_str_prefix = @byte_str_prefix @raw_str_prefix
@backslash_pair = \\ (\\|'|\"|@eol_pattern|$short_str_char)
@backslash_pair_bs = \\ (\\|'|\"|@eol_pattern|$short_byte_str_char)
@short_str_item_single = $short_str_char|@backslash_pair|\"
@short_str_item_double = $short_str_char|@backslash_pair|'
@short_byte_str_item_single = $short_byte_str_char|@backslash_pair_bs|\"
@short_byte_str_item_double = $short_byte_str_char|@backslash_pair_bs|'
@long_str_item_single = $long_str_char|@backslash_pair|@one_single_quote|@two_single_quotes|\"
@long_str_item_double = $long_str_char|@backslash_pair|@one_double_quote|@two_double_quotes|'
@long_byte_str_item_single = $long_byte_str_char|@backslash_pair_bs|@one_single_quote|@two_single_quotes|\"
@long_byte_str_item_double = $long_byte_str_char|@backslash_pair_bs|@one_double_quote|@two_double_quotes|'

tokens :-

-- these rules below could match inside a string literal, but they
-- will not be applied because the rule for the literal will always
-- match a longer sequence of characters. 

\# ($not_eol_char)* ;  -- skip comments 
$white_no_nl+  ;  -- skip whitespace 

\\ @eol_pattern ; -- line join 

<0> {
   @float_number { token Token.Float readFloat }
   $non_zero_digit $digit* { token Token.Integer read }  
   (@float_number | @int_part) (j | J) { token Token.Imaginary (readFloat.init) }
   0+ { token Token.Integer read }  
   0 (o | O) $oct_digit+ { token Token.Integer read }
   0 (x | X) $hex_digit+ { token Token.Integer read }
   0 (b | B) $bin_digit+ { token Token.Integer readBinary }
}

-- String literals 

<0> {
   ' @short_str_item_single* ' { mkString 1 1 stringToken }
   @raw_str_prefix ' @short_str_item_single* ' { mkString 2 1 rawStringToken }
   @byte_str_prefix ' @short_byte_str_item_single* ' { mkString 2 1 byteStringToken }
   @raw_byte_str_prefix ' @short_byte_str_item_single* ' { mkString 3 1 rawByteStringToken }

   \" @short_str_item_double* \" { mkString 1 1 stringToken }
   @raw_str_prefix \" @short_str_item_double* \" { mkString 2 1 rawStringToken }
   @byte_str_prefix \" @short_byte_str_item_double* \" { mkString 2 1 byteStringToken }
   @raw_byte_str_prefix \" @short_byte_str_item_double* \" { mkString 3 1 rawByteStringToken }

   ''' @long_str_item_single* ''' { mkString 3 3 stringToken }
   @raw_str_prefix ''' @long_str_item_single* ''' { mkString 4 3 rawStringToken }
   @byte_str_prefix ''' @long_byte_str_item_single* ''' { mkString 4 3 byteStringToken }
   @raw_byte_str_prefix ''' @long_byte_str_item_single* ''' { mkString 5 3 rawByteStringToken }

   \"\"\" @long_str_item_double* \"\"\" { mkString 3 3 stringToken }
   @raw_str_prefix \"\"\" @long_str_item_double* \"\"\" { mkString 4 3 rawStringToken }
   @byte_str_prefix \"\"\" @long_byte_str_item_double* \"\"\" { mkString 4 3 byteStringToken }
   @raw_byte_str_prefix \"\"\" @long_byte_str_item_double* \"\"\" { mkString 5 3 rawByteStringToken }
}

<0> {
   @eol_pattern     { begin bol }  
}

<dedent> ()                             { dedentation }

-- beginning of line
<bol> {
   @eol_pattern                         ;
   ()                                   { indentation BOL }
}

-- beginning of file
<bof> {
   @eol_pattern                         ;
   ()                                   { indentation BOF }
}


<0> $ident_letter($ident_letter|$digit)*  { \loc len str -> keywordOrIdent (take len str) loc }

-- operators and separators
--
<0> {
    "("   { openParen Token.LeftRoundBracket }
    ")"   { closeParen Token.RightRoundBracket }
    "["   { openParen Token.LeftSquareBracket }
    "]"   { closeParen Token.RightSquareBracket }
    "{"   { openParen Token.LeftBrace }
    "}"   { closeParen Token.RightBrace }
    "->"  { symbolToken Token.RightArrow }
    "."   { symbolToken Token.Dot }
    "..." { symbolToken Token.Ellipsis }
    "~"   { symbolToken Token.Tilde }
    "+"   { symbolToken Token.Plus }
    "-"   { symbolToken Token.Minus }
    "**"  { symbolToken Token.Exponent }
    "*"   { symbolToken Token.Mult }
    "/"   { symbolToken Token.Div }
    "//"  { symbolToken Token.FloorDiv }
    "%"   { symbolToken Token.Modulo }
    "<<"  { symbolToken Token.ShiftLeft }
    ">>"  { symbolToken Token.ShiftRight }
    "<"   { symbolToken Token.LessThan }
    "<="  { symbolToken Token.LessThanEquals }
    ">"   { symbolToken Token.GreaterThan }
    ">="  { symbolToken Token.GreaterThanEquals }
    "=="  { symbolToken Token.Equality }
    "!="  { symbolToken Token.NotEquals }
    "^"   { symbolToken Token.Xor }
    "|"   { symbolToken Token.BinaryOr }
    "&&"  { symbolToken Token.And }
    "&"   { symbolToken Token.BinaryAnd }
    "||"  { symbolToken Token.Or }
    ":"   { symbolToken Token.Colon }
    "="   { symbolToken Token.Assign }
    "+="  { symbolToken Token.PlusAssign }
    "-="  { symbolToken Token.MinusAssign }
    "*="  { symbolToken Token.MultAssign }
    "/="  { symbolToken Token.DivAssign }
    "%="  { symbolToken Token.ModAssign }
    "**=" { symbolToken Token.PowAssign }
    "&="  { symbolToken Token.BinAndAssign }
    "|="  { symbolToken Token.BinOrAssign }
    "^="  { symbolToken Token.BinXorAssign }
    "<<=" { symbolToken Token.LeftShiftAssign }
    ">>=" { symbolToken Token.RightShiftAssign }
    "//=" { symbolToken Token.FloorDivAssign } 
    ","   { symbolToken Token.Comma }
    "@"   { symbolToken Token.At }
    \;    { symbolToken Token.SemiColon }
}

{
-- Functions for building tokens 

type StartCode = Int
type Action = SrcLocation -> Int -> String -> P Token 

dedentation :: Action
dedentation loc _len _str = do
   let endCol = sloc_column loc 
   topIndent <- getIndent
   case compare endCol topIndent of
      EQ -> do popStartCode
               lexToken 
      LT -> do popIndent
               return dedentToken 
      GT -> failP loc ["indentation error"]

-- Beginning of. BOF = beginning of file, BOL = beginning of line
data BO = BOF | BOL

indentation :: BO -> Action 
-- Check if we are at the EOF. If yes, we may need to generate a newline,
-- in case we came here from BOL (but not BOF).
indentation bo _loc _len [] = do
   popStartCode
   case bo of
      BOF -> lexToken
      BOL -> return newlineToken
indentation bo loc _len _str = do
   popStartCode
   parenDepth <- getParenStackDepth
   if parenDepth > 0
      then lexToken
      else do 
         topIndent <- getIndent
         let endCol = sloc_column loc 
         case compare endCol topIndent of
            EQ -> case bo of
                     BOF -> lexToken
                     BOL -> return newlineToken   
            LT -> do pushStartCode dedent
                     return newlineToken 
            GT -> do pushIndent endCol 
                     return indentToken 
   where
   -- the location of the newline is not known here 
   newlineToken = Newline NoLocation
   indentToken = Indent loc 

begin :: StartCode -> Action 
begin code loc len inp = do
   pushStartCode code
   lexToken 

symbolToken :: (SrcLocation -> Token) -> Action 
symbolToken mkToken location _ _ = return (mkToken location)

token_fail :: String -> Action 
token_fail message location _ _ 
   = failP location [ "Lexical Error !", message]

token :: (SrcLocation -> a -> Token) -> (String -> a) -> Action 
token mkToken read location len str 
   = return $ mkToken location (read $ take len str)

-- a keyword or an identifier (the syntax overlaps)
keywordOrIdent :: String -> SrcLocation -> P Token
keywordOrIdent str location
   = return $ case Map.lookup str keywords of
         Just symbol -> symbol location
         Nothing -> Identifier location str  

-- mapping from strings to keywords
keywords :: Map.Map String (SrcLocation -> Token) 
keywords = Map.fromList keywordNames 

keywordNames :: [(String, SrcLocation -> Token)]
keywordNames =
   [ ("False", Token.False), ("class", Class), ("finally", Finally), ("is", Is), ("return", Return)
   , ("None", None), ("continue", Continue), ("for", For), ("lambda", Lambda), ("try", Try)
   , ("True", Token.True), ("def", Def), ("from", From), ("nonlocal", NonLocal), ("while", While)
   , ("and", And), ("del", Delete), ("global", Global), ("not", Not), ("with", With)
   , ("as", As), ("elif", Elif), ("if", If), ("or", Or), ("yield", Yield)
   , ("assert", Assert), ("else", Else), ("import", Import), ("pass", Pass)
   , ("break", Break), ("except", Except), ("in", In), ("raise", Raise)
   ]

-- The lexer starts off in the beginning of file state (bof)
initStartCodeStack :: [Int]
initStartCodeStack = [bof,0]

-- special tokens for the end of file and end of line
endOfFileToken :: Token
endOfFileToken = EOF
newlineToken = Newline NoLocation
dedentToken = Dedent NoLocation

-- Test if we are at the end of the line or file
atEOLorEOF :: a -> AlexInput -> Int -> AlexInput -> Bool
atEOLorEOF _user _inputBeforeToken _tokenLength (_loc, inputAfterToken) 
   = null inputAfterToken || nextChar == '\n' || nextChar == '\r'
   where
   nextChar = head inputAfterToken 

notEOF :: a -> AlexInput -> Int -> AlexInput -> Bool
notEOF _user _inputBeforeToken _tokenLength (_loc, inputAfterToken) 
   = not (null inputAfterToken)

readBinary :: String -> Integer
readBinary 
   = toBinary . drop 2 
   where
   toBinary = foldl' acc 0
   acc b '0' = 2 * b
   acc b '1' = 2 * b + 1

{-
floatnumber   ::=  pointfloat | exponentfloat
pointfloat    ::=  [intpart] fraction | intpart "."
exponentfloat ::=  (intpart | pointfloat) exponent
intpart       ::=  digit+
fraction      ::=  "." digit+
exponent      ::=  ("e" | "E") ["+" | "-"] digit+
-}
readFloat :: String -> Double
readFloat str@('.':cs) = read ('0':readFloatRest str)
readFloat str = read (readFloatRest str)
readFloatRest :: String -> String
readFloatRest [] = []
readFloatRest ['.'] = ".0"
readFloatRest (c:cs) = c : readFloatRest cs

mkString :: Int -> Int -> (SrcLocation -> String -> Token) -> Action
mkString leftSkip rightSkip toToken loc len str = do
   let contentLen = len - (leftSkip + rightSkip)
   let contents = take contentLen $ drop leftSkip str
   -- return $ String loc $ processString contents 
   return $ toToken loc contents 

stringToken :: SrcLocation -> String -> Token
stringToken loc str = String loc $ unescapeString str

rawStringToken :: SrcLocation -> String -> Token
rawStringToken loc str = String loc $ unescapeRawString str

byteStringToken :: SrcLocation -> String -> Token
byteStringToken loc str = ByteString loc $ BS.pack $ unescapeString str

rawByteStringToken :: SrcLocation -> String -> Token
rawByteStringToken loc str = ByteString loc $ BS.pack $ unescapeRawString str

openParen :: (SrcLocation -> Token) -> Action
openParen mkToken loc _len _str = do
   let token = mkToken loc
   pushParen token 
   return token 

closeParen :: (SrcLocation -> Token) -> Action
closeParen mkToken loc _len _str = do
  let token = mkToken loc
  topParen <- getParen
  case topParen of
     Nothing -> failP loc err1 
     Just open -> if matchParen open token 
                    then popParen >> return token
                    else failP loc err2
   where
   -- XXX fix these error messages
   err1 = ["Lexical error ! unmatched closing paren"]
   err2 = ["Lexical error ! unmatched closing paren"]

matchParen :: Token -> Token -> Bool
matchParen (LeftRoundBracket {}) (RightRoundBracket {}) = True
matchParen (LeftBrace {}) (RightBrace {}) = True
matchParen (LeftSquareBracket {}) (RightSquareBracket {}) = True
matchParen _ _ = False

unescapeString :: String -> String
unescapeString ('\\':'\\':cs) = '\\' : unescapeString cs -- Backslash (\)
unescapeString ('\\':'\'':cs) = '\'' : unescapeString cs -- Single quote (')
unescapeString ('\\':'"':cs) = '"' : unescapeString cs -- Double quote (")
unescapeString ('\\':'a':cs) = '\a' : unescapeString cs -- ASCII Bell (BEL)
unescapeString ('\\':'b':cs) = '\b' : unescapeString cs -- ASCII Backspace (BS)
unescapeString ('\\':'f':cs) = '\f' : unescapeString cs -- ASCII Formfeed (FF)
unescapeString ('\\':'n':cs) = '\n' : unescapeString cs -- ASCII Linefeed (LF)
unescapeString ('\\':'r':cs) = '\r' : unescapeString cs -- ASCII Carriage Return (CR)
unescapeString ('\\':'t':cs) = '\t' : unescapeString cs -- ASCII Horizontal Tab (TAB)
unescapeString ('\\':'v':cs) = '\v' : unescapeString cs -- ASCII Vertical Tab (VT)
unescapeString ('\\':'\n':cs) = unescapeString cs -- line continuation
unescapeString ('\\':rest@(o:_))
   | o `elem` octalDigits = unescapeNumeric 3 octalDigits (fst . head . readOct) rest 
unescapeString ('\\':'x':rest@(h:_))
   | h `elem` hexDigits = unescapeNumeric 2 hexDigits (fst . head . readHex) rest 
unescapeString (c:cs) = c : unescapeString cs 
unescapeString [] = []

unescapeRawString :: String -> String
unescapeRawString ('\\':'\'':cs) = '\'' : unescapeRawString cs -- Single quote (')
unescapeRawString ('\\':'"':cs) = '"' : unescapeRawString cs -- Double quote (")
unescapeRawString ('\\':'\n':cs) = unescapeRawString cs -- line continuation
unescapeRawString (c:cs) = c : unescapeRawString cs
unescapeRawString [] = []

{- 
   This is a bit complicated because Python allows between 1 and 3 octal
   characters after the \, and 1 and 2 hex characters after a \x.
-}
unescapeNumeric :: Int -> String -> (String -> Int) -> String -> String
unescapeNumeric n numericDigits readNumeric str
   = loop n [] str 
   where
   loop _ acc [] = [numericToChar acc]
   loop 0 acc rest
      = numericToChar acc : unescapeString rest
   loop n acc (c:cs)
      | c `elem` numericDigits = loop (n-1) (c:acc) cs
      | otherwise = numericToChar acc : unescapeString (c:cs)
   numericToChar :: String -> Char
   numericToChar = toEnum . readNumeric . reverse

octalDigits, hexDigits :: String
octalDigits = "01234567"
hexDigits = "0123456789abcdef"
 

-- -----------------------------------------------------------------------------
-- Functionality required by Alex 

type AlexInput = (SrcLocation, String)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar _ = error "alexInputPrevChar not used"

alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar (loc, input) 
   | null input  = Nothing
   | otherwise = Just (nextChar, (nextLoc, rest))
   where
   nextChar = head input
   rest = tail input 
   nextLoc = moveChar nextChar loc

moveChar :: Char -> SrcLocation -> SrcLocation 
moveChar '\n' = incLine 1 
moveChar '\t' = incTab 
moveChar '\r' = id 
moveChar _    = incColumn 1 

lexicalError :: P a
lexicalError = do
  location <- getLocation
  c <- liftM head getInput
  failP location 
        ["Lexical error !",
         "The character " ++ show c ++ " does not fit here."]

parseError :: P a
parseError = do
  token <- getLastToken
  failP (location token)
        ["Syntax error !",
         "The symbol `" ++ show token ++ "' does not fit here."]

lexToken :: P Token
lexToken = do
  location <- getLocation
  input <- getInput
  startCode <- getStartCode
  case alexScan (location, input) startCode of
    AlexEOF -> do
       depth <- getIndentStackDepth
       if depth <= 1 
          then return endOfFileToken
          else do 
             popIndent
             return dedentToken
    AlexError _ -> lexicalError
    AlexSkip (nextLocation, rest) len -> do
       setLocation nextLocation 
       setInput rest 
       lexToken
    AlexToken (nextLocation, rest) len action -> do
       setLocation nextLocation 
       setInput rest 
       token <- action location len input 
       setLastToken token
       return token

lexCont :: (Token -> P a) -> P a
lexCont cont = do
  tok <- lexToken
  cont tok
}
