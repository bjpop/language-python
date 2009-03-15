{-# OPTIONS  #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Python.Version3.Parser.ParserMonad 
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Monad support for Python parser and lexer. 
-----------------------------------------------------------------------------

module Language.Python.Version3.Parser.ParserMonad 
   ( P
   , execParser
   , failP
   , thenP
   , returnP
   , setLocation
   , getLocation
   , getInput
   , setInput
   , getLastToken
   , setLastToken
   , ParseError (ParseError)
   , State
   , initialState
   , pushStartCode
   , popStartCode
   , getStartCode
   , getIndent
   , pushIndent
   , popIndent
   , getIndentStackDepth
   , getParen
   , pushParen
   , popParen
   , getParenStackDepth
   ) where

import Language.Python.Data.SrcLocation (SrcLocation (..))
import Language.Python.Version3.Parser.Token (Token (..))

-- | Parse error. A list of error messages and a source location.
newtype ParseError = ParseError ([String], SrcLocation) 
   deriving Show

data ParseResult a
   = POk !State a
   | PFailed [String] SrcLocation   -- The error message and position

data State = 
   State 
   { location :: !SrcLocation -- position at current input location
   , input :: !String         -- the current input
   , previousToken :: Token   -- the previous token
   , startCodeStack :: [Int]  -- a stack of start codes for the state of the lexer
   , indentStack :: [Int]     -- a stack of source column positions of indentation levels
   , parenStack :: [Token]   -- a stack of parens and brackets for indentation handling
   }

initialState :: SrcLocation -> String -> [Int] -> State
initialState initLoc inp scStack
   = State
   { location = initLoc 
   , input = inp
   , previousToken = initToken
   , startCodeStack = scStack
   , indentStack = [1]
   , parenStack = []
   }

newtype P a = P { unP :: State -> ParseResult a }

instance Monad P where
   return = returnP
   (>>=) = thenP
   fail m = getLocation >>= \loc -> failP loc [m]

execParser :: P a -> State -> Either ParseError a
execParser (P parser) initialState =
   case parser initialState of
      PFailed message errloc -> Left (ParseError (message, errloc))
      POk st result -> Right result

initToken :: Token
initToken = Newline NoLocation

{-# INLINE returnP #-}
returnP :: a -> P a
returnP a = P $ \s -> POk s a

{-# INLINE thenP #-}
thenP :: P a -> (a -> P b) -> P b
(P m) `thenP` k = P $ \s ->
        case m s of
                POk s' a        -> (unP (k a)) s'
                PFailed err loc -> PFailed err loc 

failP :: SrcLocation -> [String] -> P a
failP loc msg = P $ \_ -> PFailed msg loc 

setLocation :: SrcLocation -> P ()
setLocation loc = P $ \s -> POk (s { location = loc }) ()

getLocation :: P SrcLocation
getLocation = P $ \s@State{ location = loc } -> POk s loc 

getInput :: P String 
getInput = P $ \s@State{ input = inp } -> POk s inp

setInput :: String -> P ()
setInput inp = P $ \s -> POk (s { input = inp }) ()

getLastToken :: P Token
getLastToken = P $ \s@State{ previousToken = tok } -> POk s tok

setLastToken :: Token -> P ()
setLastToken tok = P $ \s -> POk (s { previousToken = tok }) ()

pushStartCode :: Int -> P () 
pushStartCode code = P newStack
   where 
   newStack s@State{ startCodeStack = scStack } 
      = POk (s { startCodeStack = code : scStack}) () 

popStartCode :: P ()
popStartCode = P newStack
   where 
   newStack s@State{ startCodeStack = scStack, location = loc } 
      = case scStack of
           [] ->  PFailed err loc
           _:rest -> POk (s { startCodeStack = rest }) () 
   err = ["fatal error in lexer: attempt to pop empty start code stack"]

getStartCode :: P Int
getStartCode = P getCode
   where
   getCode s@State{ startCodeStack = scStack, location = loc }
      = case scStack of
           [] ->  PFailed err loc
           code:_ -> POk s code
   err = ["fatal error in lexer: start code stack empty on getStartCode"]

pushIndent :: Int -> P () 
pushIndent indent = P newStack
   where 
   newStack s@State{ indentStack = iStack } 
      = POk (s { indentStack = indent : iStack }) () 

popIndent :: P ()
popIndent = P newStack
   where 
   newStack s@State{ indentStack = iStack, location = loc } 
      = case iStack of
           [] -> PFailed err loc
           _:rest -> POk (s { indentStack = rest }) () 
   -- XXX this message needs fixing
   err = ["fatal error in lexer: attempt to pop empty indentation stack"]

getIndent :: P Int
getIndent = P get
   where
   get s@State{ indentStack = iStack, location = loc }
      = case iStack of
           [] -> PFailed err loc
           indent:_ -> POk s indent 
   -- XXX this message needs fixing
   err = ["fatal error in lexer: indent stack empty on getIndent"]

getIndentStackDepth :: P Int
getIndentStackDepth = P get
   where
   get s@State{ indentStack = iStack } = POk s (length iStack)

pushParen :: Token -> P () 
pushParen symbol = P newStack
   where 
   newStack s@State{ parenStack = pStack } 
      = POk (s { parenStack = symbol : pStack }) () 

popParen :: P ()
popParen = P newStack
   where 
   newStack s@State{ parenStack = pStack, location = loc } 
      = case pStack of
           [] -> PFailed err loc
           _:rest -> POk (s { parenStack = rest }) () 
   -- XXX this message needs fixing
   err = ["fatal error in lexer: attempt to pop empty paren stack"]

getParen :: P (Maybe Token)
getParen = P get
   where
   get s@State{ parenStack = pStack }
      = case pStack of
           [] -> POk s Nothing 
           symbol:_ -> POk s (Just symbol) 

getParenStackDepth :: P Int
getParenStackDepth = P get
   where
   get s@State{ parenStack = pStack } = POk s (length pStack)
