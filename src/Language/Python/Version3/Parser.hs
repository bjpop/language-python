module Language.Python.Version3.Parser (parse) where

import Language.Python.Version3.Parser.Parser (parseFileInput)
import Language.Python.Version3.Syntax.AST (Program)
import Language.Python.Data.SrcLocation (initialSrcLocation)
import Language.Python.Version3.Parser.ParserMonad (execParser, ParseError, initialState)
import Language.Python.Version3.Parser.Lexer (initStartCodeStack)

parse :: String -> String -> Either ParseError Program 
parse input srcName = 
   execParser parseFileInput state 
   where
   initLoc = initialSrcLocation srcName
   state = initialState initLoc input initStartCodeStack
