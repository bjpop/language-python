-----------------------------------------------------------------------------
-- |
-- Module      : Language.Python.Common.Quoter
-- License     : BSD-style
-- Stability   : experimental
-- Portability : ghc
--
-- Simple quasiquoters for converting python code into haskell expressions or patterns (it doesn't work for declarations and types). 
-- Multi-line python quotations must be left aligned in the file. 
-- Examples, that will yield :
-- @
-- binOpExpr = [pyExpr|x+y|]
-- 
-- [retStmt] = [pyStmt|return z|]
-- 
-- [py2Print] = [py2] = [py2Stmt|print somelist[1], "Hej there!"|]
-- 
-- pyMod = [pyModule|
-- import something
-- 
-- def fun(a,b):
--     c = a+b
--     print(c)
-- |]
-- @
-- Caution: This checks only for syntax of single quotes, not types or scopes or combinations of quotes.
-- For example nothing keeps you from appending py2Print to pyMod. So this can be used to generate code,
-- but checking validity is up to you.
-----------------------------------------------------------------------------

module Language.Python.Common.Quoter (
   -- * Quoting python 3.x modules
   pyModule,
   -- * Quoting python 3.x statements
   pyStmt,
   -- * Quoting python 3.x expressions
   pyExpr,
   -- * Quoting python 2.x modules
   py2Module,
   -- * Quoting python 2.x statements
   py2Stmt,
   -- * Quoting python 2.x expressions
   py2Expr) where

import Language.Python.Version3.Parser as V3
import Language.Python.Version2.Parser as V2

import Language.Python.Common.Token as Token
import Language.Python.Common.ParserMonad ( ParseError)
import Data.Data ( Data )

import Language.Haskell.TH 
import Language.Haskell.TH.Quote ( QuasiQuoter(..), dataToExpQ, dataToPatQ )

quoter :: (Data a) => (String -> String -> Either ParseError (a, [Token]))-> QuasiQuoter
quoter parser = QuasiQuoter
                { quoteExp = parseAndExpQuote parser 
                , quotePat = parseAndPatQuote parser
                , quoteDec = error "this quasiquoter does not support declarations"
                , quoteType = error "this quasiquoter does not support types"
                }


parseAndExpQuote:: (Data a) => (String -> String -> Either ParseError (a, [Token])) -> String -> Q Exp
parseAndExpQuote parser content = do 
  parseResult <- apply parser "" content
  dataToExpQ (const Nothing) parseResult 

parseAndPatQuote:: (Data a) => (String -> String -> Either ParseError (a, [Token]))-> String -> Q Pat
parseAndPatQuote parser content = do 
  parseResult <- apply parser "" content
  dataToPatQ (const Nothing) parseResult 

apply:: (Monad m, MonadFail m) => 
        (String -> String -> Either ParseError (a, [Token])) 
         -> String -> String -> m a
apply parser name content  = 
  case parser content name of
    Left parseError -> fail $ show parseError
    Right (subAST, comments) -> return subAST


pyModule :: QuasiQuoter
pyModule = quoter V3.parseModule


pyStmt :: QuasiQuoter
pyStmt = quoter V3.parseStmt


pyExpr:: QuasiQuoter
pyExpr = quoter V3.parseExpr


py2Module :: QuasiQuoter
py2Module = quoter V2.parseModule


py2Stmt :: QuasiQuoter
py2Stmt = quoter V2.parseStmt


py2Expr:: QuasiQuoter
py2Expr = quoter V2.parseExpr


