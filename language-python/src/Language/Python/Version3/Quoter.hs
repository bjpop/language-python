module Language.Python.Version3.Quoter (
   -- * Quoting modules
   pyModule,
   -- * Quoting statements
   pyStmt,
   -- * Quoting expressions
   pyExpr) where

import Language.Python.Version3.Parser as Parser

import Language.Python.Common.Token as Token
import Language.Python.Common.ParserMonad ( ParseError)
import Data.Data ( Data )

import Language.Haskell.TH 
import Language.Haskell.TH.Quote ( QuasiQuoter(..), dataToExpQ, dataToPatQ )

quoter :: (Data a) => (String -> String -> Either ParseError (a, [Token]))-> QuasiQuoter
quoter parser = QuasiQuoter
                { quoteExp = parseAndLift parser ""
                , quotePat = error "this quasiquoter does not support patterns"
                , quoteDec = error "this quasiquoter does not support declarations"
                , quoteType = error "this quasiquoter does not support types"
                }


parseAndLift:: (Data a) => (String -> String -> Either ParseError (a, [Token])) -> String -> String -> Q Exp
parseAndLift parser name content = do 
  parseResult <- apply parser name content
  dataToExpQ (const Nothing) parseResult 

apply:: (Monad m, MonadFail m) => 
        (String -> String -> Either ParseError (a, [Token])) 
         -> String -> String -> m a
apply parser name content  = 
  case parser content name of
    Left parseError -> fail $ show parseError
    Right (subAST, comments) -> return subAST


pyModule :: QuasiQuoter
pyModule = quoter Parser.parseModule


pyStmt :: QuasiQuoter
pyStmt = quoter Parser.parseStmt


pyExpr:: QuasiQuoter
pyExpr = quoter Parser.parseExpr


