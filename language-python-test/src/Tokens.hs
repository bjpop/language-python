import Language.Python.Common
import Language.Python.Version2 as V2
import Language.Python.Version3 as V3
import System.Exit
import System.Environment

data PythonVersion = Two | Three
   deriving (Eq, Show)

type Lexer = String -> String -> Either ParseError [Token]

main :: IO ()
main = do
   args <- getArgs
   case args of
      (versionStr:inFile:_rest) ->
         case parseVersion versionStr of
            Nothing -> do
               putStrLn $ "Unknown Python version: " ++ versionStr
               exitFailure
            Just version -> do
               contents <- readFile inFile
               runLexer inFile (pickLexer version) contents
      _other -> putStrLn "Incorrect command line. Expected: <2|3> inputFileName"

pickLexer :: PythonVersion -> Lexer
pickLexer Two = V2.lex
pickLexer Three = V3.lex

parseVersion :: String -> Maybe PythonVersion
parseVersion "2" = Just Two
parseVersion "3" = Just Three
parseVersion _other = Nothing

runLexer :: FilePath -> Lexer -> String -> IO ()
runLexer inFile lex contents = do
   case lex contents inFile of
      Left e -> print e
      Right toks -> putStr $ unlines $ map debugTokenString toks
