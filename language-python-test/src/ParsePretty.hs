import Language.Python.Common
import Language.Python.Version2 as V2
import Language.Python.Version3 as V3
import System.Exit
import System.Environment

data PythonVersion = Two | Three
   deriving (Eq, Show)

type Parser = String -> String -> Either ParseError (ModuleSpan, [Token])

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
               let parser = pickParser version
               case parseAndPretty parser inFile contents of
                  Left error -> putStrLn $ prettyText error
                  Right ast -> putStrLn $ prettyText ast 
      _other -> putStrLn "Incorrect command line. Expected: <2|3|n> inputFileName"

pickParser :: PythonVersion -> Parser
pickParser Two = V2.parseModule
pickParser Three = V3.parseModule

parseAndPretty :: Parser -> FilePath -> String -> Either ParseError ModuleSpan
parseAndPretty parser fileName contents =
   case parser contents fileName of
      Left e -> Left e
      Right (ast, _comments) -> Right ast

parseVersion :: String -> Maybe PythonVersion
parseVersion "2" = Just Two
parseVersion "3" = Just Three
parseVersion _other = Nothing
