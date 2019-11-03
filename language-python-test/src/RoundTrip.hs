import Language.Python.Common
import Language.Python.Version2 as V2
import Language.Python.Version3 as V3
import System.Exit
import System.Environment

data PythonVersion = Two | Three | Both
   deriving (Eq, Show)

data Comparison = ParseFailed String | Equal | NotEqual String String
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
               let parsers = pickParsers version
                   comparisons = [parseAndCompare p inFile contents | p <- parsers]
               test <- check comparisons
               if test then exitWith ExitSuccess else exitSuccess
      _other -> putStrLn "Incorrect command line. Expected: <2|3|n> inputFileName"

check :: [Comparison] -> IO Bool 
check [] = return True -- must have all been equal
check (Equal:rest) = check rest
check (NotEqual s1 s2:_rest) = do
   doubleLine
   putStrLn "Round trip parse failed"
   doubleLine
   putStrLn "pretty1"
   line
   putStrLn s1
   doubleLine
   putStrLn "pretty2"
   line
   putStrLn s2
   return False
check (ParseFailed e:_rest) = do
   putStrLn "Parse failed with error: "
   putStrLn e
   return False

pickParsers :: PythonVersion -> [Parser]
pickParsers Two = [V2.parseModule]
pickParsers Three = [V3.parseModule]
pickParsers Both = [V2.parseModule, V3.parseModule]

parseAndCompare :: Parser -> FilePath -> String -> Comparison
parseAndCompare parser inFile contents =
   case parseAndPretty parser inFile contents of
      Left e -> ParseFailed $ prettyText e
      Right pretty1 ->
         case parseAndPretty parser "<pretty printed>" pretty1 of
            Left e -> ParseFailed $ prettyText e
            Right pretty2
               | pretty1 == pretty2 -> Equal
               | otherwise -> NotEqual pretty1 pretty2

line, doubleLine :: IO ()
line = putStrLn $ replicate 80 '-'
doubleLine = putStrLn $ replicate 80 '='

parseAndPretty :: Parser -> FilePath -> String -> Either ParseError String
parseAndPretty parser fileName contents =
   case parser contents fileName of
      Left e -> Left e
      Right (ast, _comments) -> Right (prettyText ast ++ "\n")

parseVersion :: String -> Maybe PythonVersion
parseVersion "2" = Just Two
parseVersion "3" = Just Three
parseVersion "n" = Just Both
parseVersion _other = Nothing
