import Language.Python.Common
import Language.Python.Version2 as V2
import Language.Python.Version3 as V3
import System.Exit

data PythonVersion = Two | Three
   deriving (Eq, Show)

version = Two

main :: IO ()
main = do
   args <- getArgs
   case args of
      (versionStr:inFile:_rest) -> do
         let version = parseVersion versionStr
             parser = if version == Two then V2.parseModule else V3.parseModule
         contents <- readFile inFile
         pretty1 <- parseAndPretty parser inFile contents
         pretty2 <- parseAndPretty parser "<pretty printed>" pretty1
         if pretty1 == pretty2
            then do
               putStrLn "Round trip parse succeeded"
            else do
               doubleLine
               putStrLn "Round trip parse failed"
               doubleLine
               putStrLn "pretty1"
               line
               putStrLn pretty1
               doubleLine
               putStrLn "pretty2"
               line
               putStrLn pretty2
               exitFailure
      other ->
         putStrLn "Incorrect command line. Expected: pythonVersion inputFileName"

line, doubleLine :: IO ()
line = putStrLn $ replicate 80 '-'
doubleLine = putStrLn $ replicate 80 '='

parseAndPretty parser fileName contents =
   case parser contents fileName of
      Left e -> do
         putStrLn "Parse failed with error:"
         putStrLn $ prettyText e
         exitFailure
      Right (ast, _comments) -> do
         return $ prettyText ast ++ "\n"

parseVersion :: String -> PythonVersion
parseVersion "2" = Two
parseVersion "3" = Three
