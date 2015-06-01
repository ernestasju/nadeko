-- | Main entry point to the application.
module Main where

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec
import qualified Navision as N

-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Welcome to FP Haskell Center!"
    putStrLn "Have a good day!"

    args <- getArgs
    putStrLn "Args:"
    putStrLn $ unlines args
    let [[t], fn] = args
    case t of
        'C' -> parseFromFile N.parseCodeunit fn >>= printResult
        'M' -> parseFromFile N.parseMenuSuite fn >>= printResult
        x   -> error $ "Invalid object type: " ++ [x] ++ "."

printResult :: (Show a) => Either ParseError a -> IO ()
printResult result =
    putStrLn $ case result of
                 Left err ->  "Error: " ++ show err
                 Right val -> "Found: " ++ show val

runParse :: (Show a) => Int -> Parser a -> String -> IO ()
runParse k p s = do
    putStrLn ""
    putStrLn $ "---- Input (Case " ++ show k  ++ ") ----"
    putStrLn s
    putStrLn $ "---- Output (Case " ++ show k  ++ ") ----"
    parseTest p s
