module Main where
import System.Environment

main = do
    args <- getArgs
    putStrLn ("Hello, doof");
    res <- getLine
    putStrLn ("Hello, " ++ res)
