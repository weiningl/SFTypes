module Main where

import Control.Monad
import System.IO
import Text.ParserCombinators.Parsec
import LambdaCalculus
import LambdaParser

flushStr s = putStr s >> hFlush stdout

readPrompt = flushStr prompt >> getLine
  where prompt = ">>> "

main = do
  input <- readPrompt
  case parse expression "Lambda" input of
    Right expr -> (putStrLn. show. eval) expr
    Left _ -> putStrLn $ "Invalid expression: " ++ input
  main
