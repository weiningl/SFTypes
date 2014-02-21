module Main where

import System.IO
import Text.Printf
import Text.ParserCombinators.Parsec (parse)
import LambdaCalculus
import Parser

-- | environment
type Env = ShowS

createEnv :: String -> String -> Env
createEnv name expr x =
  printf "((lambda e. lambda %s. e %s) %s)" name x expr

zero = createEnv "0" "lambda f. lambda x. x"
one = createEnv "1" "lambda f. lambda x. (f x)"
add = createEnv "add" "lambda n1. lambda n2. lambda f. lambda x. ((n1 f) ((n2 f) x))"
-- | prompt
flushStr s = putStr s >> hFlush stdout

readPrompt = flushStr prompt >> getLine
  where prompt = ">>> "

runEnv :: String -> Env -> String
runEnv x env = case parse expression "lambda" (env x) of
  Right expr -> (show. eval) expr
  Left _ -> "Invalid expression: " ++ x

run :: String -> [Env] -> String
run = foldl runEnv

main = do
  input <- readPrompt
  (putStrLn. (flip run) env) input
  main
  where env = [zero, one, add]
