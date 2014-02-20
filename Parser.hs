module LambdaParser where

import Data.IORef
import Control.Monad
import Text.ParserCombinators.Parsec
import LambdaCalculus
import Expressions

-- | environment
type Env a = IORef [(String, a)]

nullEnv :: IO (Env a)
nullEnv = newIORef []

getVar :: Env a -> String -> IO (Maybe a)
getVar env name = liftM (lookup name) (readIORef env)

setVar :: Env a -> String -> a -> IO ()
setVar env name value = do
  e <- readIORef env
  writeIORef env (setVar' e name value)
  where setVar' :: [(String, a)] -> String -> a -> [(String, a)]
        setVar' xs key val = let (ys, zs) = span ((/= key). fst) xs
                             in case zs of
                               [] -> (key, val) : ys
                               (_:zs) -> ys ++ (key, val) : zs

-- Parsing
name :: Parser String
name = do
  first <- letter <|> char '_'
  rest <- many (letter <|> digit <|> char '_')
  return (first:rest)

variable :: Parser Expr
variable = liftM Variable name

abstraction :: Parser Expr
abstraction = do
  try (string "lambda")
  spaces
  n <- name
  spaces
  e <- expression
  return $ Lambda n e

application :: Parser Expr
application = do
  char '('
  e1 <- expression
  spaces
  e2 <- expression
  char ')'
  return $ App e1 e2

nat :: Parser Expr
nat = do
  n <- many digit
  return (toExpr (read n))
  where toExpr 0 = lzero
        toExpr n = lsucc (n-1)

expression :: Parser Expr
expression = abstraction <|> variable <|> application <|> nat
