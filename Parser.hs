module Parser (
  -- * parsers
  variable
  , abstraction
  , application
  , expression

  -- * evaluator
  , eval
  ) where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.Printf
import LambdaCalculus

-- Parsing
name :: Parser String
name = many1 (letter <|> digit <|> char '_')

-- | Simple variable
variable :: Parser Expr
variable = liftM Variable name

-- | Lambda
abstraction :: Parser Expr
abstraction = do
  try (string "lambda" <|> string "λ")
  spaces
  n <- name
  spaces
  char '.'
  spaces
  e <- expression
  return $ Lambda n e

-- | combination
application :: Parser Expr
application = do
  char '('
  e1 <- expression
  spaces
  e2 <- expression
  char ')'
  return $ App e1 e2

-- | Natural numbers
nat :: Parser Expr
nat = liftM (eval. Nat. read) $ many1 digit

-- | Lambda calculus expression
expression :: Parser Expr
expression = abstraction <|> nat <|> variable <|> application

-- | Evaluate lambda term
eval :: Expr -> Expr
eval (Variable n) = Variable n
eval (Lambda x e) = Lambda x e
eval (App e1 e2) = case eval e1 of
  (Lambda x body) -> subst x e2 body
  rlt -> App rlt e2
eval (Nat 0) = Variable "0"
eval (Nat 1) = Variable "1"
eval (Nat n) = (eval. runEither. parse expression "nat". succ) n
               where succ 0 = "0"
                     succ n = printf "((add 1) %d)" (n-1)
                     runEither x = case x of
                       Right r -> r

subst :: Name -> Expr -> Expr -> Expr
subst x v (Variable y)
  | x == y = v
  | otherwise = Variable y
subst x v (Lambda y body)
  | x == y = Lambda y body
  | otherwise = Lambda y (subst x v body)
subst x v (App e1 e2) = eval (App (subst x v e1) (subst x v e2))
