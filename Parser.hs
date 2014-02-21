module Parser (
  -- * parsers
  variable
  , abstraction
  , application
  , expression
  ) where

import Control.Monad
import Text.ParserCombinators.Parsec
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

-- | Lambda calculus expression
expression :: Parser Expr
expression = abstraction <|> variable <|> application
