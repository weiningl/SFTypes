{-# LANGUAGE UnicodeSyntax #-}
module LambdaCalculus (
  -- * types
  Name
  , Expr (..)

  -- * methods
  , eval
  ) where

-- | expressions
type Name = String
data Expr = Variable Name
          | Lambda Name Expr
          | App Expr Expr

instance Show Expr where
  showsPrec _ (Variable n) = (n++)
  showsPrec _ (Lambda n e) = showString "λ". (n++). showString ". ". shows e
  showsPrec _ (App e1 e2) = showParen True (shows e1. (" "++). shows e2)

-- | Evaluate lambda term
eval :: Expr -> Expr
eval (Variable n) = Variable n
eval (Lambda x e) = Lambda x e
eval (App e1 e2) = case eval e1 of
  (Lambda x body) -> subst x e2 body
  rlt -> App rlt e2

subst :: Name -> Expr -> Expr -> Expr
subst x v (Variable y)
  | x == y = v
  | otherwise = Variable y
subst x v (Lambda y body)
  | x == y = Lambda y body
  | otherwise = Lambda y (subst x v body)
subst x v (App e1 e2) = eval (App (subst x v e1) (subst x v e2))
