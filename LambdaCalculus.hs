{-# LANGUAGE UnicodeSyntax #-}
module LambdaCalculus where

-- | expressions
type Name = String
data Expr = Variable Name
          | Lambda Name Expr
          | App Expr Expr

instance Show Expr where
  showsPrec _ (Variable n) = (n++)
  showsPrec p x@(Lambda n e) = showParen (p > 10) $
                               showString "λ". (n++). showString ". ".
                               showsPrec (prec x e) e
  showsPrec p x@(App e1 e2) = showParen (p > 10) $
                              showsPrec (prec x e1) e1.
                              showString " ".
                              showsPrec (prec x e2) e2

exprMap :: (Expr -> Expr) -> Expr -> Expr
exprMap f x@(Variable _) = f x
exprMap f (Lambda n e) = Lambda n (exprMap f e)
exprMap f (App e1 e2) = App e1 (exprMap f e2)

prec :: Expr -> Expr -> Int
prec (App _ _) (Lambda _ _) = 11
prec (App _ _) (App _ _) = 11
prec _ _ = 0

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
subst x v (App e1 e2) = App (subst x v e1) (subst x v e2)
