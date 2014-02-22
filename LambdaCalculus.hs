{-# LANGUAGE UnicodeSyntax #-}
module LambdaCalculus (
  -- * types
  Name
  , Expr (..)

  ) where

-- | expressions
-- Nat is a temporary type. It goes away in the parse/eval decomposition,
-- so it is purposefully not implemented in Show.
type Name = String
data Expr = Variable Name
          | Lambda Name Expr
          | App Expr Expr
          | Nat Int

instance Show Expr where
  showsPrec _ (Variable n) = (n++)
  showsPrec _ (Lambda n e) = showString "λ". (n++). showString ". ". shows e
  showsPrec _ (App e1 e2) = showParen True (shows e1. (" "++). shows e2)
