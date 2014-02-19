module Expressions (
  -- * numbers
  lzero
  , lsucc
  ) where

import LambdaCalculus

-- | Natural numbers
f = Variable "f"
x = Variable "x"
lzero = Lambda "f" $ Lambda "x" x
lsucc 0 = exprMap (App f) lzero
lsucc n = exprMap (App f) (lsucc (n-1))
