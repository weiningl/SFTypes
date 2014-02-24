SF Types, Theorems, and Programming Languages

Untyped Lambda calculus Interpreter
- A simple implementation in Haskell that evaluates lambda expressions. 
- The design is modified from Tikhon Jelvis' call-by-name implementation [1] with a couple of enhancements.
  - Address the issue when substituting a lambda expression into an application, the β-reduction is not evaluated.
  - Unaddressed issue: renaming bound variables.
- Add parsing and show for more succinct syntax.
- Introducing contexts to allow built-in functions in the interpreter. Empty context in lambda calculus can be expressed as 
  (λe. e x), where x is the input to be interpreted. 
  By adding nested definition, such as 
  ((λe. λ0. e x) λf. λx. x),
  we can augment the interpreter with predefined definitions (0 in this case).
- Bootstrap natural numbers upon the context, recursive parse/eval to the base representation. Peano numbers are typically defined with zero and succ. Here the natural numbers are expanded from 0, 1, and add instead.
- References:
  - [1] http://jelv.is/talks/untyped-lambda-calculus.html
  - [2] http://en.wikipedia.org/wiki/Lambda_calculus