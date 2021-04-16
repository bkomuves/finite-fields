
Finite fields
=============

This is a Haskell library implementing arithmetics in finite fields.

The plan is to have different implementations tailored to different use cases:

* generic prime fields
* "small" prime fields (fitting into machine word)
* finite fields as vector spaces over prime fields, using precomputed Conway polynomials
* precalculated multiplication tables for very small fields 
* C code generation (with tables) for maximum efficiency 
* field extensions
* big finite fields relevant for cryptography
* maybe even JIT compiling for specific fields
* elliptic curves over finite fields? (again we could have "small" curves 
  for algebraic geometry and "very big" curves for cryptography...)

