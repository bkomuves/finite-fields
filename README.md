
Finite fields
=============

This is a Haskell library implementing arithmetics in finite fields.


Status
------

So far we have implemented:

* generic prime fields - module `Math.FiniteField.PrimeField.Generic`
* small prime fields, where small means `p < 2^31` - module  `Math.FiniteField.PrimeField.Small`
* small Galois fields, using a precomputed table of Conway polynomials - module `Math.FiniteField.GaloisField.Small`
* small Galois fields, using tables of Zech's logarithm
* C implementation of the Zech's logarithm stuff
* subfields (only for the Zech representation)

Further plans
--------------

* generic field extensions
* polynomials over finite fields
* big finite fields relevant for cryptography
* maybe even JIT compiling specific fields would make sense?
* optional: elliptic curves over finite fields? (again we could have "small" curves 
  for algebraic geometry and "very big" curves for cryptography...)


How to use
----------

The API is still in flux while I try to figure out the balance between ergonomy
and type-safety.

For now, each field has a "witness" which has two purposes: 1) it's a proof that
the field actually exists (eg. `p` is a prime); and 2) carrying the necessary
data to for computations (eg. Conway polynomials). So there are two Haskell types
for each implementation: the witness represents the field itself, and the other 
type represents the elements of the field.

Both the witness types and the field element types are parametrized by the field 
parameters (eg. `p` and `m` for a field of order `q = p^m`). You can "create" fields 
wrapped into existential types, and then you can do `case _ of` on that existential 
type and do the calculations inside. Unfortunately this also means that the API is not 
really `ghci`-friendly...

There is a common type class for all fields (defined in module `Math.FiniteField.Class`), 
so you can write polymorphic code and run it "in" any field implementation.

There are some example applications in the `examples` subdirectory.

