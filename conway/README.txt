
This is a database of Conway polynomials taken from:
<http://www.math.rwth-aachen.de/~Frank.Luebeck/data/ConwayPol/index.html>

It contains all prime powers q <= 2^30, and some more (I think it also 
has all those where p < 2^30 and p^m < 2^64, and for small p-s much more).

--------------------------------------------------------------------------------

All of the above polynomials are available in GAP, at least version 4.4.10. 
(September 2007)

You can import the data from these pages using this data file (1028 kB, or 
gzipped version with 219 kB). It has the following format:

 * the first line contains allConwayPolynomials := [
 * the last line contains 0 ];
 * other lines have the form [p, n, [a0, a1, ..., 1]], where p is a prime, n a positive integer 
   and [a0, a1, ..., 1] is a list of integers such that the Conway polynomial 
   fp,n(X) = (a0 mod p) + (a1 mod p) X + (a2 mod p) X2 + ...+ Xn.

The file can be read as is into, e.g., GAP or Maple. It should be easy to parse 
it with other programs, maybe after slight preparation with an editor. 