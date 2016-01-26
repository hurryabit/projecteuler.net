module Problem176 where

{-
Let C(n) be the number of right triangles where one of the catheti has length n.
Then C(2^k * p1^e1 * ... * pr^er)1 is one of the following:

If k > 1, then [(2k-1) * (2e1+1) * ... * (2er+1) - 1] / 2.
If k = 1, then 0.
If k = 0, then [(2e1+1) * ... * (2er+1) - 1]/2.

Deduce that least n with C(n) = 47547 is

    n = 2^10 * 3^6 * 5^5 * 7^3 * 11^2 .

See OEIS A046079.
-}