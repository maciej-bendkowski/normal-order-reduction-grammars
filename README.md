# Normal-order reduction grammars
Supplement to the "Normal-order reduction grammars" article [1].

[1] http://arxiv.org/abs/1603.01758

Changes:
========
Version 1.0 consisted of the normal-order reduction grammar construction
algorithm exposed as a single, standalone Haskell module. 

Version 2.0 introduces algebraic utilities solving the defining equations 
of corresponding ordinary generating functions. Computed ofgs are outputted 
as Mathematica expressions suitable for further automatic manipulation.
The main executable module serves as a convenient CLI.

The folder scripts/ includes two Mathematica scripts computing the constants in
the asymptotic approximation of combinators reducing in n normal-order reduction
steps, as well as their asymptotic density in the set of all SK-combinators.
Files 0.in up to 8.in contain the computed ogf for n = 0..8.

Building:
=========
1. stack build

Usage:
======
./norg-gf -?
