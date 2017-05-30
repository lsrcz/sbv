SBV: SMT Based Verification in Haskell
======================================

Express properties about Haskell programs and automatically prove them using SMT solvers.

```haskell
$ ghci
ghci> :m Data.SBV
ghci> prove $ \x -> x `shiftL` 2 .== 4 * (x::SWord8)
Q.E.D.
ghci> prove $ \x -> x `shiftL` 2 .== 2 * (x::SWord8)
Falsifiable. Counter-example:
  s0 = 32 :: Word8
```

The function `prove` establishes theorem-hood, while `sat` finds any satisfying model. All satisfying models can be lazily computed using `allSat`. SBV can also perform static assertion checks, such as absence of division-by-0, and other user given properties. Furthermore, SBV can perform
optimization, minimizing/maximizing arithmetic goals for their optimal values.

Overview
========

  - [![Hackage version](http://img.shields.io/hackage/v/sbv.svg?label=Hackage)](http://hackage.haskell.org/package/sbv) (Released: May 26th, 2017.)
  - [Release Notes](http://github.com/LeventErkok/sbv/tree/master/CHANGES.md). 

SBV library provides support for dealing with symbolic values in Haskell. It introduces the types:

  - `SBool`: Symbolic Booleans (bits).
  - `SWord8`, `SWord16`, `SWord32`, `SWord64`: Symbolic Words (unsigned).
  - `SInt8`,  `SInt16`,  `SInt32`,  `SInt64`: Symbolic Ints (signed).
  - `SInteger`: Symbolic unbounded integers (signed).
  - `SReal`: Symbolic infinite precision algebraic reals (signed).
  - `SFloat`: IEEE-754 single precision floating point number. (`Float`.)
  - `SDouble`: IEEE-754 double precision floating point number. (`Double.`)
  - Arrays of symbolic values.
  - Symbolic enumerations, for arbitrary user-defined enumerated types.
  - Symbolic polynomials over GF(2^n ), polynomial arithmetic, and CRCs.
  - Uninterpreted constants and functions over symbolic values, with user defined axioms.
  - Uninterpreted sorts, and proofs over such sorts, potentially with axioms.
  - Ability for users to define their own symbolic types, such as `SWord4`/`SInt4` as needed. (In a future version of SBV, we plan to support these automatically.)

The user can construct ordinary Haskell programs using these types, which behave like ordinary Haskell values when used concretely. However, when used with symbolic arguments, functions built out of these types can also be:

  - proven correct via an external SMT solver (the `prove` function),
  - checked for satisfiability (the `sat`, and `allSat` functions),
  - checked for assertion violations (the `safe` function with `sAssert` calls),
  - used in synthesis (the `sat` function with existentials),
  - optimized with respect to cost functions (the `optimize`, `maximize`, and `minimize` functions),
  - quick-checked,
  - used for generating Haskell and C test vectors (the `genTest` function),
  - compiled down to C, rendered as straight-line programs or libraries (`compileToC` and `compileToCLib` functions).


Picking the SMT solver to use
=============================
The SBV library uses third-party SMT solvers via the standard [SMT-Lib interface](http://smtlib.cs.uiowa.edu/). Use the following `import` statements to pick the corresponding solver:

  - `import Data.SBV.Bridge.ABC`<br>
     Uses [ABC](http://www.eecs.berkeley.edu/~alanmi/abc/) from University of Berkeley
  - `import Data.SBV.Bridge.Boolector`<br>
     Uses [Boolector](http://fmv.jku.at/boolector/) from Johannes Kepler University
  - `import Data.SBV.Bridge.CVC4`<br>
     Uses [CVC4](http://cvc4.cs.nyu.edu) from New York University and the University of Iowa
  - `import Data.SBV.Bridge.MathSAT`<br>
     Uses [MathSAT](http://mathsat.fbk.eu/) from FBK and DISI-University of Trento
  - `import Data.SBV.Bridge.Yices`<br>
     Uses [Yices](http://yices.csl.sri.com) from SRI
  - `import Data.SBV.Bridge.Z3`<br>
     Uses [Z3](http://github.com/Z3Prover/z3/wiki) from Microsoft
 
If you simply `import Data.SBV`, then the solver defaults to Microsoft's Z3.

See [versions](http://github.com/LeventErkok/sbv/blob/master/SMTSolverVersions.md) for a listing of the versions of these tools SBV has been tested with. Please report if you see any discrepancies!

Other SMT solvers can be used with SBV as well, with a relatively easy hook-up mechanism. Please
do get in touch if you plan to use SBV with any other solver.

Using multiple solvers, simultaneously
======================================
SBV also allows for running multiple solvers at the same time, either picking the result of the first to complete, or getting results from all. See `proveWithAny`/`proveWithAll` and `satWithAny`/`satWithAll` functions. The function `sbvAvailableSolvers` can be used to query the available solvers at run-time.

### Copyright, License
The SBV library is distributed with the BSD3 license. See [COPYRIGHT](http://github.com/LeventErkok/sbv/tree/master/COPYRIGHT) for
details. The [LICENSE](http://github.com/LeventErkok/sbv/tree/master/LICENSE) file contains
the [BSD3](http://en.wikipedia.org/wiki/BSD_licenses) verbiage.

Thanks
======
The following people reported bugs, provided comments/feedback, or contributed to the development of SBV in various ways: Kanishka Azimi, Reid Barton, Ian Blumenfeld, Ian Calvert, Christian Conkle, Iavor Diatchki, Robert Dockins, Thomas DuBuisson, Trevor Elliott, John Erickson, Adam Foltzer, Tom Hawkins, Brian Huffman, Joe Leslie-Hurd, Georges-Axel Jaloyan, Tom Sydney Kerckhove, Piërre van de Laar, Brett Letner, Philipp Meyer, Lee Pike, Gleb Popov, Rohit Ramesh, Stephan Renatus, Eric Seidel, Austin Seipp, Andrés Sicard-Ramírez, Don Stewart, Josef Svenningsson, Daniel Wagner, Sean Weaver, Nis Wegmann, and Jared Ziegler.
