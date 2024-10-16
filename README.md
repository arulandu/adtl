# ADTL: A(lvan's) Dependently Typed Language
This language takes heavy inspiration from the blog post
[How to implement dependent type theory I](http://math.andrej.com/2012/11/08/how-to-implement-dependent-type-theory-i/), which intersperses rewrite rules, explanation, and code. This project was my first line of OCaml, first chrome (lexer, parser, top-level), and first language; the guidance provided in the tutorial was a strong scaffold and I learned a lot. 

## Type Theory
The blog post described rewrite rules and implementation approaches for:
* The universes are `Type 0`, `Type 1`, `Type 2`, ...
* A dependent product is written as `forall x : T1, T2`
* A function is written as `fun x : T => e`
* Application is written as `e1 e2`

This project adds the following features to our dependently typed language:
* Natural numbers and induction (natural elimination) as `Nat, 0, succ, elimNat`
    * Type inference, normalization, lexing/parsing, pretty printing.
* Partial re-write of chrome: directive/syntax changes.
* Standard build process, testing, and new examples.

We also plan to add the following features in the near future:
* Cummulative universes, so that [A : Type k] implies [A : Type (k+1)]
* Currying syntax, e.g., allow `fun x y : nat => ...` instead of `fun x : nat => fun y : nat => ...`
* More basic types `unit`, `bool`
* Eta rule for functions
* Dependent sums

## Implementation
We use the `menhir` parser generator with `ocamllex` for lexing. We do not use de Bruijn indices, and instead generate fresh variables. 

## Usage
The build process runs using `dune`. Run `dune build` and `dune exec -- adtl`. For CLI usage, run `dune exec -- adtl -h` and for language usage type `Help.` in the REPL.

## Tests
Run `dune exec -- adtl -l ./tests/{path}.adtl` to load a file / run a single test. To run all tests, run `./test.sh` which will generate a `./tests/{name}.out` file for each test.

### Additive Commutativity
Most of the `.agda` [proof](https://plti.metareflection.club/assignments.html) has been converted and typechecks in `addcom2.adtl`. However, notice that because of our universes, `Same : (A : Type 0) -> A -> A -> Type 1`, where `Type 1` has replaced `*`. Since cumulative universes haven't been implemented yet, completing the agda proof is impossible for the time being since `Type 0` doesn't up-cast to `Type 1`. This is a bit unfortunate, but we're forced to settle. Similarly, `bool.adtl` implements the Church encoding for booleans, but this again fails for a similar reason.  
