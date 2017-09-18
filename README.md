# Implementing Event Correlation Patterns with Algebraic Effects
## Or: Solving the Expression Problem for Joins

CorrL is a DSL for defining expressive n-way joins over asynchronous
event sequences. Joins exists in different domains, e.g.,
stream-relational algebra, complex event patterns, (functional)
reactive programming and concurrent programming (join calculus).  With
CorrL, we can define join variants in a uniform and structured manner,
solving the expression problem.  The workhorse are algebraic effects
and handlers, which we employ to interpret declarative join pattern
specifications in different ways.

This repository features implementations of CorrL in different languages:

* [Koka](https://github.com/koka-lang/koka)
* [Multicore OCaml](https://github.com/ocamllabs/ocaml-multicore)

