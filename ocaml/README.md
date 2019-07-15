# Multicore OCaml Version

## Prerequisites

Install multicore OCaml from [the offical opam repository](https://github.com/ocaml-multicore/multicore-opam).

## For Emacs use

Install [tuareg](https://github.com/ocaml/tuareg). Installation via [Melpa](https://melpa.org/)
recommended. 

Additionally, install the following:
```
$ opam install merlin user-setup 
```

## Building and running

### In Emacs

Open `main.ml`, type `C-c C-b Ret` to load. 
Drawback of using tuareg: Error reporting nearly useless, as only the
absolute (!) character position is reported.
Consider using a shell within Emacs with the `dune` or REPL options below.

If changes are made, I recommend a complete reload:
Kill tuareg's REPL first with `C-c C-k` and then 
reload with `C-c C-b Ret` again.

Note: If you add new files, Be sure that the local `.ocamlinit` 
file in this repository properly loads the required `*.ml` files,
via the `#mod_use` directive.

### In a shell, using dune

```
$ opam install dune.1.2.1
```

(Note: newer versions of `dune` [do not yet work](https://github.com/ocaml-multicore/ocaml-multicore/issues/234) with multicore OCaml)

Building with `dev` profile (the default, includes debugging information)

```
$ dune build main.exe
```

Building with `release` profile (includes compiler optimizations)

```
$ dune build main.exe --profile release
```

### Notes on REPL use in the shell

Running

``` 
$ ocaml main.ml
```

_won't_ work. Neither does

```
$ ocaml -init .ocamlinit main.ml
```

Instead, first load up the REPL

```
$ ocaml
```

and then invoke

```
# #use "main.ml"
```
