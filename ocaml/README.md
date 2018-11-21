# Multicore OCaml Version

## Prerequisites

Install Multicore OCaml

```
$ opam remote add ocamllabs -k git https://github.com/ocamllabs/opam-repo-dev
$ opam switch 4.04.2+ber-multicore
```

## For Emacs use

Install [tuareg](https://github.com/ocaml/tuareg). Installation via [Melpa](https://melpa.org/)
recommended. 

Additionally, install the following:
```
$ opam install merlin user-setup 
```

## Building and running

### In Emacs

*TODO:* Currently not working properly, need to make merlin
aware of multiple project files.

Open `main.ml`, type `C-c C-b Ret` to load. 
Drawback of using tuareg: Error reporting nearly useless, as only the
absolute (!) character position is reported.

If changes are made, I recommend a complete reload:
Kill tuareg's REPL first with `C-c C-k` and then 
reload with `C-c C-b Ret` again.

Note: Be sure that the `.ocamlinit` properly loads
the required `*.ml` via `#mod_use`.

### In a shell, using dune

```
$ opam install dune
```

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
