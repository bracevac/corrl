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

### In a shell

Benefit: Better error reporting, generates native and byte code executables.

Just run

```
$ make
```

The main executable is any of `main.{native,byte,debug}`, where `main.debug`
yields more informative exception backtraces.
Each `*.ml` file whose name is prefixed with `test` is compiled to a separate
executable. 
