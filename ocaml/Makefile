EXE := main.native main.byte

all: $(EXE)

%.native: %.ml
	ocamlbuild -cflag -g -pkg unix $@

%.byte: %.ml
	ocamlbuild -cflag -g -lflag -g -pkg unix $@

.PHONY: clean
clean:
	ocamlbuild -clean
	rm -f *~
