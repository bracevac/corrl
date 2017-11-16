EXE := main.native

all: $(EXE)

%.native: %.ml
	ocamlbuild -pkg unix $@

.PHONY: clean
clean:
	ocamlbuild -clean
	rm -f *~
