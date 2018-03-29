EXE := main.native main.byte 

TEST := $(patsubst %.ml,%.native,$(wildcard test*.ml)) $(patsubst %.ml,%.byte,$(wildcard test*.ml))


all: $(EXE) $(TEST) 

test%.byte: test%.ml
	ocamlbuild -cflag -g -lflag -g -pkg unix $@

%.native: %.ml
	ocamlbuild -cflag -g -pkg unix $@

%.byte: %.ml
	ocamlbuild -cflag -g -lflag -g -pkg unix $@

.PHONY: clean
clean:
	ocamlbuild -clean
	rm -f *~
