
OCB= ocamlbuild  -use-ocamlfind 

all:    native byte

clean:
	$(OCB) -clean

run:    native
	./main.native anthemes2.maxpat

native:
	$(OCB) main.native

byte:
	$(OCB) main.byte


.PHONY: all clean byte native run
