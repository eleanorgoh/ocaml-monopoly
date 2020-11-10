MODULES=src/player
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

# test:
# 	$(OCAMLBUILD) -tag 'debug' src/test.byte && ./test.byte

check:
	bash checkenv.sh

clean:
	ocamlbuild -clean

zip:
	zip ocaml-messenger.zip *.ml* src/*.ml* .ocamlinit .merlin *.md _tags Makefile