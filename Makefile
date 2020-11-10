MODULES=author src/cc_card src/player src/action
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

action-test:
	$(OCAMLBUILD) -tag 'debug' src/action_test.byte && ./action_test.byte

check:
	bash checkenv.sh

clean:
	ocamlbuild -clean

zip:
	zip ocaml-messenger.zip *.ml* src/*.ml* .ocamlinit .merlin *.md _tags Makefile