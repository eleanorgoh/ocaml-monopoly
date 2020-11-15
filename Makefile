MODULES=author src/cc_card src/player src/action src/property src/board
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
player-test:
	$(OCAMLBUILD) -tag 'debug' src/player_test.byte && ./player_test.byte

check:
	bash checkenv.sh

clean:
	ocamlbuild -clean

zip:
	zip monopoly.zip *.ml* *.json src/*.ml* .ocamlinit .merlin *.md _tags Makefile