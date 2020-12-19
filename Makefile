MODULES=author src/cc_card src/player src/action src/property src/newboard src/command src/test_utils src/state src/render_state src/main
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
property-test:
	$(OCAMLBUILD) -tag 'debug' src/property_test.byte && ./property_test.byte
command-test:
	$(OCAMLBUILD) -tag 'debug' src/command_test.byte && ./command_test.byte
card-test:
	$(OCAMLBUILD) -tag 'debug' src/card_test.byte && ./card_test.byte
state-test:
	$(OCAMLBUILD) -tag 'debug' src/state_test.byte && ./state_test.byte

graphics:
	ocamlbuild -use-ocamlfind -pkg graphics src/render.native --

check:
	bash checkenv.sh

play:
	$(OCAMLBUILD) src/main.byte && ./main.byte

clean:
	ocamlbuild -clean

zip:
	zip monopoly.zip *.ml* *.json src/*.ml* .ocamlinit .merlin *.md _tags Makefile