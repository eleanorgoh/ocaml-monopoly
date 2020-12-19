MODULES=author src/cc_card src/player src/action src/property src/newboard src/command src/state src/main src/render
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' src/test.byte && ./test.byte

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

docs: docs-public 
	
docs-public: _build/src
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build/src -package yojson,ANSITerminal \
		-html -stars -d doc.public $(MLIS)
