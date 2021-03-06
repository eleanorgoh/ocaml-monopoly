# Dependencies
Manual install:
1. Graphics: Run `opam install graphics`. Mac users must have XQuartz; check instructions below.
2. Yojson: Run `opam install yojson`. 
3. ANSITerminal: Run `opam install ANSIterminal`.

Mac Users:
1. Download XQuartz.
2. If you used Macports to install OCaml, `opam switch reinstall 4.06.0`
3. If you used Homebrew, `brew reinstall ocaml --with-x11`

# How to install and build Monopoly: 
Open two command line windows, and from the top directory `~/monopoly`: 
1. Run `make build` in both windows to build all compilation units. 
2. In one of the windows, run `make play` to start the game and play it. 
3. In the other window, run `make graphics` to open the GUI that can be used 
as a real-life board to keep track of any moves the players make in the game. 

# Testing: 
From the top directory `~/monopoly`: 
1. Run `make test` to run all OUnit tests contained in `test.ml`. See the 
comment at the top of `test.ml` for the test plan and rationale. 

# Documentation
From the top directory `~/monopoly`: 
1. Run `make docs` to generate HTML documentation. 
2. Open `docs.public/index.html` in a local browser to view the documentation. 
