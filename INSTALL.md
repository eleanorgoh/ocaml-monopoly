# Dependencies
Manual install:
1. OCaml Graphics: Run `opam install graphics`. 
Make sure you have XQuartz (Check 3110 page for instructions).
2. Yojson: Run `opam install yojson`. 
3. ANSITerminal: Run `opam install ANSIterminal`.

# How to install and build Monopoly: 
Open two command line windows, and from the top directory `~/monopoly`: 
1. Run `make build` in both windows to build all compilation units. 
2. In one of the windows, run `make play` to start the game and play it. 
3. In the other window, run `make graphics` to open the GUI that can be used 
as a real-life board to keep track of any moves the players make in the game. 

# Testing: 
From the top directory `~/monopoly`: 
1. Run `make tests` to run all OUnit tests contained in `test.ml`. See the 
comment at the top of `test.ml` for the test plan and rationale. 

# Documentation
From the top directory `~/monopoly`: 
1. Run `make docs` to generate HTML documentation. 
2. Open `docs.public/index.html` in a local browser to view the documentation. 
