# Dependencies
Manual install:
1. OCaml Graphics: Run `opam install graphics`. Make sure you have XQuartz (Check 3110 page for instructions).
2. Yojson: Run `opam install yojson`. 
3. ANSITerminal: Run `opam install yojson`.

# How to install and build Monopoly: 

1. Run `make build` from top directory `~/monopoly`. 
2. Run `make graphics`.
3. Run `make action-test` from the top directory to run `Action` module tests. 
4. Run `make player-test` from the top directory to run `Player` module tests. 