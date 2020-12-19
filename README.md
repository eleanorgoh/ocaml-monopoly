# Monopoly
A Cornell-themed Monopoly game written in OCaml.

## Table of Contents
1. [Overview](#Overview)
2. [Game Demo](#Game-Demo)
3. [Install and Build](#Install-and-Build)
4. [Testing](#Testing)
5. [Documentation](#Documentation)
6. [Credits](#Credits)

## Overview
### Description
Just like in classic Monopoly, users exchange money and claim properties in the fight to be the last man who’s not bankrupt. Each property on the default board is a location at Cornell with a corresponding color type, price, and rent.

### Features
- All features of classic Monopoly are supported.
- Users can design a board themselves and import it into the game in JSON format 
- Users can track the state of the game using our interactive GUI. They can move their player markers and add buildings to properties

## Game Demo
### Starting the game
<img src="/assets/init_game.GIF" width=450/>

### Using the GUI as a game board
<img src="/assets/gui.GIF" width=450/>

### Playing the game
<img src="/assets/jail.GIF" width=450/>

## Install and Build
### Dependencies
Manually install the following dependencies: 
- Graphics: Run `opam install graphics`. Mac users must have XQuartz; check instructions below.
- Yojson: Run `opam install yojson`. 
- ANSITerminal: Run `opam install ANSIterminal`.

Mac Users:
1. Download XQuartz.
2. If you used Macports to install OCaml, `opam switch reinstall 4.06.0`
3. If you used Homebrew, `brew reinstall ocaml --with-x11`

### Build
Open two command line windows, and from the top directory `~/monopoly`: 
- Run `make build` in both windows to build all compilation units. 
- In one of the windows, run `make play` to start the game and play it. 
- In the other window, run `make graphics` to open the GUI that can be used 
as a real-life board to keep track of any moves the players make in the game. 

## Testing
From the top directory `~/monopoly`: 
- Run `make tests` to run all OUnit tests contained in `test.ml`. 
- See the comment at the top of `test.ml` for the test plan and rationale. 

## Documentation
From the top directory `~/monopoly`: 
- Run `make docs` to generate HTML documentation. 
- Open `docs.public/index.html` in a local browser to view the documentation. 

## Credits
This Monopoly game was created by Eleanor Goh, Samantha Liu, Alan Li, and David Velasquez 
for the Fall 2020 CS 3110 final project. 