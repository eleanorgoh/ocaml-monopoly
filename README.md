# Monopoly
A Cornell themed Monopoly game written in Ocaml.

## Table of Contents
1. [Overview](#Overview)
2. [Game Demo](#Game-Demo)
3. [Install and Build](#Install-and-Build)
4. [Testing](#Testing)
5. [Documentation](#Documentation)
6. [Credits](#Credits)

## Overview
### Description
Insert overview of system here

### Features
- Insert description of features here  

## Game Demo
### Initializing the game
<img src="/assets/init_game.GIF" width=50/>

### Using the GUI as a game board
<img src="/assets/gui.GIF" width=50/>

### Playing the game
<img src="/assets/jail.GIF" width=50/>

## Install and Build
### Dependencies
Manually install the following dependencies: 
- OCaml Graphics: Run `opam install graphics`. 
Make sure you have XQuartz (Check 3110 page for instructions).
- Yojson: Run `opam install yojson`. 
- ANSITerminal: Run `opam install ANSIterminal`.

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