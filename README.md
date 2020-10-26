# sudoku

A simple project I started for fun and to get extra practice at
translating imperative algorithms to a more declarative functional
style. Right now it is command line only. I may add an interactive 
web front end later.

## Installation

Download from https://github.com/musab/sudoku

## Usage

Assuming you have clojure (1.10.1) installed, you can use the following commands.

Run the project directly:

    $ clojure -M -m musab.sudoku

Run the project's tests (they'll fail until you edit them):

    $ clojure -M:test:runner

Build an uberjar:

    $ clojure -M:uberjar

Run that uberjar:

    $ java -jar sudoku.jar

## Input different board states

By default the board.edn file has a board state added under the input key. Apparently this particular puzzle is the hardest you can find. Feel free to modify the file

Follow the format of the example 2D array/vector for board.edn


## License

Copyright © 2020 Musab

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
