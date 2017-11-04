# GeneticProgramming

This program implements genetic programming to generate a random population of expression trees. Over generations the best
should be selected and crossed with a chance of mutation. They are judged by the distance from the target value and should
converge to it.

## Getting Started

To run program use GNU CLISP found at:
https://clisp.sourceforge.io/

## Usage

After cloning repository and going to the containing directory the program can be run with the command:

clisp GeneticProgramming.lisp <X value> <Y value> <Z value> <output value> <initial population size> <generations to run>

e.x:

clisp GeneticProgramming.lisp 5 -2 -20 25 30 20

![](https://media.giphy.com/media/xUNd9Rxjwp1Uhzm1S8/giphy.gif)

## Author

Jacob Biloki : bilokij@gmail.com

## License

This project is licensed under the MIT License

## Acknowledgments

Supporing code for tree structures provided by Charles Siska : csiska@fullerton.edu
