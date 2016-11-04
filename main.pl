/* Game Loop */

/* includes */
:- include('utilities.pl').
:- include('Board.pl').

/*

while (gameIsRunning) {

...

}

*/

game:-
  write('Q: '),
  read(Q),
  write('R: '),
  read(R),
  write('output:\n'),
  write(Q),
  write(', '),
  write(R).
