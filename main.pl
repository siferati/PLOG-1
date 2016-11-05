/* Game Loop */

/* includes */
:- ensure_loaded('utilities.pl').
:- ensure_loaded('Board.pl').

/** TODO fix output 'R: :| '
* Processes user input (while asking questions)
* @param Q Hex Coordinate to insert Piece
* @param R Hex Coordinate to insert Piece
*/
processInput(Q, R):-
  write('It\'s your turn to play!\n'),
  write('Insert Piece:\n'),
  write('Q: '), read(Q),
  write('R: '), read(R).

/*
while (gameIsRunning)
{
  processInput();
  update();
  render();
}
*/

/** TODO switch between player1 and player2 | check for game end
* @param Board Game Board
*/
game(Board):-
  processInput(Q, R),
  placePiece(player1, Board, Q, R, NewBoard), !,
  printBoard(NewBoard),
  game(NewBoard).

/**
* Interface to start the game
*/
game:-
  emptyBoard(Board),
  printBoard(Board),
  game(Board).
