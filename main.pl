/* Game Loop */

/* includes */
:- ensure_loaded('utilities.pl').
:- ensure_loaded('Board.pl').

/**
* Handles and prints errors to the terminal
* @param Log Error log
*/
printError(none).                                   /* no errors, do nothing */
printError(invalidPlay):- write('Invalid play!').   /* invalid play */

/**
* Switch player turn (in case there were no errors)
* @param Player Current player
* @param NewPlayer Next player
* @param Log Error log
*/
switchPlayer(player1, player2, none).
switchPlayer(player1, player1, _).
switchPlayer(player2, player1, none).
switchPlayer(player2, player2, _).

/**
* Get current player
* @param Player Current player
* @param PlayerName Name of the current player
*/
getPlayer(player1, 'Player 1').
getPlayer(player2, 'Player 2').

/** TODO utilizar getchar em vez de read!
* Processes user input (while asking questions)
* @param Player Current player
* @param Q Hex Coordinate to insert Piece
* @param R Hex Coordinate to insert Piece
*/
processInput(Player, Q, R):-
  write('\nCurrent Player: '), getPlayer(Player, PlayerName),
  write(PlayerName), nl,
  write('Insert Piece:\n'),
  write('Q: '), read(Q),
  write('R: '), read(R).

/** TODO game over screen
* Prints the game over screen to the terminal
*/
printGameOver:-
  write('Game Over\n').

/*
while (gameIsRunning)
{
  processInput();
  update();
  render();
}
*/

/**
* @param Player Current player
* @param Board Game Board
*/
game(Player, Board):-
  /*!,                                                  /* if game fails, it's game over, it shouldn't redo anything */
  processInput(Player, Q, R),                         /* process input */
  placePiece(Player, Board, Q, R, NewBoard, Log),     /* place piece on board */
  printBoard(NewBoard),                               /* display board */
  printError(Log),                                    /* handle errors */
  gameIsRunning(NewBoard),                            /* check if game is over */
  switchPlayer(Player, NewPlayer, Log),               /* switch player turn */
  game(NewPlayer, NewBoard).                          /* loop */

/**
* Interface to start the game
*/
game:-
  emptyBoard(Board),                            /* get empty board */
  printBoard(Board),                            /* display board */
  \+ game(player1, Board),                      /* start game loop (game fails once it's game over) */
  printGameOver.                                /* print end game screen */
