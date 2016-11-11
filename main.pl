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

/**
* Reads the coords from the terminal
* @param Q Hex coords
* @param R Hex coords
*/
readCoords(Q, R):-
  get_code(Minus),  /* read Q */
  getQ(Minus, Q),   /* get real value of Q */
  getInt(R),        /* read R */
  get_char(_).      /* ignore nl */

/** TODO do this for R
* Checks if Q is positive or negative (cause of get_char problems zzz)
* @param Minus Char read from terminal
* @param Q Real value of Q is returned
*/
getQ(Minus, Q):-    /* Q is negative */
  minus(Minus),       /* if there was a minus signal */
  getCode(ASCIQ),     /* get Q ASCI Code (ignoring \n) */
  PosQ is ASCIQ - 48, /* positive value of Q = asciQ - '0' */
  Q is PosQ * -1.     /* get real Q */

getQ(Minus, Q):-      /* Q is positive */
  get_char(_),        /* ignore \n */
  Q is Minus - 48.    /* Q = Minus - '0' */


/** TODO meter coords com letras! 3f, 2a, etc (no negative coords)
* Processes user input (while asking questions)
* @param Player Current player
* @param Q Hex Coordinate to insert Piece
* @param R Hex Coordinate to insert Piece
*/
processInput(Player, Q, R):-
  write('\nCurrent Player: '), getPlayer(Player, PlayerName),
  write(PlayerName), nl,
  write('Insert Piece (Q-R):\n'),
  readCoords(Q, R).

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
