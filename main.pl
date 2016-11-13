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
  get_code(MinusQ),      /* read Q (or minus signal) */
  getCoord(MinusQ, Q),   /* get real value of Q */
  get_code(MinusR),      /* read R (or minus signal) */
  getCoord(MinusR, R).   /* get real value of R */

/**
* Checks if Coord is positive or negative (cause of get_char problems (reading - ))
* after checking if Coord is 'e'
* @param Minus Char read from terminal
* @param Coord Real value of Coord is returned
*/

getCoord(Minus, Coord):-      /* Coord is negative */
  getCode(ASCICoord),         /* get Coord ASCI Code (ignoring \n) */
  ASCICoord =:= 

  !.

getCoord(Minus, Coord):-      /* Coord is negative */
  minus(Minus),               /* if there was a minus signal */
  getCode(ASCICoord),         /* get Coord ASCI Code (ignoring \n) */
  PosCoord is ASCICoord - 48, /* positive value of Coord = asciQ - '0' */
  Coord is PosCoord * -1,     /* get real Coord */
  !.

getCoord(Minus, Coord):-      /* Coord is positive */
  get_char(_),                /* ignore \n */
  Coord is Minus - 48,        /* Coord = Minus - '0' */
  !.


/**
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
  !, game(NewPlayer, NewBoard).                          /* loop */

/** TODO perguntar se o 2º jogar quer trocar com o 1º, na primeira jogada
* Interface to start the game
*/
game:-
  emptyBoard(Board),                            /* get empty board */
  printBoard(Board),                            /* display board */
  \+ game(player1, Board),                      /* start game loop (game fails once it's game over) */
  printGameOver.                                /* print end game screen */

/** TODO needs checking on game over... cant figure out why it bugs
* Test function for debugging
*/
game(Board):-
  printBoard(Board),                            /* display board */
  \+ game(player1, Board),                      /* start game loop (game fails once it's game over) */
  printGameOver.                                /* print end game screen */
