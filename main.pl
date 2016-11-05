/* Game Loop */

/* includes TODO include vs ensure_loaded */
:- ensure_loaded('utilities.pl').
:- ensure_loaded('Board.pl').

/**
* Switch player turn
* @param Player Player to switch
* @param NewPlayer Next player
*/
switchPlayer(player1, player2).
switchPlayer(player2, player1).

/**
* Get current player
* @param Player Current player
* @param PlayerName Name of the current player
*/
getPlayer(player1, 'Player 1').
getPlayer(player2, 'Player 2').

/** TODO fix output 'R: :| '
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

/*
while (gameIsRunning)
{
  processInput();
  update();
  render();
}
*/

/** TODO check for game end
* @param Player Current player
* @param Board Game Board
*/
game(Player, Board):-
  processInput(Player, Q, R),                   /* process input */
  placePiece(Player, Board, Q, R, NewBoard), !, /* place piece on board */
  printBoard(NewBoard),                         /* display board */
  gameIsRunning(NewBoard),                      /* check if game is over */
  switchPlayer(Player, NewPlayer),              /* switch player turn */
  game(NewPlayer, NewBoard).                    /* loop */

/**
* Interface to start the game
*/
game:-
  emptyBoard(Board),                            /* get empty board */
  printBoard(Board),                            /* display board */
  game(player1, Board).                         /* start game loop */
