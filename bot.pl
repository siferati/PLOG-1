/** Bot */

/* includes */
:- ensure_loaded('Board.pl').
:- ensure_loaded('utilities.pl').
:- use_module(library(random)).

/**
* Finds all possible plays for given player
* @param Board Game Board
* @param Piece Piece (whitePiece or blackPiece)
* @param X Array coordiante of current piece being iterated
* @param Y Array coordiante of current piece being iterated
* @param XLast Array coordinate of last piece
* @param YLast Array coordinate of last piece
* @param PossiblePlays List of possible plays (list of pairs X-Y)
*/

/* stop condition (call for last cell of board . if play is valid) */
findAllPlays(Board, Piece, XLast, YLast, XLast, YLast, [XLast-YLast]):-
  validatePlay(Piece, Board, XLast, YLast),                               /* validate play */
  !.

/* stop condition (call for last cell of board . if play is invalid) */
findAllPlays(_Board, _Piece, XLast, YLast, XLast, YLast, []):- !.

findAllPlays(Board, Piece, XLast, Y, XLast, YLast, [XLast-Y|PossiblePlays]):- /* move to next line (if play is valid) */
  Y =< YLast,                                                                 /* make sure YPiece is valid */
  validatePlay(Piece, Board, XLast, Y),                                       /* validate play */
  NewY is Y + 1,                                                              /* prepare next ite */
  findAllPlays(Board, Piece, 0, NewY, XLast, YLast, PossiblePlays),           /* loop */
  !.

findAllPlays(Board, Piece, XLast, Y, XLast, YLast, PossiblePlays):-       /* move to next line (if play is invalid) */
  NewY is Y + 1,                                                          /* prepare next ite */
  findAllPlays(Board, Piece, 0, NewY, XLast, YLast, PossiblePlays),       /* loop */
  !.

findAllPlays(Board, Piece, X, Y, XLast, YLast, [X-Y|PossiblePlays]):-     /* main loop (if play is valid) */
  X =< XLast, Y =< YLast,                                                 /* make sure XPiece and YPiece are valid */
  validatePlay(Piece, Board, X, Y),                                       /* validate play */
  NewX is X + 1,                                                          /* move to next piece */
  findAllPlays(Board, Piece, NewX, Y, XLast, YLast, PossiblePlays),       /* loop */
  !.

findAllPlays(Board, Piece, X, Y, XLast, YLast, PossiblePlays):-           /* main loop (if play is invalid) */
  NewX is X + 1,                                                          /* move to next piece */
  findAllPlays(Board, Piece, NewX, Y, XLast, YLast, PossiblePlays),       /* loop */
  !.

/**
* Interface for gameIsRunning
* @param Board Game Board
* @param Player Player
* @param PossiblePlays Returns list of possible plays
*/
findAllPlays(Board, Player, PossiblePlays):-
  boardWidth(Width), boardHeight(Height),                         /* get board width and height */
  XLast is Width - 1,                                             /* array coordiante of last piece */
  YLast is Height - 1,                                            /* array coordinate of last piece */
  toPiece(Player, Piece),                                         /* get piece */
  findAllPlays(Board, Piece, 0, 0, XLast, YLast, PossiblePlays).  /* start iterating (starting on the 1st cell)*/

/**
* Get all possible PossiblePlays
* @param Board Game Board
* @param Player Player
* @param PossiblePlays List containing the possible plays (pair of coordinates X-Y)
*/
getPossiblePlays(Board, Player, PossiblePlays):-
  findAllPlays(Board, Player, PossiblePlays).

/**
* Plays bot on easy mode
* @param Board Game Board
* @param Player player1 or player2
* @param NewBoard New Board
*/
playBotEasyMode(Board, Player, NewBoard):-
  getPossiblePlays(Board, Player, PossiblePlays),
  botEasyMode(Board, Player, PossiblePlays, NewBoard).

/**
* Chooses a random move from all of the possible plays (and actually plays it)
* @param Board Game Board
* @param Player player1 or player2
* @param PossiblePlays List containing the possible plays (pair of coordinates X-Y)
* @param NewBoard New Board
*/
botEasyMode(Board, Player, PossiblePlays, NewBoard):-
  length(PossiblePlays, MaxRandom),               /* get the upper limit for random (maxRandom is never generated)*/
  random(0, MaxRandom, RandomN),                  /* get random number */
  find(PossiblePlays, RandomN, X-Y),              /* get coords for play */
  reverseMap(Q, R, X, Y),                         /* get HEX coords */
  placePiece(Player, Board, Q, R, NewBoard, Log), /* place piece */
  printError(Log).                                /* print error (LOG SHOULD ALWAYS BE NONE!) */

/**
* Plays bot on hard mode
* @param Board Game Board
* @param Player player1 or player2
* @param NewBoard New Board
*/
playBotHardMode(Board, Player, NewBoard):-
  getPossiblePlays(Board, Player, PossiblePlays),
  botHardMode(Board, Player, PossiblePlays, NewBoard).

/**
* Chooses the best move from all of the possible plays (and actually plays it)
* Priority: Win, Not Loose, Eat Pieces, Place Piece next to bigger line of pieces
* @param Board Game Board
* @param Player player1 or player2
* @param PossiblePlays List containing the possible plays (pair of coordinates X-Y)
* @param NewBoard New Board
*/
botHardMode(Board, Player, PossiblePlays, NewBoard):-
  botCheckPriorityWin(Board, Player, PossiblePlays, NewBoard).

/**
* Checks if it's possible to win at this moment
* @param Board Game Board
* @param Player player1 or player2
* @param PossiblePlays List containing the possible plays (pair of coordinates X-Y)
* @param NewBoard New Board
*/
botCheckPriorityWin(Board, Player, [], NewBoard):- !.     /* stop condition */
  /*nextCall(Board, Player, [], NewBoard):-               /* check next priority */

botCheckPriorityWin(Board, Player, [X-Y|_T], NewBoard):-  /* if it's possible to win */
  reverseMap(Q, R, X, Y),                                 /* get HEX coords */
  placePiece(Player, Board, Q, R, TempBoard, Log),        /* place piece */
  printError(Log),                                        /* print error (LOG SHOULD ALWAYS BE NONE!) */
  \+ gameIsRunning(TempBoard),                            /* check game over */
  NewBoard = TempBoard.                                   /* return new board */

botCheckPriorityWin(Board, Player, [_H|T], NewBoard):-   /* if it's NOT possible to win */
  botCheckPriorityWin(Board, Player, T, NewBoard).        /* recursive call */