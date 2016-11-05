/**
Board is a 2D array (9x9!) (list of lists) that should ALWAYS be access like this:
board[Y][X]
*/

:- ensure_loaded('utilities.pl').

/* Board radius N = max(abs(x), abs(z)) = 4  */
boardRadius(4).

/**
* Hexagonal to array coordinates mapping: array[R + N][Q + N + min(0, R)], N = board radius
* @param Q Hexagonal coordinate (SW - NE)
* @param R Hexagonal coordinate (N - S)
* @param X Array coordinate (columns)
* @param Y Array coordinate (rows)
*/
map(Q, R, X, Y):-
  boardRadius(N), /* get board radius and store it in variable N */
  Y is R + N,
  X is Q + N + min(0, R).

/* translation rules between players and their respective pieces */
toPiece(player1, whitePiece).
toPiece(player2, blackPiece).

/* translation rules used for printing the board */
toChar(emptyCell, '   |').
toChar(blackPiece, ' B |').
toChar(whitePiece, ' W |').
toChar(nullCell, '').

/* board (2D array) measures */
boardWidth(9).
boardHeight(9).

/* empty board */
emptyBoard([
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell, nullCell]]).

/* final board (example of game over)*/
finalBoard([
[blackPiece, blackPiece, blackPiece, emptyCell, emptyCell, nullCell, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell],
[emptyCell, emptyCell, blackPiece, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell],
[emptyCell, blackPiece, emptyCell, blackPiece, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell],
[blackPiece, blackPiece, blackPiece, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell],
[emptyCell, emptyCell, blackPiece, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell],
[emptyCell, whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell, nullCell]]).

/* on-going board (example of an on-going game) */
gameBoard([
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell, nullCell],
[emptyCell, blackPiece, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell],
[emptyCell, whitePiece, emptyCell, blackPiece, emptyCell, emptyCell, emptyCell, nullCell, nullCell],
[emptyCell, blackPiece, whitePiece, whitePiece, emptyCell, emptyCell, emptyCell, emptyCell, nullCell],
[emptyCell, emptyCell, emptyCell, whitePiece, whitePiece, blackPiece, emptyCell, emptyCell, emptyCell],
[emptyCell, emptyCell, whitePiece, blackPiece, blackPiece, emptyCell, emptyCell, emptyCell, nullCell],
[emptyCell, whitePiece, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell],
[blackPiece, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell, nullCell]]).

/* test board */
testBoard([
[whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, nullCell, nullCell, nullCell, nullCell],
[whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, nullCell, nullCell, nullCell],
[whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, nullCell, nullCell],
[whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, nullCell],
[whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, whitePiece],
[whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, nullCell],
[whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, nullCell, nullCell],
[whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, nullCell, nullCell, nullCell],
[whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, nullCell, nullCell, nullCell, nullCell]]).

/**
* Interface for printing nullCells to terminal
* @param LineIndex Index of lines
* @param Offset Value to subtract from the final N value
*/
printNullCells(LineIndex, Offset):-
  numberCells(LineIndex, NCells),     /* get number of cells to draw */
  boardWidth(Width),                  /* get board width */
  N is Width - NCells,                /* number of nullCells */
  N2 is div(N, 2),                    /* number of nullCells on the left side */
  N3 is N2*4,                         /* number of sapces left side nullCells are worth */
  N4 is N3 - Offset,                  /* offset N4 */
  printSpace(N4).                     /* print spaces to the terminal */

/**
* How many cells to draw on given line
* @param LineIndex Index of the current line
* @param X Storage for the number of cells to draw
*/

/* If above (or at) the equator */
numberCells(LineIndex, X):-
  boardRadius(Radius),
  LineIndex < Radius + 1,
  boardWidth(Width),
  X is Width - Radius + LineIndex.

/* If below the equator */
numberCells(LineIndex, X):-
  boardRadius(Radius),
  LineIndex > Radius,
  boardWidth(Width),
  X is Width - LineIndex + Radius.

/**
* Prints one piece to the terminal
* @param Piece Element of a line of the board to print (will call translation rules)
* @param Index Set to '0' if it's the first element of a line.
*/

/* If it's the first element of a line */
printPiece(Piece, 0):-
  write('|'),
  printPiece(Piece, 1).

/* If it's not the first element of a line */
printPiece(Piece, _Index):-
  toChar(Piece, C),         /* char C to print */
  write(C).

/**
* Offsets the given line by either '  ', '/' or '\\'
* @param LineIndex Line of offset
* @param Char Char to offset with
*/

/* If above the equator */
offset(LineIndex, Char):-
  boardHeight(Height),      /* get board height */
  Equa is div(Height, 2),   /* get the equator */
  LineIndex < Equa,         /* make sure it's above the equator */
  write(' '), write(Char).  /* write either '/' or '\\' */

/* If below (or at) the equator */
offset(LineIndex, _Char):-
  boardHeight(Height),      /* get board height */
  Equa is div(Height, 2),   /* get the equator */
  LineIndex > Equa - 1,     /* make sure it's below (or at) the equator */
  write('  ').              /* write spaces */

/**
* Prints one line of the board (a list) to the terminal
* @param [Piece|Tail] List to print
* @param LineIndex Index of the current line being iterated
* @param ElemIndex Index of the current line element being iterated. ALWAYS set to 0 when called the 1st time.
*/

/* base case | stop condition */
printLineMid(_List, LineIndex, ElemIndex):-
  numberCells(LineIndex, NCells),           /* get number of cells to draw */
  ElemIndex =:= NCells.                     /* stop if all cells have been drawn */

/* main loop */
printLineMid([Piece|Tail], LineIndex, ElemIndex):-
  numberCells(LineIndex, NCells),              /* get number of cells to draw */
  ElemIndex > -1, ElemIndex < NCells,          /* while (i >= 0 && i <= nCells) */
  printPiece(Piece, ElemIndex),                /* print char */
  NewElemIndex is ElemIndex + 1,               /* i++ */
  printLineMid(Tail, LineIndex, NewElemIndex). /* recursive call */

/**
* Prints the bottom of a line of the board to the terminal
* @param LineIndex Index of the current line being iterated
* @param ElemIndex Index of the current line element being iterated. ALWAYS set to 0 when called the 1st time.
*/

/* base case | stop condition */
printLineBot(LineIndex, ElemIndex):-
  numberCells(LineIndex, NCells),           /* get number of cells to draw */
  ElemIndex =:= NCells.                     /* stop if all cells have been drawn */

/* main loop */
printLineBot(LineIndex, ElemIndex):-
  numberCells(LineIndex, NCells),           /* get number of cells to draw */
  ElemIndex > -1, ElemIndex < NCells,       /* while (i >= 0 && i <= nCells) */
  write(' \\ /'),                           /* print bottom of line */
  NewElemIndex is ElemIndex + 1,            /* get ready for next ite */
  printLineBot(LineIndex, NewElemIndex).    /* recursive call */

/**
* Prints the top of a line of the board to the terminal
* @param LineIndex Index of the current line being iterated
* @param ElemIndex Index of the current line element being iterated. ALWAYS set to 0 when called the 1st time.
*/

/* base case | stop condition */
printLineTop(LineIndex, ElemIndex):-
  numberCells(LineIndex, NCells),           /* get number of cells to draw */
  ElemIndex =:= NCells.                     /* stop if all cells have been drawn */

/* main loop */
printLineTop(LineIndex, ElemIndex):-
  numberCells(LineIndex, NCells),           /* get number of cells to draw */
  ElemIndex > -1, ElemIndex < NCells,       /* while (i >= 0 && i <= nCells) */
  write(' / \\'),                           /* print top of line */
  NewElemIndex is ElemIndex + 1,            /* get ready for next ite */
  printLineTop(LineIndex, NewElemIndex).    /* recursive call */

/**
* Interface for printing a line of the board to the terminal
* @param Line Line (list) to print
* @param LineIndex Index of the line to print
*/

/* print first line */
printLine(Line, 0):-
  printNullCells(0, 0),                  /* print left side null cells */
  printLineTop(0, 0), nl,                 /* print top */
  printNullCells(0, 0),                   /* print left side null cells */
  printLineMid(Line, 0, 0), nl,           /* print middle of line */
  printNullCells(0, 2),                   /* print left side null cells (offset by 2 */
  offset(0, '/'),                         /* print top of 1st element of next line */
  printLineBot(0, 0),                     /* print bottom of line */
  offset(0, '\\').                        /* print top of last element of next line */

/* print last line */
printLine(Line, LineIndex):-
  boardHeight(Height),                    /* get height */
  LineIndex =:= Height - 1,               /* make sure it's the last line */
  printNullCells(LineIndex, 0),           /* print left side null cells */
  printLineMid(Line, LineIndex, 0), nl,   /* print middle of line */
  printNullCells(LineIndex, 0),           /* print left side null cells */
  printLineBot(LineIndex, 0).             /* print bottom of line */

/* print odd lines */
printLine(Line, LineIndex):-
  \+even(LineIndex),                      /* check it's an odd line */
  printNullCells(LineIndex, 0),           /* print left side null cells */
  write('  '),                            /* offset */
  printLineMid(Line, LineIndex, 0), nl,   /* print middle of line */
  printNullCells(LineIndex, 0),           /* print left side null cells */
  offset(LineIndex, '/'),                 /* print top of 1st element of next line (default!) */
  printLineBot(LineIndex, 0),             /* print bottom of line */
  offset(LineIndex, '\\').                /* print top of last element of next line */

/* print line index 4, cause reasons */
printLine(Line, 4):-
  printNullCells(4, 0),          /* print left side null cells */
  printLineMid(Line, 4, 0), nl,  /* print middle of line */
  printNullCells(4, 2),          /* print left side null cells (offset by 2) */
  printLineBot(4, 0).            /* print bottom of line */

/* print even lines */
printLine(Line, LineIndex):-
  even(LineIndex),                       /* check it's an even line */
  printNullCells(LineIndex, 0),          /* print left side null cells */
  printLineMid(Line, LineIndex, 0), nl,  /* print middle of line */
  printNullCells(LineIndex, 2),         /* print left side null cells (offset by 2) */
  offset(LineIndex, '/'),                /* print top of 1st element of next line (default!) */
  printLineBot(LineIndex, 0),            /* print bottom of line */
  offset(LineIndex, '\\').               /* print top of last element of next line */

/**
* Prints Board to the terminal
* @param [Line|Tail] Board to print - 2D Array (list of lists) representing the game board
* @param Index Index of the current line being iterated (range: [0, board.height])
*/

/* base case | stop condition */
printBoard([], _Index).

/* main loop */
printBoard([Line|Tail], Index):-
  boardHeight(Height),              /* get board height */
  Index > -1, Index < Height,       /* while (i >= 0 && i < board.height) */
  printLine(Line, Index), nl,       /* print line */
  NewIndex is Index + 1,            /* i++ */
  printBoard(Tail, NewIndex).       /* recursive call */

/**
* Interface for printing any board
*/
printBoard(Board):-
  printBoard(Board, 0).

/** TODO succed if game is running, fail otherwise
* Checks if the game is finished.
* @param Board Game Board
*/
gameIsRunning(_Board).

/** TODO complete game rules (check nullCell, etc)
* Game Rules
* @param Piece whitePiece or blackPiece
* @param Board Game Board
* @param X Array Coordinate to insert Piece
* @param Y Array Coordinate to insert Piece
*/
validatePlay(_Piece, _Board, X, Y):-
  boardWidth(Width), boardHeight(Height), /* get board width and height */
  X >= 0, X < Width,                      /* make sure X is valid */
  Y >= 0, Y < Height.                     /* make sure Y is valid */

/**
* Places a piece on the board
* @param Player player1 or player2
* @param Board Game Board
* @param Q Hex Coordinate to insert Piece
* @param R Hex Coordinate to insert Piece
* @param NewBoard GameBoard after piece is played
*/
placePiece(Player, Board, Q, R, NewBoard):-
  map(Q, R, X, Y),                        /* get mapping for the coordinates */
  toPiece(Player, Piece),                 /* get player's piece (whitePiece or blackPiece) */
  validatePlay(Piece, Board, X, Y), !,    /* make sure it's a valid play */
  replace(Board, X, Y, Piece, NewBoard).  /* insert piece on the board */

/* In case the play is invalid (validatePlay failed) */
placePiece(_, _, _, _):-
  write('Invalid play!\n').


/**
* Interface for printing an empty board
*/
printEmptyBoard:-
  emptyBoard(Board),
  printBoard(Board, 0).

/**
* Interface for printing the final board
*/
printFinalBoard:-
  finalBoard(Board),
  printBoard(Board, 0).

/**
* Interface for printing the test board
*/
printTestBoard:-
  testBoard(Board),
  printBoard(Board, 0).

/**
* Interface for printing the game board
*/
printGameBoard:-
  gameBoard(Board),
  printBoard(Board, 0).
