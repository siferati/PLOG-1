/**
Board is a 2D array (9x9!) (list of lists) that should ALWAYS be access like this:
board[Y][X]
*/

/* utilities */

/* if number is even */
even(N):-
  0 is N mod 2.

/* abs */
abs(X, Y) :- X < 0, Y is -X.
abs(X, X) :- X > -1.

/* print N spaces to the terminal */
printSpace(0).
printSpace(N):-
  N > 0,
  write(' '),
  NewN is N - 1,
  printSpace(NewN).

/* Interface for printing nullCells to terminal */
printNullCells(LineIndex):-
  numberCells(LineIndex, NCells),     /* get number of cells to draw */
  boardWidth(Width),                  /* get board width */
  N is Width - NCells,                /* number of nullCells */
  N2 is div(N, 2),                    /* number of nullCells on the left side */
  N3 is N2*4,                         /* number of sapces left side nullCells are worth */
  printSpace(N3).                     /* print spaces to the terminal */

/* how many cells to draw */
numberCells(LineIndex, X):-
  boardRadius(Radius),
  LineIndex < Radius + 1,
  boardWidth(Width),
  X is Width - Radius + LineIndex.

numberCells(LineIndex, X):-
  boardRadius(Radius),
  LineIndex > Radius,
  boardWidth(Width),
  X is Width - LineIndex + Radius.

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
* Prints one piece to the terminal
* @param Piece Element of a line of the board to print (will call translation rules)
* @param Index Set to '0' if it's the first element of a line.
*/
printPiece(Piece, 0):-
  write('|'),
  printPiece(Piece, 1).

printPiece(Piece, _Index):-
  toChar(Piece, C), /* char C to print */
  write(C).


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

printLineMid([Piece|Tail], LineIndex, ElemIndex):-
  numberCells(LineIndex, NCells),           /* get number of cells to draw */
  ElemIndex > -1, ElemIndex < NCells,       /* while (i >= 0 && i <= nCells) */
  printPiece(Piece, ElemIndex),             /* print char */
  NewElemIndex is ElemIndex + 1,            /* i++ */
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

/* print bottom of line */
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

/* print top of line */
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
  write(0),
  printNullCells(0),                      /* print left side null cells */
  printLineTop(0, 0), nl,                 /* print top */
  write(0),
  printNullCells(0),                      /* print left side null cells */
  printLineMid(Line, 0, 0), nl,           /* print middle of line */
  write(0),
  printNullCells(0),                      /* print left side null cells */
  printLineBot(0, 0).                     /* print bottom of line */
  /*write(' \\').                          /* print top of last element of next line */

/* print last line */
printLine(Line, LineIndex):-
  boardHeight(Height),                    /* get height */
  LineIndex =:= Height - 1,               /* make sure it's the last line */
  write(LineIndex),
  printNullCells(LineIndex),              /* print left side null cells */
  printLineMid(Line, LineIndex, 0), nl,   /* print middle of line */
  write(LineIndex),
  printNullCells(LineIndex),              /* print left side null cells */
  printLineBot(LineIndex, 0).                     /* print bottom of line */

/* print odd lines */
printLine(Line, LineIndex):-
  \+even(LineIndex),                      /* check it's an odd line */
  write(LineIndex),
  printNullCells(LineIndex),              /* print left side null cells */
  write('  '),                            /* offset */
  printLineMid(Line, LineIndex, 0), nl,   /* print middle of line */
  write(LineIndex),
  printNullCells(LineIndex),              /* print left side null cells */
  /*write(' /'),                            /* print top of 1st element of next line (default!) */
  write('  '),
  printLineBot(LineIndex, 0).             /* print bottom of line */

/* ofset(LineIndex):- se tiver em cima write / e \\ em vez de '  ', em baixo excrever '  ' */

/* print even lines */
printLine(Line, LineIndex):-
  even(LineIndex),                        /* check it's an even line */
  write(LineIndex),
  printNullCells(LineIndex),              /* print left side null cells */
  printLineMid(Line, LineIndex, 0), nl,   /* print middle of line */
  write(LineIndex),
  printNullCells(LineIndex),              /* print left side null cells */
  printLineBot(LineIndex, 0).             /* print bottom of line */
  /*write(' \\').                           /* print top of last element of next line */

/**
* Prints Board to the terminal
* @param [Line|Tail] Board to print - 2D Array (list of lists) representing the game board
* @param Index Index of the current line being iterated (range: [0, board.height])
*/

/* base case | stop condition */
printBoard([], _Index).

/* print board */
printBoard([Line|Tail], Index):-
  boardHeight(Height),              /* get board height */
  Index > -1, Index < Height,       /* while (i >= 0 && i < board.height) */
  printLine(Line, Index), nl,       /* print line */
  NewIndex is Index + 1,            /* i++ */
  printBoard(Tail, NewIndex).       /* recursive call */

/* interface for print board */
printBoard:-
  emptyBoard(Board),
  printBoard(Board, 0).

/* print test board */
printTestBoard:-
  testBoard(Board),
  printBoard(Board, 0).
