/**
Board is a 2D array (9x9!) (list of lists) that should ALWAYS be access like this:
board[Y][X]
*/

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
toChar(emptyCell, ' ').
toChar(nullCell, ' ').
toChar(blackPiece, 'B').
toChar(whitePiece, 'W').

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
testBoard([[whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, whitePiece]]).

/**
* Prints one piece to the terminal
* @param Piece Element of a line of the board to print (will call translation rules)
* @param Index Set to '0' if it's the first element of a line.
*/
printPiece(Piece, 0):-
  write('|'),
  toChar(Piece, C), /* char C to print */
  write(' '),
  write(C),
  write(' |').

printPiece(Piece, _Index):-
  toChar(Piece, C), /* char C to print */
  write(' '),
  write(C),
  write(' |').


/**
* Prints one line of the board (a list) to the terminal
* @param [Piece|Tail] List to print
* @param LineIndex Index of the current line being iterated
* @param ElemIndex Index of the current line element being iterated. ALWAYS set to 0 when called the 1st time.
*/
printLineMid([], _LineIndex, _ElemIndex). /* base case */
printLineMid([Piece|Tail], LineIndex, ElemIndex):-
  boardWidth(Width),                        /* get board width */
  ElemIndex > -1, ElemIndex < Width,        /* while (i >= 0 && i < board.width) */
  printPiece(Piece, ElemIndex),             /* print char */
  NewElemIndex is ElemIndex + 1,            /* i++ */
  printLineMid(Tail, LineIndex, NewElemIndex). /* recursive call */

/**
* Prints the bottom of a line of the board to the terminal
* @param LineIndex Index of the current line being iterated
* @param ElemIndex Index of the current line element being iterated. ALWAYS set to 0 when called the 1st time.
*/
printLineBot(0, ElemIndex):-
  boardWidth(Width),
  ElemIndex =:= Width.

printLineBot(0, ElemIndex):-
  boardWidth(Width),                        /* get board width */
  ElemIndex > -1, ElemIndex < Width,        /* while (i >= 0 && i < board.width) */
  write(' \\ /'),
  NewElemIndex is ElemIndex + 1,
  printLineBot(0, NewElemIndex).

/**
* Prints the top of a line of the board to the terminal
* @param LineIndex Index of the current line being iterated
* @param ElemIndex Index of the current line element being iterated. ALWAYS set to 0 when called the 1st time.
*/
printLineTop(0, ElemIndex):-
  boardWidth(Width),
  ElemIndex =:= Width.

printLineTop(0, ElemIndex):-
  boardWidth(Width),                        /* get board width */
  ElemIndex > -1, ElemIndex < Width,        /* while (i >= 0 && i < board.width) */
  write(' / \\'),
  NewElemIndex is ElemIndex + 1,
  printLineTop(0, NewElemIndex).

/**
* Interface for printing a line of the board to the terminal
* @param Line Line (list) to print
* @param LineIndex Index of the line to print
*/
printLine(Line, LineIndex):-
  printLineTop(0, 0), nl,
  printLineMid(Line, LineIndex, 0), nl,
  printLineBot(0, 0).

/**
* Prints Board to the terminal
* @param [Line|Tail] Board to print - 2D Array (list of lists) representing the game board
* @param Index Index of the current line being iterated (range: [0, board.height])
*/
printBoard([], _Index). /* base case */
printBoard([Line|Tail], Index):-
  boardHeight(Height),              /* get board height */
  Index > -1, Index < Height,       /* while (i >= 0 && i < board.height) */
  printLine(Line, Index), nl           /* print line */
  NewIndex is Index + 1,            /* i++ */
  printBoard(Tail, NewIndex).       /* recursive call */

printBoard:-
  emptyBoard(Board),
  printBoard(Board, 0).
