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

/**
* Prints the bottom of a cell to the terminal
*/
printBottom:- write('\\ /').

/**
* Prints one piece to the terminal
* @param Piece Element of a line of the board to print (will call translation rules)
* @param Index Set to '0' if it's the first element of a line.
*/
printPiece(Piece, 0):- write('|'), printPiece(Piece, 1).
printPiece(Piece, _Index):-
  toChar(Piece, C), /* char C to print */
  write(' '),
  write(C),
  write(' |').


/**
* Prints one line of the board (a list) to the terminal
* @param [Piece|Tail] List to print
* @param LineIndex Index of the current line being iterated (range: [0, board.height])
* @param ElemIndex Index of the current line element being iterated (range: [0, board.width])
* @param Type Either one of these values: top, middle, bottom.
*/
printLine([Piece|Tail], LineIndex, ElemIndex, middle):-
  boardWidth(Width),          /* get board width */
  Index > -1, Index < Width,  /* while (i >= 0 && i < board.height) */
  printPiece(Piece, Index),   /* print char */
  NewIndex is Index + 1,      /* i++ */
  printLine(Tail, NewIndex).  /* recursive call */

/**
* Prints Board to the terminal
* @param Line|Tail] Board to print - 2D Array (list of lists) representing the game board
* @param Index Index of the current line being iterated (range: [0, board.height])
*/
printBoard([Line|Tail], Index):-
  boardHeight(Height),        /* get board height */
  Index > -1, Index < Height,  /* while (i >= 0 && i < board.height) */
  printLine(Line, Index, middle),   /* print char */
  NewIndex is Index + 1,      /* i++ */
  printLine(Tail, NewIndex).  /* recursive call */
