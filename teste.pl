:- ensure_loaded('Board.pl').
:- ensure_loaded('utilities.pl').

/* test board */
no5Board([
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, whitePiece, whitePiece, emptyCell, emptyCell, emptyCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell, nullCell]]).

/* test board */
yes5Board([
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell],
[whitePiece, whitePiece, whitePiece, whitePiece, whitePiece, emptyCell, emptyCell, emptyCell, emptyCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell, nullCell]]).

/* test disappearing pieces */
disappearBoard([
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, whitePiece, blackPiece, emptyCell, emptyCell, emptyCell],
[emptyCell, emptyCell, emptyCell, emptyCell, whitePiece, blackPiece, emptyCell, emptyCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, blackPiece, emptyCell, emptyCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, blackPiece, whitePiece, whitePiece, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, blackPiece, emptyCell, whitePiece, nullCell, nullCell, nullCell, nullCell]]).

/* teste */
teste:-
  write(' _  _'), nl,
  write('/ \\/ \\'), nl,
  write(' 1  2'), nl,
  write('\\-/\\-/').

/*
width: 5 spaces
height: 3 lines
how to draw: (sp = space, s = slash, bs = backslash, nl = newline, b = vertical bar)
sp, s, sp, bs, nl
b, sp, c, sp, b, nl
sp, bs, sp, s
*/

teste2:-
  write('   / \\'), nl,
  write('  | 3 |'), nl,
  write(' / \\ /'), nl,
  write('| 2 |'), nl,
  write(' \\ /').

:- include('utilities.pl').

/*testNo:-
  yes5Board(Board),
  test(Board).

testYes:-
  no5Board(Board),
  test(Board).

test(Board):-
  printBoard(Board),
  gameIsRunning(Board),
  write('Game is running!\n').

test(_):-
  write('Game Over!\n').*/

/* TODO pieces disappearing!? */
/* Q: -2. R: 3. */
test:-
  disappearBoard(Board),
  printBoard(Board),
  \+ game(player1, Board).
