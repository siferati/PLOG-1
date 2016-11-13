:- ensure_loaded('Board.pl').
:- ensure_loaded('utilities.pl').
:- ensure_loaded('main.pl').
:- ensure_loaded('bot.pl').

/* test board */
no5Board([
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell],
[emptyCell, whitePiece, whitePiece, whitePiece, whitePiece, blackPiece, blackPiece, blackPiece, emptyCell],
[blackPiece, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell, nullCell]]).

/* test board */
sameColorAdjBoard([
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, whitePiece, whitePiece, emptyCell, emptyCell, emptyCell, nullCell],
[emptyCell, emptyCell, emptyCell, whitePiece, whitePiece, whitePiece, emptyCell, emptyCell, emptyCell],
[emptyCell, emptyCell, emptyCell, emptyCell, whitePiece, emptyCell, emptyCell, emptyCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell, nullCell]]).

/* test board */
yes5Board([
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell],
[emptyCell, whitePiece, whitePiece, whitePiece, whitePiece, blackPiece, blackPiece, blackPiece, blackPiece],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell, nullCell]]).

/* testBoard */
testBotPlayBoard([
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell, nullCell],
[blackPiece, blackPiece, blackPiece, blackPiece, emptyCell, emptyCell, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, whitePiece, whitePiece, whitePiece, emptyCell, emptyCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell, nullCell]]).

/* test checkPiece board */
checkPieceBoard([
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell],
[emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell],
[blackPiece, blackPiece, blackPiece, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell],
[blackPiece, whitePiece, blackPiece, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, nullCell],
[blackPiece, blackPiece, blackPiece, emptyCell, emptyCell, emptyCell, emptyCell, nullCell, nullCell],
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

test:-
  checkPieceBoard(Board),
  printBoard(Board),
  \+ checkPiece(Board, 1, 5, List),
  print(List).

testGetAdj:-
  sameColorAdjBoard(Board),
  printBoard(Board),
  getSameColorAdj(Board, 0, 0, whitePiece, List),
  print(List).

testGameOver:-
  yes5Board(Board),
  game(Board).

ola(maria, joao).
ola(jose, martins).
ola(joana, castro).

testBotPlay:-
  testBotPlayBoard(Board),
  printBoard(Board),
  get_char(_),
  playBotHardMode(Board, player1, NewBoard),
  printBoard(NewBoard).

testOla:-
  findall(X-Y, ola(X, Y), Lista),
  print(Lista).

/* Q: -2. R: 3.
test:-
  disappearBoard(Board),
  printBoard(Board),
  \+ game(player1, Board).*/
