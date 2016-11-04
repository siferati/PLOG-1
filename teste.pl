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
