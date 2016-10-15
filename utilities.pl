/* utilities */

/**
* Checks if number is even
* @param N Number to check
*/
even(N):-
  0 is N mod 2.

/**
* Calculates the abs of a number
* @param X Number to calculate abs of
* @param Y Storage for abs(X)
*/
abs(X, Y) :- X < 0, Y is -X.
abs(X, X) :- X > -1.

/**
* Print N spaces to the terminal
* @param N Number of spaces to print
*/
printSpace(N):-
  N < 1.
printSpace(N):-
  N > 0,
  write(' '),
  NewN is N - 1,
  printSpace(NewN).
