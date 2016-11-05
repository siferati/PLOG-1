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

/**
* Replaces an element in a list (returns original list in case index is wrong)
* @param [H|T] List that has the element to be replaced
* @param X Index of element to be replaced
* @param Elem Element to insert (replacement)
* @param [H|R] Returned list
*/
replace([_|T], 0, Elem, [Elem|T]).
replace([H|T], X, Elem, [H|R]):- X > -1, NewX is X-1, replace(T, NewX, Elem, R), !.
replace(L, _, _, L).

/**
* Replaces an element in a list of lists (2D array) (returns original list (2D array) in case index is wrong)
* @param [H|T] List that has the element to be replaced
* @param X Index of element to be replaced (array[Y][X])
* @param Y Index of element to be replaced (array[Y][X])
* @param Elem Element to insert (replacement)
* @param [H|R] Returned list (2D array)
*/
replace([H|T], X, 0, Elem, [NewH|T]):- replace(H, X, Elem, NewH).
replace([H|T], X, Y, Elem, [H|R]):- Y > -1, NewY is Y-1, replace(T, X, NewY, Elem, R), !.
replace(L, _, _, _, L).
