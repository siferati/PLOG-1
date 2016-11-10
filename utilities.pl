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
* Finds an element in a list
* @param [H|T] List that has the element to be found
* @param X Index of element to find
* @param Elem Returns element found
*/
find([H|_], 0, H).
find([_|T], X, Elem):- X > 0, NewX is X - 1, find(T, NewX, Elem).

/**
* Finds an element in a list of lists (2D array)
* @param [H|T] List that has the element to be found
* @param X Index of element to be found (array[Y][X])
* @param Y Index of element to be found (array[Y][X])
* @param Elem Element found
*/
find([H|_], X, 0, Elem):- find(H, X, Elem).
find([_|T], X, Y, Elem):- Y > 0, NewY is Y - 1, find(T, X, NewY, Elem).

/**
* Replaces an element in a list
* @param [H|T] List that has the element to be replaced
* @param X Index of element to be replaced
* @param Elem Element to insert (replacement)
* @param [H|R] Returned list
*/
replace([_|T], 0, Elem, [Elem|T]).
replace([H|T], X, Elem, [H|R]):- X > 0, NewX is X-1, replace(T, NewX, Elem, R).

/**
* Replaces an element in a list of lists (2D array)
* @param [H|T] List that has the element to be replaced
* @param X Index of element to be replaced (array[Y][X])
* @param Y Index of element to be replaced (array[Y][X])
* @param Elem Element to insert (replacement)
* @param [H|R] Returned list (2D array)
*/
replace([H|T], X, 0, Elem, [NewH|T]):- replace(H, X, Elem, NewH).
replace([H|T], X, Y, Elem, [H|R]):- Y > 0, NewY is Y-1, replace(T, X, NewY, Elem, R).
