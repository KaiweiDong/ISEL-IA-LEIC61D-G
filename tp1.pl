% Quick Testing ------------------------------------------------------------

r() :- consult("./tp1.pl").
c(X) :- createBoard(3, X).
i(X, N) :- insertInBoard(X, 0, 0, "a", N).
p(X) :- printBoard(X).


% Game Play ------------------------------------------------------------

start(Board) :-
    write('Enter board size: '),
    read(Size),
    createBoard(Size, Board),
    printBoard(Board).

% Board Creation ------------------------------------------------------------

createBoard(Size, Out):-
    length(Out, Size),
    createLine(Size, X),
    maplist(=(X), Out).

createLine(Size, Out):-
    length(Out, Size), 
    maplist(=('.'), Out).

% Board View ------------------------------------------------------------

printBoard([]).
printBoard([H|T]):-
    printLine(H),
    nl,
    printBoard(T).

printLine([]).
printLine([H|T]):-
    write(H),
    printLine(T).
    
% Board Interaction ------------------------------------------------------------

findInBoard(Board, Line, Col, Out):-
    find(Board, Line, X),
    find(X, Col, Out).

find([X], 0, X).
find([H|_], 0, H).
find([_|T], I, Out) :- NewI is I-1, find(T, NewI, Out).

insertInBoard(Board, Line, Col, Value, NewBoard) :-
    insert(Board, Line, Col, Value, NewBoard).

insert([H|T], 0, Col, Value, [NewH|T]) :-
    replaceLine(H, Col, Value, NewH).
insert([H|T], Line, Col, Value, [H|NewT]) :-
    NewLine is Line - 1,
    insert(T, NewLine, Col, Value, NewT).

replaceLine([_|T], 0, Value, [Value|T]).
replaceLine([H|T], Col, Value, [H|NewT]) :-
    NewCol is Col - 1,
    replaceLine(T, NewCol, Value, NewT).
