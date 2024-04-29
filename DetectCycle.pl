
first([H|_], H).

% board
board0([
    [yellow, yellow, yellow, red], 
    [blue, yellow, blue, yellow],
    [blue, blue, blue, yellow],
    [blue, blue, blue, yellow]
]).

board1([
    [yellow, yellow, yellow, red], 
    [blue, yellow, red, yellow],
    [blue, red, blue, yellow],
    [blue, red, blue, yellow]
]).

% moves 
move(0, 1). % right
move(-1, 0). % down
move(0, -1). % left
move(1, 0). % up


% check if the cell is in the board
inBoard(X, Y, N, M) :- X >= 0, X < N, Y >= 0, Y < M.

% check if the cell is not visited
notVisited(X, Y, Visited) :- \+ member([X, Y], Visited).

% check if the cell has the same color
sameColor(X, Y, Color, Board) :- nth0(X, Board, Row), nth0(Y, Row, Color).

% check if the last node is adjacent to the first node
adjacent([X1, Y1], [X2, Y2]) :- 
    X1 =:= X2, abs(Y1 - Y2) =:= 1;
    Y1 =:= Y2, abs(X1 - X2) =:= 1.

% check if the cycle is valid
validCycle(Cycle, Color, Board) :- 
    length(Cycle, L), L >= 4,
    closedCycle(Cycle),
    forall(member([X, Y], Cycle), sameColor(X, Y, Color, Board)).

% check if the cycle is closed
closedCycle(Cycle) :-
    last(Cycle, Last),
    first(Cycle, First),
    adjacent(Last, First).


getColor(X, Y, Color, Board) :- nth0(X, Board, Row), nth0(Y, Row, Color).

% depth first search
dfs(X, Y, X, Y, Visited, Color, Board, _, _) :- 
    validCycle(Visited, Color, Board), 
    write('Cycle: '), write(Visited), write(' Color: '), write(Color), nl, 
    !.


dfs(X, Y, X1, Y1, Visited, Color, Board, N, M) :- 
    move(X1, Y1),
    X2 is X + X1, Y2 is Y + Y1,
    inBoard(X2, Y2, N, M),
    notVisited(X2, Y2, Visited),
    getColor(X, Y, Color, Board),
    getColor(X2, Y2, Color, Board),
    append(Visited, [[X2, Y2]], Visited1),
    dfs(X2, Y2, _, _, Visited1, Color, Board, N, M).


start :-
    board0(Board),
    length(Board, N), % number of rows
    Board = [Row|_], length(Row, M), % number of columns
    (   between(0, N, X), % for each cell
        between(0, M, Y), 
        dfs(X, Y, _, _, [[X, Y]], _, Board, N, M) -> 
        Found = true
    ;   Found = false
    ),
    (Found -> ! ; write('No cycles found'), nl), !.

:- start.