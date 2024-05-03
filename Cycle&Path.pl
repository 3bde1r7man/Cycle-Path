
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

board2([
    [red, red, yellow, yellow],
    [red, blue, red, red],
    [red, red, red, yellow],
    [blue, red, blue, yellow]
]).

board3([
    [blue, red, red, red, red, red],
    [red, red, red, red, red, red],
    [blue, blue, blue, red, red, blue]
]).

% moves 
move(0, 1). % right
move(-1, 0). % up
move(0, -1). % left
move(1, 0). % down


% check if the cell is in the board
inBoard(X, Y, N, M) :- X >= 0, X < N, Y >= 0, Y < M.

% check if the cell is not visited
notVisited(X, Y, Visited) :- \+ member([X, Y], Visited).

% check if the last node is adjacent to the first node
adjacent([X1, Y1], [X2, Y2]) :- 
    X1 =:= X2, abs(Y1 - Y2) =:= 1;
    Y1 =:= Y2, abs(X1 - X2) =:= 1.

% check if the cycle is valid
validCycle(Cycle) :- 
    length(Cycle, L), L >= 4,
    closedCycle(Cycle).

% check if the cycle is closed
closedCycle(Cycle) :-
    last(Cycle, Last),
    first(Cycle, First),
    adjacent(Last, First).


getColor(X, Y, Color, Board) :- nth0(X, Board, Row), nth0(Y, Row, Color).

% depth first search
dfs(_, _, Visited, Color, _, _, _) :- 
    validCycle(Visited), 
    write('Cycle: '), write(Visited), write(' Color: '), write(Color), nl, 
    !.


dfs(X, Y, Visited, Color, Board, N, M) :- 
    move(X1, Y1),
    X2 is X + X1, Y2 is Y + Y1,
    inBoard(X2, Y2, N, M),
    notVisited(X2, Y2, Visited),
    getColor(X, Y, Color, Board),
    getColor(X2, Y2, Color, Board),
    append(Visited, [[X2, Y2]], Visited1),
    dfs(X2, Y2, Visited1, Color, Board, N, M).


dfs(Board, N, M) :- 
    (   between(0, N, X), % for each cell
        between(0, M, Y), 
        dfs(X, Y, [[X, Y]], _, Board, N, M) -> 
        Found = true
    ;   Found = false
    ),
    (Found -> ! ; write('No cycles found'), nl), !.


% ------------------------------------------------------

% moves 
moveA([0, 1]). % right
moveA([-1, 0]). % up
moveA([0, -1]). % left
moveA([1, 0]). % down

% filling the 1d list with heuristic values using manhattan distance
fill_1d_HL([], _, _, _, _, _, _, []).
fill_1d_HL([_|Rest], X, Y, GoalX, GoalY, GoalColor, Board, [H|Rest1]) :-
    (
        \+ getColor(X, Y, GoalColor, Board)-> H = -1;
        H is abs(X - GoalX) + abs(Y - GoalY)
    ),
    Y1 is Y + 1,
    fill_1d_HL(Rest, X, Y1, GoalX, GoalY, GoalColor, Board, Rest1).

% filling the 2d list with heuristic values
fill_2d_HL([], _, _, _, _, _, _, []).
fill_2d_HL([Row|Rest], X, Y, GoalX, GoalY, GoalColor, Board, [Row1|Rest1]) :-
    fill_1d_HL(Row, X, Y, GoalX, GoalY, GoalColor, Board, Row1),
    X1 is X + 1,
    fill_2d_HL(Rest, X1, Y, GoalX, GoalY, GoalColor, Board, Rest1).

% Base case: when the list is empty, V is unified with the accumulator S
minV([], V, V). 
minV([H|Tail], S, V) :-
    S > H, minV(Tail, H, V); minV(Tail, S, V). % If the head H is smaller than S, update S with H and continue recursively

% Find index of minimum element in the given list
findMinIndex([H|T], Ind, V) :-
    minV(T, H, V),
    nth0(Ind, [H|T], V),!. % Find the index of the minimum value V in the List


% If the goal is achieved, print the result and terminate the process.
end(LX, LY, Ex, Ey, Visited, Cost, Color) :- 
	LX =:= Ex, LY =:= Ey, 
    write('Path: '), write(Visited), write(' TotalCost: '), write(Cost), write(' Color: '), write(Color), nl, 
    !.

%Takes index and list then remove the element which is in the given index and put the final list in Result list
remove_at(Index, List, Result) :-
    nth0(Index, List, Element),
    select(Element, List, Result).


getallchild(X,Y, Open, Moves, N, M, Color, Board,  Visited):-
    findall(Temp, moveA(Temp), List),
    get_open(List, X, Y, Open, Moves, N, M, Color, Board,  Visited).

get_open([], _, _, Open, Open, _, _, _, _, _).
get_open([[X1,Y1]|Rest], X, Y, Open, Open2, N, M, Color, Board,  Visited) :-
    X2 is X + X1,
    Y2 is Y + Y1,
    ((inBoard(X2,Y2, N,M), getColor(X2, Y2, Color, Board), notVisited(X2, Y2, Visited) )-> append(Open, [[X2,Y2]], NewOpen) ; NewOpen = Open),
    get_open(Rest, X, Y, NewOpen, Open2, N, M, Color, Board,  Visited).



update([], _, Visited, Visited, _, _, ExactCost, ExactCost, EstimatedTCost, EstimatedTCost).
update([[X, Y]|T], Ind, Visited, UpVisited, Heuristic, Ec, ExactCost, NExactCost, EstimatedTCost, NEstimatedTCost):-
    nth0(Ind, Visited, LVisited),
    append(LVisited, [[X, Y]], Visited1),
    append(Visited, [Visited1], UpdatedVisited),
    NEc is Ec + 1, append(ExactCost, [NEc], UpdatedNExactCost),
    nth0(X, Heuristic, HL),
    nth0(Y, HL, HV), 
    NEst is NEc + HV,
    append(EstimatedTCost, [NEst], UpdatedNEstimatedTCost),
    update(T, Ind, UpdatedVisited, UpVisited, Heuristic, Ec, UpdatedNExactCost, NExactCost, UpdatedNEstimatedTCost, NEstimatedTCost).


% ExactCost has actual total cost from start to the last nood in the visited list
aStar(Ex, Ey, Visited, ExactCost, EstimatedTCost, Color, Heuristic, Board, N, M) :- 
    % if Color, starting point color, and goal color are
    length(Visited, L), L =:= 1, nth0(0, Visited, LVisited), length(LVisited, L1), L1 =:= 1, last(LVisited, [SX, SY]),
    ((\+getColor(SX, SY, Color, Board); \+getColor(Ex, Ey, Color, Board)),
    write('The starting point, goal, and color you provided do not match'),!;
    (
        length(EstimatedTCost, L2), L2 < 1, nth0(SX, Heuristic, HL), nth0(SY, HL, HV),
        append(EstimatedTCost, [HV], UpdatedNEstimatedTCost), append(ExactCost, [0], UpdatedNExactCost),
        aStar(Ex, Ey, Visited, UpdatedNExactCost, UpdatedNEstimatedTCost, Color, Heuristic, Board, N, M)
    ));
    
    
    length(EstimatedTCost, L), L < 1, write('Your goal can not be reached'),!;
    findMinIndex(EstimatedTCost, Ind, _), nth0(Ind, ExactCost, Ec),
    nth0(Ind, Visited, LVisited), last(LVisited, [SX, SY]), 
    end(SX, SY, Ex, Ey, LVisited, Ec, Color);  % if this is the Goal stop
    
    findMinIndex(EstimatedTCost, Ind, _), nth0(Ind, ExactCost, Ec),
    nth0(Ind, Visited, LVisited), last(LVisited, [SX, SY]),
    
    getallchild(SX,SY,[],Moves, N, M, Color, Board,LVisited),
    
    update(Moves, Ind, Visited, NVisited, Heuristic, Ec, ExactCost, NExactCost, EstimatedTCost, NEstimatedTCost),
    remove_at(Ind, NEstimatedTCost, NewEstimatedTCost),
    remove_at(Ind, NExactCost, NewExactCost),
    remove_at(Ind, NVisited, NewVisited),
    aStar(Ex, Ey, NewVisited, NewExactCost, NewEstimatedTCost, Color, Heuristic, Board, N, M),!.


% Takes variables X,Y of the goal, list of pathes, color, board, N, and M then this function calls fill_2d_HL that will create the Heuristic list
aStarAlgo(Ex, Ey, Visited, Board, N, M) :- 
    getColor(Ex, Ey, Color, Board),
    fill_2d_HL(Board, 0, 0, Ex, Ey, Color, Board, Heuristic), 
    aStar(Ex, Ey, Visited, _, _, Color, Heuristic, Board, N, M), !.


astar :-
    board2(Board),
    length(Board, N), % number of rows
    Board = [Row|_], length(Row, M), % number of columns
    aStarAlgo(1, 3, [[[0, 0]]], Board, N, M).


dfs :-
    board0(Board),
    length(Board, N), % number of rows
    Board = [Row|_], length(Row, M), % number of columns
    dfs(Board, N, M).
