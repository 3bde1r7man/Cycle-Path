
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
dfs(_, _, Visited, Color, Board, _, _) :- 
    validCycle(Visited, Color, Board), 
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


:- use_module(library(random)).

random_number_less_than(Mx, RandomNumber) :-
    M is Mx - 1,
    random_between(0, M, RandomNumber).

minV([], V, V). % Base case: when the list is empty, V is unified with the accumulator S

minV([H|Tail], S, V) :-
    S > H, minV(Tail, H, V); minV(Tail, S, V). % If the head H is smaller than S, update S with H and continue recursively

findMinIndex([H|T], Ind, V) :-
    minV(T, H, V),
    nth0(Ind, [H|T], V),!. % Find the index of the minimum value V in the List

fill_list(N, Max, List) :-
    length(List, N),
    maplist(random_number_less_than(Max), List).

% Fill a 2D list with dimensions N x M with random numbers between 0 and Max
fill_2d_list(N, M, Max, Result) :-
    length(Result, N),
    maplist(fill_list(M, Max), Result).

% x, y is the nood i want to go to Ind is the path form start to the nood from it i will move 
% to the nood x, y. List has [[visited path]] as each visitedpath is from the start to a nood and the list with total costs
validMove(X, Y, List, Ind, Color, Board, N, M, NewList):-
    inBoard(X, Y, N, M),getColor(X, Y, Color, Board),
    nth0(Ind, List, Visited), notVisited(X, Y, Visited),
    append(Visited, [[X, Y]], Visited1), 
    append(List, Visited1 , NewList).


getallmoves(Moves):-
    findall(Template, move(Template), Moves).

% If the goal is achieved, print the result and terminate the process.
end(LX, LY, Ex, Ey, Visited, Cost, Color) :- 
	LX =:= Ex, LY =:= Ey, 
    write('Path: '), write(Visited), write(' TotalCost: '), write(Cost), write(' Color: '), write(Color), nl, 
    !.

remove_at(Index, List, Result) :-
    nth0(Index, List, Element),
    select(Element, List, Result).

% ExactCost has actual total cost from start to the last nood in the visited list
aStar(Ex, Ey, Visited, ExactCost, EstimatedTCost, Color, Heuristic, Board, N, M) :- 
    findMinIndex(EstimatedTCost, Ind, _), nth0(Ind, ExactCost, Ec),
    nth0(Ind, Visited, LVisited), last(LVisited, [SX, SY]), 
    end(SX, SY, Ex, Ey, LVisited, Ec, Color);  % if this is the Goal stop
    
    findMinIndex(EstimatedTCost, Ind, _), nth0(Ind, ExactCost, Ec),
    nth0(Ind, Visited, LVisited), last(LVisited, [SX, SY]),  
    getallmoves(Moves), 
    forall(member([X, Y], Moves), X1 is SX + X, Y1 is SY + Y,
           validMove(X1, Y1, Visited, Ind, Color, Board, N, M, NVisited), 
           NEc is Ec + 1,
           append(ExactCost, [NEc], NExactCost), nth0(X1, Heuristic, HL),
           nth0(Y1, HL, HV), NEst is NEc + HV,
           append(EstimatedTCost, [NEst], NEstimatedTCost)      
           ),
    remove_at(Ind, NEstimatedTCost, NewEstimatedTCost),
    remove_at(Ind, NExactCost, NewExactCost), remove_at(Ind, NVisited, NewVisited),
    aStar(Ex, Ey, NewVisited, NewExactCost, NewEstimatedTCost, Color, Heuristic, Board, N, M).


/** <examples>
?- dfs(2,0,_,_,[[2,0]],blue,[[yellow, yellow, yellow, red], [blue, yellow, blue, yellow],[blue, blue, blue, yellow],[blue, blue, blue, yellow]],4,4).
?- aStar(1,5,[[[0,1]]],[0],[5],r, [[-1,5,-1,-1,2,-1],[5,4,3,2,1,0],[-1,-1,-1,3,2,-1]], [[b,r,b,b,r,b],[r,r,r,r,r,r],[b,b,b,r,r,b]], 3, 6)
*/










