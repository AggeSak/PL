
% Read grid data from a file
read_grid(File, Grid) :-
    open(File, read, Stream),
    read_line_to_string(Stream, Line),
    number_string(N, Line),
    read_grid_lines(Stream, N, Grid),
    close(Stream).

read_grid_lines(Stream, N, Grid) :-
    read_grid_lines(Stream, N, N, [], Grid).

read_grid_lines(_, 0, _, Acc, Grid) :-
    reverse(Acc, Grid).

read_grid_lines(Stream, N, M, Acc, Grid) :-
    M > 0,
    read_line_to_string(Stream, Line),
    split_string(Line, ' ', '', StringList),
    maplist(number_string, Row, StringList),
    NewM is M - 1,
    read_grid_lines(Stream, N, NewM, [Row | Acc], Grid).

% Check if a move is valid
valid_move(Grid, N, X, Y, NewX, NewY) :-
    NewX >= 0, NewX < N,
    NewY >= 0, NewY < N,
    nth0(Y, Grid, Row),
    nth0(X, Row, Value),
    nth0(NewY, Grid, NewRow),
    nth0(NewX, NewRow, NewValue),
    NewValue < Value.

% Get direction string from move coordinates
get_direction(DX, DY, Direction) :-
    (DX =:= -1, DY =:= -1 -> Direction = 'nw';
     DX =:=  1, DY =:= -1 -> Direction = 'ne';
     DX =:= -1, DY =:=  1 -> Direction = 'sw';
     DX =:=  1, DY =:=  1 -> Direction = 'se';
     DX =:=  0, DY =:= -1 -> Direction = 'n';
     DX =:=  0, DY =:=  1 -> Direction = 's';
     DX =:= -1, DY =:=  0 -> Direction = 'w';
     DX =:=  1, DY =:=  0 -> Direction = 'e').

% Find shortest path using DFS
moves(File, Path) :-
    read_grid(File, Grid),
    length(Grid, N),
    dfs(Grid, N, 0, 0, [], Path).

% DFS recursive function
dfs(Grid, N, X, Y, Path, FinalPath) :-
    X =:= N - 1, Y =:= N - 1 ->
    reverse(Path, FinalPath);
    findall((NewX, NewY, DX, DY), 
            (between(-1, 1, DX), between(-1, 1, DY),
             (DX \= 0; DY \= 0),
             NewX is X + DX, NewY is Y + DY,
             valid_move(Grid, N, X, Y, NewX, NewY)),
            Moves),
    member((NewX, NewY, DX, DY), Moves),
    get_direction(DX, DY, Direction),
    dfs(Grid, N, NewX, NewY, [Direction | Path], FinalPath).

