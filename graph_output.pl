                  % % % % % % % % % % % % % % % % % % % % %
                  %   Dots and Boxes game implementation  %
                  %         By Roelof Ruis - 2013         %
                  % % % % % % % % % % % % % % % % % % % % %

%                               --> OVERVIEW <--
% This file contains methods for graphical output, used by the game module to
% print a state to the screen.


% ======== DEFINING MODULE ==========
:- module(graph_output,[print_board/1]).

:- use_module(analysis, [squares_owned/2]).


% =========== FUNCTIONS =============
% Main method that initiates the printing
print_board(_:[0,0]):-
    print('Nothing to print: The board was not initialized'), !.

print_board(Moves:[Xsize, Ysize]):-
    nl,
    print_per_row(1, Xsize, Ysize, Moves), nl, !.

% print_per_row/4 draws the board per 4 lines plus an extra termination line.
print_per_row(Ymax, Xsize, Ymax, State):-
    print_row_data(1, Xsize, Ymax, State), !.

print_per_row(CurY, Xsize, Ysize, State):-
    print_row_data(1, Xsize, CurY, State), nl,
    print_col_data(1, Xsize, CurY, State), nl,
    print_col_data(1, Xsize, CurY, State), nl,
    print_col_data(1, Xsize, CurY, State), nl,
    NextY is CurY + 1,
    print_per_row(NextY, Xsize, Ysize, State).

% print_row_data/4 prints the data on a line with nodes on it.
% Terminal position.
print_row_data(Xsize, Xsize, _, _):-
    print('O'), !.

% Position filled
print_row_data(Numb, Xsize, CurY, State):-
    Next is Numb + 1,
    member(_/Numb:CurY:h, State),
    print('O'),
    print('-----'),
    print_row_data(Next, Xsize, CurY, State).

% Empty position
print_row_data(Numb, Xsize, CurY, State):-
    Next is Numb + 1,
    print('O'),
    print('     '),
    print_row_data(Next, Xsize, CurY, State).

% print_col_data prints the three rows in between the nodes
% filled, last column.
print_col_data(Xsize, Xsize, CurY, State):-
    member(_/Xsize:CurY:v, State),
    print('|'),
    squares_owned(State, Owned),
    fill_square(Xsize, CurY, Owned), !.

% Empty, last column.
print_col_data(Xsize, Xsize, _, _):-
    print(' '), !.

% Filled column
print_col_data(CurX, Xsize, CurY, State):-
    NextX is CurX + 1,
    member(_/CurX:CurY:v, State),
    print('|'),
    squares_owned(State, Owned),
    fill_square(CurX, CurY, Owned),
    print_col_data(NextX, Xsize, CurY, State).

% Empty column
print_col_data(CurX, Xsize, CurY, State):-
    NextX is CurX + 1,
    print('      '),
    print_col_data(NextX, Xsize, CurY, State).

% Fill the square if it is owned
fill_square(X, Y, Owned):-
    member(1/X:Y, Owned),
    print(' 111 ').
    
fill_square(X, Y, Owned):-
    member(2/X:Y, Owned),
    print(' 222 ').

fill_square(_, _, _):-
    print('     ').