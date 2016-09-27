                  % % % % % % % % % % % % % % % % % % % % %
                  %   Dots and Boxes game implementation  %
                  %         By Roelof Ruis - 2013         %
                  % % % % % % % % % % % % % % % % % % % % %

%                               --> OVERVIEW <--
% This file contains methods for controlling the board. It holds the current
% board state, and can return this information to the namespace running the
% game.


% ======== DEFINING MODULE ==========
:- module(board, [get_state/1, init_board/1, play_move/2]).

:- use_module(analysis, [completes_new/2, valid/2]).

:- dynamic(state/1).

% =========== FUNCTIONS =============
% Return the local board state
get_state(State):-
    state(State), !.
get_state([]:[0,0]).

% Return the local board size


% Initialize a new board of given size
init_board([Xsze, Ysze]):-
    reset_board,
    assert(state([]:[Xsze,Ysze])).

% Try to update the board with a new move if this move is valid.
% A move is of the form 'Player/Xpos:Ypos:Orientation'.
play_move(Move, 2):-
    get_state(State:B),
    valid(State:B, Move),
    completes_new(Move, State),
    retractall(state(_)),
    append(State, [Move], NewState),
    assert(state(NewState:B)), !.
    
play_move(Move, 1):-
    get_state(State:B),
    valid(State:B, Move),
    retractall(state(_)),
    append(State, [Move], NewState),
    assert(state(NewState:B)), !.
    
play_move(Move, 0):-
    print('Invalid move '),
    print_move(Move).

% ======= HELPER PREDICATES =========
print_move(P/X:Y:O):-
    print(P), print('/'),
    print(X), print(':'),
    print(Y), print(':'),
    print(O), nl, !.

reset_board:-
    retractall(state(_)).