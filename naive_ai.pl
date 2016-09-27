                  % % % % % % % % % % % % % % % % % % % % %
                  %   Dots and Boxes game implementation  %
                  %         By Roelof Ruis - 2013         %
                  % % % % % % % % % % % % % % % % % % % % %

%                               --> OVERVIEW <--
% The Naive AI. It plays very simple moves, not calculating any steps ahead.


% ======== DEFINING MODULE ==========
:- module(naive_ai, []).

:- use_module(analysis, [valid/2]). % imports some analysis functionality

% =========== FUNCTIONS =============
% next_move(+State:BoardSize, -Move).
next_move(State, Move):-
    next_move(State, State, Move).

next_move([]:_, _, 1:1:h).

next_move([_/X:Y:O|_]:_, RefState, X:Y:NewO):-
    flip_orient(O, NewO),
    valid(RefState, _/X:Y:NewO).
    
next_move([_/X:Y:O|_]:_, RefState, Nx:Y:O):-
    Nx is X + 1,
    valid(RefState, _/Nx:Y:O).
    
next_move([_/X:Y:O|_]:_, RefState, X:Ny:O):-
    Ny is Y + 1,
    valid(RefState, _/X:Ny:O).
    
next_move([_|T]:B, RefState, M):-
    next_move(T:B, RefState, M).

flip_orient(h, v).
flip_orient(v, h).