                  % % % % % % % % % % % % % % % % % % % % %
                  %   Dots and Boxes game implementation  %
                  %         By Roelof Ruis - 2013         %
                  % % % % % % % % % % % % % % % % % % % % %

%                               --> OVERVIEW <--
% Module containing some analysis functions.


% ======== DEFINING MODULE ==========
:- module(analysis, [squares_owned/2, completes_new/2, valid/2, occupied/3, wins/2, num_squares/2, completes/3]).

% =========== FUNCTIONS =============
% squares_owned(+State, -Owns) takes as input a state and returns a list with
% all squares that are owned by a player of the form [Player/XPos:YPos].
squares_owned(State, Owns):-
    squares_owned([], State, Owns), !.

squares_owned(_, [], []).
squares_owned(PastMoves, [CurMove|FutureMoves], AllOwns):-
    findall(Q, completes(CurMove, PastMoves, Q), ThisMove),
    squares_owned([CurMove|PastMoves], FutureMoves, RestOwns),
    append(RestOwns, ThisMove, AllOwns).

% Check if the move completes a new square in the given state.
completes_new(Move, State):-
    completes(Move, State, _).

% Check if a move is valid in a state
valid(State:[Xsze,Ysze], _/X:Y:O):-
    \+ member(_/X:Y:O, State),
    X > 0,
    Y > 0,
    ((
      O == h,
      X < Xsze,
      Y =< Ysze
     );(
      O == v,
      X =< Xsze,
      Y < Ysze
    )), !.

% Check the number of occupied squares for every player
occupied(State, Player, Nr):-
    squares_owned(State, Owns),
    findall(*, member(Player/_:_, Owns), L),
    length(L, Nr).

% Find out who would win in this state.
wins(State:Board, Player):-
    occupied(State, 1, Pl1Occ),
    occupied(State, 2, Pl2Occ),
    num_squares(Board, NumSqr),
    check_win(Pl1Occ, Pl2Occ, NumSqr, Player), !.
    
% Find out how many squares there are on the board.
num_squares([X, Y], N):-
    N is (X - 1) * (Y - 1).

% ======= HELPER PREDICATES =========
% Move completes square below.
completes(P/X:Y:h, PastMoves, P/X:Y):-
    NextY is Y + 1,
    NextX is X + 1,
    member(_/X:Y:v, PastMoves),
    member(_/X:NextY:h, PastMoves),
    member(_/NextX:Y:v, PastMoves).

% Move completes square above.
completes(P/X:Y:h, PastMoves, P/X:PastY):-
    PastY is Y - 1,
    NextX is X + 1,
    member(_/X:PastY:v, PastMoves),
    member(_/X:PastY:h, PastMoves),
    member(_/NextX:PastY:v, PastMoves).

% Move completes square to the right
completes(P/X:Y:v, PastMoves, P/X:Y):-
    NextX is X + 1,
    NextY is Y + 1,
    member(_/X:Y:h, PastMoves),
    member(_/X:NextY:h, PastMoves),
    member(_/NextX:Y:v, PastMoves).

% Move completes square to the left
completes(P/X:Y:v, PastMoves, P/PastX:Y):-
    PastX is X - 1,
    NextY is Y + 1,
    member(_/PastX:Y:h, PastMoves),
    member(_/PastX:Y:v, PastMoves),
    member(_/PastX:NextY:h, PastMoves).

% helper predicate for the win function
check_win(Pl1Occ, _, NumSqr, 1):-
    Pl1Occ > NumSqr / 2.
    
check_win(_, Pl2Occ, NumSqr, 2):-
    Pl2Occ > NumSqr / 2.

check_win(Pl1Occ, Pl2Occ, NumSqr, 0):-
    Pl1Occ =:= Pl2Occ,
    NumSqr =:= Pl1Occ + Pl2Occ.

check_win(_, _, _, -1).