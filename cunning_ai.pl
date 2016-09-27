                  % % % % % % % % % % % % % % % % % % % % %
                  %   Dots and Boxes game implementation  %
                  %         By Roelof Ruis - 2013         %
                  % % % % % % % % % % % % % % % % % % % % %

%                               --> OVERVIEW <--
% The Cunning AI plays a bit smarter already!

% ======== DEFINING MODULE ==========
:- module(cunning_ai, []).

:- use_module([analysis]).
% =========== FUNCTIONS =============
% next_move(+State:BoardSize, -Move).
next_move(State, NextMove):-
    all_valid_moves(State, ValidMoves),
    evaluate(State, ValidMoves, EvaluatedNodes),
    pick_best(EvaluatedNodes, NextMove).

evaluate(_, [], []).

evaluate(State, [Move|OtherMoves], [[Move]:Score|ScoredMoves]):-
    score(State, Move, Score),
    evaluate(State, OtherMoves, ScoredMoves).

score(State:_, Move, 10):-
    completes(Move, State, _).
    
score(State:BS, Move, 0):-
    append(State, [Move], NewState),
    all_valid_moves(NewState:BS, ValidMoves),
    findall(*,
            ( member(M, ValidMoves),
              completes(M, NewState, _)
            ),
            L),
    \+length(L, 0).
    
score(_, _, 5).

% Pick best.
pick_best([M1:S, _:S2|T], FinalM):-
    S >= S2,
    pick_best([M1:S|T], FinalM).
    
pick_best([_, MS2|T], FinalM):-
    pick_best([MS2|T], FinalM).

pick_best([[_/X:Y:O]:_], X:Y:O).

% All moves that are still posible on this board.
all_valid_moves(State:[Bx, By], Moves):-
    gen_rand_numlist(1:Bx, Xlist),
    gen_rand_numlist(1:By, Ylist),
    findall(_/X:Y:O,
            ( member(X, Xlist),
              member(Y, Ylist),
              member(O, [h, v]),
              valid(State:[Bx, By], _/X:Y:O )),
            Moves).
            
% Generate a list of numbers
gen_rand_numlist(S:E, L):-
    gen_numlist(S:E, Nlist),
    N is random(E),
    shuffle(Nlist, N, L).

gen_numlist(E:E, [E]):- !.
gen_numlist(S:E, [S|L]):-
    S < E,
    Sn is S + 1,
    gen_numlist(Sn:E, L).
    
shuffle(Nlist, 0, Nlist):- !.

shuffle([H1, H2|T], N, NewList):-
     Nmin is N - 1,
     append(T, [H2, H1], L),
     shuffle(L, Nmin, NewList).