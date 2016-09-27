                  % % % % % % % % % % % % % % % % % % % % %
                  %   Dots and Boxes game implementation  %
                  %         By Roelof Ruis - 2013         %
                  % % % % % % % % % % % % % % % % % % % % %

% ========== LOAD MODULES ============
:- use_module(game, [new_game/0, new_game/4, new_game/5, set_ai/2]).

%         --> SET THE AI <--
% Load the modules that you want to use as AI.
% Make sure that all AIName names used in set_ai(+Player, -AIName) are loaded!
:- use_module(naive_ai, []).
:- use_module(cunning_ai, []).
:- set_ai(1, naive_ai). % set player 1 ai to correct module
:- set_ai(2, naive_ai). % set player 2 ai to correct module

:- dynamic(score/2).


% ========== FUNCTIONALITY ===========
simple_game:-
    new_game.

tournament(Type):-
    reset_scores,
    tournboards(Type, Boards),
    setup_games(Boards).
    
% ======== HELPER PREDICATES =========
setup_games([]):-
    display_score.
    
setup_games([B|T]):-
    new_game(ai, ai, B, [hide_moves, hide_board], WinCode), nl, nl,
    add_score(WinCode),
    setup_games(T).
    
add_score(-1).

add_score(Code):-
    score(Code, Numb),
    retractall(score(Code, _)),
    NewNumb is Numb + 1,
    assert(score(Code, NewNumb)).
    
reset_scores:-
    retractall(score(_, _)),
    assert(score(0, 0)),
    assert(score(1, 0)),
    assert(score(2, 0)).

:- reset_scores.

display_score:-
    score(0, Draws),
    score(1, P1Wins),
    score(2, P2Wins),
    Total is Draws + P1Wins + P2Wins,
    print('And now for the scores: '), nl,
    print('On a total of '), print(Total), print(' games ... '), nl,
    print('Player 1 won '), print(P1Wins), print(' times.'), nl,
    print('Player 2 won '), print(P2Wins), print(' times.'), nl,
    print('There were '), print(Draws), print(' draws.').
    
tournboards(large,[[2,2],[2,3],[2,4],[2,5],[2,6],[2,7],[2,8],[2,9],[2,10],
                  [3,2],[3,3],[3,4],[3,5],[3,6],[3,7],[3,8],[3,9],[3,10],
                  [4,2],[4,3],[4,4],[4,5],[4,6],[4,7],[4,8],[4,9],[4,10],
                  [5,2],[5,3],[5,4],[5,5],[5,6],[5,7],[5,8],[5,9],[5,10],
                  [6,2],[6,3],[6,4],[6,5],[6,6],[6,7],[6,8],[6,9],[6,10],
                  [7,2],[7,3],[7,4],[7,5],[7,6],[7,7],[7,8],[7,9],[7,10],
                  [8,2],[8,3],[8,4],[8,5],[8,6],[8,7],[8,8],[8,9],[8,10],
                  [9,2],[9,3],[9,4],[9,5],[9,6],[9,7],[9,8],[9,9],[9,10],
                  [10,2],[10,3],[10,4],[10,5],[10,6],[10,7],[10,8],[10,9],[10,10],
                  [15,15],[20,20],[8,13],[13,8]]).
                  
tournboards(square,[[2,2],[3,3],[4,4],[5,5],[6,6],[7,7],[8,8],[9,9],[10,10]]).
tournboards(fibo,[[2,2],[2,3],[3,4],[4,6],[6,9],[9,14],[14,22]]).
    