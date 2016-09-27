                  % % % % % % % % % % % % % % % % % % % % %
                  %   Dots and Boxes game implementation  %
                  %         By Roelof Ruis - 2013         %
                  % % % % % % % % % % % % % % % % % % % % %

%                               --> OVERVIEW <--
% This file contains the high level predicates that initialize and run a single
% game.


% ======== INITIALIZATION ==========
:- module(game, [new_game/0, new_game/4, new_game/5, set_ai/2]).

:- use_module(graph_output, [print_board/1]).
:- use_module(board, [init_board/1, get_state/1, play_move/2]).
:- use_module(analysis, [occupied/3, num_squares/2, wins/2]).

:- dynamic(ai/2).
:- dynamic(player/2).
:- dynamic(option/1).

% =========== FUNCTIONS =============
% Start a default new game, human versus human on a 5x5 board.
new_game:-
    new_game(human, human, [5,5], []).

new_game(P1, P2, Sze, Opts):-
    new_game(P1, P2, Sze, Opts, _).

% Start a new game.
new_game(Player1, Player2, [Bx, By], Opts, Win):-
    retractall(option(_)),
    set_options(Opts),
    init_board([Bx, By]),
    assert_player(1, Player1),
    assert_player(2, Player2),
    ((option(hide_board));(show_board)),
    print('Starting new game on a '), print(Bx), print(' x '), print(By),
    print(' board...'), nl,
    run(2, 1), !, nl,
    print('The game ended.'),
    get_state(State),
    wins(State, Win).

% Show the current board state.
show_board:-
    get_state(State),
    print_board(State).

% Run the game
% Human players
% Code 1: Last move was ok
run(Pl1, 1):-
    playerswitch(Pl1, Pl2),
    player(Pl2, human),
    ask_for_move(Pl2, Input),
    move([Pl2/Input]), !.

% Code 2 or 0: Last move was completing a square or faulty so last player goes
% again.
run(Pl1, C):-
    C =\= 1,
    player(Pl1, human),
    ask_for_move(Pl1, Input),
    move([Pl1/Input]), !.

% AI Players
% Code 0: Last move was faulty, ends the game for the AI should come up with
% a correct move.
run(Pl1, 0):-
    player(Pl1, ai),
    print('AI failed at making a correct move...'), nl, !.

% Code 1: Last move was ok.
run(Pl1, 1):-
    playerswitch(Pl1, Pl2),
    player(Pl2, ai),
    ai(Pl2, AIName),
    get_state(State),
    call(AIName:next_move(State, Move)),
    move([Pl2/Move]), !.

% Code 2: Last move was completing a square so the last player moves again.
run(Pl1, 2):-
    player(Pl1, ai),
    ai(Pl1, AIName),
    get_state(State),
    call(AIName:next_move(State, Move)),
    move([Pl1/Move]), !.

% Move works together with run trying to play the last entered move and returing
% to run with a code telling what was the result of the last move.
move([_/(abort)]):- !.

move([Player/(X:Y:O)]):-
    play_move(Player/X:Y:O, Code),
    ((option(hide_board));(show_board)),
    ((option(hide_moves));(print(Player),print('/'),print(X),print(':'),
    print(Y),print(':'),print(O),nl)),
    get_state(State),
    check_state(State, Player, Code).
    
move([Player/Move]):-
    print('Invalid move format '),
    print(Move), nl,
    run(Player, 0), !.

check_state(State:Sze, Player, _):-
    wins(State:Sze, Player),
    occupied(State, Player, OccNr),
    num_squares(Sze, NumSqr),
    print('Player '), print(Player), print(' wins the game by filling '),
    print(OccNr), print(' of '), print(NumSqr), print(' boxes!').
    
check_state(State:Sze, _, _):-
    wins(State:Sze, 0),
    num_squares(Sze, NumSqr),
    Half is NumSqr / 2,
    print('Draw! Both players filled '), print(Half), print(' boxes.').
    
check_state(_, Player, Code):-
    run(Player, Code).

% prompt the user
ask_for_move(P, Input):-
    print('Player '),
    print(P),
    print(' please input your next move:'), nl,
    read(Input).

% little switch..!
playerswitch(1, 2).
playerswitch(2, 1).

% save the current player stats.
assert_player(PlayerNo, Type):-
    retractall(player(PlayerNo, _)),
    assert(player(PlayerNo, Type)).

% set the options for this game
set_options([]).

set_options([H|T]):-
    assert(option(H)),
    set_options(T).
    
% set the AI names for this game.
set_ai(PlayerNo, AIName):-
    retractall(ai(PlayerNo, _)),
    assert(ai(PlayerNo, AIName)).