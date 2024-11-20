#!/usr/bin/env swipl -g main -t halt tictactoe_prolog.pl

:- module('tictactoe_prolog', [
    main/0,
    board1_player_spot_board2/4,
    player/1, spot/1, initial_state/2,
    player_other_player/2,
    game_board/1,
    board_spot_empty/3,
    win_board_player/2,
    board_winner_done/3,
    in_range_number_res/4,
    find_empty_indices/3,
    random_move_board1_player_board2/3,
    winning_move_board1_player_spot_board2/4,
    win_move_board1_player_board2/3,
    win_block_move_board1_player_board2/3,
    switch/2, end_state/4, larger_equal/3,
    best_move_board1_player_board2_endstate/4
]).

:- use_module(library(clpfd)).
:- use_module(library(reif)).
:- set_prolog_flag(double_quotes, chars).

player(x). % 'X'
player(o). % 'O'
spot(-).
spot(x).
spot(o).

board_empty([-,-,-,-,-,-,-,-,-]).

% Board starts empty.
% Player X makes first move
initial_state(Board, x) :- board_empty(Board).



% Swap the player
player_other_player(P1, P2) :-
    P1 = x,
    P2 = o.
player_other_player(P1, P2) :-
    P1 = o,
    P2 = x.

% Verify that the given list is a game board.
game_board([A,B,C,D,E,F,G,H,I]) :-
    spot(A),
    spot(B),
    spot(C),
    spot(D),
    spot(E),
    spot(F),
    spot(G),
    spot(H),
    spot(I).

% Move on the board.
% Initial board -> Player makes move at spot -> Resulting board.
% We do not need to assert that Board2 is a game_board.
% That is already given by the initial one being one and P being a player.
% Same for the others.
board1_player_spot_board2(Board1, P, 1, [P,B,C,D,E,F,G,H,I]) :-
    player(P),
    game_board(Board1),
    Board1 = [-,B,C,D,E,F,G,H,I].
board1_player_spot_board2(Board1, P, 2, [A,P,C,D,E,F,G,H,I]) :-
    player(P),
    game_board(Board1),
    Board1 = [A,-,C,D,E,F,G,H,I].
board1_player_spot_board2(Board1, P, 3, [A,B,P,D,E,F,G,H,I]) :-
    player(P),
    game_board(Board1),
    Board1 = [A,B,-,D,E,F,G,H,I].
board1_player_spot_board2(Board1, P, 4, [A,B,C,P,E,F,G,H,I]) :-
    player(P),
    game_board(Board1),
    Board1 = [A,B,C,-,E,F,G,H,I].
board1_player_spot_board2(Board1, P, 5, [A,B,C,D,P,F,G,H,I]) :-
    player(P),
    game_board(Board1),
    Board1 = [A,B,C,D,-,F,G,H,I].
board1_player_spot_board2(Board1, P, 6, [A,B,C,D,E,P,G,H,I]) :-
    player(P),
    game_board(Board1),
    Board1 = [A,B,C,D,E,-,G,H,I].
board1_player_spot_board2(Board1, P, 7, [A,B,C,D,E,F,P,H,I]) :-
    player(P),
    game_board(Board1),
    Board1 = [A,B,C,D,E,F,-,H,I].
board1_player_spot_board2(Board1, P, 8, [A,B,C,D,E,F,G,P,I]) :-
    player(P),
    game_board(Board1),
    Board1 = [A,B,C,D,E,F,G,-,I].
board1_player_spot_board2(Board1, P, 9, [A,B,C,D,E,F,G,H,P]) :-
    player(P),
    game_board(Board1),
    Board1 = [A,B,C,D,E,F,G,H,-].

% Find all the empty indices in the list.
% Used for random move. (Could also use bag of and nextmove.)
% Empty list has no empty (-) indices
find_empty_indices([],_,[]).
% If the head is - add the current index to the result
find_empty_indices([-|Ls],N,[N|Rs]):-
    N1 is N+1,
    find_empty_indices(Ls,N1,Rs).
% Do not if not
find_empty_indices([L|Ls],N,Rs):-
    L \= -,
    N1 is N+1,
    find_empty_indices(Ls,N1,Rs).

% Make a move on a random member of the empty indices
% Could also take the bag of all possible moved board and take
% a random member there.
random_move_board1_player_board2(Board1, P, Board2) :-
    find_empty_indices(Board1, 1, Indices),
    random_member(Spot, Indices),
    board1_player_spot_board2(Board1, P, Spot, Board2).

% Winning move is one, after which the player has won.
winning_move_board1_player_spot_board2(Board1, P, Spot, Board2) :-
    board1_player_spot_board2(Board1, P, Spot, Board2),
    win_board_player(Board2, P).

% Do winning or random move
win_move_board1_player_board2(Board1, P, Board2) :-
    winning_move_board1_player_spot_board2(Board1, P, _, Board2), !;
    random_move_board1_player_board2(Board1, P, Board2).

% Blocking move is one where if the opponent had made it they would have won.
% Take a winning, blocking or random move.
win_block_move_board1_player_board2(Board1, P, Board2) :-
    winning_move_board1_player_spot_board2(Board1, P, _, Board2), !;
    (player_other_player(P, Other), winning_move_board1_player_spot_board2(Board1, Other, Spot, _), board1_player_spot_board2(Board1, P, Spot, Board2)), !;
    random_move_board1_player_board2(Board1, P, Board2).

%Reifed version for use with if_/3
larger_equal(First, Second, true) :-
    First #>= Second.

larger_equal(First, Second, false) :-
    First #< Second.


% Check which of the exhaustively generated board
% Has the best end state for the player.
% Do so by recursively making best moves again.
best_of([Board], Player, Board, Endstate) :-
    player_other_player(Player, Other),
    best_move_board1_player_board2_endstate(Board, Other, _, Result),
    Endstate = (-Result),
    !.

best_of([Board|Boards], Player, BestBoard, Endstate) :-
    best_of(Boards, Player, RestBest, RestState),
    player_other_player(Player, Other),
    best_move_board1_player_board2_endstate(Board, Other, _, ThisEndstate),
    if_(larger_equal((-ThisEndstate), RestState),
        (
            (Endstate = (-ThisEndstate)),
            (BestBoard = Board)
        ),
        (
            (Endstate = RestState),
            (BestBoard = RestBest)
        )
    ), !.

% If the board is empty take the random shortcut
best_move_board1_player_board2_endstate(Board1, Player, Board2, Endstate) :-
    board_empty(Board1),
    random_move_board1_player_board2(Board1, Player, Board2),
    Endstate = 0, !.

% There are still moves to make -> the bag is not empty
best_move_board1_player_board2_endstate(Board1, Player, Board2, Endstate) :-
    end_state(Board1, Player, _, false),
    bagof(NextBoard, Spot^board1_player_spot_board2(Board1, Player, Spot, NextBoard) , NextBoardBag),
    best_of(NextBoardBag, Player, Board2, Endstate), !.

% The bag would be empty -> bagof fails.
best_move_board1_player_board2_endstate(Board1, Player, _, Endstate) :-
    end_state(Board1, Player, Endstate, true), !.

% Check if a board spot is empty.
board_spot_empty(Board, Spot, true) :-
    game_board(Board),
    nth1(Spot, Board, Elem),
    =(Elem, -).

board_spot_empty(Board, Spot, false) :-
        game_board(Board),
        nth1(Spot, Board, Elem),
        player(Elem).

% Player P has won the game on Board
% Rows
win_board_player(Board, P) :-
    player(P),
    game_board(Board),
    Board = [P,P,P,_,_,_,_,_,_].
win_board_player(Board, P) :-
    player(P),
    game_board(Board),
    Board = [_,_,_,P,P,P,_,_,_].
win_board_player(Board, P) :-
    player(P),
    game_board(Board),
    Board = [_,_,_,_,_,_,P,P,P].
% Cols
win_board_player(Board, P) :-
    player(P),
    game_board(Board),
    Board = [P,_,_,P,_,_,P,_,_].
win_board_player(Board, P) :-
    player(P),
    game_board(Board),
    Board = [_,P,_,_,P,_,_,P,_].
win_board_player(Board, P) :-
    player(P),
    game_board(Board),
    Board = [_,_,P,_,_,P,_,_,P].
% Diags
win_board_player(Board, P) :-
    player(P),
    game_board(Board),
    Board = [P,_,_,_,P,_,_,_,P].
win_board_player(Board, P) :-
    player(P),
    game_board(Board),
    Board = [_,_,P,_,P,_,P,_,_].


% Game is over when a player has won.
% Or when the board is filled.
% Win
board_winner_done(Board, Player, true) :- win_board_player(Board, Player).
% Draw
board_winner_done(Board, neither, true) :-
    Board = [A,B,C,D,E,F,G,H,I],
    game_board(Board),
    player(A),
    player(B),
    player(C),
    player(D),
    player(E),
    player(F),
    player(G),
    player(H),
    player(I),
    \+ board_winner_done(Board, x, true),
    \+ board_winner_done(Board, o, true).
% Game not done
board_winner_done(Board, Player, false) :-
    game_board(Board),
    player(Player),
    \+ board_winner_done(Board, x, true),
    \+ board_winner_done(Board, o, true),
    \+ board_winner_done(Board, neither, true).

% Check if a value is in a valid range.
% Used for board boundary and Ai strength.
in_range_number_res(N, Min, Max, true) :-
    N #> Min - 1,
    N #< Max + 1.
in_range_number_res(N, Min, Max, false) :-
    N #< Min;
    N #> Max.

% Read in a number from the user until they input a valid one.
read_number(N, Text) :-
    repeat,
    write(Text), nl,
    read_line_to_codes(current_input, Codes),
    catch(number_codes(N, Codes),
        _,
        (write('Invalid input'), nl, fail)
    ).

% Have the human player perform a move.
% Check for integer input, empty spot and board boundaries.
player_move(Board1, Player, Board2) :-
    format("Player ~w's turn.", [Player]), nl,
    show_board(Board1),
    read_number(Spot, 'Where to make your next move? [1-9]'),
    if_(in_range_number_res(Spot, 1, 9),
        true,
        (write('ERROR: Spot has to be in range [0-8]!'),nl,fail)
    ),
    if_(board_spot_empty(Board1, Spot),
        board1_player_spot_board2(Board1, Player, Spot, Board2),
        (format("ERROR: Spot ~w is already occupied!", [Spot]), fail)
    ).

% AI makes a move depending on the specified strength.
ai_move(Board1, Player, Strength, Board2) :-
    format("AI turn as ~w.", [Player]), nl,
    show_board(Board1),
    switch(Strength,
        [
            1:random_move_board1_player_board2(Board1, Player, Board2),
            2:win_move_board1_player_board2(Board1, Player, Board2),
            3:win_block_move_board1_player_board2(Board1, Player, Board2),
            4:best_move_board1_player_board2_endstate(Board1, Player, Board2, _)
        ]
    ),
    sleep(1).

% Ask the user for the ai strength.
read_ai_strength(Strength) :-
    write('AI strength settings:'), nl,
    write('1: Easy'), nl,
    write('2: Medium'), nl,
    write('3: Hard'), nl,
    write('4: Impossible'), nl,
    read_number(Strength, 'How strong should the AI be? [1 - 4]'),
    if_(in_range_number_res(Strength, 1, 4),
        true,
        (write('ERROR: AIStrength has to be in range [1-4]!'),nl,fail)
    ).

% Ask the user as which marker the AI should play.
% Determines if it moves first as 'x' always moves first.
% Chose 'n' to play with 2 players.
read_ai_marker(AIMarker) :-
    repeat,
    write('As which player should the AI play? [x, o, n (for no AI)]'), nl,
    read_line_to_codes(current_input, Codes),
    atom_codes(AIMarker, Codes),
    (AIMarker = x; AIMarker = o; AIMarker = n).

% (possibly) alternate turns between human and ai.
% Check if the game is over after each move.
turns(Board1, Player, AIPlayer, AIStrength, Res) :-
    game_board(Board1),
    player(Player),
    if_(AIPlayer = Player,
        ai_move(Board1, Player, AIStrength, Board2),
        player_move(Board1, Player, Board2)
    ),
    player_other_player(Player, Other),
    if_(board_winner_done(Board2, Winner),
        (
            if_(Winner = Player,
                (format("Player ~w wins the game!", [Player]), nl),
                (format("Match Drawn!", []), nl)
            ),
            Res = Board2
        ),
        turns(Board2, Other, AIPlayer, AIStrength, Res)
    ).

% Check the end state.
end_state(Board, Player, State, Over) :-
    if_(board_winner_done(Board, Winner),
        (
            Over = true,
            player_other_player(Player, Other),
            switch(Winner, [Player: (State=1), neither: (State=0), Other: (State=(-1))])
        ),
        (Over = false)
    ).

% Simple switch statement.
switch(X, [Val:Goal|Cases]) :-
    if_(X=Val,
        call(Goal),
        switch(X, Cases)
    ).

% %! go
% %
% % Program entry point
play :-
    initial_state(Board1, Player),
    read_ai_marker(AIMarker),
    if_(AIMarker = n,
        AIStrength = 0,
        read_ai_strength(AIStrength)
    ),
    turns(Board1, Player, AIMarker, AIStrength, Board2),
    show_board(Board2).

% Pretty print the game board.
show_board(Board) :-
    game_board(Board),
    board_pretty_board(Board, PrettyBoard),
    PrettyBoard = [A,B,C,D,E,F,G,H,I],
    Line_separator = '-------------',
    write(Line_separator), nl,
    format("| ~w | ~w | ~w |", [A,B,C]), nl,
    write(Line_separator), nl,
    format("| ~w | ~w | ~w |", [D,E,F]), nl,
    write(Line_separator), nl,
    format("| ~w | ~w | ~w |", [G,H,I]), nl,
    write(Line_separator), nl.

spot_index_pretty_spot(Spot, Index, Pretty) :-
    if_(Spot = -,
        (Pretty = Index),
        (Pretty = Spot)
    ).

board_pretty_board(Board, PrettyBoard) :-
    board_index_pretty_board(Board, 1, PrettyBoard).

board_index_pretty_board([], _, []).

board_index_pretty_board([Head|Board], Index, [PrettyHead|PrettyBoard]) :-
    spot_index_pretty_spot(Head, Index, PrettyHead),
    NextIndex #= Index + 1,
    board_index_pretty_board(Board, NextIndex, PrettyBoard).
%! main
%
% Command-line entry point
main :-
% TODO: Handle errors, allow exit via
% Ctrl-C or Ctrl-D
    play.
