#!/usr/bin/env swipl -g run_tests -t halt tictactoe_prolog_test.pl

:- use_module('tictactoe_prolog', [
    board1_player_spot_board2/4,
    player/1, spot/1, initial_state/2,
    player_other_player/2,
    game_board/1,
    board_spot_empty/3,
    win_board_player/2,
    board_winner_done/3,
    in_range_number_res/2
]).

:- use_module(library(plunit)).

:- begin_tests(player_and_swap_player).

test(player) :-
    player(x),
    player(o),
    \+ player(-).

test(player_other_player) :-
    player_other_player(x, o),
    player_other_player(o, x).

:- end_tests(player_and_swap_player).

:- begin_tests(game_board_characteristics).

test(board_spot) :-
    spot(x),
    spot(o),
    spot(-),
    \+ spot(a).

test(initial_state_board_player) :-
    initial_state([-,-,-,-,-,-,-,-,-], x).

test(game_board_valid) :-
    game_board([x,x,x,x,x,x,x,x,x]),
    game_board([-,-,-,-,-,-,-,-,-]),
    game_board([x,-,o,o,-,x,-,-,-]),
    \+ game_board([x,-,o,o,-,x,-,-]),
    \+ game_board([]),
    \+ game_board([x,-,o,o,-,x,-,-,a]).

:- end_tests(game_board_characteristics).

:- begin_tests(valid_moves).

test(valid_move) :-
    board1_player_spot_board2([x,x,x,x,x,x,x,x,-], x, 9, [x,x,x,x,x,x,x,x,x]).

test(wrong_result) :-
    \+ board1_player_spot_board2([x,x,x,x,x,x,x,x,-], x, 9, [x,x,x,x,x,x,x,x,o]).

test(invalid_board) :-
    \+ board1_player_spot_board2([a,b,c], _, _, _).

test(invalid_player) :-
    \+ board1_player_spot_board2(_, a, _, _).

test(correct_endstate, all(Board2 == [[-,-,x,-,-,-,-,-,-]])) :-
    board1_player_spot_board2([-,-,-,-,-,-,-,-,-], x, 3, Board2).

test(invalid_spot) :-
    \+ board1_player_spot_board2(_, _, 0, _).

test(invalid_result_spot) :-
    \+ board1_player_spot_board2([-,-,-,-,-,-,-,-,-], x, 3, [-,-,-,x,-,-,-,-,-]).

:- end_tests(valid_moves).



:- begin_tests(win_game).

test(win_row, all(Winner == [x])) :-
    win_board_player([x,x,x,-,-,-,-,-,-], Winner),
    win_board_player([-,-,-,x,x,x,-,-,-], Winner),
    win_board_player([-,-,-,-,-,-,x,x,x], Winner).

test(win_col, all(Winner == [o])) :-
    win_board_player([o,-,-,o,-,-,o,-,-], Winner),
    win_board_player([-,o,-,-,o,-,-,o,-], Winner),
    win_board_player([-,-,o,-,-,o,-,-,o], Winner).

test(win_diag, all(Winner == [x])) :-
    win_board_player([x,-,-,-,x,-,-,-,x], Winner),
    win_board_player([-,-,x,-,x,-,x,-,-], Winner).

:- end_tests(win_game).

:- begin_tests(game_over_win).

test(win_row, all(Res == [true])) :-
    board_winner_done([x,x,x,-,-,-,-,-,-], x, Res),
    board_winner_done([-,-,-,x,x,x,-,-,-], x, Res),
    board_winner_done([-,-,-,-,-,-,x,x,x], x, Res).

test(win_col, all(Res == [true])) :-
    board_winner_done([o,-,-,o,-,-,o,-,-], o, Res),
    board_winner_done([-,o,-,-,o,-,-,o,-], o, Res),
    board_winner_done([-,-,o,-,-,o,-,-,o], o, Res).

test(win_diag, all(Res == [true])) :-
    board_winner_done([x,-,-,-,x,-,-,-,x], x, Res),
    board_winner_done([-,-,x,-,x,-,x,-,-], x, Res).

:- end_tests(game_over_win).

:- begin_tests(game_over_draw).

test(draw, all(Winner == [neither])) :-
    board_winner_done([x,o,x,x,o,o,o,x,x], Winner, true).

:- end_tests(game_over_draw).

:- begin_tests(numbers_in_range).

test(valid, all(Res == [true])) :-
    in_range_number_res(1, Res),
    in_range_number_res(2, Res),
    in_range_number_res(3, Res),
    in_range_number_res(4, Res),
    in_range_number_res(5, Res),
    in_range_number_res(6, Res),
    in_range_number_res(7, Res),
    in_range_number_res(8, Res),
    in_range_number_res(9, Res).

test(invalid, all(Res == [false])) :-
    in_range_number_res(-10, Res),
    in_range_number_res(-1, Res),
    in_range_number_res(10, Res),
    in_range_number_res(1300, Res).


:- end_tests(numbers_in_range).
