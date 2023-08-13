#!/usr/bin/env swipl -g run_tests -t halt tictactoe_prolog_test.pl

:- use_module('tictactoe_prolog', [
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
    win_block_move_board1_player_board2/3
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
    in_range_number_res(1, 1, 9, Res),
    in_range_number_res(2, 1, 9, Res),
    in_range_number_res(3, 1, 9, Res),
    in_range_number_res(4, 1, 9, Res),
    in_range_number_res(5, 1, 9, Res),
    in_range_number_res(6, 1, 9, Res),
    in_range_number_res(7, 1, 9, Res),
    in_range_number_res(8, 1, 9, Res),
    in_range_number_res(9, 1, 9, Res).

test(invalid, all(Res == [false])) :-
    in_range_number_res(-10, 1, 9, Res),
    in_range_number_res(-1, 1, 9, Res),
    in_range_number_res(10, 1, 9, Res),
    in_range_number_res(1300, 1, 9, Res).


:- end_tests(numbers_in_range).


:- begin_tests(find_empty_indices).

test(empty_indices, all(Res == [[4,8,9]])) :-
    find_empty_indices([x,x,x,-,o,o,x,-,-], 1, Res).


:- end_tests(find_empty_indices).

:- begin_tests(random_move).

test(random_move_works, nondet) :-
    random_move_board1_player_board2([-,-,x,x,x,x,x,x,x], o, Board2),
    member(Board2, [[-,o,x,x,x,x,x,x,x], [o,-,x,x,x,x,x,x,x]]).

:- end_tests(random_move).

:- begin_tests(winning_move).

test(winning_move_finds_row, all(Res == [[o,x,o,x,o,-,x,x,x]])) :-
    winning_move_board1_player_spot_board2([o,x,o,x,o,-,x,x,-], x, 9, Res).

test(winning_move_finds_col, all(Res == [[o,x,-,o,o,-,o,x,x]])) :-
    winning_move_board1_player_spot_board2([o,x,-,-,o,-,o,x,x], o, 4, Res).

test(winning_move_finds_diag, all(Res == [[o,-,-,-,o,-,-,-,o]])) :-
    winning_move_board1_player_spot_board2([o,-,-,-,o,-,-,-,-], o, 9, Res).

test(winning_move_finds_antidiag, all(Res == [[-,-,x,-,x,-,x,-,-]])) :-
    winning_move_board1_player_spot_board2([-,-,-,-,x,-,x,-,-], x, 3, Res).

test(winning_move_fails_on_no_win) :-
    \+ winning_move_board1_player_spot_board2([x,o,x,-,o,o,o,x,x], x, _, _).

:- end_tests(winning_move).

:- begin_tests(win_move).

test(win_move_finds_row, all(Res == [[o,x,o,x,o,-,x,x,x]])) :-
    win_move_board1_player_board2([o,x,o,x,o,-,x,x,-], x, Res).

test(win_move_finds_col, all(Res == [[o,x,-,o,-,-,o,x,x]])) :-
    win_move_board1_player_board2([o,x,-,-,-,-,o,x,x], o, Res).

test(win_move_finds_diag, all(Res == [[o,-,-,-,o,-,-,-,o]])) :-
    win_move_board1_player_board2([o,-,-,-,o,-,-,-,-], o, Res).

test(win_move_finds_antidiag, all(Res == [[-,-,x,-,x,-,x,-,-]])) :-
    win_move_board1_player_board2([-,-,-,-,x,-,x,-,-], x, Res).

test(win_move_does_not_fail_on_no_win, all(Res == [[x,o,x,x,o,o,o,x,x]])) :-
    win_move_board1_player_board2([x,o,x,-,o,o,o,x,x], x, Res).

:- end_tests(win_move).

:- begin_tests(win_block_move).

test(win_block_move_prioritizes_win, all(Res == [[x,x,x,o,o,-,-,-,-]])) :-
    win_block_move_board1_player_board2([x,x,-,o,o,-,-,-,-], x, Res).

test(win_block_move_blocks, all(Res == [[x,-,-,o,o,x,-,-,-]])) :-
    win_block_move_board1_player_board2([x,-,-,o,o,-,-,-,-], x, Res).

test(win_block_move_falls_back_to_random, all(Res == [[x,o,x,x,o,o,o,x,x]])) :-
    win_block_move_board1_player_board2([x,o,x,-,o,o,o,x,x], x, Res).

:- end_tests(win_block_move).
