-module(tictactoe).
-moduledoc("A module for playing tictactoe.").

-export([play/2]).

-define(BOARD_SIZE, 3).
-define(WINNING_COMBINATIONS, [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9],
    [1, 4, 7],
    [2, 5, 8],
    [3, 6, 9],
    [1, 5, 9],
    [3, 5, 7]
]).

-record(move, {score = 0, index = 1}).

spot_free(Board, Move) ->
    Element = lists:nth(Move, Board),
    not lists:member(Element, ["X", "O"]).

make_move(Board, Move, Player) ->
    lists:sublist(Board, Move - 1) ++ [Player] ++ lists:nthtail(Move, Board).

swap_player("X") -> "O";
swap_player("O") -> "X".

display_board(Board) ->
    io:format("~s | ~s | ~s~n--+---+--~n", lists:sublist(Board, 1, 3)),
    io:format("~s | ~s | ~s~n--+---+--~n", lists:sublist(Board, 4, 3)),
    io:format("~s | ~s | ~s~n", lists:sublist(Board, 7, 3)).

winner(Board, Player) ->
    lists:any(
        fun([A, B, C]) ->
            lists:nth(A, Board) =:= Player andalso
                lists:nth(B, Board) =:= Player andalso
                lists:nth(C, Board) =:= Player
        end,
        ?WINNING_COMBINATIONS
    ).

board_full(Board) ->
    lists:all(
        fun(Element) -> lists:member(Element, ["X", "O"]) end,
        Board
    ).

player_turn(Board, Player) ->
    io:format("Player ~s, enter your move (1-9): ~n", [Player]),
    display_board(Board),
    case io:get_line("> ") of
        {error, Reason} ->
            io:format("Invalid input! Please enter a number. Reason: ~s~n", [Reason]),
            player_turn(Board, Player);
        Line ->
            case string:to_integer(string:trim(Line)) of
                {error, _} ->
                    io:format("Invalid input! Please enter a number.~n"),
                    player_turn(Board, Player);
                {Move, _} when Move < 1; Move > 9 ->
                    io:format("Out of bounds! Please enter a number between 0 and 8.~n"),
                    player_turn(Board, Player);
                {Move, _} ->
                    case spot_free(Board, Move) of
                        true ->
                            make_move(Board, Move, Player);
                        false ->
                            io:format("Spot taken! Please choose another spot.~n"),
                            player_turn(Board, Player)
                    end
            end
    end.

empty_spots(Board) ->
    lists:filter(
        fun(Index) -> spot_free(Board, Index) end,
        lists:seq(1, 9)
    ).

ai_random_move(Board) ->
    lists:nth(rand:uniform(length(empty_spots(Board))), empty_spots(Board)).

try_win_move(Board, Player) ->
    lists:keyfind(
        true,
        1,
        lists:map(
            fun(Move) -> {winner(make_move(Board, Move, Player), Player), Move} end,
            empty_spots(Board)
        )
    ).

ai_win_move(Board, Player) ->
    case try_win_move(Board, Player) of
        false ->
            ai_random_move(Board);
        {true, Move} ->
            Move
    end.

ai_win_block_move(Board, Player) ->
    case try_win_move(Board, Player) of
        false ->
            case try_win_move(Board, swap_player(Player)) of
                false ->
                    ai_random_move(Board);
                {true, Move} ->
                    Move
            end;
        {true, Move} ->
            Move
    end.

minmax(Board, Player) ->
    case {winner(Board, Player), winner(Board, swap_player(Player)), empty_spots(Board)} of
        {true, _, _} ->
            #move{score = 1};
        {_, true, _} ->
            #move{score = -1};
        {_, _, Empties} when length(Empties) == 0 ->
            #move{score = 0};
        {_, _, Empties} when length(Empties) == ?BOARD_SIZE * ?BOARD_SIZE ->
            #move{score = 0, index = ai_random_move(Board)};
        {_, _, Empties} ->
            Moves = lists:map(
                fun(Move) ->
                    NewBoard = make_move(Board, Move, Player),
                    #move{
                        score = -((minmax(NewBoard, swap_player(Player)))#move.score), index = Move
                    }
                end,
                Empties
            ),
            lists:max(Moves)
    end.

ai_best_move(Board, Player) ->
    (minmax(Board, Player))#move.index.

ai_turn(Board, Player, Strength) ->
    io:format("AI turn as player ~s with strength ~p~n", [Player, Strength]),
    display_board(Board),
    Move =
        case Strength of
            1 -> ai_random_move(Board);
            2 -> ai_win_move(Board, Player);
            3 -> ai_win_block_move(Board, Player);
            _ -> ai_best_move(Board, Player)
        end,
    timer:sleep(1000),
    make_move(Board, Move, Player).

loop(Board, Player, XStrength, OStrength) ->
    NewBoard =
        case {Player, XStrength, OStrength} of
            {"X", X, _} when X > 0 -> ai_turn(Board, Player, X);
            {"O", _, O} when O > 0 -> ai_turn(Board, Player, O);
            _ -> player_turn(Board, Player)
        end,
    case {winner(NewBoard, Player), board_full(NewBoard)} of
        {true, _} ->
            io:format("Player ~s wins!~n", [Player]),
            display_board(NewBoard);
        {_, true} ->
            io:format("It's a draw!~n"),
            display_board(NewBoard);
        _ ->
            loop(NewBoard, swap_player(Player), XStrength, OStrength)
    end.

play(XStrength, OStrength) ->
    Board = ["1", "2", "3", "4", "5", "6", "7", "8", "9"],
    Player = "X",
    loop(Board, Player, XStrength, OStrength).
