#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname tictactoe

main([Arg1, Arg2]) ->
    case {string:to_integer(Arg1), string:to_integer(Arg2)} of
        {{X_strength, _}, {O_strength, _}} when
            is_integer(X_strength) andalso is_integer(O_strength)
        ->
            tictactoe:play(X_strength, O_strength);
        _ ->
            usage()
    end;
main(_) ->
    usage().

usage() ->
    io:format("usage: tictactoe integer integer\n"),
    halt(1).
