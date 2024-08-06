module Cell exposing (..)

import Player


type Cell
    = Empty
    | Player Player.Player


toString : Cell -> String
toString cell =
    case cell of
        Empty ->
            " "

        Player player ->
            Player.toString player
