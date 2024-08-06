module GameState exposing (..)

import Player


type GameState
    = Playing
    | Win Player.Player
    | Draw


toString : GameState -> String
toString gameState =
    case gameState of
        Playing ->
            "Playing"

        Win player ->
            "Player " ++ Player.toString player ++ " wins!"

        Draw ->
            "Game Drawn!"
