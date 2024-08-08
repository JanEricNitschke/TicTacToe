module GameState exposing (..)

import Player
import Set exposing (Set)


type GameState
    = Playing
    | Win Player.Player (Set Int)
    | Draw


toString : GameState -> String
toString gameState =
    case gameState of
        Playing ->
            "Playing"

        Win player _ ->
            "Player " ++ Player.toString player ++ " wins!"

        Draw ->
            "Game Drawn!"
