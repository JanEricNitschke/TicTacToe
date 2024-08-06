module Player exposing (..)


type Player
    = X
    | O


toString : Player -> String
toString player =
    case player of
        X ->
            "X"

        O ->
            "O"


swap : Player -> Player
swap player =
    case player of
        X ->
            O

        O ->
            X
