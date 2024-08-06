module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Array
import Browser
import Cell
import GameState
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Player



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { player : Player.Player
    , board : Array.Array Cell.Cell
    , gameState : GameState.GameState
    }


init : Model
init =
    Model Player.X (Array.initialize 9 (always Cell.Empty)) GameState.Playing



-- UPDATE


getWithDefault : a -> Int -> Array.Array a -> a
getWithDefault default index array =
    Maybe.withDefault default (Array.get index array)


type Msg
    = One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine


indexes : Array.Array Msg
indexes =
    Array.fromList [ One, Two, Three, Four, Five, Six, Seven, Eight, Nine ]


update : Msg -> Model -> Model
update msg model =
    model
        |> makeModelMove msg
        |> updateModelGameState
        |> swapModelPlayer


swapModelPlayer : Model -> Model
swapModelPlayer model =
    case model.player of
        Player.X ->
            { model | player = Player.O }

        Player.O ->
            { model | player = Player.X }


updateModelGameState : Model -> Model
updateModelGameState model =
    if playerWon model.board model.player then
        { model | gameState = GameState.Win model.player }

    else if boardFull model.board then
        { model | gameState = GameState.Draw }

    else
        model


playerWon : Array.Array Cell.Cell -> Player.Player -> Bool
playerWon board player =
    let
        winningLines =
            [ -- Rows
              [ 0, 1, 2 ]
            , [ 3, 4, 5 ]
            , [ 6, 7, 8 ]

            -- Columns
            , [ 0, 3, 6 ]
            , [ 1, 4, 7 ]
            , [ 2, 5, 8 ]

            -- Diagonals
            , [ 0, 4, 8 ]
            , [ 2, 4, 6 ]
            ]
    in
    List.any (\line -> List.all (\index -> getWithDefault Cell.Empty index board == Cell.Player player) line) winningLines


boardFull : Array.Array Cell.Cell -> Bool
boardFull board =
    List.all (\cell -> cell /= Cell.Empty) (Array.toList board)


makeModelMove : Msg -> Model -> Model
makeModelMove msg model =
    case msg of
        One ->
            { model | board = Array.set 0 (Cell.Player model.player) model.board }

        Two ->
            { model | board = Array.set 1 (Cell.Player model.player) model.board }

        Three ->
            { model | board = Array.set 2 (Cell.Player model.player) model.board }

        Four ->
            { model | board = Array.set 3 (Cell.Player model.player) model.board }

        Five ->
            { model | board = Array.set 4 (Cell.Player model.player) model.board }

        Six ->
            { model | board = Array.set 5 (Cell.Player model.player) model.board }

        Seven ->
            { model | board = Array.set 6 (Cell.Player model.player) model.board }

        Eight ->
            { model | board = Array.set 7 (Cell.Player model.player) model.board }

        Nine ->
            { model | board = Array.set 8 (Cell.Player model.player) model.board }



-- VIEW
-- This should be in functions.
-- This should use a map function to generate the buttons
-- This needs to be in a grid with fixes sizes
-- Button should do nothing when already filled


view : Model -> Html Msg
view model =
    div [ class "grid-container" ]
        (List.append
            [ div [] [ text (GameState.toString model.gameState) ] ]
            (Array.toList
                (Array.indexedMap
                    (\index cell ->
                        button
                            [ class "grid-item"
                            , disabled
                                ((getWithDefault Cell.Empty index model.board /= Cell.Empty)
                                    || (model.gameState /= GameState.Playing)
                                )
                            , onClick (getWithDefault One index indexes)
                            ]
                            [ text (Cell.toString cell) ]
                    )
                    model.board
                )
            )
        )
