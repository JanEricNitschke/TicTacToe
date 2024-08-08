module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Array
import Browser
import Cell
import Element exposing (Element, centerX, column, el, height, layout, mouseDown, mouseOver, padding, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import GameState
import Html exposing (Html)
import Html.Attributes exposing (disabled)
import Player
import Set



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
        { model | gameState = GameState.Win model.player (winningCells model.board model.player) }

    else if boardFull model.board then
        { model | gameState = GameState.Draw }

    else
        model


winningCells : Array.Array Cell.Cell -> Player.Player -> Set.Set Int
winningCells board player =
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
    Set.fromList (List.concat (List.filter (\line -> List.all (\index -> getWithDefault Cell.Empty index board == Cell.Player player) line) winningLines))


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
    layout [ padding 50, centerX ] <|
        column [ spacing 40, centerX ]
            [ el [ Font.bold, Font.size 50, centerX ] (text (GameState.toString model.gameState))
            , row [ spacing 40, centerX ]
                [ tictactoeButton 0 model
                , tictactoeButton 1 model
                , tictactoeButton 2 model
                ]
            , row
                [ spacing 40, centerX ]
                [ tictactoeButton 3 model
                , tictactoeButton 4 model
                , tictactoeButton 5 model
                ]
            , row
                [ spacing 40, centerX ]
                [ tictactoeButton 6 model
                , tictactoeButton 7 model
                , tictactoeButton 8 model
                ]
            ]


tictactoeButton : Int -> Model -> Element Msg
tictactoeButton index model =
    let
        disabled =
            (getWithDefault Cell.Empty index model.board /= Cell.Empty)
                || (model.gameState /= GameState.Playing)

        inWinningSet =
            case model.gameState of
                GameState.Win _ winningSet ->
                    Set.member index winningSet

                _ ->
                    False

        nominalBorder =
            if inWinningSet then
                color.red

            else if disabled then
                color.lightGrey

            else
                color.blue

        nominalBackground =
            if inWinningSet then
                color.red

            else if disabled then
                color.white

            else
                color.lightBlue

        downBorder =
            if inWinningSet then
                color.red

            else if disabled then
                color.lightGrey

            else
                color.blue

        downBackground =
            if inWinningSet then
                color.red

            else if disabled then
                color.white

            else
                color.blue

        downFont =
            if not disabled then
                color.red

            else
                color.white

        overBorder =
            if inWinningSet then
                color.red

            else
                color.lightGrey

        overBackground =
            if inWinningSet then
                color.red

            else
                color.white
    in
    Input.button
        [ padding 10
        , height (px 100)
        , width (px 100)
        , Border.width 3
        , Border.rounded 6
        , Border.color nominalBorder
        , Background.color nominalBackground
        , Font.center
        , Font.size 40

        -- The order of mouseDown/mouseOver can be significant when changing
        -- the same attribute in both
        , mouseDown
            [ Background.color downBackground
            , Border.color downBorder
            , Font.color downFont
            ]
        , mouseOver
            [ Background.color overBackground
            , Border.color overBorder
            ]
        ]
        { onPress =
            if disabled then
                Nothing

            else
                Just (getWithDefault One index indexes)
        , label = text (Cell.toString (getWithDefault Cell.Empty index model.board))
        }


color : { blue : Element.Color, darkCharcoal : Element.Color, lightBlue : Element.Color, lightGrey : Element.Color, white : Element.Color, black : Element.Color, red : Element.Color }
color =
    { blue = rgb255 0x72 0x9F 0xCF
    , darkCharcoal = rgb255 0x2E 0x34 0x36
    , lightBlue = rgb255 0xC5 0xE8 0xF7
    , lightGrey = rgb255 0xE0 0xE0 0xE0
    , white = rgb255 0xFF 0xFF 0xFF
    , black = rgb255 0x00 0x00 0x00
    , red = rgb255 0xFF 0x00 0x00
    }
