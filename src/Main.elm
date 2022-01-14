module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (Html, div, text)
import Html.Attributes exposing (..)
import Json.Decode as Decode
import Random
import Time


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = Move Time.Posix
    | ChangeDirection Direction
    | CreateFood Cell
    | None


type alias Cell =
    { x : Int
    , y : Int
    }


type alias Snake =
    List Cell


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Model =
    { score : Int
    , snake : Snake
    , direction : Direction
    , interval : Float
    , dead : Bool
    , canMove : Bool
    , food : Cell
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.dead == True then
        Sub.none

    else
        Sub.batch [ Time.every model.interval Move, Browser.Events.onKeyDown keyDecoder ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey string =
    if string == "ArrowDown" then
        ChangeDirection Down

    else if string == "ArrowUp" then
        ChangeDirection Up

    else if string == "ArrowLeft" then
        ChangeDirection Left

    else if string == "ArrowRight" then
        ChangeDirection Right

    else
        None


boardSize : Int
boardSize =
    20


boardWidth : Int
boardWidth =
    20 * boardSize


roll : Random.Generator Int
roll =
    Random.int 0 (boardSize - 1)


initModel : Model
initModel =
    { score = 0
    , snake = List.map (\x -> { x = x, y = 9 }) (List.range 9 12)
    , direction = Left
    , interval = 200
    , dead = False
    , canMove = True
    , food = { x = -1, y = -1 }
    }


createFood : Cmd Msg
createFood =
    Random.generate CreateFood (Random.map2 Cell roll roll)


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, createFood )


goUp : Cell -> Cell
goUp pos =
    let
        newY =
            pos.y - 1
    in
    if newY > -1 then
        { pos | y = newY }

    else
        { pos | y = boardSize - 1 }


goDown : Cell -> Cell
goDown pos =
    let
        newY =
            pos.y + 1
    in
    if newY < boardSize then
        { pos | y = newY }

    else
        { pos | y = 0 }


goLeft : Cell -> Cell
goLeft pos =
    let
        newX =
            pos.x - 1
    in
    if newX > -1 then
        { pos | x = newX }

    else
        { pos | x = boardSize - 1 }


goRight : Cell -> Cell
goRight pos =
    let
        newX =
            pos.x + 1
    in
    if newX < boardSize then
        { pos | x = newX }

    else
        { pos | x = 0 }


getMoveFn direction =
    case direction of
        Up ->
            goUp

        Down ->
            goDown

        Left ->
            goLeft

        Right ->
            goRight


canChangeDirection direction currentDirection =
    case currentDirection of
        Up ->
            direction /= Down

        Down ->
            direction /= Up

        Left ->
            direction /= Right

        Right ->
            direction /= Left


first : Snake -> Cell
first snake =
    case List.head snake of
        Just head ->
            head

        -- can never be
        Nothing ->
            { x = 0, y = 0 }


isTouchingItself : Snake -> Bool
isTouchingItself snake =
    let
        head =
            first snake
    in
    List.any (\pos -> pos == head) (List.take (List.length snake - 1) (List.reverse snake))


moveSnake : Snake -> Direction -> Snake
moveSnake snake direction =
    getMoveFn direction (first snake) :: List.take (List.length snake - 1) snake


isEatingFood : Snake -> Cell -> Bool
isEatingFood snake food =
    let
        head =
            first snake
    in
    food == head


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move _ ->
            let
                newSnake =
                    moveSnake model.snake model.direction
            in
            if isTouchingItself newSnake then
                ( { model | dead = True }, Cmd.none )

            else if isEatingFood newSnake model.food then
                let
                    newScore =
                        model.score + 1

                    newInterval =
                        model.interval - logBase 2 (toFloat newScore)
                in
                ( { model
                    | food = { x = -1, y = -1 }
                    , snake = model.food :: newSnake
                    , score = newScore
                    , interval = newInterval
                  }
                , createFood
                )

            else
                ( { model | snake = newSnake, canMove = True }, Cmd.none )

        ChangeDirection direction ->
            if model.canMove && canChangeDirection direction model.direction then
                ( { model | direction = direction, canMove = False }, Cmd.none )

            else
                ( model, Cmd.none )

        CreateFood cell ->
            ( { model | food = cell }, Cmd.none )

        None ->
            ( model, Cmd.none )


getBackgroundColor : Bool -> String
getBackgroundColor occupied =
    if occupied == True then
        "#1b1e2b"

    else
        "#a6accd"


cellMatches : Int -> Cell -> Bool
cellMatches num cell =
    cell.x + cell.y * boardSize == num


isOccupied : Int -> Snake -> Cell -> Bool
isOccupied num snake food =
    cellMatches num food || List.any (cellMatches num) snake


block : Snake -> Cell -> Int -> Html Msg
block snake food num =
    div
        [ style "width" "20px"
        , style "height" "20px"
        , style "background-color" (getBackgroundColor (isOccupied num snake food))
        , id ("BLOCK-" ++ String.fromInt num)
        ]
        []


displayLost : Bool -> String
displayLost isDead =
    if isDead == True then
        "block"

    else
        "none"


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "padding-top" "24px"
        ]
        [ div
            [ style "display" "flex"
            , style "width" (String.fromInt boardWidth ++ "px")
            , style "flex-wrap" "wrap"
            ]
            (List.map (block model.snake model.food) (List.range 0 (boardSize * boardSize - 1)))
        , div []
            [ text ("score : " ++ String.fromInt model.score) ]
        , div
            [ style "color" "#d06178"
            , style "display" (displayLost model.dead)
            ]
            [ text "You Lost !" ]
        ]
