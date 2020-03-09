module Character exposing (..)

import Playground exposing (Keyboard)
import Playground.Extra as Playground
import Set


type alias Character =
    { coords : ( Float, Float )
    , direction : Direction
    , moving : Bool
    , skin : Int
    }


create : Int -> Maybe Character
create skin =
    if List.member skin skinList then
        Just { coords = ( 0, 0 ), direction = Down, moving = False, skin = skin }

    else
        Nothing


skinList : List Int
skinList =
    List.range 1 40


type Direction
    = Up
    | Down
    | Left
    | Right


update { keyboard, time } character =
    let
        ( x, y ) =
            character.coords

        ( vx, vy ) =
            toXY keyboard

        d =
            Playground.delta time |> toFloat |> clamp 0 60
    in
    { character
        | coords =
            ( x + (vx * speed * d)
            , y + (vy * speed * d)
            )
        , direction =
            if vy > 0 then
                Up

            else if vy < 0 then
                Down

            else if vx < 0 then
                Left

            else if vx > 0 then
                Right

            else
                character.direction
        , moving = toXY keyboard /= ( 0, 0 )
    }


url : Int -> String
url skin =
    "/characters/" ++ String.fromInt skin ++ ".png"


tile : Int -> Int -> Playground.Shape
tile skin =
    Playground.tile 26 36 (url skin)


render : Playground.Time -> Character -> Playground.Shape
render time char =
    let
        mod =
            Playground.now time |> modBy 500

        row =
            case char.direction of
                Down ->
                    0

                Left ->
                    3

                Right ->
                    6

                Up ->
                    9

        frame =
            if not char.moving then
                row + 1

            else if mod < 125 then
                row

            else if mod < 250 then
                row + 1

            else if mod < 375 then
                row + 2

            else
                row + 1
    in
    tile char.skin frame
        |> Playground.move (Tuple.first char.coords) (Tuple.second char.coords)


speed =
    0.2


toXY : Keyboard -> ( Float, Float )
toXY keyboard =
    let
        x =
            (if keyboard.right || Set.member "d" keyboard.keys then
                1

             else
                0
            )
                - (if keyboard.left || Set.member "a" keyboard.keys then
                    1

                   else
                    0
                  )

        y =
            (if keyboard.up || Set.member "w" keyboard.keys then
                1

             else
                0
            )
                - (if keyboard.down || Set.member "s" keyboard.keys then
                    1

                   else
                    0
                  )
    in
    if x /= 0 && y /= 0 then
        ( x / squareRootOfTwo, y / squareRootOfTwo )

    else
        ( x, y )


squareRootOfTwo : Float
squareRootOfTwo =
    sqrt 2
