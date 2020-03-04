module GamePage exposing (..)

import Character exposing (Character, Direction(..))
import Dict exposing (Dict)
import Playground exposing (Computer, Shape)
import Playground.Advanced as Playground
import Types exposing (..)
import World


init : Account -> Dict String Character -> ( Playground.Game Memory, Cmd Playground.Msg )
init account others =
    game.init
        |> Tuple.mapFirst
            (Playground.edit
                (\_ _ ->
                    { player = account.character
                    , others = others
                    }
                )
            )


game =
    Playground.embed render
        updateGame
        { player =
            { coords = ( 0, 0 )
            , direction = Down
            , moving = False
            , variant = 1
            }
        , others = Dict.empty
        }


render : Computer -> Memory -> List Shape
render computer { player, others } =
    World.render
        ++ ((player :: Dict.values others)
                |> List.sortBy (.coords >> Tuple.second >> negate)
                |> List.map (Character.render computer.time)
           )
        |> List.map
            ((if True || computer.keyboard.space then
                identity

              else
                Playground.scale 3
             )
                >> Playground.move (negate (Tuple.first player.coords)) (negate (Tuple.second player.coords))
            )


updateGame : Computer -> Memory -> Memory
updateGame computer memory =
    { memory | player = Character.update computer memory.player }