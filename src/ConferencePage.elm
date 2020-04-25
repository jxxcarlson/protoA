module ConferencePage exposing (..)

import Hash
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing(Html)
import Lamdera
import Types exposing (..)


init : Account -> List String -> (ConferenceModel, Cmd ConferenceMsg)
init account others =
    ({ foo = account.username ++ "; others = " ++ String.join ", " others}, Cmd.none)


view : ConferenceModel -> Html FrontendMsg
view model =
    Element.layout [] (el [] (text ("Conference Room: " ++ model.foo)))
