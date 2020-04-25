module Frontend exposing (app, init)

import Browser.Dom
import Browser.Events
import Character
import Css
import Dict
import Element
import Hash
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Keyed
import Html.Lazy exposing (..)
import Json.Decode as Decode
import Lamdera
import LoginPage
import Misc exposing (none)
import Playground.Advanced as Playground
import Process
import RegisterPage
import ConferencePage
import Task
import Types exposing (..)


{-| Lamdera applications define 'app' instead of 'main'.

Lamdera.frontend is the same as Browser.application with the
additional update function; updateFromBackend.

-}
app =
    Lamdera.frontend
        { init =
            \_ _ ->
                if devMode then
                    devInit

                else
                    init
        , update = update
        , updateFromBackend = updateFromBackend
        , view =
            \model ->
                { title = "Game"
                , body = [ view model ]
                }
        , subscriptions = subscriptions
        , onUrlChange = \_ -> Noop
        , onUrlRequest = \_ -> Noop
        }


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions model =
    Sub.batch
        [  Browser.Events.onKeyDown (Decode.field "code" Decode.string |> Decode.map KeyDown)
        ]


init : ( FrontendModel, Cmd FrontendMsg )
init =
    ( { page = StartPage }
    , Cmd.none
    )


devInit : ( FrontendModel, Cmd FrontendMsg )
devInit =
    case Character.create 5 of
        Just char ->
            updateFromBackend
                (LoggedIn
                    { username = ""
                    , loggedIn = Nothing
                    , passwordHash = Hash.fromString ""
                    }
                    []
                )
                { page = StartPage }

        Nothing ->
            ( { page = StartPage }
            , Cmd.none
            )


{-| This is the normal frontend update function. It handles all messages that can occur on the frontend.
-}
update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case ( msg, model.page ) of
        ( LoginMsg submsg, LoginPage submodel ) ->
            LoginPage.update submsg submodel
                |> with LoginMsg LoginPage model

        ( RegisterMsg submsg, RegisterPage submodel ) ->
            RegisterPage.update submsg submodel
                |> with RegisterMsg RegisterPage model

        ( GotoLogin, _ ) ->
            ( { model | page = LoginPage LoginPage.init }, Cmd.none )

        ( GotoRegister, _ ) ->
            ( { model | page = RegisterPage RegisterPage.init }, Cmd.none )

        _ ->
            ( model, Cmd.none )




with msg page model =
    Tuple.mapBoth (\m -> { model | page = page m })
        (Cmd.map msg)


{-| This is the added update function. It handles all messages that can arrive from the backend.
-}
updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case ( msg, model.page ) of
        ( LoggedIn account others, _ ) ->
          ConferencePage.init account others
                |> with ConferenceMsg ConferencePage model

        ( WrongUsernameOrPassword, LoginPage loginModel ) ->
            ( { page = LoginPage { loginModel | failed = True } }, Cmd.none )

        ( CheckNameResponse exists, RegisterPage registerModel ) ->
            if exists then
                ( { page = RegisterPage { registerModel | status = RegistrationPending } }, Cmd.none )

            else
                ( { page = RegisterPage { registerModel | status = RegistrationCompleted, completeRegistration = True } }, Cmd.none )

        ( UsernameAlreadyExists, RegisterPage registerModel ) ->
            ( { page = RegisterPage { registerModel | status = RegistrationPending  } }, Cmd.none )



        _ ->
            ( model, Cmd.none )


view : FrontendModel -> Html FrontendMsg
view model =
    div [ class "main" ]
        [ Html.node "style" [] [ text Css.css ]
        , case model.page of
            LoginPage loginmodel ->
                LoginPage.view loginmodel |> Html.map LoginMsg

            RegisterPage regmodel ->
                RegisterPage.view regmodel |> Html.map RegisterMsg

            ConferencePage conferencemodel ->
                ConferencePage.view conferencemodel--  |> Element.map ConferenceMsg



            StartPage ->
                startView
        ]


viewCoords coords =
    div [ class "coords" ]
        [ div [] [ text ("x: " ++ (Tuple.first >> round >> String.fromInt) coords) ]
        , div [] [ text ("y: " ++ (Tuple.second >> round >> String.fromInt) coords) ]
        ]


chat messages chatInput =
    div [ class "chat" ]
        [ messages
            |> List.reverse
            |> List.map
                (\( i, m ) ->
                    ( String.fromInt i
                    , div [ class "message" ]
                        [ div [ class "avatar", style "background-image" ("url(" ++ Character.url m.skin ++ ")") ] []
                        , div []
                            [ div [ class "username" ] [ text m.username ]
                            , div [] [ text m.message ]
                            ]
                        ]
                    )
                )
            |> Html.Keyed.node "div" []
        , case chatInput of
            Just message ->
                input [ value message, onInput ChatInput, id "chatInput" ] []

            Nothing ->
                div [ class "chatHint" ] [ text "Press enter to chat" ]
        ]


startView =
    div [ class "startPage" ]
        [ button [ onClick GotoLogin, class "big" ] [ text "Log in" ]
        , button [ onClick GotoRegister, class "big" ] [ text "Register" ]
        ]
