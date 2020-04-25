module Backend exposing (app)

import Character
import Dict
import Lamdera exposing (ClientId, SessionId)
import List.Extra as List
import Types exposing (..)
import World


app =
    Lamdera.backend
        { init = init
        , update = update
        , subscriptions = \m -> Sub.none
        , updateFromFrontend = updateFromFrontend
        }


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { accounts = []
      }
    , Cmd.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        BNoop ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        CreateAccount username passwordHash ->
            if List.any (\a -> a.username == username) model.accounts then
                ( model, Lamdera.sendToFrontend clientId UsernameAlreadyExists )

            else
              let
                  account =
                      { username = username
                      , passwordHash = passwordHash
                      , loggedIn = Just clientId
                      }

                  others =
                      model.accounts
                          |> List.filterMap
                              (\a ->
                                  if a.loggedIn == Nothing then
                                      Nothing

                                  else
                                      Just a.username
                              )

                  notifyOthers =
                      model.accounts
                          |> List.filterMap .loggedIn
                          |> List.map
                              (\id ->
                                  Lamdera.sendToFrontend id (OtherLoggedIn account.username)
                              )
              in
              ( { model
                  | accounts = account :: model.accounts
                }
              , Cmd.batch
                  (Lamdera.sendToFrontend clientId (LoggedIn account others) :: notifyOthers)
              )



        Login username passwordHash ->
            let
                match a =
                    a.username == username && a.passwordHash == passwordHash
            in
            case List.find match model.accounts of
                Just account ->
                    let
                        account_ =
                            { account | loggedIn = Just clientId }

                        others =
                            model.accounts
                                |> List.filterMap
                                    (\a ->
                                        if a.loggedIn == Nothing || a.username == account.username then
                                            Nothing

                                        else
                                            Just a.username
                                    )

                        notifyOthers =
                            model.accounts
                                |> List.filterMap .loggedIn
                                |> List.filter (\id -> id /= clientId)
                                |> List.map
                                    (\id ->
                                        Lamdera.sendToFrontend id (OtherLoggedIn account.username)
                                    )
                    in
                    ( { model | accounts = List.setIf match account_ model.accounts }
                    , Lamdera.sendToFrontend clientId (LoggedIn account_ others)
                    )

                Nothing ->
                    ( model, Lamdera.sendToFrontend clientId WrongUsernameOrPassword )


        CheckName username ->
            let
                exists =
                    List.any (\a -> a.username == username) model.accounts
            in
            ( model, Lamdera.sendToFrontend clientId (CheckNameResponse exists) )



        SendMessage message ->
            ( model
            , case List.find (\a -> a.loggedIn == Just clientId) model.accounts of
                Just sender ->
                    model.accounts
                        |> List.filterMap .loggedIn
                        |> List.map
                            (\id ->
                                Lamdera.sendToFrontend id
                                    (GotMessage
                                        { username = sender.username
                                        , message = message
                                        }
                                    )
                            )
                        |> Cmd.batch

                Nothing ->
                    Cmd.none
            )
