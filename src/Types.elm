module Types exposing (..)

import Character exposing (Character)
import Dict exposing (Dict)
import Hash exposing (Hash)
import Lamdera exposing (ClientId)
import Playground
import World exposing (Chunk)


devMode =
    False


type alias FrontendModel =
    { page : Page
    }


type Page
    = StartPage
    | LoginPage LoginModel
    | RegisterPage RegisterModel
    | ConferencePage ConferenceModel


type alias Memory =
    { player : Character
    , others : List String
    , chunks : Dict ( Int, Int ) (Request Chunk)
    , messages : List ( Int, Message )
    , chatInput : Maybe String
    , messageI : Int
    }


type alias Message =
    { username : String, message : String }


type Request a
    = Pending
    | Received a


type alias BackendModel =
    { accounts : List Account
    }


type alias Account =
    { username : String
    , passwordHash : Hash
    , loggedIn : Maybe ClientId
    }


type FrontendMsg
    = LoginMsg LoginMsg
    | RegisterMsg RegisterMsg
    | ConferenceMsg ConferenceMsg
    | GotoLogin
    | GotoRegister
    | KeyDown String
    | ChatInput String
    | ChatSubmit
    | RemoveMessage Int
    | Noop


type ToBackend
    = CheckName String
    | CreateAccount String Hash
    | Login String Hash
    | SendMessage String


type BackendMsg
    = BNoop


type ToFrontend
    = LoggedIn Account (List String)
    | OtherLoggedIn String
    | CheckNameResponse Bool
    | WrongUsernameOrPassword
    | UsernameAlreadyExists
    | UpdateOtherPlayer String
    | GotMessage Message
    | ChunkResponse Int Int Chunk


type alias RegisterModel =
    { username : String
    , password : String
    , password2 : String
    , failed : Bool
    , blurred : Bool
    }


type RegisterMsg
    = InputUsername String
    | InputPassword String
    | InputPassword2 String
    | Blurred
    | Next
    | Register


type alias LoginModel =
    { username : String
    , password : String
    , failed : Bool
    }


type LoginMsg
    = LoginUsername String
    | LoginPassword String
    | Submit

type alias ConferenceModel =
  { foo : String }

type ConferenceMsg
  = FooBar
