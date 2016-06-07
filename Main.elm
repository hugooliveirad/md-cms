import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import HttpBuilder exposing (..)
import Debug
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import Task exposing (..)

main =
  Html.program 
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

-- INIT

init : ( Model, Cmd Msg )
init =
    ( initModel
    , Cmd.none
    )

-- MODEL

type alias Model = 
  { login : Author
  , loggedIn : Bool
  , authors : Maybe Authors
  }

type alias Authors =
  List Author

type alias Author = 
  { name : String
  , nick : String
  , password : String
  }

initModel : Model
initModel = 
  { login = { name = "", nick = "", password = "" } 
  , loggedIn = False
  , authors = Just []
  }

-- UPDATE

type Msg
  = Name String
  | Nick String
  | Password String
  | Login
  | LoginSucceed (HttpBuilder.Response String)
  | LoginFail (HttpBuilder.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Name name ->
      let login = model.login
      in ({ model | login = { login | name = name } }, Cmd.none)
    Nick nick ->
      let login = model.login
      in ({ model | login = { login | nick = nick } }, Cmd.none)
    Password password ->
      let login = model.login
      in ({ model | login = { login | password = password } }, Cmd.none)
    Login ->
      (model, postLogin model.login)
    LoginSucceed _ ->
      ( { model | loggedIn = True
        , login = { name = "", nick = "", password = "" } 
        }, Cmd.none)
    LoginFail _ ->
      ({ model | loggedIn = True }, Cmd.none)

postLogin : Author -> Cmd Msg
postLogin author =
  Task.perform LoginFail LoginSucceed 
    (HttpBuilder.post "/api/login"
      |> withJsonBody (encodeAuthor author)
      |> withHeader "Content-Type" "application/json"
      |> send stringReader stringReader)

encodeAuthor : Author -> Encode.Value
encodeAuthor { name, nick, password } =
  Encode.object
    [ ("name", Encode.string name)
    , ("nick", Encode.string nick)
    , ("password", Encode.string password)]

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div [] 
      [ input [ type' "text", placeholder "Nick", onInput Nick ] []
      , input [ type' "password", placeholder "Senha", onInput Password ] [] 
      , button [ onClick Login ] [ text ("Enviar") ] ] 
    , div []
      [ text "authors" ]
    , div []
      [ text (if model.loggedIn == True then "logged in" else "logged out") ]]


