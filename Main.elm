import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Http
import Debug
import Json.Decode as Json exposing ((:=))
import Json.Encode as JSEncode
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
  , authors = Just []
  }

-- UPDATE

type Msg
  = Name String
  | Nick String
  | Password String
  | Login
  | LoginSucceed String
  | LoginFail Http.Error

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
      (model, Cmd.none)
    LoginFail _ ->
      (model, Cmd.none)

postLogin : Author -> Cmd Msg
postLogin author =
  Task.perform LoginFail LoginSucceed 
    (Http.post Json.string "/api/login" (Http.string (encodeAuthor author)))

encodeAuthor : Author -> String
encodeAuthor { name, nick, password } =
  JSEncode.encode 0 <|
    JSEncode.object
      [ ("name", JSEncode.string name)
      , ("nick", JSEncode.string nick)
      , ("password", JSEncode.string password)]

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div [] 
      [ input [ type' "text", placeholder "Nick", onInput Nick ] []
      , input [ type' "password", placeholder "Senha", onInput Password ] [] 
      , button [ onClick Login ] [ text ("Enviar") ] ] 
    , div []
      [ text "authors" ] ]


