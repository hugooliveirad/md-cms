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
    , getAuthors
    )

-- MODEL

type alias Model = 
  { login : Author
  , loggedIn : Bool
  , authors : Authors
  , newAuthor : Author
  }

type alias Authors =
  List Author

type alias Author = 
  { name : String
  , nick : String
  , password : String
  , id : Maybe Int
  }

initModel : Model
initModel = 
  { login = emptyAuthor
  , loggedIn = False
  , authors = []
  , newAuthor = emptyAuthor
  }

emptyAuthor : Author
emptyAuthor =
  { name = "", nick = "", password = "", id = Nothing }

-- UPDATE

type Msg
  = Name String
  | Nick String
  | Password String

  | Login
  | LoginSucceed (HttpBuilder.Response String)
  | LoginFail (HttpBuilder.Error String)

  | GetAuthors
  | GetAuthorsSucceed (HttpBuilder.Response (List Author))
  | GetAuthorsFail (HttpBuilder.Error String)

  | NewName String
  | NewNick String
  | NewPassword String

  | NewAuthor
  | NewAuthorSucceed (HttpBuilder.Response Author)
  | NewAuthorFail (HttpBuilder.Error String)

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
        , login = { name = "", nick = "", password = "", id = Nothing } 
        }, getAuthors )
    LoginFail _ ->
      ({ model | loggedIn = True }, Cmd.none)

    GetAuthors ->
      ( model, getAuthors )
    GetAuthorsSucceed resp ->
      ( { model | authors = resp.data }, Cmd.none )
    GetAuthorsFail _ ->
      ( model, Cmd.none )

    NewName name ->
      let newAuthor = model.newAuthor
      in ( { model | newAuthor = { newAuthor | name = name } }, Cmd.none )
    NewNick nick ->
      let newAuthor = model.newAuthor
      in ( { model | newAuthor = { newAuthor | nick = nick } }, Cmd.none )
    NewPassword password ->
      let newAuthor = model.newAuthor
      in ( { model | newAuthor = { newAuthor | password = password } }, Cmd.none )
    NewAuthor ->
      ( model, postAuthor model.newAuthor )
    NewAuthorSucceed resp ->
      ( { model | newAuthor = emptyAuthor}, getAuthors )
    NewAuthorFail _ ->
      ( model, Cmd.none )


postLogin : Author -> Cmd Msg
postLogin author =
  Task.perform LoginFail LoginSucceed 
    (HttpBuilder.post "/api/login"
      |> withJsonBody (encodeAuthor author)
      |> withHeader "Content-Type" "application/json"
      |> send stringReader stringReader)

getAuthors : Cmd Msg
getAuthors =
  Task.perform GetAuthorsFail GetAuthorsSucceed
    (HttpBuilder.get "/api/authors"
      |> withHeader "Content-Type" "application/json"
      |> send (jsonReader decodeAuthors) stringReader)

postAuthor : Author -> Cmd Msg
postAuthor author =
  Task.perform NewAuthorFail NewAuthorSucceed
    (HttpBuilder.post "/api/authors"
      |> withHeader "Content-Type" "application/json"
      |> withJsonBody (encodeAuthor author)
      |> send (jsonReader decodeAuthor) stringReader)

decodeAuthors : Decode.Decoder (List Author)
decodeAuthors =
  Decode.list decodeAuthor

decodeAuthor : Decode.Decoder Author
decodeAuthor =
  Decode.object4 Author
    ("name" := Decode.string)
    ("nick" := Decode.string)
    ("password" := Decode.string) 
    (Decode.maybe ("id" := Decode.int))

encodeAuthor : Author -> Encode.Value
encodeAuthor { name, nick, password, id } =
  let 
      eid = case id of
        Nothing -> Encode.null
        Just i -> Encode.int i
  in
      Encode.object
        [ ("name", Encode.string name)
        , ("nick", Encode.string nick)
        , ("password", Encode.string password)
        , ("id", eid )]

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div []
      [ ul []
        (List.map (viewAuthor) model.authors)]
    , viewNewAuthor ]

viewLogin : Author -> Html Msg
viewLogin login =
  div [] 
    [ input [ type' "text", placeholder "Nick", value login.nick, onInput Nick ] []
    , input [ type' "password", placeholder "Senha", value login.password, onInput Password ] [] 
    , button [ onClick Login ] [ text ("Enviar") ] ]

viewAuthor : Author -> Html Msg
viewAuthor author =
  li []
    [ span [] 
      [ (text author.name)
      , (text (" (" ++ author.nick ++ ")"))] ]

viewNewAuthor : Html Msg
viewNewAuthor =
  div []
    [ input [ type' "text", placeholder "Nick", onInput NewNick ] []
    , input [ type' "text", placeholder "Nome", onInput NewName ] []
    , input [ type' "password", placeholder "Senha", onInput NewPassword ] []
    , button [ onClick NewAuthor ] [ text ("Criar") ] ]
