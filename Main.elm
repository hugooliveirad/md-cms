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
    initModel ! [getAuthors, getPosts]

-- MODEL

type alias Model = 
  { authors : Authors
  , newAuthor : Author
  , posts : Posts
  , newPost : Post
  }

type alias Authors =
  List Author

type alias Author = 
  { name : String
  , nick : String
  , password : String
  , id : Maybe Int
  }

type alias Posts =
  List Post

type alias Post =
  { title : String
  , content : String
  , authorId : Int
  , id : Maybe Int
  }

initModel : Model
initModel = 
  { authors = []
  , newAuthor = emptyAuthor
  , posts = []
  , newPost = emptyPost
  }

emptyAuthor : Author
emptyAuthor =
  { name = "", nick = "", password = "", id = Nothing }

emptyPost : Post
emptyPost =
  { title = "", content = "", authorId = 0, id = Nothing }

-- UPDATE

type Msg
  = GetAuthors
  | GetAuthorsSucceed (HttpBuilder.Response (List Author))
  | GetAuthorsFail (HttpBuilder.Error String)

  | NewName String
  | NewNick String
  | NewPassword String

  | NewAuthor
  | NewAuthorSucceed (HttpBuilder.Response Author)
  | NewAuthorFail (HttpBuilder.Error String)

  | GetPosts
  | GetPostsSucceed (HttpBuilder.Response Posts)
  | GetPostsFail (HttpBuilder.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of

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

    GetPosts ->
      ( model, getPosts )
    GetPostsSucceed resp ->
      ( { model | posts = resp.data }, Cmd.none )
    GetPostsFail _ ->
      ( model, Cmd.none )

getAuthors : Cmd Msg
getAuthors =
  Task.perform GetAuthorsFail GetAuthorsSucceed getAuthorsTask

getAuthorsTask : Task (HttpBuilder.Error String) (HttpBuilder.Response Authors)
getAuthorsTask =
  HttpBuilder.get "/api/authors"
    |> withHeader "Content-Type" "application/json"
    |> send (jsonReader decodeAuthors) stringReader 

postAuthor : Author -> Cmd Msg
postAuthor author =
  Task.perform NewAuthorFail NewAuthorSucceed <| postAuthorTask author

postAuthorTask : Author -> Task (HttpBuilder.Error String) (HttpBuilder.Response Author)
postAuthorTask author =
  HttpBuilder.post "/api/authors"
    |> withHeader "Content-Type" "application/json"
    |> withJsonBody (encodeAuthor author)
    |> send (jsonReader decodeAuthor) stringReader

decodeAuthors : Decode.Decoder Authors
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
        , ("id", eid)]

getPosts : Cmd Msg
getPosts =
  Task.perform GetPostsFail GetPostsSucceed getPostsTask

getPostsTask : Task (HttpBuilder.Error String) (HttpBuilder.Response Posts)
getPostsTask =
  HttpBuilder.get "/api/posts"
    |> withHeader "Content-Type" "application/json"
    |> send (jsonReader decodePosts) stringReader

decodePosts : Decode.Decoder Posts
decodePosts =
  Decode.list decodePost

decodePost : Decode.Decoder Post
decodePost =
  Decode.object4 Post
    ("title" := Decode.string)
    ("content" := Decode.string)
    ("authorId" := Decode.int)
    (Decode.maybe ("id" := Decode.int))

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div []
      [ ul []
        (List.map (viewAuthor) model.authors)]
    , viewNewAuthor model.newAuthor
    , viewPosts model.posts
    ]

viewAuthor : Author -> Html Msg
viewAuthor author =
  li []
    [ span [] 
      [ (text author.name)
      , (text (" (" ++ author.nick ++ ")"))] ]

viewNewAuthor : Author -> Html Msg
viewNewAuthor { nick, name, password } =
  div []
    [ input [ type' "text", placeholder "Nick", value nick, onInput NewNick ] []
    , input [ type' "text", placeholder "Nome", value name, onInput NewName ] []
    , input [ type' "password", placeholder "Senha", value password, onInput NewPassword ] []
    , button [ onClick NewAuthor ] [ text ("Criar") ] ]

viewPosts : Posts -> Html Msg
viewPosts posts =
  ul []
    (List.map (viewPost True) posts)

viewPost : Bool -> Post -> Html Msg
viewPost preview post =
  li []
    [ text post.title ]

