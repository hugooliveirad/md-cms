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
    initModel ! [getAuthors, getPosts, getCollections]

-- MODEL

type alias Model = 
  { authors : Authors
  , newAuthor : Author
  , posts : Posts
  , newPost : Post
  , collections : Collections
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

type alias Collections =
  List Collection

type alias Collection =
  { name : String
  , authorId : Int
  , id : Maybe Int
  }

type alias Tags =
  List Tag

type alias Tag =
  { name : String
  , authorId : Int
  , id : Maybe Int
  }

type alias TagPost =
  { tagId : Int
  , postId : Int
  , id : Maybe Int
  }

type alias CollectionPost =
  { collectionId : Int
  , postId : Int
  , id : Maybe Int
  }

initModel : Model
initModel = 
  { authors = []
  , newAuthor = emptyAuthor
  , posts = []
  , newPost = emptyPost
  , collections = []
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

  | NewPostTitle String
  | NewPostContent String
  | NewPostAuthorId String

  | NewPost
  | NewPostSucceed (HttpBuilder.Response Post)
  | NewPostFail (HttpBuilder.Error String)

  | GetCollections
  | GetCollectionsSucceed (HttpBuilder.Response Collections)
  | GetCollectionsFail (HttpBuilder.Error String)

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

    NewPostTitle title ->
      let newPost = model.newPost
      in ( { model | newPost = { newPost | title = title } }, Cmd.none )
    NewPostContent content ->
      let newPost = model.newPost
      in ( { model | newPost = { newPost | content = content } }, Cmd.none )
    NewPostAuthorId strId ->
      let 
          newPost = model.newPost
          id = case String.toInt strId of
            Ok id -> id
            Err _ -> 0
      in 
          ( { model | newPost = { newPost | authorId = id } }, Cmd.none )

    NewPost ->
      ( model, postPost model.newPost )
    NewPostSucceed resp ->
      ( { model | newPost = emptyPost }, getPosts )
    NewPostFail _ ->
      ( model, Cmd.none )

    GetCollections ->
      ( model, getCollections )
    GetCollectionsSucceed resp ->
      ( { model | collections = resp.data }, Cmd.none )
    GetCollectionsFail _ ->
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

postPost : Post -> Cmd Msg
postPost post =
  Task.perform NewPostFail NewPostSucceed (postPostTask post)

postPostTask : Post -> Task (HttpBuilder.Error String) (HttpBuilder.Response Post)
postPostTask post =
  HttpBuilder.post "/api/posts"
    |> withHeader "Content-Type" "application/json"
    |> withJsonBody (encodePost post)
    |> send (jsonReader decodePost) stringReader

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

encodePost : Post -> Encode.Value
encodePost post =
  let 
      pid = case post.id of
        Nothing -> Encode.null
        Just i -> Encode.int i
  in
      Encode.object
        [ ("title", Encode.string post.title)
        , ("content", Encode.string post.content)
        , ("authorId", Encode.int post.authorId)
        , ("id", pid)]

getCollections : Cmd Msg
getCollections =
  Task.perform GetCollectionsFail GetCollectionsSucceed getCollectionsTask

getCollectionsTask : Task (HttpBuilder.Error String) (HttpBuilder.Response Collections)
getCollectionsTask =
  HttpBuilder.get "/api/collections"
    |> withHeader "Content-Type" "application/json"
    |> send (jsonReader decodeCollections) stringReader

decodeCollections : Decode.Decoder Collections
decodeCollections =
  Decode.list decodeCollection

decodeCollection : Decode.Decoder Collection
decodeCollection =
  Decode.object3 Collection
    ("name" := Decode.string)
    ("authorId" := Decode.int)
    (Decode.maybe ("id" := Decode.int))

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ viewAuthors model.authors
    , viewNewAuthor model.newAuthor
    , viewCollections model.collections
    , viewPosts model.posts
    , viewNewPost model.newPost
    ]

viewAuthors : Authors -> Html Msg
viewAuthors authors =
  ul []
    (List.map (viewAuthor) authors)

viewAuthor : Author -> Html Msg
viewAuthor author =
  li []
    [ span [] 
      [ (text author.name)
      , (text (" (" ++ author.nick ++ ")"))] ]

viewNewAuthor : Author -> Html Msg
viewNewAuthor { nick, name, password } =
  div [ class "form" ]
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
    [ div []
      [ h1 [] [ text post.title ] 
      , div [] [ text post.content ] ] ]

viewNewPost : Post -> Html Msg
viewNewPost post =
  div [ class "form" ]
    [ input [ type' "text", placeholder "Título", value post.title, onInput NewPostTitle ] []
    , textarea [ placeholder "Conteúdo", value post.content, onInput NewPostContent ] []
    , input [ type' "number", value (toString post.authorId), onInput NewPostAuthorId ] []
    , button [ onClick NewPost ] [ text ("Publicar") ] ]

viewCollections : Collections -> Html Msg
viewCollections collections =
  ul []
    (List.map viewCollection collections)

viewCollection : Collection -> Html Msg
viewCollection collection =
  li []
    [ text collection.name ]

