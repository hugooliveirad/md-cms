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
  initModel ! 
    [ getAuthors
    , getPosts
    , getCollections
    , getTags
    ]

-- MODEL

type alias Model = 
  { authors : Authors
  , newAuthor : Author
  , posts : Posts
  , newPost : Post
  , collections : Collections
  , newCollection : Collection
  , tags : Tags
  , newTag : Tag
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
  , newCollection = emptyCollection
  , tags = []
  , newTag = emptyTag
  }

emptyAuthor : Author
emptyAuthor =
  { name = "", nick = "", password = "", id = Nothing }

emptyPost : Post
emptyPost =
  { title = "", content = "", authorId = 0, id = Nothing }

emptyCollection : Collection
emptyCollection =
  { name = "", authorId = 0, id = Nothing }

emptyTag : Tag
emptyTag =
  { name = "", authorId = 0, id = Nothing }

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

  | NewCollectionName String
  | NewCollectionAuthorId String

  | NewCollection
  | NewCollectionSucceed (HttpBuilder.Response Collection)
  | NewCollectionFail (HttpBuilder.Error String)

  | GetTags
  | GetTagsSucceed (HttpBuilder.Response Tags)
  | GetTagsFail (HttpBuilder.Error String)

  | NewTagName String
  | NewTagAuthorId String

  | NewTag
  | NewTagSucceed (HttpBuilder.Response Tag)
  | NewTagFail (HttpBuilder.Error String)

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
          id = resultWithDefault 0 (String.toInt strId)
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

    NewCollectionName name ->
      let newCollection = model.newCollection
      in ( { model | newCollection = { newCollection | name = name } }, Cmd.none )
    NewCollectionAuthorId strId ->
      let 
          newCollection = model.newCollection
          id = resultWithDefault 0 (String.toInt strId)
      in 
          ( { model | newCollection = { newCollection | authorId = id } }, Cmd.none )

    NewCollection ->
      ( model, postCollection model.newCollection )
    NewCollectionSucceed resp ->
      ( { model | newCollection = emptyCollection }, getCollections )
    NewCollectionFail _ ->
      ( model, Cmd.none )

    GetTags ->
      ( model, getTags )
    GetTagsSucceed resp ->
      ( { model | tags = resp.data }, Cmd.none )
    GetTagsFail _ ->
      ( model, Cmd.none )

    NewTagName name ->
      let newTag = model.newTag
      in ( { model | newTag = { newTag | name = name } }, Cmd.none )
    NewTagAuthorId strId ->
      let 
          newTag = model.newTag
          id = resultWithDefault 0 (String.toInt strId)
      in 
          ( { model | newTag = { newTag | authorId = id } }, Cmd.none )

    NewTag ->
      ( model, postTag model.newTag )
    NewTagSucceed resp ->
      ( { model | newTag = emptyTag }, getTags )
    NewTagFail _ ->
      ( model, Cmd.none )

resultWithDefault : a -> Result b a -> a
resultWithDefault def res =
  case res of
    Ok a -> a
    Err _ -> def

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

postCollection : Collection -> Cmd Msg
postCollection collection =
  Task.perform NewCollectionFail NewCollectionSucceed (postCollectionTask collection)

postCollectionTask : Collection -> Task (HttpBuilder.Error String) (HttpBuilder.Response Collection)
postCollectionTask collection =
  HttpBuilder.post "/api/collections"
    |> withHeader "Content-Type" "application/json"
    |> withJsonBody (encodeCollection collection)
    |> send (jsonReader decodeCollection) stringReader

decodeCollections : Decode.Decoder Collections
decodeCollections =
  Decode.list decodeCollection

decodeCollection : Decode.Decoder Collection
decodeCollection =
  Decode.object3 Collection
    ("name" := Decode.string)
    ("authorId" := Decode.int)
    (Decode.maybe ("id" := Decode.int))

encodeCollection : Collection -> Encode.Value
encodeCollection collection =
  let 
      cid = case collection.id of
        Nothing -> Encode.null
        Just i -> Encode.int i
  in
      Encode.object
        [ ("name", Encode.string collection.name)
        , ("authorId", Encode.int collection.authorId)
        , ("id", cid)]


----

getTags : Cmd Msg
getTags =
  Task.perform GetTagsFail GetTagsSucceed getTagsTask

getTagsTask : Task (HttpBuilder.Error String) (HttpBuilder.Response Tags)
getTagsTask =
  HttpBuilder.get "/api/tags"
    |> withHeader "Content-Type" "application/json"
    |> send (jsonReader decodeTags) stringReader

postTag : Tag -> Cmd Msg
postTag tag =
  Task.perform NewTagFail NewTagSucceed (postTagTask tag)

postTagTask : Collection -> Task (HttpBuilder.Error String) (HttpBuilder.Response Tag)
postTagTask tag =
  HttpBuilder.post "/api/tags"
    |> withHeader "Content-Type" "application/json"
    |> withJsonBody (encodeTag tag)
    |> send (jsonReader decodeTag) stringReader

decodeTags : Decode.Decoder Tags
decodeTags =
  Decode.list decodeTag

decodeTag : Decode.Decoder Tag
decodeTag =
  Decode.object3 Tag
    ("name" := Decode.string)
    ("authorId" := Decode.int)
    (Decode.maybe ("id" := Decode.int))

encodeTag : Tag -> Encode.Value
encodeTag tag =
  let 
      tid = case tag.id of
        Nothing -> Encode.null
        Just i -> Encode.int i
  in
      Encode.object
        [ ("name", Encode.string tag.name)
        , ("authorId", Encode.int tag.authorId)
        , ("id", tid)]

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ header [ class "header" ]
      [ h1 [ class "title" ] [ text "md-cms" ] ]
    , div [ class "main" ]
      [ content 
        [ viewPosts model.posts
        , viewNewPost model.newPost
        ]
      , sidebar 
        [ sidebarSection
          [viewAuthors model.authors
          , viewNewAuthor model.newAuthor
          ]
        , sidebarSection
          [ viewCollections model.collections
          , viewNewCollection model.newCollection
          ]
        , sidebarSection
          [ viewTags model.tags
          , viewNewTag model.newTag]
          ] 
        ]
      ]

content : List (Html Msg) -> Html Msg
content children =
  div [ class "content" ] children

sidebar : List (Html Msg) -> Html Msg
sidebar children =
  aside [ class "sidebar" ] children

sidebarSection : List (Html Msg) -> Html Msg
sidebarSection children =
  section [ class "sidebar-section" ] children

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
    [ article []
      [ h2 [] [ text post.title ] 
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

viewNewCollection : Collection -> Html Msg
viewNewCollection collection =
  div [ class "form" ]
    [ input [ type' "text", placeholder "Nome", value collection.name, onInput NewCollectionName ] []
    , input [ type' "number", value (toString collection.authorId), onInput NewCollectionAuthorId] []
    , button [ onClick NewCollection ] [ text ("Criar") ]]

viewTags : Tags -> Html Msg
viewTags tags =
  ul []
    (List.map viewTag tags)

viewTag : Tag -> Html Msg
viewTag tag =
  li []
    [ text tag.name ]

viewNewTag : Tag -> Html Msg
viewNewTag tag =
  div [ class "form" ]
    [ input [ type' "text", placeholder "Nome", value tag.name, onInput NewTagName ] []
    , input [ type' "number", value (toString tag.authorId), onInput NewTagAuthorId] []
    , button [ onClick NewTag ] [ text ("Criar") ]]

