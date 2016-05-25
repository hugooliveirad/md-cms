{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Data.Time
import Control.Monad.Logger (runStdoutLoggingT)

data Page = Page{connPool :: ConnectionPool}

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Author json
  name Text
  nick Text
  password Text
  deriving Show

Post json
  title Text
  content Text
  publishDate UTCTime
  authorId Text
  deriving Show

Tag json
  name Text
  authorId AuthorId
  deriving Show

Collection json
  name Text
  authorId AuthorId
  deriving Show

PostTag json
  postId PostId
  authorId AuthorId
  deriving Show

PostCollection json
  postId PostId
  collectionId CollectionId
  deriving Show
|]

-- como declarar querystrings?
-- ex: /api/posts?author_id=2
mkYesod "Page" [parseRoutes|
/ HomeR GET
/login LoginR GET POST
/logout LogoutR GET
/error ErrorR GET

/api/authors AuthorsR GET POST
/api/authors/#AuthorID AuthorR GET PUT DELETE

/api/posts PostsR GET POST
/api/posts/#PostId PostR GET PUT DELETE

/api/tags TagsR GET POST
/api/tags/#TagId TagR GET PUT DELETE

/api/collections CollectionsR GET POST
/api/collections/#CollectionId CollectionR GET PUT DELETE
|]

instance Yesod Page where
  authRoute _ = Just LoginR
  isAuthorized HomeR _ = return Authorized
  isAuthorized LoginR _ = return Authorized
  isAuthorized ErrorR _ = return Authorized
  isAuthorized _ _ = return AuthenticationRequired

instance YesodPersist Page where
  type YesodPersistBackend Page = SqlBackend
  runDB f = do
    master <- getYesod
    let pool = connPool master
    runSqlPool f pool



-- OLD CODE FOR REF

isUser = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized

isAdmin = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized
        Just _ -> Unauthorized "Voce precisa ser admin para entrar aqui"

instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Pagina FormMessage where
    renderMessage _ _ = defaultFormMessage

formUser :: Form Users
formUser = renderDivs $ Users <$>
           areq textField "Nome: " Nothing <*>
           areq textField "Login: " Nothing <*>
           areq passwordField "Senha: " Nothing

formLogin :: Form (Text,Text)
formLogin = renderDivs $ (,) <$>
           areq textField "Login: " Nothing <*>
           areq passwordField "Senha: " Nothing

getUsuarioR :: Handler Html
getUsuarioR = do
           (widget, enctype) <- generateFormPost formUser
           defaultLayout [whamlet|
                 <form method=post enctype=#{enctype} action=@{UsuarioR}>
                     ^{widget}
                     <input type="submit" value="Enviar">
           |]

getPerfilR :: UsersId -> Handler Html
getPerfilR uid = do
      user <- runDB $ get404 uid
      defaultLayout [whamlet|
          <p><b> Pagina de #{usersNome user}
          <p><b> Login: #{usersLogin user}
      |]

postUsuarioR :: Handler Html
postUsuarioR = do
           ((result, _), _) <- runFormPost formUser
           case result of
               FormSuccess user -> (runDB $ insert user) >>= \piid -> redirect (PerfilR piid)
               _ -> redirect ErroR

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

getAdminR :: Handler Html
getAdminR = defaultLayout [whamlet|Hello World!|]

getLoginR :: Handler Html
getLoginR = do
           (widget, enctype) <- generateFormPost formLogin
           defaultLayout [whamlet|
                 <form method=post enctype=#{enctype} action=@{LoginR}>
                     ^{widget}
                     <input type="submit" value="Login">
           |]

postLoginR :: Handler Html
postLoginR = do
           ((result, _), _) <- runFormPost formLogin
           case result of
               FormSuccess ("admin","admin") -> setSession "_ID" "admin" >> redirect AdminR
               FormSuccess (login,senha) -> do
                   user <- runDB $ selectFirst [UsersLogin ==. login, UsersSenha ==. senha] []
                   case user of
                       Nothing -> redirect LoginR
                       Just (Entity pid u) -> setSession "_ID" (pack $ show $ fromSqlKey pid) >> redirect (PerfilR pid)

getErroR :: Handler Html
getErroR = defaultLayout [whamlet|
     <h1> Erro de cadastro
|]

getLogoutR :: Handler Html
getLogoutR = do
     deleteSession "_ID"
     defaultLayout [whamlet|
         <h1> ADEUS!
     |]

connStr = "dbname=dd9en8l5q4hh2a host=ec2-107-21-219-201.compute-1.amazonaws.com user=kpuwtbqndoeyqb password=aCROh525uugAWF1l7kahlNN3E0 port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool)
