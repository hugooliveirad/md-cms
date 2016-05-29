{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}

module Handlers where
import Routes
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text

import Database.Persist.Postgresql

mkYesodDispatch "Page" pRoutes

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|
     <h1> Ola!
|]

-- API

-- Author

getAuthorsR :: Handler ()
getAuthorsR = do
  authors <- runDB $ selectList [] [Asc AuthorName]
  sendResponse $ toJSON authors

postAuthorsR :: Handler ()
postAuthorsR = do
  author <- requireJsonBody :: Handler Author
  authorId <- runDB $ insert author
  author <- runDB $ get404 authorId
  sendResponse $ toJSON author

getAuthorR :: AuthorId -> Handler ()
getAuthorR id = do
  author <- runDB $ get404 id
  sendResponse $ toJSON author

putAuthorR :: AuthorId -> Handler ()
putAuthorR id = do
  author <- requireJsonBody :: Handler Author
  runDB $ update id [AuthorName =. authorName author,
                     AuthorNick =. authorNick author]
  author <- runDB $ get404 id
  sendResponse $ toJSON author

deleteAuthorR :: AuthorId -> Handler ()
deleteAuthorR id = do
  runDB $ delete id

