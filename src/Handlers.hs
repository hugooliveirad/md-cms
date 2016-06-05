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

-- Pages

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

-- Post

getPostsR :: Handler ()
getPostsR = do
  posts <- runDB $ selectList [] [Asc PostTitle]
  sendResponse $ toJSON posts

postPostsR :: Handler ()
postPostsR = do
  post <- requireJsonBody :: Handler Post
  postId <- runDB $ insert post
  post <- runDB $ get404 postId
  sendResponse $ toJSON post

getPostR :: PostId -> Handler ()
getPostR id = do
  post <- runDB $ get404 id
  sendResponse $ toJSON post

putPostR :: PostId -> Handler ()
putPostR id = do
  post <- requireJsonBody :: Handler Post
  runDB $ update id [PostTitle =. postTitle post,
                     PostContent =. postContent post,
                     PostAuthorId =. postAuthorId post]
  post <- runDB $ get404 id
  sendResponse $ toJSON post

deletePostR :: PostId -> Handler ()
deletePostR id = do
  runDB $ delete id

-- Tag

getTagsR :: Handler ()
getTagsR = do
  tags <- runDB $ selectList [] [Asc TagName]
  sendResponse $ toJSON tags

postTagsR :: Handler ()
postTagsR = do
  tag <- requireJsonBody :: Handler Tag
  tagId <- runDB $ insert tag
  tag <- runDB $ get404 tagId
  sendResponse $ toJSON tag

getTagR :: TagId -> Handler ()
getTagR id = do
  tag <- runDB $ get404 id
  sendResponse $ toJSON tag

putTagR :: TagId -> Handler ()
putTagR id = do
  tag <- requireJsonBody :: Handler Tag
  runDB $ update id [TagName =. tagName tag,
                     TagAuthorId =. tagAuthorId tag]
  tag <- runDB $ get404 id
  sendResponse $ toJSON tag

deleteTagR :: TagId -> Handler ()
deleteTagR id = do
  runDB $ delete id

-- Collection

getCollectionsR :: Handler ()
getCollectionsR = do
  collections <- runDB $ selectList [] [Asc CollectionName]
  sendResponse $ toJSON collections

postCollectionsR :: Handler ()
postCollectionsR = do
  collection <- requireJsonBody :: Handler Collection
  collectionId <- runDB $ insert collection
  collection <- runDB $ get404 collectionId
  sendResponse $ toJSON collection

getCollectionR :: CollectionId -> Handler ()
getCollectionR id = do
  collection <- runDB $ get404 id
  sendResponse $ toJSON collection

putCollectionR :: CollectionId -> Handler ()
putCollectionR id = do
  collection <- requireJsonBody :: Handler Collection
  runDB $ update id [CollectionName =. collectionName collection,
                     CollectionAuthorId =. collectionAuthorId collection]
  collection <- runDB $ get404 id
  sendResponse $ toJSON collection

deleteCollectionR :: CollectionId -> Handler ()
deleteCollectionR id = do
  runDB $ delete id

-- PostCollection

getPostCollectionsR :: CollectionId -> Handler ()
getPostCollectionsR id = do
  postCollections <- runDB $ selectList [PostCollectionCollectionId ==. id] []
  sendResponse $ toJSON postCollections

postPostCollectionR :: CollectionId -> PostId -> Handler ()
postPostCollectionR collId postId = do
  postCollectionId <- runDB $ insert $ PostCollection postId collId
  postCollection <- runDB $ get404 postCollectionId
  sendResponse $ toJSON postCollection

deletePostCollectionR :: CollectionId -> PostId -> Handler ()
deletePostCollectionR collId postId = do
  runDB $ deleteWhere [PostCollectionPostId ==. postId,
                       PostCollectionCollectionId ==. collId]
