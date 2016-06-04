{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Model where
import Yesod
import Data.Text

pModel :: [EntityDef]
pModel = [persistLowerCase|
Author json
  name Text
  nick Text
  password Text

  UniqueAuthorNick nick
  deriving Show

Post json
  title Text
  content Text
  authorId AuthorId
  deriving Show

Tag json
  name Text
  authorId AuthorId

  UniqueTagName name
  deriving Show

Collection json
  name Text
  authorId AuthorId

  UniqueCollectionName name
  deriving Show

PostTag
  postId PostId
  authorId AuthorId
  deriving Show

PostCollection
  postId PostId
  collectionId CollectionId
  deriving Show
|]
