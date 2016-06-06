{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Routes where
import Yesod
import Model

pRoutes = [parseRoutes|
  / HomeR GET

  /api/login LoginR POST
  /api/logout LogoutR POST

  /api/authors           AuthorsR GET POST
  /api/authors/#AuthorId AuthorR GET PUT DELETE

  /api/posts         PostsR GET POST
  /api/posts/#PostId PostR GET PUT DELETE

  /api/tags        TagsR GET POST
  /api/tags/#TagId TagR GET PUT DELETE

  /api/collections               CollectionsR GET POST
  /api/collections/#CollectionId CollectionR GET PUT DELETE

  /api/collections/#CollectionId/posts         CollectionPostsR GET
  /api/collections/#CollectionId/posts/#PostId CollectionPostR POST DELETE

  /api/tags/#TagId/posts         TagPostsR GET
  /api/tags/#TagId/posts/#PostId TagPostR POST DELETE
|]
