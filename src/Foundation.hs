{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Foundation where
import Routes
import Model
import Yesod
import Data.Text
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

data Page = Page { connPool :: ConnectionPool }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] pModel

mkYesodData "Page" pRoutes

instance YesodPersist Page where
   type YesodPersistBackend Page = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

instance Yesod Page where

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Page FormMessage where
    renderMessage _ _ = defaultFormMessage
