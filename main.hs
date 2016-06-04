{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}

module Main where
import Routes
import Yesod
import Foundation
import Handlers
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text

import Database.Persist.Postgresql

connStr = "dbname=d3k3divnnj013l host=ec2-54-235-119-29.compute-1.amazonaws.com user=qqnueatlikajct password=WAvHTYlVI2w6PF-m9N0sj2es4c port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
       runSqlPersistMPool (runMigrationUnsafe migrateAll) pool
       warp 8080 (Page pool)
