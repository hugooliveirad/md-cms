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

connStr = "dbname=dd9en8l5q4hh2a host=ec2-107-21-219-201.compute-1.amazonaws.com user=kpuwtbqndoeyqb password=aCROh525uugAWF1l7kahlNN3E0 port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Page pool)
