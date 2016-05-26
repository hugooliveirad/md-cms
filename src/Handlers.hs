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
