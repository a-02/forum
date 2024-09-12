{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Foundation where

import Settings
import Yesod.Core
import Database.Esqueleto (ConnectionPool)
import Yesod.Core.Types (Logger)
import Yesod.Static 
import Network.HTTP.Client
import ClassyPrelude.Yesod (Text)
import Network.Wai.Handler.Warp
import Database.Persist.Postgresql

data App = App
  { appSettings :: ApplicationSettings
  , appConnectionPool ::  ConnectionPool
  , appLogger :: Logger
  , appStatic :: Static
  , appHttpManager :: Manager
  }

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App where
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root