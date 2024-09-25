{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Foundation where

import Settings
import Yesod.Core
import Database.Esqueleto (ConnectionPool)
import Yesod.Core.Types (Logger)
import Yesod.Static 
import Text.Hamlet
import Yesod.Form
import Network.HTTP.Client
import ClassyPrelude.Yesod
import Network.Wai.Handler.Warp
import Database.Persist.Postgresql

-- The idea is we're making our own "App" type that's an instance of Yesod.
-- Anything can go in here. 
data App = App
  { appSettings :: ApplicationSettings
  , appConnectionPool ::  ConnectionPool
  , appLogger :: Logger
  , appStatic :: Static
  , appHttpManager :: Manager
  }

mkYesodData "App" $(parseRoutesFile "routes")

type DB a = forall (m :: * -> *). (MonadIO m) => ReaderT SqlBackend m a

type Form a = Html -> MForm (HandlerFor App) (FormResult a, Widget)


instance Yesod App where
    yesodMiddleware = defaultYesodMiddleware
    defaultLayout :: WidgetFor App () -> HandlerFor App Html
    defaultLayout widget = do
      master <- getYesod
      message <- getMessage
      mcurrentRoute <- getCurrentRoute
      pagecontent <- widgetToPageContent $ do
        [whamlet|
          $maybe route <- mcurrentRoute
            <p> You're at #{show route}.
          $nothing
            <p> You're lost.
          ^{widget}
        |]
      withUrlRenderer $(hamletFile "templates/wrapper.hamlet")
-- if approot is set, use that. if not, guess!
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root