{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import Foundation
import Yesod.Core
import Yesod.Static
import Network.HTTP.Client.TLS
import System.Log.FastLogger
import Home
import Settings
import Network.Wai.Handler.Warp
import Control.Monad (when)
import Control.Monad.Logger
import Language.Haskell.TH.Syntax
import Database.Persist.Postgresql 
import Yesod.Default.Config2

mkYesodDispatch "App" resourcesApp

warpSettings :: App -> Settings
warpSettings app =
    setPort (appPort $ appSettings app) $
    setHost (appHost $ appSettings app) $
    setOnException
        (\_req exception ->
            when (defaultShouldDisplayException exception) $
            messageLoggerSource
                app
                (appLogger app)
                $(qLocation >>= liftLoc)
                "yesod"
                LevelError
                (toLogStr $ "Exception from warp!" ++ show exception))
            defaultSettings

makeApplication :: YesodDispatch a => a -> IO Application
makeApplication app = do
    commonapp <- toWaiApp app
    return $ defaultMiddlewaresNoLogging commonapp

makeFoundation :: ApplicationSettings -> IO App
makeFoundation appSettings = do
    appHttpManager <- getGlobalManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings
            then staticDevel
            else static)
            (appStaticDir appSettings)
    let mkFoundation appConnectionPool = App {..}
        tempFoundation =
            mkFoundation $ error "Connection pool forced in tempFoundation."
        logFunc = messageLoggerSource tempFoundation appLogger
    pool <-
        flip runLoggingT logFunc $
        createPostgresqlPool
            (pgConnStr $ appDatabaseConf appSettings)
            (pgPoolSize $ appDatabaseConf appSettings)
    return $ mkFoundation pool

newMain :: IO ()
newMain = do
    settings <- loadYamlSettingsArgs [configSettingsYmlValue] useEnv
    app <- makeFoundation settings
    commonapp <- makeApplication app
    runSettings (warpSettings app) commonapp

