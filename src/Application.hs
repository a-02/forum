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
import Model
import Settings
import Network.Wai.Handler.Warp as Warp
import Control.Monad (when)
import Control.Monad.Logger
import Language.Haskell.TH.Syntax
import Database.Persist.Postgresql 
import Yesod.Default.Config2

mkYesodDispatch "App" resourcesApp

-- create warp settings from an us-defined App
-- that being. the warp Settings datatype. 
warpSettings :: App -> Warp.Settings
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
    -- ostensibly for keeping connections alive
    appHttpManager <- getGlobalManager
    -- yesod's rather bespoke logging system
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    -- for serving static routes
    appStatic <-
        (if appMutableStatic appSettings
            then staticDevel
            else static)
            (appStaticDir appSettings)
        -- record wild cards baby!!! can you find all 5 reused argmuments?
    let mkFoundation appConnectionPool = App {..}
        tempFoundation =
            mkFoundation $ error "Connection pool forced in tempFoundation."
        logFunc = messageLoggerSource tempFoundation appLogger
    pool <-
        flip runLoggingT logFunc $
        createPostgresqlPool
            (pgConnStr $ appDatabaseConf appSettings)
            (pgPoolSize $ appDatabaseConf appSettings)
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc
    return $ mkFoundation pool

newMain :: IO ()
newMain = do
    settings <- loadYamlSettingsArgs [configSettingsYmlValue] useEnv
    app <- makeFoundation settings
    commonapp <- makeApplication app
    runSettings (warpSettings app) commonapp

