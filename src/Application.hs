{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import Foundation
import Yesod.Core

import Home
import Settings
import Network.Wai.Handler.Warp
import Control.Monad (when)
import Control.Monad.Logger
import Language.Haskell.TH.Syntax

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

newMain :: IO ()
newMain = do
    let app = (undefined :: YesodDispatch a => a)
    commonapp <- makeApplication app
    runSettings (warpSettings app) commonapp

