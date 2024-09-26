{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Home where

import Foundation
import Yesod.Core
import ClassyPrelude.Yesod

getHomeR :: Handler Html
getHomeR = 
    defaultLayout $ do
        setTitle "Nice"
        [whamlet|
            <h4> We did it.
        |]

getComicR :: Integer -> Handler Html
getComicR pageIndex = do
    now <- liftIO getCurrentTime
    defaultLayout $ do
        setTitle "Mental Gymnastics"
        [whamlet|
            <h1>Showing page #{pageIndex}.
        |]
        toWidget [cassius|
            body
                color: red
        |]