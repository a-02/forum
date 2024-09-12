{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Foundation where

import Yesod.Core

data App = App

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App