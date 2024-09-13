{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Settings where

import           ClassyPrelude.Yesod
import           Database.Persist.Postgresql
import           Network.Wai.Handler.Warp
import Data.Aeson
import Yesod.Default.Config2 (configSettingsYml)
import Data.FileEmbed
import Data.Yaml
import Control.Exception

data ApplicationSettings = ApplicationSettings
  { appStaticDir              :: String
  , appRoot                   :: Maybe Text
  , appDatabaseConf           :: PostgresConf
  , appHost                   :: HostPreference
  , appPort                   :: Int
  , appReloadTemplate         :: Bool
  , appMutableStatic          :: Bool
  , appSkipCombining          :: Bool
  , appDetailedRequestLogging :: Bool
  }

instance FromJSON ApplicationSettings where
  parseJSON =
    withObject "ApplicationSettings" $ \ob -> do
      let defEnv = True
      appStaticDir <- ob .: "static-dir"
      appRoot <- ob .:? "app-root"
      appHost <- fromString <$> ob .: "app-host"
      appDatabaseConf <- ob .: "database-conf"
      appPort <- ob .: "app-port"
      dev <- ob .: "development" .!= defEnv
      appReloadTemplate <- ob .:? "reload-template" .!= dev
      appMutableStatic <- ob .:? "mutable-static"  .!= dev
      appSkipCombining <- ob .:? "skip-combining" .!= dev
      appDetailedRequestLogging <- ob .:? "detailed-req-log" .!= dev
      return ApplicationSettings {..}

configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

configSettingsYmlValue :: Value
configSettingsYmlValue = either throw Prelude.id $ decodeEither' configSettingsYmlBS