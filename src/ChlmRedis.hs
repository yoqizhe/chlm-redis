{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ChlmRedis
    ( rawRunRedis
    , ChlmRedis(..)
    , ConnectInfo(..)
    ) where

import Yesod.Core   -- 包含 Yesod 的核心功能，包括日志相关的函数
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (liftLoc)
import Data.Aeson (Result (..), FromJSON, parseJSON, withObject, (.:))
import Database.Redis (ConnectInfo(..), PortID(..), defaultConnectInfo)
import qualified Database.Redis as R
import Prelude (Either(..), IO, Show)
import ClassyPrelude.Yesod
import Yesod.Default.Config2 (loadYamlSettings, useEnv, configSettingsYml)

-- Helper to parse the port into a PortID
parsePort :: Int -> PortID
parsePort p = PortNumber (fromIntegral p)

instance FromJSON ConnectInfo where
  parseJSON = withObject "Redis Connect Info" $ \o -> do
    host               <- o .: "host"
    port               <- o .: "port"
    maxConn            <- o .: "maxConn"
    database            <- o .: "database"

    return defaultConnectInfo {
      connectHost = pack host,
      connectPort = parsePort port,
      connectMaxConnections = maxConn,
      connectDatabase = database
    }

data ChlmRedisException = ChlmRedisException Text
    deriving (Show)
instance Exception ChlmRedisException

class ChlmRedis app where
    redisPool :: HandlerFor app R.Connection

-- 执行 Redis 操作
rawRunRedis :: (Yesod app, ChlmRedis app) => R.Redis (Either R.Reply a) -> HandlerFor app a
rawRunRedis action = do
    redisConn <- redisPool
    eResult <- liftIO $ R.runRedis redisConn action
    case eResult of
        Left reply -> handleReply reply
        Right result -> return result
    where
        handleReply :: R.Reply -> HandlerFor app a
        handleReply reply = do
            let errorMsg = "Redis error: " <> showReply reply
            $logError errorMsg
            throwIO $ ChlmRedisException errorMsg

        showReply :: R.Reply -> Text
        showReply = pack . show
