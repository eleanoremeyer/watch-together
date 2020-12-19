{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Foundation where

import Datatypes

import Data.Text
import GHC.Generics
import Data.Aeson
import Data.IORef
import Yesod
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM (writeTChan, TChan, newBroadcastTChan, atomically, dupTChan, TVar, newTVar)

periodicBroadcastingSeconds :: Int
periodicBroadcastingSeconds = 5

data FrontendMessage = MessagePlaybackState PlaybackState
                     | MessageWebsocketCount Int deriving (Generic, Show)

-- Put your config, database connection pool, etc. in here.
data App = App {
             appStateChannel :: !(TChan FrontendMessage)
           , appWebSocketCount :: !(TVar Int)
           , appWrappedPlaybackTimer :: !WrappedPlaybackTimer
           , appVideoFile :: !String
           , periodicPlaybackStateBroadcasterId :: !ThreadId
           }


periodicPlaybackStateBroadcaster :: TChan FrontendMessage -> WrappedPlaybackTimer -> IO  ()
periodicPlaybackStateBroadcaster chan wrappedTimer = forever $ do
  dupChan <- atomically $ dupTChan chan
  threadDelay $ 1000 * 1000 * periodicBroadcastingSeconds
  withPlaybackTimer wrappedTimer $ \timerRef -> do
    state <- readIORef timerRef >>= getPlaybackState
    atomically $ writeTChan dupChan (MessagePlaybackState state)

createFoundation :: String -> IO App
createFoundation videoFile = do
  chan <- atomically newBroadcastTChan
  socketCount <- atomically $ newTVar 0
  wps <- createDefaultWrappedPlaybackState
  backgroundProcess <- forkIO (periodicPlaybackStateBroadcaster chan wps)
  pure $ App chan socketCount wps videoFile backgroundProcess



-- Derive routes and instances for App.
mkYesodData "App" [parseRoutes|
  / WatchR GET
  /Video VideoR GET
|]

instance Yesod App -- Methods in here can be overridden as needed.

-- Tells our application to use the standard English messages.
-- If you want i18n, then you can supply a translating function instead.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage


instance ToJSON FrontendMessage where
  toJSON (MessagePlaybackState state) = object [ "type" .= ("playbackstate" :: Text), "data" .= state]
  toJSON (MessageWebsocketCount c) = object [ "type" .= ("socketcount" :: Text), "data" .= c ]
