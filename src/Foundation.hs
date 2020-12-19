{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Foundation where

import Datatypes

import Data.IORef
import Yesod
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM (writeTChan, TChan, newBroadcastTChan, atomically, dupTChan)

periodicBroadcastingSeconds :: Int
periodicBroadcastingSeconds = 5

-- Put your config, database connection pool, etc. in here.
data App = App {
             appStateChannel :: !(TChan PlaybackState)
           , appWrappedPlaybackTimer :: !WrappedPlaybackTimer
           , appVideoFile :: !String
           , periodicPlaybackStateBroadcasterId :: !ThreadId
           }


periodicPlaybackStateBroadcaster :: TChan PlaybackState -> WrappedPlaybackTimer -> IO  ()
periodicPlaybackStateBroadcaster chan wrappedTimer = forever $ do
  dupChan <- atomically $ dupTChan chan
  threadDelay $ 1000 * 1000 * periodicBroadcastingSeconds
  putStrLn "Hello"
  withPlaybackTimer wrappedTimer $ \timerRef -> do
    state <- readIORef timerRef >>= getPlaybackState
    atomically $ writeTChan dupChan state

createFoundation :: String -> IO App
createFoundation videoFile = do
  chan <- atomically newBroadcastTChan
  wps <- createDefaultWrappedPlaybackState
  backgroundProcess <- forkIO (periodicPlaybackStateBroadcaster chan wps)
  pure $ App chan wps videoFile backgroundProcess



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

