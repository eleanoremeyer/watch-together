{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}

module Foundation where

import Yesod
import Data.Text (Text, pack)
import Control.Monad (forM)
import Data.List (sort)
import Data.IORef
import UnliftIO.MVar (MVar, withMVar, newMVar)
import Data.Default

-- Int corresponds to the playback position (in ms)
data PlaybackState = Pause Int
                   | Play Int

instance Default PlaybackState where
  def = Pause 0


newtype WrappedPlaybackState = WPS (IORef PlaybackState, MVar ())
-- Put your config, database connection pool, etc. in here.
data App = App {
             appWrappedPlaybackState :: WrappedPlaybackState
           , appVideoFile :: String
           }

createFoundation :: String -> IO App
createFoundation videoFile = do
  wps <- WPS <$> ((,) <$> newIORef def <*> newMVar ())
  pure $ App wps videoFile



-- Derive routes and instances for App.
mkYesodData "App" [parseRoutes|
  / WatchR GET
|]

instance Yesod App -- Methods in here can be overridden as needed.

-- Tells our application to use the standard English messages.
-- If you want i18n, then you can supply a translating function instead.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

withPlaybackState :: WrappedPlaybackState -> (IORef PlaybackState -> Handler a) -> Handler a
withPlaybackState (WPS (ps, mvar)) f = withMVar mvar $ \_ -> f ps
