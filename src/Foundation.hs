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
import Control.Concurrent.MVar (MVar)

-- Int corresponds to the playback position (in ms)
data PlaybackState = Pause Int
                   | Play Int

newtype WrappedPlaybackState = WPS (IORef PlaybackState, MVar ())
-- Put your config, database connection pool, etc. in here.
data App = App {
             appWrappedPlaybackState :: WrappedPlaybackState
           , appVideoFile :: String
           }

createFoundation :: IO App
createFoundation = undefined -- pure App



-- Derive routes and instances for App.
mkYesodData "App" [parseRoutes|
/ WatchR GET POST
|]

instance Yesod App -- Methods in here can be overridden as needed.

-- Tells our application to use the standard English messages.
-- If you want i18n, then you can supply a translating function instead.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage
