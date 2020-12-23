{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Foundation where

import Datatypes

import Data.Aeson.Types
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Text
import GHC.Generics
import Data.IORef
import Yesod
import Control.Monad
import Network.Mime (MimeType)
import Control.Concurrent
import Control.Concurrent.STM (writeTChan, TChan, newBroadcastTChan, atomically, TVar, newTVar)

periodicBroadcastingSeconds :: Int
periodicBroadcastingSeconds = 5

type Username = Text

data ChatEntry = UserJoined Text
               | UserLeft Text
               | ServerMessage Text
               | ChatMessage Username Html deriving Generic

data FrontendMessage = MessagePlaybackState PlaybackState
                     | MessageChat ChatEntry  deriving Generic

data ClientMessage = ClientChat Text
                   | ClientPlaybackState PlaybackState deriving (Generic, Show)

-- Put your config, database connection pool, etc. in here.
data App = App {
             appStateChannel :: !(TChan FrontendMessage)
           , appConnectedUsers :: !(TVar [Username])
           , appWrappedPlaybackTimer :: !WrappedPlaybackTimer
           , appVideoFile :: !String
           , appVideoFileMime :: MimeType
           , periodicPlaybackStateBroadcasterId :: !ThreadId
           }


periodicPlaybackStateBroadcaster :: TChan FrontendMessage -> WrappedPlaybackTimer -> IO  ()
periodicPlaybackStateBroadcaster chan wrappedTimer = forever $ do
  threadDelay $ 1000 * 1000 * periodicBroadcastingSeconds
  withPlaybackTimer wrappedTimer $ \timerRef -> do
    state <- readIORef timerRef >>= getPlaybackState
    atomically $ writeTChan chan (MessagePlaybackState state)

createFoundation :: String -> MimeType -> IO App
createFoundation videoFile mimeType = do
  chan <- atomically newBroadcastTChan
  connectedUsers <- atomically $ newTVar []
  wps <- createDefaultWrappedPlaybackState
  backgroundProcess <- forkIO (periodicPlaybackStateBroadcaster chan wps)
  pure $ App chan connectedUsers wps videoFile mimeType backgroundProcess



-- Derive routes and instances for App.
mkYesodData "App" [parseRoutes|
  / WatchR GET
  /Video VideoR GET
|]

instance Yesod App where -- Methods in here can be overridden as needed.
  -- If https is not recognized check if the header
  -- X-Forwarded-Proto is set to https
  -- or any other Header that is checked by Network.Wai.Request.appearsSecure
  -- Otherwise if this still causes problems
  -- approot = ApprootRelative

-- Tells our application to use the standard English messages.
-- If you want i18n, then you can supply a translating function instead.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance ToJSON ChatEntry where
  toJSON (UserLeft user)           = object [ "type" .= ("userleft" :: Text), "user" .= user]
  toJSON (UserJoined user)         = object [ "type" .= ("userjoined" :: Text), "user" .= user]
  toJSON (ChatMessage sender text) = object [ "type" .= ("message" :: Text), "from" .= sender, "text" .= renderHtml text]
  toJSON (ServerMessage msg)       = object [ "type" .= ("servermessage" :: Text), "msg".=msg]

instance ToJSON FrontendMessage where
  toJSON (MessagePlaybackState state) = object [ "type" .= ("playbackstate" :: Text), "data" .= state]
  toJSON (MessageChat entry)          = object [ "type" .= ("chatentry" :: Text), "data" .= entry]

instance FromJSON ClientMessage where
  parseJSON (Object v) =
    (v.:"type" :: Parser Text) >>= \case
      "chat" -> ClientChat <$> v.:"text"
      "playbackstate" -> ClientPlaybackState <$> v.:"state"
      _ -> error $ "Failed to parse ClientMessage from " ++ show v

  parseJSON invalid =  prependFailure "parsing PlaybackState failed, " (typeMismatch "Object" invalid)
