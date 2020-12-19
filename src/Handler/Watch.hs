{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Watch where

import Import
import Data.Aeson (encode, decodeStrict)
import Yesod.WebSockets
import Network.WebSockets (ConnectionException (..))
import Data.Text as T
import Data.Text.Encoding (encodeUtf8)

-- socketHandler :: WebSocketsT Handler ()
-- socketHandler = do
--   sendTextData ("Hello" :: Text)

socketHandler :: WebSocketsT Handler ()
socketHandler = do
  App {..} <- getYesod
  -- subscribe to channel
  chan <- liftIO $ atomically $ dupTChan appStateChannel

  -- send initial value
  withPlaybackTimer appWrappedPlaybackTimer $ \ps -> do
    psValue <- liftIO $ readIORef ps >>= getPlaybackState
    sendJSON psValue
    logInfoN "Send Playbackstate to new client "

  catch
    (race_
      (forever $ atomically (readTChan chan) >>= sendJSON)
      (forever $ receiveData >>= handleUpdateFromClient chan) )
    connectionError

  where
    handleUpdateFromClient :: TChan PlaybackState -> Text -> WebSocketsT Handler ()
    handleUpdateFromClient chan text = do
        wpt <- appWrappedPlaybackTimer <$> getYesod
        let utf8 = encodeUtf8 text
        case decodeStrict utf8 :: Maybe PlaybackState of
          Just recvPS -> do
            logInfoN $ "received " ++ T.pack (show recvPS)
            withPlaybackTimer wpt $ \timer -> do
              servPlaybackState <- liftIO $ readIORef timer >>= getPlaybackState
              if playbackStatesConsistent servPlaybackState recvPS then
                pure ()
              else do
                liftIO $ updateTimer recvPS timer
                -- share new state with all clients
                liftIO $ readIORef timer >>= getPlaybackState >>= (atomically . writeTChan chan)

          Nothing -> logInfoN $ "received Nothing from: " ++ text


    sendJSON :: PlaybackState -> WebSocketsT Handler ()
    sendJSON = sendTextData . encode . toJSON

    connectionError :: ConnectionException -> WebSocketsT Handler ()
    connectionError (ParseException s)   = logErrorN $ "ParseException " ++ T.pack s
    connectionError (UnicodeException s) = logErrorN $ "UnicodeException " ++ T.pack s
    connectionError _                    = logInfoN "websocket closed" -- Connection closed

getWatchR :: Handler Html
getWatchR = do
  webSockets socketHandler
  let defPS = def :: PlaybackState
  defaultLayout $ do
    [whamlet|
      <video width=320 height=240 id=videoframe controls>
        <source src=@{VideoR} type="video/mp4">
    |]
    toWidget [julius|
    //   let videoframe = document.getElementById("videoframe");

      let state = #{toJSON defPS};
      let url = document.URL.replace("http:", "ws:").replace("https:", "wss:");
      let conn = null;

      function onMessage(e) {
        console.log("Received status update: "+e.data);
        tmpstate = JSON.parse(e.data);
        setPlaybackstate(tmpstate.state);

        // reset playback position
        let deltaTime = Math.abs(videoframe.currentTime * 1000 - tmpstate.pos);
        console.log("DeltaTime: ");
        if (tmpstate.state === "play")
          if (deltaTime > #{show maxDeltaTime}) {
            setPosition(tmpstate.pos)
          }
        // Always set position when paused
        else {
          setPosition(tmpstate.pos)
        }
      }

      videoframe.addEventListener('loadedmetadata', function() {
        conn = new WebSocket(url);

        conn.onmessage = onMessage;
      }, false);

      function setPlaybackstate(newState) {
        if (state.state !== newState) {
          if (newState === "pause") {
            videoframe.pause();
          } else {
            videoframe.play();
          }
          state.state = newState;
          sendState();
        }
      }

      function setPosition(newPosition) {
        let inSeconds = newPosition/1000
        console.log("Setting time to " + inSeconds)
        videoframe.currentTime = inSeconds;
        state.pos = newPosition;
        console.log(state);
      }

      function sendState() {
        let currTime = videoframe.currentTime * 1000;
        let tempstate = state;
        tempstate.pos = currTime;
        console.log("Send state "+JSON.stringify(tempstate));
        conn.send(JSON.stringify(tempstate));
      }

      videoframe.onpause = function(e) {
        console.log("Playback paused");
        setPlaybackstate("pause");
      };

      videoframe.onplay = function(e) {
        console.log("Playback started");
        setPlaybackstate("play");
      };
    |]
