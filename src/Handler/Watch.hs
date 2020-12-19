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

  -- increase and send socket counter
  atomically $ modifyTVar' appWebSocketCount (+1)
  readTVarIO appWebSocketCount >>= (atomically . writeTChan chan . MessageWebsocketCount)

  -- send initial value
  withPlaybackTimer appWrappedPlaybackTimer $ \ps -> do
    psValue <- liftIO $ readIORef ps >>= getPlaybackState
    sendJSON psValue
    logInfoN "Send Playbackstate to new client"

  catch
    (race_
      (forever $ atomically (readTChan chan) >>= sendJSON)
      (forever $ receiveData >>= handleUpdateFromClient chan) )
    (connectionError chan)

  where
    handleUpdateFromClient :: TChan FrontendMessage -> Text -> WebSocketsT Handler ()
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
                liftIO $ readIORef timer >>= getPlaybackState >>= (atomically . writeTChan chan . MessagePlaybackState)

          Nothing -> logInfoN $ "received Nothing from: " ++ text


    sendJSON :: ToJSON j => j -> WebSocketsT Handler ()
    sendJSON = sendTextData . encode . toJSON

    connectionError :: TChan FrontendMessage -> ConnectionException -> WebSocketsT Handler ()
    connectionError chan e = do
      App {..} <- getYesod
      atomically $ modifyTVar' appWebSocketCount (\x -> x - 1)
      readTVarIO appWebSocketCount >>= (atomically . writeTChan chan . MessageWebsocketCount)
      case e of
        ParseException s   -> logErrorN $ "ParseException " ++ T.pack s
        UnicodeException s -> logErrorN $ "UnicodeException " ++ T.pack s
        _                  -> logInfoN "websocket closed" -- Connection closed

getWatchR :: Handler Html
getWatchR = do
  webSockets socketHandler
  let defPS = def :: PlaybackState
  defaultLayout $ do
    [whamlet|
      <video width=320 height=240 id=videoframe controls>
        <source src=@{VideoR} type="video/mp4">
      <p id=socketcount>
        ? WebSockets are connected
    |]
    toWidget [julius|
      let videoframe = document.getElementById("videoframe");
      let socketcountp = document.getElementById("socketcount");

      let playstate = #{toJSON defPS}.state;
      let url = document.URL.replace("http:", "ws:").replace("https:", "wss:");
      let conn = null;

      function onMessage(e) {
        let msg = JSON.parse(e.data);
        if (msg.type === "playbackstate") {
          onPlaybackStateMessage(msg.data)
        }
        else if (msg.type === "socketcount") {
          onSocketCountMessage(msg.data)
        }
      }

      function onSocketCountMessage(e) {
        console.log("Received socket count "+e);
        if (e !== 1) {
          socketcountp.textContent = e+" WebSockets are connected";
        } else {
          socketcountp.textContent = e+" WebSocket is connected";
        }
      }

      function onPlaybackStateMessage(newstate) {
        console.log("Received status update: "+JSON.stringify(newstate));

        // force-reset playback position if delta becomes to large
        let deltaTime = Math.abs(videoframe.currentTime * 1000 - newstate.pos);
        console.log("DeltaTime: "+deltaTime);
        if (newstate.state === "play") {
          if (deltaTime > #{show maxDeltaTime}) {
            setPosition(newstate.pos)
          }
        }

        // If the server switches state force-synchronise the client
        if (playstate !== newstate.state) {
          console.log()
          if (newstate.state === "pause") {
            videoframe.pause();
          } else {
            videoframe.play();
          }
          playstate = newstate.state;
          setPosition(newstate.pos);
        }
      }

      // Metadata needs to be loaded in order to set the playback position
      videoframe.addEventListener('loadedmetadata', function() {
        conn = new WebSocket(url);

        conn.onmessage = onMessage;
      }, false);

      function setPosition(newPosition) {
        let inSeconds = newPosition/1000;
        console.log("Setting time to " + inSeconds);
        videoframe.currentTime = inSeconds;
      }

      function sendState() {
        let currTime = videoframe.currentTime * 1000;
        let toSend = {"state":playstate, "pos":currTime};
        console.log("Send state "+JSON.stringify(toSend));
        conn.send(JSON.stringify(toSend));
      }

      videoframe.onpause = function(e) {
        console.log("Playback paused");
        playstate = "pause";
        videoframe.pause();
        sendState();
      };

      videoframe.onplay = function(e) {
        console.log("Playback started", videoframe.currentTime);
        playstate = "play";
        videoframe.play();
        sendState();
      };
    |]
