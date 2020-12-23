{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Watch where

import Import
import Data.List as L (delete)
import Text.Blaze.Html.Renderer.Text
import Data.Aeson (encode, decodeStrict)
import Yesod.WebSockets
import Network.WebSockets (ConnectionException (..))
import Data.Text as T
import System.FilePath
import Data.Text.Encoding (encodeUtf8)

import qualified Data.Text.Lazy as LazyText

fromLazyText :: LazyText.Text -> Text
fromLazyText = mconcat . LazyText.toChunks

socketHandler :: WebSocketsT Handler ()
socketHandler = do
  App {..} <- getYesod

  -- receive username from client
  username <- fromLazyText . renderHtml . toHtml <$> (receiveData :: WebSocketsT Handler Text)
  logInfoN $ "Client with username " ++ username ++ " connected"

  -- add to connected users and send all connected users
  -- to the new client
  atomically $ do
    writeTChan appStateChannel (MessageChat $ UserJoined username)
    modifyTVar' appConnectedUsers (username:)

  sendConnectedUsersMessage

  -- duplicate after sending the user joined information
  -- so that it is not displayed for this user
  -- subscribe to channel
  chan <- liftIO $ atomically $ dupTChan appStateChannel

  -- send initial value
  withPlaybackTimer appWrappedPlaybackTimer $ \ps -> do
    psValue <- liftIO $ readIORef ps >>= getPlaybackState
    sendJSON psValue
    logInfoN $ "Send Playbackstate to new client " ++ username

  catch
    (race_
      (forever $ atomically (readTChan chan) >>= sendJSON)
      (forever $ receiveData >>= handleUpdateFromClient username chan) )
    (connectionError username chan)

  where
    sendConnectedUsersMessage :: WebSocketsT Handler ()
    sendConnectedUsersMessage = do
      users <- (appConnectedUsers <$> getYesod) >>= readTVarIO
      let userlist = intercalate ", " $ (\u -> "<b>"++u++"</b>") <$> users
      sendJSON $ MessageChat $ ServerMessage $ "Connected users: " ++ userlist

    handleUpdateFromClient :: Text -> TChan FrontendMessage -> Text -> WebSocketsT Handler ()
    handleUpdateFromClient username chan recvText = do
        wpt <- appWrappedPlaybackTimer <$> getYesod
        let utf8 = encodeUtf8 recvText
        case decodeStrict utf8 :: Maybe ClientMessage of
          Just (ClientChat text) -> do
            logInfoN $ "Received Chat Message from " ++ username
            case text of
              "/list" -> sendConnectedUsersMessage

              "/help" ->
                let msg = "Possible Commands: list help" in
                sendJSON $ MessageChat $ ServerMessage msg

              _ ->
                atomically $ writeTChan chan (MessageChat $ ChatMessage username text)

          Just (ClientPlaybackState recvPS) -> do
            logInfoN $ "received " ++ T.pack (show recvPS)
            withPlaybackTimer wpt $ \timer -> do
              servPlaybackState <- liftIO $ readIORef timer >>= getPlaybackState
              if playbackStatesConsistent servPlaybackState recvPS then
                pure ()
              else do
                liftIO $ updateTimer recvPS timer
                -- share new state with all clients
                liftIO $ readIORef timer >>= getPlaybackState >>= (atomically . writeTChan chan . MessagePlaybackState)

          Nothing -> logInfoN $ "received Nothing from: " ++ recvText


    sendJSON :: ToJSON j => j -> WebSocketsT Handler ()
    sendJSON = sendTextData . encode . toJSON

    connectionError :: Text -> TChan FrontendMessage -> ConnectionException -> WebSocketsT Handler ()
    connectionError user chan e = do
      App {..} <- getYesod
      atomically $ do
        writeTChan chan (MessageChat $ UserLeft user)
        modifyTVar' appConnectedUsers (L.delete user)
      case e of
        ParseException s   -> logErrorN $ "ParseException " ++ T.pack s
        UnicodeException s -> logErrorN $ "UnicodeException " ++ T.pack s
        _                  -> logInfoN "websocket closed" -- Connection closed

getWatchR :: Handler Html
getWatchR = do
  webSockets socketHandler
  videoName <- takeBaseName . appVideoFile <$> getYesod
  let defPS = def :: PlaybackState
  defaultLayout $ do
    setTitle $ toHtml $ "Watch Together - " ++ videoName
    [whamlet|
      <div #usernamediv hidden>
        <label for=usernameinp>
          Username:
        <input #usernameinp>
        <button #usernamebutton>
          Ok
      <div #maindiv hidden>
        <div #contentdiv>
          <h1>
            #{videoName}
          <video #videoframe controls>
            <source src=@{VideoR} type="video/mp4">
        <div #chatcontainer>
          <div #chatwindow>
          <input #chatinp>
          <button #chatbutton>
            Send
    |]
    toWidget [lucius|
      #chatcontainer {
        height: 100vh;
      }
      #chatwindow {
        display: flex;
        flex-direction: column-reverse;
        height: 70%;
        max-height: 70%;
        overflow-y: auto;
      }
      #videoframe {
        width: 100%;
        max-width: 100%;
      }
      #maindiv {
        height: 100vh;
        width: 100%;
        grid-gap: 2.5%;
        padding-right: 2.5%;
        grid-template-columns: 65% 30%;
      }
      p {
        margin-top: 0.25em;
        margin-bottom: 0.25em;
      }
      body {
        background-color: rgba(40,42,54);
        color: #a9b7c6;
      }
      input, button {
        background: rgba(68, 71, 90);
        color: #a9b7c6;
        border-color: rgba(0,0,0,0);
      }
    |]
    toWidget [julius|
      let username = "noname";
      let usernamefield = document.getElementById("usernameinp");
      let usernamebutton = document.getElementById("usernamebutton");
      let usernamediv = document.getElementById("usernamediv");

      let maindiv = document.getElementById("maindiv");
      let videoframe = document.getElementById("videoframe");

      let chatwindow = document.getElementById("chatwindow");
      let chatbutton = document.getElementById("chatbutton");
      let chatfield = document.getElementById("chatinp");

      let playstate = #{toJSON defPS}.state;
      let url = document.URL.replace("http:", "ws:").replace("https:", "wss:");
      let conn = null;

      usernamefield.addEventListener("keyup", function(event) {
        if (event.keyCode === 13) { // Enter pressed
          usernamebutton.click();
        }
      });
      chatfield.addEventListener("keyup", function(event) {
        if (event.keyCode === 13) { // Enter pressed
          chatbutton.click();
        }
      });

      function onMessage(e) {
        let msg = JSON.parse(e.data);
        if (msg.type === "playbackstate") {
          onPlaybackStateMessage(msg.data)
        } else if (msg.type === "chatentry") {
          onChatEntryMessage(msg.data);
        }
      }

      function addToChatWindow(html) {
        let p = document.createElement("p");
        p.innerHTML = html;
        // the chatwindow "scroll" direction is reversed
        chatwindow.prepend(p);
      }

      function onChatEntryMessage(msg) {
        console.log("Chat: "+JSON.stringify(msg));
        if (msg.type === "userjoined") {
          addToChatWindow("User <b>"+msg.user+"</b> joined!");
        } else if (msg.type === "userleft") {
          addToChatWindow("User <b>"+msg.user+"</b> left!");
        } else if (msg.type === "message") {
          // prepend time
          let currentDate = new Date();
          let hours = currentDate.getHours();
          let minutes = currentDate.getMinutes();

          if (minutes < 10) {
            minutes = "0"+minutes;
          }
          if (hours < 10) {
            hours = "0"+hours;
          }
          let timestamp = hours + ":" + minutes;

          addToChatWindow("[<i>"+timestamp+"</i>]" + " <b>"+msg.from+"</b>: "+msg.text);
        } else if (msg.type === "servermessage") {
          addToChatWindow("<i>"+msg.msg+"</i>");
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
        usernamediv.style.display = "block";
      }, false);

      usernamebutton.onclick = function() {
        username = usernamefield.value;
        document.body.removeChild(usernamediv);
        console.log("Starting WebSocket. Username: "+username)

        conn = new WebSocket(url);
        conn.onmessage = onMessage;
        conn.onopen = onSocketOpen;
      };

      chatbutton.onclick = function () {
        let text = chatfield.value;
        let toSend = JSON.stringify({"type":"chat","text":text});
        console.log("Send chat message"+toSend);
        conn.send(toSend);
        chatfield.value = "";
      }

      function onSocketOpen() {
        conn.send(username);

        maindiv.style.display = "grid";
      }

      function setPosition(newPosition) {
        let inSeconds = newPosition/1000;
        console.log("Setting time to " + inSeconds);
        videoframe.currentTime = inSeconds;
      }

      function sendState() {
        let currTime = videoframe.currentTime * 1000;
        let toSend = {"type":"playbackstate", "state":{"state":playstate, "pos":currTime}};
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
