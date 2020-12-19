{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Datatypes
  ( PlaybackState (..)
  , WrappedPlaybackTimer
  , withPlaybackTimer
  , createDefaultWrappedPlaybackState
  , getPlaybackState
  , playbackStatesConsistent
  , updateTimer
  , maxDeltaTime)
  where

import Data.Text
import GHC.Generics
import Data.Default
import Data.Aeson
import Data.Aeson.Types
import Data.IORef
import UnliftIO.MVar (MVar, withMVar, newMVar)
import UnliftIO (MonadUnliftIO)
import Data.Time.Clock

maxDeltaTime :: Double
maxDeltaTime = 5 * 1000 -- 5 seconds


-- Int corresponds to the playback position (in ms)
data PlaybackState = Pause Double
                   | Play Double deriving (Generic, Show)


data PlaybackTimer = PlaybackTimerPlay Double UTCTime -- time passed before, timer started at
                   | PlaybackTimerPause Double

instance Default PlaybackState where
  def = Pause 0

getPlaybackStateTime :: PlaybackState -> Double
getPlaybackStateTime (Play t) = t
getPlaybackStateTime (Pause t) = t

instance Default PlaybackTimer where
  def = PlaybackTimerPause 0

playbackStatesConsistent :: PlaybackState -> PlaybackState -> Bool
playbackStatesConsistent (Pause x) (Pause x') = x == x'
playbackStatesConsistent (Play x)  (Play x')  = abs (x-x') <= maxDeltaTime
playbackStatesConsistent _          _         = False

newtype WrappedPlaybackTimer = WPS (IORef PlaybackTimer, MVar ())

createDefaultWrappedPlaybackState :: IO WrappedPlaybackTimer
createDefaultWrappedPlaybackState = WPS <$> ((,) <$> newIORef def <*> newMVar ())

withPlaybackTimer :: MonadUnliftIO m => WrappedPlaybackTimer -> (IORef PlaybackTimer -> m a) -> m a
withPlaybackTimer (WPS (ps, mvar)) f = withMVar mvar $ \_ -> f ps

getPlaybackState :: PlaybackTimer -> IO PlaybackState
getPlaybackState timer = case timer of
  PlaybackTimerPlay t startedAt -> do
    currTime <- getCurrentTime
    let msPassed = 1000 * realToFrac (diffUTCTime currTime startedAt)
    pure $ Play (t + msPassed)
  PlaybackTimerPause t -> pure $ Pause t

pausePlayback :: IORef PlaybackTimer -> IO ()
pausePlayback timerRef = do
  state <- readIORef timerRef >>= getPlaybackState
  writeIORef timerRef (PlaybackTimerPause $ getPlaybackStateTime state)


startPlayback :: IORef PlaybackTimer -> IO ()
startPlayback timerRef = readIORef timerRef >>= \case
  PlaybackTimerPlay _ _ -> pure ()
  PlaybackTimerPause t -> do
    currTime <- getCurrentTime
    writeIORef timerRef $ PlaybackTimerPlay t currTime

setPosition :: Double -> IORef PlaybackTimer -> IO ()
setPosition t timerRef = do
  currTime <- getCurrentTime
  atomicModifyIORef' timerRef $ \case
    PlaybackTimerPlay _ startedAt -> (PlaybackTimerPlay t currTime, ())
    PlaybackTimerPause _ -> (PlaybackTimerPause t, ())

updateTimer :: PlaybackState -> IORef PlaybackTimer -> IO ()
updateTimer state timerRef = case state of
  Pause t -> pausePlayback timerRef >> setPosition t timerRef
  Play t -> startPlayback timerRef >> setPosition t timerRef



instance ToJSON PlaybackState where
  toJSON (Pause c) = object [ "state" .= ("pause" :: Text), "pos" .= c]
  toJSON (Play c) = object [ "state" .= ("play" :: Text), "pos" .= c ]

instance FromJSON PlaybackState where
  parseJSON (Object v) =
    (v.:"state" :: Parser Text) >>= \case
      "pause" -> Pause <$> v.:"pos"
      "play" -> Play <$> v.:"pos"
      _ -> error $ "Failed to parse PlaybackState from " ++ show v

  parseJSON invalid =  prependFailure "parsing PlaybackState failed, " (typeMismatch "Object" invalid)
