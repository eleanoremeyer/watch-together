{-# LANGUAGE RecordWildCards   #-}

module Handler.Video where

import Import
import Network.Mime (defaultMimeLookup)
import System.FilePath.Posix

getVideoR :: Handler Html
getVideoR = do
  App {..} <- getYesod
  sendFile (defaultMimeLookup $ pack $ takeFileName appVideoFile) appVideoFile
