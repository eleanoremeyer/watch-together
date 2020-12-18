{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Watch where

import Import

getWatchR :: Handler Html
getWatchR = do
  App {..} <- getYesod
  sendFile "video/mp4" appVideoFile
