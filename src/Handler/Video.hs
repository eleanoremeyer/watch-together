{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Video where

import Import

getVideoR :: Handler Html
getVideoR = do
  App {..} <- getYesod
  sendFile "video/mp4" appVideoFile
