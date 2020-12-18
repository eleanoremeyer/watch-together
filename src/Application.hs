{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application where

import Import
import Handler.Watch

mkYesodDispatch "App" resourcesApp
