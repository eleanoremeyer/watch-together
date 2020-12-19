{-# LANGUAGE NoImplicitPrelude #-}

module Import (module Import) where

import Yesod as Import
import Datatypes as Import
import Foundation as Import
import Data.Text as Import (Text, pack, unpack, concat, lines)
import Data.Text.IO as Import (readFile)
import Prelude as Import hiding (map, concat, lines, readFile, (++))
import Control.Monad as Import
import Data.Default as Import
import Data.IORef as Import (readIORef, writeIORef)
import UnliftIO.STM as Import (atomically)
import Control.Concurrent.STM.TChan as Import (readTChan, dupTChan, TChan, writeTChan)
import Control.Monad.Logger as Import (logInfoN, logErrorN)
import UnliftIO.Exception as Import (catch)
import UnliftIO.Async as Import (race_)

(++) :: Monoid m => m -> m -> m
(++) = (<>)

map :: Functor f => (a -> b) -> f a -> f b
map = fmap
