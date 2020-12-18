{-# LANGUAGE NoImplicitPrelude #-}

module Import (module Import) where

import Yesod as Import
import Foundation as Import
import Data.Text as Import (Text, pack, unpack, concat, lines)
import Data.Text.IO as Import (readFile)
import Prelude as Import hiding (map, concat, lines, readFile)
import Control.Monad as Import (forM)
import Data.Default as Import

map :: Functor f => (a -> b) -> f a -> f b
map = fmap
