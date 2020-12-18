module Main where

import Import
import Application

main :: IO ()
main = createFoundation >>= warp 4242
