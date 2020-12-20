{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Import
import Application()
import System.Environment
import System.FilePath
import Network.Mime
import Control.Exception
import System.Directory

displayHelp :: IO ()
displayHelp = putStrLn "Usage: watch-together videofile port"

main :: IO ()
main = do
  args <- try $ do
    (videoFile:port:_) <- getArgs
    pure (videoFile, read port)

  case args of
    Left (SomeException _) ->
      displayHelp

    Right (videoFile,port) -> do
      b <- doesFileExist videoFile
      if b then do
        let mime = defaultMimeLookup $ pack $ takeFileName videoFile
        putStrLn $ "Mimetype: " ++ show mime
        createFoundation videoFile mime >>= warp port
      else
        putStrLn $ "File " ++ videoFile ++ " does not exist"
