module NetworkClient (startClient) where

import Control.Concurrent

import Network

import System.Environment
import System.Exit
import System.IO

import Types

startClient:: String -> String -> MVar [Object] -> MVar [Object] -> IO ()
startClient host port getMVar sendMVar = withSocketsDo (handleConnection host (readPortNumber port) getMVar sendMVar)

readPortNumber = PortNumber . fromIntegral . read

handleConnection host port getMVar sendMVar = do
  conn <- connectTo host port
  exit <- newEmptyMVar
  forkIO $ readNetwork exit conn getMVar
  forkIO $ sendNetwork sendMVar conn
  takeMVar exit

readNetwork :: MVar () -> Handle -> MVar [Object] -> IO ()
readNetwork exit net fromNetVar = do
  hIsEOF net >>= \bool -> case bool of
    True -> hClose net >> putMVar exit ()
    False -> do
      line <- hGetLine net
      putMVar fromNetVar (read line)
      readNetwork exit net fromNetVar

sendNetwork :: MVar [Object] -> Handle -> IO ()
sendNetwork toNetVar net = do
  toSend <- takeMVar toNetVar
  hPutStrLn net (show toSend)
  hFlush net
  sendNetwork toNetVar net
