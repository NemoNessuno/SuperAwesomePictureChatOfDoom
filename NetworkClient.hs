module NetworkClient (startClient) where

import Control.Concurrent

import Network

import System.Environment
import System.Exit
import System.IO

startClient host port = withSocketsDo (handleConnection host (readPortNumber port))

readPortNumber = PortNumber . fromIntegral . read
 
handleConnection host port = do
  conn <- connectTo host port
  exit <- newEmptyMVar
  forkIO $ readFromSendTo exit conn stdout
  forkIO $ readFromSendTo exit stdin conn
  takeMVar exit

readFromSendTo :: MVar () -> Handle -> Handle -> IO ()
readFromSendTo exit from to = do
  hIsEOF from >>= \c -> case c of
    True  -> hClose from >> putMVar exit ()
    False -> do
      hGetLine from >>= hPutStrLn to
      hFlush to
      readFromSendTo exit from to
