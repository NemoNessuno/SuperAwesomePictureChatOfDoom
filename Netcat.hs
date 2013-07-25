module Main where

import Control.Concurrent

import Network

import System.Environment
import System.Exit
import System.IO

import Text.Printf

main = getArgs >>= \args -> 
  case args of
    [host, port] -> withSocketsDo (main' host (readPortNumber port))
    _ -> getProgName >>= printf "Usage: %s <host> <port>\n"
  where
    readPortNumber = PortNumber . fromIntegral . read
    main' host port = do
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
    

