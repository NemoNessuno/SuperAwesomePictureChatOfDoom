{-# LANGUAGE Haskell2010 #-}

module Main where

import NetworkClient
import DrawingWindow

import Text.Printf

import System.Environment
import System.Exit
import System.IO

import Control.Concurrent

main = do
  args <- getArgs
  case args of
      [host, port, username] -> do
        sendMVar <- newEmptyMVar
        getMVar <- newEmptyMVar
        exit <- newEmptyMVar
        forkIO $ startClient host port getMVar sendMVar
        forkIO $ showDrawingWindow getMVar sendMVar
        takeMVar exit
      _  -> getProgName >>= printf "Usage: %s <host> <port> <username>\n"
