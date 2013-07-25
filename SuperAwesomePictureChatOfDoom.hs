{-# LANGUAGE Haskell2010 #-}

module Main where

import NetworkClient
import DrawingWindow

import Text.Printf

import System.Environment
import System.Exit
import System.IO

main = getArgs >>= \args ->
  case args of
    [host, port, username] -> startClient host port >> showDrawingWindow
    _ -> getProgName >>= printf "Usage: %s <host> <port> <username>\n"
