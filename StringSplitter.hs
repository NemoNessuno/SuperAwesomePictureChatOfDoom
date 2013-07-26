{-# LANGUAGE ViewPatterns #-}

module StringSplitter (splitString) where

import Data.Text as T

splitString :: String -> (String, String)
splitString (stripPrefix $ pack "[User-Name]" -> Just suf) = ("[User-Name]", unpack $ suf)
splitString (stripPrefix $ pack "[Chat]" -> Just suf) = ("[Chat]", unpack $ suf)
splitString (stripPrefix $ pack "[Draw]" -> Just suf) = ("[Draw]", unpack $ suf)
splitString (stripPrefix $ pack "[Guess]" -> Just suf) = ("[Guess]", unpack $ suf)
splitString _ = (" ", " ")
