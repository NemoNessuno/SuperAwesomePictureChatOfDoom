module StringSplitter (splitString) where

{-  Incoroprate 4 cases:
 -  [User-Name]
 -  [Chat]
 -  [Draw]
 -  [Guess]
 - -}

splitString :: String -> (String, String)
splitString string = 
