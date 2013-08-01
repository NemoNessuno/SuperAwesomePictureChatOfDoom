module Types where

import Graphics.Gloss (Color, rgbaOfColor)

data Shape = SLine | SCircle | SRectangle deriving (Show, Read)

data Object = Object {
    objectColor :: (Float, Float, Float, Float)
  , sposX :: Float
  , sposY :: Float
  , eposX :: Float
  , eposY :: Float
  , shape :: Shape
} deriving (Show, Read)
