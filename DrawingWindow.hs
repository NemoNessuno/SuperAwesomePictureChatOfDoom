module DrawingWindow (showDrawingWindow) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

width = 800
height = 600

data Shape = SLine | SCircle | SRectangle

data World = World {
    worldObjects :: [Object]
  , worldShape :: Shape
  , worldStartX :: Float
  , worldStartY :: Float
 }

data Object = Object {
    objectColor :: Color
  , sposX :: Float
  , sposY :: Float
  , eposX :: Float
  , eposY :: Float
  , shape :: Shape
 }

mkObject :: Float -> Float -> Float -> Float -> Shape -> Object
mkObject sx sy ex ey oshape = Object {
    objectColor = white
  , sposX = sx
  , sposY = sy
  , eposX = ex
  , eposY = ey
  , shape = oshape
 }

type DeltaSeconds = Float

showDrawingWindow = play displayMode black 100 freshWorld renderWorld handleEvent (\_ x -> x)
       where
        displayMode :: Display
        displayMode = InWindow "Super awesome picture chat of doom" (floor width, floor height) (100, 100)
        
freshWorld :: World
freshWorld = World {
  worldObjects = [],
  worldShape = SCircle,
  worldStartX = 0,
  worldStartY = 0
  }

renderWorld :: World -> Picture
renderWorld world = Pictures $ (map renderObject (worldObjects world)) 
  where
    renderObject object = Color (objectColor object) $ buildPicture object

buildPicture object = let
            sx = sposX object
            ex = eposX object
            sy = sposY object
            ey = eposY object
            in
              case shape object of
              SCircle -> Translate sx sy $ Circle $ apyth sx ex sy ey
              SLine -> Line [(sx,sy), (ex, ey)]
              SRectangle -> Line [(sx,sy), (sx,sy - (sy - ey)), (sx - (sx -ex),sy - (sy-ey)), (sx - (sx -ex),sy), (sx,sy)]

apyth sx ex sy ey = sqrt ((sx - ex)**2 + (sy - ey)**2)

handleEvent :: Event -> World -> World
handleEvent ev world = case ev of
    (EventKey key state _ (x, y)) -> case key of
        Char char -> world { worldShape = shape' }
          where shape' = case char of
                  '1' -> SLine
                  '2' -> SCircle
                  '3' -> SRectangle
                  _ -> worldShape world
        SpecialKey key -> world
        MouseButton button -> case (button, state) of
          (LeftButton, Down) -> world {worldStartX = x, worldStartY = y}
          (LeftButton, Up) -> world { worldObjects = objects' }
              where objects' = mkObject (worldStartX world) (worldStartY world) x y (worldShape world) : worldObjects world
          (RightButton, Up) -> world { worldObjects = if length (worldObjects world) > 0 then tail (worldObjects world) else worldObjects world }
          _ -> world
    (EventMotion (x, y)) -> world
