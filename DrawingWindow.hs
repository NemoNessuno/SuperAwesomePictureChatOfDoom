module DrawingWindow (showDrawingWindow) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Types

import Control.Concurrent (MVar, forkIO, takeMVar, putMVar)
import Control.Monad(forever)

width = 800
height = 600

data World = World {
    worldObjects   :: [Object]
  , foreignObjects :: [Object]
  , worldShape     :: Shape
  , worldColor     :: (Float, Float, Float, Float)
  , worldStartX    :: Float
  , worldStartY    :: Float
  , getMVar        :: MVar [Object]
  , sendMVar       :: MVar [Object]
 }

mkObject :: (Float, Float, Float, Float) -> Float -> Float -> Float -> Float -> Shape -> Object
mkObject oColor sx sy ex ey oshape = Object {
    objectColor = oColor
  , sposX = sx
  , sposY = sy
  , eposX = ex
  , eposY = ey
  , shape = oshape
 }

type DeltaSeconds = Float

showDrawingWindow getMVar sendMVar = do
  let world = freshWorld getMVar sendMVar

  forkIO $ readFromMVar world
  
  play displayMode black 100 world renderWorld handleEvent (\_ x -> x)
    where
      displayMode :: Display
      displayMode = InWindow "Super awesome picture chat of doom" (floor width, floor height) (100, 100)
        
freshWorld :: MVar [Object] -> MVar [Object] -> World
freshWorld g s = World {
  worldObjects   = [],
  foreignObjects = [],
  worldShape     = SCircle,
  worldColor     = rgbaOfColor white,
  worldStartX    = 0,
  worldStartY    = 0,
  getMVar        = g,
  sendMVar       = s
  }

readFromMVar world = forever $ do
                    foreignObjects' <- takeMVar (getMVar world)
                    return world { foreignObjects = foreignObjects'}

renderWorld :: World -> Picture
renderWorld world = Pictures $ (map renderObject (worldObjects world)) 
  where
    renderObject object = Color (makeColor r g b a ) $ buildPicture object
      where
        r = (\(x,_,_,_) -> x) (objectColor object)
        g = (\(_,x,_,_) -> x) (objectColor object)
        b = (\(_,_,x,_) -> x) (objectColor object)
        a = (\(_,_,_,x) -> x) (objectColor object)

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
        Char char -> world { worldShape = shape', worldColor = color' }
          where 
            shape' = case char of
                  '1' -> SLine
                  '2' -> SCircle
                  '3' -> SRectangle
                  _ -> worldShape world
            color' = case char of
                  'r' -> rgbaOfColor red
                  'g' -> rgbaOfColor green
                  'b' -> rgbaOfColor blue
                  'w' -> rgbaOfColor white
                  _ -> worldColor world
        SpecialKey key -> world
        MouseButton button -> case (button, state) of
          (LeftButton, Down) -> world {worldStartX = x, worldStartY = y}
          (LeftButton, Up)   -> do
                              let objects' = mkObject (worldColor world) (worldStartX world) (worldStartY world) x y (worldShape world) : worldObjects world
                              putMVar >>= sendMVar world . objects'
                              return world { worldObjects = objects' }
          (RightButton, Up)  -> do
                              let objects' = if length (worldObjects world) > 0 then tail (worldObjects world) else worldObjects world
                              putMVar (sendMVar world) objects'
                              return world { worldObjects = objects'} 
          _ -> world
    (EventMotion (x, y)) -> world
