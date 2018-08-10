module Simulation where
import Graphics.Canvas(Context2D, CanvasElement, setCanvasDimensions, getContext2D)
import Effect(Effect)
import RAF(requestAnimationFrame)
import Prelude



type Simulation a = 
  { initialValue :: a
  , render :: Context2D -> a -> Effect Unit
  , step :: a -> a
  }

simulate :: forall a. CanvasElement -> Simulation a -> Effect Unit
simulate cvs s = do 

  setCanvasDimensions cvs {width: 800.0, height:600.0}

  ctx <- getContext2D cvs

  requestAnimationFrame (go ctx s.initialValue)
  where 
    go c v = do
      s.render c v 
      requestAnimationFrame (go c (s.step v))