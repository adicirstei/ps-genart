module Lines (Model, Line, Point, sim) where

import Color
import Color.Scheme.HTML
import Graphics.Canvas
import Prelude
import Simulation

import Effect (Effect)
import Math (cos, max, pi, sin)


type Point = 
  { x:: Number 
  , y :: Number
  }


type Line =
  { start :: Point 
  , end :: Point
  }

type Model = 
  { line :: Line
  , angle :: Number
  , color :: Color
  }

startLine :: Line
startLine =
  { start : {x:400.0, y: 300.0}
  , end : {x: 420.0, y: 320.0}
  }


fade :: Color -> Color
fade c = 
  let r = toHSVA c
  in hsva r.h r.s r.v (max 0.0 (r.a - 0.001) )

toBlue :: Color -> Color
toBlue c =
  let r = toRGBA' c
  in rgba' (max 0.0 (r.r - 0.004)) (max 0.0  (r.g - 0.008)) (min 1.0 (r.b + 0.004)) r.a



initialModel :: Model
initialModel = 
  { line: startLine
  , angle : pi /6.0 + 0.1
  , color: hsl 0.0 1.0 0.5
  }

stepModel :: Model -> Model
stepModel m = 
  m { color = toBlue m.color 
    , line = connectLine m.line (scaleLine 1.01 (rotateLine m.angle m.line) )
    }

connectLine :: Line -> Line -> Line
connectLine {start:_, end: pE} l2 = startLineFrom pE l2

startLineFrom :: Point -> Line -> Line
startLineFrom startPoint@{x:x0,y:y0} {start:{x:xS, y:yS}, end:{x:xE, y:yE}} =
  {start:startPoint, end: {x:x0+xE - xS, y: y0 + yE - yS}}



rotateLine :: Number -> Line -> Line
rotateLine a {start:p, end:q} =
  {start:p, end:{x: x'+p.x, y: y' + p.y}}
  where
    x0 = q.x - p.x
    y0 = q.y - p.y
    x' = x0 * cos a - y0 * sin a
    y' = x0 * sin a + y0 * cos a

scaleLine :: Number -> Line -> Line
scaleLine factor {start:p, end:q} =
  {start:p, end:{x: x'+p.x, y: y' + p.y}}
  where
    x0 = q.x - p.x
    y0 = q.y - p.y
    x' = x0 * factor
    y' = y0 * factor




renderModel :: Context2D -> Model -> Effect Unit
renderModel ctx m = do
  setStrokeStyle ctx (cssStringHSLA  m.color)
  strokePath ctx 
    do 
      moveTo ctx (m.line.start.x) (m.line.start.y)
      lineTo ctx (m.line.end.x) (m.line.end.y)
 

sim :: Simulation Model
sim = 
  { initialValue: initialModel
  , render: renderModel
  , step: stepModel }

    
      

