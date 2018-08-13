module Koch where
import Color
import Color.Scheme.HTML
import Graphics.Canvas
import Prelude
import Simulation

import Data.Int (toNumber)
import Effect (Effect)
import Math (cos, max, pi, sin)
import Data.Array((:))


type Point = 
  { x:: Number 
  , y :: Number
  }


type Line =
  { start :: Point 
  , end :: Point
  }

type Path = Array Point

spiral :: Number -> Number -> Int -> Line -> Path
spiral angle scaleFactor n line
  = spiral' n line
  where
    spiral' n line@{start:p1, end:p2}
      | n <= 0    = []
      | otherwise = (p1 : spiral' (n - 1) newLine)
      where
        newLine = connectLine line (scaleLine scaleFactor (rotateLine angle line))

polygon :: Int -> Line -> Path
polygon n line 
  | n > 2 = spiral rotationAngle 1.0 (n + 1) line
              where 
                rotationAngle = (2.0 * pi) / (toNumber n)
  | otherwise = []
  



kochLine :: Int -> Point -> Point -> Path
kochLine n pS pE 
  | n <= 0 = []
  | otherwise = [pS]  <> kochLine (n-1) pS p1
                      <> kochLine (n-1) p1 p2
                      <> kochLine (n-1) p2 p3
                      <> kochLine (n-1) p3 pE
                      <> [pE]
  where 
    l1@{start:_, end:p1} = scaleLine (1.0/3.0) {start: pS, end: pE}
    l2@{start:_, end:p3} = connectLine l1 l1
    {start:_, end:p2} = rotateLine (5.0 / 3.0 * pi) l2


kochFlake :: Int -> Line -> Path
kochFlake n line = 
  kochLine n p1 p2 <> kochLine n p2 p3 <> kochLine n p3 p1 
  where
    [p1, p2, p3, _] = polygon 3 line


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

    
      

