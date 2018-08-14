module Main where


import Graphics.Canvas(getCanvasElementById)
import Prelude


import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)

import Simulation(simulate)
import Koch(sim) as S



main :: Effect Unit
main = do
  mc <- getCanvasElementById "genart"
  case mc of 
    Just c -> simulate c S.sim
    Nothing -> log "canvas element not found"





