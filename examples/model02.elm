
import Plot
import Simulation

import Html.App as App

type alias Point = (Float,Float)

type alias Model =
  { pos : Point
  , vel : Point
  }

(+|) (x1,y1) (x2,y2) = (x1+x2,y1+y2)

position time =
  begin (-5,7) 5 time
  +| move (-5,7) (9,-9) 5 7 time
  +| move (9,-9) (7,3) 7 20 time
  +| move (7,3) (-9,-7) 20 30 time
  +| move (-9,-7) (3,8) 30 60 time
  +| stay (3,8) 60 time

move (x0,y0) (x1,y1) t0 t1 t =
  let
    t' = (t-t0)/(t1-t0)
    x = x0 + t'*(x1-x0)
    y = y0 + t'*(y1-y0)
  in
    if t0 > t || t >= t1 then (0,0)
    else (x,y)

begin (x0,y0) t0 t =
  if t < t0 then (x0,y0)
  else (0,0)
  
stay (x0,y0) t0 t =
  if t >= t0 then (x0,y0)
  else (0,0)
    
-----------------------------------------------------------------------------
--- MAIN
-----------------------------------------------------------------------------

main =
  App.program { init = Simulation.init (position 0) ! []
              , update = Simulation.update position
              , view = Simulation.view
              , subscriptions = Simulation.subs
              }

-----------------------------------------------------------------------------
--- HELPERS
-----------------------------------------------------------------------------
