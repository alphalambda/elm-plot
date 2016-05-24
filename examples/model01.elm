
import Plot
import Simulation

import Html.App as App

type alias Point = (Float,Float)

type alias Model =
  { pos : Point
  , vel : Point
  }


position time =
  let
    x = -10 + time/3
    y = -10 + time/3
  in
    (x,y)

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
