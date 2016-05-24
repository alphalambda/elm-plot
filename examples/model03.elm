
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
    r = 4
    t = time * 36
    c = cos << degrees
    s = sin << degrees
    x = 5 + r * c t
    y = 5 + r * s t
  in
    (x,y)

-----------------------------------------------------------------------------
--- MAIN
-----------------------------------------------------------------------------

main =
  App.program { init = Simulation.init (position 0) ! Simulation.cmds
              , update = \msg model -> Simulation.update position msg model ! []
              , view = Simulation.view
              , subscriptions = Simulation.subs
              }

-----------------------------------------------------------------------------
--- HELPERS
-----------------------------------------------------------------------------
