module Main exposing (..)

import Plot
import Simulation
import Html.App as App


type alias Point =
    ( Float, Float )


type alias Model =
    { pos : Point
    , vel : Point
    }

ranges = { t = (0,50)
         , s = (0,10)
         , v = (-4,4)
         }

position time =
    let
        vx =
            0.2

        vy =
            50 / 125

        g =
            1 / 125

        time2 =
            time * time

        x =
            vx * time

        y =
            vy * time - g * time2
    in
        ( x, y )



-----------------------------------------------------------------------------
--- MAIN
-----------------------------------------------------------------------------


main =
    App.program
        { init = Simulation.init ranges (position 0) ! Simulation.cmds
        , update = Simulation.update position
        , view = Simulation.view
        , subscriptions = Simulation.subs
        }



-----------------------------------------------------------------------------
--- HELPERS
-----------------------------------------------------------------------------
