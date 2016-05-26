module Main exposing (..)

import Plot
import Simulation
import Html.App as App


type alias Point =
    ( Float, Float )

range : (Float,Float) -> Simulation.Range
range (t0,t1) = {min=t0,max=t1}

opts : Simulation.Settings
opts = { t = range (0,50)
       , s = range (0,10)
       , v = range (-4,4)
       , width = 320
       , height = 320
       , pos = position 0
       }

position time =
    let
        vx =
            -0.2

        vy =
            50 / 125

        g =
            1 / 125

        time2 =
            time * time

        x =
            10 + vx * time

        y =
            vy * time - g * time2
    in
        ( x, y )



-----------------------------------------------------------------------------
--- MAIN
-----------------------------------------------------------------------------


main =
    App.program
        { init = Simulation.initial opts ! Simulation.cmds
        , update = Simulation.update position
        , view = Simulation.view
        , subscriptions = Simulation.subs
        }



-----------------------------------------------------------------------------
--- HELPERS
-----------------------------------------------------------------------------
