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
        rad =
            4
            
        (w,time0) =
            if time < 10 then (36,0)
            else if time < 30 then (18,10)
            else (12,30)

        wt = w * (time-time0)
        
        c =
            cos << degrees

        s =
            sin << degrees

        x =
            5 + rad * c wt

        y =
            5 + rad * s wt
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
