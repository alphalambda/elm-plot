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

range (t0,t1) = {min=t0,max=t1}

ranges = { t = range (0,50)
         , s = range (0,10)
         , v = range (-4,4)
         }

position time =
    let
        r =
            4

        t =
            time * 36

        c =
            cos << degrees

        s =
            sin << degrees

        x =
            5 + r * c t

        y =
            5 + r * s t
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
