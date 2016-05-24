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
        { init = Simulation.init (position 0) ! Simulation.cmds
        , update = \msg model -> Simulation.update position msg model ! []
        , view = Simulation.view
        , subscriptions = Simulation.subs
        }



-----------------------------------------------------------------------------
--- HELPERS
-----------------------------------------------------------------------------
