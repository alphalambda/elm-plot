module Main exposing (..)

import Simulation
import Html.App as App


(=>) =
    (,)

ranges = { t = (0,50)
         , s = (0,10)
         , v = (-4,4)
         }

position1 =
    Simulation.moving
        [ 5 => ( 5, 7 )
        , 7 => ( 9, 9 )
        , 20 => ( 7, 3 )
        , 30 => ( 9, 7 )
        , 45 => ( 3, 8 )
        ]


position2 =
    Simulation.moving
        [ 3 => ( 2, 5 )
        , 4 => ( 2, 9 )
        ]


position3 =
    Simulation.moving
        [ 1 => ( 1, 10 )
        , 2 => ( 2, 9 )
        , 3 => ( 2, 7 )
        , 4 => ( 4, 4 )
        , 5 => ( 5, 6 )
        , 6 => ( 8, 7 )
        , 7 => ( 9, 1 )
        ]


position4 =
    Simulation.moving
        [ 5 => ( 0, 0 )
        , 10 => ( 5, 0 )
        , 15 => ( 0, 0 )
        ]


position =
    position3



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
