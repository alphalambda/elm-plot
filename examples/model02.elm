
import Simulation
import Html.App as App

(=>) = (,)

position1 =
  Simulation.moving
    [  5 => (5,7)
    ,  7 => (9,9)
    , 20 => (7,3)
    , 30 => (9,7)
    , 45 => (3,8)
    ]

position2 =
  Simulation.moving
    [ 3 => (2,5)
    , 4 => (2,9)
    ]

position3 =
  Simulation.moving
    [ 1 => (1,10)
    , 2 => (2,9)
    , 3 => (2,7)
    , 4 => (4,4)
    , 5 => (5,6)
    , 6 => (8,7)
    , 7 => (9,1)
    ]

position4 =
  Simulation.moving
    [ 5 => (0,0)
    , 10 => (5,0)
    , 15 => (0,0)
    ]
    
position = position1

-----------------------------------------------------------------------------
--- MAIN
-----------------------------------------------------------------------------

main =
  let
    update msg model = Simulation.update position msg model ! []
  in
    App.program
      { init = Simulation.init (position 0) ! Simulation.cmds
      , update = update
      , view = Simulation.view
      , subscriptions = Simulation.subs
      }

-----------------------------------------------------------------------------
--- HELPERS
-----------------------------------------------------------------------------
