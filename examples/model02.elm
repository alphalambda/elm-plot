
import Simulation
import Html.App as App

(=>) = (,)

position1 =
  Simulation.moving
    [  5 => (-5,7)
    ,  7 => (-9,9)
    , 20 => (7,3)
    , 30 => (-9,-7)
    , 60 => (3,8)
    ]

position2 =
  Simulation.moving
    [ 3 => (2,-5)
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
    , 7 => (-9,-1)
    ]
    
position4 =
  let
    f t = t => (t/10*cos t,t/10*sin t)
  in
    Simulation.moving <| List.map f [1..60]

position = position4

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
