module Main exposing (..)

import Simulation
import Html.App as App


(=>) =
    (,)

type alias Point =
    (Float, Float)

type alias Block =
    { top : Float
    , left : Float
    , right : Float
    , bot : Float
    }

type alias Model =
    { blocks : List Block
    }

inside block (x,y) =
    block.left <= x && x <= block.right
        && block.bot <= y && y <= block.top

separated block1 block2 =
    block1.bot > block2.top || block1.top < block2.bot
        || block1.right < block2.left || block1.left > block2.right

collided block = not << separated block

hit_wall blocks r (x,y) =
    let
        block = { top = y+r
                , left = x-r
                , right = x+r
                , bot = y-r
                }
    in
        List.any (collided block) blocks

range (t0,t1) = {min=t0,max=t1}

ranges = { t = range (0,50)
         , s = range (0,10)
         , v = range (-4,4)
         }

position1 =
    Simulation.moving
        [ 5 => ( 5, 7 )
        , 10 => ( 9, 9 )
        , 12 => (3,7)
        , 15 => (7,3)
        , 16 => (7,4)
        , 17 => (7,6)
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
    position1



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
