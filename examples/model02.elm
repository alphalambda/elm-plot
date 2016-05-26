module Main exposing (..)

import Block exposing (Block)
import Moving
import Plot
import Simulation

import Color exposing (red,green,blue,black)
import Html.App as App

opts : Simulation.Settings
opts = { t = range (0,50)
       , s = range (0,10)
       , v = range (-4,4)
       , width = 320
       , height = 320
       , pos = position 0
       }

-----------------------------------------------------------------------------
--- MODEL
-----------------------------------------------------------------------------

type alias Model =
    { blocks : List Block
    , simu : Simulation.Model
    , crashed : Bool
    }

init =
    let
        simu = Simulation.initial opts
        blocks = []
    in
        { blocks = blocks
        , simu = simu
        , crashed = False
        }
        ! Simulation.cmds

paint_blocks color model =
    let
        xyGraph = model.simu.xyGraph
            |> Plot.blocks model.blocks color
        model_simu = model.simu
        simu = { model_simu | xyGraph = xyGraph }
    in
        { model | simu = simu }

-----------------------------------------------------------------------------
--- Data
-----------------------------------------------------------------------------

position1 =
    Moving.moving
        [ 0 => ( 5, 7 )
        , 5 => ( 9, 9 )
        , 10 => ( 9, 9 )
        , 12 => (3,7)
        , 15 => (7,3)
        , 16 => (7,4)
        , 17 => (7,6)
        , 20 => ( 7, 3 )
        , 22 => ( 7, 3 )
        , 30 => ( 9, 7 )
        , 45 => ( 3, 8 )
        ]


position2 =
    Moving.moving
        [ 3 => ( 2, 5 )
        , 4 => ( 2, 9 )
        ]


position3 =
    Moving.moving
        [ 1 => ( 1, 10 )
        , 2 => ( 2, 9 )
        , 3 => ( 2, 7 )
        , 4 => ( 4, 4 )
        , 5 => ( 5, 6 )
        , 6 => ( 8, 7 )
        , 7 => ( 9, 1 )
        ]


position4 =
    Moving.moving
        [ 5 => ( 0, 0 )
        , 10 => ( 5, 0 )
        , 15 => ( 0, 0 )
        ]


position =
    position1

-----------------------------------------------------------------------------
--- MSG
-----------------------------------------------------------------------------

type alias Msg = Simulation.Msg

-----------------------------------------------------------------------------
--- VIEW,UPDATE
-----------------------------------------------------------------------------

crashed model =
    Block.hit_wall model.blocks 0.1 model.simu.position

update msg model =
    if model.crashed || crashed model
    then { model | crashed = True } ! []
    else
        let
            (simu,cmd) = Simulation.update position msg model.simu
        in
            ({model | simu = simu}, cmd)

view model =
    let
        model' =
            if model.crashed
            then paint_blocks red model
            else paint_blocks black model
    in
        Simulation.view model'.simu

-----------------------------------------------------------------------------
--- SUBS
-----------------------------------------------------------------------------

subs model =
    if model.crashed then Sub.none
    else Simulation.subs model.simu

-----------------------------------------------------------------------------
--- MAIN
-----------------------------------------------------------------------------

main =
    App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        }

-----------------------------------------------------------------------------
--- HELPERS
-----------------------------------------------------------------------------

range : (Float,Float) -> Simulation.Range
range (t0,t1) = {min=t0,max=t1}

(=>) =
    (,)

type alias Point =
    (Float, Float)
