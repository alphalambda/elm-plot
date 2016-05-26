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
        blocks = [ Block 7.5 2.5 3 0
                 , Block 10 7 7.5 2.5
                 ]
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

position =
    Moving.moving
        [ 0 => (0,0)
        , 10 => (3,9.5)
        , 15 => (7.5,0.5)
        , 20 => (8,5)
        , 30 => (0,10)
        , 50 => (10,10)
        ]

-----------------------------------------------------------------------------
--- MSG
-----------------------------------------------------------------------------

type alias Msg = Simulation.Msg

-----------------------------------------------------------------------------
--- VIEW,UPDATE
-----------------------------------------------------------------------------

crashed model =
    Block.hit_wall model.blocks 0.1 (Simulation.lastPos model.simu)

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
