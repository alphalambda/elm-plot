module Simulation
    exposing
        ( Settings
        , Range
        , Msg
        , Model
        , initial
        , update
        , view
        , mainview
        , subs
        , cmds
        )

import Axis
import Flow exposing (px, (=>))
import Planum
import Plot
import AnimationFrame
import Color exposing (red, green, blue, black)
import Html exposing (body, h1, div, text, hr, button)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List
import Task
import Window exposing (Size)


-----------------------------------------------------------------------------
--- MODEL
-----------------------------------------------------------------------------

type alias Point =
    ( Float, Float )

type alias Range =
    { min : Float, max : Float }

type Msg
    = Plot (Plot.Msg Id)
    | Tick Float
    | WindowSize Size
    | Ignore
    | Reset
    | Pause
    | Resume
    | Step

type alias Model =
    -- Parameters
    { trange : Range
    , srange : Range
    , vrange : Range
    
    -- Model variables
    , time : Float
    , position : Point
    , velocity : Point
    , xyPoints : List Point
    
    -- Analysis
    , xpPoints : List Point
    , ypPoints : List Point
    , xvPoints : List Point
    , yvPoints : List Point

    -- User interface
    , xyPlot : Plot.Model Id
    , xpPlot : Plot.Model Id
    , xvPlot : Plot.Model Id
    , ypPlot : Plot.Model Id
    , yvPlot : Plot.Model Id
    , xyGraph : Plot.Graph
    , xpGraph : Plot.Graph
    , xvGraph : Plot.Graph
    , ypGraph : Plot.Graph
    , yvGraph : Plot.Graph
    , winsize : Size
    , status : Status
    , message : String
    }

type Status = Paused | Running | Finished

type Id
    = XP
    | YP
    | XV
    | YV
    | XY

type alias Settings =
    { width : Float
    , height : Float
    , t : Range
    , s : Range
    , v : Range
    , pos : Point
    }

initial : Settings -> Model
initial d =
    { time = d.t.min
    , position = (0,0)
    , velocity = (0,0)
    , xyPlot = Plot.init XY |> Plot.mouseEnable
    , xpPlot = Plot.init XP |> Plot.mouseEnable
    , ypPlot = Plot.init YP |> Plot.mouseEnable
    , xvPlot = Plot.init XV |> Plot.mouseEnable
    , yvPlot = Plot.init YV |> Plot.mouseEnable
    , xyPoints = []
    , xpPoints = []
    , xvPoints = []
    , ypPoints = []
    , yvPoints = []
    , trange = d.t
    , srange = d.s
    , vrange = d.v
    , xyGraph = xyGraph d.width d.height d.s
    , xpGraph = make_graph d.width d.height d.t d.s (label2 hlabel tpos)
    , ypGraph = make_graph d.width d.height d.t d.s (label2 vlabel tpos)
    , xvGraph = make_graph d.width d.height d.t d.v (label2 hlabel tvel)
    , yvGraph = make_graph d.width d.height d.t d.v (label2 vlabel tvel)
    , winsize = Size 0 0
    , status = Running
    , message = ""
    }
    |> reset d.t.min d.pos

reset t0 ( x, y ) model =
    { model
        | time = t0
        , position = (x,y)
        , velocity = (0,0)
        , xyPoints = [ ( x, y ) ]
        , xpPoints = [ ( t0, x ) ]
        , xvPoints = []
        , ypPoints = [ ( t0, y ) ]
        , yvPoints = []
        , status =
            if model.status == Finished then Running
            else model.status
    }


{-
lastPos model =
    case model.xyPoints of
        ( x, y ) :: _ ->
            ( x, y )

        [] ->
            ( 0, 0 )
-}

-----------------------------------------------------------------------------
--- GRAPHS
-----------------------------------------------------------------------------


hlabel =
    "East-West"


vlabel =
    "North-South"


tpos =
    "Position"


tvel =
    "Velocity"


label2 l1 l2 =
    l1 ++ " " ++ l2

xyGraph width height srange =
    let
        srange' = (srange.min,srange.max)
    in
        Plot.size (2 * width) (2 * height)
            |> Plot.ranges srange' srange'
            |> Plot.make
            |> Plot.frame
            |> Plot.grid 4 4
            |> Plot.vbounds
            |> Plot.hbounds
            |> Plot.hlabel (label2 hlabel tpos)
            |> Plot.vlabel (label2 vlabel tpos)

make_graph : Float -> Float -> Range -> Range -> String -> Plot.Graph
make_graph width height trange range slabel =
    Plot.size width height
        |> Plot.ranges (trange.min,trange.max) (range.min,range.max)
        |> Plot.make
        |> Plot.frame
        |> Plot.grid 5 10
        |> Plot.vlabel slabel
        |> Plot.hlabel "Time"
        |> Plot.vbounds
        |> Plot.hbounds

-----------------------------------------------------------------------------
--- UPDATE and VIEW
-----------------------------------------------------------------------------


subs model = case model.status of
    Paused -> Window.resizes WindowSize
    Finished -> Window.resizes WindowSize
    Running ->
        Sub.batch
            [ Window.resizes WindowSize
            , AnimationFrame.diffs Tick
            ]

cmds =
    [ Task.perform (\_ -> Ignore) WindowSize Window.size ]


update position msg model =
    let
        get g { x, y } =
            g.plot.fromScreen (x,y)
    in
        case msg of
            WindowSize size ->
                { model
                | winsize = size
                -- , message = "Window size now " ++ toString size
                } ! []

            Plot msg' ->
                let
                    xyPlot =
                        Plot.update (get model.xyGraph) msg' model.xyPlot
                        |> fst

                    xpPlot =
                        Plot.update (get model.xpGraph) msg' model.xpPlot
                        |> fst

                    ypPlot =
                        Plot.update (get model.ypGraph) msg' model.ypPlot
                        |> fst

                    xvPlot =
                        Plot.update (get model.xvGraph) msg' model.xvPlot
                        |> fst

                    yvPlot =
                        Plot.update (get model.yvGraph) msg' model.yvPlot
                        |> fst
                in
                    { model
                        | xpPlot = xpPlot
                        , ypPlot = ypPlot
                        , xvPlot = xvPlot
                        , yvPlot = yvPlot
                        , xyPlot = xyPlot
                        -- , message = "Plot " ++ Plot.showMsg msg'
                    } ! []

            Tick dt ->
                let
                    model' =
                        case model.status of
                            Paused -> model
                            Finished -> model
                            Running ->
                                if
                                    model.time >= model.trange.max
                                then
                                    { model | status = Finished } 
                                else
                                    step position (dt/1000) model
                in
                    model' ! []
                    
            Ignore ->
                model
                -- {model | message = "Ignore" }
                ! []

            Reset ->
                let
                    t0 = model.trange.min
                in
                    reset t0 (position t0) model ! []
                
            Pause ->
                { model
                | status = Paused
                -- , message = "Pause"
                } ! []
                
            Resume ->
                { model
                | status = Running
                -- , message = "Resume"
                } ! []
            
            Step ->
                let
                    model' = case model.status of
                        Finished -> model
                        Running -> {model | status = Paused }
                        Paused ->  step position 0.1 model
                in
                    model' ! []

step position dt model =
    let
        time' =
            model.time + dt

        ( x, y ) = model.position

        ( x', y' ) =
            position time'

        vx =
            (x' - x) / dt

        vy =
            (y' - y) / dt

        model' =
            { model
            | time = time'
            , position = (x',y')
            , velocity = (vx,vy)
            , xyPoints = ( x', y' ) :: model.xyPoints
            , xpPoints = ( time', x' ) :: model.xpPoints
            , ypPoints = ( time', y' ) :: model.ypPoints
            , xvPoints = ( time', vx ) :: model.xvPoints
            , yvPoints = ( time', vy ) :: model.yvPoints
            }
    in
        model'
        -- { model' | message = "Tick " ++ toString (Axis.approx 1 dt) }

view model =
    let
        pad =
            60

        pos =
            model.position

        xyGraph' =
            model.xyGraph
                |> Plot.line green model.xyPoints
                |> Plot.shadow green pos
                |> Plot.scatter (Plot.dot black) [ pos ]
                |> Plot.draw

        -- xyGraph'' = Plot.draw model.xyGraph
        -- xpGraph'' = Plot.draw model.xpGraph
        -- xvGraph'' = Plot.draw model.xvGraph
        -- ypGraph'' = Plot.draw model.ypGraph
        -- yvGraph'' = Plot.draw model.yvGraph

        xpGraph' =
            model.xpGraph
                |> Plot.line red model.xpPoints |> Plot.draw

        ypGraph' =
            model.ypGraph
                |> Plot.line blue model.ypPoints |> Plot.draw

        xvGraph' =
            model.xvGraph
                |> Plot.line red model.xvPoints |> Plot.draw

        yvGraph' =
            model.yvGraph
                |> Plot.line blue model.yvPoints |> Plot.draw

        xyView =
            Plot.view Plot xyGraph' model.xyPlot

        xpView =
            Plot.view Plot xpGraph' model.xpPlot

        ypView =
            Plot.view Plot ypGraph' model.ypPlot

        xvView =
            Plot.view Plot xvGraph' model.xvPlot

        yvView =
            Plot.view Plot yvGraph' model.yvPlot

        ( g0w, g0h ) =
            Plot.getSize xyGraph'

        ( g11w, g11h ) =
            Plot.getSize xpGraph'

        ( g12w, g12h ) =
            Plot.getSize ypGraph'

        ( g21w, g21h ) =
            Plot.getSize xvGraph'

        graph1 =
            Flow.right (g11w + g12w + pad) (g11h + pad) [ xpView, ypView ]

        graph2 =
            Flow.right (g11w + g12w + pad) (g21h + pad) [ xvView, yvView ]

        graph12 =
            div [] [ graph1, graph2 ]

        panel =
            let
                w =
                    model.winsize.width - g11w - g12w - g0w - 2 * pad

                h =
                    g11h + g21h + 2 * pad

                t1 =
                    "Time: " ++ toString (Axis.approx 1 model.time)

                t2 =
                    "Position: " ++ toString (Planum.approx 1 1 pos)

                t3 =
                    "Velocity: " ++ toString (Planum.approx 2 2 model.velocity)

                cstyle =
                    [ "padding" => px 5, "margin" => px 20 ]

                font =
                    [ "font-size" => "200%", "font-weight" => "bold"]
            in
                Flow.down w h
                    [ div [style <| cstyle ++ font] [ text t1 ]
                    , div [style <| cstyle ++ font] [ text t2 ]
                    , div [style <| cstyle ++ font] [ text t3 ]
                    , button [ onClick Reset,style cstyle ] [ text "Reset" ]
                    , if model.status == Paused
                      then
                        button [onClick Resume,style cstyle] [ text "Resume" ]
                      else
                        button [ onClick Pause,style cstyle ] [ text "Pause" ]
                    , button [ onClick Step, style cstyle ] [ text "Step" ]
                    -- , div [] [ text model.message ]
                    ]

        graphs =
            let
                h =
                    g11h + g21h + 2 * pad
            in
                Flow.right model.winsize.width h [ graph12, xyView, panel ]
    in
        graphs

mainview model =
    body []
         [ h1 [] [ text "Simulation" ]
         , hr [] []
         , view model
         , hr [] []
         ]
