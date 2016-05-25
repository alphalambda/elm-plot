module Simulation
    exposing
        ( Range
        , init
        , update
        , view
        , subs
        , cmds
        , moving
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
--- MODEL DATA
-----------------------------------------------------------------------------


width =
    300


height =
    300

type alias Point =
    ( Float, Float )

type alias Range =
    { min : Float, max : Float }

-----------------------------------------------------------------------------
--- DATA
-----------------------------------------------------------------------------

type Msg
    = Plot (Plot.Msg Id)
    | Tick Float
    | WindowSize Size
    | Ignore
    | Reset
    | Pause
    | Resume
    | Step

type alias Data =
    -- Parameters
    { trange : Range
    , srange : Range
    , vrange : Range
    
    -- Model variables
    , time : Float
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

initial : (Range,Range,Range) -> Data
initial (t,s,v) =
    { time = t.min
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
    , trange = t
    , srange = s
    , vrange = v
    , xyGraph = xyGraph s
    , xpGraph = make_graph t s (label2 hlabel tpos)
    , ypGraph = make_graph t s (label2 vlabel tpos)
    , xvGraph = make_graph t v (label2 hlabel tvel)
    , yvGraph = make_graph t v (label2 vlabel tvel)
    , winsize = Size 0 0
    , status = Running
    , message = ""
    }


init {t,s,v} pos =
    initial (t,s,v) |> reset t.min pos


reset t0 ( x, y ) data =
    { data
        | time = t0
        , xyPoints = [ ( x, y ) ]
        , xpPoints = [ ( t0, x ) ]
        , xvPoints = []
        , ypPoints = [ ( t0, y ) ]
        , yvPoints = []
        , status =
            if data.status == Finished then Running
            else data.status
    }


lastPos data =
    case data.xyPoints of
        ( x, y ) :: _ ->
            ( x, y )

        [] ->
            ( 0, 0 )



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

xyGraph srange =
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

make_graph : Range -> Range -> String -> Plot.Graph
make_graph trange range slabel =
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


subs data = case data.status of
    Paused -> Window.resizes WindowSize
    Finished -> Window.resizes WindowSize
    Running ->
        Sub.batch
            [ Window.resizes WindowSize
            , AnimationFrame.diffs Tick
            ]

cmds =
    [ Task.perform (\_ -> Ignore) WindowSize Window.size ]


update position msg data =
    let
        approx =
            Planum.approx 2 2

        get g { x, y } =
            ( x, y ) |> g.plot.fromScreen |> g.plot.toModel |> approx
    in
        case msg of
            WindowSize size ->
                { data
                | winsize = size
                -- , message = "Window size now " ++ toString size
                } ! []

            Plot msg' ->
                let
                    xyPlot =
                        Plot.update (get data.xyGraph) msg' data.xyPlot
                        |> fst

                    xpPlot =
                        Plot.update (get data.xpGraph) msg' data.xpPlot
                        |> fst

                    ypPlot =
                        Plot.update (get data.ypGraph) msg' data.ypPlot
                        |> fst

                    xvPlot =
                        Plot.update (get data.xvGraph) msg' data.xvPlot
                        |> fst

                    yvPlot =
                        Plot.update (get data.yvGraph) msg' data.yvPlot
                        |> fst
                in
                    { data
                        | xyPlot = xyPlot
                        , xpPlot = xpPlot
                        , ypPlot = ypPlot
                        , xvPlot = xvPlot
                        , yvPlot = yvPlot
                        -- , message = "Plot " ++ Plot.showMsg msg'
                    } ! []

            Tick dt ->
                let
                    data' =
                        case data.status of
                            Paused -> data
                            Finished -> data
                            Running ->
                                if
                                    data.time >= data.trange.max
                                then
                                    { data | status = Finished } 
                                else
                                    step position (dt/1000) data
                in
                    data' ! []
                    
            Ignore ->
                data
                -- {data | message = "Ignore" }
                ! []

            Reset ->
                let
                    t0 = data.trange.min
                in
                    reset t0 (position t0) data ! []
                
            Pause ->
                { data
                | status = Paused
                -- , message = "Pause"
                } ! []
                
            Resume ->
                { data
                | status = Running
                -- , message = "Resume"
                } ! []
            
            Step ->
                step position 0.1
                    { data
                    | status = Paused
                    -- , message = "Step"
                    } ! []

step position dt data =
    let
        time' =
            data.time + dt

        ( x, y ) =
            lastPos data

        ( x', y' ) =
            position time'

        vx =
            (x' - x) / dt

        vy =
            (y' - y) / dt

        data' =
            { data
            | time = time'
            , xyPoints = ( x', y' ) :: data.xyPoints
            , xpPoints = ( time', x' ) :: data.xpPoints
            , ypPoints = ( time', y' ) :: data.ypPoints
            , xvPoints = ( time', vx ) :: data.xvPoints
            , yvPoints = ( time', vy ) :: data.yvPoints
            }
    in
        data'
        -- { data' | message = "Tick " ++ toString (Axis.approx 1 dt) }

view data =
    let
        pad =
            60

        pos =
            lastPos data

        header =
            h1 [] [ text "Simulation" ]

        xyGraph' =
            data.xyGraph
                |> Plot.line green data.xyPoints
                |> Plot.shadow green pos
                |> Plot.scatter (Plot.dot black) [ pos ]
                |> Plot.draw

        xpGraph' =
            data.xpGraph
                |> Plot.line red data.xpPoints |> Plot.draw

        ypGraph' =
            data.ypGraph
                |> Plot.line blue data.ypPoints |> Plot.draw

        xvGraph' =
            data.xvGraph
                |> Plot.line red data.xvPoints |> Plot.draw

        yvGraph' =
            data.yvGraph
                |> Plot.line blue data.yvPoints |> Plot.draw

        xyView =
            Plot.view Plot xyGraph' data.xyPlot

        xpView =
            Plot.view Plot xpGraph' data.xpPlot

        ypView =
            Plot.view Plot ypGraph' data.ypPlot

        xvView =
            Plot.view Plot xvGraph' data.xvPlot

        yvView =
            Plot.view Plot yvGraph' data.yvPlot

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
                    data.winsize.width - g11w - g12w - g0w - 2 * pad

                h =
                    g11h + g21h + 2 * pad

                t1 =
                    "Time: " ++ toString (Axis.approx 1 data.time)

                t2 =
                    "Position: " ++ toString (Planum.approx 1 1 pos)

                buttonstyle =
                    style [ "padding" => px 5, "margin" => px 20 ]

            in
                Flow.down w h
                    [ div [] [ text t1 ]
                    , div [] [ text t2 ]
                    , button [ onClick Reset, buttonstyle ] [ text "Reset" ]
                    , if data.status == Paused
                      then
                        button [onClick Resume,buttonstyle] [ text "Resume" ]
                      else
                        button [ onClick Pause, buttonstyle ] [ text "Pause" ]
                    , button [ onClick Step, buttonstyle ] [ text "Step" ]
                    -- , div [] [ text data.message ]
                    ]

        graphs =
            let
                h =
                    g11h + g21h + 2 * pad
            in
                Flow.right data.winsize.width h [ graph12, xyView, panel ]
    in
        body []
            [ header
            , hr [] []
            , graphs
            , hr [] []
            ]



-----------------------------------------------------------------------------
--- MOVING AT CONSTANT SPEED
-----------------------------------------------------------------------------

moving stations time =
    let
        loop l =
            case l of
                ( t0, p0 ) :: ( t1, p1 ) :: other ->
                    if time < t0 then
                        p0
                    else if time < t1 then
                        let
                            t' =
                                (time - t0) / (t1 - t0)

                            ( x0, y0 ) =
                                p0

                            ( x1, y1 ) =
                                p1

                            x =
                                x0 + t' * (x1 - x0)

                            y =
                                y0 + t' * (y1 - y0)
                        in
                            ( x, y )
                    else
                        loop (( t1, p1 ) :: other)

                ( t1, p1 ) :: [] ->
                    p1

                [] ->
                    ( 0, 0 )
    in
        loop stations
