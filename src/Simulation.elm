
module Simulation exposing ( init, update, view, subs
                           , trange, srange
                           )

import Axis
import Flow
import Planum
import Plot

import AnimationFrame
import Color exposing (red,green,blue,black)
import Html exposing (body,h1,div,text,hr,button)
import List

-----------------------------------------------------------------------------
--- MODEL DATA
-----------------------------------------------------------------------------

width = 300
height = 300

trange = (0,60)
srange = (-10,10)
vrange = (-4,4)

type alias Point = (Float,Float)

(+|) (x1,y1) (x2,y2) = (x1+x2,y1+y2)

(-|) (x1,y1) (x2,y2) = (x1-x2,y1-y2)

-----------------------------------------------------------------------------
--- DATA
-----------------------------------------------------------------------------

type alias Data =
  { time : Float
  , xyPlot : Plot.Model Id
  , xpPlot : Plot.Model Id
  , xvPlot : Plot.Model Id
  , ypPlot : Plot.Model Id
  , yvPlot : Plot.Model Id
  , xyPoints : List Point
  , xpPoints : List Point
  , ypPoints : List Point
  , xvPoints : List Point
  , yvPoints : List Point
  }
  
type Id = XP | YP | XV | YV | XY

init (x,y) =
  { time = 0
  , xyPlot = Plot.init XY |> Plot.mouseEnable
  , xpPlot = Plot.init XP |> Plot.mouseEnable
  , ypPlot = Plot.init YP |> Plot.mouseEnable
  , xvPlot = Plot.init XV |> Plot.mouseEnable
  , yvPlot = Plot.init YV |> Plot.mouseEnable
  , xyPoints = [(x,y)]
  , xpPoints = [(0,x)]
  , xvPoints = []
  , ypPoints = [(0,y)]
  , yvPoints = []
  }

lastPos data = case data.xyPoints of
  (x,y)::_ -> (x,y)
  [] -> (0,0)
  
-----------------------------------------------------------------------------
--- GRAPHS
-----------------------------------------------------------------------------

hlabel = "Horizontal Position"
vlabel = "Vertical Position"

xpGraph = make_graph srange hlabel
ypGraph = make_graph srange vlabel
xvGraph = make_graph vrange "Horizontal Velocity"
yvGraph = make_graph vrange "Vertical Velocity"

xyGraph =
  Plot.size (2*width) (2*height)
  |> Plot.ranges srange srange
  |> Plot.make
  |> Plot.frame
  |> Plot.grid 4 4
  |> Plot.vbounds
  |> Plot.hbounds
  |> Plot.hlabel hlabel
  |> Plot.vlabel vlabel
  
make_graph range slabel =
  Plot.size width height
  |> Plot.ranges trange range
  |> Plot.make
  |> Plot.grid 6 10
  |> Plot.axes
  |> Plot.frame
  |> Plot.vlabel slabel
  |> Plot.hlabel "Time"
  |> Plot.vbounds
  |> Plot.hbounds

-----------------------------------------------------------------------------
--- UPDATE and VIEW
-----------------------------------------------------------------------------

type Msg = Plot (Plot.Msg Id) | Tick Float


update position msg data =
  let
    approx = Planum.approx 2 2
    get g {x,y} = (x,y) |> g.plot.fromScreen |> g.plot.toModel |> approx
  in
    case msg of
      Plot msg' ->
        let
          xyPlot = Plot.update (get xyGraph) msg' data.xyPlot |> fst
          xpPlot = Plot.update (get xpGraph) msg' data.xpPlot |> fst
          ypPlot = Plot.update (get ypGraph) msg' data.ypPlot |> fst
          xvPlot = Plot.update (get xvGraph) msg' data.xvPlot |> fst
          yvPlot = Plot.update (get yvGraph) msg' data.yvPlot |> fst
        in
          { data
          | xyPlot = xyPlot
          , xpPlot = xpPlot
          , ypPlot = ypPlot
          , xvPlot = xvPlot
          , yvPlot = yvPlot
          } ! []

      Tick dt -> 
        let
          dt' = dt/1000
          time' = data.time + dt'
          (x,y) = lastPos data
          (x',y') = position time'
          vx = (x'-x)/dt'
          vy = (y'-y)/dt'
          data' =
            if data.time >= (trange |> snd)
            then data
            else
              { data
              | time=time'
              , xyPoints = (x',y')::data.xyPoints
              , xpPoints = (time',x')::data.xpPoints
              , ypPoints = (time',y')::data.ypPoints
              , xvPoints = (time',vx)::data.xvPoints
              , yvPoints = (time',vy)::data.yvPoints
              }
        in
          data' ! []

view data =
  let
    pad = 60
    pos = lastPos data
    header = h1 [] [text "Simulation"]
    xyGraph' = xyGraph
               |> Plot.line green data.xyPoints
               |> Plot.scatter (Plot.dot black) [pos]
               |> Plot.draw
    xpGraph' = xpGraph |> Plot.line red   data.xpPoints |> Plot.draw
    ypGraph' = ypGraph |> Plot.line blue  data.ypPoints |> Plot.draw
    xvGraph' = xvGraph |> Plot.line red   data.xvPoints |> Plot.draw
    yvGraph' = yvGraph |> Plot.line blue  data.yvPoints |> Plot.draw
    xyView = Plot.view Plot xyGraph' data.xyPlot
    xpView = Plot.view Plot xpGraph' data.xpPlot
    ypView = Plot.view Plot ypGraph' data.ypPlot
    xvView = Plot.view Plot xvGraph' data.xvPlot
    yvView = Plot.view Plot yvGraph' data.yvPlot
    (g0w,g0h) = Plot.getSize xyGraph'
    (g11w,g11h) = Plot.getSize xpGraph'
    (g12w,g12h) = Plot.getSize ypGraph'
    (g21w,g21h) = Plot.getSize xvGraph'
    graph1 = Flow.right (g11w + g12w + pad) (g11h + pad) [xpView,ypView]
    graph2 = Flow.right (g11w + g12w + pad) (g21h + pad) [xvView,yvView]
    graph12 = div [] [graph1,graph2]
    graphs = Flow.right (g11w + g12w + g0w + 2*pad) (g11h + g21h + 2*pad) [graph12,xyView]
  in
    body []
      [ header
      , text <| "Time: " ++ toString (Axis.approx 1 data.time)
      , text <| " | Position: " ++ toString (Planum.approx 1 1 pos)
      , hr [] []
      , graphs
      , hr [] []
      ]

subs = \_ -> AnimationFrame.diffs Tick
