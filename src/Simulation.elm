
module Simulation exposing ( init, update, view, subs, cmds
                           , trange, srange
                           , moving
                           )

import Axis
import Flow exposing (px,(=>))
import Planum
import Plot

import AnimationFrame
import Color exposing (red,green,blue,black)
import Html exposing (body,h1,div,text,hr,button)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List
import Task
import Window exposing (Size)

-----------------------------------------------------------------------------
--- MODEL DATA
-----------------------------------------------------------------------------

width = 300
height = 300

trange = (0,50)
srange = (0,10)
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
  , winsize : Size
  }
  
type Id = XP | YP | XV | YV | XY

initial =
  { time = 0
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
  , winsize = Size 0 0
  }

init pos = reset 0 pos initial

reset t (x,y) data =
  { data
  | time = t
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

hlabel = "East-West"
vlabel = "North-South"
tpos = "Position"
tvel = "Velocity"
label2 l1 l2 = l1 ++ " " ++ l2

xpGraph = make_graph srange (label2 hlabel tpos)
ypGraph = make_graph srange (label2 vlabel tpos)
xvGraph = make_graph vrange (label2 hlabel tvel)
yvGraph = make_graph vrange (label2 vlabel tvel)

xyGraph =
  Plot.size (2*width) (2*height)
  |> Plot.ranges srange srange
  |> Plot.make
  |> Plot.frame
  |> Plot.grid 4 4
  |> Plot.vbounds
  |> Plot.hbounds
  |> Plot.hlabel (label2 hlabel tpos)
  |> Plot.vlabel (label2 vlabel tpos)
  
make_graph range slabel =
  Plot.size width height
  |> Plot.ranges trange range
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

type Msg =
  Plot (Plot.Msg Id)
  | Tick Float
  | WindowSize Size
  | Ignore | Reset

subs data =
  if data.time >= (trange |> snd)
  then Window.resizes WindowSize
  else
    Sub.batch 
      [ Window.resizes WindowSize
      , AnimationFrame.diffs Tick
      ]

cmds = [Task.perform (\_ -> Ignore) WindowSize Window.size]

update position msg data =
  let
    approx = Planum.approx 2 2
    get g {x,y} = (x,y) |> g.plot.fromScreen |> g.plot.toModel |> approx
  in
    case msg of
      WindowSize size -> { data | winsize = size }

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
          }

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
          data'
          
      Ignore -> data
      Reset -> reset 0 (position 0) data

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

    reset = div []
                [ button [onClick Reset, style ["padding" => px 5, "margin" => px 20]]
                         [text "Reset"]
                ]

    panel =
      Flow.down
        (data.winsize.width - g11w - g12w - g0w - 2*pad)
        (g11h + g21h + 2*pad)
        [ div [] [text <| "Time: " ++ toString (Axis.approx 1 data.time)]
        , div [] [text <| "Position: " ++ toString (Planum.approx 1 1 pos)]
        , reset
        ]

    graphs = Flow.right data.winsize.width (g11h + g21h + 2*pad) [graph12,xyView,panel]
    -- graphs = Flow.padded(g11w + g12w + g0w + 2*pad) (g11h + g21h + 2*pad) [graph12,xyView]
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
    loop l = case l of
      (t0,p0)::(t1,p1)::other ->
        if time < t0 then p0
        else if time < t1 then
          let
            t' = (time-t0)/(t1-t0)
            (x0,y0) = p0
            (x1,y1) = p1
            x = x0 + t'*(x1-x0)
            y = y0 + t'*(y1-y0)
          in
            (x,y)
        else loop ((t1,p1)::other)
      (t1,p1)::[] -> p1
      [] -> (0,0)
  in
    loop stations
