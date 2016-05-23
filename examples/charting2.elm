
import Axis
import Plot
import Color exposing (red,green,blue)
import Html exposing (body,h1,div,text,hr,button)
import Html.App as App
import List

-----------------------------------------------------------------------------
--- DATA
-----------------------------------------------------------------------------

x1 = (-10,10)
y1 = (-25,25)

series1 = [(-10,-10), (0,20),(5,20),(10,-10)]

series2 =
  let
    f(x) = 0.05*(x^2-100)*x
  in
    map f (Axis.samples x1 50)

series3 =
  let
    f(x) = 10-0.5*(x-4)*(x+4)
  in
    map f (Axis.samples x1 20)
        
sinc(x) =
  if x == 0 then 1
  else sin(x) / x

-----------------------------------------------------------------------------
--- MODEL
-----------------------------------------------------------------------------

type alias Id = Int

type alias Model =
  { graph1 : Plot.Model Id
  , graph2 : Plot.Model Id
  }

model =
  let
    graph1 =
      Plot.size 800 400
      |> Plot.ranges x1 y1
      |> Plot.init 1
      |> Plot.axes
      |> Plot.frame
      |> Plot.grid 20 10
      |> Plot.line blue series1
      |> Plot.line red series2
      |> Plot.scatter (Plot.txt "x" green) series3
      |> Plot.vlabel "Dependent"
      |> Plot.hlabel "Independent"
      |> Plot.vbounds
      |> Plot.hbounds
    
    graph2 =
      Plot.size 400 600
      |> Plot.ranges (-50,50) (-30,30)
      |> Plot.init 2
      -- |> Plot.frame
      -- |> Plot.hbounds
      |> Plot.axes
      -- , Plot.grid 20 10
      |> Plot.eval blue 5 (\x -> 10 - abs(x))
      |> Plot.eval red 200 (\x -> 20 * sinc(x))
      |> Plot.scatter (Plot.txt "Z" green) series3
      |> Plot.hlabel "This is not a drawing"
      |> Plot.mouseEnable
  in
    {graph1=graph1, graph2=graph2}
    
-----------------------------------------------------------------------------
--- UPDATE and VIEW
-----------------------------------------------------------------------------

type Msg = Plot (Plot.Msg Id)

update msg model =
  case msg of
    Plot msg' ->
      let
        (graph1',cmd1') = Plot.update msg' model.graph1
        (graph2',cmd2') = Plot.update msg' model.graph2
      in
        {graph1=graph1',graph2=graph2'} ! [cmd1',cmd2']

view model =
  body [] [ h1 [] [text "Sample plot"]
          , hr [] []
          , Plot.view Plot model.graph1
          , hr [] []
          , Plot.view Plot model.graph2
          ]

-----------------------------------------------------------------------------
--- MAIN
-----------------------------------------------------------------------------

main =
  App.program { init = model ! []
              , update = update
              , view = view
              , subscriptions = \_ -> Sub.none
              }

-----------------------------------------------------------------------------
--- HELPERS
-----------------------------------------------------------------------------

map f pts = List.map (\x -> (x,f x)) pts
