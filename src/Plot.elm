
module Plot exposing( Msg, Model, size, ranges, init, update, view
                    , mouseEnable, mouseDisable
                    , vlabel, hlabel, vbounds, hbounds
                    , line, scatter, eval
                    , horiz, vert, axes, frame, grid
                    , dot, sqr, dia, txt, dot', sqr', dia'
                    )

import Axis
import Planum exposing (Planum)

import Element

import Color exposing (black,grey)
import Html exposing (div,text)
import Html.Attributes exposing (style)
import Html.Events exposing (onMouseLeave)
import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)
import Text
import VirtualDom

import Collage as C

type MouseMsg = Mouse Position | NoMouse
type alias Msg a = {id : a, mouse : MouseMsg }

type MouseUse = MouseEnable | MouseDisable

type alias Model id =
  { id : id
  , pos : Maybe (Float,Float)
  , plot : Planum
  , gsize : Float
  , ex : Int
  , tsize : Float
  , tmini : Float
  , mouse : MouseUse
  , varea : List Element.Element
  , harea : List Element.Element
  , marea : List C.Form
  }

size width height =
  let
    axisArgs = Axis.args
    targs = { axisArgs | size=width }
    xargs = { axisArgs | size=height }
  in
    (targs,xargs)

ranges (tmin,tmax) (xmin,xmax) (targs,xargs) =
  ( { targs | modelMin=tmin, modelMax=tmax }
  , { xargs | modelMin=xmin, modelMax=xmax }
  )

init : a -> (Axis.Args,Axis.Args) -> Model a
init id (targs,xargs) = 
  let
    plot = Planum.make (targs,xargs)
    gsize = 0.01 * min plot.tAxis.viewRange plot.xAxis.viewRange
    space = 0.05 * (plot.tAxis.viewRange + plot.xAxis.viewRange)
  in
    { id = id
    , pos = Nothing
    , plot = plot
    , gsize = gsize
    , ex = round space
    , tsize = 0.50 * space
    , tmini = max (0.25 * space) 10
    , mouse = MouseDisable
    , varea = []
    , harea = []
    , marea = []
    }

update msg model =
  let model' =
    if msg.id == model.id then updateMouse msg.mouse model
    else model
  in
    model' ! []
    
updateMouse msg model = 
  let
    approx = Planum.approx 2 2
    get {x,y} = (x,y) |> model.plot.fromScreen |> model.plot.toModel |> approx

    model' = case msg of
      Mouse pos -> { model | pos = Just (get pos) }
      NoMouse -> { model | pos = Nothing }
  in
    model'

--------------------------------------------------------------------------
---- VIEW
--------------------------------------------------------------------------

view lift model =
  let
    width = model.plot.width
    height = model.plot.height
    varea = model.varea
    marea = model.marea
    harea = model.harea
    
    e = div [] [e1,e2]
    e1 = flow_right e1w height [ Element.toHtml e11, graph ]
    e2 = flow_left e1w e2h [ Element.toHtml e22 ]

    graph =
      let
        mouseListeners = case model.mouse of
          MouseDisable -> []
          MouseEnable ->
            let
              offsetX = "offsetX" := Json.int
              offsetY = "offsetY" := Json.int
              offsetPosition = Json.object2 Position offsetX offsetY
              decode = Json.map (Mouse >> Msg model.id >> lift) offsetPosition
            in
              [ VirtualDom.on "mousemove" decode
              , onMouseLeave (lift <| Msg model.id NoMouse)
              ]
      in
        div mouseListeners [ Element.toHtml e12 ]
    
    e11 = Element.layers varea
    e12 =
      (mouseNotifier ++ List.map (singleton width height) marea)
      |> Element.layers
    e22 = Element.layers harea
    
    e1w = Element.widthOf e11 + width
    e2h = Element.heightOf e22
    
    mouseNotifier = case model.mouse of
      MouseDisable -> []
      MouseEnable ->
        case model.pos of
          Just (x,y)->
            let
              enot =
                "(" ++ toString x ++ "," ++ toString y ++ ")"
                |> Text.fromString
                |> Text.height model.tmini
                |> Element.leftAligned
            in
              [enot]
          Nothing -> []
  in
    e

mouseEnable : Model a -> Model a
mouseEnable model = { model | mouse = MouseEnable }

mouseDisable : Model a -> Model a
mouseDisable model = { model | mouse = MouseDisable }

--------------------------------------------------------------------------
---- Plot components
--------------------------------------------------------------------------

vlabel : String -> Model a -> Model a
vlabel lbl model =
  let
    t2 = Text.fromString lbl
          |> Text.height model.tsize
          |> C.text
          |> C.rotate (degrees 90)
          |> singleton model.ex model.plot.height
  in
    { model | varea = t2 :: model.varea }

hlabel : String -> Model a -> Model a
hlabel lbl model =
  let
    t2 = Text.fromString lbl
          |> Text.height model.tsize
          |> C.text
          |> singleton model.plot.width model.ex
  in
    { model | harea = t2 :: model.harea }

vbounds : Model a -> Model a
vbounds model =
  let
    t1 = toString model.plot.xAxis.modelMax
          |> Text.fromString
          |> Text.height model.tmini
          |> Element.rightAligned

    t3 = toString model.plot.xAxis.modelMin
          |> Text.fromString
          |> Text.height model.tmini
          |> Element.rightAligned

    e12 = Element.layers
            [ Element.container model.ex model.plot.height Element.topRight t1
            , Element.container model.ex model.plot.height Element.bottomRight t3
            ]
  in
    { model | varea = e12 :: model.varea }
    
hbounds : Model a -> Model a
hbounds model =
  let
    t1 = toString model.plot.tAxis.modelMin
          |> Text.fromString
          |> Text.height model.tmini
          |> Element.centered
          
    t3 = toString model.plot.tAxis.modelMax
          |> Text.fromString
          |> Text.height model.tmini
          |> Element.centered
          
    e22 = Element.layers
            [ Element.container model.plot.width model.ex Element.topLeft t1
            , Element.container model.plot.width model.ex Element.topRight t3
            ]
  in
    { model | harea = e22 :: model.harea }

--------------------------------------------------------------------------
---- Graph pieces
--------------------------------------------------------------------------

piece : Model a -> C.Form -> Model a
piece model p = { model | marea = p :: model.marea }

line color pts model =
  List.map model.plot.toView pts
  |> C.path
  |> C.traced (C.solid color)
  |> piece model

scatter : (Float -> C.Form) -> List (Float,Float) -> Model a -> Model a
scatter glyph pts model =
  let
    trans pt = glyph model.gsize |> C.move (model.plot.toView pt)
  in
    List.map trans pts
    |> C.collage model.plot.width model.plot.height
    |> C.toForm
    |> piece model

eval color n f model =
  let
    range = (model.plot.tAxis.modelMin,model.plot.tAxis.modelMax)
    pts = List.map (\x -> (x,f x)) (Axis.samples range n)
  in
    line color pts model

horiz : Model a -> Model a
horiz model =
  List.map model.plot.toView
    [(model.plot.tAxis.modelMin,0),(model.plot.tAxis.modelMax,0)]
  |> C.path
  |> C.traced (C.solid black)
  |> piece model

vert : Model a -> Model a
vert model = 
  List.map model.plot.toView
    [(0,model.plot.xAxis.modelMin),(0,model.plot.xAxis.modelMax)]
  |> C.path
  |> C.traced (C.solid black)
  |> piece model

axes : Model a -> Model a
axes = horiz >> vert

frame : Model a -> Model a
frame model =
  C.rect model.plot.tAxis.viewRange model.plot.xAxis.viewRange
  |> C.outlined (C.solid black)
  |> C.move model.plot.center
  |> piece model

grid m n model =
  let
    g = C.solid grey
    tA = model.plot.tAxis
    xA = model.plot.xAxis
    hline x =
      C.path [(tA.viewMin,x),(tA.viewMax,x)]
      |> C.traced g 
    vline t =
      C.path [(t,xA.viewMin),(t,xA.viewMax)]
      |> C.traced g
  in
    (List.map vline <| tA.grid m) ++ (List.map hline <| xA.grid n)
    |> C.group
    |> piece model
  
-------------------------------------------------------------------------
---- Glyphs
--------------------------------------------------------------------------

dot color r = C.circle r |> C.filled color

sqr color r = C.square (2*r) |> C.filled color

dia color r = C.square (2*r) |> C.filled color |> C.rotate (degrees 45)

txt t color r = Text.fromString t |> Text.height (4*r) |> Text.color color |> C.text 

dot' color r = C.circle r |> C.outlined (C.solid color)

sqr' color r = C.square (2*r) |> C.outlined (C.solid color)

dia' color r = C.square (2*r) |> C.outlined (C.solid color) |> C.rotate (degrees 45)

-------------------------------------------------------------------------
---- HELPERS
--------------------------------------------------------------------------

singleton w h x = C.collage w h [x]

(=>) = (,)

px v = toString v ++ "px"

flow_right w h =
    div [ style [ "display" => "-webkit-flex"
                , "display" => "flex"
                , "width" => px w
                , "height" => px h
                ]
        ]
      
flow_left w h =
    div [ style [ "display" => "-webkit-flex"
                , "display" => "flex"
                , "width" => px w
                , "height" => px h
                , "-webkit-justify-content" => "flex-end"
                , "justify-content" => "flex-end"
                ]
        ]
      
flow_down w h =
    div [ style [ "display" => "-webkit-flex"
                , "display" => "flex"
                , "width" => px w
                , "height" => px h
                , "flex-direction" => "column"
                ]
        ]
