module Plot
    exposing
        ( Graph
        , size
        , ranges
        , make
        , draw
        , getSize
        , Msg
        , showMsg
        , Model
        , init
        , update
        , view
        , mouseEnable
        , mouseDisable
        , vlabel
        , hlabel
        , vbounds
        , hbounds
        , line
        , shadow
        , hshadow
        , vshadow
        , scatter
        , poly
        , block
        , blocks
        , eval
        , horiz
        , vert
        , axes
        , frame
        , grid
        , dot
        , sqr
        , dia
        , txt
        , dot'
        , sqr'
        , dia'
        )

import Axis
import Block
import Flow
import Planum exposing (Planum)
import Element
import Color exposing (black, grey)
import Html exposing (div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onMouseLeave)
import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)
import String
import Text
import VirtualDom
import Collage as C


--------------------------------------------------------------------------
---- MODEL, INIT, UPDATE, VIEW
--------------------------------------------------------------------------


type MouseMsg
    = Mouse Position
    | NoMouse


type alias Msg a =
    { id : a, mouse : MouseMsg }

showMsg msg =
    toString msg.id ++ " " ++ toString msg.mouse

type MouseUse
    = MouseEnable
    | MouseDisable


type alias Model id =
    { id : id
    , mouse : MouseUse
    , pos : Maybe ( Float, Float )
    }

init : a -> Model a
init id =
    { id = id
    , pos = Nothing
    , mouse = MouseDisable
    }


update get msg model =
    let
        model' =
            if msg.id == model.id then
                updateMouse get msg.mouse model
            else
                model
    in
        model' ! []


updateMouse get msg model =
    let
        model' =
            case msg of
                Mouse pos ->
                    { model | pos = Just (get pos) }

                NoMouse ->
                    { model | pos = Nothing }
    in
        model'

mouseEnable : Model a -> Model a
mouseEnable model =
    { model | mouse = MouseEnable }


mouseDisable : Model a -> Model a
mouseDisable model =
    { model | mouse = MouseDisable }


view lift g model =
    let
        width =
            Element.widthOf g.e11 + Element.widthOf e12

        e1h =
            Element.heightOf e12

        e2h =
            Element.heightOf g.e22

        e =
            div [] [ e1, e2 ]

        e1 =
            Flow.right width e1h [ Element.toHtml g.e11, graph e12 ]

        e2 =
            Flow.left width e2h [ Element.toHtml g.e22 ]

        graph elem =
            let
                mouseListeners =
                    case model.mouse of
                        MouseDisable ->
                            []

                        MouseEnable ->
                            let
                                offsetX =
                                    "offsetX" := Json.int

                                offsetY =
                                    "offsetY" := Json.int

                                offsetPosition =
                                    Json.object2 Position offsetX offsetY

                                decode =
                                    let
                                        d =
                                            Mouse >> Msg model.id >> lift
                                    in
                                        Json.map d offsetPosition
                            in
                                [ VirtualDom.on "mousemove" decode
                                , onMouseLeave (lift <| Msg model.id NoMouse)
                                ]
            in
                div mouseListeners [ Element.toHtml elem ]

        e12 =
            (g.e12s ++ mouseNotifier) |> Element.layers

        mouseNotifier =
            case model.mouse of
                MouseDisable ->
                    []

                MouseEnable ->
                    case model.pos of
                        Just ( x, y ) ->
                            let
                                mouse1 = g.toModel (x,y)
                                         |> toString
                                         |> Text.fromString
                                         |> Text.height (2 * g.tmini)
                                         |> Element.leftAligned

                                mouse21 = C.path [(g.x0,y),(x-10,y)]
                                          |> C.traced (wide C.dashed black)
                                          
                                mouse22 = C.path [(x,g.y0),(x,y-10)]
                                          |> C.traced (wide C.dashed black)
                                          
                                mouse2 = C.collage g.pwidth g.pheight 
                                            [mouse21,mouse22]
                                
                            in
                                [ mouse1, mouse2 ]

                        Nothing ->
                            []
    in
        e



--------------------------------------------------------------------------
---- GRAPH
--------------------------------------------------------------------------


type alias Graph =
    { plot : Planum
    , gsize : Float
    , ex : Int
    , tsize : Float
    , tmini : Float
    , varea : List Element.Element
    , harea : List Element.Element
    , marea : List C.Form
    }


size width height =
    let
        axisArgs =
            Axis.args

        targs =
            { axisArgs | size = width }

        xargs =
            { axisArgs | size = height }
    in
        ( targs, xargs )


ranges ( tmin, tmax ) ( xmin, xmax ) ( targs, xargs ) =
    ( { targs | modelMin = tmin, modelMax = tmax }
    , { xargs | modelMin = xmin, modelMax = xmax }
    )


make : ( Axis.Args, Axis.Args ) -> Graph
make ( targs, xargs ) =
    let
        plot =
            Planum.make ( targs, xargs )

        gsize =
            0.01 * min plot.tAxis.viewRange plot.xAxis.viewRange

        space =
            0.05 * (plot.tAxis.viewRange + plot.xAxis.viewRange)
    in
        { plot = plot
        , gsize = gsize
        , ex = round space
        , tsize = 0.5 * space
        , tmini = max (0.25 * space) 15
        , varea = []
        , harea = []
        , marea = []
        }



--------------------------------------------------------------------------
---- Draw
--------------------------------------------------------------------------


draw graph =
    let
        width =
            graph.plot.width

        height =
            graph.plot.height

        varea =
            graph.varea

        marea =
            graph.marea

        harea =
            graph.harea

        e11 =
            Element.layers varea

        e12s =
            revMap (singleton width height) marea

        e22 =
            Element.layers harea

        e1w =
            Element.widthOf e11 + width

        e2h =
            Element.heightOf e22
            
        toModel = graph.plot.toModel >> Planum.approx 1 1
    in
        { e11 = e11
        , e12s = e12s
        , e22 = e22
        , tmini = graph.tmini
        , pwidth = width
        , pheight = height
        , x0 = graph.plot.tAxis.viewMin
        , y0 = graph.plot.xAxis.viewMin
        , toModel = toModel
        }

getSize g =
    let
        w =
            Element.widthOf g.e11 + g.pwidth

        h =
            g.pheight + Element.heightOf g.e22

    in
        ( w, h )

revMap f l =
    let
        loop o i =
            case i of
                x :: xs ->
                    loop (f x :: o) xs

                [] ->
                    o
    in
        loop [] l



--------------------------------------------------------------------------
---- Plot components
--------------------------------------------------------------------------


vlabel : String -> Graph -> Graph
vlabel lbl graph =
    let
        t2 =
            Text.fromString lbl
                |> Text.height graph.tsize
                |> C.text
                |> C.rotate (degrees 90)
                |> singleton graph.ex graph.plot.height
    in
        { graph | varea = t2 :: graph.varea }


hlabel : String -> Graph -> Graph
hlabel lbl graph =
    let
        t2 =
            Text.fromString lbl
                |> Text.height graph.tsize
                |> C.text
                |> singleton graph.plot.width graph.ex
    in
        { graph | harea = t2 :: graph.harea }


{--
-- This crashes at runtime!

vbounds : Graph -> Graph
vbounds graph =
    let
        t1 =
            toString graph.plot.xAxis.modelMax
                |> Text.fromString
                |> Text.height graph.tmini
                |> Element.rightAligned
                |> Element.container
                    graph.ex
                    graph.plot.height
                    Element.topRight

        t3 =
            toString graph.plot.xAxis.modelMin
                |> Text.fromString
                |> Text.height graph.tmini
                |> Element.rightAligned
                |> Element.container
                    graph.ex
                    graph.plot.height
                    Element.bottomRight

        e12 =
            Element.layers [t1,t3]
    in
        { graph | varea = e12 :: graph.varea }
--}

vbounds : Graph -> Graph
vbounds graph =
    let
        h = 2 + round graph.tmini
        txt1 = toString graph.plot.xAxis.modelMax
        w1 = String.length txt1 * h

        t1 =
            Text.fromString txt1
                |> Text.height graph.tmini
                |> C.text
                |> singleton w1 h
                |> Element.container
                    graph.ex
                    graph.plot.height
                    Element.topRight

        txt3 = toString graph.plot.xAxis.modelMin
        w3 = String.length txt1 * h

        t3 =
            Text.fromString txt3
                |> Text.height graph.tmini
                |> C.text
                |> singleton w3 h
                |> Element.container
                    graph.ex
                    graph.plot.height
                    Element.bottomRight

        e12 =
            Element.layers [t1,t3]
    in
        { graph | varea = e12 :: graph.varea }

hbounds : Graph -> Graph
hbounds graph =
    let
        h = 2 + round graph.tmini
        txt1 = toString graph.plot.tAxis.modelMin
        w1 = String.length txt1 * h

        t1 =
            Text.fromString txt1
                |> Text.height graph.tmini
                |> C.text
                |> singleton w1 h
                |> Element.container graph.plot.width
                    graph.ex
                    Element.topLeft

        txt3 = toString graph.plot.tAxis.modelMax
        w3 = String.length txt3 * h

        t3 =
            Text.fromString txt3
                |> Text.height graph.tmini
                |> C.text
                |> singleton w3 h
                |> Element.container graph.plot.width
                    graph.ex
                    Element.topRight

        e22 =
            Element.layers [t1,t3]
    in
        { graph | harea = e22 :: graph.harea }



--------------------------------------------------------------------------
---- Graph pieces
--------------------------------------------------------------------------


piece : Graph -> C.Form -> Graph
piece graph p =
    { graph | marea = p :: graph.marea }


wide styler color =
    let
        style =
            styler color
    in
        { style | width = 2 }


line color pts graph =
    List.map graph.plot.toView pts
        |> C.path
        |> C.traced (wide C.solid color)
        |> piece graph


hshadow color p graph =
    let
        (x,y) = graph.plot.toView p
        z = graph.plot.tAxis.viewMin
    in
        C.path [(z,y),(x,y)]
        |> C.traced (wide C.dotted color)
        |> piece graph

vshadow color p graph =
    let
        (x,y) = graph.plot.toView p
        z = graph.plot.xAxis.viewMin
    in
        C.path [(x,z),(x,y)]
        |> C.traced (C.dotted color)
        |> piece graph

shadow color p = hshadow color p >> vshadow color p

scatter : (Float -> C.Form) -> List ( Float, Float ) -> Graph -> Graph
scatter glyph pts graph =
    let
        trans pt =
            glyph graph.gsize |> C.move (graph.plot.toView pt)
    in
        List.map trans pts
            |> C.collage graph.plot.width graph.plot.height
            |> C.toForm
            |> piece graph

poly : List (Float,Float) -> Color.Color -> Graph -> Graph
poly verts color graph =
    List.map graph.plot.toView verts
        |> C.polygon
        |> C.filled color
        |> piece graph

block_ : Block.Block -> Color.Color -> Graph -> C.Form
block_ blk color graph =
    let
        t = graph.plot.xAxis.toView blk.top
        l = graph.plot.xAxis.toView blk.left
        r = graph.plot.xAxis.toView blk.right
        b = graph.plot.xAxis.toView blk.bot
        x = 0.5*(l+r)
        y = 0.5*(t+b)
    in
        C.rect (r-l) (t-b)
        |> C.filled color
        |> C.move (x,y)
        
block : Block.Block -> Color.Color -> Graph -> Graph
block blk color graph =
    block_ blk color graph
    |> piece graph

blocks : List Block.Block -> Color.Color -> Graph -> Graph
blocks blks color graph =
    List.map (\blk -> block_ blk color graph) blks
    |> C.group
    |> piece graph

eval color n f graph =
    let
        range =
            ( graph.plot.tAxis.modelMin, graph.plot.tAxis.modelMax )

        pts =
            List.map (\x -> ( x, f x )) (Axis.samples range n)
    in
        line color pts graph


horiz : Graph -> Graph
horiz graph =
    List.map graph.plot.toView
        [ ( graph.plot.tAxis.modelMin, 0 )
        , ( graph.plot.tAxis.modelMax, 0 )
        ]
        |> C.path
        |> C.traced (wide C.solid black)
        |> piece graph


vert : Graph -> Graph
vert graph =
    List.map graph.plot.toView
        [ ( 0, graph.plot.xAxis.modelMin )
        , ( 0, graph.plot.xAxis.modelMax )
        ]
        |> C.path
        |> C.traced (wide C.solid black)
        |> piece graph


axes : Graph -> Graph
axes =
    horiz >> vert


frame : Graph -> Graph
frame graph =
    C.rect graph.plot.tAxis.viewRange graph.plot.xAxis.viewRange
        |> C.outlined (wide C.solid black)
        |> C.move graph.plot.center
        |> piece graph


grid m n graph =
    let
        g =
            wide C.solid grey

        tA =
            graph.plot.tAxis

        xA =
            graph.plot.xAxis

        hline x =
            C.path [ ( tA.viewMin, x ), ( tA.viewMax, x ) ]
                |> C.traced g

        vline t =
            C.path [ ( t, xA.viewMin ), ( t, xA.viewMax ) ]
                |> C.traced g
    in
        (List.map vline <| tA.grid m)
            ++ (List.map hline <| xA.grid n)
            |> C.group
            |> piece graph



-------------------------------------------------------------------------
---- Glyphs
--------------------------------------------------------------------------


dot color r =
    C.circle r |> C.filled color


sqr color r =
    C.square (2 * r) |> C.filled color


dia color r =
    C.square (2 * r) |> C.filled color |> C.rotate (degrees 45)


txt t color r =
    Text.fromString t |> Text.height (4 * r) |> Text.color color |> C.text


dot' color r =
    C.circle r |> C.outlined (wide C.solid color)


sqr' color r =
    C.square (2 * r) |> C.outlined (wide C.solid color)


dia' color r =
    C.square (2 * r) |> C.outlined (wide C.solid color) |> C.rotate (degrees 45)



-------------------------------------------------------------------------
---- HELPERS
--------------------------------------------------------------------------


singleton w h x =
    C.collage w h [ x ]
