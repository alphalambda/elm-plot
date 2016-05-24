module Main exposing (..)

import Axis
import Planum
import Plot
import Color exposing (red, green, blue)
import Html exposing (body, h1, div, text, hr, button)
import Html.App as App
import List


-----------------------------------------------------------------------------
--- DATA
-----------------------------------------------------------------------------


x1 =
    ( -10, 10 )


y1 =
    ( -25, 25 )


series1 =
    [ ( -10, -10 ), ( 0, 20 ), ( 5, 20 ), ( 10, -10 ) ]


series2 =
    let
        f x =
            0.05 * (x ^ 2 - 100) * x
    in
        map f (Axis.samples x1 50)


series3 =
    let
        f x =
            10 - 0.5 * (x - 4) * (x + 4)
    in
        map f (Axis.samples x1 20)


sinc x =
    if x == 0 then
        1
    else
        sin (x) / x



-----------------------------------------------------------------------------
--- MODEL
-----------------------------------------------------------------------------


type alias Id =
    Int


type alias Model =
    { plot1 : Plot.Model Id
    , plot2 : Plot.Model Id
    }


model =
    { plot1 = Plot.init 1 |> Plot.mouseEnable
    , plot2 = Plot.init 2
    }


graph1 =
    Plot.size 800 400
        |> Plot.ranges x1 y1
        |> Plot.make
        |> Plot.frame
        |> Plot.grid 20 10
        |> Plot.axes
        |> Plot.line blue series1
        |> Plot.line red series2
        |> Plot.scatter (Plot.txt "x" green) series3
        |> Plot.vlabel "Dependent"
        |> Plot.hlabel "Independent"
        |> Plot.vbounds
        |> Plot.hbounds


graph2 =
    Plot.size 400 600
        |>
            Plot.ranges ( -50, 50 ) ( -30, 30 )
        |>
            Plot.make
        -- |> Plot.frame
        -- |> Plot.hbounds
        |>
            Plot.axes
        -- , Plot.grid 20 10
        |>
            Plot.eval blue 5 (\x -> 10 - abs (x))
        |>
            Plot.eval red 200 (\x -> 20 * sinc (x))
        |>
            Plot.scatter (Plot.txt "Z" green) series3
        |>
            Plot.hlabel "This is not a drawing"



-----------------------------------------------------------------------------
--- UPDATE and VIEW
-----------------------------------------------------------------------------


type Msg
    = Plot (Plot.Msg Id)


update msg model =
    let
        approx =
            Planum.approx 2 2

        get g { x, y } =
            ( x, y ) |> g.plot.fromScreen |> g.plot.toModel |> approx
    in
        case msg of
            Plot msg' ->
                let
                    ( plot1', cmd1' ) =
                        Plot.update (get graph1) msg' model.plot1

                    ( plot2', cmd2' ) =
                        Plot.update (get graph2) msg' model.plot2
                in
                    { plot1 = plot1', plot2 = plot2' } ! [ cmd1', cmd2' ]


view model =
    let
        g1 =
            Plot.draw graph1

        g2 =
            Plot.draw graph2
    in
        body []
            [ h1 [] [ text "Sample plot" ]
            , text <| "Plot 1 size: " ++ toString (Plot.getSize g1)
            , text <| " | Plot 2 size: " ++ toString (Plot.getSize g2)
            , hr [] []
            , Plot.view Plot g1 model.plot1
            , hr [] []
            , Plot.view Plot g2 model.plot2
            ]



-----------------------------------------------------------------------------
--- MAIN
-----------------------------------------------------------------------------


main =
    App.program
        { init = model ! []
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-----------------------------------------------------------------------------
--- HELPERS
-----------------------------------------------------------------------------


map f pts =
    List.map (\x -> ( x, f x )) pts
