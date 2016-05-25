module Main exposing (..)

import Axis
import Plot
import Plot
import Color exposing (red, green, blue)
import Html exposing (body, h1, div, text)
import Html.Attributes exposing (style)
import List


main : Html.Html (Plot.Msg ())
main =
    let
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
                Axis.eval f (Axis.samples x1 50)

        series3 =
            let
                f x =
                    10 - 0.5 * (x - 4) * (x + 4)
            in
                Axis.eval f (Axis.samples x1 20)

        graph1 =
            Plot.size 800 400
                |> Plot.ranges x1 y1
                |> Plot.make
                |> Plot.frame
                |> Plot.horiz
                |> Plot.line blue series1
                |> Plot.line red series2
                |> Plot.scatter (Plot.txt "x" green) series3

        graphModel =
            Plot.init ()
    in
        body []
            [ h1 [] [ text "Sample plot" ]
            , Plot.view identity (Plot.draw graph1) graphModel
            ]
