module Planum exposing (Planum, size, ranges, make, approx)

import Axis


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


make ( targs, xargs ) =
    let
        tAxis =
            Axis.make targs

        xAxis =
            Axis.make xargs

        width =
            round tAxis.viewRange

        height =
            round xAxis.viewRange

        toModel ( t, x ) =
            ( tAxis.toModel t, xAxis.toModel x )

        toView ( t, x ) =
            ( tAxis.toView t, xAxis.toView x )

        fromScreen ( t, x ) =
            ( toFloat t - 0.5 * tAxis.viewRange
            , -(toFloat x - 0.5 * xAxis.viewRange)
            )

        center =
            ( tAxis.viewCenter, xAxis.viewCenter )

        -- topleft = (tAxis.viewMin,xAxis.viewMax)
    in
        { tAxis = tAxis
        , xAxis = xAxis
        , width = width
        , height = height
        , toModel = toModel
        , toView = toView
        , fromScreen = fromScreen
        , center = center
        }


type alias Planum =
    { tAxis : Axis.Axis
    , xAxis : Axis.Axis
    , width : Int
    , height : Int
    , center : ( Float, Float )
    , toModel : ( Float, Float ) -> ( Float, Float )
    , toView : ( Float, Float ) -> ( Float, Float )
    , fromScreen : ( Int, Int ) -> ( Float, Float )
    }


approx : Int -> Int -> ( Float, Float ) -> ( Float, Float )
approx m n ( t, x ) =
    ( Axis.approx m t, Axis.approx n x )
