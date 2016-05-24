module Axis exposing (Axis, Args, args, make, samples, eval, approx)


type alias Args =
    { size : Float
    , center : Float
    , modelMin : Float
    , modelMax : Float
    }


args =
    { size = 200, center = 0, modelMin = -10, modelMax = 10 }


make args =
    let
        viewMin =
            args.center - 0.5 * args.size

        viewMax =
            args.center + 0.5 * args.size

        modelRange =
            args.modelMax - args.modelMin

        toView x =
            viewMin + (x - args.modelMin) / modelRange * args.size

        toModel s =
            args.modelMin + (s - viewMin) / args.size * modelRange

        samples n =
            let
                loop i =
                    if i < n then
                        (args.modelMin + i * spacing) :: loop (i + 1)
                    else
                        []

                spacing =
                    modelRange / (n - 1)
            in
                loop 0

        grid n =
            let
                loop i =
                    if i < n then
                        (viewMin + i * spacing) :: loop (i + 1)
                    else
                        []

                spacing =
                    args.size / n
            in
                loop 1
    in
        { toView = toView
        , toModel = toModel
        , viewRange = args.size
        , viewCenter = args.center
        , viewMin = viewMin
        , viewMax = viewMax
        , modelMin = args.modelMin
        , modelMax = args.modelMax
        , samples = samples
        , grid = grid
        }


type alias Axis =
    { toView : Float -> Float
    , toModel : Float -> Float
    , viewRange : Float
    , viewCenter : Float
    , viewMin : Float
    , viewMax : Float
    , modelMin : Float
    , modelMax : Float
    , samples : Float -> List Float
    , grid : Float -> List Float
    }



--- Handy functions


approx n x =
    let
        base =
            10 ^ n
    in
        toFloat (round (base * x)) / base


samples ( modelMin, modelMax ) n =
    let
        loop i =
            if i < n then
                (modelMin + i * spacing) :: loop (i + 1)
            else
                []

        spacing =
            (modelMax - modelMin) / (n - 1)
    in
        loop 0


eval f pts =
    List.map (\x -> ( x, f x )) pts
