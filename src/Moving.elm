
module Moving exposing (moving)

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
