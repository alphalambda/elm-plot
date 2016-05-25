module Flow exposing (right, left, down, padded, px, (=>))

import Html exposing (div, text)
import Html.Attributes exposing (style)


(=>) =
    (,)


px v =
    toString v ++ "px"


right w h =
    div
        [ style
            [ "display" => "-webkit-flex"
            , "display" => "flex"
            , "width" => px w
            , "height" => px h
            , "-webkit-justify-content" => "flex-start"
            , "justify-content" => "flex-start"
            , "-webkit-align-items" => "center"
            , "align-items" => "center"
            ]
        ]


left w h =
    div
        [ style
            [ "display" => "-webkit-flex"
            , "display" => "flex"
            , "width" => px w
            , "height" => px h
            , "-webkit-justify-content" => "flex-end"
            , "justify-content" => "flex-end"
            , "-webkit-align-items" => "center"
            , "align-items" => "center"
            ]
        ]


padded w h =
    div
        [ style
            [ "display" => "-webkit-flex"
            , "display" => "flex"
            , "width" => px w
            , "height" => px h
            , "-webkit-justify-content" => "space-around"
            , "justify-content" => "space-around"
            , "-webkit-align-items" => "center"
            , "align-items" => "center"
            ]
        ]


down w h =
    div
        [ style
            [ "display" => "-webkit-flex"
            , "display" => "flex"
            , "width" => px w
            , "height" => px h
            , "flex-direction" => "column"
            ]
        ]
