
module Flow exposing(right,left,down)

import Html exposing (div,text)
import Html.Attributes exposing (style)

(=>) = (,)

px v = toString v ++ "px"

right w h =
    div [ style [ "display" => "-webkit-flex"
                , "display" => "flex"
                , "width" => px w
                , "height" => px h
                ]
        ]
      
left w h =
    div [ style [ "display" => "-webkit-flex"
                , "display" => "flex"
                , "width" => px w
                , "height" => px h
                , "-webkit-justify-content" => "flex-end"
                , "justify-content" => "flex-end"
                ]
        ]
      
down w h =
    div [ style [ "display" => "-webkit-flex"
                , "display" => "flex"
                , "width" => px w
                , "height" => px h
                , "flex-direction" => "column"
                ]
        ]
