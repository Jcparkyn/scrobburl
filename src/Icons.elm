module Icons exposing
    ( cornerLeftDown
    , shuffle
    )

import Html exposing (Html)
import Svg exposing (Svg, svg)
import Svg.Attributes as SA


svgFeatherIcon : String -> List (Svg msg) -> Html msg
svgFeatherIcon className =
    svg
        [ SA.class <| "feather feather-" ++ className
        , SA.fill "none"
        , SA.height "24"
        , SA.stroke "currentColor"
        , SA.strokeLinecap "round"
        , SA.strokeLinejoin "round"
        , SA.strokeWidth "2"
        , SA.viewBox "0 0 24 24"
        , SA.width "24"
        ]


cornerLeftDown : Html msg
cornerLeftDown =
    svgFeatherIcon "corner-left-down"
        [ Svg.polyline [ SA.points "14 15 9 20 4 15" ] []
        , Svg.path [ SA.d "M20 4h-7a4 4 0 0 0-4 4v12" ] []
        ]


shuffle : Html msg
shuffle =
    svgFeatherIcon "shuffle"
        [ Svg.polyline [ SA.points "16 3 21 3 21 8" ] []
        , Svg.line [ SA.x1 "4", SA.y1 "20", SA.x2 "21", SA.y2 "3" ] []
        , Svg.polyline [ SA.points "21 16 21 21 16 21" ] []
        , Svg.line [ SA.x1 "15", SA.y1 "15", SA.x2 "21", SA.y2 "21" ] []
        , Svg.line [ SA.x1 "4", SA.y1 "4", SA.x2 "9", SA.y2 "9" ] []
        ]
