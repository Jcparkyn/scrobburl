module Point exposing (..)


type alias Point =
    { x : Int
    , y : Int
    }


toTuple : Point -> ( Int, Int )
toTuple point =
    ( point.x, point.y )
