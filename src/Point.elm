module Point exposing (Point, add, mul, toTuple)


type alias Point =
    { x : Int
    , y : Int
    }


toTuple : Point -> ( Int, Int )
toTuple point =
    ( point.x, point.y )


add : Point -> Point -> Point
add p1 p2 =
    Point (p1.x + p2.x) (p1.y + p2.y)


mul : Int -> Point -> Point
mul n { x, y } =
    Point (x * n) (y * n)
