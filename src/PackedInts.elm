module PackedInts exposing (..)

import Natural as N exposing (Natural)


push : Int -> Int -> Natural -> Natural
push range value stack =
    N.mul stack (N.fromSafeInt range) |> N.add (N.fromSafeInt value)


pop : Int -> Natural -> ( Int, Natural )
pop range stack =
    case N.divModBy (N.fromSafeInt range) stack of
        Just ( stack2, value ) ->
            ( N.toInt value, stack2 )

        _ ->
            -- return original stack if range is zero
            ( 0, stack )


{-| Create the natural number that represents the given list of base-`b` digits.
-}
fromBase : Int -> List Int -> Natural
fromBase b digits =
    Debug.todo ""


{-| Convert the given natural number to a list of digits in base-`b`.
-}
toBase : Int -> Natural -> List Int
toBase b n =
    Debug.todo ""
