module Array2D.Extra exposing (count, flattenToList, map2)

import Array
import Array.Extra
import Array2D exposing (Array2D)


map2 : (a -> b -> c) -> Array2D a -> Array2D b -> Array2D c
map2 f a b =
    Array.Extra.map2 (\aa ba -> Array.Extra.map2 f aa ba) a.data b.data
        |> Array2D.fromArray


flattenToList : Array2D a -> List a
flattenToList array =
    array.data
        |> Array.toList
        |> List.concatMap Array.toList


count : (a -> Bool) -> Array2D a -> Int
count predicate array =
    array.data
        |> Array.map (Array.filter predicate >> Array.length)
        |> Array.foldl (+) 0
