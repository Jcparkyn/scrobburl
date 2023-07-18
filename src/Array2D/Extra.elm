module Array2D.Extra exposing (all, any, flattenToList, map2, slice, trimLeft, trimTop)

import Array
import Array.Extra
import Array2D exposing (Array2D)


any : (a -> Bool) -> Array2D a -> Bool
any predicate array =
    array.data |> Array.Extra.any (Array.Extra.any predicate)


all : (a -> Bool) -> Array2D a -> Bool
all predicate array =
    array.data |> Array.Extra.all (Array.Extra.all predicate)


map2 : (a -> b -> c) -> Array2D a -> Array2D b -> Array2D c
map2 f a b =
    Array.Extra.map2 (\aa ba -> Array.Extra.map2 f aa ba) a.data b.data
        |> Array2D.fromArray


trimLeft : Int -> Array2D a -> Array2D a
trimLeft n array =
    array.data
        |> Array.map (Array.slice n (Array.length array.data))
        |> Array2D.fromArray


trimTop : Int -> Array2D a -> Array2D a
trimTop n array =
    array.data
        |> Array.slice n (Array.length array.data)
        |> Array2D.fromArray


slice : Int -> Int -> Int -> Int -> Array2D a -> Array2D a
slice x1 x2 y1 y2 array =
    array.data
        |> Array.slice y1 y2
        |> Array.map (Array.slice x1 x2)
        |> Array2D.fromArray


flattenToList : Array2D a -> List a
flattenToList array =
    array.data
        |> Array.toList
        |> List.concatMap Array.toList
