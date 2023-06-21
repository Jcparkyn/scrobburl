module Array2D.Extra exposing (all, any, map2)

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
