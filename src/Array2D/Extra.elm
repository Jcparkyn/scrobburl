module Array2D.Extra exposing (all, any)

import Array.Extra
import Array2D exposing (Array2D)


any : (a -> Bool) -> Array2D a -> Bool
any predicate array =
    array.data |> Array.Extra.any (Array.Extra.any predicate)


all : (a -> Bool) -> Array2D a -> Bool
all predicate array =
    array.data |> Array.Extra.all (Array.Extra.all predicate)
