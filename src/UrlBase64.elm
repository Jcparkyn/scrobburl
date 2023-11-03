module UrlBase64 exposing (fromUrl, toUrl)

import Maybe
import Regex exposing (Regex)



-- Simplified version of https://github.com/prozacchiwawa/elm-urlbase64/blob/1.0.6/src/UrlBase64.elm


replaceForUrl : Regex
replaceForUrl =
    Regex.fromString "[\\+/=]" |> Maybe.withDefault Regex.never


{-| Convert from the standard base64 alphabet to urlBase64, and trim trailing '=' characters.
-}
toUrl : String -> String
toUrl t =
    let
        replaceChar rematch =
            case rematch.match of
                "+" ->
                    "-"

                "/" ->
                    "_"

                _ ->
                    ""
    in
    Regex.replace replaceForUrl replaceChar t


replaceFromUrl : Regex
replaceFromUrl =
    Regex.fromString "[-_]" |> Maybe.withDefault Regex.never


{-| Convert from urlBase64 to standard base64
-}
fromUrl : String -> String
fromUrl e =
    let
        replaceChar rematch =
            case rematch.match of
                "-" ->
                    "+"

                _ ->
                    "/"

        strlen =
            String.length e

        hanging =
            if strlen == 0 then
                4

            else
                modBy 4 strlen

        ilen =
            if hanging == 0 then
                0

            else
                4 - hanging
    in
    Regex.replace replaceFromUrl replaceChar (e ++ String.repeat ilen "=")
