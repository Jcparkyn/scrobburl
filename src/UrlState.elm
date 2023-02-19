module UrlState exposing (getNextUrl)

import Data exposing (Point)
import Json.Encode as E
import Url


type alias UrlModel =
    { turns : List UrlTurn
    , lastTurn : UrlTurn
    , nextPlayer : UrlPlayer
    , lastPlayer : UrlPlayer
    }


type alias UrlPlayer =
    { name : String, score : Int }


type alias UrlTurn =
    List { rackIndex : Int, position : Point }


getNextUrl : UrlModel -> String
getNextUrl model =
    getNextUrlBody model
        |> E.encode 0
        |> Url.percentEncode


getNextUrlBody : UrlModel -> E.Value
getNextUrlBody model =
    E.object
        [ ( "turns", E.list encodeTurn model.turns )
        , ( "lastTurn", encodeTurn model.lastTurn )
        , ( "nextPlayer", encodePlayer model.nextPlayer )
        , ( "lastPlayer", encodePlayer model.lastPlayer )
        ]


encodePlayer : UrlPlayer -> E.Value
encodePlayer player =
    E.object
        [ ( "name", E.string player.name )
        , ( "score", E.int player.score )
        ]


encodePoint : Point -> String
encodePoint { x, y } =
    String.fromInt x ++ "," ++ String.fromInt y


encodeTurn : UrlTurn -> E.Value
encodeTurn turn =
    turn
        |> E.list
            (\{ rackIndex, position } ->
                E.string
                    (String.fromInt rackIndex ++ "," ++ encodePoint position)
            )
