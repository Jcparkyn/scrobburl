module UrlState exposing (UrlModel, getNextUrl)

import Data exposing (PlayedTurn(..), Point)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Url
import Url.Builder
import Url.Parser exposing ((<?>))
import Url.Parser.Query


type alias UrlModel =
    { turns : List PlayedTurn
    , nextPlayer : UrlPlayer
    , lastPlayer : UrlPlayer
    }


type alias UrlPlayer =
    { name : String, score : Int }


getNextUrl : UrlModel -> String
getNextUrl model =
    let
        body =
            E.encode 0 (getNextUrlBody model)
    in
    Url.Builder.relative [] [ Url.Builder.string "state" body ]


getNextUrlBody : UrlModel -> E.Value
getNextUrlBody model =
    E.object
        [ ( "turns", E.list encodeTurn model.turns )
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


encodeTurn : PlayedTurn -> E.Value
encodeTurn turn =
    case turn of
        PlayedTurn tiles ->
            tiles
                |> E.list
                    (\{ rackIndex, position } ->
                        E.string
                            (String.fromInt rackIndex ++ "," ++ encodePoint position)
                    )


decodeTurn : Decoder PlayedTurn
decodeTurn =
    let
        decodePlacement p =
            case String.split "," p of
                [ a, b, c ] ->
                    { rackIndex = 0
                    , position = Point 0 0
                    }

                _ ->
                    { rackIndex = 0
                    , position = Point 0 0
                    }

        -- D.map2
        --     (\rackIndex position -> { rackIndex = rackIndex, position = position })
        --     (D.field "rackIndex")
    in
    (D.list D.string)
        |> D.map (\z -> z)


decodeModel : Decoder UrlModel
decodeModel =
    D.map3 UrlModel
        (D.field "turns" D.array)


decodeUrl : Url.Url -> Maybe UrlModel
decodeUrl url =
    let
        route =
            Url.Parser.query (Url.Parser.Query.string "state")

        stateJson =
            Url.Parser.parse route url |> Maybe.withDefault Nothing
    in
    case stateJson of
        Just json ->
            Just
                { turns = []
                , nextPlayer =
                    { name = "next"
                    , score = 2
                    }
                , lastPlayer =
                    { name = "last"
                    , score = 69
                    }
                }

        _ ->
            Nothing
