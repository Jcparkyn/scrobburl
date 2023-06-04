module UrlState exposing (UrlModel, decodeUrl, getNextUrl)

import Data exposing (PlayedTurn(..), Point)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Url
import Url.Builder
import Url.Parser
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


decodePlayer : Decoder UrlPlayer
decodePlayer =
    D.map2 UrlPlayer
        (D.field "name" D.string)
        (D.field "score" D.int)


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
            case String.split "," p |> List.map String.toInt of
                [ Just a, Just b, Just c ] ->
                    { rackIndex = a
                    , position = Point b c
                    }

                _ ->
                    { rackIndex = 0
                    , position = Point 0 0
                    }
    in
    D.list D.string
        |> D.map (\z -> PlayedTurn (List.map decodePlacement z))


decodeModel : Decoder UrlModel
decodeModel =
    D.map3 UrlModel
        (D.field "turns" (D.list decodeTurn))
        (D.field "nextPlayer" decodePlayer)
        (D.field "lastPlayer" decodePlayer)


decodeUrl : Url.Url -> Maybe UrlModel
decodeUrl url =
    let
        route =
            Url.Parser.query (Url.Parser.Query.string "state")

        stateJson =
            Url.Parser.parse route url
                |> Maybe.withDefault Nothing
    in
    case stateJson of
        Just json ->
            D.decodeString decodeModel json
                |> Result.toMaybe

        _ ->
            Nothing
