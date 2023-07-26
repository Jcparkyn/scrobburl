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
    , initialSeed : Int
    }


type alias UrlPlayer =
    { name : String }


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
        , ( "initialSeed", E.int model.initialSeed )
        ]


encodePlayer : UrlPlayer -> E.Value
encodePlayer player =
    E.object
        [ ( "name", E.string player.name )
        ]


decodePlayer : Decoder UrlPlayer
decodePlayer =
    D.map UrlPlayer
        (D.field "name" D.string)


encodeTurn : PlayedTurn -> E.Value
encodeTurn turn =
    case turn of
        PlayedTurn tiles ->
            tiles
                |> E.list
                    (\{ rackIndex, position } ->
                        E.list E.int [ rackIndex, position.x, position.y ]
                    )


decodeTurn : Decoder PlayedTurn
decodeTurn =
    let
        decodePlacement p =
            case p of
                [ a, b, c ] ->
                    { rackIndex = a
                    , position = Point b c
                    }

                _ ->
                    { rackIndex = 0
                    , position = Point 0 0
                    }
    in
    D.list (D.list D.int)
        |> D.map (\z -> PlayedTurn (List.map decodePlacement z))


decodeModel : Decoder UrlModel
decodeModel =
    D.map4 UrlModel
        (D.field "turns" (D.list decodeTurn))
        (D.field "nextPlayer" decodePlayer)
        (D.field "lastPlayer" decodePlayer)
        (D.field "initialSeed" D.int)


type DecodeUrlError
    = QueryParseError
    | JsonDecodeError D.Error


decodeUrl : Url.Url -> Result DecodeUrlError UrlModel
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
            case D.decodeString decodeModel json of
                Ok result ->
                    Ok result

                Err err ->
                    Err (JsonDecodeError err)

        _ ->
            Err QueryParseError
