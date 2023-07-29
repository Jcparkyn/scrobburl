module UrlState exposing (UrlModel, decodeUrl, getNextUrl)

import Base64
import Bytes
import Bytes.Decode as BD
import Bytes.Encode as BE
import Data exposing (PlayedTurn(..))
import Flate exposing (deflate, inflate)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Point exposing (Point)
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


compressToBase64 : String -> String
compressToBase64 str =
    str
        |> BE.string
        |> BE.encode
        |> deflate
        |> Base64.fromBytes
        |> Maybe.withDefault ""


decompressFromBase64 : String -> Result DecodeUrlError String
decompressFromBase64 str =
    let
        decodeAsString buffer =
            BD.decode (BD.string (Bytes.width buffer)) buffer
    in
    str
        |> Base64.toBytes
        |> Result.fromMaybe Base64Error
        |> Result.andThen (inflate >> Result.fromMaybe InflateError)
        -- |> Result.andThen (\s -> Result.fromMaybe InflateError (inflate s))
        |> Result.andThen (decodeAsString >> Result.fromMaybe ByteDecodeError)


getNextUrl : UrlModel -> String
getNextUrl model =
    let
        bodyJson =
            E.encode 0 (getNextUrlBody model)

        bodyBase64 =
            compressToBase64 bodyJson
    in
    Url.Builder.relative [] [ Url.Builder.string "state" bodyBase64 ]


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
    | Base64Error
    | InflateError
    | ByteDecodeError
    | JsonDecodeError D.Error


decodeUrl : Url.Url -> Result DecodeUrlError UrlModel
decodeUrl url =
    let
        route =
            Url.Parser.query (Url.Parser.Query.string "state")

        stateBase64 =
            -- Remove the path, otherwise any leading path segments will break the parse
            Url.Parser.parse route { url | path = "" }
                |> Maybe.withDefault Nothing
                |> Result.fromMaybe QueryParseError

        jsonDecode json =
            D.decodeString decodeModel json
                |> Result.mapError JsonDecodeError
    in
    stateBase64
        |> Result.andThen decompressFromBase64
        |> Result.andThen jsonDecode
