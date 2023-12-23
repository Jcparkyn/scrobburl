module UrlState exposing (DecodeUrlError, UrlModel, decodeUrl, encodeUrl)

import Base64
import Bytes
import Bytes.Decode as BD
import Bytes.Encode as BE
import Data exposing (PlayedTurn(..))
import Flate exposing (deflate, inflate)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import List.Extra
import Point exposing (Point)
import Url
import Url.Parser
import Url.Parser.Query
import UrlBase64



-- PUBLIC INTERFACE


type alias UrlModel =
    { turns : List PlayedTurn
    , initialSeed : Int
    }


type DecodeUrlError
    = QueryParseError
    | Base64Error
    | InflateError
    | ByteDecodeError
    | JsonDecodeError D.Error


encodeUrl : UrlModel -> String
encodeUrl model =
    let
        bodyJson =
            E.encode 0 (encodeModelJson model) |> Debug.log "bodyJson"
    in
    compressToBase64 bodyJson


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
            D.decodeString decodeModelJson json
                |> Result.mapError JsonDecodeError
    in
    stateBase64
        |> Result.andThen decompressFromBase64
        |> Result.andThen jsonDecode



-- BASE 64


compressToBase64 : String -> String
compressToBase64 str =
    str
        |> BE.string
        |> BE.encode
        |> deflate
        |> Base64.fromBytes
        |> Maybe.map UrlBase64.toUrl
        |> Maybe.withDefault ""


decompressFromBase64 : String -> Result DecodeUrlError String
decompressFromBase64 str =
    let
        decodeAsString buffer =
            BD.decode (BD.string (Bytes.width buffer)) buffer
    in
    str
        |> UrlBase64.fromUrl
        |> Base64.toBytes
        |> Result.fromMaybe Base64Error
        |> Result.andThen (inflate >> Result.fromMaybe InflateError)
        |> Result.andThen (decodeAsString >> Result.fromMaybe ByteDecodeError)



-- JSON


encodeModelJson : UrlModel -> E.Value
encodeModelJson model =
    E.object
        [ ( "turns", E.list encodeTurnJson model.turns )
        , ( "s0", E.int model.initialSeed )
        ]


decodeModelJson : Decoder UrlModel
decodeModelJson =
    D.map2 UrlModel
        (D.field "turns" (D.list decodeTurnJson))
        (D.field "s0" D.int)


encodeTurnJson : PlayedTurn -> E.Value
encodeTurnJson turn =
    case turn of
        PlayedTurn tiles ->
            tiles
                |> List.concatMap
                    (\{ rackIndex, position } ->
                        [ rackIndex, position.x, position.y ]
                    )
                |> E.list E.int


decodeTurnJson : Decoder PlayedTurn
decodeTurnJson =
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

        decodePlacements : List Int -> PlayedTurn
        decodePlacements p =
            p
                |> List.Extra.groupsOf 3
                |> List.map decodePlacement
                |> PlayedTurn
    in
    D.list D.int
        |> D.map decodePlacements
