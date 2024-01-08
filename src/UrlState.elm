module UrlState exposing (DecodeUrlError, UrlModel, decodeUrl, encodeUrlState)

import Base64
import Bitwise exposing (shiftLeftBy, shiftRightBy)
import Bytes
import Bytes.Decode as DB
import Bytes.Decode.Extra
import Bytes.Encode as EB
import Data exposing (Placement, PlayedTurn(..))
import Flate exposing (inflate)
import Json.Decode as DJ exposing (Decoder)
import List.Extra
import Natural exposing (Natural)
import PackedInts
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
    | JsonDecodeError DJ.Error


encodeUrlState : UrlModel -> String
encodeUrlState model =
    bytesToBase64 (EB.encode (encodeModelBytes model))


decodeUrl : Url.Url -> Result DecodeUrlError UrlModel
decodeUrl url =
    let
        -- Back-compat with existing URLs
        oldRoute =
            Url.Parser.query (Url.Parser.Query.string "state")

        newRoute =
            Url.Parser.query (Url.Parser.Query.string "s")

        getStateBase64 route =
            -- Remove the path, otherwise any leading path segments will break the parse
            Url.Parser.parse route { url | path = "" } |> Maybe.withDefault Nothing
    in
    case getStateBase64 newRoute of
        Just s ->
            decodeUrlStateV1 s

        Nothing ->
            case getStateBase64 oldRoute of
                Just s ->
                    decodeUrlStateV0 s

                Nothing ->
                    Result.Err QueryParseError


decodeUrlStateV0 : String -> Result DecodeUrlError UrlModel
decodeUrlStateV0 stateBase64 =
    stateBase64
        |> base64ToBytes
        |> Result.andThen (inflate >> Result.fromMaybe InflateError)
        |> Result.andThen (decodeAsString >> Result.fromMaybe ByteDecodeError)
        |> Result.andThen
            (\json ->
                DJ.decodeString decodeModelJson json
                    |> Result.mapError JsonDecodeError
            )


decodeUrlStateV1 : String -> Result DecodeUrlError UrlModel
decodeUrlStateV1 stateBase64 =
    stateBase64
        |> base64ToBytes
        |> Result.andThen (DB.decode decodeModelBytes >> Result.fromMaybe ByteDecodeError)



-- BASE 64


bytesToBase64 : Bytes.Bytes -> String
bytesToBase64 bytes =
    bytes
        -- |> deflate
        |> Base64.fromBytes
        |> Maybe.map UrlBase64.toUrl
        |> Maybe.withDefault ""


base64ToBytes : String -> Result DecodeUrlError Bytes.Bytes
base64ToBytes str =
    str
        |> UrlBase64.fromUrl
        |> Base64.toBytes
        |> Result.fromMaybe Base64Error


decodeAsString : Bytes.Bytes -> Maybe String
decodeAsString buffer =
    DB.decode (DB.string (Bytes.width buffer)) buffer



-- JSON


decodeModelJson : Decoder UrlModel
decodeModelJson =
    DJ.map2 UrlModel
        (DJ.field "turns" (DJ.list decodeTurnJson))
        (DJ.field "s0" DJ.int)


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
    DJ.list DJ.int
        |> DJ.map decodePlacements



-- BYTES


end : Bytes.Endianness
end =
    Bytes.BE


encodeModelBytes : UrlModel -> EB.Encoder
encodeModelBytes model =
    EB.sequence
        [ EB.unsignedInt8 0 --URL version identifier
        , EB.unsignedInt32 end model.initialSeed
        , encodeListBytes (EB.unsignedInt16 end) (List.map encodeTurnBytes model.turns)
        ]


decodeModelBytes : DB.Decoder UrlModel
decodeModelBytes =
    DB.map3 (\_ seed turns -> UrlModel turns seed)
        DB.unsignedInt8
        (DB.unsignedInt32 end)
        (decodeListBytes (DB.unsignedInt16 end) decodeTurnBytes)


encodeTurnBytes : PlayedTurn -> EB.Encoder
encodeTurnBytes turn =
    case turn of
        PlayedTurn tiles ->
            tiles
                |> List.map
                    (\{ rackIndex, position } ->
                        EB.sequence
                            [ EB.unsignedInt8 rackIndex
                            , EB.unsignedInt8 (position.x + shiftLeftBy 4 position.y)
                            ]
                    )
                |> encodeListBytes EB.unsignedInt8


encodeTurnPacked : List Placement -> Natural
encodeTurnPacked tiles =
    case tiles of
        [] ->
            Natural.zero

        first :: rest ->
            encodeTurnPacked rest
                |> PackedInts.push 8 first.rackIndex
                |> PackedInts.push 16 first.position.x
                |> PackedInts.push 16 first.position.y


decodeTurnPacked : Natural -> List Placement
decodeTurnPacked packed =
    if Natural.isZero packed then
        -- TODO: This check isn't valid if the last turn is all zeros
        []

    else
        let
            ( rackIndex, packed1 ) =
                PackedInts.pop 8 packed

            ( px, packed2 ) =
                PackedInts.pop 16 packed1

            ( py, packed3 ) =
                PackedInts.pop 16 packed2

            placement =
                Placement rackIndex (Point px py)
        in
        placement :: decodeTurnPacked packed3


decodeTurnBytes : DB.Decoder PlayedTurn
decodeTurnBytes =
    let
        decodePlacement : DB.Decoder Placement
        decodePlacement =
            DB.map2
                (\a b ->
                    { rackIndex = a
                    , position =
                        { x = Bitwise.and b 0x0F
                        , y = shiftRightBy 4 b
                        }
                    }
                )
                DB.unsignedInt8
                DB.unsignedInt8
    in
    decodeListBytes DB.unsignedInt8 decodePlacement
        |> DB.map PlayedTurn


encodeListBytes : (Int -> EB.Encoder) -> List EB.Encoder -> EB.Encoder
encodeListBytes encodeLength encoders =
    EB.sequence <| encodeLength (List.length encoders) :: encoders


decodeListBytes : DB.Decoder Int -> DB.Decoder a -> DB.Decoder (List a)
decodeListBytes decodeLength decodeItem =
    decodeLength
        |> DB.andThen
            (\length -> Bytes.Decode.Extra.list length decodeItem)
