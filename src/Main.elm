module Main exposing (..)

import Array exposing (Array)
import Array2D
import Browser
import Checker exposing (scoreMove)
import Data exposing (..)
import Html exposing (Html, a, button, div, main_, text)
import Html.Attributes exposing (class, classList, disabled, href, style)
import Html.Attributes.Autocomplete exposing (DetailedCompletion(..))
import Html.Events exposing (onClick)
import Maybe exposing (withDefault)
import Url
import UrlState exposing (getNextUrl)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


gridSize : number
gridSize =
    9


placedTiles : Tiles
placedTiles =
    Array2D.repeat gridSize gridSize Nothing
        |> Array2D.set 1 2 (Just 'A')
        |> Array2D.set 2 2 (Just 'B')
        |> Array2D.set 3 2 (Just 'C')


init : flags -> Url.Url -> key -> ( Model, Cmd msg )
init _ url _ =
    let
        _ =
            Debug.log "URL" url
    in
    ( Playing
        { selectedCell = Point 0 0
        , selectDirection = Right
        , board = placedTiles
        , rack =
            [ 'A', 'Z', 'B', 'D', 'O', 'Y', 'I' ]
                |> List.map (\c -> RackTile c Nothing)
                |> Array.fromList
        , opponent =
            { name = "Jeff"
            , score = 21
            }
        , selfName = "Bob"
        , selfScore = 69
        , playedTurns = []
        }
    , Cmd.none
    )


-- UPDATE


type Msg
    = Select Point
    | PlaceTile Int
    | SubmitTurn
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case model of
        Playing model_ ->
            updatePlaying msg model_

        _ ->
            ( model, Cmd.none )

getUrlModel : PlayingModel -> UrlState.UrlModel
getUrlModel model =
    { turns = PlayedTurn [] :: model.playedTurns
    , nextPlayer =
        { name = "next"
        , score = 2
        }
    , lastPlayer =
        { name = "last"
        , score = 69
        }
    }


updatePlaying : Msg -> PlayingModel -> ( Model, Cmd msg )
updatePlaying msg model =
    case Debug.log "msg" msg of
        Select point ->
            ( Playing (withSelection model point)
            , Cmd.none
            )

        PlaceTile rackIndex ->
            ( Playing (withPlacedTile model rackIndex), Cmd.none )

        SubmitTurn ->
            let
                nextUrl =
                    getNextUrl (getUrlModel model)
            in
            ( Played model nextUrl, Cmd.none )

        _ ->
            ( Playing model, Cmd.none )


withSelection : PlayingModel -> Point -> PlayingModel
withSelection model point =
    case (getCellProps model point).contents of
        Placed _ ->
            model

        Preview _ ->
            { model
                | selectedCell = point
                , rack =
                    model.rack
                        |> Array.map
                            (\tile ->
                                if tile.placement == Just point then
                                    { tile | placement = Nothing }

                                else
                                    tile
                            )
            }

        Empty ->
            { model
                | selectedCell = point
                , selectDirection =
                    if model.selectedCell == point then
                        swapDirection model.selectDirection

                    else
                        model.selectDirection
            }


withPlacedTile : PlayingModel -> Int -> PlayingModel
withPlacedTile model rackIndex =
    case (getCellProps model model.selectedCell).contents of
        Placed _ ->
            model

        _ ->
            let
                { x, y } =
                    model.selectedCell
            in
            { model
                | selectedCell =
                    case model.selectDirection of
                        Right ->
                            Point (x + 1) y

                        Down ->
                            Point x (y + 1)
                , rack =
                    model.rack
                        |> updateElement rackIndex (\t -> { t | placement = Just model.selectedCell })
            }


updateElement : Int -> (a -> a) -> Array a -> Array a
updateElement index fun array =
    case Array.get index array of
        Just value ->
            array |> Array.set index (fun value)

        Nothing ->
            array



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { body =
        case model of
            Playing model_ ->
                [ main_ []
                    [ viewScoreHeader model_
                    , viewGrid model_
                    , viewRack model_.rack
                    ]
                ]

            Played _ newUrl ->
                [ a [ href newUrl ] [ text (Url.percentDecode newUrl |> withDefault "") ] ]
    , title = "Scrobburl"
    }


viewScoreHeader : PlayingModel -> Html Msg
viewScoreHeader model =
    div [ style "grid-area" "score-header" ]
        [ div [ style "display" "flex" ]
            [ div [ style "flex" "1" ]
                [ text "You: "
                , text (String.fromInt model.selfScore)
                , text " points"
                ]
            , div [ style "flex" "1", style "text-align" "right" ]
                [ text model.selfName
                , text ": "
                , text (String.fromInt model.opponent.score)
                , text " points"
                ]
            ]
        , case scoreMove model of
            Just score ->
                div []
                    [ text ("Move: " ++ String.fromInt score ++ " points")
                    , button [ onClick SubmitTurn ] [ text "Submit" ]
                    ]

            Nothing ->
                text "Not a valid move"
        ]


viewRack : RackState -> Html Msg
viewRack rack =
    div [ class "rack" ]
        (rack |> Array.toList |> List.indexedMap viewRackTile)


viewRackTile : Int -> RackTile -> Html Msg
viewRackTile index tile =
    button
        [ class "rack-tile"
        , onClick (PlaceTile index)
        , disabled (tile.placement /= Nothing)
        , style "opacity"
            (case tile.placement of
                Just _ ->
                    "0.5"

                Nothing ->
                    "1"
            )
        ]
        [ viewTile tile.tile True ]


viewGrid : PlayingModel -> Html Msg
viewGrid model =
    div
        [ class "grid" ]
        (pointGrid gridSize
            |> List.map (\point -> viewCell point (getCellProps model point))
        )


pointGrid : Int -> List Point
pointGrid size =
    List.range 0 (size - 1)
        |> List.concatMap
            (\y ->
                List.range 0 (gridSize - 1)
                    |> List.map (\x -> Point x y)
            )


getCellProps : PlayingModel -> Point -> CellProps
getCellProps model point =
    { state = getCellState model point
    , contents =
        getCellContents model point
    }


getCellState : PlayingModel -> Point -> CellSelection
getCellState model point =
    if model.selectedCell == point then
        Selected

    else if
        (model.selectDirection == Right)
            && (model.selectedCell.y == point.y)
            && (model.selectedCell.x < point.x)
    then
        Highlight

    else if
        (model.selectDirection == Down)
            && (model.selectedCell.x == point.x)
            && (model.selectedCell.y < point.y)
    then
        Highlight

    else
        Inactive


viewCell : Point -> CellProps -> Html Msg
viewCell point state =
    div
        [ onClick (Select point)
        , class "cell"
        , style "background-color" (cellColor state.state)
        ]
        [ case state.contents of
            Empty ->
                text ""

            Placed tile ->
                viewTile tile False

            Preview tile ->
                viewTile tile True
        ]


viewTile : Tile -> Bool -> Html msg
viewTile tile isPreview =
    div
        [ classList [ ( "tile", True ), ( "preview-tile", isPreview ) ] ]
        [ text (String.fromChar tile) ]


cellColor : CellSelection -> String
cellColor state =
    case state of
        Selected ->
            "red"

        Highlight ->
            "pink"

        Inactive ->
            "transparent"
