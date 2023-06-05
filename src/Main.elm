module Main exposing (..)

import Array exposing (Array)
import Array2D
import Browser
import Checker exposing (CheckerModel, scoreMove)
import Data exposing (..)
import Html exposing (Html, a, button, div, main_, text)
import Html.Attributes exposing (class, classList, disabled, href, style)
import Html.Attributes.Autocomplete exposing (DetailedCompletion(..))
import Html.Events exposing (onClick)
import List.Extra exposing (removeIfIndex)
import Maybe
import Random
import Url
import UrlState exposing (decodeUrl, getNextUrl)


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


initialBoard : Tiles
initialBoard =
    Array2D.repeat gridSize gridSize Nothing
        |> Array2D.set 1 2 (Just 'A')
        |> Array2D.set 2 2 (Just 'B')
        |> Array2D.set 3 2 (Just 'C')


allTiles : List Tile
allTiles =
    List.map Char.fromCode <| List.range (Char.toCode 'A') (Char.toCode 'Z')


initialRack : Array Tile
initialRack =
    Array.fromList [ 'A', 'X', 'B', 'L', 'D', 'E', 'I' ]


init : flags -> Url.Url -> key -> ( Model, Cmd msg )
init _ url _ =
    let
        _ =
            Debug.log "URL" url
    in
    case Debug.log "URL model" (decodeUrl url) of
        Just model ->
            ( Playing (urlModelToModel model)
            , Cmd.none
            )

        _ ->
            ( Playing
                { selectedCell = Point 0 0
                , selectDirection = Right
                , board = initialBoard
                , rack =
                    initialRack
                        |> Array.map (\c -> RackTile c Nothing)
                , opponent =
                    { name = "Jeff"
                    , score = 0
                    }
                , selfName = "Bob"
                , selfScore = 0
                , playedTurns = []
                , initialSeed = 0
                }
            , Cmd.none
            )



-- UPDATE


type Msg
    = Select Point
    | PlaceTile Int
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case model of
        Playing model_ ->
            updatePlaying msg model_


modelToUrlModel : PlayingModel -> UrlState.UrlModel
modelToUrlModel model =
    let
        nextTurn =
            model.rack
                |> Array.toList
                |> List.indexedMap
                    (\index t ->
                        t.placement |> Maybe.map (\placement -> { rackIndex = index, position = placement })
                    )
                |> List.filterMap (\x -> x)
    in
    { turns = PlayedTurn nextTurn :: model.playedTurns
    , lastPlayer =
        { name = model.selfName
        }
    , nextPlayer =
        { name = model.opponent.name
        }
    , initialSeed = model.initialSeed
    }


type alias PostTurnPlayerState =
    { rack : Array Tile
    , name : String
    , score : Int
    }


type alias PostTurnGameState =
    { board : Tiles
    , nextPlayer : PostTurnPlayerState
    , lastPlayer : PostTurnPlayerState
    , seed : Random.Seed
    }


getNextGameState : PlayedTurn -> PostTurnGameState -> PostTurnGameState
getNextGameState turn state =
    let
        boardWithPlacement placement board =
            let
                tile =
                    state.nextPlayer.rack |> Array.get placement.rackIndex
            in
            board
                |> Array2D.set placement.position.x placement.position.y tile
    in
    case turn of
        PlayedTurn placements ->
            let
                rackIndices =
                    List.map .rackIndex placements

                checkerRack =
                    playedTurnToRackState turn state.nextPlayer.rack

                score =
                    scoreMove (CheckerModel state.board checkerRack)

                -- TODO: Use real probabilities from bag
                newTilesGenerator =
                    Random.list (List.length placements) (Random.uniform 'A' allTiles)

                ( newTiles, seed ) =
                    Random.step newTilesGenerator state.seed
            in
            { board =
                List.foldl boardWithPlacement state.board placements
            , nextPlayer = state.lastPlayer
            , lastPlayer =
                { rack =
                    state.nextPlayer.rack
                        |> Array.toList
                        |> removeIfIndex (\i -> List.member i rackIndices)
                        |> List.append newTiles
                        |> Array.fromList
                , name = state.nextPlayer.name

                -- TODO: Return nothing if move is invalid
                , score = state.nextPlayer.score + Maybe.withDefault 0 score
                }
            , seed = seed
            }


urlModelToModel : UrlState.UrlModel -> PlayingModel
urlModelToModel model =
    let
        initialState =
            -- TODO: Real initial values
            PostTurnGameState
                initialBoard
                { rack = initialRack, score = 0, name = "Jeff" }
                { rack = initialRack, score = 0, name = "Bob" }
                (Random.initialSeed model.initialSeed)

        finalState =
            List.foldr getNextGameState initialState model.turns
    in
    { selectedCell = Point 0 0
    , selectDirection = Right
    , board = finalState.board
    , rack =
        finalState.nextPlayer.rack
            |> Array.map (\tile -> RackTile tile Nothing)
    , opponent =
        { name = model.lastPlayer.name
        , score = finalState.lastPlayer.score
        }
    , selfName = model.nextPlayer.name
    , selfScore = finalState.nextPlayer.score
    , playedTurns = model.turns
    , initialSeed = model.initialSeed
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
    , title = "Scrobburl"
    }


viewScoreHeader : PlayingModel -> Html Msg
viewScoreHeader model =
    div [ style "grid-area" "score-header" ]
        [ div [ style "display" "flex" ]
            [ div [ style "flex" "1" ]
                [ text ("You (" ++ model.selfName ++ "): ")
                , text (String.fromInt model.selfScore)
                , text " points"
                ]
            , div [ style "flex" "1", style "text-align" "right" ]
                [ text model.opponent.name
                , text ": "
                , text (String.fromInt model.opponent.score)
                , text " points"
                ]
            ]
        , case scoreMove (CheckerModel model.board model.rack) of
            Just score ->
                let
                    nextUrl =
                        getNextUrl (modelToUrlModel model)
                in
                div []
                    [ text ("Move: " ++ String.fromInt score ++ " points. ")
                    , a [ href nextUrl ] [ text "Next turn" ]
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
