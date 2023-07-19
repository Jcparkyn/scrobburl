module Main exposing (..)

import Array exposing (Array)
import Array2D
import Array2D.Extra
import Browser
import Checker exposing (CheckerModel, getLetterValue, gridSize, scoreMove)
import Data exposing (..)
import Html exposing (Html, a, button, div, main_, text)
import Html.Attributes exposing (class, classList, disabled, href, id, style, target)
import Html.Attributes.Autocomplete exposing (DetailedCompletion(..))
import Html.Events exposing (onClick)
import List.Extra exposing (removeIfIndex)
import Maybe
import Random
import Set exposing (Set)
import Url
import UrlState exposing (decodeUrl, getNextUrl)


main : Program Flags Model Msg
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


initialBoard : Tiles
initialBoard =
    Array2D.repeat gridSize gridSize Nothing


initialLetterCounts : List ( Char, Int )
initialLetterCounts =
    [ ( 'A', 8 )
    , ( 'B', 2 )
    , ( 'C', 2 )
    , ( 'D', 3 )
    , ( 'E', 12 )
    , ( 'F', 2 )
    , ( 'G', 2 )
    , ( 'H', 2 )
    , ( 'I', 7 )
    , ( 'J', 1 )
    , ( 'K', 1 )
    , ( 'L', 3 )
    , ( 'M', 2 )
    , ( 'N', 6 )
    , ( 'O', 7 )
    , ( 'P', 2 )
    , ( 'Q', 1 )
    , ( 'R', 5 )
    , ( 'S', 3 )
    , ( 'T', 5 )
    , ( 'U', 4 )
    , ( 'V', 2 )
    , ( 'W', 2 )
    , ( 'X', 1 )
    , ( 'Y', 3 )
    , ( 'Z', 1 )
    ]


randomTile : Random.Generator Tile
randomTile =
    case initialLetterCounts |> List.map (\( char, count ) -> ( toFloat count, char )) of
        first :: rest ->
            Random.weighted first rest

        -- This branch can never be reached
        _ ->
            Random.constant 'A'


type alias Flags =
    { wordlist : String
    , initialSeed : Int
    }


parseWordList : String -> Set String
parseWordList strList =
    String.split "\n" strList
        |> Set.fromList


init : Flags -> Url.Url -> key -> ( Model, Cmd msg )
init flags url _ =
    case Debug.log "URL model" (decodeUrl url) of
        Just model ->
            ( Playing (urlModelToModel model (parseWordList flags.wordlist))
            , Cmd.none
            )

        _ ->
            let
                initialState =
                    getInitialGameState (Random.initialSeed flags.initialSeed)

                _ =
                    Debug.log "initialSeed" flags.initialSeed
            in
            ( Playing
                { selectedCell = Nothing
                , selectDirection = Right
                , board = initialBoard
                , rack =
                    initialState.nextPlayer.rack
                        |> Array.map (\c -> RackTile c Nothing)
                , opponent =
                    { name = initialState.lastPlayer.name
                    , score = 0
                    }
                , selfName = initialState.nextPlayer.name
                , selfScore = 0
                , playedTurns = []
                , initialSeed = flags.initialSeed
                , wordlist = parseWordList flags.wordlist
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


getInitialGameState : Random.Seed -> PostTurnGameState
getInitialGameState seed0 =
    let
        rackGenerator =
            Random.list 7 randomTile |> Random.map Array.fromList

        ( rack1, seed1 ) =
            Random.step rackGenerator seed0

        ( rack2, seed2 ) =
            Random.step rackGenerator seed1
    in
    { board = initialBoard
    , nextPlayer = { rack = rack1, score = 0, name = "Player 1" }
    , lastPlayer = { rack = rack2, score = 0, name = "Player 2" }
    , seed = seed2
    }


getNextGameState : Set String -> PlayedTurn -> PostTurnGameState -> PostTurnGameState
getNextGameState wordlist turn state =
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
                    scoreMove (CheckerModel state.board checkerRack wordlist)

                -- TODO: Use real probabilities from bag
                newTilesGenerator =
                    Random.list (List.length placements) randomTile

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


urlModelToModel : UrlState.UrlModel -> Set String -> PlayingModel
urlModelToModel model wordlist =
    let
        initialState =
            getInitialGameState (Random.initialSeed model.initialSeed)

        finalState =
            List.foldr (getNextGameState wordlist) initialState model.turns
    in
    { selectedCell = Nothing
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
    , wordlist = wordlist
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
    case getCellContents model point of
        Placed _ ->
            model

        Preview _ ->
            { model
                | selectedCell = Just point
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
                | selectedCell = Just point
                , selectDirection =
                    if model.selectedCell == Just point then
                        swapDirection model.selectDirection

                    else
                        model.selectDirection
            }


withPlacedTile : PlayingModel -> Int -> PlayingModel
withPlacedTile model rackIndex =
    case model.selectedCell |> Maybe.map (getCellContents model) of
        Just (Placed _) ->
            model

        _ ->
            let
                getNextSelectedCell { x, y } =
                    let
                        next =
                            case model.selectDirection of
                                Right ->
                                    Point (modBy gridSize (x + 1)) y

                                Down ->
                                    Point x (modBy gridSize (y + 1))

                        inBounds =
                            Array2D.get next.x next.y model.board /= Nothing

                        nextContents =
                            getCellContents model next
                    in
                    case ( inBounds, nextContents ) of
                        ( False, _ ) ->
                            Nothing

                        ( True, Empty ) ->
                            Just next

                        _ ->
                            getNextSelectedCell next
            in
            { model
                | selectedCell =
                    model.selectedCell |> Maybe.andThen getNextSelectedCell
                , rack =
                    model.rack
                        |> updateElement rackIndex (\t -> { t | placement = model.selectedCell })
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
    div [ style "grid-area" "score-header", class "score-header" ]
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
        , case scoreMove (CheckerModel model.board model.rack model.wordlist) of
            Just score ->
                let
                    nextUrl =
                        getNextUrl (modelToUrlModel model)
                in
                div []
                    [ text ("Move: " ++ String.fromInt score ++ " points. ")
                    , a [ href nextUrl, target "blank" ] [ text "Next turn" ]
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
    Html.node "scroll-repeat"
        [ class "scroll-repeat-view" ]
        [ div
            [ id "super-grid" ]
            (List.repeat 4 (viewPartialGrid model))
        ]


viewPartialGrid : PlayingModel -> Html Msg
viewPartialGrid model =
    div [ class "grid" ]
        (Array2D.initialize
            gridSize
            gridSize
            (\y x -> viewCell (Point x y) (getCellProps model (Point x y)))
            |> Array2D.Extra.flattenToList
        )


getCellProps : PlayingModel -> Point -> CellProps
getCellProps model point =
    { state = getCellState model point
    , contents =
        getCellContents model point
    , multiplier = Array2D.get point.x point.y Checker.multipliers |> Maybe.withDefault (Multiplier 1 1)
    }


getCellState : PlayingModel -> Point -> CellSelection
getCellState model point =
    case model.selectedCell of
        Nothing ->
            Inactive

        Just selected ->
            if selected == point then
                Selected model.selectDirection

            else
                Inactive


viewCell : Point -> CellProps -> Html Msg
viewCell point state =
    div
        [ onClick (Select point)
        , class "cell"
        , classList
            [ ( "cell-selected", state.state == Selected Right || state.state == Selected Down )
            , ( "cell-2w", state.multiplier.word == 2 )
            , ( "cell-3w", state.multiplier.word == 3 )
            , ( "cell-2l", state.multiplier.letter == 2 )
            , ( "cell-3l", state.multiplier.letter == 3 )
            ]
        ]
        [ case ( state.contents, state.state ) of
            ( Empty, Inactive ) ->
                text ""

            ( Empty, Selected direction ) ->
                div [ class "cell-select-arrow" ]
                    [ text
                        (case direction of
                            Right ->
                                "🡆"

                            Down ->
                                "🡇"
                        )
                    ]

            ( Placed tile, _ ) ->
                viewTile tile False

            ( Preview tile, _ ) ->
                viewTile tile True
        ]


viewTile : Tile -> Bool -> Html msg
viewTile tile isPreview =
    div
        [ classList [ ( "tile", True ), ( "preview-tile", isPreview ) ] ]
        [ div [ class "tile-value " ] [ text (getLetterValue tile |> String.fromInt) ]
        , text (String.fromChar tile)
        ]
