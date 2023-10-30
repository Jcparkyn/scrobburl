port module Main exposing (Flags, Model, Msg, PlayingModel, PostTurnGameState, PostTurnPlayerState, SubmitDialogState, main)

import Array exposing (Array)
import Array.Extra
import Array2D exposing (Array2D)
import Array2D.Extra
import Browser
import Browser.Navigation as Nav
import Checker exposing (CheckerModel, CheckerResult(..), getLetterValue, gridSize, scoreMove)
import Data exposing (CellContents(..), CellProps, CellSelection(..), Multiplier, PlayedTurn(..), RackState, RackTile, SelectDirection(..), Tile, Tiles, playedTurnToRackState, swapDirection)
import Html exposing (Html, a, br, button, div, h1, h2, main_, p, text)
import Html.Attributes exposing (class, classList, disabled, href, id, style, target)
import Html.Events exposing (onClick)
import Html.Extra
import List.Extra exposing (removeIfIndex)
import Maybe
import Point exposing (Point)
import Random
import Random.List
import Set exposing (Set)
import Url
import Url.Builder
import UrlState exposing (decodeUrl, getNextUrlState)


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



-- PORTS


port shareUrl : { queryState : String, useClipboard : Bool } -> Cmd msg


port openDialog : String -> Cmd msg



-- MODEL


type Model
    = Playing PlayingModel


type alias PlayingModel =
    { selectedCell : Maybe Point
    , selectDirection : SelectDirection
    , board : Tiles
    , bag : List Tile
    , rack : RackState
    , opponent :
        { name : String
        , score : Int
        }
    , selfName : String
    , selfScore : Int
    , playedTurns : List PlayedTurn
    , initialSeed : Int
    , wordlist : Set String
    , shareUrlSupported : Bool
    , clipboardWriteSupported : Bool
    , submitDialogState : SubmitDialogState
    , gameOver : Bool
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
    , bag : List Tile
    , seed : Random.Seed
    , gameOver : Bool
    }


type alias MoveOutcome =
    { selfScore : Int
    , opponentScore : Int
    , isMoveValid : Bool
    , checkerResult : CheckerResult
    , gameOver : Bool
    }


getCellContents : PlayingModel -> Point -> CellContents
getCellContents model point =
    case model.board |> Array2D.get point.x point.y of
        Just (Just tile) ->
            Placed tile

        _ ->
            let
                previewTile =
                    model.rack
                        |> Array.toList
                        |> List.filter (\tile -> tile.placement == Just point)
                        |> List.head
            in
            case previewTile of
                Just tile ->
                    Preview tile.tile

                _ ->
                    Empty


initialBoard : Tiles
initialBoard =
    Array2D.repeat gridSize gridSize Nothing


initialLetterCounts : List ( Tile, Int )
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


initialBag : List Tile
initialBag =
    initialLetterCounts
        |> List.concatMap (\( tile, count ) -> List.repeat count tile)


drawRandomTiles : Int -> List Tile -> Random.Generator ( List Tile, List Tile )
drawRandomTiles count bag =
    Random.List.choices count bag


type alias Flags =
    { wordlist : String
    , initialSeed : Int
    , shareUrlSupported : Bool
    , clipboardWriteSupported : Bool
    }


parseWordList : String -> Set String
parseWordList strList =
    String.split "\n" strList
        |> Set.fromList


init : Flags -> Url.Url -> key -> ( Model, Cmd msg )
init flags url _ =
    case Debug.log "URL model" (decodeUrl url) of
        Ok model ->
            ( urlModelToModel model flags
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
                , bag = initialState.bag
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
                , shareUrlSupported = flags.shareUrlSupported
                , clipboardWriteSupported = flags.clipboardWriteSupported
                , submitDialogState = { clipboardSuccess = False }
                , gameOver = initialState.gameOver
                }
            , Cmd.none
            )



-- UPDATE


type Msg
    = Select Point
    | PlaceTile Int
    | OpenDialog String
    | ShareUrl { queryState : String, useClipboard : Bool }
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case model of
        Playing pm ->
            updatePlaying msg pm
                |> Tuple.mapFirst Playing


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


getMoveOutcome :
    { board : Tiles
    , rack : RackState
    , wordlist : Set String
    , bag : List Tile
    , selfScore : Int
    , opponentScore : Int
    }
    -> MoveOutcome
getMoveOutcome model =
    let
        checkerResult =
            scoreMove (CheckerModel model.board model.rack model.wordlist)

        ( isMoveValid, score ) =
            case checkerResult of
                ValidPlacement result ->
                    ( List.isEmpty result.invalidWords, result.score )

                _ ->
                    ( False, 0 )

        gameOver =
            List.isEmpty model.bag
                && (model.rack |> Array.toList |> List.all (\t -> t.placement /= Nothing))

        newSelfScore =
            model.selfScore + score

        -- TODO: Subtract tile values
        newOpponentScore =
            model.opponentScore
    in
    { selfScore = newSelfScore
    , opponentScore = newOpponentScore
    , checkerResult = checkerResult
    , isMoveValid = isMoveValid
    , gameOver = gameOver
    }
        |> Debug.log "moveOutcome"


getInitialGameState : Random.Seed -> PostTurnGameState
getInitialGameState seed0 =
    let
        rackGenerator =
            drawRandomTiles 8

        ( ( rack1, bag1 ), seed1 ) =
            Random.step (rackGenerator initialBag) seed0

        ( ( rack2, bag2 ), seed2 ) =
            Random.step (rackGenerator bag1) seed1
    in
    { board = initialBoard
    , nextPlayer = { rack = Array.fromList rack1, score = 0, name = "Player 1" }
    , lastPlayer = { rack = Array.fromList rack2, score = 0, name = "Player 2" }
    , bag = bag2
    , seed = seed2
    , gameOver = False
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

                outcome =
                    getMoveOutcome
                        { board = state.board
                        , rack = checkerRack
                        , wordlist = wordlist
                        , bag = state.bag
                        , selfScore = state.nextPlayer.score
                        , opponentScore = state.lastPlayer.score
                        }

                -- TODO: Handle game over
                newTilesGenerator =
                    drawRandomTiles (List.length placements) state.bag

                ( ( newTiles, newBag ), seed ) =
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
                , score = outcome.selfScore
                }
            , bag = newBag
            , seed = seed
            , gameOver = outcome.gameOver
            }


urlModelToModel : UrlState.UrlModel -> Flags -> Model
urlModelToModel model flags =
    let
        initialState =
            getInitialGameState (Random.initialSeed model.initialSeed)

        wordlist =
            parseWordList flags.wordlist

        finalState =
            List.foldr (getNextGameState wordlist) initialState model.turns
    in
    Playing
        { selectedCell = Nothing
        , selectDirection = Right
        , board = finalState.board
        , bag = finalState.bag
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
        , shareUrlSupported = flags.shareUrlSupported
        , clipboardWriteSupported = flags.clipboardWriteSupported
        , submitDialogState = { clipboardSuccess = False }
        , gameOver = finalState.gameOver
        }


updatePlaying : Msg -> PlayingModel -> ( PlayingModel, Cmd msg )
updatePlaying msg model =
    case Debug.log "msg" msg of
        Select point ->
            ( withSelection model point
            , Cmd.none
            )

        PlaceTile rackIndex ->
            ( withPlacedTile model rackIndex, Cmd.none )

        ShareUrl url ->
            ( { model | submitDialogState = { clipboardSuccess = url.useClipboard } }
            , shareUrl url
            )

        OpenDialog dialogId ->
            ( { model | submitDialogState = { clipboardSuccess = False } }, openDialog dialogId )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    -- We don't really need/want client-side routing, so just always load.
                    ( model, Nav.load (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        _ ->
            ( model, Cmd.none )


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
                potentialSelectionPoints x0 y0 =
                    List.range 1 (gridSize - 1)
                        |> List.map
                            (\i ->
                                case model.selectDirection of
                                    Right ->
                                        Point (modBy gridSize (x0 + i)) y0

                                    Down ->
                                        Point x0 (modBy gridSize (y0 + i))
                            )

                getNextSelectedCell { x, y } =
                    potentialSelectionPoints x y
                        |> List.Extra.find (\point -> getCellContents model point == Empty)
            in
            { model
                | selectedCell =
                    model.selectedCell |> Maybe.andThen getNextSelectedCell
                , submitDialogState = { clipboardSuccess = False }
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


view : Model -> Browser.Document Msg
view model =
    { body =
        case model of
            Playing pm ->
                let
                    moveOutcome =
                        getMoveOutcome
                            { board = pm.board
                            , rack = pm.rack
                            , wordlist = pm.wordlist
                            , bag = pm.bag
                            , selfScore = pm.selfScore
                            , opponentScore = pm.opponent.score
                            }

                    cellProps =
                        Array2D.initialize
                            gridSize
                            gridSize
                            (\y x -> getCellProps pm (Point x y))
                in
                [ main_ []
                    [ viewScoreHeader pm moveOutcome
                    , viewSubmitDialog
                        moveOutcome
                        (getNextUrlState (modelToUrlModel pm))
                        pm.shareUrlSupported
                        pm.clipboardWriteSupported
                        pm.submitDialogState
                    , viewGrid cellProps
                    , viewRack pm.rack pm.gameOver
                    , viewActionButtons moveOutcome pm.gameOver
                    ]
                ]
    , title = "Scrobburl"
    }


type alias SubmitDialogState =
    { clipboardSuccess : Bool }


gameOverText : Int -> Int -> String
gameOverText selfScore opponentScore =
    let
        pointsText x =
            String.fromInt x
                ++ (if x == 1 then
                        " point"

                    else
                        " points"
                   )
    in
    if selfScore > opponentScore then
        "You won by " ++ pointsText (selfScore - opponentScore) ++ "!"

    else if selfScore < opponentScore then
        "You lost by " ++ pointsText (opponentScore - selfScore) ++ "!"

    else
        "You tied with " ++ pointsText selfScore ++ "!"


viewSubmitDialog : MoveOutcome -> String -> Bool -> Bool -> SubmitDialogState -> Html Msg
viewSubmitDialog outcome urlQueryState shareUrlSupported clipboardWriteSupported state =
    Html.node "dialog"
        [ id "submitDialog" ]
        [ h1 []
            [ text <|
                if not outcome.gameOver then
                    "Play turn"

                else
                    gameOverText outcome.selfScore outcome.opponentScore
                        ++ " Final score: "
                        ++ String.fromInt outcome.selfScore
                        ++ " - "
                        ++ String.fromInt outcome.opponentScore
                        ++ "."
            ]
        , p []
            [ text <|
                if not outcome.gameOver then
                    "Send a link to your opponent so they can play the next turn."

                else
                    "Send a link to your opponent so they can see your final move."
            ]
        , if shareUrlSupported || clipboardWriteSupported then
            div [ class "submit-button-container" ]
                [ Html.Extra.viewIf shareUrlSupported <|
                    button
                        [ onClick (ShareUrl { queryState = urlQueryState, useClipboard = False }) ]
                        [ text "Share link" ]
                , Html.Extra.viewIf clipboardWriteSupported <|
                    button
                        [ onClick (ShareUrl { queryState = urlQueryState, useClipboard = True }) ]
                        (if state.clipboardSuccess then
                            [ text "Copied to", br [] [], text "clipboard!" ]

                         else
                            [ text "Copy link to clipboard" ]
                        )
                ]

          else
            div [ style "margin-bottom" "16px" ]
                [ p [] [ text "Your browser doesn't support sharing or copying to the clipboard, so instead you can right click this link and choose \"Copy Link\"." ]
                , a
                    [ href (Url.Builder.relative [] [ Url.Builder.string "state" urlQueryState ])
                    , target "blank"
                    , style "font-size" "1.5em"
                    , style "align-self" "center"
                    , style "padding-bottom" "0.5em"
                    ]
                    [ text "next turn" ]
                ]
        , Html.form []
            [ button [ Html.Attributes.attribute "formmethod" "dialog", id "closeDialogButton" ]
                [ text "Cancel" ]
            ]
        ]


nbsp : String
nbsp =
    "\u{00A0}"


viewScoreHeader : PlayingModel -> MoveOutcome -> Html Msg
viewScoreHeader model moveOutcome =
    div [ style "grid-area" "score-header", class "score-header" ]
        [ Html.Extra.viewIf model.gameOver <|
            h2 []
                [ text <| gameOverText model.selfScore model.opponent.score ++ " "
                , a [ href "?", style "color" "var(--col-primary)" ] [ text "Start new game" ]
                ]
        , div [ style "display" "flex", style "margin-bottom" "4px" ]
            [ div [ style "flex" "1" ]
                [ text ("You (" ++ model.selfName ++ "): ")
                , text (String.fromInt model.selfScore)
                , text (nbsp ++ "points")
                ]
            , div [ style "flex" "1", style "text-align" "right" ]
                [ text model.opponent.name
                , text ": "
                , text (String.fromInt model.opponent.score)
                , text (nbsp ++ "points")
                ]
            ]
        , div [ style "margin-bottom" "10px" ] [ viewMoveOutcome moveOutcome ]
        ]


viewMoveOutcome : MoveOutcome -> Html Msg
viewMoveOutcome outcome =
    case outcome.checkerResult of
        NothingPlaced ->
            text nbsp

        ValidPlacement { score, invalidWords } ->
            case invalidWords of
                [] ->
                    div [ style "color" "var(--col-success)" ]
                        [ text ("Your move: " ++ String.fromInt score ++ " points. ") ]

                [ invalidWord ] ->
                    text (invalidWord ++ " is not a valid word")

                first :: rest ->
                    text (String.join ", " rest ++ " and " ++ first ++ " are not valid words")

        NotThroughOrigin ->
            text "Your first word must pass through the star"

        NotEnoughTiles ->
            text "You must place at least two tiles"

        NotAnchored ->
            text "All tiles must be connected"

        NotInLine ->
            text "All your tiles must be in a single row or column"


viewActionButtons : MoveOutcome -> Bool -> Html Msg
viewActionButtons outcome gameOver =
    div [ class "bottom-action-buttons" ]
        [ Html.Extra.viewIf (not gameOver) <|
            button
                [ onClick (OpenDialog "submitDialog"), disabled (not outcome.isMoveValid) ]
                [ text "Play turn" ]
        ]


viewRack : RackState -> Bool -> Html Msg
viewRack rack disable =
    let
        rackViews =
            rack |> Array.indexedMap (viewRackTile disable)

        ( first, rest ) =
            Array.Extra.splitAt (Array.length rack - 3) rackViews
    in
    div [ class "rack" ]
        -- The last 3 tiles go in a separate div, so that they wrap together
        (Array.toList first ++ [ div [] (Array.toList rest) ])


viewRackTile : Bool -> Int -> RackTile -> Html Msg
viewRackTile disable index tile =
    button
        [ class "rack-tile"
        , onClick (PlaceTile index)
        , disabled (disable || tile.placement /= Nothing)
        ]
        [ viewTile tile.tile True ]


viewGrid : Array2D CellProps -> Html Msg
viewGrid cellProps =
    Html.node "scroll-repeat"
        [ class "scroll-repeat-view" ]
        -- We use a zero-size div to force the xy coords for panzoom to be the top-left of the grid.
        [ div [ style "width" "0", style "height" "0" ]
            [ div
                [ id "super-grid" ]
                (List.repeat 9 (viewPartialGrid cellProps))
            ]
        ]


viewPartialGrid : Array2D CellProps -> Html Msg
viewPartialGrid cellProps =
    div [ class "grid" ]
        (cellProps
            |> Array2D.indexedMap (\y x p -> viewCell (Point x y) p)
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
    let
        -- computes the desired highlight opacity in one dimension
        highlightOpacity selected current =
            (1 - toFloat (Basics.modBy gridSize (current - selected)) / 7)
                |> clamp 0 1
                |> (*) 0.4
    in
    case model.selectedCell of
        Nothing ->
            Inactive

        Just selected ->
            if selected == point then
                Selected

            else if model.selectDirection == Right && selected.y == point.y then
                SelectionHighlight (highlightOpacity selected.x point.x)

            else if model.selectDirection == Down && selected.x == point.x then
                SelectionHighlight (highlightOpacity selected.y point.y)

            else
                Inactive


viewCell : Point -> CellProps -> Html Msg
viewCell point state =
    div
        [ onClick (Select point)
        , class "cell"
        , classList <|
            if state.contents == Empty then
                [ ( "cell-2w", state.multiplier.word == 2 )
                , ( "cell-3w", state.multiplier.word == 3 )
                , ( "cell-2l", state.multiplier.letter == 2 )
                , ( "cell-3l", state.multiplier.letter == 3 )
                ]

            else
                []
        ]
        [ case ( state.contents, state.state ) of
            ( Empty, Inactive ) ->
                text ""

            ( Empty, Selected ) ->
                div [ class "cell-select-highlight" ] []

            ( Empty, SelectionHighlight highlightStrength ) ->
                div
                    [ class "cell-select-highlight"
                    , style "opacity" (String.fromFloat highlightStrength)
                    ]
                    []

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
