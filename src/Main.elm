port module Main exposing (Flags, Model, MoveOutcome, Msg, PlayingModel, PostTurnGameState, PostTurnPlayerState, SubmitDialogState, main)

import Array exposing (Array)
import Array.Extra
import Array2D exposing (Array2D)
import Array2D.Extra
import Browser
import Browser.Events exposing (onKeyDown)
import Browser.Navigation as Nav
import Checker exposing (CheckerModel, CheckerResult(..), ScoringCellContents, getLetterValue, gridSize, maxRackSize, scoreMove)
import Data exposing (CellContents(..), CellProps, CellSelection(..), Multiplier, PlayedTurn(..), RackState, RackTile, SelectDirection(..), Tile, Tiles, directionToOffset, isRackReset, playedTurnToRackState, resetRackState, shuffleRack, swapDirection)
import Html exposing (Html, a, br, button, div, h1, h2, li, main_, p, span, text, ul)
import Html.Attributes exposing (class, classList, disabled, href, id, style, target, title)
import Html.Events exposing (onClick)
import Html.Extra exposing (viewIf)
import Html5.DragDrop as DragDrop
import Icons
import Json.Decode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import List.Extra exposing (removeIfIndex)
import Maybe
import Point exposing (Point)
import Random
import Random.List
import Set exposing (Set)
import Tuple
import Url
import Url.Builder
import UrlState exposing (decodeUrl, encodeUrlState)


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- PORTS


port shareUrl : { queryState : String, useClipboard : Bool } -> Cmd msg


port openDialog : String -> Cmd msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown (Json.Decode.map HandleKeyboardEvent decodeKeyboardEvent)



-- MODEL


type DropTarget
    = DropRack Int
    | DropBoard Point


type Model
    = Playing PlayingModel


type alias PlayingModel =
    { selectedCell : Maybe Point
    , selectDirection : SelectDirection
    , lastManualSelectedCell : Maybe Point -- For resetting the cursor to an intuitive position
    , board : PostTurnBoardState
    , bag : List Tile
    , rack : RackState
    , opponent : PostTurnPlayerState
    , selfName : String
    , selfScore : Int
    , playedTurns : List PlayedTurn
    , initialSeed : Int
    , wordlist : Set String
    , shareUrlSupported : Bool
    , clipboardWriteSupported : Bool
    , submitDialogState : SubmitDialogState
    , gameOver : Bool
    , history : List { moveOutcome : MoveOutcome }
    , dragDrop : DragDrop.Model Int DropTarget
    , selectedSwapIndices : Set Int
    , pendingSwap : Maybe (List Int)
    }


type alias PostTurnPlayerState =
    { rack : Array Tile
    , name : String
    , score : Int
    }


type alias PostTurnBoardState =
    Array2D (Maybe { placedTurn : Int, tile : Tile })


type alias PostTurnGameState =
    { board : PostTurnBoardState
    , nextPlayer : PostTurnPlayerState
    , lastPlayer : PostTurnPlayerState
    , bag : List Tile
    , seed : Random.Seed
    , gameOver : Bool
    , history : List { moveOutcome : MoveOutcome }
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
            Placed { tile = tile.tile, justPlaced = tile.placedTurn == List.length model.playedTurns - 1 }

        _ ->
            let
                previewTile =
                    model.rack
                        |> Array.toIndexedList
                        |> List.filter (\( _, tile ) -> tile.placement == Just point)
                        |> List.head
            in
            case previewTile of
                Just ( i, tile ) ->
                    Preview { tile = tile.tile, rackIndex = i }

                _ ->
                    Empty


initialBoard : PostTurnBoardState
initialBoard =
    Array2D.repeat gridSize gridSize Nothing


initialLetterCounts : List ( Tile, Int )
initialLetterCounts =
    [ ( 'A', 7 )
    , ( 'B', 2 )
    , ( 'C', 2 )
    , ( 'D', 3 )
    , ( 'E', 11 )
    , ( 'F', 2 )
    , ( 'G', 2 )
    , ( 'H', 1 )
    , ( 'I', 6 )
    , ( 'J', 1 )
    , ( 'K', 1 )
    , ( 'L', 3 )
    , ( 'M', 2 )
    , ( 'N', 5 )
    , ( 'O', 6 )
    , ( 'P', 2 )
    , ( 'Q', 1 )
    , ( 'R', 4 )
    , ( 'S', 4 )
    , ( 'T', 5 )
    , ( 'U', 3 )
    , ( 'V', 2 )
    , ( 'W', 2 )
    , ( 'X', 1 )
    , ( 'Y', 2 )
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
    case decodeUrl url of
        Ok model ->
            ( urlModelToModel model flags
            , Cmd.none
            )

        _ ->
            let
                initialState =
                    getInitialGameState (Random.initialSeed flags.initialSeed)
            in
            ( Playing
                { selectedCell = Nothing
                , selectDirection = Right
                , lastManualSelectedCell = Nothing
                , board = initialBoard
                , bag = initialState.bag
                , rack =
                    initialState.nextPlayer.rack
                        |> Array.indexedMap (\i c -> RackTile i c Nothing)
                , opponent =
                    { name = initialState.lastPlayer.name
                    , score = 0
                    , rack = initialState.lastPlayer.rack
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
                , history = []
                , dragDrop = DragDrop.init
                , selectedSwapIndices = Set.empty
                , pendingSwap = Nothing
                }
            , Cmd.none
            )



-- UPDATE


type Msg
    = Select Point
    | PlaceTile Int
    | ResetRack
    | ShuffleRack
    | NewRackOrder (List Int)
    | OpenDialog String
    | OpenSwapDialog
    | ToggleSwapTile Int
    | ConfirmSwap
    | CancelSwap
    | PassTurn
    | ShareUrl { useClipboard : Bool }
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HandleKeyboardEvent KeyboardEvent
    | DragDropMsg (DragDrop.Msg Int DropTarget)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Playing pm ->
            updatePlaying msg pm
                |> Tuple.mapFirst Playing


modelToUrlModel : PlayingModel -> UrlState.UrlModel
modelToUrlModel model =
    let
        thisTurn =
            case model.pendingSwap of
                Just swapIndices ->
                    SwappedTiles swapIndices

                Nothing ->
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
                    PlayedTurn nextTurn
    in
    { turns = thisTurn :: model.playedTurns
    , initialSeed = model.initialSeed
    }


getMoveOutcome :
    { board : Tiles
    , rack : RackState
    , wordlist : Set String
    , bag : List Tile
    , selfScore : Int
    , opponent : PostTurnPlayerState
    }
    -> MoveOutcome
getMoveOutcome model =
    let
        checkerResult =
            scoreMove (CheckerModel model.board model.rack model.wordlist)

        ( isMoveValid, score ) =
            case checkerResult of
                ValidPlacement result ->
                    ( result.words |> List.all .legal, result.score )

                _ ->
                    ( False, 0 )

        gameOver =
            List.isEmpty model.bag
                && (model.rack |> Array.Extra.all (\t -> t.placement /= Nothing))

        leftoverTilesPenalty =
            if gameOver then
                model.opponent.rack |> Array.toList |> List.map getLetterValue |> List.sum

            else
                0

        newSelfScore =
            model.selfScore + score + leftoverTilesPenalty
    in
    { selfScore = newSelfScore
    , opponentScore = model.opponent.score
    , checkerResult = checkerResult
    , isMoveValid = isMoveValid
    , gameOver = gameOver
    }


getInitialGameState : Random.Seed -> PostTurnGameState
getInitialGameState seed0 =
    let
        rackGenerator =
            drawRandomTiles maxRackSize

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
    , history = []
    }


getNextGameState : Set String -> PlayedTurn -> PostTurnGameState -> PostTurnGameState
getNextGameState wordlist turn state =
    let
        boardWithPlacement : Data.Placement -> PostTurnBoardState -> PostTurnBoardState
        boardWithPlacement placement board =
            let
                tile =
                    state.nextPlayer.rack |> Array.get placement.rackIndex |> Maybe.withDefault 'A'

                newCell =
                    Just { tile = tile, placedTurn = List.length state.history }
            in
            board
                |> Array2D.set placement.position.x placement.position.y newCell
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
                        { board = state.board |> Array2D.map (Maybe.map .tile)
                        , rack = checkerRack
                        , wordlist = wordlist
                        , bag = state.bag
                        , selfScore = state.nextPlayer.score
                        , opponent = state.lastPlayer
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
            , history = { moveOutcome = outcome } :: state.history
            }

        SwappedTiles rackIndices ->
            let
                swappedTiles =
                    rackIndices
                        |> List.filterMap (\i -> Array.get i state.nextPlayer.rack)

                remainingRack =
                    state.nextPlayer.rack
                        |> Array.toList
                        |> removeIfIndex (\i -> List.member i rackIndices)

                bagWithSwapped =
                    state.bag ++ swappedTiles

                ( ( newTiles, newBag ), seed ) =
                    Random.step (drawRandomTiles (List.length rackIndices) bagWithSwapped) state.seed

                outcome =
                    { selfScore = state.nextPlayer.score
                    , opponentScore = state.lastPlayer.score
                    , checkerResult = NothingPlaced
                    , isMoveValid = True
                    , gameOver = False
                    }
            in
            { board = state.board
            , nextPlayer = state.lastPlayer
            , lastPlayer =
                { rack = remainingRack ++ newTiles |> Array.fromList
                , name = state.nextPlayer.name
                , score = state.nextPlayer.score
                }
            , bag = newBag
            , seed = seed
            , gameOver = False
            , history = { moveOutcome = outcome } :: state.history
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

        playerName n =
            "Player " ++ String.fromInt (n + 1)

        turnCount =
            List.length model.turns
    in
    Playing
        { selectedCell = Nothing
        , selectDirection = Right
        , lastManualSelectedCell = Nothing
        , board = finalState.board
        , bag = finalState.bag
        , rack =
            finalState.nextPlayer.rack
                |> Array.indexedMap (\i tile -> RackTile i tile Nothing)
        , opponent =
            { name = playerName (turnCount - 1 |> modBy 2)
            , score = finalState.lastPlayer.score
            , rack = finalState.lastPlayer.rack
            }
        , selfName = playerName (turnCount |> modBy 2)
        , selfScore = finalState.nextPlayer.score
        , playedTurns = model.turns
        , initialSeed = model.initialSeed
        , wordlist = wordlist
        , shareUrlSupported = flags.shareUrlSupported
        , clipboardWriteSupported = flags.clipboardWriteSupported
        , submitDialogState = { clipboardSuccess = False }
        , gameOver = finalState.gameOver
        , history = finalState.history
        , dragDrop = DragDrop.init
        , selectedSwapIndices = Set.empty
        , pendingSwap = Nothing
        }


updatePlaying : Msg -> PlayingModel -> ( PlayingModel, Cmd Msg )
updatePlaying msg model =
    case msg of
        Select point ->
            ( withSelection model point
            , Cmd.none
            )

        DragDropMsg dragDropMsg ->
            let
                ( newDragDrop, result ) =
                    DragDrop.update dragDropMsg model.dragDrop

                newModel =
                    case result of
                        Nothing ->
                            model

                        Just ( dragId, dropId, _ ) ->
                            case dropId of
                                DropRack dropRackId ->
                                    if dragId == dropRackId then
                                        -- Same target: just clear placement (return to rack)
                                        { model | rack = model.rack |> updateElement dragId (\t -> { t | placement = Nothing }) }

                                    else
                                        let
                                            sortedRack =
                                                model.rack
                                                    |> Array.toIndexedList
                                                    |> List.sortBy (\( _, t ) -> t.sortIndex)
                                                    |> List.map Tuple.first

                                            withoutDrag =
                                                List.Extra.remove dragId sortedRack

                                            insertIndex =
                                                if dropRackId == 999 then
                                                    List.length withoutDrag

                                                else if dropRackId == 998 then
                                                    0

                                                else
                                                    List.Extra.elemIndex dropRackId withoutDrag
                                                        |> Maybe.withDefault (List.length withoutDrag)

                                            ( before, after ) =
                                                List.Extra.splitAt insertIndex withoutDrag

                                            newOrder =
                                                before ++ (dragId :: after)

                                            newSortIndices =
                                                newOrder
                                                    |> List.indexedMap (\sortIndex originalIndex -> ( originalIndex, sortIndex ))

                                            newRack =
                                                List.foldl
                                                    (\( originalIndex, newSortIndex ) currentRack ->
                                                        case Array.get originalIndex currentRack of
                                                            Just t ->
                                                                Array.set originalIndex { t | sortIndex = newSortIndex } currentRack

                                                            Nothing ->
                                                                currentRack
                                                    )
                                                    model.rack
                                                    newSortIndices
                                        in
                                        { model | rack = newRack |> updateElement dragId (\t -> { t | placement = Nothing }) }

                                DropBoard dropPoint ->
                                    let
                                        rackWithoutConflict =
                                            model.rack
                                                |> Array.map
                                                    (\t ->
                                                        if t.placement == Just dropPoint then
                                                            { t | placement = Nothing }

                                                        else
                                                            t
                                                    )

                                        newRack =
                                            rackWithoutConflict
                                                |> updateElement dragId (\t -> { t | placement = Just dropPoint })
                                    in
                                    { model | rack = newRack, selectedCell = Nothing }
            in
            ( { newModel | dragDrop = newDragDrop }, Cmd.none )

        PlaceTile rackIndex ->
            ( withPlacedTile model rackIndex, Cmd.none )

        ResetRack ->
            ( withRackReset model, Cmd.none )

        ShuffleRack ->
            ( model, Random.generate NewRackOrder shuffleRackGenerator )

        NewRackOrder indices ->
            ( { model | rack = shuffleRack model.rack (Array.fromList indices) }, Cmd.none )

        ShareUrl url ->
            ( { model | submitDialogState = { clipboardSuccess = url.useClipboard } }
            , shareUrl { queryState = encodeUrlState (modelToUrlModel model), useClipboard = url.useClipboard }
            )

        OpenDialog dialogId ->
            ( { model | submitDialogState = { clipboardSuccess = False } }, openDialog dialogId )

        OpenSwapDialog ->
            let
                resetModel =
                    withRackReset { model | pendingSwap = Nothing }
            in
            ( { resetModel
                | selectedSwapIndices = Set.empty
                , submitDialogState = { clipboardSuccess = False }
              }
            , openDialog "swapDialog"
            )

        ToggleSwapTile index ->
            let
                newSelected =
                    if Set.member index model.selectedSwapIndices then
                        Set.remove index model.selectedSwapIndices

                    else
                        Set.insert index model.selectedSwapIndices
            in
            ( { model | selectedSwapIndices = newSelected }, Cmd.none )

        ConfirmSwap ->
            ( { model
                | pendingSwap = Just (Set.toList model.selectedSwapIndices)
                , selectedSwapIndices = Set.empty
                , submitDialogState = { clipboardSuccess = False }
              }
            , openDialog "submitDialog"
            )

        CancelSwap ->
            ( { model | pendingSwap = Nothing, selectedSwapIndices = Set.empty }, Cmd.none )

        PassTurn ->
            ( { model | rack = resetRackState model.rack, pendingSwap = Nothing, submitDialogState = { clipboardSuccess = False } }
            , openDialog "submitDialog"
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    -- We don't really need/want client-side routing, so just always load.
                    ( model, Nav.load (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        HandleKeyboardEvent event ->
            case event.key of
                Just "ArrowRight" ->
                    ( withRightArrow model, Cmd.none )

                Just "ArrowLeft" ->
                    ( withSelectionOffset model (Point -1 0), Cmd.none )

                Just "ArrowDown" ->
                    ( withDownArrow model, Cmd.none )

                Just "ArrowUp" ->
                    ( withSelectionOffset model (Point 0 -1), Cmd.none )

                Just " " ->
                    ( { model | selectDirection = swapDirection model.selectDirection }, Cmd.none )

                Just "Escape" ->
                    ( withRackReset model, Cmd.none )

                Just "Backspace" ->
                    ( withBackspace model, Cmd.none )

                Just key ->
                    if isLetterKey key then
                        ( withLetterKeyPressed model key, Cmd.none )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


{-| Find the next cell index on the board which matches a predicate, searching from p0 in a given direction.
-}
findNextCell : Point -> Point -> (Point -> Bool) -> Maybe Point
findNextCell p0 step f =
    let
        potentialPoints =
            List.range 1 (gridSize - 1)
                |> List.map
                    (\i ->
                        Point
                            (modBy gridSize (p0.x + i * step.x))
                            (modBy gridSize (p0.y + i * step.y))
                    )
    in
    potentialPoints |> List.Extra.find f


isLetterKey : String -> Bool
isLetterKey key =
    case String.toList key of
        [ c ] ->
            Char.isAlpha c

        _ ->
            False


withRightArrow : PlayingModel -> PlayingModel
withRightArrow model =
    case model.selectDirection of
        Right ->
            withSelectionOffset model (Point 1 0)

        Down ->
            { model | selectDirection = Right }


withDownArrow : PlayingModel -> PlayingModel
withDownArrow model =
    case model.selectDirection of
        Down ->
            withSelectionOffset model (Point 0 1)

        Right ->
            { model | selectDirection = Down }


withSelectionOffset : PlayingModel -> Point -> PlayingModel
withSelectionOffset model offset =
    case model.selectedCell of
        Nothing ->
            withSelection model (Point 7 7)

        Just selectedCell ->
            let
                newSelection =
                    findNextCell selectedCell offset (\p -> getCellContents model p == Empty)
                        |> Maybe.withDefault (Point 7 7)
            in
            withSelection model newSelection


withLetterKeyPressed : PlayingModel -> String -> PlayingModel
withLetterKeyPressed model key =
    let
        rackIndex =
            model.rack
                |> Array.toList
                |> List.Extra.findIndex
                    (\tile ->
                        tile.placement
                            == Nothing
                            && String.fromChar tile.tile
                            == String.toUpper key
                    )
    in
    case rackIndex of
        Just i ->
            withPlacedTile model i

        _ ->
            model


withRackReset : PlayingModel -> PlayingModel
withRackReset model =
    { model | rack = resetRackState model.rack, selectedCell = model.lastManualSelectedCell }


withBackspace : PlayingModel -> PlayingModel
withBackspace model =
    case model.selectedCell of
        Nothing ->
            model

        Just selection ->
            let
                offset =
                    model.selectDirection |> directionToOffset |> Point.mul -1

                backspacePoint =
                    findNextCell selection
                        offset
                        (\p ->
                            case getCellContents model p of
                                Preview _ ->
                                    True

                                _ ->
                                    False
                        )
            in
            case backspacePoint of
                Nothing ->
                    model

                _ ->
                    { model
                        | selectedCell = backspacePoint
                        , rack =
                            model.rack
                                |> Array.map
                                    (\tile ->
                                        if tile.placement == backspacePoint then
                                            { tile | placement = Nothing }

                                        else
                                            tile
                                    )
                    }


withSelection : PlayingModel -> Point -> PlayingModel
withSelection model point =
    case getCellContents model point of
        Placed _ ->
            model

        Preview _ ->
            { model
                | selectedCell = Just point
                , lastManualSelectedCell = Just point
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
                , lastManualSelectedCell = Just point
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
                getNextSelectedCell p0 =
                    findNextCell p0
                        (directionToOffset model.selectDirection)
                        (\point -> getCellContents model point == Empty)
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
    case model of
        Playing pm ->
            let
                moveOutcome =
                    getMoveOutcome
                        { board = pm.board |> Array2D.map (Maybe.map .tile)
                        , rack = pm.rack
                        , wordlist = pm.wordlist
                        , bag = pm.bag
                        , selfScore = pm.selfScore
                        , opponent = pm.opponent
                        }

                cellProps =
                    Array2D.initialize
                        gridSize
                        gridSize
                        (\y x -> getCellProps pm (Point x y))
            in
            { body =
                [ viewSubmitDialog moveOutcome pm
                , viewInfoDialog pm
                , viewOptionsDialog pm
                , viewSwapDialog pm
                , viewUnseenTilesDialog (getUnseenTiles pm)
                , main_ []
                    [ viewScoreHeader pm
                    , viewGrid cellProps
                    , viewBottomSummary pm moveOutcome
                    , viewRack pm
                    , viewActionButtons moveOutcome pm
                    ]
                ]
            , title = pageTitle pm
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


dialog : List (Html.Attribute msg) -> List (Html msg) -> Html msg
dialog attrs children =
    -- Child div to separate background from foreground clicks
    Html.node "dialog" attrs [ div [] children ]


viewInfoDialog : PlayingModel -> Html msg
viewInfoDialog pm =
    dialog
        [ id "infoDialog", style "width" "500px" ]
        [ h1 [] [ text "About Scrobburl" ]
        , a [ href "https://github.com/jcparkyn/scrobburl", target "_blank" ] [ text "View on GitHub" ]
        , p []
            [ text "Scrobburl (pronounced \"scrobble\") is a multiplayer word game where all state is stored in the URL. "
            ]
        , h2 [] [ text "Rules" ]
        , p []
            [ text
                """The first word played must pass through the star in the center.
                All other words must connect to at least one existing word.
                """
            ]
        , h2 [] [ text "Scoring" ]
        , p []
            [ text
                """Blue squares multiply the value of the letter placed on top of them, and orange/red squares multiply
                the value of the whole word.
                """
            ]
        , p []
            [ text "You get extra points for playing more tiles in one turn:"
            , ul []
                [ li [] [ text "5 tiles: +5 points" ]
                , li [] [ text "6 tiles: +15 points" ]
                , li [] [ text "7 tiles: +30 points" ]
                , li [] [ text "8 tiles: +50 points" ]
                ]
            ]
        , div [ class "dialog-action-buttons" ]
            [ viewIf (pm.playedTurns |> List.isEmpty |> not) <|
                a [ href "/" ] [ button [ class "close-dialog-button" ] [ text "Start new game" ] ]
            , viewCloseDialogButton [ text "Back" ]
            ]
        ]


viewOptionsDialog : PlayingModel -> Html Msg
viewOptionsDialog pm =
    dialog
        [ id "optionsDialog" ]
        [ h1 [] [ text "Options" ]
        , div [ style "display" "flex", style "flex-direction" "column", style "gap" "12px", style "margin-bottom" "20px" ]
            [ Html.form []
                [ button
                    [ Html.Attributes.attribute "formmethod" "dialog"
                    , onClick (OpenDialog "infoDialog")
                    , style "width" "100%"
                    ]
                    [ text "How to play" ]
                ]
            , Html.form []
                [ button
                    [ Html.Attributes.attribute "formmethod" "dialog"
                    , onClick OpenSwapDialog
                    , style "width" "100%"
                    ]
                    [ text "Swap tiles" ]
                ]
            , Html.form []
                [ button
                    [ Html.Attributes.attribute "formmethod" "dialog"
                    , onClick PassTurn
                    , style "width" "100%"
                    ]
                    [ text "Pass turn" ]
                ]
            ]
        , div [ class "dialog-action-buttons" ]
            [ viewCloseDialogButton [ text "Cancel" ] ]
        ]


viewSwapDialog : PlayingModel -> Html Msg
viewSwapDialog pm =
    let
        numSelected =
            Set.size pm.selectedSwapIndices

        sortedRack =
            pm.rack
                |> Array.toIndexedList
                |> List.sortBy (\( _, t ) -> t.sortIndex)
    in
    dialog
        [ id "swapDialog" ]
        [ h1 [] [ text "Swap tiles" ]
        , p [] [ text "Select the tiles you want to swap with the bag:" ]
        , div
            [ class "rack"
            , style "margin" "20px 0"
            , style "justify-content" "center"
            , style "gap" "8px"
            ]
            (List.map
                (\( originalIndex, t ) ->
                    let
                        isSelected =
                            Set.member originalIndex pm.selectedSwapIndices
                    in
                    button
                        [ classList
                            [ ( "rack-tile", True )
                            , ( "swap-tile-selected", isSelected )
                            ]
                        , onClick (ToggleSwapTile originalIndex)
                        , style "transition" "all 0.15s ease"
                        ]
                        [ viewTile t.tile False True ]
                )
                sortedRack
            )
        , p [ style "text-align" "center", style "margin-bottom" "16px" ]
            [ text
                (if numSelected == 0 then
                    "No tiles selected"

                 else
                    String.fromInt numSelected
                        ++ " tile"
                        ++ (if numSelected == 1 then
                                ""

                            else
                                "s"
                           )
                        ++ " selected"
                )
            ]
        , div [ class "dialog-action-buttons" ]
            [ Html.form []
                [ button
                    [ Html.Attributes.attribute "formmethod" "dialog"
                    , onClick ConfirmSwap
                    , disabled (numSelected == 0)
                    ]
                    [ text "Swap" ]
                ]
            , viewCloseDialogButton [ text "Cancel" ]
            ]
        ]


viewSubmitDialog : MoveOutcome -> PlayingModel -> Html Msg
viewSubmitDialog outcome pm =
    let
        isSwap =
            pm.pendingSwap /= Nothing
    in
    dialog
        [ id "submitDialog" ]
        [ h1 []
            [ text <|
                if isSwap then
                    "Swap tiles"

                else if not outcome.gameOver then
                    if outcome.checkerResult == NothingPlaced then
                        "Pass turn"

                    else
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
                if isSwap || not outcome.gameOver then
                    "Send a link to your opponent so they can play the next turn."

                else
                    "Send a link to your opponent so they can see your final move."
            ]
        , if pm.shareUrlSupported || pm.clipboardWriteSupported then
            div [ class "submit-button-container" ]
                [ Html.Extra.viewIf pm.shareUrlSupported <|
                    button
                        [ onClick (ShareUrl { useClipboard = False }) ]
                        [ text "Share link" ]
                , Html.Extra.viewIf pm.clipboardWriteSupported <|
                    button
                        [ onClick (ShareUrl { useClipboard = True }) ]
                        (if pm.submitDialogState.clipboardSuccess then
                            [ text "Copied to", br [] [], text "clipboard!" ]

                         else
                            [ text "Copy link to clipboard" ]
                        )
                ]

          else
            div [ style "margin-bottom" "16px" ]
                [ p [] [ text "Your browser doesn't support sharing or copying to the clipboard, so instead you can right click this link and choose \"Copy Link\"." ]
                , a
                    [ href
                        (Url.Builder.relative []
                            [ Url.Builder.string "s" (encodeUrlState (modelToUrlModel pm)) ]
                        )
                    , target "blank"
                    , style "font-size" "1.5em"
                    , style "align-self" "center"
                    , style "padding-bottom" "0.5em"
                    ]
                    [ text "next turn" ]
                ]
        , div [ class "dialog-action-buttons" ]
            [ viewCloseDialogButton [ text "Cancel" ] ]
        ]


viewCloseDialogButton : List (Html msg) -> Html msg
viewCloseDialogButton children =
    Html.form [ style "margin-left" "auto" ]
        [ button [ Html.Attributes.attribute "formmethod" "dialog", class "close-dialog-button" ]
            children
        ]


nbsp : String
nbsp =
    "\u{00A0}"


viewScoreHeader : PlayingModel -> Html Msg
viewScoreHeader model =
    div [ style "grid-area" "score-header", class "score-header" ]
        [ Html.Extra.viewIf model.gameOver <|
            h2 []
                [ text <| gameOverText model.selfScore model.opponent.score ++ " "
                , a [ href "?", style "color" "var(--col-primary)" ] [ text "Start new game" ]
                ]
        , div [ style "display" "flex", style "margin-bottom" "8px" ]
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
        ]


moveSummaryText : PlayingModel -> MoveOutcome -> Html Msg
moveSummaryText model outcome =
    case outcome.checkerResult of
        ValidPlacement { score, words } ->
            let
                longestWord =
                    words |> List.Extra.maximumBy (.tiles >> List.Extra.count .isPreview)

                viewLetter : ScoringCellContents -> Html msg
                viewLetter t =
                    span [ classList [ ( "just-placed-tile-text", t.isPreview ) ] ] [ text (String.fromChar t.tile) ]
            in
            case longestWord of
                Just longestWord_ ->
                    span []
                        [ text <| model.opponent.name ++ " played "
                        , span [] (longestWord_.tiles |> List.map viewLetter)
                        , text <| " for " ++ String.fromInt score ++ " points. "
                        ]

                _ ->
                    text ""

        NothingPlaced ->
            span []
                [ text <| model.opponent.name ++ " passed their turn." ]

        _ ->
            text ""


pageTitle : PlayingModel -> String
pageTitle pm =
    case pm.history |> List.map (.moveOutcome >> .checkerResult) of
        (ValidPlacement { score, words }) :: _ ->
            let
                longestWord =
                    words
                        |> List.Extra.maximumBy (.tiles >> List.Extra.count .isPreview)
                        |> Maybe.map .word
                        |> Maybe.withDefault ""
            in
            "Scrobburl | "
                ++ pm.opponent.name
                ++ " played "
                ++ longestWord
                ++ " for "
                ++ String.fromInt score
                ++ " points. "

        NothingPlaced :: _ ->
            "Scrobburl | " ++ pm.opponent.name ++ " passed their turn."

        _ ->
            "Scrobburl"


viewBottomSummary : PlayingModel -> MoveOutcome -> Html Msg
viewBottomSummary model outcome =
    div [ class "bottom-summary-container" ]
        [ div [ style "line-height" "1em", style "min-height" "2em" ]
            [ viewMoveOutcome model outcome
            , button [ class "unseen-tiles-button", onClick (OpenDialog "unseenTilesDialog") ]
                [ text <| String.fromInt (model.bag |> List.length) ++ " tiles left" ]
            ]
        ]


viewMoveOutcome : PlayingModel -> MoveOutcome -> Html Msg
viewMoveOutcome model outcome =
    case outcome.checkerResult of
        NothingPlaced ->
            case model.history of
                lastTurn :: _ ->
                    moveSummaryText model lastTurn.moveOutcome

                _ ->
                    text nbsp

        ValidPlacement { score, words } ->
            let
                invalidWords =
                    words |> List.filter (\s -> not s.legal) |> List.map .word
            in
            case invalidWords of
                [] ->
                    span [ style "color" "var(--col-success)" ]
                        [ text ("Your move: " ++ String.fromInt score ++ " points. ") ]

                [ invalidWord ] ->
                    text (invalidWord ++ " is not a valid word (" ++ String.fromInt score ++ " points)")

                first :: rest ->
                    text (String.join ", " rest ++ " and " ++ first ++ " are not valid words (" ++ String.fromInt score ++ " points)")

        NotThroughOrigin ->
            text "Your first word must pass through the star"

        NotEnoughTiles ->
            text "You must place at least two tiles"

        NotAnchored ->
            text "All tiles must be connected"

        NotInLine ->
            text "All your tiles must be in a single row or column"


shuffleRackGenerator : Random.Generator (List Int)
shuffleRackGenerator =
    Random.list maxRackSize (Random.int 0 1000)


viewActionButtons : MoveOutcome -> PlayingModel -> Html Msg
viewActionButtons outcome pm =
    Html.Extra.viewIf (not pm.gameOver) <|
        div [ class "bottom-action-buttons" ]
            [ button [ onClick ResetRack, title "Reset rack", class "button-square", disabled (isRackReset pm.rack) ]
                [ Icons.cornerLeftDown ]
            , button [ onClick ShuffleRack, title "Shuffle rack", class "button-square" ]
                [ Icons.shuffle ]
            , button
                [ onClick (OpenDialog "optionsDialog")
                , title "More options"
                , class "button-square"
                , style "margin-right" "auto"
                ]
                [ Icons.moreHorizontal ]
            , button
                [ onClick (OpenDialog "submitDialog")
                , disabled (not outcome.isMoveValid)
                , title "Play turn"
                , style "padding" "4px 8px"
                , style "margin-left" "auto"
                ]
                [ text "Play turn" ]
            ]


viewRack : PlayingModel -> Html Msg
viewRack pm =
    let
        sortedRack =
            pm.rack
                |> Array.toIndexedList
                -- Ideally this would be a stable sort, but it doesn't matter too much
                |> List.sortBy (\( _, t ) -> t.sortIndex)

        rackViews =
            List.indexedMap
                (\viewIndex ( originalIndex, t ) ->
                    let
                        isLastTile =
                            viewIndex == Array.length pm.rack - 1

                        isEndDropTarget =
                            isLastTile && DragDrop.getDropId pm.dragDrop == Just (DropRack 999)

                        isStartDropTarget =
                            viewIndex == 0 && DragDrop.getDropId pm.dragDrop == Just (DropRack 998)
                    in
                    viewRackTile pm originalIndex t isEndDropTarget isStartDropTarget
                )
                sortedRack

        endDropIndicator =
            Html.div
                (style "position" "absolute"
                    :: style "left" "50%"
                    :: style "right" "0"
                    :: style "top" "0"
                    :: style "bottom" "0"
                    :: DragDrop.droppable DragDropMsg (DropRack 999)
                )
                []

        startDropIndicator =
            Html.div
                (style "position" "absolute"
                    :: style "left" "0"
                    :: style "right" "50%"
                    :: style "top" "0"
                    :: style "bottom" "0"
                    :: DragDrop.droppable DragDropMsg (DropRack 998)
                )
                []

        ( first, rest ) =
            List.Extra.splitAt (Array.length pm.rack - 3) rackViews
    in
    div [ class "rack", style "position" "relative" ]
        -- The last 3 tiles go in a separate div, so that they wrap together
        (startDropIndicator :: first ++ [ div [ style "position" "relative", style "z-index" "1" ] rest, endDropIndicator ])


viewRackTile : PlayingModel -> Int -> RackTile -> Bool -> Bool -> Html Msg
viewRackTile pm index tile isEndDropTarget isStartDropTarget =
    let
        dragDropAttr =
            if pm.gameOver || tile.placement /= Nothing then
                []

            else
                DragDrop.draggable DragDropMsg index ++ DragDrop.droppable DragDropMsg (DropRack index)

        dropClass =
            if DragDrop.getDropId pm.dragDrop == Just (DropRack index) then
                [ class "drop-target" ]

            else if isEndDropTarget then
                [ class "drop-target-end" ]

            else if isStartDropTarget then
                [ class "drop-target" ]

            else
                []
    in
    button
        ([ class "rack-tile"
         , onClick (PlaceTile index)
         , disabled (pm.gameOver || tile.placement /= Nothing)
         ]
            ++ dragDropAttr
            ++ dropClass
        )
        [ viewTile tile.tile False True ]


viewGrid : Array2D CellProps -> Html Msg
viewGrid cellProps =
    let
        partialGrid =
            div [ class "grid" ]
                (cellProps
                    |> Array2D.indexedMap (\y x p -> viewCell (Point x y) p)
                    |> Array2D.Extra.flattenToList
                )
    in
    Html.node "scroll-repeat"
        [ class "scroll-repeat-view" ]
        -- We use a zero-size div to force the xy coords for panzoom to be the top-left of the grid.
        [ div [ style "width" "0", style "height" "0" ]
            [ div
                [ id "super-grid" ]
                (List.repeat 9 partialGrid)
            ]
        ]


getCellProps : PlayingModel -> Point -> CellProps
getCellProps model point =
    { state = getCellState model point
    , contents =
        getCellContents model point
    , multiplier = Array2D.get point.x point.y Checker.multipliers |> Maybe.withDefault (Multiplier 1 1)
    , isDropTarget = DragDrop.getDropId model.dragDrop == Just (DropBoard point)
    }


getCellState : PlayingModel -> Point -> CellSelection
getCellState model point =
    let
        -- computes the desired highlight opacity in one dimension
        highlightOpacity selected current =
            (1 - toFloat (Basics.modBy gridSize (current - selected)) / 10)
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
    let
        dropAttr =
            case state.contents of
                Placed _ ->
                    []

                _ ->
                    DragDrop.droppable DragDropMsg (DropBoard point)
    in
    div
        ([ onClick (Select point)
         , class "cell"
         , classList <|
            if state.contents == Empty then
                [ ( "cell-2w", state.multiplier.word == 2 )
                , ( "cell-3w", state.multiplier.word == 3 )
                , ( "cell-2l", state.multiplier.letter == 2 )
                , ( "cell-3l", state.multiplier.letter == 3 )
                , ( "cell-origin", point == Point (gridSize // 2) (gridSize // 2) )
                , ( "cell-selected", state.state == Selected )
                , ( "cell-drop-target", state.isDropTarget )
                ]

            else
                [ ( "cell-drop-target", state.isDropTarget ) ]
         ]
            ++ dropAttr
        )
        [ case ( state.contents, state.state ) of
            ( Empty, Inactive ) ->
                text ""

            ( Empty, Selected ) ->
                div [ class "cell-select-highlight" ] []

            ( Empty, SelectionHighlight highlightStrength ) ->
                Html.Extra.viewIf (highlightStrength > 0) <|
                    div
                        [ class "cell-select-highlight"
                        , style "opacity" (String.fromFloat highlightStrength)
                        ]
                        []

            ( Placed { tile, justPlaced }, _ ) ->
                viewTile tile justPlaced False

            ( Preview { tile, rackIndex }, _ ) ->
                div (DragDrop.draggable DragDropMsg rackIndex) [ viewTile tile False True ]
        ]


viewTile : Tile -> Bool -> Bool -> Html msg
viewTile tile isJustPlaced isPreview =
    div
        [ classList [ ( "tile", True ), ( "preview-tile", isPreview ), ( "just-placed-tile", isJustPlaced ) ] ]
        [ div [ class "tile-value " ] [ text (getLetterValue tile |> String.fromInt) ]
        , text (String.fromChar tile)
        ]


getUnseenTiles : PlayingModel -> List Tile
getUnseenTiles pm =
    pm.bag ++ Array.toList pm.opponent.rack |> List.sort


viewUnseenTilesDialog : List Tile -> Html Msg
viewUnseenTilesDialog unseenTiles =
    let
        groups =
            unseenTiles
                |> List.Extra.frequencies
                |> List.map (\( t, c ) -> t |> List.repeat c |> String.fromList)
    in
    dialog
        [ id "unseenTilesDialog" ]
        [ h1 [] [ text "Unseen tiles" ]
        , p [ style "font-family" "System Mono", style "font-size" "1.3em" ]
            (groups |> List.map text |> List.intersperse (text " "))
        , div [ class "dialog-action-buttons" ]
            [ viewCloseDialogButton [ text "Back" ] ]
        ]
