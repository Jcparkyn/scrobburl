module Data exposing (CellContents(..), CellProps, CellSelection(..), Multiplier, PlayedTurn(..), RackState, RackTile, SelectDirection(..), Tile, Tiles, getAllCellContents, isRackReset, playedTurnToRackState, resetRackState, shuffleRack, swapDirection)

import Array exposing (Array)
import Array.Extra
import Array2D exposing (Array2D)
import Point exposing (Point)


type alias Tile =
    Char


type alias RackTile =
    { sortIndex : Int
    , tile : Tile
    , placement : Maybe Point
    }


type alias RackState =
    Array RackTile


type CellSelection
    = Selected
    | SelectionHighlight Float
    | Inactive


type CellContents
    = Empty
    | Preview Tile
    | Placed Tile


type alias Multiplier =
    { letter : Int, word : Int }


type alias CellProps =
    { state : CellSelection
    , contents : CellContents
    , multiplier : Multiplier
    }


type SelectDirection
    = Right
    | Down


swapDirection : SelectDirection -> SelectDirection
swapDirection dir =
    case dir of
        Right ->
            Down

        Down ->
            Right


type alias Tiles =
    Array2D (Maybe Tile)


getAllCellContents : { board : Tiles, rack : RackState } -> Array2D CellContents
getAllCellContents model =
    let
        initialBoard =
            model.board
                |> Array2D.map (Maybe.map Placed >> Maybe.withDefault Empty)
    in
    Array.foldl
        (\previewTile board ->
            case previewTile.placement of
                Nothing ->
                    board

                Just p ->
                    Array2D.set p.x p.y (Preview previewTile.tile) board
        )
        initialBoard
        model.rack


type PlayedTurn
    = PlayedTurn (List { rackIndex : Int, position : Point })


playedTurnToRackState : PlayedTurn -> Array Tile -> RackState
playedTurnToRackState turn rack =
    case turn of
        PlayedTurn placements ->
            let
                initialPositions =
                    rack |> Array.map (\_ -> Nothing)

                updatePositions placement positions =
                    positions |> Array.set placement.rackIndex (Just placement.position)

                finalPositions =
                    placements
                        |> List.foldl updatePositions initialPositions
            in
            Array.Extra.map2 (RackTile 0) rack finalPositions


resetRackState : RackState -> RackState
resetRackState r =
    r |> Array.map (\t -> { t | placement = Nothing, sortIndex = 0 })


shuffleRack : RackState -> Array Int -> RackState
shuffleRack rack sortIndexes =
    Array.Extra.map2 (\sortIndex tile -> { tile | sortIndex = sortIndex }) sortIndexes rack


isRackReset : RackState -> Bool
isRackReset rack =
    rack |> Array.Extra.all (\t -> t.placement == Nothing && t.sortIndex == 0)
