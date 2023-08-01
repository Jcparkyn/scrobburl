module Data exposing (..)

import Array exposing (Array)
import Array.Extra
import Array2D exposing (Array2D)
import Point exposing (Point)


type alias Tile =
    Char


type alias RackTile =
    { tile : Tile
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


getTile : CellContents -> Maybe Tile
getTile cell =
    case cell of
        Empty ->
            Nothing

        Preview tile ->
            Just tile

        Placed tile ->
            Just tile


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


type alias Opponent =
    { name : String
    , score : Int
    }


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
            Array.Extra.map2 RackTile rack finalPositions
