module Data exposing (..)

import Array exposing (Array)
import Array.Extra
import Array2D exposing (Array2D)
import Array2D.Extra
import Set exposing (Set)


type alias Tile =
    Char


type alias RackTile =
    { tile : Tile
    , placement : Maybe Point
    }


type alias RackState =
    Array RackTile


type alias Point =
    { x : Int
    , y : Int
    }


type CellSelection
    = Selected SelectDirection
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


boardIsEmpty : Tiles -> Bool
boardIsEmpty =
    Array2D.Extra.all (\tile -> tile == Nothing)


getTileFromTiles : Point -> Tiles -> Maybe Tile
getTileFromTiles point tiles =
    Array2D.get point.x point.y tiles
        |> Maybe.andThen identity


type alias Opponent =
    { name : String
    , score : Int
    }


type Model
    = Playing PlayingModel


type alias PlayingModel =
    { selectedCell : Maybe Point
    , selectDirection : SelectDirection
    , board : Tiles
    , rack : RackState
    , opponent : Opponent
    , selfName : String
    , selfScore : Int
    , playedTurns : List PlayedTurn
    , initialSeed : Int
    , wordlist : Set String
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
