module Data exposing (..)

import Array exposing (Array)
import Array2D exposing (Array2D)


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
    = Selected
    | Highlight
    | Inactive


type CellContents
    = Empty
    | Preview Tile
    | Placed Tile


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


type alias Model =
    { selectedCell : Point
    , selectDirection : SelectDirection
    , board : Tiles
    , rack : RackState
    }


getCellContents : Model -> Point -> CellContents
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


getAllCellContents : Model -> Array2D CellContents
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
