module Checker exposing (getAllLines, scoreMove)

import Array exposing (Array)
import Array2D exposing (Array2D)
import Data exposing (CellContents(..), Model, Point, getAllCellContents, getTile, getTileFromTiles)
import List.Extra


scoreMove : Model -> Maybe Int
scoreMove model =
    if isValidPlacement model then
        getAllLines (getAllCellContents model)
            |> List.map scoreLine
            |> sumScores

    else
        Nothing


sumScores : List (Maybe Int) -> Maybe Int
sumScores scores =
    let
        validScores =
            List.filterMap identity scores
    in
    if List.length scores == List.length validScores then
        Just (List.sum validScores)

    else
        Nothing


isValidPlacement : Model -> Bool
isValidPlacement model =
    let
        placements : List Point
        placements =
            model.rack
                |> Array.toList
                |> List.filterMap .placement

        isPlaced point =
            getTileFromTiles point model.board /= Nothing

        isAnchored : Point -> Bool
        isAnchored point =
            [ Point point.x (point.y + 1)
            , Point point.x (point.y - 1)
            , Point (point.x + 1) point.y
            , Point (point.x - 1) point.y
            ]
                |> List.any isPlaced
    in
    case getPlacementLine placements of
        Just line ->
            line
                |> List.map (\p -> { index = p.index, anchored = isAnchored p.pos })
                |> isValidPlacementLine

        Nothing ->
            False


isValidPlacementLine : List { index : Int, anchored : Bool } -> Bool
isValidPlacementLine points =
    points
        |> List.sortBy .index
        |> List.Extra.groupWhile (\a b -> a.index + 1 == b.index)
        |> List.map (\group -> Tuple.first group :: Tuple.second group)
        |> List.all (List.any .anchored)


{-| Checks if a list of points are in a line, and returns the points along with their
positions along the line (horizontal or vertical)
-}
getPlacementLine : List Point -> Maybe (List { pos : Point, index : Int })
getPlacementLine points =
    case points of
        [] ->
            Nothing

        first :: _ ->
            let
                getLine fn =
                    Just (List.map (\p -> { pos = p, index = fn p }) points)
            in
            if List.all (\p -> p.x == first.x) points then
                getLine .y

            else if List.all (\p -> p.y == first.y) points then
                getLine .x

            else
                Nothing


getAllLines : Array2D a -> List (Array a)
getAllLines grid =
    let
        rows =
            Array.toList grid.data

        columns =
            List.range 0 (grid.columns - 1)
                |> List.map (\col -> Array2D.getColumn col grid)
                |> List.filterMap identity
    in
    rows ++ columns


scoreLine : Array CellContents -> Maybe Int
scoreLine line =
    line
        |> Array.toList
        |> List.map getTile
        |> splitByNothings
        |> List.map (String.fromList >> scoreWord)
        |> sumScores


splitByNothings : List (Maybe a) -> List (List a)
splitByNothings list =
    list
        |> List.foldr
            (\maybeTile state ->
                case maybeTile of
                    Just tile ->
                        { state | current = tile :: state.current }

                    Nothing ->
                        { prev = state.current :: state.prev, current = [] }
            )
            { prev = [], current = [] }
        |> (\state -> state.current :: state.prev)
        |> List.filter (List.isEmpty >> not)


scoreWord : String -> Maybe Int
scoreWord word =
    let
        len =
            String.length word
    in
    if len == 1 then
        Just 0

    else if len > 2 && String.length word < 5 then
        Just len

    else
        Nothing
