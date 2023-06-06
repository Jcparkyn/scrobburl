module Checker exposing (CheckerModel, getAllLines, getLetterValue, letterValues, scoreMove)

import Array exposing (Array)
import Array2D exposing (Array2D)
import Data exposing (CellContents(..), Point, RackState, Tiles, boardIsEmpty, getAllCellContents, getTileFromTiles)
import Dict
import List.Extra
import Set exposing (Set)


type alias CheckerModel =
    { board : Tiles
    , rack : RackState
    , wordlist : Set String
    }


scoreMove : CheckerModel -> Maybe Int
scoreMove model =
    if isValidPlacement model then
        getAllLines (getAllCellContents { board = model.board, rack = model.rack })
            |> List.map (scoreLine model.wordlist)
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


isConsecutive : List Int -> Bool
isConsecutive list =
    let
        sorted =
            List.sort list
    in
    sorted
        -- Get differences between each element
        |> List.map2 (-) (List.drop 1 sorted)
        |> List.all ((==) 1)


{-| This intentionally allows lines containing multiple separate words, might be reconsidered later.
-}
isValidPlacement : CheckerModel -> Bool
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
            if boardIsEmpty model.board then
                -- Tiles should be consecutive and pass through centre.
                (line |> List.map .index |> isConsecutive)
                    && (placements |> List.member (Point 7 7))
                    && (List.length placements > 1)

            else
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


scoreLine : Set String -> Array CellContents -> Maybe Int
scoreLine wordlist line =
    let
        tilesToString =
            List.map .tile >> String.fromList

        scoreWord tiles =
            if Set.member (tilesToString tiles) wordlist then
                Just (tiles |> List.map (.tile >> getLetterValue) |> List.sum)

            else
                Nothing
    in
    line
        |> Array.toList
        |> List.map
            (\cell ->
                case cell of
                    Preview tile ->
                        Just { tile = tile, isPreview = True }

                    Placed tile ->
                        Just { tile = tile, isPreview = False }

                    _ ->
                        Nothing
            )
        |> splitByNothings
        |> List.filter (List.any .isPreview)
        |> List.filter (\word -> List.length word > 1)
        |> List.map scoreWord
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


letterValues : Dict.Dict Char Int
letterValues =
    Dict.fromList
        [ ( 'A', 1 )
        , ( 'B', 4 )
        , ( 'C', 4 )
        , ( 'D', 2 )
        , ( 'E', 1 )
        , ( 'F', 4 )
        , ( 'G', 3 )
        , ( 'H', 3 )
        , ( 'I', 1 )
        , ( 'J', 10 )
        , ( 'K', 5 )
        , ( 'L', 2 )
        , ( 'M', 4 )
        , ( 'N', 2 )
        , ( 'O', 1 )
        , ( 'P', 4 )
        , ( 'Q', 10 )
        , ( 'R', 1 )
        , ( 'S', 1 )
        , ( 'T', 1 )
        , ( 'U', 2 )
        , ( 'V', 5 )
        , ( 'W', 4 )
        , ( 'X', 8 )
        , ( 'Y', 3 )
        , ( 'Z', 10 )
        ]


getLetterValue : Char -> Int
getLetterValue letter =
    Dict.get letter letterValues |> Maybe.withDefault 0
