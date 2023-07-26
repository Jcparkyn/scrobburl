module Checker exposing (CheckerModel, CheckerResult(..), getAllLines, getLetterValue, gridSize, letterValues, multipliers, scoreMove)

import Array exposing (Array)
import Array2D exposing (Array2D)
import Array2D.Extra
import Data exposing (CellContents(..), Multiplier, Point, RackState, Tiles, boardIsEmpty, getAllCellContents, getTileFromTiles)
import Dict
import List.Extra
import Set exposing (Set)


type alias CheckerModel =
    { board : Tiles
    , rack : RackState
    , wordlist : Set String
    }


type alias ScoreWordResult =
    { word : String, score : Maybe Int }


type CheckerResult
    = InvalidPlacement
    | ValidPlacement { score : Int, invalidWords : List String }


gridSize : Int
gridSize =
    14


multipliers : Array2D Multiplier
multipliers =
    let
        normalize n =
            abs (n - gridSize // 2)

        multDict =
            Dict.fromList
                [ ( ( 7, 7 ), Multiplier 1 3 )
                , ( ( 7, 0 ), Multiplier 1 3 )
                , ( ( 0, 7 ), Multiplier 1 3 )
                , ( ( 7, 4 ), Multiplier 2 1 )
                , ( ( 4, 7 ), Multiplier 2 1 )
                , ( ( 2, 6 ), Multiplier 3 1 )
                , ( ( 6, 2 ), Multiplier 3 1 )
                , ( ( 1, 5 ), Multiplier 2 1 )
                , ( ( 0, 4 ), Multiplier 2 1 )
                , ( ( 4, 0 ), Multiplier 2 1 )
                , ( ( 5, 1 ), Multiplier 2 1 )
                , ( ( 6, 6 ), Multiplier 1 2 )
                , ( ( 5, 5 ), Multiplier 1 2 )
                , ( ( 4, 4 ), Multiplier 1 2 )
                , ( ( 3, 3 ), Multiplier 1 2 )
                , ( ( 2, 2 ), Multiplier 3 1 )
                , ( ( 1, 1 ), Multiplier 2 1 )
                , ( ( 0, 0 ), Multiplier 1 2 )
                ]
    in
    Array2D.initialize gridSize
        gridSize
        (\x y ->
            multDict
                |> Dict.get ( normalize x, normalize y )
                |> Maybe.withDefault (Multiplier 1 1)
        )


scoreMove : CheckerModel -> CheckerResult
scoreMove model =
    if isValidPlacement model then
        getAllCellContents { board = model.board, rack = model.rack }
            |> Array2D.Extra.map2 Tuple.pair multipliers
            |> getAllLines
            |> List.concatMap (scoreLine model.wordlist)
            |> sumScores

    else
        InvalidPlacement


sumScores : List ScoreWordResult -> CheckerResult
sumScores scores =
    let
        validScores =
            List.filterMap .score scores

        total =
            List.sum validScores
    in
    ValidPlacement
        { score = total
        , invalidWords =
            scores
                |> List.filter (\s -> s.score == Nothing)
                |> List.map .word
        }


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


scoreLine : Set String -> Array ( Multiplier, CellContents ) -> List ScoreWordResult
scoreLine wordlist line =
    let
        tilesToString =
            List.map .tile >> String.fromList

        wordMultiplier tiles =
            tiles |> List.filter .isPreview |> List.map (.multiplier >> .word) |> List.product

        letterMultiplier tile =
            if tile.isPreview then
                tile.multiplier.letter

            else
                1

        baseScore tiles =
            tiles |> List.map (\t -> getLetterValue t.tile * letterMultiplier t) |> List.sum

        scoreWord tiles =
            { word = tilesToString tiles
            , score =
                if Set.member (tilesToString tiles) wordlist then
                    Just (baseScore tiles * wordMultiplier tiles)

                else
                    Nothing
            }
    in
    line
        |> Array.toList
        |> List.map
            (\cell ->
                case cell of
                    ( mult, Preview tile ) ->
                        Just { tile = tile, isPreview = True, multiplier = mult }

                    ( mult, Placed tile ) ->
                        Just { tile = tile, isPreview = False, multiplier = mult }

                    _ ->
                        Nothing
            )
        |> splitByNothings
        |> List.filter (List.any .isPreview)
        |> List.filter (\word -> List.length word > 1)
        |> List.map scoreWord


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
