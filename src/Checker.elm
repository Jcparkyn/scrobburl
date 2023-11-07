module Checker exposing (CheckerModel, CheckerResult(..), ScoreWordResult, ScoringCellContents, getLetterValue, gridSize, maxRackSize, multipliers, scoreMove)

import Array exposing (Array)
import Array2D exposing (Array2D)
import Array2D.Extra
import Data exposing (CellContents(..), Multiplier, RackState, Tile, Tiles, getAllCellContents)
import Dict exposing (Dict)
import List.Extra
import Point exposing (Point)
import Set exposing (Set)


type alias CheckerModel =
    { board : Tiles
    , rack : RackState
    , wordlist : Set String
    }


type alias ScoreWordResult =
    { word : String, score : Int, legal : Bool, tiles : List ScoringCellContents }


type CheckerResult
    = NotThroughOrigin
    | NotEnoughTiles
    | NotAnchored
    | NotInLine
    | NothingPlaced
    | ValidPlacement { score : Int, words : List ScoreWordResult }


gridSize : Int
gridSize =
    14


maxRackSize : Int
maxRackSize =
    8


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


wordLengthBonuses : Dict Int Int
wordLengthBonuses =
    Dict.fromList
        [ ( 5, 5 )
        , ( 6, 15 )
        , ( 7, 30 )
        , ( 8, 50 )
        ]


scoreMove : CheckerModel -> CheckerResult
scoreMove model =
    let
        placements =
            model.rack
                |> Array.toList
                |> List.filterMap .placement

        isInLine points =
            case points of
                [] ->
                    False

                first :: rest ->
                    List.all (\p -> p.x == first.x) rest
                        || List.all (\p -> p.y == first.y) rest
    in
    if List.length placements == 0 then
        NothingPlaced

    else if not <| isInLine placements then
        NotInLine

    else
        let
            cellContents =
                getAllCellContents { board = model.board, rack = model.rack }

            occupied =
                cellContents |> Array2D.map ((/=) Empty)

            anchoredPoints =
                dfsGrid (Point 7 7) occupied
        in
        if Set.size anchoredPoints == 0 then
            NotThroughOrigin

        else if Set.size anchoredPoints == 1 then
            NotEnoughTiles

        else if Set.size anchoredPoints < Array2D.Extra.count identity occupied then
            NotAnchored

        else
            let
                wordScores =
                    cellContents
                        |> Array2D.Extra.map2 Tuple.pair multipliers
                        |> getAllLines
                        |> List.concatMap (scoreLine model.wordlist)
            in
            ValidPlacement
                { score = wordScores |> List.map .score |> List.sum
                , words = wordScores
                }


dfsGrid : Point -> Array2D Bool -> Set ( Int, Int )
dfsGrid start grid =
    let
        neighbours ( x, y ) =
            [ ( x, modBy gridSize (y + 1) )
            , ( x, modBy gridSize (y - 1) )
            , ( modBy gridSize (x + 1), y )
            , ( modBy gridSize (x - 1), y )
            ]

        iter ( x, y ) visited =
            if Set.member ( x, y ) visited || Array2D.get x y grid == Just False then
                visited

            else
                List.foldr iter (Set.insert ( x, y ) visited) (neighbours ( x, y ))
    in
    iter (Point.toTuple start) Set.empty


getAllLines : Array2D a -> List (Array a)
getAllLines grid =
    let
        rows =
            Array.toList grid.data

        columns =
            List.range 0 (grid.columns - 1)
                |> List.filterMap (\col -> Array2D.getColumn col grid)
    in
    rows ++ columns


type alias ScoringCellContents =
    { tile : Tile, isPreview : Bool, multiplier : Multiplier }


scoreLine : Set String -> Array ( Multiplier, CellContents ) -> List ScoreWordResult
scoreLine wordlist line =
    let
        rotate amount list =
            List.drop amount list ++ List.take amount list

        cells : List (Maybe ScoringCellContents)
        cells =
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

        rotatedCells =
            -- rotate so that words crossing the border are re-joined
            cells |> rotate (List.Extra.findIndex ((==) Nothing) cells |> Maybe.withDefault 0)

        newWords =
            rotatedCells
                |> splitByNothings
                |> List.filter (List.any .isPreview)
                |> List.filter (\word -> List.length word > 1)
    in
    List.map (scoreWord wordlist) newWords


scoreWord : Set String -> List ScoringCellContents -> ScoreWordResult
scoreWord wordlist tiles =
    let
        letterMultiplier tile =
            if tile.isPreview then
                tile.multiplier.letter

            else
                1

        baseScore =
            tiles |> List.map (\t -> getLetterValue t.tile * letterMultiplier t) |> List.sum

        wordMultiplier =
            tiles |> List.filter .isPreview |> List.map (.multiplier >> .word) |> List.product

        word =
            tiles |> List.map .tile |> String.fromList

        wordLengthBonus =
            wordLengthBonuses
                |> Dict.get (tiles |> List.Extra.count .isPreview)
                |> Maybe.withDefault 0
    in
    { word = word
    , score = baseScore * wordMultiplier + wordLengthBonus
    , legal = Set.member word wordlist
    , tiles = tiles
    }


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
