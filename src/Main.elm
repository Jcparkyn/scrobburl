module Main exposing (main)

import Array2D exposing (Array2D, get, repeat, set)
import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


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
    | Preview Char
    | Placed Char


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
    Array2D (Maybe Char)


placedTiles : Tiles
placedTiles =
    repeat 5 5 Nothing
        |> set 2 2 (Just 'N')
        |> set 3 2 (Just 'I')


type alias Model =
    { selectedCell : Point
    , selectDirection : SelectDirection
    , tiles : Tiles
    }


init : Model
init =
    { selectedCell = Point 0 0
    , selectDirection = Right
    , tiles = placedTiles
    }



-- UPDATE


type Msg
    = Select Point


update : Msg -> Model -> Model
update msg model =
    case msg of
        Select point ->
            { model
                | selectedCell = point
                , selectDirection =
                    if model.selectedCell == point then
                        swapDirection model.selectDirection

                    else
                        model.selectDirection
            }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewGrid model
        ]


gridSize : number
gridSize =
    5


viewGrid : Model -> Html Msg
viewGrid model =
    div
        [ style "display" "grid"
        , style "grid-template-columns" "50px 50px 50px 50px 50px"
        , style "user-select" "none"
        ]
        (pointGrid gridSize
            |> List.map (\point -> viewCell point (getCellProps model point))
        )


pointGrid : Int -> List Point
pointGrid size =
    List.range 0 (size - 1)
        |> List.concatMap
            (\y ->
                List.range 0 (gridSize - 1)
                    |> List.map (\x -> Point x y)
            )


getCellProps : Model -> Point -> CellProps
getCellProps model point =
    { state = getCellState model point
    , contents =
        case model.tiles |> get point.x point.y of
            Just (Just char) ->
                Placed char

            _ ->
                Empty
    }


getCellState : Model -> Point -> CellSelection
getCellState model point =
    if model.selectedCell == point then
        Selected

    else if
        (model.selectDirection == Right)
            && (model.selectedCell.y == point.y)
            && (model.selectedCell.x < point.x)
    then
        Highlight

    else if
        (model.selectDirection == Down)
            && (model.selectedCell.x == point.x)
            && (model.selectedCell.y < point.y)
    then
        Highlight

    else
        Inactive


viewCell : Point -> CellProps -> Html Msg
viewCell point state =
    div
        [ onClick (Select point)
        , style "border" "1px solid black"
        , style "width" "50px"
        , style "height" "50px"
        , style "line-height" "50px"
        , style "background-color" (cellColor state.state)
        , style "text-align" "center"
        , style "font-size" "2em"
        ]
        [ text
            (case state.contents of
                Empty ->
                    ""

                Placed char ->
                    String.fromChar char

                Preview char ->
                    String.fromChar char
            )
        ]


cellColor : CellSelection -> String
cellColor state =
    case state of
        Selected ->
            "red"

        Highlight ->
            "pink"

        Inactive ->
            "transparent"
