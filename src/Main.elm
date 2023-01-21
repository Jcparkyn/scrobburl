module Main exposing (main)

import Array2D exposing (Array2D, get, repeat, set)
import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Tile =
    Char


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
    , rack : List Tile
    }


placedTiles : Tiles
placedTiles =
    repeat 5 5 Nothing
        |> set 2 2 (Just 'N')
        |> set 3 2 (Just 'I')


init : flags -> ( Model, Cmd msg )
init _ =
    ( { selectedCell = Point 0 0
      , selectDirection = Right
      , board = placedTiles
      , rack = [ 'A', 'Z', 'B', 'D' ]
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Select Point


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Select point ->
            ( { model
                | selectedCell = point
                , selectDirection =
                    if model.selectedCell == point then
                        swapDirection model.selectDirection

                    else
                        model.selectDirection
              }
            , Cmd.none
            )


-- VIEW


view : Model -> Browser.Document Msg
view model =
    { body =
        [ div []
            [ viewGrid model
            , viewRack model.rack
            ]
        ]
    , title = "Scrobburl"
    }


gridSize : number
gridSize =
    5


viewRack : List Tile -> Html msg
viewRack rack =
    div []
        (rack |> List.map viewTile)


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
        case model.board |> get point.x point.y of
            Just (Just tile) ->
                Placed tile

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
        , class "cell"
        , style "background-color" (cellColor state.state)
        ]
        [ case state.contents of
            Empty ->
                text ""

            Placed tile ->
                viewTile tile

            Preview char ->
                viewTile char
        ]


viewTile : Tile -> Html msg
viewTile tile =
    div
        [ class "tile"]
        [ text (String.fromChar tile) ]


cellColor : CellSelection -> String
cellColor state =
    case state of
        Selected ->
            "red"

        Highlight ->
            "pink"

        Inactive ->
            "transparent"
