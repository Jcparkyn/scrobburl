module Main exposing (main)

import Array exposing (Array)
import Array2D exposing (Array2D)
import Browser
import Html exposing (Html, button, div, main_, text)
import Html.Attributes exposing (class, classList, disabled, style)
import Html.Events exposing (onClick)


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

    -- , previewTiles : Tiles
    , rack : RackState
    }


placedTiles : Tiles
placedTiles =
    Array2D.repeat 5 5 Nothing
        |> Array2D.set 2 2 (Just 'N')
        |> Array2D.set 3 2 (Just 'I')


init : flags -> ( Model, Cmd msg )
init _ =
    ( { selectedCell = Point 0 0
      , selectDirection = Right
      , board = placedTiles

      --   , previewTiles = Array2D.repeat 5 5 Nothing
      , rack =
            [ 'A', 'Z', 'B', 'D' ]
                |> List.map (\c -> RackTile c Nothing)
                |> Array.fromList
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Select Point
    | PlaceTile Int


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case Debug.log "msg" msg of
        Select point ->
            ( withSelection model point
            , Cmd.none
            )

        PlaceTile rackIndex ->
            ( withPlacedTile model rackIndex, Cmd.none )


withSelection : Model -> Point -> Model
withSelection model point =
    case (getCellProps model point).contents of
        Placed _ ->
            model

        Preview _ ->
            { model
                | selectedCell = point
                , rack =
                    model.rack
                        |> Array.map
                            (\tile ->
                                if tile.placement == Just point then
                                    { tile | placement = Nothing }

                                else
                                    tile
                            )
            }

        Empty ->
            { model
                | selectedCell = point
                , selectDirection =
                    if model.selectedCell == point then
                        swapDirection model.selectDirection

                    else
                        model.selectDirection
            }


withPlacedTile : Model -> Int -> Model
withPlacedTile model rackIndex =
    let
        { x, y } =
            model.selectedCell

        -- tile =
        --     model.rack
        --         |> Array.get rackIndex
        --         |> Maybe.map .tile
    in
    { model
      -- | previewTiles = model.previewTiles |> Array2D.set x y tile
        | selectedCell =
            case model.selectDirection of
                Right ->
                    Point (x + 1) y

                Down ->
                    Point x (y + 1)
        , rack =
            model.rack
                |> updateElement rackIndex (\t -> { t | placement = Just model.selectedCell })
    }


updateElement : Int -> (a -> a) -> Array a -> Array a
updateElement index fun array =
    case Array.get index array of
        Just value ->
            array |> Array.set index (fun value)

        Nothing ->
            array



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { body =
        [ main_ []
            [ viewGrid model
            , viewRack model.rack
            ]
        ]
    , title = "Scrobburl"
    }


gridSize : number
gridSize =
    5


viewRack : RackState -> Html Msg
viewRack rack =
    div [ class "rack" ]
        (rack |> Array.toList |> List.indexedMap viewRackTile)


viewRackTile : Int -> RackTile -> Html Msg
viewRackTile index tile =
    button
        [ class "rack-tile"
        , onClick (PlaceTile index)
        , disabled (tile.placement /= Nothing)
        , style "opacity"
            (case tile.placement of
                Just _ ->
                    "0.5"

                Nothing ->
                    "1"
            )
        ]
        [ viewTile tile.tile True ]


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
        getCellContents model point
    }


getCellContents : Model -> Point -> CellContents
getCellContents model point =
    case model.board |> getCell point of
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


getCell : Point -> Array2D b -> Maybe b
getCell point board =
    board |> Array2D.get point.x point.y


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
                viewTile tile False

            Preview tile ->
                viewTile tile True
        ]


viewTile : Tile -> Bool -> Html msg
viewTile tile isPreview =
    div
        [ classList [ ( "tile", True ), ( "preview-tile", isPreview ) ] ]
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
