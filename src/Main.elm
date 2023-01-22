module Main exposing (..)

import Array exposing (Array)
import Array2D
import Browser
import Checker exposing (isValidMove)
import Data exposing (..)
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


gridSize : number
gridSize =
    9


placedTiles : Tiles
placedTiles =
    Array2D.repeat gridSize gridSize Nothing
        |> Array2D.set 1 2 (Just 'A')
        |> Array2D.set 2 2 (Just 'B')
        |> Array2D.set 3 2 (Just 'C')


init : flags -> ( Model, Cmd msg )
init _ =
    ( { selectedCell = Point 0 0
      , selectDirection = Right
      , board = placedTiles
      , rack =
            [ 'A', 'Z', 'B', 'D', 'O', 'Y', 'I' ]
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
    case (getCellProps model model.selectedCell).contents of
        Placed _ ->
            model

        _ ->
            let
                { x, y } =
                    model.selectedCell
            in
            { model
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
            [ viewPreviewScore model
            , viewGrid model
            , viewRack model.rack
            ]
        ]
    , title = "Scrobburl"
    }


viewPreviewScore : Model -> Html msg
viewPreviewScore model =
    div []
        [ text
            (if isValidMove model then
                "Hell yeah"

             else
                "Nah homie"
            )
        ]


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
        [ class "grid" ]
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
