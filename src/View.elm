module View exposing (view)

import Array exposing (Array)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List exposing (concat)
import Maze exposing (Maze)
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Position exposing (Direction(..), Position)
import Types exposing (Boundary(..))


mainContainer : Html.Attribute msg
mainContainer =
    style
        [ ( "backgroundColor", "" )
        , ( "width", "auto" )
        , ( "padding", "25px" )
        , ( "position", "center" )
        , ( "text-align", "center" )
        , ( "margin", "auto" )
        ]


view : Maze -> Html msg
view maze =
    div [] [ div [ mainContainer ] [ drawLineyMaze maze ] ]

drawNorthWall : Int -> Coordinate -> S.Svg msg
drawNorthWall scale { x, y } =
    drawWall { x = x, y = y } { x = x + scale, y = y }


drawEastWall : Int -> Coordinate -> S.Svg msg
drawEastWall scale { x, y } =
    drawWall { x = x + scale, y = y } { x = x + scale, y = y + scale }


drawWall : Coordinate -> Coordinate -> S.Svg msg
drawWall c1 c2 =
    S.line [ SA.x1 (toString c1.x), SA.y1 (toString c1.y), SA.x2 (toString c2.x), SA.y2 (toString c2.y), SA.style "stroke:rgb(255,0,0);stroke-width:2" ] []


drawLineyMaze : Maze -> Html msg
drawLineyMaze maze =
    --turn maze into grid of cell and position using indexedMap
    let
        scale =
            40

        width =
            maze.width

        height =
            maze.height

        fn : ( Position, Maze ) -> List (S.Svg a)
        fn ( position, maze ) =
            drawCell scale position maze

        viewWidth =
            width
                * scale
                |> (+) 2
                |> toString

        viewHeight =
            height
                * scale
                |> (+) 2
                |> toString

        viewbox : String
        viewbox =
            "-1 -1 " ++ viewWidth ++ " " ++ viewHeight

        asHtml : List (S.Svg msg) -> Html msg
        asHtml svgMsgs =
            S.svg [ SA.width viewWidth, SA.height viewHeight, SA.viewBox viewbox ] svgMsgs
    in
        drawBoundary scale width height
            |> asHtml


drawBoundary : Int -> Int -> Int -> List (S.Svg msg)
drawBoundary scale width height =
    let
        actualHeight =
            scale * height

        actualWidth =
            scale * width
    in
        [ drawWall { x = 0, y = 0 } { x = 0, y = actualHeight }, drawWall { x = 0, y = actualHeight } { x = actualWidth, y = actualHeight } ]


drawCell : Int -> Position -> Maze -> List (S.Svg msg)
drawCell scale position maze =
    let
        coordinateOfCell =
            asCoordinate position scale

        isNorthWall =
            Maze.getBoundary North position maze == Wall

        isEastWall =
            Maze.getBoundary East position maze == Wall

        drawNorthWithScaleAndCoords =
            drawNorthWall scale coordinateOfCell

        drawEastWithScaleAndCoords =
            drawEastWall scale coordinateOfCell
    in
        case ( isNorthWall, isEastWall ) of
            ( True, True ) ->
                [ drawNorthWithScaleAndCoords, drawEastWithScaleAndCoords ]

            ( True, False ) ->
                [ drawNorthWithScaleAndCoords ]

            ( False, True ) ->
                [ drawEastWithScaleAndCoords ]

            ( False, False ) ->
                []


type alias Coordinate =
    { x : Int, y : Int }


asCoordinate : Position -> Int -> Coordinate
asCoordinate ( x, y ) scale =
    { x = x * scale, y = y * scale }
