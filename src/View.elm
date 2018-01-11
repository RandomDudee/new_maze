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


drawWall : Position -> Position -> S.Svg msg
drawWall ( x1, y1 ) ( x2, y2 ) =
    S.line [ SA.x1 (toString x1), SA.y1 (toString y1), SA.x2 (toString x2), SA.y2 (toString y2), SA.style "stroke:rgb(255,0,0);stroke-width:2" ] []


drawLineyMaze : Maze -> Html msg
drawLineyMaze maze =
    let
        scale =
            40

        width =
            maze.width

        height =
            maze.height

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

        vertWalls =
            Maze.allVerticalWalls maze
                |> List.map (drawVerticalWall (scaleBy scale))

        horizWalls =
            Maze.allHorizontalWalls maze
                |> List.map (drawHorizontalWall (scaleBy scale))
    in
        (vertWalls ++ horizWalls)
            |> asHtml


type alias Transformer =
    Position -> Position


scaleBy : Int -> Transformer
scaleBy scale ( x, y ) =
    ( x * scale, y * scale )


translate : Int -> Int -> Transformer
translate xoffset yoffset ( x, y ) =
    ( x + xoffset, y + yoffset )


drawHorizontalWall : Transformer -> Position -> S.Svg msg
drawHorizontalWall transform ( x, y ) =
    drawWall (transform ( x, y )) (transform ( x + 1, y ))


drawVerticalWall : Transformer -> Position -> S.Svg msg
drawVerticalWall transform ( x, y ) =
    drawWall (transform ( x, y )) (transform ( x, y + 1 ))
