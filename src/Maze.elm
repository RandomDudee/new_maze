module Maze exposing (..)

import Array exposing (Array)
import Grid exposing (Grid)
import Types exposing (Boundary(..))
import Position exposing (Position, Direction(..))
import List.Extra



type alias Maze =
    { width : Int
    , height : Int
    , vertical : Grid Boundary
    , horizontal : Grid Boundary
    }


create : Int -> Int -> Maze
create w h =
    { width = w
    , height = h
    , vertical = Grid.create (w + 1) h Wall
    , horizontal = Grid.create w (h + 1) Wall
    }

setBoundary : Direction -> Position -> Boundary -> Maze -> Maze
setBoundary dir pos boundary maze =
      let
          convertPos : Direction -> Position -> Position
          convertPos dir ( x, y ) =
              case dir of
                  North ->
                      ( x, y + 1 )

                  South ->
                      ( x, y )

                  East ->
                      ( x + 1, y )

                  West ->
                      ( x, y )

          getGrid : Direction -> Maze -> Grid Boundary
          getGrid dir maze =
              case dir of
                  North ->
                      maze.horizontal

                  South ->
                      maze.horizontal

                  East ->
                      maze.vertical

                  West ->
                      maze.vertical

          setGrid : Direction -> Grid Boundary -> Maze -> Maze
          setGrid dir grid maze  =
                case dir of
                    North ->
                      { maze | horizontal = grid }

                    South ->
                      { maze | horizontal = grid }

                    East ->
                      { maze | vertical = grid }

                    West ->
                      { maze | vertical = grid }


          newGrid = getGrid dir maze
             |> Grid.set (convertPos dir pos) boundary
        in
          setGrid dir newGrid maze


getBoundary : Direction -> Position -> Maze -> Boundary
getBoundary dir pos maze =
    let
        convertPos : Direction -> Position -> Position
        convertPos dir ( x, y ) =
            case dir of
                North ->
                    ( x, y + 1 )

                South ->
                    ( x, y )

                East ->
                    ( x + 1, y )

                West ->
                    ( x, y )

        getGrid : Direction -> Maze -> Grid Boundary
        getGrid dir maze =
            case dir of
                North ->
                    maze.horizontal

                South ->
                    maze.horizontal

                East ->
                    maze.vertical

                West ->
                    maze.vertical
    in
        getGrid dir maze
            |> Grid.get (convertPos dir pos)
            |> Maybe.withDefault Wall


allPositions : Maze -> List Position
allPositions maze =
    let
        xs =
            List.range 0 (maze.width - 1)

        ys =
            List.range 0 (maze.height - 1)
    in
        List.Extra.lift2 (\x y -> ( x, y )) xs ys


allHorizontalWalls : Maze -> List Position
allHorizontalWalls maze =
    Grid.filterPositions (\b -> b == Wall) maze.horizontal
        |> List.map (\tuple -> Tuple.first tuple)


allVerticalWalls : Maze -> List Position
allVerticalWalls maze =
    Grid.filterPositions (\b -> b == Wall) maze.vertical
        |> List.map (\tuple -> Tuple.first tuple)


createCell : Array Direction -> Int -> Position -> Maze -> Maze
createCell directions width (  x, y ) maze =
    let
        index =
            (y * width) + x

        direction =
            Maybe.withDefault North (Array.get index directions)

        filterDirection : Direction -> Maybe Direction
        filterDirection filterDir =
            let
                lastX =
                    width - 1

                topY =
                    y == 0

                rightX =
                    x == lastX
            in
                if topY && not rightX then
                    Just East
                else if rightX && not topY then
                    Just North
                else if topY && rightX then
                    Nothing
                else
                    Just direction
    in
        case filterDirection direction of
            Just filteredDirection ->
                setBoundary
                    filteredDirection
                    (x,y)
                    Path 
                    maze

            Nothing ->
                maze
