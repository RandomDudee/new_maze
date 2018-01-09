module Maze exposing (..)

import Grid exposing (Grid)
import Types exposing (Boundary(..))
import Position exposing (Position, Direction(..))
import List.Extra

type alias Maze =
  { width: Int
  , height : Int
  , vertical : Grid Boundary
  , horizontal : Grid Boundary
  }

create : Int -> Int -> Maze
create w h =

      { width = w
      ,height = h
      , vertical = Grid.create (w + 1) h Wall
      , horizontal = Grid.create w (h + 1)  Wall}

getBoundary : Direction -> Position -> Maze -> Boundary
getBoundary dir pos maze =
  let
    convertPos : Direction -> Position -> Position
    convertPos dir (x,y) =
      case dir of
        North ->
          (x,y+1)
        South ->
          (x,y)
        East ->
          (x+1, y)
        West ->
          (x,y)

    getGrid : Direction -> Maze -> Grid Boundary
    getGrid dir maze =
      case dir of
        North -> maze.horizontal
        South -> maze.horizontal
        East -> maze.vertical
        West -> maze.vertical
  in
    getGrid dir maze
    |> Grid.get (convertPos dir pos)
    |> Maybe.withDefault Wall

allPositions : Maze -> List Position
allPositions maze =
  let
    xs = List.range 0 (maze.width - 1)
    ys = List.range 0 (maze.height - 1)
  in
    List.Extra.lift2 (\x y -> (x,y) ) xs ys
