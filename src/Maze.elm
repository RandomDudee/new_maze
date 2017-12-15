module Maze exposing (..)

import Grid exposing (Grid)
import Types exposing (Boundary(..))
import Position exposing (Position, Direction(..))

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

getBoundary : Maze -> Position -> Direction -> Boundary
getBoundary maze pos dir =
  let
    myGrid = getGrid maze dir

  in
    Grid.get pos myGrid
    |> Maybe.withDefault Wall
      

getGrid : Maze -> Direction -> Grid Boundary
getGrid maze dir =
  case dir of
    North -> maze.horizontal
    South -> maze.horizontal
    East -> maze.vertical
    West -> maze.vertical
