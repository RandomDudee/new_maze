module Maze exposing (..)

import Grid exposing (Grid)
import Types exposing (Boundary(..))

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
