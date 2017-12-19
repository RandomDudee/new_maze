module BinaryTree exposing (buildRandomMaze)

import Maze exposing(Maze)
import Array
import Random exposing (Generator)
import Position exposing (Direction(..))

buildRandomMaze : Int -> Int -> Int -> Maze
buildRandomMaze width height seedInt =
    let
        generator =
            Random.list (width * height) northOrEast

        seed =
            makeSeed seedInt

        ( directions, newSeed ) =
            Random.step generator seed
    in
        Maze.create width height

makeSeed : Int -> Random.Seed
makeSeed int =
    Random.initialSeed int

northOrEast : Generator Direction
northOrEast =
    Random.map
        (\b ->
            if b then
                North
            else
                East
        )
        Random.bool
