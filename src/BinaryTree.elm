module BinaryTree exposing (buildRandomMaze)

import Maze exposing(Maze, createCell, allPositions)
import Array
import Random exposing (Generator)
import Position exposing (Direction(..))
import Grid exposing (Grid)

buildRandomMaze : Int -> Int -> Int -> Maze
buildRandomMaze width height seedInt =
    let
        generator =
            Random.list (width * height) northOrEast

        seed =
            makeSeed seedInt

        ( directions, newSeed ) =
            Random.step generator seed
        initialMaze = Maze.create width height
        listOfPostions = allPositions initialMaze
    in
        List.foldl (Maze.createCell (Array.fromList directions) width) initialMaze listOfPostions

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
