module BinaryTree exposing (buildRandomMaze)

import Maze exposing (Maze, allPositions)
import Array exposing (Array)
import Random exposing (Generator)
import Position exposing (Position, Direction(..))
import Grid exposing (Grid)
import Types exposing (Boundary(..))


buildRandomMaze : Int -> Int -> Int -> Maze
buildRandomMaze width height seedInt =
    let
        generator =
            Random.list (width * height) northOrEast

        seed =
            makeSeed seedInt

        ( directions, newSeed ) =
            Random.step generator seed

        initialMaze =
            Maze.create width height

        listOfPostions =
            allPositions initialMaze

        listOfDirections =
            Array.fromList directions
    in
        List.foldl (makePaths listOfDirections) initialMaze listOfPostions


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


makePaths : Array Direction -> Position -> Maze -> Maze
makePaths directions ( x, y ) maze =
    let
        index =
            (y * maze.width) + x

        direction =
            Maybe.withDefault North (Array.get index directions)

        filterDirection : Direction -> Maybe Direction
        filterDirection filterDir =
            let
                isTop =
                    y == maze.height - 1

                isRight =
                    x == maze.width - 1
            in
                if isTop && not isRight then
                    Just East
                else if isRight && not isTop then
                    Just North
                else if isTop && isRight then
                    Nothing
                else
                    Just direction
    in
        case filterDirection direction of
            Just filteredDirection ->
                Maze.setBoundary
                    filteredDirection
                    ( x, y )
                    Path
                    maze

            Nothing ->
                maze
