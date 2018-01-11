module Grid exposing (..)

import Array exposing (Array)
import Html exposing (Html, div, text)
import Position exposing (..)
import Maybe.Extra
import Tuple


type alias Grid a =
    Array (Array a)


main : Html msg
main =
    let
        grid =
            create 4 6 0
                |> indexedMap (\pos a -> pos)
                |> filterPositions (\pos -> (Tuple.first pos) > 2 || (Tuple.second pos) > 3)
                |> List.map (\tup -> Tuple.first tup)
    in
        text <| toString grid


empty : Grid a
empty =
    Array.empty


create : Int -> Int -> a -> Grid a
create xs ys thing =
    Array.repeat ys thing
        |> Array.repeat xs


height : Grid a -> Int
height grid =
    Array.length grid


width : Grid a -> Int
width grid =
    Array.get 0 grid
        |> Maybe.withDefault Array.empty
        |> Array.length


flatten : Grid a -> Array a
flatten =
    Array.foldl
        (\x accum ->
            Array.append accum x
        )
        Array.empty


toList : Grid a -> List a
toList grid =
    flatten grid |> Array.toList


get : Position -> Grid a -> Maybe a
get ( x, y ) grid =
    Array.get x grid
        |> Maybe.andThen (Array.get y)


set : Position -> a -> Grid a -> Grid a
set ( x, y ) value grid =
    let
        setInRow : Array a -> Array a
        setInRow cols =
            let
                oldValue =
                    Array.get y cols
            in
                case oldValue of
                    Nothing ->
                        cols

                    Just _ ->
                        Array.set y value cols

        replaceRow : Array a -> Grid a
        replaceRow newRow =
            Array.set x newRow grid
    in
        Array.get x grid
            |> Maybe.map setInRow
            |> Maybe.map replaceRow
            |> Maybe.withDefault grid


map : (a -> b) -> Grid a -> Grid b
map fn gd =
    Array.map (\x -> Array.map fn x) gd


indexedMap : (Position -> a -> b) -> Grid a -> Grid b
indexedMap fn gd =
    Array.indexedMap (\gx ys -> Array.indexedMap (\gy value -> fn ( gx, gy ) value) ys) gd


filterPositions : (a -> Bool) -> Grid a -> List ( Position, a )
filterPositions fn gd =
    indexedMap
        (\pos value ->
            if (fn value) then
                Just ( pos, value )
            else
                Nothing
        )
        gd
        |> toList
        |> Maybe.Extra.values
