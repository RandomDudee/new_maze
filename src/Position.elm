module Position exposing (..)

type alias Position =
    (Int, Int)


type Direction
    = North
    | East
    | South
    | West


allDirections =
    [ North, South, East, West ]



move : Position -> Direction -> Position
move ( x, y ) dir =
    case dir of
        North ->
            ( x , y + 1 )

        South ->
            ( x , y - 1)

        East ->
            ( x + 1 , y )

        West ->
            ( x - 1, y )
