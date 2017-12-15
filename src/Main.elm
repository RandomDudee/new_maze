module Main exposing (main)

import Maze exposing (Maze)
import Html exposing (Html)

main =
    Html.beginnerProgram { model = model, view = view, update = update }

model : Maze
model =
  Maze.create 10 5

view : Maze -> Html a
view maze =
  Html.text "Hello"

update : a -> Maze -> Maze
update msg maze = maze



  --{ model : model, view : model -> Html msg, update : msg -> model -> model }
  --  -> Program Never model msg
