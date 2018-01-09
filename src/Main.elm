module Main exposing (main)

import View
import Maze exposing (Maze)
import Html exposing (Html)

main =
    Html.beginnerProgram { model = model, view = view, update = update }

model : Maze
model =
  Maze.create 22 15

view : Maze -> Html a
view maze =
  View.view maze

update : a -> Maze -> Maze
update msg maze = maze



  --{ model : model, view : model -> Html msg, update : msg -> model -> model }
  --  -> Program Never model msg
