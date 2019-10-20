module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- Model


type alias ToDo =
    { text : String, done : Bool, id : Int }


type alias Model =
    { todos : List ToDo
    }


init : Model
init =
    { todos =
        [ { text = "Test_1", done = False, id = 1 }
        , { text = "Test_2", done = True, id = 2 }
        ]
    }



-- Update


type Val
    = AddToDo ToDo
    | SetDone Int



-- Update


update : Val -> Model -> Model
update val model =
    case val of
        AddToDo t ->
            { model | todos = List.append model.todos [ t ] }

        SetDone todoId ->
            { model
                | todos =
                    List.map
                        (\t ->
                            if t.id == todoId then
                                { t | done = not t.done }

                            else
                                t
                        )
                        model.todos
            }



-- View


todo : ToDo -> Html Val
todo t =
    li
        [ style "display" "flex"
        , style "align-items" "center"
        ]
        [ p
            [ style "text-decoration"
                (if t.done then
                    "line-through"

                 else
                    ""
                )
            ]
            [ text t.text ]
        , input [ type_ "checkbox", checked t.done, onClick (SetDone t.id) ] []
        ]


display : List (Html li) -> List (Html li)
display list =
    list


view : Model -> Html Val
view model =
    ul []
        (display (List.map todo model.todos))
