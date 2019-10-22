module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (checked, disabled, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- Model


type alias ToDo =
    { text : String, done : Bool, id : Int }


type alias Model =
    { todos : List ToDo
    , input : String
    , id : Int
    }


init : Model
init =
    { todos =
        []
    , input = ""
    , id = 0
    }



-- Update


type Val
    = AddToDo
    | SetDone Int
    | SetInput String
    | RemoveTodo Int



-- Update


update : Val -> Model -> Model
update val model =
    case val of
        AddToDo ->
            let
                newTodo =
                    { id = model.id, text = model.input, done = False }

                newModel =
                    { model | todos = List.append model.todos [ newTodo ], input = "", id = model.id + 1 }
            in
            newModel

        SetDone todoId ->
            { model
                | todos =
                    List.map
                        (\t ->
                            let
                                isDone =
                                    if t.id == todoId then
                                        not t.done

                                    else
                                        t.done
                            in
                            { t | done = isDone }
                        )
                        model.todos
            }

        SetInput v ->
            { model | input = v }

        RemoveTodo t ->
            { model | todos = List.filter (\i -> i.id /= t) model.todos }



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
        , button [ onClick (RemoveTodo t.id) ] [ text "X" ]
        ]


display : List (Html li) -> List (Html li)
display list =
    list


view : Model -> Html Val
view model =
    let
        disabled_ =
            model.input == ""
    in
    div []
        [ form [ onSubmit AddToDo ]
            [ input [ type_ "text", onInput SetInput, value model.input ] []
            , button [ type_ "submit", disabled disabled_ ] [ text "add" ]
            ]
        , ul []
            (display (List.map todo model.todos))
        ]
