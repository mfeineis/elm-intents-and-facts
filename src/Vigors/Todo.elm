module Vigors.Todo exposing (Model, Msg, init, main, vigor)

import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Html.Events as Events exposing (onClick, onCheck, onInput)
import Json.Decode as Decode exposing (Value)
import Task
import Vigors exposing (Recipe, Vigor)


main : Program Value Model Msg
main =
    Html.programWithFlags
        { init = \flags -> (init flags, Cmd.none )
        , subscriptions = subscriptions >> Sub.map Intent
        , update = update
        , view = (\(Model state) -> view state) >> Html.map Intent
        }


vigor : Recipe Model Msg ctx msg -> Vigor ctx msg
vigor =
    Vigors.summon
        { subscriptions = subscriptions >> Sub.map Intent
        , update = update
        , view = (\(Model state) -> view state) >> Html.map Intent
        }


subscriptions : Model -> Sub Intent
subscriptions model =
    Sub.none



-- Domain


type Msg
    = Fact Fact
    | Intent Intent


type Intent
    = AddTodo Todo
    | EditTodo Todo String
    | MarkDone Todo
    | MarkOpen Todo
    | NewTodoKeyDown Int
    | RemoveAllTodos
    | RemoveDoneTodos
    | RemoveTodo Todo


type Fact
    = AllDoneTodosRemoved
    | AllTodosRemoved
    | NewTodoEdited String
    | NewTodoReset
    | TodoAdded Todo
    | TodoEdited Todo String
    | TodoMarkedAsDone Todo
    | TodoRemoved Todo
    | TodoReopened Todo


type Todo
    = Done String
    | Fresh String
    | Open String


type Model
    = Model State


type alias State =
    { newTodo : Todo
    , todos : List Todo
    }


-- FIXME: How do we provide initial `Cmd Msg`s to the parent?
init : Value -> Model
init _ =
    Model
        { newTodo = Fresh ""
        , todos = []
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        Fact fact ->
            replay fact model
                |> withoutCmd

        Intent intent ->
            interpret intent model
                |> \(fs, cmds) -> applyFacts fs model


interpret : Intent -> State -> ( List Fact, Cmd Msg )
interpret intent ({ newTodo, todos } as model) =
    case intent of
        AddTodo todo ->
            ( [ NewTodoReset, TodoAdded todo ], Cmd.none )

        EditTodo todo text ->
            if todo == newTodo then
                ( [ NewTodoEdited text ], Cmd.none )
            else
                ( [ TodoEdited todo text ], Cmd.none )

        MarkDone todo ->
            ( [ TodoMarkedAsDone todo ], Cmd.none )

        MarkOpen todo ->
            ( [ TodoReopened todo ], Cmd.none )

        NewTodoKeyDown 13 ->
            interpret (AddTodo newTodo) model

        NewTodoKeyDown _ ->
            ( [], Cmd.none )

        RemoveAllTodos ->
            ( [ NewTodoReset, AllTodosRemoved ], Cmd.none )

        RemoveDoneTodos ->
            ( [ AllDoneTodosRemoved ], Cmd.none )

        RemoveTodo todo ->
            ( [ TodoRemoved todo ], Cmd.none )



replay : Fact -> State -> State
replay fact ({ newTodo, todos } as model) =
    case fact of
        AllDoneTodosRemoved ->
            { model | todos = List.filter (not << isDone) todos }

        AllTodosRemoved ->
            { model | todos = [] }

        NewTodoEdited text ->
            { model | newTodo = Fresh text }

        NewTodoReset ->
            { model | newTodo = Fresh "" }

        TodoAdded todo ->
            let
                text =
                    case todo of
                        Done it ->
                            it

                        Fresh it ->
                            it

                        Open it ->
                            it
            in
            { model | todos = Open text :: todos }

        TodoEdited todo text ->
            let
                updateOnMatch it =
                    if it == todo then
                        case todo of
                            Done _ ->
                                it

                            Fresh _ ->
                                it

                            Open _ ->
                                Open text
                    else
                        it
            in
            { model | todos = List.map updateOnMatch todos }

        TodoMarkedAsDone todo ->
            let
                updateOnMatch it =
                    if it == todo then
                        case todo of
                            Done _ ->
                                it

                            Fresh _ ->
                                it

                            Open text ->
                                Done text
                    else
                        it
            in
            { model | todos = List.map updateOnMatch todos }

        TodoRemoved todo ->
           { model | todos = List.filter (\it -> todo /= it) todos }

        TodoReopened todo ->
            let
                updateOnMatch it =
                    if it == todo then
                        case todo of
                            Done text ->
                                Open text

                            Fresh _ ->
                                it

                            Open _ ->
                                it
                    else
                        it
            in
            { model | todos = List.map updateOnMatch todos }



isDone : Todo -> Bool
isDone todo =
    case todo of
        Done _ ->
            True

        _ ->
            False


-- View


view : State -> Html Intent
view { newTodo, todos } =
    let
       hasDoneTodos =
          todos |> List.filter isDone |> not << List.isEmpty
    in
    Html.div []
        [ baseStyle
        , whatToDo
        , Html.ol []
            (renderTodo newTodo :: List.map renderTodo todos)
        , Html.button [ Attr.disabled (not hasDoneTodos), onClick RemoveDoneTodos ]
            [ Html.text "Remove Done Todos"
            ]
        , Html.button [ Attr.disabled (List.isEmpty todos), onClick RemoveAllTodos ]
            [ Html.text "Remove All Todos"
            ]
        ]


whatToDo : Html msg
whatToDo =
    Html.h1 [] [ Html.text "What to do?" ]


renderTodo : Todo -> Html Intent
renderTodo todo =
    case todo of
        Done it ->
            Html.li []
                [ Html.label []
                    [ Html.input
                        [ Attr.checked True
                        , onCheck (always (MarkOpen todo))
                        , Attr.type_ "checkbox"
                        ]
                        []
                    ]
                , Html.input [ Attr.disabled True, Attr.value it ] []
                , Html.button [ onClick (RemoveTodo todo) ] [ Html.text "x" ]
                ]

        Fresh it ->
            Html.li []
                [ Html.label []
                    [ Html.input [ Attr.disabled True, Attr.type_ "checkbox" ] []
                    ]
                , Html.input
                    [ onInput (EditTodo todo)
                    , onKeyDown NewTodoKeyDown
                    , Attr.value it
                    ]
                    []
                , Html.button
                    [ Attr.disabled (String.isEmpty it), onClick (AddTodo todo) ]
                    [ Html.text "+" ]
                ]

        Open it ->
            Html.li []
                [ Html.label []
                    [ Html.input
                        [ Attr.checked False
                        , onCheck (always (MarkDone todo))
                        , Attr.type_ "checkbox"
                        ]
                        []
                    ]
                , Html.input [ onInput (EditTodo todo), Attr.value it ] []
                , Html.button [ Attr.disabled True ] [ Html.text "x" ]
                ]


-- Helpers


baseStyle : Html msg
baseStyle =
    Html.node "style" []
        [ Html.text
            """

html {
    box-sizing: border-box;
}
body {
    margin: 0;
    padding: 0;
}
*, *:before, *:after {
    box-sizing: inherit;
}
pre {
    background: #eee;
    border: 1px solid #ddd;
}

            """
        ]


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    Events.on "keydown" (Decode.map tagger Events.keyCode)


asCmd : msg -> Cmd msg
asCmd msg =
    Task.perform identity (Task.succeed msg)


withoutCmd : State -> ( Model, Cmd msg )
withoutCmd state =
    ( Model state, Cmd.none )


applyFacts : List Fact -> State -> ( Model, Cmd Msg )
applyFacts facts state =
    ( Model state, Cmd.batch (List.map (asCmd << Fact) facts))
