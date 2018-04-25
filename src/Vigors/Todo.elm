module Vigors.Todo exposing (init, main, vigor)

import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Html.Events as Events exposing (onClick, onCheck, onInput)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Task
import Vigors exposing (Recipe, Vigor)


main : Program Never Model Msg
main =
    Html.program
        { init = init (Encode.object [])
        , subscriptions = subscriptions >> Sub.map Intent
        , update = update
        , view = view >> Html.map Intent
        }


vigor : Recipe Model Msg ctx msg -> Vigor ctx msg
vigor =
    Vigors.summon
        { subscriptions = subscriptions >> Sub.map Intent
        , update = update
        , view = view >> Html.map Intent
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
    = NoFact


type Todo
    = Done String
    | Fresh String
    | Open String


type alias Model =
    { newTodo : Todo
    , todos : List Todo
    }


init : Value -> ( Model, Cmd Msg )
init _ =
    withoutCmd <|
        { newTodo = Fresh ""
        , todos = []
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fact fact ->
            updateFromFact fact model
                |> withoutCmd

        Intent intent ->
            updateFromIntent intent model


updateFromFact : Fact -> Model -> Model
updateFromFact msg model =
    model


updateFromIntent : Intent -> Model -> ( Model, Cmd Msg )
updateFromIntent msg ({ newTodo, todos } as model) =
    case msg of
        AddTodo todo ->
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
            withoutCmd <|
                { model
                    | newTodo = Fresh ""
                    , todos = Open text :: todos
                }

        EditTodo todo text ->
            if todo == newTodo then
                { model | newTodo = Fresh text } |> withoutCmd
            else
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
                { model | todos = List.map updateOnMatch todos } |> withoutCmd

        MarkDone todo ->
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
            { model | todos = List.map updateOnMatch todos } |> withoutCmd

        MarkOpen todo ->
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
            { model | todos = List.map updateOnMatch todos } |> withoutCmd

        NewTodoKeyDown 13 ->
            updateFromIntent (AddTodo newTodo) model

        NewTodoKeyDown _ ->
            model |> withoutCmd

        RemoveAllTodos ->
            { model | newTodo = Fresh "", todos = [] } |> withoutCmd

        RemoveDoneTodos ->
           { model | todos = List.filter (not << isDone) todos } |> withoutCmd

        RemoveTodo todo ->
           { model | todos = List.filter (\it -> todo /= it) todos } |> withoutCmd


isDone : Todo -> Bool
isDone todo =
    case todo of
        Done _ ->
            True

        _ ->
            False


-- View


view : Model -> Html Intent
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


withoutCmd : model -> ( model, Cmd msg )
withoutCmd model =
    ( model, Cmd.none )


withCmds : List (Cmd msg) -> model -> ( model, Cmd msg )
withCmds cmds model =
    ( model, Cmd.batch cmds )
