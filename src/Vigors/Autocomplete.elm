module Vigors.Autocomplete exposing (Model, Msg(..), init, vigor)

import Html exposing (Attribute, Html)
import Html.Attributes as Attr
import Html.Events as Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder, Value)
import Keyboard
import Task
import Vigors exposing (Recipe, Vigor)


type Model
    = Model State


type Msg
    = ChangeText String
    | FetchSuggestions String
    | KeyDown Int
    | SuggestionsFetched (List String)


type alias State =
    { placeholder : String
    , suggestions : List String
    , text : String
    }


vigor : Recipe Model Msg ctx msg -> Vigor ctx msg
vigor =
    Vigors.summon
        { subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Config =
    { placeholder : String
    }


init : Config -> Model
init { placeholder } =
    Model
        { placeholder = placeholder
        , suggestions = []
        , text = ""
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        ]


type Acceptance
    = Accepted
    | Denied


canAccept : State -> Acceptance
canAccept { text } =
    if String.length text > 0 then
        Accepted
    else
        Denied


fetchSuggestions : State -> ( Model, Cmd Msg )
fetchSuggestions ({ text } as state) =
    state
        |> withCmds
            [ asCmd (FetchSuggestions text)
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model state) =
    case msg of
        ChangeText changedText ->
            { state | text = changedText }
                |> withCmds []

        FetchSuggestions search ->
            Debug.log ("FetchSuggestions " ++ search)
                state
                |> withCmds [ asCmd (SuggestionsFetched [ "default", "suggestions" ]) ]

        KeyDown 13 ->
            case canAccept state of
                Accepted ->
                    state |> fetchSuggestions

                Denied ->
                    state |> withCmds []

        KeyDown _ ->
            state |> withCmds []

        SuggestionsFetched suggestions ->
            Debug.log ("SuggestionsFetched" ++ toString suggestions)
                { state
                    | suggestions = suggestions
                }
                |> withCmds []


renderSuggestions : List String -> List (Html Msg)
renderSuggestions suggestions =
    let
        renderSuggestion it =
            Html.div [] [ Html.text it ]
    in
    [ Html.div []
        (List.map renderSuggestion suggestions)
    ]


view : Model -> Html Msg
view (Model { placeholder, suggestions, text }) =
    Html.div []
        ([ Html.input
            [ onInput ChangeText
            , Attr.placeholder placeholder
            , onKeyDown KeyDown
            , Attr.value text
            ]
            []
         ]
            ++ renderSuggestions suggestions
        )



-- Helpers


withCmds : List (Cmd Msg) -> State -> ( Model, Cmd Msg )
withCmds cmds state =
    ( Model state, Cmd.batch cmds )


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    Events.on "keydown" (Decode.map tagger Events.keyCode)


asCmd : msg -> Cmd msg
asCmd msg =
    Task.perform identity (Task.succeed msg)
