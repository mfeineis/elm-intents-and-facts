module Vigors.Autocomplete exposing (Model, Msg, init, vigor)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import Vigors exposing (Recipe, Vigor)


type Model =
    Model State


type Msg
    = ChangeText String


type alias State =
    { placeholder : String
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
init { placeholder }=
    Model
        { placeholder = placeholder
        , text = ""
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


withCmds : List (Cmd Msg) -> State -> ( Model, Cmd Msg )
withCmds cmds state =
    ( Model state, Cmd.batch cmds )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model state) =
    case Debug.log "Autocomplete.update" msg of
        ChangeText text ->
            { state | text = text }
                |> withCmds []


view : Model -> Html Msg
view (Model state) =
    Html.div []
        [ Html.input
            [ onInput ChangeText
            , Attr.placeholder state.placeholder
            ]
            []
        ]
