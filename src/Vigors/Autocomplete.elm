module Vigors.Autocomplete exposing (Msg(..), init, vigor)

import Html exposing (Html)
import Html.Events exposing (onClick)
import Vigors exposing (Recipe, Vigor)
import Window


type Msg
    = Clicked
    | SizeChanged { height : Int, width : Int }


type alias Model = String


vigor : Recipe Model Msg ctx msg -> Vigor ctx msg
vigor =
    Vigors.summon
        { subscriptions = subscriptions
        , update = update
        , view = view
        }


init : String -> Model
init value =
    value


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes SizeChanged
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clicked ->
            Debug.log ("Autocomplete.clicked")
                ( model, Cmd.none )

        SizeChanged size ->
            Debug.log ("Autocomplete.sizeChanged")
                ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.button [ onClick Clicked ]
        [ Html.text model
        ]
