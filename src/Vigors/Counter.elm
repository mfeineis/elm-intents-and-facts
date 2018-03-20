module Vigors.Counter exposing (Model, Msg, init, vigor)

import Html exposing (Html)
import Html.Events exposing (onClick)
import Vigors exposing (Recipe, Vigor)


type Msg
    = Decrement
    | Increment


type Model = Model String Int


vigor : Recipe Model Msg ctx msg -> Vigor ctx msg
vigor =
    Vigors.summon
        { subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


init : String -> Int -> Model
init name value =
    Model name value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model name model) =
    case msg of
        Decrement ->
            ( Model name (model - 1), Cmd.none )

        Increment ->
            ( Model name (model + 1), Cmd.none )


view : Model -> Html Msg
view (Model name model) =
    Html.div []
        [ Html.button [ onClick Decrement ] [ Html.text "-" ]
        , Html.text (toString model)
        , Html.button [ onClick Increment ] [ Html.text "+" ]
        , Html.text ("(" ++ name ++ ")")
        ]
