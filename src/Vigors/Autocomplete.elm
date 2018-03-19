module Vigors.Autocomplete exposing (Msg(..), summon)

import Html exposing (Html)
import Html.Events exposing (onClick)
import Window


type Msg
    = Clicked
    | SizeChanged { height : Int, width : Int }

type alias Model = String


type alias Vigor ctx msg =
    { subscriptions : ctx -> Sub msg
    , update : msg -> ctx -> ( ctx, Cmd msg )
    , view : ctx -> Html msg
    }

type alias Ports myModel myMsg ctx msg =
    { ingest : msg -> Maybe myMsg
    , map : myMsg -> msg
    , read : ctx -> myModel
    , store : ctx -> myModel -> ctx
    }

summon : Ports Model Msg ctx msg -> Vigor ctx msg
summon ports =
    { subscriptions = \ctx -> subscriptions (ports.read ctx) |> Sub.map ports.map
    , update =
        \msg ctx ->
            case ports.ingest msg of
                Nothing ->
                    ( ctx, Cmd.none )

                Just myMsg ->
                    update myMsg (ports.read ctx)
                        |> \(model, cmd) -> ( ports.store ctx model, Cmd.map ports.map cmd )
    , view = \ctx -> view (ports.read ctx) |> Html.map ports.map
    }


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
