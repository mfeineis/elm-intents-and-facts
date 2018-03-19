module Vigors.Autocomplete exposing (Msg(..), summon)

import Html exposing (Html)
import Html.Events exposing (onClick)
import Window


type Msg
    = Clicked
    | SizeChanged { height : Int, width : Int }


type alias Autocomplete ctx msg =
    { subscriptions : ctx -> Sub msg
    , view : ctx -> Html msg
    }

type alias Ports ctx msg =
    { map : Msg -> msg
    , read : ctx -> String
    }

summon : Ports ctx msg -> Autocomplete ctx msg
summon ports =
    { subscriptions = \ctx -> subscriptions (ports.read ctx) |> Sub.map ports.map
    , view = \ctx -> view (ports.read ctx) |> Html.map ports.map
    }


subscriptions : String -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes SizeChanged
        ]


view : String -> Html Msg
view model =
    Html.button [ onClick Clicked ]
        [ Html.text model
        ]
