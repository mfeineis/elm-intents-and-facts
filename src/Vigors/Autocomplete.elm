module Vigors.Autocomplete exposing (Msg(..), summon)

import Html exposing (Html)
import Html.Events exposing (onClick)


type Msg
    = Clicked


type alias Autocomplete ctx msg =
    { view : ctx -> Html msg
    }

type alias Ports ctx msg =
    { map : Msg -> msg
    , read : ctx -> String
    }

summon : Ports ctx msg -> Autocomplete ctx msg
summon ports =
    { view = \ctx -> view (ports.read ctx) |> Html.map ports.map
    }


view : String -> Html Msg
view model =
    Html.button [ onClick Clicked ]
        [ Html.text model
        ]
