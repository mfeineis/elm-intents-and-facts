module Data.Character exposing (Character, decoder)

import Json.Decode as Decode exposing (Decoder)


type alias Character =
    { name : String
    , titles : List String
    }


decoder : Decoder Character
decoder =
    Decode.map2 Character
        (Decode.field "name" Decode.string)
        (Decode.field "titles" (Decode.list Decode.string))
