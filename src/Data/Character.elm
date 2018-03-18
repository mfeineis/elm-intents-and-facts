module Data.Character exposing (Character, whoIsTheKing)

import Http
import Json.Decode as Decode exposing (Decoder)


type alias Character =
    { name : String
    , titles : List String
    }


characterDecoder : Decoder Character
characterDecoder =
    Decode.map2 Character
        (Decode.field "name" Decode.string)
        (Decode.field "titles" (Decode.list Decode.string))


whoIsTheKing : (Result Http.Error Character -> msg) -> Cmd msg
whoIsTheKing toMsg =
    Http.send toMsg <|
        Http.get
            "https://anapioficeandfire.com/api/characters/583"
            characterDecoder
