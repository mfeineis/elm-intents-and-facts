module Main exposing (main)

import Cqrs
import Data.Character as Character exposing (Character)
import Html exposing (Html)
import Html.Events as Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Value)


type alias Model =
    { alice : Int
    , bob : Int
    , clicked : Int
    }


type CounterId
    = Alice
    | Bob


type Intent
    = AskWhoIsKingInTheNorth
    | Decrement CounterId
    | Increment CounterId
    | Reset
    | StateFact Fact


type Fact
    = CounterAdjusted CounterId Int
    | HasBeenReset
    | KingInTheNorthReceived (Result Http.Error Character)


type Consequence
    = FetchJonSnow


main : Program Value Model Intent
main =
    Html.programWithFlags <|
        Cqrs.programWithFlags
            { apply = apply
            , init = init
            , interpret = interpret
            , join = StateFact
            , produce = produce
            , subscriptions = subscriptions
            , view = view
            }


subscriptions : Model -> Sub Intent
subscriptions model =
    Sub.none


init : Value -> ( Model, List Fact, List Consequence )
init _ =
    ( { alice = 0
      , bob = 0
      , clicked = 0
      }
    , [ HasBeenReset ]
    , [ FetchJonSnow ]
    )


incrementOverall : Model -> Model
incrementOverall model =
    { model | clicked = model.clicked + 1 }


apply : Fact -> Model -> Model
apply fact model =
    case fact of
        CounterAdjusted Alice amount ->
            { model | alice = model.alice + amount }
                |> incrementOverall

        CounterAdjusted Bob amount ->
            { model | bob = model.bob + amount }
                |> incrementOverall

        HasBeenReset ->
            { alice = 1
            , bob = 1
            , clicked = 0
            }

        KingInTheNorthReceived (Ok { name, titles }) ->
            Debug.log (name ++ "/" ++ String.join "," titles)
                model

        KingInTheNorthReceived (Err reason) ->
            Debug.log ("Couldn't find the answer: " ++ toString reason)
                model


interpret : Intent -> Model -> ( List Fact, List Consequence )
interpret msg model =
    case msg of
        AskWhoIsKingInTheNorth ->
            ( [], [ FetchJonSnow ] )

        Decrement counter ->
            ( [ CounterAdjusted counter -1 ], [] )

        Increment counter ->
            ( [ CounterAdjusted counter 1 ], [] )

        Reset ->
            ( [ HasBeenReset ], [] )

        StateFact fact ->
            ( [ fact ], [] )


produce : Consequence -> Model -> Cmd Fact
produce fx model =
    case fx of
        FetchJonSnow ->
            Character.whoIsTheKing KingInTheNorthReceived


renderCounter : CounterId -> Int -> Html Intent
renderCounter counter amount =
    Html.div []
        [ Html.button [ onClick (Decrement counter) ] [ Html.text "-" ]
        , Html.span [] [ Html.text (toString amount) ]
        , Html.button [ onClick (Increment counter) ] [ Html.text "+" ]
        ]


view : Model -> Html Intent
view model =
    Html.div []
        [ Html.text ("Times clicked: " ++ toString model.clicked)
        , Html.button [ onClick Reset ] [ Html.text "Reset" ]
        , renderCounter Alice model.alice
        , renderCounter Bob model.bob
        ]
