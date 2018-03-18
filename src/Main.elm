module Main exposing (main)

import Data.Character as Character exposing (Character)
import Html exposing (Html)
import Html.Events as Events exposing (onClick)
import Http
import It
import Json.Decode as Decode exposing (Value)


type CounterState
    = CounterState
        { count : Int
        }


type CounterMsg
    = Decrement
    | Increment


initCounter : Int -> CounterState
initCounter count =
    CounterState { count = count }


applyCounter : (model -> CounterState) -> CounterMsg -> model -> CounterState
applyCounter selector msg model =
    let
        (CounterState { count }) =
            selector model
    in
    case msg of
        Decrement ->
            CounterState { count = count - 1 }

        Increment ->
            CounterState { count = count + 1 }


viewCounter : (model -> CounterState) -> (CounterMsg -> msg) -> model -> Html msg
viewCounter selector toMsg model =
    let
        (CounterState { count }) =
            selector model
    in
    Html.div []
        [ Html.button [ onClick Decrement ] [ Html.text "-" ]
        , Html.span [] [ Html.text (toString count) ]
        , Html.button [ onClick Increment ] [ Html.text "+" ]
        ]
        |> Html.map toMsg


type alias Model =
    { alice : CounterState
    , bob : CounterState
    , clicked : Int
    }


type Intent
    = AskWhoIsKingInTheNorth
    | Reset
    | StateFact Fact


type Fact
    = CounterUpdated CounterId CounterMsg
    | KingInTheNorthReceived (Result Http.Error Character)
    | HasBeenReset


type Consequence
    = FetchJonSnow


main : Program Value Model Intent
main =
    Html.programWithFlags <|
        It.wrap
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
    ( { alice = initCounter 0
      , bob = initCounter 0
      , clicked = 0
      }
    , [ HasBeenReset ]
    , [ FetchJonSnow ]
    )


apply : Fact -> Model -> Model
apply fact model =
    case fact of
        CounterUpdated Alice subMsg ->
            { model
                | clicked = model.clicked + 1
                , alice = applyCounter .alice subMsg model
            }

        CounterUpdated Bob subMsg ->
            { model
                | clicked = model.clicked + 1
                , bob = applyCounter .bob subMsg model
            }

        HasBeenReset ->
            { alice = initCounter 1
            , bob = initCounter 1
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

        Reset ->
            ( [ HasBeenReset ], [] )

        StateFact fact ->
            ( [ fact ], [] )


produce : Consequence -> Model -> Cmd Fact
produce fx model =
    case fx of
        FetchJonSnow ->
            let
                request =
                    Http.get
                        "https://anapioficeandfire.com/api/characters/583"
                        Character.decoder
            in
            Http.send KingInTheNorthReceived request


type CounterId
    = Alice
    | Bob


renderAlice =
    viewCounter .alice (StateFact << CounterUpdated Alice)


renderBob =
    viewCounter .bob (StateFact << CounterUpdated Bob)


view : Model -> Html Intent
view model =
    Html.div []
        [ Html.text ("Times clicked: " ++ toString model.clicked)
        , Html.button [ onClick Reset ] [ Html.text "Reset" ]
        , renderAlice model
        , renderBob model
        ]


