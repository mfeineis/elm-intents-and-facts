module Main exposing (main)

import Cqrs
import Data.Character as Character exposing (Character)
import Html exposing (Html)
import Html.Events as Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Value)
import Vigors
import Vigors.Autocomplete exposing (Msg(..))

-- [ ] Datepicker
-- [ ] Dragndrop
-- [ ] Material Design Snackbar
-- [ ] Filterable Dropdown
-- [ ] Autocomplete
-- [ ] Paging
-- [ ] 


type alias Model =
    { alice : Int
    , bob : Int
    , clicked : Int
    , docs : String
    , search : String
    }


type CounterId
    = Alice
    | Bob


type Intent
    = AskWhoIsKingInTheNorth
    | Decrement CounterId
    | ExternalClick Which
    | AutocompleteMsg Which Msg
    | Increment CounterId
    | Noop
    | Reset
    | StateFact Fact


type Fact
    = CounterAdjusted CounterId Int
    | HasBeenReset
    | KingInTheNorthReceived (Result Http.Error Character)
    | OverallIncremented


type Consequence
    = FetchJonSnow


type Which
    = Docs
    | Search


main : Program Value Model Intent
main =
    let
        docSearch =
            Vigors.Autocomplete.summon
                { incoming =
                    \msg ->
                        case msg of
                            AutocompleteMsg Docs it ->
                                Just it

                            _ ->
                                Nothing

                , outgoing =
                    \msg ->
                        case msg of
                            Clicked ->
                                ExternalClick Docs

                            it ->
                                AutocompleteMsg Docs it

                , read = \{ docs } -> docs
                , store = \model state -> { model | docs = state }
                }

        mainSearch =
            Vigors.Autocomplete.summon
                { incoming =
                    \msg ->
                        case msg of
                            AutocompleteMsg Search it ->
                                Just it

                            _ ->
                                Nothing

                , outgoing =
                    \msg ->
                        case msg of
                            Clicked ->
                                ExternalClick Search

                            it ->
                                AutocompleteMsg Search it

                , read = \{ search } -> search
                , store = \model state -> { model | search = state }
                }

        compositeView model =
            view
                { docSearch = docSearch.view model
                , mainSearch = mainSearch.view model
                }
                model

        program =
            Cqrs.programWithFlags
                { apply = apply
                , init = init
                , interpret = interpret
                , join = StateFact
                , produce = produce
                , subscriptions = subscriptions
                , view = compositeView
                }
    in
    Html.programWithFlags <|
        Vigors.compose program
            [ docSearch
            , mainSearch
            ]


subscriptions : Model -> Sub Intent
subscriptions model =
    Sub.none


init : Value -> ( Model, List Fact, List Consequence )
init _ =
    ( { alice = 0
      , bob = 0
      , clicked = 0
      , docs = Vigors.Autocomplete.init "Docs"
      , search = Vigors.Autocomplete.init "Search"
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
            , docs = "Docs"
            , search = "Search"
            }

        KingInTheNorthReceived (Ok { name, titles }) ->
            Debug.log (name ++ "/" ++ String.join "," titles)
                model

        KingInTheNorthReceived (Err reason) ->
            Debug.log ("Couldn't find the answer: " ++ toString reason)
                model

        OverallIncremented ->
            model |> incrementOverall


interpret : Intent -> Model -> ( List Fact, List Consequence )
interpret msg model =
    case msg of
        AskWhoIsKingInTheNorth ->
            ( [], [ FetchJonSnow ] )

        AutocompleteMsg _ _ ->
            ( [], [] )

        Decrement counter ->
            ( [ CounterAdjusted counter -1 ], [] )

        ExternalClick _ ->
            ( [ OverallIncremented ], [] )

        Increment counter ->
            ( [ CounterAdjusted counter 1 ], [] )

        Noop ->
            ( [], [] )

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


type alias Partials =
    { docSearch : Html Intent
    , mainSearch : Html Intent
    }


view : Partials -> Model -> Html Intent
view partials model =
    Html.div []
        [ Html.div []
            [ Html.text ("Times clicked: " ++ toString model.clicked)
            ]
        , partials.mainSearch
        , Html.button [ onClick Reset ] [ Html.text "Reset" ]
        , renderCounter Alice model.alice
        , renderCounter Bob model.bob
        , partials.docSearch
        ]
