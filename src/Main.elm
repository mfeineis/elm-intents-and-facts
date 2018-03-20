module Main exposing (main)

import Cqrs
import Data.Character as Character exposing (Character)
import Html exposing (Html)
import Html.Events as Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Value)
import Vigors
import Vigors.Autocomplete exposing (Msg(..))
import Vigors.Counter

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
    , carol : Vigors.Counter.Model
    , clicked : Int
    , docs : Vigors.Autocomplete.Model
    , search : Vigors.Autocomplete.Model
    }


type CounterId
    = Alice
    | Bob
    | Carol


type Intent
    = AskWhoIsKingInTheNorth
    | AutocompleteMsg Which Msg
    | CounterMsg CounterId Vigors.Counter.Msg
    | Decrement CounterId
    | ExternalClick Which
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
        carol =
            Vigors.Counter.vigor
                { incoming =
                    \msg ->
                        case msg of
                            CounterMsg Carol it ->
                                Just it

                            _ ->
                                Nothing
                , outgoing = CounterMsg Carol
                , read = .carol
                , store = \model state -> { model | carol = state }
                }

        docSearch =
            Vigors.Autocomplete.vigor
                { incoming =
                    \msg ->
                        case msg of
                            AutocompleteMsg Docs it ->
                                Just it

                            _ ->
                                Nothing

                , outgoing = AutocompleteMsg Docs
                , read = .docs
                , store = \model state -> { model | docs = state }
                }

        mainSearch =
            Vigors.Autocomplete.vigor
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
                , read = .search
                , store = \model state -> { model | search = state }
                }

        compositeView model =
            view
                { carol = carol.view model
                , docSearch = docSearch.view model
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
            [ carol
            , docSearch
            , mainSearch
            ]


subscriptions : Model -> Sub Intent
subscriptions model =
    Sub.none


init : Value -> ( Model, List Fact, List Consequence )
init _ =
    ( { alice = 0
      , bob = 0
      , carol = Vigors.Counter.init "Carol" 0
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

        CounterAdjusted Carol _ ->
            model
                |> incrementOverall

        HasBeenReset ->
            { alice = 1
            , bob = 1
            , carol = Vigors.Counter.init "Carol" 1
            , clicked = 0
            , docs = Vigors.Autocomplete.init "Docs"
            , search = Vigors.Autocomplete.init "Search"
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

        CounterMsg _ _ ->
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
    { carol : Html Intent
    , docSearch : Html Intent
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
        , partials.carol
        , partials.docSearch
        ]
