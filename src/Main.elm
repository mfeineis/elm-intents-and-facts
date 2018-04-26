module Main exposing (init, main, vigor)

import Cqrs
import Data.Character as Character exposing (Character)
import Html exposing (Html)
import Html.Events as Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import MainDcft
import Task
import Vigors
import Vigors.Autocomplete exposing (Msg(..))
import Vigors.Counter
import Vigors.Todo


-- [ ] Datepicker
-- [ ] Dragndrop
-- [ ] Material Design Snackbar
-- [ ] Filterable Dropdown
-- [ ] Autocomplete
-- [ ] Paging
-- [ ] Todo
-- [ ]


type alias Model =
    { alice : Int
    , bob : Int
    , carol : Vigors.Counter.Model
    , dcft : MainDcft.Model
    , clicked : Int
    , danika : Vigors.Counter.Model
    , search : Vigors.Autocomplete.Model
    , todo : Vigors.Todo.Model
    }


type CounterId
    = Alice
    | Bob
    | Carol
    | Danika


type Intent
    = AskWhoIsKingInTheNorth
    | AutocompleteMsg Vigors.Autocomplete.Msg
    | CounterMsg CounterId Vigors.Counter.Msg
    | Decrement CounterId
    | Increment CounterId
    | MainDcftMsg MainDcft.Msg
    | Reset
    | StateFact Fact
    | TodoMsg Vigors.Todo.Msg


type Fact
    = CounterAdjusted CounterId Int
    | HasBeenReset
    | KingInTheNorthReceived (Result Http.Error Character)
    | OverallIncremented


type Consequence
    = FetchJonSnow
    | SupplyStaticSuggestions


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


danika =
    Vigors.Counter.vigor
        { incoming =
            \msg ->
                case msg of
                    CounterMsg Danika it ->
                        Just it

                    _ ->
                        Nothing
        , outgoing = CounterMsg Danika
        , read = .danika
        , store = \model state -> { model | danika = state }
        }


dcft =
    MainDcft.vigor
        { incoming =
            \msg ->
                case msg of
                    MainDcftMsg it ->
                        Just it

                    _ ->
                        Nothing
        , outgoing = MainDcftMsg
        , read = .dcft
        , store = \model state -> { model | dcft = state }
        }


mainSearch =
    Vigors.Autocomplete.vigor
        { incoming =
            \msg ->
                case msg of
                    AutocompleteMsg it ->
                        Just it

                    _ ->
                        Nothing
        , outgoing = AutocompleteMsg
        , read = .search
        , store = \model state -> { model | search = state }
        }


todo =
    Vigors.Todo.vigor
        { incoming =
            \msg ->
                case msg of
                    TodoMsg it ->
                        Just it

                    _ ->
                        Nothing
        , outgoing = TodoMsg
        , read = .todo
        , store = \model state -> { model | todo = state }
        }


compositeView model =
    view
        { carol = carol.view model
        , danika = danika.view model
        , dcft = dcft.view model
        , mainSearch = mainSearch.view model
        , todo = todo.view model
        }
        model


program =
    Cqrs.programWithFlags
        { apply = apply
        , init = init
        , interpret = interpret
        , produce = produce
        , subscriptions = subscriptions
        , view = compositeView
        }


vigor =
    Vigors.summon
        { subscriptions = program.subscriptions
        , update = program.update
        , view = program.view
        }


main : Program Value Model Intent
main =
    Html.programWithFlags <|
        Vigors.compose program
            [ carol
            , danika
            , dcft
            , mainSearch
            , todo
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
      , danika = Vigors.Counter.init "Danika" 0
      , dcft = MainDcft.init (Encode.object [])
      , search =
          Vigors.Autocomplete.init
              { placeholder = "Search for something..."
              }
      , todo = Vigors.Todo.init (Encode.object [])
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

        CounterAdjusted _ _ ->
            model
                |> incrementOverall

        HasBeenReset ->
            { alice = 1
            , bob = 1
            , carol = Vigors.Counter.init "Carol" 1
            , clicked = 0
            , danika = Vigors.Counter.init "Danika" 1
            , dcft = MainDcft.init (Encode.object [])
            , search =
                Vigors.Autocomplete.init
                    { placeholder = "Search for something..."
                    }
            , todo = Vigors.Todo.init (Encode.object [])
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
interpret intent model =
    case intent of
        AskWhoIsKingInTheNorth ->
            ( [], [ FetchJonSnow ] )

        AutocompleteMsg (FetchSuggestions text) ->
            ( [], [ SupplyStaticSuggestions ] )

        AutocompleteMsg _ ->
            ( [], [] )

        CounterMsg _ _ ->
            ( [], [] )

        Decrement counter ->
            ( [ CounterAdjusted counter -1 ], [] )

        Increment counter ->
            ( [ CounterAdjusted counter 1 ], [] )

        MainDcftMsg _ ->
            ( [], [] )

        Reset ->
            ( [ HasBeenReset ], [] )

        StateFact fact ->
            ( [ fact ], [] )

        TodoMsg _ ->
            ( [], [] )


produce : Consequence -> Model -> Cmd Intent
produce fx model =
    case fx of
        FetchJonSnow ->
            Character.whoIsTheKing (StateFact << KingInTheNorthReceived)

        SupplyStaticSuggestions ->
            AutocompleteMsg (SuggestionsFetched [ "Aang", "Katara", "Sooka" ])
                |> asCmd


renderCounter : CounterId -> Int -> Html Intent
renderCounter counter amount =
    Html.div []
        [ Html.button [ onClick (Decrement counter) ] [ Html.text "-" ]
        , Html.span [] [ Html.text (toString amount) ]
        , Html.button [ onClick (Increment counter) ] [ Html.text "+" ]
        ]


type alias Partials =
    { carol : Html Intent
    , danika : Html Intent
    , dcft : Html Intent
    , mainSearch : Html Intent
    , todo : Html Intent
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
        , partials.danika
        , Html.hr [] []
        , Html.div []
            [ partials.dcft
            ]
        , Html.hr [] []
        , partials.todo
        ]



-- Helpers


asCmd : msg -> Cmd msg
asCmd msg =
    Task.perform identity (Task.succeed msg)
