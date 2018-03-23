module MainDcft exposing (Msg, Model, init, main, vigor)

import Data.Character as Character exposing (Character)
import Dcft
import Html exposing (Html)
import Html.Events as Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Value)
import Task
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
    , danika : Vigors.Counter.Model
    , kingRequest : KingRequest
    , search : Vigors.Autocomplete.Model
    , suggestionSupply : SuggestionSupply
    }


type KingRequest = NotAskingForKing | AskingForKing | KingRequested Character


type SuggestionSupply = NotAskingForSuggestions | AskingForSuggestions | StaticSuggestions (List String)


type CounterId
    = Alice
    | Bob
    | Carol
    | Danika


type Msg
    = AskWhoIsKingInTheNorth
    | AutocompleteMsg Vigors.Autocomplete.Msg
    | CounterMsg CounterId Vigors.Counter.Msg
    | Decrement CounterId
    | Increment CounterId
    | KingInTheNorthReceived (Result Http.Error Character)
    | Reset


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


compositeView model =
    view
        { carol = carol.view model
        , danika = danika.view model
        , mainSearch = mainSearch.view model
        }
        model


program =
    Dcft.programWithFlags
        { init = init
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


main : Program Value Model Msg
main =
    Html.programWithFlags <|
        Vigors.compose program
            [ carol
            , danika
            , mainSearch
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : Value -> Model
init _ =
    { alice = 0
    , bob = 0
    , carol = Vigors.Counter.init "Carol" 0
    , danika = Vigors.Counter.init "Danika" 0
    , clicked = 0
    , kingRequest = NotAskingForKing
    , search =
        Vigors.Autocomplete.init
            { placeholder = "Search for something..."
            }
    , suggestionSupply = NotAskingForSuggestions
    }


incrementOverall : Model -> Model
incrementOverall model =
    { model | clicked = model.clicked + 1 }


interpret : Msg -> Model -> Model
interpret intent model =
    case intent of
        AskWhoIsKingInTheNorth ->
            { model | kingRequest = AskingForKing
            }

        AutocompleteMsg (FetchSuggestions text) ->
            { model | suggestionSupply = AskingForSuggestions
            }

        AutocompleteMsg (SuggestionsFetched suggestions) ->
            { model | suggestionSupply = StaticSuggestions suggestions
            }

        AutocompleteMsg _ ->
            model

        CounterMsg _ _ ->
            model

        Decrement Alice ->
            { model | alice = model.alice - 1 }
                |> incrementOverall

        Decrement Bob ->
            { model | bob = model.bob - 1 }
                |> incrementOverall

        Decrement _ ->
            model

        Increment Alice ->
            { model | alice = model.alice + 1 }
                |> incrementOverall

        Increment Bob ->
            { model | bob = model.bob + 1 }
                |> incrementOverall

        Increment _ ->
            model
                |> incrementOverall

        KingInTheNorthReceived (Ok king) ->
            Debug.log (king.name ++ "/" ++ String.join "," king.titles)
                { model | kingRequest = KingRequested king }

        KingInTheNorthReceived (Err reason) ->
            Debug.log ("Couldn't find the answer: " ++ toString reason)
                { model | kingRequest = NotAskingForKing }

        Reset ->
            { alice = 1
            , bob = 1
            , carol = Vigors.Counter.init "Carol" 1
            , clicked = 0
            , danika = Vigors.Counter.init "Danika" 1
            , kingRequest = NotAskingForKing
            , search =
                Vigors.Autocomplete.init
                    { placeholder = "Search for something..."
                    }
            , suggestionSupply = NotAskingForSuggestions
            }


produce : Model -> Cmd Msg
produce model =
    Cmd.batch
        [ case model.kingRequest of
            AskingForKing ->
                Character.whoIsTheKing KingInTheNorthReceived

            _ ->
                Cmd.none

        , case model.suggestionSupply of
            AskingForSuggestions ->
                AutocompleteMsg (SuggestionsFetched [ "Aang", "Katara", "Sooka" ])
                    |> asCmd

            _ ->
                Cmd.none
        ]


renderCounter : CounterId -> Int -> Html Msg
renderCounter counter amount =
    Html.div []
        [ Html.button [ onClick (Decrement counter) ] [ Html.text "-" ]
        , Html.span [] [ Html.text (toString amount) ]
        , Html.button [ onClick (Increment counter) ] [ Html.text "+" ]
        ]


type alias Partials =
    { carol : Html Msg
    , danika : Html Msg
    , mainSearch : Html Msg
    }


view : Partials -> Model -> Html Msg
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
        ]



-- Helpers


asCmd : msg -> Cmd msg
asCmd msg =
    Task.perform identity (Task.succeed msg)
