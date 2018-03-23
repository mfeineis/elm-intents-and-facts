module Dcft exposing (programWithFlags)


programWithFlags setup =
    { init =
        \flags ->
            setup.init flags
                |> \model -> ( model, setup.produce model )

    , subscriptions = setup.subscriptions
    , update =
        \intent model ->
            setup.interpret intent model
                |> \model -> ( model, setup.produce model )

    , view = setup.view
    }
