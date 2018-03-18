module It exposing (wrap)


wrap setup =
    let
        merge =
            do setup.apply setup.produce

        coalesce ( model, cmd ) =
            ( model, Cmd.map setup.join cmd )

        doInit ( model, facts, fxs ) =
            merge model ( facts, fxs ) |> coalesce

        init flags =
            setup.init flags |> doInit

        update intent model =
            setup.interpret intent model |> merge model |> coalesce
    in
    { init = init
    , subscriptions = setup.subscriptions
    , update = update
    , view = setup.view
    }


type alias ProduceEffect fx fact model =
    fx -> model -> Cmd fact


type alias ApplyFact fact model =
    fact -> model -> model


do : ApplyFact fact model -> ProduceEffect fx fact model -> model -> ( List fact, List fx ) -> ( model, Cmd fact )
do apply produce model ( facts, fxs ) =
    mergeFxs produce (List.foldl apply model facts) Cmd.none fxs


mergeFxs : ProduceEffect fx fact model -> model -> Cmd fact -> List fx -> ( model, Cmd fact )
mergeFxs produce model cmd fxs =
    case fxs of
        fx :: rest ->
            mergeFxs produce model (Cmd.batch [ cmd, produce fx model ]) rest

        [] ->
            ( model, cmd )
