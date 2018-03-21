module CqrsWithOutcome exposing (Outcome(..), programWithFlags)


type Outcome intent fact = Intent intent | Fact fact


programWithFlags setup =
    let
        merge =
            do setup.apply setup.produce

        doInit ( model, facts, fxs ) =
            merge model ( facts, fxs )

        init flags =
            setup.init flags |> doInit

        update outcome model =
            case outcome of
                Fact fact ->
                    ( [ fact ], [] ) |> merge model

                Intent intent ->
                    setup.interpret intent model |> merge model
    in
    { init = init
    , subscriptions = setup.subscriptions
    , update = update
    , view = setup.view
    }


type alias ProduceEffect fx outcome model =
    fx -> model -> Cmd outcome


type alias ApplyFact fact model =
    fact -> model -> model


do : ApplyFact fact model -> ProduceEffect fx outcome model -> model -> ( List fact, List fx ) -> ( model, Cmd outcome )
do apply produce model ( facts, fxs ) =
    mergeFxs produce (List.foldl apply model facts) Cmd.none fxs


mergeFxs : ProduceEffect fx outcome model -> model -> Cmd outcome -> List fx -> ( model, Cmd outcome )
mergeFxs produce model cmd fxs =
    case fxs of
        fx :: rest ->
            mergeFxs produce model (Cmd.batch [ cmd, produce fx model ]) rest

        [] ->
            ( model, cmd )
