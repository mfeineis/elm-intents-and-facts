module Cqrs exposing (programWithFlags)


programWithFlags setup =
    let
        merge =
            do setup.apply setup.produce

        doInit ( model, facts, fxs ) =
            merge model ( facts, fxs )

        init flags =
            setup.init flags |> doInit

        update intent model =
            setup.interpret intent model |> merge model
    in
    { init = init
    , subscriptions = setup.subscriptions
    , update = update
    , view = setup.view
    }


type alias ProduceEffect fx model intent =
    fx -> model -> Cmd intent


type alias ApplyFact fact model =
    fact -> model -> model


do : ApplyFact fact model -> ProduceEffect fx model intent -> model -> ( List fact, List fx ) -> ( model, Cmd intent )
do apply produce model ( facts, fxs ) =
    mergeFxs produce (List.foldl apply model facts) Cmd.none fxs


mergeFxs : ProduceEffect fx model intent -> model -> Cmd intent -> List fx -> ( model, Cmd intent )
mergeFxs produce model cmd fxs =
    case fxs of
        fx :: rest ->
            mergeFxs produce model (Cmd.batch [ cmd, produce fx model ]) rest

        [] ->
            ( model, cmd )
