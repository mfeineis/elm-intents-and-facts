module Vigors exposing (compose)



compose program vigors =
    let
        init = program.init

        subscriptions =
            \model ->
                Sub.batch <|
                    program.subscriptions model ::
                        List.map (\vigor -> vigor.subscriptions model) vigors

        merge msg vigors ( model, cmd ) =
            case vigors of
                vigor :: rest ->
                    let
                        ( newModel, newCmd ) =
                            vigor msg model
                    in
                    merge msg rest ( newModel, Cmd.batch [ cmd, newCmd ])

                [] ->
                    ( model, cmd )

        update =
            \msg model ->
                merge msg (program.update :: List.map .update vigors) ( model, Cmd.none )

        view = program.view
    in
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }

