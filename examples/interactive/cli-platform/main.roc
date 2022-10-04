platform "cli"
    requires {} { main : InternalProgram }
    exposes []
    packages {}
    imports [Effect.{ Effect }, InternalProgram.{ InternalProgram }]
    imports [Task.{ Task }, InternalTask, Effect.{ Effect }]
    tests expectFx
    targets {
        x86_64-unknown-freebsd: "my_x64_freebsd_build_cmd",
        wasm32-unknown-unknown: "my_wasm_build_cmd",
    }
    provides [mainForHost]

mainForHost : Effect U8 as Fx
mainForHost = InternalProgram.toEffect main

expectFx : Task {} * * -> Effect {}
expectFx = \task ->
    Effect.after (InternalTask.toEffect task) \taskResult ->
        # we're using `== Ok {}` here so that if it's an `Err` instead,
        # we'll see the contents of the `Err` in the failure message
        expect taskResult == Ok {}

        Effect.always {}