interface Cmd
    exposes
        [
            Cmd,
        ]
    imports [ fx.Effect, Child ]

Cmd :
    @Cmd
        {
            exe : Str,
            args : List Str,
            env : {}, # TODO
            cwd : {}, # TODO
            stdio : {}, # TODO
        }

init : Path -> Cmd
init = \exe ->
    @Cmd
        {
            exe,
            args: [],
            env: {},
            cwd: {},
            stdio: {},
        }

spawn : Cmd -> Task Child Io.Err
spawn = \cmd ->
    # TODO
