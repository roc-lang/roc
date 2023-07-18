interface InternalCommand
    exposes [
        Command,
        Output,
        CommandErr,
    ]
    imports []

CommandErr : [
    ExitCode I32,
    KilledBySignal,
    IOError Str,
]

Command : {
    program : Str,
    args : List Str, # [arg0, arg1, arg2, arg3, ...]
    envs : List Str, # [key0, value0, key1, value1, key2, value2, ...]
    clearEnvs : Bool,
}

Output : {
    status : Result {} CommandErr,
    stdout : List U8,
    stderr : List U8,
}
