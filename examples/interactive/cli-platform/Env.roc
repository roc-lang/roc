interface Env
    exposes [varUtf8]
    imports [Effect, Task.{ Task }, InternalTask]

EnvErr a : [
    GetEnvErr [
        VarNotFound Str,
        VarNotUtf8 #Str.Utf8Problem
    ]
]a

varUtf8 : Str -> Task Str (EnvErr *) [Env]*
varUtf8 = \var ->
    Effect.envVarUtf8 var
    |> Effect.map Ok # TODO actually handle errors
    |> InternalTask.fromEffect
