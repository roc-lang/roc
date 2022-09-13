interface Env
    exposes [cwd]
    imports [Task.{ Task }, Path.{ Path }, InternalPath, Effect, InternalTask]

## Reads the [current working directory](https://en.wikipedia.org/wiki/Working_directory)
## from the environment.
cwd : Task Path [CwdUnavailable]* [Env]*
cwd =
    effect = Effect.map Effect.cwd \bytes ->
        if List.isEmpty bytes then
            Err CwdUnavailable
        else
            Ok (InternalPath.fromArbitraryBytes bytes)

    InternalTask.fromEffect effect
