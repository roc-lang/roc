interface Process
    exposes [exit]
    imports [Task.{ Task }, InternalTask, Effect]

## Exit the process with
##
##     {} <- Stderr.line "Exiting right now!" |> Task.await
##     Process.exit 1
exit : U8 -> Task {} *
exit = \code ->
    Effect.processExit code
    |> Effect.map \_ -> Ok {}
    |> InternalTask.fromEffect
