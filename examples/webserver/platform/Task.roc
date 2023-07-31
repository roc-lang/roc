interface Task
    exposes [
        Task,
        ok,
        err,
        await,
        awaitResult,
        map,
        mapErr,
        onErr,
        attempt,
        forever,
        loop,
        fromResult,
        batch,
    ]
    imports [Effect, InternalTask]

## A Task represents an effect; an interaction with state outside your Roc
## program, such as the terminal's standard output, or a file.
Task ok err : InternalTask.Task ok err

## Run a task that never ends. Note that this task does not return a value.
forever : Task a err -> Task * err
forever = \task ->
    looper = \{} ->
        task
        |> InternalTask.toEffect
        |> Effect.map
            \res ->
                when res is
                    Ok _ -> Step {}
                    Err e -> Done (Err e)

    Effect.loop {} looper
    |> InternalTask.fromEffect

## Run a task repeatedly, until it fails with `err` or completes with `done`.
loop : state, (state -> Task [Step state, Done done] err) -> Task done err
loop = \state, step ->
    looper = \current ->
        step current
        |> InternalTask.toEffect
        |> Effect.map
            \res ->
                when res is
                    Ok (Step newState) -> Step newState
                    Ok (Done result) -> Done (Ok result)
                    Err e -> Done (Err e)

    Effect.loop state looper
    |> InternalTask.fromEffect

## Create a task that always succeeds with the value provided.
##
## ```
## # Always succeeds with "Louis"
## getName : Task.Task Str *
## getName = Task.ok "Louis"
## ```
##
ok : a -> Task a *
ok = \a -> InternalTask.ok a

## Create a task that always fails with the error provided.
##
## ```
## # Always fails with the tag `CustomError Str`
## customError : Str -> Task.Task {} [CustomError Str]
## customError = \err -> Task.err (CustomError err)
## ```
##
err : a -> Task * a
err = \a -> InternalTask.err a

## Transform a given Task with a function that handles the success or error case
## and returns another task based on that. This is useful for chaining tasks
## together or performing error handling and recovery.
##
## Consider a the following task;
##
## `canFail : Task {} [Failure, AnotherFail, YetAnotherFail]`
##
## We can use [attempt] to handle the failure cases using the following;
##
## ```
## result <- canFail |> Task.attempt
## when result is
##     Ok Success -> Stdout.line "Success!"
##     Err Failure -> Stdout.line "Oops, failed!"
##     Err AnotherFail -> Stdout.line "Ooooops, another failure!"
##     Err YetAnotherFail -> Stdout.line "Really big oooooops, yet again!"
## ```
##
## Here we know that the `canFail` task may fail, and so we use
## `Task.attempt` to convert the task to a `Result` and then use pattern
## matching to handle the success and possible failure cases.
##
attempt : Task a b, (Result a b -> Task c d) -> Task c d
attempt = \task, transform ->
    effect = Effect.after
        (InternalTask.toEffect task)
        \result ->
            when result is
                Ok a -> transform (Ok a) |> InternalTask.toEffect
                Err b -> transform (Err b) |> InternalTask.toEffect

    InternalTask.fromEffect effect

## Take the success value from a given [Task] and use that to generate a new [Task].
##
## For example we can use this to run tasks in sequence like follows;
##
## ```
## # Prints "Hello World!\n" to standard output.
## {} <- Stdout.write "Hello "|> Task.await
## {} <- Stdout.write "World!\n"|> Task.await
##
## Task.ok {}
## ```
await : Task a b, (a -> Task c b) -> Task c b
await = \task, transform ->
    effect = Effect.after
        (InternalTask.toEffect task)
        \result ->
            when result is
                Ok a -> transform a |> InternalTask.toEffect
                Err b -> err b |> InternalTask.toEffect

    InternalTask.fromEffect effect

## Take the error value from a given [Task] and use that to generate a new [Task].
##
## ```
## # Prints "Something went wrong!" to standard error if `canFail` fails.
## canFail
## |> Task.onErr \_ -> Stderr.line "Something went wrong!"
## ```
onErr : Task a b, (b -> Task a c) -> Task a c
onErr = \task, transform ->
    effect = Effect.after
        (InternalTask.toEffect task)
        \result ->
            when result is
                Ok a -> ok a |> InternalTask.toEffect
                Err b -> transform b |> InternalTask.toEffect

    InternalTask.fromEffect effect

## Transform the success value of a given [Task] with a given function.
##
## ```
## # Succeeds with a value of "Bonjour Louis!"
## Task.ok "Louis"
## |> Task.map (\name -> "Bonjour \(name)!")
## ```
map : Task a c, (a -> b) -> Task b c
map = \task, transform ->
    effect = Effect.after
        (InternalTask.toEffect task)
        \result ->
            when result is
                Ok a -> ok (transform a) |> InternalTask.toEffect
                Err b -> err b |> InternalTask.toEffect

    InternalTask.fromEffect effect

## Transform the error value of a given [Task] with a given function.
##
## ```
## # Ignore the fail value, and map it to the tag `CustomError`
## canFail
## |> Task.mapErr \_ -> CustomError
## ```
mapErr : Task c a, (a -> b) -> Task c b
mapErr = \task, transform ->
    effect = Effect.after
        (InternalTask.toEffect task)
        \result ->
            when result is
                Ok c -> ok c |> InternalTask.toEffect
                Err a -> err (transform a) |> InternalTask.toEffect

    InternalTask.fromEffect effect

## Use a Result among other Tasks by converting it into a [Task].
fromResult : Result a b -> Task a b
fromResult = \result ->
    when result is
        Ok a -> ok a
        Err b -> err b

## Shorthand for calling [Task.fromResult] on a [Result], and then
## [Task.await] on that [Task].
awaitResult : Result a err, (a -> Task c err) -> Task c err
awaitResult = \result, transform ->
    when result is
        Ok a -> transform a
        Err b -> err b

## Apply a task to another task applicatively. This can be used with
## [ok] to build a [Task] that returns a record.
##
## The following example returns a Record with two fields, `apples` and
## `oranges`, each of which is a `List Str`. If it fails it returns the tag
## `NoFruitAvailable`.
##
## ```
## getFruitBasket : Task { apples : List Str, oranges : List Str } [NoFruitAvailable]
## getFruitBasket = Task.ok {
##     apples: <- getFruit Apples |> Task.batch,
##     oranges: <- getFruit Oranges |> Task.batch,
## }
## ```
batch : Task a c -> (Task (a -> b) c -> Task b c)
batch = \current -> \next ->
        f <- next |> await

        map current f
