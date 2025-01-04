module [
    Task,
    ok,
    err,
    await,
    map,
    map_err,
    on_err,
    attempt,
    forever,
    loop,
    from_result,
    batch,
    combine,
    sequence,
    for_each,
    result,
]

import List
import Result exposing [Result]

## A Task represents an effect; an interaction with state outside your Roc
## program, such as the terminal's standard output, or a file.
Task ok err := {} -> Result ok err

## Run a task repeatedly, until it fails with `err`. Note that this task does not return a success value.
forever : Task a err -> Task * err
forever = \@Task(task) ->
    looper = \{} ->
        when task({}) is
            Err(e) -> Err(e)
            Ok(_) -> looper({})

    @Task(\{} -> looper({}))

## Run a task repeatedly, until it fails with `err` or completes with `done`.
##
## ```
## sum =
##     Task.loop!(0, \total ->
##         num_result =
##             Stdin.line
##                 |> Task.result!
##                 |> Result.try(Str.toU64)
##
##         when num_result is
##             Ok num -> Task.ok(Step(total + num))
##             Err(StdinErr(EndOfFile)) -> Task.ok(Done(total))
##             Err(InvalidNumStr) -> Task.err(NonNumberGiven)
##     )
## ```
loop : state, (state -> Task [Step state, Done done] err) -> Task done err
loop = \state, step ->
    looper = \current ->
        @Task(next) = step(current)
        when next({}) is
            Err(e) -> Err(e)
            Ok(Done(new_result)) -> Ok(new_result)
            Ok(Step(new_state)) -> looper(new_state)

    @Task(\{} -> looper(state))

## Create a task that always succeeds with the value provided.
##
## ```
## # Always succeeds with "Louis"
## get_name : Task.Task Str *
## get_name = Task.ok("Louis")
## ```
##
ok : a -> Task a *
ok = \a -> @Task(\{} -> Ok(a))

## Create a task that always fails with the error provided.
##
## ```
## # Always fails with the tag `CustomError Str`
## customError : Str -> Task.Task {} [CustomError Str]
## customError = \err -> Task.err(CustomError(err))
## ```
##
err : a -> Task * a
err = \a -> @Task(\{} -> Err(a))

## Transform a given Task with a function that handles the success or error case
## and returns another task based on that. This is useful for chaining tasks
## together or performing error handling and recovery.
##
## Consider the following task:
##
## `can_fail : Task {} [Failure, AnotherFail, YetAnotherFail]`
##
## We can use [attempt] to handle the failure cases using the following:
##
## ```
## Task.attempt(can_fail, \result ->
##     when result is
##         Ok(Success) -> Stdout.line("Success!")
##         Err(Failure) -> Stdout.line("Oops, failed!")
##         Err(AnotherFail) -> Stdout.line("Ooooops, another failure!")
##         Err(YetAnotherFail) -> Stdout.line("Really big oooooops, yet again!")
## )
## ```
##
## Here we know that the `can_fail` task may fail, and so we use
## `Task.attempt` to convert the task to a `Result` and then use pattern
## matching to handle the success and possible failure cases.
attempt : Task a b, (Result a b -> Task c d) -> Task c d
attempt = \@Task(task), transform ->
    @Task(\{} ->
        @Task(transformed) = transform(task({}))

        transformed({}))

## Take the success value from a given [Task] and use that to generate a new [Task].
##
## We can [await] Task results with callbacks:
##
## ```
## Task.await(Stdin.line("What's your name?")), \name ->
##     Stdout.line("Your name is: $(name)")
## )
## ```
##
## Or we can more succinctly use the `!` bang operator, which desugars to [await]:
##
## ```
## name = Stdin.line!("What's your name?")
## Stdout.line("Your name is: $(name)")
## ```
await : Task a b, (a -> Task c b) -> Task c b
await = \@Task(task), transform ->
    @Task(\{} ->
        when task({}) is
            Ok(a) ->
                @Task(transformed) = transform(a)
                transformed({})

            Err(b) ->
                Err(b))

## Take the error value from a given [Task] and use that to generate a new [Task].
##
## ```
## # Prints "Something went wrong!" to standard error if `can_fail` fails.
## can_fail
## |> Task.on_err(\_ -> Stderr.line("Something went wrong!"))
## ```
on_err : Task a b, (b -> Task a c) -> Task a c
on_err = \@Task(task), transform ->
    @Task(\{} ->
        when task({}) is
            Ok(a) ->
                Ok(a)

            Err(b) ->
                @Task(transformed) = transform(b)
                transformed({}))

## Transform the success value of a given [Task] with a given function.
##
## ```
## # Succeeds with a value of "Bonjour Louis!"
## Task.ok("Louis")
## |> Task.map(\name -> "Bonjour $(name)!")
## ```
map : Task a c, (a -> b) -> Task b c
map = \@Task(task), transform ->
    @Task(\{} ->
        when task({}) is
            Ok(a) -> Ok(transform(a))
            Err(b) -> Err(b))

## Transform the error value of a given [Task] with a given function.
##
## ```
## # Ignore the fail value, and map it to the tag `CustomError`
## can_fail
## |> Task.map_err(\_ -> CustomError)
## ```
map_err : Task c a, (a -> b) -> Task c b
map_err = \@Task(task), transform ->
    @Task(\{} ->
        when task({}) is
            Ok(a) -> Ok(a)
            Err(b) -> Err(transform(b)))

## Use a Result among other Tasks by converting it into a [Task].
from_result : Result a b -> Task a b
from_result = \res ->
    @Task(\{} -> res)

## Apply a task to another task applicatively.
##
## DEPRECATED: Modern record builders use [combine].
batch : Task a c -> (Task (a -> b) c -> Task b c)
batch = \current ->
    \next ->
        await(next, \f ->
            map(current, f))

## Combine the values of two tasks with a custom combining function.
##
## This is primarily used with record builders.
##
## ```
## { a, b, c } =
##     { Task.combine <-
##         a: Task.ok(123),
##         b: File.read("file.txt"),
##         c: Http.get("http://api.com/"),
##     }!
## ```
combine : Task a err, Task b err, (a, b -> c) -> Task c err
combine = \@Task(left_task), @Task(right_task), combiner ->
    @Task(\{} ->
        left = try(left_task, {})
        right = try(right_task, {})

        Ok(combiner(left, right)))

## Apply each task in a list sequentially, and return a list of the resulting values.
## Each task will be awaited before beginning the next task.
##
## ```
## fetch_author_tasks : List (Task Author [DbError])
##
## get_authors : Task (List Author) [DbError]
## get_authors = Task.sequence(fetch_author_tasks)
## ```
##
sequence : List (Task ok err) -> Task (List ok) err
sequence = \task_list ->
    Task.loop((task_list, List.with_capacity(List.len(task_list))), \(tasks, values) ->
        when tasks is
            [task, .. as rest] ->
                Task.map(task, \value ->
                    Step((rest, List.append(values, value))))

            [] ->
                Task.ok(Done(values)))

## Apply a task repeatedly for each item in a list
##
## ```
## authors : List Author
## save_author : Author -> Task {} [DbError]
##
## save_authors : Task (List Author) [DbError]
## save_authors = Task.for_each(authors, save_author)
## ```
##
for_each : List a, (a -> Task {} b) -> Task {} b
for_each = \items, fn ->
    List.walk(items, ok({}), \state, item ->
        state |> await(\_ -> fn(item)))

## Transform a task that can either succeed with `ok`, or fail with `err`, into
## a task that succeeds with `Result ok err`.
##
## This is useful when chaining tasks using the `!` suffix. For example:
##
## ```
## # Path.roc
## check_file : Str -> Task [Good, Bad] [IOError]
##
## # main.roc
## when check_file("/usr/local/bin/roc") |> Task.result! is
##     Ok(Good) -> "..."
##     Ok(Bad) -> "..."
##     Err(IOError) -> "..."
## ```
##
result : Task ok err -> Task (Result ok err) *
result = \@Task(task) ->
    @Task(\{} ->
        Ok(task({})))
