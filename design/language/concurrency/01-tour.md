# A Tour of Concurrency in Roc

This tour shows the proposed design the way you'd use it: as code. It adds no
`async`, no futures and no task type. The effectful functions you already
write stay exactly as they are, and you run them concurrently by putting them
in a list.

The examples use a hypothetical platform with functions like
`Http.get_utf8! : Str => Try(Str, [HttpErr(U16)])` and
`Sleep.millis! : U64 => {}`. No compiler checks these examples yet, and every
name in them is a placeholder.

## Running Effects at the Same Time

`Concurrent.both!` runs two effectful functions concurrently and returns when
both have finished:

```roc
fetch_pair! : Str, Str => Try((Str, Str), [HttpErr(U16)])
fetch_pair! = |a, b| {
    (first, second) = Concurrent.both!(|| Http.get_utf8!(a), || Http.get_utf8!(b))
    Ok((first?, second?))
}
```

Each argument is an ordinary effectful function that takes no arguments.
Nothing about `Http.get_utf8!` changes: it's the same function a sequential
program calls.

`Concurrent.all_ok!` does the same for a list of functions that return `Try`.
The first `Err` to arrive cancels the rest and is returned; otherwise you get
every result, in input order:

```roc
fetch_all! : List(Str) => Try(List(Str), [HttpErr(U16)])
fetch_all! = |urls| Concurrent.all_ok!(urls.map(|url| || Http.get_utf8!(url)))
```

`List.map` stays pure here. The function passed to it only creates effectful
closures, and creating a function isn't an effect. `Concurrent.all!` is the
same without the `Try` handling.

## Reacting as Results Arrive

When you want to decide what to do as each result comes in, loop over a
_plan_. A plan is a value that describes work that may overlap. Building one,
storing it, or returning it runs nothing:

```roc
# Done once 2 of 3 replicas acknowledge; the third write is cancelled.
write_quorum! : List(Replica), Bytes => Try({}, [NoQuorum])
write_quorum! = |replicas, data| {
    var $acks = 0
    for! result in replicas.map(|r| || Db.write!(r, data)).concurrently() {
        match result {
            Ok({}) => {
                $acks = $acks + 1
                if $acks == 2 { return Ok({}) }
            }
            Err(_) => {}
        }
    }
    Err(NoQuorum)
}
```

`List.concurrently` turns a list of effectful functions into a plan. The
`for!` loop is what runs it:

- Entering the loop starts each _entry_ of the plan as a child task.
- Each iteration gets one entry's result, in the order the entries finish.
- Leaving the loop by `break`, `return` or `?` cancels the entries still
  running, and waits until they've stopped. Reaching the end of the loop means
  every entry finished.

The body is ordinary effectful code. Its state lives in `var`s, and it can
log, call other functions, and run loops of its own. The library functions
above, `both!`, `all!`, `all_ok!` and `race!`, are written with loops like
this one.

## Shaping a Plan

Plans compose. Each of these is a pure function that returns a new plan:

| | |
|---|---|
| `plan.limit(n)` | At most `n` of the plan's entries run at once; the next starts as soon as one finishes |
| `plan.shielded()` | Leaving the loop waits for entries that have begun, instead of cancelling them |
| `plan.background()` | These entries don't keep the loop going: once every other entry has finished, they're cancelled and the loop ends |
| `plan.concat(other)` | `plan`'s entries, then `other`'s |

A deadline is a background entry. This search returns whatever the shards
have answered after 200 milliseconds, or sooner if they all answer:

```roc
search! = |shards, query| {
    var $hits = []
    deadline = [|| { Sleep.millis!(200); TimedOut }].concurrently().background()
    for! arrival in shards.map(|s| || Answered(Shard.search!(s, query))).concurrently().concat(deadline) {
        match arrival {
            Answered(result) => { $hits = $hits.concat(result ?? []) }
            TimedOut => break
        }
    }
    $hits
}
```

The entries return different tags, and the anonymous union
`[Answered(…), TimedOut]` holds both. That's how one loop combines different
kinds of work: each entry tags its result, and the body matches on it.

Plans also have `all!` and `all_ok!` methods, which return the results in
input order. This fetches every URL, at most eight at a time:

```roc
fetch_all_politely! = |urls| urls.map(|url| || Http.get_utf8!(url)).concurrently().limit(8).all_ok!()
```

A type can define its own `concurrently` method that returns a plan, the way
it can define `iter` for `for` loops. Your type decides what runs and how it's
grouped; the runtime decides how it runs.

## Adding Work From Inside a Loop

Inside a loop body, `spawn f` adds one more entry to the running loop. A
hedged request sends a backup only when the primary is slow:

```roc
hedged_get! = |primary, backup, key| {
    for! arrival in [|| Got(Kv.get!(primary, key)), || { Sleep.millis!(50); Slow }].concurrently() {
        match arrival {
            Got(Ok(value)) => return Ok(value)
            Got(Err(_)) => {}
            Slow => spawn || Got(Kv.get!(backup, key))
        }
    }
    Err(AllFailed)
}
```

If the loop's plan is limited, spawned entries count against the limit. A
crawler that spawns a fetch for every link it finds, in a loop over
`[fetch(start)].concurrently().limit(8)`, never runs more than eight fetches at
once.

## Racing and Timeouts

`Concurrent.race!` returns whichever function finishes first and cancels the
rest. It takes its first function separately, so it can never be called with
nothing to race:

```roc
fetch_with_timeout! : Str => Try(Str, [HttpErr(U16), TimedOut])
fetch_with_timeout! = |url|
    match Concurrent.race!(|| Fetched(Http.get_utf8!(url)), [|| { Sleep.millis!(2_000); TimedOut }]) {
        Fetched(Ok(body)) => Ok(body)
        Fetched(Err(HttpErr(code))) => Err(HttpErr(code))
        TimedOut => Err(TimedOut)
    }
```

## Cancellation

A cancelled entry stops at the point where it's waiting, and none of its code
runs afterward. Its values are released. Effects it already performed stay
performed, since nothing can unsend a request.

When you must account for every effect, as a saga that compensates each
completed step must, don't leave the loop at the first failure. Keep looping
and compensate each later success as it arrives, or shield the entries whose
outcome you need to know.

## Crashes and Supervision

Roc follows Erlang/OTP's "let it crash" approach. Errors you expect, like a
failed request, stay `Err` values. A crash is for a bug or a condition the
program can't handle, and it ends the work it happens in:

- **A crash ends its task.** No Roc code catches it.
- **Crashes travel up the structure.** When an entry crashes, the loop's task
  crashes with the same message wherever it's waiting, and its other entries
  are cancelled.
- **Supervisors stop them.** The platform supervises every top-level task. A
  web server can fail just the crashed request and keep serving; a
  command-line tool can exit with an error.

`Concurrent.supervise!` puts a supervisor inside your program. It runs a
function and restarts it after a crash, up to a limit, and then crashes
itself. It never reports the crash to its caller, so it can't be used to catch
errors:

```roc
# Keep a queue consumer running across bugs in individual messages.
run_consumer! = |queue|
    Concurrent.supervise!({ max_restarts: 10 }, || consume_forever!(queue))
```

## What You Can Rely On

- **Tasks switch only where they wait.** A function runs without interruption
  until it calls a platform function that can pause, waits at a loop, or
  waits on shared state. Pure code is never interrupted.
- **There are no data races.** Concurrent tasks share only immutable values,
  plus the shared state described next, which changes only through atomic
  updates.
- **Nothing outlives the loop that started it.** A loop's entries, and
  everything they start, finish or are cancelled before the loop is left.
- **Effects within one task happen in program order.** Between tasks, the
  order is whatever the schedule gives.
- **Pure code is untouched.** A pure function can build plans but can't run
  them, and it compiles exactly as it does today.

## Sharing State That Changes

Everything so far shares only immutable values. When several running tasks
need the same changing state, such as a count, a cache or a pool of
connections, you put it behind a _handle_:

```roc
hits = Shared.new!(0)
```

`hits` has type `Shared(U64)`. You can capture it, pass it, and store it in a
record like any value. You change the state with `update!`, giving it a pure
function that gets the current state and answers with a tag:

```roc
decision = breaker.update!(|state| match state {
    Open(until) if now < until => Keep(Reject)
    Open(_) => Set(Probing, Probe)
    Probing => Keep(Reject)
    Closed(_) => Keep(Run)
})
```

| The function answers | The state | `update!` returns |
|---|---|---|
| `Set(new, r)` | Becomes `new` | `r` |
| `Keep(r)` | Stays as it was | `r` |
| `Wait` | Stays as it was | Nothing yet: the update runs again after the state changes |

The function runs atomically: no other task changes the state while it runs.
That needs no keyword. The function is pure, so it can't do I/O or pause, and
nothing can happen in the middle of it. Two shorthands cover the commonest
cases: `hits.get!()` reads the state, and `hits.modify!(|n| n + 1)` replaces
it.

## Waiting for State to Change

An update that answers `Wait` pauses its task until another task changes the
state, then tries again. This is a whole semaphore:

```roc
with_permit! : Shared(U64), (() => a) => a
with_permit! = |permits, f| {
    permits.update!(|n| if n == 0 { Wait } else { Set(n - 1, {}) })
    permits.finally!(|n| n + 1, f)
}
```

When a permit comes back, the waiting tasks aren't all woken to compete for
it. Their functions are pure, so the runtime can ask each one, oldest first,
what it would do with the new state, and wake only the first that wouldn't
wait. Ten tasks waiting for one permit cost one wakeup per release, not ten.

## Cleanup However a Task Ends

`finally!` in the semaphore above runs `f`, then applies `|n| n + 1` to the
state however `f` ends: by returning, by being cancelled, or by crashing. So
the permit always comes back. `on_failure!` is the same, except its cleanup
runs only on cancellation or a crash.

A cleanup is a pure function of the state, `s -> s`. That's what lets the
runtime run it for a cancelled task, which can't run any more of its own
effectful code.

## Streaming Between Tasks

A queue in a handle is how tasks stream values to each other. `Queue` is
library code written with `update!`, and `for!` can loop over one:

```roc
import_all! = |lines| {
    queue = Queue.new!(100)
    produce = || queue.closing!(|| {
        for! line in lines {
            match queue.push!(parse(line)) {
                Pushed => {}
                Closed => break
            }
        }
    })
    consume = || {
        for! record in queue {
            Db.insert!(record)
        }
    }
    _ = Concurrent.all!(List.repeat(consume, 4).prepend(produce))
    {}
}
```

- **Backpressure:** `push!` waits while the queue is full, and the reader's
  loop waits while it's empty.
- **The end of the stream:** `closing!` closes the queue however its function
  ends, and a loop over a closed queue takes what's left and then ends.
- **Many readers:** each item goes to exactly one of the four consumers.

A request that needs an answer carries a `Reply`, a one-shot cell the caller
waits on:

```roc
ask! = |requests, request| {
    reply = Reply.new!()
    match requests.push!((request, reply)) {
        Pushed => reply.wait!()
        Closed => Dropped
    }
}

serve! = |requests| {
    for! (request, reply) in requests {
        reply.answer!(|| handle!(request))
    }
}
```

If `handle!` crashes or is cancelled, the reply becomes `Dropped`, so the
caller stops waiting.

## Big States Change in Place

`update!` hands the state to its function, and the handle holds nothing while
the function runs. So unless some task still holds the state from a `get!`,
the function has the only reference, and the state changes in place, the same
as any unique value in Roc. Appending to a queue in a handle costs what
appending to a list costs.

This depends on one habit, which is the usual one for Roc values: decide
first, then build the new state in the branch that answers `Set`.

## Crashes and Shared State

An update or cleanup that crashes _poisons_ its handle: every later use of it
crashes, including every task waiting on it. The crashed function had the
state to itself, so no task ever sees a half-made state.

So when an update finds the state wrong, it's better to answer with a tag that
says so and crash after the update. That way, other tasks keep the state.

A handle created inside a supervised function is new on every restart, so
restarting repairs it. A poisoned handle created outside stays poisoned, and
the restarts crash until the limit is reached. That's deliberate: the broken
state is outside the part being restarted.

## On Platforms That Don't Schedule Tasks

A platform opts into concurrency. On one that doesn't, your program still
compiles and means the same thing: it schedules its own tasks on one thread,
and platform calls finish before they return. Where no task waits for
another, a loop runs its entries one at a time, in order, so:

- `all!` and `both!` run every function and return the same results.
- `all_ok!` stops at the first `Err`, and later functions never start.
- `race!` returns the first function's result, and the others never start.

A timeout can't interrupt a call that blocks, so `fetch_with_timeout!` returns
the fetch's result. Tasks that wait for each other, like a permit that must
come back, still take turns. If no task can run, nothing can end the wait,
and the program crashes with a deadlock message.

So a package can use concurrency freely. It runs concurrently where the
platform supports it, and sequentially everywhere else.

## Choosing a Tool

| To | Use |
|---|---|
| Run a fixed set of effects and use every result | `Concurrent.both!`, `all!`, `all_ok!` |
| Take the first result and cancel the rest | `Concurrent.race!` |
| React to results as they arrive, stop early, or add work | `for! x in plan { … }`, with `spawn` |
| Bound how many run at once | `plan.limit(n)`, or a `Semaphore` across loops |
| Get a plan's results in input order | `plan.all!()`, `plan.all_ok!()` |
| Keep an entry, like a deadline, from holding the loop open | `plan.background()` |
| Let several tasks change one piece of state | `Shared(s)` and `update!` |
| Wait until state changes | `update!` answering `Wait` |
| Undo a change however a task ends | `finally!`, `on_failure!` |
| Stream values between tasks | `Queue`, read with `for!` |
| Restart work after a bug | `Concurrent.supervise!` |

[The API](02-api.md) lists every signature. [The examples](examples.md)
show about forty common patterns, and [the decisions](README.md#the-decisions)
explain why the design looks like this.
