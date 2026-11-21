# The API

Every function the proposal adds, by signature. [The tour](01-tour.md) shows
them in use. All names are placeholders.

## Plans

`Concurrently(a)` is a builtin opaque type. A _plan_ is an inert value: a tree
of entries, limits, shields and background marks. It owns its closures, is
reference counted like any value, and runs nothing until a `for!` loop or one
of its `!` methods runs it.

```roc
List.concurrently : List(() => a) -> Concurrently(a)

Concurrently.limit : Concurrently(a), U64 -> Concurrently(a)       # crashes if the limit is 0
Concurrently.shielded : Concurrently(a) -> Concurrently(a)
Concurrently.background : Concurrently(a) -> Concurrently(a)
Concurrently.concat : Concurrently(a), Concurrently(a) -> Concurrently(a)

Concurrently.all! : Concurrently(a) => List(a)                     # results in input order
Concurrently.all_ok! : Concurrently(Try(a, err)) => Try(List(a), err)
```

The results of background entries aren't in the lists `all!` and `all_ok!`
return.

## Loops

```roc
for! x in plan { … }      # plan : Concurrently(a), so x : a
spawn f                   # inside the body: f : () => a joins the running loop
```

`for!` already calls `stream` on what it loops over. A plan is a third case,
chosen by its type. The set of running entries exists only inside the loop,
and no expression names it, so it can't be stored, captured or returned.

| Way the loop ends | What happens to entries still running |
|---|---|
| Every entry that isn't background has finished | Background entries still running are cancelled |
| `break`, `return` or `?` | Cancelled, and the loop waits until they've stopped. Shielded entries that have begun are waited for instead |
| An entry crashes | The loop's task crashes wherever it waits, and every entry is cancelled, shielded ones included |

A spawned entry counts against the limit at the root of the loop's plan, if
the root is a limit. A spawned entry is never a background entry.

## `Concurrent`

```roc
Concurrent :: [].{
    both! : (() => a), (() => b) => (a, b)
    all! : List(() => a) => List(a)
    all_ok! : List(() => Try(a, err)) => Try(List(a), err)
    race! : (() => a), List(() => a) => a
    supervise! : { max_restarts : U64 }, (() => a) => a
}
```

- `Concurrent.all!(fs)` is `fs.concurrently().all!()`, and likewise
  `all_ok!`. `both!` is `all!` over two results.
- `race!` leaves its loop at the first result.
- `supervise!` runs its function as a child task and runs it again after a
  crash, until it has restarted `max_restarts` times. Then it crashes with the
  last message.

Every function here except `supervise!` is ordinary library code written with
a loop.

## Shared State

`Shared(s)` is a builtin opaque type: a _handle_ to state that several tasks
can change. It has no other functions, no equality and no hashing, and
`Inspect` shows only `<Shared>`.

```roc
Shared.new! : s => Shared(s)
Shared.update! : Shared(s), (s -> [Set(s, r), Keep(r), Wait]) => r
Shared.finally! : Shared(s), (s -> s), (() => a) => a
Shared.on_failure! : Shared(s), (s -> s), (() => a) => a
Shared.get! : Shared(s) => s                                       # update!(|s| Keep(s))
Shared.modify! : Shared(s), (s -> s) => {}                         # update!(|s| Set(g(s), {}))
```

- **The functions passed in are pure.** That's the whole atomicity rule, and
  the type checker already enforces it.
- **`s` can be any type,** functions and other handles included.
- **Platform function signatures can't mention `Shared`** in the first
  version.

| The update answers | The state | `update!` returns | Waiting tasks |
|---|---|---|---|
| `Set(new, r)` | Becomes `new` | `r` | The oldest one that wouldn't wait is woken |
| `Keep(r)` | Unchanged | `r` | Not woken |
| `Wait` | Unchanged | Nothing yet: the update runs again after the state next changes | Not woken |

`finally!` applies its cleanup after its function ends, however it ends: by
returning, by cancellation, or by a crash. `on_failure!` applies it only on
cancellation or a crash. Pending cleanups run innermost first.

An update or cleanup that crashes poisons its handle. Every later update of a
poisoned handle crashes, and so does every task waiting on it. No code can test
for poison or clear it.

### What the Compiler Warns About

- **State that's never shared:** a handle bound to a name that's used only as
  the first argument of `Shared` calls in one function. A `var` does the job.
- **Reading in one update and writing in another,** as in `n = c.get!()`
  followed by `c.modify!(|_| n - 1)`. That's a lost update.
- **A pause between an update and its cleanup:** a call that can suspend
  between an `update!` and the `finally!` or `on_failure!` on the same handle
  that follows it. Cancelling the task there would skip the cleanup.

## The Library

These are library types written with the functions above. They need nothing
else from the runtime.

```roc
Queue(a) :: Shared({ items : List(a), capacity : U64, closed : Bool })
Queue.new! : U64 => Queue(a)
Queue.push! : Queue(a), a => [Pushed, Closed]                       # waits while full
Queue.try_push! : Queue(a), a => [Pushed, Full, Closed]
Queue.pop! : Queue(a) => [Item(a), Closed]                          # waits while empty
Queue.close! : Queue(a) => {}
Queue.closing! : Queue(a), (() => r) => r                           # closes however it ends
Queue.stream : Queue(a) -> Stream(a)                                # so for! can read it

Reply(a) :: Shared([Pending, Done(a), Dropped])
Reply.new! : () => Reply(a)
Reply.answer! : Reply(a), (() => a) => {}                           # Dropped if it fails
Reply.wait! : Reply(a) => [Done(a), Dropped]

Semaphore :: Shared(U64)
Semaphore.new! : U64 => Semaphore
Semaphore.with! : Semaphore, (() => a) => a                         # waits for a permit
Semaphore.try_with! : Semaphore, (() => a) => [Ran(a), Busy]        # sheds load instead

Pool(r) :: Shared({ free : List(r), lost : U64 })
Pool.new! : List(r) => Pool(r)
Pool.with! : Pool(r), (() => r), (r => a) => a                      # opens a replacement for a lost one

Latch :: Shared(U64)
Latch.new! : U64 => Latch
Latch.counting_down! : Latch, (() => a) => a                        # counts down however it ends
Latch.wait! : Latch => {}

Barrier :: { parties : U64, cell : Shared({ arrived : U64, round : U64 }) }
Barrier.new! : U64 => Barrier
Barrier.arrive_and_wait! : Barrier => {}

Watch(a) :: Shared({ version : U64, value : a })
Watch.new! : a => Watch(a)
Watch.publish! : Watch(a), a => {}
Watch.changed! : Watch(a), U64 => { version : U64, value : a }      # waits for a newer version

RwLock :: Shared({ readers : U64, writing : Bool, writers_waiting : U64 })
RwLock.new! : () => RwLock
RwLock.read! : RwLock, (() => a) => a
RwLock.write! : RwLock, (() => a) => a                              # writers go first

Memo(k, v) :: Shared(Dict(k, [Loading, Ready(v)]))
Memo.new! : () => Memo(k, v)
Memo.get! : Memo(k, v), k, (k => v) => v                            # loads each key once

Breaker :: Shared([Closed(U64), Open(Instant), Probing])
Breaker.guarded! : Breaker, Instant, (() => Try(a, [CircuitOpen, ..e])) => Try(a, [CircuitOpen, ..e])

Bucket :: Shared({ tokens : U64, next_refill : Instant })
Bucket.take! : Bucket => {}                                         # sleeps until a token is due
```

What each does when a task using it is cancelled or crashes:

| Type | While the task is… | What happens |
|---|---|---|
| `Queue` | Waiting in `push!` or `pop!` | Nothing: a `Wait` takes nothing |
| `Queue` | Inside `closing!` | The queue closes, so readers finish and writers stop |
| `Reply` | Inside `answer!` | The reply becomes `Dropped`, and the caller stops waiting |
| `Semaphore` | Holding a permit | The permit comes back |
| `Pool` | Holding or opening a resource | The slot is counted as lost, and the next borrower opens a new one |
| `Latch` | Inside `counting_down!` | The count goes down anyway |
| `Barrier` | Waiting for the others | Its arrival is withdrawn, so the barrier doesn't open early |
| `Watch` | Anywhere | Nothing: readers hold nothing |
| `RwLock` | Reading or writing | The lock is released |
| `RwLock` | Waiting to write | Its claim is withdrawn, so readers aren't blocked by it |
| `Memo` | Loading a key | The key is unmarked, and one of the tasks waiting for it loads it instead |
| `Breaker` | Probing | The breaker reopens instead of staying `Probing` |
| `Bucket` | Sleeping for a token | Nothing: it took no token |

The full code of `Queue` and `Reply` is in the shared-state RFC's
["The library"](../concurrency-shared-variables.md#the-library), and most of
the others are in [the examples](examples.md).

## For Platforms

```roc
platform ""
    ...
    concurrency: Tasks        # or ParallelTasks; leave it out for neither
```

```roc
Http := [].{
    suspending get_utf8! : Str => Try(Str, [HttpErr(U16)])
}
```

A `suspending` hosted function can pause its task. One that isn't marked
always finishes before it returns. [Platforms](05-platforms.md) covers what
the host implements.
