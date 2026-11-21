# RFC: Shared State

| | |
|---|---|
| Status | Draft. Not adopted; nothing here is part of `design.md`. |
| Scope | The builtin `Shared(s)` type and its functions (`new!`, `update!`, `finally!`, `on_failure!`), atomic updates, waiting, cleanup on cancellation and crash, poisoning, releasing cells, deadlock detection, their runtime representation and lowering, the library built on them (queues that stream between tasks, replies, semaphores, pools and the rest), and the changes they make to the companion RFC |
| Companion | [Suspendable Effects and Platform-Scheduled Concurrency](concurrency-explicit-suspension.md): the task tree, plans, concurrent loops and crashes this RFC builds on |
| Supersedes | [Sessions, Channels and Shared Resources](concurrency-sessions.md): queues replace its channels and mailboxes, `Shared` replaces its resource libraries, and this RFC restates the holder sets and deadlock detection it keeps |
| Prototype | [`concurrency-prototype/`](concurrency-prototype/): `shared.py`, `shared_model.py`, the holder sets in `sessions.py`, the libraries in `library.py`, and the `SharedVariables`, `SharedConveniences`, `SharedModel` and `CellRelease` tests; `cycles.py` and `NoCycles` measure the rejected type rules. See "What the prototype shows" |

## Summary

The companion RFC's tasks share only immutable values. That covers fan-out,
races and supervision, but not a resource that many tasks use while they run:
a semaphore, a rate limiter, a pool, a circuit breaker shared across requests.
The sessions RFC, which this one supersedes, built each of those as a server
task that other tasks reach through a mailbox, and streamed values between
tasks over one-shot channels. This RFC instead adds one builtin opaque type,
four functions and two shorthands, and no syntax:

```roc
Shared.new! : s => Shared(s)
Shared.update! : Shared(s), (s -> [Set(s, r), Keep(r), Wait]) => r
Shared.finally! : Shared(s), (s -> s), (() => a) => a
Shared.on_failure! : Shared(s), (s -> s), (() => a) => a

Shared.get! : Shared(s) => s                   # update!(|s| Keep(s))
Shared.modify! : Shared(s), (s -> s) => {}     # update!(|s| Set(g(s), {}))
```

1. **A handle.** `Shared.new!(4)` creates shared state and returns a handle to
   it, a value of type `Shared(U64)`. The handle can be captured, passed and
   stored like any value. Like a TCP stream or a database connection in a
   platform's API, it names state that lives outside Roc's values, and only
   effectful functions can use it.
2. **Pure updates.** `permits.update!(f)` runs the pure function `f` on the
   current state, atomically. `f` answers with a tag: `Set(new, r)` replaces
   the state and returns `r`, `Keep(r)` leaves it and returns `r`, and `Wait`
   abandons the update until another task changes the state, then runs `f`
   again. The `->` in `f`'s type is the atomicity boundary: a pure function
   cannot suspend or perform I/O, so nothing can happen in the middle of it.
   `get!` and `modify!` are `update!` with a fixed answer, for reading and for
   plain replacement.
3. **Cleanup.** `permits.finally!(cleanup, body)` runs `body`, then applies
   `cleanup` to the state however `body` ends: by returning, by cancellation or
   by a crash. `on_failure!` applies it only on cancellation or a crash.
   Cleanups are pure too, so a cancelled task's drop can run them.
4. **A library.** Queues, replies, semaphores, pools, latches, barriers,
   watched values, readers–writer locks, single-flight caches, circuit
   breakers and rate limiters are library types written with these
   functions. A queue has a `stream` method, so `for! item in queue` reads
   from it, which is how tasks stream values to each other.

An update has the state to itself. `update!` moves the state out of the
handle into `f`, and `Keep` and `Wait` put the same state back, so a state
that nothing else holds changes in place, as a unique value does anywhere in
Roc. A queue in a handle costs what a list costs.

Crashes keep OTP's semantics. An OTP server that crashes loses its state, and
an update or a cleanup that crashes poisons its handle: every later use of it
crashes, and restarting the scope that created it makes a new one. Shared state
is never seen half-updated, and a broken update or cleanup obligation becomes a
crash at its next use instead of a silent leak.

A state can hold any value, functions and its own handle included.
Reference counting frees a cell when its last handle is gone. Cells that hold
each other keep their counts above zero, so the runtime also records which
tasks hold each handle. When none of them is still running, it drops the
cell's state, which breaks the cycle.

All names (`Shared`, `update!`, `Set`, `Keep`, `Wait`, `finally!`,
`on_failure!`, `get!`, `modify!`) are placeholders.

## Motivation

### What the task tree cannot express

A child task's only output is its result, and a parent can gather results in
an ordinary `var` in its loop body. What the tree cannot express is state that
several running tasks use directly:

- **Limits shared across tasks:** a cap on database connections across all of
  a request's tasks, or across requests.
- **Rate limiters and circuit breakers** consulted by every outgoing call.
- **Pools** that lend resources and take them back.
- **Caches, counters and progress** read by one task while others update them.

### Why not tasks that own the state

The sessions RFC answered this with actors: a semaphore is a task that owns the
count, serves requests from a mailbox and hands out permits over channels. The
prototype showed that this works, and also what it costs:

- **A server task per resource**, kept alive by running the program beside it
  in `both!` (`Semaphore.with!`), plus a mailbox and a reply channel for every
  request. Each acquire is two messages and two task switches.
- **Rules the type system cannot check.** Ends are used once, the next end is
  threaded through each result (`Got(msg, rest)`), and every receive has
  `Closed` and `Cancelled` arms. Roc has no linear types, so misuse is a
  run-time crash.
- **Code shaped as recursion.** Because the end changes on every message, a
  server is a tail-recursive function, not a loop.

Erlang/OTP's let-it-crash does not require actors. It requires that a crash
stay isolated, and that a restart begin from known-good state. The companion
RFC's crash links and `supervise!` already give the first. Pure atomic updates
give the second more strongly than OTP does: a crash can never leave shared
state half-written, so a restart never has to rebuild it.

### Why not channels for streams

The sessions RFC streamed values between tasks over one-shot channels: each
message carried the end the next one would arrive on. That has the same costs
as its actors. Each end is used once, checked only at run time, a consumer
threads the next end through every receive, and a stream has one reader. A
sender never waits, so a fast producer fills memory unless the protocol adds
replies for credit.

A bounded queue in a handle has none of these. Any number of tasks write to it
and read from it, a full queue makes writers wait, and `for!` reads it like
any other stream. It is a library type over `Shared`, so streams need no
mechanism of their own.

### Why not shared variables

An earlier draft of this RFC added shared variables: `var $$permits = 4`, a
name that nested functions and other tasks could assign, an `&$$permits` that
passed the variable itself, and `wait` and `defer` keywords. Every access was
marked, but the result was references in Roc: a new kind of name whose plain
reads see other tasks' writes, an operator that aliases storage, and a
statement-level rule for which code ran atomically. Roc has none of these, and
this RFC keeps it that way.

A handle is not a reference in that sense:

- **No expression reads it.** There is no way to write "the current value of
  `permits`" in an expression. The state appears only as the argument of a
  pure function that the runtime calls during an atomic update.
- **Nothing writes through it.** The new state is the pure function's result,
  as `Set(new, r)`. No assignment, place or alias is involved.
- **Pure code can hold it but not use it.** `Shared(s)` has no pure functions,
  no equality and no hashing, so a pure function given a handle can only pass
  it along. `->` keeps its meaning.
- **The boundary is the one Roc already has.** `!` marks where state can
  change between two calls; `->` marks where it cannot. No new keyword or
  statement rule decides what is atomic.

### Requirements

1. Every use of shared state is visible in the source: a call to a `Shared`
   function ending in `!`.
2. Pure functions never see shared state, so `->` keeps its meaning.
3. No data races and no torn updates. Every change is an atomic update.
4. No lock-order deadlocks, and no lock held across a suspension, by
   construction.
5. Cleanup that runs on cancellation, without letting a cancelled task perform
   effects or suspend.
6. A crash never leaves shared state half-written, and a crash in an update
   or a broken cleanup obligation is loud.
7. No new syntax, no new kind of name and no references: one builtin opaque
   type and its functions. No new type rules, no effect polymorphism, no
   non-escaping types, no implicit conversions.
8. Every cell is freed, including cells in a reference cycle, which Roc's
   reference counting alone cannot free. No type rule limits what a state
   holds.
9. The same meaning under `None`, `Tasks` and `ParallelTasks`, up to
   scheduling.
10. Changing a state costs what changing a value of its type costs: a state
    that only the update holds changes in place.

## Guide-level explanation

### Most state is not shared

A concurrent loop's body runs in the parent task, one arrival at a time. State
that the parent builds from its children's results is an ordinary `var`:

```roc
write_quorum! = |replicas, data| {
    var $acks = 0
    for! result in replicas.map(|r| || Db.write!(r, data)).concurrently() {
        if result.is_ok() { $acks = $acks + 1 }
        if $acks == 2 { return Ok({}) }
    }
    Err(NoQuorum)
}
```

`Shared` is for state that the children themselves use while they run. The
compiler warns when a handle never reaches another task (see "What the
compiler warns about").

### Creating and updating shared state

`Shared.new!` creates shared state. `update!` changes it with a pure function
that answers with a tag:

```roc
download_all! = |urls| {
    done = Shared.new!(0)
    total = urls.len()
    downloads = urls.map(|url| || {
        file = Http.get_bytes!(url)
        done.modify!(|n| n + 1)
        Downloaded(file)
    })
    reporter = || {
        var $seen = 0
        while $seen < total {
            $seen = done.update!(|n| if n == $seen { Wait } else { Keep(n) })
            Stdout.line!("${$seen.to_str()} of ${total.to_str()}")
        }
        Reported
    }

    var $files = []
    for! arrival in downloads.append(reporter).concurrently() {
        match arrival {
            Downloaded(file) => { $files = $files.append(file) }
            Reported => {}
        }
    }
    $files
}
```

The function passed to `update!` gets the current state and returns one of
three tags:

| Tag | The state | `update!` returns | Waiting tasks |
|---|---|---|---|
| `Set(new, r)` | Becomes `new` | `r` | Woken |
| `Keep(r)` | Unchanged | `r` | Not woken |
| `Wait` | Unchanged | Nothing yet: the update runs again after the state next changes | Not woken |

The reporter above waits for the count to move instead of polling it. Its
function captures `$seen` as nested functions capture any `var`: as the value
it had when the function was defined, which is a new one on each iteration.

Two shorthands cover the commonest updates. `done.get!()` reads the state: it
is `done.update!(|n| Keep(n))`. `done.modify!(|n| n + 1)` replaces it: it is
`done.update!(|n| Set(n + 1, {}))`, as in the downloads above. What `get!`
returns may be out of date by the next line. To act on the state, decide
inside an update.

### Atomic updates

**The function passed to `update!` runs atomically.** While it runs, no other
task changes the state, and its result takes effect at once when it returns.
That needs no keyword: the function is pure, so it cannot call `!` functions,
suspend or perform I/O.

Because the answer is a tag, an update can change the state and make a
decision together, with `match`:

```roc
decision = breaker.update!(|state| match state {
    Open(until) if now < until => Keep(Reject)
    Open(_) => Set(Probing, Probe)
    Probing => Keep(Reject)
    Closed(_) => Keep(Run)
})
```

An update uses exactly one handle. State that must change together belongs in
one handle holding a record:

```roc
moved = accounts.update!(|a|
    if a.checking >= amount {
        Set({ checking: a.checking - amount, savings: a.savings + amount }, Moved)
    } else {
        Keep(Short)
    })
```

### Waiting

`Wait` abandons the update, with the state unchanged, and runs the function
again once another task's update returns `Set`. A semaphore takes a permit
like this:

```roc
permits.update!(|n| if n == 0 { Wait } else { Set(n - 1, {}) })
```

When a permit is given back, the waiting tasks are not all woken to compete
for it. The functions are pure, so the runtime can ask each one, oldest
waiter first, what it would do with the new state, without storing anything.
It wakes only the first that would not wait, which then runs its function for
real. Ten tasks waiting for one permit cost one wakeup per
release, not ten.

When the function is a `match`, the case that waits is written out, so a
forgotten case is a compile error, not a hang.

### Large states change in place

`update!` moves the state into its function. The handle holds nothing while
the function runs, so unless a task still holds the state from a `get!` or an
earlier answer, the function holds the only reference, and the state changes
in place as any unique value does. `Queue.push!` (see "The library") appends
to a list in a handle at the cost of appending to a list:

```roc
push! = |Queue.(cell), item| cell.update!(|q|
    if q.closed {
        Keep(Closed)
    } else if q.items.len() == q.capacity {
        Wait
    } else {
        Set({ ..q, items: q.items.append(item) }, Pushed)
    })
```

`Keep` and `Wait` put back the state the function was given, so a branch that
answers one of them still needs the old state. The rule is the one for any
Roc value: decide first, then build the new state in the branch that answers
`Set`. A new state built before the decision, or in another function that
returns the tag, is built while the old state may still be needed, so it is a
copy.

### Cleanup that always runs

`permits.finally!(cleanup, body)` calls `body` and returns its result. However
`body` ends, `cleanup` is then applied to the state as an update that sets it:
when `body` returns, when the task is cancelled while in `body`, and when
`body` crashes. A semaphore needs nothing else:

```roc
with_permit! : Shared(U64), (() => a) => a
with_permit! = |permits, f| {
    permits.update!(|n| if n == 0 { Wait } else { Set(n - 1, {}) })
    permits.finally!(|n| n + 1, f)
}
```

- `cleanup` has type `s -> s`. It cannot wait, because its type has no `Wait`.
- `body` is a function, so `return` and `?` inside it leave `body`, and
  `cleanup` runs after them. There is no question of which block a cleanup
  belongs to.
- When several are pending in one task, they run innermost first.

A cancelled task runs no more of its own code, because that code could
perform effects or suspend. A cleanup can do neither: it is a pure function on
one handle's state. So the runtime can run it while it drops the task.

### Cleanup on failure only

`on_failure!` is `finally!` with a cleanup that runs only when `body` is
cancelled or crashes. A pool needs it: a connection whose user crashed is in
an unknown state, so the pool should count it as lost and open a new one,
rather than lend it again.

```roc
with_conn! = |pool, f| {
    lease = pool.update!(|p| match p.free {
        [c, .. as rest] => Set({ ..p, free: rest }, Lent(c))
        [] if p.lost > 0 => Set({ ..p, lost: p.lost - 1 }, Open)
        [] => Wait
    })
    pool.on_failure!(|p| { ..p, lost: p.lost + 1 }, || {
        conn = match lease {
            Lent(c) => c
            Open => Db.connect!()
        }
        result = f(conn)
        pool.update!(|p| Set({ ..p, free: p.free.append(conn) }, {}))
        result
    })
}
```

The `on_failure!` begins right after the update that took the slot, and the
connection is opened inside it, so a borrower cancelled while connecting gives
the slot back as lost.

### Passing and storing handles

A handle is an ordinary value. Functions take it as a parameter, closures
capture it, and records hold it:

```roc
Server : { hits : Shared(U64), breaker : Shared(Breaker) }

main! = |{}| {
    server = { hits: Shared.new!(0), breaker: Shared.new!(Closed(0)) }
    serve!(server)
}

handle! = |server, req| {
    server.hits.modify!(|n| n + 1)
    guarded!(server.breaker, Clock.now!(), || route!(req))
}
```

Two handles from two calls to `Shared.new!` are different state, even if they
start equal. Copying a handle copies the handle, not the state.

### What a state can hold

Anything: numbers, records, lists, functions, and handles, the state's own
handle included. A crawler keeps its pending jobs in shared state, and each
job captures the queue so that it can add the links it finds:

```roc
crawl! = |queue, url| {
    links = Http.get!(url).links
    queue.modify!(|jobs| jobs.concat(links.map(|link| || crawl!(queue, link))))
}
```

A pending job and its queue hold each other: a reference cycle, which
reference counting alone cannot free. The cycle breaks in normal use, when a
worker takes the job out, but a program can also abandon the queue with jobs
still in it. So a cell is freed in one of two ways:

- **When its last handle is gone**, like any value. This covers every state
  that does not reach its own handle.
- **When no running task can use it any more.** The runtime knows which tasks
  hold each handle. When the last of them finishes, it drops the cell's state,
  and whatever the state held, the cycle included, is freed.

So a cycle that a program abandons is freed at the latest when the tasks that
used it have finished. Work done in its own task, such as one request, is
freed when that task ends. A cycle abandoned inside a task that runs for the
whole program, such as a server's loop in `main!`, stays until that task
ends. A long-running loop that keeps making cycles of cells and dropping them
should clear their states first, or run each iteration in a task.

### Crashes

- **An update or a cleanup that crashes poisons its handle.** The function
  had the state to itself, so the state goes with the crash, and the task
  crashes at the call. Every later update of a poisoned handle crashes with
  "this shared state was poisoned when an update crashed at …" (or a
  cleanup), and so does every task waiting on it. No task ever sees a
  half-made state. No code can test for poison or clear it.
- **Decide inside, crash outside.** An update that finds the state wrong
  answers with a tag that says so, and its caller crashes after the update, so
  that other tasks keep the state. `Reply.answer!` below does this.
- **Where the handle is created decides what a restart does.** A handle
  created inside a supervised function is new on every restart:

```roc
Concurrent.supervise!({ max_restarts: 3 }, || {
    permits = Shared.new!(4)
    run_workers!(permits)
})
```

A poisoned handle created outside the supervised function stays poisoned, so
the restarts crash again until the limit is reached and the crash escalates.
That is correct: the broken state lies outside the subtree being restarted.

### What the compiler warns about

- **State that is never shared:** "`seen` is never captured, passed or stored,
  so no other task can use it; use `var $seen`". It fires when the result of
  `Shared.new!` is bound to a name that appears only as the first argument of
  `Shared` calls in the same function body.
- **Reading in one update and writing in another:** an update function that
  ignores its argument and returns `Set` of a value computed from an earlier
  read of the same handle, as in `n = c.get!()` followed by
  `c.modify!(|_| n - 1)`. That is a lost update. The warning suggests deciding
  inside one update.
- **A suspension point between an update and its cleanup:** a call that can
  suspend between an `update!` and the `finally!` or `on_failure!` on the same
  handle that follows it in the same body. A cancellation there skips the
  cleanup and leaks whatever the update took.

### On platforms that do not schedule tasks

A `None` build runs its own tasks on one thread (companion RFC, "On
platforms that do not schedule tasks"). A `Wait` pauses the task, as on a task
platform, and the next ready task runs. Waking follows the same rule
everywhere. If no task can run, nothing can ever change the state, and the
task crashes with "deadlock: no task can ever change the shared state this
task waits for".

### Streams between tasks

A queue in a handle is how tasks stream values to each other. `Queue` is a
library type over `Shared` (see "The library"), and its `stream` method makes it
something `for!` can loop over:

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

- **Backpressure.** `push!` waits while the queue is full, and the loop waits
  while it is empty.
- **The end of the stream.** `closing!` runs its function and closes the queue
  however the function ends: by returning, by cancellation or by a crash. A
  loop over a closed queue takes what is left in it, then ends.
- **Many readers.** Any number of tasks can loop over one queue. Each item goes
  to exactly one of them, so a worker pool is several loops over the same
  queue.
- **A reader that stops early.** A reader that may `break` or `return` runs in
  `closing!` too. Closing the queue makes the producer's next `push!` answer
  `Closed`, so it stops instead of waiting for room that will never come.

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

If `handle!` crashes or is cancelled, `answer!` marks the reply `Dropped`, so
the caller stops waiting. A service whose transitions are pure needs neither:
it is an update.

### The library

These types are libraries over `Shared`. Each is written with the functions
above, and needs nothing else from the runtime. `Queue` and `Reply` are given
in full; the others are in [the examples](concurrency/examples.md) or below.

```roc
Queue(a) :: Shared({ items : List(a), capacity : U64, closed : Bool }).{
    new! : U64 => Queue(a)
    new! = |capacity| Queue.(Shared.new!({ items: [], capacity, closed: False }))

    push! : Queue(a), a => [Pushed, Closed]
    push! = |Queue.(cell), item| cell.update!(|q|
        if q.closed {
            Keep(Closed)
        } else if q.items.len() == q.capacity {
            Wait
        } else {
            Set({ ..q, items: q.items.append(item) }, Pushed)
        })

    try_push! : Queue(a), a => [Pushed, Full, Closed]
    try_push! = |Queue.(cell), item| cell.update!(|q|
        if q.closed {
            Keep(Closed)
        } else if q.items.len() == q.capacity {
            Keep(Full)
        } else {
            Set({ ..q, items: q.items.append(item) }, Pushed)
        })

    pop! : Queue(a) => [Item(a), Closed]
    pop! = |Queue.(cell)| cell.update!(|q| match q.items {
        [item, .. as rest] => Set({ ..q, items: rest }, Item(item))
        [] if q.closed => Keep(Closed)
        [] => Wait
    })

    close! : Queue(a) => {}
    close! = |Queue.(cell)| cell.modify!(|q| { ..q, closed: True })

    closing! : Queue(a), (() => r) => r
    closing! = |Queue.(cell), f| cell.finally!(|q| { ..q, closed: True }, f)

    stream : Queue(a) -> Stream(a)
    stream = |queue| Stream.custom(queue, Err(Unknown), |q| match q.pop!() {
        Item(item) => Ok((item, q))
        Closed => Err(NoMore)
    })
}

Reply(a) :: Shared([Pending, Done(a), Dropped]).{
    new! : () => Reply(a)
    new! = || Reply.(Shared.new!(Pending))

    answer! : Reply(a), (() => a) => {}
    answer! = |Reply.(cell), f| {
        answered = cell.on_failure!(|r| match r { Pending => Dropped, _ => r }, || {
            value = f()
            cell.update!(|r| match r {
                Pending => Set(Done(value), Answered)
                _ => Keep(AlreadyAnswered)
            })
        })
        match answered {
            Answered => {}
            AlreadyAnswered => crash "this reply was already answered"
        }
    }

    wait! : Reply(a) => [Done(a), Dropped]
    wait! = |Reply.(cell)| cell.update!(|r| match r {
        Pending => Wait
        Done(value) => Keep(Done(value))
        Dropped => Keep(Dropped)
    })
}
```

The rest, by signature:

```roc
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
| `Barrier` | Waiting for the others | Its arrival is withdrawn, so the barrier does not open early |
| `Watch` | Anywhere | Nothing: readers hold nothing |
| `RwLock` | Reading or writing | The lock is released |
| `RwLock` | Waiting to write | Its claim is withdrawn, so readers are not blocked by it |
| `Memo` | Loading a key | The key is unmarked, and one of the tasks waiting for it loads it instead |
| `Breaker` | Probing | The breaker reopens instead of staying `Probing` |
| `Bucket` | Sleeping for a token | Nothing: it took no token |

The implementations of two of them follow.

**Circuit breaker.** The state machine is a tag union, and each transition is
a `match` inside an update:

```roc
Breaker : [Closed(U64), Open(Instant), Probing]

guarded! = |breaker, now, f| {
    decision = breaker.update!(|state| match state {
        Open(until) if now < until => Keep(Reject)
        Open(_) => Set(Probing, Probe)
        Probing => Keep(Reject)
        Closed(_) => Keep(Run)
    })
    match decision {
        Reject => Err(CircuitOpen)
        Run => call_and_record!(breaker, now, f)
        Probe => {
            # If the probe is cancelled or crashes, reopen instead of
            # staying Probing forever.
            breaker.finally!(
                |state| match state {
                    Probing => Open(now.add_seconds(30))
                    other => other
                },
                || call_and_record!(breaker, now, f),
            )
        }
    }
}

call_and_record! = |breaker, now, f| {
    result = f()
    breaker.update!(|state| match (state, result) {
        (_, Ok(_)) => Set(Closed(0), {})
        (Closed(n), Err(_)) if n < 4 => Set(Closed(n + 1), {})
        _ => Set(Open(now.add_seconds(30)), {})
    })
    result
}
```

**Rate limiter.** A token bucket refills with time, and an update cannot read
the clock, so the caller passes the time in. Waiting for time to pass is a
sleep, not a `Wait`, because no task's update ends it:

```roc
take_token! = |bucket| {
    now = Clock.now!()
    outcome = bucket.update!(|b| {
        refilled = b.refilled(now)
        if refilled.tokens > 0 {
            Set({ ..refilled, tokens: refilled.tokens - 1 }, Took)
        } else {
            Set(refilled, RetryAt(refilled.next_refill))
        }
    })
    match outcome {
        Took => {}
        RetryAt(t) => {
            Sleep.until!(t)
            take_token!(bucket)
        }
    }
}
```

`Semaphore.with!` is `with_permit!` above, and `Pool.with!` is `with_conn!`.
`Latch.counting_down!(latch, f)` is `cell.finally!(|n| n - 1, f)`.
`Latch`, `Barrier`, `Watch` and `RwLock` are in [the
examples](concurrency/examples.md), section 2, and `Memo.get!` is `cached!` in
section 6.

## Reference-level explanation

### Typing

```roc
Shared.new! : s => Shared(s)
Shared.update! : Shared(s), (s -> [Set(s, r), Keep(r), Wait]) => r
Shared.finally! : Shared(s), (s -> s), (() => a) => a
Shared.on_failure! : Shared(s), (s -> s), (() => a) => a
Shared.get! : Shared(s) => s
Shared.modify! : Shared(s), (s -> s) => {}
```

- `Shared(s)` is a builtin opaque type. It has no other functions, no equality
  and no hashing. `Inspect` shows only `<Shared>`.
- Every function on it is effectful. `new!` is an effect because two calls
  create two distinct states. Binding, holding, passing or storing a handle is
  not an effect.
- The update and cleanup functions are pure. That is the whole of the
  atomicity rule, and the type checker already enforces it.
- `get!` and `modify!` are defined with `update!` and add no semantics.
- **No rule on what a state holds.** `s` can be any type, functions and
  handles included. The runtime frees cells that form reference cycles; see
  "Releasing cells".
- Hosted function signatures cannot mention `Shared` in the first version.

### `update!`

`h.update!(f)` runs as if no other task ran during it:

1. Acquire `h` (see "Runtime").
2. If `h` is poisoned, release it and crash.
3. Move the state out of `h` and call the update form of `f` on it (see
   "Lowering"). `h` holds nothing while `f` runs, so `f` holds the state's
   reference, and where it is the only one, `f` changes the state in place.
   `f` cannot be told apart from a call on an ordinary value.
4. On `Set(new, r)`: store `new`, wake waiters as "Waking" under "Runtime"
   describes, release `h`, and return `r`.
5. On `Keep(r)`: put back the state `f` was given, release `h` and return `r`.
6. On `Wait`: put back the state `f` was given, register the task as a waiter
   on `h` while still holding it, release `h` and park. When another task's
   update stores to `h`, or `h` is poisoned, the waiter may be readied (see
   "Waking" under "Runtime"). A readied waiter starts again at step 1.

If `f` crashes, `h` stays empty and is poisoned, its waiters are readied so
that they crash, `h` is released as part of the crash, and the task crashes at
the call. The state `f` held is in the crashed frames, which the companion
RFC's R5 leaks.

`update!` is a suspension point and adds a wait source: the handle the task
waits on. Cancelling a task parked in `Wait` removes it from the waiter list
and drops it as usual. It has taken nothing.

### `finally!` and `on_failure!`

- **Entry.** The call pushes an entry holding the handle and the cleanup
  closure onto the task's cleanup stack, then calls `body`. The entry is task
  state, owned like a record, so the companion RFC's C5 ownership
  conservation covers it.
- **Return.** When `body` returns, the call pops its entry. `finally!` then
  applies the cleanup as an update that sets the state; `on_failure!` drops
  the entry unrun. Either way, it returns `body`'s result.
- **Cancellation.** The task's drop runs every pending entry, innermost first,
  after its descendants have been dropped (R3).
- **Crash.** After the non-local exit, the runtime cancels and drops the
  task's children, then runs the task's pending entries, innermost first. The
  closures were built before `body` was called, so they do not depend on the
  native frames whose state the crash left unknown.
- **A cleanup** gets the state moved in, as an update does, and its result
  is stored. **A cleanup that crashes** stores nothing and poisons its handle. The
  remaining entries still run. If the task was being cancelled, it now
  crashes, and crash links take over. If it was already crashing, the first
  crash propagates, with the cleanup's message attached.

A cleanup can never wait: `s -> s` has no `Wait`. An update's pending `Wait`
holds no entry, so a task parked in `Wait` has nothing of the update to undo.

### Releasing cells

Immutable values cannot form a cycle, so every reference cycle passes through
at least one cell. The runtime frees cycles by releasing cells. For each cell
it records a set of **holders**, the tasks that may hold a reference to it, and
acts when none is left.

- `Shared.new!` makes a cell held by the current task.
- When a value crosses to another task, every cell reachable from it gains that
  task as a holder. A value crosses when an entry starts, for the values its
  closure captured; when a child's result arrives at its parent; and when the
  host takes a hosted call's arguments, for the host.
- If the value reaches the cell only through objects whose count is one, it
  was the only way to reach the cell, and the receiving task becomes its only
  holder. Otherwise the sender may keep a copy, and it stays a holder.
- **A store passes holders on.** When an update stores a new state, every cell
  reachable from the state gains the stored cell's holders. The walk continues
  into the states of the cells it reaches. So a task that can reach a cell
  through other cells' states is one of that cell's holders.
- A task leaves every holder set when it finishes.

One holder per cell would not be enough. `supervise!` shares its closure, and
the handles it captured, between the supervisor and each attempt. If an attempt
crashes with a handle in a running frame, a restarted attempt may still use
it, so the cell cannot be released when the first attempt ends. Holder sets
keep it until the supervisor itself finishes.

When a live cell has no holders left, no running task can reach it, and no
task started later can either, since only a holder can pass a handle on. The
runtime then **releases** the cell: it drops the cell's state and leaves the
cell empty. Dropping the state releases what the state held. If that led back
to the cell, the cycle unwinds and reference counting frees the rest. A
released cell is never updated again (V9).

Holder sets over-approximate: a task stays a holder until it finishes, even
after it drops its handle. So release is not the usual way cells are freed. A
cell whose state does not lead back to it is freed by its count when its last
handle goes, as any value is. Release matters for cycles, and for cells left
in a crashed task's frames, which the companion RFC's R5 leaks: their holders
finish, and the cells are released.

- **`None` builds** track no holders. The runtime releases every cell still
  live when the program ends.
- **The walk** is generated from the stored value's type, like the per-type
  helpers the compiler already generates, and visits nothing for a type that
  cannot contain a handle. Deadlock detection needs the same holder sets
  ("Deadlocks and liveness"), so release adds no tracking, only the drop.
- **`ParallelTasks`.** A cell's lock also guards its holder set, so a task
  finishing on one thread and an update storing on another do not race.
- **Release is a runtime action.** The runtime drops the state with the
  type's generated drop helper. Backends still follow only the explicit
  `incref` and `decref` statements in LIR.

### Poisoning

Each handle's state carries a poisoned flag. An update or a cleanup that
crashes sets it. Any later update of a poisoned handle crashes with a message
naming the source location of the function that crashed. Waiters on a poisoned handle are readied, so that
they crash too.

### Runtime

A handle points to a reference-counted cell holding the state, a poisoned
flag, a list of waiters, in `Tasks` and `ParallelTasks` builds a holder set
(see "Releasing cells"), and in `ParallelTasks` builds a lock word.

| Build | Acquiring a cell | Waiters |
|---|---|---|
| `None` | Nothing to do: one thread runs every task | A list per cell, as in `Tasks` builds |
| `Tasks` | Nothing to do: the function is pure and cannot suspend, and resumes are serial (P10), so no other task can run during it | A list per cell |
| `ParallelTasks` | Lock the cell | A list per cell, guarded by its lock |

**Waking.** Waiters are kept in the order they began waiting. After a `Set`,
while still holding the cell, the runtime probes them, oldest first: it
calls the probe form of each waiter's function on the new state, which the
cell keeps, and stores nothing. The probe form decides the tag and builds no
state and no answer (see "Lowering"), so a probe never copies the state. It readies the first waiter whose function would not answer
`Wait`, and stops. Oldest first is fair: a waiter that could go ahead is
never passed over for one that began waiting later. A readied waiter that
waits again keeps its place. The readied waiter is marked as carrying the
wake:

- If it stores, its own `Set` probes the remaining waiters.
- If it ends without storing, because it answers `Keep`, answers `Wait`
  again (another task changed the state first), crashes, or is cancelled
  before it runs, the runtime probes on from there, so that the wake is never
  lost with it.

Probing is exact: a pure function given the same state and captures gives
the same answer, and the probe form answers `Wait` exactly when the function
would. It commits nothing on a waiter's behalf, so a waiter
cancelled after it was readied has taken nothing. A probe that crashes counts
as not waiting: the waiter is readied and crashes when it runs. Each probe
runs inside a crash boundary in the storing task, and in `ParallelTasks`
builds while the cell's lock is held.

Under contention, this keeps task dispatches linear in the number of
waiters. Readying every waiter instead dispatches each of them after every
`Set`, and all but one answer `Wait` again (see "What the prototype shows").
Poisoning still readies every waiter, so that each crashes.

An update holds one lock and runs only pure code while holding it. A pure
function cannot call `update!`, so locks never nest, and lock-order deadlock
is impossible without any ordering. A task waiting for a lock is not at a
suspension point: it spins briefly and then yields its thread, and it cannot
be cancelled there.

### Lowering

`Shared` functions are builtins, and their structure is explicit before LIR,
so later stages consume it and never reconstruct it:

- `Shared.new!` lowers to a call that allocates a cell.
- `update!` lowers to an acquire, a move of the state out of the cell, a
  call of the function's update form, a `match` on its tag with a store,
  release and wake on `Set`, a put-back and release on `Keep`, and a
  put-back, register, release and suspend followed by a jump back to the
  acquire on `Wait`. In `None` and `Tasks` builds, acquire and release are
  omitted; the `concurrency` field is already an explicit input to every
  unit's compilation.
- `finally!` and `on_failure!` lower to a push, the call of `body`, and a pop
  followed by the cleanup update (`finally!`) or by dropping the entry
  (`on_failure!`). Push and pop are balanced by construction, and certified.
- `get!` and `modify!` are library functions that call `update!`.
- **The update form.** Each function literal written as `update!`'s argument
  is lowered twice. In the update form, each tail position of its body,
  followed through `if`, `match` and blocks, is rewritten: a tail that builds
  `Keep(r)` or `Wait` also returns the parameter, the state the function was
  given; a tail that builds `Set(new, r)` is unchanged; and any other tail
  expression `e`, such as a call that returns the tag, becomes a `match` on
  `e` that does both. Reference counting then runs as usual. On a path that
  ends in `Set`, the state is not used after the new one is built, so a state
  with a count of one changes in place. On a path that ends in `Keep` or
  `Wait`, the state is still used and is kept. A tail that is a call keeps the
  state alive across the call, so a `Set` built inside it copies. A function
  passed to `update!` as a value is called from such a tail. The rule reads
  only the literal's own body, so it is the same in `.lss` and `.boxy` builds.
- **The probe form.** In the probe form, a tail that builds `Set(new, r)` or
  `Keep(r)` evaluates neither `new` nor `r` and answers that the waiter would
  go ahead; a `Wait` tail answers that it would wait; any other tail calls
  `e` and inspects its tag. A crash in `new` or `r` would also have readied
  the waiter ("Waking"), so skipping them changes no outcome.
- **Cleanups** are `s -> s`, so they need no rewrite: the state is moved in
  and the result stored.

Backends see calls to builtins and ordinary `incref` and `decref` statements.
They need no new knowledge of reference counting.

### Reference counting

In `ParallelTasks` builds, a cell and every value stored in it can be reached
by tasks on several threads. Storing a value into a cell is an atomicity seed,
like a hosted argument in the companion RFC's C6, and cells always use atomic
counts. In `None` and `Tasks` builds, nothing changes.

Reference counting frees a cell once the last handle to it is gone. A cell
on a reference cycle is freed when the runtime releases it (see "Releasing
cells"), so every cell is freed (V9).

### Deadlocks and liveness

Updates cannot deadlock on locks. `Wait` can wait forever: when no task that
will run again can change the state. This is a liveness bug in the program,
and the runtime detects it.

A root's task tree is **stalled** when none of its tasks is queued, running or
waiting on a hosted operation, at least one waits on a cell, and every holder
of every cell waited on belongs to the tree. No task of the tree can run
again, and nothing outside it can change those cells. The runtime then crashes
the waiter that began waiting last, with the deadlock message, counting by the
number of tasks started so far, and on a tie the task created later. The
choice is deterministic for a given schedule. The crash readies other tasks,
through cancellation and propagation, so one detection is enough to make
progress, and a cycle that a supervisor restarts is detected again.

A cell held by another root, or by the host, may still change, so a tree
waiting on one is not stalled. With one root this is the same as the host
having nothing to run. In a `None` build it is the scheduler finding no ready
task. A runtime can keep the information incrementally, as a count per root of
tasks not waiting on a cell, checking holders only when it reaches zero.

### Changes to the companion RFC

- **Terms.** `update!` joins the list of suspension points, and a handle joins
  the list of wait sources.
- **G2** becomes: "No data races. Tasks share immutable values and `Shared`
  handles. Every change to shared state is an atomic update, and counts on
  shared values are atomic."
- **R4** gains one exception: a task's drop runs its pending cleanups, which
  are pure functions of one handle's state.
- **R6** gains waits on shared state, which can form cycles, and the stall
  detection in "Deadlocks and liveness".
- **C2** counts `update!` as a declared suspension point.
- **C6** gains cell stores as seeds.
- **The sessions RFC is superseded.** Queues and replies replace its channels
  and mailboxes ("Streams between tasks"), `Shared` replaces its resource
  libraries, and this RFC restates the holder sets and the stall detection
  that it keeps. Its coroutine `None` builds are now the companion RFC's.
- **G5 and G7 are unchanged.** Shared state can outlive the loop that created
  it, but no task does. Poisoning turns a crash into later crashes; no code
  can observe it.

## Invariants

| | Invariant | Kind | Established by |
|---|---|---|---|
| V1 | **Visibility.** Shared state is reached only through `Shared` functions, all effectful; pure code can hold a handle but has no operation on it | Language guarantee | Typing rules |
| V2 | **Update atomicity.** An update's function sees one state, and its `Set` is stored whole. A function that crashes stores nothing and poisons the handle, so no task sees a half-made state | Language guarantee | Model; differential runs under random schedules |
| V3 | **Updates do not suspend.** Update and cleanup functions are pure, so they contain no effectful call and no suspension point; `Wait` suspends only after its update is abandoned | Compiler invariant | Certify |
| V4 | **No lock deadlock.** An update holds one cell's lock, runs only pure code while holding it, and releases it before any suspension | Runtime invariant | Model |
| V5 | **Cleanups run exactly once:** a `finally!` cleanup on every exit from its body (return, cancellation, crash), an `on_failure!` cleanup on cancellation and crash only | Language guarantee | Certify push and pop; cancel at every suspension point; crash at every update |
| V6 | **Poisoning.** After an update or a cleanup crashes, every update of its handle crashes, and so does every task waiting on it | Language guarantee | Tests |
| V7 | **No lost wakeups.** A waiter registers before it releases its cell, so a store that follows the release always probes it. A waiter that a probe readies and that ends without storing, including by cancellation, probes on, so a waiter whose function would not wait is never left parked | Runtime invariant | Model (readying every waiter); tests and differential runs (readying the first) |
| V8 | **Sequential equivalence.** C9 holds for programs that use `Shared`: a `None` build's `Wait` pauses its task and runs the next ready one, as the coroutine schedule does, and is a deadlock only where that schedule deadlocks | Language guarantee | Differential |
| V9 | **Every cell is freed.** A cell is freed when its last handle goes, or, when its remaining references are only in the states of cells that no running task holds or in crashed frames, when its last holder finishes and the runtime releases it. A released cell is never updated again | Runtime invariant | Exact heap accounting after every run of generated programs whose states hold handles and jobs; an assertion at every use of a cell; mutations |
| V10 | **Forms agree.** A function's probe form answers `Wait` exactly when its update form does, and its update form puts back on `Keep` and `Wait` the state it was given | Compiler invariant | Certify; differential runs of probes against updates |

## Drawbacks

- **It adds shared mutable state to a language built on values.** Even with
  every use marked, programs can now have logic races, such as reading in one
  update and acting in the next. The warning catches the common shape, not
  every one.
- **Handles have identity.** Two equal-looking handles can be different state.
  Roc programs already handle values like this (streams, connections), but
  only from platforms.
- **Transitions are lambdas.** `get!` and `modify!` cover reading and plain
  replacement, but an update that decides or waits is a function that answers
  with a tag, longer than an assignment.
- **One handle per update.** State that changes together must share a handle,
  so unrelated state put in one record for one update becomes contended for
  every update.
- **Contention.** Under `ParallelTasks`, one hot handle serializes every task
  that updates it, and a long update delays them all.
- **A crash in an update loses the state.** Borrowing the state would keep
  it. Here a bug in an update function poisons its handle, and every task that
  uses the handle crashes, not just the one that hit the bug. Deciding inside
  and crashing outside keeps intended crashes from doing this.
- **Where the new state is built decides whether it is copied.** A state
  built before the decision, or by another function that returns the tag, is
  built while `Keep` or `Wait` may still need the old one, so it is a copy.
  The rule is the one for any Roc value, but the old state's last use is
  hidden in `Keep` and `Wait`.
- **Waking many waiters is serialized.** When a `Set` lets many waiters go
  ahead without changing the state, as when a barrier opens or a watched value
  changes, each is readied only after the one before it has run and passed
  the wake on. Releasing `n` of them takes `n` dispatches one after another,
  and a `ParallelTasks` build cannot run them at the same time. See open
  decision (c).
- **Stores run other tasks' functions.** After each `Set` on a cell with
  waiters, the storing task probes them, each inside a crash boundary. Under
  contention that is a quadratic number of pure calls, though a linear
  number of task dispatches, and in `ParallelTasks` builds it lengthens the
  time the lock is held.
- **Code runs during cancellation.** Cleanups are pure, but they are still Roc
  code running in a dropped task.
- **A cycle can outlive its use.** An abandoned cycle of cells is freed when
  the last task that held any of them finishes, not when the program drops
  its last handle. In a task that runs for the whole program, such as a
  server's loop in `main!`, cycles that the loop keeps making and dropping
  accumulate until it ends. A `None` build frees them only when the program
  exits. See open decision (e).
- **Stores walk their state.** To pass holders on, every store walks the parts
  of the new state that can hold a handle. Deadlock detection already needs
  this walk, but a large state full of handles makes every store O(n), even
  when the change itself is in place.
- **A reply nobody will answer is a deadlock, not a value.** A channel's
  receiver learned `Cancelled` when the sender's end was dropped. A cell has
  no ends: a `Reply` becomes `Dropped` only when the task answering it fails
  inside `answer!`. If a server stops with requests still queued, their
  askers wait until stall detection crashes them.
- **A local timeout gives a permit back early.** When a task holding a permit
  is cancelled while a hosted call is in progress, the call is aborted
  locally, but the remote operation may still run. The permit then counts
  local scopes, not remote operations. The sessions RFC fixed this by passing
  the permit's end to the hosted call; see "Future possibilities".
- **Poison outside a supervised scope** crashes every restart until the limit,
  by design.

## Rationale and alternatives

| Alternative | Why not |
|---|---|
| One-shot channels that carry the next end, for streams (the sessions RFC) | Ends used once and checked only at run time, one reader per stream, and no backpressure without a protocol of replies. See "Why not channels for streams" |
| Tasks that own the state, reached through mailboxes (the sessions RFC) | A server task per resource, two messages per acquire, end rules checked only at run time, and servers written as recursion. See "Why not tasks that own the state" |
| Shared variables: `var $$x`, `&$$x`, `wait` and `defer` (this RFC's earlier draft) | References in Roc: names whose plain reads see other tasks' writes, an operator that aliases storage, and a statement rule for atomicity. See "Why not shared variables" |
| Lending the state while the cell keeps it, so that an update that crashes changes nothing (this RFC's earlier draft) | The function never holds the only reference, so every `Set` copies the whole state: a queue, a log or a cache in a handle costs O(n) per operation, and a waiter's probe copies it again |
| Lending the state and recommending collections that share structure for large states | Keeps the old state for a crash at O(log n) per change, but the obvious `List` and `Dict` stay at O(n), a cost that is easy to miss |
| Returning the state on every path, as Lean's `modifyGet` does | A `Keep` could return a changed state, and waiters would never hear of the change. Here the compiler puts the state back, so `Keep` cannot change it |
| `update!` returning a pair `(new, r)`, as Haskell's `atomicModifyIORef` does | Cannot say "unchanged", so every read would wake waiters, and cannot wait |
| A separate `when!(condition, f)` for waiting | Two functions where one tag does, and the condition and the change could disagree |
| `defer` and `errdefer` keywords | Keywords, and a choice between block scope and function scope. A closure's scope settles it |
| Updates over several handles at once | The prototype supports them, locking cells in one global order, and the model checks that order. No library needed them; one record covers what they do. See "Future possibilities" |
| Readying every waiter at each `Set` | Under contention, every waiter is dispatched after every `Set`, and all but one answer `Wait` again: dispatches grow with the square of the waiters. See "What the prototype shows" |
| Requiring equality on the state, and parking again a dispatched waiter whose state equals the one it waited on | Replaces running the function with a comparison, but the waiter is still dispatched to make it, so dispatches stay quadratic. It also restricts state types and keeps old states alive |
| Readying only waiters whose state changed, compared at the `Set` | Never skips anything in the libraries: each `Set` that matters changes the state, and the waste comes from waiters that run after the first one took what the `Set` gave |
| Running the first eligible waiter's update at the `Set` and handing it the result, as the sessions RFC's semaphore cell hands over a permit | Commits an update on behalf of a task that has not run. If that task is cancelled first, its update stays committed and the `finally!` after it never runs, so a permit is lost. The semaphore cell can give a permit back; a pure update has no inverse |
| Probing the most recent waiter first (this RFC's earlier rule) | Needed only while `None` builds ran every task on one plain stack, where only the most recent waiter can continue. Probing the oldest first broke C9 against that stack on a fuzzed program. With `None` builds that pause waiting tasks (companion RFC), the order is free, and most recent first lets an early waiter starve under steady contention |
| Waking one waiter without probing, which passes the wake on if it waits again | Each `Set` by the woken waiter starts the chain again, so a semaphore still dispatches every waiter on every release |
| Type rules against cycles: a state holds no function, and no type reaches its own handle through a state (this RFC's earlier draft) | Sound, and checkable once per declaration. But the checker cannot see captures, so the rules rejected about one in ten acyclic state types in the prototype's model, every one for a function in a state. They add an inferred requirement to generic code, and they still reject cycles that break in normal use, such as a crawler's jobs that capture their queue |
| Deciding cycles from lambda sets, which show what each function captures | Exact, as the prototype shows. But only `.lss` builds compute lambda sets, and a program's validity cannot depend on how it is lowered |
| Capture summaries: function types record which handle types their captures reach, inferred as rows | Removes the type rules' false rejections of functions, but adds a component to every function type, polymorphism over it, and a default for function fields in declarations. A generic queue type still rejects jobs that capture another queue of the same type, and real cycles such as the crawler's stay errors |
| Reachability tracked per value, as in reachability types | Inferred by unification, it merges every level of a recursive type, so it still rejects every recursive state the type rules reject. It accepts more only where two cells of one type never meet, so a line elsewhere that puts them in one list becomes an error. Subset constraints give up principal types, and ordering cells by creation rejects a registry of sessions created after it. Signatures would name values, and real cycles stay errors |
| A cycle collector for cells, such as trial deletion | A second mechanism, whose timing depends on the collector. Holder sets already exist for ports and deadlock detection, and release through them follows the task tree |
| Weak handles | The programmer must decide which handle is weak, and a weak handle can find its state gone |
| Allowing functions in states, but not closures that capture handles | A function's type does not say what it captures, so the checker cannot enforce it |
| A `match` with no case for a state means `Wait` | A forgotten case would become a hang instead of a compile error |
| Optimistic retries, as in software transactional memory | Pure updates make retries safe, but long updates would redo work and could starve under contention. A lock fits because an update touches one known cell |
| Handles that cannot escape their scope | Requires non-escaping closures in the type checker, and state shared across requests must escape anyway |
| A way to clear poison | Restarting the scope that created the handle is the recovery, as in OTP |

## Prior art

| Work | Relation |
|---|---|
| Hoare, *Towards a Theory of Parallel Programming* (1972) | Conditional critical regions: a region that runs once a condition on shared state holds. An update that answers `Wait` is the same idea |
| Harris, Marlow, Peyton Jones and Herlihy, *Composable Memory Transactions* (PPoPP 2005) | `retry` abandons a transaction until something it read changes, as `Wait` does. Haskell's types keep effects out of transactions, as `->` does here |
| Haskell `atomicModifyIORef'` and `bracket`, `bracketOnError` | A pure function that returns the new state and a result; cleanup scoped to a function, always or only on an exception |
| Clojure atoms | `swap!` applies a pure function to shared state atomically |
| Rust `Mutex` | Poisoning when a thread panics while holding the lock, as here when an update or a cleanup crashes |
| Lean 4 `ST.Ref.modify` | Takes the value out of the reference while the function runs, so the function holds the only reference and changes it in place, as `update!` moves the state |
| Zig `errdefer` | Cleanup only on an error exit, as `on_failure!` |
| Erlang/OTP supervisors | Restart from known state. Where a handle is created decides which restarts reset it |
| Bacon and Rajan, *Concurrent Cycle Collection in Reference Counted Systems* (ECOOP 2001); Nim's ORC | Reference counting plus a cycle collector, restricted to types that can form cycles. Here no collector runs: holder sets say when the task tree can no longer reach a cell |
| Tofte and Talpin, *Region-Based Memory Management* (1997) | Memory tied to a scope. Release ties a cycle's memory to the tasks that used it |
| Scala 3 capture checking; reachability types (Wei, Bao, Jia, Tan and Rompf, POPL 2024) | Type systems that know what a closure captures or what a value reaches. See the alternatives table |

## What the prototype shows

The prototype's IR spells the API as primitive operations, so the libraries
and the generated programs exercise each part of it:

| API | Prototype IR |
|---|---|
| `Shared.new!(v)` | `(shared v)` |
| `h.update!(f)` | `(step f (h) args)`, where `f` is a `pure` procedure returning `Set`, `Keep` or `Wait`; `Set` puts the result first, then the new state |
| `h.finally!(c, body)` | `(defer c (h) args)`, the body, then `(undefer)` |
| `h.on_failure!(c, body)` | `(errdefer c (h) args)`, the body, then `(undefer)` |
| `h.get!()`, `h.modify!(g)` | `sv_get` and `sv_modify` in `library.py`, steps whose functions answer `Keep(s)` and, in the IR's order, `Set({}, g(s))`; the generated programs' peeks and increments |
| Reading in a test | `(read h)` |

`certify_defers` checks that every `defer` is matched on every path, so the
IR's push and pop stay balanced as the builtins guarantee by construction. The
IR also allows a step over several cells, which the generated programs use to
exercise the lock order.

**Libraries** (`library.SHARED_SOURCE`): a semaphore, a pool with a lost count
and `on_failure!`, the same pool with `finally!`, and the circuit breaker.

**Tests** (`SharedVariables`, 17 tests, `SharedConveniences`, `SharedModel`
and `CellRelease`):

- Every permit comes back under every schedule and when the root is cancelled
  at any suspension point, and at most one job holds the permit at once.
- A crash while holding runs the cleanup, so a restart can take the permit;
  skipping cleanups at drops deadlocks.
- Cancelled holders return their permits; cancelled waiters take none.
- A crashing cleanup poisons the handle: later updates and parked waiters
  crash, a handle created inside the supervised function is new after a
  restart, and not poisoning is caught. A cancelled task whose cleanup crashes
  crashes.
- A `Wait` ends when another task stores; a store that readies no waiter is
  caught as a deadlock. A `None` build pauses the waiting task, runs the
  storing entry, and matches the coroutine schedule. A `Wait` nothing can
  end is the same deadlock in every build.
- The pool with `on_failure!` counts a crashed user's connection as lost and
  opens one replacement; with `finally!` it lends the crashed user's
  connection again. A borrower cancelled while opening a replacement leaves
  the pool accounting for every slot under every schedule.
- The breaker opens after the third failure, rejects four requests without
  calling through, probes and closes; a cancelled probe reopens it, and
  without its cleanup it stays `Probing`.
- An update moves the state into its function's update form, which the
  prototype makes before ARC by rewriting tails as "Lowering" describes. Every
  `Keep` and `Wait` is checked to put back the very state it was given, and a
  waiter that a probe readied is checked not to wait when it runs on the
  state the probe saw. Probes run the function as written; the probe form is
  not modeled. An update that crashes poisons its handle: a restart's read
  crashes with a message that names an update, and a waiter parked on the
  handle crashes, under every schedule. The state the update held is in its
  crashed frames, and the exact heap finds it there. With the earlier rule,
  which kept the state, the restart reads the old state and the waiter
  deadlocks.
- 20 clients contend for one permit. Readying only the oldest waiter that a
  probe finds would not wait takes 80 task resumes and runs 19 updates again,
  as most recent first did; readying every waiter, the regression switch,
  takes 251 and 190.
- A waiter readied by a probe and cancelled by a race before it runs passes
  the wake on: the other waiter gets the permit under every schedule. Every
  client of the semaphore without a race holds the permit alone.
  Without passing it on, some schedule deadlocks.
- Static checks: unbalanced cleanups, impure update functions, and `Shared`
  operations in pure procedures are rejected.
- The download counter: three workers call `modify!`, a reporter waits for
  the third, and `get!` reads the total, with the same result under every
  schedule and in a `None` build.
- `shared_model.py` explores every interleaving of the `ParallelTasks` lock
  protocol. It finds no deadlock, lost wakeup or lost update, and catches
  locking two-cell steps in each thread's own order (V4), registering a waiter
  after unlocking (V7), and dropping the locks (V2).
- A crawler whose jobs capture their own queue gives 7 under every schedule
  and in a `None` build. The queue it abandons with a job still in it is
  released when `main` finishes, and nothing leaks.
- Two tasks that each make two cells holding each other's handles release
  one cell of each pair when they finish, which frees the other.
- A task that stores a cell it made into a cell `main` holds, then ends,
  leaves that cell usable by `main`. Not passing a cell's holders on to the
  cells stored in it releases it too early, and the assertion at every use
  catches it.

**Fuzzing.** 2,000 generated programs, 628 of them using shared state
(permits with `finally!` or `on_failure!`, crashing cleanups, increments,
reads, waits and two-cell moves), each run under every schedule, both
lowerings, a `None` build, and cancellation at every suspension point, with
exact heap accounting. All checks passed: 63,926 runs, 1,964 compared against
the input-order schedule (C9), 15,926 cancelled at a suspension point, and
164,554 updates, of which 3,726 ran again after a `Wait` and 6,208 cleanups
ran at a drop.

**Cost against the sessions RFC's libraries.** `bench/shared_vs_actor.py`
runs 30 clients of a semaphore with one permit and of a pool of two, under the
pause schedule:

| Written as | Tasks spawned | Resumes | Allocations | Messages | Update functions run | Probes |
|---|---|---|---|---|---|---|
| Semaphore, server task (sessions RFC) | 93 | 276 | 725 | 90 | | |
| Semaphore, the runtime's semaphore cell (sessions RFC) | 30 | 120 | 189 | 0 | | |
| Semaphore, `Shared`, every waiter readied | 30 | 526 | 654 | 0 | 495 | |
| Semaphore, `Shared`, every waiter readied, equal states compared | 30 | 526 | 248 | 0 | 89 | |
| Semaphore, `Shared`, oldest waiter that would not wait | 30 | 120 | 683 | 0 | 89 | 435 |
| Pool, server task (sessions RFC) | 93 | 276 | 758 | 90 | | |
| Pool, `Shared`, every waiter readied | 30 | 497 | 777 | 0 | 466 | |
| Pool, `Shared`, every waiter readied, equal states compared | 30 | 497 | 399 | 0 | 88 | |
| Pool, `Shared`, oldest waiter that would not wait | 30 | 119 | 889 | 0 | 88 | 406 |

These are counts of operations in the Python prototype, not timings. A resume
is one dispatch of a task by the host, including a readied waiter that runs
its function, answers `Wait` and parks again. A probe is a call of a waiter's
function by the storing task, whose result is discarded.

- `Shared` needs no server task, no mailbox and no reply channel.
- Readying every waiter dispatches each of them after every `Set`, and all but
  one wait again: about n² resumes under contention.
- Comparing a dispatched waiter's state with the one it waited on saves
  running its function, but not the dispatch. The second row of each library
  is the earlier proposal to require equality on the state; it leaves resumes
  unchanged.
- Readying only the oldest waiter that a probe finds would not wait makes
  resumes linear: the semaphore then matches the runtime's semaphore cell.
  The cost moves to probes, pure calls that are still about n², and to the
  allocations they make for the results they discard.
- Most recent first, the earlier rule, gives the same counts in every
  column, under this schedule and a random parallel one.
- Under the coroutine schedule, which `None` builds of programs that can wait
  use, both finish: the server tasks with 247 resumes, `Shared` with 61. Under
  the input-order schedule, which models one plain stack, the server tasks
  deadlock and `Shared` does not.

With oldest first as the prototype's default, and `None` builds of programs
that can wait running as coroutines, the suite passes, and so do the same
2,000 fuzzed programs that passed under the earlier rule. Of these, 1,005 can
wait and are compared against the coroutine schedule, and 995 cannot and are
compared against the input-order schedule; 1,965 comparisons in all (C9), and
for each program that cannot wait, the coroutine build gives the same results
and effects as one stack. The run had 15,926 cancellations at a suspension
point, 5,006 probes, and 338 wakes passed on by a readied waiter that was
dropped before it ran. Under the earlier rule the figures were 1,964
comparisons, 15,924 cancellations, 5,028 probes and 336 wakes passed on.

The generator then composed spawns under the limit at a plan's root,
background entries and `plan.all!()` / `plan.all_ok!()`. Over 2,000 programs
built that way, every check passed: 64,086 runs, 1,960 compared under C9,
16,086 cancellations, 22,112 entries spawned under a root limit, 61,818 loops
that ended with background entries left (107,256 of them cancelled and 11,894
discarded before they started), 3,600 probes and 482 wakes passed on.

**Releasing cells** (`CellRelease`, and `check.py` with `links`). The
generator also composes cells whose states hold items: handles to cells,
their own included, and jobs that capture cells. Workers and jobs, side by
side, link cells, push jobs, pop the newest item and run it (a popped
handle's cell is updated), clear states and make cells of their own. So
states form cycles, and programs abandon them at random. Every run's heap is
verified exact, so a cycle that survived would be reported as a leak, and
every use of a cell asserts that it was not released.

Over 2,000 such programs (1,338 with linked cells), each run under every
schedule, both lowerings, a `None` build and cancellation at suspension
points, every check passed:

| | Count |
|---|---|
| Runs | 64,094 |
| Cells released when a child task finished | 77,430 |
| Cells released when the root finished | 10,316 |
| Cells released by a `None` build (1,486 of the programs run there as coroutines, which release a cell when the last task holding it finishes; the other 514 on one stack, which releases at the end) | 3,425 |
| Leaks, and cells used after release | 0 |

These programs come from the generator that also composes spawns under a
root limit, background entries and `plan.all!()`. The default programs from
it, which form no cycles, give 64,086 runs and 16,086 cancellations at a
suspension point, and release 282 cells.
Without a cycle, a live cell that no task holds can only be one left in a
crashed frame.

Never releasing leaks on the fourth generated program. Not passing a cell's
holders on at a store releases a cell too early on the 56th, and the
assertion catches it. Not making a task a holder of what its entry captures
is not caught: a task that captures a handle runs inside a task that holds
it, which outlives it.

**The rejected type rules** (`cycles.py`). The IR is untyped, so the type
rules have a model of their own. It generates type declarations, possibly
recursive and generic, and closed types over them, by composing records, tag
unions, lists, handles, function types and declared types at random.
Function types carry their lambda sets, which a Roc type would not show. It
decides each type with lambda sets (exact), by the rules on the expanded
type, and by the rules once per declaration with summaries and requirements,
and runs random programs over accepted types on a heap that counts
references. Over 100,000 generated types:

| | Types | Result |
|---|---|---|
| Accepted with lambda sets | 87,114 | 348,456 random programs, 1,518,146 cells, no leak |
| Rejected with lambda sets | 12,886 | Every witness leaks; 3,436 random programs leak too |
| Accepted by the type rules | 78,755 | Each also accepted with lambda sets |
| Rejected by the type rules only | 8,359 | Every one for a function in a state |
| Per declaration against expanded types | 100,000 | The same answer for every type |

The rules are sound, and checking each declaration once suffices. But they
reject about one in ten acyclic state types, and every cyclic one, including
cycles that break in normal use. Release needs neither the rules nor this
model.

## Decisions left to review

Names are placeholders. Seven earlier questions are settled, six of them by
the prototype:

- **Cleanup that knows how its body ended.** Needed: the pool must not lend a
  connection whose user crashed. `on_failure!` is the second form.
- **Block or function scope for cleanup.** Moot: a cleanup belongs to the
  closure passed to `finally!` or `on_failure!`.
- **Updates over several handles.** Not needed by the semaphore, pool or
  breaker. One handle per update keeps locks from nesting.
- **Waking waiters.** After a `Set`, the runtime probes the waiters, oldest
  first, and readies the first whose function would not wait; a readied
  waiter that ends without storing probes on. The prototype first settled
  this with most recent first, because its `None` builds ran on one plain
  stack, where oldest first broke C9. With `None` builds that pause waiting
  tasks, oldest first passes every check, gives the same counts, and is fair.
  Readying every waiter makes
  dispatches quadratic under contention. Comparing states, which the earlier
  proposal needed equality for, leaves dispatches quadratic. Handing a result
  to a waiter at the `Set` loses permits when the waiter is cancelled. See
  "Runtime" and the alternatives table.
- **Cycles through states.** Released through holder sets, so no type rule
  limits what a state holds. The type rules were sound, but rejected about
  one in ten acyclic state types in the prototype's model, every one for a
  function in a state. They added an inferred requirement to generic code,
  and still rejected cycles that break in normal use, such as a crawler's
  jobs that capture their queue. Inferring captures or reachability would
  remove the first cost but not the others. See the alternatives table.
- **Conveniences.** `get!` and `modify!` are in, defined with `update!`.
  `get!` also makes reading and then acting separately easier to write, so
  the read-then-write warning covers it.
- **Changing a large state in place** (open decision (f) in earlier drafts).
  `update!` moves the state into its function, `Keep` and `Wait` put the same
  state back, and an update that crashes poisons its handle. A state that the
  update holds alone changes in place, so a queue, a log or a cache in a
  handle costs what the same collection costs as a value. Lending the state
  kept "an update that crashes changes nothing", but copied the whole state on
  every `Set`, and again in the probe that readies a waiter. Collections that
  share structure would have cut that to O(log n) while leaving the obvious
  `List` and `Dict` at O(n). The two rules differ only when an update crashes,
  which is a bug in a pure function, and an OTP server that crashes loses its
  state too. Moving never copies where lending would not, so this needed no
  benchmark: it was not measured.

Still open:

- **(a) Detecting a `Wait` that can never end across roots.** Per-root
  detection with holder sets is implemented and tested within one root. A
  root waiting on a handle that a task of another root holds is untested for
  cells. It was tested for the superseded sessions RFC's ports, which used the
  same sets.
- **(b) Contention under `ParallelTasks`:** whether spinning then yielding is
  enough, and how a hot handle compares with a semaphore task. The Python
  prototype cannot measure it; it needs the real runtime.
- **(c) The cost of probing.** Probes are still about n² pure calls under
  contention, each inside a crash boundary and, under `ParallelTasks`, under
  the cell's lock. Options to cut them: skip a waiter whose state equals the
  one it waited on, for state types that have equality, which saves calls but
  keeps old states alive; or let a library declare that its waiters are
  interchangeable, so that a probe that finds one waiter waiting stops. The
  real runtime should measure whether either is worth it. Separately, waiters
  that would `Keep` are released one at a time, each passing the wake on.
  Readying at once every waiter whose probe would `Keep`, with the first that
  would `Set`, would release them together, at the cost of probing every
  waiter after each `Set`.
- **(d) Channel ends in states.** Moot: channels are superseded by queues
  ("Streams between tasks"), which are cells.
- **(e) Cycles in long-lived tasks.** Release frees an abandoned cycle when
  the last task that held it finishes. A task that runs for the whole program
  and keeps making and dropping cycles accumulates them, and so does a `None`
  build until it exits. Releasing sooner would need to know when a task no
  longer holds a handle, which no count says for a cycle. Whether real
  programs do this, and whether a debug diagnostic that reports cells freed
  by release, with where each was made, is enough, needs programs written
  against the real runtime.

## Future possibilities

- **Hosted functions that hold a permit.** A local timeout gives a permit
  back while the remote operation it started may still run (see
  "Drawbacks"). A hosted function that took the `Semaphore` and released the
  permit when the platform had finished with the operation would count
  operations instead of local scopes.
- **Updates over several handles**, such as `Shared.update2!(a, b, f)`, if
  libraries come to need them. The prototype's runtime and model already cover
  locking several cells in one global order.
- **Hosted functions that take `Shared(s)`**, for metrics the platform reads.
- **A static check for read-then-act races** beyond the syntactic warning.

## Implementation plan

1. **Prototype.** Done; see "What the prototype shows".
2. **Builtins.** `Shared(s)`, its four functions and the two shorthands, with
   their types.
3. **Checker.** The three warnings.
4. **Post-check IR.** Explicit updates with their update and probe forms,
   cleanup pushes and pops, certified (V3, V5, V10).
5. **Runtime.** Cells, locking per build, waiters, cleanup stacks, poisoning,
   and holder sets with release, checked against the model (V4, V7, V9).
6. **Reference counting.** Cell stores as atomicity seeds in `ParallelTasks`
   builds.
7. **Documentation.** A langref section beside "Variables (with `var`)".

## References

- C. A. R. Hoare, *Towards a Theory of Parallel Programming*, in Operating
  Systems Techniques, Academic Press, 1972
- T. Harris, S. Marlow, S. Peyton Jones and M. Herlihy, *Composable Memory
  Transactions*, PPoPP 2005
