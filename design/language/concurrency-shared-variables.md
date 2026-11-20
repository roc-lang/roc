# RFC: Shared State

| | |
|---|---|
| Status | Draft. Not adopted; nothing here is part of `design.md`. |
| Scope | The builtin `Shared(s)` type and its functions (`new!`, `update!`, `finally!`, `on_failure!`), atomic updates, waiting, cleanup on cancellation and crash, poisoning, their runtime representation and lowering, and the changes they make to the companion RFCs |
| Companions | [Suspendable Effects and Platform-Scheduled Concurrency](concurrency-explicit-suspension.md): the task tree, plans, concurrent loops and crashes this RFC builds on. [Sessions, Channels and Shared Resources](concurrency-sessions.md): if this RFC is adopted, it replaces that RFC's mailboxes and its shared-resource libraries; channels remain for streaming |
| Prototype | [`concurrency-prototype/`](concurrency-prototype/): `shared.py`, `shared_model.py`, the libraries in `library.py` and the `SharedVariables` and `SharedModel` tests. See "What the prototype shows" |

## Summary

The companion RFC's tasks share only immutable values. That covers fan-out,
races and supervision, but not a resource that many tasks use while they run:
a semaphore, a rate limiter, a pool, a circuit breaker shared across requests.
The sessions RFC builds each of those as a server task that other tasks reach
through a mailbox. This RFC instead adds one builtin opaque type and four
functions, and no syntax:

```roc
Shared.new! : s => Shared(s)
Shared.update! : Shared(s), (s -> [Set(s, r), Keep(r), Wait]) => r
    where [s.is_eq : s, s -> Bool]
Shared.finally! : Shared(s), (s -> s), (() => a) => a
Shared.on_failure! : Shared(s), (s -> s), (() => a) => a
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
   again. The state type needs equality, so that a waiter is not resumed
   when the state is back to the one it waited on. The `->` in `f`'s type is the atomicity boundary: a pure function
   cannot suspend or perform I/O, so nothing can happen in the middle of it.
3. **Cleanup.** `permits.finally!(cleanup, body)` runs `body`, then applies
   `cleanup` to the state however `body` ends: by returning, by cancellation or
   by a crash. `on_failure!` applies it only on cancellation or a crash.
   Cleanups are pure too, so a cancelled task's drop can run them.

Crashes keep OTP's semantics. An update that crashes changes nothing, so
shared state is never left half-updated. A cleanup that crashes poisons the
state, so a broken cleanup obligation becomes a crash at its next use instead
of a silent leak.

All names (`Shared`, `update!`, `Set`, `Keep`, `Wait`, `finally!`,
`on_failure!`) are placeholders.

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

The sessions RFC answers this with actors: a semaphore is a task that owns the
count, serves requests from a mailbox and hands out permits over channels. The
prototype shows that this works, and also what it costs:

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
6. A crash never leaves shared state half-written, and a broken cleanup
   obligation is loud.
7. No new syntax, no new kind of name and no references: one builtin opaque
   type and its functions. No effect polymorphism, no non-escaping types, no
   implicit conversions.
8. No reference cycles, which Roc's reference counting cannot free.
9. The same meaning under `None`, `Tasks` and `ParallelTasks`, up to
   scheduling.

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
        done.update!(|n| Set(n + 1, {}))
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

Reading the state is an update that keeps it: `done.update!(|n| Keep(n))`.
What a read returns may be out of date by the next line. To act on the state,
decide inside the update.

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

When a permit is given back, every waiting task is readied. The first to run
takes the permit, and the count is 0 again. The others are not resumed: the
function is pure, so on the state it already answered `Wait` to, it would
answer `Wait` again. This is why `update!` needs equality on the state type.
Numbers, strings, lists, records and tag unions of those all have it; a state
holding a function does not.

When the function is a `match`, the case that waits is written out, so a
forgotten case is a compile error, not a hang.

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
    server.hits.update!(|n| Set(n + 1, {}))
    guarded!(server.breaker, Clock.now!(), || route!(req))
}
```

Two handles from two calls to `Shared.new!` are different state, even if they
start equal. Copying a handle copies the handle, not the state.

### Crashes

- **An update that crashes changes nothing.** An overflow inside the function
  leaves the state as it was, and the task crashes at the `update!` call.
- **A cleanup that crashes poisons the state.** Every later update of a
  poisoned handle crashes with "this shared state was poisoned when a cleanup
  crashed at …". No code can test for poison or clear it.
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
  read of the same handle, as in `n = c.update!(|s| Keep(s))` followed by
  `c.update!(|_| Set(n - 1, {}))`. That is a lost update. The warning suggests
  deciding inside one update.
- **A suspension point between an update and its cleanup:** a call that can
  suspend between an `update!` and the `finally!` or `on_failure!` on the same
  handle that follows it in the same body. A cancellation there skips the
  cleanup and leaks whatever the update took.

### On platforms that do not schedule tasks

In a `None` build, updates are ordinary calls. A `Wait` runs the concurrent
loop entries that have not started, earliest in input order first, until the
state changes, as a receive does in the sessions RFC. If none is left, nothing
can ever change the state, and the task crashes with "deadlock: no task can
ever change the shared state this task waits for". This matches the
input-order schedule of the companion RFC's C9.

### Library patterns

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

**Semaphore and pool:** `with_permit!` and `with_conn!` above.

## Reference-level explanation

### Typing

```roc
Shared.new! : s => Shared(s)
Shared.update! : Shared(s), (s -> [Set(s, r), Keep(r), Wait]) => r
    where [s.is_eq : s, s -> Bool]
Shared.finally! : Shared(s), (s -> s), (() => a) => a
Shared.on_failure! : Shared(s), (s -> s), (() => a) => a
```

- `Shared(s)` is a builtin opaque type. It has no other functions, no equality
  and no hashing. `Inspect` shows only `<Shared>`.
- `update!` requires the state type to have equality (`is_eq`), for the
  wake rule in "Runtime". The handle itself still has none.
- Every function on it is effectful. `new!` is an effect because two calls
  create two distinct states. Binding, holding, passing or storing a handle is
  not an effect.
- The update and cleanup functions are pure. That is the whole of the
  atomicity rule, and the type checker already enforces it.
- **No cycles.** The state type `s` of a `Shared(s)` may not contain
  `Shared(s)`, directly, through other handles' state types, inside opaque
  types, or in the captures of functions it holds. A reference cycle through
  handles would need such a type, so this rules them out. Roc's lambda sets
  make captures explicit, so the check consumes explicit data.
- Hosted function signatures cannot mention `Shared` in the first version.

### `update!`

`h.update!(f)` runs as if no other task ran during it:

1. Acquire `h` (see "Runtime").
2. If `h` is poisoned, release it and crash.
3. Call `f` on the state. While `f` runs, the state is borrowed: `f` sees the
   current value and cannot be told apart from a call on an ordinary value.
4. On `Set(new, r)`: store `new`, release the old state, ready every task
   waiting on `h`, release `h`, and return `r`.
5. On `Keep(r)`: release `h` and return `r`.
6. On `Wait`: register the task as a waiter on `h` while still holding it,
   release `h` and park. When another task's update stores to `h`, or `h` is
   poisoned, the waiter is readied. Before the runtime resumes it, it acquires
   `h` and compares the state with the one the waiter's `f` answered `Wait`
   to. If they are equal and `h` is not poisoned, the waiter registers again
   while still holding `h`, and parks without being resumed. Otherwise it is
   resumed and starts again at step 1.

If `f` crashes, nothing is stored, `h` is released as part of the crash, and
the task crashes at the call.

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
- **A cleanup that crashes** stores nothing and poisons its handle. The
  remaining entries still run. If the task was being cancelled, it now
  crashes, and crash links take over. If it was already crashing, the first
  crash propagates, with the cleanup's message attached.

A cleanup can never wait: `s -> s` has no `Wait`. An update's pending `Wait`
holds no entry, so a task parked in `Wait` has nothing of the update to undo.

### Poisoning

Each handle's state carries a poisoned flag. A cleanup that crashes sets it.
Any later update of a poisoned handle crashes with a message naming the
cleanup's source location. Waiters on a poisoned handle are readied, so that
they crash too.

### Runtime

A handle points to a reference-counted cell holding the state, a poisoned
flag, a list of waiters, and in `ParallelTasks` builds a lock word.

| Build | Acquiring a cell | Waiters |
|---|---|---|
| `None` | Nothing to do | None: `Wait` runs loop entries on demand |
| `Tasks` | Nothing to do: the function is pure and cannot suspend, and resumes are serial (P10), so no other task can run during it | A list per cell |
| `ParallelTasks` | Lock the cell | A list per cell, guarded by its lock |

**Waking.** A `Set` readies every waiter on the cell. Each of them answered
`Wait` to the state that the `Set` replaced, so the runtime keeps that state,
with one reference per `Set` rather than per waiter, until the last waiter it
readied has been compared. While a cell has waiters, an update's function
therefore cannot reuse the old state in place, and a `Set` copies it. Before
resuming a readied waiter, the runtime compares the cell's state with the kept
state using the state type's `is_eq`, which the specialization of `update!`
supplies, and parks the waiter again if they are equal. This is exact: on an
equal state, the waiter's function would answer `Wait` again. Under
contention, it is what keeps updates linear in the number of waiters rather
than quadratic (see "What the prototype shows").

An update holds one lock and runs only pure code while holding it. A pure
function cannot call `update!`, so locks never nest, and lock-order deadlock
is impossible without any ordering. A task waiting for a lock is not at a
suspension point: it spins briefly and then yields its thread, and it cannot
be cancelled there.

### Lowering

`Shared` functions are builtins, and their structure is explicit before LIR,
so later stages consume it and never reconstruct it:

- `Shared.new!` lowers to a call that allocates a cell.
- `update!` lowers to an acquire, a load of the state, a call of the function,
  a `match` on its tag with a store, release and wake on `Set`, a release on
  `Keep`, and a register, release and suspend followed by a jump back to the
  acquire on `Wait`. In `None` and `Tasks` builds, acquire and release are
  omitted; the `concurrency` field is already an explicit input to every
  unit's compilation.
- `finally!` and `on_failure!` lower to a push, the call of `body`, and a pop
  followed by the cleanup update (`finally!`) or by dropping the entry
  (`on_failure!`). Push and pop are balanced by construction, and certified.

Backends see calls to builtins and ordinary `incref` and `decref` statements.
They need no new knowledge of reference counting.

### Reference counting

In `ParallelTasks` builds, a cell and every value stored in it can be reached
by tasks on several threads. Storing a value into a cell is an atomicity seed,
like a hosted argument in the companion RFC's C6, and cells always use atomic
counts. In `None` and `Tasks` builds, nothing changes.

Because no state type can reach its own handle type, cells form no cycles,
and reference counting frees every cell once the last handle to it is gone.

### Deadlocks and liveness

Updates cannot deadlock on locks. `Wait` can wait forever: when no task that
will run again can change the state. This is a liveness bug in the program, as
a receive that never gets a message is under the sessions RFC, and the runtime
detects it the same way: each cell records the set of live tasks that hold its
handle, and when every task of a root is parked and none of the holders of a
waited-on cell can run, the waiters crash with the deadlock message.

### Changes to the companion RFC

- **Terms.** `update!` joins the list of suspension points, and a handle joins
  the list of wait sources.
- **G2** becomes: "No data races. Tasks share immutable values and `Shared`
  handles. Every change to shared state is an atomic update, and counts on
  shared values are atomic."
- **R4** gains one exception: a task's drop runs its pending cleanups, which
  are pure functions of one handle's state.
- **R6** gains waits on shared state, which can form cycles.
- **C2** counts `update!` as a declared suspension point.
- **C6** gains cell stores as seeds.
- **G5 and G7 are unchanged.** Shared state can outlive the loop that created
  it, but no task does. Poisoning turns a crash into later crashes; no code
  can observe it.

## Invariants

| | Invariant | Kind | Established by |
|---|---|---|---|
| V1 | **Visibility.** Shared state is reached only through `Shared` functions, all effectful; pure code can hold a handle but has no operation on it | Language guarantee | Typing rules |
| V2 | **Update atomicity.** An update's function sees one state, and its `Set` is stored whole or not at all | Language guarantee | Model; differential runs under random schedules |
| V3 | **Updates do not suspend.** Update and cleanup functions are pure, so they contain no effectful call and no suspension point; `Wait` suspends only after its update is abandoned | Compiler invariant | Certify |
| V4 | **No lock deadlock.** An update holds one cell's lock, runs only pure code while holding it, and releases it before any suspension | Runtime invariant | Model |
| V5 | **Cleanups run exactly once:** a `finally!` cleanup on every exit from its body (return, cancellation, crash), an `on_failure!` cleanup on cancellation and crash only | Language guarantee | Certify push and pop; cancel at every suspension point; crash at every update |
| V6 | **Poisoning.** After a cleanup crashes, every update of its handle crashes, and so does every task waiting on it | Language guarantee | Tests |
| V7 | **No lost wakeups.** A waiter registers before it releases its cell, so a store that follows the release always readies it. A readied waiter is parked again without resuming only when, with its cell held, the state equals the one it waited on, which is where its function would answer `Wait` again | Runtime invariant | Model; tests |
| V8 | **Sequential equivalence.** C9 holds for programs that use `Shared`: a `None` build's `Wait` runs loop entries on demand, and is a deadlock only where the input-order schedule deadlocks | Language guarantee | Differential |
| V9 | **No cycles.** No state type reaches its own handle type, so cells are freed when their last handle is. Not yet prototyped: the prototype's IR is untyped | Language guarantee | Typing rules |

## Drawbacks

- **It adds shared mutable state to a language built on values.** Even with
  every use marked, programs can now have logic races, such as reading in one
  update and acting in the next. The warning catches the common shape, not
  every one.
- **Handles have identity.** Two equal-looking handles can be different state.
  Roc programs already handle values like this (streams, connections), but
  only from platforms.
- **Every transition is a lambda.** `done.update!(|n| Set(n + 1, {}))` is
  longer than an assignment. Small conveniences could shorten the common
  cases; see "Decisions left to review".
- **One handle per update.** State that changes together must share a handle,
  so unrelated state put in one record for one update becomes contended for
  every update.
- **Contention.** Under `ParallelTasks`, one hot handle serializes every task
  that updates it, and a long update delays them all.
- **State types need equality**, for the wake rule. A state holding a
  function cannot be shared, and while a cell has waiters, each `Set` copies
  the state and the runtime compares states before resuming waiters.
- **Code runs during cancellation.** Cleanups are pure, but they are still Roc
  code running in a dropped task.
- **The no-cycles rule** forbids some data structures, such as shared state
  holding a closure that captures its own handle.
- **Poison outside a supervised scope** crashes every restart until the limit,
  by design.

## Rationale and alternatives

| Alternative | Why not |
|---|---|
| Tasks that own the state, reached through mailboxes (the sessions RFC) | A server task per resource, two messages per acquire, end rules checked only at run time, and servers written as recursion. See "Why not tasks that own the state" |
| Shared variables: `var $$x`, `&$$x`, `wait` and `defer` (this RFC's earlier draft) | References in Roc: names whose plain reads see other tasks' writes, an operator that aliases storage, and a statement rule for atomicity. See "Why not shared variables" |
| `update!` returning a pair `(new, r)`, as Haskell's `atomicModifyIORef` does | Cannot say "unchanged", so every read would wake waiters, and cannot wait |
| A separate `when!(condition, f)` for waiting | Two functions where one tag does, and the condition and the change could disagree |
| `defer` and `errdefer` keywords | Keywords, and a choice between block scope and function scope. A closure's scope settles it |
| Updates over several handles at once | The prototype supports them, locking cells in one global order, and the model checks that order. No library needed them; one record covers what they do. See "Future possibilities" |
| Waking every waiter at each `Set`, with no equality on the state | Under contention, every waiter runs its function after every `Set`, and all but one wait again: updates run again grow with the square of the waiters. See "What the prototype shows" |
| Readying only waiters whose state changed, compared at the `Set` | Never skips anything in the libraries: each `Set` that matters changes the state, and the waste comes from waiters that run after the first one took what the `Set` gave |
| Running each waiter's function at the `Set` and handing it the result, as the sessions RFC's semaphore cell hands over a permit | Commits an update on behalf of a task that has not resumed. If that task is cancelled first, its update stays committed and the `finally!` after it never runs, so a permit is lost. The semaphore cell can give a permit back; a pure update has no inverse |
| Waking one waiter, which passes the wake on if it waits again | Each `Set` by the woken waiter starts the chain again, so a semaphore still runs every waiter's function on every release |
| Comparing states by identity instead of equality | Exact for numbers and for a state the `Set` returned unchanged, but a pool's state is a new record after every update, so the pool would keep the quadratic cost |
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
| Rust `Mutex` | Poisoning when a thread panics while holding the lock. Here only a crashing cleanup poisons, since an update that crashes stores nothing |
| Zig `errdefer` | Cleanup only on an error exit, as `on_failure!` |
| Erlang/OTP supervisors | Restart from known state. Where a handle is created decides which restarts reset it |

## What the prototype shows

The prototype's IR spells the API as primitive operations, so the libraries
and the generated programs exercise each part of it:

| API | Prototype IR |
|---|---|
| `Shared.new!(v)` | `(shared v)` |
| `h.update!(f)` | `(step f (h) args)`, where `f` is a `pure` procedure returning `Set`, `Keep` or `Wait`; `Set` puts the result first, then the new state |
| `h.finally!(c, body)` | `(defer c (h) args)`, the body, then `(undefer)` |
| `h.on_failure!(c, body)` | `(errdefer c (h) args)`, the body, then `(undefer)` |
| Reading in a test | `(read h)` |

`certify_defers` checks that every `defer` is matched on every path, so the
IR's push and pop stay balanced as the builtins guarantee by construction. The
IR also allows a step over several cells, which the generated programs use to
exercise the lock order.

**Libraries** (`library.SHARED_SOURCE`): a semaphore, a pool with a lost count
and `on_failure!`, the same pool with `finally!`, and the circuit breaker.

**Tests** (`SharedVariables`, 16 tests, and `SharedModel`):

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
  caught as a deadlock. A `None` build runs the storing entry on demand and
  matches the input-order schedule. A `Wait` nothing can end is the same
  deadlock in every build.
- The pool with `on_failure!` counts a crashed user's connection as lost and
  opens one replacement; with `finally!` it lends the crashed user's
  connection again. A borrower cancelled while opening a replacement leaves
  the pool accounting for every slot under every schedule.
- The breaker opens after the third failure, rejects four requests without
  calling through, probes and closes; a cancelled probe reopens it, and
  without its cleanup it stays `Probing`.
- An update that crashes stores nothing.
- 20 clients contend for one permit. A waiter whose count is back at 0 when
  it would be resumed is parked again instead: 19 updates run again and 80
  resumes. Resuming every readied waiter, the regression switch, gives 190
  and 251.
- Static checks: unbalanced cleanups, impure update functions, and `Shared`
  operations in pure procedures are rejected.
- `shared_model.py` explores every interleaving of the `ParallelTasks` lock
  protocol. It finds no deadlock, lost wakeup or lost update, and catches
  locking two-cell steps in each thread's own order (V4), registering a waiter
  after unlocking (V7), and dropping the locks (V2).

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

| Written as | Tasks spawned | Resumes | Allocations | Messages | Updates run again |
|---|---|---|---|---|---|
| Semaphore, server task (sessions RFC) | 93 | 276 | 721 | 90 | |
| Semaphore, the runtime's semaphore cell (sessions RFC) | 30 | 120 | 187 | 0 | |
| Semaphore, `Shared`, every waiter resumed | 30 | 526 | 652 | 0 | 435 |
| Semaphore, `Shared`, compared at the `Set` | 30 | 526 | 652 | 0 | 435 |
| Semaphore, `Shared`, compared before resuming (the rule) | 30 | 120 | 246 | 0 | 29 |
| Pool, server task (sessions RFC) | 93 | 276 | 754 | 90 | |
| Pool, `Shared`, every waiter resumed | 30 | 497 | 775 | 0 | 406 |
| Pool, `Shared`, compared at the `Set` | 30 | 497 | 775 | 0 | 406 |
| Pool, `Shared`, compared before resuming (the rule) | 30 | 119 | 397 | 0 | 28 |

These are counts of operations in the Python prototype, not timings.
`Shared` needs no server task, no mailbox and no reply channel. Resuming every
waiter after each `Set` costs about n² updates run again under contention: one
client takes the permit, and every other one runs its function, finds none and
waits again. Comparing at the `Set` saves nothing, because each `Set` changes
the state. Comparing before resuming makes the cost linear, and the semaphore
then matches the runtime's semaphore cell. With that rule, the default in the
prototype, the suite and 2,000 fuzzed programs pass. Under the input-order
schedule, the server tasks deadlock, as they do in a `None` build; `Shared`
does not.

## Decisions left to review

Names are placeholders. The prototype settled three earlier questions:

- **Cleanup that knows how its body ended.** Needed: the pool must not lend a
  connection whose user crashed. `on_failure!` is the second form.
- **Block or function scope for cleanup.** Moot: a cleanup belongs to the
  closure passed to `finally!` or `on_failure!`.
- **Updates over several handles.** Not needed by the semaphore, pool or
  breaker. One handle per update keeps locks from nesting.

Still open:

- **(a) Detecting a `Wait` that can never end across roots.** Per-root
  detection with holder sets is implemented and tested within one root. A
  root waiting on a handle that a task of another root holds is untested for
  cells, though tested for the sessions RFC's ports, which use the same sets.
- **(b) Contention under `ParallelTasks`:** whether spinning then yielding is
  enough, and how a hot handle compares with a semaphore task. The Python
  prototype cannot measure it; it needs the real runtime.
- **(c) How the checker states the no-cycles rule for polymorphic code:** as a
  constraint on `s` that callers inherit, or as a check when a state type is
  known. Not prototyped.
- **(d) Skipping waiters whose state has not changed.** An update function
  is pure, and its captures are fixed, so a waiter whose state equals the
  state it waited on would answer `Wait` again. Skipping such waiters at the
  `Set`, and again just before resuming one, makes contention linear: the
  shared semaphore then matches the runtime's semaphore cell. The prototype
  has this rule behind a switch (`Shared.skip_unchanged`), and the tests and
  1,000 fuzzed programs pass with it on. A real runtime needs a way to tell
  that the state is unchanged. Options: require equality of every state type
  that an update can wait on; skip only for state types that have it; compare
  by identity, which is exact for unboxed values and for a boxed value the
  `Set` passed back unchanged, and otherwise only wakes a waiter that would
  wait again; or wake waiters one at a time and have a waiter that waits
  again pass the wake on.
- **(e) Conveniences:** `get!` for `update!(|s| Keep(s))`, and `modify!` for
  `update!(|s| Set(g(s), {}))`. Each would make the common case shorter; `get!`
  also makes reading and then acting separately easier to write.

## Future possibilities

- **Channels as streams.** With shared resources handled here, the sessions
  RFC narrows to pipelines and backpressure. A receiving end can be a builtin
  `Stream`, consumed by `for!`, readable by several workers, and finished when
  every sender is gone.
- **Updates over several handles**, such as `Shared.update2!(a, b, f)`, if
  libraries come to need them. The prototype's runtime and model already cover
  locking several cells in one global order.
- **Hosted functions that take `Shared(s)`**, for metrics the platform reads.
- **A static check for read-then-act races** beyond the syntactic warning.

## Implementation plan

1. **Prototype.** Done for the semantics; see "What the prototype shows". The
   no-cycles rule needs a typed prototype or the checker itself.
2. **Builtins.** `Shared(s)` and its four functions, with their types.
3. **Checker.** The no-cycles rule, and the three warnings.
4. **Post-check IR.** Explicit updates, cleanup pushes and pops, certified
   (V3, V5).
5. **Runtime.** Cells, locking per build, waiters, cleanup stacks, poisoning
   and holder sets, checked against the model (V4, V7).
6. **Reference counting.** Cell stores as atomicity seeds in `ParallelTasks`
   builds.
7. **Documentation.** A langref section beside "Variables (with `var`)".

## References

- C. A. R. Hoare, *Towards a Theory of Parallel Programming*, in Operating
  Systems Techniques, Academic Press, 1972
- T. Harris, S. Marlow, S. Peyton Jones and M. Herlihy, *Composable Memory
  Transactions*, PPoPP 2005
