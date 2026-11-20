# RFC: Shared State

| | |
|---|---|
| Status | Draft. Not adopted; nothing here is part of `design.md`. |
| Scope | The builtin `Shared(s)` type and its functions (`new!`, `update!`, `finally!`, `on_failure!`), atomic updates, waiting, cleanup on cancellation and crash, poisoning, their runtime representation and lowering, and the changes they make to the companion RFCs |
| Companions | [Suspendable Effects and Platform-Scheduled Concurrency](concurrency-explicit-suspension.md): the task tree, plans, concurrent loops and crashes this RFC builds on. [Sessions, Channels and Shared Resources](concurrency-sessions.md): if this RFC is adopted, it replaces that RFC's mailboxes and its shared-resource libraries; channels remain for streaming |
| Prototype | [`concurrency-prototype/`](concurrency-prototype/): `shared.py`, `shared_model.py`, `cycles.py`, the libraries in `library.py`, and the `SharedVariables`, `SharedConveniences`, `SharedModel` and `NoCycles` tests. See "What the prototype shows" |

## Summary

The companion RFC's tasks share only immutable values. That covers fan-out,
races and supervision, but not a resource that many tasks use while they run:
a semaphore, a rate limiter, a pool, a circuit breaker shared across requests.
The sessions RFC builds each of those as a server task that other tasks reach
through a mailbox. This RFC instead adds one builtin opaque type, four
functions and two shorthands, and no syntax:

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

Crashes keep OTP's semantics. An update that crashes changes nothing, so
shared state is never left half-updated. A cleanup that crashes poisons the
state, so a broken cleanup obligation becomes a crash at its next use instead
of a silent leak.

Two type rules keep reference counting complete: a state cannot hold a
function, and no type can reach its own handle through a state. So cells never
form a cycle, and each is freed when its last handle is.

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
   type and its functions, and one inferred requirement on the types a state
   holds. No effect polymorphism, no non-escaping types, no implicit
   conversions.
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
for it. The functions are pure, so the runtime can ask each one, most recent
waiter first, what it would do with the new state, without storing anything.
It wakes only the first that would not wait, which then runs its function for
real. Ten tasks waiting for one permit cost one wakeup per
release, not ten.

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
    server.hits.modify!(|n| n + 1)
    guarded!(server.breaker, Clock.now!(), || route!(req))
}
```

Two handles from two calls to `Shared.new!` are different state, even if they
start equal. Copying a handle copies the handle, not the state.

### What a state can hold

Reference counting cannot free a cycle, and before `Shared` Roc had none: an
immutable value cannot come to hold itself. A state can, if an update stores a
value that reaches the same handle. Two type rules prevent it:

- **A state holds no function**, anywhere inside it, including inside the
  state of another handle it holds. A function's type does not say what it
  captures, so any function in a state could capture that state's own handle.
- **No type reaches its own handle through a state.** This is an error:

```roc
Node : [Nil, Next(Shared(Node))]
```

A `Node` could be stored in the state its own `Next` points to.

Handles inside states are fine when they cannot lead back. A registry of
sessions, `Shared(Dict(Str, Shared(Session)))`, is valid as long as `Session`
holds no function and no `Shared(Dict(Str, Shared(Session)))`. A queue of jobs,
`Shared(List(() => {}))`, is not valid, even if no job captures anything.

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
  read of the same handle, as in `n = c.get!()` followed by
  `c.modify!(|_| n - 1)`. That is a lost update. The warning suggests deciding
  inside one update.
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
- **No cycles.** For every `Shared(s)` the program forms:
  1. No function type is reachable from `s` through records, tag unions,
     lists, the contents of opaque types, and the state types of handles.
  2. `Shared(s)` is not reachable from `s` through the same constructs.

  A reference cycle between cells needs a path from a state back to its own
  handle. That path runs through types, which rule 2 forbids, or through a
  function's captures, which rule 1 forbids. See "No cycles" below for how the
  checker applies the rules.
- Hosted function signatures cannot mention `Shared` in the first version.

### `update!`

`h.update!(f)` runs as if no other task ran during it:

1. Acquire `h` (see "Runtime").
2. If `h` is poisoned, release it and crash.
3. Call `f` on the state. While `f` runs, the state is borrowed: `f` sees the
   current value and cannot be told apart from a call on an ordinary value.
4. On `Set(new, r)`: store `new`, release the old state, wake waiters as
   "Waking" under "Runtime" describes, release `h`, and return `r`.
5. On `Keep(r)`: release `h` and return `r`.
6. On `Wait`: register the task as a waiter on `h` while still holding it,
   release `h` and park. When another task's update stores to `h`, or `h` is
   poisoned, the waiter may be readied (see "Waking" under "Runtime"). A
   readied waiter starts again at step 1.

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

### No cycles

The checker applies the two rules once per declaration, as it checks a
declaration's other properties, and never to expanded types:

- **Declarations.** A type declaration is checked with its parameters as
  leaves. Each earlier declaration it uses is replaced by a summary: whether
  its values hold a function, which of its parameters its values hold, and
  which parameters end up inside a state. A type can reach itself only
  through a recursive declaration, or a recursive tag union the solver infers
  (checked where it is inferred), so every cycle is seen where it is declared.
- **Requirements.** A parameter that ends up inside a state passes the rules
  on to its arguments. `Box(a) : { h : Shared(a) }` is valid; `S : [Nil,
  N(Box(S))]` is an error at `S`, and so is `Box(() => {})` wherever it is
  written. Generic code that calls `Shared.new!` on a value of type `a` gets
  the same requirement on `a`, inferred and passed on to its callers like a
  static dispatch requirement.
- **Phantom parameters.** A parameter that a declaration's values never hold,
  and that no state of it holds, places no requirement. Its arguments are
  still types the program writes, and are checked like any other.

The rules need no lambda sets, and could not use them: `.boxy` builds never
compute lambda sets, and a program's validity cannot depend on how it is
lowered. That is why rule 1 is conservative. Without lambda sets, the checker
cannot tell a function that captures nothing from one that captures the
handle.

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

**Waking.** Waiters are kept in the order they began waiting. After a `Set`,
while still holding the cell, the runtime probes them, most recent first: it
calls each waiter's function on the new state and discards the result,
storing nothing. It readies the first waiter whose function would not answer
`Wait`, and stops. Most recent first is the order of a `None` build's one
stack, where a task that began waiting later is above the earlier one and
runs first, so C9 holds (V8). A waiter that waits again moves to the most
recent place, as it stays on top of the stack in a `None` build. The readied waiter is marked as carrying the wake:

- If it stores, its own `Set` probes the remaining waiters.
- If it ends without storing, because it answers `Keep`, answers `Wait`
  again (another task changed the state first), crashes, or is cancelled
  before it runs, the runtime probes on from there, so that the wake is never
  lost with it.

Probing is exact: a pure function given the same state and captures gives
the same answer. It commits nothing on a waiter's behalf, so a waiter
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
- `update!` lowers to an acquire, a load of the state, a call of the function,
  a `match` on its tag with a store, release and wake on `Set`, a release on
  `Keep`, and a register, release and suspend followed by a jump back to the
  acquire on `Wait`. In `None` and `Tasks` builds, acquire and release are
  omitted; the `concurrency` field is already an explicit input to every
  unit's compilation.
- `finally!` and `on_failure!` lower to a push, the call of `body`, and a pop
  followed by the cleanup update (`finally!`) or by dropping the entry
  (`on_failure!`). Push and pop are balanced by construction, and certified.
- `get!` and `modify!` are library functions that call `update!`.

Backends see calls to builtins and ordinary `incref` and `decref` statements.
They need no new knowledge of reference counting.

### Reference counting

In `ParallelTasks` builds, a cell and every value stored in it can be reached
by tasks on several threads. Storing a value into a cell is an atomicity seed,
like a hosted argument in the companion RFC's C6, and cells always use atomic
counts. In `None` and `Tasks` builds, nothing changes.

Because of the no-cycles rules, cells form no cycles, and reference counting
frees every cell once the last handle to it is gone (V9).

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
| V7 | **No lost wakeups.** A waiter registers before it releases its cell, so a store that follows the release always probes it. A waiter that a probe readies and that ends without storing, including by cancellation, probes on, so a waiter whose function would not wait is never left parked | Runtime invariant | Model (readying every waiter); tests and differential runs (readying the first) |
| V8 | **Sequential equivalence.** C9 holds for programs that use `Shared`: a `None` build's `Wait` runs loop entries on demand, and is a deadlock only where the input-order schedule deadlocks | Language guarantee | Differential |
| V9 | **No cycles.** No state holds a function, and no type reaches its own handle through a state, so cells form no reference cycles and each is freed when its last handle is | Language guarantee | Typing rules; a typed model against a counting heap |

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
- **Changing a large state copies it.** The update borrows the state, so it
  never holds the only reference, and appending to a queue in a handle copies
  the whole queue. See open decision (f).
- **No fairness among waiters.** The most recent waiter is probed first, so
  under sustained contention an early waiter can wait indefinitely while
  later ones are served. Readying every waiter promised nothing either: the
  schedule chose. A fair order would break C9, because a `None` build can
  only run the most recent waiter.
- **Stores run other tasks' functions.** After each `Set` on a cell with
  waiters, the storing task probes them, each inside a crash boundary. Under
  contention that is a quadratic number of pure calls, though a linear
  number of task dispatches, and in `ParallelTasks` builds it lengthens the
  time the lock is held.
- **Code runs during cancellation.** Cleanups are pure, but they are still Roc
  code running in a dropped task.
- **States cannot hold functions.** A job queue or a table of callbacks in
  shared state is a type error, even when no function in it captures a
  handle, because the checker cannot see captures. Among generated types that
  could never form a cycle, the rule rejected about one in ten; real programs
  will differ.
- **A new kind of inferred requirement.** Generic code that puts a type
  variable inside a state passes a requirement on to its callers, and error
  messages must explain it.
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
| Readying every waiter at each `Set` | Under contention, every waiter is dispatched after every `Set`, and all but one answer `Wait` again: dispatches grow with the square of the waiters. See "What the prototype shows" |
| Requiring equality on the state, and parking again a dispatched waiter whose state equals the one it waited on | Replaces running the function with a comparison, but the waiter is still dispatched to make it, so dispatches stay quadratic. It also restricts state types and keeps old states alive |
| Readying only waiters whose state changed, compared at the `Set` | Never skips anything in the libraries: each `Set` that matters changes the state, and the waste comes from waiters that run after the first one took what the `Set` gave |
| Running the first eligible waiter's update at the `Set` and handing it the result, as the sessions RFC's semaphore cell hands over a permit | Commits an update on behalf of a task that has not run. If that task is cancelled first, its update stays committed and the `finally!` after it never runs, so a permit is lost. The semaphore cell can give a permit back; a pure update has no inverse |
| Probing the oldest waiter first, for fairness | A `None` build runs only the most recent waiter, so a `Tasks` build that served the oldest could not follow the input-order schedule, and C9 fails. The fuzzer found such a program |
| Waking one waiter without probing, which passes the wake on if it waits again | Each `Set` by the woken waiter starts the chain again, so a semaphore still dispatches every waiter on every release |
| Deciding cycles from lambda sets, which show what each function captures | Exact, as the prototype shows, and it would allow functions in states. But only `.lss` builds compute lambda sets, and a program's validity cannot depend on how it is lowered |
| Checking each instantiation of a generic type when it is specialized | `.boxy` builds compile each generic function once and never specialize. Summaries and requirements give the same answer once per declaration, and the prototype checks that they agree |
| A cycle collector for cells | Roc has no tracing collector, and cells would be the one thing that needed one |
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
| `h.get!()`, `h.modify!(g)` | `sv_get` and `sv_modify` in `library.py`, steps whose functions answer `Keep(s)` and, in the IR's order, `Set({}, g(s))`; the generated programs' peeks and increments |
| Reading in a test | `(read h)` |

`certify_defers` checks that every `defer` is matched on every path, so the
IR's push and pop stay balanced as the builtins guarantee by construction. The
IR also allows a step over several cells, which the generated programs use to
exercise the lock order.

**Libraries** (`library.SHARED_SOURCE`): a semaphore, a pool with a lost count
and `on_failure!`, the same pool with `finally!`, and the circuit breaker.

**Tests** (`SharedVariables`, 17 tests, `SharedConveniences`, `SharedModel`
and `NoCycles`):

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
- 20 clients contend for one permit. Readying only the most recent waiter
  that a probe finds would not wait takes 80 task resumes and runs 19
  updates again;
  readying every waiter, the regression switch, takes 251 and 190.
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
| Semaphore, server task (sessions RFC) | 93 | 276 | 721 | 90 | | |
| Semaphore, the runtime's semaphore cell (sessions RFC) | 30 | 120 | 187 | 0 | | |
| Semaphore, `Shared`, every waiter readied | 30 | 526 | 652 | 0 | 495 | |
| Semaphore, `Shared`, every waiter readied, equal states compared | 30 | 526 | 246 | 0 | 89 | |
| Semaphore, `Shared`, most recent waiter that would not wait (the rule) | 30 | 120 | 681 | 0 | 89 | 435 |
| Pool, server task (sessions RFC) | 93 | 276 | 754 | 90 | | |
| Pool, `Shared`, every waiter readied | 30 | 497 | 775 | 0 | 466 | |
| Pool, `Shared`, every waiter readied, equal states compared | 30 | 497 | 397 | 0 | 88 | |
| Pool, `Shared`, most recent waiter that would not wait (the rule) | 30 | 119 | 887 | 0 | 88 | 406 |

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
- Readying only the most recent waiter that a probe finds would not wait
  makes resumes linear: the semaphore then matches the runtime's semaphore cell.
  The cost moves to probes, pure calls that are still about n², and to the
  allocations they make for the results they discard.
- Under the input-order schedule, the server tasks deadlock, as they do in a
  `None` build; `Shared` does not.

With the rule as the prototype's default, the suite passes, and so do 2,000
fuzzed programs: 1,964 compared against the input-order schedule (C9),
15,924 cancelled at a suspension point, 5,028 probes, and 336 wakes passed on
by a readied waiter that was dropped before it ran.

**No cycles** (`cycles.py`). The prototype's IR is untyped, so the rule has a
model of its own. It generates type declarations, in a library group and a
program group, each possibly recursive and generic, and closed types over
them, by composing records, tag unions, lists, handles, function types and
declared types at random. Function types carry their lambda sets, which a Roc
type would not show. For each type it decides three ways:

- **With lambda sets**, where a function reaches its captures: no handle lies
  on a cycle of the type graph.
- **The checker's rules**, without lambda sets, on the expanded type.
- **The checker's rules per declaration**, with summaries and requirements,
  as "No cycles" describes.

It runs random programs over each accepted type on a heap that counts
references: they make values, store new states that may hold any handle the
program has, the cell's own included, read states back and drop handles. When
the program ends, any cell reference counting cannot free is a leak. For each
rejected type it builds a witness: a cell whose state reaches its own handle
along the cycle.

Over 100,000 generated types:

| | Types | Result |
|---|---|---|
| Accepted with lambda sets | 87,114 | 348,456 random programs, 1,518,146 cells, no leak |
| Rejected with lambda sets | 12,886 | Every witness leaks; 3,436 random programs leak too |
| Accepted by the checker's rules | 78,755 | Each also accepted with lambda sets |
| Rejected by the checker's rules only | 8,359 | Every one for a function in a state |
| Per declaration against expanded types | 100,000 | The same answer for every type |

So the rule with lambda sets is exact, the checker's rules are sound, and
checking each declaration once suffices. Dropping rule 1 accepts types whose
cells form a cycle through captures. Dropping the requirements on parameters
accepts uses that put a function or a cycle inside a state. The tests catch
both. Building the model found three things the checker must do: count a
declared type's arguments as types the program writes, even for a parameter
the declaration ignores; give a phantom parameter no requirement; and keep
the requirement of a parameter that appears only inside a function's captured
state.

## Decisions left to review

Names are placeholders. The prototype settled six earlier questions:

- **Cleanup that knows how its body ended.** Needed: the pool must not lend a
  connection whose user crashed. `on_failure!` is the second form.
- **Block or function scope for cleanup.** Moot: a cleanup belongs to the
  closure passed to `finally!` or `on_failure!`.
- **Updates over several handles.** Not needed by the semaphore, pool or
  breaker. One handle per update keeps locks from nesting.
- **Waking waiters.** After a `Set`, the runtime probes the waiters, most
  recent first, and readies the first whose function would not wait; a
  readied waiter that ends without storing probes on. Probing the oldest
  first broke C9 on a fuzzed program. Readying every waiter makes
  dispatches quadratic under contention. Comparing states, which the earlier
  proposal needed equality for, leaves dispatches quadratic. Handing a result
  to a waiter at the `Set` loses permits when the waiter is cancelled. See
  "Runtime" and the alternatives table.
- **The no-cycles rule for generic code.** Checked once per declaration, with
  summaries of earlier declarations and requirements on parameters, and an
  inferred requirement on type variables in generic code. It agrees with
  checking every expanded type on 100,000 generated types. It cannot use
  lambda sets, so states hold no functions.
- **Conveniences.** `get!` and `modify!` are in, defined with `update!`.
  `get!` also makes reading and then acting separately easier to write, so
  the read-then-write warning covers it.

Still open:

- **(a) Detecting a `Wait` that can never end across roots.** Per-root
  detection with holder sets is implemented and tested within one root. A
  root waiting on a handle that a task of another root holds is untested for
  cells, though tested for the sessions RFC's ports, which use the same sets.
- **(b) Contention under `ParallelTasks`:** whether spinning then yielding is
  enough, and how a hot handle compares with a semaphore task. The Python
  prototype cannot measure it; it needs the real runtime.
- **(c) The cost of probing.** Probes are still about n² pure calls under
  contention, each inside a crash boundary and, under `ParallelTasks`, under
  the cell's lock. Options to cut them: skip a waiter whose state equals the
  one it waited on, for state types that have equality, which saves calls but
  keeps old states alive; or let a library declare that its waiters are
  interchangeable, so that a probe that finds one waiter waiting stops. The
  real runtime should measure whether either is worth it.
- **(d) Channel ends in states.** An end reaches the values in its channel's
  buffer, so the rules should treat it as holding its message type. The
  sessions RFC's ends also have holder sets, which this model does not
  include. Not prototyped.
- **(e) Whether functions in states are worth a finer rule.** Job queues and
  callback tables are the cases rule 1 rejects. If real programs need them in
  shared state, a finer rule needs to know captures, and so would have to
  work without lambda sets in `.boxy` builds.
- **(f) Changing a large state in place.** In step 3 of `update!`, `f`
  borrows the state, so the cell still holds it if `f` crashes, and an update
  that crashes changes nothing. But `f` then never holds the only reference.
  Every `Set` that appends to a list or inserts into a dictionary copies all
  of it. A queue, a log or a cache in a handle costs O(n) per update, not
  O(1). The bounded queue, the broadcast log, the single-flight cache and the
  replicated log in [the examples](concurrency-examples.md) all do this.
  - **Move the state into `f`.** `f` would return the state along with its
    answer, and lowering would add the return to the `Keep` and `Wait` paths.
    A unique state then changes in place, as a unique `var` does. The cost is
    rollback: if `f` crashes after changing the state, the old state is gone,
    so the handle is poisoned, as when a cleanup crashes. "An update that
    crashes changes nothing" would no longer hold.
  - **Keep borrowing, and recommend collections that share structure.** A
    persistent queue or map makes each copy O(log n) and keeps rollback. But
    the cost is easy to miss, and the obvious `List` costs O(n).

  The prototype counts operations, not copies, so it cannot decide this. A
  benchmark in the real runtime of a queue in a handle under each option
  can.

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

1. **Prototype.** Done; see "What the prototype shows".
2. **Builtins.** `Shared(s)`, its four functions and the two shorthands, with
   their types.
3. **Checker.** The no-cycles rules, per declaration with summaries, the
   inferred requirement on type variables, and the three warnings.
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
