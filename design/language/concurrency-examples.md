# Concurrency examples

These examples test whether Roc's concurrency design covers the programs people
write, and whether it covers them the Roc way: fast, friendly and functional.
They follow the catalog of concurrent and distributed patterns that most
programs draw from, and they use the APIs of three RFCs:

- [Suspendable effects and platform-scheduled concurrency](concurrency-explicit-suspension.md):
  concurrent loops over plans, `spawn` and the `Concurrent` module.
- [Shared state](concurrency-shared-variables.md): `Shared(s)`, `update!`,
  `finally!` and `on_failure!`.
- [Sessions](concurrency-sessions.md): channels and mailboxes. If the
  shared-state RFC is adopted, it replaces mailboxes and keeps channels for
  streaming. So channels appear here only for streaming, and a mailbox appears
  once, for comparison.

No compiler checks these examples yet. Modules such as `Http`, `Sleep`,
`Clock`, `Fs`, `Net`, `Kv`, `Db` and `Rand` stand for platform functions that
suspend. All spellings are the RFCs' placeholders.

## What to look for

Each example should pass three tests:

- **Fast.** Work runs concurrently where it can and waits only where it must.
  No task polls or spins, and no task is woken for nothing.
- **Friendly.** It reads like the sequential program that does the same job.
  A mistake is a compile error or a crash, not a silent race or an unexplained
  hang.
- **Functional.** State is a value, decisions are pure functions, and effects
  stay at the edges.

Where an example fails one of these, the failure is listed in "What the
examples expose" at the end.

## Choosing a tool

| To | Use |
|---|---|
| Run a fixed set of effects and use every result | `Concurrent.both!`, `all!`, `all_ok!` |
| Take the first result and cancel the rest | `Concurrent.race!` |
| React to results as they arrive, stop early or add work | `for! x in plan { … }`, with `spawn` |
| Bound how many run at once | `plan.limit(n)`, or a semaphore across loops |
| Let several tasks change one piece of state | `Shared(s)` and `update!` |
| Wait until state changes | `update!` returning `Wait` |
| Undo a change however a task ends | `finally!`, `on_failure!` |
| Restart work after a bug | `Concurrent.supervise!` |

The shared-state RFC's two shorthands recur below: `h.get!()` is
`h.update!(|s| Keep(s))`, and `h.modify!(g)` is `h.update!(|s| Set(g(s), {}))`.

## 1. Fan-out and fan-in

### Several independent calls

```roc
dashboard! = |user_id| {
    user = Users.get!(user_id)?
    (orders, recs) = Concurrent.both!(|| Orders.recent!(user.id), || Recs.for_user!(user))
    Ok({ user, orders: orders?, recs: recs? })
}
```

The program is sequential where one call needs another's result, and
concurrent where the calls are independent. The calls themselves are the same
functions a sequential program uses.

### A bounded parallel map

```roc
mirror! : List(Str), Str => Try({}, [HttpErr(U16), FsErr(Str)])
mirror! = |urls, dir| {
    downloads = urls.map(|url| || (url, Http.get_bytes!(url)))
    for! (url, result) in downloads.concurrently().limit(8) {
        Fs.write_bytes!("${dir}/${file_name(url)}", result?)?
    }
    Ok({})
}
```

At most eight downloads run at once, and a new one starts as soon as one
finishes. The body handles one arrival at a time, so the writes need no lock.
The first error leaves the loop through `?`, which cancels the downloads still
running.

### Reduce as results arrive

```roc
word_counts! : List(Str) => Dict(Str, U64)
word_counts! = |paths| {
    var $total = Dict.empty()
    for! counts in paths.map(|path| || count_words(Fs.read_utf8!(path))).concurrently().limit(16) {
        $total = merge_counts($total, counts)
    }
    $total
}
```

`count_words` and `merge_counts` are pure. Counting happens inside each entry,
so in a `ParallelTasks` build it runs on several threads. Merging happens in
the body, one result at a time, in a `var`, so it needs no synchronization.

### Recursive fan-out

```roc
disk_usage! : Shared(U64), Str => U64
disk_usage! = |handles, path|
    match with_permit!(handles, || Fs.metadata!(path)) {
        File(size) => size
        Dir => {
            children = with_permit!(handles, || Fs.list_dir!(path))
            Concurrent.all!(children.map(|child| || disk_usage!(handles, child))).sum()
        }
    }
```

Each directory fans out to its children. Tasks are cheap, but file handles are
not, so a semaphore (`with_permit!`, from the shared-state RFC) bounds the open
handles across the whole tree. A `limit` would bound only one level.

### First success

```roc
first_success! : List(() => Try(a, e)) => Try(a, [AllFailed(List(e))])
first_success! = |attempts| {
    var $errors = []
    for! result in attempts.concurrently() {
        match result {
            Ok(value) => return Ok(value)
            Err(e) => { $errors = $errors.append(e) }
        }
    }
    Err(AllFailed($errors))
}
```

`race!` returns whichever finishes first, failures included. Waiting for the
first success is a loop that returns from its first `Ok`, which cancels the
attempts still running. Library combinators like this are ordinary functions.

### Scatter-gather with a deadline

```roc
search! = |shards, query, budget_ms| {
    var $hits = []
    var $heard = 0
    queries = shards.map(|shard| || Answered(Shard.search!(shard, query)))
    deadline = || { Sleep.millis!(budget_ms); Deadline }
    for! arrival in queries.append(deadline).concurrently() {
        match arrival {
            Answered(result) => {
                $hits = $hits.concat(result ?? [])
                $heard = $heard + 1
                if $heard == shards.len() { break }
            }
            Deadline => break
        }
    }
    { hits: $hits, missing: shards.len() - $heard }
}
```

The answer is whatever arrived in time. The deadline is one more entry, tagged
like the others. The loop has to `break` once every shard has answered,
because otherwise it would wait for the timer too.

## 2. Coordination primitives

In most languages a lock protects data. In Roc no data is ever shared mutably:
shared state lives in a `Shared` handle, and the function passed to `update!`
is the critical section. What remains of the classic primitives are ways to
wait for a condition. Each is a few lines over `update!`.

### A lock around effects

```roc
append_report! = |lock, lines|
    with_permit!(lock, || {
        for line in lines {
            Fs.append_line!(report_path, line)
        }
    })
```

When effects must not interleave, as several lines written to one file must
not, a semaphore with one permit serializes them. It is needed only for
effects: interleaving happens only at suspension points, and pure code is
never interrupted.

### Latch

```roc
Latch :: Shared(U64).{
    new! : U64 => Latch
    new! = |n| Latch.(Shared.new!(n))

    count_down! : Latch => {}
    count_down! = |Latch.(left)| left.modify!(|n| n - 1)

    wait! : Latch => {}
    wait! = |Latch.(left)| left.update!(|n| if n == 0 { Keep({}) } else { Wait })
}
```

Inside one loop, the end of the loop is already a latch. A `Latch` is for tasks
in different parts of the tree, such as request handlers that must not start
until a warm-up elsewhere has finished. One `count_down!` too many is a bug: it
overflows inside the update, so it crashes and changes nothing.

### Barrier

```roc
Barrier :: { parties : U64, cell : Shared({ arrived : U64, round : U64 }) }.{
    new! = |parties| Barrier.{ parties, cell: Shared.new!({ arrived: 0, round: 0 }) }

    arrive_and_wait! : Barrier => {}
    arrive_and_wait! = |Barrier.{ parties, cell }| {
        arrival = cell.update!(|b|
            if b.arrived + 1 == parties {
                Set({ arrived: 0, round: b.round + 1 }, Released)
            } else {
                Set({ ..b, arrived: b.arrived + 1 }, Waiting(b.round))
            })
        match arrival {
            Released => {}
            Waiting(round) =>
                cell.on_failure!(
                    |b| { ..b, arrived: if b.round == round { b.arrived - 1 } else { b.arrived } },
                    || cell.update!(|b| if b.round == round { Wait } else { Keep({}) }),
                )
        }
    }
}
```

The last task to arrive opens the next round. A task cancelled while waiting
takes its arrival back, so the barrier does not open early for the others. In
most languages that case is a known source of broken barriers. Here it is one
cleanup.

### Readers and writers, writers first

```roc
Rw : { readers : U64, writing : Bool, writers_waiting : U64 }

read! : Shared(Rw), (() => a) => a
read! = |rw, f| {
    rw.update!(|s|
        if s.writing or s.writers_waiting > 0 {
            Wait
        } else {
            Set({ ..s, readers: s.readers + 1 }, {})
        })
    rw.finally!(|s| { ..s, readers: s.readers - 1 }, f)
}

write! : Shared(Rw), (() => a) => a
write! = |rw, f| {
    rw.modify!(|s| { ..s, writers_waiting: s.writers_waiting + 1 })
    rw.on_failure!(|s| { ..s, writers_waiting: s.writers_waiting - 1 }, || {
        rw.update!(|s|
            if s.writing or s.readers > 0 {
                Wait
            } else {
                Set({ ..s, writing: True, writers_waiting: s.writers_waiting - 1 }, {})
            })
    })
    rw.finally!(|s| { ..s, writing: False }, f)
}
```

The policy is a record and two conditions, so a reader-first or a fair lock is
a change to the conditions. A writer that is cancelled while waiting withdraws
its claim, so readers are not blocked by a writer that has gone.

### Watching a value

```roc
Watched(a) : { version : U64, value : a }

publish! = |watch, value| watch.modify!(|w| { version: w.version + 1, value })

# Waits for a newer version than the one this task last saw.
changed! = |watch, seen| watch.update!(|w| if w.version == seen { Wait } else { Keep(w) })
```

A configuration reload, a leader address or a feature flag is a watched value.
A reader loops on `changed!`, passing the version it last saw. It never polls,
and it never misses the latest value, although it may skip the ones in between.

### A bounded queue

```roc
Queue(a) : { items : List(a), capacity : U64, closed : Bool }

push! : Shared(Queue(a)), a => [Pushed, Closed]
push! = |q, item| q.update!(|s|
    if s.closed {
        Keep(Closed)
    } else if s.items.len() == s.capacity {
        Wait
    } else {
        Set({ ..s, items: s.items.append(item) }, Pushed)
    })

pop! : Shared(Queue(a)) => [Item(a), Closed]
pop! = |q| q.update!(|s| match s.items {
    [item, .. as rest] => Set({ ..s, items: rest }, Item(item))
    [] if s.closed => Keep(Closed)
    [] => Wait
})

close! = |q| q.modify!(|s| { ..s, closed: True })
```

A full queue makes producers wait, and an empty one makes consumers wait: that
is backpressure. A non-blocking `try_push!` is the same function with
`Keep(Full)` in place of `Wait`. Whether a call waits is just the tag its
function returns.

## 3. Producers, consumers and dataflow

### Workers over a fixed list of jobs

```roc
process_all! = |jobs| {
    for! result in jobs.map(|job| || process!(job)).concurrently().limit(8) {
        record!(result)
    }
}
```

A worker pool over known jobs needs no queue and no workers: the plan and its
limit are the pool.

### Producers and consumers

```roc
import_all! = |records| {
    queue = Shared.new!({ items: [], capacity: 100, closed: False })
    produce = || {
        queue.finally!(|s| { ..s, closed: True }, || {
            for! record in records {
                _ = push!(queue, record)
            }
        })
        Produced
    }
    consume = || {
        var $stored = 0
        while True {
            match pop!(queue) {
                Item(record) => {
                    Db.insert!(record)
                    $stored = $stored + 1
                }
                Closed => break
            }
        }
        Stored($stored)
    }

    var $total = 0
    for! arrival in List.repeat(consume, 4).prepend(produce).concurrently() {
        match arrival {
            Stored(n) => { $total = $total + n }
            Produced => {}
        }
    }
    $total
}
```

`records` is a `Stream`. The producer closes the queue however it ends,
through `finally!`, so the consumers always finish. If a consumer crashes, the
loop crashes and cancels the producer: the failure is not silently lost
behind a queue.

### A pipeline

```roc
stage! = |input, output, f!| {
    output.finally!(|s| { ..s, closed: True }, || {
        while True {
            match pop!(input) {
                Item(x) => { _ = push!(output, f!(x)) }
                Closed => break
            }
        }
    })
}

run! = |lines| {
    parsed = Shared.new!({ items: [], capacity: 64, closed: False })
    enriched = Shared.new!({ items: [], capacity: 64, closed: False })
    _ = Concurrent.all!([
        || feed!(lines, parsed),
        || stage!(parsed, enriched, enrich!),
        || drain!(enriched, Db.insert!),
    ])
    {}
}
```

`feed!` pushes a stream's items and closes its queue, and `drain!` pops until
the queue closes. Each stage is a sequential loop, and the bounded queues between them carry
backpressure from the slowest stage back to the source. Fan-in is several
producers pushing to one queue. Fan-out is several consumers popping from it.

### Broadcast to subscribers that read at their own pace

```roc
Log(a) : { first : U64, items : List(a), cap : U64 }

publish! = |log, event| log.modify!(|l| {
    items = l.items.append(event)
    if items.len() > l.cap {
        { ..l, first: l.first + 1, items: items.drop_first(1) }
    } else {
        { ..l, items }
    }
})

# The events from index `next` on, waiting if there are none yet.
read_from! = |log, next| log.update!(|l|
    if next < l.first {
        Keep(Lagged(l.first))
    } else if next == l.first + l.items.len() {
        Wait
    } else {
        Keep(Events(l.items.drop_first(next - l.first)))
    })
```

Each subscriber keeps its own position in a `var` and reads every event after
it. No queue exists per subscriber, and a subscriber that falls more than `cap`
events behind is told so and skips ahead.

### Streaming over channels

The sessions RFC's channel protocols remain the tool for a stream between two
tasks, where each message carries the end the next one arrives on:

```roc
Stream(a) : [More(a, In(Stream(a))), Done]
```

A pipeline of many workers is simpler with the bounded queue above, which any
number of tasks can read. The shared-state RFC's "Channels as streams"
possibility would let `for!` consume a receiving end directly.

## 4. Resource control

The shared-state RFC already has a semaphore (`with_permit!`), a connection
pool (`with_conn!`) and a token-bucket rate limiter (`take_token!`). These add
the rest of the usual set.

### Load shedding

```roc
admit! = |slots, f| {
    admitted = slots.update!(|n| if n == 0 { Keep(Rejected) } else { Set(n - 1, Admitted) })
    match admitted {
        Admitted => Ok(slots.finally!(|n| n + 1, f))
        Rejected => Err(Overloaded)
    }
}
```

The semaphore, with `Keep(Rejected)` where it would have waited.

### Bulkheads

```roc
main! = |{}| {
    limits = { db: Shared.new!(20), search: Shared.new!(5), email: Shared.new!(2) }
    serve!(limits)
}

send_receipt! = |limits, order| admit!(limits.email, || Email.send!(receipt(order)))
```

A record of handles gives each dependency its own budget, so a slow email
service cannot take every slot that the database calls need.

### An adaptive limit

```roc
Limit : { limit : U64, in_flight : U64 }

with_adaptive_limit! = |cell, f| {
    cell.update!(|c|
        if c.in_flight >= c.limit {
            Wait
        } else {
            Set({ ..c, in_flight: c.in_flight + 1 }, {})
        })
    cell.on_failure!(|c| { ..c, in_flight: c.in_flight - 1 }, || {
        started = Clock.now!()
        result = f()
        slow = Clock.now!().since(started).to_millis() > 500
        cell.update!(|c| {
            limit =
                if slow or result.is_err() {
                    if c.limit > 1 { c.limit // 2 } else { 1 }
                } else {
                    c.limit + 1
                }
            Set({ limit, in_flight: c.in_flight - 1 }, {})
        })
        result
    })
}
```

Additive increase and multiplicative decrease: the limit grows while calls are
fast and halves when they slow down or fail. The policy is the pure
expression inside the last update, so it can be tested without running
anything.

## 5. Fault tolerance

### Retry with backoff

```roc
retry! : U64, U64, (() => Try(a, e)) => Try(a, e)
retry! = |attempts, delay_ms, f|
    match f() {
        Ok(value) => Ok(value)
        Err(e) if attempts <= 1 => Err(e)
        Err(_) => {
            Sleep.millis!(delay_ms + Rand.u64_below!(delay_ms))
            retry!(attempts - 1, delay_ms * 2, f)
        }
    }
```

It needs no concurrency feature. The sleep suspends, so other tasks run while
this one backs off.

### Deadlines

```roc
with_deadline! = |deadline, f|
    match Concurrent.race!(|| Finished(f()), [|| { Sleep.until!(deadline); Expired }]) {
        Finished(result) => result
        Expired => Err(DeadlineExceeded)
    }
```

A deadline is an `Instant` passed down to the calls that need it, and nested
deadlines nest. Losing the race cancels `f` and everything it started, at
their suspension points. No context object has to be threaded through for
cancellation to reach them, because the task tree already does it.

### Hedging and circuit breaking

Hedged requests are in the main RFC (`hedged_get!`), and the circuit breaker
is in the shared-state RFC (`guarded!`).

### Supervision trees

```roc
main! = |{}|
    Concurrent.supervise!({ max_restarts: 5 }, || {
        cache = Shared.new!(Dict.empty())
        _ = Concurrent.both!(
            || Concurrent.supervise!({ max_restarts: 100 }, || serve!(cache)),
            || refresh_forever!(cache),
        )
        {}
    })
```

- Wrapping one child in `supervise!` restarts only that child, like OTP's
  `one_for_one`. Here a crash in the server is restarted on its own.
- A crash that reaches an outer `supervise!` restarts everything under it, like
  `one_for_all`. Here a crash in the refresher restarts the server too, with a
  new cache.
- Nesting gives `rest_for_one`. In
  `Concurrent.supervise!(cfg, || Concurrent.both!(a, || Concurrent.supervise!(cfg, b)))`,
  a crash in `a` restarts `a` and `b`, but a crash in `b` restarts only `b`.

### Graceful shutdown

```roc
serve! = |listener, stopping, handle!| {
    accept = || Concurrent.race!(
        || Accepted(Net.accept!(listener)),
        [|| { wait_until_set!(stopping); Stopping }],
    )
    for! event in [accept].concurrently() {
        match event {
            Accepted(conn) => {
                spawn accept
                spawn || { handle!(conn); Handled }
            }
            Stopping => {}
            Handled => {}
        }
    }
}

wait_until_set! = |flag| flag.update!(|set| if set { Keep({}) } else { Wait })

main! = |{}| {
    stopping = Shared.new!(False)
    _ = Concurrent.both!(
        || {
            Signal.wait_for_term!()
            stopping.modify!(|_| True)
        },
        || Concurrent.race!(
            || { serve!(Net.listen!(8080), stopping, handle_request!); Drained },
            [|| { wait_until_set!(stopping); Sleep.millis!(30_000); Forced }],
        ),
    )
    {}
}
```

Shutdown stops the accept loop, after which the server loop ends by itself
when the last request finishes. If draining takes longer than 30 seconds,
the race cancels whatever is still running. Leaving the loop early would cancel
the requests in flight, so the loop does not leave early: it stops adding
work, and the requests finish.

### Sagas

```roc
book_trip! = |trip| {
    steps = [
        || Flights.book!(trip.flight).map_ok(|id| Flight(id)),
        || Hotels.book!(trip.hotel).map_ok(|id| Hotel(id)),
        || Cars.book!(trip.car).map_ok(|id| Car(id)),
    ]
    var $booked = []
    var $failed = False
    for! outcome in steps.concurrently() {
        match outcome {
            Ok(booking) => { $booked = $booked.append(booking) }
            Err(_) => { $failed = True }
        }
    }
    if $failed {
        for booking in $booked {
            cancel_booking!(booking)
        }
        Err(TripNotBooked)
    } else {
        Ok($booked)
    }
}
```

The loop does not leave at the first failure, because a cancelled booking may
or may not have happened. It waits for every outcome, then compensates the
ones that succeeded. If `book_trip!` itself is cancelled, the compensations do
not run: see "What the examples expose".

## 6. Caching

### Loading each key once

```roc
Entry(v) : [Loading, Ready(v)]

cached! : Shared(Dict(k, Entry(v))), k, (k => v) => v
cached! = |cache, key, load!| {
    lookup = cache.update!(|entries| match entries.get(key) {
        Ok(Ready(value)) => Keep(Hit(value))
        Ok(Loading) => Wait
        Err(KeyNotFound) => Set(entries.insert(key, Loading), Miss)
    })
    match lookup {
        Hit(value) => value
        Miss =>
            cache.on_failure!(|entries| entries.remove(key), || {
                value = load!(key)
                cache.modify!(|entries| entries.insert(key, Ready(value)))
                value
            })
    }
}
```

The first caller for a key loads it, and later callers wait for the result
instead of loading it again, a pattern often called single-flight. If the
loader crashes or is cancelled, its `Loading` mark is removed, and one of the
waiters becomes the loader instead. Expiry adds a time to `Ready`, passed in
from `Clock.now!()` by the caller, since an update cannot read the clock.

The same function makes message handling idempotent: `cached!(seen, msg.id,
|_| handle!(msg))` runs each message once, however many times it is delivered
concurrently.

### Refreshing in the background

```roc
serve_with_refresh! = |cache| {
    stale = Shared.new!({ items: [], capacity: 1_000, closed: False })
    _ = Concurrent.both!(|| serve!(cache, stale), || refresh_forever!(cache, stale))
    {}
}
```

A request that finds a stale entry returns it, and pushes the key onto `stale`
with a non-blocking push. A request cannot start work that outlives it, so the
background work is a sibling of the server, with an owner. It is cancelled when
the server is, and a crash in it is not lost.

## 7. Distributed systems

### Quorum reads with read repair

```roc
quorum_read! = |replicas, key, needed| {
    var $replies = []
    for! (replica, result) in replicas.map(|r| || (r, Kv.get_versioned!(r, key))).concurrently() {
        match result {
            Ok(versioned) => {
                $replies = $replies.append((replica, versioned))
                if $replies.len() == needed { break }
            }
            Err(_) => {}
        }
    }
    match newest($replies) {
        Ok(latest) if $replies.len() == needed => {
            stale = $replies.keep_if(|(_, v)| v.version < latest.version)
            _ = Concurrent.all!(stale.map(|(r, _)| || Kv.put_versioned!(r, key, latest)))
            Ok(latest.value)
        }
        _ => Err(NoQuorum)
    }
}
```

The read returns after `needed` replies and cancels the slower replicas. The
repair finishes before the call returns. To return sooner, the stale replicas
go onto a repair queue served alongside, as in the background refresh above.
The write side is the main RFC's `write_quorum!`.

### Two-phase commit

```roc
commit! = |participants, txn| {
    votes = Concurrent.all!(participants.map(|p| || Participant.prepare!(p, txn)))
    decision = if votes.all(|vote| vote == Ok(Yes)) { Commit } else { Abort }
    Log.write_decision!(txn, decision)
    _ = Concurrent.all!(participants.map(|p| || Participant.finish!(p, txn, decision)))
    decision
}
```

The protocol reads as its specification: prepare everywhere, decide, make the
decision durable, then tell everyone. `all!` rather than `all_ok!`, because
every participant must hear the decision, including those that voted no.

### Holding a leader lease

```roc
lead! = |lease, work!|
    Concurrent.race!(|| { work!(); Finished }, [|| { keep_lease!(lease); LostLease }])

keep_lease! = |lease|
    match Lease.renew!(lease) {
        Ok({}) => {
            Sleep.millis!(lease.ttl_ms // 3)
            keep_lease!(lease)
        }
        Err(_) => {}
    }
```

Losing the lease ends the race, which cancels the leader's work at its next
suspension point. Requests it already sent stay sent, so storage still needs
fencing tokens, as it does in any language.

### Replicating a log

```roc
Replication : { log : List(Entry), matched : Dict(Peer, U64), commit : U64 }

append! = |rep, entry| {
    index = rep.update!(|r| Set({ ..r, log: r.log.append(entry) }, r.log.len() + 1))
    rep.update!(|r| if r.commit >= index { Keep({}) } else { Wait })
}

replicate_to! = |rep, peer, cluster_size| {
    var $sent = 0
    while True {
        batch = rep.update!(|r| if r.log.len() == $sent { Wait } else { Keep(r.log.drop_first($sent)) })
        match Rpc.append_entries!(peer, $sent, batch) {
            Ok({}) => {
                $sent = $sent + batch.len()
                rep.modify!(|r| {
                    matched = r.matched.insert(peer, $sent)
                    { ..r, matched, commit: majority_index(matched, cluster_size) }
                })
            }
            Err(_) => Sleep.millis!(50)
        }
    }
}
```

This is the leader's side of Raft, without terms or elections. The leader runs
one `replicate_to!` per follower, and `majority_index` is a pure function. A
client's `append!` waits until a majority holds its entry. Every follower loop
is sequential code, and all the coordination is in one record.

### A service that owns its effects

```roc
account! = |inbox, balance|
    match inbox.receive!() {
        Got(Deposit(n), rest) => account!(rest, balance + n)
        Got(Withdraw(n, reply), rest) =>
            if n <= balance {
                reply.send!(Ok({}))
                account!(rest, balance - n)
            } else {
                reply.send!(Err(Insufficient))
                account!(rest, balance)
            }
        Closed => balance
    }
```

The actor style uses the sessions RFC's mailboxes. The same account with
shared state is one update:
`balance.update!(|b| if n <= b { Set(b - n, Ok({})) } else { Keep(Err(Insufficient)) })`.
Shared state is shorter when the transition is pure. A service whose
transitions perform effects, like a connection that must write each message in
order, is where a mailbox fits.

## 8. Classic problems

### Dining philosophers

```roc
philosopher! = |table, seat, seats, meals| {
    left = seat
    right = (seat + 1) % seats
    for _ in 0..<meals {
        think!(seat)
        table.update!(|taken|
            if taken.contains(left) or taken.contains(right) {
                Wait
            } else {
                Set(taken.insert(left).insert(right), {})
            })
        table.finally!(|taken| taken.remove(left).remove(right), || eat!(seat))
    }
}
```

The table is one handle holding the set of forks in use. Both forks are taken
in one update, so the deadlock the problem is famous for cannot happen. The
rule that an update uses exactly one handle makes the correct solution the
obvious one.

### A crawler with bounded fetches and no shared state

```roc
crawl! = |start, max_in_flight| {
    fetch = |url| || (url, Http.get_utf8!(url))
    var $seen = Set.single(start)
    var $waiting = []
    var $in_flight = 1
    var $pages = []
    for! (url, result) in [fetch(start)].concurrently() {
        $in_flight = $in_flight - 1
        match result {
            Ok(html) => {
                $pages = $pages.append(url)
                for link in links(html) {
                    if !$seen.contains(link) {
                        $seen = $seen.insert(link)
                        $waiting = $waiting.append(link)
                    }
                }
            }
            Err(_) => {}
        }
        while $in_flight < max_in_flight {
            match $waiting {
                [next, .. as rest] => {
                    spawn fetch(next)
                    $waiting = rest
                    $in_flight = $in_flight + 1
                }
                [] => break
            }
        }
    }
    $pages
}
```

The visited set, the frontier and the count in flight are `var`s in the loop
body, which handles one arrival at a time. Nothing is shared and nothing is
locked. The loop ends when the last fetch has arrived and nothing is waiting.

### The others

- **Bounded buffer:** the bounded queue.
- **Sleeping barber:** the bounded queue with `try_push!`, where a full waiting
  room turns customers away.
- **Readers and writers:** `read!` and `write!`.
- **Cigarette smokers and other multi-resource problems:** one handle holding
  every resource, taken in one update, as with the philosophers.
- **Parallel tree and graph walks:** recursive fan-out, or the crawler.

## 9. Time

### A periodic task without drift

```roc
every! = |period_ms, f!| {
    var $next = Clock.now!().add_millis(period_ms)
    while True {
        Sleep.until!($next)
        f!()
        $next = $next.add_millis(period_ms)
    }
}
```

### Debouncing

```roc
debounce! = |events, quiet_ms, flush!| {
    var $pending = []
    var $generation = 0
    for! arrival in [|| Event(pop!(events))].concurrently() {
        match arrival {
            Event(Item(e)) => {
                $pending = $pending.append(e)
                $generation = $generation + 1
                spawn || Event(pop!(events))
                spawn || { Sleep.millis!(quiet_ms); Quiet($generation) }
            }
            Event(Closed) => {
                if !$pending.is_empty() { flush!($pending) }
                break
            }
            Quiet(generation) if generation == $generation and !$pending.is_empty() => {
                flush!($pending)
                $pending = []
            }
            Quiet(_) => {}
        }
    }
}
```

Each event starts a timer tagged with the generation it belongs to. A timer
from an older generation arrives and is ignored. The spawned closure captures
`$generation` as it was when the closure was created, which is the value the
timer needs. Throttling is the rate limiter, and timeouts are deadlines.

## Coverage

| Area | Patterns | Shown by |
|---|---|---|
| Fan-out and fan-in | parallel calls, bounded map, map-reduce, recursive fan-out, first success, scatter-gather | Section 1 |
| Coordination | mutex, semaphore, latch, barrier, readers–writer lock, condition variable, watch, bounded queue | Section 2, shared-state RFC |
| Dataflow | worker pool, producer–consumer, pipeline, fan-in, fan-out, broadcast, channel streams | Section 3, sessions RFC |
| Resource control | semaphore, pool, rate limiter, load shedding, bulkhead, adaptive limit | Section 4, shared-state RFC |
| Fault tolerance | retry, deadline, timeout, hedging, circuit breaker, supervision, graceful shutdown, saga | Section 5, main RFC |
| Caching | single-flight, TTL, background refresh, idempotent handling | Section 6 |
| Distributed systems | quorum read and write, read repair, two-phase commit, leases, log replication, actors | Section 7, main RFC |
| Classic problems | dining philosophers, crawler, bounded buffer, sleeping barber, readers–writers | Section 8 |
| Time | periodic task, debounce, throttle, timeout | Section 9 |

## What the examples expose

The examples need no new primitive. They do expose places where the design is
slower, wordier or weaker than it could be, roughly in order of importance.

1. **Updates may copy large state.** The bounded queue, the log, the cache
   and the replication record all change a collection inside `update!`. An
   update that crashes changes nothing, so the cell must still hold the old
   state if the function crashes. If the cell keeps its own reference for that
   while the function runs, the function never has the only reference, and
   every `append` or `insert` copies the whole collection. `Keep` and `Wait`
   paths could hand the state back unchanged, but a crash halfway through an
   in-place change cannot. The alternative is to lend the state uniquely and
   poison the handle when an update crashes, as a crashing cleanup already
   does. The shared-state RFC leaves this as open decision (f), and the choice
   decides whether queues and caches in `Shared` cost O(1) or O(n) per
   operation.
2. **Waking many tasks is serialized.** A barrier opening, a watched value
   changing, a log gaining an event, or a cache entry becoming ready all release
   many waiters whose updates keep the state. Under the settled wake rule each
   is readied only after the one before it has run and passed the wake on, so
   releasing `n` waiters takes `n` dispatches one after another, and a
   `ParallelTasks` build cannot release them in parallel. Readying at once every
   waiter whose probe would `Keep`, together with the first that would `Set`,
   would fix this at the cost of probing every waiter. It belongs with open
   decision (c).
3. **Waiters on unrelated keys are probed.** In `cached!`, every store to the
   dictionary probes the waiters for every other key. Holding one handle per
   key inside an outer handle (`Shared(Dict(k, Shared(Entry(v))))`) avoids it,
   and a state may hold handles, but nothing points programmers there.
4. **No fairness.** Most-recent-first waking can starve an old waiter under
   steady contention: a philosopher, a writer, a pool borrower. A program that
   needs fairness can take a ticket in one update and wait for its turn in
   another, but it must then use `on_failure!` to give the turn up if it is
   cancelled. A `Fair` variant in the library would save every program from
   writing that.
5. **`spawn` ignores limits.** The crawler counts what it has in flight by
   hand, because an entry added with `spawn` runs under no limit. A loop over a
   limited plan that also spawns is easy to misread as bounded. `spawn` could
   inherit the limit of the plan the loop runs over.
6. **A limited `all!`.** `Concurrent.all!` takes a list, so it cannot be
   limited, and getting a limited plan's results in input order takes a loop
   that stores each result by index. `Concurrently.all! : Concurrently(a) =>
   List(a)` would let any plan be collected in order.
7. **One entry cannot be cancelled on its own.** The debouncer leaves a
   sleeping task for every event and ignores the stale ones, and graceful
   shutdown races each accept against the stop flag. Both work, and both cost
   a task or a race that a way to cancel one entry would save. Such a
   cancellation would need a name for an entry, which the loop deliberately
   lacks.
8. **Compensation cannot run on cancellation.** Cleanups are pure, so a saga
   whose caller cancels it cannot undo the bookings it made. This follows from
   silent cancellation, which the main RFC chose deliberately. A saga must
   either run where nothing cancels it, or record each step durably so that a
   recovery task compensates later, as sagas do in practice.
9. **One handle per update.** The philosophers and the replication record fit
   in one handle. A transfer between two accounts held in separate handles
   does not, so it must use a record of both, which is the shared-state RFC's
   `update2!` future possibility.
10. **Supervision has no restart window or backoff.** OTP limits restarts per
    period of time, and production supervisors wait between restarts.
    `supervise!` counts restarts for its whole life. Both would be fields of
    its configuration record.
11. **Pure parallel computation is out of scope.** Sorting or transforming
    large data in parallel, without effects, is the main RFC's future
    deterministic parallelism, not something these features give.

Everything else reads like the sequential program. The concurrency is in a
plan, a loop or an update, and the state is a value. Most examples in sections
1, 3, 5 and 7 do not mention shared state at all, because the loop body
already sees one arrival at a time.
