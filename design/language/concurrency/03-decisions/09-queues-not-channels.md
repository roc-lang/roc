# 9. Queues, Not Channels

| | |
|---|---|
| Status | Settled by argument. `Queue` and `Reply` are library code over primitives the prototype checks; the prototype doesn't run them as written |
| Builds on | [Structured tasks](03-structured-tasks.md), [Shared state](06-shared-state.md), [Waking waiters](07-waking-waiters.md) |
| Detail | [Why not channels for streams](../../concurrency-shared-variables.md#why-not-channels-for-streams), [Streams between tasks](../../concurrency-shared-variables.md#streams-between-tasks), [The library](../../concurrency-shared-variables.md#the-library), [Drawbacks](../../concurrency-shared-variables.md#drawbacks); the superseded [sessions RFC](../../concurrency-sessions.md#protocols-by-passing-ends) |

A child task's only output is its result. How do tasks that are both still running stream values to each other, and how does one task ask another for an answer?

## The Decision

Two library types over `Shared`, with no mechanism of their own.

A `Queue(a)` is a cell holding `{ items : List(a), capacity : U64, closed : Bool }`:

| Function | What it does |
|---|---|
| `push!` | Adds an item, waiting while the queue is full. Answers `Pushed` or `Closed` |
| `try_push!` | Like `push!`, but answers `Full` instead of waiting |
| `pop!` | Takes the oldest item, waiting while the queue is empty. Answers `Closed` once the queue is closed and empty |
| `close!` | Closes the queue |
| `closing!(f)` | Runs `f`, and closes the queue however `f` ends: by returning, by cancellation or by a crash |
| `stream` | Makes the queue a `Stream`, so `for! item in queue` reads it until it's closed and empty |

Each is a few lines of `update!`. Here's `pop!`:

```roc
pop! = |Queue.(cell)| cell.update!(|q| match q.items {
    [item, .. as rest] => Set({ ..q, items: rest }, Item(item))
    [] if q.closed => Keep(Closed)
    [] => Wait # Empty and still open: wait for a push.
})
```

A producer and four consumers:

```roc
import_all! = |lines| {
    queue = Queue.new!(100)
    produce = || queue.closing!(|| { # Closes the queue however the producer ends.
        for! line in lines {
            match queue.push!(parse(line)) { # Waits while the queue is full.
                Pushed => {}
                Closed => break
            }
        }
    })
    consume = || {
        for! record in queue { # Ends once the queue is closed and empty.
            Db.insert!(record)
        }
    }
    _ = Concurrent.all!(List.repeat(consume, 4).prepend(produce))
    {}
}
```

A `Reply(a)` is a one-shot cell holding `[Pending, Done(a), Dropped]`. A request that needs an answer carries one:

```roc
ask! = |requests, request| {
    reply = Reply.new!()
    match requests.push!((request, reply)) {
        Pushed => reply.wait!() # Done(answer), or Dropped
        Closed => Dropped
    }
}

serve! = |requests| {
    for! (request, reply) in requests {
        reply.answer!(|| handle!(request)) # Dropped if handle! crashes or is cancelled.
    }
}
```

A service whose transitions are pure needs neither a queue nor a reply: it's an update.

## What We Tried

The [sessions RFC](../../concurrency-sessions.md), now superseded, gave tasks one-shot channels. `Session.pair!()` returned an `Out(a)` and an `In(a)` end, each usable once. A protocol of many messages continued by sending a fresh end inside each message, so a stream was `Stream(a) : [More(a, In(Stream(a))), Done]`. Mailboxes (`Post`, `Inbox`) let many tasks send to one reader, and shared resources were server tasks reached through them.

| Option | What broke |
|---|---|
| One-shot channels that carry the next end | Each end is used once, which Roc's types can't check, so a second use is a run-time crash ("a session endpoint was used twice"). A consumer threads the next end through every receive, so it's written as tail recursion, not a loop. A stream has one reader. |
| Channels without replies | A sender never waits, so a fast producer fills memory unless the protocol adds replies for credit. In the prototype, three senders of four messages each to a slow reader queued at most 3 messages when each waited for a reply, and 12 when they didn't. |
| Mailboxes | Many senders, but one reader: receiving consumes the `Inbox` and returns the next one (`Got(msg, rest)`), so a second receive on the same `Inbox` crashes. Posting never waits. |
| Server tasks for requests | A task per service, kept alive beside the program, plus a reply channel per request. See [decision 6](06-shared-state.md). |

## Why This One

A bounded queue in a handle has none of those costs. Any number of tasks write to it and read from it, a full queue makes writers wait, and `for!` reads it like any other stream:

- **Backpressure.** `push!` waits while the queue is full, and the reading loop waits while it's empty. No reply protocol is needed for credit.
- **Many readers.** Each item goes to exactly one reader, so a worker pool is several loops over the same queue. Fan-in is several producers pushing to one queue.
- **A reliable end.** `closing!` closes the queue however the producer ends, so readers' loops always end. A reader that may `break` early runs in `closing!` too; closing makes the producer's next `push!` answer `Closed`, so it stops instead of waiting for room that will never come.
- **Cancellation takes nothing.** A task cancelled while waiting in `push!` or `pop!` took nothing, because a `Wait` takes nothing. A task that fails inside `Reply.answer!` marks the reply `Dropped`, so the asker stops waiting.
- **No new mechanism.** Streams need nothing the runtime doesn't already have for `Shared`, and the other shapes (a broadcast log, a watched value, a pipeline of stages) are written the same way. Since `update!` moves the state into its function ([decision 10](10-in-place-updates.md)), a queue in a handle costs what a list costs.

Ends gave the sessions RFC one thing cells don't: when the last `Out` end went away, its receiver learned `Cancelled`. That's the main cost below.

## What It Costs

- **A reply nobody will answer is a deadlock, not a value.** A channel's receiver learned `Cancelled` when the sender's end was dropped. A cell has no ends: a `Reply` becomes `Dropped` only when the task answering it fails inside `answer!`. If a server stops with requests still queued, their askers wait until stall detection ([decision 8](08-cycles.md)) crashes them.
- **The end of a stream is explicit.** A loop over a queue ends only when the queue is closed, which is why a producer, and a reader that may stop early, runs inside `closing!`.
- **Every push and pop is an update on one hot cell.** Under contention, each `Set` probes the waiting readers or writers ([decision 7](07-waking-waiters.md)), and under `ParallelTasks` one queue serializes every task that uses it ([decision 6](06-shared-state.md)).

## Evidence

Argued, not measured. The prototype checks the primitives `Queue` and `Reply` are made of: `update!` with `Wait`, `finally!` (which `closing!` is) and `on_failure!` (which `answer!` uses), under every schedule, cancellation at every suspension point, and fuzzing ([decision 6](06-shared-state.md)). Its crawler test uses a cell of jobs as a queue. It doesn't run the `Queue` and `Reply` code above.

The channels and mailboxes they replace were prototyped in full, and their findings remain as the evidence for the costs above: the run-time end checks, and the backpressure counts from replies. Those tests stay in the prototype as the record of the superseded design.
