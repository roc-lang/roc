# RFC: Sessions, Channels and Shared Resources

| | |
|---|---|
| Status | Draft. Not adopted; nothing here is part of `design.md`. |
| Scope | Channel types and operations, mailboxes, their runtime state, `None` semantics for receives, deadlock detection, library patterns for shared resources |
| Companion | [Suspendable Effects and Platform-Scheduled Concurrency](concurrency-explicit-suspension.md): the task tree, plans, concurrent loops and crashes this RFC builds on |
| Prototype | [concurrency-prototype/](concurrency-prototype/README.md): `sessions.py`, and the `Sessions` tests |

## Summary

The companion RFC gives Roc a tree of tasks. Each child talks to its parent
exactly once, by returning its result. This RFC lets tasks talk while they run,
and builds shared resources from that, without adding anything to the type
system. It has four parts:

1. **One-shot channels.** `Session.pair!()` returns two ends, `Out(a)` and
   `In(a)`. Sending a value never suspends. Receiving suspends until the value
   arrives, and returns `Got(v)` or `Cancelled`. Each end may be used once, and
   a protocol of many messages continues by sending a fresh end along with each
   message.
2. **Cancellation is a message.** When an unused `Out` end goes away, its
   receiver gets `Cancelled`. That happens however it goes away: dropped by
   code, dropped with a cancelled task's state, closed by a crash, or sent to a
   receiver that has gone. A cancelled task runs no code of its own (R4), so
   the task on the other end of a session does the cleanup.
3. **Shared resources are libraries.** A semaphore, a pool or a circuit breaker
   is a task that serves requests from a mailbox and hands out ends. Because an
   end always closes, a permit always comes back, whether its holder releases
   it, returns, is cancelled or crashes. Applications use scoped functions such
   as `with_permit!`; only library authors handle ends.
4. **Deadlocks are detected, not prevented.** Channels can form cycles that the
   task tree cannot. When a task tree can make no more progress while some of
   its tasks wait at receives, the runtime crashes the receiver that waited
   last, and the crash propagates as any crash does.

Misusing an end is a run-time crash, not a type error. Session types, linear
types and capability tracking were considered and are not part of this design
(see "Rationale and alternatives"). A static check can later report the same
misuse earlier without changing what programs mean.

## Motivation

### What the task tree cannot express

In the companion RFC, a child task's only output is its result, and a parent
learns nothing from a child until the child has finished. That covers
fetching, fan-out, racing and supervision. It does not cover:

- **Streams.** A producer that yields items while a consumer handles them, with
  the consumer able to stop early.
- **Conversations.** A request, a reply, then a follow-up that depends on the
  reply, between two tasks that are both still running.
- **Shared resources.** A limit on database connections across all the tasks
  of a request, a pool, a rate limiter, a circuit breaker. Each needs one owner
  that many tasks reach.
- **Cleanup after cancellation.** A cancelled task runs no more of its code
  (R4), so it cannot release a permit or close a connection itself. Something
  else has to learn that it stopped.

### Requirements

1. Add no new kind of type or effect annotation. Channel ends are ordinary
   values of builtin opaque types.
2. Keep the companion RFC's guarantees. Interleaving still happens only at
   suspension points, there are still no data races, and nothing outlives the
   loop that started it.
3. Make cancellation observable to the other end of every session, so cleanup
   can be written as ordinary code in a task that is still running.
4. Never lose a value. Every message is received, or dropped exactly once.
5. Turn a deadlock into a crash with a clear message instead of a hang.
6. Work on every target, including wasm without stack switching. Channels add
   suspension points; they add no new way of suspending.

## Guide-level explanation

### Channels

```roc
Session.pair! : () => (Out(a), In(a))
Out.send! : Out(a), a => {}
In.receive! : In(a) => [Got(a), Cancelled]
```

A channel carries one message. `send!` stores the message and returns at once.
`receive!` waits until the message is there, and is a suspension point like a
suspending hosted call:

```roc
answer! : {} => U64
answer! = |{}| {
    (out, input) = Session.pair!()
    (_, got) = Concurrent.both!(|| out.send!(42), || input.receive!())
    match got {
        Got(n) => n
        Cancelled => 0
    }
}
```

`pair!` is effectful because each call makes a new channel: two calls give
different channels.

### Protocols by passing ends

Each end may be used once. A protocol with more than one message continues by
sending the next channel's end inside the current message. A stream of numbers
is a message that carries a number and the end the rest of the stream will
arrive on:

```roc
Stream(a) : [More(a, In(Stream(a))), Done]

produce! : Out(Stream(U64)), U64 => {}
produce! = |out, n|
    if n == 0 {
        out.send!(Done)
    } else {
        (next_out, next_in) = Session.pair!()
        out.send!(More(n, next_in))
        produce!(next_out, n - 1)
    }

sum! : In(Stream(U64)), U64 => U64
sum! = |input, acc|
    match input.receive!() {
        Got(More(n, rest)) => sum!(rest, acc + n)
        Got(Done) => acc
        Cancelled => acc
    }
```

`produce!` and `sum!` are tail-recursive, so a stream of any length runs in
constant space (G8). The message's type is the protocol: a consumer of
`In(Stream(U64))` can only match the tags a stream has.

### Cancellation is a message

A receive returns `Cancelled` when the matching `Out` end will never be used:

- the code holding it dropped it without sending;
- the task holding it was cancelled, which dropped the paused state that held
  it;
- the task holding it crashed (see "Crashes close ends");
- it was inside a message sent to a receiver that had gone.

In the stream above, if the task running `produce!` is cancelled halfway, the
consumer's next receive returns `Cancelled`, and `sum!` returns what it has.
The producer runs none of its own code after cancellation; the consumer, which
is still running, handles the end of the stream.

The other direction is silent. When a receiver goes away without receiving,
the message sent to it, or sent later, is dropped. A sender never learns
whether its message was received, as with any asynchronous send. A protocol
that needs to know asks for a reply.

### Using an end twice crashes

```roc
twice! = |out| {
    out.send!(1)
    out.send!(2)    # crashes: "a session endpoint was used twice"
}
```

The check is at run time. Roc's types do not track how many times a value is
used, and adding linear types for this would change the language far more than
channels do. A second use is a bug in the program, so it crashes, and the crash
propagates through the task tree like any other.

### Requests and replies

A request carries the `Out` end its reply should go to:

```roc
ask! : Out((req, Out(resp))), req => [Got(resp), Cancelled]
ask! = |server, request| {
    (reply, answer) = Session.pair!()
    server.send!((request, reply))
    answer.receive!()
}
```

If the server crashes, is cancelled, or drops the reply end, the caller gets
`Cancelled` instead of waiting forever. If the caller is cancelled while
waiting, its receive is abandoned. The reply channel then closes, and whatever
the server sends there is dropped. If that reply holds an end of its own, such
as a permit, dropping it tells that end's receiver.

### Mailboxes

A one-shot channel has one sender. A service that many tasks reach needs a
mailbox:

```roc
Mailbox.new! : () => (Post(a), Inbox(a))
Post.post! : Post(a), a => {}
Inbox.receive! : Inbox(a) => [Got(a, Inbox(a)), Closed]
```

A `Post` is an ordinary value that can be copied, stored and given to any
number of tasks. Posting never suspends. The `Inbox` has one reader: receiving
consumes it and returns the next message along with the inbox to receive from
next, so a second receive on the same `Inbox` value crashes, just as it does on
an `In` end. A receive returns `Closed` once every `Post` is gone and every
message has been taken. Messages from one sender arrive in the order they were
posted; the order across senders is unspecified.

Mailboxes are proposed here and are not yet in the prototype.

### Waiting on several channels

A concurrent loop over receives waits for whichever message comes first:

```roc
for! event in [|| Request(inbox.receive!()), || Tick(timer.receive!())].concurrently() {
    match event {
        Request(Got(msg, rest)) => {
            spawn || Request(rest.receive!())
            handle!(msg)
        }
        Request(Closed) => {}
        Tick(_) => log!("tick")
    }
}
```

Each receive runs as its own entry, and the body starts the next receive with
`spawn`. No separate `select` is needed. Leaving the loop early cancels the
receives still waiting, and a cancelled receive closes its channel, as above. A
loop that must keep its channels open does not leave early. It ends when every
channel it waits on has closed.

### Shared resources are libraries

A semaphore is a task that owns a count, serves requests from a mailbox, and
hands out permits. A permit is the `Out` end of a channel back to the
semaphore. However the permit goes away, that channel's receive returns, and
the semaphore counts the permit as returned:

```roc
Permit : Out([Release])

Event : [Request([Got(Out(Permit), Inbox(Out(Permit))), Closed]), Returned]

serve! : Inbox(Out(Permit)), U64 => {}
serve! = |inbox, permits| {
    var $free = permits
    var $queue = []
    for! event in [|| Request(inbox.receive!())].concurrently() {
        match event {
            Request(Got(reply, rest)) => {
                spawn || Request(rest.receive!())
                if $free > 0 {
                    $free = $free - 1
                    spawn grant(reply)
                } else {
                    $queue = $queue.append(reply)
                }
            }
            Request(Closed) => {}
            Returned =>
                match $queue.first() {
                    Ok(reply) => {
                        $queue = $queue.drop_first(1)
                        spawn grant(reply)
                    }
                    Err(ListWasEmpty) => $free = $free + 1
                }
        }
    }
}

## Hand out one permit and wait until it comes back, however it comes back.
grant : Out(Permit) -> (() => Event)
grant = |reply| || {
    (permit, back) = Session.pair!()
    reply.send!(permit)
    _ = back.receive!()
    Returned
}
```

Callers never see a permit:

```roc
Semaphore := { requests : Post(Out(Permit)) }

## Run `f` while holding one permit.
with_permit! : Semaphore, (() => a) => a
with_permit! = |sem, f| {
    (reply, granted) = Session.pair!()
    sem.requests.post!(reply)
    match granted.receive!() {
        Got(permit) => {
            result = f()
            permit.send!(Release)
            result
        }
        Cancelled => crash "the semaphore stopped"
    }
}

## Run `body` with a semaphore of `permits` permits, serving it alongside.
Semaphore.with! : U64, (Semaphore => a) => a
Semaphore.with! = |permits, body| {
    (requests, inbox) = Mailbox.new!()
    (result, {}) = Concurrent.both!(|| body(@Semaphore({ requests })), || serve!(inbox, permits))
    result
}
```

The permit comes back in every case:

| What happens to the caller | How the permit returns |
|---|---|
| `f` returns | `Release` is sent |
| The caller is cancelled while `f` runs | Its paused state drops the permit: `Cancelled` |
| `f` crashes | The crash closes the permit (see "Crashes close ends"): `Cancelled` |
| The caller is cancelled while waiting for the grant | Its receive is abandoned and the reply channel closes; the grant sent there is dropped, which drops the permit: `Cancelled` |
| The caller is cancelled before its request is served | Same as the previous row, at the moment the semaphore grants |

When `body` returns, its closure and the `Post` inside it go away, the inbox
reports `Closed`, and `serve!` ends once every permit has come back. The
semaphore cannot outlive `Semaphore.with!`. If `body` keeps a `Post` somewhere
that outlives it, the inbox never closes and the program deadlocks. That
deadlock is detected (see "Deadlocks"), so the mistake crashes instead of
hanging.

### What application code sees

Applications use scoped functions like `with_permit!`, `Pool.with_connection!`
or `Breaker.call!`. Channels and mailboxes are for the libraries that implement
them, in the way that applications use `all!` and leave concurrent loops to
code that needs a policy of its own.

A library that runs a protocol on the application's behalf can keep its ends
private and ask the application only for decisions:

```roc
Purchase.run!(order, {
    on_quote: |quote| if acceptable(quote) { Buy(payment) } else { Decline },
    on_receipt: |receipt| Completed(receipt),
})
```

The callbacks return ordinary values. The library performs every send and
receive, so no application code can use an end twice.

### Crashes close ends

A crash leaks what the crashed task's running frames held (companion RFC,
"Crash mechanics"). If those frames held an end, the end would never be
dropped, and its receiver would wait forever. So a crash closes every end its
task held: an unused `Out` end as if dropped, so its receiver gets `Cancelled`,
and an unused `In` end as if gone, so messages to it are dropped.

Which task an end belongs to is defined by where it travels, not by which
frame holds it. That is what makes this possible after a crash, when the frames
cannot be inspected. An end belongs to the task that made it, until it is sent
on a channel or arrives as part of a child's result, and then it belongs to
the receiving task.

### Deadlocks

The task tree cannot deadlock: a parent waits only on its own children, which
never wait on it. Channels can form cycles. Here each task waits for the other
to send first:

```roc
(a_out, a_in) = Session.pair!()
(b_out, b_in) = Session.pair!()
Concurrent.both!(
    || { _ = a_in.receive!(); b_out.send!(1) },
    || { _ = b_in.receive!(); a_out.send!(2) },
)
```

When no task in a tree can run, and the tree waits on no hosted operation, the
runtime crashes the task that started waiting at a receive last. The message
is "deadlock: no task can ever send the message this task waits for". The
crash propagates as any crash does, cancelling the other side of the cycle and
reaching the nearest supervisor. Detection is a backstop, not a type rule:
programs that never wait in a cycle never see it.

### On platforms that do not schedule tasks

A `None` build runs one task at a time on one stack (companion RFC, "On
platforms that do not schedule tasks"). A receive whose message has not been
sent cannot wait for another task to run. Instead, it runs loop entries that
have not started yet, earliest in input order first and each to completion,
until its message arrives. If none is left, nothing can ever send the message,
and the receive crashes with the deadlock message. A loop that later asks for
its next result gets the entries those receives ran first, in the order they
finished.

So the stream example runs the same way on every platform: the consumer's
first receive runs the producer, which sends every item and finishes, and the
consumer reads them.

**What does not work on one stack.** A protocol in which two tasks take turns
more than once cannot run there. In the semaphore, the caller asks for a
permit, the semaphore grants it and then waits for the permit to come back,
and the caller can use the permit only after the semaphore's wait returns. On
one stack the caller is below the semaphore and cannot run, so the program
deadlocks and crashes. The `Tasks` build under the input-order schedule that
the companion RFC's C9 compares against deadlocks in the same way, so G3 still
holds. Whether `None` builds should run such protocols, and how, is the main
open question of this RFC (see "Decisions left to prototypes").

## Reference-level explanation

### Types and operations

`Out(a)`, `In(a)`, `Post(a)` and `Inbox(a)` are opaque builtin types. Values of
these types are reference counted like any value. Their operations lower to
internal operations with explicit `RcEffect` data:

| Operation | Ownership | Meaning in `Tasks` builds | Meaning in `None` builds |
|---|---|---|---|
| `pair()` | Returns two fresh ends | Make a channel; both ends belong to the current task | Make a channel |
| `send(o, v)` | Consumes `o` and `v` | Crash if `o` was used. Otherwise mark it used; drop `v` if the channel is closed, else store `v`, transfer its ends to the `In` end's holder, and ready a task waiting at the receive | The same, without readying |
| `receive(i)` | Consumes `i` | Crash if `i` was used. Otherwise mark it used; return `Got(v)` or `Cancelled` if the channel has an outcome, else wait: a suspension point | Return the outcome, running unstarted loop entries on demand until there is one; crash with the deadlock message if none is left |
| `mailbox()`, `post(p, v)`, `receive_inbox(b)` | As `pair`, `send`, `receive` | As above; `post` appends; a receive returns `Got(v, b')` or `Closed` | As above |

`receive` is a suspension point. It extends the companion RFC's C2: a
suspension point may also trace to a channel or inbox receive. A receive's
wait source is its channel, with its own counted wake word (R1), so a
completion arriving for one of the task's loops never readies a task waiting at
a receive, and a send never readies a task waiting anywhere else.

### Channel states

A channel holds one slot, and flags for whether each end has been used and
whether the `In` end has gone:

| State | Meaning | Becomes |
|---|---|---|
| `empty` | Nothing sent yet | `full` on a send; `cancelled` when the `Out` end goes away unused |
| `full` | A message waits | `taken` on a receive; the message is dropped when the `In` end goes away |
| `taken` | The message was received | Final |
| `cancelled` | The `Out` end went away unused | Final; a receive returns `Cancelled` |

The `In` end **goes away** when it is dropped unused, when a crash closes it,
or when a receive that consumed it is abandoned because its task was cancelled
or crashed while waiting. Once the `In` end has gone, a message in the slot is
dropped at once, and a message sent later is dropped at the send.

An abandoned receive must close its channel. The receive consumed the `In`
end, so no other code can ever take a message from that channel. A message left
there would be owned by nothing: it would never be dropped, and an end inside
it would never tell its receiver. In the prototype, leaving it open lost the
semaphore's permit in about a third of the schedules of one program, and the
fuzzer finds the mistake in 17 of 300 generated programs.

### Holders

Every end has a holder task, which the runtime records in the channel:

- `pair()` makes both ends held by the current task.
- `send(o, v)` makes every end reachable from `v` held by the holder of the
  channel's `In` end.
- When a child's result arrives at its parent, every end reachable from the
  result becomes held by the parent.

Finding the ends reachable from a value is a walk the compiler generates from
the value's type, like the other per-type helpers it already generates. It
visits nothing for a type that cannot contain an end, which is almost every
type.

When a task crashes, the runtime closes every channel end it holds that has not
been used: the receiver of an `Out` end gets `Cancelled`, and an `In` end goes
away. This happens after the crashed task's records are dropped (companion RFC,
"Crash mechanics", step 1). The ends that its dropped records held are
dropped exactly. Closing reaches only the ones its running frames leaked.

### Cancellation

Cancellation needs no new mechanism. A cancelled task's paused records are
dropped exactly (C5), and dropping an unused `Out` end delivers `Cancelled`.
Releasing a task parked at a receive abandons that receive, which closes its
channel. Neither runs any Roc code, so R4 holds.

`race!` and loop exits get the same behavior for results that were never
selected. An entry that finished but whose result the loop never took has
that result dropped at the exit, and any end inside it closes. When two
entries both open a resource reached through an end, the loser's resource owner
learns that it lost.

### Deadlock detection

A receive is a new kind of wait, and waits on receives can form cycles, so the
companion RFC's R6 no longer holds as stated for programs that receive. The
runtime adds detection.

A task tree is **stalled** when none of its tasks is running or ready, none
waits on a hosted operation, and at least one waits at a receive. Every task in
a stalled tree waits on a receive or on its own loop's entries, and those
entries are in the same tree, so no task can ever run again. The runtime then
crashes the receiver that started waiting last. Its clock is the number of
tasks started so far, read when a task starts to wait, with ties going to the
task created later.

That choice is deterministic for a given schedule and matches a `None` build,
where the receiver that waited last is the top of the stack. A crash readies
other tasks, through cancellation and propagation, so one detection is enough
to make progress. If the cycle survives, as it can under a supervisor that
restarts the task, it is detected again.

The prototype detects a stall when its host has nothing to run, which is the
same as a stalled tree when there is one root. A host that serves many roots
needs detection per root, which the runtime can track with a count, per root,
of tasks that are not waiting at a receive. A root whose receivers wait on a
`Post` held by another root is never stalled, because that root may still
post. See "Decisions left to prototypes".

### `None` builds

`None` builds keep the companion RFC's operations and change two of them:

- **`receive`**, when the message has not arrived, runs unstarted loop entries
  whose limits have room, choosing the earliest by task-tree path, until the
  message arrives. If no entry is left, it crashes with the deadlock message.
- **`next`** delivers entries that a receive already ran to completion, in the
  order they finished, before it starts any entry itself.

A program that never receives is unaffected, since no entry finishes before
`next` asks for it. C9 still holds for programs that do. The `Tasks` schedule it
compares against now also models the one stack:

- Only the task that started waiting at a receive last, and the entries its
  receive starts, can run until its message arrives.
- A loop's head is held while one of its entries runs, so the parent continues
  only after that entry finishes.
- A crash reaches its parent at once, ahead of results the parent has not taken
  yet.

The prototype's input-order policy implements this schedule. On every fuzzed
program that C9 compares, it gives the same results and effects as the `None`
engine, deadlocks included.

### Reference counting

A message moves from one task to another, as an entry's capture moves from a
parent to a child. In `ParallelTasks` builds the companion RFC's C6 must
therefore treat every sent value as a seed: an allocation that the receiving
task can reach while the sender may still update counts on it uses atomic
counts. Ends themselves are reached by two tasks, the holders of the two ends.
The channel state they share is updated only through the runtime's channel
operations, which are atomic. The prototype's atomicity analysis does not model
channels yet.

### Mailboxes

A mailbox is a queue with any number of senders and one reader. A `Post` is
counted like any value, and an inbox is `Closed` once its last `Post` reference
is gone and its queue is empty. A post to an inbox that has gone drops the
message, and any end inside it closes. So a request sent to a service that has
stopped is answered with `Cancelled` on its reply end.

A crashed task may leak a reference to a `Post` in its running frames, and the
inbox would then never close. Closing must therefore also cover `Post`s: the
runtime records, for each task, the `Post`s it holds, by the same rules as for
ends, and a crash releases them. This is designed but not yet prototyped.

### Services across roots

A web server runs each request as its own root. A limit shared by every
request, such as a database connection limit, cannot be a task inside one
request's tree. A platform can run a **service root**: a long-lived root that
runs Roc code like `serve!`, started by the host, with its `Post` passed to
every request root as an argument.

- Values cross roots only through posts and the replies to them. In
  `ParallelTasks` builds they are seeds, and so are hosted arguments, so they
  use atomic counts.
- A crash in the service closes its ends and stops its inbox, so requests
  waiting on it get `Cancelled`. The platform supervises the service root as
  it supervises any root, and decides whether to restart it.
- Only operations that must be native, such as database transactions with
  their own concurrency control and recovery, need to be platform services
  written in the host language.

### Hosted operations that hold ends

A local timeout can stop a task while the remote operation it started is still
in progress. Cancellation aborts the hosted call, and P4 makes the host release
any late result, but the remote effect may still happen. A semaphore whose
permit is released when the local scope ends then counts local scopes, not
operations still running on the remote side:

| Event | Free permits | Remote operations still running |
|---|---|---|
| A starts, takes the only permit | 0 | 1 |
| A times out locally; its permit returns | 1 | 1 |
| B starts, takes the permit | 0 | 2 |

Which count a permit keeps depends on where the end goes. If the permit stays
in the caller's scope, it counts local scopes. If it is passed as an argument
to the hosted call, the host releases it when the platform has finished with
the operation, which P5 already allows to be after an abort, and the permit
counts operations still in progress. No new mechanism is needed. A platform
whose remote services can lose track of an operation, as during a network
partition, has to choose between holding the permit indefinitely and a lease,
which is a policy that belongs to its API.

## Invariants

These extend the companion RFC's tables. Each is established in the prototype
by the regression tests that `coverage.py` lists for it, unless the table says
it is not yet prototyped.

| | Invariant | Kind | Established by |
|---|---|---|---|
| S1 | **Affinity.** Each end is used at most once. A second use crashes the task that tried, with "a session endpoint was used twice", and changes no channel. | Language guarantee | Run-time check; regression tests |
| S2 | **One outcome per receive.** A receive returns `Got(v)` with the one message sent, or `Cancelled` exactly when the `Out` end can no longer be used: it went away unused, or a crash closed it. | Language guarantee | Model, with the fuzzer's producers that end, drop their end or crash |
| S3 | **Message ownership.** A channel owns a sent message until a receive takes it. When the `In` end goes away, whether dropped unused, closed by a crash, or abandoned by a receive whose task was cancelled or crashed, the message is dropped exactly once, then or at its send. | Runtime invariant | Exact heap accounting after every run, which counts a message as owned only while its channel's `In` end exists |
| S4 | **Crashes close ends.** After a crash, every unused end the crashed task held is closed, and the runtime's record of holders follows every end through sends and results. | Runtime invariant | Model; a mutation that skips closing is caught |
| S5 | **Channels are wait sources.** A send readies only a task waiting at that channel's receive, exactly once, and a completion for any other source never readies it (R1 with channels as sources). | Runtime invariant | Model; a mutation to one wake word per task is caught |
| S6 | **Deadlocks crash.** When a task tree stalls with some of its tasks waiting at receives, the receiver that waited last crashes with the deadlock message, and the crash propagates by G7. Every run of the prototype ends with every task finished. | Runtime invariant | Model; runs that would hang are errors |
| S7 | **Sequential equivalence with receives.** A `None` build's results and effects equal a `Tasks` build's under the input-order schedule of "`None` builds", deadlocks included. | Compiler invariant (extends C9) | Differential, on every fuzzed program |
| S8 | **Sent values are seeds.** In `ParallelTasks` builds, every allocation reachable from a sent message uses atomic counts if the sender may still update counts on it. | Compiler invariant (extends C6) | Certify. Not yet prototyped |

The prototype adds channels to the companion RFC's fuzzer. Producers and
consumers, clients and servers are called directly or packed into closures
that flow into loops and combinators. Producers end, drop their end or crash,
consumers stop early, and requests carry the end their reply goes to. On 600
generated programs, every check of the companion RFC passes, and the `None`
engine matches the input-order schedule on all 590 that C9 compares. The other
10 crash inside `supervise!`, where a `None` build stops the whole program by
design (companion RFC). Each mutation switch in `sessions.py` (no
`Cancelled` on drop, no closing on crash, no closing of abandoned receives) is
caught by a regression test.

## Library patterns

These are written as libraries over channels and mailboxes. Only the
semaphore's permit handling is exercised in the prototype so far, through a
one-shot request channel instead of a mailbox.

- **Semaphore.** As above. `with_permit!` holds a permit for the duration of
  its function.
- **Pool.** A semaphore whose grant carries a resource and whose `Release`
  returns it. When a lease ends in `Cancelled`, the resource did not come back,
  so the pool opens a replacement. A pool that must never lose a resource keeps
  it, and lends access to it through a session instead.
- **Circuit breaker.** A task holding the breaker's state. Callers ask for
  permission and report each outcome by message, and the breaker reopens after
  a delay that an entry of its own loop waits out.
- **Transactions.** The database's own concurrency control belongs to the
  platform. A `Db.transaction!(db, |tx| …)` library retries the whole body on a
  serialization failure, each attempt with a fresh transaction. Roc cannot stop
  the body from performing other effects, such as sending an email, since
  effects are not tracked by type, so the library must document that the body
  may run more than once.
- **Sagas.** A saga runs a shielded compensating entry next to its steps. Each
  step sends the compensator its undo action *before* submitting the forward
  operation, together with an identity the remote service can use to recognise
  the operation. A step that is cancelled after submitting may or may not have
  happened, and the undo, keyed by that identity, must handle both. Making
  undo correct when a delayed forward request arrives after its undo is the
  remote service's protocol, such as a tombstone for the identity, not
  something the runtime can supply. Surviving a restart of the program needs
  durable execution (companion RFC, "Future possibilities").

Composition order expresses policy and is never inferred.
`with_permit!(|| retry!(f))` holds one permit across all attempts and their
backoff, while `retry!(|| with_permit!(f))` takes a permit per attempt. A
breaker around a retry sees each logical operation, while a retry around a
breaker sees each attempt. Whether a body is safe to retry is a property of
the operation's protocol, not of its code. Two operations that are each safe to
repeat can be unsafe to repeat together, so no analysis of the body could
decide it.

## Drawbacks

- **Protocol bugs are found at run time.** Using an end twice, or waiting in a
  cycle, crashes instead of failing to compile.
- **Deadlocks are possible.** The task tree alone cannot deadlock. Channels can,
  and detection only turns a hang into a crash.
- **Turn-taking protocols do not run in `None` builds.** A library like the
  semaphore works only on platforms that schedule tasks, until the open
  question below is settled.
- **Holder tracking costs a walk** over each sent message and each child result
  whose type can contain an end, and a scan of a crashed task's channels.
- **Sends are unbounded.** A mailbox queue grows without limit if its reader is
  slower than its senders, as in Erlang. Backpressure must be a protocol: a
  reply that grants the right to send again.
- **More to understand.** Ends are consumed by use and protocols continue
  through fresh ends, which is unfamiliar. Applications that use only scoped
  library functions never see this.

## Rationale and alternatives

| Alternative | Why not |
|---|---|
| Session types or linear types checked statically | Requires linearity in Roc's type system, and multiplicity polymorphism for higher-order functions such as `List.map`. The run-time check gives the same safety, as FuSe does, and a lint can report the obvious misuses earlier without changing what programs mean |
| A capability and contract system (scoped computations with pre- and postconditions, capture tracking as in Effekt's System C) | A research-grade type system, which needs effect rows and second-class functions that Roc does not have. Its composition guarantees still rest on each library meeting its contract, which this design tests instead |
| Many-use channels (Go, Concurrent ML) | One channel carries many messages to many readers, so `Cancelled` cannot be tied to one message, and a buffered message has no single owner. Mailboxes cover many senders with one reader |
| Erlang's selective receive | Scans the mailbox with a pattern, so cost depends on its contents. A loop over receives expresses the same waits |
| Cleanup code in a cancelled task (`finally`, `defer`) | Contradicts R4: a cancelled task would run code after cancellation, and could suspend while doing so. The other end of a session does the cleanup instead |
| Leaving an abandoned receive's channel open | Strands any message later sent there, which the prototype showed loses permits |
| Only platform-provided synchronizers | Every platform would reimplement every synchronizer, and the policy would be fixed by the platform. Platform services remain for what must be native |
| Sessions only along the task tree (as in Wadler's CP, where the structure rules out deadlock) | Rules out a semaphore, whose channels connect tasks that are not parent and child. Detection keeps those programs and turns their mistakes into crashes |
| Preventing deadlocks with a global lock order or channel priorities | Requires annotations on every channel; a library could not hide them |

## Prior art

| Work | Relevance |
|---|---|
| Honda, Vasconcelos and Kubo; Gay and Vasconcelos | Session types: a protocol as the type of a channel, continued by fresh channels |
| Wadler, *Propositions as Sessions* (ICFP 2012) | Sessions that follow a tree are deadlock-free by construction; the task tree has this property and channels give it up |
| Caires and Toninho, *The Linear Session Abstract Machine* (TOPLAS 2026) | A deterministic sequential strategy for session-typed programs, switching to the writer when a reader would block. The basis of one way to run turn-taking protocols in `None` builds |
| Padovani, *A Simple Library Implementation of Binary Sessions* (JFP 2017) | FuSe checks linearity at run time, as this design checks affinity |
| Mostrous and Vasconcelos, *Affine Sessions* (LMCS 2018) | Sessions that may end early, with the peer told by cancellation, without losing progress |
| Fowler, Lindley, Morris and Décova, *Exceptional Asynchronous Session Types* (POPL 2019) | Cancellation propagated to peers in asynchronous sessions, in Links; the model for `Cancelled` on drop |
| Kokke, *Rusty Variation* (ICE 2019) | Affine sessions in Rust, where dropping an endpoint cancels the session |
| Thiemann, *Intrinsically Typed Sessions with Callbacks* (ICFP 2023) | Protocols run by a library that asks the application for decisions through callbacks, so application code cannot misuse an endpoint |
| Brachthäuser, Schuster, Lee and Boruch-Gruszecki, *Effects, Capabilities, and Boxes* (OOPSLA 2022) | Capture tracking that stops a capability from escaping its scope; the static alternative this design does not take |
| Erlang/OTP | Mailboxes with one reader and unbounded queues; links and supervisors, which this design keeps from the companion RFC |
| Reppy, Concurrent ML | First-class synchronous events and choice; the select this design expresses with a loop over receives |
| Go channels | Many-use channels shared by many tasks, and their deadlocks, which Go's runtime detects only when every goroutine is blocked |
| Trio | Cancellation scopes in which cleanup is ordinary code in a task that is still running |

## Decisions left to prototypes

| Decision | Settled by |
|---|---|
| Whether `None` builds run protocols in which two tasks take turns, and how. (a) Coroutines on one stack, as in the Linear SAM: a receive that would block switches to the writer, by unwinding into records as the companion RFC's lowering already does. (b) Shared cells with a wait queue, an atomic "change the state or wait" operation, so that a semaphore is a cell instead of a task and needs no turn-taking. (c) Accept the limit, so such libraries need a `Tasks` platform | For (a), measure the cost of switching by unwinding on wasm, in ReleaseFast builds, against the protocols in the library patterns. For (b), prototype a cell-based semaphore in the `None` engine; it adds shared mutable state to the language, which needs its own justification |
| Bounds and backpressure for mailboxes | Writing a service with a slow reader, and comparing a bounded queue whose post suspends with a protocol that grants send credits |
| How a crash releases the `Post`s its task held | Prototyping mailboxes with holder sets |
| Deadlock detection per root, and with `Post`s that cross roots | Prototyping a host with many roots and a service root |
| How a host releases an end that a hosted operation holds, since releasing an `Out` end readies a task | Building a reference host with one hosted function that takes a permit |
| Whether a lint for the obvious double uses of an end is worth adding to the first version | Writing the library patterns and recording the mistakes made |
| Syntax spellings (`Session.pair!`, `Out`, `In`, `Post`, `Inbox`, `Got`, `Cancelled`, `Closed`) | Judgement; they are placeholders |

## Future possibilities

- **Static checks.** A lint that reports an end used twice on one path, using
  the uniqueness information the compiler already computes. Later, session
  types as an opt-in check over the same runtime, rejecting only programs that
  would crash.
- **Durable obligations.** With durable execution, a saga's compensator and
  the operation identities it holds could survive a restart of the program.
- **Typed service roots.** A platform header could declare the services it
  starts and the type of each one's `Post`, so a request root receives them
  without passing them by hand.
- **Deterministic simulation.** The companion RFC's seeded test platform also
  makes channel interleavings reproducible, including deadlocks.

## Implementation plan

These steps follow the companion RFC's plan, after concurrent loops exist.

1. Add `Out`, `In` and their operations to LIR with `RcEffect` data; make
   `receive` a suspension point (C2).
2. Add channel state, holders, closing on drop, on crash and on an abandoned
   receive, and channel wake words to the task runtime. Extend the
   conformance harness and the exact heap accounting (S1–S5).
3. Add the `None` semantics of `receive` and `next`, and the input-order
   schedule of S7 to the deterministic test host.
4. Add deadlock detection per root (S6).
5. In `ParallelTasks` builds, make sent values seeds (S8).
6. Add mailboxes, with `Post` holders.
7. Write the semaphore, pool and breaker libraries, and test them under
   cancellation at every suspension point and crashes everywhere.
8. Settle the `None` question above, then add service roots to one reference
   platform.

## References

- Honda, Vasconcelos and Kubo, *Language Primitives and Type Discipline for Structured Communication-Based Programming*, ESOP 1998
- Gay and Vasconcelos, *Linear Type Theory for Asynchronous Session Types*, JFP 20(1), 2010
- Wadler, [Propositions as Sessions](https://doi.org/10.1145/2364527.2364568), ICFP 2012; journal version JFP 24(2–3), 2014
- Caires and Toninho, [The Linear Session Abstract Machine](https://doi.org/10.1145/3819583), TOPLAS 48(3), 2026
- Padovani, [A Simple Library Implementation of Binary Sessions](https://doi.org/10.1017/S0956796816000289), JFP 27, e4, 2017
- Mostrous and Vasconcelos, [Affine Sessions](https://doi.org/10.23638/LMCS-14(4:14)2018), LMCS 14(4), 2018; extended abstract at COORDINATION 2014
- Fowler, Lindley, Morris and Décova, [Exceptional Asynchronous Session Types: Session Types without Tiers](https://doi.org/10.1145/3290341), POPL 2019
- Kokke, *Rusty Variation: Deadlock-free Sessions with Failure in Rust*, ICE 2019, EPTCS 304
- Thiemann, [Intrinsically Typed Sessions with Callbacks (Functional Pearl)](https://doi.org/10.1145/3607854), ICFP 2023
- Brachthäuser, Schuster, Lee and Boruch-Gruszecki, [Effects, Capabilities, and Boxes: From Scope-Based Reasoning to Type-Based Reasoning and Back](https://doi.org/10.1145/3527320), OOPSLA 2022
- Reppy, *Concurrent Programming in ML*, Cambridge University Press, 1999
- Erlang/OTP, [processes and message passing](https://www.erlang.org/doc/system/ref_man_processes)
- Trio, [cancellation and timeouts](https://trio.readthedocs.io/en/stable/reference-core.html#cancellation-and-timeouts)
- PostgreSQL, [serialization failure handling](https://www.postgresql.org/docs/16/mvcc-serialization-failure-handling.html)
