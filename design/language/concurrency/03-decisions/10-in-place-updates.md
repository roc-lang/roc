# 10. In-Place Updates

| | |
|---|---|
| Status | Settled by argument (commit `5c09149b97`, which closed what earlier drafts called open decision (f)). Modeled in the prototype; not measured |
| Builds on | [Shared state](06-shared-state.md), [Waking waiters](07-waking-waiters.md) |
| Detail | [Large states change in place](../../concurrency-shared-variables.md#large-states-change-in-place), [`update!`](../../concurrency-shared-variables.md#update), [Lowering](../../concurrency-shared-variables.md#lowering), [Crashes](../../concurrency-shared-variables.md#crashes), [Decisions left to review](../../concurrency-shared-variables.md#decisions-left-to-review) |

A queue, a log or a cache in a handle can be large. Does changing it, such as appending one item, copy the whole state?

## The Decision

`update!` *moves* the state out of the handle into its function. The handle holds nothing while the function runs, so unless a task still holds the state from a `get!` or an earlier answer, the function holds the only reference, and the state changes in place as any unique value does in Roc. `Keep` and `Wait` put back the same state the function was given.

`Queue.push!` appends at the cost of appending to a list:

```roc
push! = |Queue.(cell), item| cell.update!(|q|
    if q.closed {
        Keep(Closed) # Puts back q, unchanged.
    } else if q.items.len() == q.capacity {
        Wait # Puts back q, unchanged.
    } else {
        Set({ ..q, items: q.items.append(item) }, Pushed) # q isn't needed after this, so it changes in place.
    })
```

The rule is the one for any Roc value: decide first, then build the new state in the branch that answers `Set`. A new state built before the decision, or inside another function that returns the tag, is built while `Keep` or `Wait` may still need the old state, so it's a copy.

**How lowering does it.** Each function literal written as `update!`'s argument is compiled twice:

- **The update form.** Each tail position of the body, followed through `if`, `match` and blocks, is rewritten. A tail that builds `Keep(r)` or `Wait` also returns the parameter, the state the function was given. A tail that builds `Set(new, r)` is unchanged. Any other tail expression `e`, such as a call that returns the tag, becomes a `match` on `e` that does both. Reference counting then runs as usual: on a path that ends in `Set`, the old state isn't used after the new one is built, so a state with a count of one changes in place.
- **The probe form.** A tail that builds `Set(new, r)` or `Keep(r)` evaluates neither `new` nor `r` and answers that the waiter would go ahead; a `Wait` tail answers that it would wait; any other tail calls `e` and looks at its tag. So probing a waiter ([decision 7](07-waking-waiters.md)) never builds or copies a state. A crash in `new` or `r` would also have readied the waiter, so skipping them changes no outcome.
- **Cleanups** have type `s -> s`, so they need no rewrite: the state is moved in and the result stored.

The rule reads only the literal's own body, so it's the same in `.lss` and `.boxy` builds. It's a compiler invariant (V10) that the two forms agree: the probe form answers `Wait` exactly when the update form does, and the update form puts back the state it was given on `Keep` and `Wait`.

**Crashes.** The function had the state to itself, so an update that crashes takes the state with it: the handle stays empty and is poisoned, its waiters crash, and every later update of it crashes. To keep an *intended* crash from poisoning, decide inside and crash outside: the update answers with a tag that says the state is wrong, and the caller crashes after the update returns. `Reply.answer!` does this:

```roc
answer! = |Reply.(cell), f| {
    answered = cell.on_failure!(|r| match r { Pending => Dropped, _ => r }, || {
        value = f()
        cell.update!(|r| match r {
            Pending => Set(Done(value), Answered)
            _ => Keep(AlreadyAnswered) # Decide inside: the state stays good.
        })
    })
    match answered {
        Answered => {}
        AlreadyAnswered => crash "this reply was already answered" # Crash outside: nothing is poisoned.
    }
}
```

## What We Tried

| Option | What broke |
|---|---|
| Lending the state while the cell keeps it, so an update that crashes changes nothing (the earlier draft) | The function never holds the only reference, so every `Set` copies the whole state. A queue, a log or a cache in a handle costs O(n) per operation, and a waiter's probe copies it again. The bounded queue, the broadcast log, the single-flight cache and the replicated log in [the examples](../examples.md) all paid this. |
| Lending the state, and recommending collections that share structure for large states | Keeps the old state for a crash, at O(log n) per change. But the obvious `List` and `Dict` stay at O(n), a cost that's easy to miss. |
| Returning the state on every path, as Lean's `modifyGet` does | A `Keep` could return a changed state, and waiters would never hear of the change. Here the compiler puts the state back, so `Keep` can't change it. |
| Moving the state, with lowering adding the return to the `Keep` and `Wait` paths (how open decision (f) first described it) | A `Keep` or `Wait` can be built anywhere, such as inside a helper that returns the tag, so there's no fixed set of "paths" to add a return to. The rewrite has to work on the literal's tail positions, and turn any other tail into a `match` on the tag it returns. |

## Why This One

Requirement 10 of the shared-state RFC asks that changing a state cost what changing a value of its type costs. Moving meets it; lending can't, because the cell's reference means the function never holds the only one.

Moving and lending differ only when an update crashes. That's a bug in a pure function, and an OTP server that crashes loses its state too. Moving never copies where lending wouldn't, so this needed no benchmark.

Putting the state back in the compiler, instead of asking the function to return it, keeps `Keep` honest. A `Keep` can't change the state, so it's still safe to wake waiters only after a `Set` ([decision 7](07-waking-waiters.md)). Lean 4's `ST.Ref.modify` takes the value out of the reference the same way.

## What It Costs

- **A bug in an update loses the state for every task using the handle.** Lending would have kept it. Here the handle is poisoned, and every task that uses it crashes, not just the one that hit the bug. Deciding inside and crashing outside keeps intended crashes from doing this, but not unintended ones such as an overflow. The state the crashed function held is in the crashed frames, which the companion RFC's R5 leaks.
- **Whether a change is in place depends on where the new state is built.** A state built before the decision, or by another function that returns the tag, is a copy. A function passed to `update!` as a value, rather than written as a literal, is called from a tail that keeps the state alive across the call, so a `Set` built inside it copies. The rule is the one for any Roc value, but the old state's last use is hidden in `Keep` and `Wait`.
- **A state someone else holds is copied.** A task that still holds the state from a `get!`, or from an earlier answer that returned it, keeps its count above one.
- **Stores still walk their state** to pass holders on ([decision 8](08-cycles.md)), so a large state full of handles makes every store O(n) even when the change itself is in place.

## Evidence

Argued, not measured: the prototype counts operations, not copies, so it couldn't decide between moving and lending. It does model the rule:

- **Update forms.** Every function an update or cleanup calls gets an update form, made by rewriting its tails as above, before reference counting runs. A test covers a tail that calls another function returning the tag.
- **Moving.** The cell is empty while the function runs. `Keep` and `Wait` put the state back, and an assertion checks it's the same object (V10).
- **Crashes.** A crashing update leaves the cell empty and poisoned, and readies its waiters so they crash. Under the earlier rule, a parked waiter deadlocked instead. The state the crashed function held counts as a crashed-frame leak, and the exact heap check passes in both lowerings.
- **Fuzzing.** The generator now makes updates that crash, including pops that crash while a `Keep` still needs the state. 2,000 programs in each of two runs pass, with 392 and 1,556 crashed updates; in 278 programs an update crashed while its `Keep` needed the state, and every heap was exact.

The probe form isn't modeled: probes run the function as written on a state the cell keeps, and a check confirms that a waiter a probe readied doesn't answer `Wait` on the state the probe saw.
