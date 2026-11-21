# 8. Cycles

| | |
|---|---|
| Status | Settled by prototype, with parts open: cycles in long-lived tasks (open decision (e)) and stall detection across roots (open decision (a)) |
| Builds on | [Structured tasks](03-structured-tasks.md), [Shared state](06-shared-state.md) |
| Detail | [What a state can hold](../../concurrency-shared-variables.md#what-a-state-can-hold), [Releasing cells](../../concurrency-shared-variables.md#releasing-cells), [Deadlocks and liveness](../../concurrency-shared-variables.md#deadlocks-and-liveness), [Rationale and alternatives](../../concurrency-shared-variables.md#rationale-and-alternatives), [What the prototype shows](../../concurrency-shared-variables.md#what-the-prototype-shows) |

By design, Roc has had no way to express a reference cycle, so reference counting frees everything. A cell's state can hold any value, handles and functions that capture handles included, so cells can now hold each other. How are those cycles freed?

## The Decision

The runtime keeps a *holder set* for each cell: the tasks that may hold a reference to it. When no task in the set is still running, the runtime *releases* the cell: it drops the cell's state and leaves the cell empty. Dropping the state breaks any cycle through it, and reference counting frees the rest.

Cycles come up in ordinary code. A crawler keeps its pending jobs in shared state, and each job captures the queue so it can add the links it finds:

```roc
crawl! = |queue, url| {
    links = Http.get!(url).links
    queue.modify!(|jobs| jobs.concat(links.map(|link| || crawl!(queue, link)))) # Each job captures `queue`.
}
```

A pending job and its queue hold each other. The cycle breaks in normal use, when a worker takes the job out, but a program can also abandon the queue with jobs still in it.

Holder sets are kept like this:

- **`Shared.new!`** makes a cell held by the current task.
- **A value that crosses to another task** makes that task a holder of every cell reachable from it. A value crosses when an entry starts (its captures), when a child's result arrives at its parent, and when the host takes a hosted call's arguments. If the value reached the cell only through objects whose count is one, the receiver becomes the only holder; otherwise the sender stays one too.
- **A store passes holders on.** When an update stores a new state, every cell reachable from that state gains the stored cell's holders, and the walk continues into those cells' states. So a task that can reach a cell through other cells' states is one of its holders.
- **A task leaves every holder set when it finishes.**

When a live cell has no holders left, no running task can reach it, and no task started later can either, since only a holder can pass a handle on. A released cell is never updated again.

Release isn't the usual way cells are freed. A cell whose state doesn't lead back to it is freed by its count when its last handle goes, as any value is. Release matters for cycles, and for cells left in a crashed task's frames, which the companion RFC's R5 leaks: their holders finish, and the cells are released.

**Stall detection uses the same sets.** A root's task tree is *stalled* when none of its tasks is queued, running or waiting on a hosted operation, at least one waits on a cell, and every holder of every cell waited on belongs to the tree. Nothing can ever change those cells, so the runtime crashes the waiter that began waiting last, with "deadlock: no task can ever change the shared state this task waits for". The crash readies other tasks through cancellation and propagation, so one detection is enough to make progress. In a `None` build, a stall is the scheduler finding no ready task.

A few details:

- **Why a set, not one holder.** `supervise!` shares its closure, and the handles it captured, between the supervisor and each attempt. If an attempt crashes with a handle in a running frame, a restarted attempt may still use it, so the cell must stay until the supervisor finishes.
- **The walk** is generated from the stored value's type, like the per-type helpers the compiler already generates, and visits nothing for a type that can't contain a handle.
- **Under `ParallelTasks`**, a cell's lock also guards its holder set.
- **`None` builds** track no holders; the runtime releases every cell still live when the program ends.
- **Release is a runtime action.** The runtime drops the state with the type's generated drop helper. Backends still follow only the explicit `incref` and `decref` statements in LIR.

## What We Tried

| Option | What broke |
|---|---|
| Type rules against cycles: no function in a state, and no type that reaches its own handle through a state (an earlier draft) | Sound, and checkable once per declaration. But the checker can't see what a function captures, so in the prototype's model the rules rejected 8,359 of 87,114 acyclic state types, about one in ten, every one for a function in a state. They added an inferred requirement to generic code, and still rejected cycles that break in normal use, such as the crawler's jobs that capture their queue. |
| Deciding cycles from lambda sets, which show what each function captures | Exact, as the prototype shows. But only `.lss` builds compute lambda sets, and a program's validity can't depend on how it's lowered. |
| Capture summaries: function types record which handle types their captures reach, inferred as rows | Removes the type rules' false rejections of functions, but adds a component to every function type, polymorphism over it, and a default for function fields in declarations. A generic queue type still rejects jobs that capture another queue of the same type, and real cycles such as the crawler's stay errors. |
| Reachability tracked per value, as in reachability types | Inferred by unification, it merges every level of a recursive type, so it still rejects every recursive state the type rules reject. Subset constraints give up principal types, signatures would name values, and real cycles stay errors. |
| A cycle collector for cells, such as trial deletion (as in Nim's ORC) | A second mechanism, whose timing depends on the collector. Holder sets already existed for deadlock detection, and release through them follows the task tree. |
| Weak handles | You'd decide which handle is weak, and a weak handle can find its state gone. |
| Allowing functions in states, but not closures that capture handles | A function's type doesn't say what it captures, so the checker can't enforce it. |

## Why This One

The type rules were the obvious answer, and the prototype measured them carefully: sound, and cheap to check. They failed on what they rejected. A state that holds functions, such as a queue of jobs, is ordinary, and the rules turned many of those into compile errors with no cycle anywhere. The only exact rule needs lambda sets, which `.boxy` builds never compute.

Holder sets cost almost nothing extra. Stall detection needs them anyway, so release adds no tracking, only the drop. They need no type rule, no new syntax and no annotation, so a state can hold anything.

They also fit the task tree. A cycle's memory is tied to the tasks that used it, as region-based memory ties memory to a scope. Work done in its own task, such as one request, is freed when that task ends.

## What It Costs

- **Stores walk their state.** To pass holders on, every store walks the parts of the new state that can hold a handle. A large state full of handles makes every store O(n), even when the change itself is in place.
- **Holder sets over-approximate.** A task stays a holder until it finishes, even after it drops its handle. An abandoned cycle is freed when the last task that held any of its cells finishes, not when the program drops its last handle.
- **Long-lived tasks accumulate cycles** (open decision (e)). A task that runs for the whole program, such as a server's loop in `main!`, keeps every cycle it makes and drops until it ends, and a `None` build keeps them until the program exits. Such a loop should clear its cells' states first, or run each iteration in a task. Releasing sooner would need to know when a task no longer holds a handle, which no count says for a cycle. Whether real programs hit this, and whether a debug diagnostic that reports cells freed by release is enough, needs programs written against the real runtime.
- **Stall detection across roots is untested for cells** (open decision (a)). A cell held by another root, or by the host, may still change, so a tree waiting on one isn't stalled. Per-root detection is implemented and tested within one root. Across roots it was tested only for the superseded sessions RFC's ports, which used the same sets; there, detection that ignored other roots' holders crashed requests that would have been served.
- **A stall is detected, not prevented.** `Wait` can wait forever when no task that will run again can change the state. That's a bug in the program, and it becomes a crash, not a compile error.

## Evidence

`CellRelease`, and `check.py` with `links`, generate programs whose states hold handles (their own included) and jobs that capture cells. Workers and jobs link cells, push and pop jobs, clear states and make cells of their own, so states form cycles and programs abandon them at random. Every run's heap is verified exact, so a surviving cycle would be reported as a leak, and every use of a cell asserts that it wasn't released.

Over 2,000 such programs (1,338 with linked cells), each run under every schedule, both lowerings, a `None` build and cancellation at suspension points, every check passed:

| | Count |
|---|---|
| Runs | 64,094 |
| Cells released when a child task finished | 77,430 |
| Cells released when the root finished | 10,316 |
| Cells released by a `None` build | 3,425 |
| Leaks, and cells used after release | 0 |

- **Mutations.** Never releasing leaks on the fourth generated program. Not passing holders on at a store releases a cell too early on the 56th, and the assertion catches it. Not making a task a holder of what its entry captures isn't caught: a task that captures a handle runs inside a task that holds it, which outlives it.
- **Pinned tests.** The crawler gives 7 under every schedule and in a `None` build, and the queue it abandons with a job still in it is released when `main` finishes. Two tasks that each make two cells holding each other's handles release one cell of each pair when they finish, which frees the other.
- **The rejected type rules** (`cycles.py`): over 100,000 generated types, 87,114 were acyclic by the exact lambda-set rule and ran 348,456 random programs without a leak. Every one of the 12,886 rejected types had a leaking witness. The type rules accepted 78,755, each also accepted by the exact rule, and rejected 8,359 acyclic types, every one for a function in a state.
