# Questions You Might Have

## Why Not Async/Await?

`async` marks the functions a compiler must transform so they can pause.
Roc already marks them: effectful functions use `=>`, and only effectful code
can pause. Roc also has no effect polymorphism, so that mark is exact. Adding
`async` would make you write the same fact twice, and it would split
effectful functions into two kinds that can't call each other freely.

Roc removed its `Task` type in favor of effectful functions, and an `async`
type would bring it back. See
[decision 2](03-decisions/02-explicit-suspension.md).

## Isn't `!` Already Function Coloring?

Roc already has two colors, pure and effectful, for reasons that have nothing to
do with concurrency. This proposal doesn't add a third. An effectful function
that can pause is called exactly like one that can't, and a sequential
program's effectful functions run concurrently without changes.

## Why Not Give Each Task Its Own Stack, Like Go?

We built that first, on the `greenthreads` branch, and it works natively.
It doesn't work on WebAssembly engines that can't switch stacks, it leaks
what a cancelled task's stack holds, and it needs a fixed stack per task
plus hand-written assembly for each architecture. See
[decision 1](03-decisions/01-stackful-green-threads.md).

## Do I Ever Write a Lock?

No. You write a pure function that gets the state and answers `Set`, `Keep` or
`Wait`. The runtime runs it atomically. A pure function can't pause or do I/O,
so an update can't deadlock on a lock and can't leave the state half-changed.
Locks, semaphores, queues and pools are library code built that way.

You can still write a program that waits forever, such as two tasks each
waiting for the other. The runtime detects that and crashes the task that
began waiting last, with a deadlock message.

## Can a Task Outlive the Function That Started It?

No. By design, every task belongs to a loop, and the loop doesn't end until its
tasks have finished or been cancelled. A long-running job lives in a loop
that runs for as long as the job does, such as a server's main loop.

## What Happens When Something Crashes?

The crash ends its task, cancels the tasks linked to it, and travels up to the
nearest supervisor: `Concurrent.supervise!`, or the platform. Roc code never
catches a crash. Errors you expect stay `Err` values. This is Erlang/OTP's
"let it crash" approach.

## What If My Platform Doesn't Support Concurrency?

Your program means the same thing. It runs its own tasks on one thread, and
platform calls block. A loop whose entries don't wait for each other runs them
one at a time, in order. A timeout can't interrupt a call that blocks. See
[decision 4](03-decisions/04-one-meaning-per-build.md).

## What Does It Cost When I Don't Use It?

Nothing for pure code, and nothing for a program that can't wait on a platform
that doesn't schedule tasks, in optimized builds. (Dev builds check a flag after
calls through effectful function values.) When a task can pause, each call that might
pause gets a flag check afterward, which cost 2.5% to 5% in a microbenchmark
made only of calls. [Open questions](08-open-questions.md#what-it-costs)
lists every cost and says which are measured.

## Can I Run Pure Code in Parallel?

Not with this proposal. Parallel evaluation of pure code is a separate,
deterministic feature: results in input order, and the same crash a
sequential run would hit. It's listed as a future possibility.

## Are Counts Atomic Now?

Only on platforms that run tasks on several threads at once
(`ParallelTasks`), and only for values that tasks on different threads can
reach. The compiler decides which ones from the program's structure. See
[decision 5](03-decisions/05-refcount-atomicity.md).

## Does a Shared Queue Copy Its List on Every Push?

No. `update!` hands the state to its function, so a state nothing else holds
changes in place. See [decision 10](03-decisions/10-in-place-updates.md).

## Why Are the Names So Plain?

They're placeholders. `Shared`, `update!`, `Concurrently`, `spawn`,
`suspending` and the rest are open for discussion separately from what they
mean.
