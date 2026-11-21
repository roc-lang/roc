# Platforms

This page is for platform authors: the people who write basic-cli, basic-webserver and other platforms, and their hosts. Application and package authors don't need any of it. Their code means the same thing on every platform, and nothing they write changes when a platform opts in.

The full host ABI, with every rule it relies on, is in the main RFC's [Host ABI](../concurrency-explicit-suspension.md#host-abi) section. This page covers what you need to decide and build.

## Choosing a Concurrency Mode

A platform opts in to concurrency with a `concurrency` field in its header:

```roc
platform ""
    requires { main! : List(Str) => Try({}, [Exit(I8), ..]) }
    exposes [Stdout, Http, Sleep]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {
        "roc_stdout_line": Stdout.line!,
        "roc_http_get_utf8": Http.get_utf8!,
        "roc_sleep_millis": Sleep.millis!,
    }
    concurrency: Tasks # This platform's host schedules tasks.
    targets: { … }
```

The field takes one of three values:

| Value | What your host does | What it costs Roc code |
|---|---|---|
| `None` | Schedules no tasks. This is what every platform that doesn't opt in gets | Nothing, unless the program can wait (see [below](#platforms-that-dont-opt-in)) |
| `Tasks` | Runs at most one resume at a time. It may move tasks between threads, as long as each resume happens after the one before it ends | Pausing and resuming; no atomic counts for values shared with child tasks |
| `ParallelTasks` | Runs resumes at the same time on several threads | The same, plus atomic counts on values that child tasks may share |

Everything this page says about `Tasks` applies to `ParallelTasks` too. The only difference is reference counting, which [decision 5](03-decisions/05-refcount-atomicity.md) covers.

Declare `Tasks` unless your host really runs tasks in parallel. That covers wasm hosts without threads, JavaScript event loops, and single-threaded `epoll` or `io_uring` loops, and none of them pay for atomic counts.

> Note that the `concurrency` field is part of the compilation key of every module in the program, like the target. It changes the code generated for Builtin and for packages that never import your platform.

## Marking Hosted Functions That Can Pause

A hosted function that may return to the host before its result is ready is *suspending*. You mark it where it's declared, in its hosted type module:

```roc
# Http.roc
Http := [].{
    suspending get_utf8! : Str => Try(Str, [HttpErr(U16)])
}
```

A hosted function you don't mark, like `Stdout.line!`, always completes before it returns, and costs nothing extra. Mark only the functions that really wait on something: every call to a `suspending` function pays a flag check, and the code that calls it gets compiled for pausing.

The marker lives with the declaration, not in the platform header, because packages and `.boxy` units don't import the platform's root module. They still need to know which calls can pause.

A `suspending` declaration in a platform that doesn't declare `concurrency: Tasks` or `concurrency: ParallelTasks` gives an error at compile time. In a `None` build hosted calls never pause, so the marker would mean nothing there.

## What the Host Implements

Your host implements the *task ABI*, a small set of C functions shared with compiled Roc code through linker symbols, and a scheduler. Only `Tasks` and `ParallelTasks` platforms see any of it. The new runtime symbols join the fixed set in `host_abi.ExternHostFns` and the glue templates for opting-in platforms.

```c
typedef struct RocTask RocTask;              // opaque, owned by Roc
typedef enum { ROC_READY, ROC_PENDING } RocPoll;
typedef enum {
    ROC_TASK_PENDING, ROC_TASK_DONE, ROC_TASK_CANCELLED, ROC_TASK_CRASHED
} RocTaskStatus;

// Each `provides` entry, under its header symbol, starts a root task.
RocTask *roc_main(RocList args, void *result_out);

// The Roc runtime exports these.
RocTaskStatus roc_task_resume(RocTask *task);
void roc_task_wake(RocTask *task);           // a pending hosted result has been written
void roc_task_cancel(RocTask *root);         // request cancellation of a root task
void roc_task_free(RocTask *root);           // after DONE, CANCELLED or CRASHED
RocStr roc_task_crash_message(RocTask *root);  // after CRASHED; borrowed

// Your host implements these.
void roc_task_ready(RocTask *task);          // resume this task once, on any thread
void roc_task_abort(RocTask *task);          // abandon the operation it's waiting on

// A suspending hosted function gains its task and a result slot.
RocPoll roc_http_get_utf8(RocTask *task, HttpResult *out, RocStr url);
```

A suspending hosted function either finishes at once, writes `*out` and returns `ROC_READY`, or starts the operation and returns `ROC_PENDING`. In the second case you later write `*out` exactly once and call `roc_task_wake(task)`. The Roc runtime then calls your `roc_task_ready` when the task should run again, and your scheduler calls `roc_task_resume` on it.

Return `ROC_READY` whenever you can, for buffered data or a cache hit. It costs one branch, while `ROC_PENDING` costs a pause and a resume.

You never track which task is waiting on what, or whether a wake is stale. The runtime's wake protocol guarantees you get at most one `roc_task_ready` per pause, and that a completion arriving while the task is still running is never lost.

Child tasks belong to Roc. Your scheduler sees them only through `roc_task_ready` and resumes them like any other task. After `roc_task_resume` returns for a child, forget it.

A minimal single-threaded host loop, in Zig:

```zig
var result: MainResult = undefined;
const root = roc_main(args, &result);
scheduler.push(root);
while (scheduler.pop()) |task| switch (roc_task_resume(task)) {
    .pending => {}, // a hosted completion or a child will ready it again
    .done => if (task == root) { finish(result); roc_task_free(root); },
    .cancelled => if (task == root) roc_task_free(root),
    .crashed => if (task == root) { report(roc_task_crash_message(root)); roc_task_free(root); },
};
// roc_task_ready(task) pushes onto `scheduler`.
```

A wake-and-ready protocol fits io_uring, IOCP, JavaScript promises and callback-based APIs directly. Hosts don't poll tasks the way Rust's `Future::poll` does.

## Roots and Supervision

Each `provides` entry starts a *root task*. You resume it once after creating it, then once per `roc_task_ready`, until it reports `ROC_TASK_DONE`, `ROC_TASK_CANCELLED` or `ROC_TASK_CRASHED`. Then you read `result_out` (only for `DONE`) or the crash message (only for `CRASHED`) and call `roc_task_free`.

Roc follows Erlang/OTP: a crash ends the task it happens in, cancels the tasks linked to it, and propagates upward until a supervisor decides what to do. Your platform supervises every root. A crashed root is your decision:

- **A web server** can fail just the crashed request and keep serving the others, much as an OTP supervisor restarts one worker.
- **A command-line tool** can exit with an error.
- **Any platform** can start the entry again.

Roc code can't catch a crash, so your platform is the only place a crash that escapes every `supervise!` gets handled.

To cancel a root, for example when a client disconnects, call `roc_task_cancel` and keep resuming the root when it's readied, until it reports a terminal status. A cancelled task stops at its current suspension point and releases everything it owns.

The task a crash happens in leaks what its running native frames owned, since their state at the moment of the crash is unknown. Everything else is released exactly.

> Note that a platform that crashes often can get that memory back with per-root region allocation, as the `nea` server does per request. It must free a region only when nothing outside the crashed tree can reach it.

Crash isolation needs a non-local exit back to `roc_task_resume`: `src/sljmp` on native targets, and WebAssembly exception handling on wasm. On a target that has neither (`src/sljmp` doesn't cover 32-bit x86 yet, for example), a crash ends every task in the process, as crashes do today.

## Cancelling Hosted Calls

When a task is cancelled or crashes while waiting on a hosted call, the runtime calls your `roc_task_abort(task)`. This happens when a loop exits early, when `race!` cancels the losers, or when a crash propagates through a waiting parent.

After `roc_task_abort` returns, you must never write that task's result slot or wake it. If the abandoned operation finishes anyway, release its result yourself.

The safe way to meet this is to write the result and call `roc_task_wake` while holding the operation's lock, the same lock `roc_task_abort` takes. Waking after releasing that lock can wake a freed task. The prototype's model checker finds exactly that race.

Aborting is local. An effect the operation already performed stays performed, and a remote operation may keep running after you abandon it.

## Platform Obligations

These are the rules your host must follow. A debug harness that a host links against checks each one as it happens, and debug builds of the runtime trap on any invalid wake-state transition. Breaking P7 can only hang tasks; breaking any other rule is undefined behavior, as with a `roc_alloc` that returns null today.

| | Obligation |
|---|---|
| P1 | **Resume discipline.** Resume a root once after creating it, and a task once per `roc_task_ready`. Never resume from inside a hosted function or inside another resume |
| P2 | **Handoff ordering.** A resume on another thread happens after the event that readied the task. Any mutex or queue handoff provides this |
| P3 | **Completion.** For each `ROC_PENDING`, write `*out` exactly once, then call `roc_task_wake` exactly once, and do neither after an abort |
| P4 | **Abort finality.** After `roc_task_abort` returns, never touch that task's result slot or wake it, and release any result that arrives late |
| P5 | **Ownership.** Release hosted arguments exactly once, even when the operation completes later. What you write to `*out` belongs to Roc |
| P6 | **Cross-thread allocation.** Allow an allocation to be released on a different thread from the one that made it, or keep every task and shared value on one allocator's thread |
| P7 | **Liveness.** Eventually resume every readied task, and eventually complete or abort every pending operation |
| P8 | **Root lifecycle.** After a terminal status, read `result_out` only for `DONE` and the crash message only for `CRASHED`, then free the root exactly once and never use it again |
| P9 | **Region safety**, if you reclaim a crashed tree's memory in bulk: free a region only when nothing outside the crashed tree can reach it |
| P10 | **Serial resumes**, for `Tasks` platforms: never run two resumes at the same time |

The full statements, and how the runtime's own guarantees fit with them, are in the RFC's [Platform obligations](../concurrency-explicit-suspension.md#platform-obligations).

## Platforms That Don't Opt In

A platform that doesn't opt in gets concurrency that runs on one thread, scheduled by the program itself. You implement nothing new, and existing hosted functions keep working.

- **Hosted calls block.** Every hosted call finishes before it returns, as today.
- **Tasks take turns.** The program runs its own tasks one at a time, always picking the ready task that comes first in the task tree. A concurrent loop whose entries never wait for another task runs them one at a time, in order.
- **Waiting still works.** A task that waits for another, such as one waiting for a permit or for an item in a queue, pauses, and the next ready task runs. If no task can run, the program crashes with a deadlock message.
- **Crashes still stop the program**, as they do today, and `supervise!` runs its function once.

So `all!` and `both!` return the same results as on a task platform, and `race!` returns the first function's result without starting the others. A timeout can't interrupt a hosted call that blocks.

A program that can't wait costs nothing on these platforms in optimized (`.lss`) builds: the compiler's pass does nothing. In `.boxy` dev builds, every call through an erased effectful function value still gets a flag check. A program that can wait pays a flag check after each call that may reach a wait. [Decision 4](03-decisions/04-one-meaning-per-build.md) explains why every build means the same thing.

## Making Your Host Fast

- **Return `ROC_READY` when you can.** It costs one branch.
- **Run a parent soon after its last child readies it**, as tokio's "LIFO slot" does. Completions on the task's own thread need no atomics; other threads need one compare-and-swap per pause.
- **Declare `Tasks` rather than `ParallelTasks`** unless your host really resumes tasks in parallel.

Nothing about host performance has been measured yet in compiled code. See [open questions](08-open-questions.md).

## Open Items for Platforms

- **Hosted functions that hold a permit.** When a task holding a [shared-state](03-decisions/06-shared-state.md) `Semaphore` permit is cancelled during a hosted call, the call is aborted locally but the remote operation may still run. The permit then counts local scopes, not remote operations. A hosted function that took the `Semaphore` and released the permit when the platform had finished with the operation would fix this. It's listed under the shared-state RFC's [Future possibilities](../concurrency-shared-variables.md#future-possibilities) and isn't designed yet.
- **Hosted functions that take `Shared(s)`**, for metrics the platform reads, are a future possibility in the same list.
- **Wasm hosts.** How hosts map tasks onto WASI 0.3 and onto JavaScript promises gets settled by building browser and WASI 0.3 reference hosts, the last step of the [implementation plan](../concurrency-explicit-suspension.md#implementation-plan).
