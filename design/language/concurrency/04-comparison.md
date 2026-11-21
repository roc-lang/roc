# Compared With Other Languages

Most readers will judge this design against a concurrency model they already
use. This page puts them side by side. Each row says what the other language
does and where this proposal differs. It isn't a ranking: each language made
its choices for its own constraints.

## At a Glance

| | Roc (proposed) | Go | Rust async | Kotlin, Swift | Erlang/OTP | Haskell |
|---|---|---|---|---|---|---|
| Marking code that can pause | Nothing new: Roc's `=>` already marks effectful code | Nothing | `async fn`, `.await` | `suspend`, `async`/`await` | Nothing | `IO` |
| How a task pauses | Saves live values into small heap records and returns | Own growable stack | State machine per `async fn` | State machine (Kotlin); per-task frame allocator (Swift) | Own process heap and stack | Own green thread stack |
| Structure | Always structured: nothing outlives the loop that started it | Unstructured `go` | Unstructured `spawn`; structure by library | Structured task groups and scopes, plus detached tasks | Unstructured processes, linked and supervised | Unstructured `forkIO`; structure by library |
| Cancellation | Silent, at pause points; no code of the cancelled task runs | Cooperative, through `context` | Dropping the future | Cooperative, checked by the task | Exit signals, trappable except `kill` | Asynchronous exceptions |
| Shared mutable state | `Shared(s)` with pure atomic updates; no locks in user code | Mutexes and channels | `Mutex`, atomics, channels | Actors (Swift), mutexes, channels | None: messages between processes | `MVar`, `TVar` (STM) |
| Crashes | Contained to their task and propagated to a supervisor | A panic kills the program unless recovered | A panic ends the task or thread | Errors propagate through scopes | Links and supervisors | Exceptions |
| Who schedules | The platform | The Go runtime | A library (tokio and others) | The language runtime | The BEAM | The GHC runtime |

## Go

Go gives every goroutine its own growable stack, so any function can block
without marking. Roc gets the same direct style from a different source: its
effectful functions are already marked with `=>`, and only they can pause.

Go can move and grow stacks because it controls its own calling convention.
Roc's native code uses the C ABI, so a stack per task would be fixed in size
([decision 1](03-decisions/01-stackful-green-threads.md)). And by design,
nothing in Roc outlives the loop that started it, so there's no `go`
statement that leaks a goroutine.

## Rust Async

Rust compiles each `async fn` into a state machine, which runs anywhere,
wasm included. The cost is a second kind of function: you write `async` and
`.await`, and sync code can't call async code directly.

This proposal also compiles to code that runs on every wasm engine, without
the second kind of function. Roc's effectful functions are the ones that can
pause, and Roc has no effect polymorphism, so the compiler knows exactly which
procedures to transform ([decision 2](03-decisions/02-explicit-suspension.md)).
It saves frames into heap records when a task pauses, instead of laying out
a state machine for the whole program ahead of time.

## Kotlin and Swift

Both have structured concurrency: Kotlin's coroutine scopes, and Swift's task
groups and `async let`. A concurrent loop here is close to a Swift task group
consumed with `for await`. Two differences:

- **Leaving the loop cancels.** A Swift task group waits for its remaining
  children on a normal exit. Here every exit cancels what's still running,
  unless the plan is shielded, because most loops leave early once they have
  their answer.
- **There's no handle to keep.** A Swift task group is a value passed to a
  closure. Here the running set has no name, so it can't escape the loop. Roc
  has no non-escaping types to enforce that any other way.

## Erlang/OTP

The crash semantics come straight from OTP: let it crash, links that follow the
structure, and supervisors that restart from known state. `supervise!` is a
supervisor, and wrapping each child in one gives `one_for_one`.

Erlang gives each process its own heap, so a crashed process's memory is
reclaimed at once. Roc's heap is shared and reference counted, so a crash
leaks what its running frames owned, and a platform that crashes often uses
region allocation to get the memory back.

OTP shares state by putting it in a process and sending it messages. The
superseded sessions RFC tried that, and it needed a server task per resource
and two messages per acquire. `Shared(s)` keeps the state as a value and
changes it with pure functions instead ([decision 6](03-decisions/06-shared-state.md)).

## Haskell

`update!` is closest to Haskell's `atomicModifyIORef`, with one addition: the
function can answer `Wait`, which is STM's `retry` for a single cell. Purity
does the same job in both languages. A pure update can't perform I/O, so it
can't deadlock on a lock or leave state half-changed.

Haskell's STM retries transactions over many variables optimistically. Here an
update touches exactly one handle and runs under that handle's lock, so long
updates never redo work and locks never nest.

## Lean and Koka

Like Roc, these are functional languages with reference counting and in-place
updates of unique values. Two ideas come from them:

- Lean's `ST.Ref.modify` takes the value out of the reference while its
  function runs, so the function holds the only reference. `update!` does the
  same ([decision 10](03-decisions/10-in-place-updates.md)).
- Both mark an object as shared when a task can reach it, and test that mark on
  every count update. This proposal decides which counts are atomic at compile
  time instead ([decision 5](03-decisions/05-refcount-atomicity.md)).
