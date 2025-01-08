# Plans

This is not a roadmap, but it is a set of current plans for the language. Plans can change, of course, but the goal here is to lay out some of the current plans.

This is not an exhaustive list, it aims to highlight that there are significant changes to the language, tooling, and broader ecosystem ahead.

## [Planned Breaking Changes](#planned-breaking-changes) {#planned-breaking-changes}

These are changes that are both planned and planned to be breaking (so, not backwards-compatible), meaning you may need to make changes to your Roc code if they end up getting released.

The best time to make breaking changes (that will benefit more and more people as the community grows) is when the number of affected code bases is small. That said, the frequency of breaking changes should naturally decrease over time, and of course past a certain level of maturity, the number of justifiable breaking changes approaches zero. (That level of maturity is quite a ways away!)

### [Builtins](#builtins) {#builtins}

Currently, [builtins](https://www.roc-lang.org/builtins) get breaking changes from time to time.

The builtins are being migrated from `camelCase` to `snake_case`. This is a breaking change, and will likely take some time to be completed across the ecosystem of packages, platforms, and example code.

### [Purity Inference](#purity-inference) {#purity-inference}

Recently the changes outlined in the [design proposal for Purity Inference](https://docs.google.com/document/d/1ZVD3h5jLpQNFSDXTg2RkzPhNXz5EErUXBBjN8TuyiqQ/edit?usp=sharing) were released in the basic-cli and basic-webserver platforms (among others).

Therefore `Task` is no longer required and has been deprecated.

This is a significant upgrade to the language and will take some time for the ecosystem to upgrade.

### [Static Dispatch](#static-dispatch) {#static-dispatch}

There are significant changes outlined in the [static dispatch design proposal](https://docs.google.com/document/d/1OUd0f4PQjH8jb6i1vEJ5DOnfpVBJbGTjnCakpXAYeT8/edit?usp=sharing). These are being discussed and worked on, but likely to affect other features like abilities, record default fields, and potentiall module params.

### [Syntax changes](#syntax-changes) {#syntax-changes}

There are a number of syntax changes that are planned, but not yet completed see [tracking issue](https://github.com/roc-lang/roc/issues/7106) for more information.

### [Shadowing](#shadowing) {#shadowing}

Shadowing is [currently disallowed](https://www.roc-lang.org/functional#no-reassignment), which means that once a name has been assigned to a value, nothing in the same scope can assign it again.

The plan is to enable shadowing in a future re-write of the [Canonicalization](https://en.wikipedia.org/wiki/Canonicalization) pass as a trial to see if it's a good idea. If it turns out that shadowing isn't the best fit for Roc, we'll remove it as we've done for other experiments, e.g. backpassing.

### Platform Author Specific Breaking Changes

All of the following changes only affect platform authors.
They will not be noticeable from the Roc application developer perspective.

#### Glue

Much like with builtins, we periodically make changes to code generation or to the `roc glue` API that aren't backwards compatible. When this happens, relevant glue scripts need to be updated and then `roc glue` needs to be re-run on platforms to regenerate their host glue.

As with builtins, it's hard to predict when these will happen and what they'll be, but right now it's a safe bet that they will happen from time to time.

#### Effect Interpreters

Currently, Roc effects directly call functions in the platform.
For example, [Stdout.line](https://github.com/roc-lang/basic-cli/blob/e022fba2b01216678d62f07c2f3ba702e80fa00c/platform/Stdout.roc#L9-L13) in basic-cli calls the [roc_fx_stdoutLine](https://github.com/roc-lang/basic-cli/blob/e022fba2b01216678d62f07c2f3ba702e80fa00c/platform/src/lib.rs#L380-L384) function.
Roc directly calling these functions synchronously greatly limits the possibilities of how a platform can implement the effect.
With the effect interpreter model, on each effect, roc will return a tag union to the platform.
That tag union will contain all of the function arguments along with a continuation closure.
The platform can execute the effect however it likes (including running it asynchronously).
After the effect completes, the platform simply has to call the continuation closure with the results.

In terms of actual implementation, this is quite similar to an async state machine in other languages like Rust.

#### Platform-side Explicit Allocators

Related to the effect interpreter changes, for memory allocation functions (plus a few others), currently Roc always directly calls `roc_alloc/roc_etc`.
This makes it hard to implement more interesting allocation strategies (like arena allocation).
With this change, all calls to Roc will require the platform to pass in an Allocator struct.
Roc will directly use that struct to call each of the allocation related functions.
This struct will also hold a few other functions like `roc_dbg` and `roc_panic`.

## [Planned Non-Breaking Changes](#planned-non-breaking-changes) {#planned-non-breaking-changes}

These are planned changes to how things work, which should be backwards-compatible and require no code changes. These won't include bugfixes, just changing something that currently works as designed to have a different design.

### [Tag Union Refinement](#tag-union-refinement) {#tag-union-refinement}

This doesn't come up a lot, but [the feature](https://github.com/roc-lang/roc/issues/5504) basically means you can match on some tags in a `when`, and then have an `other ->` branch which has the tags you already matched on removed from the union. That means if you later do another `when` on the `other` value, you won't have to match on (or use `_ ->` to ignore) the tags you already matched in the first `when`, like you do today.

This is planned but nobody is currently working on it. It's a quality of life improvement but doesn't unblock anything; today you can just add a `_ ->` branch to the inner `when`, which is undesirable but not a blocker.
