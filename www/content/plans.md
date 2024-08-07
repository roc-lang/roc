# Plans

This is not a roadmap, but it is a set of current plans for the language. Plans can change, of course, but the goal here is to lay out some of the current plans. It won't be an exhaustive list, but it should give you the highlights.

## [Planned Breaking Changes](#planned-breaking-changes) {#planned-breaking-changes}

These are changes that are both planned and planned to be breaking (so, not backwards-compatible), meaning you may need to make changes to your Roc code if they end up getting released.

The best time to make breaking changes (that will benefit more and more people as the community grows) is when the number of affected code bases is small. That said, the frequency of breaking changes should naturally decrease over time, and of course past a certain level of maturity, the number of justifiable breaking changes approaches zero. (That level of maturity is quite a ways away!)

### [Builtins](#builtins) {#builtins}

Currently, [builtins](https://www.roc-lang.org/builtins) get breaking changes from time to time. There aren't any specific plans to make particular breaking changes to them, because typically when we decide a change is warranted, we discuss and implement the change pretty quickly.

As an example, we had [a discussion](https://roc.zulipchat.com/#narrow/stream/304641-ideas/topic/Drop.20n.20elements.20from.20the.20end.20of.20a.20list) about changing the API for how elements get dropped from a list, and then a week later [announced](https://roc.zulipchat.com/#narrow/stream/397893-announcements/topic/List.2Edrop.2C.20dropFirst.2C.20dropLast) that the change had shipped.

This has been consistently happening a few times per year. It's hard to predict exactly what the next one will be, but it's a safe bet that it will happen again.

### [Import syntax](#import-syntax) {#import-syntax}

Implementing the very important [module params](https://docs.google.com/document/d/110MwQi7Dpo1Y69ECFXyyvDWzF4OYv1BLojIm08qDTvg/edit?usp=sharing) feature requires a breaking syntax change to how imports work. This plan is not at all tentative; there is a high degree of confidence that it will happen!

Work has not started on this yet, but we'd like to have the project completed sometime in 2024.

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
