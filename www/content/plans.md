# Plans

This is not a roadmap, but it is a set of current plans for the language. Plans can change, of course, but the goal here is to lay out some of the current plans. It won't be an exhaustive list, but it should give you the highlights.

## Planned Breaking Changes

These are changes that are both planned and planned to be breaking (so, not backwards-compatible), meaning you may need to make changes to your Roc code if they end up getting released.

The best time to make breaking changes (that will benefit more and more people as the community grows) is when the number of affected code bases is small. That said, the frequency of breaking changes should naturally decrease over time, and of course past a certain level of maturity, the number of justifiable breaking changes approaches zero. (That level of maturity is quite a ways away!)

### Builtins

Currently, [builtins](https://www.roc-lang.org/builtins) get breaking changes from time to time. There aren't any specific plans to make particular breaking changes to them, because typically when we decide a change is warranted, we discuss and implement the change pretty quickly.

As an example, we had [a discussion](https://roc.zulipchat.com/#narrow/stream/304641-ideas/topic/Drop.20n.20elements.20from.20the.20end.20of.20a.20list) about changing the API for how elements get dropped from a list, and then a week later [announced](https://roc.zulipchat.com/#narrow/stream/397893-announcements/topic/List.2Edrop.2C.20dropFirst.2C.20dropLast) that the change had shipped.

This has been consistently happening a few times per year. It's hard to predict exactly what the next one will be, but it's a safe bet that it will happen again.

### Glue

This one only applies to platform authors.

Much like with builtins, we periodically make changes to code generation or to the `roc glue` API that aren't backwards compatible. When this happens, relevant glue scripts need to be updated and then `roc glue` needs to be re-run on platforms to regenerate their host glue.

As with builtins, it's hard to predict when these will happen and what they'll be, but right now it's a safe bet that they will happen from time to time.

### Import syntax

Implementing the very important [module params](https://docs.google.com/document/d/110MwQi7Dpo1Y69ECFXyyvDWzF4OYv1BLojIm08qDTvg/edit?usp=sharing) feature requires a breaking syntax change to how imports work. This plan is not at all tentative; there is a high degree of confidence that it will happen!

Work has not started on this yet, but we'd like to have the project completed sometime in 2024.

### Removal of `Nat`

We are removing the `Nat` number type in favour of using `U64` as the default. This will further improve the portability of Roc programs, by removing a potential source of different behaviour across architectures.

You can track progress in [this PR](https://github.com/roc-lang/roc/pull/5923).

## Planned Non-Breaking Changes

These are planned changes to how things work, which should be backwards-compatible and require no code changes. These won't include bugfixes, just changing something that currently works as designed to have a different design.

### Tag Union Refinement

This doesn't come up a lot, but [the feature](https://github.com/roc-lang/roc/issues/5504) basically means you can match on some tags in a `when`, and then have an `other ->` branch which has the tags you already matched on removed from the union. That means if you later do another `when` on the `other` value, you won't have to match on (or use `_ ->` to ignore) the tags you already matched in the first `when`, like you do today.

This is planned but nobody is currently working on it. It's a quality of life improvement but doesn't unblock anything; today you can just add a `_ ->` branch to the inner `when`, which is undesirable but not a blocker.

### `Inspect` Inference

When this lands, all Roc types will have a default `implements Inspect`, which you can override if desired. `dbg` will use it to display things, which in turn means you'll be able to customize `dbg` output. Also it will mean you can do things like turning any Roc type into a string and writing it to a log file.

Note that in this design, functions will have an `Inspect` implementation which essentially renders them as `"<function>"` with no other information, and opaque types will be `"<opaque>"` by default unless you customize them. This is important because neither functions nor opaque types should expose their internal details, so that you can safely refactor them without causing regressions in distant parts of the code base because something depended on an internal implementation detail.
