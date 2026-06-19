# Issue 9706: Numeric addition default app dispatch plan lacks method owner

## Reproduction

The local CLI repro is `test/cli/issue_9706_dispatch_plan_addition.roc`, wired in `src/cli/test/parallel_cli_runner.zig`.

```roc
main! = |_args| {
    a1 = 1
    a2 = 2
    Ok(a1 + a2)
}
```

Running the default app path crashes during Monotype lowering:

```text
postcheck invariant violated: dispatch plan had no method owner and no structural equality permission
```

The panic is in `src/postcheck/monotype/lower.zig`:

```zig
const owner = methodOwnerFromType(..., dispatcher_ty) orelse {
    if (plan.result_mode == .equality and plan.result_mode.equality.structural_allowed) return null;
    Common.invariant("dispatch plan had no method owner and no structural equality permission");
};
```

## Background

Arithmetic is represented as static dispatch. Post-check lowering expects each checked dispatch plan to identify a dispatcher type that has a method owner, unless the plan is structural equality.

For numeric literals, checking must resolve `from_numeral`/arithmetic constraints before checked artifact publication. Post-check is not allowed to guess a default number type.

## Root Cause

The default echo platform required `main! : List(Str) => Try({}, [Exit(I8), ..])` and matched `Ok({})`. That forced the user program's success payload to `{}` even though the platform ignores it.

For this repro, the `Ok(a1 + a2)` payload was lowered at the expected type `{}`. The `+` dispatch plan then reached Monotype with `{}` as its dispatcher type, which has no method owner. Since arithmetic is not structural equality, `dispatchTarget` correctly treated that as a checked-boundary invariant violation.

## Fix

The default echo platform now requires `main! : List(Str) => Try(_, [Exit(I8), ..])` and matches `Ok(_)` in all default-platform variants. This makes the ignored success payload explicit, so `a1 + a2` remains governed by its numeric constraints and defaults to the language-defined numeric default before post-check lowering.

Post-check keeps the existing invariant; it does not infer numeric owners or recover from ownerless arithmetic dispatch.
