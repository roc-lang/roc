# Issue 9705: Bool alias panic during boolean operator canonicalization

## Reproduction

The local regression test is `issue 9705: Bool shadowing warns but boolean operators still use builtin Bool` in `src/check/test/issue_9705_test.zig`.

The reproducer is:

```roc
Bool : U8

main! = |_args| {
    a = True
    b = False
    _ = a or b
    Ok({})
}
```

It crashes in `src/canonicalize/Can.zig`:

```text
Bool type binding was not a nominal type during boolean operator canonicalization
```

## Background

Canonicalization desugars `and`/`or` by synthesizing `Bool.True` or `Bool.False` tag expressions through `addBoolTagExpr`.

That helper does a normal type-binding lookup for the identifier `Bool`:

```zig
const binding_location = (try self.scopeLookupTypeBinding(self.env.idents.bool)) orelse ...

return switch (binding_location.binding.*) {
    .local_nominal, .associated_nominal => ...
    .external_nominal => ...
    .local_alias => @panic(...)
};
```

The design says `Bool` is an ordinary nominal tag union at runtime, but the builtin `Bool` identity must remain explicit compiler data.

## Root Cause

The canonicalizer is using ordinary source scope lookup for the builtin `Bool` required by boolean operator desugaring. A user local alias named `Bool` shadows the builtin nominal binding. The canonicalizer then tries to wrap generated `True`/`False` tags in that user alias and panics because aliases are not nominal tag constructors.

This is a producer bug in canonicalization. The generated boolean tags are not references to the user's local `Bool`; they are compiler-generated references to the builtin nominal `Bool`.

Separately, shadowing a builtin name should still be accepted as a source definition and reported as a shadowing warning. The old behavior accidentally allowed the shadowing to proceed in some paths but did not reliably report the warning for auto-imported builtin type bindings.

## Fix

Boolean operator desugaring now consumes explicit builtin identity, not source-name lookup:

1. `Can.addBoolTagExpr` uses the auto-imported compiler-owned `Builtin.Bool` entry when synthesizing `True` and `False` for `and`/`or`.
2. User source may still define or shadow a type named `Bool`; generated boolean operators keep referencing the imported builtin `Bool` by checked identity.
3. Replacing an auto-imported external type binding now emits the existing `shadowing_warning`, so the shadowing succeeds while still warning.

The regression test verifies both requirements: `Bool : U8` plus `a or b` no longer panics, and canonicalization reports exactly one shadowing warning.
