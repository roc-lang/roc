# Issue 9709: External typed numeric suffix reaches checking without suffix target

## Reproduction

The local regression test is `issue 9709: typed numeric suffix uses resolved external target for from_numeral` in `src/check/test/issue_9709_test.zig`.

```roc
main! = |_args| {
    _ = 12.Str
    Ok({})
}
```

It crashes in `src/check/Check.zig`:

```text
typed numeric literal reached checking without a canonicalized suffix target
```

## Background

Typed numeric suffixes such as `123.U64` should be normal explicit type targets for `from_numeral`. Canonicalization owns suffix scope resolution and checking should consume the resolved type directly:

```zig
const suffix_type = self.cir.numericSuffixTypeForNode(ModuleEnv.nodeIdxFrom(expr_idx)) orelse {
    std.debug.panic("typed numeric literal reached checking without a canonicalized suffix target", .{});
};
```

For builtins, this can fold to a literal when the value is exactly representable. For custom or non-builtin targets, checking should still type-check the explicit `from_numeral` conversion against the resolved type. There should be no suffix-specific type system and no later name lookup.

## Root Cause

`12.Str` is syntactically parsed as a typed numeric literal, and canonicalization verifies that `Str` is a type binding. But `recordTypedNumericSuffix` only records builtin numeric types and local type declarations:

```zig
if (builtinNumKindFromTypeIdent(...)) ...
if (try self.scopeLookupOrPrepareTypeDecl(type_ident)) |stmt_idx| ...
```

For an external builtin nominal such as `Str`, scope lookup succeeds enough to avoid an undeclared-type diagnostic, but no explicit suffix target is recorded. Checking then sees a literal form that claims to have an explicit suffix but has no resolved target and panics.

Semantically, `12.Str` should not be a canonicalization failure just because `Str` is not numeric. It should mean: resolve `Str` as the explicit target type, call/check `Str.from_numeral`, and report a normal missing-method error because `Str` does not implement `from_numeral`.

## Ideal Long-Term Fix

Canonicalization should make typed numeric suffix resolution total:

1. Resolve the suffix identifier to an explicit normal type target: builtin, local declaration, or external declaration.
2. Checking should use that target as the explicit result type for `from_numeral`.
3. Declared-but-non-numeric targets such as `Str` should proceed to normal static-dispatch validation and report `MISSING METHOD` for `from_numeral`.
4. Missing or otherwise unresolvable targets should be represented explicitly as an error target so checking consumes explicit data and never redoes suffix lookup or panics on absence.

The long-term shape should remove the parallel node-id side table and put the resolved suffix target directly on the typed numeric CIR nodes. The immediate fix keeps the side table but makes it total and treats external suffixes as normal type targets.
