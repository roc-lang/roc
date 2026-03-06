# Glue Codegen Gaps

Known limitations in `roc glue` Zig code generation (`src/glue/src/ZigGlue.roc`).

## Nested/anonymous records resolve to `*anyopaque`

When a hosted function returns a tuple or record containing nested anonymous records,
the nested record fields are generated as `*anyopaque` instead of a proper extern struct.

**Example:**

```roc
test! : Str => ({field1: I32, field2: Str}, Str)
```

Generates:

```zig
pub const StderrTestRetRecord = extern struct {
    _0: *anyopaque,  // should be a nested struct with field1/field2
    _1: RocStr,
};
```

**Root cause:** `type_repr_to_zig` returns `*anyopaque` when `rec.name == ""`. Anonymous
records (those without a named type alias) have an empty name in the type table. The
codegen would need to either:

1. Generate inline anonymous structs for unnamed records, or
2. Assign synthetic names (e.g. `StderrTestRetRecord_0`) and emit separate struct definitions

## Box types resolve to `*anyopaque`

`Box(T)` is represented as a heap pointer in the type table but the glue generator
doesn't unwrap it to `*T`. Both args and return positions are affected.

```roc
test! : Box(List(Str)) => Box(Str)
```

Generates `*anyopaque` for both the arg and return type instead of `*RocList(RocStr)`
and `*RocStr`.

## Nested record fields in tuple return types

When a tuple element is itself a record with sub-records:

```roc
test! : Str => ({field1: I32, field3: { sub_field: Str, list: List(U8) }}, Str)
```

Both the outer record (`_0`) and its nested `field3` record would need struct
definitions generated. Currently neither is emitted.
