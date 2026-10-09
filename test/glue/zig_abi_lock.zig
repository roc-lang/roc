//! ABI lock for generated Zig glue.
//!
//! The glue templates restate parts of the host ABI as text in a foreign
//! output language, so they cannot import `builtins` directly. This file is
//! the enforcement for that mirror: the build generates `roc_platform_abi.zig`
//! with `roc glue` and compiles this root against it (as `glue_abi`) and the
//! canonical `builtins` definitions. Any drift between the ZigGlue template
//! and the canonical ABI is a compile error here.
//!
//! `RocHost` is deliberately not the full `RocOps`: compiled Roc code reaches
//! the host through direct linker symbols, and hosted dispatch is
//! symbol-based, so the generated host-internal vtable has no `hosted_fns`
//! field. What this lock enforces is the intended relationship: `RocHost` is
//! exactly the `env` + callback prefix of `RocOps`, with `*RocHost` in the
//! self-pointer position.

const std = @import("std");
const builtins = @import("builtins");
const abi = @import("glue_abi");

const RocOps = builtins.host_abi.RocOps;
const extern_host = builtins.host_abi.ExternHostFns;
const erased_callable = builtins.erased_callable;
const shim_symbols = builtins.shim_symbols;

comptime {
    lockStruct("RocStr", abi.RocStr, builtins.str.RocStr, &.{ "bytes", "capacity_or_alloc_ptr", "length" });
    lockStruct("RocList", abi.RocList(u8), builtins.list.RocList, &.{ "elements_ptr", "length", "capacity_or_alloc_ptr" });
    lockStruct("RocDec", abi.RocDec, builtins.dec.RocDec, &.{"num"});
    lockRocHost();
    lockRuntimeSymbols();
    lockErasedCallable();
}

/// Assert `Generated` matches `Canonical` in size, alignment, field count, and
/// per-field offset and size. `generated_field_names` pins the generated field
/// order; names may differ from the canonical ones (e.g. the typed
/// `elements_ptr` vs the untyped `bytes`), so field identity is positional.
fn lockStruct(
    comptime what: []const u8,
    comptime Generated: type,
    comptime Canonical: type,
    comptime generated_field_names: []const []const u8,
) void {
    if (@sizeOf(Generated) != @sizeOf(Canonical)) {
        @compileError("generated " ++ what ++ " size differs from builtins");
    }
    if (@alignOf(Generated) != @alignOf(Canonical)) {
        @compileError("generated " ++ what ++ " alignment differs from builtins");
    }

    const generated_fields = @typeInfo(Generated).@"struct".field_names;
    const canonical_fields = @typeInfo(Canonical).@"struct".field_names;
    if (generated_fields.len != canonical_fields.len) {
        @compileError("generated " ++ what ++ " field count differs from builtins");
    }
    if (generated_fields.len != generated_field_names.len) {
        @compileError("generated " ++ what ++ " field list is out of date in zig_abi_lock.zig");
    }

    inline for (generated_fields, canonical_fields, generated_field_names) |gf, cf, expected_name| {
        if (!std.mem.eql(u8, gf, expected_name)) {
            @compileError("generated " ++ what ++ " field order changed: expected " ++
                expected_name ++ ", found " ++ gf);
        }
        const canonical_name = if (std.mem.eql(u8, expected_name, "elements_ptr")) "bytes" else expected_name;
        if (!std.mem.eql(u8, cf, canonical_name)) {
            @compileError("canonical " ++ what ++ " field order changed: expected " ++ canonical_name);
        }
        if (@offsetOf(Generated, gf) != @offsetOf(Canonical, cf)) {
            @compileError("generated " ++ what ++ "." ++ gf ++ " offset differs from builtins " ++
                what ++ "." ++ cf);
        }
        if (@FieldType(Generated, gf) != @FieldType(Canonical, cf)) {
            @compileError("generated " ++ what ++ "." ++ gf ++ " type differs from builtins");
        }
        if (@sizeOf(@FieldType(Generated, gf)) != @sizeOf(@FieldType(Canonical, cf))) {
            @compileError("generated " ++ what ++ "." ++ gf ++ " size differs from builtins " ++
                what ++ "." ++ cf);
        }
    }
}

/// Assert `RocHost` is the `env` + callback prefix of `RocOps`: same field
/// names in the same order at the same offsets, and every callback signature
/// identical after substituting `*RocHost` for `*RocOps` in the self-pointer
/// position.
fn lockRocHost() void {
    const host_fields = @typeInfo(abi.RocHost).@"struct".field_names;
    const ops_fields = @typeInfo(RocOps).@"struct".field_names;

    // RocOps = RocHost prefix + hosted_fns.
    if (host_fields.len + 1 != ops_fields.len) {
        @compileError("generated RocHost field count is not RocOps minus hosted_fns");
    }
    if (!std.mem.eql(u8, ops_fields[ops_fields.len - 1], "hosted_fns")) {
        @compileError("RocOps no longer ends with hosted_fns; update ZigGlue's RocHost and this lock");
    }

    if (@alignOf(abi.RocHost) != @alignOf(RocOps) or
        @sizeOf(abi.RocHost) != @offsetOf(RocOps, "hosted_fns"))
    {
        @compileError("generated RocHost size/alignment differs from the RocOps helper prefix");
    }

    inline for (host_fields, ops_fields[0..host_fields.len]) |hf, of| {
        if (!std.mem.eql(u8, hf, of)) {
            @compileError("generated RocHost field " ++ hf ++ " does not match RocOps field " ++ of);
        }
        if (@offsetOf(abi.RocHost, hf) != @offsetOf(RocOps, of)) {
            @compileError("generated RocHost." ++ hf ++ " offset differs from RocOps");
        }
        if (@FieldType(abi.RocHost, hf) != @FieldType(RocOps, of) and !fnPointersMatchModuloSelf(@FieldType(abi.RocHost, hf), @FieldType(RocOps, of))) {
            @compileError("generated RocHost." ++ hf ++ " signature differs from RocOps." ++ of);
        }
    }
}

/// Assert the generated extern runtime-symbol declarations exist under the
/// canonical names with the canonical signatures.
fn lockRuntimeSymbols() void {
    inline for (shim_symbols.runtime_set) |name| {
        if (!@hasDecl(abi, name)) {
            @compileError("generated glue is missing extern " ++ name);
        }
        if (*const @TypeOf(@field(abi, name)) != @field(extern_host, name)) {
            @compileError("generated extern " ++ name ++ " signature differs from host_abi.extern_host");
        }
    }
}

fn lockErasedCallable() void {
    if (!fnPointersMatchModuloSelf(abi.RocErasedCallableFn, erased_callable.ErasedCallableFn)) {
        @compileError("generated RocErasedCallableFn signature differs from builtins.erased_callable");
    }
    if (!fnPointersMatchModuloSelf(abi.RocErasedCallableOnDrop, erased_callable.OnDropFn)) {
        @compileError("generated RocErasedCallableOnDrop signature differs from builtins.erased_callable");
    }

    const generated_fields = @typeInfo(abi.RocErasedCallablePayload).@"struct".field_names;
    const canonical_fields = @typeInfo(erased_callable.Payload).@"struct".field_names;
    if (generated_fields.len != canonical_fields.len) {
        @compileError("generated RocErasedCallablePayload field count differs from builtins");
    }
    inline for (generated_fields, canonical_fields) |gf, cf| {
        if (!std.mem.eql(u8, gf, cf)) {
            @compileError("generated RocErasedCallablePayload field " ++ gf ++
                " does not match builtins field " ++ cf);
        }
        if (@offsetOf(abi.RocErasedCallablePayload, gf) != @offsetOf(erased_callable.Payload, cf)) {
            @compileError("generated RocErasedCallablePayload." ++ gf ++ " offset differs from builtins");
        }
    }
    if (@sizeOf(abi.RocErasedCallablePayload) != @sizeOf(erased_callable.Payload)) {
        @compileError("generated RocErasedCallablePayload size differs from builtins");
    }

    if (abi.roc_erased_callable_capture_alignment != erased_callable.capture_alignment) {
        @compileError("generated erased-callable capture alignment differs from builtins");
    }
}

/// Whether two `*const fn` pointer types have identical signatures after
/// treating the generated `*abi.RocHost` and the canonical `*RocOps` as the
/// same type in the self-pointer position.
fn fnPointersMatchModuloSelf(comptime Generated: type, comptime Canonical: type) bool {
    const generated_info = @typeInfo(@typeInfo(Generated).pointer.child).@"fn";
    const canonical_info = @typeInfo(@typeInfo(Canonical).pointer.child).@"fn";

    if (generated_info.param_types.len != canonical_info.param_types.len) return false;
    if (generated_info.attrs.varargs != canonical_info.attrs.varargs) return false;
    if (generated_info.return_type != canonical_info.return_type) return false;
    if (!std.meta.eql(generated_info.attrs.@"callconv", canonical_info.attrs.@"callconv")) return false;

    inline for (generated_info.param_types, canonical_info.param_types) |generated_param, canonical_param| {
        if (canonical_param == *RocOps) {
            if (generated_param != *abi.RocHost) return false;
        } else if (generated_param != canonical_param) {
            return false;
        }
    }
    return true;
}
