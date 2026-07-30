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
const extern_host = builtins.host_abi.extern_host;
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

    const generated_fields = @typeInfo(Generated).@"struct".fields;
    const canonical_fields = @typeInfo(Canonical).@"struct".fields;
    if (generated_fields.len != canonical_fields.len) {
        @compileError("generated " ++ what ++ " field count differs from builtins");
    }
    if (generated_fields.len != generated_field_names.len) {
        @compileError("generated " ++ what ++ " field list is out of date in zig_abi_lock.zig");
    }

    inline for (generated_fields, canonical_fields, generated_field_names) |gf, cf, expected_name| {
        if (!std.mem.eql(u8, gf.name, expected_name)) {
            @compileError("generated " ++ what ++ " field order changed: expected " ++
                expected_name ++ ", found " ++ gf.name);
        }
        if (@offsetOf(Generated, gf.name) != @offsetOf(Canonical, cf.name)) {
            @compileError("generated " ++ what ++ "." ++ gf.name ++ " offset differs from builtins " ++
                what ++ "." ++ cf.name);
        }
        if (@sizeOf(gf.type) != @sizeOf(cf.type)) {
            @compileError("generated " ++ what ++ "." ++ gf.name ++ " size differs from builtins " ++
                what ++ "." ++ cf.name);
        }
    }
}

/// Assert `RocHost` is the `env` + callback prefix of `RocOps`: same field
/// names in the same order at the same offsets, and every callback signature
/// identical after substituting `*RocHost` for `*RocOps` in the self-pointer
/// position.
fn lockRocHost() void {
    const host_fields = @typeInfo(abi.RocHost).@"struct".fields;
    const ops_fields = @typeInfo(RocOps).@"struct".fields;

    // RocOps = RocHost prefix + hosted_fns.
    if (host_fields.len + 1 != ops_fields.len) {
        @compileError("generated RocHost field count is not RocOps minus hosted_fns");
    }
    if (!std.mem.eql(u8, ops_fields[ops_fields.len - 1].name, "hosted_fns")) {
        @compileError("RocOps no longer ends with hosted_fns; update ZigGlue's RocHost and this lock");
    }

    inline for (host_fields, ops_fields[0..host_fields.len]) |hf, of| {
        if (!std.mem.eql(u8, hf.name, of.name)) {
            @compileError("generated RocHost field " ++ hf.name ++ " does not match RocOps field " ++ of.name);
        }
        if (@offsetOf(abi.RocHost, hf.name) != @offsetOf(RocOps, of.name)) {
            @compileError("generated RocHost." ++ hf.name ++ " offset differs from RocOps");
        }
        if (hf.type != of.type and !fnPointersMatchModuloSelf(hf.type, of.type)) {
            @compileError("generated RocHost." ++ hf.name ++ " signature differs from RocOps." ++ of.name);
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
        if (@TypeOf(@field(abi, name)) != @TypeOf(@field(extern_host, name))) {
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

    const generated_fields = @typeInfo(abi.RocErasedCallablePayload).@"struct".fields;
    const canonical_fields = @typeInfo(erased_callable.Payload).@"struct".fields;
    if (generated_fields.len != canonical_fields.len) {
        @compileError("generated RocErasedCallablePayload field count differs from builtins");
    }
    inline for (generated_fields, canonical_fields) |gf, cf| {
        if (!std.mem.eql(u8, gf.name, cf.name)) {
            @compileError("generated RocErasedCallablePayload field " ++ gf.name ++
                " does not match builtins field " ++ cf.name);
        }
        if (@offsetOf(abi.RocErasedCallablePayload, gf.name) != @offsetOf(erased_callable.Payload, cf.name)) {
            @compileError("generated RocErasedCallablePayload." ++ gf.name ++ " offset differs from builtins");
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

    if (generated_info.params.len != canonical_info.params.len) return false;
    if (generated_info.return_type != canonical_info.return_type) return false;
    if (!std.meta.eql(generated_info.calling_convention, canonical_info.calling_convention)) return false;

    inline for (generated_info.params, canonical_info.params) |generated_param, canonical_param| {
        if (canonical_param.type == *RocOps) {
            if (generated_param.type != *abi.RocHost) return false;
        } else if (generated_param.type != canonical_param.type) {
            return false;
        }
    }
    return true;
}
