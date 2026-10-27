//! The C-ABI entry points compiled code calls (design.md "Object Symbol
//! Names": the `roc_desc_*` symbols).
//!
//! Compiled code carries no `RocOps`, so these reach the host through
//! `in_process_host.ops()`: in a platform build the process-wide table over
//! the host's runtime symbols, in-process the ops the current thread entered.
//! Composition goes through the process-global cache. Array operands are a
//! count and a pointer to `extern` items. `static_lib.zig` exports these
//! under their own names.

const builtins = @import("builtins");
const desc_mod = @import("desc.zig");
const compose = @import("compose.zig");
const walk = @import("walk.zig");

const RocOps = builtins.host_abi.RocOps;
const RocStr = builtins.str.RocStr;
const TypeDesc = desc_mod.TypeDesc;
const Name = desc_mod.Name;
const Methods = desc_mod.Methods;

fn ops() *RocOps {
    return builtins.in_process_host.ops();
}

fn cache() *compose.Cache {
    return &compose.process_cache;
}

fn items(comptime T: type, count: u32, ptr: ?[*]const T) []const T {
    if (count == 0) return &.{};
    return ptr.?[0..count];
}

fn valuePtr(ptr: ?[*]u8) [*]u8 {
    return ptr orelse walk.zeroSizedPtr();
}

pub fn roc_desc_list(elem: *const TypeDesc) callconv(.c) *const TypeDesc {
    return compose.list(cache(), ops(), elem);
}

pub fn roc_desc_box(payload: *const TypeDesc) callconv(.c) *const TypeDesc {
    return compose.box(cache(), ops(), payload);
}

pub fn roc_desc_recursive_box(payload: *const TypeDesc) callconv(.c) *const TypeDesc {
    return compose.recursiveBox(cache(), ops(), payload);
}

pub fn roc_desc_record(count: u32, fields: ?[*]const compose.FieldInput) callconv(.c) *const TypeDesc {
    return compose.record(cache(), ops(), items(compose.FieldInput, count, fields));
}

pub fn roc_desc_declared_record(count: u32, fields: ?[*]const compose.DeclaredFieldInput) callconv(.c) *const TypeDesc {
    return compose.declaredRecord(cache(), ops(), items(compose.DeclaredFieldInput, count, fields));
}

pub fn roc_desc_tuple(count: u32, elems: ?[*]const *const TypeDesc) callconv(.c) *const TypeDesc {
    return compose.tuple(cache(), ops(), items(*const TypeDesc, count, elems));
}

pub fn roc_desc_tag_union(count: u32, variants: ?[*]const compose.VariantInput) callconv(.c) *const TypeDesc {
    return compose.tagUnion(cache(), ops(), items(compose.VariantInput, count, variants));
}

pub fn roc_desc_args(count: u32, descs: ?[*]const *const TypeDesc) callconv(.c) *const TypeDesc {
    return compose.args(cache(), ops(), items(*const TypeDesc, count, descs));
}

pub fn roc_desc_nominal(backing: *const TypeDesc, name: ?*const Name, methods: ?*const Methods, inspect_opaque: bool) callconv(.c) *const TypeDesc {
    return compose.nominal(cache(), ops(), backing, name, methods, inspect_opaque);
}

pub fn roc_desc_copy(desc: *const TypeDesc, dst: ?[*]u8, src: ?[*]u8) callconv(.c) void {
    walk.copy(desc, valuePtr(dst), valuePtr(src));
}

pub fn roc_desc_incref(desc: *const TypeDesc, ptr: ?[*]u8, count: isize) callconv(.c) void {
    walk.incref(desc, valuePtr(ptr), count, ops());
}

pub fn roc_desc_decref(desc: *const TypeDesc, ptr: ?[*]u8) callconv(.c) void {
    walk.decref(desc, valuePtr(ptr), ops());
}

pub fn roc_desc_eq(desc: *const TypeDesc, lhs: ?[*]u8, rhs: ?[*]u8) callconv(.c) bool {
    return walk.eq(desc, valuePtr(lhs), valuePtr(rhs), ops());
}

pub fn roc_desc_hash(desc: *const TypeDesc, hasher: u64, ptr: ?[*]u8) callconv(.c) u64 {
    return walk.hash(desc, hasher, valuePtr(ptr), ops());
}

pub fn roc_desc_inspect(out: *RocStr, desc: *const TypeDesc, ptr: ?[*]u8) callconv(.c) void {
    out.* = walk.inspect(desc, valuePtr(ptr), ops());
}

/// The `Payload.on_drop` of an erased callable whose capture record is
/// dynamic. Compiled code passes no ops in the second slot, so it is ignored.
pub fn roc_desc_capture_drop(capture: ?[*]u8, _: ?*RocOps) callconv(.c) void {
    walk.captureDrop(capture, ops());
}

pub fn roc_desc_elem_incref(context: ?*anyopaque, element: ?[*]u8) callconv(.c) void {
    walk.elemIncref(context, element);
}

pub fn roc_desc_elem_decref(context: ?*anyopaque, element: ?[*]u8) callconv(.c) void {
    walk.elemDecref(context, element);
}
