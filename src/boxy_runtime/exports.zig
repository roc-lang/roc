//! The C-ABI entry points compiled code calls (design.md "Object Symbol
//! Names": the `roc_desc_*` symbols).
//!
//! Compiled code carries no `RocOps`, so these reach the host through
//! `in_process_host.ops()`: in a platform build the process-wide table over
//! the host's runtime symbols, in-process the ops the current thread entered.
//! Composition goes through the process-global cache and allocates the
//! immortal descriptors and dictionaries through `compiled.immortalOps()`.
//! Array operands are a count and a pointer to `extern` items; value operands
//! are the addresses of the values' bytes. `static_lib.zig` exports these
//! under their own names, and the dev backend calls them by address when it
//! runs code in the compiler's own process.

const builtins = @import("builtins");
const desc_mod = @import("desc.zig");
const compose = @import("compose.zig");
const walk = @import("walk.zig");
const compiled = @import("compiled.zig");

const RocOps = builtins.host_abi.RocOps;
const RocStr = builtins.str.RocStr;
const RocList = builtins.list.RocList;
const TypeDesc = desc_mod.TypeDesc;
const Name = desc_mod.Name;
const Methods = desc_mod.Methods;

fn ops() *RocOps {
    return builtins.in_process_host.ops();
}

fn cache() *compose.Cache {
    return &compose.process_cache;
}

fn composeOps() *RocOps {
    return compiled.immortalOps();
}

fn items(comptime T: type, count: u32, ptr: ?[*]const T) []const T {
    if (count == 0) return &.{};
    return ptr.?[0..count];
}

fn valuePtr(ptr: ?[*]u8) [*]u8 {
    return ptr orelse walk.zeroSizedPtr();
}

pub fn roc_desc_list(elem: *const TypeDesc) callconv(.c) *const TypeDesc {
    return compose.list(cache(), composeOps(), elem);
}

pub fn roc_desc_box(payload: *const TypeDesc) callconv(.c) *const TypeDesc {
    return compose.box(cache(), composeOps(), payload);
}

pub fn roc_desc_recursive_box(payload: *const TypeDesc) callconv(.c) *const TypeDesc {
    return compose.recursiveBox(cache(), composeOps(), payload);
}

pub fn roc_desc_record(count: u32, fields: ?[*]const compose.FieldInput) callconv(.c) *const TypeDesc {
    return compose.record(cache(), composeOps(), items(compose.FieldInput, count, fields));
}

pub fn roc_desc_record_extended(count: u32, fields: ?[*]const compose.FieldInput, ext: *const TypeDesc) callconv(.c) *const TypeDesc {
    return compose.recordExtended(cache(), composeOps(), items(compose.FieldInput, count, fields), ext);
}

pub fn roc_desc_tag_union_extended(count: u32, variants: ?[*]const compose.VariantInput, ext: *const TypeDesc) callconv(.c) *const TypeDesc {
    return compose.tagUnionExtended(cache(), composeOps(), items(compose.VariantInput, count, variants), ext);
}

pub fn roc_desc_declared_record(count: u32, fields: ?[*]const compose.DeclaredFieldInput) callconv(.c) *const TypeDesc {
    return compose.declaredRecord(cache(), composeOps(), items(compose.DeclaredFieldInput, count, fields));
}

pub fn roc_desc_tuple(count: u32, elems: ?[*]const *const TypeDesc) callconv(.c) *const TypeDesc {
    return compose.tuple(cache(), composeOps(), items(*const TypeDesc, count, elems));
}

pub fn roc_desc_tag_union(count: u32, variants: ?[*]const compose.VariantInput) callconv(.c) *const TypeDesc {
    return compose.tagUnion(cache(), composeOps(), items(compose.VariantInput, count, variants));
}

pub fn roc_desc_args(count: u32, descs: ?[*]const *const TypeDesc) callconv(.c) *const TypeDesc {
    return compose.args(cache(), composeOps(), items(*const TypeDesc, count, descs));
}

pub fn roc_desc_nominal(backing: *const TypeDesc, name: ?*const Name, methods: ?*const Methods, inspect_opaque: bool) callconv(.c) *const TypeDesc {
    return compose.nominal(cache(), composeOps(), backing, name, methods, inspect_opaque);
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

pub fn roc_desc_field_addr(desc: *const TypeDesc, base: ?[*]u8, semantic_index: u32) callconv(.c) [*]u8 {
    return compiled.fieldAddr(desc, valuePtr(base), semantic_index, ops());
}

pub fn roc_desc_field_addr_named(desc: *const TypeDesc, base: ?[*]u8, name: *const Name) callconv(.c) [*]u8 {
    return compiled.fieldAddrNamed(desc, valuePtr(base), name, ops());
}

pub fn roc_desc_list_elem_addr(elem: *const TypeDesc, list: *const RocList, index: u64) callconv(.c) [*]u8 {
    return compiled.listElemAddr(elem, list, index);
}

pub fn roc_desc_box_payload_addr(desc: *const TypeDesc, box: *const usize) callconv(.c) [*]u8 {
    return compiled.boxPayloadAddr(desc, box);
}

pub fn roc_desc_tag_discriminant(desc: *const TypeDesc, base: ?[*]u8) callconv(.c) u32 {
    return compiled.tagDiscriminant(desc, valuePtr(base), ops());
}

pub fn roc_desc_tag_discriminant_of(desc: *const TypeDesc, name: *const Name) callconv(.c) u32 {
    return compiled.tagDiscriminantOf(desc, name, ops());
}

pub fn roc_desc_tag_payload_addr(desc: *const TypeDesc, base: ?[*]u8, name: *const Name, payload_index: u32) callconv(.c) [*]u8 {
    return compiled.tagPayloadAddr(desc, valuePtr(base), name, payload_index, ops());
}

pub fn roc_desc_build_record(desc: *const TypeDesc, dst: ?[*]u8, count: u32, fields: ?[*]const [*]const u8) callconv(.c) void {
    compiled.buildRecord(desc, valuePtr(dst), items([*]const u8, count, fields), ops());
}

pub fn roc_desc_build_tag(desc: *const TypeDesc, name: *const Name, dst: ?[*]u8, count: u32, payloads: ?[*]const [*]const u8) callconv(.c) void {
    compiled.buildTag(desc, name, valuePtr(dst), items([*]const u8, count, payloads), ops());
}

pub fn roc_desc_build_list(elem: *const TypeDesc, dst: *RocList, count: u32, elems: ?[*]const [*]const u8) callconv(.c) void {
    compiled.buildList(elem, dst, items([*]const u8, count, elems), ops());
}

pub fn roc_desc_build_box(desc: *const TypeDesc, dst: *usize, payload: ?[*]u8) callconv(.c) void {
    compiled.buildBox(desc, dst, valuePtr(payload), ops());
}

/// A dictionary callable of the erased-record entry `callable_fn` capturing
/// `captures` after `capture_prefix` reserved bytes.
pub fn roc_desc_dict_compose(callable_fn: builtins.erased_callable.CallableFnPtr, capture_prefix: u32, count: u32, captures: ?[*]const usize) callconv(.c) [*]u8 {
    return compiled.dictCompose(callable_fn, capture_prefix, items(usize, count, captures), composeOps());
}

pub fn roc_desc_dict_record(count: u32, methods: ?[*]const usize) callconv(.c) [*]const usize {
    return compiled.dictRecord(items(usize, count, methods), composeOps());
}

pub fn roc_desc_list_op(request: *const compiled.ListOpRequest) callconv(.c) void {
    compiled.listOp(request, ops());
}
