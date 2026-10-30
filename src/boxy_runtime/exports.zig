//! The C-ABI entry points compiled code calls (design.md "Object Symbol
//! Names": the `roc_desc_*` symbols).
//!
//! Compiled code carries no `RocOps`, so these reach the host through
//! `in_process_host.ops()`: in a platform build the process-wide table over
//! the host's runtime symbols, in-process the ops the current thread entered.
//! Composition interns in `compiled.descriptorCache()` and allocates the
//! immortal descriptors and dictionaries through `compiled.immortalOps()`:
//! the process's in a platform build, the current program's
//! (`compiled.InProcessState`) in the compiler's process.
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
    return compiled.descriptorCache();
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

/// The C-ABI form of `compose.list`.
pub fn roc_desc_list(elem: *const TypeDesc) callconv(.c) *const TypeDesc {
    return compose.list(cache(), composeOps(), elem);
}

/// The C-ABI form of `compose.presenceSlot`.
pub fn roc_desc_presence_slot(missing: *const Name, present: *const Name, payload: *const TypeDesc) callconv(.c) *const TypeDesc {
    return compose.presenceSlot(cache(), composeOps(), missing, present, payload);
}

/// The C-ABI form of `compose.box`.
pub fn roc_desc_box(payload: *const TypeDesc) callconv(.c) *const TypeDesc {
    return compose.box(cache(), composeOps(), payload);
}

/// The C-ABI form of `compose.recursiveBox`.
pub fn roc_desc_recursive_box(payload: *const TypeDesc) callconv(.c) *const TypeDesc {
    return compose.recursiveBox(cache(), composeOps(), payload);
}

/// The C-ABI form of `compose.recursiveOpen`.
pub fn roc_desc_recursive_open(key: *const u8, count: u32, vars: ?[*]const *const TypeDesc) callconv(.c) *const TypeDesc {
    return compose.recursiveOpen(cache(), composeOps(), key, items(*const TypeDesc, count, vars));
}

/// The C-ABI form of `compose.recursiveClose`.
pub fn roc_desc_recursive_close(shell: *const TypeDesc, desc: *const TypeDesc, key: *const u8, count: u32, vars: ?[*]const *const TypeDesc) callconv(.c) *const TypeDesc {
    return compose.recursiveClose(cache(), composeOps(), shell, desc, key, items(*const TypeDesc, count, vars));
}

/// The C-ABI form of `compose.record`.
pub fn roc_desc_record(count: u32, fields: ?[*]const compose.FieldInput) callconv(.c) *const TypeDesc {
    return compose.record(cache(), composeOps(), items(compose.FieldInput, count, fields));
}

/// The C-ABI form of `compose.recordExtended`.
pub fn roc_desc_record_extended(count: u32, fields: ?[*]const compose.FieldInput, ext: *const TypeDesc) callconv(.c) *const TypeDesc {
    return compose.recordExtended(cache(), composeOps(), items(compose.FieldInput, count, fields), ext);
}

/// The C-ABI form of `compose.tagUnionExtended`.
pub fn roc_desc_tag_union_extended(count: u32, variants: ?[*]const compose.VariantInput, ext: *const TypeDesc) callconv(.c) *const TypeDesc {
    return compose.tagUnionExtended(cache(), composeOps(), items(compose.VariantInput, count, variants), ext);
}

/// The C-ABI form of `compose.declaredRecord`.
pub fn roc_desc_declared_record(count: u32, fields: ?[*]const compose.DeclaredFieldInput) callconv(.c) *const TypeDesc {
    return compose.declaredRecord(cache(), composeOps(), items(compose.DeclaredFieldInput, count, fields));
}

/// The C-ABI form of `compose.tuple`.
pub fn roc_desc_tuple(count: u32, elems: ?[*]const *const TypeDesc) callconv(.c) *const TypeDesc {
    return compose.tuple(cache(), composeOps(), items(*const TypeDesc, count, elems));
}

/// The C-ABI form of `compose.tagUnion`.
pub fn roc_desc_tag_union(count: u32, variants: ?[*]const compose.VariantInput) callconv(.c) *const TypeDesc {
    return compose.tagUnion(cache(), composeOps(), items(compose.VariantInput, count, variants));
}

/// The C-ABI form of `compose.args`.
pub fn roc_desc_args(count: u32, descs: ?[*]const *const TypeDesc) callconv(.c) *const TypeDesc {
    return compose.args(cache(), composeOps(), items(*const TypeDesc, count, descs));
}

/// The C-ABI form of `compose.nominal`.
pub fn roc_desc_nominal(backing: *const TypeDesc, name: ?*const Name, methods: ?*const Methods, inspect_opaque: bool) callconv(.c) *const TypeDesc {
    return compose.nominal(cache(), composeOps(), backing, name, methods, inspect_opaque);
}

/// The C-ABI form of `compiled.adapt`.
pub fn roc_desc_adapt(from: *const TypeDesc, to: *const TypeDesc, dst: ?[*]u8, src: ?[*]u8) callconv(.c) void {
    compiled.adapt(from, to, valuePtr(dst), valuePtr(src), ops());
}

/// The C-ABI form of `walk.copy`.
pub fn roc_desc_copy(desc: *const TypeDesc, dst: ?[*]u8, src: ?[*]u8) callconv(.c) void {
    walk.copy(desc, valuePtr(dst), valuePtr(src));
}

/// The C-ABI form of `walk.incref`.
pub fn roc_desc_incref(desc: *const TypeDesc, ptr: ?[*]u8, count: isize) callconv(.c) void {
    walk.incref(desc, valuePtr(ptr), count, ops());
}

/// The C-ABI form of `walk.decref`.
pub fn roc_desc_decref(desc: *const TypeDesc, ptr: ?[*]u8) callconv(.c) void {
    walk.decref(desc, valuePtr(ptr), ops());
}

/// The C-ABI form of `walk.eq`.
pub fn roc_desc_eq(desc: *const TypeDesc, lhs: ?[*]u8, rhs: ?[*]u8) callconv(.c) bool {
    return walk.eq(desc, valuePtr(lhs), valuePtr(rhs), ops());
}

/// The C-ABI form of `walk.hash`.
pub fn roc_desc_hash(desc: *const TypeDesc, hasher: u64, ptr: ?[*]u8) callconv(.c) u64 {
    return walk.hash(desc, hasher, valuePtr(ptr), ops());
}

/// The C-ABI form of `walk.inspect`.
pub fn roc_desc_inspect(out: *RocStr, desc: *const TypeDesc, ptr: ?[*]u8) callconv(.c) void {
    out.* = walk.inspect(desc, valuePtr(ptr), ops());
}

/// The `Payload.on_drop` of an erased callable whose capture record is
/// dynamic. Compiled code passes no ops in the second slot, so it is ignored.
pub fn roc_desc_capture_drop(capture: ?[*]u8, _: ?*RocOps) callconv(.c) void {
    walk.captureDrop(capture, ops());
}

/// The C-ABI form of `walk.elemIncref`.
pub fn roc_desc_elem_incref(context: ?*anyopaque, element: ?[*]u8) callconv(.c) void {
    walk.elemIncref(context, element);
}

/// The C-ABI form of `walk.elemDecref`.
pub fn roc_desc_elem_decref(context: ?*anyopaque, element: ?[*]u8) callconv(.c) void {
    walk.elemDecref(context, element);
}

/// The C-ABI form of `compiled.fieldAddr`.
pub fn roc_desc_field_addr(desc: *const TypeDesc, base: ?[*]u8, field_index: u32) callconv(.c) [*]u8 {
    return compiled.fieldAddr(desc, valuePtr(base), field_index, ops());
}

/// The C-ABI form of `compiled.fieldAddrNamed`.
pub fn roc_desc_field_addr_named(desc: *const TypeDesc, base: ?[*]u8, name: *const Name) callconv(.c) [*]u8 {
    return compiled.fieldAddrNamed(desc, valuePtr(base), name, ops());
}

/// The C-ABI form of `compiled.listElemAddr`.
pub fn roc_desc_list_item_addr(elem: *const TypeDesc, list: *const RocList, index: u64) callconv(.c) [*]u8 {
    return compiled.listElemAddr(elem, list, index);
}

/// The C-ABI form of `compiled.boxPayloadAddr`.
pub fn roc_desc_box_payload_addr(desc: *const TypeDesc, box: *const usize) callconv(.c) [*]u8 {
    return compiled.boxPayloadAddr(desc, box);
}

/// The C-ABI form of `compiled.tagDiscriminant`.
pub fn roc_desc_tag_discriminant(desc: *const TypeDesc, base: ?[*]u8) callconv(.c) u32 {
    return compiled.tagDiscriminant(desc, valuePtr(base), ops());
}

/// The C-ABI form of `compiled.tagDiscriminantOf`.
pub fn roc_desc_tag_discriminant_of(desc: *const TypeDesc, name: *const Name) callconv(.c) u32 {
    return compiled.tagDiscriminantOf(desc, name, ops());
}

/// The C-ABI form of `compiled.tagPayloadAddr`.
pub fn roc_desc_tag_payload_addr(desc: *const TypeDesc, base: ?[*]u8, name: *const Name, payload_index: u32) callconv(.c) [*]u8 {
    return compiled.tagPayloadAddr(desc, valuePtr(base), name, payload_index, ops());
}

/// The C-ABI form of `compiled.buildRecord`.
pub fn roc_desc_build_record(desc: *const TypeDesc, dst: ?[*]u8, count: u32, fields: ?[*]const [*]const u8) callconv(.c) void {
    compiled.buildRecord(desc, valuePtr(dst), items([*]const u8, count, fields), ops());
}

/// The C-ABI form of `compiled.updateRecord`.
pub fn roc_desc_update_record(base_desc: *const TypeDesc, base: ?[*]u8, desc: *const TypeDesc, dst: ?[*]u8, count: u32, names: ?[*]const *const Name, values: ?[*]const [*]const u8) callconv(.c) void {
    compiled.updateRecord(base_desc, valuePtr(base), desc, valuePtr(dst), items(*const Name, count, names), items([*]const u8, count, values), ops());
}

/// The C-ABI form of `compiled.buildTag`.
pub fn roc_desc_build_tag(desc: *const TypeDesc, name: *const Name, dst: ?[*]u8, count: u32, payloads: ?[*]const [*]const u8) callconv(.c) void {
    compiled.buildTag(desc, name, valuePtr(dst), items([*]const u8, count, payloads), ops());
}

/// The C-ABI form of `compiled.buildList`.
pub fn roc_desc_build_list(elem: *const TypeDesc, dst: *RocList, count: u32, elems: ?[*]const [*]const u8) callconv(.c) void {
    compiled.buildList(elem, dst, items([*]const u8, count, elems), ops());
}

/// The C-ABI form of `compiled.buildBox`.
pub fn roc_desc_build_box(desc: *const TypeDesc, dst: *usize, payload: ?[*]u8) callconv(.c) void {
    compiled.buildBox(desc, dst, valuePtr(payload), ops());
}

/// The C-ABI form of `compiled.allocSlot`.
pub fn roc_desc_slot_alloc(desc: *const TypeDesc, dst: *usize) callconv(.c) void {
    compiled.allocSlot(desc, dst, ops());
}

/// The C-ABI form of `compiled.storeSlot`.
pub fn roc_desc_slot_store(desc: *const TypeDesc, slot: *const usize, value: ?[*]u8, dst: *usize) callconv(.c) void {
    compiled.storeSlot(desc, slot, valuePtr(value), dst);
}

/// A dictionary callable of the erased-record entry `callable_fn` capturing
/// `captures` after `capture_prefix` reserved bytes.
pub fn roc_desc_dict_compose(callable_fn: builtins.erased_callable.CallableFnPtr, capture_prefix: u32, count: u32, captures: ?[*]const usize) callconv(.c) [*]u8 {
    return compiled.dictCompose(callable_fn, capture_prefix, items(usize, count, captures), composeOps());
}

/// The C-ABI form of `compiled.dictRecord`.
pub fn roc_desc_dict_record(count: u32, methods: ?[*]const usize) callconv(.c) [*]const usize {
    return compiled.dictRecord(items(usize, count, methods), composeOps());
}

/// The C-ABI form of `compiled.listOp`.
pub fn roc_desc_list_op(request: *const compiled.ListOpRequest) callconv(.c) void {
    compiled.listOp(request, ops());
}

/// Storage for the dynamic arguments a frame-replacing call moves out of the
/// frame it replaces. The callee releases it with `roc_desc_carry_free`.
pub fn roc_desc_carry_alloc(size: usize) callconv(.c) [*]u8 {
    return @ptrCast(ops().allocRaw(@max(size, 1), carry_alignment));
}

/// Release storage `roc_desc_carry_alloc` returned; null is a no-op.
pub fn roc_desc_carry_free(carry: ?[*]u8) callconv(.c) void {
    ops().dealloc(@ptrCast(carry orelse return), carry_alignment);
}

const carry_alignment = 16;

/// A function value of the erased-record entry `callable_fn` whose capture
/// is the dynamic record at `record`, whose first word is the record's own
/// descriptor; the callable takes over the record's references. With a
/// hot-reload drop, the capture is preceded by the hot-reload header holding
/// `code_ref`, and that drop runs the record's release after it.
pub fn roc_desc_closure(
    callable_fn: builtins.erased_callable.CallableFnPtr,
    record: [*]const u8,
    hot_reload_code_ref: ?*anyopaque,
    hot_reload_drop: ?builtins.erased_callable.OnDropFn,
) callconv(.c) [*]u8 {
    const erased = builtins.erased_callable;
    const record_desc: *const TypeDesc = @as(*const *const TypeDesc, @ptrCast(@alignCast(record))).*;
    const prefix: usize = if (hot_reload_drop != null) erased.hot_reload_capture_prefix_size else 0;
    const data = erased.allocate(callable_fn, hot_reload_drop orelse &roc_desc_capture_drop, prefix + record_desc.size, ops());
    const capture = erased.capturePtr(data);
    if (hot_reload_drop != null) {
        @as(*erased.HotReloadCaptureHeader, @ptrCast(@alignCast(capture))).* = .{
            .code_ref = hot_reload_code_ref,
            .original_on_drop = &roc_desc_capture_drop,
        };
    }
    @memcpy((capture + prefix)[0..record_desc.size], record[0..record_desc.size]);
    return data;
}
