//! The C-ABI entry points compiled code calls (design.md "Object Symbol
//! Names": the `roc_desc_*` symbols).
//!
//! Compiled code carries no `RocOps`, so these reach the host through
//! `in_process_host.ops()`: in a platform build the process-wide table over
//! the host's runtime symbols, in-process the ops the current thread entered.
//! Array operands are a count and a pointer to `extern` items; value operands
//! are the addresses of the values' bytes. `static_lib.zig` exports these
//! under their own names, and the dev backend calls them by address when it
//! runs code in the compiler's own process.

const builtins = @import("builtins");
const desc_mod = @import("desc.zig");
const walk = @import("walk.zig");
const compiled = @import("compiled.zig");

const RocOps = builtins.host_abi.RocOps;
const RocStr = builtins.str.RocStr;
const RocList = builtins.list.RocList;
const TypeDesc = desc_mod.TypeDesc;
const AdaptPlan = desc_mod.AdaptPlan;

fn ops() *RocOps {
    return builtins.in_process_host.ops();
}

fn items(comptime T: type, count: u32, ptr: ?[*]const T) []const T {
    if (count == 0) return &.{};
    return ptr.?[0..count];
}

fn valuePtr(ptr: ?[*]u8) [*]u8 {
    return ptr orelse walk.zeroSizedPtr();
}

/// The end of a run of `count` dynamic locals of type `desc` placed after
/// `offset` bytes of a procedure's frame region: the run starts at `offset`
/// rounded up to the type's alignment, and each local takes the type's size
/// rounded up to its alignment.
pub fn roc_desc_frame_run_size(desc: *const TypeDesc, count: u32, offset: usize) callconv(.c) usize {
    return frameRunStart(offset, desc) + frameStride(desc) * count;
}

/// Place a run of `count` dynamic locals of type `desc` in `region`, where
/// `roc_desc_frame_run_size` measured it, writing each local's address to
/// `slots[i]`, and return the run's end. A procedure places its frame as one
/// run per descriptor, so it makes one call and names one address slot array
/// per run rather than storing one address and one descriptor per local;
/// those stores would each be a stack access of the procedure's entry block,
/// which LLVM's WebAssembly register stackifier handles quadratically.
pub fn roc_desc_frame_place_run(desc: *const TypeDesc, count: u32, region: [*]u8, offset: usize, slots: [*][*]u8) callconv(.c) usize {
    const start = frameRunStart(offset, desc);
    const stride = frameStride(desc);
    for (slots[0..count], 0..) |*slot, i| slot.* = region + start + stride * i;
    return start + stride * count;
}

/// `offset` rounded up to `desc`'s alignment, a power of two.
fn frameRunStart(offset: usize, desc: *const TypeDesc) usize {
    const alignment: usize = desc.alignment;
    return (offset +% alignment -% 1) & (0 -% alignment);
}

/// The bytes between consecutive locals of type `desc` in a run.
fn frameStride(desc: *const TypeDesc) usize {
    return frameRunStart(desc.size, desc);
}

/// The C-ABI form of `compiled.adapt`.
pub fn roc_desc_adapt(plan: *const AdaptPlan, dst: ?[*]u8, src: ?[*]u8) callconv(.c) void {
    compiled.adapt(plan, valuePtr(dst), valuePtr(src), ops());
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

/// The C-ABI form of `compiled.tagPayloadAddr`.
pub fn roc_desc_tag_payload_addr(desc: *const TypeDesc, base: ?[*]u8, payload_index: u32) callconv(.c) [*]u8 {
    return compiled.tagPayloadAddr(desc, valuePtr(base), payload_index, ops());
}

/// The C-ABI form of `compiled.buildRecord`.
pub fn roc_desc_build_record(desc: *const TypeDesc, dst: ?[*]u8, count: u32, fields: ?[*]const [*]const u8) callconv(.c) void {
    compiled.buildRecord(desc, valuePtr(dst), items([*]const u8, count, fields), ops());
}

/// The C-ABI form of `compiled.updateRecord`.
pub fn roc_desc_update_record(base_desc: *const TypeDesc, base: ?[*]u8, desc: *const TypeDesc, dst: ?[*]u8, plan: [*]const u32, count: u32, values: ?[*]const [*]const u8) callconv(.c) void {
    compiled.updateRecord(base_desc, valuePtr(base), desc, valuePtr(dst), plan, items([*]const u8, count, values), ops());
}

/// The C-ABI form of `compiled.buildTag`.
pub fn roc_desc_build_tag(desc: *const TypeDesc, discriminant: u32, dst: ?[*]u8, count: u32, payloads: ?[*]const [*]const u8) callconv(.c) void {
    compiled.buildTag(desc, discriminant, valuePtr(dst), items([*]const u8, count, payloads), ops());
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
/// descriptor and whose second word this writes as the allocation's capture
/// capacity (design.md "Function Values"); the callable takes over the
/// record's references. With a hot-reload drop, the capture is preceded by
/// the hot-reload header holding `code_ref`, and that drop runs the record's
/// release after it.
pub fn roc_desc_closure(
    callable_fn: builtins.erased_callable.CallableFnPtr,
    record: [*]const u8,
    hot_reload_code_ref: ?*anyopaque,
    hot_reload_drop: ?builtins.erased_callable.OnDropFn,
) callconv(.c) [*]u8 {
    const erased = builtins.erased_callable;
    const record_desc = captureRecordDesc(record);
    const prefix = hotReloadPrefix(hot_reload_drop);
    const data = erased.allocate(callable_fn, hot_reload_drop orelse &roc_desc_capture_drop, prefix + record_desc.size, ops());
    writeClosure(data, callable_fn, record, record_desc, record_desc.size, hot_reload_code_ref, hot_reload_drop);
    return data;
}

/// `roc_desc_closure` repacking the consumed erased callable `reuse` in
/// place when it is unique (or `unique` says the compiler proved it) and
/// the record fits its capture capacity: its old capture is released by its
/// drop callback first, then the header and capture are overwritten. A
/// shared or too small allocation is released and the callable is fresh. A
/// null `reuse` transfers nothing.
pub fn roc_desc_closure_reuse(
    callable_fn: builtins.erased_callable.CallableFnPtr,
    record: [*]const u8,
    reuse: ?[*]u8,
    unique: u8,
    hot_reload_code_ref: ?*anyopaque,
    hot_reload_drop: ?builtins.erased_callable.OnDropFn,
) callconv(.c) [*]u8 {
    const erased = builtins.erased_callable;
    const old = reuse orelse return roc_desc_closure(callable_fn, record, hot_reload_code_ref, hot_reload_drop);
    const record_desc = captureRecordDesc(record);
    const prefix = hotReloadPrefix(hot_reload_drop);
    const old_capture = erased.capturePtr(old) + prefix;
    const capacity = readWord(old_capture + @sizeOf(usize));
    if (record_desc.size <= capacity and (unique != 0 or builtins.utils.isUnique(old, ops()))) {
        const payload = erased.payloadPtr(old);
        if (payload.on_drop) |old_drop| old_drop(erased.capturePtr(old), ops());
        writeClosure(old, callable_fn, record, record_desc, capacity, hot_reload_code_ref, hot_reload_drop);
        return old;
    }
    const fresh = roc_desc_closure(callable_fn, record, hot_reload_code_ref, hot_reload_drop);
    erased.decref(old, ops());
    return fresh;
}

fn captureRecordDesc(record: [*]const u8) *const TypeDesc {
    return @ptrFromInt(readWord(record));
}

fn readWord(ptr: [*]const u8) usize {
    return @as(*align(1) const usize, @ptrCast(ptr)).*;
}

fn hotReloadPrefix(hot_reload_drop: ?builtins.erased_callable.OnDropFn) usize {
    return if (hot_reload_drop != null) builtins.erased_callable.hot_reload_capture_prefix_size else 0;
}

/// Write the header and capture of the closure `data` (an allocation of at
/// least `capacity` capture bytes after any hot-reload header): the record,
/// with the capacity in its second word.
fn writeClosure(
    data: [*]u8,
    callable_fn: builtins.erased_callable.CallableFnPtr,
    record: [*]const u8,
    record_desc: *const TypeDesc,
    capacity: usize,
    hot_reload_code_ref: ?*anyopaque,
    hot_reload_drop: ?builtins.erased_callable.OnDropFn,
) void {
    const erased = builtins.erased_callable;
    erased.payloadPtr(data).* = .{
        .callable_fn_ptr = callable_fn,
        .on_drop = hot_reload_drop orelse &roc_desc_capture_drop,
    };
    const capture = erased.capturePtr(data);
    if (hot_reload_drop != null) {
        @as(*erased.HotReloadCaptureHeader, @ptrCast(@alignCast(capture))).* = .{
            .code_ref = hot_reload_code_ref,
            .original_on_drop = &roc_desc_capture_drop,
        };
    }
    const prefix = hotReloadPrefix(hot_reload_drop);
    @memcpy((capture + prefix)[0..record_desc.size], record[0..record_desc.size]);
    @as(*align(1) usize, @ptrCast(capture + prefix + @sizeOf(usize))).* = capacity;
}
