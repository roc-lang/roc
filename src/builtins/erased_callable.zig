//! Runtime ABI for boxed erased callables.
//!
//! A boxed erased callable is one ordinary Roc refcounted allocation. The Roc
//! value is the allocation's data pointer, like `Box(T)`. The allocation payload
//! starts with `Payload`, and the erased callable's hidden capture bytes live
//! inline at a fixed aligned offset after that header.

const std = @import("std");

const utils = @import("utils.zig");

pub const RocOps = utils.RocOps;

/// Uniform ABI of a boxed erased callable function pointer.
///
/// `args` points at the generated fixed-arity argument struct for this erased
/// call signature, or is null for arity 0. `ret` points at caller-owned result
/// storage, or is null for zero-sized results. `capture` is always the pointer
/// returned by `capturePtr(payload_data_ptr)`.
pub const ErasedCallableFn = *const fn (
    ops: *RocOps,
    ret: ?[*]u8,
    args: ?[*]const u8,
    capture: ?[*]u8,
) callconv(.c) void;

/// Stored function-pointer field type in `Payload`.
pub const CallableFnPtr = ErasedCallableFn;

/// Final-drop callback for the inline hidden capture.
///
/// The callback receives a pointer to the first capture byte. It must recursively
/// decref/free any refcounted data contained inside the capture. It must not free
/// the erased callable allocation; the erased callable runtime does that after
/// this callback returns.
///
/// The ops argument is whatever the final release passed along. Compiled Roc
/// code carries no RocOps under the symbol ABI and passes null here, so a
/// host-installed callback must reach the host's RocOps through the host's
/// own storage, never through this parameter.
pub const OnDropFn = *const fn (?[*]u8, *RocOps) callconv(.c) void;

/// Fixed header at the beginning of a boxed erased callable payload.
pub const Payload = extern struct {
    callable_fn_ptr: CallableFnPtr,
    on_drop: ?OnDropFn,
};

/// Shim-only metadata stored at the beginning of a Roc-created erased
/// callable's capture bytes while the dev machine-code shim is running in hot
/// reload mode. The public erased-callable payload ABI remains unchanged:
/// hosts still see `Payload` followed by bytes at `capturePtr`.
pub const HotReloadCaptureHeader = extern struct {
    code_ref: ?*anyopaque,
    original_on_drop: ?OnDropFn,
};

/// Captures are aligned to this boundary so any legal Roc capture layout can be
/// copied inline without an extra descriptor or runtime offset field.
pub const capture_alignment: u32 = 16;

/// Alignment used for the single Roc allocation that stores `Payload` plus the
/// inline capture bytes.
pub const payload_alignment: u32 = capture_alignment;

/// Fixed byte offset from the start of `Payload` to the first capture byte.
pub const capture_offset: u32 = @intCast(std.mem.alignForward(usize, @sizeOf(Payload), capture_alignment));

/// Bytes reserved before the ordinary Roc capture in shim-execution erased
/// callables. This is aligned like captures so the adjusted capture pointer
/// keeps the same alignment guarantees as `capturePtr`.
pub const hot_reload_capture_prefix_size: u32 = @intCast(std.mem.alignForward(usize, @sizeOf(HotReloadCaptureHeader), capture_alignment));

comptime {
    std.debug.assert(capture_offset % capture_alignment == 0);
    std.debug.assert(payload_alignment == 16);
    std.debug.assert(hot_reload_capture_prefix_size % capture_alignment == 0);
}

/// The runtime allocation never relies on Roc's list-style element-count header.
/// Nested data in the capture is handled solely by `Payload.on_drop`.
pub const allocation_has_refcounted_children = false;

/// Return the payload byte count required for a capture with the given size.
pub fn payloadSize(capture_size: usize) usize {
    return capture_offset + capture_size;
}

/// Allocate the payload bytes for a boxed erased callable and initialize its
/// header. Capture bytes, when present, must be copied by the caller into
/// `capturePtr(data_ptr)`.
pub fn allocate(
    callable_fn_ptr: CallableFnPtr,
    on_drop: ?OnDropFn,
    capture_size: usize,
    roc_ops: *RocOps,
) [*]u8 {
    const data_ptr = utils.allocateWithRefcount(
        payloadSize(capture_size),
        payload_alignment,
        allocation_has_refcounted_children,
        roc_ops,
    );
    const payload = payloadPtr(data_ptr);
    payload.* = .{
        .callable_fn_ptr = callable_fn_ptr,
        .on_drop = on_drop,
    };
    return data_ptr;
}

/// Interpret a boxed-erased-callable data pointer as its payload header.
pub fn payloadPtr(data_ptr: [*]u8) *Payload {
    return @ptrCast(@alignCast(data_ptr));
}

/// Return the payload header for a nullable data pointer, or null.
pub fn maybePayloadPtr(data_ptr: ?[*]u8) ?*Payload {
    const ptr = data_ptr orelse return null;
    return payloadPtr(ptr);
}

/// Return the fixed inline capture pointer for a boxed-erased-callable payload.
pub fn capturePtr(data_ptr: [*]u8) [*]u8 {
    return data_ptr + capture_offset;
}

/// Return the fixed inline capture pointer for a nullable payload, or null.
pub fn maybeCapturePtr(data_ptr: ?[*]u8) ?[*]u8 {
    const ptr = data_ptr orelse return null;
    return capturePtr(ptr);
}

/// Interpret a shim-execution erased-callable capture pointer as the hot-reload
/// prefix header. Only call this for payloads whose on_drop was set to the
/// shim hot-reload drop helper.
pub fn hotReloadCaptureHeader(capture_ptr: ?[*]u8) ?*HotReloadCaptureHeader {
    const ptr = capture_ptr orelse return null;
    return @ptrCast(@alignCast(ptr));
}

/// Return the ordinary Roc capture pointer after the shim-only hot-reload
/// prefix.
pub fn hotReloadAdjustedCapturePtr(capture_ptr: ?[*]u8) ?[*]u8 {
    const ptr = capture_ptr orelse return null;
    return ptr + hot_reload_capture_prefix_size;
}

/// Increment the outer boxed-erased-callable allocation refcount.
pub fn incref(data_ptr: ?[*]u8, amount: isize, roc_ops: *RocOps) callconv(.c) void {
    utils.increfDataPtrC(data_ptr, amount, roc_ops);
}

/// Decrement the outer refcount, running `on_drop` if this was the final ref.
pub fn decref(data_ptr: ?[*]u8, roc_ops: *RocOps) callconv(.c) void {
    if (data_ptr) |ptr| {
        if (utils.isUnique(ptr, roc_ops)) {
            const payload = payloadPtr(ptr);
            if (payload.on_drop) |on_drop| {
                on_drop(capturePtr(ptr), roc_ops);
            }
        }
    }
    utils.decrefDataPtrC(data_ptr, payload_alignment, allocation_has_refcounted_children, roc_ops);
}

/// Run final-drop logic and free the boxed-erased-callable allocation.
pub fn free(data_ptr: ?[*]u8, roc_ops: *RocOps) callconv(.c) void {
    if (data_ptr) |ptr| {
        const payload = payloadPtr(ptr);
        if (payload.on_drop) |on_drop| {
            on_drop(capturePtr(ptr), roc_ops);
        }
    }
    utils.freeDataPtrC(data_ptr, payload_alignment, allocation_has_refcounted_children, roc_ops);
}
