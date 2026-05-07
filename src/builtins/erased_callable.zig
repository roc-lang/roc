//! Runtime ABI for boxed erased callables.
//!
//! A boxed erased callable is one ordinary Roc refcounted allocation. The Roc
//! value is the allocation's data pointer, like `Box(T)`. The allocation payload
//! starts with `Payload`, and the erased callable's hidden capture bytes live
//! inline at a fixed aligned offset after that header.

const std = @import("std");

const utils = @import("utils.zig");

pub const RocOps = utils.RocOps;

/// ABI of a boxed erased callable function pointer.
///
/// The concrete generated function decides its exact explicit Roc arguments.
/// The erased callable runtime layout only stores this pointer-sized value.
pub const CallableFnPtr = *const anyopaque;

/// Final-drop callback for the inline hidden capture.
///
/// The callback receives a pointer to the first capture byte. It must recursively
/// decref/free any refcounted data contained inside the capture. It must not free
/// the erased callable allocation; the erased callable runtime does that after
/// this callback returns.
pub const OnDropFn = *const fn (?[*]u8, *RocOps) callconv(.c) void;

/// Fixed header at the beginning of a boxed erased callable payload.
pub const Payload = extern struct {
    callable_fn_ptr: CallableFnPtr,
    on_drop: ?OnDropFn,
};

/// Captures are aligned to this boundary so any legal Roc capture layout can be
/// copied inline without an extra descriptor or runtime offset field.
pub const capture_alignment: u32 = 16;

/// Alignment used for the single Roc allocation that stores `Payload` plus the
/// inline capture bytes.
pub const payload_alignment: u32 = capture_alignment;

/// Fixed byte offset from the start of `Payload` to the first capture byte.
pub const capture_offset: u32 = @intCast(std.mem.alignForward(usize, @sizeOf(Payload), capture_alignment));

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

pub fn payloadPtr(data_ptr: [*]u8) *Payload {
    return @ptrCast(@alignCast(data_ptr));
}

pub fn maybePayloadPtr(data_ptr: ?[*]u8) ?*Payload {
    const ptr = data_ptr orelse return null;
    return payloadPtr(ptr);
}

pub fn capturePtr(data_ptr: [*]u8) [*]u8 {
    return data_ptr + capture_offset;
}

pub fn maybeCapturePtr(data_ptr: ?[*]u8) ?[*]u8 {
    const ptr = data_ptr orelse return null;
    return capturePtr(ptr);
}

pub fn incref(data_ptr: ?[*]u8, amount: isize, roc_ops: *RocOps) callconv(.c) void {
    utils.increfDataPtrC(data_ptr, amount, roc_ops);
}

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

pub fn free(data_ptr: ?[*]u8, roc_ops: *RocOps) callconv(.c) void {
    if (data_ptr) |ptr| {
        const payload = payloadPtr(ptr);
        if (payload.on_drop) |on_drop| {
            on_drop(capturePtr(ptr), roc_ops);
        }
    }
    utils.freeDataPtrC(data_ptr, payload_alignment, allocation_has_refcounted_children, roc_ops);
}
