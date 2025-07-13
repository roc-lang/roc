//! Panic handling utilities for the Roc runtime.
//!
//! This module provides functions to handle runtime panics in Roc programs,
//! including signaling panic conditions to the host environment. It bridges
//! between Roc's internal panic representation and the external host interface.
const std = @import("std");
const RocStr = @import("str.zig").RocStr;

// Signals to the host that the program has panicked
extern fn roc_panic(msg: *const RocStr, tag_id: u32) callconv(.C) noreturn;

/// TODO: Document panic_help.
pub fn panic_help(msg: []const u8, tag_id: u32) noreturn {
    var str = RocStr.init(msg.ptr, msg.len);
    roc_panic(&str, tag_id);
}

// must export this explicitly because right now it is not used from zig code
/// TODO: Document panic.
pub fn panic(msg: *const RocStr, alignment: u32) callconv(.C) noreturn {
    return roc_panic(msg, alignment);
}
