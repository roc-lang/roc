//! Pointer relocation utilities for ModuleEnv
//!
//! This module provides functionality to relocate all pointers within a ModuleEnv
//! by a fixed offset. This is useful when transferring a ModuleEnv across address
//! spaces via shared memory or similar mechanisms.

const std = @import("std");
const ModuleEnv = @import("ModuleEnv.zig");
const collections = @import("../collections.zig");
const types_mod = @import("../types.zig");
const Ident = @import("Ident.zig");
const StringLiteral = @import("StringLiteral.zig");

/// Relocate all pointers in a ModuleEnv by the given offset
/// This function traverses all data structures within the ModuleEnv and adjusts
/// every pointer by adding the provided offset.
pub fn relocateModuleEnv(env: *ModuleEnv, offset: isize) void {
    // Relocate allocator pointer if it's a vtable pointer
    // Note: We don't relocate the allocator itself as it's typically a vtable
    // pointer that should remain valid in the new address space

    // Relocate idents
    relocateIdentStore(&env.idents, offset);

    // Relocate ident_ids_for_slicing
    env.ident_ids_for_slicing.relocate(offset);

    // Relocate strings
    relocateStringLiteralStore(&env.strings, offset);

    // Relocate types
    relocateTypeStore(&env.types, offset);

    // Relocate exposed_by_str
    env.exposed_by_str.relocate(offset);

    // Relocate exposed_nodes
    env.exposed_nodes.relocate(offset);

    // Relocate line_starts
    env.line_starts.relocate(offset);

    // Relocate source pointer
    if (env.source.len > 0) {
        const old_ptr = @intFromPtr(env.source.ptr);
        const new_ptr = @as(usize, @intCast(@as(isize, @intCast(old_ptr)) + offset));
        env.source.ptr = @ptrFromInt(new_ptr);
    }
}

/// Relocate pointers in an Ident.Store
fn relocateIdentStore(store: *Ident.Store, offset: isize) void {
    // The SmallStringInterner contains:
    // - buffer: std.ArrayListUnmanaged(u8)
    // - string_offsets: std.ArrayListUnmanaged(u32)
    relocateArrayListUnmanaged(u8, &store.store.buffer, offset);
    relocateArrayListUnmanaged(u32, &store.store.string_offsets, offset);
}

/// Relocate pointers in a StringLiteral.Store
fn relocateStringLiteralStore(store: *StringLiteral.Store, offset: isize) void {
    relocateArrayListUnmanaged(u8, &store.buffer, offset);
}

/// Relocate pointers in a types.Store
fn relocateTypeStore(store: *types_mod.Store, offset: isize) void {
    // Relocate slots
    relocateSlotStore(&store.slots, offset);

    // Relocate descs
    relocateDescStore(&store.descs, offset);

    // Relocate vars
    store.vars.relocate(offset);

    // Relocate record_fields
    store.record_fields.relocate(offset);

    // Relocate tags
    store.tags.relocate(offset);
}

/// Relocate pointers in a SlotStore
fn relocateSlotStore(store: anytype, offset: isize) void {
    store.backing.relocate(offset);
}

/// Relocate pointers in a DescStore
fn relocateDescStore(store: anytype, offset: isize) void {
    store.backing.relocate(offset);
}

/// Relocate pointers in an ArrayListUnmanaged
fn relocateArrayListUnmanaged(comptime T: type, list: *std.ArrayListUnmanaged(T), offset: isize) void {
    if (list.items.len > 0) {
        const old_ptr = @intFromPtr(list.items.ptr);
        const new_ptr = @as(usize, @intCast(@as(isize, @intCast(old_ptr)) + offset));
        list.items.ptr = @ptrFromInt(new_ptr);
    }
}
