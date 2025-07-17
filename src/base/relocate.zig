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
    relocateSafeList(Ident.Idx, &env.ident_ids_for_slicing, offset);

    // Relocate strings
    relocateStringLiteralStore(&env.strings, offset);

    // Relocate types
    relocateTypeStore(&env.types, offset);

    // Relocate exposed_by_str
    relocateSafeStringHashMap(void, &env.exposed_by_str, offset);

    // Relocate exposed_nodes
    relocateSafeStringHashMap(u16, &env.exposed_nodes, offset);

    // Relocate line_starts
    relocateSafeList(u32, &env.line_starts, offset);

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
    relocateSafeList(types_mod.Var, &store.vars, offset);

    // Relocate record_fields
    relocateSafeMultiList(types_mod.RecordField, &store.record_fields, offset);

    // Relocate tags
    relocateSafeMultiList(types_mod.Tag, &store.tags, offset);
}

/// Relocate pointers in a SlotStore
fn relocateSlotStore(store: anytype, offset: isize) void {
    relocateSafeList(types_mod.store.Slot, &store.backing, offset);
}

/// Relocate pointers in a DescStore
fn relocateDescStore(store: anytype, offset: isize) void {
    relocateMultiArrayList(types_mod.store.Desc, &store.backing, offset);
}

/// Relocate pointers in a SafeList
fn relocateSafeList(comptime T: type, list: *collections.SafeList(T), offset: isize) void {
    relocateArrayListUnmanaged(T, &list.items, offset);
}

/// Relocate pointers in a SafeMultiList
fn relocateSafeMultiList(comptime T: type, list: *collections.SafeMultiList(T), offset: isize) void {
    relocateMultiArrayList(T, &list.items, offset);
}

/// Relocate pointers in a SafeStringHashMap
fn relocateSafeStringHashMap(comptime V: type, map: *collections.SafeStringHashMap(V), offset: isize) void {
    // The underlying map is std.StringHashMapUnmanaged which contains:
    // - metadata: ?[*]Metadata
    // - keys: [*][]const u8
    // - values: [*]V
    // - capacity: u32
    // - available: u32

    // Relocate metadata pointer
    if (map.map.metadata) |metadata| {
        const old_ptr = @intFromPtr(metadata);
        const new_ptr = @as(usize, @intCast(@as(isize, @intCast(old_ptr)) + offset));
        map.map.metadata = @ptrFromInt(new_ptr);
    }

    // Relocate keys pointer and each individual key
    if (map.map.keys()) |keys| {
        const old_keys_ptr = @intFromPtr(keys);
        const new_keys_ptr = @as(usize, @intCast(@as(isize, @intCast(old_keys_ptr)) + offset));
        const new_keys = @as([*][]const u8, @ptrFromInt(new_keys_ptr));

        // Update each key string pointer
        const capacity = map.map.capacity();
        var i: usize = 0;
        while (i < capacity) : (i += 1) {
            if (new_keys[i].len > 0) {
                const old_str_ptr = @intFromPtr(new_keys[i].ptr);
                const new_str_ptr = @as(usize, @intCast(@as(isize, @intCast(old_str_ptr)) + offset));
                new_keys[i].ptr = @ptrFromInt(new_str_ptr);
            }
        }
    }

    // Relocate values pointer if V is not void
    if (V != void) {
        if (map.map.values()) |values| {
            const old_values_ptr = @intFromPtr(values);
            _ = @as(usize, @intCast(@as(isize, @intCast(old_values_ptr)) + offset));
            // Note: We don't need to update the pointer in the map structure directly
            // as it's computed from metadata pointer
        }
    }
}

/// Relocate pointers in an ArrayListUnmanaged
fn relocateArrayListUnmanaged(comptime T: type, list: *std.ArrayListUnmanaged(T), offset: isize) void {
    if (list.items.len > 0) {
        const old_ptr = @intFromPtr(list.items.ptr);
        const new_ptr = @as(usize, @intCast(@as(isize, @intCast(old_ptr)) + offset));
        list.items.ptr = @ptrFromInt(new_ptr);
    }
}

/// Relocate pointers in a MultiArrayList
fn relocateMultiArrayList(comptime T: type, list: *std.MultiArrayList(T), offset: isize) void {
    // MultiArrayList has internal fields we need to update
    // The structure contains:
    // - bytes: []u8 (the actual data storage)
    // - len: usize
    // - capacity: usize

    // Access the internal bytes field directly
    const bytes_ptr = &@field(list, "bytes");

    if (bytes_ptr.len > 0) {
        const old_ptr = @intFromPtr(bytes_ptr.ptr);
        const new_ptr = @as(usize, @intCast(@as(isize, @intCast(old_ptr)) + offset));
        bytes_ptr.ptr = @ptrFromInt(new_ptr);
    }
}
