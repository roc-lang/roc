//! Proof of concept for FixupCache serialization strategy
//!
//! This demonstrates serializing data structures with pointers by converting
//! pointers to file offsets during serialization, then relocating them back
//! to valid memory addresses during deserialization.
//!
//! The tests demonstrate that:
//! - Complex data structures (SafeList, SmallStringInterner, SafeStringHashMap) can be serialized
//! - All pointers are correctly converted to file offsets and back
//! - All functionality is preserved after deserialization (lookups, iteration, deduplication)
//! - The approach is deterministic (zeroed padding ensures identical output for identical input)

const std = @import("std");
const builtin = @import("builtin");
const collections = @import("../../collections.zig");
const SmallStringInterner = collections.SmallStringInterner;
const SafeList = collections.SafeList;
const SafeStringHashMap = collections.SafeStringHashMap;

/// Test structure containing the three main collection types
const TestStruct = struct {
    list: SafeList(u32),
    interner: SmallStringInterner,
    map: SafeStringHashMap(u16),
};

/// Relocate all pointers in a TestStruct by the given offset
fn relocateTestStruct(s: *TestStruct, offset: isize) void {
    // Relocate SafeList
    relocateSafeList(u32, &s.list, offset);

    // Relocate SmallStringInterner
    relocateSmallStringInterner(&s.interner, offset);

    // Relocate SafeStringHashMap
    relocateSafeStringHashMap(u16, &s.map, offset);
}

/// Relocate pointers in a SafeList
fn relocateSafeList(comptime T: type, list: *SafeList(T), offset: isize) void {
    if (list.items.items.len > 0) {
        const old_ptr = @intFromPtr(list.items.items.ptr);
        const new_ptr = @as(usize, @intCast(@as(isize, @intCast(old_ptr)) + offset));
        list.items.items.ptr = @ptrFromInt(new_ptr);
    }
}

/// Relocate pointers in a SmallStringInterner
fn relocateSmallStringInterner(interner: *SmallStringInterner, offset: isize) void {
    // Relocate bytes buffer
    if (interner.bytes.items.len > 0) {
        const old_ptr = @intFromPtr(interner.bytes.items.ptr);
        const new_ptr = @as(usize, @intCast(@as(isize, @intCast(old_ptr)) + offset));
        interner.bytes.items.ptr = @ptrFromInt(new_ptr);
    }

    // Relocate outer_indices
    if (interner.outer_indices.items.len > 0) {
        const old_ptr = @intFromPtr(interner.outer_indices.items.ptr);
        const new_ptr = @as(usize, @intCast(@as(isize, @intCast(old_ptr)) + offset));
        interner.outer_indices.items.ptr = @ptrFromInt(new_ptr);
    }

    // Relocate regions
    if (interner.regions.items.len > 0) {
        const old_ptr = @intFromPtr(interner.regions.items.ptr);
        const new_ptr = @as(usize, @intCast(@as(isize, @intCast(old_ptr)) + offset));
        interner.regions.items.ptr = @ptrFromInt(new_ptr);
    }

    // Relocate the strings hash map
    relocateStringIdxTable(&interner.strings, offset);
}

/// Relocate pointers in the StringIdx.Table
fn relocateStringIdxTable(table: *SmallStringInterner.StringIdx.Table, offset: isize) void {
    // The table is a HashMapUnmanaged, which has metadata pointer
    if (table.unmanaged.metadata) |metadata| {
        const old_ptr = @intFromPtr(metadata);
        const new_ptr = @as(usize, @intCast(@as(isize, @intCast(old_ptr)) + offset));
        table.unmanaged.metadata = @ptrFromInt(new_ptr);
    }
}

/// Relocate pointers in a SafeStringHashMap
fn relocateSafeStringHashMap(comptime V: type, map: *SafeStringHashMap(V), offset: isize) void {
    // Relocate metadata pointer
    if (map.map.metadata) |metadata| {
        const old_ptr = @intFromPtr(metadata);
        const new_ptr = @as(usize, @intCast(@as(isize, @intCast(old_ptr)) + offset));
        map.map.metadata = @ptrFromInt(new_ptr);
    }

    // If we have metadata, we need to relocate the string pointers
    if (map.map.metadata != null and map.map.capacity() > 0) {
        const keys_ptr = map.map.keys();
        const capacity = map.map.capacity();

        // Update each string pointer in the keys array
        var i: usize = 0;
        while (i < capacity) : (i += 1) {
            // Check if this slot is occupied
            const metadata_byte = map.map.metadata.?[i];
            if (metadata_byte != 0xFF) { // 0xFF means empty slot
                if (keys_ptr[i].len > 0) {
                    const old_str_ptr = @intFromPtr(keys_ptr[i].ptr);
                    const new_str_ptr = @as(usize, @intCast(@as(isize, @intCast(old_str_ptr)) + offset));
                    keys_ptr[i].ptr = @ptrFromInt(new_str_ptr);
                }
            }
        }
    }
}

/// Write data to buffer with proper alignment, zeroing padding bytes for deterministic output
fn writeAlignedData(buffer: []u8, write_offset: *usize, data: []const u8, alignment: usize) usize {
    // Align the write offset
    const aligned_offset = std.mem.alignForward(usize, write_offset.*, alignment);

    // Zero out padding bytes for deterministic output
    if (aligned_offset > write_offset.*) {
        @memset(buffer[write_offset.*..aligned_offset], 0);
    }

    write_offset.* = aligned_offset;

    const data_offset = write_offset.*;
    @memcpy(buffer[data_offset .. data_offset + data.len], data);
    write_offset.* += data.len;
    return data_offset;
}

test "FixupCache proof of concept with real data structures" {
    const allocator = std.testing.allocator;

    // Define Region locally since we can't import it in tests
    const Region = struct {
        start: u32,
        end: u32,
    };

    // Create and populate the test struct
    var original = TestStruct{
        .list = SafeList(u32){},
        .interner = try SmallStringInterner.initCapacity(allocator, 10),
        .map = SafeStringHashMap(u16){},
    };
    defer original.list.deinit(allocator);
    defer original.interner.deinit(allocator);
    defer original.map.deinit(allocator);

    // Populate the list
    try original.list.append(allocator, 42);
    try original.list.append(allocator, 123);
    try original.list.append(allocator, 456);
    try original.list.append(allocator, 789);
    try original.list.append(allocator, 0xDEADBEEF);

    // Populate the interner
    const region1 = Region{ .start = 0, .end = 5 };
    const region2 = Region{ .start = 6, .end = 11 };
    const region3 = Region{ .start = 12, .end = 22 };
    const region4 = Region{ .start = 23, .end = 54 };

    const idx1 = try original.interner.insert(allocator, "hello", region1);
    const idx2 = try original.interner.insert(allocator, "world", region2);
    const idx3 = try original.interner.insert(allocator, "roc rocks!", region3);
    const idx4 = try original.interner.insert(allocator, "this is a longer string to test", region4);
    const idx5 = try original.interner.insert(allocator, "hello", region1); // Duplicate

    // Populate the map
    try original.map.put(allocator, "first", 100);
    try original.map.put(allocator, "second", 200);
    try original.map.put(allocator, "third", 300);
    try original.map.put(allocator, "fourth", 400);
    try original.map.put(allocator, "fifth", 500);

    // Calculate the total size needed
    var total_size: usize = 0;

    // TestStruct itself
    total_size += @sizeOf(TestStruct);

    // SafeList data
    total_size += original.list.items.items.len * @sizeOf(u32);

    // SmallStringInterner data
    total_size += original.interner.bytes.items.len;
    total_size += original.interner.outer_indices.items.len * @sizeOf(SmallStringInterner.StringIdx);
    total_size += original.interner.regions.items.len * @sizeOf(Region);

    // StringIdx.Table hash map
    if (original.interner.strings.unmanaged.metadata) |_| {
        const metadata_size = original.interner.strings.capacity();
        const entry_size = metadata_size;
        const indexes_size = metadata_size * @sizeOf(SmallStringInterner.StringIdx);
        const values_size = metadata_size * @sizeOf(void);
        total_size += entry_size + indexes_size + values_size;
    }

    // SafeStringHashMap data
    if (original.map.map.metadata) |_| {
        const map_capacity = original.map.map.capacity();
        const metadata_size = map_capacity;
        const keys_size = map_capacity * @sizeOf([]const u8);
        const values_size = map_capacity * @sizeOf(u16);
        total_size += metadata_size + keys_size + values_size;

        // String data for keys
        var iter = original.map.iterator();
        while (iter.next()) |entry| {
            total_size += entry.key_ptr.len;
        }
    }

    // Add padding for alignment
    total_size += 128; // Extra space for alignment padding

    // Allocate buffer for serialization with proper alignment
    const buffer = try allocator.alignedAlloc(u8, @alignOf(TestStruct), total_size);
    defer allocator.free(buffer);

    var write_offset: usize = 0;

    // Write TestStruct at the beginning (with placeholder pointers)
    const struct_offset = write_offset;
    write_offset += @sizeOf(TestStruct);

    // Write all data sequentially and record offsets
    const list_data_offset = if (original.list.items.items.len > 0) blk: {
        const data = std.mem.sliceAsBytes(original.list.items.items);
        const offset = writeAlignedData(buffer, &write_offset, data, @alignOf(u32));
        break :blk offset;
    } else 0;

    const interner_bytes_offset = if (original.interner.bytes.items.len > 0) blk: {
        const offset = writeAlignedData(buffer, &write_offset, original.interner.bytes.items, @alignOf(u8));
        break :blk offset;
    } else 0;

    const interner_indices_offset = if (original.interner.outer_indices.items.len > 0) blk: {
        const data = std.mem.sliceAsBytes(original.interner.outer_indices.items);
        const offset = writeAlignedData(buffer, &write_offset, data, @alignOf(SmallStringInterner.StringIdx));
        break :blk offset;
    } else 0;

    const interner_regions_offset = if (original.interner.regions.items.len > 0) blk: {
        const data = std.mem.sliceAsBytes(original.interner.regions.items);
        const offset = writeAlignedData(buffer, &write_offset, data, @alignOf(Region));
        break :blk offset;
    } else 0;

    // Serialize StringIdx.Table
    var interner_table_metadata_offset: usize = 0;
    if (original.interner.strings.unmanaged.metadata) |metadata| {
        // Align for metadata
        write_offset = std.mem.alignForward(usize, write_offset, @alignOf(@TypeOf(metadata)));
        interner_table_metadata_offset = write_offset;

        const capacity = original.interner.strings.capacity();
        const metadata_size = capacity;

        // Write metadata bytes
        @memcpy(buffer[write_offset .. write_offset + metadata_size], @as([*]const u8, @ptrCast(metadata))[0..metadata_size]);
        write_offset += metadata_size;

        // Write the keys and values data that follows metadata in memory
        const entry_size = capacity * @sizeOf(SmallStringInterner.StringIdx) + capacity * @sizeOf(void);
        const data_ptr = @as([*]const u8, @ptrCast(metadata)) + metadata_size;
        @memcpy(buffer[write_offset .. write_offset + entry_size], data_ptr[0..entry_size]);
        write_offset += entry_size;
    }

    // Serialize SafeStringHashMap
    var map_metadata_offset: usize = 0;
    var map_key_offsets = std.ArrayList(usize).init(allocator);
    defer map_key_offsets.deinit();

    if (original.map.map.metadata) |metadata| {
        // Align for metadata
        write_offset = std.mem.alignForward(usize, write_offset, @alignOf(@TypeOf(metadata)));
        map_metadata_offset = write_offset;

        const map_capacity = original.map.map.capacity();

        // Write metadata bytes
        @memcpy(buffer[write_offset .. write_offset + map_capacity], @as([*]const u8, @ptrCast(metadata))[0..map_capacity]);
        write_offset += map_capacity;

        // Reserve space for keys array
        write_offset = std.mem.alignForward(usize, write_offset, @alignOf([]const u8));
        const keys_array_offset = write_offset;
        write_offset += map_capacity * @sizeOf([]const u8);

        // Reserve space for values array
        write_offset = std.mem.alignForward(usize, write_offset, @alignOf(u16));
        const values_array_offset = write_offset;
        write_offset += map_capacity * @sizeOf(u16);

        // Write string data and update key pointers
        const keys_ptr = original.map.map.keys();
        const values_ptr = original.map.map.values();

        for (0..map_capacity) |i| {
            const metadata_byte = metadata[i];
            if (metadata_byte != 0xFF) { // Occupied slot
                // Write string data
                write_offset = std.mem.alignForward(usize, write_offset, @alignOf(u8));
                const key_data_offset = write_offset;
                @memcpy(buffer[write_offset .. write_offset + keys_ptr[i].len], keys_ptr[i]);
                write_offset += keys_ptr[i].len;
                try map_key_offsets.append(key_data_offset);

                // Write key slice with offset as pointer
                const key_slice_ptr = @as(*[]const u8, @ptrCast(@alignCast(&buffer[keys_array_offset + i * @sizeOf([]const u8)])));
                key_slice_ptr.* = .{
                    .ptr = @ptrFromInt(key_data_offset),
                    .len = keys_ptr[i].len,
                };

                // Copy value
                const value_ptr = @as(*u16, @ptrCast(@alignCast(&buffer[values_array_offset + i * @sizeOf(u16)])));
                value_ptr.* = values_ptr[i];
            } else {
                // Empty slot - write zeros
                @memset(buffer[keys_array_offset + i * @sizeOf([]const u8) ..][0..@sizeOf([]const u8)], 0);
                @memset(buffer[values_array_offset + i * @sizeOf(u16) ..][0..@sizeOf(u16)], 0);
            }
        }
    }

    // Write the TestStruct at the beginning with file offsets as pointers
    const struct_ptr = @as(*TestStruct, @ptrCast(@alignCast(buffer.ptr + struct_offset)));

    // Set up SafeList pointers as file offsets
    if (list_data_offset > 0) {
        struct_ptr.list.items.items.ptr = @ptrFromInt(list_data_offset);
    }
    struct_ptr.list.items.items.len = original.list.items.items.len;
    struct_ptr.list.items.capacity = original.list.items.capacity;

    // Set up SmallStringInterner pointers as file offsets
    if (interner_bytes_offset > 0) {
        struct_ptr.interner.bytes.items.ptr = @ptrFromInt(interner_bytes_offset);
    }
    struct_ptr.interner.bytes.items.len = original.interner.bytes.items.len;
    struct_ptr.interner.bytes.capacity = original.interner.bytes.capacity;

    if (interner_indices_offset > 0) {
        struct_ptr.interner.outer_indices.items.ptr = @ptrFromInt(interner_indices_offset);
    }
    struct_ptr.interner.outer_indices.items.len = original.interner.outer_indices.items.len;
    struct_ptr.interner.outer_indices.capacity = original.interner.outer_indices.capacity;

    if (interner_regions_offset > 0) {
        struct_ptr.interner.regions.items.ptr = @ptrFromInt(interner_regions_offset);
    }
    struct_ptr.interner.regions.items.len = original.interner.regions.items.len;
    struct_ptr.interner.regions.capacity = original.interner.regions.capacity;

    // Set up StringIdx.Table pointers as file offsets
    if (interner_table_metadata_offset != 0) {
        struct_ptr.interner.strings.unmanaged.metadata = @ptrFromInt(interner_table_metadata_offset);
        struct_ptr.interner.strings.unmanaged.size = original.interner.strings.size();
        struct_ptr.interner.strings.unmanaged.available = original.interner.strings.available();
        struct_ptr.interner.strings.unmanaged.header = original.interner.strings.unmanaged.header;
    }

    // Set up SafeStringHashMap pointers as file offsets
    if (map_metadata_offset != 0) {
        struct_ptr.map.map.metadata = @ptrFromInt(map_metadata_offset);
        struct_ptr.map.map.size = original.map.map.size;
        struct_ptr.map.map.available = original.map.map.available;
        struct_ptr.map.map.header = original.map.map.header;
    }

    // Simulate writing to disk and reading back
    const file_content = buffer[0..write_offset];

    // "Read" from disk - allocate aligned memory
    const read_buffer = try allocator.alignedAlloc(u8, @alignOf(TestStruct), file_content.len);
    defer allocator.free(read_buffer);
    @memcpy(read_buffer, file_content);

    // Cast buffer to TestStruct
    const loaded_struct = @as(*TestStruct, @ptrCast(@alignCast(read_buffer.ptr)));

    // Apply relocations - convert file offsets back to memory addresses
    const base_offset = @as(isize, @intCast(@intFromPtr(read_buffer.ptr)));
    relocateTestStruct(loaded_struct, base_offset);

    // === Verify SafeList data ===
    try std.testing.expectEqual(original.list.len(), loaded_struct.list.len());
    try std.testing.expectEqual(@as(u32, 42), loaded_struct.list.get(0));
    try std.testing.expectEqual(@as(u32, 123), loaded_struct.list.get(1));
    try std.testing.expectEqual(@as(u32, 456), loaded_struct.list.get(2));
    try std.testing.expectEqual(@as(u32, 789), loaded_struct.list.get(3));
    try std.testing.expectEqual(@as(u32, 0xDEADBEEF), loaded_struct.list.get(4));

    // === Verify SmallStringInterner data ===
    try std.testing.expectEqual(original.interner.outer_indices.items.len, loaded_struct.interner.outer_indices.items.len);

    // Test that strings are correctly stored
    try std.testing.expectEqualStrings("hello", loaded_struct.interner.getText(idx1));
    try std.testing.expectEqualStrings("world", loaded_struct.interner.getText(idx2));
    try std.testing.expectEqualStrings("roc rocks!", loaded_struct.interner.getText(idx3));
    try std.testing.expectEqualStrings("this is a longer string to test", loaded_struct.interner.getText(idx4));

    // Test that duplicate detection works
    try std.testing.expect(loaded_struct.interner.indicesHaveSameText(idx1, idx5));

    // Test regions are preserved
    const loaded_region1 = loaded_struct.interner.getRegion(idx1);
    const loaded_region2 = loaded_struct.interner.getRegion(idx2);
    const loaded_region3 = loaded_struct.interner.getRegion(idx3);
    const loaded_region4 = loaded_struct.interner.getRegion(idx4);

    try std.testing.expectEqual(region1.start, loaded_region1.start);
    try std.testing.expectEqual(region1.end, loaded_region1.end);
    try std.testing.expectEqual(region2.start, loaded_region2.start);
    try std.testing.expectEqual(region2.end, loaded_region2.end);
    try std.testing.expectEqual(region3.start, loaded_region3.start);
    try std.testing.expectEqual(region3.end, loaded_region3.end);
    try std.testing.expectEqual(region4.start, loaded_region4.start);
    try std.testing.expectEqual(region4.end, loaded_region4.end);

    // === Verify SafeStringHashMap data ===
    try std.testing.expectEqual(original.map.count(), loaded_struct.map.count());

    // Test direct constant-time lookups with exact keys
    try std.testing.expectEqual(@as(u16, 100), loaded_struct.map.get("first").?);
    try std.testing.expectEqual(@as(u16, 200), loaded_struct.map.get("second").?);
    try std.testing.expectEqual(@as(u16, 300), loaded_struct.map.get("third").?);
    try std.testing.expectEqual(@as(u16, 400), loaded_struct.map.get("fourth").?);
    try std.testing.expectEqual(@as(u16, 500), loaded_struct.map.get("fifth").?);

    // Test lookups for non-existent keys
    try std.testing.expect(loaded_struct.map.get("missing") == null);
    try std.testing.expect(loaded_struct.map.get("") == null);
    try std.testing.expect(loaded_struct.map.get("firs") == null); // partial key
    try std.testing.expect(loaded_struct.map.get("firstt") == null); // extra char
    try std.testing.expect(loaded_struct.map.get("FIRST") == null); // different case

    // Test lookups with keys that would hash to similar buckets
    try std.testing.expect(loaded_struct.map.get("tsrif") == null); // "first" reversed
    try std.testing.expect(loaded_struct.map.get("second2") == null);
    try std.testing.expect(loaded_struct.map.get("2second") == null);

    // Test contains() method
    try std.testing.expect(loaded_struct.map.contains("first"));
    try std.testing.expect(loaded_struct.map.contains("second"));
    try std.testing.expect(loaded_struct.map.contains("third"));
    try std.testing.expect(loaded_struct.map.contains("fourth"));
    try std.testing.expect(loaded_struct.map.contains("fifth"));
    try std.testing.expect(!loaded_struct.map.contains("sixth"));
    try std.testing.expect(!loaded_struct.map.contains(""));

    // Verify the hash map's internal integrity by doing many lookups
    // This ensures the hash function and bucket calculation work correctly
    const test_keys = [_][]const u8{
        "a",    "ab",      "abc",    "abcd",   "abcde", "abcdef",   "abcdefg",   "abcdefgh",
        "1",    "12",      "123",    "1234",   "12345", "123456",   "1234567",   "12345678",
        "test", "testing", "tester", "tested", "tests", "testable", "testament", "testimony",
    };

    // All these should return null (not found)
    for (test_keys) |key| {
        try std.testing.expect(loaded_struct.map.get(key) == null);
        try std.testing.expect(!loaded_struct.map.contains(key));
    }

    // Verify original keys still work after all the failed lookups
    try std.testing.expectEqual(@as(u16, 100), loaded_struct.map.get("first").?);
    try std.testing.expectEqual(@as(u16, 500), loaded_struct.map.get("fifth").?);

    // Verify we can iterate over the map
    var count: usize = 0;
    var sum: u32 = 0;
    var iter = loaded_struct.map.iterator();
    while (iter.next()) |entry| {
        count += 1;
        sum += entry.value_ptr.*;

        // Verify each entry
        if (std.mem.eql(u8, entry.key_ptr.*, "first")) {
            try std.testing.expectEqual(@as(u16, 100), entry.value_ptr.*);
        } else if (std.mem.eql(u8, entry.key_ptr.*, "second")) {
            try std.testing.expectEqual(@as(u16, 200), entry.value_ptr.*);
        } else if (std.mem.eql(u8, entry.key_ptr.*, "third")) {
            try std.testing.expectEqual(@as(u16, 300), entry.value_ptr.*);
        } else if (std.mem.eql(u8, entry.key_ptr.*, "fourth")) {
            try std.testing.expectEqual(@as(u16, 400), entry.value_ptr.*);
        } else if (std.mem.eql(u8, entry.key_ptr.*, "fifth")) {
            try std.testing.expectEqual(@as(u16, 500), entry.value_ptr.*);
        } else {
            return error.UnexpectedKey;
        }
    }
    try std.testing.expectEqual(@as(usize, 5), count);
    try std.testing.expectEqual(@as(u32, 1500), sum); // 100+200+300+400+500
}

test "FixupCache hash map stress test with many entries" {
    const allocator = std.testing.allocator;

    // Create a hash map with many entries to test hash collisions and bucket distribution
    var map = SafeStringHashMap(u32){};
    defer map.deinit(allocator);

    // Add many entries
    const num_entries = 100;
    for (0..num_entries) |i| {
        var key_buf: [32]u8 = undefined;
        const key = try std.fmt.bufPrint(&key_buf, "key_{d}", .{i});
        try map.put(allocator, key, @intCast(i * 10));
    }

    // Calculate size needed
    var total_size: usize = @sizeOf(SafeStringHashMap(u32));

    if (map.map.metadata) |_| {
        const map_capacity = map.map.capacity();
        const metadata_size = map_capacity;
        const keys_size = map_capacity * @sizeOf([]const u8);
        const values_size = map_capacity * @sizeOf(u32);
        total_size += metadata_size + keys_size + values_size;

        var iter = map.iterator();
        while (iter.next()) |entry| {
            total_size += entry.key_ptr.len;
        }
    }

    total_size += 256; // Extra padding

    // Serialize
    const buffer = try allocator.alignedAlloc(u8, 16, total_size);
    defer allocator.free(buffer);

    var write_offset: usize = 0;
    const map_ptr = @as(*SafeStringHashMap(u32), @ptrCast(@alignCast(buffer.ptr)));
    write_offset += @sizeOf(SafeStringHashMap(u32));

    // Serialize the hash map data
    if (map.map.metadata) |metadata| {
        write_offset = std.mem.alignForward(usize, write_offset, @alignOf(@TypeOf(metadata)));
        const metadata_offset = write_offset;

        const map_capacity = map.map.capacity();
        @memcpy(buffer[write_offset .. write_offset + map_capacity], @as([*]const u8, @ptrCast(metadata))[0..map_capacity]);
        write_offset += map_capacity;

        write_offset = std.mem.alignForward(usize, write_offset, @alignOf([]const u8));
        const keys_array_offset = write_offset;
        write_offset += map_capacity * @sizeOf([]const u8);

        write_offset = std.mem.alignForward(usize, write_offset, @alignOf(u32));
        const values_array_offset = write_offset;
        write_offset += map_capacity * @sizeOf(u32);

        const keys_ptr = map.map.keys();
        const values_ptr = map.map.values();

        for (0..map_capacity) |i| {
            const metadata_byte = metadata[i];
            if (metadata_byte != 0xFF) {
                write_offset = std.mem.alignForward(usize, write_offset, @alignOf(u8));
                const key_data_offset = write_offset;
                @memcpy(buffer[write_offset .. write_offset + keys_ptr[i].len], keys_ptr[i]);
                write_offset += keys_ptr[i].len;

                const key_slice_ptr = @as(*[]const u8, @ptrCast(@alignCast(&buffer[keys_array_offset + i * @sizeOf([]const u8)])));
                key_slice_ptr.* = .{
                    .ptr = @ptrFromInt(key_data_offset),
                    .len = keys_ptr[i].len,
                };

                const value_ptr = @as(*u32, @ptrCast(@alignCast(&buffer[values_array_offset + i * @sizeOf(u32)])));
                value_ptr.* = values_ptr[i];
            }
        }

        // Set up the map structure
        map_ptr.map.metadata = @ptrFromInt(metadata_offset);
        map_ptr.map.size = map.map.size;
        map_ptr.map.available = map.map.available;
        map_ptr.map.header = map.map.header;
    }

    // Simulate deserialization
    const read_buffer = try allocator.alignedAlloc(u8, 16, buffer.len);
    defer allocator.free(read_buffer);
    @memcpy(read_buffer, buffer);

    const loaded_map = @as(*SafeStringHashMap(u32), @ptrCast(@alignCast(read_buffer.ptr)));
    const base_offset = @as(isize, @intCast(@intFromPtr(read_buffer.ptr)));
    relocateSafeStringHashMap(u32, loaded_map, base_offset);

    // Verify all lookups work correctly
    for (0..num_entries) |i| {
        var key_buf: [32]u8 = undefined;
        const key = try std.fmt.bufPrint(&key_buf, "key_{d}", .{i});

        const value = loaded_map.get(key);
        try std.testing.expect(value != null);
        try std.testing.expectEqual(@as(u32, @intCast(i * 10)), value.?);
        try std.testing.expect(loaded_map.contains(key));
    }

    // Test non-existent keys to ensure hash function works correctly
    try std.testing.expect(loaded_map.get("key_100") == null);
    try std.testing.expect(loaded_map.get("key_-1") == null);
    try std.testing.expect(loaded_map.get("not_a_key") == null);
    try std.testing.expect(loaded_map.get("") == null);

    // Verify count is preserved
    try std.testing.expectEqual(@as(usize, num_entries), loaded_map.count());
}

test "FixupCache hash map distribution preserved" {
    const allocator = std.testing.allocator;

    // Create a hash map and track bucket distribution
    var map = SafeStringHashMap(u32){};
    defer map.deinit(allocator);

    // Add entries with keys designed to test hash distribution
    const test_entries = [_]struct { key: []const u8, value: u32 }{
        .{ .key = "apple", .value = 1 },
        .{ .key = "banana", .value = 2 },
        .{ .key = "cherry", .value = 3 },
        .{ .key = "date", .value = 4 },
        .{ .key = "elderberry", .value = 5 },
        .{ .key = "fig", .value = 6 },
        .{ .key = "grape", .value = 7 },
        .{ .key = "honeydew", .value = 8 },
        // Keys that might collide
        .{ .key = "ab", .value = 10 },
        .{ .key = "ba", .value = 11 },
        .{ .key = "abc", .value = 12 },
        .{ .key = "bca", .value = 13 },
        .{ .key = "cab", .value = 14 },
    };

    for (test_entries) |entry| {
        try map.put(allocator, entry.key, entry.value);
    }

    // Get original bucket distribution
    var original_distribution = std.ArrayList(u8).init(allocator);
    defer original_distribution.deinit();

    if (map.map.metadata) |metadata| {
        const capacity = map.map.capacity();
        try original_distribution.resize(capacity);
        @memcpy(original_distribution.items, metadata[0..capacity]);
    }

    // Serialize and deserialize
    var total_size: usize = @sizeOf(SafeStringHashMap(u32));
    if (map.map.metadata) |_| {
        const map_capacity = map.map.capacity();
        total_size += map_capacity; // metadata
        total_size += map_capacity * @sizeOf([]const u8); // keys
        total_size += map_capacity * @sizeOf(u32); // values
        var iter = map.iterator();
        while (iter.next()) |entry| {
            total_size += entry.key_ptr.len;
        }
    }
    total_size += 256; // padding

    const buffer = try allocator.alignedAlloc(u8, 16, total_size);
    defer allocator.free(buffer);

    var write_offset: usize = 0;
    const map_ptr = @as(*SafeStringHashMap(u32), @ptrCast(@alignCast(buffer.ptr)));
    write_offset += @sizeOf(SafeStringHashMap(u32));

    if (map.map.metadata) |metadata| {
        write_offset = std.mem.alignForward(usize, write_offset, @alignOf(@TypeOf(metadata)));
        const metadata_offset = write_offset;
        const map_capacity = map.map.capacity();

        @memcpy(buffer[write_offset .. write_offset + map_capacity], @as([*]const u8, @ptrCast(metadata))[0..map_capacity]);
        write_offset += map_capacity;

        write_offset = std.mem.alignForward(usize, write_offset, @alignOf([]const u8));
        const keys_array_offset = write_offset;
        write_offset += map_capacity * @sizeOf([]const u8);

        write_offset = std.mem.alignForward(usize, write_offset, @alignOf(u32));
