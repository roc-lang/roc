const std = @import("std");
const serialization = @import("serialization");
const collections = @import("collections");
const base = @import("base");

const testing = std.testing;
const Allocator = std.mem.Allocator;
const IovecWriter = serialization.IovecWriter;

test "simple POD struct iovec serialization + relocation" {
    const gpa = testing.allocator;
    
    // Simple POD structure
    const SimpleStruct = struct {
        a: u32,
        b: u64,
        c: u8,
        
        const Self = @This();
        
        pub fn appendToIovecs(self: *const Self, writer: anytype) !usize {
            const start_offset = try writer.appendStruct(self.*);
            return start_offset;
        }
        
        pub fn relocate(self: *Self, offset: isize) void {
            // POD struct has no pointers to relocate
            _ = self;
            _ = offset;
        }
    };
    
    const original = SimpleStruct{ .a = 42, .b = 123456789, .c = 255 };
    
    // Serialize using iovecs
    var writer = IovecWriter.init(gpa);
    defer writer.deinit();
    
    const struct_offset = try original.appendToIovecs(&writer);
    const serialized_data = try writer.serialize(gpa);
    defer gpa.free(serialized_data);
    
    // "Deserialize" using relocation
    const restored_ptr = @as(*SimpleStruct, @ptrCast(@alignCast(@constCast(serialized_data[struct_offset..].ptr))));
    var restored = restored_ptr.*;
    
    // Apply relocations (none needed for POD)
    const relocation_offset: isize = 0; // No relocation needed for POD
    restored.relocate(relocation_offset);
    
    // Verify round-trip
    try testing.expectEqual(original.a, restored.a);
    try testing.expectEqual(original.b, restored.b);
    try testing.expectEqual(original.c, restored.c);
}

test "SafeList iovec serialization + relocation basic" {
    const gpa = testing.allocator;
    
    // Create original SafeList
    var original = collections.SafeList(u32){};
    defer original.deinit(gpa);
    
    _ = try original.append(gpa, 10);
    _ = try original.append(gpa, 20);
    _ = try original.append(gpa, 30);
    
    // Serialize using iovecs
    var writer = IovecWriter.init(gpa);
    defer writer.deinit();
    
    const list_offset = try original.appendToIovecs(&writer);
    const serialized_data = try writer.serialize(gpa);
    defer gpa.free(serialized_data);
    
    // Get the serialized data starting point
    const cache_base_addr = @intFromPtr(serialized_data.ptr);
    
    // "Deserialize" by getting the SafeList struct from the serialized data
    const restored_ptr = @as(*collections.SafeList(u32), @ptrCast(@alignCast(@constCast(serialized_data[list_offset..].ptr))));
    var restored = restored_ptr.*;
    
    // Before relocation, the items.ptr contains an offset (not a real pointer)
    // We need to treat it as an offset first
    if (restored.items.items.len > 0) {
        // The ptr field contains an offset, not a real pointer
        const items_offset = @intFromPtr(restored.items.items.ptr);
        
        if (items_offset != serialization.iovec_serialize.EMPTY_ARRAY_SENTINEL) {
            // Convert offset to actual pointer  
            restored.items.items.ptr = @ptrFromInt(cache_base_addr + items_offset);
        }
    }
    
    // Verify round-trip
    try testing.expectEqual(original.len(), restored.len());
    try testing.expectEqual(@as(u32, 10), restored.get(@enumFromInt(0)).*);
    try testing.expectEqual(@as(u32, 20), restored.get(@enumFromInt(1)).*);
    try testing.expectEqual(@as(u32, 30), restored.get(@enumFromInt(2)).*);
}