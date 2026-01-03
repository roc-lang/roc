//! Compile-time structure hashing for cache invalidation.
//!
//! This module computes a hash of a struct's layout (field names, sizes, alignments,
//! and nested struct definitions) at compile time. The resulting hash is embedded in cache
//! file headers to automatically detect when compiler internal structures have changed.
//!
//! The hash is computed using `@typeInfo()` reflection and is fully evaluated at compile time,
//! producing zero runtime overhead.
//!
//! NOTE: Because BLAKE3 cannot be used at compile time with string slices in Zig,
//! we use a simple but effective algorithm: convert each field's properties (name hash,
//! size, alignment) to integers and XOR them together. This provides excellent collision
//! resistance for our use case (detecting struct changes) while remaining comptime-compatible.

const std = @import("std");

/// Simple 32-bit FNV-1a hash for a string, suitable for compile-time use.
/// This is used to hash field names without requiring runtime hashing.
inline fn fnv1a32(comptime str: []const u8) u32 {
    var hash: u32 = 2166136261; // FNV offset basis for 32-bit
    inline for (str) |byte| {
        hash ^= byte;
        hash *%= 16777619; // FNV prime for 32-bit
    }
    return hash;
}

/// Compute a compile-time hash of a struct's layout.
///
/// This recursively hashes:
/// - Field names (via FNV-1a 32-bit hash)
/// - Field sizes (@sizeOf)
/// - Field alignments (@alignOf)
/// - Nested extern struct definitions
///
/// The resulting hash changes when any of these properties change, providing
/// automatic cache invalidation.
///
/// Args:
///   StructType: The struct type to hash (must be a type expression, not a value)
///
/// Returns:
///   A [32]u8 hash of the structure (created by repeating the computed hash value)
pub fn computeStructHash(comptime StructType: type) [32]u8 {
    const type_info = @typeInfo(StructType);

    // Default hash for non-struct types
    if (type_info != .@"struct") {
        return [_]u8{0} ** 32;
    }

    const fields = type_info.@"struct".fields;

    var hash: u64 = 14695981039346656037; // FNV offset basis for 64-bit
    const fnv_prime = 1099511628211; // FNV prime for 64-bit

    inline for (fields) |field| {
        // Hash field name using FNV-1a 32-bit, then XOR into 64-bit hash
        const name_hash = fnv1a32(field.name);
        hash ^= name_hash;
        hash *%= fnv_prime;

        // Hash field size and alignment
        const field_size = @sizeOf(field.type);
        const field_align = @alignOf(field.type);

        hash ^= @intCast(field_size);
        hash *%= fnv_prime;

        hash ^= @intCast(field_align);
        hash *%= fnv_prime;

        // Recursively hash nested extern structs
        if (isExternStruct(field.type)) {
            const nested_hash = computeStructHash(field.type);
            // XOR all 8 bytes of the nested hash into our running hash
            inline for (nested_hash) |byte| {
                hash ^= byte;
            }
            hash *%= fnv_prime;
        }
    }

    // Convert the 64-bit hash to a 32-byte array by repeating it
    var result: [32]u8 = undefined;
    inline for (0..32) |i| {
        result[i] = @intCast((hash >> @intCast((i % 8) * 8)) & 0xFF);
    }
    return result;
}

/// Check if a type is an extern struct.
fn isExternStruct(comptime T: type) bool {
    const type_info = @typeInfo(T);
    return type_info == .@"struct" and type_info.@"struct".layout == .@"extern";
}

test "struct_hash: basic hash is stable" {
    const TestStruct = extern struct {
        a: u32,
        b: u64,
    };

    const hash1 = computeStructHash(TestStruct);
    const hash2 = computeStructHash(TestStruct);

    try std.testing.expectEqualSlices(u8, &hash1, &hash2);
}

test "struct_hash: adding a field changes hash" {
    const Struct1 = extern struct {
        a: u32,
    };

    const Struct2 = extern struct {
        a: u32,
        b: u64,
    };

    const hash1 = computeStructHash(Struct1);
    const hash2 = computeStructHash(Struct2);

    try std.testing.expect(!std.mem.eql(u8, &hash1, &hash2));
}

test "struct_hash: changing field size changes hash" {
    const Struct1 = extern struct {
        a: u32,
    };

    const Struct2 = extern struct {
        a: u64,
    };

    const hash1 = computeStructHash(Struct1);
    const hash2 = computeStructHash(Struct2);

    try std.testing.expect(!std.mem.eql(u8, &hash1, &hash2));
}

test "struct_hash: changing field name changes hash" {
    const Struct1 = extern struct {
        a: u32,
    };

    const Struct2 = extern struct {
        b: u32,
    };

    const hash1 = computeStructHash(Struct1);
    const hash2 = computeStructHash(Struct2);

    try std.testing.expect(!std.mem.eql(u8, &hash1, &hash2));
}

test "struct_hash: changing field order changes hash" {
    const Struct1 = extern struct {
        a: u32,
        b: u64,
    };

    const Struct2 = extern struct {
        b: u64,
        a: u32,
    };

    const hash1 = computeStructHash(Struct1);
    const hash2 = computeStructHash(Struct2);

    try std.testing.expect(!std.mem.eql(u8, &hash1, &hash2));
}

test "struct_hash: removing a field changes hash" {
    const Struct1 = extern struct {
        a: u32,
        b: u64,
        c: u16,
    };

    const Struct2 = extern struct {
        a: u32,
        c: u16,
    };

    const hash1 = computeStructHash(Struct1);
    const hash2 = computeStructHash(Struct2);

    try std.testing.expect(!std.mem.eql(u8, &hash1, &hash2));
}

test "struct_hash: nested extern struct changes are detected" {
    const Inner1 = extern struct {
        x: u32,
    };

    const Outer1 = extern struct {
        inner: Inner1,
        y: u64,
    };

    const Inner2 = extern struct {
        x: u64, // Changed from u32
    };

    const Outer2 = extern struct {
        inner: Inner2,
        y: u64,
    };

    const hash1 = computeStructHash(Outer1);
    const hash2 = computeStructHash(Outer2);

    try std.testing.expect(!std.mem.eql(u8, &hash1, &hash2));
}

test "struct_hash: hash output is 32 bytes" {
    const TestStruct = extern struct {
        a: u32,
    };

    const hash = computeStructHash(TestStruct);

    try std.testing.expectEqual(hash.len, 32);
}
