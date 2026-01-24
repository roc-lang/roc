//! Platform ABI header generation for C interop
//!
//! This module generates C header files that describe the ABI contract between
//! a Roc platform and its hosted functions. The header includes function
//! signatures, struct definitions, and size/alignment information.

const std = @import("std");

const Allocator = std.mem.Allocator;

/// C struct field information
pub const CField = struct {
    name: []const u8, // Field name (e.g., "arg0" or "message")
    c_type: []const u8, // C type (e.g., "RocStr", "uint64_t")
    roc_type: []const u8, // Original Roc type for comments
};

/// Information about a hosted function for ABI generation
pub const HostedFunctionAbi = struct {
    name: []const u8, // e.g., "Stdout.line"
    qualified_name: []const u8, // e.g., "Stdout.line"
    index: u32, // Index in hosted_fns array (alphabetically sorted)
    roc_type_str: []const u8, // e.g., "Str => {}"
    args_struct_name: []const u8, // e.g., "StdoutLineArgs"
    fields: []CField, // Argument fields
    return_roc_type: []const u8, // Return type in Roc syntax (e.g., "{}", "Str")
    return_c_type: []const u8, // Return type in C syntax (e.g., "void", "RocStr*")
    return_is_void: bool, // True if return type is {} or ()

    pub fn deinit(self: *HostedFunctionAbi, gpa: Allocator) void {
        gpa.free(self.name);
        gpa.free(self.qualified_name);
        gpa.free(self.roc_type_str);
        gpa.free(self.args_struct_name);
        // Free fields
        for (self.fields) |field| {
            gpa.free(field.name);
            gpa.free(field.c_type);
            gpa.free(field.roc_type);
        }
        gpa.free(self.fields);
        gpa.free(self.return_roc_type);
        gpa.free(self.return_c_type);
    }
};

/// Check if a Roc type represents void/unit (empty return)
pub fn isVoidType(roc_type: []const u8) bool {
    return std.mem.eql(u8, roc_type, "{}") or
        std.mem.eql(u8, roc_type, "{  }") or
        std.mem.eql(u8, roc_type, "()") or
        std.mem.eql(u8, roc_type, "({})");
}

/// Map a Roc type string to its C representation
/// NOTE: Returns VALUE types, not pointers. In RocCall ABI:
/// - Arguments are passed by VALUE in the args struct
/// - Return values are written by VALUE to *ret_ptr
pub fn rocTypeToCType(gpa: Allocator, roc_type: []const u8) ![]const u8 {
    // Handle common primitive types
    if (std.mem.eql(u8, roc_type, "Str")) {
        // RocStr is a 24-byte struct (on 64-bit), passed by value
        return try gpa.dupe(u8, "RocStr");
    } else if (std.mem.eql(u8, roc_type, "Bool")) {
        return try gpa.dupe(u8, "bool");
    } else if (std.mem.eql(u8, roc_type, "U8")) {
        return try gpa.dupe(u8, "uint8_t");
    } else if (std.mem.eql(u8, roc_type, "U16")) {
        return try gpa.dupe(u8, "uint16_t");
    } else if (std.mem.eql(u8, roc_type, "U32")) {
        return try gpa.dupe(u8, "uint32_t");
    } else if (std.mem.eql(u8, roc_type, "U64")) {
        return try gpa.dupe(u8, "uint64_t");
    } else if (std.mem.eql(u8, roc_type, "U128")) {
        return try gpa.dupe(u8, "unsigned __int128");
    } else if (std.mem.eql(u8, roc_type, "I8")) {
        return try gpa.dupe(u8, "int8_t");
    } else if (std.mem.eql(u8, roc_type, "I16")) {
        return try gpa.dupe(u8, "int16_t");
    } else if (std.mem.eql(u8, roc_type, "I32")) {
        return try gpa.dupe(u8, "int32_t");
    } else if (std.mem.eql(u8, roc_type, "I64")) {
        return try gpa.dupe(u8, "int64_t");
    } else if (std.mem.eql(u8, roc_type, "I128")) {
        return try gpa.dupe(u8, "__int128");
    } else if (std.mem.eql(u8, roc_type, "F32")) {
        return try gpa.dupe(u8, "float");
    } else if (std.mem.eql(u8, roc_type, "F64")) {
        return try gpa.dupe(u8, "double");
    } else if (std.mem.startsWith(u8, roc_type, "List ") or std.mem.startsWith(u8, roc_type, "List(")) {
        // RocList is a 24-byte struct (on 64-bit), passed by value
        // Handles both "List Str" and "List(U8)" syntax
        return try gpa.dupe(u8, "RocList");
    } else if (std.mem.eql(u8, roc_type, "{}") or std.mem.eql(u8, roc_type, "{  }")) {
        // Empty record - no arguments (handle both {} and {  } from TypeWriter)
        return try gpa.dupe(u8, "void");
    } else if (std.mem.eql(u8, roc_type, "()") or std.mem.eql(u8, roc_type, "({})")) {
        // Unit type
        return try gpa.dupe(u8, "void");
    } else if (std.mem.startsWith(u8, roc_type, "{")) {
        // Record type - for now, use opaque pointer
        // Don't include comments in type string - causes nested comment issues
        return try gpa.dupe(u8, "void*");
    } else if (std.mem.startsWith(u8, roc_type, "[")) {
        // Tag union - use opaque pointer for now
        // Don't include comments in type string - causes nested comment issues
        return try gpa.dupe(u8, "void*");
    } else {
        // Unknown type - use opaque pointer
        // Don't include comments in type string - causes nested comment issues
        return try gpa.dupe(u8, "void*");
    }
}

/// Generate C header file with platform ABI
pub fn generateCHeader(
    gpa: Allocator,
    output_path: []const u8,
    hosted_fns: []const HostedFunctionAbi,
    target_ptr_size: u8, // 4 for 32-bit, 8 for 64-bit
) !void {
    const file = try std.fs.cwd().createFile(output_path, .{});
    defer file.close();

    var file_buffer: [8192]u8 = undefined;
    var file_writer = file.writer(&file_buffer);
    const writer = &file_writer.interface;

    // Calculate target-specific values for documentation
    const size_bytes = target_ptr_size * 3; // RocStr/RocList size
    const align_bytes = target_ptr_size; // Alignment same as pointer
    const ptr_bits = target_ptr_size * 8; // Pointer width in bits

    // 1. Header guard and documentation
    try writer.writeAll(
        \\/* Auto-generated Platform ABI Header */
        \\/*
        \\ * This header defines the C ABI for Roc platform hosted functions.
        \\ *
        \\ * All hosted functions follow the RocCall ABI:
        \\ *   void hosted_fn(const RocOps* ops, void* ret_ptr, void* args_ptr)
        \\ *
        \\ * Where:
        \\ *   - ops: Operations struct for memory management and effects
        \\ *   - ret_ptr: Pointer to return value location (NULL for void returns)
        \\ *   - args_ptr: Pointer to arguments struct
        \\ *
        \\
    );

    try writer.print(
        \\ * CRITICAL ABI CONVENTIONS:
        \\ *
        \\ * DATA PASSING:
        \\ *   - Arguments are passed BY VALUE in the args struct
        \\ *   - RocStr and RocList are STRUCT TYPES ({d} bytes), NOT pointers
        \\ *   - ret_ptr points to WHERE to write the return value (by value)
        \\ *   - For return type Str: cast to (RocStr*)ret_ptr, write with *ret = value
        \\ *   - For return type {{}}: ret_ptr is NULL (void return)
        \\ *
        \\ * MEMORY MANAGEMENT:
        \\ *   - NEVER manually free RocStr/RocList - they use automatic refcounting
        \\ *   - Functions BORROW (caller keeps ownership) or CONSUME (callee takes ownership)
        \\ *   - Readonly/saturated refcounts are LEAKED PERMANENTLY
        \\ *   - See roc/src/builtins/OWNERSHIP.md for complete ownership semantics
        \\ *
        \\ * ALIGNMENT:
        \\ *   - RocStr/RocList alignment = void* alignment = {d} bytes on this target
        \\ *   - Use _Alignas(void*) or __attribute__((aligned({d}))) if needed
        \\ *
        \\ * Hosted functions are indexed alphabetically by fully-qualified name.
        \\ */
        \\#ifndef ROC_PLATFORM_ABI_H
        \\#define ROC_PLATFORM_ABI_H
        \\
        \\#include <stdint.h>
        \\#include <stddef.h>
        \\
        \\
    , .{ size_bytes, align_bytes, align_bytes });

    // 2. Reference documentation section
    try writer.print(
        \\/* ========== REFERENCE DOCUMENTATION ========== */
        \\/*
        \\ * TARGET CONFIGURATION:
        \\ *   - Pointer size: {d} bytes ({d}-bit)
        \\ *   - RocStr/RocList size: {d} bytes
        \\ *   - RocStr/RocList alignment: {d} bytes
        \\ *
        \\ * OWNERSHIP & MEMORY MANAGEMENT:
        \\ *   See roc/src/builtins/OWNERSHIP.md for complete documentation on:
        \\ *   - Borrow vs Consume semantics
        \\ *   - Reference counting rules
        \\ *   - Seamless slice ownership
        \\ *   - Copy-on-write behavior
        \\ *
        \\ * COMMON PITFALLS TO AVOID:
        \\ *   1. DO NOT free RocStr/RocList manually - use Roc's memory management
        \\ *   2. DO NOT assume pointer stability - copy-on-write may return same pointer
        \\ *   3. DO NOT forget readonly state - saturated refcounts leak permanently
        \\ *   4. DO NOT mix borrow/consume patterns - check function signatures carefully
        \\ */
        \\
        \\
    , .{ target_ptr_size, ptr_bits, size_bytes, align_bytes });

    // 3. Type definitions
    try writer.writeAll(
        \\/* Roc runtime types */
        \\
        \\
    );

    // RocStr typedef with target-specific documentation
    try writer.print(
        \\/* RocStr: Reference-counted string type
        \\ *
        \\ * SIZE: {d} bytes on this target ({d}-bit pointers)
        \\ * ALIGNMENT: {d} bytes (same as void*)
        \\ *
        \\ * LAYOUT:
        \\ * - Small strings (≤23 bytes on 64-bit): stored inline, no allocation
        \\ * - Large strings: heap-allocated with automatic refcounting
        \\ *
        \\ * MEMORY MANAGEMENT - CRITICAL:
        \\ * - DO NOT manually free RocStr - use Roc's refcount system
        \\ * - Readonly state: When refcount saturates, string is leaked permanently
        \\ * - Functions borrow or consume - see OWNERSHIP.md for details
        \\ *
        \\ * For complete ownership semantics, see:
        \\ *   roc/src/builtins/OWNERSHIP.md
        \\ */
        \\typedef struct RocStr {{
        \\    uint8_t bytes[3 * sizeof(void*)];
        \\}} RocStr;
        \\
        \\
    , .{ size_bytes, ptr_bits, align_bytes });

    // RocList typedef with target-specific documentation
    try writer.print(
        \\/* RocList: Reference-counted list type
        \\ *
        \\ * SIZE: {d} bytes on this target ({d}-bit pointers)
        \\ * ALIGNMENT: {d} bytes (same as void*)
        \\ *
        \\ * LAYOUT:
        \\ * - Empty list: zero-initialized, no allocation
        \\ * - Non-empty: heap-allocated elements with automatic refcounting
        \\ *
        \\ * MEMORY MANAGEMENT - CRITICAL:
        \\ * - DO NOT manually free RocList - use Roc's refcount system
        \\ * - Readonly state: When refcount saturates, list is leaked permanently
        \\ * - Seamless slices: May share underlying allocation with parent list
        \\ * - Functions borrow or consume - see OWNERSHIP.md for details
        \\ *
        \\ * For complete ownership semantics, see:
        \\ *   roc/src/builtins/OWNERSHIP.md
        \\ */
        \\typedef struct RocList {{
        \\    uint8_t bytes[3 * sizeof(void*)];
        \\}} RocList;
        \\
        \\
    , .{ size_bytes, ptr_bits, align_bytes });

    // Forward declarations
    try writer.writeAll(
        \\/* RocOps: Operations struct for memory management
        \\ * Full definition in builtins module */
        \\typedef struct RocOps RocOps;
        \\
        \\/* Hosted function pointer type */
        \\typedef void (*HostedFn)(const RocOps*, void*, void*);
        \\
        \\
    );

    // 3. Function count and indices
    try writer.print("#define HOSTED_FUNCTION_COUNT {}\n\n", .{hosted_fns.len});

    if (hosted_fns.len > 0) {
        try writer.writeAll("/* Hosted function indices (alphabetically sorted) */\n");
        for (hosted_fns, 0..) |fn_abi, idx| {
            try writer.writeAll("#define HOSTED_IDX_");
            for (fn_abi.name) |c| {
                if (c == '.') {
                    try writer.writeByte('_');
                } else {
                    try writer.writeByte(std.ascii.toUpper(c));
                }
            }
            try writer.print(" {}\n", .{idx});
        }
        try writer.writeAll("\n/* ========== Hosted Function Definitions ========== */\n\n");

        // 4. For each function: struct, return docs, example, assertions
        for (hosted_fns) |fn_abi| {
            // Function signature comment
            try writer.print("/* {s} : {s} */\n", .{ fn_abi.qualified_name, fn_abi.roc_type_str });

            // Arguments struct
            try writer.print("typedef struct {s} {{\n", .{fn_abi.args_struct_name});
            if (fn_abi.fields.len == 0) {
                try writer.writeAll("    char _dummy;  /* No arguments */\n");
            } else {
                for (fn_abi.fields) |field| {
                    try writer.print("    {s} {s};  /* {s} */\n", .{ field.c_type, field.name, field.roc_type });
                }
            }
            try writer.print("}} {s};\n\n", .{fn_abi.args_struct_name});

            // Return type documentation
            if (fn_abi.return_is_void) {
                try writer.print("/* Return: {s} (void - ret_ptr is NULL) */\n\n", .{fn_abi.return_roc_type});
            } else {
                const is_refcounted = std.mem.eql(u8, fn_abi.return_c_type, "RocStr") or
                                      std.mem.eql(u8, fn_abi.return_c_type, "RocList");

                if (is_refcounted) {
                    try writer.print(
                        \\/* Return: {s} ({s} - {d} bytes, {d}-byte aligned)
                        \\ * OWNERSHIP: Caller owns return value - responsible for eventual cleanup
                        \\ */
                        \\
                        \\
                    , .{ fn_abi.return_roc_type, fn_abi.return_c_type, size_bytes, align_bytes });
                } else {
                    try writer.print("/* Return: {s} ({s}) */\n\n", .{ fn_abi.return_roc_type, fn_abi.return_c_type });
                }
            }

            // Example implementation skeleton
            const fn_name_lower = try generateCFunctionName(gpa, fn_abi.name);
            defer gpa.free(fn_name_lower);

            try writer.print("/* Example implementation:\n", .{});
            try writer.print("void hosted_{s}(const RocOps* ops, void* ret_ptr, void* args_ptr) {{\n", .{fn_name_lower});
            try writer.print("    {s}* args = ({s}*)args_ptr;\n", .{ fn_abi.args_struct_name, fn_abi.args_struct_name });

            if (!fn_abi.return_is_void) {
                try writer.print("    {s}* ret = ({s}*)ret_ptr;\n", .{ fn_abi.return_c_type, fn_abi.return_c_type });
            }

            // Show how to access args
            if (fn_abi.fields.len > 0) {
                try writer.writeAll("    // Access arguments:\n");
                for (fn_abi.fields) |field| {
                    try writer.print("    //   args->{s}\n", .{field.name});
                }
            }

            if (!fn_abi.return_is_void) {
                try writer.writeAll("    // Set return value:\n");
                try writer.writeAll("    //   *ret = ...\n");
            }

            try writer.writeAll("}\n*/\n\n");

            // Static assertion for struct alignment
            try writer.print("_Static_assert(sizeof({s}) % _Alignof({s}) == 0 || sizeof({s}) == 1,\n", .{ fn_abi.args_struct_name, fn_abi.args_struct_name, fn_abi.args_struct_name });
            try writer.print("               \"{s} size not aligned\");\n\n", .{fn_abi.args_struct_name});
        }

        // 5. HostedFunctions struct and example
        try writer.writeAll(
            \\/* ========== Platform Interface ========== */
            \\
            \\/* Array of hosted function pointers (sorted alphabetically) */
            \\typedef struct HostedFunctions {
            \\    uint32_t count;
            \\    const HostedFn* fns;
            \\} HostedFunctions;
            \\
            \\/*
            \\ * Platform must provide an array of function pointers in alphabetical order:
            \\ * const HostedFn hosted_fns[] = {
            \\
        );

        for (hosted_fns) |fn_abi| {
            const fn_name_lower = try generateCFunctionName(gpa, fn_abi.name);
            defer gpa.free(fn_name_lower);
            try writer.print(" *     hosted_{s},  // Index {}\n", .{ fn_name_lower, fn_abi.index });
        }

        try writer.writeAll(
            \\ * };
            \\ */
            \\
            \\
        );
    }

    try writer.writeAll("#endif /* ROC_PLATFORM_ABI_H */\n");

    // Flush the buffered writer before closing the file
    try file_writer.interface.flush();
}

/// Generate C struct name from qualified function name
/// E.g., "Stdout.line" -> "StdoutLineArgs"
pub fn generateStructName(gpa: Allocator, qualified_name: []const u8) ![]const u8 {
    var buf = std.ArrayList(u8).empty;
    defer buf.deinit(gpa);

    var capitalize_next = true;
    for (qualified_name) |c| {
        if (c == '.' or c == '!') {
            // Skip punctuation, capitalize next letter
            capitalize_next = true;
        } else if (capitalize_next) {
            try buf.append(gpa, std.ascii.toUpper(c));
            capitalize_next = false;
        } else {
            try buf.append(gpa, c);
        }
    }

    try buf.appendSlice(gpa, "Args");

    return buf.toOwnedSlice(gpa);
}

/// Generate a C-friendly function name from qualified Roc name
/// E.g., "Stdout.line" -> "stdout_line"
fn generateCFunctionName(gpa: Allocator, qualified_name: []const u8) ![]const u8 {
    var buf: std.ArrayList(u8) = .empty;
    errdefer buf.deinit(gpa);

    for (qualified_name) |c| {
        if (c == '.' or c == '!') {
            try buf.append(gpa, '_');
        } else {
            try buf.append(gpa, std.ascii.toLower(c));
        }
    }

    return buf.toOwnedSlice(gpa);
}
