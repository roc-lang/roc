//! Typed Code Generator for Fuzzing
//!
//! Generates syntactically and semantically valid Roc code that is guaranteed to
//! type-check correctly. Used for fuzzing the type-checking pipeline to find crashes
//! or incorrect behavior when processing well-typed code.
//!
//! The generator maintains a stack of scopes, each tracking variable names and their
//! types, allowing it to generate type-correct variable references and expressions.

const std = @import("std");

const Self = @This();

/// Supported types for code generation (initial version)
pub const Type = union(enum) {
    bool,
    str,
    list: *const Type,
    // Integer types
    u8,
    i8,
    u16,
    i16,
    u32,
    i32,
    u64,
    i64,
    u128,
    i128,
    // Floating point types
    f32,
    f64,
    dec,

    /// Format type as a Roc type annotation string
    pub fn format(self: Type, out: anytype) !void {
        switch (self) {
            .bool => try out.writeAll("Bool"),
            .str => try out.writeAll("Str"),
            .list => |elem_type| {
                try out.writeAll("List ");
                try elem_type.format(out);
            },
            .u8 => try out.writeAll("U8"),
            .i8 => try out.writeAll("I8"),
            .u16 => try out.writeAll("U16"),
            .i16 => try out.writeAll("I16"),
            .u32 => try out.writeAll("U32"),
            .i32 => try out.writeAll("I32"),
            .u64 => try out.writeAll("U64"),
            .i64 => try out.writeAll("I64"),
            .u128 => try out.writeAll("U128"),
            .i128 => try out.writeAll("I128"),
            .f32 => try out.writeAll("F32"),
            .f64 => try out.writeAll("F64"),
            .dec => try out.writeAll("Dec"),
        }
    }

    /// Check if two types are equal
    pub fn eql(self: Type, other: Type) bool {
        const self_tag = std.meta.activeTag(self);
        const other_tag = std.meta.activeTag(other);
        if (self_tag != other_tag) return false;

        if (self == .list) {
            return self.list.eql(other.list.*);
        }
        return true;
    }
};

/// A variable binding in scope
const Variable = struct {
    name: []const u8,
    type_: Type,
};

/// A scope containing variable bindings
const Scope = struct {
    variables: std.ArrayList(Variable),

    fn init() Scope {
        return .{
            .variables = std.ArrayList(Variable).empty,
        };
    }

    fn deinit(self: *Scope, allocator: std.mem.Allocator) void {
        self.variables.deinit(allocator);
    }

    fn addVariable(self: *Scope, allocator: std.mem.Allocator, name: []const u8, type_: Type) !void {
        try self.variables.append(allocator, .{ .name = name, .type_ = type_ });
    }
};

/// The allocator used for intermediate allocations
allocator: std.mem.Allocator,

/// Output buffer where generated code is written
output: std.ArrayList(u8),

/// Stack of scopes for variable tracking
scopes: std.ArrayList(Scope),

/// Random number generator
random: std.Random,

/// Counter for generating unique variable names
var_counter: u32,

/// Current indentation level
indent_level: u32,

/// Pre-allocated type storage for list element types
/// We use a fixed pool to avoid dynamic allocation for nested types
list_elem_types: [8]Type,
list_elem_type_count: u8,

/// Track allocated variable names for cleanup
allocated_var_names: std.ArrayList([]const u8),

/// Initialize a new TypedCodeGenerator
pub fn init(allocator: std.mem.Allocator, seed: u64) Self {
    var pcg = std.Random.Pcg.init(seed);
    return .{
        .allocator = allocator,
        .output = std.ArrayList(u8).empty,
        .scopes = std.ArrayList(Scope).empty,
        .random = pcg.random(),
        .var_counter = 0,
        .indent_level = 0,
        .list_elem_types = undefined,
        .list_elem_type_count = 0,
        .allocated_var_names = std.ArrayList([]const u8).empty,
    };
}

/// Free resources used by the generator
pub fn deinit(self: *Self) void {
    for (self.scopes.items) |*scope| {
        scope.deinit(self.allocator);
    }
    self.scopes.deinit(self.allocator);
    self.output.deinit(self.allocator);
    // Free allocated variable names
    for (self.allocated_var_names.items) |name| {
        self.allocator.free(name);
    }
    self.allocated_var_names.deinit(self.allocator);
}

/// Get the generated output
pub fn getOutput(self: *const Self) []const u8 {
    return self.output.items;
}

/// Reset the generator for reuse
pub fn reset(self: *Self, new_seed: u64) void {
    for (self.scopes.items) |*scope| {
        scope.deinit(self.allocator);
    }
    self.scopes.clearRetainingCapacity();
    self.output.clearRetainingCapacity();
    // Free allocated variable names
    for (self.allocated_var_names.items) |name| {
        self.allocator.free(name);
    }
    self.allocated_var_names.clearRetainingCapacity();
    var pcg = std.Random.Pcg.init(new_seed);
    self.random = pcg.random();
    self.var_counter = 0;
    self.indent_level = 0;
    self.list_elem_type_count = 0;
}

/// Generate a complete Roc module
pub fn generateModule(self: *Self) !void {
    // Push the module-level scope
    try self.pushScope();
    defer self.popScope();

    // Choose a return type for main
    const main_type = self.randomNonListType();

    // Write module header (exposes main function)
    try self.write("module [main]\n\n");

    // Write main with type annotation
    try self.write("main : ");
    try main_type.format(self.writer());
    try self.write("\nmain = {\n");

    self.indent_level = 1;

    // Generate module body as a block-like structure
    const num_bindings = self.random.intRangeAtMost(u8, 1, 4);
    for (0..num_bindings) |_| {
        try self.generateLetBinding();
    }

    // Generate final expression of the main type
    try self.writeIndent();
    try self.generateLiteral(main_type); // Use literal to avoid nested if issues
    try self.write("\n}\n");
}

/// Generate a let binding and add the variable to current scope
fn generateLetBinding(self: *Self) !void {
    const var_name = try self.freshVarName();
    const var_type = self.randomType();

    // Write the type annotation on one line
    try self.writeIndent();
    try self.write(var_name);
    try self.write(" : ");
    try var_type.format(self.writer());
    try self.write("\n");

    // Write the value assignment on the next line
    try self.writeIndent();
    try self.write(var_name);
    try self.write(" = ");
    try self.generateLiteral(var_type); // Use literal to keep it simple
    try self.write("\n");

    // Add to current scope
    try self.currentScope().addVariable(self.allocator, var_name, var_type);
}

/// Generate an expression of the given type
fn generateExpr(self: *Self, type_: Type) std.mem.Allocator.Error!void {
    // Decide what kind of expression to generate
    const choice = self.random.intRangeAtMost(u8, 0, 9);

    // Try to use a variable if one exists with the right type
    if (choice < 3) {
        if (self.findVariableOfType(type_)) |var_name| {
            try self.write(var_name);
            return;
        }
    }

    // Generate if-then-else sometimes
    if (choice == 3) {
        try self.generateIfExpr(type_);
        return;
    }

    // Generate a block sometimes
    if (choice == 4 and self.indent_level < 3) {
        try self.generateBlock(type_);
        return;
    }

    // Generate a literal
    try self.generateLiteral(type_);
}

/// Generate a literal of the given type
fn generateLiteral(self: *Self, type_: Type) !void {
    switch (type_) {
        .bool => {
            if (self.random.boolean()) {
                try self.write("True");
            } else {
                try self.write("False");
            }
        },
        .str => {
            try self.write("\"");
            const len = self.random.intRangeAtMost(u8, 0, 10);
            for (0..len) |_| {
                const char = self.random.intRangeAtMost(u8, 'a', 'z');
                try self.output.append(self.allocator, char);
            }
            try self.write("\"");
        },
        .list => |elem_type| {
            try self.write("[");
            const len = self.random.intRangeAtMost(u8, 0, 3);
            for (0..len) |i| {
                if (i > 0) try self.write(", ");
                try self.generateExpr(elem_type.*);
            }
            try self.write("]");
        },
        .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64 => {
            const val = self.random.intRangeAtMost(i32, 0, 100);
            try self.writer().print("{d}", .{val});
        },
        .u128, .i128 => {
            const val = self.random.intRangeAtMost(i64, 0, 1000);
            try self.writer().print("{d}", .{val});
        },
        .f32, .f64, .dec => {
            const int_part = self.random.intRangeAtMost(i32, 0, 100);
            const frac_part = self.random.intRangeAtMost(u32, 0, 99);
            try self.writer().print("{d}.{d}", .{ int_part, frac_part });
        },
    }
}

/// Generate an if-then-else expression
fn generateIfExpr(self: *Self, type_: Type) !void {
    try self.write("(if ");
    try self.generateLiteral(.bool); // Use literal for condition to avoid nesting issues
    try self.write(" ");
    try self.generateLiteral(type_); // Use literal for then branch
    try self.write(" else ");
    try self.generateLiteral(type_); // Use literal for else branch
    try self.write(")");
}

/// Generate a block expression
fn generateBlock(self: *Self, type_: Type) !void {
    try self.write("{\n");
    self.indent_level += 1;

    try self.pushScope();
    defer self.popScope();

    // Generate some let bindings
    const num_bindings = self.random.intRangeAtMost(u8, 0, 2);
    for (0..num_bindings) |_| {
        try self.generateLetBinding();
    }

    // Generate final expression
    try self.writeIndent();
    try self.generateExpr(type_);
    try self.write("\n");

    self.indent_level -= 1;
    try self.writeIndent();
    try self.write("}");
}

/// Find a variable in scope with the given type
fn findVariableOfType(self: *Self, type_: Type) ?[]const u8 {
    // Collect all matching variables
    var matches: [32][]const u8 = undefined;
    var match_count: usize = 0;

    for (self.scopes.items) |scope| {
        for (scope.variables.items) |v| {
            if (v.type_.eql(type_) and match_count < 32) {
                matches[match_count] = v.name;
                match_count += 1;
            }
        }
    }

    if (match_count == 0) return null;

    // Pick one randomly
    const idx = self.random.intRangeLessThan(usize, 0, match_count);
    return matches[idx];
}

/// Generate a fresh variable name
fn freshVarName(self: *Self) ![]const u8 {
    var buf: [16]u8 = undefined;
    const name = std.fmt.bufPrint(&buf, "var{d}", .{self.var_counter}) catch unreachable;
    self.var_counter += 1;

    // We need to copy the name since buf is on the stack
    const name_copy = try self.allocator.dupe(u8, name);
    // Track for cleanup
    try self.allocated_var_names.append(self.allocator, name_copy);
    return name_copy;
}

/// Choose a random type (including lists)
fn randomType(self: *Self) Type {
    const choice = self.random.intRangeAtMost(u8, 0, 15);
    return switch (choice) {
        0 => .bool,
        1 => .str,
        2 => self.randomListType(),
        3 => .u8,
        4 => .i8,
        5 => .u16,
        6 => .i16,
        7 => .u32,
        8 => .i32,
        9 => .u64,
        10 => .i64,
        11 => .u128,
        12 => .i128,
        13 => .f32,
        14 => .f64,
        15 => .dec,
        else => unreachable,
    };
}

/// Choose a random non-list type (for simpler contexts)
fn randomNonListType(self: *Self) Type {
    const choice = self.random.intRangeAtMost(u8, 0, 14);
    return switch (choice) {
        0 => .bool,
        1 => .str,
        2 => .u8,
        3 => .i8,
        4 => .u16,
        5 => .i16,
        6 => .u32,
        7 => .i32,
        8 => .u64,
        9 => .i64,
        10 => .u128,
        11 => .i128,
        12 => .f32,
        13 => .f64,
        14 => .dec,
        else => unreachable,
    };
}

/// Generate a random list type
fn randomListType(self: *Self) Type {
    // Store the element type in our pre-allocated pool
    if (self.list_elem_type_count < 8) {
        self.list_elem_types[self.list_elem_type_count] = self.randomNonListType();
        const ptr = &self.list_elem_types[self.list_elem_type_count];
        self.list_elem_type_count += 1;
        return .{ .list = ptr };
    }
    // Fallback if pool is exhausted
    return .str;
}

/// Push a new scope
fn pushScope(self: *Self) !void {
    try self.scopes.append(self.allocator, Scope.init());
}

/// Pop the current scope
fn popScope(self: *Self) void {
    if (self.scopes.items.len > 0) {
        var scope = self.scopes.pop().?;
        scope.deinit(self.allocator);
    }
}

/// Get the current (innermost) scope
fn currentScope(self: *Self) *Scope {
    return &self.scopes.items[self.scopes.items.len - 1];
}

/// Write a string to the output
fn write(self: *Self, s: []const u8) !void {
    try self.output.appendSlice(self.allocator, s);
}

/// Write indentation
fn writeIndent(self: *Self) !void {
    for (0..self.indent_level) |_| {
        try self.write("    ");
    }
}

/// Get a writer for the output
fn writer(self: *Self) std.ArrayList(u8).Writer {
    return self.output.writer(self.allocator);
}

// Tests
test "generate simple module" {
    const allocator = std.testing.allocator;
    var gen = Self.init(allocator, 12345);
    defer gen.deinit();

    try gen.generateModule();
    const output = gen.getOutput();

    // Should start with main declaration
    try std.testing.expect(std.mem.startsWith(u8, output, "main : "));
    // Should contain main =
    try std.testing.expect(std.mem.indexOf(u8, output, "main =") != null);
}

test "type formatting" {
    var buf: [64]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);

    const bool_type = Type.bool;
    try bool_type.format(fbs.writer());
    try std.testing.expectEqualStrings("Bool", fbs.getWritten());

    fbs.reset();
    const str_type = Type.str;
    try str_type.format(fbs.writer());
    try std.testing.expectEqualStrings("Str", fbs.getWritten());

    fbs.reset();
    const i32_type = Type.i32;
    try i32_type.format(fbs.writer());
    try std.testing.expectEqualStrings("I32", fbs.getWritten());
}
