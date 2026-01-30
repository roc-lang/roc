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
    tuple: TupleType,
    record: RecordType,
    func: FuncType,
    tag_union: TagUnionType,
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

    /// Tuple type with up to 4 elements
    pub const TupleType = struct {
        elem_types: [4]*const Type,
        len: u8,
    };

    /// Record type with named fields
    pub const RecordType = struct {
        fields: [6]RecordField,
        len: u8,
    };

    pub const RecordField = struct {
        name: []const u8,
        type_: *const Type,
    };

    /// Function type with arguments and return type
    pub const FuncType = struct {
        arg_types: [4]*const Type,
        arg_count: u8,
        return_type: *const Type,
    };

    /// Tag union type with variants
    pub const TagUnionType = struct {
        variants: [4]TagVariant,
        len: u8,
    };

    pub const TagVariant = struct {
        name: []const u8,
        payload: ?*const Type,
    };

    /// Format type as a Roc type annotation string
    pub fn format(self: Type, out: anytype) !void {
        switch (self) {
            .bool => try out.writeAll("Bool"),
            .str => try out.writeAll("Str"),
            .list => |elem_type| {
                try out.writeAll("List ");
                try elem_type.format(out);
            },
            .tuple => |t| {
                try out.writeAll("(");
                for (0..t.len) |i| {
                    if (i > 0) try out.writeAll(", ");
                    try t.elem_types[i].format(out);
                }
                try out.writeAll(")");
            },
            .record => |r| {
                try out.writeAll("{ ");
                for (0..r.len) |i| {
                    if (i > 0) try out.writeAll(", ");
                    try out.writeAll(r.fields[i].name);
                    try out.writeAll(" : ");
                    try r.fields[i].type_.format(out);
                }
                try out.writeAll(" }");
            },
            .func => |f| {
                // Roc function type syntax: `ArgType -> ReturnType` or `() -> ReturnType`
                if (f.arg_count == 0) {
                    try out.writeAll("(() -> ");
                } else {
                    try out.writeAll("(");
                    for (0..f.arg_count) |i| {
                        if (i > 0) try out.writeAll(", ");
                        try f.arg_types[i].format(out);
                    }
                    try out.writeAll(" -> ");
                }
                try f.return_type.format(out);
                try out.writeAll(")");
            },
            .tag_union => |t| {
                try out.writeAll("[");
                for (0..t.len) |i| {
                    if (i > 0) try out.writeAll(", ");
                    try out.writeAll(t.variants[i].name);
                    if (t.variants[i].payload) |payload| {
                        // Modern Roc syntax: TagName(PayloadType)
                        try out.writeAll("(");
                        try payload.format(out);
                        try out.writeAll(")");
                    }
                }
                try out.writeAll("]");
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

        switch (self) {
            .list => return self.list.eql(other.list.*),
            .tuple => |t| {
                const ot = other.tuple;
                if (t.len != ot.len) return false;
                for (0..t.len) |i| {
                    if (!t.elem_types[i].eql(ot.elem_types[i].*)) return false;
                }
                return true;
            },
            .record => |r| {
                const or_ = other.record;
                if (r.len != or_.len) return false;
                for (0..r.len) |i| {
                    if (!std.mem.eql(u8, r.fields[i].name, or_.fields[i].name)) return false;
                    if (!r.fields[i].type_.eql(or_.fields[i].type_.*)) return false;
                }
                return true;
            },
            .func => |f| {
                const of = other.func;
                if (f.arg_count != of.arg_count) return false;
                for (0..f.arg_count) |i| {
                    if (!f.arg_types[i].eql(of.arg_types[i].*)) return false;
                }
                return f.return_type.eql(of.return_type.*);
            },
            .tag_union => |t| {
                const ot = other.tag_union;
                if (t.len != ot.len) return false;
                for (0..t.len) |i| {
                    if (!std.mem.eql(u8, t.variants[i].name, ot.variants[i].name)) return false;
                    const p1 = t.variants[i].payload;
                    const p2 = ot.variants[i].payload;
                    if ((p1 == null) != (p2 == null)) return false;
                    if (p1 != null and !p1.?.eql(p2.?.*)) return false;
                }
                return true;
            },
            else => return true,
        }
    }

    /// Check if this is a numeric type
    pub fn isNumeric(self: Type) bool {
        return switch (self) {
            .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128, .f32, .f64, .dec => true,
            else => false,
        };
    }

    /// Check if this is an integer type
    pub fn isInteger(self: Type) bool {
        return switch (self) {
            .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 => true,
            else => false,
        };
    }

    /// Check if this is a floating point type
    pub fn isFloat(self: Type) bool {
        return switch (self) {
            .f32, .f64, .dec => true,
            else => false,
        };
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

/// Random number generator state (must be stored, not just the interface)
prng: std.Random.Pcg,

/// Counter for generating unique variable names
var_counter: u32,

/// Current indentation level
indent_level: u32,

/// Pre-allocated type storage for nested types (list elements, tuple elements, etc.)
/// We use a fixed pool to avoid dynamic allocation for nested types
type_pool: [128]Type,
type_pool_count: u8,

/// Track allocated variable names for cleanup
allocated_var_names: std.ArrayList([]const u8),

/// Field name pool for records
const field_names = [_][]const u8{ "x", "y", "name", "value", "count", "result" };

/// Initialize a new TypedCodeGenerator
pub fn init(allocator: std.mem.Allocator, seed: u64) Self {
    return .{
        .allocator = allocator,
        .output = std.ArrayList(u8).empty,
        .scopes = std.ArrayList(Scope).empty,
        .prng = std.Random.Pcg.init(seed),
        .var_counter = 0,
        .indent_level = 0,
        .type_pool = undefined,
        .type_pool_count = 0,
        .allocated_var_names = std.ArrayList([]const u8).empty,
    };
}

/// Get the random interface from the stored PRNG state
fn random(self: *Self) std.Random {
    return self.prng.random();
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
    self.prng = std.Random.Pcg.init(new_seed);
    self.var_counter = 0;
    self.indent_level = 0;
    self.type_pool_count = 0;
}

/// Generate a complete Roc type module
/// Format: TypeName := TypeDef.{ associated functions }
pub fn generateModule(self: *Self) !void {
    // Push the module-level scope
    try self.pushScope();
    defer self.popScope();

    // Generate a Type module - the type name should match the file
    // Nominal types can only wrap tag unions or records (not primitives)

    // Choose what kind of type to define: tag union or record
    const type_choice = self.random().intRangeAtMost(u8, 0, 3);
    switch (type_choice) {
        0 => {
            // Result-like tag union: [Ok(T), Err(E)]
            try self.write("Main := [Ok(");
            const ok_type = self.randomPrimitiveType();
            try ok_type.format(self.writer());
            try self.write("), Err(Str)]");
        },
        1 => {
            // Maybe-like tag union: [Some(T), None]
            try self.write("Main := [Some(");
            const some_type = self.randomPrimitiveType();
            try some_type.format(self.writer());
            try self.write("), None]");
        },
        2 => {
            // Simple tag union without payloads: [A, B, C]
            try self.write("Main := [Red, Green, Blue]");
        },
        3 => {
            // Record type
            try self.write("Main := { value : ");
            const val_type = self.randomPrimitiveType();
            try val_type.format(self.writer());
            try self.write(", count : U64 }");
        },
        else => unreachable,
    }

    try self.write(".{\n");
    self.indent_level = 1;

    // Generate some associated functions with various return types
    const num_funcs = self.random().intRangeAtMost(u8, 2, 5);
    for (0..num_funcs) |_| {
        try self.generateAssociatedFunction();
    }

    try self.write("}\n");
}

/// Generate an associated function for a type module
fn generateAssociatedFunction(self: *Self) !void {
    const func_name = try self.freshVarName();
    const return_type = self.randomTypeForReturn();

    // Randomly decide function style
    const style = self.random().intRangeAtMost(u8, 0, 4);

    switch (style) {
        0 => {
            // Simple: Main -> T with |_| literal
            try self.writeIndent();
            try self.write(func_name);
            try self.write(" : Main -> ");
            try return_type.format(self.writer());
            try self.write("\n");
            try self.writeIndent();
            try self.write(func_name);
            try self.write(" = |_| ");
            try self.generateSimpleLiteral(return_type);
            try self.write("\n");
        },
        1 => {
            // Multi-arg: Main, ExtraArg -> T
            const extra_type = self.randomPrimitiveType();
            try self.writeIndent();
            try self.write(func_name);
            try self.write(" : Main, ");
            try extra_type.format(self.writer());
            try self.write(" -> ");
            try return_type.format(self.writer());
            try self.write("\n");
            try self.writeIndent();
            try self.write(func_name);
            try self.write(" = |_, ");
            const arg_name = try self.freshVarName();
            try self.write(arg_name);
            try self.write("| ");
            // Sometimes use the arg, sometimes not
            if (extra_type.eql(return_type) and self.random().boolean()) {
                try self.write(arg_name);
            } else {
                try self.generateSimpleLiteral(return_type);
            }
            try self.write("\n");
        },
        2 => {
            // No type annotation (rely on inference)
            try self.writeIndent();
            try self.write(func_name);
            try self.write(" = |_| ");
            try self.generateSimpleLiteral(return_type);
            try self.write("\n");
        },
        3 => {
            // Block body with let bindings
            try self.writeIndent();
            try self.write(func_name);
            try self.write(" : Main -> ");
            try return_type.format(self.writer());
            try self.write("\n");
            try self.writeIndent();
            try self.write(func_name);
            try self.write(" = |m| {\n");
            self.indent_level += 1;

            // Push scope for the block
            try self.pushScope();
            // Add 'm' to scope as Main type
            try self.currentScope().addVariable(self.allocator, "m", .{ .tag_union = .{
                .variants = .{ .{ .name = "Main", .payload = null }, undefined, undefined, undefined },
                .len = 1,
            } });

            // Generate 1-2 let bindings
            const num_bindings = self.random().intRangeAtMost(u8, 1, 2);
            for (0..num_bindings) |_| {
                try self.generateLetBindingSimple();
            }

            // Final expression
            try self.writeIndent();
            try self.generateSimpleLiteral(return_type);
            try self.write("\n");

            self.popScope();
            self.indent_level -= 1;
            try self.writeIndent();
            try self.write("}\n");
        },
        4 => {
            // Use Main arg with match expression (if Main is a tag union we know)
            try self.writeIndent();
            try self.write(func_name);
            try self.write(" : Main -> ");
            try return_type.format(self.writer());
            try self.write("\n");
            try self.writeIndent();
            try self.write(func_name);
            try self.write(" = |_| ");
            try self.generateSimpleLiteral(return_type);
            try self.write("\n");
        },
        else => unreachable,
    }
}

/// Generate a simple let binding for inside a block
/// In Roc, local let bindings are just `name = value` (no inline type annotation)
fn generateLetBindingSimple(self: *Self) !void {
    const var_name = try self.freshVarName();
    const var_type = self.randomPrimitiveType();

    try self.writeIndent();
    try self.write(var_name);
    try self.write(" = ");
    try self.generateSimpleLiteral(var_type);
    try self.write("\n");

    try self.currentScope().addVariable(self.allocator, var_name, var_type);
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
    const choice = self.random().intRangeAtMost(u8, 0, 14);

    // Try to use a variable if one exists with the right type
    if (choice < 3) {
        if (self.findVariableOfType(type_)) |var_name| {
            // For tuples, sometimes generate element access
            if (type_ == .tuple and self.random().boolean()) {
                const t = type_.tuple;
                const elem_idx = self.random().intRangeLessThan(usize, 0, t.len);
                try self.generateTupleAccess(var_name, t, elem_idx);
                // Note: This returns a different type, so we need to be careful
                // For simplicity, just return the variable itself
                try self.write(var_name);
                return;
            }
            // For records, sometimes generate field access
            if (type_ == .record and self.random().boolean()) {
                const r = type_.record;
                const field_idx = self.random().intRangeLessThan(usize, 0, r.len);
                try self.generateRecordAccess(var_name, r.fields[field_idx].name);
                // Same note as above
                try self.write(var_name);
                return;
            }
            // For functions, sometimes generate a function call
            if (type_ == .func and self.random().boolean()) {
                try self.generateFunctionCall(var_name, type_.func);
                return;
            }
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

    // Generate binary operators for numeric/bool types
    if (choice == 5 and (type_.isNumeric() or type_ == .bool)) {
        try self.generateBinaryOp(type_);
        return;
    }

    // Generate comparison for bool type
    if (choice == 6 and type_ == .bool) {
        try self.generateComparisonOp();
        return;
    }

    // Generate when expression for tag unions
    if (choice == 7) {
        // Find a tag union variable to match on
        for (self.scopes.items) |scope| {
            for (scope.variables.items) |v| {
                if (v.type_ == .tag_union) {
                    try self.generateMatchExpr(v.type_.tag_union, type_);
                    return;
                }
            }
        }
    }

    // Generate tuple element access if we have a tuple variable and want its element type
    if (choice == 8) {
        for (self.scopes.items) |scope| {
            for (scope.variables.items) |v| {
                if (v.type_ == .tuple) {
                    const t = v.type_.tuple;
                    for (0..t.len) |elem_idx| {
                        if (t.elem_types[elem_idx].eql(type_)) {
                            try self.generateTupleAccess(v.name, t, elem_idx);
                            return;
                        }
                    }
                }
            }
        }
    }

    // Generate record field access if we have a record variable with matching field type
    if (choice == 9) {
        for (self.scopes.items) |scope| {
            for (scope.variables.items) |v| {
                if (v.type_ == .record) {
                    const r = v.type_.record;
                    for (0..r.len) |field_idx| {
                        if (r.fields[field_idx].type_.eql(type_)) {
                            try self.generateRecordAccess(v.name, r.fields[field_idx].name);
                            return;
                        }
                    }
                }
            }
        }
    }

    // Generate a literal
    try self.generateLiteral(type_);
}

/// Generate a literal of the given type
fn generateLiteral(self: *Self, type_: Type) !void {
    switch (type_) {
        .bool => {
            if (self.random().boolean()) {
                try self.write("True");
            } else {
                try self.write("False");
            }
        },
        .str => {
            try self.write("\"");
            const len = self.random().intRangeAtMost(u8, 0, 10);
            for (0..len) |_| {
                const char = self.random().intRangeAtMost(u8, 'a', 'z');
                try self.output.append(self.allocator, char);
            }
            try self.write("\"");
        },
        .list => |elem_type| {
            try self.write("[");
            const len = self.random().intRangeAtMost(u8, 0, 3);
            for (0..len) |i| {
                if (i > 0) try self.write(", ");
                // Use simple literals to avoid generating blocks inside lists
                try self.generateSimpleLiteral(elem_type.*);
            }
            try self.write("]");
        },
        .tuple => |t| {
            try self.write("(");
            for (0..t.len) |i| {
                if (i > 0) try self.write(", ");
                // Use simple literals to avoid generating blocks inside tuples
                try self.generateSimpleLiteral(t.elem_types[i].*);
            }
            try self.write(")");
        },
        .record => |r| {
            try self.write("{ ");
            for (0..r.len) |i| {
                if (i > 0) try self.write(", ");
                try self.write(r.fields[i].name);
                try self.write(": ");
                // Use simple literals to avoid generating blocks inside records
                try self.generateSimpleLiteral(r.fields[i].type_.*);
            }
            try self.write(" }");
        },
        .func => |f| {
            try self.generateLambda(f);
        },
        .tag_union => |t| {
            try self.generateTagConstruction(t);
        },
        // Integer types - sometimes use suffix, sometimes rely on type annotation
        .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 => {
            const val = self.random().intRangeAtMost(i32, 0, 100);
            const use_suffix = self.random().boolean();
            if (use_suffix) {
                const suffix = switch (type_) {
                    .u8 => ".U8",
                    .i8 => ".I8",
                    .u16 => ".U16",
                    .i16 => ".I16",
                    .u32 => ".U32",
                    .i32 => ".I32",
                    .u64 => ".U64",
                    .i64 => ".I64",
                    .u128 => ".U128",
                    .i128 => ".I128",
                    else => unreachable,
                };
                try self.writer().print("{d}{s}", .{ val, suffix });
            } else {
                try self.writer().print("{d}", .{val});
            }
        },
        // Float types - sometimes use suffix, sometimes rely on inference
        .f32, .f64 => {
            const val = self.random().intRangeAtMost(i32, 0, 100);
            const use_suffix = self.random().boolean();
            if (use_suffix) {
                const suffix = if (type_ == .f32) ".F32" else ".F64";
                try self.writer().print("{d}{s}", .{ val, suffix });
            } else {
                // Plain integer, relies on type annotation for inference
                try self.writer().print("{d}", .{val});
            }
        },
        .dec => {
            // Dec - use decimal literal, optionally with suffix
            const int_part = self.random().intRangeAtMost(i32, 0, 100);
            const frac_part = self.random().intRangeAtMost(u32, 0, 99);
            const use_suffix = self.random().boolean();
            if (use_suffix) {
                try self.writer().print("{d}.{d}.Dec", .{ int_part, frac_part });
            } else {
                // Plain decimal literal (inferred as Dec)
                try self.writer().print("{d}.{d}", .{ int_part, frac_part });
            }
        },
    }
}

/// Generate a simple literal (no recursion - used for lambda bodies)
fn generateSimpleLiteral(self: *Self, type_: Type) !void {
    switch (type_) {
        .bool => {
            if (self.random().boolean()) {
                try self.write("True");
            } else {
                try self.write("False");
            }
        },
        .str => {
            try self.write("\"");
            const len = self.random().intRangeAtMost(u8, 0, 5);
            for (0..len) |_| {
                const char = self.random().intRangeAtMost(u8, 'a', 'z');
                try self.output.append(self.allocator, char);
            }
            try self.write("\"");
        },
        // Integer types - randomly use suffix or rely on type inference
        .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 => {
            const val = self.random().intRangeAtMost(i32, 0, 100);
            const use_suffix = self.random().boolean();
            if (use_suffix) {
                const suffix = switch (type_) {
                    .u8 => ".U8",
                    .i8 => ".I8",
                    .u16 => ".U16",
                    .i16 => ".I16",
                    .u32 => ".U32",
                    .i32 => ".I32",
                    .u64 => ".U64",
                    .i64 => ".I64",
                    .u128 => ".U128",
                    .i128 => ".I128",
                    else => unreachable,
                };
                try self.writer().print("{d}{s}", .{ val, suffix });
            } else {
                try self.writer().print("{d}", .{val});
            }
        },
        .f32, .f64 => {
            const val = self.random().intRangeAtMost(i32, 0, 100);
            const use_suffix = self.random().boolean();
            if (use_suffix) {
                const suffix = if (type_ == .f32) ".F32" else ".F64";
                try self.writer().print("{d}{s}", .{ val, suffix });
            } else {
                try self.writer().print("{d}", .{val});
            }
        },
        .dec => {
            const int_part = self.random().intRangeAtMost(i32, 0, 100);
            const frac_part = self.random().intRangeAtMost(u32, 0, 99);
            const use_suffix = self.random().boolean();
            if (use_suffix) {
                try self.writer().print("{d}.{d}.Dec", .{ int_part, frac_part });
            } else {
                try self.writer().print("{d}.{d}", .{ int_part, frac_part });
            }
        },
        // For compound types, generate a simple placeholder
        .list => try self.write("[]"),
        .tuple => |t| {
            try self.write("(");
            for (0..t.len) |i| {
                if (i > 0) try self.write(", ");
                try self.generateSimpleLiteral(t.elem_types[i].*);
            }
            try self.write(")");
        },
        .record => |r| {
            try self.write("{ ");
            for (0..r.len) |i| {
                if (i > 0) try self.write(", ");
                try self.write(r.fields[i].name);
                try self.write(": ");
                try self.generateSimpleLiteral(r.fields[i].type_.*);
            }
            try self.write(" }");
        },
        .func => {
            // Just generate a simple lambda that returns a constant
            try self.write("|| True");
        },
        .tag_union => |t| {
            // Pick first variant without payload, or first one
            for (t.variants[0..t.len]) |variant| {
                if (variant.payload == null) {
                    try self.write(variant.name);
                    return;
                }
            }
            // Fallback: use first variant with simple payload
            const v = t.variants[0];
            try self.write(v.name);
            if (v.payload) |p| {
                // Modern Roc syntax: TagName(payload)
                try self.write("(");
                try self.generateSimpleLiteral(p.*);
                try self.write(")");
            }
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
    const num_bindings = self.random().intRangeAtMost(u8, 0, 2);
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

/// Generate a lambda expression
/// Roc lambda syntax: |arg1, arg2| body  or  || body (zero args)
fn generateLambda(self: *Self, f: Type.FuncType) !void {
    try self.write("|");

    // Push a scope for lambda parameters
    try self.pushScope();
    defer self.popScope();

    // Generate parameter names (empty for zero args)
    if (f.arg_count > 0) {
        var param_names: [4][]const u8 = undefined;
        for (0..f.arg_count) |i| {
            if (i > 0) try self.write(", ");
            const param_name = try self.freshVarName();
            param_names[i] = param_name;
            try self.write(param_name);
            // Add parameter to scope
            try self.currentScope().addVariable(self.allocator, param_name, f.arg_types[i].*);
        }
    }

    try self.write("| ");

    // Generate the body - try to use a parameter if possible
    const return_type = f.return_type.*;
    if (self.findVariableOfType(return_type)) |var_name| {
        try self.write(var_name);
    } else {
        // Use simple literal to avoid deep recursion
        try self.generateSimpleLiteral(return_type);
    }
}

/// Generate a function call expression
fn generateFunctionCall(self: *Self, func_var: []const u8, f: Type.FuncType) !void {
    try self.write("(");
    try self.write(func_var);
    for (0..f.arg_count) |i| {
        try self.write(" ");
        try self.generateLiteral(f.arg_types[i].*);
    }
    try self.write(")");
}

/// Generate a tag construction expression
/// Modern Roc syntax: TagName or TagName(payload)
fn generateTagConstruction(self: *Self, t: Type.TagUnionType) !void {
    // Pick a random variant
    const variant_idx = self.random().intRangeLessThan(usize, 0, t.len);
    const variant = t.variants[variant_idx];

    try self.write(variant.name);
    if (variant.payload) |payload_type| {
        // Modern Roc syntax: TagName(payload)
        try self.write("(");
        try self.generateSimpleLiteral(payload_type.*);
        try self.write(")");
    }
}

/// Generate a match expression for pattern matching on a tag union
/// Roc syntax: match expr { pattern => result, ... }
fn generateMatchExpr(self: *Self, tag_type: Type.TagUnionType, result_type: Type) !void {
    try self.write("(match ");

    // Generate the scrutinee - either a variable or a literal
    const scrutinee_type = Type{ .tag_union = tag_type };
    if (self.findVariableOfType(scrutinee_type)) |var_name| {
        try self.write(var_name);
    } else {
        try self.generateTagConstruction(tag_type);
    }

    try self.write(" {\n");
    self.indent_level += 1;

    // Generate a branch for each variant
    for (0..tag_type.len) |i| {
        const variant = tag_type.variants[i];
        try self.writeIndent();
        try self.write(variant.name);

        if (variant.payload != null) {
            // Bind the payload to a variable
            try self.write("(payload");
            try self.write(@as([]const u8, &[_]u8{'0' + @as(u8, @intCast(i))}));
            try self.write(")");
        }

        try self.write(" => ");
        try self.generateSimpleLiteral(result_type);
        try self.write("\n");
    }

    self.indent_level -= 1;
    try self.writeIndent();
    try self.write("})");
}

/// Generate a binary operator expression
fn generateBinaryOp(self: *Self, type_: Type) !void {
    if (type_.isNumeric()) {
        // Arithmetic operator
        const ops = [_][]const u8{ "+", "-", "*" };
        const op = ops[self.random().intRangeLessThan(usize, 0, ops.len)];
        try self.write("(");
        try self.generateLiteral(type_);
        try self.write(" ");
        try self.write(op);
        try self.write(" ");
        try self.generateLiteral(type_);
        try self.write(")");
    } else if (type_ == .bool) {
        // Boolean operator
        const choice = self.random().intRangeAtMost(u8, 0, 2);
        if (choice == 0) {
            // Unary not
            try self.write("!");
            try self.generateLiteral(.bool);
        } else if (choice == 1) {
            // &&
            try self.write("(");
            try self.generateLiteral(.bool);
            try self.write(" && ");
            try self.generateLiteral(.bool);
            try self.write(")");
        } else {
            // ||
            try self.write("(");
            try self.generateLiteral(.bool);
            try self.write(" || ");
            try self.generateLiteral(.bool);
            try self.write(")");
        }
    } else {
        // Fallback to literal
        try self.generateLiteral(type_);
    }
}

/// Generate a comparison expression (returns Bool)
fn generateComparisonOp(self: *Self) !void {
    const ops = [_][]const u8{ "==", "!=", ">", "<", ">=", "<=" };
    const op = ops[self.random().intRangeLessThan(usize, 0, ops.len)];

    // Use a numeric type for comparison
    const cmp_type = self.randomPrimitiveType();

    try self.write("(");
    try self.generateLiteral(cmp_type);
    try self.write(" ");
    try self.write(op);
    try self.write(" ");
    try self.generateLiteral(cmp_type);
    try self.write(")");
}

/// Generate tuple element access
fn generateTupleAccess(self: *Self, tuple_var: []const u8, _: Type.TupleType, elem_idx: usize) !void {
    try self.write(tuple_var);
    try self.write(".");
    try self.writer().print("{d}", .{elem_idx});
}

/// Generate record field access
fn generateRecordAccess(self: *Self, record_var: []const u8, field_name: []const u8) !void {
    try self.write(record_var);
    try self.write(".");
    try self.write(field_name);
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
    const idx = self.random().intRangeLessThan(usize, 0, match_count);
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

/// Choose a random type (including compound types)
fn randomType(self: *Self) Type {
    const choice = self.random().intRangeAtMost(u8, 0, 19);
    return switch (choice) {
        0 => .bool,
        1 => .str,
        2 => self.randomListType(),
        3 => self.randomTupleType(),
        4 => self.randomRecordType(),
        5 => self.randomTagUnionType(),
        6 => .u8,
        7 => .i8,
        8 => .u16,
        9 => .i16,
        10 => .u32,
        11 => .i32,
        12 => .u64,
        13 => .i64,
        14 => .u128,
        15 => .i128,
        16 => .f32,
        17 => .f64,
        18 => .dec,
        19 => self.randomFuncType(),
        else => unreachable,
    };
}

/// Choose a random non-compound type (for simpler contexts)
fn randomNonListType(self: *Self) Type {
    const choice = self.random().intRangeAtMost(u8, 0, 14);
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

/// Choose a random primitive type (no containers)
fn randomPrimitiveType(self: *Self) Type {
    return self.randomNonListType();
}

/// Choose a random type suitable for function return values
/// Includes primitives, tuples, records, and tag unions (but not functions or lists)
fn randomTypeForReturn(self: *Self) Type {
    const choice = self.random().intRangeAtMost(u8, 0, 17);
    return switch (choice) {
        0 => .bool,
        1 => .str,
        2 => self.randomTupleType(),
        3 => self.randomRecordType(),
        4 => self.randomTagUnionType(),
        5 => .u8,
        6 => .i8,
        7 => .u16,
        8 => .i16,
        9 => .u32,
        10 => .i32,
        11 => .u64,
        12 => .i64,
        13 => .u128,
        14 => .i128,
        15 => .f32,
        16 => .f64,
        17 => .dec,
        else => unreachable,
    };
}

/// Allocate a type in the type pool
fn allocType(self: *Self, t: Type) ?*const Type {
    if (self.type_pool_count < 128) {
        self.type_pool[self.type_pool_count] = t;
        const ptr = &self.type_pool[self.type_pool_count];
        self.type_pool_count += 1;
        return ptr;
    }
    return null;
}

/// Generate a random list type
fn randomListType(self: *Self) Type {
    const elem_type = self.randomPrimitiveType();
    if (self.allocType(elem_type)) |ptr| {
        return .{ .list = ptr };
    }
    // Fallback if pool is exhausted
    return .str;
}

/// Generate a random tuple type (2-4 elements)
fn randomTupleType(self: *Self) Type {
    const len = self.random().intRangeAtMost(u8, 2, 4);
    var tuple: Type.TupleType = .{
        .elem_types = undefined,
        .len = len,
    };

    for (0..len) |i| {
        const elem_type = self.randomPrimitiveType();
        if (self.allocType(elem_type)) |ptr| {
            tuple.elem_types[i] = ptr;
        } else {
            // Pool exhausted, return a simple type
            return .str;
        }
    }

    return .{ .tuple = tuple };
}

/// Generate a random record type (1-4 fields)
fn randomRecordType(self: *Self) Type {
    const len = self.random().intRangeAtMost(u8, 1, 4);
    var record: Type.RecordType = .{
        .fields = undefined,
        .len = len,
    };

    // Use distinct field names
    var used_indices: [6]bool = .{ false, false, false, false, false, false };
    for (0..len) |i| {
        // Find an unused field name
        var idx = self.random().intRangeAtMost(usize, 0, field_names.len - 1);
        var attempts: u8 = 0;
        while (used_indices[idx] and attempts < 10) {
            idx = (idx + 1) % field_names.len;
            attempts += 1;
        }
        used_indices[idx] = true;

        const field_type = self.randomPrimitiveType();
        if (self.allocType(field_type)) |ptr| {
            record.fields[i] = .{
                .name = field_names[idx],
                .type_ = ptr,
            };
        } else {
            // Pool exhausted, return a simple type
            return .str;
        }
    }

    return .{ .record = record };
}

/// Generate a random function type (0-3 args)
fn randomFuncType(self: *Self) Type {
    const arg_count = self.random().intRangeAtMost(u8, 0, 3);
    var func: Type.FuncType = .{
        .arg_types = undefined,
        .arg_count = arg_count,
        .return_type = undefined,
    };

    for (0..arg_count) |i| {
        const arg_type = self.randomPrimitiveType();
        if (self.allocType(arg_type)) |ptr| {
            func.arg_types[i] = ptr;
        } else {
            return .str;
        }
    }

    const return_type = self.randomPrimitiveType();
    if (self.allocType(return_type)) |ptr| {
        func.return_type = ptr;
    } else {
        return .str;
    }

    return .{ .func = func };
}

/// Generate a random tag union type (Maybe or Result style)
fn randomTagUnionType(self: *Self) Type {
    const choice = self.random().intRangeAtMost(u8, 0, 2);
    return switch (choice) {
        0 => self.maybeLikeTagUnion(),
        1 => self.resultLikeTagUnion(),
        2 => self.simpleTwoTagUnion(),
        else => unreachable,
    };
}

/// Generate a Maybe-like tag union: [Some payload, None]
fn maybeLikeTagUnion(self: *Self) Type {
    const payload_type = self.randomPrimitiveType();
    if (self.allocType(payload_type)) |ptr| {
        return .{ .tag_union = .{
            .variants = .{
                .{ .name = "Some", .payload = ptr },
                .{ .name = "None", .payload = null },
                undefined,
                undefined,
            },
            .len = 2,
        } };
    }
    return .str;
}

/// Generate a Result-like tag union: [Ok ok_type, Err err_type]
fn resultLikeTagUnion(self: *Self) Type {
    const ok_type = self.randomPrimitiveType();
    const err_type = self.randomPrimitiveType();
    const ok_ptr = self.allocType(ok_type);
    const err_ptr = self.allocType(err_type);
    if (ok_ptr != null and err_ptr != null) {
        return .{ .tag_union = .{
            .variants = .{
                .{ .name = "Ok", .payload = ok_ptr },
                .{ .name = "Err", .payload = err_ptr },
                undefined,
                undefined,
            },
            .len = 2,
        } };
    }
    return .str;
}

/// Generate a simple two-tag union without payloads: [On, Off]
fn simpleTwoTagUnion(_: *Self) Type {
    return .{ .tag_union = .{
        .variants = .{
            .{ .name = "On", .payload = null },
            .{ .name = "Off", .payload = null },
            undefined,
            undefined,
        },
        .len = 2,
    } };
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
