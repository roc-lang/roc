//! Diagnostics for user-caused or compiler-caused issues during compilation.
//!
//! Wherever possible, we want to emit diagnostics and continue either compilation
//! with a malformed IR node that wraps said diagnostics or continue running the
//! executable/interpreter until the diagnostic is encountered. This lets us maximize
//! our commitment to "always inform, never block" and to boost development speed.

const std = @import("std");
const Allocator = std.mem.Allocator;
const base = @import("base.zig");
const collections = @import("collections.zig");

const Ident = base.Ident;
const Region = base.Region;

/// Represents a problem encountered during the compilation process.
pub const Problem = union(enum) {
    tokenize: @import("check/parse/tokenize.zig").Diagnostic,
    parser: @import("check/parse/IR.zig").Diagnostic,
    canonicalize: Canonicalize,
    compiler: Compiler,

    /// User errors preventing a module from being canonicalized correctly,
    /// e.g. a variable that was used but not defined.
    pub const Canonicalize = union(enum) {
        NotYetImplemented,
        DuplicateImport: struct {
            duplicate_import_region: Region,
        },
        DuplicateExposes: struct {
            first_exposes: Ident.Idx,
            duplicate_exposes: Ident.Idx,
        },
        AliasNotInScope: struct {
            name: Ident.Idx,
            suggestions: collections.SafeList(Ident.Idx).Range,
        },
        IdentNotInScope: struct {
            ident: Ident.Idx,
            suggestions: collections.SafeList(Ident.Idx).Range,
        },
        AliasAlreadyInScope: struct {
            original_name: Ident.Idx,
            shadow: Ident.Idx,
        },
        IdentAlreadyInScope: struct {
            original_ident: Ident.Idx,
            shadow: Ident.Idx,
        },
        InvalidTopLevelStatement: struct {
            ty: StatementType,
            region: Region,

            const StatementType = enum(u8) { @"var", expr, @"for", crash, @"return" };
        },
        InvalidNumLiteral: struct {
            region: Region,
            literal: []const u8,
        },

        /// Make a `Problem` based on a canonicalization problem.
        pub fn make(can_problem: @This()) Problem {
            return Problem{ .canonicalize = can_problem };
        }

        pub fn toStr(self: @This(), gpa: Allocator, writer: anytype) !void {
            _ = gpa;
            // use a stack allocation for printing our tag errors
            var buf: [1000]u8 = undefined;

            switch (self) {
                .NotYetImplemented => {
                    const err_msg = try std.fmt.bufPrint(&buf, "CAN: Not yet implemented", .{});
                    try writer.writeAll(err_msg);
                },
                .DuplicateImport => |e| {
                    _ = e; // TODO: Use this capture in a meaningful way (make sure to update Canonicalize tests)
                    const err_msg = try std.fmt.bufPrint(&buf, "CAN: Duplicate Import", .{});
                    try writer.writeAll(err_msg);
                },
                .DuplicateExposes => |e| {
                    _ = e; // TODO: Use this capture in a meaningful way (make sure to update Canonicalize tests)
                    const err_msg = try std.fmt.bufPrint(&buf, "CAN: Duplicate Exposes", .{});
                    try writer.writeAll(err_msg);
                },
                .AliasNotInScope => |e| {
                    _ = e; // TODO: Use this capture in a meaningful way (make sure to update Canonicalize tests)
                    const err_msg = try std.fmt.bufPrint(&buf, "CAN: Alias not in scope", .{});
                    try writer.writeAll(err_msg);
                },
                .IdentNotInScope => |e| {
                    _ = e; // TODO: Use this capture in a meaningful way (make sure to update Canonicalize tests)
                    const err_msg = try std.fmt.bufPrint(&buf, "CAN: Ident not in scope", .{});
                    try writer.writeAll(err_msg);
                },
                .AliasAlreadyInScope => |e| {
                    _ = e; // TODO: Use this capture in a meaningful way (make sure to update Canonicalize tests)
                    const err_msg = try std.fmt.bufPrint(&buf, "CAN: Alias already in scope", .{});
                    try writer.writeAll(err_msg);
                },
                .IdentAlreadyInScope => |e| {
                    _ = e; // TODO: Use this capture in a meaningful way (make sure to update Canonicalize tests)
                    const err_msg = try std.fmt.bufPrint(&buf, "CAN: Ident already in scope", .{});
                    try writer.writeAll(err_msg);
                },
                .InvalidTopLevelStatement => |e| {
                    _ = e; // TODO: Use this capture in a meaningful way (make sure to update Canonicalize tests)
                    const err_msg = try std.fmt.bufPrint(&buf, "CAN: Invalid top level statement", .{});
                    try writer.writeAll(err_msg);
                },
                .InvalidNumLiteral => |e| {
                    const err_msg = try std.fmt.bufPrint(&buf, "CAN: Invalid number literal {s}", .{e.literal});
                    try writer.writeAll(err_msg);
                },
            }
        }
    };

    /// Internal compiler error due to a bug in the compiler implementation.
    pub const Compiler = union(enum) {
        canonicalize: enum {
            exited_top_scope_level,
        },
        resolve_imports,
        type_check,
        specialize_types,
        lift_functions,
        solve_functions,
        specialize_functions,
        lower_statements,
        reference_count,

        /// Make a `Problem` based on a compiler error.
        pub fn make(compiler_error: @This()) Problem {
            return Problem{ .compiler = compiler_error };
        }
    };

    /// A list of problems.
    pub const List = collections.SafeList(@This());
    /// An index into a list of problems.
    pub const Idx = List.Idx;

    /// Format a `Problem` for display.
    pub fn toStr(self: @This(), gpa: Allocator, source: []const u8, writer: anytype) !void {

        // use a stack allocation for printing our tag errors
        var buf: [1000]u8 = undefined;

        switch (self) {
            .tokenize => |a| try a.toStr(gpa, source, writer),
            .parser => |a| {
                const err_msg = try std.fmt.bufPrint(&buf, "PARSER: {s}", .{@tagName(a.tag)});
                try writer.writeAll(err_msg);
            },
            .canonicalize => |err| {
                try err.toStr(gpa, writer);
            },
            .compiler => |err| {
                const err_msg = try std.fmt.bufPrint(&buf, "COMPILER: {?}", .{err});
                try writer.writeAll(err_msg);
            },
        }
    }
};
