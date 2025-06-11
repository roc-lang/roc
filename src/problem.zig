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
    canonicalize: @import("check/canonicalize/CIR.zig").Diagnostic,
    compiler: Compiler,

    /// Internal compiler error due to a bug in the compiler implementation.
    pub const Compiler = union(enum) {
        canonicalize: Can,
        resolve_imports,
        type_check,
        specialize_types,
        lift_functions,
        solve_functions,
        specialize_functions,
        lower_statements,
        reference_count,

        pub const Can = enum {
            not_implemented,
            exited_top_scope_level,
            unable_to_resolve_identifier,
            failed_to_canonicalize_decl,
            unexpected_token_binop,
        };

        /// Make a `Problem` based on a compiler error.
        pub fn make(compiler_error: Compiler) Problem {
            return Problem{ .compiler = compiler_error };
        }

        /// Make a  based on a compiler error.
        pub fn can(tag: Can) Problem {
            return Problem{ .compiler = .{ .canonicalize = tag } };
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
            .tokenize => |a| {
                try a.toStr(gpa, source, writer);
            },
            .parser => |a| {
                const err_msg = try std.fmt.bufPrint(&buf, "PARSER: {s}", .{@tagName(a.tag)});
                try writer.writeAll(err_msg);
            },
            .canonicalize => |a| {
                const err_msg = try std.fmt.bufPrint(&buf, "CANONICALIZE: {s}", .{@tagName(a.tag)});
                try writer.writeAll(err_msg);
            },
            .compiler => |err| {
                const err_msg = try std.fmt.bufPrint(&buf, "COMPILER: {?}", .{err});
                try writer.writeAll(err_msg);
            },
        }
    }
};
