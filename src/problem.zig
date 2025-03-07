const std = @import("std");
const base = @import("base.zig");
const collections = @import("collections.zig");

const Ident = base.Ident;
const Region = base.Region;

/// Represents a problem encountered during the compilation process.
pub const Problem = union(enum) {
    canonicalize: Canonicalize,
    compiler: Compiler,

    // User error preventing canonicalize from completing fully
    // For example a variable that was used but not defined
    pub const Canonicalize = union(enum) {
        DuplicateImport: struct {
            duplicate_import_region: Region,
        },
        DuplicateExposes: struct {
            first_exposes: Ident.Idx,
            duplicate_exposes: Ident.Idx,
        },
        AliasNotInScope: struct {
            name: Ident.Idx,
            suggestions: collections.SafeList(Ident.Idx).Slice,
        },
        IdentNotInScope: struct {
            ident: Ident.Idx,
            suggestions: collections.SafeList(Ident.Idx).Slice,
        },
        AliasAlreadyInScope: struct {
            original_name: Ident.Idx,
            shadow: Ident.Idx,
        },
        IdentAlreadyInScope: struct {
            original_ident: Ident.Idx,
            shadow: Ident.Idx,
        },

        pub fn make(can_problem: @This()) Problem {
            return Problem{ .canonicalize = can_problem };
        }
    };

    // Internal compiler error due to a bug in the compiler implementation
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

        pub fn make(compiler_error: @This()) Problem {
            return Problem{ .compiler = compiler_error };
        }
    };

    pub const List = collections.SafeList(@This());
    pub const Idx = List.Idx;
};
