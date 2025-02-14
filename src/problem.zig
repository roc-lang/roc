const std = @import("std");
const base = @import("base.zig");
const collections = @import("collections.zig");

const Ident = base.Ident;
const Region = base.Region;
const TagName = base.TagName;

pub const Problem = union(enum) {
    Parse: Parse,
    Canonicalize: Canonicalize,
    Compiler: Compiler,

    pub const Parse = union(enum) {
        IdentIssue: struct {
            problems: Ident.Problems,
            region: Region,
        },

        pub fn make(problem: Parse) Problem {
            return Problem{ .Parse = problem };
        }
    };

    pub const Canonicalize = union(enum) {
        DuplicateImport: struct {
            duplicate_import_region: Region,
        },
        DuplicateExposes: struct {
            first_exposes: Ident.Idx,
            duplicate_exposes: Ident.Idx,
        },
        AliasNotInScope: struct {
            name: TagName.Idx,
            suggestions: collections.SafeList(TagName.Idx).Slice,
        },
        IdentNotInScope: struct {
            ident: Ident.Idx,
            suggestions: collections.SafeList(Ident.Idx).Slice,
        },
        AliasAlreadyInScope: struct {
            original_name: TagName.Idx,
            shadow: TagName.Idx,
        },
        IdentAlreadyInScope: struct {
            original_ident: Ident.Idx,
            shadow: Ident.Idx,
        },

        pub fn make(problem: Canonicalize) Problem {
            return Problem{ .Canonicalize = problem };
        }
    };

    pub const Compiler = union(enum) {
        Parse: Compiler.Parse,
        Canonicalize: Compiler.Canonicalize,
        ResolveImports: Compiler.ResolveImports,
        TypeCheck: Compiler.TypeCheck,
        SpecializeTypes: Compiler.SpecializeTypes,
        LiftFunctions: Compiler.LiftFunctions,
        SolveFunctions: Compiler.SolveFunctions,
        SpecializeFunctions: Compiler.SpecializeFunctions,
        LowerStatements: Compiler.LowerStatements,
        ReferenceCount: Compiler.ReferenceCount,

        pub fn make(problem: Compiler) Problem {
            return Problem{ .Compiler = problem };
        }

        pub const Parse = union(enum) {};

        pub const Canonicalize = union(enum) {
            ExitedTopScopeLevel,

            pub fn make(problem: Compiler.Canonicalize) Problem {
                return Compiler.make(.{ .Canonicalize = problem });
            }
        };

        pub const ResolveImports = union(enum) {};
        pub const TypeCheck = union(enum) {};
        pub const SpecializeTypes = union(enum) {};
        pub const LiftFunctions = union(enum) {};
        pub const SolveFunctions = union(enum) {};
        pub const SpecializeFunctions = union(enum) {};
        pub const LowerStatements = union(enum) {};
        pub const ReferenceCount = union(enum) {};
    };

    // pub const List = std.ArrayList(Problem);
};
