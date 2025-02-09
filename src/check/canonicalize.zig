const std = @import("std");
const base = @import("../base.zig");
const parse = @import("parse.zig");
const problem = @import("../problem.zig");
const collections = @import("../collections.zig");

const Problem = problem.Problem;
const Scope = @import("canonicalize/Scope.zig");

const can = @This();
pub const IR = @import("canonicalize/IR.zig");

/// After parsing a Roc program, the [ParseIR](src/check/parse/ir.zig) is transformed into a [canonical
/// form](src/check/canonicalize/ir.zig) called CanIR.
///
/// Canonicalization performs analysis to catch user errors, and sets up the state necessary to solve the types in a
/// program. Among other things, canonicalization;
/// - Uniquely identifies names (think variable and function names). Along the way,
///     canonicalization builds a graph of all variables' references, and catches
///     unused definitions, undefined definitions, and shadowed definitions.
/// - Resolves type signatures, including aliases, into a form suitable for type
///     solving.
/// - Determines the order definitions are used in, if they are defined
///     out-of-order.
/// - Eliminates syntax sugar (for example, renaming `+` to the function call `add`).
///
/// The canonicalization occurs on a single module (file) in isolation. This allows for this work to be easily parallelized and also cached. So where the source code for a module has not changed, the CanIR can simply be loaded from disk and used immediately.
pub fn canonicalize(
    can_ir: IR,
    parse_ir: parse.IR,
    allocator: std.mem.Allocator,
) void {
    const env = can_ir.env;
    const scope = Scope.init(can_ir.env, .{}, .{}, allocator);
    _ = scope;

    for (parse_ir.defs.items.items) |stmt| {
        switch (stmt) {
            .Import => |import| {
                const res = env.modules.getOrInsert(
                    import.name,
                    import.package_shorthand,
                );

                if (res.was_present) {
                    env.problems.append(Problem.Canonicalize.make(.{.DuplicateImport{
                        .duplicate_import_region = import.name_region,
                    }}));
                }

                for (import.exposing.items.items) |exposed_ident| {
                    const module_ident = base.ModuleIdent{
                        .module_idx = res.module_idx,
                        .ident_idx = exposed_ident,
                    };

                    env.addExposedIdentForModule(module_ident);
                }
            },
        }
    }

    @panic("not implemented");
}
