const std = @import("std");
const base = @import("base/main.zig");
const resolve = @import("check/resolve_imports.zig");
const type_spec = @import("build/specialize_types.zig");
const func_lift = @import("build/lift_functions.zig");
const func_spec = @import("build/specialize_types.zig");
const func_solve = @import("build/lift_functions.zig");
const lower = @import("build/lower_ir.zig");
const refcount = @import("build/reference_count.zig");

const ResolveIR = type_spec.ResolveIR;
const TypeSpecIR = type_spec.TypeSpecIR;
const RefCountIR = refcount.RefCountIR;

// to compile:
// load file, returning early on failure to load (e.g. missing file)
// parse just header, taking out:
// - exported idents
// - packages as pairs of shorthands and URLs
// - position to finish parsing rest of file from
// - if we are trying to build on top of typechecking, fail if module is not an app/platform
// for package in discovered packages, do the same as above recursively
// assemble a directed graph of packages and modules with the loaded module as the root

fn typecheck_module(filepath: []u8) std.AutoHashMap(base.ModuleId, ResolveIR) {
    const allocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer allocator.deinit();

    _ = filepath;

    // const main_module = .{};
    // const dep_modules: [_][]u8 = [_].{};
    const module_dep_adjacencies = std.AutoHashMap(base.ModuleId, base.ModuleId).init(allocator);

    // TODO: in order of dependencies before who depends on them, run each phase of the compiler
    return module_dep_adjacencies;
}

/// Run the `build` phase of compiling Roc code except for codegen.
///
/// For now, we pass the IR from the last `check` stage (e.g. the `resolve_imports`
/// IR that is narrowed by typechecking) as a single, combined module here along
/// with a singleton `Env` that holds all common large data, like interned strings,
/// symbols, and interned tag names.
///
/// In the future, we will not be combining all modules into a single "module" so this
/// will need to do more complicated coordination of the compiler stages. That said,
/// this still represents the long-term planned order of compilation.
fn pipe_ir_from_typechecking_to_codegen(
    typechecked_modules: []TypeSpecIR,
    resolve_ir: ResolveIR,
) RefCountIR {
    const type_spec_ir = type_spec.specialize_types(resolve_ir, typechecked_modules);
    const func_lift_ir = func_lift.lift_functions(type_spec_ir);
    const func_solve_ir = func_solve.solve_functions(func_lift_ir);
    const func_spec_ir = func_spec.specialize_functions(func_solve_ir);
    const lower_ir_data = lower.lower_ir(func_spec_ir);
    const ref_count_ir = refcount.reference_count(lower_ir_data);

    return ref_count_ir;
}
