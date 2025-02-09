const std = @import("std");
const base = @import("base.zig");
const parse = @import("check/parse.zig");
const can = @import("check/canonicalize.zig");
const resolve = @import("check/resolve_imports.zig");
const type_spec = @import("build/specialize_types.zig");
const func_lift = @import("build/lift_functions.zig");
const func_spec = @import("build/specialize_types.zig");
const func_solve = @import("build/lift_functions.zig");
const lower = @import("build/lower_statements.zig");
const refcount = @import("build/reference_count.zig");
const tokenize = @import("check/parse/tokenize.zig");

const Package = base.Package;
const exitOnOom = @import("collections/utils.zig").exitOnOom;

const ResolveIR = type_spec.IR;
const TypeSpecIR = type_spec.IR;
const RefCountIR = refcount.IR;

// to compile:
//
// load file, returning early on failure to load (e.g. missing file)
// parse just header, taking out:
// - exported idents
// - packages as pairs of shorthands and URLs
// - position to finish parsing rest of file from
// - if we are trying to build on top of typechecking, fail if module is not an app/platform/package
// for package in discovered packages, do the same as above recursively
// assemble a directed graph of packages and modules with the loaded module as the root

// we have a map where the key is the package "key" (pending what key we use)
// a package has these attributes
// - download URL
// - content hash
// - version string
// - a root file path
// - a collection of files with paths relative to the root file of the package (AKA `main.roc`)
//
// we have many external packages with the above qualities, and we also have a single "current" package
// it only has these attributes
// - a root file path
// - a collection of files with paths relative to the root file of the package (AKA `main.roc`)
//
// traverse graph of dependencies looking for a DAG relationship between the packages
// - If this isn't the case, show an error with the cyclic dependency
// reverse the dependency graph and traverse it for compilation order
//
// In full parallel (no inter-module dependencies considered):
// - get the base64-encoded BLAKE3 hash of each file
// - if we have a cached artifact of that module's hash for that Roc version:
//   - load it into an arena that contains the CanIR for that module with no more or less data in that arena
// - otherwise, parse and then (solo) canonicalize that module, and cache when finished
//
// For future compilation of stages, compile the modules in order of dependencies first, appending freshly-compiled modules to a full list of modules such that they are consistently ordered between stages and densely stored
//
// Then, in reverse order of the dependencies of each module (AKA dependencies first):
// - resolve imports
// - typecheck
//
// - if just typechecking, return here
// - if using the interpreter as a backend, use the typechecked ResolveIR to run the code as-is
// - if using LLVM as a backend, use the typechecked ResolveIR to compile constants to their values or to errors, keeping a mapping of generic constants to their possible types
//
// Afterwards, in reverse order of the dependencies of each module (AKA dependencies first):
// - specialize types
// - lift functions
// - specialize functions
// - lower statements
// - reference count
//
// Lastly, pass all modules together to LLVM codegen

pub const TypeCheckResult = union(enum) {
    Success: struct {
        packages: Package.Store,
        main_module: *ResolveIR,
        all_modules: std.ArrayList(ResolveIR),
    },
    FailedToReadFile: struct { filepath: []u8 },
    FailedToLoadPackage: anyerror,
    // other errors...
};

pub fn typecheck_module(
    filepath: []u8,
    allocator: std.mem.Allocator,
) TypeCheckResult {
    _ = filepath;
    _ = allocator;

    @panic("not implemented");
}
