//! Target-layout readonly data symbols for provided non-function constants.
//!
//! Static data exports are produced from checked const data plus committed LIR
//! layouts. They do not depend on post-check representation payload stores.

const std = @import("std");

const backend = @import("backend");
const check = @import("check");
const lir = @import("lir");
const roc_target = @import("roc_target");

const Allocator = std.mem.Allocator;
const Checked = check.CheckedArtifact;
const StaticDataExport = backend.StaticDataExport;
const StaticDataRelocation = backend.StaticDataRelocation;

pub const ModuleViews = struct {
    root: ?Checked.LoweringModuleView = null,
    imports: []const Checked.ImportedModuleView = &.{},
};

const MaterializationError = Allocator.Error || error{
    UnsupportedTarget,
};

pub fn buildProvidedDataExports(
    allocator: Allocator,
    modules: ModuleViews,
    lowered: ?*const lir.CheckedPipeline.LoweredProgram,
    target: roc_target.RocTarget,
) MaterializationError![]StaticDataExport {
    _ = lowered;
    _ = target;

    if (hasProvidedData(modules)) {
        staticDataInvariant("provided data exports must be materialized from ConstStore and direct LIR layouts");
    }

    return try allocator.alloc(StaticDataExport, 0);
}

pub fn deinitProvidedDataExports(allocator: Allocator, exports: []StaticDataExport) void {
    for (exports) |static_export| {
        allocator.free(static_export.symbol_name);
        allocator.free(static_export.bytes);
        deinitRelocationSlice(allocator, static_export.relocations);
        allocator.free(static_export.relocations);
    }
    allocator.free(exports);
}

fn hasProvidedData(modules: ModuleViews) bool {
    if (modules.root) |root| {
        if (moduleHasProvidedData(root.artifact)) return true;
    }
    for (modules.imports) |imported| {
        for (imported.provided_exports.exports) |provided| {
            switch (provided) {
                .data => return true,
                .procedure => {},
            }
        }
    }
    return false;
}

fn moduleHasProvidedData(module: *const Checked.CheckedModuleArtifact) bool {
    for (module.provided_exports.exports) |provided| {
        switch (provided) {
            .data => return true,
            .procedure => {},
        }
    }
    return false;
}

fn deinitRelocationSlice(allocator: Allocator, relocations: []const StaticDataRelocation) void {
    for (relocations) |relocation| {
        if (relocation.owns_target_symbol_name) allocator.free(relocation.target_symbol_name);
    }
}

fn staticDataInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        std.debug.panic("static data invariant violated: {s}", .{message});
    }
    unreachable;
}

test "static data declarations are referenced" {
    std.testing.refAllDecls(@This());
}
