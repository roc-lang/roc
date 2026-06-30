//! Helpers for loading the compiled Builtin module in canonicalize tests.
const std = @import("std");
const Allocator = std.mem.Allocator;
const compiled_builtins = @import("compiled_builtins");

const CIR = @import("../CIR.zig");
const Can = @import("../Can.zig");
const builtin_static = @import("../BuiltinStatic.zig");

/// Loads the compiled Builtin module for canonicalize tests that need real builtin types.
pub const BuiltinTestContext = struct {
    builtin_indices: CIR.BuiltinIndices,
    builtin_module: builtin_static.BuiltinModuleView,

    pub fn init(gpa: std.mem.Allocator) Allocator.Error!BuiltinTestContext {
        return .{
            .builtin_indices = compiled_builtins.builtinIndices(CIR),
            .builtin_module = builtin_static.moduleView(gpa, compiled_builtins.builtin_bin[0..], "Builtin", compiled_builtins.builtin_source) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.CorruptEmbeddedBuiltins => @panic("embedded Builtin.bin is corrupt"),
            },
        };
    }

    pub fn deinit(self: *BuiltinTestContext) void {
        self.builtin_module.deinit();
    }

    pub fn canInitContext(self: *const BuiltinTestContext) Can.ModuleInitContext {
        return .{
            .builtin_types = .{
                .builtin_module_env = self.builtin_module.env,
                .builtin_indices = self.builtin_indices,
            },
        };
    }
};
