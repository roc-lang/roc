//! Wasm composition for the standalone Boxy runtime and its exact LIR sidecar.
//! CLI builds seal the result before platform linking; in-process evaluation
//! merges it directly into its private module.

const std = @import("std");
const lir = @import("lir");
const StaticDataExport = @import("../dev/StaticDataExport.zig").StaticDataExport;
const WasmModule = @import("WasmModule.zig");

/// Reserved function-symbol namespace provided by the Boxy runtime object.
pub const function_namespace = "roc_boxy_";

/// Runtime object and exact serialized sidecar merged into one Wasm module.
pub const Input = struct {
    runtime_object: []const u8,
    sidecar_blob: []const u8,
    sidecar_desc: lir.LirImage.BoxySidecar,
};

/// Errors possible while constructing, merging, and verifying the two
/// relocatable modules.
pub const Error = WasmModule.StaticDataError || WasmModule.ParseError || WasmModule.MergeError || error{UnresolvedBuiltinImport};

/// Merge the exact descriptor, layout, and string tables for this LIR.
///
/// CLI builds use this entrypoint while assembling their compiler-only object;
/// the runtime object is added by the subsequent relocatable composition.
pub fn mergeSidecar(
    allocator: std.mem.Allocator,
    module: *WasmModule,
    sidecar_blob: []const u8,
    sidecar_desc: lir.LirImage.BoxySidecar,
    mode: WasmModule.MergeMode,
) Error!void {
    const blob_len: u64 = @intCast(sidecar_blob.len);
    const exports = [_]StaticDataExport{
        .{ .symbol_name = "roc_boxy_sidecar_blob", .bytes = sidecar_blob, .alignment = 16, .is_global = true },
        .{ .symbol_name = "roc_boxy_sidecar_blob_len", .bytes = std.mem.asBytes(&blob_len), .alignment = 8, .is_global = true },
        .{ .symbol_name = "roc_boxy_sidecar_desc", .bytes = std.mem.asBytes(&sidecar_desc), .alignment = 8, .is_global = true },
    };

    var sidecar_module = try WasmModule.staticDataModule(allocator, &exports);
    defer sidecar_module.deinit();
    var sidecar_merge = try module.mergeModuleMode(&sidecar_module, mode);
    sidecar_merge.deinit();
}

/// Merge sidecar definitions before the runtime so its relocations resolve
/// directly to the exact descriptor, layout, and string tables for this LIR.
pub fn merge(
    allocator: std.mem.Allocator,
    module: *WasmModule,
    input: Input,
    mode: WasmModule.MergeMode,
) Error!void {
    try mergeSidecar(allocator, module, input.sidecar_blob, input.sidecar_desc, mode);

    var runtime_module = try WasmModule.preload(allocator, input.runtime_object, .relocatable_for_merge);
    defer runtime_module.deinit();
    var runtime_merge = try module.mergeModuleMode(&runtime_module, mode);
    runtime_merge.deinit();
    try module.verifyNoUndefinedFunctionSymbolsInNamespace(function_namespace);
}
