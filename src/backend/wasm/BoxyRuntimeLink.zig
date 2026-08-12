//! Surgical Wasm linking for the standalone Boxy runtime and its exact LIR
//! sidecar. Both CLI builds and in-process evaluation use this path.

const std = @import("std");
const lir = @import("lir");
const StaticDataExport = @import("../dev/StaticDataExport.zig").StaticDataExport;
const WasmModule = @import("WasmModule.zig");

/// Runtime object and exact serialized sidecar merged into one Wasm module.
pub const Input = struct {
    runtime_object: []const u8,
    sidecar_blob: []const u8,
    sidecar_desc: lir.LirImage.BoxySidecar,
};

/// Errors possible while constructing and merging the two relocatable modules.
pub const Error = WasmModule.StaticDataError || WasmModule.ParseError || WasmModule.MergeError;

/// Merge sidecar definitions before the PIC runtime so its GOT globals resolve
/// directly to the exact descriptor, layout, and string tables for this LIR.
pub fn merge(
    allocator: std.mem.Allocator,
    module: *WasmModule,
    input: Input,
    mode: WasmModule.MergeMode,
) Error!void {
    const blob_len: u64 = @intCast(input.sidecar_blob.len);
    const exports = [_]StaticDataExport{
        .{ .symbol_name = "roc_boxy_sidecar_blob", .bytes = input.sidecar_blob, .alignment = 16, .is_global = true },
        .{ .symbol_name = "roc_boxy_sidecar_blob_len", .bytes = std.mem.asBytes(&blob_len), .alignment = 8, .is_global = true },
        .{ .symbol_name = "roc_boxy_sidecar_desc", .bytes = std.mem.asBytes(&input.sidecar_desc), .alignment = 8, .is_global = true },
    };

    var sidecar_module = try WasmModule.staticDataModule(allocator, &exports);
    defer sidecar_module.deinit();
    var sidecar_merge = try module.mergeModuleMode(&sidecar_module, mode);
    sidecar_merge.deinit();

    var runtime_module = try WasmModule.preload(allocator, input.runtime_object, true);
    defer runtime_module.deinit();
    var runtime_merge = try module.mergeModuleMode(&runtime_module, mode);
    runtime_merge.deinit();
}
