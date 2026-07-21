//! Code generation backends for the Roc compiler.
//!
//! This module provides various code generation backends:
//! - LLVM: Full-featured backend that generates native code via LLVM
//! - Dev: Fast development backends that generate native code directly (x86_64, aarch64)
//! - Wasm: WebAssembly backend that generates wasm bytecode

const StructuralTest = @import("structural_test.zig");

pub const dev = @import("dev/mod.zig");
pub const wasm = @import("wasm/mod.zig");

// Re-export dev backend types at top level.
pub const x86_64 = dev.x86_64;
pub const aarch64 = dev.aarch64;
pub const object = dev.object;
pub const Relocation = dev.Relocation;
pub const applyRelocations = dev.applyRelocations;
pub const applyRelocationsWithContext = dev.applyRelocationsWithContext;
pub const SymbolResolver = dev.SymbolResolver;
pub const SymbolResolverContext = dev.SymbolResolverContext;
pub const CodeGen = dev.CodeGen;
pub const Backend = dev.Backend;
pub const ExecutableMemory = dev.ExecutableMemory;
pub const HostLirCodeGen = dev.HostLirCodeGen;
pub const host_lir_codegen_available = dev.host_lir_codegen_available;
pub const LirCodeGenMod = dev.LirCodeGenMod;
pub const DevBackend = dev.DevBackend;
pub const Storage = dev.Storage;
pub const X86_64LinuxBackend = dev.X86_64LinuxBackend;
pub const X86_64MacBackend = dev.X86_64MacBackend;
pub const X86_64WinBackend = dev.X86_64WinBackend;
pub const AArch64Backend = dev.AArch64Backend;
pub const Entrypoint = dev.Entrypoint;
pub const StaticDataExport = dev.StaticDataExport;
pub const StaticDataRelocation = dev.StaticDataRelocation;
pub const StaticDataImage = dev.StaticDataImage;
pub const StaticDataImageFunctionResolver = dev.StaticDataImageFunctionResolver;
pub const StaticStringData = dev.StaticStringData;
pub const RunImage = dev.RunImage;
pub const procSymbolName = dev.procSymbolName;
pub const ObjectFileCompiler = dev.ObjectFileCompiler;
pub const CompilationResult = dev.CompilationResult;
pub const CompilationError = dev.CompilationError;
pub const writeFileWindowsAvSafe = dev.writeFileWindowsAvSafe;
pub const resolveBuiltinFunction = dev.resolveBuiltinFunction;

test "backend tests" {
    const std = @import("std");
    std.testing.refAllDecls(StructuralTest);
    std.testing.refAllDecls(dev);
    std.testing.refAllDecls(wasm);
}

test "x86_64 Windows hosted U128 return stores all 16 bytes from XMM0" {
    const std = @import("std");
    const layout = @import("layout");
    const lir = @import("lir");

    const allocator = std.testing.allocator;
    var store = lir.LirStore.init(allocator);
    defer store.deinit();
    var layout_store = try layout.Store.init(allocator, .u64);
    defer layout_store.deinit();

    // Repro for https://github.com/roc-lang/roc/issues/10163: the clang/Rust
    // Windows x64 ABI returns a bare U128 in XMM0, so the caller must copy the
    // complete 16-byte register into Roc's result slot.
    const symbol = try store.insertString("hosted_u128_identity");
    _ = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = lir.LIR.LocalSpan.empty(),
        .ret_layout = .u128,
        .hosted = .{ .symbol = symbol, .dispatch_index = 0 },
    });

    const WinCodeGen = dev.LirCodeGenMod.LirCodeGen(.x64win);
    var codegen = try WinCodeGen.init(allocator, &store, &layout_store, &.{});
    defer codegen.deinit();
    codegen.generation_mode = .object_file;

    try codegen.compileAllProcSpecs(store.getProcSpecs());

    // MOVDQU m128, XMM0 is the unaligned full-width store into the result slot.
    const code = codegen.getGeneratedCode();
    var return_code: ?[]const u8 = null;
    for (codegen.getRelocations()) |relocation| {
        switch (relocation) {
            .linked_function => |linked| {
                if (std.mem.eql(u8, linked.name, "hosted_u128_identity")) {
                    const call_end: usize = @intCast(linked.offset + 4);
                    return_code = code[call_end..@min(call_end + 32, code.len)];
                    break;
                }
            },
            else => {},
        }
    }
    const after_hosted_call = return_code orelse return error.TestUnexpectedResult;
    const store_start = std.mem.find(u8, after_hosted_call, &.{ 0xF3, 0x0F }) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqualSlices(u8, &.{ 0xF3, 0x0F, 0x7F, 0x85 }, after_hosted_call[store_start..][0..4]);
}

test "x86_64 Windows U128 entrypoint return loads all 16 bytes into XMM0" {
    const std = @import("std");
    const layout = @import("layout");
    const lir = @import("lir");

    const allocator = std.testing.allocator;
    var store = lir.LirStore.init(allocator);
    defer store.deinit();
    var layout_store = try layout.Store.init(allocator, .u64);
    defer layout_store.deinit();

    const result = try store.addLocal(.{ .layout_idx = .u128 });
    const ret = try store.addCFStmt(.{ .ret = .{ .value = result } });
    const body = try store.addCFStmt(.{ .assign_literal = .{
        .target = result,
        .value = .{ .i128_literal = .{ .value = 0x11111111111111112222222222222222, .layout_idx = .u128 } },
        .next = ret,
    } });
    const proc = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = lir.LIR.LocalSpan.empty(),
        .body = body,
        .ret_layout = .u128,
    });

    const WinCodeGen = dev.LirCodeGenMod.LirCodeGen(.x64win);
    var codegen = try WinCodeGen.init(allocator, &store, &layout_store, &.{});
    defer codegen.deinit();
    codegen.generation_mode = .object_file;

    try codegen.compileAllProcSpecs(store.getProcSpecs());
    const entrypoint = try codegen.generateEntrypointWrapper("roc_u128_identity", proc, &.{}, .u128);
    const code = codegen.getGeneratedCode();
    const entrypoint_code = code[entrypoint.offset..][0..entrypoint.size];

    // MOVDQU XMM0, m128 returns the complete value using clang/Rust's convention.
    try std.testing.expect(std.mem.find(u8, entrypoint_code, &.{ 0xF3, 0x0F, 0x6F, 0x85 }) != null);
}
