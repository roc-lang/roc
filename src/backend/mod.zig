//! Code generation backends for the Roc compiler.
//!
//! This module provides various code generation backends:
//! - LLVM: Full-featured backend that generates native code via LLVM
//! - Dev: Fast development backends that generate native code directly (x86_64, aarch64)
//! - Wasm: WebAssembly backend that generates wasm bytecode

const StructuralTest = @import("structural_test.zig");
const roc_target = @import("roc_target");

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
pub const atomicRcHelperSymbolName = dev.atomicRcHelperSymbolName;
pub const collectRequiredRcHelpers = dev.collectRequiredRcHelpers;
pub const collectReferencedProcs = dev.collectReferencedProcs;
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

test "issue 10295: dev backend preserves deep structural equality under register pressure" {
    if (comptime !dev.host_lir_codegen_available) return error.SkipZigTest;

    // https://github.com/roc-lang/roc/issues/10295
    const std = @import("std");
    const layout = @import("layout");
    const lir = @import("lir");

    const allocator = std.testing.allocator;
    var store = lir.LirStore.init(allocator);
    defer store.deinit();
    var layout_store = try layout.Store.init(allocator, .u64);
    defer layout_store.deinit();

    const depth = 32;
    var nested_layouts: [depth + 1]layout.Idx = undefined;
    nested_layouts[0] = .i64;
    for (1..nested_layouts.len) |i| {
        nested_layouts[i] = try layout_store.putStructFields(&.{.{ .index = 0, .layout = nested_layouts[i - 1] }});
    }

    var lhs: [depth + 1]lir.LIR.LocalId = undefined;
    var rhs: [depth + 1]lir.LIR.LocalId = undefined;
    for (0..nested_layouts.len) |i| {
        lhs[i] = try store.addLocal(.{ .layout_idx = nested_layouts[i] });
        rhs[i] = try store.addLocal(.{ .layout_idx = nested_layouts[i] });
    }
    const answer = try store.addLocal(.{ .layout_idx = .bool });

    const ret = try store.addCFStmt(.{ .ret = .{ .value = answer } });
    const eq_args = try store.addLocalSpan(&.{ lhs[depth], rhs[depth] });
    var body = try store.addCFStmt(.{ .assign_low_level = .{
        .target = answer,
        .op = .num_is_eq,
        .rc_effect = lir.LowLevel.num_is_eq.rcEffect(),
        .args = eq_args,
        .next = ret,
    } });

    var level: usize = depth;
    while (level > 0) : (level -= 1) {
        const rhs_fields = try store.addLocalSpan(&.{rhs[level - 1]});
        body = try store.addCFStmt(.{ .assign_struct = .{
            .target = rhs[level],
            .fields = rhs_fields,
            .next = body,
        } });
        const lhs_fields = try store.addLocalSpan(&.{lhs[level - 1]});
        body = try store.addCFStmt(.{ .assign_struct = .{
            .target = lhs[level],
            .fields = lhs_fields,
            .next = body,
        } });
    }
    body = try store.addCFStmt(.{ .assign_literal = .{
        .target = rhs[0],
        .value = .{ .i64_literal = .{ .value = 42, .layout_idx = .i64 } },
        .next = body,
    } });
    body = try store.addCFStmt(.{ .assign_literal = .{
        .target = lhs[0],
        .value = .{ .i64_literal = .{ .value = 42, .layout_idx = .i64 } },
        .next = body,
    } });

    const root = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = lir.LIR.LocalSpan.empty(),
        .body = body,
        .ret_layout = .bool,
    });

    var codegen = try dev.HostLirCodeGen.init(allocator, &store, &layout_store, &.{}, .preserve, roc_target.host_cpu.level());
    defer codegen.deinit();
    try codegen.compileAllProcSpecs(store.getProcSpecs());
    const generated = try codegen.generateCode(root, .bool, 1);
    defer allocator.free(generated.code);

    var executable = try dev.ExecutableMemory.initWithEntryOffsetAndUnwindInfo(
        generated.code,
        generated.entry_offset,
        codegen.getUnwindFunctions(),
    );
    defer executable.deinit();

    var actual: u8 = 0;
    var dummy_roc_ops: u8 = 0;
    const entry: *const fn (*anyopaque, *anyopaque) callconv(.c) void = @ptrCast(@alignCast(executable.entryPtr()));
    entry(@ptrCast(&actual), @ptrCast(&dummy_roc_ops));
    try std.testing.expectEqual(@as(u8, 1), actual);
}

test "issue 10295: nested list equality has bounded register pressure" {
    if (comptime !dev.host_lir_codegen_available) return error.SkipZigTest;

    const std = @import("std");
    const layout = @import("layout");
    const lir = @import("lir");

    const allocator = std.testing.allocator;
    var store = lir.LirStore.init(allocator);
    defer store.deinit();
    var layout_store = try layout.Store.init(allocator, .u64);
    defer layout_store.deinit();

    const depth = 32;
    var nested_layouts: [depth + 1]layout.Idx = undefined;
    nested_layouts[0] = .i64;
    for (1..nested_layouts.len) |i| {
        nested_layouts[i] = try layout_store.insertLayout(layout.Layout.list(nested_layouts[i - 1]));
    }

    const lhs = try store.addLocal(.{ .layout_idx = nested_layouts[depth] });
    const rhs = try store.addLocal(.{ .layout_idx = nested_layouts[depth] });
    const answer = try store.addLocal(.{ .layout_idx = .bool });

    const ret = try store.addCFStmt(.{ .ret = .{ .value = answer } });
    const eq_args = try store.addLocalSpan(&.{ lhs, rhs });
    const eq = try store.addCFStmt(.{ .assign_low_level = .{
        .target = answer,
        .op = .num_is_eq,
        .rc_effect = lir.LowLevel.num_is_eq.rcEffect(),
        .args = eq_args,
        .next = ret,
    } });
    const empty_elems = try store.addLocalSpan(&.{});
    const assign_rhs = try store.addCFStmt(.{ .assign_list = .{
        .target = rhs,
        .elems = empty_elems,
        .next = eq,
    } });
    const body = try store.addCFStmt(.{ .assign_list = .{
        .target = lhs,
        .elems = empty_elems,
        .next = assign_rhs,
    } });
    const root = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = lir.LIR.LocalSpan.empty(),
        .body = body,
        .ret_layout = .bool,
    });

    var codegen = try dev.HostLirCodeGen.init(allocator, &store, &layout_store, &.{}, .preserve, roc_target.host_cpu.level());
    defer codegen.deinit();
    try codegen.compileAllProcSpecs(store.getProcSpecs());
    const generated = try codegen.generateCode(root, .bool, 1);
    defer allocator.free(generated.code);

    var executable = try dev.ExecutableMemory.initWithEntryOffsetAndUnwindInfo(
        generated.code,
        generated.entry_offset,
        codegen.getUnwindFunctions(),
    );
    defer executable.deinit();

    var actual: u8 = 0;
    var dummy_roc_ops: u8 = 0;
    const entry: *const fn (*anyopaque, *anyopaque) callconv(.c) void = @ptrCast(@alignCast(executable.entryPtr()));
    entry(@ptrCast(&actual), @ptrCast(&dummy_roc_ops));
    try std.testing.expectEqual(@as(u8, 1), actual);
}

test "issue 10993: erased callable ABI writes exactly ret_size bytes through the return pointer" {
    if (comptime !dev.host_lir_codegen_available) return error.SkipZigTest;

    // https://github.com/roc-lang/roc/issues/10993
    //
    // The uniform erased-callable ABI hands the callee a pointer to
    // caller-owned result storage of exactly the return layout's size (see
    // builtins/erased_callable.zig). A callee that stores past that size
    // corrupts whatever the caller keeps next to the result slot:
    // `listSortWith` comparators return a 1-byte ordering into a
    // `var ordering: u8` stack slot, so any wider store smashes the caller's
    // frame and segfaults `List.sort` under the machine-code shim.
    const std = @import("std");
    const layout = @import("layout");
    const lir = @import("lir");
    const builtins = @import("builtins");

    const allocator = std.testing.allocator;
    var store = lir.LirStore.init(allocator);
    defer store.deinit();
    var layout_store = try layout.Store.init(allocator, .u64);
    defer layout_store.deinit();

    const capture_arg = try store.addLocal(.{ .layout_idx = .opaque_ptr });
    const reuse_arg = try store.addLocal(.{ .layout_idx = .opaque_ptr });
    const args = try store.addLocalSpan(&.{ capture_arg, reuse_arg });
    const arg_plan = try store.internErasedCallArgsPlan(&layout_store, &.{});

    const result = try store.addLocal(.{ .layout_idx = .u8 });
    const ret = try store.addCFStmt(.{ .ret = .{ .value = result } });
    const assign = try store.addCFStmt(.{ .assign_literal = .{
        .target = result,
        .value = .{ .i64_literal = .{ .value = 2, .layout_idx = .u8 } },
        .next = ret,
    } });

    const proc_id = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = args,
        .body = assign,
        .ret_layout = .u8,
        .abi = .erased_callable,
        .erased_capture_arg = capture_arg,
        .erased_reuse_arg = reuse_arg,
        .erased_call_args = arg_plan,
    });

    var codegen = try dev.HostLirCodeGen.init(allocator, &store, &layout_store, &.{}, .preserve, roc_target.host_cpu.level());
    defer codegen.deinit();
    try codegen.compileAllProcSpecs(store.getProcSpecs());

    const compiled = codegen.proc_registry.get(@intFromEnum(proc_id)) orelse return error.TestUnexpectedResult;
    var executable = try dev.ExecutableMemory.initWithEntryOffsetAndUnwindInfo(
        codegen.codegen.getCode(),
        compiled.code_start,
        codegen.getUnwindFunctions(),
    );
    defer executable.deinit();

    // Sentinel bytes on both sides of the 1-byte result slot; the callee owns
    // only ret_buf[4].
    var ret_buf align(16) = [_]u8{0xAA} ** 16;
    var out_desc: ?*const anyopaque = null;
    var dummy_roc_ops: builtins.erased_callable.RocOps = undefined;
    const callable: builtins.erased_callable.ErasedCallableFn = @ptrCast(@alignCast(executable.entryPtr()));
    callable(&dummy_roc_ops, ret_buf[4..].ptr, null, null, null, &out_desc);

    try std.testing.expectEqual(@as(u8, 2), ret_buf[4]);
    for (ret_buf, 0..) |byte, i| {
        if (i == 4) continue;
        try std.testing.expectEqual(@as(u8, 0xAA), byte);
    }
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
    var codegen = try WinCodeGen.init(allocator, &store, &layout_store, &.{}, .preserve, .default);
    defer codegen.deinit();
    codegen.generation_mode = .object_file;

    try codegen.compileAllProcSpecs(store.getProcSpecs());

    // MOVDQU m128, XMM0 is the unaligned full-width store into the result slot.
    const code = codegen.getGeneratedCode();
    var return_code: ?[]const u8 = null;
    for (codegen.getRelocations()) |relocation| {
        if (relocation == .linked_function and std.mem.eql(u8, relocation.linked_function.name, "hosted_u128_identity")) {
            const call_end: usize = @intCast(relocation.linked_function.offset + 4);
            return_code = code[call_end..@min(call_end + 32, code.len)];
            break;
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
    var codegen = try WinCodeGen.init(allocator, &store, &layout_store, &.{}, .preserve, .default);
    defer codegen.deinit();
    codegen.generation_mode = .object_file;

    try codegen.compileAllProcSpecs(store.getProcSpecs());
    const entrypoint = try codegen.generateEntrypointWrapper("roc_u128_identity", proc, &.{}, .u128);
    const code = codegen.getGeneratedCode();
    const entrypoint_code = code[entrypoint.offset..][0..entrypoint.size];

    // MOVDQU XMM0, m128 returns the complete value using clang/Rust's convention.
    try std.testing.expect(std.mem.find(u8, entrypoint_code, &.{ 0xF3, 0x0F, 0x6F, 0x85 }) != null);
}
