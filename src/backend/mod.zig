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
pub const in_process_abi = @import("in_process_abi.zig");

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
    std.testing.refAllDecls(in_process_abi);
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

    var codegen = try dev.HostLirCodeGen.init(allocator, &store, &layout_store, .{}, &.{}, .preserve, roc_target.host_cpu.level());
    defer codegen.deinit();
    try codegen.compileAllProcSpecs(store.getProcSpecs());
    const generated = try codegen.generateCode(root, .bool);
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

    var codegen = try dev.HostLirCodeGen.init(allocator, &store, &layout_store, .{}, &.{}, .preserve, roc_target.host_cpu.level());
    defer codegen.deinit();
    try codegen.compileAllProcSpecs(store.getProcSpecs());
    const generated = try codegen.generateCode(root, .bool);
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
    //
    // The four return layouts cover every copy shape: a register-sized
    // scalar (1 byte), sub-word aggregates with and without an overlapping
    // tail chunk (3 and 7 bytes), and a non-word-multiple aggregate above
    // 8 bytes (12 bytes: one whole word plus an overlapping 8-byte tail).
    const std = @import("std");
    const layout = @import("layout");
    const lir = @import("lir");
    const builtins = @import("builtins");

    const allocator = std.testing.allocator;
    var store = lir.LirStore.init(allocator);
    defer store.deinit();
    var layout_store = try layout.Store.init(allocator, .u64);
    defer layout_store.deinit();

    const helpers = struct {
        fn addErasedProc(
            s: *lir.LirStore,
            ls: *layout.Store,
            body: lir.CFStmtId,
            ret_layout: layout.Idx,
        ) std.mem.Allocator.Error!lir.LIR.LirProcSpecId {
            const capture_arg = try s.addLocal(.{ .layout_idx = .opaque_ptr });
            const reuse_arg = try s.addLocal(.{ .layout_idx = .opaque_ptr });
            const args = try s.addLocalSpan(&.{ capture_arg, reuse_arg });
            const arg_plan = try s.internErasedCallArgsPlan(ls, &.{});
            return s.addProcSpec(.{
                .name = s.freshSyntheticSymbol(),
                .args = args,
                .body = body,
                .ret_layout = ret_layout,
                .abi = .erased_callable,
                .erased_capture_arg = capture_arg,
                .erased_reuse_arg = reuse_arg,
                .erased_call_args = arg_plan,
            });
        }

        fn addStructBody(
            s: *lir.LirStore,
            struct_layout: layout.Idx,
            field_layout: layout.Idx,
            field_values: []const i64,
        ) std.mem.Allocator.Error!lir.CFStmtId {
            var field_locals: [7]lir.LIR.LocalId = undefined;
            for (field_values, 0..) |_, i| {
                field_locals[i] = try s.addLocal(.{ .layout_idx = field_layout });
            }
            const result = try s.addLocal(.{ .layout_idx = struct_layout });
            const ret = try s.addCFStmt(.{ .ret = .{ .value = result } });
            const fields = try s.addLocalSpan(field_locals[0..field_values.len]);
            var body = try s.addCFStmt(.{ .assign_struct = .{
                .target = result,
                .fields = fields,
                .next = ret,
            } });
            for (field_values, 0..) |value, i| {
                body = try s.addCFStmt(.{ .assign_literal = .{
                    .target = field_locals[i],
                    .value = .{ .i64_literal = .{ .value = value, .layout_idx = field_layout } },
                    .next = body,
                } });
            }
            return body;
        }

        fn abort(_: *builtins.host_abi.RocOps, _: [*]const u8, _: usize) callconv(.c) void {
            @panic("erased callable ret-size test must not reach RocOps");
        }
        fn abortAlloc(_: *builtins.host_abi.RocOps, _: usize, _: usize) callconv(.c) ?*anyopaque {
            @panic("erased callable ret-size test must not allocate");
        }
        fn abortDealloc(_: *builtins.host_abi.RocOps, _: *anyopaque, _: usize) callconv(.c) void {
            @panic("erased callable ret-size test must not deallocate");
        }
        fn abortRealloc(_: *builtins.host_abi.RocOps, _: *anyopaque, _: usize, _: usize) callconv(.c) ?*anyopaque {
            @panic("erased callable ret-size test must not reallocate");
        }
    };

    // u8 scalar returning 2 (an Ordering-sized result, the List.sort shape).
    const scalar_result = try store.addLocal(.{ .layout_idx = .u8 });
    const scalar_ret = try store.addCFStmt(.{ .ret = .{ .value = scalar_result } });
    const scalar_body = try store.addCFStmt(.{ .assign_literal = .{
        .target = scalar_result,
        .value = .{ .i64_literal = .{ .value = 2, .layout_idx = .u8 } },
        .next = scalar_ret,
    } });
    const scalar_proc = try helpers.addErasedProc(&store, &layout_store, scalar_body, .u8);

    const u8x3_layout = try layout_store.putStructFields(&.{
        .{ .index = 0, .layout = .u8 },
        .{ .index = 1, .layout = .u8 },
        .{ .index = 2, .layout = .u8 },
    });
    const u8x3_body = try helpers.addStructBody(&store, u8x3_layout, .u8, &.{ 0x11, 0x22, 0x33 });
    const u8x3_proc = try helpers.addErasedProc(&store, &layout_store, u8x3_body, u8x3_layout);

    const u8x7_layout = try layout_store.putStructFields(&.{
        .{ .index = 0, .layout = .u8 },
        .{ .index = 1, .layout = .u8 },
        .{ .index = 2, .layout = .u8 },
        .{ .index = 3, .layout = .u8 },
        .{ .index = 4, .layout = .u8 },
        .{ .index = 5, .layout = .u8 },
        .{ .index = 6, .layout = .u8 },
    });
    const u8x7_body = try helpers.addStructBody(&store, u8x7_layout, .u8, &.{ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47 });
    const u8x7_proc = try helpers.addErasedProc(&store, &layout_store, u8x7_body, u8x7_layout);

    const u32x3_layout = try layout_store.putStructFields(&.{
        .{ .index = 0, .layout = .u32 },
        .{ .index = 1, .layout = .u32 },
        .{ .index = 2, .layout = .u32 },
    });
    const u32x3_body = try helpers.addStructBody(&store, u32x3_layout, .u32, &.{ 0x01020304, 0x05060708, 0x090A0B0C });
    const u32x3_proc = try helpers.addErasedProc(&store, &layout_store, u32x3_body, u32x3_layout);

    var codegen = try dev.HostLirCodeGen.init(allocator, &store, &layout_store, .{}, &.{}, .preserve, roc_target.host_cpu.level());
    defer codegen.deinit();
    try codegen.compileAllProcSpecs(store.getProcSpecs());

    // This test jumps straight into the shared code buffer, so it can only
    // execute code with no unapplied relocations. If the erased-callable
    // prologue ever grows a relocation-backed call, resolve the relocations
    // here instead of deleting this check.
    try std.testing.expectEqual(@as(usize, 0), codegen.codegen.relocations.items.len);

    var executable = try dev.ExecutableMemory.initWithEntryOffsetAndUnwindInfo(
        codegen.codegen.getCode(),
        0,
        codegen.getUnwindFunctions(),
    );
    defer executable.deinit();

    var roc_ops = builtins.host_abi.RocOps{
        .env = undefined,
        .roc_alloc = &helpers.abortAlloc,
        .roc_dealloc = &helpers.abortDealloc,
        .roc_realloc = &helpers.abortRealloc,
        .roc_dbg = &helpers.abort,
        .roc_expect_failed = &helpers.abort,
        .roc_crashed = &helpers.abort,
        .hosted_fns = builtins.host_abi.emptyHostedFunctions(),
    };

    const cases = [_]struct {
        proc_id: lir.LIR.LirProcSpecId,
        expected: []const u8,
    }{
        .{ .proc_id = scalar_proc, .expected = &.{2} },
        .{ .proc_id = u8x3_proc, .expected = &.{ 0x11, 0x22, 0x33 } },
        .{ .proc_id = u8x7_proc, .expected = &.{ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47 } },
        .{ .proc_id = u32x3_proc, .expected = &.{ 0x04, 0x03, 0x02, 0x01, 0x08, 0x07, 0x06, 0x05, 0x0C, 0x0B, 0x0A, 0x09 } },
    };

    for (cases) |case| {
        const compiled = codegen.proc_registry.get(@intFromEnum(case.proc_id)) orelse return error.TestUnexpectedResult;
        const callable: builtins.erased_callable.ErasedCallableFn = @ptrCast(@alignCast(executable.codePtr() + compiled.code_start));

        // Sentinel bytes on both sides of the result slot; the callee owns
        // only ret_buf[8 .. 8 + expected.len].
        var ret_buf align(16) = [_]u8{0xAA} ** 32;
        var out_desc: ?*const anyopaque = null;
        callable(&roc_ops, ret_buf[8..].ptr, null, null, null, &out_desc);

        try std.testing.expectEqualSlices(u8, case.expected, ret_buf[8 .. 8 + case.expected.len]);
        for (ret_buf, 0..) |byte, i| {
            if (i >= 8 and i < 8 + case.expected.len) continue;
            try std.testing.expectEqual(@as(u8, 0xAA), byte);
        }
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
    var codegen = try WinCodeGen.init(allocator, &store, &layout_store, .{}, &.{}, .preserve, .default);
    defer codegen.deinit();
    codegen.generation_mode = .object_file;

    try codegen.compileAllProcSpecs(store.getProcSpecs());

    // MOVDQU m128, XMM0 is the unaligned full-width store into the result slot.
    const code = codegen.getGeneratedCode();
    var return_code: ?[]const u8 = null;
    for (codegen.getRelocations()) |relocation| {
        if (relocation == .linked_function and std.mem.eql(u8, codegen.getSymbolNames()[@intFromEnum(relocation.linked_function.symbol)], "hosted_u128_identity")) {
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
    var codegen = try WinCodeGen.init(allocator, &store, &layout_store, .{}, &.{}, .preserve, .default);
    defer codegen.deinit();
    codegen.generation_mode = .object_file;

    try codegen.compileAllProcSpecs(store.getProcSpecs());
    const entrypoint = try codegen.generateEntrypointWrapper("roc_u128_identity", proc, &.{}, .u128);
    const code = codegen.getGeneratedCode();
    const entrypoint_code = code[entrypoint.offset..][0..entrypoint.size];

    // MOVDQU XMM0, m128 returns the complete value using clang/Rust's convention.
    try std.testing.expect(std.mem.find(u8, entrypoint_code, &.{ 0xF3, 0x0F, 0x6F, 0x85 }) != null);
}
