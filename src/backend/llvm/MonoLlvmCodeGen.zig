//! Statement-LIR to LLVM code generator.
//!
//! This backend intentionally uses a small internal ABI:
//!
//!     void roc_proc_N(*RocOps, ret_ptr, args_ptr)
//!
//! Direct Roc calls pack argument bytes into `args_ptr` using Roc's canonical
//! alignment order, and callees write results into caller-owned storage. This
//! avoids target-specific aggregate return and argument rules while still
//! letting LLVM optimize ordinary local stack traffic.

const std = @import("std");
const builtin = @import("builtin");
const builtins = @import("builtins");
const layout = @import("layout");
const lir = @import("lir");

const CoreCtx = @import("ctx").CoreCtx;
const LlvmBuilder = @import("Builder.zig");

const Allocator = std.mem.Allocator;
const CFStmtId = lir.CFStmtId;
const LocalId = lir.LocalId;
const LocalSpan = lir.LocalSpan;
const LirProcSpec = lir.LirProcSpec;
const LirProcSpecId = lir.LirProcSpecId;
const StrLiteral = lir.LIR.StrLiteral;

fn getLlvmTriple(target: std.Target) []const u8 {
    return switch (target.cpu.arch) {
        .x86_64 => switch (target.os.tag) {
            .windows => if (target.abi == .msvc) "x86_64-pc-windows-msvc" else "x86_64-w64-windows-gnu",
            .macos => "x86_64-apple-macosx13.0.0",
            .linux => switch (target.abi) {
                .musl => "x86_64-unknown-linux-musl",
                .android => "x86_64-unknown-linux-android",
                else => "x86_64-unknown-linux-gnu",
            },
            .freebsd => "x86_64-unknown-freebsd",
            .openbsd => "x86_64-unknown-openbsd",
            .netbsd => "x86_64-unknown-netbsd",
            .freestanding => "x86_64-unknown-unknown",
            else => "x86_64-unknown-unknown",
        },
        .aarch64 => switch (target.os.tag) {
            .windows => if (target.abi == .msvc) "aarch64-pc-windows-msvc" else "aarch64-w64-windows-gnu",
            .macos => "aarch64-apple-macosx13.0.0",
            .ios => "aarch64-apple-ios",
            .linux => switch (target.abi) {
                .musl => "aarch64-unknown-linux-musl",
                .android => "aarch64-unknown-linux-android",
                else => "aarch64-unknown-linux-gnu",
            },
            .freebsd => "aarch64-unknown-freebsd",
            .openbsd => "aarch64-unknown-openbsd",
            .netbsd => "aarch64-unknown-netbsd",
            .freestanding => "aarch64-unknown-unknown",
            else => "aarch64-unknown-unknown",
        },
        .x86 => switch (target.os.tag) {
            .windows => if (target.abi == .msvc) "i686-pc-windows-msvc" else "i686-w64-windows-gnu",
            .linux => switch (target.abi) {
                .musl => "i686-unknown-linux-musl",
                .android => "i686-unknown-linux-android",
                else => "i686-unknown-linux-gnu",
            },
            .freestanding => "i686-unknown-unknown",
            else => "i686-unknown-unknown",
        },
        .arm, .armeb, .thumb, .thumbeb => switch (target.os.tag) {
            .linux => switch (target.abi) {
                .musleabihf => "arm-unknown-linux-musleabihf",
                .gnueabihf => "arm-unknown-linux-gnueabihf",
                .musleabi => "arm-unknown-linux-musleabi",
                .gnueabi => "arm-unknown-linux-gnueabi",
                else => "arm-unknown-linux-gnueabihf",
            },
            .freestanding => "arm-unknown-unknown",
            else => "arm-unknown-unknown",
        },
        .wasm32 => switch (target.os.tag) {
            .wasi => "wasm32-wasi",
            else => "wasm32-unknown-unknown",
        },
        .wasm64 => switch (target.os.tag) {
            .wasi => "wasm64-wasi",
            else => "wasm64-unknown-unknown",
        },
        .riscv32 => "riscv32-unknown-unknown",
        .riscv64 => "riscv64-unknown-unknown",
        else => "unknown-unknown-unknown",
    };
}

/// Lowers statement-only LIR procedures to LLVM bitcode.
pub const MonoLlvmCodeGen = struct {
    allocator: Allocator,
    target: std.Target,
    triple: []const u8,
    builtin_symbol_mode: BuiltinSymbolMode = .bitcode,
    store: *const lir.LirStore,

    /// Layout store for resolving composite type layouts (records, tuples).
    /// Set by the evaluator before calling generateCode.
    layout_store: ?*const layout.Store = null,

    builder: ?*LlvmBuilder = null,
    wip: ?*LlvmBuilder.WipFunction = null,
    roc_ops_arg: ?LlvmBuilder.Value = null,
    ret_ptr_arg: ?LlvmBuilder.Value = null,
    args_ptr_arg: ?LlvmBuilder.Value = null,
    capture_ptr_arg: ?LlvmBuilder.Value = null,
    current_ret_layout: layout.Idx = .zst,

    proc_registry: std.AutoHashMap(u32, LlvmBuilder.Function.Index),
    builtin_functions: std.StringHashMap(LlvmBuilder.Function.Index),
    rc_helpers: std.AutoHashMap(u64, RcHelperEntry),
    join_points: std.AutoHashMap(u32, JoinInfo),
    compiled_joins: std.AutoHashMap(u32, void),
    loop_continue_blocks: std.ArrayList(LlvmBuilder.Function.Block.Index),
    loop_break_blocks: std.ArrayList(LlvmBuilder.Function.Block.Index),
    local_slots: []LocalSlot = &.{},
    string_counter: u32 = 0,

    /// Errors reported while building LLVM IR.
    pub const Error = error{
        OutOfMemory,
        CompilationFailed,
        UnsupportedLowLevel,
    };

    /// Owned serialized LLVM bitcode produced by this backend.
    pub const GenerateResult = struct {
        bitcode: []const u32,
        allocator: Allocator,

        /// Releases the serialized bitcode buffer.
        pub fn deinit(self: *GenerateResult) void {
            self.allocator.free(self.bitcode);
        }
    };

    /// Backwards-compatible alias for entrypoint module generation.
    pub const ModuleBitcodeResult = GenerateResult;

    const BuiltinSymbolMode = enum {
        bitcode,
        native_object,
    };

    const LocalSlot = struct {
        ptr: LlvmBuilder.Value,
        layout_idx: layout.Idx,
        size: u32,
        alignment: LlvmBuilder.Alignment,
    };

    const JoinInfo = struct {
        block: LlvmBuilder.Function.Block.Index,
        params: LocalSpan,
        body: CFStmtId,
    };

    const StrFromUtf8LayoutInfo = struct {
        ok_tag: u16,
        err_tag: u16,
        outer_disc_offset: u32,
        outer_disc_size: u32,
        err_index_offset: u32,
        err_problem_offset: u32,
        inner_disc_offset: u32,
        inner_disc_size: u32,
        inner_bad_utf8_tag: u32,
    };

    const RcHelperEntry = struct {
        key: layout.RcHelperKey,
        function: LlvmBuilder.Function.Index,
        compiled: bool = false,
    };

    const ArgOrder = struct {
        index: usize,
        alignment: u32,
        size: u32,
    };

    const ResolvedBase = struct {
        ptr: LlvmBuilder.Value,
        layout_idx: layout.Idx,
    };

    /// Initializes the backend for the host target.
    pub fn init(allocator: Allocator, store: *const lir.LirStore) MonoLlvmCodeGen {
        return .{
            .allocator = allocator,
            .target = builtin.target,
            .triple = getLlvmTriple(builtin.target),
            .store = store,
            .proc_registry = std.AutoHashMap(u32, LlvmBuilder.Function.Index).init(allocator),
            .builtin_functions = std.StringHashMap(LlvmBuilder.Function.Index).init(allocator),
            .rc_helpers = std.AutoHashMap(u64, RcHelperEntry).init(allocator),
            .join_points = std.AutoHashMap(u32, JoinInfo).init(allocator),
            .compiled_joins = std.AutoHashMap(u32, void).init(allocator),
            .loop_continue_blocks = .empty,
            .loop_break_blocks = .empty,
        };
    }

    /// Initializes the backend for an explicit target.
    pub fn initWithTarget(allocator: Allocator, store: *const lir.LirStore, target: std.Target) MonoLlvmCodeGen {
        var self = init(allocator, store);
        self.target = target;
        self.triple = getLlvmTriple(target);
        return self;
    }

    /// Releases backend-owned scratch maps.
    pub fn deinit(self: *MonoLlvmCodeGen) void {
        self.proc_registry.deinit();
        self.builtin_functions.deinit();
        self.rc_helpers.deinit();
        self.join_points.deinit();
        self.compiled_joins.deinit();
        self.loop_continue_blocks.deinit(self.allocator);
        self.loop_break_blocks.deinit(self.allocator);
    }

    /// Clears per-module caches while retaining allocated capacity.
    pub fn reset(self: *MonoLlvmCodeGen) void {
        self.proc_registry.clearRetainingCapacity();
        self.builtin_functions.clearRetainingCapacity();
        self.rc_helpers.clearRetainingCapacity();
        self.join_points.clearRetainingCapacity();
        self.compiled_joins.clearRetainingCapacity();
        self.loop_continue_blocks.clearRetainingCapacity();
        self.loop_break_blocks.clearRetainingCapacity();
        self.string_counter = 0;
    }

    /// Generates a single eval-style module for `root_proc`.
    pub fn generateCode(
        self: *MonoLlvmCodeGen,
        root_proc: LirProcSpecId,
        result_layout: layout.Idx,
    ) Error!GenerateResult {
        const proc = self.store.getProcSpec(root_proc);
        const arg_layouts = try self.procArgLayouts(proc, proc.abi == .erased_callable);
        defer self.allocator.free(arg_layouts);
        return self.generateEntrypointModule("roc_eval_module", &.{
            .{
                .symbol_name = "roc_eval",
                .proc = root_proc,
                .arg_layouts = arg_layouts,
                .ret_layout = result_layout,
            },
        });
    }

    /// Generates a module with exported wrappers for the requested entrypoints.
    pub fn generateEntrypointModule(
        self: *MonoLlvmCodeGen,
        module_name: []const u8,
        entrypoints: anytype,
    ) Error!ModuleBitcodeResult {
        self.reset();

        var builder = try self.createBuilder(module_name);
        defer builder.deinit();

        self.builder = &builder;
        defer self.builder = null;

        const procs = self.store.getProcSpecs();
        try self.compileAllProcSpecs(procs);
        try self.compilePendingRcHelpers();

        inline for (entrypoints) |entrypoint| {
            try self.generateEntrypointWrapper(
                entrypoint.symbol_name,
                entrypoint.proc,
                entrypoint.arg_layouts,
            );
        }

        return .{
            .bitcode = try self.serializeBuilderToBitcode(&builder),
            .allocator = self.allocator,
        };
    }

    /// Declares and compiles every procedure in dependency-index order.
    pub fn compileAllProcSpecs(self: *MonoLlvmCodeGen, procs: []const LirProcSpec) Error!void {
        for (procs, 0..) |proc, i| {
            try self.declareProcSpec(@enumFromInt(@as(u32, @intCast(i))), proc);
        }
        for (procs, 0..) |proc, i| {
            try self.compileProcBody(@enumFromInt(@as(u32, @intCast(i))), proc);
        }
    }

    fn declareProcSpec(self: *MonoLlvmCodeGen, proc_id: LirProcSpecId, proc: LirProcSpec) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const ptr_ty = builder.ptrType(.default) catch return error.OutOfMemory;
        const params: []const LlvmBuilder.Type = if (proc.abi == .erased_callable)
            &.{ ptr_ty, ptr_ty, ptr_ty, ptr_ty }
        else
            &.{ ptr_ty, ptr_ty, ptr_ty };
        const fn_ty = builder.fnType(.void, params, .normal) catch return error.OutOfMemory;
        const name = builder.strtabStringFmt("roc_proc_{d}", .{@intFromEnum(proc_id)}) catch return error.OutOfMemory;
        const func = builder.addFunction(fn_ty, name, .default) catch return error.OutOfMemory;
        func.setLinkage(.internal, builder);
        try self.proc_registry.put(@intFromEnum(proc_id), func);
    }

    fn compileProcBody(self: *MonoLlvmCodeGen, proc_id: LirProcSpecId, proc: LirProcSpec) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const func = self.proc_registry.get(@intFromEnum(proc_id)) orelse return error.CompilationFailed;

        const outer_wip = self.wip;
        const outer_roc_ops = self.roc_ops_arg;
        const outer_ret = self.ret_ptr_arg;
        const outer_args = self.args_ptr_arg;
        const outer_capture = self.capture_ptr_arg;
        const outer_ret_layout = self.current_ret_layout;
        const outer_slots = self.local_slots;
        defer {
            self.wip = outer_wip;
            self.roc_ops_arg = outer_roc_ops;
            self.ret_ptr_arg = outer_ret;
            self.args_ptr_arg = outer_args;
            self.capture_ptr_arg = outer_capture;
            self.current_ret_layout = outer_ret_layout;
            self.local_slots = outer_slots;
        }

        self.join_points.clearRetainingCapacity();
        self.compiled_joins.clearRetainingCapacity();
        self.loop_continue_blocks.clearRetainingCapacity();
        self.loop_break_blocks.clearRetainingCapacity();

        var wip = LlvmBuilder.WipFunction.init(builder, .{ .function = func, .strip = true }) catch return error.OutOfMemory;
        defer wip.deinit();
        self.wip = &wip;

        const entry = wip.block(0, "entry") catch return error.OutOfMemory;
        wip.cursor = .{ .block = entry };

        self.roc_ops_arg = wip.arg(0);
        self.ret_ptr_arg = wip.arg(1);
        self.args_ptr_arg = wip.arg(2);
        self.capture_ptr_arg = if (proc.abi == .erased_callable) wip.arg(3) else null;
        self.current_ret_layout = proc.ret_layout;

        self.local_slots = try self.allocator.alloc(LocalSlot, self.store.locals.items.len);
        defer self.allocator.free(self.local_slots);
        try self.allocLocalSlots();
        try self.unpackProcArgs(proc);

        if (proc.hosted) |hosted| {
            try self.emitHostedProcBody(hosted, proc);
        } else {
            const body = proc.body orelse return error.CompilationFailed;
            try self.compileStmt(body);
            if (!self.currentBlockHasTerminator()) {
                _ = wip.retVoid() catch return error.OutOfMemory;
            }
        }

        try self.finishCurrentWipFunction();
    }

    fn generateEntrypointWrapper(
        self: *MonoLlvmCodeGen,
        symbol_name: []const u8,
        entry_proc: LirProcSpecId,
        arg_layouts: []const layout.Idx,
    ) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const proc_fn = self.proc_registry.get(@intFromEnum(entry_proc)) orelse return error.CompilationFailed;
        const ptr_ty = builder.ptrType(.default) catch return error.OutOfMemory;
        const wrapper_ty = builder.fnType(.void, &.{ ptr_ty, ptr_ty, ptr_ty }, .normal) catch return error.OutOfMemory;
        const wrapper_name = try self.exportedFunctionName(builder, symbol_name);
        const wrapper = builder.addFunction(wrapper_ty, wrapper_name, .default) catch return error.OutOfMemory;
        wrapper.setLinkage(.external, builder);
        self.configureExportCallConv(wrapper, builder);

        const outer_wip = self.wip;
        const outer_roc_ops = self.roc_ops_arg;
        defer {
            self.wip = outer_wip;
            self.roc_ops_arg = outer_roc_ops;
        }

        var wip = LlvmBuilder.WipFunction.init(builder, .{ .function = wrapper, .strip = true }) catch return error.OutOfMemory;
        defer wip.deinit();
        self.wip = &wip;

        const entry = wip.block(0, "entry") catch return error.OutOfMemory;
        wip.cursor = .{ .block = entry };

        const roc_ops = wip.arg(0);
        const ret_ptr = wip.arg(1);
        const args_ptr = wip.arg(2);
        self.roc_ops_arg = roc_ops;

        const args_buf = try self.allocArgBuffer(arg_layouts, true);
        try self.copyEntrypointArgsToInternalBuffer(args_ptr, args_buf, arg_layouts);
        _ = try self.callFunctionIndex(proc_fn, &.{ roc_ops, ret_ptr, args_buf });
        _ = wip.retVoid() catch return error.OutOfMemory;
        try self.finishCurrentWipFunction();
    }

    fn createBuilder(self: *MonoLlvmCodeGen, name: []const u8) Error!LlvmBuilder {
        return LlvmBuilder.init(.{
            .allocator = self.allocator,
            .name = name,
            .target = &self.target,
            .triple = self.triple,
        }) catch return error.OutOfMemory;
    }

    fn exportedFunctionName(self: *MonoLlvmCodeGen, builder: *LlvmBuilder, name: []const u8) Error!LlvmBuilder.StrtabString {
        if (self.target.os.tag != .macos) {
            return builder.strtabString(name) catch return error.OutOfMemory;
        }
        const exact_name = try std.fmt.allocPrint(self.allocator, "\x01_{s}", .{name});
        defer self.allocator.free(exact_name);
        return builder.strtabString(exact_name) catch return error.OutOfMemory;
    }

    fn configureExportCallConv(self: *MonoLlvmCodeGen, func: LlvmBuilder.Function.Index, builder: *LlvmBuilder) void {
        if (self.target.os.tag == .windows) {
            func.ptrConst(builder).global.setDllStorageClass(.dllexport, builder);
        }
        if (self.target.cpu.arch != .x86_64) return;
        if (self.target.os.tag == .windows) {
            func.setCallConv(.win64cc, builder);
        } else {
            func.setCallConv(.x86_64_sysvcc, builder);
        }
    }

    fn serializeBuilderToBitcode(self: *MonoLlvmCodeGen, builder: *LlvmBuilder) Error![]const u32 {
        const producer = LlvmBuilder.Producer{
            .name = "Roc statement LLVM CodeGen",
            .version = .{ .major = 1, .minor = 0, .patch = 0 },
        };
        const environ: std.process.Environ = if (builtin.os.tag == .windows)
            .{ .block = .global }
        else blk: {
            const env_ptr: [*:null]const ?[*:0]const u8 = @ptrCast(std.c.environ);
            break :blk .{ .block = .{ .slice = std.mem.sliceTo(env_ptr, null) } };
        };
        if (environ.getAlloc(self.allocator, "ROC_LLVM_KEEP_IR")) |keep_path| {
            defer self.allocator.free(keep_path);
            // Render the IR into a buffer and write it through the CoreCtx
            // filesystem abstraction rather than reaching into the cwd directory
            // handle directly, keeping compiler-core decoupled from the OS I/O layer.
            var ir_text: std.Io.Writer.Allocating = .init(self.allocator);
            defer ir_text.deinit();
            builder.print(&ir_text.writer) catch return error.CompilationFailed;
            CoreCtx.writeFileCwd(std.Options.debug_io, keep_path, ir_text.written()) catch return error.CompilationFailed;
        } else |_| {}
        return builder.toBitcode(self.allocator, producer) catch return error.OutOfMemory;
    }

    fn procArgLayouts(self: *MonoLlvmCodeGen, proc: LirProcSpec, exclude_erased_capture: bool) Error![]layout.Idx {
        const params = self.store.getLocalSpan(proc.args);
        const count = if (exclude_erased_capture and params.len != 0) params.len - 1 else params.len;
        const result = try self.allocator.alloc(layout.Idx, count);
        for (params[0..count], result) |local, *layout_slot| {
            layout_slot.* = self.store.getLocal(local).layout_idx;
        }
        return result;
    }

    fn allocLocalSlots(self: *MonoLlvmCodeGen) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        for (self.store.locals.items, self.local_slots) |local, *local_slot| {
            const sa = self.sizeAlignOf(local.layout_idx);
            const len = builder.intValue(.i32, @max(sa.size, 1)) catch return error.OutOfMemory;
            const ptr = wip.alloca(.normal, .i8, len, self.llvmAlignment(sa.alignment), .default, "local") catch return error.OutOfMemory;
            local_slot.* = .{
                .ptr = ptr,
                .layout_idx = local.layout_idx,
                .size = sa.size,
                .alignment = self.llvmAlignment(sa.alignment),
            };
            if (sa.size > 0) try self.zeroBytes(ptr, sa.size);
        }
    }

    fn unpackProcArgs(self: *MonoLlvmCodeGen, proc: LirProcSpec) Error!void {
        const params = self.store.getLocalSpan(proc.args);
        const explicit_count = if (proc.abi == .erased_callable and params.len != 0) params.len - 1 else params.len;
        const arg_layouts = try self.procArgLayouts(proc, proc.abi == .erased_callable);
        defer self.allocator.free(arg_layouts);
        const offsets = try self.computeArgOffsets(arg_layouts, true);
        defer self.allocator.free(offsets);

        const args_ptr = self.args_ptr_arg orelse return error.CompilationFailed;
        for (params[0..explicit_count], offsets) |param, offset| {
            const param_slot = self.slot(param);
            if (param_slot.size == 0) continue;
            const src = try self.offsetPtr(args_ptr, offset);
            try self.copyBytes(param_slot.ptr, src, param_slot.size, param_slot.alignment);
        }

        if (proc.abi == .erased_callable and params.len != 0) {
            const capture_param = params[params.len - 1];
            const capture_ptr = self.capture_ptr_arg orelse return error.CompilationFailed;
            try self.storePointer(self.slot(capture_param).ptr, capture_ptr);
        }
    }

    fn emitHostedProcBody(self: *MonoLlvmCodeGen, hosted: lir.LIR.HostedProc, proc: LirProcSpec) Error!void {
        const params = self.store.getLocalSpan(proc.args);
        const arg_layouts = try self.procArgLayouts(proc, false);
        defer self.allocator.free(arg_layouts);
        const args_buf = try self.allocHostedArgBuffer(arg_layouts);
        try self.packSequentialArgsFromLocals(args_buf, params, arg_layouts);
        const ret_ptr = self.ret_ptr_arg orelse return error.CompilationFailed;
        try self.callHostedFunction(hosted.dispatch_index, ret_ptr, args_buf);
        const wip = self.wip orelse return error.CompilationFailed;
        _ = wip.retVoid() catch return error.OutOfMemory;
    }

    fn compileStmt(self: *MonoLlvmCodeGen, stmt_id: CFStmtId) Error!void {
        if (self.currentBlockHasTerminator()) return;
        const stmt = self.store.getCFStmt(stmt_id);
        switch (stmt) {
            .assign_ref => |assign| {
                try self.emitAssignRef(assign.target, assign.op);
                try self.compileStmt(assign.next);
            },
            .assign_literal => |assign| {
                try self.emitLiteral(assign.target, assign.value);
                try self.compileStmt(assign.next);
            },
            .assign_call => |assign| {
                try self.emitDirectCall(assign.target, assign.proc, assign.args);
                try self.compileStmt(assign.next);
            },
            .assign_call_erased => |assign| {
                try self.emitErasedCall(assign.target, assign.closure, assign.args);
                try self.compileStmt(assign.next);
            },
            .assign_packed_erased_fn => |assign| {
                try self.emitPackedErasedFn(assign.target, assign.proc, assign.capture, assign.capture_layout, assign.on_drop);
                try self.compileStmt(assign.next);
            },
            .assign_low_level => |assign| {
                try self.emitLowLevel(assign.target, assign.op, assign.args);
                try self.compileStmt(assign.next);
            },
            .assign_list => |assign| {
                try self.emitListLiteral(assign.target, assign.elems);
                try self.compileStmt(assign.next);
            },
            .assign_struct => |assign| {
                try self.emitStructLiteral(assign.target, assign.fields);
                try self.compileStmt(assign.next);
            },
            .assign_tag => |assign| {
                try self.emitTagLiteral(assign.target, assign.discriminant, assign.payload);
                try self.compileStmt(assign.next);
            },
            .set_local => |assign| {
                try self.copyLocal(assign.target, assign.value);
                try self.compileStmt(assign.next);
            },
            .debug => |debug_stmt| {
                try self.callBuiltinVoid("roc_builtins_dbg_str", &.{ try self.ptrType(), try self.ptrType() }, &.{ self.slot(debug_stmt.message).ptr, self.rocOps() });
                try self.compileStmt(debug_stmt.next);
            },
            .expect => |expect_stmt| {
                try self.emitExpect(expect_stmt.condition);
                try self.compileStmt(expect_stmt.next);
            },
            .runtime_error => {
                try self.emitCrashBytes("hit a runtime error");
            },
            .incref => |inc| {
                try self.emitRcForLocal(.incref, inc.value, inc.count);
                try self.compileStmt(inc.next);
            },
            .decref => |dec| {
                try self.emitRcForLocal(.decref, dec.value, 1);
                try self.compileStmt(dec.next);
            },
            .free => |free_stmt| {
                try self.emitRcForLocal(.free, free_stmt.value, 1);
                try self.compileStmt(free_stmt.next);
            },
            .switch_stmt => |sw| try self.emitSwitch(sw),
            .loop_continue => try self.emitLoopContinue(),
            .loop_break => try self.emitLoopBreak(),
            .join => |join_stmt| try self.emitJoin(join_stmt),
            .jump => |jump_stmt| try self.emitJump(jump_stmt),
            .ret => |ret_stmt| try self.emitReturn(ret_stmt.value),
            .crash => |crash_stmt| try self.emitCrashBytes(self.store.getString(crash_stmt.msg)),
        }
    }

    fn emitAssignRef(self: *MonoLlvmCodeGen, target: LocalId, op: lir.LIR.RefOp) Error!void {
        const target_slot = self.slot(target);
        switch (op) {
            .local => |source| try self.copyLocal(target, source),
            .list_reinterpret => |ref| try self.copyLocal(target, ref.backing_ref),
            .nominal => |ref| try self.copyLocal(target, ref.backing_ref),
            .discriminant => |ref| {
                const base = try self.resolveTagBase(ref.source);
                const discrim = try self.readTagDiscriminant(base.ptr, base.layout_idx);
                try self.storeIntToLayout(target_slot.ptr, discrim, target_slot.layout_idx);
            },
            .field => |ref| {
                const base = try self.resolveStructBase(ref.source);
                const base_layout = self.layoutValue(base.layout_idx);
                if (base_layout.tag != .struct_) return error.CompilationFailed;
                const offset = self.layouts().getStructFieldOffsetByOriginalIndex(base_layout.getStruct().idx, ref.field_idx);
                const field_layout = self.layouts().getStructFieldLayoutByOriginalIndex(base_layout.getStruct().idx, ref.field_idx);
                const src = try self.offsetPtr(base.ptr, offset);
                try self.copyBytes(target_slot.ptr, src, self.layoutByteSize(field_layout), self.alignmentForLayout(field_layout));
            },
            .tag_payload => |ref| {
                const base = try self.resolveTagBase(ref.source);
                const payload_layout = self.tagPayloadLayout(base.layout_idx, ref.tag_discriminant);
                const payload_layout_val = self.layoutValue(payload_layout);
                var src = base.ptr;
                var copy_layout = payload_layout;
                if (payload_layout_val.tag == .struct_) {
                    const offset = self.layouts().getStructFieldOffsetByOriginalIndex(payload_layout_val.getStruct().idx, ref.payload_idx);
                    src = try self.offsetPtr(base.ptr, offset);
                    copy_layout = self.layouts().getStructFieldLayoutByOriginalIndex(payload_layout_val.getStruct().idx, ref.payload_idx);
                }
                try self.copyBytes(target_slot.ptr, src, self.layoutByteSize(copy_layout), self.alignmentForLayout(copy_layout));
            },
            .tag_payload_struct => |ref| {
                const base = try self.resolveTagBase(ref.source);
                const payload_layout = self.tagPayloadLayout(base.layout_idx, ref.tag_discriminant);
                try self.copyBytes(target_slot.ptr, base.ptr, self.layoutByteSize(payload_layout), self.alignmentForLayout(payload_layout));
            },
        }
    }

    fn emitLiteral(self: *MonoLlvmCodeGen, target: LocalId, value: lir.LIR.LiteralValue) Error!void {
        const slot_v = self.slot(target);
        switch (value) {
            .i64_literal => |lit| try self.storeIntLiteral(slot_v.ptr, slot_v.layout_idx, lit.value),
            .i128_literal => |lit| try self.storeI128Literal(slot_v.ptr, slot_v.layout_idx, lit.value),
            .f64_literal => |lit| try self.storeFloatLiteral(slot_v.ptr, .f64, lit),
            .f32_literal => |lit| try self.storeFloatLiteral(slot_v.ptr, .f32, lit),
            .dec_literal => |lit| try self.storeI128Literal(slot_v.ptr, .dec, lit),
            .str_literal => |str_idx| try self.emitStrLiteral(slot_v.ptr, str_idx),
            .null_ptr => {
                if (slot_v.size > 0) try self.zeroBytes(slot_v.ptr, slot_v.size);
            },
            .proc_ref => |proc_id| {
                const func = self.proc_registry.get(@intFromEnum(proc_id)) orelse return error.CompilationFailed;
                try self.storePointer(slot_v.ptr, func.toValue(self.builder.?));
            },
        }
    }

    fn emitDirectCall(self: *MonoLlvmCodeGen, target: LocalId, proc_id: LirProcSpecId, args: LocalSpan) Error!void {
        const proc = self.store.getProcSpec(proc_id);
        const arg_locals = self.store.getLocalSpan(args);
        const param_locals = self.store.getLocalSpan(proc.args);
        if (arg_locals.len != param_locals.len) return error.CompilationFailed;
        if (proc.hosted) |hosted| {
            const arg_layouts = try self.allocator.alloc(layout.Idx, arg_locals.len);
            defer self.allocator.free(arg_layouts);
            for (param_locals, arg_layouts) |param, *slot_layout| slot_layout.* = self.store.getLocal(param).layout_idx;
            const args_buf = try self.allocHostedArgBuffer(arg_layouts);
            try self.packSequentialArgsFromLocals(args_buf, arg_locals, arg_layouts);
            try self.callHostedFunction(hosted.dispatch_index, self.slot(target).ptr, args_buf);
            return;
        }

        const arg_layouts = try self.allocator.alloc(layout.Idx, arg_locals.len);
        defer self.allocator.free(arg_layouts);
        for (param_locals, arg_layouts) |param, *slot_layout| slot_layout.* = self.store.getLocal(param).layout_idx;
        const args_buf = try self.allocArgBuffer(arg_layouts, true);
        try self.packRocArgsFromLocals(args_buf, arg_locals, arg_layouts);
        const func = self.proc_registry.get(@intFromEnum(proc_id)) orelse return error.CompilationFailed;
        _ = try self.callFunctionIndex(func, &.{ self.rocOps(), self.slot(target).ptr, args_buf });
    }

    fn emitErasedCall(self: *MonoLlvmCodeGen, target: LocalId, closure: LocalId, args: LocalSpan) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const ptr_ty = try self.ptrType();
        const closure_ptr = try self.loadPointer(self.slot(closure).ptr);
        const fn_ptr = try self.loadPointer(closure_ptr);
        const capture_ptr = try self.offsetPtr(closure_ptr, builtins.erased_callable.capture_offset);
        const arg_locals = self.store.getLocalSpan(args);
        const arg_layouts = try self.allocator.alloc(layout.Idx, arg_locals.len);
        defer self.allocator.free(arg_layouts);
        for (arg_locals, arg_layouts) |local, *slot_layout| slot_layout.* = self.localLayout(local);
        const args_buf = if (arg_locals.len == 0)
            builder.nullValue(ptr_ty) catch return error.OutOfMemory
        else blk: {
            const buf = try self.allocHostedArgBuffer(arg_layouts);
            try self.packSequentialArgsFromLocals(buf, arg_locals, arg_layouts);
            break :blk buf;
        };
        const ret_ptr = if (self.slot(target).size == 0)
            builder.nullValue(ptr_ty) catch return error.OutOfMemory
        else
            self.slot(target).ptr;
        const fn_ty = builder.fnType(.void, &.{ ptr_ty, ptr_ty, ptr_ty, ptr_ty }, .normal) catch return error.OutOfMemory;
        _ = wip.call(.normal, .ccc, .none, fn_ty, fn_ptr, &.{ self.rocOps(), ret_ptr, args_buf, capture_ptr }, "") catch return error.OutOfMemory;
    }

    fn emitPackedErasedFn(
        self: *MonoLlvmCodeGen,
        target: LocalId,
        proc_id: LirProcSpecId,
        capture: ?LocalId,
        capture_layout: ?layout.Idx,
        on_drop: lir.LIR.ErasedCallableOnDrop,
    ) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const capture_size = if (capture_layout) |idx| self.layoutByteSize(idx) else 0;
        const payload_size: u64 = builtins.erased_callable.payloadSize(capture_size);
        const data_ptr = try self.callBuiltin(
            "roc_builtins_allocate_with_refcount",
            try self.ptrType(),
            &.{ self.ptrSizedIntType(), .i32, .i1, try self.ptrType() },
            &.{
                builder.intValue(self.ptrSizedIntType(), payload_size) catch return error.OutOfMemory,
                builder.intValue(.i32, builtins.erased_callable.payload_alignment) catch return error.OutOfMemory,
                builder.intValue(.i1, 0) catch return error.OutOfMemory,
                self.rocOps(),
            },
        );
        const proc_fn = self.proc_registry.get(@intFromEnum(proc_id)) orelse return error.CompilationFailed;
        try self.storePointer(data_ptr, proc_fn.toValue(builder));
        const on_drop_ptr = try self.offsetPtr(data_ptr, @sizeOf(usize));
        switch (on_drop) {
            .none => try self.storePointer(on_drop_ptr, builder.nullValue(try self.ptrType()) catch return error.OutOfMemory),
            .rc_helper => |helper_key| {
                const helper_value = if (try self.declareRcHelper(helper_key)) |helper_fn|
                    helper_fn.toValue(builder)
                else
                    builder.nullValue(try self.ptrType()) catch return error.OutOfMemory;
                try self.storePointer(on_drop_ptr, helper_value);
            },
            .interpreter_context_drop => return error.CompilationFailed,
        }
        if (capture) |capture_local| {
            if (capture_size > 0) {
                const capture_dst = try self.offsetPtr(data_ptr, builtins.erased_callable.capture_offset);
                try self.copyBytes(capture_dst, self.slot(capture_local).ptr, capture_size, self.alignmentForLayout(capture_layout.?));
            }
        }
        try self.storePointer(self.slot(target).ptr, data_ptr);
    }

    fn emitListLiteral(self: *MonoLlvmCodeGen, target: LocalId, elems: LocalSpan) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const elem_locals = self.store.getLocalSpan(elems);
        const target_layout = self.localLayout(target);
        const abi = self.layouts().builtinListAbi(target_layout);
        const target_ptr = self.slot(target).ptr;
        if (elem_locals.len == 0) {
            try self.zeroBytes(target_ptr, self.layoutByteSize(target_layout));
            return;
        }
        if (abi.elem_size == 0) {
            try self.storeListFields(target_ptr, builder.nullValue(try self.ptrType()) catch return error.OutOfMemory, elem_locals.len, elem_locals.len << 1);
            return;
        }
        try self.callBuiltinVoid(
            "roc_builtins_list_with_capacity",
            &.{ try self.ptrType(), self.ptrSizedIntType(), .i32, self.ptrSizedIntType(), .i1, try self.ptrType() },
            &.{
                target_ptr,
                builder.intValue(self.ptrSizedIntType(), elem_locals.len) catch return error.OutOfMemory,
                builder.intValue(.i32, abi.elem_alignment) catch return error.OutOfMemory,
                builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory,
                builder.intValue(.i1, @intFromBool(abi.contains_refcounted)) catch return error.OutOfMemory,
                self.rocOps(),
            },
        );
        const bytes_ptr = try self.loadPointer(target_ptr);
        for (elem_locals, 0..) |elem_local, i| {
            const dst = try self.offsetPtr(bytes_ptr, @as(u32, @intCast(i)) * abi.elem_size);
            try self.copyBytes(dst, self.slot(elem_local).ptr, abi.elem_size, self.alignmentForLayout(abi.elem_layout_idx.?));
        }
        try self.storeScalar(
            try self.offsetPtr(target_ptr, @sizeOf(usize)),
            .u64,
            builder.intValue(self.ptrSizedIntType(), elem_locals.len) catch return error.OutOfMemory,
        );
    }

    fn emitStructLiteral(self: *MonoLlvmCodeGen, target: LocalId, fields: LocalSpan) Error!void {
        const field_locals = self.store.getLocalSpan(fields);
        const allocated = try self.allocAggregateTarget(target);
        const base_layout = self.layoutValue(allocated.layout_idx);
        if (base_layout.tag != .struct_) return;
        for (field_locals, 0..) |field_local, i| {
            const field_layout = self.layouts().getStructFieldLayoutByOriginalIndex(base_layout.getStruct().idx, @intCast(i));
            const field_size = self.layoutByteSize(field_layout);
            if (field_size == 0) continue;
            const offset = self.layouts().getStructFieldOffsetByOriginalIndex(base_layout.getStruct().idx, @intCast(i));
            const dst = try self.offsetPtr(allocated.ptr, offset);
            try self.copyBytes(dst, self.slot(field_local).ptr, field_size, self.alignmentForLayout(field_layout));
        }
    }

    fn emitTagLiteral(self: *MonoLlvmCodeGen, target: LocalId, discriminant: u16, payload: ?LocalId) Error!void {
        const allocated = try self.allocAggregateTarget(target);
        if (self.layoutByteSize(allocated.layout_idx) > 0) {
            try self.writeTagDiscriminant(allocated.ptr, allocated.layout_idx, discriminant);
        }
        if (payload) |payload_local| {
            const payload_layout = self.tagPayloadLayout(allocated.layout_idx, discriminant);
            const payload_size = self.layoutByteSize(payload_layout);
            if (payload_size > 0) {
                try self.copyBytes(allocated.ptr, self.slot(payload_local).ptr, payload_size, self.alignmentForLayout(payload_layout));
            }
        }
    }

    fn allocAggregateTarget(self: *MonoLlvmCodeGen, target: LocalId) Error!ResolvedBase {
        const builder = self.builder orelse return error.CompilationFailed;
        const target_layout = self.localLayout(target);
        const layout_val = self.layoutValue(target_layout);
        const target_ptr = self.slot(target).ptr;
        switch (layout_val.tag) {
            .box => {
                const abi = self.layouts().builtinBoxAbi(target_layout);
                const data_ptr = try self.callBuiltin(
                    "roc_builtins_allocate_with_refcount",
                    try self.ptrType(),
                    &.{ self.ptrSizedIntType(), .i32, .i1, try self.ptrType() },
                    &.{
                        builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory,
                        builder.intValue(.i32, abi.elem_alignment) catch return error.OutOfMemory,
                        builder.intValue(.i1, @intFromBool(abi.contains_refcounted)) catch return error.OutOfMemory,
                        self.rocOps(),
                    },
                );
                try self.zeroBytes(data_ptr, abi.elem_size);
                try self.storePointer(target_ptr, data_ptr);
                return .{ .ptr = data_ptr, .layout_idx = abi.elem_layout_idx orelse .zst };
            },
            .box_of_zst => {
                try self.storePointer(target_ptr, builder.nullValue(try self.ptrType()) catch return error.OutOfMemory);
                return .{ .ptr = target_ptr, .layout_idx = .zst };
            },
            else => {
                if (self.slot(target).size > 0) try self.zeroBytes(target_ptr, self.slot(target).size);
                return .{ .ptr = target_ptr, .layout_idx = target_layout };
            },
        }
    }

    fn emitLowLevel(self: *MonoLlvmCodeGen, target: LocalId, op: lir.LowLevel, args: LocalSpan) Error!void {
        const arg_locals = self.store.getLocalSpan(args);
        switch (op) {
            .bool_not => {
                const value = try self.loadBool(self.slot(arg_locals[0]).ptr);
                const not_value = (self.wip orelse return error.CompilationFailed).not(value, "") catch return error.OutOfMemory;
                try self.storeBool(self.slot(target).ptr, not_value);
            },
            .num_is_eq => try self.storeBool(self.slot(target).ptr, try self.emitValueEqual(self.slot(arg_locals[0]).ptr, self.slot(arg_locals[1]).ptr, self.localLayout(arg_locals[0]))),
            .num_is_gt, .num_is_gte, .num_is_lt, .num_is_lte => try self.emitNumericCompare(target, op, arg_locals),
            .num_plus, .num_minus, .num_times, .num_div_by, .num_div_trunc_by, .num_rem_by, .num_mod_by, .num_shift_left_by, .num_shift_right_by, .num_shift_right_zf_by, .num_bitwise_and, .num_bitwise_or, .num_bitwise_xor => try self.emitNumericBinary(target, op, arg_locals),
            .num_bitwise_not => try self.emitNumericBitwiseNot(target, arg_locals[0]),
            .num_negate => try self.emitNumericNegate(target, arg_locals[0]),
            .num_abs => try self.emitNumericAbs(target, arg_locals[0]),
            .num_abs_diff => try self.emitNumericAbsDiff(target, arg_locals),
            .list_len => try self.storeIntToLayout(self.slot(target).ptr, try self.loadUsize(try self.offsetPtr(self.slot(arg_locals[0]).ptr, @sizeOf(usize))), self.localLayout(target)),
            .list_get_unsafe => try self.emitListGetUnsafe(target, arg_locals),
            .list_with_capacity => try self.emitListWithCapacity(target, arg_locals),
            .list_append_unsafe => try self.emitListAppendUnsafe(target, arg_locals),
            .list_concat => try self.emitListConcat(target, arg_locals),
            .list_prepend => try self.emitListPrepend(target, arg_locals),
            .list_sublist, .list_drop_first, .list_drop_last, .list_take_first, .list_take_last => try self.emitListSublist(target, op, arg_locals),
            .list_drop_at => try self.emitListDropAt(target, arg_locals),
            .list_set => try self.emitListSet(target, arg_locals),
            .list_reserve => try self.emitListReserve(target, arg_locals),
            .list_release_excess_capacity => try self.emitListReleaseExcess(target, arg_locals),
            .list_first, .list_last => try self.emitListFirstLast(target, op, arg_locals),
            .str_is_eq => try self.emitStrBoolBuiltin(target, "roc_builtins_str_equal", arg_locals),
            .str_contains => try self.emitStrBoolBuiltin(target, "roc_builtins_str_contains", arg_locals),
            .str_starts_with => try self.emitStrBoolBuiltin(target, "roc_builtins_str_starts_with", arg_locals),
            .str_ends_with => try self.emitStrBoolBuiltin(target, "roc_builtins_str_ends_with", arg_locals),
            .str_caseless_ascii_equals => try self.emitStrBoolBuiltin(target, "roc_builtins_str_caseless_ascii_equals", arg_locals),
            .str_count_utf8_bytes => try self.emitStrCountUtf8Bytes(target, arg_locals[0]),
            .str_concat => try self.emitStrRetBuiltin(target, "roc_builtins_str_concat", arg_locals),
            .str_trim => try self.emitStrUnaryRetBuiltin(target, "roc_builtins_str_trim", arg_locals[0]),
            .str_trim_start => try self.emitStrUnaryRetBuiltin(target, "roc_builtins_str_trim_start", arg_locals[0]),
            .str_trim_end => try self.emitStrUnaryRetBuiltin(target, "roc_builtins_str_trim_end", arg_locals[0]),
            .str_with_ascii_lowercased => try self.emitStrUnaryRetBuiltin(target, "roc_builtins_str_with_ascii_lowercased", arg_locals[0]),
            .str_with_ascii_uppercased => try self.emitStrUnaryRetBuiltin(target, "roc_builtins_str_with_ascii_uppercased", arg_locals[0]),
            .str_drop_prefix => try self.emitStrRetBuiltin(target, "roc_builtins_str_drop_prefix", arg_locals),
            .str_drop_suffix => try self.emitStrRetBuiltin(target, "roc_builtins_str_drop_suffix", arg_locals),
            .str_split_on => try self.emitStrRetBuiltin(target, "roc_builtins_str_split", arg_locals),
            .str_join_with => try self.emitStrJoinWith(target, arg_locals),
            .str_repeat => try self.emitStrRepeat(target, arg_locals),
            .str_with_capacity => try self.emitStrWithCapacity(target, arg_locals[0]),
            .str_reserve => try self.emitStrReserve(target, arg_locals),
            .str_release_excess_capacity => try self.emitStrUnaryRetBuiltin(target, "roc_builtins_str_release_excess_capacity", arg_locals[0]),
            .str_to_utf8 => try self.emitStrToUtf8(target, arg_locals[0]),
            .str_from_utf8_lossy => try self.emitStrFromUtf8Lossy(target, arg_locals[0]),
            .str_from_utf8 => try self.emitStrFromUtf8(target, arg_locals[0]),
            .str_inspect => try self.emitStrUnaryRetBuiltin(target, "roc_builtins_str_escape_and_quote", arg_locals[0]),
            .u8_from_str => try self.emitIntFromStr(target, arg_locals[0], 1, false),
            .i8_from_str => try self.emitIntFromStr(target, arg_locals[0], 1, true),
            .u16_from_str => try self.emitIntFromStr(target, arg_locals[0], 2, false),
            .i16_from_str => try self.emitIntFromStr(target, arg_locals[0], 2, true),
            .u32_from_str => try self.emitIntFromStr(target, arg_locals[0], 4, false),
            .i32_from_str => try self.emitIntFromStr(target, arg_locals[0], 4, true),
            .u64_from_str => try self.emitIntFromStr(target, arg_locals[0], 8, false),
            .i64_from_str => try self.emitIntFromStr(target, arg_locals[0], 8, true),
            .u128_from_str => try self.emitIntFromStr(target, arg_locals[0], 16, false),
            .i128_from_str => try self.emitIntFromStr(target, arg_locals[0], 16, true),
            .dec_from_str => try self.emitDecFromStr(target, arg_locals[0]),
            .f32_from_str => try self.emitFloatFromStr(target, arg_locals[0], 4),
            .f64_from_str => try self.emitFloatFromStr(target, arg_locals[0], 8),
            .u8_to_str, .i8_to_str, .u16_to_str, .i16_to_str, .u32_to_str, .i32_to_str, .u64_to_str, .i64_to_str, .u128_to_str, .i128_to_str => try self.emitIntToStr(target, arg_locals[0]),
            .f32_to_str, .f64_to_str => try self.emitFloatToStr(target, arg_locals[0]),
            .dec_to_str => try self.emitDecToStr(target, arg_locals[0]),
            .num_to_str => try self.emitNumToStr(target, arg_locals[0]),
            .box_box => try self.emitBoxBox(target, arg_locals[0]),
            .box_unbox => try self.emitBoxUnbox(target, arg_locals[0]),
            .erased_capture_load => try self.emitErasedCaptureLoad(target, arg_locals[0]),
            .crash => try self.emitCrashBytes("Roc crashed"),
            else => try self.emitNumericConversionOrCrash(target, op, arg_locals),
        }
    }

    fn emitNumericCompare(self: *MonoLlvmCodeGen, target: LocalId, op: lir.LowLevel, args: []const LocalId) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const layout_idx = self.localLayout(args[0]);
        const lhs = try self.loadScalar(self.slot(args[0]).ptr, layout_idx);
        const rhs = try self.loadScalar(self.slot(args[1]).ptr, layout_idx);
        const cond: LlvmBuilder.Value = if (isFloatLayout(layout_idx)) blk: {
            const cmp_cond: LlvmBuilder.FloatCondition = switch (op) {
                .num_is_gt => .ogt,
                .num_is_gte => .oge,
                .num_is_lt => .olt,
                .num_is_lte => .ole,
                else => unreachable,
            };
            break :blk wip.fcmp(.normal, cmp_cond, lhs, rhs, "") catch return error.OutOfMemory;
        } else blk: {
            const signed = layout_idx.isSigned();
            const cmp_cond: LlvmBuilder.IntegerCondition = switch (op) {
                .num_is_gt => if (signed) .sgt else .ugt,
                .num_is_gte => if (signed) .sge else .uge,
                .num_is_lt => if (signed) .slt else .ult,
                .num_is_lte => if (signed) .sle else .ule,
                else => unreachable,
            };
            break :blk wip.icmp(cmp_cond, lhs, rhs, "") catch return error.OutOfMemory;
        };
        try self.storeBool(self.slot(target).ptr, cond);
    }

    fn emitNumericBinary(self: *MonoLlvmCodeGen, target: LocalId, op: lir.LowLevel, args: []const LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const target_layout = self.localLayout(target);
        if (target_layout == .dec) {
            try self.emitDecBinary(target, op, args);
            return;
        }
        const lhs_layout = self.localLayout(args[0]);
        var lhs = try self.loadScalar(self.slot(args[0]).ptr, lhs_layout);
        var rhs = try self.loadScalar(self.slot(args[1]).ptr, self.localLayout(args[1]));
        const result_ty = self.scalarType(target_layout);
        lhs = try self.coerceScalar(lhs, result_ty, lhs_layout.isSigned());
        rhs = try self.coerceScalar(rhs, result_ty, self.localLayout(args[1]).isSigned());
        const result = if (isFloatLayout(target_layout)) blk: {
            const tag: LlvmBuilder.Function.Instruction.Tag = switch (op) {
                .num_plus => .fadd,
                .num_minus => .fsub,
                .num_times => .fmul,
                .num_div_by, .num_div_trunc_by => .fdiv,
                .num_rem_by, .num_mod_by => .frem,
                else => return error.UnsupportedLowLevel,
            };
            break :blk wip.bin(tag, lhs, rhs, "") catch return error.OutOfMemory;
        } else blk: {
            if (op == .num_shift_left_by or op == .num_shift_right_by or op == .num_shift_right_zf_by) {
                const result = try self.emitIntegerShift(op, lhs, rhs, target_layout);
                try self.storeScalar(self.slot(target).ptr, target_layout, result);
                return;
            }

            const signed = target_layout.isSigned();
            const tag: LlvmBuilder.Function.Instruction.Tag = switch (op) {
                .num_plus => .add,
                .num_minus => .sub,
                .num_times => .mul,
                .num_div_by, .num_div_trunc_by => if (signed) .sdiv else .udiv,
                .num_rem_by, .num_mod_by => if (signed) .srem else .urem,
                .num_bitwise_and => .@"and",
                .num_bitwise_or => .@"or",
                .num_bitwise_xor => .xor,
                else => return error.UnsupportedLowLevel,
            };
            const raw = wip.bin(tag, lhs, rhs, "") catch return error.OutOfMemory;
            if (op != .num_mod_by or !signed) break :blk raw;

            const zero = builder.zeroInitValue(result_ty) catch return error.OutOfMemory;
            const rem_is_zero = wip.icmp(.eq, raw, zero, "") catch return error.OutOfMemory;
            const rem_negative = wip.icmp(.slt, raw, zero, "") catch return error.OutOfMemory;
            const rhs_negative = wip.icmp(.slt, rhs, zero, "") catch return error.OutOfMemory;
            const sign_differs = wip.bin(.xor, rem_negative, rhs_negative, "") catch return error.OutOfMemory;
            const adjusted = wip.bin(.add, raw, rhs, "") catch return error.OutOfMemory;
            const adjusted_or_raw = wip.select(.normal, sign_differs, adjusted, raw, "") catch return error.OutOfMemory;
            break :blk wip.select(.normal, rem_is_zero, zero, adjusted_or_raw, "") catch return error.OutOfMemory;
        };
        try self.storeScalar(self.slot(target).ptr, target_layout, result);
    }

    fn emitIntegerShift(self: *MonoLlvmCodeGen, op: lir.LowLevel, lhs: LlvmBuilder.Value, rhs: LlvmBuilder.Value, target_layout: layout.Idx) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const result_ty = self.scalarType(target_layout);
        const shift_bits = builder.intValue(.i8, self.intBits(target_layout)) catch return error.OutOfMemory;
        const rhs_u8 = try self.coerceScalar(rhs, .i8, false);
        const too_large = wip.icmp(.uge, rhs_u8, shift_bits, "") catch return error.OutOfMemory;
        const zero_amount = builder.zeroInitValue(result_ty) catch return error.OutOfMemory;
        const amount = try self.coerceScalar(rhs_u8, result_ty, false);
        const safe_amount = wip.select(.normal, too_large, zero_amount, amount, "") catch return error.OutOfMemory;
        const tag: LlvmBuilder.Function.Instruction.Tag = switch (op) {
            .num_shift_left_by => .shl,
            .num_shift_right_by => if (target_layout.isSigned()) .ashr else .lshr,
            .num_shift_right_zf_by => .lshr,
            else => unreachable,
        };
        const shifted = wip.bin(tag, lhs, safe_amount, "") catch return error.OutOfMemory;
        const zero_result = builder.zeroInitValue(result_ty) catch return error.OutOfMemory;
        return wip.select(.normal, too_large, zero_result, shifted, "") catch return error.OutOfMemory;
    }

    fn emitDecBinary(self: *MonoLlvmCodeGen, target: LocalId, op: lir.LowLevel, args: []const LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const lhs = try self.loadScalar(self.slot(args[0]).ptr, .dec);
        const rhs = try self.loadScalar(self.slot(args[1]).ptr, .dec);
        const result = switch (op) {
            .num_plus => wip.bin(.add, lhs, rhs, "") catch return error.OutOfMemory,
            .num_minus => wip.bin(.sub, lhs, rhs, "") catch return error.OutOfMemory,
            .num_times => try self.callDecBinaryBuiltin("roc_builtins_dec_mul_saturated", lhs, rhs, false),
            .num_div_by => try self.callDecBinaryBuiltin("roc_builtins_dec_div", lhs, rhs, true),
            .num_div_trunc_by => try self.callDecBinaryBuiltin("roc_builtins_dec_div_trunc", lhs, rhs, true),
            .num_rem_by, .num_mod_by => blk: {
                const quotient = try self.callDecBinaryBuiltin("roc_builtins_dec_div_trunc", lhs, rhs, true);
                const product = try self.callDecBinaryBuiltin("roc_builtins_dec_mul_saturated", quotient, rhs, false);
                const remainder = wip.bin(.sub, lhs, product, "") catch return error.OutOfMemory;
                if (op == .num_rem_by) break :blk remainder;

                const zero = builder.intValue(.i128, 0) catch return error.OutOfMemory;
                const rem_is_zero = wip.icmp(.eq, remainder, zero, "") catch return error.OutOfMemory;
                const rem_positive = wip.icmp(.sgt, remainder, zero, "") catch return error.OutOfMemory;
                const rhs_positive = wip.icmp(.sgt, rhs, zero, "") catch return error.OutOfMemory;
                const sign_differs = wip.bin(.xor, rem_positive, rhs_positive, "") catch return error.OutOfMemory;
                const adjusted = wip.bin(.add, remainder, rhs, "") catch return error.OutOfMemory;
                const nonzero_mod = wip.select(.normal, sign_differs, adjusted, remainder, "") catch return error.OutOfMemory;
                break :blk wip.select(.normal, rem_is_zero, zero, nonzero_mod, "") catch return error.OutOfMemory;
            },
            else => return error.UnsupportedLowLevel,
        };
        try self.storeScalar(self.slot(target).ptr, .dec, result);
    }

    fn callDecBinaryBuiltin(self: *MonoLlvmCodeGen, name: []const u8, lhs: LlvmBuilder.Value, rhs: LlvmBuilder.Value, pass_roc_ops: bool) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const out_low = wip.alloca(.normal, .i64, .@"1", LlvmBuilder.Alignment.fromByteUnits(8), .default, "dec_low") catch return error.OutOfMemory;
        const out_high = wip.alloca(.normal, .i64, .@"1", LlvmBuilder.Alignment.fromByteUnits(8), .default, "dec_high") catch return error.OutOfMemory;
        const lhs_parts = try self.splitI128Value(lhs);
        const rhs_parts = try self.splitI128Value(rhs);
        if (pass_roc_ops) {
            try self.callBuiltinVoid(
                name,
                &.{ try self.ptrType(), try self.ptrType(), .i64, .i64, .i64, .i64, try self.ptrType() },
                &.{ out_low, out_high, lhs_parts.low, lhs_parts.high, rhs_parts.low, rhs_parts.high, self.rocOps() },
            );
        } else {
            try self.callBuiltinVoid(
                name,
                &.{ try self.ptrType(), try self.ptrType(), .i64, .i64, .i64, .i64 },
                &.{ out_low, out_high, lhs_parts.low, lhs_parts.high, rhs_parts.low, rhs_parts.high },
            );
        }

        const low = wip.load(.normal, .i64, out_low, LlvmBuilder.Alignment.fromByteUnits(8), "") catch return error.OutOfMemory;
        const high = wip.load(.normal, .i64, out_high, LlvmBuilder.Alignment.fromByteUnits(8), "") catch return error.OutOfMemory;
        return self.combineI128Parts(low, high);
    }

    const I128Parts = struct {
        low: LlvmBuilder.Value,
        high: LlvmBuilder.Value,
    };

    fn splitI128Value(self: *MonoLlvmCodeGen, value: LlvmBuilder.Value) Error!I128Parts {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const high = wip.bin(.lshr, value, builder.intValue(.i128, 64) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        return .{
            .low = try self.coerceScalar(value, .i64, false),
            .high = try self.coerceScalar(high, .i64, false),
        };
    }

    fn combineI128Parts(self: *MonoLlvmCodeGen, low: LlvmBuilder.Value, high: LlvmBuilder.Value) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const low128 = try self.coerceScalar(low, .i128, false);
        const high128 = try self.coerceScalar(high, .i128, false);
        const shifted_high = wip.bin(.shl, high128, builder.intValue(.i128, 64) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        return wip.bin(.@"or", shifted_high, low128, "") catch return error.OutOfMemory;
    }

    fn emitNumericNegate(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const target_layout = self.localLayout(target);
        const value = try self.loadScalar(self.slot(arg).ptr, self.localLayout(arg));
        const result = if (isFloatLayout(target_layout))
            wip.un(.fneg, value, "") catch return error.OutOfMemory
        else
            wip.neg(value, "") catch return error.OutOfMemory;
        try self.storeScalar(self.slot(target).ptr, target_layout, result);
    }

    fn emitNumericBitwiseNot(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const target_layout = self.localLayout(target);
        const value = try self.loadScalar(self.slot(arg).ptr, self.localLayout(arg));
        const result = wip.not(value, "") catch return error.OutOfMemory;
        try self.storeScalar(self.slot(target).ptr, target_layout, result);
    }

    fn emitNumericAbs(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const target_layout = self.localLayout(target);
        const value = try self.loadScalar(self.slot(arg).ptr, self.localLayout(arg));
        if (!target_layout.isSigned() and !isFloatLayout(target_layout)) {
            try self.storeScalar(self.slot(target).ptr, target_layout, value);
            return;
        }
        const zero = builder.zeroInitValue(value.typeOfWip(wip)) catch return error.OutOfMemory;
        const is_neg = if (isFloatLayout(target_layout))
            wip.fcmp(.normal, .olt, value, zero, "") catch return error.OutOfMemory
        else
            wip.icmp(.slt, value, zero, "") catch return error.OutOfMemory;
        const neg = if (isFloatLayout(target_layout))
            wip.un(.fneg, value, "") catch return error.OutOfMemory
        else
            wip.neg(value, "") catch return error.OutOfMemory;
        const result = wip.select(.normal, is_neg, neg, value, "") catch return error.OutOfMemory;
        try self.storeScalar(self.slot(target).ptr, target_layout, result);
    }

    fn emitNumericAbsDiff(self: *MonoLlvmCodeGen, target: LocalId, args: []const LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const lhs_layout = self.localLayout(args[0]);
        const rhs_layout = self.localLayout(args[1]);
        const lhs = try self.coerceScalar(try self.loadScalar(self.slot(args[0]).ptr, lhs_layout), .i128, lhs_layout.isSigned());
        const rhs = try self.coerceScalar(try self.loadScalar(self.slot(args[1]).ptr, rhs_layout), .i128, rhs_layout.isSigned());
        const zero = builder.intValue(.i128, 0) catch return error.OutOfMemory;

        const result = if (lhs_layout.isSigned() or rhs_layout.isSigned()) blk: {
            const diff = wip.bin(.sub, lhs, rhs, "") catch return error.OutOfMemory;
            const is_neg = wip.icmp(.slt, diff, zero, "") catch return error.OutOfMemory;
            const neg = wip.bin(.sub, zero, diff, "") catch return error.OutOfMemory;
            break :blk wip.select(.normal, is_neg, neg, diff, "") catch return error.OutOfMemory;
        } else blk: {
            const lhs_ge_rhs = wip.icmp(.uge, lhs, rhs, "") catch return error.OutOfMemory;
            const lhs_minus_rhs = wip.bin(.sub, lhs, rhs, "") catch return error.OutOfMemory;
            const rhs_minus_lhs = wip.bin(.sub, rhs, lhs, "") catch return error.OutOfMemory;
            break :blk wip.select(.normal, lhs_ge_rhs, lhs_minus_rhs, rhs_minus_lhs, "") catch return error.OutOfMemory;
        };

        const target_layout = self.localLayout(target);
        const coerced = try self.coerceScalar(result, self.scalarType(target_layout), false);
        try self.storeScalar(self.slot(target).ptr, target_layout, coerced);
    }

    fn emitNumericConversionOrCrash(self: *MonoLlvmCodeGen, target: LocalId, op: lir.LowLevel, args: []const LocalId) Error!void {
        const name = @tagName(op);
        if (args.len >= 1 and isIntegerLayout(self.localLayout(args[0])) and self.localLayout(target) == .dec and std.mem.endsWith(u8, name, "_to_dec")) {
            try self.emitIntToDec(target, args[0]);
            return;
        }
        if (args.len >= 1 and isIntegerLayout(self.localLayout(args[0])) and std.mem.endsWith(u8, name, "_try")) {
            try self.emitIntTryConversion(target, args[0]);
            return;
        }
        if (std.mem.find(u8, name, "_to_") != null and args.len >= 1 and
            std.mem.find(u8, name, "_try") == null and
            std.mem.find(u8, name, "_str") == null)
        {
            const value = try self.loadScalar(self.slot(args[0]).ptr, self.localLayout(args[0]));
            const coerced = try self.coerceScalar(value, self.scalarType(self.localLayout(target)), self.localLayout(args[0]).isSigned());
            try self.storeScalar(self.slot(target).ptr, self.localLayout(target), coerced);
            return;
        }
        try self.emitCrashBytes(name);
    }

    fn emitIntToDec(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const arg_layout = self.localLayout(arg);
        const value = try self.loadScalar(self.slot(arg).ptr, arg_layout);
        const value64 = try self.coerceScalar(value, .i64, arg_layout.isSigned());
        const low_ptr = wip.alloca(.normal, .i64, .@"1", LlvmBuilder.Alignment.fromByteUnits(8), .default, "dec_low") catch return error.OutOfMemory;
        const high_ptr = wip.alloca(.normal, .i64, .@"1", LlvmBuilder.Alignment.fromByteUnits(8), .default, "dec_high") catch return error.OutOfMemory;
        const fn_name = if (arg_layout.isSigned()) "roc_builtins_i64_to_dec" else "roc_builtins_u64_to_dec";
        try self.callBuiltinVoid(fn_name, &.{ try self.ptrType(), try self.ptrType(), .i64 }, &.{ low_ptr, high_ptr, value64 });
        const low = wip.load(.normal, .i64, low_ptr, LlvmBuilder.Alignment.fromByteUnits(8), "") catch return error.OutOfMemory;
        const high = wip.load(.normal, .i64, high_ptr, LlvmBuilder.Alignment.fromByteUnits(8), "") catch return error.OutOfMemory;
        try self.storeScalar(self.slot(target).ptr, .dec, try self.combineI128Parts(low, high));
    }

    fn emitIntTryConversion(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const allocated = try self.allocAggregateTarget(target);
        const target_payload_layout = self.tagPayloadLayout(allocated.layout_idx, 1);
        if (!isIntegerLayout(target_payload_layout)) return error.CompilationFailed;
        const source_layout = self.localLayout(arg);
        const value = try self.loadScalar(self.slot(arg).ptr, source_layout);
        const value128 = try self.coerceScalar(value, .i128, source_layout.isSigned());
        const parts = try self.splitI128Value(value128);
        const disc_offset = try self.tagDiscriminantOffset(allocated.layout_idx);
        const func_name = if (source_layout.isSigned()) "roc_builtins_i128_try_convert" else "roc_builtins_u128_try_convert";
        try self.callBuiltinVoid(
            func_name,
            &.{ try self.ptrType(), .i64, .i64, .i32, .i32, .i32, .i32 },
            &.{
                allocated.ptr,
                parts.low,
                parts.high,
                builder.intValue(.i32, self.intBits(target_payload_layout)) catch return error.OutOfMemory,
                builder.intValue(.i32, @intFromBool(target_payload_layout.isSigned())) catch return error.OutOfMemory,
                builder.intValue(.i32, self.layoutByteSize(target_payload_layout)) catch return error.OutOfMemory,
                builder.intValue(.i32, disc_offset) catch return error.OutOfMemory,
            },
        );
    }

    fn emitSwitch(self: *MonoLlvmCodeGen, sw: anytype) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const branches = self.store.getCFSwitchBranches(sw.branches);
        const default_block = wip.block(0, "switch_default") catch return error.OutOfMemory;
        const branch_blocks = try self.allocator.alloc(LlvmBuilder.Function.Block.Index, branches.len);
        defer self.allocator.free(branch_blocks);
        for (branch_blocks) |*block| block.* = wip.block(0, "switch_case") catch return error.OutOfMemory;
        const cond = try self.readSwitchValue(self.slot(sw.cond).ptr, self.localLayout(sw.cond));
        var switch_inst = wip.@"switch"(cond, default_block, @intCast(branches.len), .none) catch return error.OutOfMemory;
        for (branches, branch_blocks) |branch, block| {
            switch_inst.addCase(builder.intConst(cond.typeOfWip(wip), branch.value) catch return error.OutOfMemory, block, wip) catch return error.OutOfMemory;
        }
        switch_inst.finish(wip);
        for (branches, branch_blocks) |branch, block| {
            wip.cursor = .{ .block = block };
            try self.compileStmt(branch.body);
        }
        wip.cursor = .{ .block = default_block };
        try self.compileStmt(sw.default_branch);
    }

    fn emitJoin(self: *MonoLlvmCodeGen, join_stmt: anytype) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const key = @intFromEnum(join_stmt.id);
        const join_block = wip.block(0, "join_body") catch return error.OutOfMemory;
        const after_block = wip.block(0, "join_after") catch return error.OutOfMemory;
        try self.join_points.put(key, .{ .block = join_block, .params = join_stmt.params, .body = join_stmt.body });

        try self.compileStmt(join_stmt.remainder);
        if (!self.currentBlockHasTerminator()) _ = wip.br(after_block) catch return error.OutOfMemory;

        if (!self.compiled_joins.contains(key)) {
            try self.compiled_joins.put(key, {});
            wip.cursor = .{ .block = join_block };
            try self.compileStmt(join_stmt.body);
            if (!self.currentBlockHasTerminator()) _ = wip.br(after_block) catch return error.OutOfMemory;
        }

        wip.cursor = .{ .block = after_block };
    }

    fn emitJump(self: *MonoLlvmCodeGen, jump_stmt: anytype) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const info = self.join_points.get(@intFromEnum(jump_stmt.target)) orelse return error.CompilationFailed;
        _ = wip.br(info.block) catch return error.OutOfMemory;
    }

    fn emitLoopContinue(self: *MonoLlvmCodeGen) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const dest = self.loop_continue_blocks.items[self.loop_continue_blocks.items.len - 1];
        _ = wip.br(dest) catch return error.OutOfMemory;
    }

    fn emitLoopBreak(self: *MonoLlvmCodeGen) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const dest = self.loop_break_blocks.items[self.loop_break_blocks.items.len - 1];
        _ = wip.br(dest) catch return error.OutOfMemory;
    }

    fn emitReturn(self: *MonoLlvmCodeGen, value: LocalId) Error!void {
        const ret_ptr = self.ret_ptr_arg orelse return error.CompilationFailed;
        const size = self.layoutByteSize(self.current_ret_layout);
        if (size > 0) {
            try self.copyBytes(ret_ptr, self.slot(value).ptr, size, self.alignmentForLayout(self.current_ret_layout));
        }
        const wip = self.wip orelse return error.CompilationFailed;
        _ = wip.retVoid() catch return error.OutOfMemory;
    }

    fn emitExpect(self: *MonoLlvmCodeGen, condition: LocalId) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const ok_block = wip.block(0, "expect_ok") catch return error.OutOfMemory;
        const fail_block = wip.block(0, "expect_fail") catch return error.OutOfMemory;
        const cond = try self.loadBool(self.slot(condition).ptr);
        _ = wip.brCond(cond, ok_block, fail_block, .then_likely) catch return error.OutOfMemory;
        wip.cursor = .{ .block = fail_block };
        try self.emitStaticRocOpsMessageCall(@offsetOf(builtins.host_abi.RocOps, "roc_expect_failed"), "expect failed");
        _ = wip.br(ok_block) catch return error.OutOfMemory;
        wip.cursor = .{ .block = ok_block };
    }

    fn emitCrashBytes(self: *MonoLlvmCodeGen, msg: []const u8) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        try self.emitStaticRocOpsMessageCall(@offsetOf(builtins.host_abi.RocOps, "roc_crashed"), msg);
        // Linux AArch64 eval tests handle crashes by returning to the Zig host.
        // Longjmping through LLVM-generated frames is not reliable on that target.
        if (self.target.cpu.arch == .aarch64 and self.target.os.tag == .linux) {
            _ = wip.retVoid() catch return error.OutOfMemory;
        } else {
            _ = wip.@"unreachable"() catch return error.OutOfMemory;
        }
    }

    fn emitStaticRocOpsMessageCall(self: *MonoLlvmCodeGen, callback_offset: usize, msg: []const u8) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const ptr_ty = try self.ptrType();

        if (callback_offset == @offsetOf(builtins.host_abi.RocOps, "roc_expect_failed") or
            callback_offset == @offsetOf(builtins.host_abi.RocOps, "roc_crashed"))
        {
            const wrapper_name = if (callback_offset == @offsetOf(builtins.host_abi.RocOps, "roc_crashed"))
                "roc_builtins_roc_crashed"
            else
                "roc_builtins_roc_expect_failed";

            try self.callBuiltinVoid(
                wrapper_name,
                &.{ ptr_ty, self.ptrSizedIntType(), ptr_ty },
                &.{
                    try self.staticBytes(msg),
                    builder.intValue(self.ptrSizedIntType(), msg.len) catch return error.OutOfMemory,
                    self.rocOps(),
                },
            );
            return;
        }

        const wip = self.wip orelse return error.CompilationFailed;
        const args_ptr = wip.alloca(
            .normal,
            .i8,
            builder.intValue(.i32, @sizeOf(builtins.host_abi.RocCrashed)) catch return error.OutOfMemory,
            LlvmBuilder.Alignment.fromByteUnits(@alignOf(builtins.host_abi.RocCrashed)),
            .default,
            "roc_ops_msg",
        ) catch return error.OutOfMemory;

        try self.storePointer(args_ptr, try self.staticBytes(msg));
        try self.storeUsize(
            try self.offsetPtr(args_ptr, @offsetOf(builtins.host_abi.RocCrashed, "len")),
            builder.intValue(self.ptrSizedIntType(), msg.len) catch return error.OutOfMemory,
        );

        const env_ptr = try self.loadPointer(self.rocOps());
        const callback_ptr_ptr = try self.offsetPtr(self.rocOps(), @intCast(callback_offset));
        const callback_ptr = try self.loadPointer(callback_ptr_ptr);
        const fn_ty = builder.fnType(.void, &.{ ptr_ty, ptr_ty }, .normal) catch return error.OutOfMemory;
        _ = wip.call(.normal, .ccc, .none, fn_ty, callback_ptr, &.{ args_ptr, env_ptr }, "") catch return error.OutOfMemory;
    }

    fn copyLocal(self: *MonoLlvmCodeGen, target: LocalId, source: LocalId) Error!void {
        const target_slot = self.slot(target);
        if (target_slot.size == 0) return;
        try self.copyBytes(target_slot.ptr, self.slot(source).ptr, target_slot.size, target_slot.alignment);
    }

    fn emitStrLiteral(self: *MonoLlvmCodeGen, out: LlvmBuilder.Value, literal: StrLiteral) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const bytes = self.store.getStringLiteral(literal);
        try self.callBuiltinVoid(
            "roc_builtins_str_from_literal",
            &.{ try self.ptrType(), try self.ptrType(), self.ptrSizedIntType(), try self.ptrType() },
            &.{
                out,
                try self.staticBytes(bytes),
                builder.intValue(self.ptrSizedIntType(), bytes.len) catch return error.OutOfMemory,
                self.rocOps(),
            },
        );
    }

    fn staticBytes(self: *MonoLlvmCodeGen, bytes: []const u8) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const actual = if (bytes.len == 0) "\x00" else bytes;
        const arr_ty = builder.arrayType(actual.len, .i8) catch return error.OutOfMemory;
        const name = builder.strtabStringFmt(".roc.bytes.{d}", .{self.string_counter}) catch return error.OutOfMemory;
        self.string_counter += 1;
        const variable = builder.addVariable(name, arr_ty, .default) catch return error.OutOfMemory;
        variable.ptrConst(builder).global.setLinkage(.internal, builder);
        variable.setMutability(.constant, builder);
        variable.setInitializer(builder.stringConst(builder.string(actual) catch return error.OutOfMemory) catch return error.OutOfMemory, builder) catch return error.OutOfMemory;
        return variable.toValue(builder);
    }

    fn emitStrBoolBuiltin(self: *MonoLlvmCodeGen, target: LocalId, name: []const u8, args: []const LocalId) Error!void {
        var call_args = try self.rocStrArgs2(args[0], args[1], false);
        defer call_args.deinit(self.allocator);
        const result = try self.callBuiltin(name, .i1, call_args.types.items, call_args.values.items);
        try self.storeBool(self.slot(target).ptr, result);
    }

    fn emitStrRetBuiltin(self: *MonoLlvmCodeGen, target: LocalId, name: []const u8, args: []const LocalId) Error!void {
        var call_args = try self.rocStrArgs2(args[0], args[1], true);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinVoid(name, call_args.types.items, call_args.values.items);
    }

    fn emitStrUnaryRetBuiltin(self: *MonoLlvmCodeGen, target: LocalId, name: []const u8, arg: LocalId) Error!void {
        var call_args = try self.rocStrArgs1(arg);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinVoid(name, call_args.types.items, call_args.values.items);
    }

    fn emitStrCountUtf8Bytes(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        var call_args = try self.rocStrArgs1(arg);
        defer call_args.deinit(self.allocator);
        const result = try self.callBuiltin("roc_builtins_str_count_utf8_bytes", self.ptrSizedIntType(), call_args.types.items, call_args.values.items);
        try self.storeIntToLayout(self.slot(target).ptr, result, self.localLayout(target));
    }

    fn emitStrJoinWith(self: *MonoLlvmCodeGen, target: LocalId, args: []const LocalId) Error!void {
        var call_args = try self.rocListArgs1(args[0]);
        defer call_args.deinit(self.allocator);
        const sep_args = try self.rocStrArgs1(args[1]);
        defer {
            var owned = sep_args;
            owned.deinit(self.allocator);
        }
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.types.appendSlice(self.allocator, sep_args.types.items);
        try call_args.values.appendSlice(self.allocator, sep_args.values.items);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinVoid("roc_builtins_str_join_with", call_args.types.items, call_args.values.items);
    }

    fn emitStrRepeat(self: *MonoLlvmCodeGen, target: LocalId, args: []const LocalId) Error!void {
        var call_args = try self.rocStrArgs1(args[0]);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, .i64, try self.coerceScalar(try self.loadScalar(self.slot(args[1]).ptr, self.localLayout(args[1])), .i64, false));
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinVoid("roc_builtins_str_repeat", call_args.types.items, call_args.values.items);
    }

    fn emitStrWithCapacity(self: *MonoLlvmCodeGen, target: LocalId, capacity: LocalId) Error!void {
        const cap = try self.coerceScalar(try self.loadScalar(self.slot(capacity).ptr, self.localLayout(capacity)), self.ptrSizedIntType(), false);
        try self.callBuiltinVoid("roc_builtins_str_with_capacity", &.{ try self.ptrType(), self.ptrSizedIntType(), try self.ptrType() }, &.{ self.slot(target).ptr, cap, self.rocOps() });
    }

    fn emitStrReserve(self: *MonoLlvmCodeGen, target: LocalId, args: []const LocalId) Error!void {
        var call_args = try self.rocStrArgs1(args[0]);
        defer call_args.deinit(self.allocator);
        const spare = try self.coerceScalar(try self.loadScalar(self.slot(args[1]).ptr, self.localLayout(args[1])), .i64, false);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, .i64, spare);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinVoid("roc_builtins_str_reserve", call_args.types.items, call_args.values.items);
    }

    fn emitStrToUtf8(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        var call_args = try self.rocStrArgs1(arg);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinVoid("roc_builtins_str_to_utf8", call_args.types.items, call_args.values.items);
    }

    fn emitStrFromUtf8Lossy(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        var call_args = try self.rocListArgs1(arg);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinVoid("roc_builtins_str_from_utf8_lossy", call_args.types.items, call_args.values.items);
    }

    fn emitStrFromUtf8(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const target_slot = self.slot(target);
        const info = try self.resolveStrFromUtf8Layout(target_slot.layout_idx);
        if (target_slot.size > 0) try self.zeroBytes(target_slot.ptr, target_slot.size);

        const layout_ptr = wip.alloca(
            .normal,
            .i8,
            builder.intValue(.i32, @sizeOf(builtins.dev_wrappers.StrFromUtf8Layout)) catch return error.OutOfMemory,
            LlvmBuilder.Alignment.fromByteUnits(@alignOf(builtins.dev_wrappers.StrFromUtf8Layout)),
            .default,
            "str_from_utf8_layout",
        ) catch return error.OutOfMemory;

        try self.storeRawInt(layout_ptr, 0, .i64, info.ok_tag, 8);
        try self.storeRawInt(layout_ptr, 8, .i64, info.err_tag, 8);
        try self.storeRawInt(layout_ptr, 16, .i32, info.outer_disc_offset, 4);
        try self.storeRawInt(layout_ptr, 20, .i32, info.outer_disc_size, 4);
        try self.storeRawInt(layout_ptr, 24, .i32, info.err_index_offset, 4);
        try self.storeRawInt(layout_ptr, 28, .i32, info.err_problem_offset, 4);
        try self.storeRawInt(layout_ptr, 32, .i32, info.inner_disc_offset, 4);
        try self.storeRawInt(layout_ptr, 36, .i32, info.inner_disc_size, 4);
        try self.storeRawInt(layout_ptr, 40, .i32, info.inner_bad_utf8_tag, 4);

        var call_args = try self.rocListArgs1(arg);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), target_slot.ptr);
        try call_args.append(self.allocator, try self.ptrType(), layout_ptr);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinVoid("roc_builtins_str_from_utf8_result", call_args.types.items, call_args.values.items);
    }

    fn emitIntFromStr(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId, width: u8, signed: bool) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const allocated = try self.allocAggregateTarget(target);
        const disc_offset = try self.tagDiscriminantOffset(allocated.layout_idx);
        var call_args = try self.rocStrArgs1(arg);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), allocated.ptr);
        try call_args.append(self.allocator, .i8, builder.intValue(.i8, width) catch return error.OutOfMemory);
        try call_args.append(self.allocator, .i1, builder.intValue(.i1, @intFromBool(signed)) catch return error.OutOfMemory);
        try call_args.append(self.allocator, .i32, builder.intValue(.i32, disc_offset) catch return error.OutOfMemory);
        try self.callBuiltinVoid("roc_builtins_int_from_str", call_args.types.items, call_args.values.items);
    }

    fn emitDecFromStr(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const allocated = try self.allocAggregateTarget(target);
        const disc_offset = try self.tagDiscriminantOffset(allocated.layout_idx);
        var call_args = try self.rocStrArgs1(arg);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), allocated.ptr);
        try call_args.append(self.allocator, .i32, builder.intValue(.i32, disc_offset) catch return error.OutOfMemory);
        try self.callBuiltinVoid("roc_builtins_dec_from_str", call_args.types.items, call_args.values.items);
    }

    fn emitFloatFromStr(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId, width: u8) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const allocated = try self.allocAggregateTarget(target);
        const disc_offset = try self.tagDiscriminantOffset(allocated.layout_idx);
        var call_args = try self.rocStrArgs1(arg);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), allocated.ptr);
        try call_args.append(self.allocator, .i8, builder.intValue(.i8, width) catch return error.OutOfMemory);
        try call_args.append(self.allocator, .i32, builder.intValue(.i32, disc_offset) catch return error.OutOfMemory);
        try self.callBuiltinVoid("roc_builtins_float_from_str", call_args.types.items, call_args.values.items);
    }

    fn emitIntToStr(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const arg_layout = self.localLayout(arg);
        const bits = self.intBits(arg_layout);
        const value = try self.coerceScalar(try self.loadScalar(self.slot(arg).ptr, arg_layout), .i128, arg_layout.isSigned());
        const lo = try self.coerceScalar(value, .i64, false);
        const hi = (self.wip orelse return error.CompilationFailed).bin(.lshr, value, builder.intValue(.i128, 64) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const hi64 = try self.coerceScalar(hi, .i64, false);
        const byte_width: u8 = @intCast(bits / 8);
        try self.callBuiltinVoid("roc_builtins_int_to_str", &.{ try self.ptrType(), .i64, .i64, .i8, .i1, try self.ptrType() }, &.{
            self.slot(target).ptr,
            lo,
            hi64,
            builder.intValue(.i8, byte_width) catch return error.OutOfMemory,
            builder.intValue(.i1, @intFromBool(arg_layout.isSigned())) catch return error.OutOfMemory,
            self.rocOps(),
        });
    }

    fn emitFloatToStr(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const arg_layout = self.localLayout(arg);
        const value = try self.loadScalar(self.slot(arg).ptr, arg_layout);
        const bits = if (arg_layout == .f32)
            try self.coerceScalar(wip.cast(.bitcast, value, .i32, "") catch return error.OutOfMemory, .i64, false)
        else
            wip.cast(.bitcast, value, .i64, "") catch return error.OutOfMemory;
        try self.callBuiltinVoid("roc_builtins_float_to_str", &.{ try self.ptrType(), .i64, .i1, try self.ptrType() }, &.{
            self.slot(target).ptr,
            bits,
            builder.intValue(.i1, @intFromBool(arg_layout == .f32)) catch return error.OutOfMemory,
            self.rocOps(),
        });
    }

    fn emitDecToStr(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const value = try self.loadScalar(self.slot(arg).ptr, .dec);
        const lo = try self.coerceScalar(value, .i64, false);
        const hi = (self.wip orelse return error.CompilationFailed).bin(.lshr, value, builder.intValue(.i128, 64) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const hi64 = try self.coerceScalar(hi, .i64, false);
        try self.callBuiltinVoid("roc_builtins_dec_to_str", &.{ try self.ptrType(), .i64, .i64, try self.ptrType() }, &.{ self.slot(target).ptr, lo, hi64, self.rocOps() });
    }

    fn emitNumToStr(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        switch (self.localLayout(arg)) {
            .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 => try self.emitIntToStr(target, arg),
            .dec => try self.emitDecToStr(target, arg),
            .f32, .f64 => try self.emitFloatToStr(target, arg),
            else => return error.CompilationFailed,
        }
    }

    const CallArgs = struct {
        types: std.ArrayList(LlvmBuilder.Type),
        values: std.ArrayList(LlvmBuilder.Value),

        fn init() CallArgs {
            return .{ .types = .empty, .values = .empty };
        }

        fn deinit(self: *CallArgs, allocator: Allocator) void {
            self.types.deinit(allocator);
            self.values.deinit(allocator);
        }

        fn append(self: *CallArgs, allocator: Allocator, ty: LlvmBuilder.Type, value: LlvmBuilder.Value) !void {
            try self.types.append(allocator, ty);
            try self.values.append(allocator, value);
        }

        fn prepend(self: *CallArgs, allocator: Allocator, ty: LlvmBuilder.Type, value: LlvmBuilder.Value) !void {
            try self.types.insert(allocator, 0, ty);
            try self.values.insert(allocator, 0, value);
        }
    };

    fn rocStrArgs1(self: *MonoLlvmCodeGen, arg: LocalId) Error!CallArgs {
        var result = CallArgs.init();
        const ptr = self.slot(arg).ptr;
        try result.append(self.allocator, try self.ptrType(), try self.loadPointer(ptr));
        try result.append(self.allocator, self.ptrSizedIntType(), try self.loadUsize(try self.offsetPtr(ptr, 2 * @sizeOf(usize))));
        try result.append(self.allocator, self.ptrSizedIntType(), try self.loadUsize(try self.offsetPtr(ptr, @sizeOf(usize))));
        return result;
    }

    fn rocStrArgs2(self: *MonoLlvmCodeGen, a: LocalId, b: LocalId, _: bool) Error!CallArgs {
        var result = try self.rocStrArgs1(a);
        const rhs = try self.rocStrArgs1(b);
        defer {
            var owned = rhs;
            owned.deinit(self.allocator);
        }
        try result.types.appendSlice(self.allocator, rhs.types.items);
        try result.values.appendSlice(self.allocator, rhs.values.items);
        return result;
    }

    fn rocListArgs1(self: *MonoLlvmCodeGen, arg: LocalId) Error!CallArgs {
        var result = CallArgs.init();
        const ptr = self.slot(arg).ptr;
        try result.append(self.allocator, try self.ptrType(), try self.loadPointer(ptr));
        try result.append(self.allocator, self.ptrSizedIntType(), try self.loadUsize(try self.offsetPtr(ptr, @sizeOf(usize))));
        try result.append(self.allocator, self.ptrSizedIntType(), try self.loadUsize(try self.offsetPtr(ptr, 2 * @sizeOf(usize))));
        return result;
    }

    fn appendListElementRcArgs(
        self: *MonoLlvmCodeGen,
        call_args: *CallArgs,
        abi: layout.Store.BuiltinListAbi,
        needs_incref: bool,
        needs_decref: bool,
    ) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const ptr_ty = try self.ptrType();
        const null_ptr = builder.nullValue(ptr_ty) catch return error.OutOfMemory;
        const enabled = abi.contains_refcounted and abi.elem_layout_idx != null;

        try call_args.append(
            self.allocator,
            .i1,
            builder.intValue(.i1, @intFromBool(enabled)) catch return error.OutOfMemory,
        );

        if (needs_incref) {
            const incref_fn = if (enabled)
                (try self.declareRcHelper(.{ .op = .incref, .layout_idx = abi.elem_layout_idx.? }))
            else
                null;
            try call_args.append(
                self.allocator,
                ptr_ty,
                if (incref_fn) |func| func.toValue(builder) else null_ptr,
            );
        }

        if (needs_decref) {
            const decref_fn = if (enabled)
                (try self.declareRcHelper(.{ .op = .decref, .layout_idx = abi.elem_layout_idx.? }))
            else
                null;
            try call_args.append(
                self.allocator,
                ptr_ty,
                if (decref_fn) |func| func.toValue(builder) else null_ptr,
            );
        }
    }

    fn emitListGetUnsafe(self: *MonoLlvmCodeGen, target: LocalId, args: []const LocalId) Error!void {
        const list_layout = self.localLayout(args[0]);
        const abi = self.layouts().builtinListAbi(list_layout);
        if (abi.elem_size == 0) return;
        const bytes = try self.loadPointer(self.slot(args[0]).ptr);
        const idx = try self.coerceScalar(try self.loadScalar(self.slot(args[1]).ptr, self.localLayout(args[1])), self.ptrSizedIntType(), false);
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const offset = wip.bin(.mul, idx, builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const src = wip.gep(.inbounds, .i8, bytes, &.{offset}, "") catch return error.OutOfMemory;
        try self.copyBytes(self.slot(target).ptr, src, self.slot(target).size, self.slot(target).alignment);
    }

    fn emitListWithCapacity(self: *MonoLlvmCodeGen, target: LocalId, args: []const LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const abi = self.layouts().builtinListAbi(self.localLayout(target));
        const cap = try self.coerceScalar(try self.loadScalar(self.slot(args[0]).ptr, self.localLayout(args[0])), self.ptrSizedIntType(), false);
        try self.callBuiltinVoid("roc_builtins_list_with_capacity", &.{ try self.ptrType(), self.ptrSizedIntType(), .i32, self.ptrSizedIntType(), .i1, try self.ptrType() }, &.{
            self.slot(target).ptr,
            cap,
            builder.intValue(.i32, abi.elem_alignment) catch return error.OutOfMemory,
            builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory,
            builder.intValue(.i1, @intFromBool(abi.contains_refcounted)) catch return error.OutOfMemory,
            self.rocOps(),
        });
    }

    fn emitListAppendUnsafe(self: *MonoLlvmCodeGen, target: LocalId, args: []const LocalId) Error!void {
        const abi = self.layouts().builtinListAbi(self.localLayout(args[0]));
        var call_args = try self.rocListArgs1(args[0]);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, try self.ptrType(), self.slot(args[1]).ptr);
        try call_args.append(self.allocator, self.ptrSizedIntType(), (self.builder orelse return error.CompilationFailed).intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinVoid("roc_builtins_list_append_unsafe", call_args.types.items, call_args.values.items);
    }

    fn emitListConcat(self: *MonoLlvmCodeGen, target: LocalId, args: []const LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const abi = self.layouts().builtinListAbi(self.localLayout(target));
        if (abi.elem_size == 0) {
            const lhs_len = try self.loadUsize(try self.offsetPtr(self.slot(args[0]).ptr, @sizeOf(usize)));
            const rhs_len = try self.loadUsize(try self.offsetPtr(self.slot(args[1]).ptr, @sizeOf(usize)));
            const total_len = (self.wip orelse return error.CompilationFailed).bin(.add, lhs_len, rhs_len, "") catch return error.OutOfMemory;
            const null_ptr = builder.nullValue(try self.ptrType()) catch return error.OutOfMemory;
            try self.storePointer(self.slot(target).ptr, null_ptr);
            try self.storeScalar(try self.offsetPtr(self.slot(target).ptr, @sizeOf(usize)), .u64, total_len);
            try self.storeScalar(
                try self.offsetPtr(self.slot(target).ptr, 2 * @sizeOf(usize)),
                .u64,
                (self.wip orelse return error.CompilationFailed).bin(.shl, total_len, builder.intValue(self.ptrSizedIntType(), 1) catch return error.OutOfMemory, "") catch return error.OutOfMemory,
            );
            return;
        }
        var call_args = try self.rocListArgs1(args[0]);
        defer call_args.deinit(self.allocator);
        const rhs = try self.rocListArgs1(args[1]);
        defer {
            var owned = rhs;
            owned.deinit(self.allocator);
        }
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.types.appendSlice(self.allocator, rhs.types.items);
        try call_args.values.appendSlice(self.allocator, rhs.values.items);
        try call_args.append(self.allocator, .i32, builder.intValue(.i32, abi.elem_alignment) catch return error.OutOfMemory);
        try call_args.append(self.allocator, self.ptrSizedIntType(), builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory);
        try self.appendListElementRcArgs(&call_args, abi, true, true);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinVoid("roc_builtins_list_concat", call_args.types.items, call_args.values.items);
    }

    fn emitListPrepend(self: *MonoLlvmCodeGen, target: LocalId, args: []const LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const abi = self.layouts().builtinListAbi(self.localLayout(args[0]));
        var call_args = try self.rocListArgs1(args[0]);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, .i32, builder.intValue(.i32, abi.elem_alignment) catch return error.OutOfMemory);
        try call_args.append(self.allocator, try self.ptrType(), self.slot(args[1]).ptr);
        try call_args.append(self.allocator, self.ptrSizedIntType(), builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory);
        try self.appendListElementRcArgs(&call_args, abi, true, false);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinVoid("roc_builtins_list_prepend", call_args.types.items, call_args.values.items);
    }

    fn emitListSublist(self: *MonoLlvmCodeGen, target: LocalId, op: lir.LowLevel, args: []const LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const abi = self.layouts().builtinListAbi(self.localLayout(args[0]));
        const len = try self.loadUsize(try self.offsetPtr(self.slot(args[0]).ptr, @sizeOf(usize)));
        const zero = builder.intValue(self.ptrSizedIntType(), 0) catch return error.OutOfMemory;
        const one = builder.intValue(self.ptrSizedIntType(), 1) catch return error.OutOfMemory;
        const max_count = builder.intValue(self.ptrSizedIntType(), -1) catch return error.OutOfMemory;
        const slice = switch (op) {
            .list_drop_first => ListSlice{ .start = one, .len = max_count },
            .list_drop_last => blk: {
                const len_is_zero = (self.wip orelse return error.CompilationFailed).icmp(.eq, len, zero, "") catch return error.OutOfMemory;
                const decremented = (self.wip orelse return error.CompilationFailed).bin(.sub, len, one, "") catch return error.OutOfMemory;
                const safe_len = (self.wip orelse return error.CompilationFailed).select(.normal, len_is_zero, zero, decremented, "") catch return error.OutOfMemory;
                break :blk ListSlice{ .start = zero, .len = safe_len };
            },
            .list_take_first => ListSlice{ .start = zero, .len = try self.loadIntegerLocalAsUsize(args[1]) },
            .list_take_last => blk: {
                const count = try self.loadIntegerLocalAsUsize(args[1]);
                const takes_all = (self.wip orelse return error.CompilationFailed).icmp(.uge, count, len, "") catch return error.OutOfMemory;
                const suffix_start = (self.wip orelse return error.CompilationFailed).bin(.sub, len, count, "") catch return error.OutOfMemory;
                const safe_start = (self.wip orelse return error.CompilationFailed).select(.normal, takes_all, zero, suffix_start, "") catch return error.OutOfMemory;
                break :blk ListSlice{ .start = safe_start, .len = count };
            },
            .list_sublist => try self.loadSublistStartLen(args[1]),
            else => return error.UnsupportedLowLevel,
        };
        var call_args = try self.rocListArgs1(args[0]);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, .i32, builder.intValue(.i32, abi.elem_alignment) catch return error.OutOfMemory);
        try call_args.append(self.allocator, self.ptrSizedIntType(), builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory);
        try call_args.append(self.allocator, self.ptrSizedIntType(), slice.start);
        try call_args.append(self.allocator, self.ptrSizedIntType(), slice.len);
        try self.appendListElementRcArgs(&call_args, abi, false, true);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinVoid("roc_builtins_list_sublist", call_args.types.items, call_args.values.items);
    }

    const ListSlice = struct {
        start: LlvmBuilder.Value,
        len: LlvmBuilder.Value,
    };

    fn loadSublistStartLen(self: *MonoLlvmCodeGen, record: LocalId) Error!ListSlice {
        const record_layout = self.localLayout(record);
        const record_layout_val = self.layoutValue(record_layout);
        if (record_layout_val.tag != .struct_) return error.CompilationFailed;
        const record_idx = record_layout_val.getStruct().idx;
        const len_offset = self.layouts().getStructFieldOffsetByOriginalIndex(record_idx, 0);
        const start_offset = self.layouts().getStructFieldOffsetByOriginalIndex(record_idx, 1);
        const len_layout = self.layouts().getStructFieldLayoutByOriginalIndex(record_idx, 0);
        const start_layout = self.layouts().getStructFieldLayoutByOriginalIndex(record_idx, 1);
        return .{
            .start = try self.loadIntegerPtrAsUsize(try self.offsetPtr(self.slot(record).ptr, start_offset), start_layout),
            .len = try self.loadIntegerPtrAsUsize(try self.offsetPtr(self.slot(record).ptr, len_offset), len_layout),
        };
    }

    fn loadIntegerLocalAsUsize(self: *MonoLlvmCodeGen, local: LocalId) Error!LlvmBuilder.Value {
        return self.loadIntegerPtrAsUsize(self.slot(local).ptr, self.localLayout(local));
    }

    fn loadIntegerPtrAsUsize(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, value_layout: layout.Idx) Error!LlvmBuilder.Value {
        switch (value_layout) {
            .dec => {
                const value = try self.loadScalar(ptr, .dec);
                const parts = try self.splitI128Value(value);
                const truncated = try self.callBuiltin("roc_builtins_dec_to_i64_trunc", .i64, &.{ .i64, .i64 }, &.{ parts.low, parts.high });
                return self.coerceScalar(truncated, self.ptrSizedIntType(), true);
            },
            .f32, .f64 => return error.CompilationFailed,
            else => {
                const value = try self.loadScalar(ptr, value_layout);
                return self.coerceScalar(value, self.ptrSizedIntType(), value_layout.isSigned());
            },
        }
    }

    fn emitListDropAt(self: *MonoLlvmCodeGen, target: LocalId, args: []const LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const abi = self.layouts().builtinListAbi(self.localLayout(args[0]));
        var call_args = try self.rocListArgs1(args[0]);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, .i32, builder.intValue(.i32, abi.elem_alignment) catch return error.OutOfMemory);
        try call_args.append(self.allocator, self.ptrSizedIntType(), builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory);
        try call_args.append(self.allocator, self.ptrSizedIntType(), try self.coerceScalar(try self.loadScalar(self.slot(args[1]).ptr, self.localLayout(args[1])), self.ptrSizedIntType(), false));
        try self.appendListElementRcArgs(&call_args, abi, true, true);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinVoid("roc_builtins_list_drop_at", call_args.types.items, call_args.values.items);
    }

    fn emitListSet(self: *MonoLlvmCodeGen, target: LocalId, args: []const LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const abi = self.layouts().builtinListAbi(self.localLayout(args[0]));
        var call_args = try self.rocListArgs1(args[0]);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, .i32, builder.intValue(.i32, abi.elem_alignment) catch return error.OutOfMemory);
        try call_args.append(self.allocator, self.ptrSizedIntType(), try self.coerceScalar(try self.loadScalar(self.slot(args[1]).ptr, self.localLayout(args[1])), self.ptrSizedIntType(), false));
        try call_args.append(self.allocator, try self.ptrType(), self.slot(args[2]).ptr);
        try call_args.append(self.allocator, self.ptrSizedIntType(), builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory);
        try call_args.append(self.allocator, try self.ptrType(), builder.nullValue(try self.ptrType()) catch return error.OutOfMemory);
        try self.appendListElementRcArgs(&call_args, abi, true, true);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinVoid("roc_builtins_list_replace", call_args.types.items, call_args.values.items);
    }

    fn emitListReserve(self: *MonoLlvmCodeGen, target: LocalId, args: []const LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const abi = self.layouts().builtinListAbi(self.localLayout(args[0]));
        var call_args = try self.rocListArgs1(args[0]);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, .i32, builder.intValue(.i32, abi.elem_alignment) catch return error.OutOfMemory);
        try call_args.append(self.allocator, self.ptrSizedIntType(), try self.coerceScalar(try self.loadScalar(self.slot(args[1]).ptr, self.localLayout(args[1])), self.ptrSizedIntType(), false));
        try call_args.append(self.allocator, self.ptrSizedIntType(), builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory);
        try self.appendListElementRcArgs(&call_args, abi, true, false);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinVoid("roc_builtins_list_reserve", call_args.types.items, call_args.values.items);
    }

    fn emitListReleaseExcess(self: *MonoLlvmCodeGen, target: LocalId, args: []const LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const abi = self.layouts().builtinListAbi(self.localLayout(args[0]));
        var call_args = try self.rocListArgs1(args[0]);
        defer call_args.deinit(self.allocator);
        try call_args.prepend(self.allocator, try self.ptrType(), self.slot(target).ptr);
        try call_args.append(self.allocator, .i32, builder.intValue(.i32, abi.elem_alignment) catch return error.OutOfMemory);
        try call_args.append(self.allocator, self.ptrSizedIntType(), builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory);
        try self.appendListElementRcArgs(&call_args, abi, true, true);
        try call_args.append(self.allocator, try self.ptrType(), self.rocOps());
        try self.callBuiltinVoid("roc_builtins_list_release_excess_capacity", call_args.types.items, call_args.values.items);
    }

    fn emitListFirstLast(self: *MonoLlvmCodeGen, target: LocalId, op: lir.LowLevel, args: []const LocalId) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const list_ptr = self.slot(args[0]).ptr;
        const len = try self.loadUsize(try self.offsetPtr(list_ptr, @sizeOf(usize)));
        const non_empty = wip.icmp(.ne, len, builder.intValue(self.ptrSizedIntType(), 0) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const empty_block = wip.block(0, "list_empty") catch return error.OutOfMemory;
        const full_block = wip.block(0, "list_full") catch return error.OutOfMemory;
        const after = wip.block(0, "list_first_after") catch return error.OutOfMemory;
        _ = wip.brCond(non_empty, full_block, empty_block, .then_likely) catch return error.OutOfMemory;
        wip.cursor = .{ .block = empty_block };
        try self.emitTagLiteral(target, 0, null);
        _ = wip.br(after) catch return error.OutOfMemory;
        wip.cursor = .{ .block = full_block };
        const abi = self.layouts().builtinListAbi(self.localLayout(args[0]));
        const bytes = try self.loadPointer(list_ptr);
        const idx = if (op == .list_first)
            builder.intValue(self.ptrSizedIntType(), 0) catch return error.OutOfMemory
        else
            wip.bin(.sub, len, builder.intValue(self.ptrSizedIntType(), 1) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const offset = wip.bin(.mul, idx, builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const elem_src = wip.gep(.inbounds, .i8, bytes, &.{offset}, "") catch return error.OutOfMemory;
        const payload_layout = self.tagPayloadLayout(self.localLayout(target), 1);
        try self.emitTagLiteral(target, 1, null);
        try self.copyBytes(self.slot(target).ptr, elem_src, self.layoutByteSize(payload_layout), self.alignmentForLayout(payload_layout));
        _ = wip.br(after) catch return error.OutOfMemory;
        wip.cursor = .{ .block = after };
    }

    fn emitBoxBox(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const allocated = try self.allocAggregateTarget(target);
        try self.copyBytes(allocated.ptr, self.slot(arg).ptr, self.slot(arg).size, self.slot(arg).alignment);
    }

    fn emitBoxUnbox(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const ptr = try self.loadPointer(self.slot(arg).ptr);
        if (self.slot(target).size > 0) try self.copyBytes(self.slot(target).ptr, ptr, self.slot(target).size, self.slot(target).alignment);
    }

    fn emitErasedCaptureLoad(self: *MonoLlvmCodeGen, target: LocalId, arg: LocalId) Error!void {
        const capture_ptr = try self.loadPointer(self.slot(arg).ptr);
        if (self.slot(target).size > 0) try self.copyBytes(self.slot(target).ptr, capture_ptr, self.slot(target).size, self.slot(target).alignment);
    }

    fn emitValueEqual(self: *MonoLlvmCodeGen, lhs_ptr: LlvmBuilder.Value, rhs_ptr: LlvmBuilder.Value, layout_idx: layout.Idx) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const layout_val = self.layoutValue(layout_idx);
        if (self.layoutByteSize(layout_idx) == 0) return builder.intValue(.i1, 1) catch return error.OutOfMemory;
        switch (layout_val.tag) {
            .scalar => switch (layout_val.getScalar().tag) {
                .str => {
                    const lhs_fields = try self.rocStrArgFields(lhs_ptr);
                    const rhs_fields = try self.rocStrArgFields(rhs_ptr);
                    return self.callBuiltin("roc_builtins_str_equal", .i1, &.{ try self.ptrType(), self.ptrSizedIntType(), self.ptrSizedIntType(), try self.ptrType(), self.ptrSizedIntType(), self.ptrSizedIntType() }, &.{ lhs_fields[0], lhs_fields[1], lhs_fields[2], rhs_fields[0], rhs_fields[1], rhs_fields[2] });
                },
                else => {
                    const lhs = try self.loadScalar(lhs_ptr, layout_idx);
                    const rhs = try self.loadScalar(rhs_ptr, layout_idx);
                    return if (isFloatLayout(layout_idx))
                        wip.fcmp(.normal, .oeq, lhs, rhs, "") catch return error.OutOfMemory
                    else
                        wip.icmp(.eq, lhs, rhs, "") catch return error.OutOfMemory;
                },
            },
            .box, .erased_callable => {
                const lhs = try self.loadPointer(lhs_ptr);
                const rhs = try self.loadPointer(rhs_ptr);
                return wip.icmp(.eq, lhs, rhs, "") catch return error.OutOfMemory;
            },
            .list, .list_of_zst => return self.emitListEqual(lhs_ptr, rhs_ptr, layout_idx),
            .struct_ => {
                const info = self.layouts().getStructInfo(layout_val);
                var result = builder.intValue(.i1, 1) catch return error.OutOfMemory;
                for (0..info.fields.len) |i| {
                    const field = info.fields.get(@intCast(i));
                    const offset = self.layouts().getStructFieldOffset(layout_val.getStruct().idx, @intCast(i));
                    const field_eq = try self.emitValueEqual(try self.offsetPtr(lhs_ptr, offset), try self.offsetPtr(rhs_ptr, offset), field.layout);
                    result = wip.bin(.@"and", result, field_eq, "") catch return error.OutOfMemory;
                }
                return result;
            },
            .tag_union => return self.emitTagEqual(lhs_ptr, rhs_ptr, layout_idx),
            else => return self.emitMemoryEqual(lhs_ptr, rhs_ptr, self.layoutByteSize(layout_idx)),
        }
    }

    fn emitMemoryEqual(self: *MonoLlvmCodeGen, lhs_ptr: LlvmBuilder.Value, rhs_ptr: LlvmBuilder.Value, size: u32) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        var result = builder.intValue(.i1, 1) catch return error.OutOfMemory;
        var offset: u32 = 0;
        while (offset < size) : (offset += 1) {
            const lhs = wip.load(.normal, .i8, try self.offsetPtr(lhs_ptr, offset), LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.OutOfMemory;
            const rhs = wip.load(.normal, .i8, try self.offsetPtr(rhs_ptr, offset), LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.OutOfMemory;
            const eq = wip.icmp(.eq, lhs, rhs, "") catch return error.OutOfMemory;
            result = wip.bin(.@"and", result, eq, "") catch return error.OutOfMemory;
        }
        return result;
    }

    fn emitListEqual(self: *MonoLlvmCodeGen, lhs_ptr: LlvmBuilder.Value, rhs_ptr: LlvmBuilder.Value, list_layout: layout.Idx) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const abi = self.layouts().builtinListAbi(list_layout);
        const lhs_len = try self.loadUsize(try self.offsetPtr(lhs_ptr, @sizeOf(usize)));
        const rhs_len = try self.loadUsize(try self.offsetPtr(rhs_ptr, @sizeOf(usize)));
        const len_eq = wip.icmp(.eq, lhs_len, rhs_len, "") catch return error.OutOfMemory;
        if (abi.elem_size == 0) return len_eq;

        const result_ptr = wip.alloca(.normal, .i8, .@"1", LlvmBuilder.Alignment.fromByteUnits(1), .default, "list_eq") catch return error.OutOfMemory;
        try self.storeBool(result_ptr, len_eq);
        const header = wip.block(0, "list_eq_header") catch return error.OutOfMemory;
        const body = wip.block(0, "list_eq_body") catch return error.OutOfMemory;
        const after = wip.block(0, "list_eq_after") catch return error.OutOfMemory;
        const idx_ptr = wip.alloca(.normal, .i64, .@"1", LlvmBuilder.Alignment.fromByteUnits(8), .default, "list_eq_idx") catch return error.OutOfMemory;
        _ = wip.store(.normal, builder.intValue(.i64, 0) catch return error.OutOfMemory, idx_ptr, LlvmBuilder.Alignment.fromByteUnits(8)) catch return error.OutOfMemory;
        _ = wip.br(header) catch return error.OutOfMemory;
        wip.cursor = .{ .block = header };
        const so_far = try self.loadBool(result_ptr);
        const idx = wip.load(.normal, .i64, idx_ptr, LlvmBuilder.Alignment.fromByteUnits(8), "") catch return error.OutOfMemory;
        const idx_usize = try self.coerceScalar(idx, self.ptrSizedIntType(), false);
        const in_range = wip.icmp(.ult, idx_usize, lhs_len, "") catch return error.OutOfMemory;
        const continue_loop = wip.bin(.@"and", so_far, in_range, "") catch return error.OutOfMemory;
        _ = wip.brCond(continue_loop, body, after, .none) catch return error.OutOfMemory;
        wip.cursor = .{ .block = body };
        const lhs_bytes = try self.loadPointer(lhs_ptr);
        const rhs_bytes = try self.loadPointer(rhs_ptr);
        const offset = wip.bin(.mul, idx_usize, builder.intValue(self.ptrSizedIntType(), abi.elem_size) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const elem_eq = try self.emitValueEqual(
            wip.gep(.inbounds, .i8, lhs_bytes, &.{offset}, "") catch return error.OutOfMemory,
            wip.gep(.inbounds, .i8, rhs_bytes, &.{offset}, "") catch return error.OutOfMemory,
            abi.elem_layout_idx orelse .zst,
        );
        try self.storeBool(result_ptr, elem_eq);
        const next = wip.bin(.add, idx, builder.intValue(.i64, 1) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        _ = wip.store(.normal, next, idx_ptr, LlvmBuilder.Alignment.fromByteUnits(8)) catch return error.OutOfMemory;
        _ = wip.br(header) catch return error.OutOfMemory;
        wip.cursor = .{ .block = after };
        return self.loadBool(result_ptr);
    }

    fn emitTagEqual(self: *MonoLlvmCodeGen, lhs_ptr: LlvmBuilder.Value, rhs_ptr: LlvmBuilder.Value, tag_layout: layout.Idx) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const lhs_disc = try self.readTagDiscriminant(lhs_ptr, tag_layout);
        const rhs_disc = try self.readTagDiscriminant(rhs_ptr, tag_layout);
        const disc_eq = wip.icmp(.eq, lhs_disc, rhs_disc, "") catch return error.OutOfMemory;
        const data = self.layouts().getTagUnionData(self.layoutValue(tag_layout).getTagUnion().idx);
        const variants = self.layouts().getTagUnionVariants(data);
        const result_ptr = wip.alloca(.normal, .i8, .@"1", LlvmBuilder.Alignment.fromByteUnits(1), .default, "tag_eq") catch return error.OutOfMemory;
        try self.storeBool(result_ptr, disc_eq);
        const after = wip.block(0, "tag_eq_after") catch return error.OutOfMemory;
        const mismatch = wip.block(0, "tag_eq_mismatch") catch return error.OutOfMemory;
        const case_blocks = try self.allocator.alloc(LlvmBuilder.Function.Block.Index, variants.len);
        defer self.allocator.free(case_blocks);
        for (case_blocks) |*block| block.* = wip.block(0, "tag_eq_case") catch return error.OutOfMemory;
        _ = wip.brCond(disc_eq, case_blocks[0], mismatch, .then_likely) catch return error.OutOfMemory;
        wip.cursor = .{ .block = mismatch };
        try self.storeBool(result_ptr, builder.intValue(.i1, 0) catch return error.OutOfMemory);
        _ = wip.br(after) catch return error.OutOfMemory;
        for (case_blocks, 0..) |block, i| {
            wip.cursor = .{ .block = block };
            if (i + 1 < case_blocks.len) {
                const is_case = wip.icmp(.eq, lhs_disc, builder.intValue(lhs_disc.typeOfWip(wip), i) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
                const next_case = case_blocks[i + 1];
                const do_case = wip.block(0, "tag_eq_do_case") catch return error.OutOfMemory;
                _ = wip.brCond(is_case, do_case, next_case, .none) catch return error.OutOfMemory;
                wip.cursor = .{ .block = do_case };
            }
            const payload_layout = variants.get(@intCast(i)).payload_layout;
            const eq = try self.emitValueEqual(lhs_ptr, rhs_ptr, payload_layout);
            try self.storeBool(result_ptr, eq);
            _ = wip.br(after) catch return error.OutOfMemory;
        }
        wip.cursor = .{ .block = after };
        return self.loadBool(result_ptr);
    }

    fn emitRcForLocal(self: *MonoLlvmCodeGen, op: layout.RcOp, local: LocalId, count: u16) Error!void {
        const slot_v = self.slot(local);
        if (slot_v.size == 0) return;

        const layout_val = self.layoutValue(slot_v.layout_idx);
        if (!self.layouts().layoutContainsRefcounted(layout_val)) return;

        const helper_key: layout.RcHelperKey = if (layout_val.tag == .closure)
            .{
                .op = if (op == .free) .decref else op,
                .layout_idx = layout_val.getClosure().captures_layout_idx,
            }
        else
            .{ .op = op, .layout_idx = slot_v.layout_idx };

        const builder = self.builder orelse return error.CompilationFailed;
        const count_value = if (helper_key.op == .incref)
            builder.intValue(.i64, count) catch return error.OutOfMemory
        else
            null;
        try self.emitRcHelperCall(helper_key, slot_v.ptr, count_value);
    }

    fn declareRcHelper(self: *MonoLlvmCodeGen, helper_key: layout.RcHelperKey) Error!?LlvmBuilder.Function.Index {
        const builder = self.builder orelse return error.CompilationFailed;
        if (self.layouts().rcHelperPlan(helper_key) == .noop) return null;

        const cache_key = helper_key.encode();
        if (self.rc_helpers.get(cache_key)) |entry| return entry.function;

        const ptr_ty = try self.ptrType();
        const params: []const LlvmBuilder.Type = switch (helper_key.op) {
            .incref => &.{ ptr_ty, .i64, ptr_ty },
            .decref, .free => &.{ ptr_ty, ptr_ty },
        };
        const fn_ty = builder.fnType(.void, params, .normal) catch return error.OutOfMemory;
        const fn_name = builder.strtabStringFmt("roc_llvm_rc_{s}_{d}", .{
            @tagName(helper_key.op),
            @intFromEnum(helper_key.layout_idx),
        }) catch return error.OutOfMemory;
        const func = builder.addFunction(fn_ty, fn_name, .default) catch return error.OutOfMemory;
        func.setLinkage(.internal, builder);
        try self.rc_helpers.put(cache_key, .{
            .key = helper_key,
            .function = func,
        });
        return func;
    }

    fn compilePendingRcHelpers(self: *MonoLlvmCodeGen) Error!void {
        while (true) {
            var pending_key: ?layout.RcHelperKey = null;
            var iter = self.rc_helpers.iterator();
            while (iter.next()) |entry| {
                if (!entry.value_ptr.compiled) {
                    pending_key = entry.value_ptr.key;
                    break;
                }
            }
            const helper_key = pending_key orelse return;
            try self.compileRcHelper(helper_key);
        }
    }

    fn compileRcHelper(self: *MonoLlvmCodeGen, helper_key: layout.RcHelperKey) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const cache_key = helper_key.encode();
        const func = blk: {
            const entry = self.rc_helpers.getPtr(cache_key) orelse return error.CompilationFailed;
            if (entry.compiled) return;
            entry.compiled = true;
            break :blk entry.function;
        };
        errdefer {
            if (self.rc_helpers.getPtr(cache_key)) |entry| {
                entry.compiled = false;
            }
        }

        const outer_wip = self.wip;
        const outer_roc_ops = self.roc_ops_arg;
        const outer_ret = self.ret_ptr_arg;
        const outer_args = self.args_ptr_arg;
        const outer_capture = self.capture_ptr_arg;
        const outer_ret_layout = self.current_ret_layout;
        const outer_slots = self.local_slots;
        defer {
            self.wip = outer_wip;
            self.roc_ops_arg = outer_roc_ops;
            self.ret_ptr_arg = outer_ret;
            self.args_ptr_arg = outer_args;
            self.capture_ptr_arg = outer_capture;
            self.current_ret_layout = outer_ret_layout;
            self.local_slots = outer_slots;
        }

        var wip = LlvmBuilder.WipFunction.init(builder, .{ .function = func, .strip = true }) catch return error.OutOfMemory;
        defer wip.deinit();
        self.wip = &wip;
        self.ret_ptr_arg = null;
        self.args_ptr_arg = null;
        self.capture_ptr_arg = null;
        self.current_ret_layout = .zst;
        self.local_slots = &.{};

        const entry = wip.block(0, "entry") catch return error.OutOfMemory;
        const body = wip.block(0, "rc_body") catch return error.OutOfMemory;
        const done = wip.block(0, "rc_done") catch return error.OutOfMemory;
        wip.cursor = .{ .block = entry };

        const value_ptr = wip.arg(0);
        const count_value: ?LlvmBuilder.Value = switch (helper_key.op) {
            .incref => wip.arg(1),
            .decref, .free => null,
        };
        self.roc_ops_arg = switch (helper_key.op) {
            .incref => wip.arg(2),
            .decref, .free => wip.arg(1),
        };

        const is_null = wip.icmp(.eq, value_ptr, builder.nullValue(try self.ptrType()) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        _ = wip.brCond(is_null, done, body, .else_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = body };
        try self.emitRcHelperBody(helper_key, value_ptr, count_value);
        if (!self.currentBlockHasTerminator()) {
            _ = wip.br(done) catch return error.OutOfMemory;
        }

        wip.cursor = .{ .block = done };
        _ = wip.retVoid() catch return error.OutOfMemory;
        try self.finishCurrentWipFunction();
    }

    fn emitRcHelperBody(self: *MonoLlvmCodeGen, helper_key: layout.RcHelperKey, value_ptr: LlvmBuilder.Value, count_value: ?LlvmBuilder.Value) Error!void {
        switch (self.layouts().rcHelperPlan(helper_key)) {
            .noop => {},
            .str_incref => try self.emitRcHelperStrIncref(value_ptr, count_value.?),
            .str_decref => try self.emitRcHelperStrDrop(value_ptr, "roc_builtins_decref_data_ptr"),
            .str_free => try self.emitRcHelperStrDrop(value_ptr, "roc_builtins_free_data_ptr"),
            .list_incref => |list_plan| try self.emitRcHelperListIncref(list_plan, value_ptr, count_value.?),
            .list_decref => |list_plan| try self.emitRcHelperListDrop(list_plan, value_ptr, "roc_builtins_list_decref_with"),
            .list_free => |list_plan| try self.emitRcHelperListDrop(list_plan, value_ptr, "roc_builtins_list_free_with"),
            .box_incref => try self.emitRcHelperBoxIncref(value_ptr, count_value.?),
            .box_decref => |box_plan| try self.emitRcHelperBoxDrop(box_plan, value_ptr, "roc_builtins_box_decref_with"),
            .box_free => |box_plan| try self.emitRcHelperBoxDrop(box_plan, value_ptr, "roc_builtins_box_free_with"),
            .erased_callable_incref => try self.emitRcHelperErasedCallableIncref(value_ptr, count_value.?),
            .erased_callable_decref => try self.emitRcHelperErasedCallableDrop(value_ptr, "roc_builtins_erased_callable_decref"),
            .erased_callable_free => try self.emitRcHelperErasedCallableDrop(value_ptr, "roc_builtins_erased_callable_free"),
            .struct_ => |struct_plan| try self.emitRcHelperStruct(struct_plan, value_ptr, count_value),
            .tag_union => |tag_plan| try self.emitRcHelperTagUnion(tag_plan, value_ptr, count_value),
            .closure => |child_key| {
                const captures_ptr = try self.loadPointer(value_ptr);
                try self.emitRcHelperCall(child_key, captures_ptr, if (child_key.op == .incref) count_value else null);
            },
        }
    }

    fn emitRcHelperCall(self: *MonoLlvmCodeGen, helper_key: layout.RcHelperKey, value_ptr: LlvmBuilder.Value, count_value: ?LlvmBuilder.Value) Error!void {
        const func = (try self.declareRcHelper(helper_key)) orelse return;
        switch (helper_key.op) {
            .incref => _ = try self.callFunctionIndex(func, &.{ value_ptr, count_value.?, self.rocOps() }),
            .decref, .free => _ = try self.callFunctionIndex(func, &.{ value_ptr, self.rocOps() }),
        }
    }

    fn loadStrDataPtrForRc(self: *MonoLlvmCodeGen, value_ptr: LlvmBuilder.Value) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const bytes = try self.loadPointer(value_ptr);
        const cap_or_alloc = try self.loadUsize(try self.offsetPtr(value_ptr, @sizeOf(usize)));
        const ptr_int_ty = self.ptrSizedIntType();
        const bytes_int = wip.cast(.ptrtoint, bytes, ptr_int_ty, "") catch return error.OutOfMemory;
        const slice_tag = wip.bin(.@"and", cap_or_alloc, builder.intValue(ptr_int_ty, 1) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const slice_mask = wip.bin(.sub, builder.intValue(ptr_int_ty, 0) catch return error.OutOfMemory, slice_tag, "") catch return error.OutOfMemory;
        const owned_mask = wip.not(slice_mask, "") catch return error.OutOfMemory;
        const owned_ptr = wip.bin(.@"and", bytes_int, owned_mask, "") catch return error.OutOfMemory;
        const slice_alloc = wip.bin(.@"and", cap_or_alloc, builder.intValue(ptr_int_ty, -2) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const slice_ptr = wip.bin(.@"and", slice_alloc, slice_mask, "") catch return error.OutOfMemory;
        const data_ptr = wip.bin(.@"or", owned_ptr, slice_ptr, "") catch return error.OutOfMemory;
        return wip.cast(.inttoptr, data_ptr, try self.ptrType(), "") catch return error.OutOfMemory;
    }

    fn emitRcHelperStrIncref(self: *MonoLlvmCodeGen, value_ptr: LlvmBuilder.Value, count_value: LlvmBuilder.Value) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const len = try self.loadUsize(try self.offsetPtr(value_ptr, 2 * @sizeOf(usize)));
        const is_small = wip.icmp(.slt, len, builder.intValue(self.ptrSizedIntType(), 0) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const heap_str = wip.block(0, "str_heap") catch return error.OutOfMemory;
        const after = wip.block(0, "str_after") catch return error.OutOfMemory;
        _ = wip.brCond(is_small, after, heap_str, .else_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = heap_str };
        const data_ptr = try self.loadStrDataPtrForRc(value_ptr);
        try self.callBuiltinVoid("roc_builtins_incref_data_ptr", &.{ try self.ptrType(), .i64, try self.ptrType() }, &.{ data_ptr, count_value, self.rocOps() });
        _ = wip.br(after) catch return error.OutOfMemory;

        wip.cursor = .{ .block = after };
    }

    fn emitRcHelperStrDrop(self: *MonoLlvmCodeGen, value_ptr: LlvmBuilder.Value, builtin_name: []const u8) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const len = try self.loadUsize(try self.offsetPtr(value_ptr, 2 * @sizeOf(usize)));
        const is_small = wip.icmp(.slt, len, builder.intValue(self.ptrSizedIntType(), 0) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
        const heap_str = wip.block(0, "str_heap") catch return error.OutOfMemory;
        const after = wip.block(0, "str_after") catch return error.OutOfMemory;
        _ = wip.brCond(is_small, after, heap_str, .else_likely) catch return error.OutOfMemory;

        wip.cursor = .{ .block = heap_str };
        const data_ptr = try self.loadStrDataPtrForRc(value_ptr);
        try self.callBuiltinVoid(
            builtin_name,
            &.{ try self.ptrType(), .i32, .i1, try self.ptrType() },
            &.{
                data_ptr,
                builder.intValue(.i32, 1) catch return error.OutOfMemory,
                builder.intValue(.i1, 0) catch return error.OutOfMemory,
                self.rocOps(),
            },
        );
        _ = wip.br(after) catch return error.OutOfMemory;

        wip.cursor = .{ .block = after };
    }

    fn emitRcHelperListIncref(self: *MonoLlvmCodeGen, list_plan: layout.RcListPlan, value_ptr: LlvmBuilder.Value, count_value: LlvmBuilder.Value) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const fields = try self.rocListArgFields(value_ptr);
        try self.callBuiltinVoid(
            "roc_builtins_list_incref",
            &.{ try self.ptrType(), self.ptrSizedIntType(), self.ptrSizedIntType(), .i64, .i1, try self.ptrType() },
            &.{
                fields[0],
                fields[1],
                fields[2],
                count_value,
                builder.intValue(.i1, @intFromBool(list_plan.child != null)) catch return error.OutOfMemory,
                self.rocOps(),
            },
        );
    }

    fn emitRcHelperListDrop(self: *MonoLlvmCodeGen, list_plan: layout.RcListPlan, value_ptr: LlvmBuilder.Value, builtin_name: []const u8) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const fields = try self.rocListArgFields(value_ptr);
        const child_fn = if (list_plan.child) |child_key|
            (try self.declareRcHelper(child_key))
        else
            null;
        try self.callBuiltinVoid(
            builtin_name,
            &.{ try self.ptrType(), self.ptrSizedIntType(), self.ptrSizedIntType(), .i32, self.ptrSizedIntType(), try self.ptrType(), try self.ptrType() },
            &.{
                fields[0],
                fields[1],
                fields[2],
                builder.intValue(.i32, list_plan.elem_alignment) catch return error.OutOfMemory,
                builder.intValue(self.ptrSizedIntType(), list_plan.elem_width) catch return error.OutOfMemory,
                if (child_fn) |func| func.toValue(builder) else builder.nullValue(try self.ptrType()) catch return error.OutOfMemory,
                self.rocOps(),
            },
        );
    }

    fn emitRcHelperBoxIncref(self: *MonoLlvmCodeGen, value_ptr: LlvmBuilder.Value, count_value: LlvmBuilder.Value) Error!void {
        const payload_ptr = try self.loadPointer(value_ptr);
        try self.callBuiltinVoid(
            "roc_builtins_incref_data_ptr",
            &.{ try self.ptrType(), .i64, try self.ptrType() },
            &.{ payload_ptr, count_value, self.rocOps() },
        );
    }

    fn emitRcHelperBoxDrop(self: *MonoLlvmCodeGen, box_plan: layout.RcBoxPlan, value_ptr: LlvmBuilder.Value, builtin_name: []const u8) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const payload_ptr = try self.loadPointer(value_ptr);
        const child_fn = if (box_plan.child) |child_key|
            (try self.declareRcHelper(child_key))
        else
            null;
        try self.callBuiltinVoid(
            builtin_name,
            &.{ try self.ptrType(), .i32, try self.ptrType(), try self.ptrType() },
            &.{
                payload_ptr,
                builder.intValue(.i32, box_plan.elem_alignment) catch return error.OutOfMemory,
                if (child_fn) |func| func.toValue(builder) else builder.nullValue(try self.ptrType()) catch return error.OutOfMemory,
                self.rocOps(),
            },
        );
    }

    fn emitRcHelperErasedCallableIncref(self: *MonoLlvmCodeGen, value_ptr: LlvmBuilder.Value, count_value: LlvmBuilder.Value) Error!void {
        const payload_ptr = try self.loadPointer(value_ptr);
        try self.callBuiltinVoid(
            "roc_builtins_erased_callable_incref",
            &.{ try self.ptrType(), .i64, try self.ptrType() },
            &.{ payload_ptr, count_value, self.rocOps() },
        );
    }

    fn emitRcHelperErasedCallableDrop(self: *MonoLlvmCodeGen, value_ptr: LlvmBuilder.Value, builtin_name: []const u8) Error!void {
        const payload_ptr = try self.loadPointer(value_ptr);
        try self.callBuiltinVoid(
            builtin_name,
            &.{ try self.ptrType(), try self.ptrType() },
            &.{ payload_ptr, self.rocOps() },
        );
    }

    fn emitRcHelperStruct(self: *MonoLlvmCodeGen, struct_plan: layout.RcStructPlan, value_ptr: LlvmBuilder.Value, count_value: ?LlvmBuilder.Value) Error!void {
        const field_count = self.layouts().rcHelperStructFieldCount(struct_plan);
        var i: u32 = 0;
        while (i < field_count) : (i += 1) {
            const field_plan = self.layouts().rcHelperStructFieldPlan(struct_plan, i) orelse continue;
            const field_ptr = try self.offsetPtr(value_ptr, field_plan.offset);
            try self.emitRcHelperCall(field_plan.child, field_ptr, if (field_plan.child.op == .incref) count_value else null);
        }
    }

    fn emitRcHelperTagUnion(self: *MonoLlvmCodeGen, tag_plan: layout.RcTagUnionPlan, value_ptr: LlvmBuilder.Value, count_value: ?LlvmBuilder.Value) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const variant_count = self.layouts().rcHelperTagUnionVariantCount(tag_plan);
        if (variant_count == 0) return;

        if (variant_count == 1) {
            if (self.layouts().rcHelperTagUnionVariantPlan(tag_plan, 0)) |child_key| {
                try self.emitRcHelperCall(child_key, value_ptr, if (child_key.op == .incref) count_value else null);
            }
            return;
        }

        const disc_size = self.layouts().rcHelperTagUnionDiscriminantSize(tag_plan);
        if (disc_size == 0) return;
        const disc_offset = self.layouts().rcHelperTagUnionDiscriminantOffset(tag_plan);
        const disc_ptr = try self.offsetPtr(value_ptr, disc_offset);
        const disc_raw = wip.load(.normal, intTypeForBytes(disc_size), disc_ptr, LlvmBuilder.Alignment.fromByteUnits(@max(disc_size, 1)), "") catch return error.OutOfMemory;
        const disc = try self.coerceScalar(disc_raw, .i64, false);
        const after = wip.block(0, "rc_tag_after") catch return error.OutOfMemory;

        var variant_i: u32 = 0;
        while (variant_i < variant_count) : (variant_i += 1) {
            const child_key = self.layouts().rcHelperTagUnionVariantPlan(tag_plan, variant_i) orelse continue;
            const do_case = wip.block(0, "rc_tag_case") catch return error.OutOfMemory;
            const next_case = wip.block(0, "rc_tag_next") catch return error.OutOfMemory;
            const is_case = wip.icmp(.eq, disc, builder.intValue(.i64, variant_i) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
            _ = wip.brCond(is_case, do_case, next_case, .none) catch return error.OutOfMemory;

            wip.cursor = .{ .block = do_case };
            try self.emitRcHelperCall(child_key, value_ptr, if (child_key.op == .incref) count_value else null);
            _ = wip.br(after) catch return error.OutOfMemory;

            wip.cursor = .{ .block = next_case };
        }

        _ = wip.br(after) catch return error.OutOfMemory;
        wip.cursor = .{ .block = after };
    }

    fn ptrType(self: *MonoLlvmCodeGen) Error!LlvmBuilder.Type {
        return (self.builder orelse return error.CompilationFailed).ptrType(.default) catch return error.OutOfMemory;
    }

    fn ptrSizedIntType(self: *const MonoLlvmCodeGen) LlvmBuilder.Type {
        return switch (self.target.ptrBitWidth()) {
            16 => .i16,
            32 => .i32,
            64 => .i64,
            else => .i64,
        };
    }

    fn rocOps(self: *MonoLlvmCodeGen) LlvmBuilder.Value {
        return self.roc_ops_arg orelse unreachable;
    }

    fn layouts(self: *MonoLlvmCodeGen) *const layout.Store {
        return self.layout_store orelse @panic("LLVM codegen missing layout_store");
    }

    fn layoutValue(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) layout.Layout {
        return self.layouts().getLayout(layout_idx);
    }

    fn localLayout(self: *MonoLlvmCodeGen, local: LocalId) layout.Idx {
        return self.store.getLocal(local).layout_idx;
    }

    fn slot(self: *MonoLlvmCodeGen, local: LocalId) LocalSlot {
        return self.local_slots[@intFromEnum(local)];
    }

    fn sizeAlignOf(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) layout.SizeAlign {
        return self.layouts().layoutSizeAlign(self.layoutValue(layout_idx));
    }

    fn layoutByteSize(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) u32 {
        return self.sizeAlignOf(layout_idx).size;
    }

    fn llvmAlignment(_: *MonoLlvmCodeGen, roc_alignment: layout.RocAlignment) LlvmBuilder.Alignment {
        return LlvmBuilder.Alignment.fromByteUnits(@max(roc_alignment.toByteUnits(), 1));
    }

    fn alignmentForLayout(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) LlvmBuilder.Alignment {
        return self.llvmAlignment(self.sizeAlignOf(layout_idx).alignment);
    }

    fn offsetPtr(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, offset: u32) Error!LlvmBuilder.Value {
        if (offset == 0) return ptr;
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        return wip.gep(.inbounds, .i8, ptr, &.{builder.intValue(.i32, offset) catch return error.OutOfMemory}, "") catch return error.OutOfMemory;
    }

    fn copyBytes(self: *MonoLlvmCodeGen, dst: LlvmBuilder.Value, src: LlvmBuilder.Value, size: u32, alignment: LlvmBuilder.Alignment) Error!void {
        if (size == 0) return;
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        _ = wip.callMemCpy(dst, alignment, src, alignment, builder.intValue(self.ptrSizedIntType(), size) catch return error.OutOfMemory, .normal, false) catch return error.OutOfMemory;
    }

    fn zeroBytes(self: *MonoLlvmCodeGen, dst: LlvmBuilder.Value, size: u32) Error!void {
        if (size == 0) return;
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        _ = wip.callMemSet(dst, LlvmBuilder.Alignment.fromByteUnits(1), builder.intValue(.i8, 0) catch return error.OutOfMemory, builder.intValue(self.ptrSizedIntType(), size) catch return error.OutOfMemory, .normal, false) catch return error.OutOfMemory;
    }

    fn storePointer(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, value: LlvmBuilder.Value) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        _ = wip.store(.normal, value, ptr, LlvmBuilder.Alignment.fromByteUnits(@sizeOf(usize))) catch return error.OutOfMemory;
    }

    fn storeUsize(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, value: LlvmBuilder.Value) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        _ = wip.store(.normal, value, ptr, LlvmBuilder.Alignment.fromByteUnits(@sizeOf(usize))) catch return error.OutOfMemory;
    }

    fn storeRawInt(self: *MonoLlvmCodeGen, base: LlvmBuilder.Value, offset: u32, ty: LlvmBuilder.Type, value: u64, alignment: u32) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const ptr = try self.offsetPtr(base, offset);
        _ = wip.store(
            .normal,
            builder.intValue(ty, @as(i64, @intCast(value))) catch return error.OutOfMemory,
            ptr,
            LlvmBuilder.Alignment.fromByteUnits(alignment),
        ) catch return error.OutOfMemory;
    }

    fn loadPointer(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        return wip.load(.normal, try self.ptrType(), ptr, LlvmBuilder.Alignment.fromByteUnits(@sizeOf(usize)), "") catch return error.OutOfMemory;
    }

    fn loadUsize(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        return wip.load(.normal, self.ptrSizedIntType(), ptr, LlvmBuilder.Alignment.fromByteUnits(@sizeOf(usize)), "") catch return error.OutOfMemory;
    }

    fn scalarType(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) LlvmBuilder.Type {
        return switch (layout_idx) {
            .bool, .u8, .i8 => .i8,
            .u16, .i16 => .i16,
            .u32, .i32 => .i32,
            .u64, .i64 => .i64,
            .u128, .i128, .dec => .i128,
            .f32 => .float,
            .f64 => .double,
            .opaque_ptr => self.ptrSizedIntType(),
            else => self.ptrSizedIntType(),
        };
    }

    fn loadScalar(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, layout_idx: layout.Idx) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        return wip.load(.normal, self.scalarType(layout_idx), ptr, self.alignmentForLayout(layout_idx), "") catch return error.OutOfMemory;
    }

    fn storeScalar(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, layout_idx: layout.Idx, value: LlvmBuilder.Value) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const store_value = try self.coerceScalar(value, self.scalarType(layout_idx), layout_idx.isSigned());
        _ = wip.store(.normal, store_value, ptr, self.alignmentForLayout(layout_idx)) catch return error.OutOfMemory;
    }

    fn storeIntToLayout(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, value: LlvmBuilder.Value, layout_idx: layout.Idx) Error!void {
        try self.storeScalar(ptr, layout_idx, value);
    }

    fn storeIntLiteral(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, layout_idx: layout.Idx, value: i64) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        try self.storeScalar(ptr, layout_idx, builder.intValue(self.scalarType(layout_idx), value) catch return error.OutOfMemory);
    }

    fn storeI128Literal(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, layout_idx: layout.Idx, value: i128) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        try self.storeScalar(ptr, layout_idx, builder.intValue(self.scalarType(layout_idx), value) catch return error.OutOfMemory);
    }

    fn storeFloatLiteral(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, layout_idx: layout.Idx, value: anytype) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const llvm_value = if (layout_idx == .f32)
            builder.floatValue(@floatCast(value)) catch return error.OutOfMemory
        else
            builder.doubleValue(@floatCast(value)) catch return error.OutOfMemory;
        try self.storeScalar(ptr, layout_idx, llvm_value);
    }

    fn coerceScalar(self: *MonoLlvmCodeGen, value: LlvmBuilder.Value, target_ty: LlvmBuilder.Type, signed: bool) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const value_ty = value.typeOfWip(wip);
        if (value_ty == target_ty) return value;
        return wip.conv(if (signed) .signed else .unsigned, value, target_ty, "") catch return error.OutOfMemory;
    }

    fn loadBool(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const byte = wip.load(.normal, .i8, ptr, LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.OutOfMemory;
        return wip.icmp(.ne, byte, builder.intValue(.i8, 0) catch return error.OutOfMemory, "") catch return error.OutOfMemory;
    }

    fn storeBool(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, cond: LlvmBuilder.Value) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const byte = try self.coerceScalar(cond, .i8, false);
        _ = wip.store(.normal, byte, ptr, LlvmBuilder.Alignment.fromByteUnits(1)) catch return error.OutOfMemory;
    }

    fn readSwitchValue(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, layout_idx: layout.Idx) Error!LlvmBuilder.Value {
        const layout_val = self.layoutValue(layout_idx);
        if (layout_val.tag == .tag_union) {
            return self.readTagDiscriminant(ptr, layout_idx);
        }
        const value = if (layout_idx == .bool) blk: {
            const b = try self.loadBool(ptr);
            break :blk try self.coerceScalar(b, .i64, false);
        } else try self.loadScalar(ptr, layout_idx);
        return self.coerceScalar(value, .i64, false);
    }

    fn readTagDiscriminant(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, layout_idx: layout.Idx) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const layout_val = self.layoutValue(layout_idx);
        if (layout_val.tag != .tag_union) return builder.intValue(.i64, 0) catch return error.OutOfMemory;
        const data = self.layouts().getTagUnionData(layout_val.getTagUnion().idx);
        if (data.discriminant_size == 0) return builder.intValue(.i64, 0) catch return error.OutOfMemory;
        const disc_ptr = try self.offsetPtr(ptr, data.discriminant_offset);
        const ty = intTypeForBytes(data.discriminant_size);
        const raw = wip.load(.normal, ty, disc_ptr, LlvmBuilder.Alignment.fromByteUnits(@max(data.discriminant_size, 1)), "") catch return error.OutOfMemory;
        return self.coerceScalar(raw, .i64, false);
    }

    fn tagDiscriminantOffset(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) Error!u32 {
        const layout_val = self.layoutValue(layout_idx);
        if (layout_val.tag != .tag_union) return error.CompilationFailed;
        const data = self.layouts().getTagUnionData(layout_val.getTagUnion().idx);
        return data.discriminant_offset;
    }

    fn writeTagDiscriminant(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, layout_idx: layout.Idx, discriminant: u16) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const layout_val = self.layoutValue(layout_idx);
        if (layout_val.tag != .tag_union) return;
        const data = self.layouts().getTagUnionData(layout_val.getTagUnion().idx);
        if (data.discriminant_size == 0) return;
        const disc_ptr = try self.offsetPtr(ptr, data.discriminant_offset);
        const ty = intTypeForBytes(data.discriminant_size);
        _ = wip.store(.normal, builder.intValue(ty, discriminant) catch return error.OutOfMemory, disc_ptr, LlvmBuilder.Alignment.fromByteUnits(@max(data.discriminant_size, 1))) catch return error.OutOfMemory;
    }

    fn tagPayloadLayout(self: *MonoLlvmCodeGen, layout_idx: layout.Idx, discriminant: u16) layout.Idx {
        var tag_layout = self.layoutValue(layout_idx);
        if (tag_layout.tag == .box) tag_layout = self.layoutValue(tag_layout.getIdx());
        if (tag_layout.tag != .tag_union) return .zst;
        const data = self.layouts().getTagUnionData(tag_layout.getTagUnion().idx);
        const variants = self.layouts().getTagUnionVariants(data);
        if (discriminant >= variants.len) return .zst;
        return variants.get(discriminant).payload_layout;
    }

    fn resolveStrFromUtf8Layout(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) Error!StrFromUtf8LayoutInfo {
        const ret_layout_val = self.layoutValue(layout_idx);
        if (ret_layout_val.tag != .tag_union) return error.CompilationFailed;
        const tu_data = self.layouts().getTagUnionData(ret_layout_val.getTagUnion().idx);
        const variants = self.layouts().getTagUnionVariants(tu_data);

        var ok_disc: ?u16 = null;
        var err_disc: ?u16 = null;
        var err_record_idx: ?layout.StructIdx = null;
        var inner_disc_offset: u32 = 0;
        var inner_disc_size: u32 = 0;
        var inner_bad_utf8_disc: u32 = 0;

        for (0..variants.len) |i| {
            const payload = variants.get(@intCast(i)).payload_layout;
            const candidate = self.unwrapSingleFieldPayloadLayout(payload) orelse payload;
            if (candidate == .str) {
                ok_disc = @intCast(i);
                continue;
            }

            err_disc = @intCast(i);
            const err_layout = self.layoutValue(candidate);
            switch (err_layout.tag) {
                .struct_ => err_record_idx = err_layout.getStruct().idx,
                .tag_union => {
                    const inner_tu = self.layouts().getTagUnionData(err_layout.getTagUnion().idx);
                    if (self.findBadUtf8Variant(inner_tu)) |info| {
                        err_record_idx = info.struct_idx;
                        inner_disc_offset = inner_tu.discriminant_offset;
                        inner_disc_size = inner_tu.discriminant_size;
                        inner_bad_utf8_disc = info.disc;
                    }
                },
                else => {},
            }
        }

        const rec_idx = err_record_idx orelse return error.CompilationFailed;
        const struct_data = self.layouts().getStructData(rec_idx);
        const fields = self.layouts().struct_fields.sliceRange(struct_data.getFields());
        var index_offset: ?u32 = null;
        var problem_offset: ?u32 = null;
        for (0..fields.len) |i| {
            const field = fields.get(i);
            const field_layout = self.layoutValue(field.layout);
            const field_size = self.layoutByteSize(field.layout);
            const field_offset = self.layouts().getStructFieldOffsetByOriginalIndex(rec_idx, field.index);
            const is_index = switch (field_layout.tag) {
                .scalar => field_layout.getScalar().tag == .int and switch (field_layout.getScalar().getInt()) {
                    .u64, .i64 => true,
                    else => false,
                },
                else => false,
            };
            if (is_index and field_size == 8) {
                index_offset = field_offset;
            } else if (field_size == 1) {
                problem_offset = field_offset;
            }
        }

        return .{
            .ok_tag = ok_disc orelse return error.CompilationFailed,
            .err_tag = err_disc orelse return error.CompilationFailed,
            .outer_disc_offset = tu_data.discriminant_offset,
            .outer_disc_size = tu_data.discriminant_size,
            .err_index_offset = index_offset orelse return error.CompilationFailed,
            .err_problem_offset = problem_offset orelse return error.CompilationFailed,
            .inner_disc_offset = inner_disc_offset,
            .inner_disc_size = inner_disc_size,
            .inner_bad_utf8_tag = inner_bad_utf8_disc,
        };
    }

    fn unwrapSingleFieldPayloadLayout(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) ?layout.Idx {
        const layout_val = self.layoutValue(layout_idx);
        if (layout_val.tag != .struct_) return null;

        const struct_data = self.layouts().getStructData(layout_val.getStruct().idx);
        const fields = self.layouts().struct_fields.sliceRange(struct_data.getFields());
        if (fields.len != 1) return null;

        const field = fields.get(0);
        if (field.index != 0) return null;
        return field.layout;
    }

    fn findBadUtf8Variant(self: *MonoLlvmCodeGen, inner_tu: *const layout.TagUnionData) ?struct { disc: u16, struct_idx: layout.StructIdx } {
        const variants = self.layouts().getTagUnionVariants(inner_tu);
        for (0..variants.len) |i| {
            const payload = variants.get(@intCast(i)).payload_layout;
            const candidate = self.unwrapSingleFieldPayloadLayout(payload) orelse payload;
            const payload_layout = self.layoutValue(candidate);
            if (payload_layout.tag != .struct_) continue;

            const struct_idx = payload_layout.getStruct().idx;
            const struct_data = self.layouts().getStructData(struct_idx);
            const fields = self.layouts().struct_fields.sliceRange(struct_data.getFields());
            if (fields.len != 2) continue;

            var has_index_field = false;
            var has_problem_field = false;
            for (0..fields.len) |field_i| {
                const field = fields.get(field_i);
                const field_size = self.layoutByteSize(field.layout);
                switch (field.index) {
                    0 => has_index_field = field_size == 8,
                    1 => has_problem_field = field_size == 1,
                    else => {},
                }
            }
            if (has_index_field and has_problem_field) {
                return .{ .disc = @intCast(i), .struct_idx = struct_idx };
            }
        }

        return null;
    }

    fn resolveStructBase(self: *MonoLlvmCodeGen, source: LocalId) Error!ResolvedBase {
        const source_layout = self.localLayout(source);
        const layout_val = self.layoutValue(source_layout);
        return switch (layout_val.tag) {
            .box => .{ .ptr = try self.loadPointer(self.slot(source).ptr), .layout_idx = layout_val.getIdx() },
            .box_of_zst => .{ .ptr = self.slot(source).ptr, .layout_idx = .zst },
            else => .{ .ptr = self.slot(source).ptr, .layout_idx = source_layout },
        };
    }

    fn resolveTagBase(self: *MonoLlvmCodeGen, source: LocalId) Error!ResolvedBase {
        return self.resolveStructBase(source);
    }

    fn rocStrArgFields(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value) Error![3]LlvmBuilder.Value {
        return .{
            try self.loadPointer(ptr),
            try self.loadUsize(try self.offsetPtr(ptr, 2 * @sizeOf(usize))),
            try self.loadUsize(try self.offsetPtr(ptr, @sizeOf(usize))),
        };
    }

    fn rocListArgFields(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value) Error![3]LlvmBuilder.Value {
        return .{
            try self.loadPointer(ptr),
            try self.loadUsize(try self.offsetPtr(ptr, @sizeOf(usize))),
            try self.loadUsize(try self.offsetPtr(ptr, 2 * @sizeOf(usize))),
        };
    }

    fn storeListFields(self: *MonoLlvmCodeGen, ptr: LlvmBuilder.Value, bytes: LlvmBuilder.Value, len: usize, cap: usize) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        try self.storePointer(ptr, bytes);
        try self.storeScalar(try self.offsetPtr(ptr, @sizeOf(usize)), .u64, builder.intValue(self.ptrSizedIntType(), len) catch return error.OutOfMemory);
        try self.storeScalar(try self.offsetPtr(ptr, 2 * @sizeOf(usize)), .u64, builder.intValue(self.ptrSizedIntType(), cap) catch return error.OutOfMemory);
    }

    fn entrypointParamSlotSize(self: *MonoLlvmCodeGen, layout_idx: layout.Idx) u32 {
        const runtime_layout_idx = self.layouts().runtimeRepresentationLayoutIdx(layout_idx);
        if (runtime_layout_idx == .str) return 24;
        if (runtime_layout_idx == .i128 or runtime_layout_idx == .u128 or runtime_layout_idx == .dec) return 16;
        const layout_val = self.layoutValue(runtime_layout_idx);
        const size = self.layoutByteSize(runtime_layout_idx);
        if (layout_val.tag == .zst or size == 0) return 0;
        if (layout_val.tag == .list or layout_val.tag == .list_of_zst) return 24;
        if (layout_val.tag == .struct_ or layout_val.tag == .tag_union) {
            if (size > 8) return @intCast(std.mem.alignForward(u32, size, 8));
        }
        return 8;
    }

    fn computeArgOffsets(self: *MonoLlvmCodeGen, arg_layouts: []const layout.Idx, rounded_slots: bool) Error![]u32 {
        const ordered = try self.allocator.alloc(ArgOrder, arg_layouts.len);
        defer self.allocator.free(ordered);
        const offsets = try self.allocator.alloc(u32, arg_layouts.len);
        for (arg_layouts, 0..) |arg_layout, i| {
            const sa = self.sizeAlignOf(arg_layout);
            ordered[i] = .{
                .index = i,
                .alignment = @intCast(@max(sa.alignment.toByteUnits(), 1)),
                .size = if (rounded_slots) self.entrypointParamSlotSize(arg_layout) else sa.size,
            };
        }
        const SortCtx = struct {
            fn lessThan(_: void, lhs: ArgOrder, rhs: ArgOrder) bool {
                if (lhs.alignment != rhs.alignment) return lhs.alignment > rhs.alignment;
                return lhs.index < rhs.index;
            }
        };
        std.mem.sort(ArgOrder, ordered, {}, SortCtx.lessThan);
        var current: u32 = 0;
        for (ordered) |arg| {
            current = std.mem.alignForward(u32, current, arg.alignment);
            offsets[arg.index] = current;
            current += arg.size;
        }
        return offsets;
    }

    fn argBufferSize(self: *MonoLlvmCodeGen, arg_layouts: []const layout.Idx, rounded_slots: bool) Error!u32 {
        if (arg_layouts.len == 0) return 8;
        const offsets = try self.computeArgOffsets(arg_layouts, rounded_slots);
        defer self.allocator.free(offsets);
        var total: u32 = 0;
        for (arg_layouts, offsets) |arg_layout, offset| {
            const slot_size = if (rounded_slots) self.entrypointParamSlotSize(arg_layout) else self.layoutByteSize(arg_layout);
            total = @max(total, offset + slot_size);
        }
        return @max(total, 8);
    }

    fn allocArgBuffer(self: *MonoLlvmCodeGen, arg_layouts: []const layout.Idx, rounded_slots: bool) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const size = try self.argBufferSize(arg_layouts, rounded_slots);
        const ptr = wip.alloca(.normal, .i8, builder.intValue(.i32, size) catch return error.OutOfMemory, LlvmBuilder.Alignment.fromByteUnits(16), .default, "args") catch return error.OutOfMemory;
        try self.zeroBytes(ptr, size);
        return ptr;
    }

    fn allocHostedArgBuffer(self: *MonoLlvmCodeGen, arg_layouts: []const layout.Idx) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        var total: u32 = 0;
        for (arg_layouts) |arg_layout| {
            const sa = self.sizeAlignOf(arg_layout);
            total = std.mem.alignForward(u32, total, @intCast(@max(sa.alignment.toByteUnits(), 1)));
            total += sa.size;
        }
        total = @max(total, 8);
        const ptr = wip.alloca(.normal, .i8, builder.intValue(.i32, total) catch return error.OutOfMemory, LlvmBuilder.Alignment.fromByteUnits(16), .default, "host_args") catch return error.OutOfMemory;
        try self.zeroBytes(ptr, total);
        return ptr;
    }

    fn copyEntrypointArgsToInternalBuffer(self: *MonoLlvmCodeGen, src_args: LlvmBuilder.Value, dst_args: LlvmBuilder.Value, arg_layouts: []const layout.Idx) Error!void {
        const offsets = try self.computeArgOffsets(arg_layouts, true);
        defer self.allocator.free(offsets);
        for (arg_layouts, offsets) |arg_layout, offset| {
            const size = self.layoutByteSize(arg_layout);
            if (size == 0) continue;
            try self.copyBytes(try self.offsetPtr(dst_args, offset), try self.offsetPtr(src_args, offset), size, self.alignmentForLayout(arg_layout));
        }
    }

    fn packRocArgsFromLocals(self: *MonoLlvmCodeGen, dst_args: LlvmBuilder.Value, arg_locals: []const LocalId, arg_layouts: []const layout.Idx) Error!void {
        const offsets = try self.computeArgOffsets(arg_layouts, true);
        defer self.allocator.free(offsets);
        for (arg_locals, arg_layouts, offsets) |arg, arg_layout, offset| {
            const size = self.layoutByteSize(arg_layout);
            if (size == 0) continue;
            try self.copyBytes(try self.offsetPtr(dst_args, offset), self.slot(arg).ptr, size, self.alignmentForLayout(arg_layout));
        }
    }

    fn packSequentialArgsFromLocals(self: *MonoLlvmCodeGen, dst_args: LlvmBuilder.Value, arg_locals: []const LocalId, arg_layouts: []const layout.Idx) Error!void {
        var offset: u32 = 0;
        for (arg_locals, arg_layouts) |arg, arg_layout| {
            const sa = self.sizeAlignOf(arg_layout);
            offset = std.mem.alignForward(u32, offset, @intCast(@max(sa.alignment.toByteUnits(), 1)));
            if (sa.size > 0) {
                try self.copyBytes(try self.offsetPtr(dst_args, offset), self.slot(arg).ptr, sa.size, self.alignmentForLayout(arg_layout));
            }
            offset += sa.size;
        }
    }

    fn callHostedFunction(self: *MonoLlvmCodeGen, dispatch_index: u32, ret_ptr: LlvmBuilder.Value, args_ptr: LlvmBuilder.Value) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const ptr_ty = try self.ptrType();
        const hosted_fns_offset: u32 = @intCast(@offsetOf(builtins.host_abi.RocOps, "hosted_fns"));
        const fns_offset: u32 = hosted_fns_offset + @as(u32, @intCast(@offsetOf(builtins.host_abi.HostedFunctions, "fns")));
        const table_ptr_ptr = try self.offsetPtr(self.rocOps(), fns_offset);
        const table_ptr = try self.loadPointer(table_ptr_ptr);
        const fn_ptr_ptr = try self.offsetPtr(table_ptr, dispatch_index * @as(u32, @intCast(@sizeOf(builtins.host_abi.HostedFn))));
        const fn_ptr = try self.loadPointer(fn_ptr_ptr);
        const fn_ty = builder.fnType(.void, &.{ ptr_ty, ptr_ty, ptr_ty }, .normal) catch return error.OutOfMemory;
        _ = wip.call(.normal, .ccc, .none, fn_ty, fn_ptr, &.{ self.rocOps(), ret_ptr, args_ptr }, "") catch return error.OutOfMemory;
    }

    fn callBuiltin(
        self: *MonoLlvmCodeGen,
        name: []const u8,
        ret_type: LlvmBuilder.Type,
        param_types: []const LlvmBuilder.Type,
        args: []const LlvmBuilder.Value,
    ) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const func = try self.declareBuiltin(name, ret_type, param_types);
        return wip.call(.normal, .ccc, .none, func.typeOf(builder), func.toValue(builder), args, "") catch return error.OutOfMemory;
    }

    fn callBuiltinVoid(self: *MonoLlvmCodeGen, name: []const u8, param_types: []const LlvmBuilder.Type, args: []const LlvmBuilder.Value) Error!void {
        _ = try self.callBuiltin(name, .void, param_types, args);
    }

    fn declareBuiltin(self: *MonoLlvmCodeGen, name: []const u8, ret_type: LlvmBuilder.Type, param_types: []const LlvmBuilder.Type) Error!LlvmBuilder.Function.Index {
        const builder = self.builder orelse return error.CompilationFailed;
        if (self.builtin_functions.get(name)) |func| return func;
        const fn_ty = builder.fnType(ret_type, param_types, .normal) catch return error.OutOfMemory;
        const fn_name = if (self.builtin_symbol_mode == .native_object)
            try self.exportedFunctionName(builder, name)
        else
            builder.strtabString(name) catch return error.OutOfMemory;
        const func = builder.addFunction(fn_ty, fn_name, .default) catch return error.OutOfMemory;
        try self.builtin_functions.put(name, func);
        return func;
    }

    fn callFunctionIndex(self: *MonoLlvmCodeGen, func: LlvmBuilder.Function.Index, args: []const LlvmBuilder.Value) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        return wip.call(.normal, .ccc, .none, func.typeOf(builder), func.toValue(builder), args, "") catch return error.OutOfMemory;
    }

    fn currentBlockHasTerminator(self: *const MonoLlvmCodeGen) bool {
        const wip = self.wip orelse return false;
        const block = wip.cursor.block.ptrConst(wip);
        if (block.instructions.items.len == 0) return false;
        return block.instructions.items[block.instructions.items.len - 1].isTerminatorWip(wip);
    }

    fn finishCurrentWipFunction(self: *MonoLlvmCodeGen) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;

        for (wip.blocks.items, 0..) |*block, block_idx| {
            if (block.instructions.items.len == 0 or !block.instructions.items[block.instructions.items.len - 1].isTerminatorWip(wip)) {
                wip.cursor = .{ .block = @enumFromInt(block_idx) };
                _ = wip.@"unreachable"() catch return error.OutOfMemory;
            }
        }

        for (wip.blocks.items) |*block| {
            for (block.instructions.items) |instruction| {
                switch (wip.instructions.get(@intFromEnum(instruction)).tag) {
                    .phi, .@"phi fast" => if (block.incoming != block.branches) return error.CompilationFailed,
                    else => {},
                }
            }
            block.incoming = block.branches;
        }

        wip.finish() catch return error.OutOfMemory;
    }

    fn intBits(_: *MonoLlvmCodeGen, layout_idx: layout.Idx) u32 {
        return switch (layout_idx) {
            .bool, .u8, .i8 => 8,
            .u16, .i16 => 16,
            .u32, .i32, .f32 => 32,
            .u64, .i64, .f64, .opaque_ptr => 64,
            .u128, .i128, .dec => 128,
            else => 64,
        };
    }
};

fn isFloatLayout(layout_idx: layout.Idx) bool {
    return layout_idx == .f32 or layout_idx == .f64;
}

fn isIntegerLayout(layout_idx: layout.Idx) bool {
    return switch (layout_idx) {
        .bool, .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 => true,
        else => false,
    };
}

fn intTypeForBytes(size: u8) LlvmBuilder.Type {
    return switch (size) {
        1 => .i8,
        2 => .i16,
        4 => .i32,
        8 => .i64,
        else => .i64,
    };
}
