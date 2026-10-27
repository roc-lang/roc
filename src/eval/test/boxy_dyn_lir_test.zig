//! Hand-built `.boxy` LIR programs: dynamic locals, descriptor composition,
//! dynamic values, dictionaries, and list operations that take an element
//! descriptor (design.md "`.boxy` Runtime Lowering"). Each runs on the
//! interpreter, on the LLVM backend, and, where this host has a native code
//! generator, on the dev backend, all in this process; every backend must
//! produce the same result bytes. Each is built again at the wasm32 pointer
//! width and run on the wasm backend, whose module merges the wasm32 Boxy
//! runtime object.

const std = @import("std");
const base = @import("base");
const builtins = @import("builtins");
const boxy = @import("boxy_runtime");
const layout_mod = @import("layout");
const lir = @import("lir");
const backend = @import("backend");
const roc_target = @import("roc_target");
const Interpreter = @import("../interpreter.zig").Interpreter;
const RuntimeHostEnv = @import("../runtime_host.zig");
const InspectedRun = @import("../inspected_run.zig");
const object_image = @import("../object_image.zig");
const WasmRunner = @import("../wasm_runner.zig");
const wasm32_builtins = @import("wasm32_builtins");

const LIR = lir.LIR;
const LirStore = lir.LirStore;
const LocalId = LIR.LocalId;
const CFStmtId = LIR.CFStmtId;
const LirProcSpecId = LIR.LirProcSpecId;
const DynOp = LIR.DynOp;
const RocStr = builtins.str.RocStr;
const TypeDesc = boxy.TypeDesc;
const Name = boxy.Name;
const allocator = std.testing.allocator;
const StaticDataExport = backend.StaticDataExport;
const StaticDataRelocation = lir.Program.StaticDataRelocation;
const wasm_word_size: u32 = base.target.TargetUsize.u32.size();

fn testName(comptime text: []const u8, comptime id: u32) boxy.Name {
    return .{ .id = id, .len = text.len, .bytes = text.ptr };
}

var name_a = testName("a", 1);
var name_b = testName("b", 2);
var name_err = testName("Err", 3);
var name_ok = testName("Ok", 4);

/// An immortal object a static-data slot addresses.
const StaticObject = union(enum) {
    desc: *const TypeDesc,
    name: *const Name,
};

/// Builds procedures statement by statement, last statement first.
const Builder = struct {
    store: LirStore,
    layouts: layout_mod.Store,
    /// The static data values: pointers to immortal objects.
    statics: std.ArrayList(usize) = .empty,
    cells: std.ArrayList(*usize) = .empty,
    /// The object each static slot addresses, which the wasm program
    /// serializes at its own pointer width.
    objects: std.ArrayList(StaticObject) = .empty,
    proc_locals: std.ArrayList(LocalId) = .empty,
    dynamic: layout_mod.Idx,
    next_identity: u32 = 1,

    fn init(target_usize: base.target.TargetUsize) !Builder {
        var layouts = try layout_mod.Store.init(allocator, target_usize);
        errdefer layouts.deinit();
        const dynamic = try layouts.insertLayout(layout_mod.Layout.dynamic());
        var store = LirStore.init(allocator);
        store.runtime_contract = .boxy;
        return .{ .store = store, .layouts = layouts, .dynamic = dynamic };
    }

    fn deinit(self: *Builder) void {
        self.proc_locals.deinit(allocator);
        self.statics.deinit(allocator);
        for (self.cells.items) |cell| allocator.destroy(cell);
        self.cells.deinit(allocator);
        self.objects.deinit(allocator);
        self.layouts.deinit();
        self.store.deinit();
    }

    fn local(self: *Builder, layout_idx: layout_mod.Idx) !LocalId {
        const id = try self.store.addLocal(.{ .layout_idx = layout_idx });
        try self.proc_locals.append(allocator, id);
        return id;
    }

    fn dynLocal(self: *Builder, desc: LocalId, storage: LIR.DynStorage) !LocalId {
        const id = try self.store.addLocal(.{ .layout_idx = self.dynamic, .dyn = .{ .desc = desc, .storage = storage } });
        try self.proc_locals.append(allocator, id);
        return id;
    }

    fn proc(self: *Builder, args: []const LocalId, body: CFStmtId, ret_layout: layout_mod.Idx, options: struct {
        dyn_result: ?LocalId = null,
        join_points: []const LIR.JoinPoint = &.{},
        abi: LIR.ProcAbi = .roc,
        erased_capture_arg: ?LocalId = null,
        tail_group: ?LIR.TailGroupId = null,
    }) !LirProcSpecId {
        defer self.proc_locals.clearRetainingCapacity();
        self.next_identity += 1;
        return self.store.addProcSpec(.{
            .name = self.store.freshSyntheticSymbol(),
            .identity = LIR.ProcIdentity.forTest(self.next_identity),
            .args = try self.store.addLocalSpan(args),
            .body = body,
            .ret_layout = ret_layout,
            .frame_locals = try self.store.addLocalSpan(self.proc_locals.items),
            .join_points = try self.store.addJoinPointSpan(options.join_points),
            .dyn_result = options.dyn_result,
            .abi = options.abi,
            .erased_capture_arg = options.erased_capture_arg,
            .tail_group = options.tail_group,
        }, .none);
    }

    fn ret(self: *Builder, value: LocalId) !CFStmtId {
        return self.store.addCFStmt(.{ .ret = .{ .value = value } }, .test_fixture);
    }

    fn literal(self: *Builder, target: LocalId, value: LIR.LiteralValue, next: CFStmtId) !CFStmtId {
        return self.store.addCFStmt(.{ .assign_literal = .{ .target = target, .value = value, .next = next } }, .test_fixture);
    }

    fn int(self: *Builder, target: LocalId, value: i64, next: CFStmtId) !CFStmtId {
        return self.literal(target, .{ .i64_literal = .{ .value = value, .layout_idx = self.store.getLocal(target).layout_idx } }, next);
    }

    fn str(self: *Builder, target: LocalId, text: []const u8, next: CFStmtId) !CFStmtId {
        return self.literal(target, .{ .str_literal = try self.store.insertStringView(text, 0, @intCast(text.len)) }, next);
    }

    /// `target` = the address of an immortal object (a static descriptor or
    /// name), read as static data.
    fn static(self: *Builder, target: LocalId, object: StaticObject, next: CFStmtId) !CFStmtId {
        const id: LIR.StaticDataId = @fromBackingInt(@intCast(self.statics.items.len));
        const cell = try allocator.create(usize);
        cell.* = switch (object) {
            .desc => |desc| @intFromPtr(desc),
            .name => |name| @intFromPtr(name),
        };
        try self.cells.append(allocator, cell);
        try self.statics.append(allocator, @intFromPtr(cell));
        try self.objects.append(allocator, object);
        return self.literal(target, .{ .static_data = .{ .id = id } }, next);
    }

    /// The static data of the wasm program: each slot's word addresses its
    /// object, serialized in the descriptor format at the wasm32 pointer
    /// width (`roc__d{slot}_1`), and a name's text follows (`roc__d{slot}_2`).
    fn wasmStaticData(self: *Builder) ![]StaticDataExport {
        var exports: std.ArrayList(StaticDataExport) = .empty;
        errdefer {
            for (exports.items) |item| freeExport(item);
            exports.deinit(allocator);
        }
        for (self.objects.items, 0..) |object, slot| {
            const id: LIR.StaticDataId = @fromBackingInt(@intCast(slot));
            const slot_name = try lir.Program.staticDataSymbolName(allocator, id);
            errdefer allocator.free(slot_name);
            const object_name = try std.fmt.allocPrint(allocator, "roc__d{d}_1", .{slot});
            errdefer allocator.free(object_name);
            try exports.append(allocator, try wordExport(slot_name, object_name));
            switch (object) {
                .desc => |desc| try exports.append(allocator, try descExport(object_name, desc)),
                .name => |name| {
                    const text_name = try std.fmt.allocPrint(allocator, "roc__d{d}_2", .{slot});
                    errdefer allocator.free(text_name);
                    try exports.append(allocator, try nameExport(object_name, text_name, name));
                    try exports.append(allocator, .{
                        .symbol_name = text_name,
                        .bytes = try allocator.dupe(u8, name.text()),
                        .alignment = 1,
                        .is_exported = false,
                        .relocations = try allocator.alloc(StaticDataRelocation, 0),
                    });
                },
            }
        }
        return exports.toOwnedSlice(allocator);
    }

    /// A pointer-sized word holding the address of `target`.
    fn wordExport(symbol_name: []u8, target: []const u8) !StaticDataExport {
        const relocations = try allocator.alloc(StaticDataRelocation, 1);
        errdefer allocator.free(relocations);
        relocations[0] = .{ .offset = 0, .target_symbol_name = target };
        const bytes = try allocator.alloc(u8, wasm_word_size);
        @memset(bytes, 0);
        return .{
            .symbol_name = symbol_name,
            .bytes = bytes,
            .alignment = wasm_word_size,
            .is_exported = false,
            .relocations = relocations,
        };
    }

    /// A leaf descriptor (one without children) at the wasm32 width: a
    /// `Str` is three words there, every other leaf keeps its native size.
    fn descExport(symbol_name: []u8, desc: *const TypeDesc) !StaticDataExport {
        std.debug.assert(desc.child == null and desc.entries == null and desc.name == null and desc.methods == null);
        const desc_layout = TypeDesc.layout(.u32);
        const bytes = try allocator.alloc(u8, desc_layout.size);
        errdefer allocator.free(bytes);
        @memset(bytes, 0);
        const size: u32 = if (desc.kind == .str) RocStr.word_count * wasm_word_size else desc.size;
        const alignment: u32 = if (desc.kind == .str) wasm_word_size else desc.alignment;
        std.mem.writeInt(u32, bytes[desc_layout.of(.size)..][0..4], size, .little);
        std.mem.writeInt(u32, bytes[desc_layout.of(.alignment)..][0..4], alignment, .little);
        bytes[desc_layout.of(.kind)] = @backingInt(desc.kind);
        bytes[desc_layout.of(.scalar)] = @backingInt(desc.scalar);
        bytes[desc_layout.of(.sort_key)] = desc.sort_key;
        bytes[desc_layout.of(.flags)] = @bitCast(desc.flags);
        return .{
            .symbol_name = symbol_name,
            .bytes = bytes,
            .alignment = desc_layout.alignment,
            .is_exported = false,
            .relocations = try allocator.alloc(StaticDataRelocation, 0),
        };
    }

    fn nameExport(symbol_name: []u8, text_symbol: []const u8, name: *const Name) !StaticDataExport {
        const name_layout = boxy.desc.externLayout(Name, .u32);
        const bytes = try allocator.alloc(u8, name_layout.size);
        errdefer allocator.free(bytes);
        @memset(bytes, 0);
        std.mem.writeInt(u32, bytes[name_layout.of(.id)..][0..4], name.id, .little);
        std.mem.writeInt(u32, bytes[name_layout.of(.len)..][0..4], name.len, .little);
        const relocations = try allocator.alloc(StaticDataRelocation, 1);
        errdefer allocator.free(relocations);
        relocations[0] = .{ .offset = name_layout.of(.bytes), .target_symbol_name = text_symbol };
        return .{
            .symbol_name = symbol_name,
            .bytes = bytes,
            .alignment = name_layout.alignment,
            .is_exported = false,
            .relocations = relocations,
        };
    }

    fn freeExport(item: StaticDataExport) void {
        allocator.free(item.symbol_name);
        allocator.free(item.bytes);
        allocator.free(item.relocations);
    }

    fn freeStaticData(exports: []StaticDataExport) void {
        for (exports) |item| freeExport(item);
        allocator.free(exports);
    }

    fn dyn(self: *Builder, target: ?LocalId, op: DynOp, next: CFStmtId) !CFStmtId {
        return self.store.addDynStmt(target, op, next, .test_fixture);
    }

    fn call(self: *Builder, target: LocalId, proc_id: LirProcSpecId, args: []const LocalId, next: CFStmtId) !CFStmtId {
        return self.store.addCFStmt(.{ .assign_call = .{ .target = target, .proc = proc_id, .args = try self.store.addLocalSpan(args), .next = next } }, .test_fixture);
    }

    /// A call that replaces this procedure's frame with the callee's.
    fn tailCall(self: *Builder, target: LocalId, proc_id: LirProcSpecId, args: []const LocalId, next: CFStmtId) !CFStmtId {
        return self.store.addCFStmt(.{ .assign_call = .{
            .target = target,
            .proc = proc_id,
            .args = try self.store.addLocalSpan(args),
            .extra = try self.store.addStmtRow(LIR.CallExtra, .{ .replaces_frame = true }),
            .next = next,
        } }, .test_fixture);
    }

    fn lowLevel(self: *Builder, target: LocalId, op: base.LowLevel, args: []const LocalId, dyn_elem_desc: bool, next: CFStmtId) !CFStmtId {
        return self.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = op,
            .rc_effect = try self.store.addRcEffect(op.rcEffect()),
            .extra = if (dyn_elem_desc) try self.store.addStmtRow(LIR.LowLevelExtra, .{ .dyn_elem_desc = true }) else .none,
            .args = try self.store.addLocalSpan(args),
            .next = next,
        } }, .test_fixture);
    }

    fn setLocal(self: *Builder, target: LocalId, value: LocalId, mode: LIR.SetLocalWriteMode, next: CFStmtId) !CFStmtId {
        return self.store.addCFStmt(.{ .set_local = .{ .target = target, .value = value, .mode = mode, .next = next } }, .test_fixture);
    }

    fn decref(self: *Builder, value: LocalId, next: CFStmtId) !CFStmtId {
        return self.store.addCFStmt(.{ .decref = .{
            .value = value,
            .rc = .{ .concrete = .{ .op = .decref, .layout_idx = self.store.getLocal(value).layout_idx } },
            .next = next,
        } }, .test_fixture);
    }

    fn span(self: *Builder, locals: []const LocalId) !LIR.LocalSpan {
        return self.store.addLocalSpan(locals);
    }
};

const Run = struct {
    env: RuntimeHostEnv,
    static_strings: backend.StaticStringData.Table,
    interpreter: Interpreter,
    builder: *Builder,

    fn init(self: *Run, b: *Builder) !void {
        self.builder = b;
        self.env = RuntimeHostEnv.init(allocator);
        self.static_strings = try Interpreter.buildStaticStrings(allocator, &b.store);
        self.interpreter = try Interpreter.init(allocator, &b.store, &b.layouts, self.static_strings.view(), self.env.get_ops());
        self.interpreter.setStaticData(b.statics.items);
    }

    fn deinit(self: *Run) void {
        self.interpreter.deinit();
        self.static_strings.deinit();
        self.env.deinit();
    }

    /// The interpreter's result, after checking the LLVM and dev backends'
    /// match it.
    fn eval(self: *Run, proc_id: LirProcSpecId, ret_layout: layout_mod.Idx) !Interpreter.EvalResult {
        const result = try self.interpreter.eval(.{ .proc_id = proc_id, .ret_layout = ret_layout });
        try self.expectSame(result, ret_layout, try self.runLlvm(proc_id, ret_layout));
        if (comptime !backend.host_lir_codegen_available) return result;
        try self.expectSame(result, ret_layout, try self.runDev(proc_id, ret_layout));
        return result;
    }

    /// Check a backend's result bytes against the interpreter's, and release
    /// them.
    fn expectSame(self: *Run, result: Interpreter.EvalResult, ret_layout: layout_mod.Idx, bytes: []align(16) u8) !void {
        defer allocator.free(bytes);
        const size = self.builder.layouts.layoutSize(self.builder.layouts.getLayout(ret_layout));
        if (ret_layout == .str) {
            const backend_str: *const RocStr = @ptrCast(bytes.ptr);
            defer backend_str.decref(self.env.get_ops());
            try std.testing.expectEqualStrings(readStr(result.value), backend_str.asSlice());
        } else {
            try std.testing.expectEqualSlices(u8, result.value.ptr[0..size], bytes[0..size]);
        }
    }

    /// Zeroed storage for a result of `ret_layout`.
    fn resultBytes(self: *Run, ret_layout: layout_mod.Idx) ![]align(16) u8 {
        const size = self.builder.layouts.layoutSize(self.builder.layouts.getLayout(ret_layout));
        const bytes = try allocator.alignedAlloc(u8, .@"16", @max(size, 1));
        @memset(bytes, 0);
        return bytes;
    }

    /// Compile every procedure with the LLVM backend and run `proc_id` in
    /// this process, returning its result bytes.
    fn runLlvm(self: *Run, proc_id: LirProcSpecId, ret_layout: layout_mod.Idx) ![]align(16) u8 {
        const llvm_compile = @import("llvm_compile");
        const b = self.builder;
        const entrypoints = [_]llvm_compile.MonoLlvmCodeGen.Entrypoint{.{
            .symbol_name = "roc_eval_main",
            .proc = proc_id,
            .arg_layouts = &.{},
            .ret_layout = ret_layout,
        }};
        var bitcode = generate: {
            var codegen = llvm_compile.MonoLlvmCodeGen.init(allocator, &b.store);
            defer codegen.deinit();
            codegen.layout_store = &b.layouts;
            codegen.static_data_addresses = b.statics.items;
            break :generate try codegen.generateEntrypointModule("roc_boxy_dyn_test", &entrypoints);
        };
        defer bitcode.deinit();
        var compile_options = try InspectedRun.llvmCompileOptions(allocator, b.layouts.targetUsize(), llvm_compile.bindings.IrOptimizationLevel.O3);
        defer compile_options.deinit(allocator);
        const object_bytes = try llvm_compile.compileBitcodeModulesToObject(allocator, std.Options.debug_io, &.{bitcode.bitcode}, compile_options.options);
        defer allocator.free(object_bytes);
        var image = try object_image.load(allocator, object_bytes);
        defer image.deinit();
        const EntryFn = *const fn ([*]u8, ?*anyopaque) callconv(.c) void;
        const entry = image.lookup(EntryFn, "roc_eval_main") orelse return error.MissingEntrypoint;
        const out = try self.resultBytes(ret_layout);
        errdefer allocator.free(out);
        const entered = builtins.in_process_host.enter(self.env.get_ops(), null);
        defer builtins.in_process_host.leave(entered);
        entry(out.ptr, null);
        return out;
    }

    /// Compile every procedure with the dev backend and run `proc_id` in this
    /// process, returning its result bytes.
    fn runDev(self: *Run, proc_id: LirProcSpecId, ret_layout: layout_mod.Idx) ![]align(16) u8 {
        const b = self.builder;
        var static_strings = try backend.StaticStringData.build(allocator, &b.store, backend.dev.LirCodeGenMod.host_lir_codegen_target);
        defer static_strings.deinit();
        var codegen = try backend.HostLirCodeGen.initWithCapturePrefix(allocator, &b.store, &b.layouts, static_strings.view(), 0, roc_target.host_cpu.level());
        defer codegen.deinit();
        codegen.setNativeStaticData(b.statics.items);
        try codegen.compileAllProcSpecs(b.store.getProcSpecs());
        const entrypoint = try codegen.generateEntrypointWrapper("roc_eval_main", proc_id, &.{}, ret_layout);
        try codegen.finishImage();
        var executable = try backend.ExecutableMemory.initWithEntryOffsetAndUnwindInfo(
            codegen.getGeneratedCode(),
            entrypoint.offset,
            codegen.getUnwindFunctions(),
        );
        defer executable.deinit();
        const out = try self.resultBytes(ret_layout);
        errdefer allocator.free(out);
        const entered = builtins.in_process_host.enter(self.env.get_ops(), null);
        defer builtins.in_process_host.leave(entered);
        executable.callRocABI(@ptrCast(out.ptr), null);
        return out;
    }
};

fn readStr(value: anytype) []const u8 {
    const rs: *const RocStr = @ptrCast(@alignCast(value.ptr));
    return rs.asSlice();
}

/// Compile every procedure of the wasm32-width program `b` with the wasm
/// backend, run `proc_id` on bytebox, and return its result: a string's
/// text, or a scalar's little-endian bytes. The caller owns the bytes.
fn runWasm(b: *Builder, proc_id: LirProcSpecId, ret_layout: layout_mod.Idx) ![]u8 {
    std.debug.assert(b.layouts.targetUsize() == .u32);
    var codegen = backend.wasm.WasmCodeGen.init(allocator, &b.store, &b.layouts, .default);
    defer codegen.deinit();
    const exports = try b.wasmStaticData();
    defer Builder.freeStaticData(exports);
    try codegen.module.addStaticDataExports(exports);
    const generated = try codegen.generateModule(proc_id, ret_layout, wasm32_builtins.bytes, wasm32_builtins.boxy_runtime_bytes);
    defer allocator.free(generated.wasm_bytes);
    const shape: WasmRunner.RawResult = switch (ret_layout) {
        .str => .str,
        .i64, .u64 => .i64,
        .u32, .bool => .i32,
        else => unreachable, // every test procedure returns one of these
    };
    return WasmRunner.runWasmRaw(allocator, generated.wasm_bytes, generated.heap_base, generated.has_imports, shape);
}

fn expectWasmInt(b: *Builder, proc_id: LirProcSpecId, ret_layout: layout_mod.Idx, expected: i64) !void {
    const bytes = try runWasm(b, proc_id, ret_layout);
    defer allocator.free(bytes);
    const actual: i64 = switch (bytes.len) {
        4 => @as(i32, @bitCast(std.mem.readInt(u32, bytes[0..4], .little))),
        8 => @bitCast(std.mem.readInt(u64, bytes[0..8], .little)),
        else => unreachable, // `runWasm` returns a four- or eight-byte scalar
    };
    try std.testing.expectEqual(expected, actual);
}

fn expectWasmStr(b: *Builder, proc_id: LirProcSpecId, expected: []const u8) !void {
    const text = try runWasm(b, proc_id, .str);
    defer allocator.free(text);
    try std.testing.expectEqualStrings(expected, text);
}

/// `id : a -> a`, lowered once: its parameter is the caller's storage and its
/// result is written to the storage the caller passes.
fn buildIdentity(b: *Builder) !LirProcSpecId {
    const desc = try b.local(.opaque_ptr);
    const x = try b.dynLocal(desc, .param);
    const result = try b.dynLocal(desc, .result);
    return b.proc(&.{ x, desc }, try b.ret(x), b.dynamic, .{ .dyn_result = result });
}

const IdentityProgram = struct {
    int_proc: LirProcSpecId,
    str_proc: LirProcSpecId,

    fn build(b: *Builder) !IdentityProgram {
        const id = try buildIdentity(b);

        const int_desc = try b.local(.opaque_ptr);
        const int_arg = try b.local(.i64);
        const int_out = try b.local(.i64);
        const int_body = try b.static(int_desc, .{ .desc = boxy.compose.scalarDesc(.i64) }, try b.int(int_arg, -42, try b.call(int_out, id, &.{ int_arg, int_desc }, try b.ret(int_out))));
        const int_proc = try b.proc(&.{}, int_body, .i64, .{});

        const str_desc = try b.local(.opaque_ptr);
        const left = try b.local(.str);
        const right = try b.local(.str);
        const joined = try b.local(.str);
        const str_out = try b.local(.str);
        const str_body = try b.static(str_desc, .{ .desc = boxy.compose.strDesc() }, try b.str(left, "a string long enough to live on the heap, ", try b.str(right, "joined at runtime", try b.lowLevel(joined, .str_concat, &.{ left, right }, false, try b.call(str_out, id, &.{ joined, str_desc }, try b.ret(str_out))))));
        const str_proc = try b.proc(&.{}, str_body, .str, .{});
        return .{ .int_proc = int_proc, .str_proc = str_proc };
    }
};

test "boxy interpreter: a generic identity over I64 and Str" {
    var b = try Builder.init(.native);
    defer b.deinit();
    const program = try IdentityProgram.build(&b);

    var run: Run = undefined;
    try run.init(&b);
    defer run.deinit();

    const int_result = try run.eval(program.int_proc, .i64);
    try std.testing.expectEqual(@as(i64, -42), int_result.value.read(i64));

    const str_result = try run.eval(program.str_proc, .str);
    try std.testing.expectEqualStrings("a string long enough to live on the heap, joined at runtime", readStr(str_result.value));
    run.interpreter.dropValue(str_result.value, .str);
    try run.env.checkForLeaks();
}

test "boxy wasm: a generic identity over I64 and Str" {
    var b = try Builder.init(.u32);
    defer b.deinit();
    const program = try IdentityProgram.build(&b);
    try expectWasmInt(&b, program.int_proc, .i64, -42);
    try expectWasmStr(&b, program.str_proc, "a string long enough to live on the heap, joined at runtime");
}

/// Composes `{ a : I64, b : Str }` and builds one in a dynamic frame local.
const RecordFixture = struct {
    record_desc: LocalId,
    record: LocalId,

    /// Statements composing the descriptor and building the record, then
    /// `next`.
    fn build(b: *Builder, next_after: fn (*Builder, RecordFixture) anyerror!CFStmtId) !CFStmtId {
        const int_desc = try b.local(.opaque_ptr);
        const str_desc = try b.local(.opaque_ptr);
        const a = try b.local(.opaque_ptr);
        const n_b = try b.local(.opaque_ptr);
        const record_desc = try b.local(.opaque_ptr);
        const record = try b.dynLocal(record_desc, .frame);
        const x = try b.local(.i64);
        const s = try b.local(.str);
        const fixture: RecordFixture = .{ .record_desc = record_desc, .record = record };
        const rest = try next_after(b, fixture);
        return b.static(int_desc, .{ .desc = boxy.compose.scalarDesc(.i64) }, try b.static(str_desc, .{ .desc = boxy.compose.strDesc() }, try b.static(a, .{ .name = &name_a }, try b.static(n_b, .{ .name = &name_b }, try b.dyn(record_desc, .{
            .desc_record = .{
                // Names in any order; the runtime orders by name.
                .names = try b.span(&.{ n_b, a }),
                .descs = try b.span(&.{ str_desc, int_desc }),
            },
        }, try b.dyn(null, .frame_alloc, try b.int(x, 7, try b.str(s, "hi", try b.dyn(record, .{ .record = .{
            .desc = record_desc,
            .fields = try b.span(&.{ x, s }),
        } }, rest)))))))));
    }
};

const RecordReadProgram = struct {
    int_proc: LirProcSpecId,
    str_proc: LirProcSpecId,

    fn build(b: *Builder) !RecordReadProgram {
        const ReadInt = struct {
            fn next(builder: *Builder, f: RecordFixture) anyerror!CFStmtId {
                const a = try builder.local(.opaque_ptr);
                const out = try builder.local(.i64);
                return builder.static(a, .{ .name = &name_a }, try builder.dyn(out, .{ .field_addr_named = .{ .base = f.record, .desc = f.record_desc, .name = a } }, try builder.ret(out)));
            }
        };
        const int_proc = try b.proc(&.{}, try RecordFixture.build(b, ReadInt.next), .i64, .{});

        const ReadStr = struct {
            fn next(builder: *Builder, f: RecordFixture) anyerror!CFStmtId {
                const field_desc = try builder.local(.opaque_ptr);
                const view = try builder.dynLocal(field_desc, .view);
                const out = try builder.local(.str);
                return builder.static(field_desc, .{ .desc = boxy.compose.strDesc() }, try builder.dyn(view, .{ .field_addr = .{ .base = f.record, .desc = f.record_desc, .semantic_index = 1 } }, try builder.dyn(out, .{ .copy = .{ .src = view, .desc = field_desc } }, try builder.ret(out))));
            }
        };
        const str_proc = try b.proc(&.{}, try RecordFixture.build(b, ReadStr.next), .str, .{});
        return .{ .int_proc = int_proc, .str_proc = str_proc };
    }
};

test "boxy interpreter: a dynamic record is built and its fields read back" {
    var b = try Builder.init(.native);
    defer b.deinit();
    const program = try RecordReadProgram.build(&b);

    var run: Run = undefined;
    try run.init(&b);
    defer run.deinit();
    try std.testing.expectEqual(@as(i64, 7), (try run.eval(program.int_proc, .i64)).value.read(i64));
    try std.testing.expectEqualStrings("hi", readStr((try run.eval(program.str_proc, .str)).value));
    try run.env.checkForLeaks();
}

test "boxy wasm: a dynamic record is built and its fields read back" {
    var b = try Builder.init(.u32);
    defer b.deinit();
    const program = try RecordReadProgram.build(&b);
    try expectWasmInt(&b, program.int_proc, .i64, 7);
    try expectWasmStr(&b, program.str_proc, "hi");
}

const RecordValueProgram = struct {
    eq_proc: LirProcSpecId,
    hash_proc: LirProcSpecId,
    inspect_proc: LirProcSpecId,

    fn build(b: *Builder) !RecordValueProgram {
        const Eq = struct {
            fn next(builder: *Builder, f: RecordFixture) anyerror!CFStmtId {
                const out = try builder.local(.bool);
                return builder.dyn(out, .{ .eq = .{ .lhs = f.record, .rhs = f.record, .desc = f.record_desc } }, try builder.ret(out));
            }
        };
        const eq_proc = try b.proc(&.{}, try RecordFixture.build(b, Eq.next), .bool, .{});

        const Hash = struct {
            fn next(builder: *Builder, f: RecordFixture) anyerror!CFStmtId {
                const seed = try builder.local(.u64);
                const out = try builder.local(.u64);
                return builder.int(seed, 99, try builder.dyn(out, .{ .hash = .{ .hasher = seed, .value = f.record, .desc = f.record_desc } }, try builder.ret(out)));
            }
        };
        const hash_proc = try b.proc(&.{}, try RecordFixture.build(b, Hash.next), .u64, .{});

        const Inspect = struct {
            fn next(builder: *Builder, f: RecordFixture) anyerror!CFStmtId {
                const out = try builder.local(.str);
                return builder.dyn(out, .{ .inspect = .{ .value = f.record, .desc = f.record_desc } }, try builder.ret(out));
            }
        };
        const inspect_proc = try b.proc(&.{}, try RecordFixture.build(b, Inspect.next), .str, .{});
        return .{ .eq_proc = eq_proc, .hash_proc = hash_proc, .inspect_proc = inspect_proc };
    }
};

test "boxy interpreter: eq, hash, and inspect of a dynamic record" {
    var b = try Builder.init(.native);
    defer b.deinit();
    const program = try RecordValueProgram.build(&b);

    var run: Run = undefined;
    try run.init(&b);
    defer run.deinit();
    try std.testing.expectEqual(@as(u8, 1), (try run.eval(program.eq_proc, .bool)).value.read(u8));

    const hashed = (try run.eval(program.hash_proc, .u64)).value.read(u64);
    try std.testing.expect(hashed != 99);
    try std.testing.expectEqual(hashed, (try run.eval(program.hash_proc, .u64)).value.read(u64));

    const rendered = try run.eval(program.inspect_proc, .str);
    try std.testing.expectEqualStrings("{ a: 7, b: \"hi\" }", readStr(rendered.value));
    run.interpreter.dropValue(rendered.value, .str);
    try run.env.checkForLeaks();
}

test "boxy wasm: eq, hash, and inspect of a dynamic record" {
    var b = try Builder.init(.u32);
    defer b.deinit();
    const program = try RecordValueProgram.build(&b);
    try expectWasmInt(&b, program.eq_proc, .bool, 1);
    const hashed = try runWasm(&b, program.hash_proc, .u64);
    defer allocator.free(hashed);
    try std.testing.expect(std.mem.readInt(u64, hashed[0..8], .little) != 99);
    try expectWasmInt(&b, program.hash_proc, .u64, @bitCast(std.mem.readInt(u64, hashed[0..8], .little)));
    try expectWasmStr(&b, program.inspect_proc, "{ a: 7, b: \"hi\" }");
}

/// Procedures reading a tag union's discriminant, a tag's discriminant
/// under its descriptor, and a payload, in that order.
fn buildTagUnionProgram(b: *Builder) ![3]LirProcSpecId {
    const Proc = enum { discriminant, discriminant_of, payload };
    var procs: [3]LirProcSpecId = undefined;
    inline for (.{ Proc.discriminant, Proc.discriminant_of, Proc.payload }, 0..) |which, index| {
        const int_desc = try b.local(.opaque_ptr);
        const str_desc = try b.local(.opaque_ptr);
        const err = try b.local(.opaque_ptr);
        const ok = try b.local(.opaque_ptr);
        const union_desc = try b.local(.opaque_ptr);
        const value = try b.dynLocal(union_desc, .frame);
        const x = try b.local(.i64);
        const tail: CFStmtId = switch (which) {
            .discriminant => blk: {
                const out = try b.local(.u32);
                break :blk try b.dyn(out, .{ .tag_discriminant = .{ .base = value, .desc = union_desc } }, try b.ret(out));
            },
            .discriminant_of => blk: {
                const out = try b.local(.u32);
                break :blk try b.dyn(out, .{ .tag_discriminant_of = .{ .desc = union_desc, .name = ok } }, try b.ret(out));
            },
            .payload => blk: {
                const out = try b.local(.i64);
                break :blk try b.dyn(out, .{ .tag_payload_addr = .{ .base = value, .desc = union_desc, .name = ok, .payload_index = 0 } }, try b.ret(out));
            },
        };
        const body = try b.static(int_desc, .{ .desc = boxy.compose.scalarDesc(.i64) }, try b.static(str_desc, .{ .desc = boxy.compose.strDesc() }, try b.static(err, .{ .name = &name_err }, try b.static(ok, .{ .name = &name_ok }, try b.dyn(union_desc, .{ .desc_tag_union = .{
            .names = try b.span(&.{ ok, err }),
            .payload_counts = try b.store.addU32Span(&.{ 1, 1 }),
            .payloads = try b.span(&.{ int_desc, str_desc }),
        } }, try b.dyn(null, .frame_alloc, try b.int(x, 1234, try b.dyn(value, .{ .tag = .{ .desc = union_desc, .name = ok, .payloads = try b.span(&.{x}) } }, tail))))))));
        procs[index] = try b.proc(&.{}, body, if (which == .payload) .i64 else .u32, .{});
    }
    return procs;
}

test "boxy interpreter: a tag union's discriminant and payload" {
    var b = try Builder.init(.native);
    defer b.deinit();
    const procs = try buildTagUnionProgram(&b);

    var run: Run = undefined;
    try run.init(&b);
    defer run.deinit();
    // Tags are ordered by name: Err = 0, Ok = 1.
    try std.testing.expectEqual(@as(u32, 1), (try run.eval(procs[0], .u32)).value.read(u32));
    try std.testing.expectEqual(@as(u32, 1), (try run.eval(procs[1], .u32)).value.read(u32));
    try std.testing.expectEqual(@as(i64, 1234), (try run.eval(procs[2], .i64)).value.read(i64));
    try run.env.checkForLeaks();
}

test "boxy wasm: a tag union's discriminant and payload" {
    var b = try Builder.init(.u32);
    defer b.deinit();
    const procs = try buildTagUnionProgram(&b);
    try expectWasmInt(&b, procs[0], .u32, 1);
    try expectWasmInt(&b, procs[1], .u32, 1);
    try expectWasmInt(&b, procs[2], .i64, 1234);
}

/// acc = 5; i = 3; loop { if i == 0 then return acc else i -= 1; acc = acc }
fn buildJoinProgram(b: *Builder) !LirProcSpecId {
    const desc = try b.local(.opaque_ptr);
    const start = try b.local(.i64);
    const acc = try b.dynLocal(desc, .frame);
    const i = try b.local(.i64);
    const i_init = try b.local(.i64);
    const one = try b.local(.i64);
    const next_i = try b.local(.i64);
    const out = try b.local(.i64);
    const join_id: LIR.JoinPointId = @fromBackingInt(0);

    const jump = try b.store.addCFStmt(.{ .jump = .{ .target = join_id } }, .test_fixture);
    const step = try b.int(one, 1, try b.lowLevel(next_i, .num_int_sub_wrap, &.{ i, one }, false, try b.setLocal(i, next_i, .replace_existing, try b.setLocal(acc, acc, .replace_existing, jump))));
    const done = try b.dyn(out, .{ .copy = .{ .src = acc, .desc = desc } }, try b.ret(out));
    const loop_body = try b.store.addCFStmt(.{ .switch_stmt = .{
        .cond = i,
        .branches = try b.store.addCFSwitchBranches(&.{.{ .value = 0, .body = done }}),
        .default_branch = step,
    } }, .test_fixture);
    const params = try b.span(&.{ acc, i });
    const enter = try b.setLocal(acc, start, .initialize_join_param, try b.setLocal(i, i_init, .initialize_join_param, jump));
    const join = try b.store.addCFStmt(.{ .join = .{ .id = join_id, .params = params, .body = loop_body, .remainder = enter } }, .test_fixture);
    const body = try b.static(desc, .{ .desc = boxy.compose.scalarDesc(.i64) }, try b.dyn(null, .frame_alloc, try b.int(start, 5, try b.int(i_init, 3, join))));
    return b.proc(&.{}, body, .i64, .{ .join_points = &.{.{ .id = join_id, .params = params, .body = loop_body }} });
}

test "boxy interpreter: a dynamic join parameter carried around a loop" {
    var b = try Builder.init(.native);
    defer b.deinit();
    const proc_id = try buildJoinProgram(&b);

    var run: Run = undefined;
    try run.init(&b);
    defer run.deinit();
    try std.testing.expectEqual(@as(i64, 5), (try run.eval(proc_id, .i64)).value.read(i64));
    try run.env.checkForLeaks();
}

test "boxy wasm: a dynamic join parameter carried around a loop" {
    var b = try Builder.init(.u32);
    defer b.deinit();
    try expectWasmInt(&b, try buildJoinProgram(&b), .i64, 5);
}

/// Procedures taking a two-string list's length and its second element.
fn buildListProgram(b: *Builder) ![2]LirProcSpecId {
    const ListProc = enum { len, get };
    var procs: [2]LirProcSpecId = undefined;
    inline for (.{ ListProc.len, ListProc.get }, 0..) |which, index| {
        const elem_desc = try b.local(.opaque_ptr);
        const list_desc = try b.local(.opaque_ptr);
        const list = try b.dynLocal(list_desc, .frame);
        const x = try b.dynLocal(elem_desc, .frame);
        const y = try b.dynLocal(elem_desc, .frame);
        const s1 = try b.local(.str);
        const s2 = try b.local(.str);
        const tail: CFStmtId = switch (which) {
            .len => blk: {
                const out = try b.local(.u64);
                break :blk try b.lowLevel(out, .list_len, &.{ list, elem_desc }, true, try b.decref(list, try b.ret(out)));
            },
            .get => blk: {
                const index_local = try b.local(.u64);
                const elem = try b.dynLocal(elem_desc, .frame);
                const out = try b.local(.str);
                break :blk try b.int(index_local, 1, try b.lowLevel(elem, .list_get_unsafe, &.{ list, index_local, elem_desc }, true, try b.dyn(out, .{ .copy = .{ .src = elem, .desc = elem_desc } }, try b.decref(list, try b.ret(out)))));
            },
        };
        const body = try b.static(elem_desc, .{ .desc = boxy.compose.strDesc() }, try b.dyn(list_desc, .{ .desc_list = .{ .elem = elem_desc } }, try b.dyn(null, .frame_alloc, try b.str(s1, "first", try b.str(s2, "second", try b.setLocal(x, s1, .replace_existing, try b.setLocal(y, s2, .replace_existing, try b.dyn(list, .{ .list = .{ .elem_desc = elem_desc, .elems = try b.span(&.{ x, y }) } }, tail))))))));
        procs[index] = try b.proc(&.{}, body, if (which == .len) .u64 else .str, .{});
    }
    return procs;
}

test "boxy interpreter: list operations through an element descriptor" {
    var b = try Builder.init(.native);
    defer b.deinit();
    const procs = try buildListProgram(&b);

    var run: Run = undefined;
    try run.init(&b);
    defer run.deinit();
    try std.testing.expectEqual(@as(u64, 2), (try run.eval(procs[0], .u64)).value.read(u64));
    try std.testing.expectEqualStrings("second", readStr((try run.eval(procs[1], .str)).value));
    try run.env.checkForLeaks();
}

test "boxy wasm: list operations through an element descriptor" {
    var b = try Builder.init(.u32);
    defer b.deinit();
    const procs = try buildListProgram(&b);
    try expectWasmInt(&b, procs[0], .u64, 2);
    try expectWasmStr(&b, procs[1], "second");
}

/// A caller of `\(x) -> x`, a worker over a type variable whose descriptor
/// is its one capture, through a composed dictionary record.
fn buildDictionaryProgram(b: *Builder) !LirProcSpecId {
    const capture = try b.local(.opaque_ptr);
    const elem_desc = try b.local(.opaque_ptr);
    const args_desc = try b.local(.opaque_ptr);
    const args = try b.dynLocal(args_desc, .param);
    const x = try b.dynLocal(elem_desc, .view);
    const result = try b.dynLocal(elem_desc, .result);
    const worker_body = try b.lowLevel(elem_desc, .erased_capture_load, &.{capture}, false, try b.dyn(args_desc, .{ .desc_args = .{ .elems = try b.span(&.{elem_desc}) } }, try b.dyn(x, .{ .field_addr = .{ .base = args, .desc = args_desc, .semantic_index = 0 } }, try b.ret(x))));
    const worker = try b.proc(&.{ args, capture }, worker_body, b.dynamic, .{ .dyn_result = result, .abi = .erased_record, .erased_capture_arg = capture });

    const record_layout = try b.layouts.putTuple(&.{b.layouts.getLayout(.i64)});
    const int_desc = try b.local(.opaque_ptr);
    const dict = try b.local(.opaque_ptr);
    const dict_again = try b.local(.opaque_ptr);
    const methods = try b.local(.opaque_ptr);
    const closure = try b.local(try b.layouts.insertErasedCallable());
    const value = try b.local(.i64);
    const record = try b.local(record_layout);
    const out = try b.local(.i64);
    const body = try b.static(int_desc, .{ .desc = boxy.compose.scalarDesc(.i64) }, try b.dyn(dict, .{ .dict_compose = .{ .worker = worker, .captures = try b.span(&.{int_desc}) } }, try b.dyn(dict_again, .{ .dict_compose = .{ .worker = worker, .captures = try b.span(&.{int_desc}) } }, try b.dyn(methods, .{ .dict_record = .{ .methods = try b.span(&.{ dict, dict_again }) } }, try b.dyn(closure, .{ .dict_method = .{ .dict = methods, .index = 1 } }, try b.int(value, 31337, try b.store.addCFStmt(.{ .assign_struct = .{ .target = record, .fields = try b.span(&.{value}), .next = try b.dyn(out, .{ .call_erased = .{ .closure = closure, .args = record } }, try b.ret(out)) } }, .test_fixture)))))));
    return b.proc(&.{}, body, .i64, .{});
}

test "boxy interpreter: a composed dictionary called through the erased record ABI" {
    var b = try Builder.init(.native);
    defer b.deinit();
    const caller = try buildDictionaryProgram(&b);

    var run: Run = undefined;
    try run.init(&b);
    defer run.deinit();
    try std.testing.expectEqual(@as(i64, 31337), (try run.eval(caller, .i64)).value.read(i64));
    try std.testing.expectEqual(@as(i64, 31337), (try run.eval(caller, .i64)).value.read(i64));
    try run.env.checkForLeaks();
}

test "boxy wasm: a composed dictionary called through the erased record ABI" {
    var b = try Builder.init(.u32);
    defer b.deinit();
    try expectWasmInt(&b, try buildDictionaryProgram(&b), .i64, 31337);
}

const TailProgram = struct {
    int_proc: LirProcSpecId,
    str_proc: LirProcSpecId,

    fn build(b: *Builder) !TailProgram {
        const group: LIR.TailGroupId = @fromBackingInt(@intCast(0));

        // `leaf(x) = x`, entered by a frame-replacing call.
        const leaf_desc = try b.local(.opaque_ptr);
        const leaf_x = try b.dynLocal(leaf_desc, .param);
        const leaf_result = try b.dynLocal(leaf_desc, .result);
        const leaf = try b.proc(&.{ leaf_x, leaf_desc }, try b.ret(leaf_x), b.dynamic, .{ .dyn_result = leaf_result, .tail_group = group });

        // `hop(x) = leaf(copy of x)`: the copy lives in `hop`'s own frame, which
        // the call to `leaf` replaces.
        const desc = try b.local(.opaque_ptr);
        const x = try b.dynLocal(desc, .param);
        const result = try b.dynLocal(desc, .result);
        const copy = try b.dynLocal(desc, .frame);
        const out = try b.dynLocal(desc, .frame);
        const hop_body = try b.dyn(null, .frame_alloc, try b.dyn(copy, .{ .copy = .{ .src = x, .desc = desc } }, try b.tailCall(out, leaf, &.{ copy, desc }, try b.ret(out))));
        const hop = try b.proc(&.{ x, desc }, hop_body, b.dynamic, .{ .dyn_result = result, .tail_group = group });

        const str_desc = try b.local(.opaque_ptr);
        const left = try b.local(.str);
        const right = try b.local(.str);
        const joined = try b.local(.str);
        const str_out = try b.local(.str);
        const str_body = try b.static(str_desc, .{ .desc = boxy.compose.strDesc() }, try b.str(left, "a string long enough to live on the heap, ", try b.str(right, "moved by a tail call", try b.lowLevel(joined, .str_concat, &.{ left, right }, false, try b.call(str_out, hop, &.{ joined, str_desc }, try b.ret(str_out))))));
        const str_proc = try b.proc(&.{}, str_body, .str, .{});

        const int_desc = try b.local(.opaque_ptr);
        const int_arg = try b.local(.i64);
        const int_out = try b.local(.i64);
        const int_body = try b.static(int_desc, .{ .desc = boxy.compose.scalarDesc(.i64) }, try b.int(int_arg, 77, try b.call(int_out, hop, &.{ int_arg, int_desc }, try b.ret(int_out))));
        const int_proc = try b.proc(&.{}, int_body, .i64, .{});
        return .{ .int_proc = int_proc, .str_proc = str_proc };
    }
};

test "boxy interpreter: a frame-replacing call moves a dynamic argument out of the replaced frame" {
    var b = try Builder.init(.native);
    defer b.deinit();
    const program = try TailProgram.build(&b);

    var run: Run = undefined;
    try run.init(&b);
    defer run.deinit();
    try std.testing.expectEqual(@as(i64, 77), (try run.eval(program.int_proc, .i64)).value.read(i64));
    const str_result = try run.eval(program.str_proc, .str);
    try std.testing.expectEqualStrings("a string long enough to live on the heap, moved by a tail call", readStr(str_result.value));
    run.interpreter.dropValue(str_result.value, .str);
    try run.env.checkForLeaks();
}

test "boxy wasm: a frame-replacing call moves a dynamic argument out of the replaced frame" {
    var b = try Builder.init(.u32);
    defer b.deinit();
    const program = try TailProgram.build(&b);
    try expectWasmInt(&b, program.int_proc, .i64, 77);
    try expectWasmStr(&b, program.str_proc, "a string long enough to live on the heap, moved by a tail call");
}
