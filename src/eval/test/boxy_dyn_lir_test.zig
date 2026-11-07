//! Hand-built `.boxy` LIR programs: dynamic locals, type table reads,
//! dynamic values, dictionaries, and list operations that take an item
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

/// The errors these tests' helpers return.
const TestError = std.mem.Allocator.Error || Interpreter.Error || @import("llvm_compile").Error || object_image.LoadError || error{
    TestExpectedEqual,
    TestUnexpectedResult,
    MissingSymbol,
    CompilationFailed,
    UnsupportedLowLevel,
    EmptyCode,
    MmapFailed,
    MprotectFailed,
    UnsupportedPlatform,
    UnwindRegistrationFailed,
    VirtualAllocFailed,
    VirtualProtectFailed,
    HostedFunctionTypeMismatch,
    UnsupportedTarget,
    WasmExecFailed,
    MissingEntrypoint,
};
const wasm_word_size: u32 = base.target.TargetUsize.u32.size();

fn testName(comptime text: []const u8) boxy.Name {
    @setEvalBranchQuota(200_000);
    return .{ .id = boxy.labelId(text), .len = text.len, .bytes = text.ptr };
}

var name_a = testName("a");
var name_b = testName("b");
var name_err = testName("Err");
var name_ok = testName("Ok");

/// An immortal object a static-data slot addresses.
const StaticObject = union(enum) {
    desc: *const TypeDesc,
    name: *const Name,
    /// A type table whose words address these descriptors.
    table: []const *const TypeDesc,
};

/// Builds procedures statement by statement, last statement first.
const Builder = struct {
    store: LirStore,
    layouts: layout_mod.Store,
    /// The static data values: pointers to immortal objects.
    statics: std.ArrayList(usize) = .empty,
    cells: std.ArrayList(*usize) = .empty,
    /// The words of each native type table, and the descriptors each
    /// table's words address.
    tables: std.ArrayList([]usize) = .empty,
    table_descs: std.ArrayList([]*const TypeDesc) = .empty,
    /// The object each static slot addresses, which the wasm program
    /// serializes at its own pointer width.
    objects: std.ArrayList(StaticObject) = .empty,
    proc_locals: std.ArrayList(LocalId) = .empty,
    /// The program's descriptors, laid out at its pointer width.
    descs: boxy.test_descs.Builder,
    dynamic: layout_mod.Idx,
    next_identity: u32 = 1,
    next_join_point: u32 = 0,
    next_tail_group: u32 = 0,

    fn init(target_usize: base.target.TargetUsize) TestError!Builder {
        var layouts = try layout_mod.Store.init(allocator, target_usize);
        errdefer layouts.deinit();
        const dynamic = try layouts.insertLayout(layout_mod.Layout.dynamic());
        var store = LirStore.init(allocator);
        store.runtime_contract = .boxy;
        const width: boxy.desc.PointerWidth = switch (target_usize) {
            .u32 => .u32,
            .u64 => .u64,
        };
        return .{ .store = store, .layouts = layouts, .dynamic = dynamic, .descs = .init(allocator, width) };
    }

    fn deinit(self: *Builder) void {
        self.proc_locals.deinit(allocator);
        self.statics.deinit(allocator);
        for (self.cells.items) |cell| allocator.destroy(cell);
        self.cells.deinit(allocator);
        for (self.tables.items) |words| allocator.free(words);
        self.tables.deinit(allocator);
        for (self.table_descs.items) |descs| allocator.free(descs);
        self.table_descs.deinit(allocator);
        self.objects.deinit(allocator);
        self.descs.deinit();
        self.layouts.deinit();
        self.store.deinit();
    }

    fn local(self: *Builder, layout_idx: layout_mod.Idx) TestError!LocalId {
        const id = try self.store.addLocal(.{ .layout_idx = layout_idx });
        try self.proc_locals.append(allocator, id);
        return id;
    }

    fn dynLocal(self: *Builder, desc: LocalId, storage: LIR.DynStorage) TestError!LocalId {
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
        uses_pending_cell: bool = false,
    }) TestError!LirProcSpecId {
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
            .uses_pending_cell = options.uses_pending_cell,
        }, .none);
    }

    fn freshJoinPointId(self: *Builder) LIR.JoinPointId {
        const id: LIR.JoinPointId = @fromBackingInt(self.next_join_point);
        self.next_join_point += 1;
        return id;
    }

    fn freshTailGroupId(self: *Builder) LIR.TailGroupId {
        const id: LIR.TailGroupId = @fromBackingInt(self.next_tail_group);
        self.next_tail_group += 1;
        return id;
    }

    fn ret(self: *Builder, value: LocalId) TestError!CFStmtId {
        return self.store.addCFStmt(.{ .ret = .{ .value = value } }, .test_fixture);
    }

    fn literal(self: *Builder, target: LocalId, value: LIR.LiteralValue, next: CFStmtId) TestError!CFStmtId {
        return self.store.addCFStmt(.{ .assign_literal = .{ .target = target, .value = value, .next = next } }, .test_fixture);
    }

    fn int(self: *Builder, target: LocalId, value: i64, next: CFStmtId) TestError!CFStmtId {
        return self.literal(target, .{ .i64_literal = .{ .value = value, .layout_idx = self.store.getLocal(target).layout_idx } }, next);
    }

    fn str(self: *Builder, target: LocalId, text: []const u8, next: CFStmtId) TestError!CFStmtId {
        return self.literal(target, .{ .str_literal = try self.store.insertStringView(text, 0, @intCast(text.len)) }, next);
    }

    /// `target` = the address of an immortal object (a static descriptor or
    /// name), read as static data.
    fn static(self: *Builder, target: LocalId, given: StaticObject, next: CFStmtId) TestError!CFStmtId {
        const id: LIR.StaticDataId = @fromBackingInt(@intCast(self.statics.items.len));
        // A table's descriptors outlive the caller's slice of them.
        const object: StaticObject = switch (given) {
            .desc, .name => given,
            .table => |descs| blk: {
                const owned = try allocator.dupe(*const TypeDesc, descs);
                errdefer allocator.free(owned);
                try self.table_descs.append(allocator, owned);
                break :blk .{ .table = owned };
            },
        };
        const cell = try allocator.create(usize);
        cell.* = switch (object) {
            .desc => |desc| @intFromPtr(desc),
            .name => |name| @intFromPtr(name),
            .table => |descs| blk: {
                const words = try allocator.alloc(usize, descs.len);
                try self.tables.append(allocator, words);
                for (words, descs) |*word, desc| word.* = @intFromPtr(desc);
                break :blk @intFromPtr(words.ptr);
            },
        };
        try self.cells.append(allocator, cell);
        try self.statics.append(allocator, @intFromPtr(cell));
        try self.objects.append(allocator, object);
        return self.literal(target, .{ .static_data = .{ .id = id } }, next);
    }

    /// The static data of the wasm program: each slot's word addresses its
    /// object, serialized in the descriptor format at the wasm32 pointer
    /// width: a descriptor and everything it reaches, a name and its text,
    /// or a table's words and their descriptors, each object once
    /// (`WasmObjects`).
    fn wasmStaticData(self: *Builder) TestError![]StaticDataExport {
        var objects: WasmObjects = .{};
        errdefer objects.deinit();
        defer objects.symbols.deinit(allocator);
        for (self.objects.items, 0..) |object, slot| {
            const id: LIR.StaticDataId = @fromBackingInt(@intCast(slot));
            const slot_name = try lir.Program.staticDataSymbolName(allocator, id);
            errdefer allocator.free(slot_name);
            const target = switch (object) {
                .desc => |desc| try objects.desc(desc),
                .name => |name| try objects.name(name),
                .table => |descs| blk: {
                    const bytes = try allocator.alloc(u8, descs.len * wasm_word_size);
                    errdefer allocator.free(bytes);
                    @memset(bytes, 0);
                    const relocations = try allocator.alloc(StaticDataRelocation, descs.len);
                    errdefer allocator.free(relocations);
                    for (descs, relocations, 0..) |desc, *relocation, index| {
                        relocation.* = .{ .offset = LIR.tableEntryOffset(@intCast(index), wasm_word_size), .target_symbol_name = try objects.desc(desc) };
                    }
                    break :blk try objects.add(bytes, wasm_word_size, relocations);
                },
            };
            try objects.exports.append(allocator, try wordExport(slot_name, target));
        }
        return objects.exports.toOwnedSlice(allocator);
    }

    /// Objects serialized at the wasm32 width, each under one symbol
    /// (`roc__w{index}`), by the address of the native object it encodes.
    const WasmObjects = struct {
        exports: std.ArrayList(StaticDataExport) = .empty,
        symbols: std.AutoHashMapUnmanaged(usize, []const u8) = .empty,

        /// Free the exports; the symbol map is freed apart from them.
        fn deinit(self: *WasmObjects) void {
            for (self.exports.items) |item| freeExport(item);
            self.exports.deinit(allocator);
        }

        /// Export `bytes` under a fresh symbol, which the caller's
        /// relocations may name before the export they target.
        fn add(self: *WasmObjects, bytes: []u8, alignment: u32, relocations: []StaticDataRelocation) TestError![]const u8 {
            const symbol = try std.fmt.allocPrint(allocator, "roc__w{d}", .{self.exports.items.len});
            try self.exports.append(allocator, .{
                .symbol_name = symbol,
                .bytes = bytes,
                .alignment = alignment,
                .is_exported = false,
                .relocations = relocations,
            });
            return symbol;
        }

        /// The symbol of `name` and its text.
        fn name(self: *WasmObjects, value: *const Name) TestError![]const u8 {
            if (self.symbols.get(@intFromPtr(value))) |known| return known;
            const text = try self.add(try allocator.dupe(u8, value.text()), 1, try allocator.alloc(StaticDataRelocation, 0));
            const name_layout = boxy.desc.externLayout(Name, .u32);
            const bytes = try allocator.alloc(u8, name_layout.size);
            @memset(bytes, 0);
            @memcpy(bytes[name_layout.of(.id)..][0..32], &boxy.desc.nameIdDigest(value.id));
            std.mem.writeInt(u32, bytes[name_layout.of(.len)..][0..4], value.len, .little);
            const relocations = try allocator.alloc(StaticDataRelocation, 1);
            relocations[0] = .{ .offset = name_layout.of(.bytes), .target_symbol_name = text };
            const symbol = try self.add(bytes, name_layout.alignment, relocations);
            try self.symbols.put(allocator, @intFromPtr(value), symbol);
            return symbol;
        }

        /// The symbol of `value` and of everything it reaches. A `Str` or
        /// function leaf is the native one, whose size is the wasm32 width's
        /// there; every other descriptor is laid out at the wasm32 width
        /// already (`test_descs.Builder`).
        fn desc(self: *WasmObjects, value: *const TypeDesc) TestError![]const u8 {
            if (self.symbols.get(@intFromPtr(value))) |known| return known;
            std.debug.assert(value.methods == null);
            const desc_layout = TypeDesc.layout(.u32);
            const bytes = try allocator.alloc(u8, desc_layout.size);
            @memset(bytes, 0);
            const relocations = try allocator.alloc(StaticDataRelocation, 3);
            const symbol = try self.add(bytes, desc_layout.alignment, relocations);
            // Reserved first: a recursive type's descriptor reaches itself.
            try self.symbols.put(allocator, @intFromPtr(value), symbol);
            const size: u32, const alignment: u32 = switch (value.kind) {
                .str => .{ RocStr.word_count * wasm_word_size, wasm_word_size },
                .erased_callable => .{ wasm_word_size, wasm_word_size },
                .zst, .scalar, .bool, .list, .box, .record, .tuple, .tag_union, .nominal, .recursive_box => .{ value.size, value.alignment },
            };
            std.mem.writeInt(u32, bytes[desc_layout.of(.size)..][0..4], size, .little);
            std.mem.writeInt(u32, bytes[desc_layout.of(.alignment)..][0..4], alignment, .little);
            bytes[desc_layout.of(.kind)] = @backingInt(value.kind);
            bytes[desc_layout.of(.scalar)] = @backingInt(value.scalar);
            bytes[desc_layout.of(.sort_key)] = value.sort_key;
            bytes[desc_layout.of(.flags)] = @bitCast(value.flags);
            std.mem.writeInt(u32, bytes[desc_layout.of(.count)..][0..4], value.count, .little);
            std.mem.writeInt(u32, bytes[desc_layout.of(.discriminant_offset)..][0..4], value.discriminant_offset, .little);
            std.mem.writeInt(u32, bytes[desc_layout.of(.discriminant_size)..][0..4], value.discriminant_size, .little);
            var count: usize = 0;
            if (value.child) |child| {
                relocations[count] = .{ .offset = desc_layout.of(.child), .target_symbol_name = try self.desc(child) };
                count += 1;
            }
            if (value.name) |nominal_name| {
                relocations[count] = .{ .offset = desc_layout.of(.name), .target_symbol_name = try self.name(nominal_name) };
                count += 1;
            }
            const entries: ?[]const u8 = switch (value.kind) {
                .record, .tuple => if (value.count == 0) null else try self.fields(value),
                .tag_union => if (value.count == 0) null else try self.variants(value),
                .zst, .scalar, .str, .bool, .list, .box, .erased_callable, .nominal, .recursive_box => null,
            };
            if (entries) |target| {
                relocations[count] = .{ .offset = desc_layout.of(.entries), .target_symbol_name = target };
                count += 1;
            }
            // The export's relocations are the ones written.
            const at = for (self.exports.items, 0..) |item, index| {
                if (item.symbol_name.ptr == symbol.ptr) break index;
            } else unreachable; // `add` exported the symbol above
            self.exports.items[at].relocations = try allocator.realloc(relocations, count);
            return symbol;
        }

        /// The symbol of a record's or tuple's field table and the trailer
        /// of its reference-holding slots.
        fn fields(self: *WasmObjects, value: *const TypeDesc) TestError![]const u8 {
            const field_layout = boxy.Field.layout(.u32);
            const slots = value.refcountedFieldSlots();
            const trailer_words: usize = if (value.flags.contains_refcounted) slots.len + 1 else 0;
            const table_size = value.count * field_layout.size;
            const bytes = try allocator.alloc(u8, table_size + trailer_words * 4);
            @memset(bytes, 0);
            var relocations: std.ArrayList(StaticDataRelocation) = .empty;
            for (value.fields(), 0..) |field, index| {
                const at: u32 = @intCast(index * field_layout.size);
                std.mem.writeInt(u32, bytes[at + field_layout.of(.field_index) ..][0..4], field.field_index, .little);
                std.mem.writeInt(u32, bytes[at + field_layout.of(.offset) ..][0..4], field.offset, .little);
                std.mem.writeInt(u32, bytes[at + field_layout.of(.is_padding) ..][0..4], field.is_padding, .little);
                std.mem.writeInt(u32, bytes[at + field_layout.of(.field_slot) ..][0..4], field.field_slot, .little);
                if (field.name) |field_name| try relocations.append(allocator, .{ .offset = at + field_layout.of(.name), .target_symbol_name = try self.name(field_name) });
                try relocations.append(allocator, .{ .offset = at + field_layout.of(.desc), .target_symbol_name = try self.desc(field.desc) });
            }
            if (trailer_words != 0) {
                std.mem.writeInt(u32, bytes[table_size..][0..4], @intCast(slots.len), .little);
                for (slots, 0..) |slot, index| std.mem.writeInt(u32, bytes[table_size + 4 * (index + 1) ..][0..4], slot, .little);
            }
            return try self.add(bytes, field_layout.alignment, try relocations.toOwnedSlice(allocator));
        }

        /// The symbol of a tag union's variant table.
        fn variants(self: *WasmObjects, value: *const TypeDesc) TestError![]const u8 {
            const variant_layout = boxy.Variant.layout(.u32);
            const bytes = try allocator.alloc(u8, value.count * variant_layout.size);
            @memset(bytes, 0);
            var relocations: std.ArrayList(StaticDataRelocation) = .empty;
            for (value.variants(), 0..) |variant, index| {
                const at: u32 = @intCast(index * variant_layout.size);
                std.mem.writeInt(u32, bytes[at + variant_layout.of(.discriminant) ..][0..4], variant.discriminant, .little);
                std.mem.writeInt(u32, bytes[at + variant_layout.of(.payload_count) ..][0..4], variant.payload_count, .little);
                try relocations.append(allocator, .{ .offset = at + variant_layout.of(.name), .target_symbol_name = try self.name(variant.name) });
                try relocations.append(allocator, .{ .offset = at + variant_layout.of(.payload), .target_symbol_name = try self.desc(variant.payload) });
            }
            return try self.add(bytes, variant_layout.alignment, try relocations.toOwnedSlice(allocator));
        }
    };

    /// A pointer-sized word holding the address of `target`.
    fn wordExport(symbol_name: []const u8, target: []const u8) TestError!StaticDataExport {
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

    fn freeExport(item: StaticDataExport) void {
        allocator.free(item.symbol_name);
        allocator.free(item.bytes);
        allocator.free(item.relocations);
    }

    fn freeStaticData(exports: []StaticDataExport) void {
        for (exports) |item| freeExport(item);
        allocator.free(exports);
    }

    fn dyn(self: *Builder, target: ?LocalId, op: DynOp, next: CFStmtId) TestError!CFStmtId {
        return self.store.addDynStmt(target, op, next, .test_fixture);
    }

    fn call(self: *Builder, target: LocalId, proc_id: LirProcSpecId, args: []const LocalId, next: CFStmtId) TestError!CFStmtId {
        return self.store.addCFStmt(.{ .assign_call = .{ .target = target, .proc = proc_id, .args = try self.store.addLocalSpan(args), .next = next } }, .test_fixture);
    }

    /// A call that replaces this procedure's frame with the callee's.
    fn tailCall(self: *Builder, target: LocalId, proc_id: LirProcSpecId, args: []const LocalId, next: CFStmtId) TestError!CFStmtId {
        return self.store.addCFStmt(.{ .assign_call = .{
            .target = target,
            .proc = proc_id,
            .args = try self.store.addLocalSpan(args),
            .extra = try self.store.addStmtRow(LIR.CallExtra, .{ .replaces_frame = true }),
            .next = next,
        } }, .test_fixture);
    }

    fn lowLevel(self: *Builder, target: LocalId, op: base.LowLevel, args: []const LocalId, dyn_elem_desc: bool, next: CFStmtId) TestError!CFStmtId {
        return self.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = op,
            .rc_effect = try self.store.addRcEffect(op.rcEffect()),
            .extra = if (dyn_elem_desc) try self.store.addStmtRow(LIR.LowLevelExtra, .{ .dyn_elem_desc = true }) else .none,
            .args = try self.store.addLocalSpan(args),
            .next = next,
        } }, .test_fixture);
    }

    fn setLocal(self: *Builder, target: LocalId, value: LocalId, mode: LIR.SetLocalWriteMode, next: CFStmtId) TestError!CFStmtId {
        return self.store.addCFStmt(.{ .set_local = .{ .target = target, .value = value, .mode = mode, .next = next } }, .test_fixture);
    }

    fn decref(self: *Builder, value: LocalId, next: CFStmtId) TestError!CFStmtId {
        return self.store.addCFStmt(.{ .decref = .{
            .value = value,
            .rc = .{ .concrete = .{ .op = .decref, .layout_idx = self.store.getLocal(value).layout_idx } },
            .next = next,
        } }, .test_fixture);
    }

    /// `target` = an erased callable of `worker` whose capture is the word
    /// `capture`.
    fn packClosure(self: *Builder, target: LocalId, worker: LirProcSpecId, capture: LocalId, next: CFStmtId) TestError!CFStmtId {
        return self.store.addCFStmt(.{ .assign_packed_erased_fn = .{
            .target = target,
            .data = try self.store.addStmtRow(LIR.PackedErasedFnData, .{
                .proc = worker,
                .capture = capture,
                .capture_layout = .opaque_ptr,
                .on_drop = .none,
            }),
            .next = next,
        } }, .test_fixture);
    }

    fn span(self: *Builder, locals: []const LocalId) TestError!LIR.LocalSpan {
        return self.store.addLocalSpan(locals);
    }
};

const Run = struct {
    env: RuntimeHostEnv,
    static_strings: backend.StaticStringData.Table,
    interpreter: Interpreter,
    builder: *Builder,

    fn init(self: *Run, b: *Builder) TestError!void {
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
    fn eval(self: *Run, proc_id: LirProcSpecId, ret_layout: layout_mod.Idx) TestError!Interpreter.EvalResult {
        const result = try self.interpreter.eval(.{ .proc_id = proc_id, .ret_layout = ret_layout });
        try self.expectSame(result, ret_layout, try self.runLlvm(proc_id, ret_layout));
        if (comptime !backend.host_lir_codegen_available) return result;
        try self.expectSame(result, ret_layout, try self.runDev(proc_id, ret_layout));
        return result;
    }

    /// Check a backend's result bytes against the interpreter's, and release
    /// them.
    fn expectSame(self: *Run, result: Interpreter.EvalResult, ret_layout: layout_mod.Idx, bytes: []align(16) u8) TestError!void {
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
    fn resultBytes(self: *Run, ret_layout: layout_mod.Idx) TestError![]align(16) u8 {
        const size = self.builder.layouts.layoutSize(self.builder.layouts.getLayout(ret_layout));
        const bytes = try allocator.alignedAlloc(u8, .@"16", @max(size, 1));
        @memset(bytes, 0);
        return bytes;
    }

    /// Compile every procedure with the LLVM backend and run `proc_id` in
    /// this process, returning its result bytes.
    fn runLlvm(self: *Run, proc_id: LirProcSpecId, ret_layout: layout_mod.Idx) TestError![]align(16) u8 {
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
    fn runDev(self: *Run, proc_id: LirProcSpecId, ret_layout: layout_mod.Idx) TestError![]align(16) u8 {
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
fn runWasm(b: *Builder, proc_id: LirProcSpecId, ret_layout: layout_mod.Idx) TestError![]u8 {
    std.debug.assert(b.layouts.targetUsize() == .u32);
    var codegen = backend.wasm.WasmCodeGen.init(allocator, &b.store, &b.layouts, .default);
    defer codegen.deinit();
    const exports = try b.wasmStaticData();
    defer Builder.freeStaticData(exports);
    try codegen.module.addStaticDataExports(exports);
    const generated = try codegen.generateModule(proc_id, ret_layout, wasm32_builtins.bytes, wasm32_builtins.boxy_runtime_bytes);
    defer allocator.free(generated.wasm_bytes);
    // Every test procedure returns one of these layouts.
    std.debug.assert(ret_layout == .str or ret_layout == .i64 or ret_layout == .u64 or ret_layout == .u32 or ret_layout == .bool);
    const shape: WasmRunner.RawResult = if (ret_layout == .str)
        .str
    else if (ret_layout == .i64 or ret_layout == .u64)
        .i64
    else
        .i32;
    return WasmRunner.runWasmRaw(allocator, generated.wasm_bytes, generated.heap_base, generated.has_imports, shape);
}

fn expectWasmInt(b: *Builder, proc_id: LirProcSpecId, ret_layout: layout_mod.Idx, expected: i64) TestError!void {
    const bytes = try runWasm(b, proc_id, ret_layout);
    defer allocator.free(bytes);
    const actual: i64 = switch (bytes.len) {
        4 => @as(i32, @bitCast(std.mem.readInt(u32, bytes[0..4], .little))),
        8 => @bitCast(std.mem.readInt(u64, bytes[0..8], .little)),
        else => unreachable, // `runWasm` returns a four- or eight-byte scalar
    };
    try std.testing.expectEqual(expected, actual);
}

fn expectWasmStr(b: *Builder, proc_id: LirProcSpecId, expected: []const u8) TestError!void {
    const text = try runWasm(b, proc_id, .str);
    defer allocator.free(text);
    try std.testing.expectEqualStrings(expected, text);
}

/// `id : a -> a`, lowered once: its parameter is the caller's storage and its
/// result is written to the storage the caller passes.
fn buildIdentity(b: *Builder) TestError!LirProcSpecId {
    const desc = try b.local(.opaque_ptr);
    const x = try b.dynLocal(desc, .param);
    const result = try b.dynLocal(desc, .result);
    return b.proc(&.{ x, desc }, try b.ret(x), b.dynamic, .{ .dyn_result = result });
}

const IdentityProgram = struct {
    int_proc: LirProcSpecId,
    str_proc: LirProcSpecId,

    fn build(b: *Builder) TestError!IdentityProgram {
        const id = try buildIdentity(b);

        const int_desc = try b.local(.opaque_ptr);
        const int_arg = try b.local(.i64);
        const int_out = try b.local(.i64);
        const int_body = try b.static(int_desc, .{ .desc = boxy.desc.scalarDesc(.i64) }, try b.int(int_arg, -42, try b.call(int_out, id, &.{ int_arg, int_desc }, try b.ret(int_out))));
        const int_proc = try b.proc(&.{}, int_body, .i64, .{});

        const str_desc = try b.local(.opaque_ptr);
        const left = try b.local(.str);
        const right = try b.local(.str);
        const joined = try b.local(.str);
        const str_out = try b.local(.str);
        const str_body = try b.static(str_desc, .{ .desc = b.descs.str() }, try b.str(left, "a string long enough to live on the heap, ", try b.str(right, "joined at runtime", try b.lowLevel(joined, .str_concat, &.{ left, right }, false, try b.call(str_out, id, &.{ joined, str_desc }, try b.ret(str_out))))));
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

/// `id : a -> a` reading its descriptor from word 1 of its type table, the
/// one hidden argument it takes; word 0 describes another type, so a read
/// of the wrong word returns the wrong bytes.
const TableIdentityProgram = struct {
    int_proc: LirProcSpecId,
    str_proc: LirProcSpecId,

    fn build(b: *Builder) TestError!TableIdentityProgram {
        const table = try b.local(.opaque_ptr);
        const desc = try b.local(.opaque_ptr);
        const x = try b.dynLocal(desc, .param);
        const result = try b.dynLocal(desc, .result);
        const id = try b.proc(&.{ x, table }, try b.dyn(desc, .{ .table_entry = .{ .table = table, .index = 1 } }, try b.ret(x)), b.dynamic, .{ .dyn_result = result });

        const int_table = try b.local(.opaque_ptr);
        const int_arg = try b.local(.i64);
        const int_out = try b.local(.i64);
        const int_body = try b.static(int_table, .{ .table = &.{ b.descs.str(), boxy.desc.scalarDesc(.i64) } }, try b.int(int_arg, -42, try b.call(int_out, id, &.{ int_arg, int_table }, try b.ret(int_out))));
        const int_proc = try b.proc(&.{}, int_body, .i64, .{});

        const str_table = try b.local(.opaque_ptr);
        const left = try b.local(.str);
        const right = try b.local(.str);
        const str_arg = try b.local(.str);
        const str_out = try b.local(.str);
        const str_body = try b.static(str_table, .{ .table = &.{ boxy.desc.scalarDesc(.i64), b.descs.str() } }, try b.str(left, "a string long enough to live on the heap, ", try b.str(right, "read through a table", try b.lowLevel(str_arg, .str_concat, &.{ left, right }, false, try b.call(str_out, id, &.{ str_arg, str_table }, try b.ret(str_out))))));
        const str_proc = try b.proc(&.{}, str_body, .str, .{});
        return .{ .int_proc = int_proc, .str_proc = str_proc };
    }
};

test "boxy interpreter: a generic identity reads its descriptor from its type table" {
    var b = try Builder.init(.native);
    defer b.deinit();
    const program = try TableIdentityProgram.build(&b);

    var run: Run = undefined;
    try run.init(&b);
    defer run.deinit();

    const int_result = try run.eval(program.int_proc, .i64);
    try std.testing.expectEqual(@as(i64, -42), int_result.value.read(i64));

    const str_result = try run.eval(program.str_proc, .str);
    try std.testing.expectEqualStrings("a string long enough to live on the heap, read through a table", readStr(str_result.value));
    run.interpreter.dropValue(str_result.value, .str);
    try run.env.checkForLeaks();
}

test "boxy wasm: a generic identity reads its descriptor from its type table" {
    var b = try Builder.init(.u32);
    defer b.deinit();
    const program = try TableIdentityProgram.build(&b);
    try expectWasmInt(&b, program.int_proc, .i64, -42);
    try expectWasmStr(&b, program.str_proc, "a string long enough to live on the heap, read through a table");
}

/// Builds `{ a : I64, b : Str }` in a dynamic frame local.
const RecordFixture = struct {
    record_desc: LocalId,
    record: LocalId,

    /// Statements reading the descriptor and building the record, then
    /// `next`.
    fn build(b: *Builder, next_after: fn (*Builder, RecordFixture) TestError!CFStmtId) TestError!CFStmtId {
        const record_desc = try b.local(.opaque_ptr);
        const record = try b.dynLocal(record_desc, .frame);
        const x = try b.local(.i64);
        const s = try b.local(.str);
        const fixture: RecordFixture = .{ .record_desc = record_desc, .record = record };
        const rest = try next_after(b, fixture);
        // Fields in any order; the descriptor orders them by name.
        const desc = b.descs.record(&.{ .{ .name = &name_b, .desc = b.descs.str() }, .{ .name = &name_a, .desc = boxy.desc.scalarDesc(.i64) } });
        return b.static(record_desc, .{ .desc = desc }, try b.dyn(null, .frame_alloc, try b.int(x, 7, try b.str(s, "hi", try b.dyn(record, .{ .record = .{
            .desc = record_desc,
            .fields = try b.span(&.{ x, s }),
        } }, rest)))));
    }
};

const RecordReadProgram = struct {
    int_proc: LirProcSpecId,
    str_proc: LirProcSpecId,

    fn build(b: *Builder) TestError!RecordReadProgram {
        const ReadInt = struct {
            fn next(builder: *Builder, f: RecordFixture) TestError!CFStmtId {
                // `a` ranks first; its position arrives in a local, as a
                // type table word does.
                const a = try builder.local(.u32);
                const out = try builder.local(.i64);
                return builder.int(a, 0, try builder.dyn(out, .{ .field_addr = .{ .base = f.record, .desc = f.record_desc, .field_index = .{ .local = a } } }, try builder.ret(out)));
            }
        };
        const int_proc = try b.proc(&.{}, try RecordFixture.build(b, ReadInt.next), .i64, .{});

        const ReadStr = struct {
            fn next(builder: *Builder, f: RecordFixture) TestError!CFStmtId {
                const field_desc = try builder.local(.opaque_ptr);
                const view = try builder.dynLocal(field_desc, .view);
                const out = try builder.local(.str);
                return builder.static(field_desc, .{ .desc = builder.descs.str() }, try builder.dyn(view, .{ .field_addr = .{ .base = f.record, .desc = f.record_desc, .field_index = .{ .immediate = 1 } } }, try builder.dyn(out, .{ .copy = .{ .src = view, .desc = field_desc } }, try builder.ret(out))));
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

    fn build(b: *Builder) TestError!RecordValueProgram {
        const Eq = struct {
            fn next(builder: *Builder, f: RecordFixture) TestError!CFStmtId {
                const out = try builder.local(.bool);
                return builder.dyn(out, .{ .eq = .{ .lhs = f.record, .rhs = f.record, .desc = f.record_desc } }, try builder.ret(out));
            }
        };
        const eq_proc = try b.proc(&.{}, try RecordFixture.build(b, Eq.next), .bool, .{});

        const Hash = struct {
            fn next(builder: *Builder, f: RecordFixture) TestError!CFStmtId {
                const seed = try builder.local(.u64);
                const out = try builder.local(.u64);
                return builder.int(seed, 99, try builder.dyn(out, .{ .hash = .{ .hasher = seed, .value = f.record, .desc = f.record_desc } }, try builder.ret(out)));
            }
        };
        const hash_proc = try b.proc(&.{}, try RecordFixture.build(b, Hash.next), .u64, .{});

        const Inspect = struct {
            fn next(builder: *Builder, f: RecordFixture) TestError!CFStmtId {
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

/// Procedures reading a tag union's discriminant and a payload, in that
/// order, of a value built by its discriminant, which arrives in a local as
/// a type table word does.
fn buildTagUnionProgram(b: *Builder) TestError![2]LirProcSpecId {
    const Proc = enum { discriminant, payload };
    var procs: [2]LirProcSpecId = undefined;
    inline for (.{ Proc.discriminant, Proc.payload }, 0..) |which, index| {
        const ok = try b.local(.u32);
        const union_desc = try b.local(.opaque_ptr);
        const value = try b.dynLocal(union_desc, .frame);
        const x = try b.local(.i64);
        const tail: CFStmtId = switch (which) {
            .discriminant => blk: {
                const out = try b.local(.u32);
                break :blk try b.dyn(out, .{ .tag_discriminant = .{ .base = value, .desc = union_desc } }, try b.ret(out));
            },
            .payload => blk: {
                const out = try b.local(.i64);
                break :blk try b.dyn(out, .{ .tag_payload_addr = .{ .base = value, .desc = union_desc, .payload_index = 0 } }, try b.ret(out));
            },
        };
        const desc = b.descs.tagUnion(&.{ .{ .name = &name_ok, .payloads = &.{boxy.desc.scalarDesc(.i64)} }, .{ .name = &name_err, .payloads = &.{b.descs.str()} } });
        // Tags are ordered by name: Err = 0, Ok = 1.
        const body = try b.int(ok, 1, try b.static(union_desc, .{ .desc = desc }, try b.dyn(null, .frame_alloc, try b.int(x, 1234, try b.dyn(value, .{ .tag = .{ .desc = union_desc, .discriminant = .{ .local = ok }, .payloads = try b.span(&.{x}) } }, tail)))));
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
    try std.testing.expectEqual(@as(u32, 1), (try run.eval(procs[0], .u32)).value.read(u32));
    try std.testing.expectEqual(@as(i64, 1234), (try run.eval(procs[1], .i64)).value.read(i64));
    try run.env.checkForLeaks();
}

test "boxy wasm: a tag union's discriminant and payload" {
    var b = try Builder.init(.u32);
    defer b.deinit();
    const procs = try buildTagUnionProgram(&b);
    try expectWasmInt(&b, procs[0], .u32, 1);
    try expectWasmInt(&b, procs[1], .i64, 1234);
}

/// acc = 5; i = 3; loop { if i == 0 then return acc else i -= 1; acc = acc }
fn buildJoinProgram(b: *Builder) TestError!LirProcSpecId {
    const desc = try b.local(.opaque_ptr);
    const start = try b.local(.i64);
    const acc = try b.dynLocal(desc, .frame);
    const i = try b.local(.i64);
    const i_init = try b.local(.i64);
    const one = try b.local(.i64);
    const next_i = try b.local(.i64);
    const out = try b.local(.i64);
    const join_id = b.freshJoinPointId();

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
    const body = try b.static(desc, .{ .desc = boxy.desc.scalarDesc(.i64) }, try b.dyn(null, .frame_alloc, try b.int(start, 5, try b.int(i_init, 3, join))));
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
fn buildListProgram(b: *Builder) TestError![2]LirProcSpecId {
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
        const body = try b.static(elem_desc, .{ .desc = b.descs.str() }, try b.static(list_desc, .{ .desc = b.descs.list(b.descs.str()) }, try b.dyn(null, .frame_alloc, try b.str(s1, "first", try b.str(s2, "second", try b.setLocal(x, s1, .replace_existing, try b.setLocal(y, s2, .replace_existing, try b.dyn(list, .{ .list = .{ .elem_desc = elem_desc, .elems = try b.span(&.{ x, y }) } }, tail))))))));
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

/// A caller of `\(x) -> x`, a worker over a type variable whose type table
/// is its one capture: the table's words describe the variable and the
/// worker's argument record.
fn buildDictionaryProgram(b: *Builder) TestError!LirProcSpecId {
    const capture = try b.local(.opaque_ptr);
    const table = try b.local(.opaque_ptr);
    const elem_desc = try b.local(.opaque_ptr);
    const args_desc = try b.local(.opaque_ptr);
    const args = try b.dynLocal(args_desc, .param);
    const x = try b.dynLocal(elem_desc, .view);
    const result = try b.dynLocal(elem_desc, .result);
    const worker_body = try b.lowLevel(table, .erased_capture_load, &.{capture}, false, try b.dyn(elem_desc, .{ .table_entry = .{ .table = table, .index = 0 } }, try b.dyn(args_desc, .{ .table_entry = .{ .table = table, .index = 1 } }, try b.dyn(x, .{ .field_addr = .{ .base = args, .desc = args_desc, .field_index = .{ .immediate = 0 } } }, try b.ret(x)))));
    const worker = try b.proc(&.{ args, capture }, worker_body, b.dynamic, .{ .dyn_result = result, .abi = .erased_record, .erased_capture_arg = capture });

    const record_layout = try b.layouts.putTuple(&.{b.layouts.getLayout(.i64)});
    const int_table = try b.local(.opaque_ptr);
    const closure = try b.local(try b.layouts.insertErasedCallable());
    const value = try b.local(.i64);
    const record = try b.local(record_layout);
    const out = try b.local(.i64);
    const int_desc = boxy.desc.scalarDesc(.i64);
    const call = try b.dyn(out, .{ .call_erased = .{ .closure = closure, .args = record } }, try b.decref(closure, try b.ret(out)));
    const body = try b.static(int_table, .{ .table = &.{ int_desc, b.descs.args(&.{int_desc}) } }, try b.packClosure(closure, worker, int_table, try b.int(value, 31337, try b.store.addCFStmt(.{ .assign_struct = .{ .target = record, .fields = try b.span(&.{value}), .next = call } }, .test_fixture))));
    return b.proc(&.{}, body, .i64, .{});
}

test "boxy interpreter: a closure over a type table called through the erased record ABI" {
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

test "boxy wasm: a closure over a type table called through the erased record ABI" {
    var b = try Builder.init(.u32);
    defer b.deinit();
    try expectWasmInt(&b, try buildDictionaryProgram(&b), .i64, 31337);
}

const TailProgram = struct {
    int_proc: LirProcSpecId,
    str_proc: LirProcSpecId,

    fn build(b: *Builder) TestError!TailProgram {
        const group = b.freshTailGroupId();

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
        const str_body = try b.static(str_desc, .{ .desc = b.descs.str() }, try b.str(left, "a string long enough to live on the heap, ", try b.str(right, "moved by a tail call", try b.lowLevel(joined, .str_concat, &.{ left, right }, false, try b.call(str_out, hop, &.{ joined, str_desc }, try b.ret(str_out))))));
        const str_proc = try b.proc(&.{}, str_body, .str, .{});

        const int_desc = try b.local(.opaque_ptr);
        const int_arg = try b.local(.i64);
        const int_out = try b.local(.i64);
        const int_body = try b.static(int_desc, .{ .desc = boxy.desc.scalarDesc(.i64) }, try b.int(int_arg, 77, try b.call(int_out, hop, &.{ int_arg, int_desc }, try b.ret(int_out))));
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

/// Return a one-field generic record either directly in the caller's result
/// storage or from a distinct frame local. Its heap string is moved out again
/// by the caller, so every backend must preserve exactly one owned reference.
fn buildReturnedRecordProgram(b: *Builder, in_result: bool) TestError!struct { worker: LirProcSpecId, root: LirProcSpecId } {
    const elem_desc = try b.local(.opaque_ptr);
    const record_desc = try b.local(.opaque_ptr);
    const x = try b.dynLocal(elem_desc, .param);
    const result = try b.dynLocal(record_desc, .result);
    const produced = if (in_result) result else try b.dynLocal(record_desc, .frame);
    const worker_body = try b.dyn(null, .frame_alloc, try b.dyn(produced, .{ .record = .{
        .desc = record_desc,
        .fields = try b.span(&.{x}),
    } }, try b.ret(produced)));
    const worker = try b.proc(&.{ x, elem_desc, record_desc }, worker_body, b.dynamic, .{ .dyn_result = result });

    const str_desc = try b.local(.opaque_ptr);
    const desc = try b.local(.opaque_ptr);
    const record = try b.dynLocal(desc, .frame);
    const field = try b.dynLocal(str_desc, .view);
    const left = try b.local(.str);
    const right = try b.local(.str);
    const joined = try b.local(.str);
    const out = try b.local(.str);
    const body = try b.static(str_desc, .{ .desc = b.descs.str() }, try b.static(desc, .{ .desc = b.descs.record(&.{.{ .name = &name_a, .desc = b.descs.str() }}) }, try b.dyn(null, .frame_alloc, try b.str(left, "a returned record containing a heap string, ", try b.str(right, "still owned after returning", try b.lowLevel(joined, .str_concat, &.{ left, right }, false, try b.decref(right, try b.call(record, worker, &.{ joined, str_desc, desc }, try b.dyn(field, .{ .field_addr = .{ .base = record, .desc = desc, .field_index = .{ .immediate = 0 } } }, try b.dyn(out, .{ .copy = .{ .src = field, .desc = str_desc } }, try b.ret(out)))))))))));
    return .{ .worker = worker, .root = try b.proc(&.{}, body, .str, .{}) };
}

test "boxy dynamic returns copy distinct storage and omit an already constructed result" {
    for ([_]bool{ false, true }) |in_result| {
        var b = try Builder.init(.native);
        defer b.deinit();
        const program = try buildReturnedRecordProgram(&b, in_result);
        if (comptime backend.host_lir_codegen_available) {
            var codegen = try backend.HostLirCodeGen.init(allocator, &b.store, &b.layouts, .{}, .default);
            defer codegen.deinit();
            try codegen.emitProcFragment(program.worker);
            var copies_result = false;
            for (codegen.getSymbolNames()) |symbol| {
                if (std.mem.eql(u8, symbol, "roc_desc_copy")) copies_result = true;
            }
            try std.testing.expectEqual(!in_result, copies_result);
        }
        var run: Run = undefined;
        try run.init(&b);
        defer run.deinit();
        const result = try run.eval(program.root, .str);
        try std.testing.expectEqualStrings("a returned record containing a heap string, still owned after returning", readStr(result.value));
        run.interpreter.dropValue(result.value, .str);
        try run.env.checkForLeaks();
    }
}

test "boxy wasm dynamic returns preserve a heap string in either result storage" {
    for ([_]bool{ false, true }) |in_result| {
        var b = try Builder.init(.u32);
        defer b.deinit();
        const program = try buildReturnedRecordProgram(&b, in_result);
        try expectWasmStr(&b, program.root, "a returned record containing a heap string, still owned after returning");
    }
}

/// Both entry representations accept the same caller bytes. Exercise a
/// direct owned parameter and the original record/view, caller reuse, and
/// the deferred runtime's copy out of the caller's frame.
fn buildUnaryArgumentProgram(b: *Builder, dynamic: bool, reuse: bool, deferred: bool, direct_entry: bool) TestError!LirProcSpecId {
    const capture = try b.local(.opaque_ptr);
    const table = try b.local(.opaque_ptr);
    const elem_desc = try b.local(.opaque_ptr);
    const args_desc = try b.local(.opaque_ptr);
    const args = try b.dynLocal(if (direct_entry) elem_desc else args_desc, .param);
    const x = if (direct_entry) args else try b.dynLocal(elem_desc, .view);
    const result = try b.dynLocal(elem_desc, .result);
    const use_arg = if (direct_entry) try b.ret(x) else try b.dyn(args_desc, .{ .table_entry = .{ .table = table, .index = 1 } }, try b.dyn(x, .{ .field_addr = .{ .base = args, .desc = args_desc, .field_index = .{ .immediate = 0 } } }, try b.ret(x)));
    const worker_body = try b.lowLevel(table, .erased_capture_load, &.{capture}, false, try b.dyn(elem_desc, .{ .table_entry = .{ .table = table, .index = 0 } }, use_arg));
    const worker = try b.proc(&.{ args, capture }, worker_body, b.dynamic, .{ .dyn_result = result, .abi = .erased_record, .erased_capture_arg = capture });

    const desc = try b.local(.opaque_ptr);
    const desc_table = try b.local(.opaque_ptr);
    const closure = try b.local(try b.layouts.insertErasedCallable());
    const left = try b.local(.str);
    const right = try b.local(.str);
    const joined = try b.local(.str);
    const arg = if (dynamic) try b.dynLocal(desc, .frame) else joined;
    const out = try b.local(.str);
    const final = if (reuse) try b.local(.str) else out;
    // A deferred call owns one reference to the closure until it is driven.
    var next = if (deferred) try b.ret(final) else try b.decref(closure, try b.ret(final));
    if (reuse) next = try b.lowLevel(final, .str_concat, &.{ out, joined }, false, try b.decref(joined, next));
    next = try b.dyn(out, .{ .call_erased = .{
        .closure = closure,
        .args = arg,
        .deferred = deferred,
        .drive = if (deferred) .always else .none,
    } }, next);
    if (reuse) next = try b.store.addCFStmt(.{ .incref = .{
        .value = arg,
        .count = 1,
        .rc = .{ .concrete = .{ .op = .incref, .layout_idx = b.store.getLocal(arg).layout_idx } },
        .next = next,
    } }, .test_fixture);
    if (dynamic) next = try b.dyn(arg, .{ .copy = .{ .src = joined, .desc = desc } }, next);
    next = try b.str(left, "a heap string passed ", try b.str(right, "without a temporary argument record", try b.lowLevel(joined, .str_concat, &.{ left, right }, false, try b.decref(right, next))));
    next = try b.dyn(null, .frame_alloc, next);
    next = try b.packClosure(closure, worker, desc_table, next);
    next = try b.static(desc_table, .{ .table = &.{ b.descs.str(), b.descs.args(&.{b.descs.str()}) } }, next);
    next = try b.static(desc, .{ .desc = b.descs.str() }, next);
    return b.proc(&.{}, next, .str, .{ .uses_pending_cell = deferred });
}

const unary_text = "a heap string passed without a temporary argument record";

test "boxy unary argument storage supports caller reuse and deferred calls" {
    for ([_]bool{ false, true }) |dynamic| {
        for ([_]bool{ false, true }) |reuse| {
            for ([_]bool{ false, true }) |deferred| for ([_]bool{ false, true }) |direct_entry| {
                var b = try Builder.init(.native);
                defer b.deinit();
                const root = try buildUnaryArgumentProgram(&b, dynamic, reuse, deferred, direct_entry);
                var run: Run = undefined;
                try run.init(&b);
                defer run.deinit();
                const result = try run.eval(root, .str);
                try std.testing.expectEqualStrings(if (reuse) unary_text ++ unary_text else unary_text, readStr(result.value));
                run.interpreter.dropValue(result.value, .str);
                try run.env.checkForLeaks();
            };
        }
    }
}

test "boxy wasm unary argument storage supports caller reuse and deferred calls" {
    for ([_]bool{ false, true }) |dynamic| {
        for ([_]bool{ false, true }) |reuse| {
            for ([_]bool{ false, true }) |deferred| for ([_]bool{ false, true }) |direct_entry| {
                var b = try Builder.init(.u32);
                defer b.deinit();
                try expectWasmStr(&b, try buildUnaryArgumentProgram(&b, dynamic, reuse, deferred, direct_entry), if (reuse) unary_text ++ unary_text else unary_text);
            };
        }
    }
}
