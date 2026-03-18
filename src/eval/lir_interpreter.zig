//! LIR Expression Interpreter
//!
//! Evaluates post-RC LIR expressions directly, producing concrete runtime values.
//!
//! This interpreter replaces the CIR-based interpreter by consuming the same
//! lowered IR already used by the dev and wasm code generators.
//!
//! Design principles:
//! - Values are raw (pointer, layout) pairs — no runtime type variables
//! - RC ops (incref/decref/free) are executed literally from LIR
//! - Symbol-based environment (no pattern-index lookup)
//! - Follow the LIR control flow exactly

const std = @import("std");
const base = @import("base");
const layout_mod = @import("layout");
const lir = @import("lir");
const mir = @import("mir");
const lir_value = @import("lir_value.zig");
const lir_program_mod = @import("lir_program.zig");
const builtins = @import("builtins");
const types = @import("types");
const sljmp = @import("sljmp");

const Allocator = std.mem.Allocator;
const LirExprStore = lir.LirExprStore;
const LirExprId = lir.LirExprId;
const LirExpr = lir.LirExpr;
const LirPatternId = lir.LirPatternId;
const LirPattern = lir.LirPattern;
const LirStmt = lir.LirStmt;
const Symbol = lir.Symbol;
const Layout = layout_mod.Layout;
const Value = lir_value.Value;
const LayoutHelper = lir_value.LayoutHelper;
const RocDec = builtins.dec.RocDec;
const i128h = builtins.compiler_rt_128;

// Builtin types for direct dispatch
const RocStr = builtins.str.RocStr;
const RocList = builtins.list.RocList;
const RocOps = builtins.host_abi.RocOps;
const RocAlloc = builtins.host_abi.RocAlloc;
const RocDealloc = builtins.host_abi.RocDealloc;
const RocRealloc = builtins.host_abi.RocRealloc;
const RocDbg = builtins.host_abi.RocDbg;
const RocExpectFailed = builtins.host_abi.RocExpectFailed;
const RocCrashed = builtins.host_abi.RocCrashed;
const UpdateMode = builtins.utils.UpdateMode;
const JmpBuf = sljmp.JmpBuf;
const setjmp = sljmp.setjmp;
const longjmp = sljmp.longjmp;

/// Environment for RocOps in the LIR interpreter.
/// Uses a thread-local static buffer for allocation (same pattern as DevRocEnv)
/// to avoid Zig allocator vtable issues from C-calling-convention callbacks.
const InterpreterRocEnv = struct {
    allocator: Allocator,
    crashed: bool = false,
    crash_message: ?[]const u8 = null,
    jmp_buf: JmpBuf = undefined,

    /// Thread-local static buffer for allocations from builtins.
    const StaticAlloc = struct {
        threadlocal var buffer: [1024 * 1024]u8 align(16) = undefined;
        threadlocal var offset: usize = 0;
        const max_allocs = 4096;
        threadlocal var alloc_ptrs: [max_allocs]usize = [_]usize{0} ** max_allocs;
        threadlocal var alloc_sizes: [max_allocs]usize = [_]usize{0} ** max_allocs;
        threadlocal var alloc_count: usize = 0;

        fn recordAlloc(ptr: usize, size: usize) void {
            if (alloc_count < max_allocs) {
                alloc_ptrs[alloc_count] = ptr;
                alloc_sizes[alloc_count] = size;
                alloc_count += 1;
            }
        }

        fn getAllocSize(ptr: usize) usize {
            var i: usize = alloc_count;
            while (i > 0) {
                i -= 1;
                if (alloc_ptrs[i] == ptr) return alloc_sizes[i];
            }
            return 0;
        }

        fn reset() void {
            offset = 0;
            alloc_count = 0;
        }
    };

    fn init(allocator: Allocator) InterpreterRocEnv {
        return .{ .allocator = allocator };
    }

    fn deinit(self: *InterpreterRocEnv) void {
        if (self.crash_message) |msg| self.allocator.free(msg);
    }

    /// Reset the static buffer — call once at the start of a full evaluation.
    fn resetForEval(self: *InterpreterRocEnv) void {
        self.crashed = false;
        if (self.crash_message) |msg| self.allocator.free(msg);
        self.crash_message = null;
        StaticAlloc.reset();
    }

    /// Reset just the crash state before calling a builtin that might crash.
    fn resetCrash(self: *InterpreterRocEnv) void {
        self.crashed = false;
    }

    fn rocAllocFn(roc_alloc: *RocAlloc, env: *anyopaque) callconv(.c) void {
        const alignment = roc_alloc.alignment;
        const mask = alignment - 1;
        const aligned_offset = (StaticAlloc.offset + mask) & ~mask;
        if (aligned_offset + roc_alloc.length > StaticAlloc.buffer.len) {
            const self: *InterpreterRocEnv = @ptrCast(@alignCast(env));
            self.crashed = true;
            if (self.crash_message) |old| self.allocator.free(old);
            self.crash_message = self.allocator.dupe(u8, "static buffer overflow in alloc") catch null;
            longjmp(&self.jmp_buf, 1);
        }
        const ptr: [*]u8 = @ptrCast(&StaticAlloc.buffer[aligned_offset]);
        StaticAlloc.offset = aligned_offset + roc_alloc.length;
        StaticAlloc.recordAlloc(@intFromPtr(ptr), roc_alloc.length);
        roc_alloc.answer = @ptrCast(ptr);
    }

    fn rocDeallocFn(_: *RocDealloc, _: *anyopaque) callconv(.c) void {}

    fn rocReallocFn(roc_realloc: *RocRealloc, env: *anyopaque) callconv(.c) void {
        const alignment = roc_realloc.alignment;
        const mask = alignment - 1;
        const aligned_offset = (StaticAlloc.offset + mask) & ~mask;
        if (aligned_offset + roc_realloc.new_length > StaticAlloc.buffer.len) {
            const self: *InterpreterRocEnv = @ptrCast(@alignCast(env));
            self.crashed = true;
            if (self.crash_message) |old| self.allocator.free(old);
            self.crash_message = self.allocator.dupe(u8, "static buffer overflow in realloc") catch null;
            longjmp(&self.jmp_buf, 1);
        }
        const new_ptr: [*]u8 = @ptrCast(&StaticAlloc.buffer[aligned_offset]);
        StaticAlloc.offset = aligned_offset + roc_realloc.new_length;
        StaticAlloc.recordAlloc(@intFromPtr(new_ptr), roc_realloc.new_length);
        const old_ptr: [*]u8 = @ptrCast(@alignCast(roc_realloc.answer));
        const old_size = StaticAlloc.getAllocSize(@intFromPtr(old_ptr));
        const copy_len = @min(old_size, roc_realloc.new_length);
        if (copy_len > 0) {
            @memmove(new_ptr[0..copy_len], old_ptr[0..copy_len]);
        }
        roc_realloc.answer = @ptrCast(new_ptr);
    }

    fn rocDbgFn(_: *const RocDbg, _: *anyopaque) callconv(.c) void {}

    fn rocExpectFailedFn(_: *const RocExpectFailed, _: *anyopaque) callconv(.c) void {}

    fn rocCrashedFn(roc_crashed: *const RocCrashed, env: *anyopaque) callconv(.c) void {
        const self: *InterpreterRocEnv = @ptrCast(@alignCast(env));
        self.crashed = true;
        const msg = roc_crashed.utf8_bytes[0..roc_crashed.len];
        if (self.crash_message) |old| self.allocator.free(old);
        self.crash_message = self.allocator.dupe(u8, msg) catch null;
        longjmp(&self.jmp_buf, 1);
    }
};

pub const LirInterpreter = struct {
    allocator: Allocator,
    store: *const LirExprStore,
    layout_store: *const layout_mod.Store,
    helper: LayoutHelper,

    /// Symbol → (value pointer, size) bindings.
    bindings: std.AutoHashMap(u64, Binding),

    /// Mutable cells: symbol → pointer to current value.
    cells: std.AutoHashMap(u64, Binding),

    /// Top-level def cache: symbol → evaluated value.
    top_level_cache: std.AutoHashMap(u64, Binding),

    /// Set of symbols currently being evaluated (cycle detection).
    evaluating: std.AutoHashMap(u64, void),

    /// Arena for interpreter-allocated memory (temporaries, copies).
    arena: std.heap.ArenaAllocator,

    /// RocOps environment for builtin dispatch.
    roc_env: *InterpreterRocEnv,
    roc_ops: RocOps,

    /// Guard to reset the static buffer only once per top-level eval.
    eval_active: bool = false,

    pub const Error = error{
        OutOfMemory,
        RuntimeError,
        Crash,
    };

    const Binding = struct {
        val: Value,
        size: u32,
    };

    /// Result of evaluating an expression.
    /// Normal evaluation produces a value. Control flow is signaled as variants.
    pub const EvalResult = union(enum) {
        value: Value,
        early_return: Value,
        break_expr: void,
    };

    pub fn init(
        allocator: Allocator,
        store: *const LirExprStore,
        layout_store: *const layout_mod.Store,
    ) LirInterpreter {
        const roc_env = allocator.create(InterpreterRocEnv) catch @panic("OOM");
        roc_env.* = InterpreterRocEnv.init(allocator);

        const empty_hosted_fns = struct {
            fn dummyHostedFn(_: *anyopaque, _: *anyopaque, _: *anyopaque) callconv(.c) void {}
            var empty: [1]builtins.host_abi.HostedFn = .{builtins.host_abi.hostedFn(&dummyHostedFn)};
        };

        return .{
            .allocator = allocator,
            .store = store,
            .layout_store = layout_store,
            .helper = LayoutHelper.init(layout_store),
            .bindings = std.AutoHashMap(u64, Binding).init(allocator),
            .cells = std.AutoHashMap(u64, Binding).init(allocator),
            .top_level_cache = std.AutoHashMap(u64, Binding).init(allocator),
            .evaluating = std.AutoHashMap(u64, void).init(allocator),
            .arena = std.heap.ArenaAllocator.init(allocator),
            .roc_env = roc_env,
            .roc_ops = RocOps{
                .env = @ptrCast(roc_env),
                .roc_alloc = &InterpreterRocEnv.rocAllocFn,
                .roc_dealloc = &InterpreterRocEnv.rocDeallocFn,
                .roc_realloc = &InterpreterRocEnv.rocReallocFn,
                .roc_dbg = &InterpreterRocEnv.rocDbgFn,
                .roc_expect_failed = &InterpreterRocEnv.rocExpectFailedFn,
                .roc_crashed = &InterpreterRocEnv.rocCrashedFn,
                .hosted_fns = .{ .count = 0, .fns = &empty_hosted_fns.empty },
            },
        };
    }

    pub fn deinit(self: *LirInterpreter) void {
        self.roc_env.deinit();
        self.allocator.destroy(self.roc_env);
        self.arena.deinit();
        self.evaluating.deinit();
        self.top_level_cache.deinit();
        self.cells.deinit();
        self.bindings.deinit();
    }

    /// Allocate memory for a value of the given layout.
    fn alloc(self: *LirInterpreter, layout_idx: layout_mod.Idx) Error!Value {
        const size = self.helper.sizeOf(layout_idx);
        if (size == 0) return Value.zst;
        const slice = self.arena.allocator().alloc(u8, size) catch return error.OutOfMemory;
        @memset(slice, 0);
        return Value.fromSlice(slice);
    }

    /// Allocate raw bytes.
    fn allocBytes(self: *LirInterpreter, size: usize) Error!Value {
        if (size == 0) return Value.zst;
        const slice = self.arena.allocator().alloc(u8, size) catch return error.OutOfMemory;
        @memset(slice, 0);
        return Value.fromSlice(slice);
    }

    /// Allocate heap data through roc_ops with a refcount header.
    /// Use this for data that RocList.bytes or RocStr.bytes will point to,
    /// so builtins can safely call isUnique()/decref() on it.
    fn allocRocData(self: *LirInterpreter, data_bytes: usize, element_alignment: u32) Error![*]u8 {
        self.roc_env.resetCrash();
        const sj = setjmp(&self.roc_env.jmp_buf);
        if (sj != 0) return error.Crash;
        return builtins.utils.allocateWithRefcount(data_bytes, element_alignment, false, &self.roc_ops);
    }

    // ──────────────────────────────────────────────────────────────
    // Expression evaluation
    // ──────────────────────────────────────────────────────────────

    /// Evaluate a LIR expression, returning its value.
    pub fn eval(self: *LirInterpreter, expr_id: LirExprId) Error!EvalResult {
        // Reset static buffer on first eval call only (avoid resetting during recursion)
        if (!self.eval_active) {
            self.roc_env.resetForEval();
            self.eval_active = true;
        }
        const expr = self.store.getExpr(expr_id);
        return switch (expr) {
            .i64_literal => |lit| .{ .value = try self.evalI64Literal(lit.value, lit.layout_idx) },
            .i128_literal => |lit| .{ .value = try self.evalI128Literal(lit.value, lit.layout_idx) },
            .f64_literal => |v| .{ .value = try self.evalF64Literal(v) },
            .f32_literal => |v| .{ .value = try self.evalF32Literal(v) },
            .dec_literal => |v| .{ .value = try self.evalDecLiteral(v) },
            .str_literal => |idx| .{ .value = try self.evalStrLiteral(idx) },
            .bool_literal => |b| .{ .value = try self.evalBoolLiteral(b) },
            .lookup => |l| .{ .value = try self.evalLookup(l.symbol, l.layout_idx) },
            .cell_load => |l| .{ .value = try self.evalCellLoad(l.cell, l.layout_idx) },
            .block => |b| try self.evalBlock(b),
            .struct_ => |s| .{ .value = try self.evalStruct(s) },
            .struct_access => |sa| .{ .value = try self.evalStructAccess(sa) },
            .zero_arg_tag => |z| .{ .value = try self.evalZeroArgTag(z) },
            .tag => |t| .{ .value = try self.evalTag(t) },
            .if_then_else => |ite| try self.evalIfThenElse(ite),
            .match_expr => |m| try self.evalMatch(m),
            .discriminant_switch => |ds| try self.evalDiscriminantSwitch(ds),
            .tag_payload_access => |tpa| .{ .value = try self.evalTagPayloadAccess(tpa) },
            .call => |c| try self.evalCall(c),
            .lambda => |l| .{ .value = try self.evalLambda(l, expr_id) },
            .empty_list => |l| .{ .value = try self.evalEmptyList(l) },
            .list => |l| .{ .value = try self.evalList(l) },
            .nominal => |n| try self.eval(n.backing_expr),
            .early_return => |er| try self.evalEarlyReturn(er),
            .break_expr => .{ .break_expr = {} },
            .for_loop => |fl| try self.evalForLoop(fl),
            .while_loop => |wl| try self.evalWhileLoop(wl),
            .crash => |c| try self.evalCrash(c),
            .runtime_error => return error.RuntimeError,
            // RC ops — for now, evaluate the value and discard (no-op RC).
            .incref => |ir| blk: {
                _ = try self.eval(ir.value);
                break :blk .{ .value = Value.zst };
            },
            .decref => |dr| blk: {
                _ = try self.eval(dr.value);
                break :blk .{ .value = Value.zst };
            },
            .free => |f| blk: {
                _ = try self.eval(f.value);
                break :blk .{ .value = Value.zst };
            },
            // Cell operations
            .dbg => |d| try self.evalDbg(d),
            .expect => |e| try self.evalExpect(e),
            .hosted_call => return error.RuntimeError, // Phase 3
            .low_level => |ll| .{ .value = try self.evalLowLevel(ll) },
            .str_concat => |sc| .{ .value = try self.evalStrConcat(sc) },
            .int_to_str => |its| .{ .value = try self.evalIntToStr(its) },
            .float_to_str => |fts| .{ .value = try self.evalFloatToStr(fts) },
            .dec_to_str => |dts| .{ .value = try self.evalDecToStr(dts) },
            .str_escape_and_quote => |seq| .{ .value = try self.evalStrEscapeAndQuote(seq) },
        };
    }

    /// Evaluate an expression, expecting a normal value (not control flow).
    fn evalValue(self: *LirInterpreter, expr_id: LirExprId) Error!Value {
        const result = try self.eval(expr_id);
        return switch (result) {
            .value => |v| v,
            .early_return => |v| v,
            .break_expr => error.RuntimeError,
        };
    }

    // ──────────────────────────────────────────────────────────────
    // Literals
    // ──────────────────────────────────────────────────────────────

    fn evalI64Literal(self: *LirInterpreter, value: i64, layout_idx: layout_mod.Idx) Error!Value {
        const val = try self.alloc(layout_idx);
        const size = self.helper.sizeOf(layout_idx);
        const bits: u64 = @bitCast(value);
        switch (size) {
            1 => val.write(u8, @truncate(bits)),
            2 => val.write(u16, @truncate(bits)),
            4 => val.write(u32, @truncate(bits)),
            8 => val.write(u64, bits),
            else => return error.RuntimeError,
        }
        return val;
    }

    fn evalI128Literal(self: *LirInterpreter, value: i128, layout_idx: layout_mod.Idx) Error!Value {
        const val = try self.alloc(layout_idx);
        val.write(i128, value);
        return val;
    }

    fn evalF64Literal(self: *LirInterpreter, value: f64) Error!Value {
        const val = try self.alloc(.f64);
        val.write(f64, value);
        return val;
    }

    fn evalF32Literal(self: *LirInterpreter, value: f32) Error!Value {
        const val = try self.alloc(.f32);
        val.write(f32, value);
        return val;
    }

    fn evalDecLiteral(self: *LirInterpreter, value: i128) Error!Value {
        const val = try self.alloc(.dec);
        val.write(i128, value);
        return val;
    }

    fn evalStrLiteral(self: *LirInterpreter, idx: base.StringLiteral.Idx) Error!Value {
        const str_bytes = self.store.getString(idx);
        return self.makeRocStr(str_bytes);
    }

    fn evalBoolLiteral(self: *LirInterpreter, b: bool) Error!Value {
        const val = try self.alloc(.bool);
        val.write(u8, if (b) 1 else 0);
        return val;
    }

    // ──────────────────────────────────────────────────────────────
    // String helpers (RocStr construction)
    // ──────────────────────────────────────────────────────────────

    fn makeRocStr(self: *LirInterpreter, bytes: []const u8) Error!Value {
        const str_size = self.helper.sizeOf(.str);
        const val = try self.allocBytes(str_size);

        const target_usize = self.layout_store.targetUsize();
        const ptr_size = target_usize.size();

        if (ptr_size == 8) {
            // 64-bit: RocStr = { ptr, len, cap }
            const small_str_max = 3 * 8 - 1; // 23 bytes
            if (bytes.len <= small_str_max) {
                // Small string: store inline
                const dest = val.ptr[0..small_str_max];
                @memcpy(dest[0..bytes.len], bytes);
                // Set length in the last byte with high bit set
                val.ptr[small_str_max] = @intCast(bytes.len | 0x80);
            } else {
                // Heap string: allocate through roc_ops so builtins
                // can safely call isUnique()/decref() on the data.
                const heap_data = try self.allocRocData(bytes.len, 1);
                @memcpy(heap_data[0..bytes.len], bytes);
                val.write(usize, @intFromPtr(heap_data)); // ptr
                val.offset(8).write(usize, bytes.len); // len
                val.offset(16).write(usize, bytes.len); // cap
            }
        } else {
            // 32-bit: same layout but smaller
            const small_str_max = 3 * 4 - 1; // 11 bytes
            if (bytes.len <= small_str_max) {
                const dest = val.ptr[0..small_str_max];
                @memcpy(dest[0..bytes.len], bytes);
                val.ptr[small_str_max] = @intCast(bytes.len | 0x80);
            } else {
                const heap_data = try self.allocRocData(bytes.len, 1);
                @memcpy(heap_data[0..bytes.len], bytes);
                val.write(u32, @intCast(@intFromPtr(heap_data)));
                val.offset(4).write(u32, @intCast(bytes.len));
                val.offset(8).write(u32, @intCast(bytes.len));
            }
        }
        return val;
    }

    /// Read the bytes from a RocStr value.
    fn readRocStr(self: *LirInterpreter, val: Value) []const u8 {
        const target_usize = self.layout_store.targetUsize();
        const ptr_size = target_usize.size();

        if (ptr_size == 8) {
            const last_byte = val.ptr[23];
            if (last_byte & 0x80 != 0) {
                // Small string
                const len = last_byte & 0x7F;
                return val.ptr[0..len];
            } else {
                const str_ptr = val.read(usize);
                const len = val.offset(8).read(usize);
                if (str_ptr == 0 or len == 0) return "";
                const p: [*]const u8 = @ptrFromInt(str_ptr);
                return p[0..len];
            }
        } else {
            const last_byte = val.ptr[11];
            if (last_byte & 0x80 != 0) {
                const len = last_byte & 0x7F;
                return val.ptr[0..len];
            } else {
                const str_ptr = val.read(u32);
                const len = val.offset(4).read(u32);
                if (str_ptr == 0 or len == 0) return "";
                const p: [*]const u8 = @ptrFromInt(str_ptr);
                return p[0..len];
            }
        }
    }

    // ──────────────────────────────────────────────────────────────
    // Lookup
    // ──────────────────────────────────────────────────────────────

    fn evalLookup(self: *LirInterpreter, symbol: Symbol, layout_idx: layout_mod.Idx) Error!Value {
        // Check local bindings first
        if (self.bindings.get(symbol.raw())) |binding| {
            return binding.val;
        }

        // Check top-level cache
        if (self.top_level_cache.get(symbol.raw())) |binding| {
            return binding.val;
        }

        // Try evaluating as a top-level def
        if (self.store.getSymbolDef(symbol)) |def_expr_id| {
            // Cycle detection
            if (self.evaluating.contains(symbol.raw())) {
                return error.RuntimeError;
            }
            self.evaluating.put(symbol.raw(), {}) catch return error.OutOfMemory;
            defer _ = self.evaluating.remove(symbol.raw());

            const result = try self.eval(def_expr_id);
            const val = switch (result) {
                .value => |v| v,
                else => return error.RuntimeError,
            };

            const size = self.helper.sizeOf(layout_idx);
            self.top_level_cache.put(symbol.raw(), .{ .val = val, .size = size }) catch return error.OutOfMemory;
            return val;
        }

        return error.RuntimeError;
    }

    fn evalCellLoad(self: *LirInterpreter, symbol: Symbol, layout_idx: layout_mod.Idx) Error!Value {
        if (self.cells.get(symbol.raw())) |binding| {
            // Copy the cell's current value
            const size = self.helper.sizeOf(layout_idx);
            const copy = try self.allocBytes(size);
            copy.copyFrom(binding.val, size);
            return copy;
        }
        return error.RuntimeError;
    }

    // ──────────────────────────────────────────────────────────────
    // Blocks and statements
    // ──────────────────────────────────────────────────────────────

    fn evalBlock(self: *LirInterpreter, b: anytype) Error!EvalResult {
        // Execute statements
        const stmts = self.store.getStmts(b.stmts);
        for (stmts) |stmt| {
            switch (stmt) {
                .decl, .mutate => |binding| {
                    const result = try self.eval(binding.expr);
                    switch (result) {
                        .value => |val| try self.bindPattern(binding.pattern, val),
                        .early_return => return result,
                        .break_expr => return result,
                    }
                },
                .cell_init => |cb| {
                    const result = try self.eval(cb.expr);
                    const val = switch (result) {
                        .value => |v| v,
                        .early_return => return result,
                        .break_expr => return result,
                    };
                    const size = self.helper.sizeOf(cb.layout_idx);
                    self.cells.put(cb.cell.raw(), .{ .val = val, .size = size }) catch return error.OutOfMemory;
                },
                .cell_store => |cb| {
                    const result = try self.eval(cb.expr);
                    const val = switch (result) {
                        .value => |v| v,
                        .early_return => return result,
                        .break_expr => return result,
                    };
                    const size = self.helper.sizeOf(cb.layout_idx);
                    // Update the cell value
                    if (self.cells.getPtr(cb.cell.raw())) |entry| {
                        entry.val = val;
                        entry.size = size;
                    } else {
                        self.cells.put(cb.cell.raw(), .{ .val = val, .size = size }) catch return error.OutOfMemory;
                    }
                },
                .cell_drop => |cd| {
                    _ = cd;
                    // No-op for now (arena handles cleanup)
                },
            }
        }

        // Evaluate final expression
        return self.eval(b.final_expr);
    }

    // ──────────────────────────────────────────────────────────────
    // Pattern binding
    // ──────────────────────────────────────────────────────────────

    fn bindPattern(self: *LirInterpreter, pattern_id: LirPatternId, val: Value) Error!void {
        const pat = self.store.getPattern(pattern_id);
        switch (pat) {
            .bind => |b| {
                const size = self.helper.sizeOf(b.layout_idx);
                self.bindings.put(b.symbol.raw(), .{ .val = val, .size = size }) catch return error.OutOfMemory;
            },
            .wildcard => {}, // Nothing to bind
            .struct_ => |s| {
                const fields = self.store.getPatternSpan(s.fields);
                for (fields, 0..) |field_pat_id, i| {
                    const field_offset = self.helper.structFieldOffset(s.struct_layout, @intCast(i));
                    const field_val = val.offset(field_offset);
                    try self.bindPattern(field_pat_id, field_val);
                }
            },
            .tag => |t| {
                // Payload is at offset 0 in the tag union
                const args = self.store.getPatternSpan(t.args);
                if (args.len == 1) {
                    try self.bindPattern(args[0], val);
                } else if (args.len > 1) {
                    // Multiple args = struct-like payload
                    const payload_layout = self.tagPayloadLayout(t.union_layout, t.discriminant);
                    for (args, 0..) |arg_pat_id, i| {
                        const field_offset = self.helper.structFieldOffset(payload_layout, @intCast(i));
                        try self.bindPattern(arg_pat_id, val.offset(field_offset));
                    }
                }
            },
            .as_pattern => |ap| {
                // Bind the name
                const size = self.helper.sizeOf(ap.layout_idx);
                self.bindings.put(ap.symbol.raw(), .{ .val = val, .size = size }) catch return error.OutOfMemory;
                // Also bind the inner pattern
                try self.bindPattern(ap.inner, val);
            },
            .int_literal, .float_literal, .str_literal => {}, // Literal patterns don't bind
            .list => {}, // TODO: list pattern destructuring
        }
    }

    /// Check if a value matches a pattern.
    fn matchPattern(self: *LirInterpreter, pattern_id: LirPatternId, val: Value) Error!bool {
        const pat = self.store.getPattern(pattern_id);
        return switch (pat) {
            .bind, .wildcard, .as_pattern => true,
            .int_literal => |lit| blk: {
                const size = self.helper.sizeOf(lit.layout_idx);
                break :blk switch (size) {
                    1 => val.read(i8) == @as(i8, @intCast(lit.value)),
                    2 => val.read(i16) == @as(i16, @intCast(lit.value)),
                    4 => val.read(i32) == @as(i32, @intCast(lit.value)),
                    8 => val.read(i64) == @as(i64, @intCast(lit.value)),
                    16 => val.read(i128) == lit.value,
                    else => false,
                };
            },
            .float_literal => |lit| val.read(f64) == lit.value,
            .str_literal => |idx| blk: {
                const expected = self.store.getString(idx);
                const actual = self.readRocStr(val);
                break :blk std.mem.eql(u8, actual, expected);
            },
            .tag => |t| blk: {
                const disc = self.helper.readTagDiscriminant(val, t.union_layout);
                if (disc != t.discriminant) break :blk false;
                // Check payload patterns
                const args = self.store.getPatternSpan(t.args);
                if (args.len == 1) {
                    if (!try self.matchPattern(args[0], val)) break :blk false;
                } else if (args.len > 1) {
                    const payload_layout = self.tagPayloadLayout(t.union_layout, t.discriminant);
                    for (args, 0..) |arg_pat_id, i| {
                        const field_offset = self.helper.structFieldOffset(payload_layout, @intCast(i));
                        if (!try self.matchPattern(arg_pat_id, val.offset(field_offset))) break :blk false;
                    }
                }
                break :blk true;
            },
            .struct_ => |s| blk: {
                const fields = self.store.getPatternSpan(s.fields);
                for (fields, 0..) |field_pat_id, i| {
                    const field_offset = self.helper.structFieldOffset(s.struct_layout, @intCast(i));
                    const field_val = val.offset(field_offset);
                    if (!try self.matchPattern(field_pat_id, field_val)) break :blk false;
                }
                break :blk true;
            },
            .list => true, // TODO: list pattern matching
        };
    }

    // ──────────────────────────────────────────────────────────────
    // Aggregates
    // ──────────────────────────────────────────────────────────────

    fn evalStruct(self: *LirInterpreter, s: anytype) Error!Value {
        const val = try self.alloc(s.struct_layout);
        const field_exprs = self.store.getExprSpan(s.fields);
        for (field_exprs, 0..) |field_expr_id, i| {
            const field_offset = self.helper.structFieldOffset(s.struct_layout, @intCast(i));
            const field_result = try self.eval(field_expr_id);
            const field_val = switch (field_result) {
                .value => |v| v,
                else => return error.RuntimeError,
            };
            const field_layout = self.fieldLayoutOf(s.struct_layout, @intCast(i));
            const field_size = self.helper.sizeOf(field_layout);
            if (field_size > 0) {
                val.offset(field_offset).copyFrom(field_val, field_size);
            }
        }
        return val;
    }

    fn evalStructAccess(self: *LirInterpreter, sa: anytype) Error!Value {
        const struct_val = try self.evalValue(sa.struct_expr);
        const field_offset = self.helper.structFieldOffset(sa.struct_layout, sa.field_idx);
        return struct_val.offset(field_offset);
    }

    fn evalZeroArgTag(self: *LirInterpreter, z: anytype) Error!Value {
        const val = try self.alloc(z.union_layout);
        self.helper.writeTagDiscriminant(val, z.union_layout, z.discriminant);
        return val;
    }

    fn evalTag(self: *LirInterpreter, t: anytype) Error!Value {
        const val = try self.alloc(t.union_layout);
        self.helper.writeTagDiscriminant(val, t.union_layout, t.discriminant);

        // Write payload at offset 0
        const arg_exprs = self.store.getExprSpan(t.args);
        if (arg_exprs.len == 1) {
            const arg_result = try self.eval(arg_exprs[0]);
            const arg_val = switch (arg_result) {
                .value => |v| v,
                else => return error.RuntimeError,
            };
            const payload_layout = self.tagPayloadLayout(t.union_layout, t.discriminant);
            const payload_size = self.helper.sizeOf(payload_layout);
            if (payload_size > 0) {
                val.copyFrom(arg_val, payload_size);
            }
        } else if (arg_exprs.len > 1) {
            // Multiple args form a struct-like payload
            const payload_layout = self.tagPayloadLayout(t.union_layout, t.discriminant);
            for (arg_exprs, 0..) |arg_expr_id, i| {
                const arg_result = try self.eval(arg_expr_id);
                const arg_val = switch (arg_result) {
                    .value => |v| v,
                    else => return error.RuntimeError,
                };
                const field_layout_idx = self.fieldLayoutOf(payload_layout, @intCast(i));
                const field_size = self.helper.sizeOf(field_layout_idx);
                const field_offset = self.helper.structFieldOffset(payload_layout, @intCast(i));
                if (field_size > 0) {
                    val.offset(field_offset).copyFrom(arg_val, field_size);
                }
            }
        }
        return val;
    }

    fn evalEmptyList(self: *LirInterpreter, l: anytype) Error!Value {
        // RocList with all zeros = empty list
        return self.alloc(l.list_layout);
    }

    fn evalList(self: *LirInterpreter, l: anytype) Error!Value {
        const elem_exprs = self.store.getExprSpan(l.elems);
        const elem_size = self.helper.sizeOf(l.elem_layout);
        const count = elem_exprs.len;

        // Allocate the RocList header
        const val = try self.alloc(l.list_layout);

        if (count == 0) return val;

        // ZST lists need no element storage, but must record the length.
        if (elem_size == 0) {
            const target_usize = self.layout_store.targetUsize();
            if (target_usize.size() == 8) {
                val.offset(8).write(usize, count);
            } else {
                val.offset(4).write(u32, @intCast(count));
            }
            return val;
        }

        // Allocate element storage through roc_ops so builtins can safely
        // call isUnique()/decref() on the data pointer.
        const total_elem_bytes = elem_size * count;
        const sa = self.helper.sizeAlignOf(l.elem_layout);
        const elem_alignment: u32 = @intCast(sa.alignment.toByteUnits());
        const elem_data = try self.allocRocData(total_elem_bytes, elem_alignment);
        const elem_mem = elem_data[0..total_elem_bytes];
        @memset(elem_mem, 0);

        // Evaluate each element
        for (elem_exprs, 0..) |elem_expr_id, i| {
            const elem_result = try self.eval(elem_expr_id);
            const elem_val = switch (elem_result) {
                .value => |v| v,
                else => return error.RuntimeError,
            };
            const dest_offset = i * elem_size;
            @memcpy(elem_mem[dest_offset..][0..elem_size], elem_val.ptr[0..elem_size]);
        }

        // Write the RocList fields
        const target_usize = self.layout_store.targetUsize();
        const ptr_size = target_usize.size();
        if (ptr_size == 8) {
            val.write(usize, @intFromPtr(elem_mem.ptr)); // bytes ptr
            val.offset(8).write(usize, count); // length
            val.offset(16).write(usize, count); // capacity
        } else {
            val.write(u32, @intCast(@intFromPtr(elem_mem.ptr)));
            val.offset(4).write(u32, @intCast(count));
            val.offset(8).write(u32, @intCast(count));
        }

        return val;
    }

    // ──────────────────────────────────────────────────────────────
    // Control flow
    // ──────────────────────────────────────────────────────────────

    fn evalIfThenElse(self: *LirInterpreter, ite: anytype) Error!EvalResult {
        const branches = self.store.getIfBranches(ite.branches);
        for (branches) |branch| {
            const cond_result = try self.eval(branch.cond);
            const cond_val = switch (cond_result) {
                .value => |v| v,
                else => return cond_result,
            };
            if (cond_val.read(u8) != 0) {
                return self.eval(branch.body);
            }
        }
        return self.eval(ite.final_else);
    }

    fn evalMatch(self: *LirInterpreter, m: anytype) Error!EvalResult {
        const match_val = try self.evalValue(m.value);
        const branches = self.store.getMatchBranches(m.branches);
        for (branches) |branch| {
            if (try self.matchPattern(branch.pattern, match_val)) {
                try self.bindPattern(branch.pattern, match_val);
                // Check guard if present
                if (!branch.guard.isNone()) {
                    const guard_val = try self.evalValue(branch.guard);
                    if (guard_val.read(u8) == 0) continue;
                }
                return self.eval(branch.body);
            }
        }
        return error.RuntimeError; // No branch matched
    }

    fn evalDiscriminantSwitch(self: *LirInterpreter, ds: anytype) Error!EvalResult {
        const switch_val = try self.evalValue(ds.value);
        const disc = self.helper.readTagDiscriminant(switch_val, ds.union_layout);
        const branches = self.store.getExprSpan(ds.branches);
        if (disc < branches.len) {
            return self.eval(branches[disc]);
        }
        return error.RuntimeError;
    }

    fn evalTagPayloadAccess(self: *LirInterpreter, tpa: anytype) Error!Value {
        const val = try self.evalValue(tpa.value);
        // Payload is always at offset 0 in the tag union
        return val;
    }

    fn evalEarlyReturn(self: *LirInterpreter, er: anytype) Error!EvalResult {
        const val = try self.evalValue(er.expr);
        return .{ .early_return = val };
    }

    fn evalForLoop(self: *LirInterpreter, fl: anytype) Error!EvalResult {
        const list_val = try self.evalValue(fl.list_expr);
        const elem_size = self.helper.sizeOf(fl.elem_layout);
        const target_usize = self.layout_store.targetUsize();
        const ptr_size = target_usize.size();

        // Read list length and data pointer
        var data_ptr: usize = 0;
        var count: usize = 0;
        if (ptr_size == 8) {
            data_ptr = list_val.read(usize);
            count = list_val.offset(8).read(usize);
        } else {
            data_ptr = list_val.read(u32);
            count = list_val.offset(4).read(u32);
        }

        if (count == 0) return .{ .value = Value.zst };

        const data: [*]u8 = if (data_ptr != 0) @ptrFromInt(data_ptr) else undefined;
        var i: usize = 0;
        while (i < count) : (i += 1) {
            const elem_val = if (elem_size > 0)
                Value{ .ptr = data + i * elem_size }
            else
                Value.zst;
            try self.bindPattern(fl.elem_pattern, elem_val);
            const body_result = try self.eval(fl.body);
            switch (body_result) {
                .value => {},
                .break_expr => break,
                .early_return => return body_result,
            }
        }
        return .{ .value = Value.zst };
    }

    fn evalWhileLoop(self: *LirInterpreter, wl: anytype) Error!EvalResult {
        while (true) {
            const cond_val = try self.evalValue(wl.cond);
            if (cond_val.read(u8) == 0) break;
            const body_result = try self.eval(wl.body);
            switch (body_result) {
                .value => {},
                .break_expr => break,
                .early_return => return body_result,
            }
        }
        return .{ .value = Value.zst };
    }

    // ──────────────────────────────────────────────────────────────
    // Function calls
    // ──────────────────────────────────────────────────────────────

    fn evalCall(self: *LirInterpreter, c: anytype) Error!EvalResult {
        // Evaluate arguments
        const arg_exprs = self.store.getExprSpan(c.args);
        var args = std.array_list.AlignedManaged(Value, null).init(self.allocator);
        defer args.deinit();
        for (arg_exprs) |arg_expr_id| {
            const arg_result = try self.eval(arg_expr_id);
            const arg_val = switch (arg_result) {
                .value => |v| v,
                else => return arg_result,
            };
            args.append(arg_val) catch return error.OutOfMemory;
        }

        // Resolve the function
        const fn_expr = self.store.getExpr(c.fn_expr);
        switch (fn_expr) {
            .lookup => |l| {
                // Direct call to a named function
                if (self.store.getSymbolDef(l.symbol)) |def_expr_id| {
                    const def_expr = self.store.getExpr(def_expr_id);
                    if (def_expr == .lambda) {
                        return self.callLambda(def_expr.lambda, args.items);
                    }
                }
                // Try calling through the evaluated value
                const fn_val = try self.evalLookup(l.symbol, l.layout_idx);
                _ = fn_val;
                return error.RuntimeError;
            },
            .lambda => |lambda| {
                return self.callLambda(lambda, args.items);
            },
            else => {
                // Evaluate the function expression and try to call it
                return error.RuntimeError;
            },
        }
    }

    fn callLambda(self: *LirInterpreter, lambda: anytype, args: []const Value) Error!EvalResult {
        const params = self.store.getPatternSpan(lambda.params);

        // Save current bindings
        const saved_bindings = self.bindings.clone() catch return error.OutOfMemory;
        defer {
            self.bindings.deinit();
            self.bindings = saved_bindings;
        }

        // Bind parameters
        const param_count = @min(params.len, args.len);
        for (0..param_count) |i| {
            try self.bindPattern(params[i], args[i]);
        }

        // Evaluate body
        const result = try self.eval(lambda.body);
        return switch (result) {
            .early_return => |v| .{ .value = v },
            else => result,
        };
    }

    fn evalLambda(self: *LirInterpreter, lambda: anytype, expr_id: LirExprId) Error!Value {
        // Lambda as a value — store a reference to the expression.
        // The actual call will resolve via the expression ID.
        _ = lambda;
        // Return a value that encodes the expr_id for later dispatch
        const val = try self.allocBytes(8);
        val.write(u32, @intFromEnum(expr_id));
        return val;
    }

    // ──────────────────────────────────────────────────────────────
    // Crash / dbg / expect
    // ──────────────────────────────────────────────────────────────

    fn evalCrash(self: *LirInterpreter, c: anytype) Error!EvalResult {
        _ = self;
        _ = c;
        return error.Crash;
    }

    fn evalDbg(self: *LirInterpreter, d: anytype) Error!EvalResult {
        // Evaluate the expression and return it (debug printing is a TODO)
        return self.eval(d.expr);
    }

    fn evalExpect(self: *LirInterpreter, e: anytype) Error!EvalResult {
        const cond_val = try self.evalValue(e.cond);
        if (cond_val.read(u8) == 0) {
            // Expect failed — evaluate body for side effects but don't crash
            _ = try self.eval(e.body);
        }
        return .{ .value = Value.zst };
    }

    // ──────────────────────────────────────────────────────────────
    // Low-level operations — direct builtin dispatch
    // ──────────────────────────────────────────────────────────────

    /// Resolve the result layout of a LIR expression.
    fn exprLayout(self: *LirInterpreter, expr_id: LirExprId) layout_mod.Idx {
        return lir_program_mod.lirExprResultLayout(self.store, expr_id);
    }

    // ── Value ↔ RocStr/RocList marshaling ──

    fn valueToRocStr(val: Value) RocStr {
        var rs: RocStr = undefined;
        @memcpy(std.mem.asBytes(&rs), val.ptr[0..@sizeOf(RocStr)]);
        return rs;
    }

    fn rocStrToValue(self: *LirInterpreter, rs: RocStr, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        @memcpy(val.ptr[0..@sizeOf(RocStr)], std.mem.asBytes(&rs));
        return val;
    }

    fn valueToRocList(val: Value) RocList {
        var rl: RocList = undefined;
        @memcpy(std.mem.asBytes(&rl), val.ptr[0..@sizeOf(RocList)]);
        return rl;
    }

    fn rocListToValue(self: *LirInterpreter, rl: RocList, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        @memcpy(val.ptr[0..@sizeOf(RocList)], std.mem.asBytes(&rl));
        return val;
    }

    const ListElemInfo = struct { alignment: u32, width: usize, rc: bool };

    fn listElemInfo(self: *LirInterpreter, list_layout: layout_mod.Idx) ListElemInfo {
        const l = self.layout_store.getLayout(list_layout);
        if (l.tag == .list) {
            const elem_idx = l.data.list;
            const sa = self.helper.sizeAlignOf(elem_idx);
            return .{
                .alignment = @intCast(sa.alignment.toByteUnits()),
                .width = sa.size,
                .rc = self.helper.containsRefcounted(elem_idx),
            };
        }
        return .{ .alignment = 1, .width = 0, .rc = false };
    }

    fn listElemLayout(self: *LirInterpreter, list_layout: layout_mod.Idx) layout_mod.Idx {
        const l = self.layout_store.getLayout(list_layout);
        if (l.tag == .list) return l.data.list;
        return .zst;
    }

    // ── Builtin call with crash recovery ──

    fn callBuiltinStr1(self: *LirInterpreter, comptime func: anytype, a: RocStr, ret_layout: layout_mod.Idx) Error!Value {
        self.roc_env.resetCrash();
        const sj = setjmp(&self.roc_env.jmp_buf);
        if (sj != 0) return error.Crash;
        const result = func(a, &self.roc_ops);
        return self.rocStrToValue(result, ret_layout);
    }

    fn callBuiltinStr2(self: *LirInterpreter, comptime func: anytype, a: RocStr, b: RocStr, ret_layout: layout_mod.Idx) Error!Value {
        self.roc_env.resetCrash();
        const sj = setjmp(&self.roc_env.jmp_buf);
        if (sj != 0) return error.Crash;
        const result = func(a, b, &self.roc_ops);
        return self.rocStrToValue(result, ret_layout);
    }

    fn evalLowLevel(self: *LirInterpreter, ll: anytype) Error!Value {
        const arg_exprs = self.store.getExprSpan(ll.args);
        var args: [8]Value = undefined;
        const n = @min(arg_exprs.len, 8);
        for (0..n) |i| {
            args[i] = try self.evalValue(arg_exprs[i]);
        }

        // Determine argument layout for numeric ops (operand type, not return type)
        const arg_layout: layout_mod.Idx = if (arg_exprs.len > 0)
            self.exprLayout(arg_exprs[0])
        else
            ll.ret_layout;

        return switch (ll.op) {
            // ── String ops ──
            .str_is_eq => blk: {
                const result = builtins.str.strEqual(valueToRocStr(args[0]), valueToRocStr(args[1]));
                const val = try self.alloc(ll.ret_layout);
                val.write(u8, if (result) 1 else 0);
                break :blk val;
            },
            .str_concat => self.callBuiltinStr2(builtins.str.strConcatC, valueToRocStr(args[0]), valueToRocStr(args[1]), ll.ret_layout),
            .str_contains => blk: {
                const result = builtins.str.strContains(valueToRocStr(args[0]), valueToRocStr(args[1]));
                const val = try self.alloc(ll.ret_layout);
                val.write(u8, if (result) 1 else 0);
                break :blk val;
            },
            .str_starts_with => blk: {
                const result = builtins.str.startsWith(valueToRocStr(args[0]), valueToRocStr(args[1]));
                const val = try self.alloc(ll.ret_layout);
                val.write(u8, if (result) 1 else 0);
                break :blk val;
            },
            .str_ends_with => blk: {
                const result = builtins.str.endsWith(valueToRocStr(args[0]), valueToRocStr(args[1]));
                const val = try self.alloc(ll.ret_layout);
                val.write(u8, if (result) 1 else 0);
                break :blk val;
            },
            .str_trim => self.callBuiltinStr1(builtins.str.strTrim, valueToRocStr(args[0]), ll.ret_layout),
            .str_trim_start => self.callBuiltinStr1(builtins.str.strTrimStart, valueToRocStr(args[0]), ll.ret_layout),
            .str_trim_end => self.callBuiltinStr1(builtins.str.strTrimEnd, valueToRocStr(args[0]), ll.ret_layout),
            .str_with_ascii_lowercased => self.callBuiltinStr1(builtins.str.strWithAsciiLowercased, valueToRocStr(args[0]), ll.ret_layout),
            .str_with_ascii_uppercased => self.callBuiltinStr1(builtins.str.strWithAsciiUppercased, valueToRocStr(args[0]), ll.ret_layout),
            .str_caseless_ascii_equals => blk: {
                const result = builtins.str.strCaselessAsciiEquals(valueToRocStr(args[0]), valueToRocStr(args[1]));
                const val = try self.alloc(ll.ret_layout);
                val.write(u8, if (result) 1 else 0);
                break :blk val;
            },
            .str_repeat => blk: {
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.str.repeatC(valueToRocStr(args[0]), args[1].read(u64), &self.roc_ops);
                break :blk self.rocStrToValue(result, ll.ret_layout);
            },
            .str_drop_prefix => self.callBuiltinStr2(builtins.str.strDropPrefix, valueToRocStr(args[0]), valueToRocStr(args[1]), ll.ret_layout),
            .str_drop_suffix => self.callBuiltinStr2(builtins.str.strDropSuffix, valueToRocStr(args[0]), valueToRocStr(args[1]), ll.ret_layout),
            .str_count_utf8_bytes => blk: {
                const result = builtins.str.countUtf8Bytes(valueToRocStr(args[0]));
                const val = try self.alloc(ll.ret_layout);
                val.write(u64, result);
                break :blk val;
            },
            .str_to_utf8 => blk: {
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.str.strToUtf8C(valueToRocStr(args[0]), &self.roc_ops);
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .str_from_utf8 => blk: {
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.str.fromUtf8C(valueToRocList(args[0]), UpdateMode.Immutable, &self.roc_ops);
                // FromUtf8Try is { byte_index: u64, string: RocStr, is_ok: bool, problem_code: u8 }
                const val = try self.alloc(ll.ret_layout);
                @memcpy(val.ptr[0..@sizeOf(builtins.str.FromUtf8Try)], std.mem.asBytes(&result));
                break :blk val;
            },
            .str_from_utf8_lossy => blk: {
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.str.fromUtf8Lossy(valueToRocList(args[0]), &self.roc_ops);
                break :blk self.rocStrToValue(result, ll.ret_layout);
            },
            .str_split_on => blk: {
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.str.strSplitOn(valueToRocStr(args[0]), valueToRocStr(args[1]), &self.roc_ops);
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .str_join_with => self.evalStrJoinWith(args[0], args[1], ll.ret_layout),
            .str_with_capacity => blk: {
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.str.withCapacityC(args[0].read(u64), &self.roc_ops);
                break :blk self.rocStrToValue(result, ll.ret_layout);
            },
            .str_reserve => blk: {
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.str.reserveC(valueToRocStr(args[0]), args[1].read(u64), &self.roc_ops);
                break :blk self.rocStrToValue(result, ll.ret_layout);
            },
            .str_release_excess_capacity => blk: {
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.str.strReleaseExcessCapacity(&self.roc_ops, valueToRocStr(args[0]));
                break :blk self.rocStrToValue(result, ll.ret_layout);
            },
            .str_inspekt => blk: {
                // str_inspekt is identity on strings (already formatted)
                break :blk args[0];
            },

            // ── Numeric to_str ops ──
            .u8_to_str => self.numToStr(u8, args[0], ll.ret_layout),
            .i8_to_str => self.numToStr(i8, args[0], ll.ret_layout),
            .u16_to_str => self.numToStr(u16, args[0], ll.ret_layout),
            .i16_to_str => self.numToStr(i16, args[0], ll.ret_layout),
            .u32_to_str => self.numToStr(u32, args[0], ll.ret_layout),
            .i32_to_str => self.numToStr(i32, args[0], ll.ret_layout),
            .u64_to_str => self.numToStr(u64, args[0], ll.ret_layout),
            .i64_to_str => self.numToStr(i64, args[0], ll.ret_layout),
            .u128_to_str => self.numToStr(u128, args[0], ll.ret_layout),
            .i128_to_str => self.numToStr(i128, args[0], ll.ret_layout),
            .dec_to_str => blk: {
                const dec = RocDec{ .num = args[0].read(i128) };
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.dec.to_str(dec, &self.roc_ops);
                break :blk self.rocStrToValue(result, ll.ret_layout);
            },
            .f32_to_str => blk: {
                var buf: [400]u8 = undefined;
                const slice = i128h.f64_to_str(&buf, @as(f64, args[0].read(f32)));
                break :blk self.makeRocStr(slice);
            },
            .f64_to_str => blk: {
                var buf: [400]u8 = undefined;
                const slice = i128h.f64_to_str(&buf, args[0].read(f64));
                break :blk self.makeRocStr(slice);
            },
            .num_to_str => blk: {
                // Generic num_to_str uses arg layout to determine type
                const size = self.helper.sizeOf(arg_layout);
                const l = self.layout_store.getLayout(arg_layout);
                const is_float = l.tag == .scalar and l.data.scalar.tag == .frac;
                if (isDec(arg_layout)) {
                    const dec = RocDec{ .num = args[0].read(i128) };
                    self.roc_env.resetCrash();
                    const sj = setjmp(&self.roc_env.jmp_buf);
                    if (sj != 0) return error.Crash;
                    const result = builtins.dec.to_str(dec, &self.roc_ops);
                    break :blk self.rocStrToValue(result, ll.ret_layout);
                } else if (is_float) {
                    var buf: [400]u8 = undefined;
                    const slice = switch (size) {
                        4 => i128h.f64_to_str(&buf, @as(f64, args[0].read(f32))),
                        else => i128h.f64_to_str(&buf, args[0].read(f64)),
                    };
                    break :blk self.makeRocStr(slice);
                } else {
                    break :blk self.numToStrByLayout(args[0], arg_layout, ll.ret_layout);
                }
            },

            // ── List ops ──
            .list_len => blk: {
                const rl = valueToRocList(args[0]);
                const val = try self.alloc(ll.ret_layout);
                val.write(u64, @intCast(rl.len()));
                break :blk val;
            },
            .list_get_unsafe => blk: {
                const rl = valueToRocList(args[0]);
                const idx = args[1].read(u64);
                const info = self.listElemInfo(arg_layout);
                if (info.width == 0 or rl.bytes == null) break :blk try self.alloc(ll.ret_layout);
                const elem_ptr = rl.bytes.? + idx * info.width;
                const val = try self.allocBytes(info.width);
                @memcpy(val.ptr[0..info.width], elem_ptr[0..info.width]);
                break :blk val;
            },
            .list_append_unsafe => blk: {
                // The Roc List.append function emits list_append_unsafe directly.
                // Use the safe listAppend which reserves capacity first,
                // matching the dev codegen (LirCodeGen) behavior.
                const info = self.listElemInfo(arg_layout);
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.list.listAppend(
                    valueToRocList(args[0]),
                    info.alignment,
                    @ptrCast(args[1].ptr),
                    info.width,
                    false,
                    null,
                    &builtins.utils.rcNone,
                    .InPlace,
                    &builtins.list.copy_fallback,
                    &self.roc_ops,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_concat => blk: {
                const info = self.listElemInfo(arg_layout);
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.list.listConcat(
                    valueToRocList(args[0]),
                    valueToRocList(args[1]),
                    info.alignment,
                    info.width,
                    false, // no RC in interpreter
                    null,
                    &builtins.utils.rcNone,
                    null,
                    &builtins.utils.rcNone,
                    &self.roc_ops,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_prepend => blk: {
                const info = self.listElemInfo(arg_layout);
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const copy_fn: *const fn (?[*]u8, ?[*]u8) callconv(.c) void = &(struct {
                    fn f(_: ?[*]u8, _: ?[*]u8) callconv(.c) void {}
                }).f;
                const result = builtins.list.listPrepend(
                    valueToRocList(args[0]),
                    info.alignment,
                    @ptrCast(args[1].ptr),
                    info.width,
                    false,
                    null,
                    &builtins.utils.rcNone,
                    copy_fn,
                    &self.roc_ops,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_sublist => blk: {
                const info = self.listElemInfo(arg_layout);
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.list.listSublist(
                    valueToRocList(args[0]),
                    info.alignment,
                    info.width,
                    false,
                    args[1].read(u64),
                    args[2].read(u64),
                    null,
                    &builtins.utils.rcNone,
                    &self.roc_ops,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_drop_at => blk: {
                const info = self.listElemInfo(arg_layout);
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.list.listDropAt(
                    valueToRocList(args[0]),
                    info.alignment,
                    info.width,
                    false,
                    args[1].read(u64),
                    null,
                    &builtins.utils.rcNone,
                    null,
                    &builtins.utils.rcNone,
                    &self.roc_ops,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_set => blk: {
                const info = self.listElemInfo(arg_layout);
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const copy_fn: *const fn (?[*]u8, ?[*]u8) callconv(.c) void = &(struct {
                    fn f(_: ?[*]u8, _: ?[*]u8) callconv(.c) void {}
                }).f;
                // listReplace writes old element into out_element
                const old_elem = try self.allocBytes(info.width);
                const result = builtins.list.listReplace(
                    valueToRocList(args[0]),
                    info.alignment,
                    args[1].read(u64),
                    @ptrCast(args[2].ptr),
                    info.width,
                    false,
                    null,
                    &builtins.utils.rcNone,
                    null,
                    &builtins.utils.rcNone,
                    @ptrCast(old_elem.ptr),
                    copy_fn,
                    &self.roc_ops,
                );
                // ret_layout is a struct { list, old_element }
                const val = try self.alloc(ll.ret_layout);
                @memcpy(val.ptr[0..@sizeOf(RocList)], std.mem.asBytes(&result));
                @memcpy(val.ptr[@sizeOf(RocList)..][0..info.width], old_elem.ptr[0..info.width]);
                break :blk val;
            },
            .list_with_capacity => blk: {
                const elem_layout = self.listElemLayout(ll.ret_layout);
                const sa = self.helper.sizeAlignOf(elem_layout);
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.list.listWithCapacity(
                    args[0].read(u64),
                    @intCast(sa.alignment.toByteUnits()),
                    sa.size,
                    false,
                    null,
                    &builtins.utils.rcNone,
                    &self.roc_ops,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_reserve => blk: {
                const info = self.listElemInfo(arg_layout);
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.list.listReserve(
                    valueToRocList(args[0]),
                    info.alignment,
                    args[1].read(u64),
                    info.width,
                    false,
                    null,
                    &builtins.utils.rcNone,
                    UpdateMode.Immutable,
                    &self.roc_ops,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_release_excess_capacity => blk: {
                const info = self.listElemInfo(arg_layout);
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) return error.Crash;
                const result = builtins.list.listReleaseExcessCapacity(
                    valueToRocList(args[0]),
                    info.alignment,
                    info.width,
                    false,
                    null,
                    &builtins.utils.rcNone,
                    null,
                    &builtins.utils.rcNone,
                    UpdateMode.Immutable,
                    &self.roc_ops,
                );
                break :blk self.rocListToValue(result, ll.ret_layout);
            },
            .list_first => self.evalListFirst(args[0], arg_layout, ll.ret_layout),
            .list_last => self.evalListLast(args[0], arg_layout, ll.ret_layout),
            .list_drop_first => self.evalListDropFirst(args[0], arg_layout, ll.ret_layout),
            .list_drop_last => self.evalListDropLast(args[0], arg_layout, ll.ret_layout),
            .list_take_first => self.evalListTakeFirst(args[0], args[1], arg_layout, ll.ret_layout),
            .list_take_last => self.evalListTakeLast(args[0], args[1], arg_layout, ll.ret_layout),
            .list_contains => self.evalListContains(args[0], args[1], arg_layout, ll.ret_layout),
            .list_reverse => self.evalListReverse(args[0], arg_layout, ll.ret_layout),
            .list_sort_with => try self.alloc(ll.ret_layout), // requires callback — return unchanged
            .list_split_first => self.evalListSplitFirst(args[0], arg_layout, ll.ret_layout),
            .list_split_last => self.evalListSplitLast(args[0], arg_layout, ll.ret_layout),

            // ── Arithmetic ──
            .num_plus => self.numBinOp(args[0], args[1], ll.ret_layout, arg_layout, .add),
            .num_minus => self.numBinOp(args[0], args[1], ll.ret_layout, arg_layout, .sub),
            .num_times => self.numBinOp(args[0], args[1], ll.ret_layout, arg_layout, .mul),
            .num_div_by => self.numBinOp(args[0], args[1], ll.ret_layout, arg_layout, .div),
            .num_div_trunc_by => self.numBinOp(args[0], args[1], ll.ret_layout, arg_layout, .div_trunc),
            .num_rem_by => self.numBinOp(args[0], args[1], ll.ret_layout, arg_layout, .rem),
            .num_mod_by => self.numBinOp(args[0], args[1], ll.ret_layout, arg_layout, .mod),
            .num_negate => self.numUnaryOp(args[0], ll.ret_layout, arg_layout, .negate),
            .num_abs => self.numUnaryOp(args[0], ll.ret_layout, arg_layout, .abs),
            .num_abs_diff => self.numBinOp(args[0], args[1], ll.ret_layout, arg_layout, .abs_diff),
            .num_pow => self.evalNumPow(args[0], args[1], ll.ret_layout, arg_layout),
            .num_sqrt => self.evalNumSqrt(args[0], ll.ret_layout, arg_layout),
            .num_log => self.evalNumLog(args[0], ll.ret_layout, arg_layout),
            .num_round => self.evalNumRound(args[0], ll.ret_layout, arg_layout),
            .num_floor => self.evalNumFloor(args[0], ll.ret_layout, arg_layout),
            .num_ceiling => self.evalNumCeiling(args[0], ll.ret_layout, arg_layout),

            // ── Bitwise shifts ──
            .num_shift_left_by => self.numShiftOp(args[0], args[1], ll.ret_layout, arg_layout, .shl),
            .num_shift_right_by => self.numShiftOp(args[0], args[1], ll.ret_layout, arg_layout, .shr),
            .num_shift_right_zf_by => self.numShiftOp(args[0], args[1], ll.ret_layout, arg_layout, .shr_zf),

            // ── Comparison ──
            .num_is_eq => self.numCmpOp(args[0], args[1], arg_layout, .eq),
            .num_is_lt => self.numCmpOp(args[0], args[1], arg_layout, .lt),
            .num_is_lte => self.numCmpOp(args[0], args[1], arg_layout, .lte),
            .num_is_gt => self.numCmpOp(args[0], args[1], arg_layout, .gt),
            .num_is_gte => self.numCmpOp(args[0], args[1], arg_layout, .gte),
            .compare => self.evalCompare(args[0], args[1], arg_layout, ll.ret_layout),

            // ── Boolean ──
            .bool_not => blk: {
                const val = try self.alloc(.bool);
                val.write(u8, if (args[0].read(u8) == 0) 1 else 0);
                break :blk val;
            },

            // ── Numeric parsing ──
            .num_from_str => try self.alloc(ll.ret_layout), // complex — return default
            .num_from_numeral => args[0], // identity

            // ── Numeric conversions ──
            .u8_to_i16, .u8_to_i32, .u8_to_i64, .u8_to_i128, .u8_to_u16, .u8_to_u32, .u8_to_u64, .u8_to_u128 => self.numWiden(u8, args[0], ll.ret_layout),
            .u8_to_f32, .u8_to_f64 => self.intToFloat(u8, args[0], ll.ret_layout),
            .u8_to_dec => self.intToDec(u8, args[0], ll.ret_layout),
            .u8_to_i8_wrap => self.numTruncate(u8, i8, args[0], ll.ret_layout),
            .u8_to_i8_try => self.numTry(u8, i8, args[0], ll.ret_layout),

            .i8_to_i16, .i8_to_i32, .i8_to_i64, .i8_to_i128 => self.numWiden(i8, args[0], ll.ret_layout),
            .i8_to_u8_wrap => self.numTruncate(i8, u8, args[0], ll.ret_layout),
            .i8_to_u8_try => self.numTry(i8, u8, args[0], ll.ret_layout),
            .i8_to_u16_wrap => self.numTruncateWiden(i8, i16, u16, args[0], ll.ret_layout),
            .i8_to_u16_try => self.numTry(i8, u16, args[0], ll.ret_layout),
            .i8_to_u32_wrap => self.numTruncateWiden(i8, i32, u32, args[0], ll.ret_layout),
            .i8_to_u32_try => self.numTry(i8, u32, args[0], ll.ret_layout),
            .i8_to_u64_wrap => self.numTruncateWiden(i8, i64, u64, args[0], ll.ret_layout),
            .i8_to_u64_try => self.numTry(i8, u64, args[0], ll.ret_layout),
            .i8_to_u128_wrap => self.numTruncateWiden(i8, i128, u128, args[0], ll.ret_layout),
            .i8_to_u128_try => self.numTry(i8, u128, args[0], ll.ret_layout),
            .i8_to_f32, .i8_to_f64 => self.intToFloat(i8, args[0], ll.ret_layout),
            .i8_to_dec => self.intToDec(i8, args[0], ll.ret_layout),

            .u16_to_i32, .u16_to_i64, .u16_to_i128, .u16_to_u32, .u16_to_u64, .u16_to_u128 => self.numWiden(u16, args[0], ll.ret_layout),
            .u16_to_i8_wrap => self.numTruncate(u16, i8, args[0], ll.ret_layout),
            .u16_to_i8_try => self.numTry(u16, i8, args[0], ll.ret_layout),
            .u16_to_i16_wrap => self.numTruncate(u16, i16, args[0], ll.ret_layout),
            .u16_to_i16_try => self.numTry(u16, i16, args[0], ll.ret_layout),
            .u16_to_u8_wrap => self.numTruncate(u16, u8, args[0], ll.ret_layout),
            .u16_to_u8_try => self.numTry(u16, u8, args[0], ll.ret_layout),
            .u16_to_f32, .u16_to_f64 => self.intToFloat(u16, args[0], ll.ret_layout),
            .u16_to_dec => self.intToDec(u16, args[0], ll.ret_layout),

            .i16_to_i32, .i16_to_i64, .i16_to_i128 => self.numWiden(i16, args[0], ll.ret_layout),
            .i16_to_i8_wrap => self.numTruncate(i16, i8, args[0], ll.ret_layout),
            .i16_to_i8_try => self.numTry(i16, i8, args[0], ll.ret_layout),
            .i16_to_u8_wrap => self.numTruncate(i16, u8, args[0], ll.ret_layout),
            .i16_to_u8_try => self.numTry(i16, u8, args[0], ll.ret_layout),
            .i16_to_u16_wrap => self.numTruncate(i16, u16, args[0], ll.ret_layout),
            .i16_to_u16_try => self.numTry(i16, u16, args[0], ll.ret_layout),
            .i16_to_u32_wrap => self.numTruncateWiden(i16, i32, u32, args[0], ll.ret_layout),
            .i16_to_u32_try => self.numTry(i16, u32, args[0], ll.ret_layout),
            .i16_to_u64_wrap => self.numTruncateWiden(i16, i64, u64, args[0], ll.ret_layout),
            .i16_to_u64_try => self.numTry(i16, u64, args[0], ll.ret_layout),
            .i16_to_u128_wrap => self.numTruncateWiden(i16, i128, u128, args[0], ll.ret_layout),
            .i16_to_u128_try => self.numTry(i16, u128, args[0], ll.ret_layout),
            .i16_to_f32, .i16_to_f64 => self.intToFloat(i16, args[0], ll.ret_layout),
            .i16_to_dec => self.intToDec(i16, args[0], ll.ret_layout),

            .u32_to_i64, .u32_to_i128, .u32_to_u64, .u32_to_u128 => self.numWiden(u32, args[0], ll.ret_layout),
            .u32_to_i8_wrap => self.numTruncate(u32, i8, args[0], ll.ret_layout),
            .u32_to_i8_try => self.numTry(u32, i8, args[0], ll.ret_layout),
            .u32_to_i16_wrap => self.numTruncate(u32, i16, args[0], ll.ret_layout),
            .u32_to_i16_try => self.numTry(u32, i16, args[0], ll.ret_layout),
            .u32_to_i32_wrap => self.numTruncate(u32, i32, args[0], ll.ret_layout),
            .u32_to_i32_try => self.numTry(u32, i32, args[0], ll.ret_layout),
            .u32_to_u8_wrap => self.numTruncate(u32, u8, args[0], ll.ret_layout),
            .u32_to_u8_try => self.numTry(u32, u8, args[0], ll.ret_layout),
            .u32_to_u16_wrap => self.numTruncate(u32, u16, args[0], ll.ret_layout),
            .u32_to_u16_try => self.numTry(u32, u16, args[0], ll.ret_layout),
            .u32_to_f32, .u32_to_f64 => self.intToFloat(u32, args[0], ll.ret_layout),
            .u32_to_dec => self.intToDec(u32, args[0], ll.ret_layout),

            .i32_to_i64, .i32_to_i128 => self.numWiden(i32, args[0], ll.ret_layout),
            .i32_to_i8_wrap => self.numTruncate(i32, i8, args[0], ll.ret_layout),
            .i32_to_i8_try => self.numTry(i32, i8, args[0], ll.ret_layout),
            .i32_to_i16_wrap => self.numTruncate(i32, i16, args[0], ll.ret_layout),
            .i32_to_i16_try => self.numTry(i32, i16, args[0], ll.ret_layout),
            .i32_to_u8_wrap => self.numTruncate(i32, u8, args[0], ll.ret_layout),
            .i32_to_u8_try => self.numTry(i32, u8, args[0], ll.ret_layout),
            .i32_to_u16_wrap => self.numTruncate(i32, u16, args[0], ll.ret_layout),
            .i32_to_u16_try => self.numTry(i32, u16, args[0], ll.ret_layout),
            .i32_to_u32_wrap => self.numTruncate(i32, u32, args[0], ll.ret_layout),
            .i32_to_u32_try => self.numTry(i32, u32, args[0], ll.ret_layout),
            .i32_to_u64_wrap => self.numTruncateWiden(i32, i64, u64, args[0], ll.ret_layout),
            .i32_to_u64_try => self.numTry(i32, u64, args[0], ll.ret_layout),
            .i32_to_u128_wrap => self.numTruncateWiden(i32, i128, u128, args[0], ll.ret_layout),
            .i32_to_u128_try => self.numTry(i32, u128, args[0], ll.ret_layout),
            .i32_to_f32, .i32_to_f64 => self.intToFloat(i32, args[0], ll.ret_layout),
            .i32_to_dec => self.intToDec(i32, args[0], ll.ret_layout),

            .u64_to_i128, .u64_to_u128 => self.numWiden(u64, args[0], ll.ret_layout),
            .u64_to_i8_wrap => self.numTruncate(u64, i8, args[0], ll.ret_layout),
            .u64_to_i8_try => self.numTry(u64, i8, args[0], ll.ret_layout),
            .u64_to_i16_wrap => self.numTruncate(u64, i16, args[0], ll.ret_layout),
            .u64_to_i16_try => self.numTry(u64, i16, args[0], ll.ret_layout),
            .u64_to_i32_wrap => self.numTruncate(u64, i32, args[0], ll.ret_layout),
            .u64_to_i32_try => self.numTry(u64, i32, args[0], ll.ret_layout),
            .u64_to_i64_wrap => self.numTruncate(u64, i64, args[0], ll.ret_layout),
            .u64_to_i64_try => self.numTry(u64, i64, args[0], ll.ret_layout),
            .u64_to_u8_wrap => self.numTruncate(u64, u8, args[0], ll.ret_layout),
            .u64_to_u8_try => self.numTry(u64, u8, args[0], ll.ret_layout),
            .u64_to_u16_wrap => self.numTruncate(u64, u16, args[0], ll.ret_layout),
            .u64_to_u16_try => self.numTry(u64, u16, args[0], ll.ret_layout),
            .u64_to_u32_wrap => self.numTruncate(u64, u32, args[0], ll.ret_layout),
            .u64_to_u32_try => self.numTry(u64, u32, args[0], ll.ret_layout),
            .u64_to_f32, .u64_to_f64 => self.intToFloat(u64, args[0], ll.ret_layout),
            .u64_to_dec => self.intToDec(u64, args[0], ll.ret_layout),

            .i64_to_i128 => self.numWiden(i64, args[0], ll.ret_layout),
            .i64_to_i8_wrap => self.numTruncate(i64, i8, args[0], ll.ret_layout),
            .i64_to_i8_try => self.numTry(i64, i8, args[0], ll.ret_layout),
            .i64_to_i16_wrap => self.numTruncate(i64, i16, args[0], ll.ret_layout),
            .i64_to_i16_try => self.numTry(i64, i16, args[0], ll.ret_layout),
            .i64_to_i32_wrap => self.numTruncate(i64, i32, args[0], ll.ret_layout),
            .i64_to_i32_try => self.numTry(i64, i32, args[0], ll.ret_layout),
            .i64_to_u8_wrap => self.numTruncate(i64, u8, args[0], ll.ret_layout),
            .i64_to_u8_try => self.numTry(i64, u8, args[0], ll.ret_layout),
            .i64_to_u16_wrap => self.numTruncate(i64, u16, args[0], ll.ret_layout),
            .i64_to_u16_try => self.numTry(i64, u16, args[0], ll.ret_layout),
            .i64_to_u32_wrap => self.numTruncate(i64, u32, args[0], ll.ret_layout),
            .i64_to_u32_try => self.numTry(i64, u32, args[0], ll.ret_layout),
            .i64_to_u64_wrap => self.numTruncate(i64, u64, args[0], ll.ret_layout),
            .i64_to_u64_try => self.numTry(i64, u64, args[0], ll.ret_layout),
            .i64_to_u128_wrap => self.numTruncateWiden(i64, i128, u128, args[0], ll.ret_layout),
            .i64_to_u128_try => self.numTry(i64, u128, args[0], ll.ret_layout),
            .i64_to_f32, .i64_to_f64 => self.intToFloat(i64, args[0], ll.ret_layout),
            .i64_to_dec => self.intToDec(i64, args[0], ll.ret_layout),

            .u128_to_i8_wrap => self.numTruncate(u128, i8, args[0], ll.ret_layout),
            .u128_to_i8_try => self.numTry(u128, i8, args[0], ll.ret_layout),
            .u128_to_i16_wrap => self.numTruncate(u128, i16, args[0], ll.ret_layout),
            .u128_to_i16_try => self.numTry(u128, i16, args[0], ll.ret_layout),
            .u128_to_i32_wrap => self.numTruncate(u128, i32, args[0], ll.ret_layout),
            .u128_to_i32_try => self.numTry(u128, i32, args[0], ll.ret_layout),
            .u128_to_i64_wrap => self.numTruncate(u128, i64, args[0], ll.ret_layout),
            .u128_to_i64_try => self.numTry(u128, i64, args[0], ll.ret_layout),
            .u128_to_i128_wrap => self.numTruncate(u128, i128, args[0], ll.ret_layout),
            .u128_to_i128_try => self.numTry(u128, i128, args[0], ll.ret_layout),
            .u128_to_u8_wrap => self.numTruncate(u128, u8, args[0], ll.ret_layout),
            .u128_to_u8_try => self.numTry(u128, u8, args[0], ll.ret_layout),
            .u128_to_u16_wrap => self.numTruncate(u128, u16, args[0], ll.ret_layout),
            .u128_to_u16_try => self.numTry(u128, u16, args[0], ll.ret_layout),
            .u128_to_u32_wrap => self.numTruncate(u128, u32, args[0], ll.ret_layout),
            .u128_to_u32_try => self.numTry(u128, u32, args[0], ll.ret_layout),
            .u128_to_u64_wrap => self.numTruncate(u128, u64, args[0], ll.ret_layout),
            .u128_to_u64_try => self.numTry(u128, u64, args[0], ll.ret_layout),
            .u128_to_f32, .u128_to_f64 => self.intToFloat(u128, args[0], ll.ret_layout),
            .u128_to_dec_try_unsafe => self.intToDec(u128, args[0], ll.ret_layout),

            .i128_to_i8_wrap => self.numTruncate(i128, i8, args[0], ll.ret_layout),
            .i128_to_i8_try => self.numTry(i128, i8, args[0], ll.ret_layout),
            .i128_to_i16_wrap => self.numTruncate(i128, i16, args[0], ll.ret_layout),
            .i128_to_i16_try => self.numTry(i128, i16, args[0], ll.ret_layout),
            .i128_to_i32_wrap => self.numTruncate(i128, i32, args[0], ll.ret_layout),
            .i128_to_i32_try => self.numTry(i128, i32, args[0], ll.ret_layout),
            .i128_to_i64_wrap => self.numTruncate(i128, i64, args[0], ll.ret_layout),
            .i128_to_i64_try => self.numTry(i128, i64, args[0], ll.ret_layout),
            .i128_to_u8_wrap => self.numTruncate(i128, u8, args[0], ll.ret_layout),
            .i128_to_u8_try => self.numTry(i128, u8, args[0], ll.ret_layout),
            .i128_to_u16_wrap => self.numTruncate(i128, u16, args[0], ll.ret_layout),
            .i128_to_u16_try => self.numTry(i128, u16, args[0], ll.ret_layout),
            .i128_to_u32_wrap => self.numTruncate(i128, u32, args[0], ll.ret_layout),
            .i128_to_u32_try => self.numTry(i128, u32, args[0], ll.ret_layout),
            .i128_to_u64_wrap => self.numTruncate(i128, u64, args[0], ll.ret_layout),
            .i128_to_u64_try => self.numTry(i128, u64, args[0], ll.ret_layout),
            .i128_to_u128_wrap => self.numTruncate(i128, u128, args[0], ll.ret_layout),
            .i128_to_u128_try => self.numTry(i128, u128, args[0], ll.ret_layout),
            .i128_to_f32, .i128_to_f64 => self.intToFloat(i128, args[0], ll.ret_layout),
            .i128_to_dec_try_unsafe => self.intToDec(i128, args[0], ll.ret_layout),

            // Float → int (truncating)
            .f32_to_i8_trunc => self.floatToInt(f32, i8, args[0], ll.ret_layout),
            .f32_to_i16_trunc => self.floatToInt(f32, i16, args[0], ll.ret_layout),
            .f32_to_i32_trunc => self.floatToInt(f32, i32, args[0], ll.ret_layout),
            .f32_to_i64_trunc => self.floatToInt(f32, i64, args[0], ll.ret_layout),
            .f32_to_i128_trunc => self.floatToInt(f32, i128, args[0], ll.ret_layout),
            .f32_to_u8_trunc => self.floatToInt(f32, u8, args[0], ll.ret_layout),
            .f32_to_u16_trunc => self.floatToInt(f32, u16, args[0], ll.ret_layout),
            .f32_to_u32_trunc => self.floatToInt(f32, u32, args[0], ll.ret_layout),
            .f32_to_u64_trunc => self.floatToInt(f32, u64, args[0], ll.ret_layout),
            .f32_to_u128_trunc => self.floatToInt(f32, u128, args[0], ll.ret_layout),
            .f32_to_f64 => self.floatWiden(f32, f64, args[0], ll.ret_layout),
            // Float → int (try)
            .f32_to_i8_try_unsafe => self.floatToIntTry(f32, i8, args[0], ll.ret_layout),
            .f32_to_i16_try_unsafe => self.floatToIntTry(f32, i16, args[0], ll.ret_layout),
            .f32_to_i32_try_unsafe => self.floatToIntTry(f32, i32, args[0], ll.ret_layout),
            .f32_to_i64_try_unsafe => self.floatToIntTry(f32, i64, args[0], ll.ret_layout),
            .f32_to_i128_try_unsafe => self.floatToIntTry(f32, i128, args[0], ll.ret_layout),
            .f32_to_u8_try_unsafe => self.floatToIntTry(f32, u8, args[0], ll.ret_layout),
            .f32_to_u16_try_unsafe => self.floatToIntTry(f32, u16, args[0], ll.ret_layout),
            .f32_to_u32_try_unsafe => self.floatToIntTry(f32, u32, args[0], ll.ret_layout),
            .f32_to_u64_try_unsafe => self.floatToIntTry(f32, u64, args[0], ll.ret_layout),
            .f32_to_u128_try_unsafe => self.floatToIntTry(f32, u128, args[0], ll.ret_layout),

            .f64_to_i8_trunc => self.floatToInt(f64, i8, args[0], ll.ret_layout),
            .f64_to_i16_trunc => self.floatToInt(f64, i16, args[0], ll.ret_layout),
            .f64_to_i32_trunc => self.floatToInt(f64, i32, args[0], ll.ret_layout),
            .f64_to_i64_trunc => self.floatToInt(f64, i64, args[0], ll.ret_layout),
            .f64_to_i128_trunc => self.floatToInt(f64, i128, args[0], ll.ret_layout),
            .f64_to_u8_trunc => self.floatToInt(f64, u8, args[0], ll.ret_layout),
            .f64_to_u16_trunc => self.floatToInt(f64, u16, args[0], ll.ret_layout),
            .f64_to_u32_trunc => self.floatToInt(f64, u32, args[0], ll.ret_layout),
            .f64_to_u64_trunc => self.floatToInt(f64, u64, args[0], ll.ret_layout),
            .f64_to_u128_trunc => self.floatToInt(f64, u128, args[0], ll.ret_layout),
            .f64_to_f32_wrap => self.floatNarrow(f64, f32, args[0], ll.ret_layout),
            .f64_to_i8_try_unsafe => self.floatToIntTry(f64, i8, args[0], ll.ret_layout),
            .f64_to_i16_try_unsafe => self.floatToIntTry(f64, i16, args[0], ll.ret_layout),
            .f64_to_i32_try_unsafe => self.floatToIntTry(f64, i32, args[0], ll.ret_layout),
            .f64_to_i64_try_unsafe => self.floatToIntTry(f64, i64, args[0], ll.ret_layout),
            .f64_to_i128_try_unsafe => self.floatToIntTry(f64, i128, args[0], ll.ret_layout),
            .f64_to_u8_try_unsafe => self.floatToIntTry(f64, u8, args[0], ll.ret_layout),
            .f64_to_u16_try_unsafe => self.floatToIntTry(f64, u16, args[0], ll.ret_layout),
            .f64_to_u32_try_unsafe => self.floatToIntTry(f64, u32, args[0], ll.ret_layout),
            .f64_to_u64_try_unsafe => self.floatToIntTry(f64, u64, args[0], ll.ret_layout),
            .f64_to_u128_try_unsafe => self.floatToIntTry(f64, u128, args[0], ll.ret_layout),
            .f64_to_f32_try_unsafe => blk: {
                const sv = args[0].read(f64);
                const val = try self.alloc(ll.ret_layout);
                if (!std.math.isNan(sv) and !std.math.isInf(sv) and
                    sv <= std.math.floatMax(f32) and sv >= -std.math.floatMax(f32))
                {
                    val.write(f32, @floatCast(sv));
                    val.offset(4).write(u8, 1);
                } else {
                    val.offset(4).write(u8, 0);
                }
                break :blk val;
            },

            // Dec → numeric
            .dec_to_i8_trunc => self.decToInt(i8, args[0], ll.ret_layout),
            .dec_to_i16_trunc => self.decToInt(i16, args[0], ll.ret_layout),
            .dec_to_i32_trunc => self.decToInt(i32, args[0], ll.ret_layout),
            .dec_to_i64_trunc => self.decToInt(i64, args[0], ll.ret_layout),
            .dec_to_i128_trunc => self.decToInt(i128, args[0], ll.ret_layout),
            .dec_to_u8_trunc => self.decToInt(u8, args[0], ll.ret_layout),
            .dec_to_u16_trunc => self.decToInt(u16, args[0], ll.ret_layout),
            .dec_to_u32_trunc => self.decToInt(u32, args[0], ll.ret_layout),
            .dec_to_u64_trunc => self.decToInt(u64, args[0], ll.ret_layout),
            .dec_to_u128_trunc => self.decToInt(u128, args[0], ll.ret_layout),
            .dec_to_i8_try_unsafe => self.decToIntTry(i8, args[0], ll.ret_layout),
            .dec_to_i16_try_unsafe => self.decToIntTry(i16, args[0], ll.ret_layout),
            .dec_to_i32_try_unsafe => self.decToIntTry(i32, args[0], ll.ret_layout),
            .dec_to_i64_try_unsafe => self.decToIntTry(i64, args[0], ll.ret_layout),
            .dec_to_i128_try_unsafe => self.decToIntTry(i128, args[0], ll.ret_layout),
            .dec_to_u8_try_unsafe => self.decToIntTry(u8, args[0], ll.ret_layout),
            .dec_to_u16_try_unsafe => self.decToIntTry(u16, args[0], ll.ret_layout),
            .dec_to_u32_try_unsafe => self.decToIntTry(u32, args[0], ll.ret_layout),
            .dec_to_u64_try_unsafe => self.decToIntTry(u64, args[0], ll.ret_layout),
            .dec_to_u128_try_unsafe => self.decToIntTry(u128, args[0], ll.ret_layout),
            .dec_to_f32_wrap => blk: {
                const dec = RocDec{ .num = args[0].read(i128) };
                const val = try self.alloc(ll.ret_layout);
                val.write(f32, @floatCast(dec.toF64()));
                break :blk val;
            },
            .dec_to_f32_try_unsafe => blk: {
                const dec = RocDec{ .num = args[0].read(i128) };
                const val = try self.alloc(ll.ret_layout);
                if (builtins.dec.toF32Try(dec)) |f| {
                    val.write(f32, f);
                    val.offset(4).write(u8, 1); // is_ok
                } else {
                    val.write(f32, 0);
                    val.offset(4).write(u8, 0);
                }
                break :blk val;
            },
            .dec_to_f64 => blk: {
                const dec = RocDec{ .num = args[0].read(i128) };
                const val = try self.alloc(ll.ret_layout);
                val.write(f64, dec.toF64());
                break :blk val;
            },

            // ── Box ops ──
            .box_box, .box_unbox => args[0],

            // ── Crash ──
            .crash => return error.Crash,
        };
    }

    const NumOp = enum { add, sub, mul, div, div_trunc, rem, mod, negate, abs, abs_diff };
    const CmpOp = enum { eq, lt, lte, gt, gte };
    const ShiftOp = enum { shl, shr, shr_zf };

    /// Determine if a layout index represents a Dec type.
    fn isDec(layout_idx: layout_mod.Idx) bool {
        return layout_idx == .dec;
    }

    /// Determine if a layout index represents an unsigned integer.
    fn isUnsigned(layout_idx: layout_mod.Idx) bool {
        return switch (layout_idx) {
            .u8, .u16, .u32, .u64, .u128 => true,
            else => false,
        };
    }

    fn numBinOp(self: *LirInterpreter, a: Value, b: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx, op: NumOp) Error!Value {
        const val = try self.alloc(ret_layout);
        const size = self.helper.sizeOf(arg_layout);

        switch (size) {
            1 => {
                if (isUnsigned(arg_layout)) {
                    val.write(u8, intBinOp(u8, a.read(u8), b.read(u8), op));
                } else {
                    val.write(i8, intBinOp(i8, a.read(i8), b.read(i8), op));
                }
            },
            2 => {
                if (isUnsigned(arg_layout)) {
                    val.write(u16, intBinOp(u16, a.read(u16), b.read(u16), op));
                } else {
                    val.write(i16, intBinOp(i16, a.read(i16), b.read(i16), op));
                }
            },
            4 => {
                const l = self.layout_store.getLayout(arg_layout);
                if (l.tag == .scalar and l.data.scalar.tag == .frac) {
                    val.write(f32, floatBinOp(f32, a.read(f32), b.read(f32), op));
                } else if (isUnsigned(arg_layout)) {
                    val.write(u32, intBinOp(u32, a.read(u32), b.read(u32), op));
                } else {
                    val.write(i32, intBinOp(i32, a.read(i32), b.read(i32), op));
                }
            },
            8 => {
                const l = self.layout_store.getLayout(arg_layout);
                if (l.tag == .scalar and l.data.scalar.tag == .frac) {
                    val.write(f64, floatBinOp(f64, a.read(f64), b.read(f64), op));
                } else if (isUnsigned(arg_layout)) {
                    val.write(u64, intBinOp(u64, a.read(u64), b.read(u64), op));
                } else {
                    val.write(i64, intBinOp(i64, a.read(i64), b.read(i64), op));
                }
            },
            16 => {
                if (isDec(arg_layout)) {
                    val.write(i128, self.decBinOp(a.read(i128), b.read(i128), op));
                } else if (isUnsigned(arg_layout)) {
                    val.write(u128, intBinOp(u128, a.read(u128), b.read(u128), op));
                } else {
                    val.write(i128, intBinOp(i128, a.read(i128), b.read(i128), op));
                }
            },
            else => {},
        }
        return val;
    }

    fn numUnaryOp(self: *LirInterpreter, a: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx, op: NumOp) Error!Value {
        return self.numBinOp(a, a, ret_layout, arg_layout, op);
    }

    fn numCmpOp(self: *LirInterpreter, a: Value, b: Value, arg_layout: layout_mod.Idx, op: CmpOp) Error!Value {
        const val = try self.alloc(.bool);
        const size = self.helper.sizeOf(arg_layout);

        const result: bool = switch (size) {
            1 => if (isUnsigned(arg_layout))
                cmpOp(u8, a.read(u8), b.read(u8), op)
            else
                cmpOp(i8, a.read(i8), b.read(i8), op),
            2 => if (isUnsigned(arg_layout))
                cmpOp(u16, a.read(u16), b.read(u16), op)
            else
                cmpOp(i16, a.read(i16), b.read(i16), op),
            4 => blk: {
                const l = self.layout_store.getLayout(arg_layout);
                break :blk if (l.tag == .scalar and l.data.scalar.tag == .frac)
                    cmpOp(f32, a.read(f32), b.read(f32), op)
                else if (isUnsigned(arg_layout))
                    cmpOp(u32, a.read(u32), b.read(u32), op)
                else
                    cmpOp(i32, a.read(i32), b.read(i32), op);
            },
            8 => blk: {
                const l = self.layout_store.getLayout(arg_layout);
                break :blk if (l.tag == .scalar and l.data.scalar.tag == .frac)
                    cmpOp(f64, a.read(f64), b.read(f64), op)
                else if (isUnsigned(arg_layout))
                    cmpOp(u64, a.read(u64), b.read(u64), op)
                else
                    cmpOp(i64, a.read(i64), b.read(i64), op);
            },
            16 => if (isUnsigned(arg_layout))
                cmpOp(u128, a.read(u128), b.read(u128), op)
            else
                cmpOp(i128, a.read(i128), b.read(i128), op),
            else => false,
        };
        val.write(u8, if (result) 1 else 0);
        return val;
    }

    fn evalCompare(self: *LirInterpreter, a: Value, b: Value, arg_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const size = self.helper.sizeOf(arg_layout);
        // Returns 0=LT, 1=EQ, 2=GT
        const result: u8 = switch (size) {
            1 => if (isUnsigned(arg_layout))
                cmpOrder(u8, a.read(u8), b.read(u8))
            else
                cmpOrder(i8, a.read(i8), b.read(i8)),
            2 => if (isUnsigned(arg_layout))
                cmpOrder(u16, a.read(u16), b.read(u16))
            else
                cmpOrder(i16, a.read(i16), b.read(i16)),
            4 => blk: {
                const l = self.layout_store.getLayout(arg_layout);
                break :blk if (l.tag == .scalar and l.data.scalar.tag == .frac)
                    cmpOrder(f32, a.read(f32), b.read(f32))
                else if (isUnsigned(arg_layout))
                    cmpOrder(u32, a.read(u32), b.read(u32))
                else
                    cmpOrder(i32, a.read(i32), b.read(i32));
            },
            8 => blk: {
                const l = self.layout_store.getLayout(arg_layout);
                break :blk if (l.tag == .scalar and l.data.scalar.tag == .frac)
                    cmpOrder(f64, a.read(f64), b.read(f64))
                else if (isUnsigned(arg_layout))
                    cmpOrder(u64, a.read(u64), b.read(u64))
                else
                    cmpOrder(i64, a.read(i64), b.read(i64));
            },
            16 => if (isUnsigned(arg_layout))
                cmpOrder(u128, a.read(u128), b.read(u128))
            else
                cmpOrder(i128, a.read(i128), b.read(i128)),
            else => 1, // EQ as default
        };
        val.write(u8, result);
        return val;
    }

    fn numShiftOp(self: *LirInterpreter, a: Value, b: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx, op: ShiftOp) Error!Value {
        const val = try self.alloc(ret_layout);
        const size = self.helper.sizeOf(arg_layout);
        switch (size) {
            1 => if (isUnsigned(arg_layout))
                val.write(u8, shiftOp(u8, a.read(u8), b.read(u8), op))
            else
                val.write(i8, shiftOp(i8, a.read(i8), b.read(u8), op)),
            2 => if (isUnsigned(arg_layout))
                val.write(u16, shiftOp(u16, a.read(u16), b.read(u8), op))
            else
                val.write(i16, shiftOp(i16, a.read(i16), b.read(u8), op)),
            4 => if (isUnsigned(arg_layout))
                val.write(u32, shiftOp(u32, a.read(u32), b.read(u8), op))
            else
                val.write(i32, shiftOp(i32, a.read(i32), b.read(u8), op)),
            8 => if (isUnsigned(arg_layout))
                val.write(u64, shiftOp(u64, a.read(u64), b.read(u8), op))
            else
                val.write(i64, shiftOp(i64, a.read(i64), b.read(u8), op)),
            16 => if (isUnsigned(arg_layout))
                val.write(u128, shiftOp(u128, a.read(u128), b.read(u8), op))
            else
                val.write(i128, shiftOp(i128, a.read(i128), b.read(u8), op)),
            else => {},
        }
        return val;
    }

    fn evalNumPow(self: *LirInterpreter, a: Value, b: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const size = self.helper.sizeOf(arg_layout);
        const l = self.layout_store.getLayout(arg_layout);
        if (isDec(arg_layout)) {
            self.roc_env.resetCrash();
            const sj = setjmp(&self.roc_env.jmp_buf);
            if (sj != 0) return error.Crash;
            val.write(i128, builtins.dec.powC(RocDec{ .num = a.read(i128) }, RocDec{ .num = b.read(i128) }, &self.roc_ops));
        } else if (l.tag == .scalar and l.data.scalar.tag == .frac) {
            if (size == 4)
                val.write(f32, std.math.pow(f32, a.read(f32), b.read(f32)))
            else
                val.write(f64, std.math.pow(f64, a.read(f64), b.read(f64)));
        } else {
            // Integer power — use wrapping multiply loop
            val.write(i128, intPow(a.read(i128), b.read(i128)));
        }
        return val;
    }

    fn evalNumSqrt(self: *LirInterpreter, a: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const size = self.helper.sizeOf(arg_layout);
        if (isDec(arg_layout)) {
            // Dec sqrt: convert to f64, sqrt, convert back
            const dec = RocDec{ .num = a.read(i128) };
            const f = @sqrt(dec.toF64());
            val.write(i128, (RocDec{ .num = builtins.dec.fromF64C(f, &self.roc_ops) }).num);
        } else if (size == 4)
            val.write(f32, @sqrt(a.read(f32)))
        else
            val.write(f64, @sqrt(a.read(f64)));
        return val;
    }

    fn evalNumLog(self: *LirInterpreter, a: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const size = self.helper.sizeOf(arg_layout);
        if (isDec(arg_layout)) {
            val.write(i128, builtins.dec.logC(RocDec{ .num = a.read(i128) }));
        } else if (size == 4)
            val.write(f32, @log(a.read(f32)))
        else
            val.write(f64, @log(a.read(f64)));
        return val;
    }

    fn evalNumRound(self: *LirInterpreter, a: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const size = self.helper.sizeOf(arg_layout);
        if (isDec(arg_layout)) {
            // Dec round: divide by scale, round
            const dec = RocDec{ .num = a.read(i128) };
            const f = @round(dec.toF64());
            val.write(i128, @as(i128, @intFromFloat(f)));
        } else if (size == 4)
            val.write(i32, @as(i32, @intFromFloat(@round(a.read(f32)))))
        else
            val.write(i64, @as(i64, @intFromFloat(@round(a.read(f64)))));
        return val;
    }

    fn evalNumFloor(self: *LirInterpreter, a: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const size = self.helper.sizeOf(arg_layout);
        if (isDec(arg_layout)) {
            const dec = RocDec{ .num = a.read(i128) };
            const f = @floor(dec.toF64());
            val.write(i128, @as(i128, @intFromFloat(f)));
        } else if (size == 4)
            val.write(i32, @as(i32, @intFromFloat(@floor(a.read(f32)))))
        else
            val.write(i64, @as(i64, @intFromFloat(@floor(a.read(f64)))));
        return val;
    }

    fn evalNumCeiling(self: *LirInterpreter, a: Value, ret_layout: layout_mod.Idx, arg_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const size = self.helper.sizeOf(arg_layout);
        if (isDec(arg_layout)) {
            const dec = RocDec{ .num = a.read(i128) };
            const f = @ceil(dec.toF64());
            val.write(i128, @as(i128, @intFromFloat(f)));
        } else if (size == 4)
            val.write(i32, @as(i32, @intFromFloat(@ceil(a.read(f32)))))
        else
            val.write(i64, @as(i64, @intFromFloat(@ceil(a.read(f64)))));
        return val;
    }

    // ── Numeric conversion helpers ──

    fn numWiden(self: *LirInterpreter, comptime Src: type, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const ret_size = self.helper.sizeOf(ret_layout);
        const sv = arg.read(Src);
        switch (ret_size) {
            1 => val.write(if (@typeInfo(Src).int.signedness == .signed) i8 else u8, @intCast(sv)),
            2 => val.write(if (@typeInfo(Src).int.signedness == .signed) i16 else u16, @intCast(sv)),
            4 => val.write(if (@typeInfo(Src).int.signedness == .signed) i32 else u32, @intCast(sv)),
            8 => val.write(if (@typeInfo(Src).int.signedness == .signed) i64 else u64, @intCast(sv)),
            16 => val.write(if (@typeInfo(Src).int.signedness == .signed) i128 else u128, @intCast(sv)),
            else => {},
        }
        return val;
    }

    fn numTruncate(self: *LirInterpreter, comptime Src: type, comptime Dst: type, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const sv = arg.read(Src);
        // Truncate to same-width as Dst, then bitcast if signedness differs
        const SrcBits = @typeInfo(Src).int.bits;
        const DstBits = @typeInfo(Dst).int.bits;
        _ = SrcBits;
        const SameSigned = std.meta.Int(@typeInfo(Src).int.signedness, DstBits);
        const truncated: SameSigned = @truncate(sv);
        val.write(Dst, @bitCast(truncated));
        return val;
    }

    fn numTruncateWiden(self: *LirInterpreter, comptime Src: type, comptime Mid: type, comptime Dst: type, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const mid: Mid = @intCast(arg.read(Src));
        val.write(Dst, @bitCast(mid));
        return val;
    }

    fn numTry(self: *LirInterpreter, comptime Src: type, comptime Dst: type, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const sv = arg.read(Src);
        const dst_size = @sizeOf(Dst);
        if (std.math.cast(Dst, sv)) |dv| {
            val.write(Dst, dv);
            val.offset(dst_size).write(u8, 1); // is_ok = true
        } else {
            val.offset(dst_size).write(u8, 0); // is_ok = false
        }
        return val;
    }

    fn intToFloat(self: *LirInterpreter, comptime Src: type, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const ret_size = self.helper.sizeOf(ret_layout);
        const sv = arg.read(Src);
        if (ret_size == 4)
            val.write(f32, @floatFromInt(sv))
        else
            val.write(f64, @floatFromInt(sv));
        return val;
    }

    fn intToDec(self: *LirInterpreter, comptime Src: type, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const sv = arg.read(Src);
        const scale: i128 = 1_000_000_000_000_000_000; // 10^18
        val.write(i128, @as(i128, @intCast(sv)) *% scale);
        return val;
    }

    fn floatToInt(self: *LirInterpreter, comptime Src: type, comptime Dst: type, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const sv = arg.read(Src);
        if (std.math.isNan(sv) or std.math.isInf(sv)) {
            val.write(Dst, 0);
        } else {
            val.write(Dst, @intFromFloat(sv));
        }
        return val;
    }

    fn floatToIntTry(self: *LirInterpreter, comptime Src: type, comptime Dst: type, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const sv = arg.read(Src);
        const dst_size = @sizeOf(Dst);
        const min_val = comptime @as(Src, @floatFromInt(std.math.minInt(Dst)));
        const max_val = comptime @as(Src, @floatFromInt(std.math.maxInt(Dst)));
        if (!std.math.isNan(sv) and !std.math.isInf(sv)) {
            const truncated: Src = @trunc(sv);
            if (truncated >= min_val and truncated <= max_val) {
                val.write(Dst, @intFromFloat(truncated));
                val.offset(dst_size).write(u8, 1);
                return val;
            }
        }
        val.offset(dst_size).write(u8, 0);
        return val;
    }

    fn floatWiden(self: *LirInterpreter, comptime Src: type, comptime Dst: type, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        val.write(Dst, @as(Dst, arg.read(Src)));
        return val;
    }

    fn floatNarrow(self: *LirInterpreter, comptime Src: type, comptime Dst: type, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        val.write(Dst, @floatCast(arg.read(Src)));
        return val;
    }

    fn decToInt(self: *LirInterpreter, comptime Dst: type, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const dec = RocDec{ .num = arg.read(i128) };
        val.write(Dst, builtins.dec.toIntWrap(Dst, dec));
        return val;
    }

    fn decToIntTry(self: *LirInterpreter, comptime Dst: type, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const val = try self.alloc(ret_layout);
        const dec = RocDec{ .num = arg.read(i128) };
        const dst_size = @sizeOf(Dst);
        if (builtins.dec.toIntTry(Dst, dec)) |dv| {
            val.write(Dst, dv);
            val.offset(dst_size).write(u8, 1);
        } else {
            val.offset(dst_size).write(u8, 0);
        }
        return val;
    }

    fn numToStr(self: *LirInterpreter, comptime T: type, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const arena = self.arena.allocator();
        const formatted = std.fmt.allocPrint(arena, "{d}", .{arg.read(T)}) catch return error.OutOfMemory;
        const str_val = try self.makeRocStr(formatted);
        _ = ret_layout;
        return str_val;
    }

    fn numToStrByLayout(self: *LirInterpreter, arg: Value, arg_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const size = self.helper.sizeOf(arg_layout);
        return switch (size) {
            1 => if (isUnsigned(arg_layout)) self.numToStr(u8, arg, ret_layout) else self.numToStr(i8, arg, ret_layout),
            2 => if (isUnsigned(arg_layout)) self.numToStr(u16, arg, ret_layout) else self.numToStr(i16, arg, ret_layout),
            4 => if (isUnsigned(arg_layout)) self.numToStr(u32, arg, ret_layout) else self.numToStr(i32, arg, ret_layout),
            8 => if (isUnsigned(arg_layout)) self.numToStr(u64, arg, ret_layout) else self.numToStr(i64, arg, ret_layout),
            16 => if (isUnsigned(arg_layout)) self.numToStr(u128, arg, ret_layout) else self.numToStr(i128, arg, ret_layout),
            else => self.makeRocStr("0"),
        };
    }

    // ── List operation helpers ──

    fn evalListFirst(self: *LirInterpreter, list_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const rl = valueToRocList(list_arg);
        const info = self.listElemInfo(list_layout);
        const val = try self.alloc(ret_layout);
        if (rl.len() > 0 and rl.bytes != null and info.width > 0) {
            // Result tag union: payload at 0, discriminant after
            @memcpy(val.ptr[0..info.width], rl.bytes.?[0..info.width]);
            self.helper.writeTagDiscriminant(val, ret_layout, 1); // Ok tag
        } else {
            self.helper.writeTagDiscriminant(val, ret_layout, 0); // Err tag
        }
        return val;
    }

    fn evalListLast(self: *LirInterpreter, list_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const rl = valueToRocList(list_arg);
        const info = self.listElemInfo(list_layout);
        const val = try self.alloc(ret_layout);
        if (rl.len() > 0 and rl.bytes != null and info.width > 0) {
            const last_offset = (rl.len() - 1) * info.width;
            @memcpy(val.ptr[0..info.width], rl.bytes.?[last_offset..][0..info.width]);
            self.helper.writeTagDiscriminant(val, ret_layout, 1);
        } else {
            self.helper.writeTagDiscriminant(val, ret_layout, 0);
        }
        return val;
    }

    fn evalListDropFirst(self: *LirInterpreter, list_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const info = self.listElemInfo(list_layout);
        self.roc_env.resetCrash();
        const sj = setjmp(&self.roc_env.jmp_buf);
        if (sj != 0) return error.Crash;
        const result = builtins.list.listSublist(
            valueToRocList(list_arg),
            info.alignment,
            info.width,
            false,
            1,
            std.math.maxInt(u64),
            null,
            &builtins.utils.rcNone,
            &self.roc_ops,
        );
        return self.rocListToValue(result, ret_layout);
    }

    fn evalListDropLast(self: *LirInterpreter, list_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const rl = valueToRocList(list_arg);
        const info = self.listElemInfo(list_layout);
        const len = rl.len();
        if (len == 0) return self.rocListToValue(rl, ret_layout);
        self.roc_env.resetCrash();
        const sj = setjmp(&self.roc_env.jmp_buf);
        if (sj != 0) return error.Crash;
        const result = builtins.list.listSublist(
            rl,
            info.alignment,
            info.width,
            false,
            0,
            len - 1,
            null,
            &builtins.utils.rcNone,
            &self.roc_ops,
        );
        return self.rocListToValue(result, ret_layout);
    }

    fn evalListTakeFirst(self: *LirInterpreter, list_arg: Value, count_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const info = self.listElemInfo(list_layout);
        self.roc_env.resetCrash();
        const sj = setjmp(&self.roc_env.jmp_buf);
        if (sj != 0) return error.Crash;
        const result = builtins.list.listSublist(
            valueToRocList(list_arg),
            info.alignment,
            info.width,
            false,
            0,
            count_arg.read(u64),
            null,
            &builtins.utils.rcNone,
            &self.roc_ops,
        );
        return self.rocListToValue(result, ret_layout);
    }

    fn evalListTakeLast(self: *LirInterpreter, list_arg: Value, count_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const rl = valueToRocList(list_arg);
        const info = self.listElemInfo(list_layout);
        const len = rl.len();
        const take = count_arg.read(u64);
        const start = if (take >= len) 0 else len - @as(usize, @intCast(take));
        self.roc_env.resetCrash();
        const sj = setjmp(&self.roc_env.jmp_buf);
        if (sj != 0) return error.Crash;
        const result = builtins.list.listSublist(
            rl,
            info.alignment,
            info.width,
            false,
            @intCast(start),
            take,
            null,
            &builtins.utils.rcNone,
            &self.roc_ops,
        );
        return self.rocListToValue(result, ret_layout);
    }

    fn evalListContains(self: *LirInterpreter, list_arg: Value, elem_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const rl = valueToRocList(list_arg);
        const info = self.listElemInfo(list_layout);
        const val = try self.alloc(ret_layout);
        var found = false;
        if (rl.bytes != null and info.width > 0) {
            for (0..rl.len()) |i| {
                const elem_ptr = rl.bytes.? + i * info.width;
                if (std.mem.eql(u8, elem_ptr[0..info.width], elem_arg.ptr[0..info.width])) {
                    found = true;
                    break;
                }
            }
        }
        val.write(u8, if (found) 1 else 0);
        return val;
    }

    fn evalListReverse(self: *LirInterpreter, list_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const rl = valueToRocList(list_arg);
        const info = self.listElemInfo(list_layout);
        if (rl.len() <= 1 or rl.bytes == null or info.width == 0)
            return self.rocListToValue(rl, ret_layout);
        // Clone and reverse in-place
        self.roc_env.resetCrash();
        const sj = setjmp(&self.roc_env.jmp_buf);
        if (sj != 0) return error.Crash;
        const new_list = builtins.list.shallowClone(rl, rl.len(), info.width, info.alignment, false, &self.roc_ops);
        if (new_list.bytes) |bytes| {
            var lo: usize = 0;
            var hi: usize = new_list.len() - 1;
            const tmp = self.arena.allocator().alloc(u8, info.width) catch return error.OutOfMemory;
            while (lo < hi) {
                @memcpy(tmp, bytes[lo * info.width ..][0..info.width]);
                @memcpy(bytes[lo * info.width ..][0..info.width], bytes[hi * info.width ..][0..info.width]);
                @memcpy(bytes[hi * info.width ..][0..info.width], tmp);
                lo += 1;
                hi -= 1;
            }
        }
        return self.rocListToValue(new_list, ret_layout);
    }

    fn evalListSplitFirst(self: *LirInterpreter, list_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const rl = valueToRocList(list_arg);
        const info = self.listElemInfo(list_layout);
        const val = try self.alloc(ret_layout);
        if (rl.len() > 0 and rl.bytes != null and info.width > 0) {
            // Ok: { first_elem, rest_list }
            @memcpy(val.ptr[0..info.width], rl.bytes.?[0..info.width]);
            // Rest list starts at offset info.width
            self.roc_env.resetCrash();
            const sj = setjmp(&self.roc_env.jmp_buf);
            if (sj != 0) return error.Crash;
            const rest = builtins.list.listSublist(
                rl,
                info.alignment,
                info.width,
                false,
                1,
                std.math.maxInt(u64),
                null,
                &builtins.utils.rcNone,
                &self.roc_ops,
            );
            // Write rest list after the element, aligned to list alignment
            const list_offset = std.mem.alignForward(usize, info.width, @alignOf(RocList));
            @memcpy(val.ptr[list_offset..][0..@sizeOf(RocList)], std.mem.asBytes(&rest));
            self.helper.writeTagDiscriminant(val, ret_layout, 1);
        } else {
            self.helper.writeTagDiscriminant(val, ret_layout, 0);
        }
        return val;
    }

    fn evalListSplitLast(self: *LirInterpreter, list_arg: Value, list_layout: layout_mod.Idx, ret_layout: layout_mod.Idx) Error!Value {
        const rl = valueToRocList(list_arg);
        const info = self.listElemInfo(list_layout);
        const val = try self.alloc(ret_layout);
        if (rl.len() > 0 and rl.bytes != null and info.width > 0) {
            // Ok: { last_elem, rest_list }
            const last_offset = (rl.len() - 1) * info.width;
            @memcpy(val.ptr[0..info.width], rl.bytes.?[last_offset..][0..info.width]);
            self.roc_env.resetCrash();
            const sj = setjmp(&self.roc_env.jmp_buf);
            if (sj != 0) return error.Crash;
            const rest = builtins.list.listSublist(
                rl,
                info.alignment,
                info.width,
                false,
                0,
                rl.len() - 1,
                null,
                &builtins.utils.rcNone,
                &self.roc_ops,
            );
            const list_offset = std.mem.alignForward(usize, info.width, @alignOf(RocList));
            @memcpy(val.ptr[list_offset..][0..@sizeOf(RocList)], std.mem.asBytes(&rest));
            self.helper.writeTagDiscriminant(val, ret_layout, 1);
        } else {
            self.helper.writeTagDiscriminant(val, ret_layout, 0);
        }
        return val;
    }

    /// Generic integer binary operation.
    fn intBinOp(comptime T: type, av: T, bv: T, op: NumOp) T {
        return switch (op) {
            .add => av +% bv,
            .sub => av -% bv,
            .mul => av *% bv,
            .negate => if (@typeInfo(T).int.signedness == .signed) -%av else -%av,
            .abs => if (@typeInfo(T).int.signedness == .signed)
                (if (av < 0) -%av else av)
            else
                av,
            .abs_diff => if (@typeInfo(T).int.signedness == .signed)
                (if (av > bv) av -% bv else bv -% av)
            else
                (if (av > bv) av - bv else bv - av),
            .div, .div_trunc => if (bv != 0) @divTrunc(av, bv) else 0,
            .rem => if (bv != 0) @rem(av, bv) else 0,
            .mod => if (bv != 0) @mod(av, bv) else 0,
        };
    }

    /// Generic float binary operation.
    fn floatBinOp(comptime T: type, av: T, bv: T, op: NumOp) T {
        return switch (op) {
            .add => av + bv,
            .sub => av - bv,
            .mul => av * bv,
            .negate => -av,
            .abs => @abs(av),
            .abs_diff => @abs(av - bv),
            .div, .div_trunc => av / bv,
            .rem, .mod => @rem(av, bv),
        };
    }

    /// Dec (fixed-point i128 with 10^18 scale) binary operation.
    fn decBinOp(self: *LirInterpreter, av: i128, bv: i128, op: NumOp) i128 {
        return switch (op) {
            .add => av +% bv,
            .sub => av -% bv,
            .negate => -%av,
            .abs => if (av < 0) -%av else av,
            .abs_diff => if (av > bv) av -% bv else bv -% av,
            .mul => blk: {
                const result = RocDec.mulWithOverflow(RocDec{ .num = av }, RocDec{ .num = bv });
                break :blk result.value.num;
            },
            .div => blk: {
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) break :blk @as(i128, 0);
                break :blk builtins.dec.divC(RocDec{ .num = av }, RocDec{ .num = bv }, &self.roc_ops);
            },
            .div_trunc => blk: {
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) break :blk @as(i128, 0);
                break :blk builtins.dec.divTruncC(RocDec{ .num = av }, RocDec{ .num = bv }, &self.roc_ops);
            },
            .rem => blk: {
                // Dec rem: a - trunc(a/b) * b
                if (bv == 0) break :blk @as(i128, 0);
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) break :blk @as(i128, 0);
                const div_result = builtins.dec.divTruncC(RocDec{ .num = av }, RocDec{ .num = bv }, &self.roc_ops);
                const mul_result = RocDec.mulWithOverflow(RocDec{ .num = div_result }, RocDec{ .num = bv });
                break :blk av -% mul_result.value.num;
            },
            .mod => blk: {
                if (bv == 0) break :blk @as(i128, 0);
                self.roc_env.resetCrash();
                const sj = setjmp(&self.roc_env.jmp_buf);
                if (sj != 0) break :blk @as(i128, 0);
                const div_result = builtins.dec.divTruncC(RocDec{ .num = av }, RocDec{ .num = bv }, &self.roc_ops);
                const mul_result = RocDec.mulWithOverflow(RocDec{ .num = div_result }, RocDec{ .num = bv });
                const remainder = av -% mul_result.value.num;
                // Mod adjusts sign to match divisor
                if (remainder == 0) break :blk @as(i128, 0);
                if ((remainder > 0) != (bv > 0))
                    break :blk remainder +% bv
                else
                    break :blk remainder;
            },
        };
    }

    /// Generic comparison operation.
    fn cmpOp(comptime T: type, av: T, bv: T, op: CmpOp) bool {
        return switch (op) {
            .eq => av == bv,
            .lt => av < bv,
            .lte => av <= bv,
            .gt => av > bv,
            .gte => av >= bv,
        };
    }

    fn cmpOrder(comptime T: type, av: T, bv: T) u8 {
        if (av < bv) return 0; // LT
        if (av == bv) return 1; // EQ
        return 2; // GT
    }

    fn shiftOp(comptime T: type, av: T, amount: u8, op: ShiftOp) T {
        const Bits = std.math.Log2Int(T);
        const max_bits = @typeInfo(T).int.bits;
        if (amount >= max_bits) return 0;
        const shift: Bits = @intCast(amount);
        return switch (op) {
            .shl => av << shift,
            .shr => av >> shift,
            .shr_zf => blk: {
                const U = std.meta.Int(.unsigned, max_bits);
                break :blk @bitCast(@as(U, @bitCast(av)) >> shift);
            },
        };
    }

    fn intPow(base_val: i128, exp: i128) i128 {
        if (exp <= 0) return 1;
        var result: i128 = 1;
        var b = base_val;
        var e = exp;
        while (e > 0) {
            if (e & 1 != 0) result = result *% b;
            b = b *% b;
            e >>= 1;
        }
        return result;
    }

    // ──────────────────────────────────────────────────────────────
    // String operations
    // ──────────────────────────────────────────────────────────────

    fn evalStrJoinWith(self: *LirInterpreter, list_arg: Value, sep_arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const rl = valueToRocList(list_arg);
        const sep = self.readRocStr(sep_arg);
        const count = rl.len();
        if (count == 0) return self.makeRocStr("");

        // Read each RocStr element from the list
        const str_size = @sizeOf(RocStr);
        var total_len: usize = 0;
        var parts = std.array_list.AlignedManaged([]const u8, null).init(self.allocator);
        defer parts.deinit();
        for (0..count) |i| {
            const elem_ptr = rl.bytes.? + i * str_size;
            const elem_val = Value{ .ptr = elem_ptr };
            const s = self.readRocStr(elem_val);
            total_len += s.len;
            parts.append(s) catch return error.OutOfMemory;
        }
        total_len += sep.len * (count - 1);

        const buf = self.arena.allocator().alloc(u8, total_len) catch return error.OutOfMemory;
        var offset: usize = 0;
        for (parts.items, 0..) |s, i| {
            @memcpy(buf[offset..][0..s.len], s);
            offset += s.len;
            if (i < parts.items.len - 1) {
                @memcpy(buf[offset..][0..sep.len], sep);
                offset += sep.len;
            }
        }
        _ = ret_layout;
        return self.makeRocStr(buf);
    }

    fn evalStrConcat(self: *LirInterpreter, sc: lir.LirExprSpan) Error!Value {
        const parts = self.store.getExprSpan(sc);
        if (parts.len == 0) return self.makeRocStr("");

        var total_len: usize = 0;
        var part_strs = std.array_list.AlignedManaged([]const u8, null).init(self.allocator);
        defer part_strs.deinit();

        for (parts) |part_id| {
            const part_val = try self.evalValue(part_id);
            const s = self.readRocStr(part_val);
            total_len += s.len;
            part_strs.append(s) catch return error.OutOfMemory;
        }

        const buf = self.arena.allocator().alloc(u8, total_len) catch return error.OutOfMemory;
        var offset: usize = 0;
        for (part_strs.items) |s| {
            @memcpy(buf[offset..][0..s.len], s);
            offset += s.len;
        }
        return self.makeRocStr(buf);
    }

    fn evalIntToStr(self: *LirInterpreter, its: anytype) Error!Value {
        const int_val = try self.evalValue(its.value);
        const arena = self.arena.allocator();
        const formatted: []const u8 = switch (its.int_precision) {
            .u8 => std.fmt.allocPrint(arena, "{d}", .{int_val.read(u8)}) catch return error.OutOfMemory,
            .i8 => std.fmt.allocPrint(arena, "{d}", .{int_val.read(i8)}) catch return error.OutOfMemory,
            .u16 => std.fmt.allocPrint(arena, "{d}", .{int_val.read(u16)}) catch return error.OutOfMemory,
            .i16 => std.fmt.allocPrint(arena, "{d}", .{int_val.read(i16)}) catch return error.OutOfMemory,
            .u32 => std.fmt.allocPrint(arena, "{d}", .{int_val.read(u32)}) catch return error.OutOfMemory,
            .i32 => std.fmt.allocPrint(arena, "{d}", .{int_val.read(i32)}) catch return error.OutOfMemory,
            .u64 => std.fmt.allocPrint(arena, "{d}", .{int_val.read(u64)}) catch return error.OutOfMemory,
            .i64 => std.fmt.allocPrint(arena, "{d}", .{int_val.read(i64)}) catch return error.OutOfMemory,
            .u128 => std.fmt.allocPrint(arena, "{d}", .{int_val.read(u128)}) catch return error.OutOfMemory,
            .i128 => std.fmt.allocPrint(arena, "{d}", .{int_val.read(i128)}) catch return error.OutOfMemory,
        };
        return self.makeRocStr(formatted);
    }

    fn evalFloatToStr(self: *LirInterpreter, fts: anytype) Error!Value {
        const float_val = try self.evalValue(fts.value);
        var buf: [400]u8 = undefined;
        const slice: []const u8 = switch (fts.float_precision) {
            .f32 => i128h.f64_to_str(&buf, @as(f64, float_val.read(f32))),
            .f64 => i128h.f64_to_str(&buf, float_val.read(f64)),
            .dec => blk: {
                const dec = RocDec{ .num = float_val.read(i128) };
                var dec_buf: [RocDec.max_str_length]u8 = undefined;
                break :blk dec.format_to_buf(&dec_buf);
            },
        };
        return self.makeRocStr(slice);
    }

    fn evalDecToStr(self: *LirInterpreter, dts: LirExprId) Error!Value {
        const dec_val = try self.evalValue(dts);
        const dec = RocDec{ .num = dec_val.read(i128) };
        var buf: [RocDec.max_str_length]u8 = undefined;
        const slice = dec.format_to_buf(&buf);
        return self.makeRocStr(slice);
    }

    fn evalStrEscapeAndQuote(self: *LirInterpreter, seq: LirExprId) Error!Value {
        const str_val = try self.evalValue(seq);
        const s = self.readRocStr(str_val);
        // Escape backslashes and quotes, then wrap in quotes
        var escaped = std.array_list.AlignedManaged(u8, null).init(self.allocator);
        defer escaped.deinit();
        escaped.append('"') catch return error.OutOfMemory;
        for (s) |ch| {
            switch (ch) {
                '\\' => escaped.appendSlice("\\\\") catch return error.OutOfMemory,
                '"' => escaped.appendSlice("\\\"") catch return error.OutOfMemory,
                else => escaped.append(ch) catch return error.OutOfMemory,
            }
        }
        escaped.append('"') catch return error.OutOfMemory;
        return self.makeRocStr(escaped.items);
    }

    // ──────────────────────────────────────────────────────────────
    // Layout helpers
    // ──────────────────────────────────────────────────────────────

    /// Get the layout of the i-th field in a struct layout.
    fn fieldLayoutOf(self: *LirInterpreter, struct_layout: layout_mod.Idx, field_idx: u32) layout_mod.Idx {
        const l = self.layout_store.getLayout(struct_layout);
        if (l.tag != .struct_) return .zst;
        const sd = self.layout_store.getStructData(l.data.struct_.idx);
        const fields = self.layout_store.struct_fields.sliceRange(sd.getFields());
        if (field_idx < fields.len) {
            return fields.get(field_idx).layout;
        }
        return .zst;
    }

    /// Get the payload layout for a given tag discriminant.
    fn tagPayloadLayout(self: *LirInterpreter, union_layout: layout_mod.Idx, discriminant: u16) layout_mod.Idx {
        const l = self.layout_store.getLayout(union_layout);
        if (l.tag != .tag_union) return .zst;
        const tu_data = self.layout_store.getTagUnionData(l.data.tag_union.idx);
        const variants = self.layout_store.getTagUnionVariants(tu_data);
        if (discriminant < variants.len) {
            return variants.get(discriminant).payload_layout;
        }
        return .zst;
    }

    fn getLayout(self: *LirInterpreter, idx: layout_mod.Idx) Layout {
        return self.layout_store.getLayout(idx);
    }
};
