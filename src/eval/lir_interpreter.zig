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
const builtins = @import("builtins");

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
        };
    }

    pub fn deinit(self: *LirInterpreter) void {
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

    // ──────────────────────────────────────────────────────────────
    // Expression evaluation
    // ──────────────────────────────────────────────────────────────

    /// Evaluate a LIR expression, returning its value.
    pub fn eval(self: *LirInterpreter, expr_id: LirExprId) Error!EvalResult {
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
        switch (size) {
            1 => val.write(u8, @bitCast(@as(i8, @intCast(value)))),
            2 => val.write(u16, @bitCast(@as(i16, @intCast(value)))),
            4 => val.write(u32, @bitCast(@as(i32, @intCast(value)))),
            8 => val.write(u64, @bitCast(value)),
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
                // Heap string: allocate and store
                const heap_mem = self.arena.allocator().alloc(u8, bytes.len) catch return error.OutOfMemory;
                @memcpy(heap_mem, bytes);
                val.write(usize, @intFromPtr(heap_mem.ptr)); // ptr
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
                const heap_mem = self.arena.allocator().alloc(u8, bytes.len) catch return error.OutOfMemory;
                @memcpy(heap_mem, bytes);
                val.write(u32, @intCast(@intFromPtr(heap_mem.ptr)));
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
                    for (args, 0..) |arg_pat_id, i| {
                        // Tag payload fields are struct-ordered
                        _ = i;
                        try self.bindPattern(arg_pat_id, val);
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
                for (args) |arg_pat_id| {
                    if (!try self.matchPattern(arg_pat_id, val)) break :blk false;
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

        if (count == 0 or elem_size == 0) return val;

        // Allocate element storage
        const total_elem_bytes = elem_size * count;
        const elem_mem = self.arena.allocator().alloc(u8, total_elem_bytes) catch return error.OutOfMemory;
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

        if (data_ptr == 0 or count == 0 or elem_size == 0) return .{ .value = Value.zst };

        const data: [*]u8 = @ptrFromInt(data_ptr);
        var i: usize = 0;
        while (i < count) : (i += 1) {
            const elem_val = Value{ .ptr = data + i * elem_size };
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
        var args = std.ArrayList(Value).init(self.allocator);
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
    // Low-level operations (stub — will be expanded in Phase 4)
    // ──────────────────────────────────────────────────────────────

    fn evalLowLevel(self: *LirInterpreter, ll: anytype) Error!Value {
        const arg_exprs = self.store.getExprSpan(ll.args);
        var args: [8]Value = undefined;
        const n = @min(arg_exprs.len, 8);
        for (0..n) |i| {
            args[i] = try self.evalValue(arg_exprs[i]);
        }

        // Basic numeric operations
        return switch (ll.op) {
            .num_add => self.numBinOp(args[0], args[1], ll.ret_layout, .add),
            .num_sub => self.numBinOp(args[0], args[1], ll.ret_layout, .sub),
            .num_mul => self.numBinOp(args[0], args[1], ll.ret_layout, .mul),
            .num_negate => self.numUnaryOp(args[0], ll.ret_layout, .negate),
            .num_eq => self.numCmpOp(args[0], args[1], ll.ret_layout, .eq),
            .num_neq => self.numCmpOp(args[0], args[1], ll.ret_layout, .neq),
            .num_lt => self.numCmpOp(args[0], args[1], ll.ret_layout, .lt),
            .num_lte => self.numCmpOp(args[0], args[1], ll.ret_layout, .lte),
            .num_gt => self.numCmpOp(args[0], args[1], ll.ret_layout, .gt),
            .num_gte => self.numCmpOp(args[0], args[1], ll.ret_layout, .gte),
            .bool_and => blk: {
                const val = try self.alloc(.bool);
                val.write(u8, if (args[0].read(u8) != 0 and args[1].read(u8) != 0) 1 else 0);
                break :blk val;
            },
            .bool_or => blk: {
                const val = try self.alloc(.bool);
                val.write(u8, if (args[0].read(u8) != 0 or args[1].read(u8) != 0) 1 else 0);
                break :blk val;
            },
            .bool_not => blk: {
                const val = try self.alloc(.bool);
                val.write(u8, if (args[0].read(u8) == 0) 1 else 0);
                break :blk val;
            },
            else => blk: {
                // Unsupported low-level op — return zero value
                break :blk try self.alloc(ll.ret_layout);
            },
        };
    }

    const NumOp = enum { add, sub, mul, negate };
    const CmpOp = enum { eq, neq, lt, lte, gt, gte };

    fn numBinOp(self: *LirInterpreter, a: Value, b: Value, layout_idx: layout_mod.Idx, op: NumOp) Error!Value {
        const size = self.helper.sizeOf(layout_idx);
        const val = try self.alloc(layout_idx);
        switch (size) {
            8 => {
                const l = self.store.getLayout(layout_idx);
                if (l.tag == .scalar and l.data.scalar.tag == .frac) {
                    const av = a.read(f64);
                    const bv = b.read(f64);
                    val.write(f64, switch (op) {
                        .add => av + bv,
                        .sub => av - bv,
                        .mul => av * bv,
                        .negate => -av,
                    });
                } else {
                    const av = a.read(i64);
                    const bv = b.read(i64);
                    val.write(i64, switch (op) {
                        .add => av +% bv,
                        .sub => av -% bv,
                        .mul => av *% bv,
                        .negate => -%av,
                    });
                }
            },
            4 => {
                const l = self.store.getLayout(layout_idx);
                if (l.tag == .scalar and l.data.scalar.tag == .frac) {
                    const av = a.read(f32);
                    const bv = b.read(f32);
                    val.write(f32, switch (op) {
                        .add => av + bv,
                        .sub => av - bv,
                        .mul => av * bv,
                        .negate => -av,
                    });
                } else {
                    const av = a.read(i32);
                    const bv = b.read(i32);
                    val.write(i32, switch (op) {
                        .add => av +% bv,
                        .sub => av -% bv,
                        .mul => av *% bv,
                        .negate => -%av,
                    });
                }
            },
            16 => {
                const av = a.read(i128);
                const bv = b.read(i128);
                val.write(i128, switch (op) {
                    .add => av +% bv,
                    .sub => av -% bv,
                    .mul => av *% bv,
                    .negate => -%av,
                });
            },
            else => {},
        }
        return val;
    }

    fn numUnaryOp(self: *LirInterpreter, a: Value, layout_idx: layout_mod.Idx, op: NumOp) Error!Value {
        return self.numBinOp(a, a, layout_idx, op);
    }

    fn numCmpOp(self: *LirInterpreter, a: Value, b: Value, ret_layout: layout_mod.Idx, op: CmpOp) Error!Value {
        _ = ret_layout;
        const val = try self.alloc(.bool);

        // Determine the size of the values being compared from the first argument
        // We need to check the layout of the arguments, not the return type (which is bool)
        // For now, compare as i64 (most common case)
        const av = a.read(i64);
        const bv = b.read(i64);
        const result: bool = switch (op) {
            .eq => av == bv,
            .neq => av != bv,
            .lt => av < bv,
            .lte => av <= bv,
            .gt => av > bv,
            .gte => av >= bv,
        };
        val.write(u8, if (result) 1 else 0);
        return val;
    }

    // ──────────────────────────────────────────────────────────────
    // String operations (stubs)
    // ──────────────────────────────────────────────────────────────

    fn evalStrConcat(self: *LirInterpreter, sc: lir.LirExprSpan) Error!Value {
        const parts = self.store.getExprSpan(sc);
        if (parts.len == 0) return self.makeRocStr("");

        // Concatenate all parts
        var total_len: usize = 0;
        var part_strs = std.ArrayList([]const u8).init(self.allocator);
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
        _ = its;
        return self.makeRocStr("<int>");
    }

    fn evalFloatToStr(self: *LirInterpreter, fts: anytype) Error!Value {
        _ = fts;
        return self.makeRocStr("<float>");
    }

    fn evalDecToStr(self: *LirInterpreter, dts: LirExprId) Error!Value {
        _ = dts;
        return self.makeRocStr("<dec>");
    }

    fn evalStrEscapeAndQuote(self: *LirInterpreter, seq: LirExprId) Error!Value {
        const str_val = try self.evalValue(seq);
        const s = self.readRocStr(str_val);
        // Simple escape: wrap in quotes
        const buf = self.arena.allocator().alloc(u8, s.len + 2) catch return error.OutOfMemory;
        buf[0] = '"';
        @memcpy(buf[1..][0..s.len], s);
        buf[s.len + 1] = '"';
        return self.makeRocStr(buf);
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
