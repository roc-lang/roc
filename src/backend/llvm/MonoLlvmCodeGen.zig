//! LIR to LLVM code generator
//!
//! The previous LLVM backend implementation has been deleted. Strongest-form LIR is now
//! statement-only and local-based, so LLVM code generation must be rewritten
//! directly against `lir.LirStore` CF statements and explicit locals.

const std = @import("std");
const layout = @import("layout");
const lir = @import("lir");
const RocIo = @import("io").RocIo;

const LlvmBuilder = @import("Builder.zig");

/// Alias for the underlying system I/O type carried by RocIo.
const SysIo = @FieldType(RocIo, "sys_io");

const LirExprStore = lir.LirExprStore;
const LirExprId = lir.LirExprId;
const LirPatternId = lir.LirPatternId;
const Symbol = lir.Symbol;
const LirProc = lir.LirProc;
const CFStmtId = lir.CFStmtId;


const Allocator = std.mem.Allocator;

/// Public struct `MonoLlvmCodeGen`.
pub const MonoLlvmCodeGen = struct {
    allocator: Allocator,
    store: *const lir.LirStore,
    layout_store: ?*const layout.Store = null,

    pub const Error = error{
        OutOfMemory,
        CompilationFailed,
    };

    pub const GenerateResult = struct {
        bitcode: []const u32,
        allocator: Allocator,

        pub fn deinit(self: *GenerateResult) void {
            self.allocator.free(self.bitcode);
        }
    };

    pub fn init(allocator: Allocator, store: *const lir.LirStore) MonoLlvmCodeGen {
        return .{
            .allocator = allocator,
            .store = store,
            .layout_store = null,
        };
    }

    pub fn deinit(_: *MonoLlvmCodeGen) void {}

    pub fn reset(_: *MonoLlvmCodeGen) void {}

    pub fn generateCode(
        self: *MonoLlvmCodeGen,
        expr_id: LirExprId,
        result_layout: layout.Idx,
    ) Error!BitcodeResult {
        // Create LLVM Builder
        var builder = LlvmBuilder.init(.{
            .allocator = self.allocator,
            .name = "roc_mono_eval",
            .target = &builtin.target,
            .triple = getLlvmTriple(),
        }) catch return error.OutOfMemory;
        defer builder.deinit();

        self.builder = &builder;
        defer self.builder = null;

        // Create the eval function: void roc_eval(<type>* out_ptr, roc_ops* ops_ptr)
        const ptr_type = builder.ptrType(.default) catch return error.OutOfMemory;
        const eval_fn_type = builder.fnType(.void, &.{ ptr_type, ptr_type }, .normal) catch return error.OutOfMemory;
        const eval_name = if (builtin.os.tag == .macos)
            builder.strtabString("\x01_roc_eval") catch return error.OutOfMemory
        else
            builder.strtabString("roc_eval") catch return error.OutOfMemory;
        const eval_fn = builder.addFunction(eval_fn_type, eval_name, .default) catch return error.OutOfMemory;
        eval_fn.setLinkage(.external, &builder);

        // Set calling convention for x86_64
        if (builtin.cpu.arch == .x86_64) {
            if (builtin.os.tag == .windows) {
                eval_fn.setCallConv(.win64cc, &builder);
            } else {
                eval_fn.setCallConv(.x86_64_sysvcc, &builder);
            }
        }

        // Build function body
        var wip = LlvmBuilder.WipFunction.init(&builder, .{
            .function = eval_fn,
            .strip = true,
        }) catch return error.OutOfMemory;
        defer wip.deinit();

        self.wip = &wip;
        defer self.wip = null;

        const entry_block = wip.block(0, "entry") catch return error.OutOfMemory;
        wip.cursor = .{ .block = entry_block };

        // Get the output pointer and roc_ops pointer
        const out_ptr = wip.arg(0);
        self.out_ptr = out_ptr;
        self.fn_out_ptr = out_ptr;
        defer self.out_ptr = null;
        self.roc_ops_arg = wip.arg(1);
        defer self.roc_ops_arg = null;

        // Store result layout for early_return
        self.result_layout = result_layout;
        defer self.result_layout = null;

        // Compile all procedures now that the builder is available.
        // Must happen before generateExpr so that call sites can find procs.
        const procs = self.store.getProcs();
        if (procs.len > 0) {
            try self.compileAllProcs(procs);
        }

        // Generate LLVM IR for the expression
        const value = try self.generateExpr(expr_id);

        // Store the result to the output pointer.
        // Some generators (e.g., string literals) write directly to out_ptr and
        // return .none as a sentinel — skip the storage step for those.
        if (value == .none) {
            // Result already written to out_ptr by the generator.
        } else {

            // For scalar types, extend to the canonical size (i64 for ints, i128 for wide ints).
            // For composite types (records, tuples), store the struct directly.
            const is_scalar = switch (result_layout) {
                .bool,
                .i8,
                .i16,
                .i32,
                .i64,
                .u8,
                .u16,
                .u32,
                .u64,
                .i128,
                .u128,
                .dec,
                .f32,
                .f64,
                => true,
                else => false,
            };

            if (is_scalar) {
                const final_type: LlvmBuilder.Type = switch (result_layout) {
                    .bool, .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64 => .i64,
                    .i128, .u128, .dec => .i128,
                    .f32 => .float,
                    .f64 => .double,
                    else => unreachable,
                };

                const signedness: LlvmBuilder.Constant.Cast.Signedness = switch (result_layout) {
                    .i8, .i16, .i32, .i64, .i128 => .signed,
                    .bool, .u8, .u16, .u32, .u64, .u128 => .unsigned,
                    .f32, .f64, .dec => .unneeded,
                    else => .unneeded,
                };

                // Check actual generated value type vs expected store type.
                // value_type (from layoutToLlvmType) may not match the actual value if
                // the expression generates a narrower type (e.g., i64 when result is Dec/i128).
                const actual_value_type = value.typeOfWip(&wip);
                const store_value = if (actual_value_type == final_type)
                    value
                else conv: {
                    // For Dec result_layout, integer values need sign extension
                    const conv_sign: LlvmBuilder.Constant.Cast.Signedness = if (signedness == .unneeded and isIntType(actual_value_type))
                        .signed
                    else
                        signedness;
                    break :conv wip.conv(conv_sign, value, final_type, "") catch return error.CompilationFailed;
                };

                const alignment = LlvmBuilder.Alignment.fromByteUnits(switch (final_type) {
                    .i64 => 8,
                    .i128 => 16,
                    .float => 4,
                    .double => 8,
                    else => 0,
                });
                _ = wip.store(.normal, store_value, out_ptr, alignment) catch return error.CompilationFailed;
            } else {
                // Composite type (record, tuple, tag_union, str, list, etc.)
                // For 24-byte types (str, list), decompose to individual field stores
                // to avoid aggregate store issues where only the first field is written.
                const is_24byte_struct = switch (result_layout) {
                    .str => true,
                    else => blk: {
                        if (self.layout_store) |ls2| {
                            const l = ls2.getLayout(result_layout);
                            break :blk (l.tag == .list or l.tag == .list_of_zst);
                        }
                        break :blk false;
                    },
                };
                if (is_24byte_struct) {
                    const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                    for (0..3) |fi| {
                        const field_val = wip.extractValue(value, &.{@as(u32, @intCast(fi))}, "") catch return error.CompilationFailed;
                        const field_ptr = if (fi == 0) out_ptr else blk: {
                            const off = builder.intValue(.i32, @as(u32, @intCast(fi * 8))) catch return error.OutOfMemory;
                            break :blk wip.gep(.inbounds, .i8, out_ptr, &.{off}, "") catch return error.CompilationFailed;
                        };
                        _ = wip.store(.@"volatile", field_val, field_ptr, alignment) catch return error.CompilationFailed;
                    }
                } else {
                    const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                    _ = wip.store(.normal, value, out_ptr, alignment) catch return error.CompilationFailed;
                }
            }
        } // end of value != .none else

        _ = wip.retVoid() catch return error.CompilationFailed;

        wip.finish() catch return error.CompilationFailed;

        // Serialize to bitcode
        const producer = LlvmBuilder.Producer{
            .name = "Roc Mono LLVM CodeGen",
            .version = .{ .major = 1, .minor = 0, .patch = 0 },
        };

        if (std.process.getEnvVarOwned(self.allocator, "ROC_LLVM_KEEP_IR")) |keep_path| {
            defer self.allocator.free(keep_path);
            const sys_io = SysIo.Threaded.global_single_threaded.io();
            builder.printToFilePath(sys_io, keep_path) catch return error.CompilationFailed;
        } else |_| {}

        const bitcode = builder.toBitcode(self.allocator, producer) catch return error.CompilationFailed;

        return BitcodeResult{
            .bitcode = bitcode,
            .result_layout = result_layout,
            .allocator = self.allocator,
        };
    }

    /// Compile all procedures as LLVM functions.
    /// Mirrors dev backend's compileAllProcs: creates each proc as a callable
    /// LLVM function and registers it in proc_registry before compiling the body,
    /// so recursive calls within the body can find the function.
    pub fn compileAllProcs(self: *MonoLlvmCodeGen, procs: []const LirProc) Error!void {
        for (procs) |proc| {
            self.compileProc(proc) catch {
                // Skip procs that can't be compiled (e.g. OOM).
                // The proc won't be in proc_registry, so call sites will
                // hit unreachable if they try to invoke it.
                continue;
            };
        }
    }

    fn compileProc(self: *MonoLlvmCodeGen, proc: LirProc) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;

        // Build the LLVM function type from arg_layouts and ret_layout.
        // An extra ptr parameter is appended for roc_ops (hidden ABI argument)
        // so that proc bodies can call builtins like allocateWithRefcountC.
        const arg_layouts = self.store.getLayoutIdxSpan(proc.arg_layouts);
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
        var param_types: std.ArrayList(LlvmBuilder.Type) = .empty;
        defer param_types.deinit(self.allocator);
        for (arg_layouts) |arg_layout| {
            param_types.append(self.allocator, try self.layoutToLlvmTypeFull(arg_layout)) catch return error.OutOfMemory;
        }
        // Hidden roc_ops parameter at the end
        param_types.append(self.allocator, ptr_type) catch return error.OutOfMemory;

        const ret_type = try self.layoutToLlvmTypeFull(proc.ret_layout);
        const fn_type = builder.fnType(ret_type, param_types.items, .normal) catch return error.OutOfMemory;

        // Create a unique function name from the symbol
        const key: u64 = @bitCast(proc.name);
        var name_buf: [64]u8 = undefined;
        const name_str = std.fmt.bufPrint(&name_buf, "roc_proc_{d}", .{key}) catch return error.OutOfMemory;
        const fn_name = builder.strtabString(name_str) catch return error.OutOfMemory;

        const func = builder.addFunction(fn_type, fn_name, .default) catch return error.OutOfMemory;

        // Register in proc_registry BEFORE compiling body (for recursive calls)
        self.proc_registry.put(key, func) catch return error.OutOfMemory;
        errdefer _ = self.proc_registry.remove(key);

        // Save and restore outer wip state and roc_ops_arg
        const outer_wip = self.wip;
        defer self.wip = outer_wip;
        const outer_roc_ops = self.roc_ops_arg;
        defer self.roc_ops_arg = outer_roc_ops;
        const outer_out_ptr = self.out_ptr;
        defer self.out_ptr = outer_out_ptr;
        const outer_fn_out_ptr = self.fn_out_ptr;
        defer self.fn_out_ptr = outer_fn_out_ptr;
        self.out_ptr = null; // Procs don't have a top-level out_ptr
        self.fn_out_ptr = null;

        // Create a new WipFunction for this procedure
        var proc_wip = LlvmBuilder.WipFunction.init(builder, .{
            .function = func,
            .strip = true,
        }) catch return error.OutOfMemory;
        defer proc_wip.deinit();

        self.wip = &proc_wip;

        const proc_entry = proc_wip.block(0, "entry") catch return error.OutOfMemory;
        proc_wip.cursor = .{ .block = proc_entry };

        // Bind parameters to argument values
        const params = self.store.getPatternSpan(proc.args);
        for (params, 0..) |param_id, i| {
            const arg_val = proc_wip.arg(@intCast(i));
            try self.bindPattern(param_id, arg_val);
        }

        // Set roc_ops_arg to the hidden last parameter
        self.roc_ops_arg = proc_wip.arg(@intCast(params.len));

        // Generate the body (control flow statements)
        self.generateStmt(proc.body) catch return error.CompilationFailed;

        proc_wip.finish() catch return error.CompilationFailed;
    }

    /// Convert layout to LLVM type (scalar types only — composite layouts fall through to i64)
    fn layoutToLlvmType(result_layout: layout.Idx) LlvmBuilder.Type {
        return switch (result_layout) {
            .zst => .i8,
            .bool => .i1,
            .u8, .i8 => .i8,
            .u16, .i16 => .i16,
            .u32, .i32 => .i32,
            .u64, .i64 => .i64,
            .u128, .i128, .dec => .i128,
            .f32 => .float,
            .f64 => .double,
            else => .i64,
        };
    }

    fn layoutToLlvmTypeWithOptions(
        self: *MonoLlvmCodeGen,
        result_layout: layout.Idx,
        bool_in_memory: bool,
    ) Error!LlvmBuilder.Type {
        const builder = self.builder orelse return error.CompilationFailed;

        return switch (result_layout) {
            .zst => .i8,
            .bool => if (bool_in_memory) .i8 else .i1,
            .u8, .i8 => .i8,
            .u16, .i16 => .i16,
            .u32, .i32 => .i32,
            .u64, .i64 => .i64,
            .u128, .i128, .dec => .i128,
            .f32 => .float,
            .f64 => .double,
            .str => blk: {
                const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
                break :blk builder.structType(.normal, &.{ ptr_type, .i64, .i64 }) catch return error.CompilationFailed;
            },
            else => blk: {
                const ls = self.layout_store orelse break :blk .i64;
                const stored_layout = ls.getLayout(result_layout);
                switch (stored_layout.tag) {
                    .list, .list_of_zst => {
                        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
                        break :blk builder.structType(.normal, &.{ ptr_type, .i64, .i64 }) catch return error.CompilationFailed;
                    },
                    .struct_ => {
                        const struct_data = ls.getStructData(stored_layout.getStruct().idx);
                        const fields = struct_data.getFields();
                        if (fields.count == 0) break :blk .i8;

                        var field_types: [32]LlvmBuilder.Type = undefined;
                        for (0..fields.count) |field_idx| {
                            field_types[field_idx] = try self.layoutToLlvmTypeWithOptions(
                                ls.getStructFieldLayout(stored_layout.getStruct().idx, @intCast(field_idx)),
                                true,
                            );
                        }
                        break :blk builder.structType(.normal, field_types[0..fields.count]) catch return error.CompilationFailed;
                    },
                    .tag_union => {
                        const tu_data = ls.getTagUnionData(stored_layout.getTagUnion().idx);
                        const variants = ls.getTagUnionVariants(tu_data);
                        for (0..variants.len) |variant_idx| {
                            if (variants.get(@intCast(variant_idx)).payload_layout != .zst) {
                                break :blk builder.ptrType(.default) catch return error.CompilationFailed;
                            }
                        }
                        break :blk .i64;
                    },
                    else => break :blk .i64,
                }
            },
        };
    }

    /// Convert layout to LLVM type, handling str/list/struct layouts correctly for
    /// values passed between generated functions.
    fn layoutToLlvmTypeFull(self: *MonoLlvmCodeGen, result_layout: layout.Idx) Error!LlvmBuilder.Type {
        return self.layoutToLlvmTypeWithOptions(result_layout, false);
    }

    // Control Flow Statement generation (mirrors dev backend's generateStmt)

    fn generateStmt(self: *MonoLlvmCodeGen, stmt_id: CFStmtId) Error!void {
        const stmt = self.store.getCFStmt(stmt_id);

        switch (stmt) {
            .let_stmt => |let_s| {
                const val = try self.generateExpr(let_s.value);
                try self.bindPattern(let_s.pattern, val);
                try self.generateStmt(let_s.next);
            },
            .ret => |r| {
                const wip = self.wip orelse return error.CompilationFailed;
                const val = try self.generateExpr(r.value);
                _ = wip.ret(val) catch return error.CompilationFailed;
            },
            .join => |j| {
                const wip = self.wip orelse return error.CompilationFailed;
                const builder = self.builder orelse return error.CompilationFailed;

                // Store join point parameters for rebinding on jumps
                const jp_key = @intFromEnum(j.id);
                const params = self.store.getPatternSpan(j.params);
                const params_copy = self.allocator.dupe(LirPatternId, params) catch return error.OutOfMemory;
                self.join_point_params.put(jp_key, params_copy) catch return error.OutOfMemory;

                // Create allocas for each join point parameter so that jumps from
                // different predecessor blocks can store values SSA-correctly.
                // The join body loads from these allocas before executing.
                if (params.len > 0) {
                    const allocas = self.allocator.alloc(LlvmBuilder.Value, params.len) catch return error.OutOfMemory;
                    const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                    const alloca_count = builder.intValue(.i32, 3) catch return error.OutOfMemory;
                    for (allocas) |*a| {
                        // Each param gets 24 bytes (3×i64), enough for str/list/record structs
                        a.* = wip.alloca(.normal, .i64, alloca_count, alignment, .default, "jp") catch return error.CompilationFailed;
                    }
                    self.join_param_allocas.put(jp_key, allocas) catch return error.OutOfMemory;

                    // Create type tracking array (default i64, updated at first jump)
                    const types = self.allocator.alloc(LlvmBuilder.Type, params.len) catch return error.OutOfMemory;
                    @memset(types, .i64);
                    self.join_param_types.put(jp_key, types) catch return error.OutOfMemory;

                    // Initialize allocas to 0 to avoid undef
                    const zero = builder.intValue(.i64, 0) catch return error.OutOfMemory;
                    for (allocas) |a| {
                        _ = wip.store(.normal, zero, a, alignment) catch return error.CompilationFailed;
                    }
                }

                // Create a block for the join point body
                const join_block = wip.block(2, "join") catch return error.CompilationFailed;
                self.join_points.put(jp_key, join_block) catch return error.OutOfMemory;

                // Generate the remainder first (code that jumps TO join point)
                try self.generateStmt(j.remainder);

                // Now generate the join point body: load params from allocas first
                wip.cursor = .{ .block = join_block };
                if (self.join_param_allocas.get(jp_key)) |allocas| {
                    const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                    const types = self.join_param_types.get(jp_key);
                    for (params, allocas, 0..) |param_id, alloca_ptr, i| {
                        const load_type: LlvmBuilder.Type = if (types) |ts| ts[i] else .i64;
                        const loaded = wip.load(.normal, load_type, alloca_ptr, alignment, "") catch return error.CompilationFailed;
                        try self.bindPattern(param_id, loaded);
                    }
                }
                try self.generateStmt(j.body);
            },
            .jump => |jmp| {
                const wip = self.wip orelse return error.CompilationFailed;
                const jp_key = @intFromEnum(jmp.target);

                // Null out_ptr so sub-expressions produce SSA values (not write to out_ptr)
                const saved_out_ptr = self.out_ptr;
                self.out_ptr = null;
                defer self.out_ptr = saved_out_ptr;

                // Evaluate all arguments and store to join point allocas
                const args = self.store.getExprSpan(jmp.args);
                const allocas = self.join_param_allocas.get(jp_key);
                const types = self.join_param_types.get(jp_key);
                const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                for (args, 0..) |arg_id, i| {
                    const val = try self.generateExpr(arg_id);
                    if (allocas) |a| {
                        if (i < a.len) {
                            _ = wip.store(.normal, val, a[i], alignment) catch return error.CompilationFailed;
                            // Record the actual LLVM type for correct loading later
                            if (types) |ts| {
                                if (i < ts.len) {
                                    ts[i] = val.typeOfWip(wip);
                                }
                            }
                        }
                    }
                }

                // Branch to the join point block
                if (self.join_points.get(jp_key)) |join_block| {
                    _ = wip.br(join_block) catch return error.CompilationFailed;
                } else {
                    return error.CompilationFailed;
                }
            },
            .switch_stmt => |sw| {
                try self.generateSwitchStmt(sw);
            },
            .expr_stmt => |e| {
                _ = try self.generateExpr(e.value);
                try self.generateStmt(e.next);
            },
            .match_stmt => |ms| {
                // Pattern match statement (when in tail position of a proc)
                // Similar to match_expr but each branch is a statement, not an expression.
                const scrutinee = try self.generateExpr(ms.value);
                const branches = self.store.getCFMatchBranches(ms.branches);
                std.debug.assert(branches.len != 0);

                for (branches, 0..) |branch, i| {
                    const pattern = self.store.getPattern(branch.pattern);
                    const is_last = (i == branches.len - 1);

                    switch (pattern) {
                        .wildcard, .bind => {
                            if (pattern == .bind) {
                                const bind = pattern.bind;
                                const symbol_key: u64 = @bitCast(bind.symbol);
                                self.symbol_values.put(symbol_key, scrutinee) catch return error.OutOfMemory;
                            }
                            try self.generateStmt(branch.body);
                            break;
                        },
                        .int_literal => |int_pat| {
                            const wip = self.wip orelse return error.CompilationFailed;
                            const builder = self.builder orelse return error.CompilationFailed;
                            const pat_type = layoutToLlvmType(int_pat.layout_idx);
                            const pat_val = builder.intValue(pat_type, @as(u64, @truncate(@as(u128, @bitCast(int_pat.value))))) catch return error.OutOfMemory;
                            const cmp_scrutinee = if (scrutinee.typeOfWip(wip) == pat_type)
                                scrutinee
                            else
                                wip.conv(.unsigned, scrutinee, pat_type, "") catch return error.CompilationFailed;
                            const cmp = wip.icmp(.eq, cmp_scrutinee, pat_val, "") catch return error.OutOfMemory;

                            if (is_last) {
                                try self.generateStmt(branch.body);
                            } else {
                                const then_block = wip.block(1, "match_then") catch return error.OutOfMemory;
                                const else_block = wip.block(1, "match_else") catch return error.OutOfMemory;
                                _ = wip.brCond(cmp, then_block, else_block, .none) catch return error.CompilationFailed;
                                wip.cursor = .{ .block = then_block };
                                try self.generateStmt(branch.body);
                                wip.cursor = .{ .block = else_block };
                            }
                        },
                        else => {
                            // For other patterns, just generate the branch body (best effort)
                            try self.generateStmt(branch.body);
                            break;
                        },
                    }
                }
            },
        }
    }

    fn generateSwitchStmt(self: *MonoLlvmCodeGen, sw: anytype) Error!void {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        const cond_val = try self.generateExpr(sw.cond);
        const branches = self.store.getCFSwitchBranches(sw.branches);

        if (branches.len == 0) {
            // No branches, just generate the default
            try self.generateStmt(sw.default_branch);
            return;
        }

        // Create blocks for each branch and the default.
        // Each block has incoming=1 from the switch instruction.
        const default_block = wip.block(1, "switch_default") catch return error.CompilationFailed;

        var switch_inst = wip.@"switch"(cond_val, default_block, @intCast(branches.len), .none) catch return error.CompilationFailed;

        var branch_blocks: std.ArrayList(LlvmBuilder.Function.Block.Index) = .empty;
        defer branch_blocks.deinit(self.allocator);

        for (branches) |branch| {
            const branch_block = wip.block(1, "switch_case") catch return error.CompilationFailed;
            branch_blocks.append(self.allocator, branch_block) catch return error.OutOfMemory;
            const case_val = builder.intConst(cond_val.typeOfWip(wip), branch.value) catch return error.OutOfMemory;
            switch_inst.addCase(case_val, branch_block, wip) catch return error.CompilationFailed;
        }

        switch_inst.finish(wip);

        // Generate code for each branch
        for (branches, branch_blocks.items) |branch, branch_block| {
            wip.cursor = .{ .block = branch_block };
            try self.generateStmt(branch.body);
        }

        // Generate default branch
        wip.cursor = .{ .block = default_block };
        try self.generateStmt(sw.default_branch);
    }

    // Expression generation

    /// Generate LLVM IR for an expression
    fn generateExpr(self: *MonoLlvmCodeGen, expr_id: LirExprId) Error!LlvmBuilder.Value {
        const expr = self.store.getExpr(expr_id);

        return switch (expr) {
            // Literals
            .i64_literal => |val| self.emitI64(val.value),
            .i128_literal => |val| self.emitI128(val.value),
            .f64_literal => |val| self.emitF64(val),
            .f32_literal => |val| self.emitF32(val),
            .dec_literal => |val| self.emitI128(val),
            .bool_literal => |val| self.emitBool(val),

            // Lookups
            .lookup => |lookup| self.generateLookup(lookup.symbol, lookup.layout_idx),
            .cell_load => |load| self.generateCellLoad(load.cell, load.layout_idx),

            // Control flow
            .if_then_else => |ite| self.generateIfThenElse(ite),

            // Blocks
            .block => |block| self.generateBlock(block),

            // Function calls and lambdas
            .call => |call| self.generateCall(call),
            .lambda => |lambda| self.generateLambdaExpr(lambda, expr_id),

            // Structs
            .struct_ => |struct_expr| self.generateStruct(struct_expr),
            .struct_access => |access| self.generateStructAccess(access),

            // Tag unions
            .zero_arg_tag => |zat| self.generateZeroArgTag(zat),
            .tag => |t| self.generateTagWithPayload(t),
            .discriminant_switch => |ds| self.generateDiscriminantSwitch(ds),

            // Strings
            .str_literal => |str_idx| self.generateStrLiteral(str_idx),
            .int_to_str => |its| self.generateIntToStr(its),
            .float_to_str => |fts| self.generateFloatToStr(fts),
            .dec_to_str => |dts| self.generateDecToStr(dts),
            .str_escape_and_quote => |seq| self.generateStrEscapeAndQuote(seq),

            // Nominal wrappers are transparent
            .nominal => |nom| self.generateExpr(nom.backing_expr),

            // Pattern matching
            .match_expr => |m| self.generateMatchExpr(m),

            // Debug and expect — just evaluate the inner expression
            .dbg => |d| self.generateExpr(d.expr),
            .expect => |e| self.generateExpr(e.body),

            // Lists
            .empty_list => self.generateEmptyList(),
            .list => |l| self.generateList(l),

            // Low-level builtins
            .low_level => |ll| self.generateLowLevel(ll),

            // Early return (? operator)
            .early_return => |er| self.generateEarlyReturn(er),

            // Runtime error (unreachable) — emit LLVM unreachable
            .runtime_error => |re| self.generateRuntimeError(re.ret_layout),
            .crash => |c| self.generateRuntimeError(c.ret_layout),

            // Reference counting — no-ops in the evaluator (short-lived memory)
            .incref => |inc| {
                _ = try self.generateExpr(inc.value);
                return (self.builder orelse return error.CompilationFailed).intValue(.i8, 0) catch return error.OutOfMemory;
            },
            .decref => |dec_rc| {
                _ = try self.generateExpr(dec_rc.value);
                return (self.builder orelse return error.CompilationFailed).intValue(.i8, 0) catch return error.OutOfMemory;
            },
            .free => |f| {
                _ = try self.generateExpr(f.value);
                return (self.builder orelse return error.CompilationFailed).intValue(.i8, 0) catch return error.OutOfMemory;
            },

            // Loops
            .while_loop => |wl| self.generateWhileLoop(wl),
            .for_loop => |fl| self.generateForLoop(fl),
            .break_expr => self.generateBreakExpr(),

            // These should never reach LLVM codegen:
            // str_concat is lowered to low_level ops before codegen
            // hosted_call is not used in the evaluator
            .str_concat,
            .hosted_call,
            => unreachable,

            .tag_payload_access => |tpa| self.generateTagPayloadAccess(tpa),
        };
    }

    fn emitI64(self: *MonoLlvmCodeGen, val: i64) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        return (builder.intConst(.i64, @as(u64, @bitCast(val))) catch return error.OutOfMemory).toValue();
    }

    fn emitI128(self: *MonoLlvmCodeGen, val: i128) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        return (builder.intConst(.i128, val) catch return error.OutOfMemory).toValue();
    }

    fn emitF64(self: *MonoLlvmCodeGen, val: f64) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        return (builder.doubleConst(val) catch return error.OutOfMemory).toValue();
    }

    fn emitF32(self: *MonoLlvmCodeGen, val: f32) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        return (builder.floatConst(val) catch return error.OutOfMemory).toValue();
    }

    fn emitBool(self: *MonoLlvmCodeGen, val: bool) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        return (builder.intConst(.i1, @intFromBool(val)) catch return error.OutOfMemory).toValue();
    }

    fn generateLookup(self: *MonoLlvmCodeGen, symbol: Symbol, _: layout.Idx) Error!LlvmBuilder.Value {
        const symbol_key: u64 = @bitCast(symbol);

        if (self.cell_allocas.get(symbol_key)) |cell_alloca| {
            const wip = self.wip orelse return error.CompilationFailed;
            return wip.load(.normal, cell_alloca.elem_type, cell_alloca.alloca_ptr, LlvmBuilder.Alignment.fromByteUnits(@intCast(@max(llvmTypeByteSize(cell_alloca.elem_type), 1))), "") catch return error.CompilationFailed;
        }

        // Check if we have a value for this symbol
        if (self.symbol_values.get(symbol_key)) |val| {
            return val;
        }

        // Check if it's a top-level definition
        if (self.store.getSymbolDef(symbol)) |def_expr_id| {
            const val = try self.generateExpr(def_expr_id);
            self.symbol_values.put(symbol_key, val) catch return error.OutOfMemory;
            return val;
        }

        unreachable; // Symbol must exist in symbol_values or as a top-level def
    }

    /// Compare two aggregate values (structs) field-by-field.
    /// Returns i1: true if all fields are equal (or not equal for neq).
    fn generateAggregateEquality(self: *MonoLlvmCodeGen, lhs: LlvmBuilder.Value, rhs: LlvmBuilder.Value, is_neq: bool) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        const lhs_type = lhs.typeOfWip(wip);
        std.debug.assert(lhs_type.isStruct(builder));
        const fields = lhs_type.structFields(builder);
        if (fields.len == 0) {
            // Empty struct — always equal
            const eq_val: i64 = if (is_neq) 0 else 1;
            return builder.intValue(.i1, eq_val) catch return error.OutOfMemory;
        }

        // Check if this struct looks like a RocStr/RocList: {ptr, i64, i64}
        // If so, use the str_equal builtin instead of field-by-field comparison.
        if (fields.len == 3 and self.isStrLikeStruct(fields)) {
            {
                const result = try self.callStrStr2BoolFromValues(lhs, rhs, "roc_builtins_str_equal");
                if (is_neq) {
                    const one = builder.intValue(.i1, 1) catch return error.OutOfMemory;
                    return wip.bin(.xor, result, one, "") catch return error.CompilationFailed;
                }
                return result;
            }
        }

        // Compare each field and AND results together
        var result = builder.intValue(.i1, 1) catch return error.OutOfMemory;
        for (0..fields.len) |i| {
            const idx: u32 = @intCast(i);
            const lhs_field = wip.extractValue(lhs, &.{idx}, "") catch return error.CompilationFailed;
            const rhs_field = wip.extractValue(rhs, &.{idx}, "") catch return error.CompilationFailed;

            const field_type = lhs_field.typeOfWip(wip);
            const field_eq = if (!isIntType(field_type) and field_type != .float and field_type != .double and field_type.isStruct(builder))
                // Nested struct — recurse
                try self.generateAggregateEquality(lhs_field, rhs_field, false)
            else if (!isIntType(field_type) and field_type != .float and field_type != .double)
                // Non-struct aggregate (pointer, etc.) — should not occur
                unreachable
            else if (field_type == .float or field_type == .double)
                wip.fcmp(.normal, .oeq, lhs_field, rhs_field, "") catch return error.CompilationFailed
            else
                wip.icmp(.eq, lhs_field, rhs_field, "") catch return error.CompilationFailed;

            result = wip.bin(.@"and", result, field_eq, "") catch return error.CompilationFailed;
        }

        // For neq, invert the result
        if (is_neq) {
            const one = builder.intValue(.i1, 1) catch return error.OutOfMemory;
            result = wip.bin(.xor, result, one, "") catch return error.CompilationFailed;
        }

        return result;
    }

    /// Check if a struct's fields match the RocStr/RocList pattern: {ptr, i64, i64}
    fn isStrLikeStruct(self: *MonoLlvmCodeGen, fields: []const LlvmBuilder.Type) bool {
        if (fields.len != 3) return false;
        const builder_ptr = self.builder orelse return false;
        const ptr_type = builder_ptr.ptrType(.default) catch return false;
        return fields[0] == ptr_type and fields[1] == .i64 and fields[2] == .i64;
    }

    /// Call str_equal from already-generated struct values (not from expr IDs).
    /// Decomposes both structs into (ptr, len, cap) and calls the named builtin.
    fn callStrStr2BoolFromValues(self: *MonoLlvmCodeGen, lhs: LlvmBuilder.Value, rhs: LlvmBuilder.Value, builtin_name: []const u8) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;

        // Extract fields from both structs
        const a_bytes = wip.extractValue(lhs, &.{0}, "") catch return error.CompilationFailed;
        const a_len = wip.extractValue(lhs, &.{1}, "") catch return error.CompilationFailed;
        const a_cap = wip.extractValue(lhs, &.{2}, "") catch return error.CompilationFailed;

        const b_bytes = wip.extractValue(rhs, &.{0}, "") catch return error.CompilationFailed;
        const b_len = wip.extractValue(rhs, &.{1}, "") catch return error.CompilationFailed;
        const b_cap = wip.extractValue(rhs, &.{2}, "") catch return error.CompilationFailed;

        return self.callBuiltin(builtin_name, .i1, &.{
            ptr_type, .i64, .i64, ptr_type, .i64, .i64,
        }, &.{
            a_bytes, a_len, a_cap, b_bytes, b_len, b_cap,
        });
    }

    fn isSigned(result_layout: layout.Idx) bool {
        return switch (result_layout) {
            .i8, .i16, .i32, .i64, .i128, .dec => true,
            else => false,
        };
    }

    fn isFloatLayout(l: layout.Idx) bool {
        return l == .f32 or l == .f64;
    }

    /// Convert a layout.Idx to the LLVM type used for struct fields.
    /// Unlike layoutToLlvmType, this maps bool to i8 (1 byte in memory)
    /// instead of i1 (1 bit), matching the layout store's memory representation.
    fn layoutToStructFieldType(self: *MonoLlvmCodeGen, field_layout: layout.Idx) Error!LlvmBuilder.Type {
        return self.layoutToLlvmTypeWithOptions(field_layout, true);
    }

    /// Build an LLVM struct type from the actual LLVM types of generated values.
    fn buildStructTypeFromValues(builder: *LlvmBuilder, wip: *LlvmBuilder.WipFunction, values: []const LlvmBuilder.Value) Error!LlvmBuilder.Type {
        var field_types: [32]LlvmBuilder.Type = undefined;
        for (values, 0..) |val, i| {
            field_types[i] = val.typeOfWip(wip);
        }
        return builder.structType(.normal, field_types[0..values.len]) catch return error.OutOfMemory;
    }

    /// Convert a value to match the expected struct field type.
    /// Handles i1→i8 (bool), integer widening/narrowing, etc.
    fn convertToFieldType(self: *MonoLlvmCodeGen, val: LlvmBuilder.Value, field_layout: layout.Idx) Error!LlvmBuilder.Value {
        if (val == .none) return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const target_type = try self.layoutToStructFieldType(field_layout);
        const actual_type = val.typeOfWip(wip);

        if (actual_type == target_type) return val;

        // i1 → i8 (bool in struct)
        if (actual_type == .i1 and target_type == .i8) {
            return wip.cast(.zext, val, .i8, "") catch return error.CompilationFailed;
        }

        // Integer widening (e.g., i64 → i128 for Dec fields)
        if (isIntType(actual_type) and isIntType(target_type)) {
            const actual_bits = intTypeBits(actual_type);
            const target_bits = intTypeBits(target_type);
            if (actual_bits < target_bits) {
                // Widen: use sext for signed, zext for unsigned
                return wip.cast(if (isSigned(field_layout)) .sext else .zext, val, target_type, "") catch return error.CompilationFailed;
            } else if (actual_bits > target_bits) {
                return wip.cast(.trunc, val, target_type, "") catch return error.CompilationFailed;
            }
        }

        // Int → Float conversions
        if (isIntType(actual_type) and (target_type == .float or target_type == .double)) {
            return wip.cast(if (isSigned(field_layout)) .sitofp else .uitofp, val, target_type, "") catch return error.CompilationFailed;
        }

        // If types don't match and we can't convert, return as-is (may cause assertion)
        return val;
    }

    fn coerceValueToLayout(self: *MonoLlvmCodeGen, val: LlvmBuilder.Value, target_layout: layout.Idx) Error!LlvmBuilder.Value {
        if (val == .none) return error.CompilationFailed;

        const wip = self.wip orelse return error.CompilationFailed;
        const target_type = try self.layoutToLlvmTypeFull(target_layout);
        const actual_type = val.typeOfWip(wip);

        if (actual_type == target_type) return val;

        if (target_layout == .bool and isIntType(actual_type) and actual_type != .i1) {
            return wip.cast(.trunc, val, .i1, "") catch return error.CompilationFailed;
        }

        if (target_layout == .bool and actual_type == .ptr) {
            const raw_bool = wip.load(.normal, .i8, val, LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.CompilationFailed;
            return wip.cast(.trunc, raw_bool, .i1, "") catch return error.CompilationFailed;
        }

        if (actual_type == .i1 and target_type == .i8) {
            return wip.cast(.zext, val, .i8, "") catch return error.CompilationFailed;
        }

        if (isIntType(actual_type) and isIntType(target_type)) {
            const actual_bits = intTypeBits(actual_type);
            const target_bits = intTypeBits(target_type);
            if (actual_bits < target_bits) {
                return wip.cast(if (isSigned(target_layout)) .sext else .zext, val, target_type, "") catch return error.CompilationFailed;
            }
            if (actual_bits > target_bits) {
                return wip.cast(.trunc, val, target_type, "") catch return error.CompilationFailed;
            }
        }

        if (isIntType(actual_type) and (target_type == .float or target_type == .double)) {
            return wip.cast(if (isSigned(target_layout)) .sitofp else .uitofp, val, target_type, "") catch return error.CompilationFailed;
        }

        if ((actual_type == .float or actual_type == .double) and isIntType(target_type)) {
            return wip.cast(if (isSigned(target_layout)) .fptosi else .fptoui, val, target_type, "") catch return error.CompilationFailed;
        }

        return val;
    }

    fn isIntType(t: LlvmBuilder.Type) bool {
        return t == .i1 or t == .i8 or t == .i16 or t == .i32 or t == .i64 or t == .i128;
    }

    fn intTypeBits(t: LlvmBuilder.Type) u32 {
        return switch (t) {
            .i1 => 1,
            .i8 => 8,
            .i16 => 16,
            .i32 => 32,
            .i64 => 64,
            .i128 => 128,
            else => 0,
        };
    }

    fn coerceValueToType(self: *MonoLlvmCodeGen, value: LlvmBuilder.Value, expected_type: LlvmBuilder.Type, value_layout: ?layout.Idx) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const actual_type = value.typeOfWip(wip);

        if (actual_type == expected_type) return value;

        if (expected_type == .i1 and isIntType(actual_type)) {
            return wip.cast(.trunc, value, .i1, "") catch return error.CompilationFailed;
        }

        if (actual_type == .i1 and expected_type == .i8) {
            return wip.cast(.zext, value, .i8, "") catch return error.CompilationFailed;
        }

        if (isIntType(actual_type) and isIntType(expected_type)) {
            const actual_bits = intTypeBits(actual_type);
            const expected_bits = intTypeBits(expected_type);
            if (actual_bits < expected_bits) {
                const signed = if (value_layout) |l| isSigned(l) else false;
                return wip.cast(if (signed) .sext else .zext, value, expected_type, "") catch return error.CompilationFailed;
            }
            if (actual_bits > expected_bits) {
                return wip.cast(.trunc, value, expected_type, "") catch return error.CompilationFailed;
            }
        }

        if (isIntType(actual_type) and (expected_type == .float or expected_type == .double)) {
            const signed = if (value_layout) |l| isSigned(l) else false;
            return wip.cast(if (signed) .sitofp else .uitofp, value, expected_type, "") catch return error.CompilationFailed;
        }

        if ((actual_type == .float or actual_type == .double) and isIntType(expected_type)) {
            const signed = if (value_layout) |l| isSigned(l) else false;
            return wip.cast(if (signed) .fptosi else .fptoui, value, expected_type, "") catch return error.CompilationFailed;
        }

        if ((actual_type == .float or actual_type == .double) and (expected_type == .float or expected_type == .double)) {
            return wip.cast(if (actual_type == .float) .fpext else .fptrunc, value, expected_type, "") catch return error.CompilationFailed;
        }

        if (actual_type.isPointer(builder) and expected_type.isPointer(builder)) {
            return wip.cast(.bitcast, value, expected_type, "") catch return error.CompilationFailed;
        }

        if (!actual_type.isPointer(builder) and expected_type.isPointer(builder)) {
            if (value_layout) |layout_idx| {
                return try self.materializeGeneratedValueToPtr(value, layout_idx);
            }
        }

        return value;
    }

    fn materializeGeneratedValueToPtr(self: *MonoLlvmCodeGen, value: LlvmBuilder.Value, layout_idx: layout.Idx) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const size = try self.materializedLayoutSize(layout_idx);
        const byte_array_type = builder.arrayType(size, .i8) catch return error.OutOfMemory;
        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
        const alloca_ptr = wip.alloca(.normal, byte_array_type, .none, alignment, .default, "coerce_tmp") catch return error.CompilationFailed;

        const zero_byte = builder.intValue(.i8, 0) catch return error.OutOfMemory;
        const size_val = builder.intValue(.i32, size) catch return error.OutOfMemory;
        _ = wip.callMemSet(alloca_ptr, alignment, zero_byte, size_val, .normal, false) catch return error.CompilationFailed;
        _ = wip.store(.normal, value, alloca_ptr, alignment) catch return error.CompilationFailed;
        return alloca_ptr;
    }

    fn llvmTypeByteSize(t: LlvmBuilder.Type) u64 {
        return switch (t) {
            .i1, .i8 => 1,
            .i16 => 2,
            .i32, .float => 4,
            .i64, .double => 8,
            .i128 => 16,
            else => 0,
        };
    }

    // Record and tuple generation

    fn generateEmptyRecord(self: *MonoLlvmCodeGen) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        return (builder.intConst(.i8, 0) catch return error.OutOfMemory).toValue();
    }

    fn generateStruct(self: *MonoLlvmCodeGen, struct_expr: anytype) Error!LlvmBuilder.Value {
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        defer self.out_ptr = saved_out_ptr;

        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const ls = self.layout_store orelse return error.CompilationFailed;

        const stored_layout = ls.getLayout(struct_expr.struct_layout);
        if (stored_layout.tag == .zst) {
            return self.generateEmptyRecord();
        }
        std.debug.assert(stored_layout.tag == .struct_);

        const struct_data = ls.getStructData(stored_layout.getStruct().idx);
        const field_count = struct_data.getFields().count;
        if (field_count == 0) {
            return self.generateEmptyRecord();
        }

        const field_exprs = self.store.getExprSpan(struct_expr.fields);
        var field_values_buf: [32]LlvmBuilder.Value = undefined;

        for (field_exprs, 0..) |field_expr_id, i| {
            const raw_val = try self.generateExpr(field_expr_id);
            const field_layout = ls.getStructFieldLayout(stored_layout.getStruct().idx, @intCast(i));
            field_values_buf[i] = try self.convertToFieldType(raw_val, field_layout);
        }

        const struct_type = try buildStructTypeFromValues(builder, wip, field_values_buf[0..field_count]);
        var struct_val = builder.poisonValue(struct_type) catch return error.OutOfMemory;
        for (0..field_count) |i| {
            struct_val = wip.insertValue(struct_val, field_values_buf[i], &.{@intCast(i)}, "") catch return error.CompilationFailed;
        }

        return struct_val;
    }

    fn generateStructAccess(self: *MonoLlvmCodeGen, access: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        const struct_val = try self.generateExpr(access.struct_expr);
        if (struct_val == .none) return error.CompilationFailed;
        if (!struct_val.typeOfWip(wip).isStruct(builder)) return error.CompilationFailed;

        var val = wip.extractValue(struct_val, &.{@intCast(access.field_idx)}, "") catch return error.CompilationFailed;
        if (access.field_layout == .bool and val.typeOfWip(wip) == .i8) {
            val = wip.cast(.trunc, val, .i1, "") catch return error.CompilationFailed;
        }

        return val;
    }

    fn generateCellLoad(self: *MonoLlvmCodeGen, cell: Symbol, layout_idx: layout.Idx) Error!LlvmBuilder.Value {
        const key: u64 = @bitCast(cell);
        if (self.cell_allocas.get(key)) |cell_alloca| {
            const wip = self.wip orelse return error.CompilationFailed;
            return wip.load(.normal, cell_alloca.elem_type, cell_alloca.alloca_ptr, self.alignmentForLayout(layout_idx), "") catch return error.CompilationFailed;
        }

        if (self.loop_var_allocas.get(key)) |lva| {
            const wip = self.wip orelse return error.CompilationFailed;
            const alignment = LlvmBuilder.Alignment.fromByteUnits(@intCast(@max(llvmTypeByteSize(lva.elem_type), 1)));
            return wip.load(.normal, lva.elem_type, lva.alloca_ptr, alignment, "") catch return error.CompilationFailed;
        }

        return self.generateLookup(cell, layout_idx);
    }

    // Tag union generation

    /// Get the LLVM integer type for a discriminant size.
    fn discriminantIntType(disc_size: u8) LlvmBuilder.Type {
        return switch (disc_size) {
            1 => .i8,
            2 => .i16,
            4 => .i32,
            8 => .i64,
            else => .i8,
        };
    }

    /// Generate a zero-argument tag (just the discriminant value).
    fn generateZeroArgTag(self: *MonoLlvmCodeGen, zat: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ls = self.layout_store orelse unreachable;

        const stored_layout = ls.getLayout(zat.union_layout);

        switch (stored_layout.tag) {
            .scalar => {
                // Tag union with no payloads (e.g., Bool, Color) → just the discriminant integer
                const llvm_type = layoutToLlvmType(zat.union_layout);
                return (builder.intConst(llvm_type, @as(u64, zat.discriminant)) catch return error.OutOfMemory).toValue();
            },
            .zst => {
                // Zero-sized tag union
                return (builder.intConst(.i8, 0) catch return error.OutOfMemory).toValue();
            },
            .tag_union => {
                const tu_data = ls.getTagUnionData(stored_layout.getTagUnion().idx);
                const variants = ls.getTagUnionVariants(tu_data);
                var has_payloads = false;
                for (0..variants.len) |variant_idx| {
                    if (variants.get(@intCast(variant_idx)).payload_layout != .zst) {
                        has_payloads = true;
                        break;
                    }
                }

                if (!has_payloads) {
                    return (builder.intConst(.i64, @as(u64, zat.discriminant)) catch return error.OutOfMemory).toValue();
                }

                const tu_size = tu_data.size;
                const tu_align_bytes: u64 = @intCast(stored_layout.getTagUnion().alignment.toByteUnits());
                const min_align: u64 = @max(tu_align_bytes, 8);
                const alignment = LlvmBuilder.Alignment.fromByteUnits(min_align);
                const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
                const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
                const size_val_i64 = builder.intValue(.i64, tu_size) catch return error.OutOfMemory;
                const align_val = builder.intValue(.i32, @as(u32, @intCast(min_align))) catch return error.OutOfMemory;
                const refcounted_val = builder.intValue(.i1, 0) catch return error.OutOfMemory;
                const heap_ptr = try self.callBuiltin(
                    "roc_builtins_allocate_with_refcount",
                    ptr_type,
                    &.{ .i64, .i32, .i1, ptr_type },
                    &.{ size_val_i64, align_val, refcounted_val, roc_ops },
                );

                // Zero the memory
                const zero_val = builder.intValue(.i8, 0) catch return error.OutOfMemory;
                const size_val = builder.intValue(.i32, tu_size) catch return error.OutOfMemory;
                _ = wip.callMemSet(heap_ptr, alignment, zero_val, size_val, .normal, false) catch return error.OutOfMemory;

                // Store discriminant at discriminant_offset
                const disc_offset = tu_data.discriminant_offset;
                const disc_type = discriminantIntType(tu_data.discriminant_size);
                const disc_val = builder.intValue(disc_type, @as(u64, zat.discriminant)) catch return error.OutOfMemory;
                const disc_ptr = wip.gep(.inbounds, .i8, heap_ptr, &.{builder.intValue(.i32, disc_offset) catch return error.OutOfMemory}, "") catch return error.OutOfMemory;
                _ = wip.store(.normal, disc_val, disc_ptr, LlvmBuilder.Alignment.fromByteUnits(@as(u64, tu_data.discriminant_size))) catch return error.CompilationFailed;

                return heap_ptr;
            },
            .closure, .struct_, .list, .list_of_zst, .box, .box_of_zst => unreachable,
        }
    }

    /// Generate a tag with payload arguments.
    fn generateTagWithPayload(self: *MonoLlvmCodeGen, tag_expr: anytype) Error!LlvmBuilder.Value {
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        defer self.out_ptr = saved_out_ptr;

        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ls = self.layout_store orelse unreachable;

        const stored_layout = ls.getLayout(tag_expr.union_layout);
        std.debug.assert(stored_layout.tag == .tag_union);

        const tu_data = ls.getTagUnionData(stored_layout.getTagUnion().idx);
        const tu_size = tu_data.size;
        const tu_align_bytes: u64 = @intCast(stored_layout.getTagUnion().alignment.toByteUnits());
        // Heap-allocate the tag union so returned pointer values remain valid after
        // the current function returns.
        const min_align: u64 = @max(tu_align_bytes, 8);
        const padded_size: u32 = @intCast((@as(u64, tu_size) + min_align - 1) / min_align * min_align);
        const forced_alignment = LlvmBuilder.Alignment.fromByteUnits(min_align);
        const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
        const size_val_i64 = builder.intValue(.i64, padded_size) catch return error.OutOfMemory;
        const align_val = builder.intValue(.i32, @as(u32, @intCast(min_align))) catch return error.OutOfMemory;
        const refcounted_val = builder.intValue(.i1, 0) catch return error.OutOfMemory;
        const heap_ptr = try self.callBuiltin(
            "roc_builtins_allocate_with_refcount",
            ptr_type,
            &.{ .i64, .i32, .i1, ptr_type },
            &.{ size_val_i64, align_val, refcounted_val, roc_ops },
        );
    }
};
