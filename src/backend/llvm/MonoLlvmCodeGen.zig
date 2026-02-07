//! Mono IR to LLVM Code Generator
//!
//! This module generates LLVM IR from Mono IR expressions.
//! It uses Zig's std.zig.llvm.Builder for IR generation.
//!
//! Pipeline position:
//! ```
//! CIR -> Mono IR Lowering -> MonoLlvmCodeGen -> LLVM Bitcode -> Native Code
//! ```
//!
//! Key properties:
//! - Consumes the same Mono IR as the dev backend
//! - Generates LLVM IR via Zig's llvm.Builder
//! - Produces bitcode that can be compiled to native code via LLVM

const std = @import("std");
const builtin = @import("builtin");
const layout = @import("layout");
const mono = @import("mono");

const LlvmBuilder = std.zig.llvm.Builder;

const MonoExprStore = mono.MonoExprStore;
const MonoExprId = mono.MonoExprId;
const MonoPatternId = mono.MonoPatternId;
const MonoSymbol = mono.MonoSymbol;
const MonoProc = mono.MonoProc;
const CFStmtId = mono.CFStmtId;

const Allocator = std.mem.Allocator;

/// Get the LLVM target triple for the current platform.
fn getLlvmTriple() []const u8 {
    const arch = switch (builtin.cpu.arch) {
        .x86_64 => "x86_64",
        .aarch64 => "aarch64",
        .x86 => "i686",
        .arm, .armeb => "arm",
        .thumb, .thumbeb => "thumb",
        .wasm32 => "wasm32",
        .wasm64 => "wasm64",
        .riscv32 => "riscv32",
        .riscv64 => "riscv64",
        else => "unknown",
    };

    const vendor_os = switch (builtin.os.tag) {
        .windows => "-w64-windows",
        .macos => "-apple-darwin",
        .ios => "-apple-ios",
        .linux => "-unknown-linux",
        .freebsd => "-unknown-freebsd",
        .openbsd => "-unknown-openbsd",
        .netbsd => "-unknown-netbsd",
        .freestanding => "-unknown-unknown",
        .wasi => "-wasi",
        else => "-unknown-unknown",
    };

    const abi = switch (builtin.os.tag) {
        .windows => "-gnu",
        .linux => switch (builtin.abi) {
            .musleabihf => "-musleabihf",
            .gnueabihf => "-gnueabihf",
            .musleabi => "-musleabi",
            .gnueabi => "-gnueabi",
            .musl => "-musl",
            .gnu => "-gnu",
            .android => "-android",
            else => "-gnu",
        },
        .freestanding, .wasi => "",
        else => "",
    };

    return arch ++ vendor_os ++ abi;
}

/// LLVM code generator for Mono IR expressions
pub const MonoLlvmCodeGen = struct {
    allocator: Allocator,

    /// The Mono IR store containing expressions to compile
    store: *const MonoExprStore,

    /// Map from MonoSymbol to LLVM value
    symbol_values: std.AutoHashMap(u48, LlvmBuilder.Value),

    /// Map from MonoSymbol to lambda expression ID (for callable bindings)
    lambda_bindings: std.AutoHashMap(u48, MonoExprId),

    /// Registry of compiled procedures (symbol -> function index)
    proc_registry: std.AutoHashMap(u48, LlvmBuilder.Function.Index),

    /// Join point blocks (join point id -> block index)
    join_points: std.AutoHashMap(u32, LlvmBuilder.Function.Block.Index),

    /// Join point parameters (join point id -> parameter patterns)
    join_point_params: std.AutoHashMap(u32, []MonoPatternId),

    /// Current LLVM builder (set during code generation)
    builder: ?*LlvmBuilder = null,

    /// Current WIP function (set during code generation)
    wip: ?*LlvmBuilder.WipFunction = null,

    /// The roc_ops argument passed to roc_eval (second parameter)
    roc_ops_arg: ?LlvmBuilder.Value = null,

    /// Address of Dec multiply builtin (mulSaturatedC). Set by the evaluator.
    dec_mul_addr: usize = 0,

    /// Address of Dec divide builtin (divC). Set by the evaluator.
    dec_div_addr: usize = 0,

    /// Address of Dec truncating divide builtin (divTruncC). Set by the evaluator.
    dec_div_trunc_addr: usize = 0,

    /// Address of allocateWithRefcountC builtin. Set by the evaluator.
    alloc_with_refcount_addr: usize = 0,

    /// Layout store for resolving composite type layouts (records, tuples).
    /// Set by the evaluator before calling generateCode.
    layout_store: ?*const layout.Store = null,

    /// Output pointer for the top-level expression result.
    /// Set during generateCode so that string/composite generators can
    /// store data directly to the output buffer, avoiding LLVM constant
    /// pool references to .rodata (which we don't extract from the object file).
    out_ptr: ?LlvmBuilder.Value = null,

    /// Block for early returns — branches here skip the rest of the function.
    /// The block contains retVoid since the value is already stored to out_ptr.
    early_return_block: ?LlvmBuilder.Function.Block.Index = null,

    /// Result layout for the top-level expression (needed by early_return).
    result_layout: ?layout.Idx = null,

    /// Result of bitcode generation
    pub const BitcodeResult = struct {
        bitcode: []const u32,
        result_layout: layout.Idx,
        allocator: Allocator,

        pub fn deinit(self: *BitcodeResult) void {
            self.allocator.free(self.bitcode);
        }
    };

    /// Errors that can occur during code generation
    pub const Error = error{
        OutOfMemory,
        UnsupportedExpression,
        CompilationFailed,
    };

    /// Initialize the code generator
    pub fn init(
        allocator: Allocator,
        store: *const MonoExprStore,
    ) MonoLlvmCodeGen {
        return .{
            .allocator = allocator,
            .store = store,
            .symbol_values = std.AutoHashMap(u48, LlvmBuilder.Value).init(allocator),
            .lambda_bindings = std.AutoHashMap(u48, MonoExprId).init(allocator),
            .proc_registry = std.AutoHashMap(u48, LlvmBuilder.Function.Index).init(allocator),
            .join_points = std.AutoHashMap(u32, LlvmBuilder.Function.Block.Index).init(allocator),
            .join_point_params = std.AutoHashMap(u32, []MonoPatternId).init(allocator),
        };
    }

    /// Clean up resources
    pub fn deinit(self: *MonoLlvmCodeGen) void {
        self.symbol_values.deinit();
        self.lambda_bindings.deinit();
        self.proc_registry.deinit();
        self.join_points.deinit();
        // Free allocated param slices
        var it = self.join_point_params.valueIterator();
        while (it.next()) |params| {
            self.allocator.free(params.*);
        }
        self.join_point_params.deinit();
    }

    /// Reset the code generator for a new expression
    pub fn reset(self: *MonoLlvmCodeGen) void {
        self.symbol_values.clearRetainingCapacity();
        self.lambda_bindings.clearRetainingCapacity();
        self.proc_registry.clearRetainingCapacity();
        self.join_points.clearRetainingCapacity();
        // Free allocated param slices before clearing
        var it = self.join_point_params.valueIterator();
        while (it.next()) |params| {
            self.allocator.free(params.*);
        }
        self.join_point_params.clearRetainingCapacity();
    }

    /// Generate LLVM bitcode for a Mono IR expression
    pub fn generateCode(
        self: *MonoLlvmCodeGen,
        expr_id: MonoExprId,
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

        // Determine the LLVM type for the result
        const value_type = layoutToLlvmType(result_layout);

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
            .strip = false,
        }) catch return error.OutOfMemory;
        defer wip.deinit();

        self.wip = &wip;
        defer self.wip = null;

        const entry_block = wip.block(0, "entry") catch return error.OutOfMemory;
        wip.cursor = .{ .block = entry_block };

        // Get the output pointer and roc_ops pointer
        const out_ptr = wip.arg(0);
        self.out_ptr = out_ptr;
        defer self.out_ptr = null;
        self.roc_ops_arg = wip.arg(1);
        defer self.roc_ops_arg = null;

        // Store result layout for early_return
        self.result_layout = result_layout;
        defer self.result_layout = null;

        // Create the early return block (used by early_return expressions)
        const early_ret_block = wip.block(0, "early_ret") catch return error.OutOfMemory;
        self.early_return_block = early_ret_block;
        defer self.early_return_block = null;

        // Generate LLVM IR for the expression
        const value = self.generateExpr(expr_id) catch return error.UnsupportedExpression;

        // Store the result to the output pointer.
        // Some generators (e.g., string literals) write directly to out_ptr and
        // return .none as a sentinel — skip the storage step for those.
        if (value == .none) {
            // Result already written to out_ptr by the generator.
        } else {

        // For scalar types, extend to the canonical size (i64 for ints, i128 for wide ints).
        // For composite types (records, tuples), store the struct directly.
        const is_scalar = switch (result_layout) {
            .bool, .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64,
            .i128, .u128, .dec, .f32, .f64,
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

            const store_value = if (value_type == final_type)
                value
            else
                wip.conv(signedness, value, final_type, "") catch return error.CompilationFailed;

            const alignment = LlvmBuilder.Alignment.fromByteUnits(switch (final_type) {
                .i64 => 8,
                .i128 => 16,
                .float => 4,
                .double => 8,
                else => 0,
            });
            _ = wip.store(.normal, store_value, out_ptr, alignment) catch return error.CompilationFailed;
        } else {
            // Composite type (record, tuple, tag_union, str, etc.)
            if (result_layout == .str) {
                // String — alloca pointer, memcpy 24 bytes to out_ptr
                const copy_size = builder.intValue(.i32, roc_str_size) catch return error.OutOfMemory;
                const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                _ = wip.callMemCpy(out_ptr, alignment, value, alignment, copy_size, .normal, false) catch return error.OutOfMemory;
            } else {
                const ls = self.layout_store orelse return error.UnsupportedExpression;
                const stored_layout = ls.getLayout(result_layout);
                switch (stored_layout.tag) {
                    .tag_union => {
                        // Tag union — alloca pointer, memcpy to out_ptr
                        const tu_data = ls.getTagUnionData(stored_layout.data.tag_union.idx);
                        const copy_size = builder.intValue(.i32, tu_data.size) catch return error.OutOfMemory;
                        const tu_align: u64 = @intCast(stored_layout.data.tag_union.alignment.toByteUnits());
                        const alignment = LlvmBuilder.Alignment.fromByteUnits(tu_align);
                        _ = wip.callMemCpy(out_ptr, alignment, value, alignment, copy_size, .normal, false) catch return error.OutOfMemory;
                    },
                    else => {
                        // Record, tuple, etc. — store the LLVM struct value directly.
                        const align_bytes: u32 = switch (stored_layout.tag) {
                            .record => @intCast(stored_layout.data.record.alignment.toByteUnits()),
                            .tuple => @intCast(stored_layout.data.tuple.alignment.toByteUnits()),
                            .zst => 1,
                            else => return error.UnsupportedExpression,
                        };
                        const alignment = LlvmBuilder.Alignment.fromByteUnits(align_bytes);
                        _ = wip.store(.normal, value, out_ptr, alignment) catch return error.CompilationFailed;
                    },
                }
            }
        }
        } // end of value != .none else

        _ = wip.retVoid() catch return error.CompilationFailed;

        // Fill in the early return block (just ret void — value already stored to out_ptr)
        wip.cursor = .{ .block = early_ret_block };
        _ = wip.retVoid() catch return error.CompilationFailed;

        wip.finish() catch return error.CompilationFailed;

        // Serialize to bitcode
        const producer = LlvmBuilder.Producer{
            .name = "Roc Mono LLVM CodeGen",
            .version = .{ .major = 1, .minor = 0, .patch = 0 },
        };

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
    pub fn compileAllProcs(self: *MonoLlvmCodeGen, procs: []const MonoProc) Error!void {
        for (procs) |proc| {
            try self.compileProc(proc);
        }
    }

    fn compileProc(self: *MonoLlvmCodeGen, proc: MonoProc) Error!void {
        const builder = self.builder orelse return error.CompilationFailed;

        // Build the LLVM function type from arg_layouts and ret_layout
        const arg_layouts = self.store.getLayoutIdxSpan(proc.arg_layouts);
        var param_types: std.ArrayList(LlvmBuilder.Type) = .{};
        defer param_types.deinit(self.allocator);
        for (arg_layouts) |arg_layout| {
            param_types.append(self.allocator, layoutToLlvmType(arg_layout)) catch return error.OutOfMemory;
        }

        const ret_type = layoutToLlvmType(proc.ret_layout);
        const fn_type = builder.fnType(ret_type, param_types.items, .normal) catch return error.OutOfMemory;

        // Create a unique function name from the symbol
        const key: u48 = @bitCast(proc.name);
        var name_buf: [64]u8 = undefined;
        const name_str = std.fmt.bufPrint(&name_buf, "roc_proc_{d}", .{key}) catch return error.OutOfMemory;
        const fn_name = builder.strtabString(name_str) catch return error.OutOfMemory;

        const func = builder.addFunction(fn_type, fn_name, .default) catch return error.OutOfMemory;

        // Register in proc_registry BEFORE compiling body (for recursive calls)
        self.proc_registry.put(key, func) catch return error.OutOfMemory;

        // Save and restore outer wip state
        const outer_wip = self.wip;
        defer self.wip = outer_wip;

        // Create a new WipFunction for this procedure
        var proc_wip = LlvmBuilder.WipFunction.init(builder, .{
            .function = func,
            .strip = false,
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

        // Generate the body (control flow statements)
        self.generateStmt(proc.body) catch return error.CompilationFailed;

        proc_wip.finish() catch return error.CompilationFailed;
    }

    /// Convert layout to LLVM type
    fn layoutToLlvmType(result_layout: layout.Idx) LlvmBuilder.Type {
        return switch (result_layout) {
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

    // ---------------------------------------------------------------
    // Control Flow Statement generation (mirrors dev backend's generateStmt)
    // ---------------------------------------------------------------

    fn generateStmt(self: *MonoLlvmCodeGen, stmt_id: CFStmtId) Error!void {
        const stmt = self.store.getCFStmt(stmt_id);

        switch (stmt) {
            .let_stmt => |let_s| {
                // Check if the value is a lambda/closure - if so, bind it for later call resolution
                const value_expr = self.store.getExpr(let_s.value);
                switch (value_expr) {
                    .lambda, .closure => {
                        try self.bindLambdaPattern(let_s.pattern, let_s.value);
                    },
                    else => {},
                }
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

                // Store join point parameters for rebinding on jumps
                const jp_key = @intFromEnum(j.id);
                const params = self.store.getPatternSpan(j.params);
                const params_copy = self.allocator.dupe(MonoPatternId, params) catch return error.OutOfMemory;
                self.join_point_params.put(jp_key, params_copy) catch return error.OutOfMemory;

                // Create a block for the join point body
                // Use incoming=1 as a minimum (will be incremented by branches)
                const join_block = wip.block(2, "join") catch return error.CompilationFailed;
                self.join_points.put(jp_key, join_block) catch return error.OutOfMemory;

                // Generate the remainder first (code that jumps TO join point)
                try self.generateStmt(j.remainder);

                // Now generate the join point body
                wip.cursor = .{ .block = join_block };
                try self.generateStmt(j.body);
            },
            .jump => |jmp| {
                const wip = self.wip orelse return error.CompilationFailed;
                const jp_key = @intFromEnum(jmp.target);

                // Evaluate all arguments and rebind join point parameters
                const args = self.store.getExprSpan(jmp.args);
                const params = self.join_point_params.get(jp_key);
                for (args, 0..) |arg_id, i| {
                    const val = try self.generateExpr(arg_id);
                    if (params) |p| {
                        if (i < p.len) {
                            try self.bindPattern(p[i], val);
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

        // Create blocks for each branch and the default
        const default_block = wip.block(0, "switch_default") catch return error.CompilationFailed;

        var switch_inst = wip.@"switch"(cond_val, default_block, @intCast(branches.len), .none) catch return error.CompilationFailed;

        var branch_blocks: std.ArrayList(LlvmBuilder.Function.Block.Index) = .{};
        defer branch_blocks.deinit(self.allocator);

        for (branches) |branch| {
            const branch_block = wip.block(0, "switch_case") catch return error.CompilationFailed;
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

    // ---------------------------------------------------------------
    // Expression generation
    // ---------------------------------------------------------------

    /// Generate LLVM IR for an expression
    fn generateExpr(self: *MonoLlvmCodeGen, expr_id: MonoExprId) Error!LlvmBuilder.Value {
        const expr = self.store.getExpr(expr_id);

        return switch (expr) {
            // Literals
            .i64_literal => |val| self.emitI64(val),
            .i128_literal => |val| self.emitI128(val),
            .f64_literal => |val| self.emitF64(val),
            .f32_literal => |val| self.emitF32(val),
            .dec_literal => |val| self.emitI128(val),
            .bool_literal => |val| self.emitBool(val),

            // Lookups
            .lookup => |lookup| self.generateLookup(lookup.symbol, lookup.layout_idx),

            // Binary operations
            .binop => |binop| self.generateBinop(binop),

            // Unary operations
            .unary_minus => |unary| self.generateUnaryMinus(unary),
            .unary_not => |unary| self.generateUnaryNot(unary),

            // Control flow
            .if_then_else => |ite| self.generateIfThenElse(ite),

            // Blocks
            .block => |block| self.generateBlock(block),

            // Function calls and lambdas
            .call => |call| self.generateCall(call),
            .lambda => |lambda| self.generateLambda(lambda),
            .closure => |closure| self.generateClosure(closure),

            // Records and tuples
            .record => |rec| self.generateRecord(rec),
            .empty_record => self.generateEmptyRecord(),
            .field_access => |fa| self.generateFieldAccess(fa),
            .tuple => |tup| self.generateTuple(tup),
            .tuple_access => |ta| self.generateTupleAccess(ta),

            // Tag unions
            .zero_arg_tag => |zat| self.generateZeroArgTag(zat),
            .tag => |t| self.generateTagWithPayload(t),
            .discriminant_switch => |ds| self.generateDiscriminantSwitch(ds),

            // Strings
            .str_literal => |str_idx| self.generateStrLiteral(str_idx),

            // Nominal wrappers are transparent
            .nominal => |nom| self.generateExpr(nom.backing_expr),

            // Pattern matching
            .when => |w| self.generateWhen(w),

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
            .runtime_error => self.generateRuntimeError(),
            .crash => self.generateRuntimeError(),

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

            else => error.UnsupportedExpression,
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

    fn generateLookup(self: *MonoLlvmCodeGen, symbol: MonoSymbol, _: layout.Idx) Error!LlvmBuilder.Value {
        const symbol_key: u48 = @bitCast(symbol);

        // Check if we have a value for this symbol
        if (self.symbol_values.get(symbol_key)) |val| {
            return val;
        }

        // Check if it's a lambda binding that needs to be evaluated
        if (self.lambda_bindings.get(symbol_key)) |lambda_expr_id| {
            const val = try self.generateExpr(lambda_expr_id);
            self.symbol_values.put(symbol_key, val) catch return error.OutOfMemory;
            return val;
        }

        // Check if it's a top-level definition
        if (self.store.getSymbolDef(symbol)) |def_expr_id| {
            const val = try self.generateExpr(def_expr_id);
            self.symbol_values.put(symbol_key, val) catch return error.OutOfMemory;
            return val;
        }

        return error.UnsupportedExpression;
    }

    fn generateBinop(self: *MonoLlvmCodeGen, binop: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;

        var lhs = try self.generateExpr(binop.lhs);
        var rhs = try self.generateExpr(binop.rhs);

        // Check if operands are aggregate types (structs) - LLVM can't compare them directly
        if (!isIntType(lhs.typeOfWip(wip)) and lhs.typeOfWip(wip) != .float and lhs.typeOfWip(wip) != .double) {
            return error.UnsupportedExpression;
        }

        // For comparison operations, result_layout is .bool, so check operand type instead.
        const operand_layout = self.getExprResultLayout(binop.lhs) orelse binop.result_layout;
        const is_float = isFloatLayout(binop.result_layout) or isFloatLayout(operand_layout);

        // When result is float but operands may be integer (e.g. `b : F32; b = 2`
        // lowers the literal as i64), cast operands to the target float type.
        // Check actual LLVM types to detect mismatches from lookups of int-valued symbols.
        if (is_float) {
            const float_layout = if (isFloatLayout(binop.result_layout)) binop.result_layout else operand_layout;
            const target_type = layoutToLlvmType(float_layout);
            const lhs_type = lhs.typeOfWip(wip);
            const rhs_type = rhs.typeOfWip(wip);
            if (lhs_type != target_type and lhs_type != .float and lhs_type != .double) {
                const lhs_layout = self.getExprResultLayout(binop.lhs) orelse binop.result_layout;
                lhs = wip.cast(if (isSigned(lhs_layout)) .sitofp else .uitofp, lhs, target_type, "") catch return error.CompilationFailed;
            }
            if (rhs_type != target_type and rhs_type != .float and rhs_type != .double) {
                const rhs_layout = self.getExprResultLayout(binop.rhs) orelse binop.result_layout;
                rhs = wip.cast(if (isSigned(rhs_layout)) .sitofp else .uitofp, rhs, target_type, "") catch return error.CompilationFailed;
            }
        }

        // Align integer operand widths (e.g., i64 vs i128 from Dec literals)
        if (!is_float) {
            const lhs_type = lhs.typeOfWip(wip);
            const rhs_type = rhs.typeOfWip(wip);
            if (isIntType(lhs_type) and isIntType(rhs_type) and lhs_type != rhs_type) {
                const lhs_bits = intTypeBits(lhs_type);
                const rhs_bits = intTypeBits(rhs_type);
                if (lhs_bits < rhs_bits) {
                    const lhs_layout = self.getExprResultLayout(binop.lhs) orelse binop.result_layout;
                    lhs = wip.cast(if (isSigned(lhs_layout)) .sext else .zext, lhs, rhs_type, "") catch return error.CompilationFailed;
                } else {
                    const rhs_layout = self.getExprResultLayout(binop.rhs) orelse binop.result_layout;
                    rhs = wip.cast(if (isSigned(rhs_layout)) .sext else .zext, rhs, lhs_type, "") catch return error.CompilationFailed;
                }
            }
        }

        return switch (binop.op) {
            .add => if (is_float)
                wip.bin(.fadd, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.bin(.add, lhs, rhs, "") catch return error.CompilationFailed,

            .sub => if (is_float)
                wip.bin(.fsub, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.bin(.sub, lhs, rhs, "") catch return error.CompilationFailed,

            .mul => if (is_float)
                wip.bin(.fmul, lhs, rhs, "") catch return error.CompilationFailed
            else if (binop.result_layout == .dec)
                self.callDecMul(lhs, rhs) catch return error.CompilationFailed
            else
                wip.bin(.mul, lhs, rhs, "") catch return error.CompilationFailed,

            .div => if (is_float)
                wip.bin(.fdiv, lhs, rhs, "") catch return error.CompilationFailed
            else if (binop.result_layout == .dec)
                self.callDecDiv(lhs, rhs) catch return error.CompilationFailed
            else if (isSigned(binop.result_layout))
                wip.bin(.sdiv, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.bin(.udiv, lhs, rhs, "") catch return error.CompilationFailed,

            .div_trunc => if (is_float)
                wip.bin(.fdiv, lhs, rhs, "") catch return error.CompilationFailed
            else if (binop.result_layout == .dec)
                self.callDecDivTrunc(lhs, rhs) catch return error.CompilationFailed
            else if (isSigned(binop.result_layout))
                wip.bin(.sdiv, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.bin(.udiv, lhs, rhs, "") catch return error.CompilationFailed,

            .mod => if (is_float)
                wip.bin(.frem, lhs, rhs, "") catch return error.CompilationFailed
            else if (isSigned(binop.result_layout))
                wip.bin(.srem, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.bin(.urem, lhs, rhs, "") catch return error.CompilationFailed,

            .eq => if (is_float)
                wip.fcmp(.normal, .oeq, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.icmp(.eq, lhs, rhs, "") catch return error.CompilationFailed,

            .neq => if (is_float)
                wip.fcmp(.normal, .one, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.icmp(.ne, lhs, rhs, "") catch return error.CompilationFailed,

            .lt => if (is_float)
                wip.fcmp(.normal, .olt, lhs, rhs, "") catch return error.CompilationFailed
            else if (isSigned(operand_layout))
                wip.icmp(.slt, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.icmp(.ult, lhs, rhs, "") catch return error.CompilationFailed,

            .lte => if (is_float)
                wip.fcmp(.normal, .ole, lhs, rhs, "") catch return error.CompilationFailed
            else if (isSigned(operand_layout))
                wip.icmp(.sle, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.icmp(.ule, lhs, rhs, "") catch return error.CompilationFailed,

            .gt => if (is_float)
                wip.fcmp(.normal, .ogt, lhs, rhs, "") catch return error.CompilationFailed
            else if (isSigned(operand_layout))
                wip.icmp(.sgt, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.icmp(.ugt, lhs, rhs, "") catch return error.CompilationFailed,

            .gte => if (is_float)
                wip.fcmp(.normal, .oge, lhs, rhs, "") catch return error.CompilationFailed
            else if (isSigned(operand_layout))
                wip.icmp(.sge, lhs, rhs, "") catch return error.CompilationFailed
            else
                wip.icmp(.uge, lhs, rhs, "") catch return error.CompilationFailed,

            .@"and" => wip.bin(.@"and", lhs, rhs, "") catch return error.CompilationFailed,
            .@"or" => wip.bin(.@"or", lhs, rhs, "") catch return error.CompilationFailed,
        };
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
    fn layoutToStructFieldType(field_layout: layout.Idx) LlvmBuilder.Type {
        return switch (field_layout) {
            .bool => .i8,
            else => layoutToLlvmType(field_layout),
        };
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
        const wip = self.wip orelse return error.CompilationFailed;
        const target_type = layoutToStructFieldType(field_layout);
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

    // ---------------------------------------------------------------
    // Record and tuple generation
    // ---------------------------------------------------------------

    fn generateEmptyRecord(self: *MonoLlvmCodeGen) Error!LlvmBuilder.Value {
        const builder = self.builder orelse return error.CompilationFailed;
        return (builder.intConst(.i8, 0) catch return error.OutOfMemory).toValue();
    }

    fn generateRecord(self: *MonoLlvmCodeGen, rec: anytype) Error!LlvmBuilder.Value {
        // Clear out_ptr — record field values must not write to the output buffer.
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        defer self.out_ptr = saved_out_ptr;

        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const ls = self.layout_store orelse return error.UnsupportedExpression;

        const stored_layout = ls.getLayout(rec.record_layout);
        if (stored_layout.tag != .record) return error.UnsupportedExpression;

        const record_data = ls.getRecordData(stored_layout.data.record.idx);
        const field_count = record_data.getFields().count;

        if (field_count == 0) {
            return self.generateEmptyRecord();
        }

        // Get field expressions (already in sorted order from the lowerer)
        const field_exprs = self.store.getExprSpan(rec.fields);

        // Generate field values and convert to their expected types
        var field_values_buf: [32]LlvmBuilder.Value = undefined;

        for (field_exprs, 0..) |field_expr_id, i| {
            const raw_val = try self.generateExpr(field_expr_id);
            const field_layout = ls.getRecordFieldLayout(stored_layout.data.record.idx, @intCast(i));
            field_values_buf[i] = try self.convertToFieldType(raw_val, field_layout);
        }

        // Create LLVM struct type from actual generated value types
        const struct_type = try buildStructTypeFromValues(builder, wip, field_values_buf[0..field_count]);

        // Build struct value using insertvalue (types match by construction)
        var struct_val = builder.poisonValue(struct_type) catch return error.OutOfMemory;
        for (0..field_count) |i| {
            struct_val = wip.insertValue(struct_val, field_values_buf[i], &.{@intCast(i)}, "") catch return error.CompilationFailed;
        }

        return struct_val;
    }

    fn generateFieldAccess(self: *MonoLlvmCodeGen, access: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;

        // Generate the record value
        const record_val = try self.generateExpr(access.record_expr);

        // Extract the field at the sorted index
        var val = wip.extractValue(record_val, &.{@intCast(access.field_idx)}, "") catch return error.CompilationFailed;

        // Truncate i8 back to i1 for bool fields
        if (access.field_layout == .bool and val.typeOfWip(wip) == .i8) {
            val = wip.cast(.trunc, val, .i1, "") catch return error.CompilationFailed;
        }

        return val;
    }

    fn generateTuple(self: *MonoLlvmCodeGen, tup: anytype) Error!LlvmBuilder.Value {
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        defer self.out_ptr = saved_out_ptr;

        const builder = self.builder orelse return error.CompilationFailed;
        const wip = self.wip orelse return error.CompilationFailed;
        const ls = self.layout_store orelse return error.UnsupportedExpression;

        const stored_layout = ls.getLayout(tup.tuple_layout);
        if (stored_layout.tag != .tuple) return error.UnsupportedExpression;

        const tuple_data = ls.getTupleData(stored_layout.data.tuple.idx);
        const sorted_elements = ls.tuple_fields.sliceRange(tuple_data.getFields());
        const elem_count = sorted_elements.len;

        if (elem_count == 0) {
            return self.generateEmptyRecord();
        }

        // Get element expressions (in source/original order)
        const elem_exprs = self.store.getExprSpan(tup.elems);

        // Build sorted-position-to-value mapping.
        // elem_exprs[i] has original index i; we need to find its sorted position.
        var sorted_values: [32]LlvmBuilder.Value = undefined;
        var sorted_layouts: [32]layout.Idx = undefined;

        // First, fill in sorted layouts from the layout store
        for (0..elem_count) |sorted_i| {
            const element = sorted_elements.get(@intCast(sorted_i));
            sorted_layouts[sorted_i] = element.layout;
        }

        // Generate each source-order element, convert to field type, and map to sorted position
        for (elem_exprs, 0..) |elem_expr_id, original_i| {
            const raw_val = try self.generateExpr(elem_expr_id);
            // Find sorted position for this original index
            var sorted_pos: usize = 0;
            for (0..elem_count) |si| {
                const element = sorted_elements.get(@intCast(si));
                if (element.index == original_i) {
                    sorted_pos = si;
                    break;
                }
            }
            sorted_values[sorted_pos] = try self.convertToFieldType(raw_val, sorted_layouts[sorted_pos]);
        }

        // Create LLVM struct type from actual generated value types
        const struct_type = try buildStructTypeFromValues(builder, wip, sorted_values[0..elem_count]);

        // Build struct value using insertvalue (types match by construction)
        var struct_val = builder.poisonValue(struct_type) catch return error.OutOfMemory;
        for (0..elem_count) |i| {
            struct_val = wip.insertValue(struct_val, sorted_values[i], &.{@intCast(i)}, "") catch return error.CompilationFailed;
        }

        return struct_val;
    }

    fn generateTupleAccess(self: *MonoLlvmCodeGen, access: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;

        // Generate the tuple value
        const tuple_val = try self.generateExpr(access.tuple_expr);

        // Extract the element at the sorted index
        var val = wip.extractValue(tuple_val, &.{@intCast(access.elem_idx)}, "") catch return error.CompilationFailed;

        // Truncate i8 back to i1 for bool fields
        if (access.elem_layout == .bool and val.typeOfWip(wip) == .i8) {
            val = wip.cast(.trunc, val, .i1, "") catch return error.CompilationFailed;
        }

        return val;
    }

    // ---------------------------------------------------------------
    // Tag union generation
    // ---------------------------------------------------------------

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
        const ls = self.layout_store orelse return error.UnsupportedExpression;

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
                // Tag union with payloads — need alloca for the full union
                const tu_data = ls.getTagUnionData(stored_layout.data.tag_union.idx);
                const tu_size = tu_data.size;
                const tu_align_bytes: u64 = @intCast(stored_layout.data.tag_union.alignment.toByteUnits());
                const alignment = LlvmBuilder.Alignment.fromByteUnits(tu_align_bytes);

                // Alloca the tag union
                const alloca_ptr = wip.alloca(.normal, .i8, builder.intValue(.i32, tu_size) catch return error.OutOfMemory, alignment, .default, "") catch return error.OutOfMemory;

                // Zero the memory
                const zero_val = builder.intValue(.i8, 0) catch return error.OutOfMemory;
                const size_val = builder.intValue(.i32, tu_size) catch return error.OutOfMemory;
                _ = wip.callMemSet(alloca_ptr, alignment, zero_val, size_val, .normal, false) catch return error.OutOfMemory;

                // Store discriminant at discriminant_offset
                const disc_offset = tu_data.discriminant_offset;
                const disc_type = discriminantIntType(tu_data.discriminant_size);
                const disc_val = builder.intValue(disc_type, @as(u64, zat.discriminant)) catch return error.OutOfMemory;
                const disc_ptr = wip.gep(.inbounds, .i8, alloca_ptr, &.{builder.intValue(.i32, disc_offset) catch return error.OutOfMemory}, "") catch return error.OutOfMemory;
                _ = wip.store(.normal, disc_val, disc_ptr, LlvmBuilder.Alignment.fromByteUnits(@as(u64, tu_data.discriminant_size))) catch return error.CompilationFailed;

                return alloca_ptr;
            },
            else => return error.UnsupportedExpression,
        }
    }

    /// Generate a tag with payload arguments.
    fn generateTagWithPayload(self: *MonoLlvmCodeGen, tag_expr: anytype) Error!LlvmBuilder.Value {
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        defer self.out_ptr = saved_out_ptr;

        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ls = self.layout_store orelse return error.UnsupportedExpression;

        const stored_layout = ls.getLayout(tag_expr.union_layout);
        if (stored_layout.tag != .tag_union) return error.UnsupportedExpression;

        const tu_data = ls.getTagUnionData(stored_layout.data.tag_union.idx);
        const tu_size = tu_data.size;
        const tu_align_bytes: u64 = @intCast(stored_layout.data.tag_union.alignment.toByteUnits());
        const alignment = LlvmBuilder.Alignment.fromByteUnits(tu_align_bytes);

        // Alloca the tag union
        const alloca_ptr = wip.alloca(.normal, .i8, builder.intValue(.i32, tu_size) catch return error.OutOfMemory, alignment, .default, "") catch return error.OutOfMemory;

        // Zero the memory
        const zero_val = builder.intValue(.i8, 0) catch return error.OutOfMemory;
        const size_val = builder.intValue(.i32, tu_size) catch return error.OutOfMemory;
        _ = wip.callMemSet(alloca_ptr, alignment, zero_val, size_val, .normal, false) catch return error.OutOfMemory;

        // Store payload arguments
        const arg_exprs = self.store.getExprSpan(tag_expr.args);
        const variants = ls.getTagUnionVariants(tu_data);
        const variant = variants.get(tag_expr.discriminant);

        if (arg_exprs.len == 1) {
            // Single argument — store directly at offset 0
            const arg_val = try self.generateExpr(arg_exprs[0]);
            _ = wip.store(.normal, arg_val, alloca_ptr, alignment) catch return error.CompilationFailed;
        } else if (arg_exprs.len > 1) {
            // Multiple arguments — payload is a tuple
            const payload_layout = ls.getLayout(variant.payload_layout);
            if (payload_layout.tag == .tuple) {
                const tuple_data = ls.getTupleData(payload_layout.data.tuple.idx);
                const sorted_elements = ls.tuple_fields.sliceRange(tuple_data.getFields());

                for (arg_exprs, 0..) |arg_expr_id, i| {
                    const arg_val = try self.generateExpr(arg_expr_id);
                    // Find the offset for this original-order argument
                    var offset: u32 = 0;
                    for (0..sorted_elements.len) |si| {
                        const elem = sorted_elements.get(@intCast(si));
                        if (elem.index == i) {
                            offset = ls.getTupleElementOffset(payload_layout.data.tuple.idx, @intCast(si));
                            break;
                        }
                    }
                    const field_ptr = wip.gep(.inbounds, .i8, alloca_ptr, &.{builder.intValue(.i32, offset) catch return error.OutOfMemory}, "") catch return error.OutOfMemory;
                    _ = wip.store(.normal, arg_val, field_ptr, .default) catch return error.CompilationFailed;
                }
            }
        }

        // Store discriminant at discriminant_offset
        const disc_offset = tu_data.discriminant_offset;
        const disc_type = discriminantIntType(tu_data.discriminant_size);
        const disc_val = builder.intValue(disc_type, @as(u64, tag_expr.discriminant)) catch return error.OutOfMemory;
        const disc_ptr = wip.gep(.inbounds, .i8, alloca_ptr, &.{builder.intValue(.i32, disc_offset) catch return error.OutOfMemory}, "") catch return error.OutOfMemory;
        _ = wip.store(.normal, disc_val, disc_ptr, LlvmBuilder.Alignment.fromByteUnits(@as(u64, tu_data.discriminant_size))) catch return error.CompilationFailed;

        return alloca_ptr;
    }

    /// Generate a discriminant switch — dispatch on a tag union's discriminant.
    fn generateDiscriminantSwitch(self: *MonoLlvmCodeGen, ds: anytype) Error!LlvmBuilder.Value {
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        defer self.out_ptr = saved_out_ptr;

        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ls = self.layout_store orelse return error.UnsupportedExpression;

        // Generate the value to switch on
        const value = try self.generateExpr(ds.value);

        // Get branch expressions
        const branch_exprs = self.store.getExprSpan(ds.branches);
        if (branch_exprs.len == 0) return error.UnsupportedExpression;

        // Extract discriminant based on layout type
        const stored_layout = ls.getLayout(ds.union_layout);
        const discriminant: LlvmBuilder.Value = switch (stored_layout.tag) {
            .scalar => blk: {
                // Scalar tag union — value IS the discriminant (just an integer)
                // May need truncation from i64 to the actual discriminant type
                const val_type = value.typeOfWip(wip);
                if (val_type == .i8 or val_type == .i1) break :blk value;
                // Truncate to i8 for comparison
                break :blk wip.cast(.trunc, value, .i8, "") catch return error.CompilationFailed;
            },
            .tag_union => blk: {
                // Tag union — load discriminant from pointer
                const tu_data = ls.getTagUnionData(stored_layout.data.tag_union.idx);
                const disc_type = discriminantIntType(tu_data.discriminant_size);
                const disc_offset = tu_data.discriminant_offset;
                const disc_ptr = wip.gep(.inbounds, .i8, value, &.{builder.intValue(.i32, disc_offset) catch return error.OutOfMemory}, "") catch return error.OutOfMemory;
                break :blk wip.load(.normal, disc_type, disc_ptr, LlvmBuilder.Alignment.fromByteUnits(@as(u64, tu_data.discriminant_size)), "") catch return error.OutOfMemory;
            },
            else => return error.UnsupportedExpression,
        };

        // Create basic blocks for each branch and the merge block
        const merge_block = wip.block(@intCast(branch_exprs.len), "ds_merge") catch return error.OutOfMemory;

        // Generate branches as a chain of compare-and-branch (like if-else-if)
        // For each discriminant value, check if it matches, and if so generate that branch.
        var result_vals: [64]LlvmBuilder.Value = undefined;
        var result_blocks: [64]LlvmBuilder.Function.Block.Index = undefined;
        var branch_count: usize = 0;

        for (branch_exprs, 0..) |branch_expr_id, i| {
            const is_last = (i == branch_exprs.len - 1);

            if (!is_last) {
                // Compare discriminant == i
                const disc_type = discriminant.typeOfWip(wip);
                const idx_val = builder.intValue(disc_type, @as(u64, @intCast(i))) catch return error.OutOfMemory;
                const cmp = wip.icmp(.eq, discriminant, idx_val, "") catch return error.OutOfMemory;

                const then_block = wip.block(0, "") catch return error.OutOfMemory;
                const else_block = wip.block(0, "") catch return error.OutOfMemory;
                _ = wip.brCond(cmp, then_block, else_block, .none) catch return error.OutOfMemory;

                // Then block — generate branch body
                wip.cursor = .{ .block = then_block };
                const branch_val = try self.generateExpr(branch_expr_id);
                _ = wip.br(merge_block) catch return error.OutOfMemory;
                result_vals[branch_count] = branch_val;
                result_blocks[branch_count] = wip.cursor.block;
                branch_count += 1;

                // Else block — continue to next comparison
                wip.cursor = .{ .block = else_block };
            } else {
                // Last branch — no comparison needed (default case)
                const branch_val = try self.generateExpr(branch_expr_id);
                _ = wip.br(merge_block) catch return error.OutOfMemory;
                result_vals[branch_count] = branch_val;
                result_blocks[branch_count] = wip.cursor.block;
                branch_count += 1;
            }
        }

        // Merge block with phi
        wip.cursor = .{ .block = merge_block };
        const result_type = result_vals[0].typeOfWip(wip);
        const phi_inst = wip.phi(result_type, "") catch return error.OutOfMemory;
        phi_inst.finish(
            result_vals[0..branch_count],
            result_blocks[0..branch_count],
            wip,
        );
        return phi_inst.toValue();
    }

    // ---------------------------------------------------------------
    // When/match expression generation
    // ---------------------------------------------------------------

    /// Generate a when/match expression.
    /// Evaluates the scrutinee, then checks each branch pattern sequentially.
    /// Unconditional patterns (wildcard, bind) go directly to the body.
    /// Conditional patterns (int_literal, tag) compare and branch.
    // ---------------------------------------------------------------
    // Loop generation
    // ---------------------------------------------------------------

    /// Generate a while loop: header checks condition, body executes, then loops back.
    /// Returns unit (i8 0).
    fn generateWhileLoop(self: *MonoLlvmCodeGen, wl: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        // Clear out_ptr for loop body
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        defer self.out_ptr = saved_out_ptr;

        // Create blocks: cond has 2 incoming (entry + back-edge), body/exit have 1 each
        const cond_block = wip.block(2, "while_cond") catch return error.OutOfMemory;
        const body_block = wip.block(1, "while_body") catch return error.OutOfMemory;
        const exit_block = wip.block(1, "while_exit") catch return error.OutOfMemory;

        // Branch to condition check
        _ = wip.br(cond_block) catch return error.CompilationFailed;

        // Condition block
        wip.cursor = .{ .block = cond_block };
        var cond_val = try self.generateExpr(wl.cond);
        if (cond_val.typeOfWip(wip) != .i1) {
            cond_val = wip.cast(.trunc, cond_val, .i1, "") catch return error.CompilationFailed;
        }
        _ = wip.brCond(cond_val, body_block, exit_block, .none) catch return error.CompilationFailed;

        // Body block
        wip.cursor = .{ .block = body_block };
        _ = try self.generateExpr(wl.body);
        _ = wip.br(cond_block) catch return error.CompilationFailed;

        // Exit block — return unit
        wip.cursor = .{ .block = exit_block };
        return builder.intValue(.i8, 0) catch return error.OutOfMemory;
    }

    /// Generate a for loop over a list: iterate elements, bind pattern, execute body.
    /// Returns unit (i8 0).
    fn generateForLoop(self: *MonoLlvmCodeGen, fl: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ls = self.layout_store orelse return error.UnsupportedExpression;

        // Get element size for pointer arithmetic
        const elem_layout_data = ls.getLayout(fl.elem_layout);
        const elem_sa = ls.layoutSizeAlign(elem_layout_data);
        const elem_size: u32 = elem_sa.size;

        // Materialize the list as a pointer so we can read ptr/len
        const list_ptr = try self.materializeAsPtr(fl.list_expr, 24);

        // Load list data pointer (offset 0) and length (offset 8)
        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
        const data_ptr = wip.load(.normal, ptr_type, list_ptr, alignment, "") catch return error.CompilationFailed;

        const len_off = builder.intValue(.i32, 8) catch return error.OutOfMemory;
        const len_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{len_off}, "") catch return error.CompilationFailed;
        const list_len = wip.load(.normal, .i64, len_ptr, alignment, "") catch return error.CompilationFailed;

        // Clear out_ptr for loop body
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        defer self.out_ptr = saved_out_ptr;

        // Create blocks: header (phi for index), body, exit
        const header_block = wip.block(2, "for_header") catch return error.OutOfMemory;
        const body_block = wip.block(1, "for_body") catch return error.OutOfMemory;
        const exit_block = wip.block(1, "for_exit") catch return error.OutOfMemory;

        // Entry → header
        const zero = builder.intValue(.i64, 0) catch return error.OutOfMemory;
        const entry_block = wip.cursor.block;
        _ = wip.br(header_block) catch return error.CompilationFailed;

        // Header: phi for loop index, compare with length
        wip.cursor = .{ .block = header_block };
        const idx_phi = wip.phi(.i64, "idx") catch return error.CompilationFailed;
        const idx_val = idx_phi.toValue();
        const cond = wip.icmp(.ult, idx_val, list_len, "") catch return error.OutOfMemory;
        _ = wip.brCond(cond, body_block, exit_block, .none) catch return error.CompilationFailed;

        // Body: load element, bind pattern, execute body, increment index
        wip.cursor = .{ .block = body_block };

        // Load element from data_ptr + idx * elem_size
        const elem_llvm_type = layoutToLlvmType(fl.elem_layout);
        const size_const = builder.intValue(.i64, elem_size) catch return error.OutOfMemory;
        const byte_offset = wip.bin(.mul, idx_val, size_const, "") catch return error.CompilationFailed;
        const elem_ptr = wip.gep(.inbounds, .i8, data_ptr, &.{byte_offset}, "") catch return error.CompilationFailed;
        const elem_val = wip.load(.normal, elem_llvm_type, elem_ptr, LlvmBuilder.Alignment.fromByteUnits(1), "") catch return error.CompilationFailed;

        // Bind the element to the pattern
        try self.bindPattern(fl.elem_pattern, elem_val);

        // Execute the body
        _ = try self.generateExpr(fl.body);

        // Increment index and loop back
        const one = builder.intValue(.i64, 1) catch return error.OutOfMemory;
        const next_idx = wip.bin(.add, idx_val, one, "") catch return error.CompilationFailed;
        const body_end_block = wip.cursor.block;
        _ = wip.br(header_block) catch return error.CompilationFailed;

        // Finish phi: entry→0, body_end→next_idx
        idx_phi.finish(
            &.{ zero, next_idx },
            &.{ entry_block, body_end_block },
            wip,
        );

        // Exit block — return unit
        wip.cursor = .{ .block = exit_block };
        return builder.intValue(.i8, 0) catch return error.OutOfMemory;
    }

    // ---------------------------------------------------------------
    // Pattern matching
    // ---------------------------------------------------------------

    fn generateWhen(self: *MonoLlvmCodeGen, w: anytype) Error!LlvmBuilder.Value {
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        defer self.out_ptr = saved_out_ptr;

        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        // Evaluate the scrutinee
        const scrutinee = try self.generateExpr(w.value);

        // Get branches
        const branches = self.store.getWhenBranches(w.branches);
        if (branches.len == 0) return error.UnsupportedExpression;

        // Compute the incoming count for the merge block by scanning patterns.
        // Each branch contributes one incoming edge. Unconditional patterns
        // (wildcard/bind) stop further branches, so count up to and including
        // the first unconditional one.
        var merge_incoming: u32 = 0;
        for (branches) |branch| {
            merge_incoming += 1;
            const pat = self.store.getPattern(branch.pattern);
            if (pat == .wildcard or pat == .bind) break;
        }

        const merge_block = wip.block(merge_incoming, "when_merge") catch return error.OutOfMemory;

        // Buffers for phi node
        var result_vals: [32]LlvmBuilder.Value = undefined;
        var result_blocks: [32]LlvmBuilder.Function.Block.Index = undefined;
        var branch_count: u32 = 0;

        for (branches, 0..) |branch, i| {
            const pattern = self.store.getPattern(branch.pattern);
            const is_last = (i == branches.len - 1);

            switch (pattern) {
                .wildcard => {
                    // Always matches — generate body directly
                    const body_val = try self.generateExpr(branch.body);
                    _ = wip.br(merge_block) catch return error.OutOfMemory;
                    result_vals[branch_count] = body_val;
                    result_blocks[branch_count] = wip.cursor.block;
                    branch_count += 1;
                    break; // No more branches after wildcard
                },

                .bind => |bind| {
                    // Always matches, bind the scrutinee to the symbol
                    const symbol_key: u48 = @bitCast(bind.symbol);
                    self.symbol_values.put(symbol_key, scrutinee) catch return error.OutOfMemory;
                    const body_val = try self.generateExpr(branch.body);
                    _ = wip.br(merge_block) catch return error.OutOfMemory;
                    result_vals[branch_count] = body_val;
                    result_blocks[branch_count] = wip.cursor.block;
                    branch_count += 1;
                    break; // No more branches after bind
                },

                .int_literal => |int_pat| {
                    // Compare scrutinee with pattern value
                    const pat_type = layoutToLlvmType(int_pat.layout_idx);
                    const pat_val = builder.intValue(pat_type, @as(u64, @truncate(@as(u128, @bitCast(int_pat.value))))) catch return error.OutOfMemory;
                    // Ensure scrutinee is the right type for comparison
                    const cmp_scrutinee = if (scrutinee.typeOfWip(wip) == pat_type)
                        scrutinee
                    else
                        wip.conv(.unsigned, scrutinee, pat_type, "") catch return error.CompilationFailed;
                    const cmp = wip.icmp(.eq, cmp_scrutinee, pat_val, "") catch return error.OutOfMemory;

                    if (is_last) {
                        // Last branch — treat as default (skip comparison)
                        const body_val = try self.generateExpr(branch.body);
                        _ = wip.br(merge_block) catch return error.OutOfMemory;
                        result_vals[branch_count] = body_val;
                        result_blocks[branch_count] = wip.cursor.block;
                        branch_count += 1;
                    } else {
                        const then_block = wip.block(1, "int_match") catch return error.OutOfMemory;
                        const else_block = wip.block(1, "int_next") catch return error.OutOfMemory;
                        _ = wip.brCond(cmp, then_block, else_block, .none) catch return error.OutOfMemory;

                        // Then block — pattern matches
                        wip.cursor = .{ .block = then_block };
                        const body_val = try self.generateExpr(branch.body);
                        _ = wip.br(merge_block) catch return error.OutOfMemory;
                        result_vals[branch_count] = body_val;
                        result_blocks[branch_count] = wip.cursor.block;
                        branch_count += 1;

                        // Else block — continue to next branch
                        wip.cursor = .{ .block = else_block };
                    }
                },

                .tag => |tag_pat| {
                    // Extract discriminant and compare
                    const ls = self.layout_store orelse return error.UnsupportedExpression;
                    const stored_layout = ls.getLayout(tag_pat.union_layout);

                    const discriminant = switch (stored_layout.tag) {
                        .scalar => scrutinee, // Scalar tag — value IS the discriminant
                        .tag_union => blk: {
                            const tu_data = ls.getTagUnionData(stored_layout.data.tag_union.idx);
                            const disc_type = discriminantIntType(tu_data.discriminant_size);
                            const disc_offset_val = builder.intValue(.i32, tu_data.discriminant_offset) catch return error.OutOfMemory;
                            const disc_ptr = wip.gep(.inbounds, .i8, scrutinee, &.{disc_offset_val}, "") catch return error.CompilationFailed;
                            break :blk wip.load(.normal, disc_type, disc_ptr, LlvmBuilder.Alignment.fromByteUnits(@as(u64, tu_data.discriminant_size)), "") catch return error.CompilationFailed;
                        },
                        else => return error.UnsupportedExpression,
                    };

                    const disc_type = discriminant.typeOfWip(wip);
                    const pat_disc = builder.intValue(disc_type, @as(u64, tag_pat.discriminant)) catch return error.OutOfMemory;
                    const cmp = wip.icmp(.eq, discriminant, pat_disc, "") catch return error.OutOfMemory;

                    if (is_last) {
                        // Last branch — bind payload args if needed, generate body
                        try self.bindTagPayloadArgs(tag_pat, scrutinee);
                        const body_val = try self.generateExpr(branch.body);
                        _ = wip.br(merge_block) catch return error.OutOfMemory;
                        result_vals[branch_count] = body_val;
                        result_blocks[branch_count] = wip.cursor.block;
                        branch_count += 1;
                    } else {
                        const then_block = wip.block(1, "tag_match") catch return error.OutOfMemory;
                        const else_block = wip.block(1, "tag_next") catch return error.OutOfMemory;
                        _ = wip.brCond(cmp, then_block, else_block, .none) catch return error.OutOfMemory;

                        wip.cursor = .{ .block = then_block };
                        try self.bindTagPayloadArgs(tag_pat, scrutinee);
                        const body_val = try self.generateExpr(branch.body);
                        _ = wip.br(merge_block) catch return error.OutOfMemory;
                        result_vals[branch_count] = body_val;
                        result_blocks[branch_count] = wip.cursor.block;
                        branch_count += 1;

                        wip.cursor = .{ .block = else_block };
                    }
                },

                else => return error.UnsupportedExpression,
            }
        }

        if (branch_count == 0) return error.UnsupportedExpression;

        // Merge block with phi
        wip.cursor = .{ .block = merge_block };
        const result_type = result_vals[0].typeOfWip(wip);
        const phi_inst = wip.phi(result_type, "") catch return error.OutOfMemory;
        phi_inst.finish(
            result_vals[0..branch_count],
            result_blocks[0..branch_count],
            wip,
        );
        return phi_inst.toValue();
    }

    /// Bind tag payload arguments from a matched tag pattern.
    /// For tag unions with payloads, extracts field values from the tag struct.
    fn bindTagPayloadArgs(self: *MonoLlvmCodeGen, tag_pat: anytype, scrutinee: LlvmBuilder.Value) Error!void {
        const args = self.store.getPatternSpan(tag_pat.args);
        if (args.len == 0) return;

        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const ls = self.layout_store orelse return error.UnsupportedExpression;

        const stored_layout = ls.getLayout(tag_pat.union_layout);
        if (stored_layout.tag != .tag_union) return;

        const tu_data = ls.getTagUnionData(stored_layout.data.tag_union.idx);
        const variants = ls.getTagUnionVariants(tu_data);
        if (tag_pat.discriminant >= variants.len) return;
        const variant = variants.get(tag_pat.discriminant);

        // Get the payload layout to determine field offsets
        const payload_layout = ls.getLayout(variant.payload_layout);

        for (args, 0..) |arg_id, arg_i| {
            const arg_pattern = self.store.getPattern(arg_id);
            switch (arg_pattern) {
                .bind => |bind| {
                    // Load the field value from the tag struct
                    const field_val = if (args.len == 1) blk: {
                        // Single arg — payload is at offset 0
                        const field_type = layoutToLlvmType(bind.layout_idx);
                        break :blk wip.load(.normal, field_type, scrutinee, .default, "") catch return error.CompilationFailed;
                    } else blk: {
                        // Multiple args — payload is a tuple, load from offset
                        const field_type = layoutToLlvmType(bind.layout_idx);
                        const offset: u32 = if (payload_layout.tag == .tuple)
                            ls.getTupleElementOffset(payload_layout.data.tuple.idx, @intCast(arg_i))
                        else
                            0;
                        const offset_val = builder.intValue(.i32, offset) catch return error.OutOfMemory;
                        const field_ptr = wip.gep(.inbounds, .i8, scrutinee, &.{offset_val}, "") catch return error.CompilationFailed;
                        break :blk wip.load(.normal, field_type, field_ptr, .default, "") catch return error.CompilationFailed;
                    };
                    const symbol_key: u48 = @bitCast(bind.symbol);
                    self.symbol_values.put(symbol_key, field_val) catch return error.OutOfMemory;
                },
                .wildcard => {
                    // Skip — no binding needed
                },
                else => return error.UnsupportedExpression,
            }
        }
    }

    // ---------------------------------------------------------------
    // Early return
    // ---------------------------------------------------------------

    /// Generate an early return — stores the result to out_ptr and branches to the
    /// early return block (which contains retVoid). Used for the `?` operator.
    fn generateEarlyReturn(self: *MonoLlvmCodeGen, er: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const out_ptr = self.out_ptr orelse return error.UnsupportedExpression;
        const ret_block = self.early_return_block orelse return error.UnsupportedExpression;
        const ret_layout = self.result_layout orelse return error.UnsupportedExpression;

        // Generate the return value
        const value = try self.generateExpr(er.expr);

        // Store to output pointer (same logic as in generateCode)
        if (value != .none) {
            const is_scalar = switch (ret_layout) {
                .bool, .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64,
                .i128, .u128, .dec, .f32, .f64,
                => true,
                else => false,
            };

            if (is_scalar) {
                const final_type: LlvmBuilder.Type = switch (ret_layout) {
                    .bool, .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64 => .i64,
                    .i128, .u128, .dec => .i128,
                    .f32 => .float,
                    .f64 => .double,
                    else => unreachable,
                };
                const signedness: LlvmBuilder.Constant.Cast.Signedness = switch (ret_layout) {
                    .i8, .i16, .i32, .i64, .i128 => .signed,
                    .bool, .u8, .u16, .u32, .u64, .u128 => .unsigned,
                    .f32, .f64, .dec => .unneeded,
                    else => .unneeded,
                };
                const store_value = if (value.typeOfWip(wip) == final_type)
                    value
                else
                    wip.conv(signedness, value, final_type, "") catch return error.CompilationFailed;
                const alignment = LlvmBuilder.Alignment.fromByteUnits(switch (final_type) {
                    .i64 => 8,
                    .i128 => 16,
                    .float => 4,
                    .double => 8,
                    else => 0,
                });
                _ = wip.store(.normal, store_value, out_ptr, alignment) catch return error.CompilationFailed;
            } else {
                // Composite — memcpy
                const ls = self.layout_store orelse return error.UnsupportedExpression;
                const stored_layout = ls.getLayout(ret_layout);
                const copy_size = switch (stored_layout.tag) {
                    .tag_union => ls.getTagUnionData(stored_layout.data.tag_union.idx).size,
                    else => return error.UnsupportedExpression,
                };
                const size_val = builder.intValue(.i32, copy_size) catch return error.OutOfMemory;
                const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
                _ = wip.callMemCpy(out_ptr, alignment, value, alignment, size_val, .normal, false) catch return error.OutOfMemory;
            }
        }

        // Branch to early return block
        _ = wip.br(ret_block) catch return error.OutOfMemory;

        // Create a dead block for subsequent code
        const dead_block = wip.block(0, "after_early_ret") catch return error.OutOfMemory;
        wip.cursor = .{ .block = dead_block };
        return builder.intValue(.i64, 0) catch return error.OutOfMemory;
    }

    // ---------------------------------------------------------------
    // Runtime error / unreachable
    // ---------------------------------------------------------------

    /// Generate an LLVM unreachable instruction for runtime_error and crash expressions.
    /// Returns a poison value so the caller has something to work with
    /// (the unreachable guarantees this code is never actually reached).
    /// Generate an empty list: ptr=null(0), len=0, capacity=0.
    /// A RocList is 24 bytes (3 x i64). Write zeros to out_ptr.
    fn generateEmptyList(self: *MonoLlvmCodeGen) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const dest_ptr = self.out_ptr orelse return error.UnsupportedExpression;

        const zero = builder.intValue(.i64, 0) catch return error.OutOfMemory;
        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);

        // Store ptr (offset 0)
        _ = wip.store(.normal, zero, dest_ptr, alignment) catch return error.CompilationFailed;

        // Store len (offset 8)
        const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
        const ptr8 = wip.gep(.inbounds, .i8, dest_ptr, &.{off8}, "") catch return error.CompilationFailed;
        _ = wip.store(.normal, zero, ptr8, alignment) catch return error.CompilationFailed;

        // Store capacity (offset 16)
        const off16 = builder.intValue(.i32, 16) catch return error.OutOfMemory;
        const ptr16 = wip.gep(.inbounds, .i8, dest_ptr, &.{off16}, "") catch return error.CompilationFailed;
        _ = wip.store(.normal, zero, ptr16, alignment) catch return error.CompilationFailed;

        return .none;
    }

    /// Generate a list with elements: allocate heap, store elements, write RocList to out_ptr.
    fn generateList(self: *MonoLlvmCodeGen, list: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const dest_ptr = self.out_ptr orelse return error.UnsupportedExpression;
        const ls = self.layout_store orelse return error.UnsupportedExpression;

        const elems = self.store.getExprSpan(list.elems);
        if (elems.len == 0) return self.generateEmptyList();

        // Get element layout info
        const elem_layout_data = ls.getLayout(list.elem_layout);
        const elem_sa = ls.layoutSizeAlign(elem_layout_data);
        const elem_size: u64 = elem_sa.size;
        const elem_align: u32 = @intCast(elem_sa.alignment.toByteUnits());
        const num_elems: u64 = @intCast(elems.len);
        const total_bytes: u64 = elem_size * num_elems;

        // Call allocateWithRefcountC(data_bytes, elem_align, elements_refcounted, roc_ops)
        if (self.alloc_with_refcount_addr == 0) return error.CompilationFailed;
        const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;

        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
        // Signature: ptr(usize, u32, i1, ptr) -> ptr
        const alloc_fn_type = builder.fnType(ptr_type, &.{ .i64, .i32, .i1, ptr_type }, .normal) catch return error.CompilationFailed;
        const addr_val = builder.intValue(.i64, self.alloc_with_refcount_addr) catch return error.CompilationFailed;
        const fn_ptr = wip.cast(.inttoptr, addr_val, ptr_type, "") catch return error.CompilationFailed;

        const size_val = builder.intValue(.i64, total_bytes) catch return error.OutOfMemory;
        const align_val = builder.intValue(.i32, elem_align) catch return error.OutOfMemory;
        const refcounted_val = builder.intValue(.i1, 0) catch return error.OutOfMemory;

        const heap_ptr = wip.call(.normal, .ccc, .none, alloc_fn_type, fn_ptr, &.{ size_val, align_val, refcounted_val, roc_ops }, "heap") catch return error.CompilationFailed;

        // Store each element to heap memory
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        defer self.out_ptr = saved_out_ptr;

        for (elems, 0..) |elem_id, i| {
            const elem_val = try self.generateExpr(elem_id);

            const offset: u64 = @as(u64, @intCast(i)) * elem_size;
            const elem_ptr = if (offset == 0)
                heap_ptr
            else blk: {
                const off_val = builder.intValue(.i32, @as(u32, @intCast(offset))) catch return error.OutOfMemory;
                break :blk wip.gep(.inbounds, .i8, heap_ptr, &.{off_val}, "") catch return error.CompilationFailed;
            };

            // Convert value to match element layout
            const store_val = try self.convertToFieldType(elem_val, list.elem_layout);
            const store_align = LlvmBuilder.Alignment.fromByteUnits(@intCast(elem_align));
            _ = wip.store(.normal, store_val, elem_ptr, store_align) catch return error.CompilationFailed;
        }

        // Write RocList struct to out_ptr: {ptr, len, capacity}
        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);

        // Store ptr (offset 0)
        _ = wip.store(.normal, heap_ptr, dest_ptr, alignment) catch return error.CompilationFailed;

        // Store len (offset 8)
        const len_val = builder.intValue(.i64, num_elems) catch return error.OutOfMemory;
        const off8 = builder.intValue(.i32, 8) catch return error.OutOfMemory;
        const ptr8 = wip.gep(.inbounds, .i8, dest_ptr, &.{off8}, "") catch return error.CompilationFailed;
        _ = wip.store(.normal, len_val, ptr8, alignment) catch return error.CompilationFailed;

        // Store capacity (offset 16) — same as len for new lists
        const off16 = builder.intValue(.i32, 16) catch return error.OutOfMemory;
        const ptr16 = wip.gep(.inbounds, .i8, dest_ptr, &.{off16}, "") catch return error.CompilationFailed;
        _ = wip.store(.normal, len_val, ptr16, alignment) catch return error.CompilationFailed;

        return .none;
    }

    fn generateRuntimeError(self: *MonoLlvmCodeGen) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        _ = wip.@"unreachable"() catch return error.CompilationFailed;
        // We need to return a value for the switch expression even though
        // this code is unreachable. Create a new block so subsequent code
        // (like phi nodes) has somewhere to live.
        const dead_block = wip.block(0, "unreachable") catch return error.OutOfMemory;
        wip.cursor = .{ .block = dead_block };
        return builder.intValue(.i64, 0) catch return error.OutOfMemory;
    }

    // ---------------------------------------------------------------
    // Low-level builtins
    // ---------------------------------------------------------------

    /// Generate code for low-level builtin operations.
    /// Handles numeric conversions and simple operations directly as LLVM
    /// instructions. Complex operations (list, string) fall through to
    /// UnsupportedExpression.
    fn generateLowLevel(self: *MonoLlvmCodeGen, ll: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const args = self.store.getExprSpan(ll.args);

        switch (ll.op) {
            // --- Numeric arithmetic ---
            .num_add => {
                if (args.len < 2) return error.UnsupportedExpression;
                const lhs = try self.generateExpr(args[0]);
                const rhs = try self.generateExpr(args[1]);
                const is_float = isFloatLayout(ll.ret_layout);
                return if (is_float)
                    wip.bin(.fadd, lhs, rhs, "") catch return error.CompilationFailed
                else
                    wip.bin(.add, lhs, rhs, "") catch return error.CompilationFailed;
            },
            .num_sub => {
                if (args.len < 2) return error.UnsupportedExpression;
                const lhs = try self.generateExpr(args[0]);
                const rhs = try self.generateExpr(args[1]);
                const is_float = isFloatLayout(ll.ret_layout);
                return if (is_float)
                    wip.bin(.fsub, lhs, rhs, "") catch return error.CompilationFailed
                else
                    wip.bin(.sub, lhs, rhs, "") catch return error.CompilationFailed;
            },
            .num_mul => {
                if (args.len < 2) return error.UnsupportedExpression;
                const lhs = try self.generateExpr(args[0]);
                const rhs = try self.generateExpr(args[1]);
                const is_float = isFloatLayout(ll.ret_layout);
                return if (is_float)
                    wip.bin(.fmul, lhs, rhs, "") catch return error.CompilationFailed
                else
                    wip.bin(.mul, lhs, rhs, "") catch return error.CompilationFailed;
            },
            .num_neg => {
                if (args.len < 1) return error.UnsupportedExpression;
                const operand = try self.generateExpr(args[0]);
                const is_float = isFloatLayout(ll.ret_layout);
                if (is_float) {
                    return wip.un(.fneg, operand, "") catch return error.CompilationFailed;
                } else {
                    const zero = builder.intValue(operand.typeOfWip(wip), 0) catch return error.OutOfMemory;
                    return wip.bin(.sub, zero, operand, "") catch return error.CompilationFailed;
                }
            },
            .num_abs => {
                if (args.len < 1) return error.UnsupportedExpression;
                const operand = try self.generateExpr(args[0]);
                const is_float = isFloatLayout(ll.ret_layout);
                if (is_float) {
                    // abs(x) = x < 0.0 ? -x : x
                    const zero = if (ll.ret_layout == .f32)
                        (builder.floatConst(0.0) catch return error.OutOfMemory).toValue()
                    else
                        (builder.doubleConst(0.0) catch return error.OutOfMemory).toValue();
                    const is_neg = wip.fcmp(.normal, .olt, operand, zero, "") catch return error.OutOfMemory;
                    const neg_val = wip.un(.fneg, operand, "") catch return error.CompilationFailed;
                    return wip.select(.normal, is_neg, neg_val, operand, "") catch return error.CompilationFailed;
                } else {
                    // abs(x) = x < 0 ? -x : x
                    const zero = builder.intValue(operand.typeOfWip(wip), 0) catch return error.OutOfMemory;
                    const is_neg = wip.icmp(.slt, operand, zero, "") catch return error.OutOfMemory;
                    const neg_val = wip.bin(.sub, zero, operand, "") catch return error.CompilationFailed;
                    return wip.select(.normal, is_neg, neg_val, operand, "") catch return error.CompilationFailed;
                }
            },

            // --- Widening integer conversions (always safe) ---
            .u8_to_i16, .u8_to_i32, .u8_to_i64, .u8_to_i128,
            .u8_to_u16, .u8_to_u32, .u8_to_u64, .u8_to_u128,
            .u16_to_i32, .u16_to_i64, .u16_to_i128,
            .u16_to_u32, .u16_to_u64, .u16_to_u128,
            .u32_to_i64, .u32_to_i128,
            .u32_to_u64, .u32_to_u128,
            .u64_to_i128, .u64_to_u128,
            => {
                if (args.len < 1) return error.UnsupportedExpression;
                const operand = try self.generateExpr(args[0]);
                const target_type = layoutToLlvmType(ll.ret_layout);
                return wip.conv(.unsigned, operand, target_type, "") catch return error.CompilationFailed;
            },

            .i8_to_i16, .i8_to_i32, .i8_to_i64, .i8_to_i128,
            .i16_to_i32, .i16_to_i64, .i16_to_i128,
            .i32_to_i64, .i32_to_i128,
            .i64_to_i128,
            => {
                if (args.len < 1) return error.UnsupportedExpression;
                const operand = try self.generateExpr(args[0]);
                const target_type = layoutToLlvmType(ll.ret_layout);
                return wip.conv(.signed, operand, target_type, "") catch return error.CompilationFailed;
            },

            // --- Narrowing/wrapping integer conversions ---
            .u8_to_i8_wrap, .i8_to_u8_wrap,
            .u16_to_i8_wrap, .u16_to_i16_wrap, .u16_to_u8_wrap,
            .i16_to_i8_wrap, .i16_to_u8_wrap, .i16_to_u16_wrap,
            .u32_to_i8_wrap, .u32_to_i16_wrap, .u32_to_i32_wrap, .u32_to_u8_wrap, .u32_to_u16_wrap,
            .i32_to_i8_wrap, .i32_to_i16_wrap, .i32_to_u8_wrap, .i32_to_u16_wrap, .i32_to_u32_wrap,
            .u64_to_i8_wrap, .u64_to_i16_wrap, .u64_to_i32_wrap, .u64_to_i64_wrap,
            .u64_to_u8_wrap, .u64_to_u16_wrap, .u64_to_u32_wrap,
            .i64_to_i8_wrap, .i64_to_i16_wrap, .i64_to_i32_wrap,
            .i64_to_u8_wrap, .i64_to_u16_wrap, .i64_to_u32_wrap, .i64_to_u64_wrap,
            .u128_to_i8_wrap, .u128_to_i16_wrap, .u128_to_i32_wrap, .u128_to_i64_wrap, .u128_to_i128_wrap,
            .u128_to_u8_wrap, .u128_to_u16_wrap, .u128_to_u32_wrap, .u128_to_u64_wrap,
            .i128_to_i8_wrap, .i128_to_i16_wrap, .i128_to_i32_wrap, .i128_to_i64_wrap,
            .i128_to_u8_wrap, .i128_to_u16_wrap, .i128_to_u32_wrap, .i128_to_u64_wrap, .i128_to_u128_wrap,
            => {
                if (args.len < 1) return error.UnsupportedExpression;
                const operand = try self.generateExpr(args[0]);
                const target_type = layoutToLlvmType(ll.ret_layout);
                return wip.conv(.unsigned, operand, target_type, "") catch return error.CompilationFailed;
            },

            // --- Integer to float conversions ---
            .u8_to_f32, .u16_to_f32, .u32_to_f32, .u64_to_f32, .u128_to_f32,
            .u8_to_f64, .u16_to_f64, .u32_to_f64, .u64_to_f64, .u128_to_f64,
            => {
                if (args.len < 1) return error.UnsupportedExpression;
                const operand = try self.generateExpr(args[0]);
                const target_type = layoutToLlvmType(ll.ret_layout);
                return wip.cast(.uitofp, operand, target_type, "") catch return error.CompilationFailed;
            },
            .i8_to_f32, .i16_to_f32, .i32_to_f32, .i64_to_f32, .i128_to_f32,
            .i8_to_f64, .i16_to_f64, .i32_to_f64, .i64_to_f64, .i128_to_f64,
            => {
                if (args.len < 1) return error.UnsupportedExpression;
                const operand = try self.generateExpr(args[0]);
                const target_type = layoutToLlvmType(ll.ret_layout);
                return wip.cast(.sitofp, operand, target_type, "") catch return error.CompilationFailed;
            },

            // --- Float to integer conversions (truncating) ---
            .f32_to_i8_trunc, .f32_to_i16_trunc, .f32_to_i32_trunc, .f32_to_i64_trunc, .f32_to_i128_trunc,
            .f64_to_i8_trunc, .f64_to_i16_trunc, .f64_to_i32_trunc, .f64_to_i64_trunc, .f64_to_i128_trunc,
            => {
                if (args.len < 1) return error.UnsupportedExpression;
                const operand = try self.generateExpr(args[0]);
                const target_type = layoutToLlvmType(ll.ret_layout);
                return wip.cast(.fptosi, operand, target_type, "") catch return error.CompilationFailed;
            },
            .f32_to_u8_trunc, .f32_to_u16_trunc, .f32_to_u32_trunc, .f32_to_u64_trunc, .f32_to_u128_trunc,
            .f64_to_u8_trunc, .f64_to_u16_trunc, .f64_to_u32_trunc, .f64_to_u64_trunc, .f64_to_u128_trunc,
            => {
                if (args.len < 1) return error.UnsupportedExpression;
                const operand = try self.generateExpr(args[0]);
                const target_type = layoutToLlvmType(ll.ret_layout);
                return wip.cast(.fptoui, operand, target_type, "") catch return error.CompilationFailed;
            },

            // --- Float to float conversions ---
            .f32_to_f64 => {
                if (args.len < 1) return error.UnsupportedExpression;
                const operand = try self.generateExpr(args[0]);
                return wip.cast(.fpext, operand, .double, "") catch return error.CompilationFailed;
            },
            .f64_to_f32_wrap => {
                if (args.len < 1) return error.UnsupportedExpression;
                const operand = try self.generateExpr(args[0]);
                return wip.cast(.fptrunc, operand, .float, "") catch return error.CompilationFailed;
            },

            // --- Integer to Dec conversion (multiply by 10^18) ---
            .u8_to_dec, .u16_to_dec, .u32_to_dec, .u64_to_dec,
            .i8_to_dec, .i16_to_dec, .i32_to_dec, .i64_to_dec,
            => {
                if (args.len < 1) return error.UnsupportedExpression;
                const operand = try self.generateExpr(args[0]);
                // Extend to i128 first
                const operand_layout = self.getExprResultLayout(args[0]) orelse ll.ret_layout;
                const ext = wip.conv(
                    if (isSigned(operand_layout)) .signed else .unsigned,
                    operand,
                    .i128,
                    "",
                ) catch return error.CompilationFailed;
                // Multiply by 10^18 (Dec fixed-point scale)
                const scale = builder.intValue(.i128, 1_000_000_000_000_000_000) catch return error.OutOfMemory;
                return wip.bin(.mul, ext, scale, "") catch return error.CompilationFailed;
            },

            // --- List operations (need builtins) ---
            .list_len => {
                // List is a (ptr, len, capacity) triple — length at offset 8
                if (args.len < 1) return error.UnsupportedExpression;
                const list_ptr = try self.materializeAsPtr(args[0], 24);
                const len_offset = builder.intValue(.i32, 8) catch return error.OutOfMemory;
                const len_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{len_offset}, "") catch return error.CompilationFailed;
                return wip.load(.normal, .i64, len_ptr, LlvmBuilder.Alignment.fromByteUnits(8), "") catch return error.CompilationFailed;
            },

            .list_is_empty => {
                // List is empty if length == 0
                if (args.len < 1) return error.UnsupportedExpression;
                const list_ptr = try self.materializeAsPtr(args[0], 24);
                const len_offset = builder.intValue(.i32, 8) catch return error.OutOfMemory;
                const len_ptr = wip.gep(.inbounds, .i8, list_ptr, &.{len_offset}, "") catch return error.CompilationFailed;
                const len_val = wip.load(.normal, .i64, len_ptr, LlvmBuilder.Alignment.fromByteUnits(8), "") catch return error.CompilationFailed;
                const zero = builder.intValue(.i64, 0) catch return error.OutOfMemory;
                const is_empty = wip.icmp(.eq, len_val, zero, "") catch return error.OutOfMemory;
                return wip.conv(.unsigned, is_empty, .i8, "") catch return error.CompilationFailed;
            },

            else => return error.UnsupportedExpression,
        }
    }

    /// Materialize a sub-expression as a pointer to memory.
    /// For composite types (lists, strings) that write to out_ptr, this allocates
    /// a temporary stack slot via alloca, points out_ptr at it, generates the
    /// expression, and returns the alloca pointer. For scalar types, it allocates,
    /// stores the value, and returns the pointer.
    fn materializeAsPtr(self: *MonoLlvmCodeGen, expr_id: MonoExprId, size: u32) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        // Allocate stack space via alloca
        const byte_array_type = builder.arrayType(size, .i8) catch return error.OutOfMemory;
        const alignment = LlvmBuilder.Alignment.fromByteUnits(8);
        const alloca_ptr = wip.alloca(.normal, byte_array_type, .none, alignment, .default, "buf") catch return error.CompilationFailed;

        // Zero-initialize
        const zero64 = builder.intValue(.i64, 0) catch return error.OutOfMemory;
        for (0..size / 8) |i| {
            if (i == 0) {
                _ = wip.store(.normal, zero64, alloca_ptr, alignment) catch return error.CompilationFailed;
            } else {
                const off = builder.intValue(.i32, @as(u32, @intCast(i * 8))) catch return error.OutOfMemory;
                const ptr = wip.gep(.inbounds, .i8, alloca_ptr, &.{off}, "") catch return error.CompilationFailed;
                _ = wip.store(.normal, zero64, ptr, alignment) catch return error.CompilationFailed;
            }
        }

        // Set out_ptr to the alloca, generate the expression, then restore
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = alloca_ptr;
        defer self.out_ptr = saved_out_ptr;

        const result = try self.generateExpr(expr_id);
        if (result != .none) {
            // Scalar value — store it to the alloca
            _ = wip.store(.normal, result, alloca_ptr, alignment) catch return error.CompilationFailed;
        }

        return alloca_ptr;
    }

    // ---------------------------------------------------------------
    // String generation
    // ---------------------------------------------------------------

    /// RocStr size in bytes (3 pointer-sized words: ptr/bytes, length, capacity)
    const roc_str_size: u32 = 24; // 3 * 8 on 64-bit
    const small_str_max_len: u32 = roc_str_size - 1; // 23 bytes

    /// Generate a string literal by storing bytes directly to the output pointer.
    /// Small strings (≤ 23 bytes) are stored inline in the RocStr struct.
    /// Large strings (> 23 bytes) are NOT yet supported.
    ///
    /// Stores bytes directly to `out_ptr` to avoid LLVM placing constants
    /// in .rodata (which we don't extract from the object file). Individual
    /// i8 stores use small immediates that stay inline in the code.
    ///
    /// Returns a sentinel value (.none) since the result is written directly
    /// to the output buffer. The caller should skip the result storage step.
    fn generateStrLiteral(self: *MonoLlvmCodeGen, str_idx: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;
        const dest_ptr = self.out_ptr orelse return error.UnsupportedExpression;

        const str_bytes = self.store.getString(str_idx);

        if (str_bytes.len >= roc_str_size) {
            // Large strings not yet supported
            return error.UnsupportedExpression;
        }

        const byte_alignment = LlvmBuilder.Alignment.fromByteUnits(1);

        // Store each string byte individually to dest_ptr using volatile stores.
        // Volatile prevents LLVM from combining adjacent byte stores into
        // word stores that reference a constant pool in .rodata (which we
        // don't extract from the object file).
        for (str_bytes, 0..) |byte, i| {
            if (byte == 0) continue;
            const byte_val = builder.intValue(.i8, byte) catch return error.OutOfMemory;
            if (i == 0) {
                _ = wip.store(.@"volatile", byte_val, dest_ptr, byte_alignment) catch return error.CompilationFailed;
            } else {
                const offset = builder.intValue(.i32, @as(u32, @intCast(i))) catch return error.OutOfMemory;
                const ptr = wip.gep(.inbounds, .i8, dest_ptr, &.{offset}, "") catch return error.CompilationFailed;
                _ = wip.store(.@"volatile", byte_val, ptr, byte_alignment) catch return error.CompilationFailed;
            }
        }

        // Store the length byte at position 23: length | 0x80
        const len_byte: u8 = @intCast(str_bytes.len | 0x80);
        const len_val = builder.intValue(.i8, len_byte) catch return error.OutOfMemory;
        const len_offset = builder.intValue(.i32, small_str_max_len) catch return error.OutOfMemory;
        const len_ptr = wip.gep(.inbounds, .i8, dest_ptr, &.{len_offset}, "") catch return error.CompilationFailed;
        _ = wip.store(.@"volatile", len_val, len_ptr, byte_alignment) catch return error.CompilationFailed;

        // Return .none as sentinel — data already written to out_ptr
        return .none;
    }

    /// Get the result layout of a mono expression (for determining operand types).
    fn getExprResultLayout(self: *const MonoLlvmCodeGen, expr_id: MonoExprId) ?layout.Idx {
        const MonoExpr = mono.MonoIR.MonoExpr;
        const expr: MonoExpr = self.store.getExpr(expr_id);
        return switch (expr) {
            .block => |b| self.getExprResultLayout(b.final_expr),
            .binop => |b| b.result_layout,
            .unary_minus => |um| um.result_layout,
            .call => |c| c.ret_layout,
            .low_level => |ll| ll.ret_layout,
            .lookup => |l| l.layout_idx,
            .i64_literal => .i64,
            .f64_literal => .f64,
            .f32_literal => .f32,
            .bool_literal => .bool,
            .i128_literal => .i128,
            .dec_literal => .dec,
            .str_literal => .str,
            .record => |r| r.record_layout,
            .empty_record => .zst,
            .tuple => |t| t.tuple_layout,
            .field_access => |fa| fa.field_layout,
            .tuple_access => |ta| ta.elem_layout,
            .nominal => |nom| self.getExprResultLayout(nom.backing_expr),
            .if_then_else => |ite| ite.result_layout,
            .zero_arg_tag => |zat| zat.union_layout,
            .tag => |t| t.union_layout,
            .discriminant_switch => |ds| ds.union_layout,
            .when => |w| w.result_layout,
            .dbg => |d| d.result_layout,
            .expect => |e| e.result_layout,
            .early_return => |er| er.ret_layout,
            .runtime_error, .crash => .i64, // dummy layout for unreachable
            else => null,
        };
    }

    /// Call Dec multiply builtin via indirect call through function pointer.
    /// Dec multiplication requires (a * b) / 10^18, which is handled by mulSaturatedC.
    fn callDecMul(self: *MonoLlvmCodeGen, lhs: LlvmBuilder.Value, rhs: LlvmBuilder.Value) !LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        if (self.dec_mul_addr == 0) return error.CompilationFailed;

        // Create function type: i128(i128, i128) — mulSaturatedC(RocDec, RocDec) -> RocDec
        const fn_type = builder.fnType(.i128, &.{ .i128, .i128 }, .normal) catch return error.CompilationFailed;

        // Create constant with the function address and cast to pointer
        const addr_val = builder.intValue(.i64, self.dec_mul_addr) catch return error.CompilationFailed;
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
        const fn_ptr = wip.cast(.inttoptr, addr_val, ptr_type, "") catch return error.CompilationFailed;

        // Call the function
        return wip.call(.normal, .ccc, .none, fn_type, fn_ptr, &.{ lhs, rhs }, "") catch return error.CompilationFailed;
    }

    /// Call Dec divide builtin via indirect call through function pointer.
    /// Dec division requires (a * 10^18) / b, which is handled by divC.
    fn callDecDiv(self: *MonoLlvmCodeGen, lhs: LlvmBuilder.Value, rhs: LlvmBuilder.Value) !LlvmBuilder.Value {
        return self.callDecBuiltin3(self.dec_div_addr, lhs, rhs);
    }

    /// Call Dec truncating divide builtin via indirect call through function pointer.
    fn callDecDivTrunc(self: *MonoLlvmCodeGen, lhs: LlvmBuilder.Value, rhs: LlvmBuilder.Value) !LlvmBuilder.Value {
        return self.callDecBuiltin3(self.dec_div_trunc_addr, lhs, rhs);
    }

    /// Call a Dec builtin with signature (RocDec, RocDec, *RocOps) -> i128
    fn callDecBuiltin3(self: *MonoLlvmCodeGen, fn_addr: usize, lhs: LlvmBuilder.Value, rhs: LlvmBuilder.Value) !LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        if (fn_addr == 0) return error.CompilationFailed;

        const roc_ops = self.roc_ops_arg orelse return error.CompilationFailed;

        // Create function type: i128(i128, i128, ptr) — divC(RocDec, RocDec, *RocOps) -> i128
        const ptr_type = builder.ptrType(.default) catch return error.CompilationFailed;
        const fn_type = builder.fnType(.i128, &.{ .i128, .i128, ptr_type }, .normal) catch return error.CompilationFailed;

        // Create constant with the function address and cast to pointer
        const addr_val = builder.intValue(.i64, fn_addr) catch return error.CompilationFailed;
        const fn_ptr = wip.cast(.inttoptr, addr_val, ptr_type, "") catch return error.CompilationFailed;

        // Call the function with roc_ops
        return wip.call(.normal, .ccc, .none, fn_type, fn_ptr, &.{ lhs, rhs, roc_ops }, "") catch return error.CompilationFailed;
    }

    fn generateUnaryMinus(self: *MonoLlvmCodeGen, unary: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        var val = try self.generateExpr(unary.expr);
        const is_float = isFloatLayout(unary.result_layout);

        if (is_float) {
            // Cast integer operand to float if needed
            const expr_layout = self.getExprResultLayout(unary.expr) orelse unary.result_layout;
            if (!isFloatLayout(expr_layout)) {
                val = wip.cast(if (isSigned(expr_layout)) .sitofp else .uitofp, val, layoutToLlvmType(unary.result_layout), "") catch return error.CompilationFailed;
            }
            return wip.un(.fneg, val, "") catch return error.CompilationFailed;
        } else {
            // Align value type to result type (e.g., i64_literal produces i64 but result may be i8)
            const result_type = layoutToLlvmType(unary.result_layout);
            const val_type = val.typeOfWip(wip);
            if (isIntType(val_type) and isIntType(result_type) and val_type != result_type) {
                const val_bits = intTypeBits(val_type);
                const result_bits = intTypeBits(result_type);
                if (val_bits > result_bits) {
                    val = wip.cast(.trunc, val, result_type, "") catch return error.CompilationFailed;
                } else {
                    val = wip.cast(if (isSigned(unary.result_layout)) .sext else .zext, val, result_type, "") catch return error.CompilationFailed;
                }
            }
            // For integers, subtract from 0
            const zero = (builder.intConst(result_type, 0) catch return error.OutOfMemory).toValue();
            return wip.bin(.sub, zero, val, "") catch return error.CompilationFailed;
        }
    }

    fn generateUnaryNot(self: *MonoLlvmCodeGen, unary: anytype) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        const val = try self.generateExpr(unary.expr);
        // XOR with 1 to flip the boolean
        const one = (builder.intConst(.i1, 1) catch return error.OutOfMemory).toValue();
        return wip.bin(.xor, val, one, "") catch return error.CompilationFailed;
    }

    fn generateIfThenElse(self: *MonoLlvmCodeGen, ite: anytype) Error!LlvmBuilder.Value {
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        defer self.out_ptr = saved_out_ptr;

        const wip = self.wip orelse return error.CompilationFailed;

        // Get the branches
        const branches = self.store.getIfBranches(ite.branches);

        if (branches.len == 0) {
            // No branches, just generate the else
            return self.generateExpr(ite.final_else);
        }

        // For simplicity, handle single branch if-then-else
        const first_branch = branches[0];
        var cond_val = try self.generateExpr(first_branch.cond);

        // Ensure condition is i1 for brCond (tag unions may produce i8 for Bool-like types)
        if (cond_val.typeOfWip(wip) != .i1) {
            cond_val = wip.cast(.trunc, cond_val, .i1, "") catch return error.CompilationFailed;
        }

        // Create basic blocks
        // Each of then/else has 1 incoming edge (from the conditional branch),
        // merge has 2 incoming edges (one from then, one from else).
        const then_block = wip.block(1, "then") catch return error.CompilationFailed;
        const else_block = wip.block(1, "else") catch return error.CompilationFailed;
        const merge_block = wip.block(2, "merge") catch return error.CompilationFailed;

        // Conditional branch
        _ = wip.brCond(cond_val, then_block, else_block, .none) catch return error.CompilationFailed;

        // Then block
        wip.cursor = .{ .block = then_block };
        const then_val = try self.generateExpr(first_branch.body);
        _ = wip.br(merge_block) catch return error.CompilationFailed;
        const then_exit_block = wip.cursor.block;

        // Else block
        wip.cursor = .{ .block = else_block };
        const else_val = try self.generateExpr(ite.final_else);
        _ = wip.br(merge_block) catch return error.CompilationFailed;
        const else_exit_block = wip.cursor.block;

        // Merge block with phi — use actual value type, not layout (which may be wrong for structs)
        wip.cursor = .{ .block = merge_block };
        const result_type = then_val.typeOfWip(wip);
        const phi_inst = wip.phi(result_type, "") catch return error.CompilationFailed;
        phi_inst.finish(
            &.{ then_val, else_val },
            &.{ then_exit_block, else_exit_block },
            wip,
        );

        return phi_inst.toValue();
    }

    fn generateBlock(self: *MonoLlvmCodeGen, block_data: anytype) Error!LlvmBuilder.Value {
        // Save out_ptr — intermediate statements must not write to it.
        // Only the final expression should use out_ptr for direct-store types (strings).
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;

        // Process all statements (let bindings)
        const stmts = self.store.getStmts(block_data.stmts);
        for (stmts) |stmt| {
            // Check if the expression is a lambda/closure for binding
            const stmt_expr = self.store.getExpr(stmt.expr);
            switch (stmt_expr) {
                .lambda, .closure => {
                    try self.bindLambdaPattern(stmt.pattern, stmt.expr);
                },
                else => {},
            }

            // Generate the expression
            const val = try self.generateExpr(stmt.expr);

            // Bind the pattern
            try self.bindPattern(stmt.pattern, val);
        }

        // Restore out_ptr for the final expression
        self.out_ptr = saved_out_ptr;
        // Generate and return the final expression
        return self.generateExpr(block_data.final_expr);
    }

    // ---------------------------------------------------------------
    // Call generation (mirrors dev backend's dispatch)
    // ---------------------------------------------------------------

    fn generateCall(self: *MonoLlvmCodeGen, call: anytype) Error!LlvmBuilder.Value {
        const saved_out_ptr = self.out_ptr;
        self.out_ptr = null;
        defer self.out_ptr = saved_out_ptr;

        const fn_expr = self.store.getExpr(call.fn_expr);

        return switch (fn_expr) {
            .lambda => |lambda| self.generateLambdaCall(lambda, call.args, call.ret_layout),
            .closure => |closure| self.generateClosureCall(closure, call.args, call.ret_layout),
            .lookup => |lookup| self.generateLookupCall(lookup, call.args, call.ret_layout),
            .call => error.UnsupportedExpression,
            .block => error.UnsupportedExpression,
            else => error.UnsupportedExpression,
        };
    }

    /// Generate a lambda call by inlining: bind args to params, generate body.
    fn generateLambdaCall(self: *MonoLlvmCodeGen, lambda: anytype, args_span: anytype, _: layout.Idx) Error!LlvmBuilder.Value {
        const params = self.store.getPatternSpan(lambda.params);
        const args = self.store.getExprSpan(args_span);

        for (params, args) |param_id, arg_id| {
            // Check if the argument is a lambda/closure
            const arg_expr = self.store.getExpr(arg_id);
            switch (arg_expr) {
                .lambda, .closure => {
                    try self.bindLambdaPattern(param_id, arg_id);
                },
                else => {},
            }
            const arg_val = try self.generateExpr(arg_id);
            try self.bindPattern(param_id, arg_val);
        }

        return self.generateExpr(lambda.body);
    }

    /// Generate a closure call: bind captures based on representation, then call lambda.
    fn generateClosureCall(self: *MonoLlvmCodeGen, closure: anytype, args_span: anytype, ret_layout: layout.Idx) Error!LlvmBuilder.Value {
        // Bind captured values based on representation
        switch (closure.representation) {
            .direct_call => {
                // No captures to bind
            },
            .unwrapped_capture => {
                const captures = self.store.getCaptures(closure.captures);
                if (captures.len > 0) {
                    const cap = captures[0];
                    const symbol_key: u48 = @bitCast(cap.symbol);
                    if (self.symbol_values.get(symbol_key) == null) {
                        if (self.store.getSymbolDef(cap.symbol)) |def_expr_id| {
                            const val = try self.generateExpr(def_expr_id);
                            self.symbol_values.put(symbol_key, val) catch return error.OutOfMemory;
                        }
                    }
                }
            },
            .struct_captures => |repr| {
                const captures = self.store.getCaptures(repr.captures);
                for (captures) |cap| {
                    const symbol_key: u48 = @bitCast(cap.symbol);
                    if (self.symbol_values.get(symbol_key) == null) {
                        if (self.store.getSymbolDef(cap.symbol)) |def_expr_id| {
                            const val = try self.generateExpr(def_expr_id);
                            self.symbol_values.put(symbol_key, val) catch return error.OutOfMemory;
                        }
                    }
                }
            },
            .enum_dispatch, .union_repr => {
                const captures = self.store.getCaptures(closure.captures);
                for (captures) |cap| {
                    const symbol_key: u48 = @bitCast(cap.symbol);
                    if (self.symbol_values.get(symbol_key) == null) {
                        if (self.store.getSymbolDef(cap.symbol)) |def_expr_id| {
                            const val = try self.generateExpr(def_expr_id);
                            self.symbol_values.put(symbol_key, val) catch return error.OutOfMemory;
                        }
                    }
                }
            },
        }

        // Get the lambda and call it (inlined)
        const lambda_expr = self.store.getExpr(closure.lambda);
        return switch (lambda_expr) {
            .lambda => |lambda| self.generateLambdaCall(lambda, args_span, ret_layout),
            else => error.UnsupportedExpression,
        };
    }

    /// Generate a call through a lookup: check proc_registry, lambda_bindings, symbol defs.
    fn generateLookupCall(self: *MonoLlvmCodeGen, lookup: anytype, args_span: anytype, ret_layout: layout.Idx) Error!LlvmBuilder.Value {
        const symbol_key: u48 = @bitCast(lookup.symbol);

        // First: check if the function was compiled as a procedure
        if (self.proc_registry.get(symbol_key)) |func_index| {
            return self.generateCallToCompiledProc(func_index, args_span, ret_layout);
        }

        // Check if the symbol is bound to a lambda/closure in local scope
        if (self.lambda_bindings.get(symbol_key)) |lambda_expr_id| {
            const lambda_expr = self.store.getExpr(lambda_expr_id);
            return switch (lambda_expr) {
                .lambda => |lambda| self.generateLambdaCall(lambda, args_span, ret_layout),
                .closure => |closure| self.generateClosureCall(closure, args_span, ret_layout),
                else => error.UnsupportedExpression,
            };
        }

        // Look up in top-level definitions
        if (self.store.getSymbolDef(lookup.symbol)) |def_expr_id| {
            const def_expr = self.store.getExpr(def_expr_id);
            return switch (def_expr) {
                .lambda => |lambda| self.generateLambdaCall(lambda, args_span, ret_layout),
                .closure => |closure| self.generateClosureCall(closure, args_span, ret_layout),
                else => error.UnsupportedExpression,
            };
        }

        return error.UnsupportedExpression;
    }

    /// Generate a call to a compiled procedure via LLVM call instruction.
    fn generateCallToCompiledProc(self: *MonoLlvmCodeGen, func_index: LlvmBuilder.Function.Index, args_span: anytype, _: layout.Idx) Error!LlvmBuilder.Value {
        const wip = self.wip orelse return error.CompilationFailed;
        const builder = self.builder orelse return error.CompilationFailed;

        const args = self.store.getExprSpan(args_span);
        var arg_values: std.ArrayList(LlvmBuilder.Value) = .{};
        defer arg_values.deinit(self.allocator);

        for (args) |arg_id| {
            const val = try self.generateExpr(arg_id);
            arg_values.append(self.allocator, val) catch return error.OutOfMemory;
        }

        const fn_type = func_index.typeOf(builder);
        const callee = func_index.toValue(builder);

        return wip.call(.normal, .ccc, .none, fn_type, callee, arg_values.items, "") catch return error.CompilationFailed;
    }

    // ---------------------------------------------------------------
    // Standalone lambda/closure (not at call sites)
    // ---------------------------------------------------------------

    fn generateLambda(self: *MonoLlvmCodeGen, _: anytype) Error!LlvmBuilder.Value {
        // Standalone lambda - return 0 placeholder.
        // Actual evaluation happens at call site via inlining.
        return self.emitI64(0);
    }

    fn generateClosure(self: *MonoLlvmCodeGen, closure: anytype) Error!LlvmBuilder.Value {
        switch (closure.representation) {
            .direct_call => {
                return self.emitI64(0);
            },
            .unwrapped_capture => {
                // Single capture - the closure value IS the captured value
                const captures = self.store.getCaptures(closure.captures);
                if (captures.len > 0) {
                    const cap = captures[0];
                    const symbol_key: u48 = @bitCast(cap.symbol);
                    if (self.symbol_values.get(symbol_key)) |loc| {
                        return loc;
                    }
                    if (self.store.getSymbolDef(cap.symbol)) |def_expr_id| {
                        return self.generateExpr(def_expr_id);
                    }
                }
                return self.emitI64(0);
            },
            .struct_captures => |repr| {
                // Multiple captures - bind all for inline evaluation
                const captures = self.store.getCaptures(repr.captures);
                var first_val: ?LlvmBuilder.Value = null;
                for (captures) |cap| {
                    const symbol_key: u48 = @bitCast(cap.symbol);
                    if (self.symbol_values.get(symbol_key)) |val| {
                        if (first_val == null) first_val = val;
                    } else if (self.store.getSymbolDef(cap.symbol)) |def_expr_id| {
                        const val = try self.generateExpr(def_expr_id);
                        self.symbol_values.put(symbol_key, val) catch return error.OutOfMemory;
                        if (first_val == null) first_val = val;
                    }
                }
                return first_val orelse self.emitI64(0);
            },
            .enum_dispatch => |repr| {
                // Multiple functions, no captures - return the tag
                return self.emitI64(@intCast(repr.tag));
            },
            .union_repr => |repr| {
                // Multiple functions with captures - bind captures, return tag
                const captures = self.store.getCaptures(repr.captures);
                for (captures) |cap| {
                    const symbol_key: u48 = @bitCast(cap.symbol);
                    if (self.symbol_values.get(symbol_key) == null) {
                        if (self.store.getSymbolDef(cap.symbol)) |def_expr_id| {
                            const val = try self.generateExpr(def_expr_id);
                            self.symbol_values.put(symbol_key, val) catch return error.OutOfMemory;
                        }
                    }
                }
                return self.emitI64(@intCast(repr.tag));
            },
        }
    }

    // ---------------------------------------------------------------
    // Pattern binding helpers
    // ---------------------------------------------------------------

    /// Bind a pattern to an LLVM value.
    fn bindPattern(self: *MonoLlvmCodeGen, pattern_id: MonoPatternId, value: LlvmBuilder.Value) Error!void {
        const pattern = self.store.getPattern(pattern_id);
        switch (pattern) {
            .bind => |bind| {
                const key: u48 = @bitCast(bind.symbol);
                self.symbol_values.put(key, value) catch return error.OutOfMemory;
            },
            .wildcard => {},
            else => {},
        }
    }

    /// Record that a pattern is bound to a lambda/closure expression for later call resolution.
    fn bindLambdaPattern(self: *MonoLlvmCodeGen, pattern_id: MonoPatternId, expr_id: MonoExprId) Error!void {
        const pattern = self.store.getPattern(pattern_id);
        switch (pattern) {
            .bind => |bind| {
                const key: u48 = @bitCast(bind.symbol);
                self.lambda_bindings.put(key, expr_id) catch return error.OutOfMemory;
            },
            else => {},
        }
    }
};
