//! Mono IR Code Generator
//!
//! This module generates native machine code from Mono IR expressions.
//! It uses the Emit.zig infrastructure for instruction encoding and
//! CodeGen.zig for register allocation.
//!
//! Pipeline position:
//! ```
//! CIR -> Mono IR Lowering -> MonoExprCodeGen -> Machine Code
//! ```
//!
//! Key properties:
//! - Uses real machine instructions via Emit.zig
//! - Proper register allocation with spilling support
//! - Handles System V ABI (x86_64/aarch64) calling convention
//! - Generates position-independent code with relocations
//! - Supports x86_64 and aarch64 architectures

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const layout = @import("layout");
const mono = @import("mono");
const builtins = @import("builtins");

const x86_64 = @import("x86_64/mod.zig");
const aarch64 = @import("aarch64/mod.zig");

// Num builtin functions for 128-bit integer operations
const num_divTruncI128 = builtins.num.divTruncI128;
const num_divTruncU128 = builtins.num.divTruncU128;
const num_remTruncI128 = builtins.num.remTruncI128;
const num_remTruncU128 = builtins.num.remTruncU128;

// Utils builtin functions for memory allocation and reference counting
const allocateWithRefcountC = builtins.utils.allocateWithRefcountC;
const increfDataPtrC = builtins.utils.increfDataPtrC;
const decrefDataPtrC = builtins.utils.decrefDataPtrC;
const freeDataPtrC = builtins.utils.freeDataPtrC;
const rcNone = builtins.utils.rcNone;

// List builtin functions - using C-compatible wrappers to avoid ABI issues
// with 24-byte RocList struct returns on aarch64
const listWithCapacityC = builtins.list.listWithCapacityC;
const listAppendUnsafeC = builtins.list.listAppendUnsafeC;
const listAppendSafeC = builtins.list.listAppendSafeC;
const copy_fallback = builtins.list.copy_fallback;

const Relocation = @import("Relocation.zig").Relocation;
const StaticDataInterner = @import("StaticDataInterner.zig");

const MonoExprStore = mono.MonoExprStore;
const MonoExpr = mono.MonoExpr;
const MonoExprId = mono.MonoExprId;
const MonoPatternId = mono.MonoPatternId;
const MonoSymbol = mono.MonoSymbol;
const SelfRecursive = mono.SelfRecursive;
const JoinPointId = mono.JoinPointId;
const LambdaSetMember = mono.LambdaSetMember;
const LambdaSetMemberSpan = mono.LambdaSetMemberSpan;

// Layout store for accessing record/tuple/tag field offsets
const LayoutStore = layout.Store;

// Control flow statement types (for two-pass compilation)
const CFStmtId = mono.CFStmtId;
const LayoutIdxSpan = mono.LayoutIdxSpan;

/// Special layout index for List I64 type (must match dev_evaluator.zig).
/// Lists are (ptr, len, capacity) = 24 bytes and need special handling when returning results.

// Number-to-string C wrapper functions (explicit output pointer to avoid struct return ABI issues)
const RocStr = builtins.str.RocStr;
const RocOps = builtins.host_abi.RocOps;

fn intToStrC(comptime T: type) *const fn (*RocStr, T, *RocOps) callconv(.c) void {
    const S = struct {
        fn func(out: *RocStr, value: T, roc_ops: *RocOps) callconv(.c) void {
            const max_len = comptime blk: {
                var buf: [40]u8 = undefined;
                const min_str = std.fmt.bufPrint(&buf, "{}", .{std.math.minInt(T)}) catch unreachable;
                const max_str = std.fmt.bufPrint(&buf, "{}", .{std.math.maxInt(T)}) catch unreachable;
                break :blk @max(min_str.len, max_str.len);
            };
            var buf: [max_len]u8 = undefined;
            const result = std.fmt.bufPrint(&buf, "{}", .{value}) catch unreachable;
            out.* = RocStr.init(&buf, result.len, roc_ops);
        }
    };
    return &S.func;
}

fn floatToStrC(comptime T: type) *const fn (*RocStr, T, *RocOps) callconv(.c) void {
    const S = struct {
        fn func(out: *RocStr, value: T, roc_ops: *RocOps) callconv(.c) void {
            var buf: [400]u8 = undefined;
            const result = std.fmt.bufPrint(&buf, "{d}", .{value}) catch unreachable;
            out.* = RocStr.init(&buf, result.len, roc_ops);
        }
    };
    return &S.func;
}

fn decToStrC(out: *RocStr, value: i128, roc_ops: *RocOps) callconv(.c) void {
    const dec = builtins.dec.RocDec{ .num = value };
    var buf: [builtins.dec.RocDec.max_str_length]u8 = undefined;
    const slice = dec.format_to_buf(&buf);
    out.* = RocStr.init(&buf, slice.len, roc_ops);
}
const MonoProc = mono.MonoProc;

const Allocator = std.mem.Allocator;

/// Code generator for Mono IR expressions
/// Generic over the architecture-specific code generator
pub fn MonoExprCodeGenFor(comptime CodeGen: type, comptime GeneralReg: type, comptime FloatReg: type, comptime Condition: type) type {
    return struct {
        const Self = @This();

        allocator: Allocator,

        /// Architecture-specific code generator with register allocation
        codegen: CodeGen,

        /// The Mono IR store containing expressions to compile
        store: *const MonoExprStore,

        /// Layout store for accessing record/tuple/tag field offsets
        layout_store: ?*const LayoutStore,

        /// Static data interner for string literals
        static_interner: ?*StaticDataInterner,

        /// Map from MonoSymbol to value location (register or stack slot)
        symbol_locations: std.AutoHashMap(u48, ValueLocation),

        /// Map from mutable variable symbol to fixed stack slot info
        /// Mutable variables need fixed slots so re-bindings can update the value at runtime
        mutable_var_slots: std.AutoHashMap(u48, MutableVarInfo),

        /// Map from JoinPointId to code offset (for recursive closure jumps)
        join_points: std.AutoHashMap(u32, usize),

        /// Current recursive context (for detecting recursive calls)
        /// When set, lookups of this symbol should jump to the join point instead of re-entering
        current_recursive_symbol: ?MonoSymbol,
        current_recursive_join_point: ?JoinPointId,

        /// The symbol currently being bound (during let statement processing).
        current_binding_symbol: ?MonoSymbol,

        /// Registry of compiled procedures (symbol -> CompiledProc)
        /// Used to find call targets during second pass
        proc_registry: std.AutoHashMap(u48, CompiledProc),

        /// Registry of compiled lambdas by expression ID.
        /// Used when a lambda is called - we compile it once and reuse.
        /// Key is @intFromEnum(MonoExprId), value is code start offset.
        compiled_lambdas: std.AutoHashMap(u32, usize),

        /// Pending calls that need to be patched after all procedures are compiled
        pending_calls: std.ArrayList(PendingCall),

        /// Map from JoinPointId to list of jumps that target it (for patching)
        join_point_jumps: std.AutoHashMap(u32, std.ArrayList(JumpRecord)),

        /// Map from JoinPointId to parameter layouts (for i128 handling in rebind)
        join_point_param_layouts: std.AutoHashMap(u32, LayoutIdxSpan),

        /// Map from JoinPointId to parameter patterns (for rebinding to correct stack slots)
        join_point_param_patterns: std.AutoHashMap(u32, mono.MonoPatternSpan),

        /// Stack of early return jump patches.
        /// When generateEarlyReturn is called inside compileLambdaAsProc,
        /// it emits a jump to the epilogue and records the patch location here.
        /// After generating the lambda body, compileLambdaAsProc patches all
        /// early return jumps to point to the epilogue.
        early_return_patches: std.ArrayList(usize),

        /// Stack slot where early return value is stored (for compileLambdaAsProc).
        /// Set by compileLambdaAsProc before generating the body.
        early_return_result_slot: ?i32 = null,

        /// Layout for early return result (to know how to move to return register)
        early_return_ret_layout: ?layout.Idx = null,

        /// Register where RocOps pointer is saved (for calling builtins that need it)
        roc_ops_reg: ?GeneralReg = null,

        /// Counter for unique temporary local IDs.
        /// Starts at 0x8000_0000 to avoid collision with real local variables.
        /// Used by allocTempGeneral() for temporaries that don't correspond to real locals.
        next_temp_local: u32 = 0x8000_0000,

        /// Info about a mutable variable's fixed stack slot
        pub const MutableVarInfo = struct {
            /// The fixed stack slot offset (from frame pointer)
            slot: i32,
            /// The size of the variable in bytes
            size: u32,
        };

        /// Compiled procedure information for two-pass compilation.
        /// After a procedure is fully compiled (including RET), it's registered here.
        pub const CompiledProc = struct {
            /// Offset into the code buffer where this procedure starts
            code_start: usize,
            /// Offset where this procedure ends
            code_end: usize,
            /// The symbol this procedure is bound to
            name: MonoSymbol,
        };

        /// A pending call that needs to be patched after all procedures are compiled.
        pub const PendingCall = struct {
            /// Offset where the call instruction is (needs patching)
            call_site: usize,
            /// The function being called
            target_symbol: MonoSymbol,
        };

        /// Record of a jump instruction that needs patching to a join point.
        pub const JumpRecord = struct {
            /// Offset of the jump instruction
            location: usize,
        };

        /// Where a value is stored
        pub const ValueLocation = union(enum) {
            /// Value is in a general-purpose register
            general_reg: GeneralReg,
            /// Value is in a float register
            float_reg: FloatReg,
            /// Value is on the stack at given offset from frame pointer
            stack: i32,
            /// 128-bit value on the stack (16 bytes: low at offset, high at offset+8)
            stack_i128: i32,
            /// 24-byte string value on the stack (for RocStr: ptr/data, len, capacity)
            stack_str: i32,
            /// List value on the stack - tracks both struct and element locations
            /// for proper copying when returning lists
            list_stack: struct {
                /// Offset of the list struct (ptr, len) from frame pointer
                struct_offset: i32,
                /// Offset of the element data from frame pointer
                data_offset: i32,
                /// Number of elements in the list
                num_elements: u32,
            },
            /// Immediate value known at compile time
            immediate_i64: i64,
            /// Immediate float value
            immediate_f64: f64,
            /// Immediate 128-bit value
            immediate_i128: i128,
            /// Compiled lambda code location (for first-class functions)
            lambda_code: struct {
                /// Offset into code buffer where the procedure starts
                code_offset: usize,
                /// Layout of the function's return type
                ret_layout: layout.Idx,
            },
            /// Closure value on stack - for lambda set dispatch at call sites.
            /// Used when a closure is passed as an argument to a higher-order function,
            /// or when a single-function closure captures variables.
            closure_value: struct {
                /// Stack offset where closure data (captures) is stored
                stack_offset: i32,
                /// The closure representation (contains lambda set info for dispatch)
                representation: mono.ClosureRepresentation,
                /// The lambda body expression (for single-function closures)
                lambda: mono.MonoExprId,
                /// Capture specifications (symbols and layouts)
                captures: mono.MonoIR.MonoCaptureSpan,
            },
        };

        /// Result of code generation
        pub const CodeResult = struct {
            /// Generated machine code
            code: []const u8,
            /// Relocations for external references
            relocations: []const Relocation,
            /// Layout of the result
            result_layout: layout.Idx,
            /// Offset from start of code where execution should begin
            /// (procedures may be compiled before the main expression)
            entry_offset: usize = 0,
        };

        /// Errors that can occur during code generation
        pub const Error = error{
            OutOfMemory,
            UnsupportedExpression,
            NoRegisterToSpill,
            InvalidLocalLocation,
            LocalNotFound,
            Crash,
            RuntimeError,
        };

        /// Initialize the code generator
        pub fn init(
            allocator: Allocator,
            store: *const MonoExprStore,
            layout_store_opt: ?*const LayoutStore,
            static_interner: ?*StaticDataInterner,
        ) Self {
            return .{
                .allocator = allocator,
                .codegen = CodeGen.init(allocator),
                .store = store,
                .layout_store = layout_store_opt,
                .static_interner = static_interner,
                .symbol_locations = std.AutoHashMap(u48, ValueLocation).init(allocator),
                .mutable_var_slots = std.AutoHashMap(u48, MutableVarInfo).init(allocator),
                .join_points = std.AutoHashMap(u32, usize).init(allocator),
                .current_recursive_symbol = null,
                .current_recursive_join_point = null,
                .current_binding_symbol = null,
                .proc_registry = std.AutoHashMap(u48, CompiledProc).init(allocator),
                .compiled_lambdas = std.AutoHashMap(u32, usize).init(allocator),
                .pending_calls = std.ArrayList(PendingCall).empty,
                .join_point_jumps = std.AutoHashMap(u32, std.ArrayList(JumpRecord)).init(allocator),
                .join_point_param_layouts = std.AutoHashMap(u32, LayoutIdxSpan).init(allocator),
                .join_point_param_patterns = std.AutoHashMap(u32, mono.MonoPatternSpan).init(allocator),
                .early_return_patches = std.ArrayList(usize).empty,
            };
        }

        /// Clean up resources
        pub fn deinit(self: *Self) void {
            self.codegen.deinit();
            self.symbol_locations.deinit();
            self.mutable_var_slots.deinit();
            self.join_points.deinit();
            self.proc_registry.deinit();
            self.compiled_lambdas.deinit();
            self.pending_calls.deinit(self.allocator);
            // Clean up the nested ArrayLists in join_point_jumps
            var it = self.join_point_jumps.valueIterator();
            while (it.next()) |list| {
                list.deinit(self.allocator);
            }
            self.join_point_jumps.deinit();
            self.join_point_param_layouts.deinit();
            self.join_point_param_patterns.deinit();
            self.early_return_patches.deinit(self.allocator);
        }

        /// Reset the code generator for generating a new expression
        pub fn reset(self: *Self) void {
            self.codegen.reset();
            self.symbol_locations.clearRetainingCapacity();
            self.mutable_var_slots.clearRetainingCapacity();
            self.join_points.clearRetainingCapacity();
            self.current_recursive_symbol = null;
            self.current_recursive_join_point = null;
            self.current_binding_symbol = null;
            self.proc_registry.clearRetainingCapacity();
            self.compiled_lambdas.clearRetainingCapacity();
            self.pending_calls.clearRetainingCapacity();
            // Clear nested ArrayLists
            var it = self.join_point_jumps.valueIterator();
            while (it.next()) |list| {
                list.clearRetainingCapacity();
            }
            self.join_point_jumps.clearRetainingCapacity();
            self.join_point_param_layouts.clearRetainingCapacity();
            self.join_point_param_patterns.clearRetainingCapacity();
        }

        /// Generate code for a Mono IR expression
        ///
        /// The generated code follows the calling convention:
        /// - First arg (RDI/X0) contains the pointer to the result buffer
        /// - Second arg (RSI/X1) contains the pointer to RocOps
        /// - The function writes the result to the result buffer and returns
        ///
        /// For tuples, pass tuple_len > 1 to copy all elements to the result buffer.
        pub fn generateCode(
            self: *Self,
            expr_id: MonoExprId,
            result_layout: layout.Idx,
            tuple_len: usize,
        ) Error!CodeResult {
            // Clear any leftover state from compileAllProcs to ensure clean slate
            // for the main expression. This is critical because procedure compilation
            // uses positive stack offsets while main expression uses negative offsets.
            self.symbol_locations.clearRetainingCapacity();
            self.mutable_var_slots.clearRetainingCapacity();

            // Track where the main expression code starts
            // (procedures may have been compiled before this, at the start of the buffer)
            const main_code_start = self.codegen.currentOffset();

            // Reserve argument registers so they don't get allocated for temporaries
            // X0/RDI = result pointer, X1/RSI = RocOps pointer
            self.reserveArgumentRegisters();

            // Emit prologue to save callee-saved registers we'll use (X19 for result ptr)
            try self.emitMainPrologue();

            // IMPORTANT: Save the result pointer and RocOps pointer to callee-saved registers
            // before generating code that might call procedures (which would clobber them).
            // On aarch64: save X0 to X19, X1 to X20 (callee-saved)
            // On x86_64: save RDI to RBX, RSI to R12 (callee-saved)
            const result_ptr_save_reg = if (comptime builtin.cpu.arch == .aarch64)
                aarch64.GeneralReg.X19
            else
                x86_64.GeneralReg.RBX;

            const roc_ops_save_reg = if (comptime builtin.cpu.arch == .aarch64)
                aarch64.GeneralReg.X20
            else
                x86_64.GeneralReg.R12;

            try self.emitMovRegReg(result_ptr_save_reg, if (comptime builtin.cpu.arch == .aarch64)
                aarch64.GeneralReg.X0
            else
                x86_64.GeneralReg.RDI);

            try self.emitMovRegReg(roc_ops_save_reg, if (comptime builtin.cpu.arch == .aarch64)
                aarch64.GeneralReg.X1
            else
                x86_64.GeneralReg.RSI);

            // Store RocOps save reg for use by Dec operations
            self.roc_ops_reg = roc_ops_save_reg;

            // Generate code for the expression - result ends up in a register
            const result_loc = try self.generateExpr(expr_id);

            // Store result to the saved result pointer
            try self.storeResultToSavedPtr(result_loc, result_layout, result_ptr_save_reg, tuple_len);

            // Emit epilogue to restore callee-saved registers and return
            try self.emitMainEpilogue();

            // Patch all pending calls now that all procedures are compiled
            try self.patchPendingCalls();

            // Get ALL the generated code (including procedures at the start)
            // Execution will start at main_code_start via entry_offset
            const all_code = self.codegen.getCode();

            // Make a copy of the code since codegen buffer may be reused
            const code_copy = self.allocator.dupe(u8, all_code) catch return Error.OutOfMemory;

            return CodeResult{
                .code = code_copy,
                .relocations = self.codegen.relocations.items,
                .result_layout = result_layout,
                .entry_offset = main_code_start,
            };
        }

        /// Reserve argument registers so they don't get allocated for temporaries
        fn reserveArgumentRegisters(self: *Self) void {
            if (comptime builtin.cpu.arch == .aarch64) {
                // Clear X0 and X1 from the free register mask
                // X0 = bit 0, X1 = bit 1
                self.codegen.free_general &= ~@as(u32, 0b11);
                // Reserve X19 (result pointer) and X20 (RocOps pointer)
                const x19_bit = @as(u32, 1) << @intFromEnum(aarch64.GeneralReg.X19);
                const x20_bit = @as(u32, 1) << @intFromEnum(aarch64.GeneralReg.X20);
                self.codegen.callee_saved_available &= ~(x19_bit | x20_bit);
            } else {
                // Clear RDI and RSI from the free register mask
                // RDI = 7, RSI = 6
                const rdi_bit = @as(u32, 1) << @intFromEnum(x86_64.GeneralReg.RDI);
                const rsi_bit = @as(u32, 1) << @intFromEnum(x86_64.GeneralReg.RSI);
                self.codegen.free_general &= ~(rdi_bit | rsi_bit);
                // Reserve RBX (result pointer) and R12 (RocOps pointer)
                const rbx_bit = @as(u32, 1) << @intFromEnum(x86_64.GeneralReg.RBX);
                const r12_bit = @as(u32, 1) << @intFromEnum(x86_64.GeneralReg.R12);
                self.codegen.callee_saved_available &= ~(rbx_bit | r12_bit);
            }
        }

        /// Get the layout of an expression (if available and valid for our layout store)
        fn getExprLayout(self: *Self, expr_id: MonoExprId) ?layout.Idx {
            const expr = self.store.getExpr(expr_id);
            const raw_layout: ?layout.Idx = switch (expr) {
                // Expressions that store their layout
                .record => |rec| rec.record_layout,
                .tuple => |tup| tup.tuple_layout,
                .tag => |tag| tag.union_layout,
                .lookup => |lookup| lookup.layout_idx,
                .field_access => |fa| fa.field_layout,
                .binop => |binop| binop.result_layout,
                .unary_minus => |um| um.result_layout,
                .call => |call| call.ret_layout,
                .low_level => |ll| ll.ret_layout,
                // Compound expressions with result layouts
                .if_then_else => |ite| ite.result_layout,
                .when => |w| w.result_layout,
                .block => |b| b.result_layout,
                .dbg => |d| d.result_layout,
                .expect => |e| e.result_layout,
                .early_return => |er| er.ret_layout,
                // Literals with known layouts
                .i64_literal => .i64,
                .f64_literal => .f64,
                .f32_literal => .f32,
                .bool_literal => .bool,
                .i128_literal => .i128,
                .dec_literal => .dec,
                // For other expressions, no layout available
                else => null,
            };

            if (raw_layout) |layout_idx| {
                return layout_idx;
            }
            return null;
        }

        /// Generate code for an expression. The result is ALWAYS in a stable location
        /// (stack, immediate, lambda_code, closure_value) — never a bare register.
        fn generateExpr(self: *Self, expr_id: MonoExprId) Error!ValueLocation {
            const loc = try self.generateExprRaw(expr_id);
            return self.stabilize(loc);
        }

        /// Spill bare register values to the stack. All other locations pass through.
        fn stabilize(self: *Self, loc: ValueLocation) Error!ValueLocation {
            return switch (loc) {
                .general_reg => |reg| {
                    const slot = self.codegen.allocStackSlot(8);
                    try self.codegen.emitStoreStack(.w64, slot, reg);
                    self.codegen.freeGeneral(reg);
                    return .{ .stack = slot };
                },
                .float_reg => |reg| {
                    const slot = self.codegen.allocStackSlot(8);
                    try self.codegen.emitStoreStackF64(slot, reg);
                    self.codegen.freeFloat(reg);
                    return .{ .stack = slot };
                },
                else => loc,
            };
        }

        /// Generate code for an expression (raw — may return bare register locations).
        fn generateExprRaw(self: *Self, expr_id: MonoExprId) Error!ValueLocation {
            const expr = self.store.getExpr(expr_id);

            return switch (expr) {
                // Literals
                .i64_literal => |val| .{ .immediate_i64 = val },
                .i128_literal => |val| try self.generateI128Literal(val),
                .f64_literal => |val| .{ .immediate_f64 = val },
                .f32_literal => |val| .{ .immediate_f64 = @floatCast(val) },
                .bool_literal => |val| .{ .immediate_i64 = if (val) 1 else 0 },
                .dec_literal => |val| try self.generateI128Literal(val),

                // Lookups
                .lookup => |lookup| try self.generateLookup(lookup.symbol, lookup.layout_idx),

                // Binary operations
                .binop => |binop| try self.generateBinop(binop),

                // Unary operations
                .unary_minus => |unary| try self.generateUnaryMinus(unary),
                .unary_not => |unary| try self.generateUnaryNot(unary),

                // Control flow
                .if_then_else => |ite| try self.generateIfThenElse(ite),
                .when => |when_expr| try self.generateWhen(when_expr),

                // Blocks
                .block => |block| try self.generateBlock(block),

                // Function calls and lambdas
                .call => |call| try self.generateCall(call),
                // Lambdas and closures as first-class values (stored to variables)
                .lambda => |lambda| {
                    const code_offset = self.compileLambdaAsProc(expr_id, lambda) catch {
                        // Can't compile as proc (e.g., body references mutable variables
                        // or captures from enclosing scope). Return as closure_value so
                        // it can be dispatched via callLambdaBodyDirect at call sites.
                        return .{ .closure_value = .{
                            .stack_offset = 0,
                            .representation = .direct_call,
                            .lambda = expr_id,
                            .captures = mono.MonoIR.MonoCaptureSpan.empty(),
                        } };
                    };
                    return .{ .lambda_code = .{
                        .code_offset = code_offset,
                        .ret_layout = lambda.ret_layout,
                    } };
                },
                .closure => |closure| {
                    return try self.generateClosure(closure);
                },

                // Records
                .empty_record => .{ .immediate_i64 = 0 },
                .record => |record| try self.generateRecord(record),
                .field_access => |fa| try self.generateFieldAccess(fa),

                // Tuples
                .tuple => |tuple| try self.generateTuple(tuple),
                .tuple_access => |ta| try self.generateTupleAccess(ta),

                // Tags (tagged unions)
                .zero_arg_tag => |tag| try self.generateZeroArgTag(tag),
                .tag => |tag| try self.generateTag(tag),

                // Lists (not fully implemented - returns placeholder for now)
                .list => |list| try self.generateList(list),
                .empty_list => try self.generateEmptyList(),

                // Low-level operations
                .low_level => |ll| try self.generateLowLevel(ll),

                // Nominal types (transparent wrappers)
                .nominal => |nom| try self.generateExpr(nom.backing_expr),

                // String literals
                .str_literal => |str_idx| try self.generateStrLiteral(str_idx),

                // Reference counting operations
                .incref => |rc_op| try self.generateIncref(rc_op),
                .decref => |rc_op| try self.generateDecref(rc_op),
                .free => |rc_op| try self.generateFree(rc_op),

                // For loop over a list
                .for_loop => |for_loop| try self.generateForLoop(for_loop),

                // While loop
                .while_loop => |while_loop| try self.generateWhileLoop(while_loop),

                // Early return from a block
                .early_return => |er| try self.generateEarlyReturn(er),

                // Debug and assertions
                .dbg => |dbg_expr| try self.generateDbg(dbg_expr),
                .expect => |expect_expr| try self.generateExpect(expect_expr),

                // Crash and runtime errors
                .crash => return Error.Crash,
                .runtime_error => return Error.RuntimeError,

                // String formatting for inspect
                .str_concat => |exprs| try self.generateStrConcat(exprs),
                .int_to_str => |its| try self.generateIntToStr(its),
                .float_to_str => |fts| try self.generateFloatToStr(fts),
                .dec_to_str => |dec_expr| try self.generateDecToStr(dec_expr),
                .str_escape_and_quote => |quote_expr| try self.generateStrEscapeAndQuote(quote_expr),

                // Discriminant switch for tag unions
                .discriminant_switch => |ds| try self.generateDiscriminantSwitch(ds),
            };
        }

        /// Generate code for low-level operations
        fn generateLowLevel(self: *Self, ll: anytype) Error!ValueLocation {
            const args = self.store.getExprSpan(ll.args);

            switch (ll.op) {
                .list_len => {
                    // List is a (ptr, len, capacity) triple - length is at offset 8
                    if (args.len < 1) return Error.UnsupportedExpression;
                    const list_loc = try self.generateExpr(args[0]);

                    // Get base offset from either stack or list_stack location
                    const base_offset: i32 = switch (list_loc) {
                        .stack => |off| off,
                        .list_stack => |ls_info| ls_info.struct_offset,
                        .immediate_i64 => |val| {
                            // Empty list - length is 0
                            if (val == 0) {
                                return .{ .immediate_i64 = 0 };
                            }
                            return Error.UnsupportedExpression;
                        },
                        else => return Error.UnsupportedExpression,
                    };

                    // Length is at offset 8 in the list struct
                    const result_reg = try self.allocTempGeneral();
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.ldrRegMemSoff(.w64, result_reg, .FP, base_offset + 8);
                    } else {
                        try self.codegen.emit.movRegMem(.w64, result_reg, .RBP, base_offset + 8);
                    }
                    return .{ .general_reg = result_reg };
                },
                .list_is_empty => {
                    // List is empty if length is 0
                    if (args.len < 1) return Error.UnsupportedExpression;
                    const list_loc = try self.generateExpr(args[0]);

                    switch (list_loc) {
                        .stack => |base_offset| {
                            // Length is at offset 8 - check if zero
                            const len_reg = try self.allocTempGeneral();
                            if (comptime builtin.cpu.arch == .aarch64) {
                                try self.codegen.emit.ldrRegMemSoff(.w64, len_reg, .FP, base_offset + 8);
                            } else {
                                try self.codegen.emit.movRegMem(.w64, len_reg, .RBP, base_offset + 8);
                            }
                            // Compare with 0
                            try self.emitCmpImm(len_reg, 0);
                            // Set result to 1 if equal (empty), 0 otherwise
                            const result_reg = try self.allocTempGeneral();
                            try self.codegen.emitLoadImm(result_reg, 0);
                            const one_reg = try self.allocTempGeneral();
                            try self.codegen.emitLoadImm(one_reg, 1);
                            if (comptime builtin.cpu.arch == .aarch64) {
                                try self.codegen.emit.csel(.w64, result_reg, one_reg, result_reg, .eq);
                            } else {
                                try self.codegen.emit.cmovcc(.equal, .w64, result_reg, one_reg);
                            }
                            self.codegen.freeGeneral(one_reg);
                            self.codegen.freeGeneral(len_reg);
                            return .{ .general_reg = result_reg };
                        },
                        .immediate_i64 => |val| {
                            // Empty list - is_empty returns true (1)
                            if (val == 0) {
                                return .{ .immediate_i64 = 1 };
                            }
                            return Error.UnsupportedExpression;
                        },
                        else => return Error.UnsupportedExpression,
                    }
                },
                .list_with_capacity => {
                    // listWithCapacity(capacity, alignment, elem_width, elements_refcounted,
                    //                  inc_context, inc, roc_ops) -> RocList
                    if (args.len < 1) {
                        std.debug.print("BUG: list_with_capacity requires at least 1 argument\n", .{});
                        unreachable;
                    }

                    const roc_ops_reg = self.roc_ops_reg orelse {
                        std.debug.print("BUG: list_with_capacity requires roc_ops_reg\n", .{});
                        unreachable;
                    };
                    const capacity_loc = try self.generateExpr(args[0]);

                    // Get element layout from return type (which is List(elem))
                    const ls = self.layout_store orelse {
                        std.debug.print("BUG: list_with_capacity requires layout_store\n", .{});
                        unreachable;
                    };
                    const ret_layout = ls.getLayout(ll.ret_layout);

                    const elem_size_align: layout.SizeAlign = switch (ret_layout.tag) {
                        .list => blk: {
                            const elem_layout = ls.getLayout(ret_layout.data.list);
                            break :blk ls.layoutSizeAlign(elem_layout);
                        },
                        .list_of_zst => .{ .size = 0, .alignment = .@"1" },
                        else => unreachable, // list_with_capacity must return a list
                    };

                    const fn_addr: usize = @intFromPtr(&listWithCapacityC);
                    const rc_none_addr: usize = @intFromPtr(&rcNone);

                    // Convert RocAlignment enum to actual byte alignment
                    const alignment_bytes = elem_size_align.alignment.toByteUnits();

                    // Allocate stack space for result (RocList = 24 bytes)
                    const result_offset = self.codegen.allocStackSlot(24);

                    if (comptime builtin.cpu.arch == .aarch64) {
                        // aarch64 calling convention: X0-X7 for args
                        // listWithCapacityC(out, capacity, alignment, elem_width, elements_refcounted,
                        //                   inc_context, inc, roc_ops) -> void
                        // Using C wrapper to avoid ABI issues with 24-byte struct return

                        // IMPORTANT: First save capacity to X1 before we clobber any argument registers.
                        // capacity_loc might be in X0-X7, and we need to preserve it.
                        const cap_reg = try self.ensureInGeneralReg(capacity_loc);
                        try self.codegen.emit.movRegReg(.w64, .X1, cap_reg);
                        self.codegen.freeGeneral(cap_reg);

                        // Now set the other argument registers (X2-X7)
                        try self.codegen.emitLoadImm(.X2, @intCast(alignment_bytes));
                        try self.codegen.emitLoadImm(.X3, @intCast(elem_size_align.size));
                        try self.codegen.emitLoadImm(.X4, 0); // elements_refcounted = false
                        try self.codegen.emitLoadImm(.X5, 0); // inc_context = null
                        try self.codegen.emitLoadImm(.X6, @intCast(rc_none_addr));
                        try self.codegen.emit.movRegReg(.w64, .X7, roc_ops_reg);

                        // X0 = output pointer (FP + result_offset)
                        try self.codegen.emit.movRegImm64(.X0, @bitCast(@as(i64, result_offset)));
                        try self.codegen.emit.addRegRegReg(.w64, .X0, .FP, .X0);

                        // Load function address into X9 right before the call
                        // (after all register allocations to avoid X9 being clobbered)
                        try self.codegen.emitLoadImm(.X9, @intCast(fn_addr));

                        // Call
                        try self.codegen.emit.blrReg(.X9);

                        // No result storage needed - function writes directly to output pointer
                    } else if (comptime builtin.cpu.arch == .x86_64) {
                        // x86_64 calling convention: RDI, RSI, RDX, RCX, R8, R9, then stack
                        // listWithCapacityC(out, capacity, alignment, elem_width, elements_refcounted,
                        //                   inc_context, inc, roc_ops) -> void
                        const cap_reg = try self.ensureInGeneralReg(capacity_loc);

                        // Push 8th arg (roc_ops) and 7th arg (inc) to stack
                        try self.codegen.emit.pushReg(roc_ops_reg);
                        try self.codegen.emitLoadImm(.R11, @intCast(rc_none_addr));
                        try self.codegen.emit.pushReg(.R11);

                        // RDI = output pointer
                        try self.codegen.emit.leaRegMem(.RDI, .RBP, result_offset);

                        // RSI = capacity
                        try self.codegen.emit.movRegReg(.w64, .RSI, cap_reg);
                        self.codegen.freeGeneral(cap_reg);

                        try self.codegen.emitLoadImm(.RDX, @intCast(alignment_bytes));
                        try self.codegen.emitLoadImm(.RCX, @intCast(elem_size_align.size));
                        try self.codegen.emitLoadImm(.R8, 0); // elements_refcounted = false
                        try self.codegen.emitLoadImm(.R9, 0); // inc_context = null

                        try self.codegen.emitLoadImm(.R11, @intCast(fn_addr));
                        try self.codegen.emit.callReg(.R11);

                        // Clean up stack args (2 * 8 bytes)
                        try self.codegen.emit.addImm(.RSP, 16);

                        // No result storage needed - function writes directly to output pointer
                    } else {
                        unreachable;
                    }

                    // Return as .list_stack so recursive calls properly detect this as a list argument
                    return .{ .list_stack = .{
                        .struct_offset = result_offset,
                        .data_offset = 0, // Data location is stored in the list struct itself
                        .num_elements = 0, // Unknown at compile time
                    } };
                },
                .list_append => {
                    // list_append(list, element) -> List
                    // Uses SAFE listAppendSafeC that reserves capacity if needed
                    if (args.len != 2) {
                        std.debug.print("BUG: list_append requires exactly 2 arguments, got {}\n", .{args.len});
                        unreachable;
                    }

                    const ls = self.layout_store orelse {
                        std.debug.print("BUG: list_append requires layout_store\n", .{});
                        unreachable;
                    };

                    const roc_ops_reg = self.roc_ops_reg orelse {
                        std.debug.print("BUG: list_append requires roc_ops_reg\n", .{});
                        unreachable;
                    };

                    // Generate list argument (must be on stack - 24 bytes)
                    const list_loc = try self.generateExpr(args[0]);

                    // Generate element value
                    const elem_loc = try self.generateExpr(args[1]);

                    // Determine element size from layout - this is the reliable way to detect ZST
                    // Using the LAYOUT rather than the generated value avoids issues with
                    // test interference or stale values
                    const elem_size_align: layout.SizeAlign = blk: {
                        const ret_layout_val = ls.getLayout(ll.ret_layout);
                        break :blk switch (ret_layout_val.tag) {
                            .list => ls.layoutSizeAlign(ls.getLayout(ret_layout_val.data.list)),
                            .list_of_zst => .{ .size = 0, .alignment = .@"1" },
                            else => unreachable, // list_append must return a list
                        };
                    };

                    // ZST detection based on LAYOUT size, not generated value
                    const is_zst = (elem_size_align.size == 0);
                    const list_offset: i32 = switch (list_loc) {
                        .stack => |off| off,
                        .list_stack => |ls_info| ls_info.struct_offset,
                        .immediate_i64 => |val| blk: {
                            // Empty list case: materialize on stack (ptr=0, len=0, capacity=0)
                            if (val != 0) {
                                std.debug.print("BUG: list_append got immediate_i64 that's not 0: {}\n", .{val});
                                unreachable;
                            }
                            const slot = self.codegen.allocStackSlot(24);
                            const temp = try self.allocTempGeneral();
                            try self.codegen.emitLoadImm(temp, 0);
                            if (comptime builtin.cpu.arch == .aarch64) {
                                try self.codegen.emit.strRegMemSoff(.w64, temp, .FP, slot);
                                try self.codegen.emit.strRegMemSoff(.w64, temp, .FP, slot + 8);
                                try self.codegen.emit.strRegMemSoff(.w64, temp, .FP, slot + 16);
                            } else {
                                try self.codegen.emit.movMemReg(.w64, .RBP, slot, temp);
                                try self.codegen.emit.movMemReg(.w64, .RBP, slot + 8, temp);
                                try self.codegen.emit.movMemReg(.w64, .RBP, slot + 16, temp);
                            }
                            self.codegen.freeGeneral(temp);
                            break :blk slot;
                        },
                        else => {
                            std.debug.print("BUG: list_append list arg must be on stack: {s}\n", .{@tagName(list_loc)});
                            unreachable;
                        },
                    };

                    // Ensure element is on stack
                    const elem_offset: i32 = try self.ensureOnStack(elem_loc, elem_size_align.size);

                    // Allocate result slot (24 bytes for RocList)
                    const result_offset = self.codegen.allocStackSlot(24);

                    const copy_fallback_addr: usize = @intFromPtr(&copy_fallback);

                    // For ZST (zero-sized types), use the unsafe version since no capacity is needed.
                    // For regular elements, use the safe version that reserves capacity.
                    if (is_zst) {
                        // ZST: use listAppendUnsafeC (fewer args, doesn't need capacity reservation)
                        const fn_addr: usize = @intFromPtr(&listAppendUnsafeC);

                        if (comptime builtin.cpu.arch == .aarch64) {
                            // listAppendUnsafeC(out, list_bytes, list_len, list_cap, element, elem_width, copy_fn) -> void

                            // X0 = output pointer (FP + result_offset)
                            try self.codegen.emit.movRegImm64(.X0, @bitCast(@as(i64, result_offset)));
                            try self.codegen.emit.addRegRegReg(.w64, .X0, .FP, .X0);

                            // X1-X3 = list fields (bytes, length, capacity)
                            try self.codegen.emit.ldrRegMemSoff(.w64, .X1, .FP, list_offset);
                            try self.codegen.emit.ldrRegMemSoff(.w64, .X2, .FP, list_offset + 8);
                            try self.codegen.emit.ldrRegMemSoff(.w64, .X3, .FP, list_offset + 16);

                            // X4 = element pointer (FP + elem_offset)
                            try self.codegen.emit.movRegImm64(.X4, @bitCast(@as(i64, elem_offset)));
                            try self.codegen.emit.addRegRegReg(.w64, .X4, .FP, .X4);

                            // X5 = element width (0 for ZST)
                            try self.codegen.emitLoadImm(.X5, 0);

                            // X6 = copy_fallback
                            try self.codegen.emitLoadImm(.X6, @intCast(copy_fallback_addr));

                            // Load function address into X9 right before the call
                            try self.codegen.emitLoadImm(.X9, @intCast(fn_addr));

                            // Call
                            try self.codegen.emit.blrReg(.X9);
                        } else if (comptime builtin.cpu.arch == .x86_64) {
                            // listAppendUnsafeC(out, list_bytes, list_len, list_cap, element, elem_width, copy_fn) -> void

                            // Push 7th arg (copy_fn) to stack
                            try self.codegen.emitLoadImm(.R11, @intCast(copy_fallback_addr));
                            try self.codegen.emit.pushReg(.R11);

                            // RDI = output pointer
                            try self.codegen.emit.leaRegMem(.RDI, .RBP, result_offset);

                            // RSI, RDX, RCX = list fields
                            try self.codegen.emit.movRegMem(.w64, .RSI, .RBP, list_offset);
                            try self.codegen.emit.movRegMem(.w64, .RDX, .RBP, list_offset + 8);
                            try self.codegen.emit.movRegMem(.w64, .RCX, .RBP, list_offset + 16);

                            // R8 = element pointer
                            try self.codegen.emit.leaRegMem(.R8, .RBP, elem_offset);

                            // R9 = element width (0 for ZST)
                            try self.codegen.emitLoadImm(.R9, 0);

                            try self.codegen.emitLoadImm(.R11, @intCast(fn_addr));
                            try self.codegen.emit.callReg(.R11);

                            // Clean up stack arg
                            try self.codegen.emit.addImm(.RSP, 8);
                        } else {
                            unreachable;
                        }
                    } else {
                        // Non-ZST: use listAppendSafeC which reserves capacity
                        const fn_addr: usize = @intFromPtr(&listAppendSafeC);
                        const alignment_bytes = elem_size_align.alignment.toByteUnits();

                        if (comptime builtin.cpu.arch == .aarch64) {
                            // listAppendSafeC(out, list_bytes, list_len, list_cap, element,
                            //                 alignment, elem_width, elements_refcounted, copy_fn, roc_ops) -> void
                            // 10 args: X0-X7 + 2 on stack

                            // Allocate stack space for 2 args (16 bytes, aligned)
                            try self.codegen.emit.subRegRegImm12(.w64, .ZRSP, .ZRSP, 16);

                            // Store stack args: copy_fn at [SP+0], roc_ops at [SP+8]
                            const temp = try self.allocTempGeneral();
                            try self.codegen.emitLoadImm(temp, @intCast(copy_fallback_addr));
                            try self.codegen.emit.strRegMemUoff(.w64, temp, .ZRSP, 0); // copy_fn at SP+0
                            try self.codegen.emit.strRegMemUoff(.w64, roc_ops_reg, .ZRSP, 1); // roc_ops at SP+8 (scaled by 8)
                            self.codegen.freeGeneral(temp);

                            // X0 = output pointer (FP + result_offset)
                            try self.codegen.emit.movRegImm64(.X0, @bitCast(@as(i64, result_offset)));
                            try self.codegen.emit.addRegRegReg(.w64, .X0, .FP, .X0);

                            // X1-X3 = list fields (bytes, length, capacity)
                            try self.codegen.emit.ldrRegMemSoff(.w64, .X1, .FP, list_offset);
                            try self.codegen.emit.ldrRegMemSoff(.w64, .X2, .FP, list_offset + 8);
                            try self.codegen.emit.ldrRegMemSoff(.w64, .X3, .FP, list_offset + 16);

                            // X4 = element pointer (FP + elem_offset)
                            try self.codegen.emit.movRegImm64(.X4, @bitCast(@as(i64, elem_offset)));
                            try self.codegen.emit.addRegRegReg(.w64, .X4, .FP, .X4);

                            // X5 = alignment
                            try self.codegen.emitLoadImm(.X5, @intCast(alignment_bytes));

                            // X6 = element width
                            try self.codegen.emitLoadImm(.X6, @intCast(elem_size_align.size));

                            // X7 = elements_refcounted (false = 0)
                            try self.codegen.emitLoadImm(.X7, 0);

                            // Load function address into X9 right before the call
                            try self.codegen.emitLoadImm(.X9, @intCast(fn_addr));

                            // Call
                            try self.codegen.emit.blrReg(.X9);

                            // Clean up stack (16 bytes)
                            try self.codegen.emit.addRegRegImm12(.w64, .ZRSP, .ZRSP, 16);
                        } else if (comptime builtin.cpu.arch == .x86_64) {
                            // x86_64 calling convention: RDI, RSI, RDX, RCX, R8, R9, then stack
                            // listAppendSafeC(out, list_bytes, list_len, list_cap, element,
                            //                 alignment, elem_width, elements_refcounted, copy_fn, roc_ops) -> void
                            // 10 args: 6 in regs + 4 on stack

                            // Push stack args in reverse order
                            try self.codegen.emit.pushReg(roc_ops_reg); // arg 10: roc_ops
                            try self.codegen.emitLoadImm(.R11, @intCast(copy_fallback_addr));
                            try self.codegen.emit.pushReg(.R11); // arg 9: copy_fn
                            try self.codegen.emitLoadImm(.R11, 0);
                            try self.codegen.emit.pushReg(.R11); // arg 8: elements_refcounted = false
                            try self.codegen.emitLoadImm(.R11, @intCast(elem_size_align.size));
                            try self.codegen.emit.pushReg(.R11); // arg 7: elem_width

                            // RDI = output pointer
                            try self.codegen.emit.leaRegMem(.RDI, .RBP, result_offset);

                            // RSI, RDX, RCX = list fields
                            try self.codegen.emit.movRegMem(.w64, .RSI, .RBP, list_offset);
                            try self.codegen.emit.movRegMem(.w64, .RDX, .RBP, list_offset + 8);
                            try self.codegen.emit.movRegMem(.w64, .RCX, .RBP, list_offset + 16);

                            // R8 = element pointer
                            try self.codegen.emit.leaRegMem(.R8, .RBP, elem_offset);

                            // R9 = alignment
                            try self.codegen.emitLoadImm(.R9, @intCast(alignment_bytes));

                            try self.codegen.emitLoadImm(.R11, @intCast(fn_addr));
                            try self.codegen.emit.callReg(.R11);

                            // Clean up stack args (4 * 8 bytes = 32)
                            try self.codegen.emit.addImm(.RSP, 32);
                        } else {
                            unreachable;
                        }
                    }

                    // Return as .list_stack so recursive calls properly detect this as a list argument
                    return .{ .list_stack = .{
                        .struct_offset = result_offset,
                        .data_offset = 0, // Data location is stored in the list struct itself
                        .num_elements = 0, // Unknown at compile time
                    } };
                },
                .list_get => {
                    // list_get(list, index) -> element
                    // Unsafe variant (no bounds checking), called by List.get_unsafe
                    if (args.len < 2) return Error.UnsupportedExpression;
                    const list_loc = try self.generateExpr(args[0]);
                    const index_loc = try self.generateExpr(args[1]);

                    // Get base offset of list struct
                    const list_base: i32 = switch (list_loc) {
                        .stack => |off| off,
                        .list_stack => |ls_info| ls_info.struct_offset,
                        else => return Error.UnsupportedExpression,
                    };

                    // Get element size from ret_layout
                    const ls = self.layout_store orelse unreachable;
                    const ret_layout_val = ls.getLayout(ll.ret_layout);
                    const elem_size: u32 = ls.layoutSizeAlign(ret_layout_val).size;

                    if (elem_size == 0) {
                        // ZST element - no actual data to load
                        return .{ .immediate_i64 = 0 };
                    }

                    // Get index into a register
                    const index_reg = try self.allocTempGeneral();
                    switch (index_loc) {
                        .immediate_i64 => |val| {
                            try self.codegen.emitLoadImm(index_reg, val);
                        },
                        .general_reg => |reg| {
                            try self.emitMovRegReg(index_reg, reg);
                            self.codegen.freeGeneral(reg);
                        },
                        .stack => |off| {
                            if (comptime builtin.cpu.arch == .aarch64) {
                                try self.codegen.emit.ldrRegMemSoff(.w64, index_reg, .FP, off);
                            } else {
                                try self.codegen.emit.movRegMem(.w64, index_reg, .RBP, off);
                            }
                        },
                        else => return Error.UnsupportedExpression,
                    }

                    // Load list pointer (offset 0 in list struct)
                    const ptr_reg = try self.allocTempGeneral();
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.ldrRegMemSoff(.w64, ptr_reg, .FP, list_base);
                    } else {
                        try self.codegen.emit.movRegMem(.w64, ptr_reg, .RBP, list_base);
                    }

                    // Calculate element address: ptr + index * elem_size
                    const addr_reg = try self.allocTempGeneral();
                    try self.codegen.emit.movRegReg(.w64, addr_reg, index_reg);
                    self.codegen.freeGeneral(index_reg);

                    if (elem_size != 1) {
                        const size_reg = try self.allocTempGeneral();
                        try self.codegen.emitLoadImm(size_reg, elem_size);
                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emit.mulRegRegReg(.w64, addr_reg, addr_reg, size_reg);
                        } else {
                            try self.codegen.emit.imulRegReg(.w64, addr_reg, size_reg);
                        }
                        self.codegen.freeGeneral(size_reg);
                    }

                    // Add base pointer
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.addRegRegReg(.w64, addr_reg, addr_reg, ptr_reg);
                    } else {
                        try self.codegen.emit.addRegReg(.w64, addr_reg, ptr_reg);
                    }
                    self.codegen.freeGeneral(ptr_reg);

                    // Load element to stack slot
                    const elem_slot = self.codegen.allocStackSlot(@intCast(elem_size));
                    const temp_reg = try self.allocTempGeneral();

                    if (elem_size <= 8) {
                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emit.ldrRegMemSoff(.w64, temp_reg, addr_reg, 0);
                            try self.codegen.emit.strRegMemSoff(.w64, temp_reg, .FP, elem_slot);
                        } else {
                            try self.codegen.emit.movRegMem(.w64, temp_reg, addr_reg, 0);
                            try self.codegen.emit.movMemReg(.w64, .RBP, elem_slot, temp_reg);
                        }
                    } else {
                        // For larger elements, copy in 8-byte chunks
                        var copied: u32 = 0;
                        while (copied < elem_size) : (copied += 8) {
                            if (comptime builtin.cpu.arch == .aarch64) {
                                try self.codegen.emit.ldrRegMemSoff(.w64, temp_reg, addr_reg, @intCast(copied));
                                try self.codegen.emit.strRegMemSoff(.w64, temp_reg, .FP, elem_slot + @as(i32, @intCast(copied)));
                            } else {
                                try self.codegen.emit.movRegMem(.w64, temp_reg, addr_reg, @intCast(copied));
                                try self.codegen.emit.movMemReg(.w64, .RBP, elem_slot + @as(i32, @intCast(copied)), temp_reg);
                            }
                        }
                    }

                    self.codegen.freeGeneral(temp_reg);
                    self.codegen.freeGeneral(addr_reg);

                    // Return with appropriate value location based on element type
                    if (ll.ret_layout == .i128 or ll.ret_layout == .u128 or ll.ret_layout == .dec) {
                        return .{ .stack_i128 = elem_slot };
                    } else if (ll.ret_layout == .str) {
                        return .{ .stack_str = elem_slot };
                    } else if (ret_layout_val.tag == .list or ret_layout_val.tag == .list_of_zst) {
                        return .{ .list_stack = .{
                            .struct_offset = elem_slot,
                            .data_offset = 0,
                            .num_elements = 0,
                        } };
                    } else {
                        return .{ .stack = elem_slot };
                    }
                },
                .list_concat,
                .list_prepend,
                .list_repeat,
                .list_drop_first,
                .list_drop_last,
                .list_take_first,
                .list_take_last,
                => {
                    // These list operations are complex and require calling builtin functions
                    std.debug.print("UNIMPLEMENTED low-level op in MonoExprCodeGen: {s}\n", .{@tagName(ll.op)});
                    std.debug.print("  args.len: {}\n", .{args.len});
                    unreachable;
                },
                else => {
                    std.debug.print("UNIMPLEMENTED low-level op in MonoExprCodeGen: {s}\n", .{@tagName(ll.op)});
                    return Error.UnsupportedExpression;
                },
            }
        }

        /// Generate code for an i128 literal
        fn generateI128Literal(_: *Self, val: i128) Error!ValueLocation {
            // Return as immediate - will be materialized when needed
            return .{ .immediate_i128 = val };
        }

        /// Generate code for a symbol lookup
        fn generateLookup(self: *Self, symbol: MonoSymbol, _: layout.Idx) Error!ValueLocation {

            // Check if we have a location for this symbol
            const symbol_key: u48 = @bitCast(symbol);
            if (self.symbol_locations.get(symbol_key)) |loc| {
                return loc;
            }

            // Symbol not found - it might be a top-level definition
            if (self.store.getSymbolDef(symbol)) |def_expr_id| {
                // Generate code for the definition
                const loc = try self.generateExpr(def_expr_id);
                // Cache the location
                try self.symbol_locations.put(symbol_key, loc);
                return loc;
            }

            return Error.LocalNotFound;
        }

        /// Generate code for a binary operation
        fn generateBinop(self: *Self, binop: anytype) Error!ValueLocation {
            // Generate code for LHS first (always stable due to generateExpr wrapper)
            const lhs_loc = try self.generateExpr(binop.lhs);

            // Evaluate RHS (safe — LHS is in a stable location, never a bare register)
            const rhs_loc = try self.generateExpr(binop.rhs);

            // Check if this is a structural comparison (records/tuples/lists)
            // We need to check the layout, not the expression type, since the LHS
            // might be a function call that returns a record/tuple/list
            if (binop.op == .eq or binop.op == .neq) {
                const lhs_expr = self.store.getExpr(binop.lhs);
                const rhs_expr = self.store.getExpr(binop.rhs);

                // First try expression-based detection for direct literals (on either side)
                if (lhs_expr == .record or lhs_expr == .tuple) {
                    return self.generateStructuralComparison(lhs_loc, rhs_loc, lhs_expr, binop.op);
                }
                if (rhs_expr == .record) {
                    // RHS is a record literal - use layout-based comparison with its layout
                    if (self.layout_store != null) {
                        const record_layout = rhs_expr.record.record_layout;
                        return self.generateRecordComparisonByLayout(lhs_loc, rhs_loc, record_layout, binop.op);
                    }
                }
                if (rhs_expr == .tuple) {
                    return self.generateStructuralComparison(lhs_loc, rhs_loc, rhs_expr, binop.op);
                }
                if (lhs_expr == .list) {
                    return self.generateListComparison(lhs_loc, rhs_loc, lhs_expr, binop.op);
                }
                if (rhs_expr == .list) {
                    return self.generateListComparison(lhs_loc, rhs_loc, rhs_expr, binop.op);
                }

                // For calls/lookups/blocks, check the layout to detect composite types
                if (self.layout_store) |ls| {
                    // Try to get layout from LHS first, then RHS
                    const operand_layout: ?layout.Idx = switch (lhs_expr) {
                        .call => |call| call.ret_layout,
                        .lookup => |lookup| lookup.layout_idx,
                        .block => |block| block.result_layout,
                        else => switch (rhs_expr) {
                            .call => |call| call.ret_layout,
                            .lookup => |lookup| lookup.layout_idx,
                            .block => |block| block.result_layout,
                            else => null,
                        },
                    };

                    if (operand_layout) |op_layout| {
                        // Always check the layout tag to detect composite types
                        const stored_layout = ls.getLayout(op_layout);
                        if (stored_layout.tag == .record) {
                            return self.generateRecordComparisonByLayout(lhs_loc, rhs_loc, op_layout, binop.op);
                        } else if (stored_layout.tag == .tuple) {
                            return self.generateTupleComparisonByLayout(lhs_loc, rhs_loc, op_layout, binop.op);
                        } else if (stored_layout.tag == .list) {
                            return self.generateListComparisonByLayout(lhs_loc, rhs_loc, op_layout, binop.op);
                        }
                    }
                }
            }

            // Check if operands are i128/Dec (need special handling even for comparisons that return bool)
            const operands_are_i128 = switch (lhs_loc) {
                .immediate_i128, .stack_i128 => true,
                else => switch (rhs_loc) {
                    .immediate_i128, .stack_i128 => true,
                    else => false,
                },
            };

            // Determine if this is an integer or float operation
            const is_float = switch (binop.result_layout) {
                .f32, .f64 => true,
                else => false,
            };

            if (is_float) {
                return self.generateFloatBinop(binop.op, lhs_loc, rhs_loc);
            } else if (operands_are_i128 or binop.result_layout == .i128 or binop.result_layout == .u128 or binop.result_layout == .dec) {
                // Use i128 path for Dec/i128 operands (even for comparisons that return bool)
                // Convert .stack locations to .stack_i128 for Dec operations, since Dec values are 16 bytes
                // but may be stored with .stack location type (e.g., mutable variables)
                const is_dec_op = binop.result_layout == .dec or binop.result_layout == .i128 or binop.result_layout == .u128;
                const adj_lhs = if (is_dec_op and lhs_loc == .stack) ValueLocation{ .stack_i128 = lhs_loc.stack } else lhs_loc;
                const adj_rhs = if (is_dec_op and rhs_loc == .stack) ValueLocation{ .stack_i128 = rhs_loc.stack } else rhs_loc;
                return self.generateI128Binop(binop.op, adj_lhs, adj_rhs, binop.result_layout);
            } else {
                return self.generateIntBinop(binop.op, lhs_loc, rhs_loc, binop.result_layout);
            }
        }

        /// Generate integer binary operation
        fn generateIntBinop(
            self: *Self,
            op: MonoExpr.BinOp,
            lhs_loc: ValueLocation,
            rhs_loc: ValueLocation,
            result_layout: layout.Idx,
        ) Error!ValueLocation {
            // Load operands into registers
            const rhs_reg = try self.ensureInGeneralReg(rhs_loc);
            const lhs_reg = try self.ensureInGeneralReg(lhs_loc);

            // Allocate result register
            const result_reg = try self.allocTempGeneral();

            // Determine if this is an unsigned type (for division/modulo)
            const is_unsigned = switch (result_layout) {
                layout.Idx.u8, layout.Idx.u16, layout.Idx.u32, layout.Idx.u64, layout.Idx.u128 => true,
                else => false,
            };

            switch (op) {
                .add => try self.codegen.emitAdd(.w64, result_reg, lhs_reg, rhs_reg),
                .sub => try self.codegen.emitSub(.w64, result_reg, lhs_reg, rhs_reg),
                .mul => try self.codegen.emitMul(.w64, result_reg, lhs_reg, rhs_reg),
                .div, .div_trunc => {
                    // For integers, div and div_trunc are the same (integer division truncates)
                    if (is_unsigned) {
                        try self.codegen.emitUDiv(.w64, result_reg, lhs_reg, rhs_reg);
                    } else {
                        try self.codegen.emitSDiv(.w64, result_reg, lhs_reg, rhs_reg);
                    }
                },
                .mod => {
                    if (is_unsigned) {
                        try self.codegen.emitUMod(.w64, result_reg, lhs_reg, rhs_reg);
                    } else {
                        try self.codegen.emitSMod(.w64, result_reg, lhs_reg, rhs_reg);
                    }
                },
                // Comparison operations
                .eq => {
                    try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, condEqual());
                },
                .neq => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, condNotEqual()),
                .lt => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, condLess()),
                .lte => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, condLessOrEqual()),
                .gt => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, condGreater()),
                .gte => try self.codegen.emitCmp(.w64, result_reg, lhs_reg, rhs_reg, condGreaterOrEqual()),
                // Boolean operations - AND/OR two values
                // Boolean values in Roc are represented as 0 (false) or 1 (true).
                // Bitwise AND/OR work correctly for single-bit boolean values.
                .@"and" => try self.codegen.emitAnd(.w64, result_reg, lhs_reg, rhs_reg),
                .@"or" => try self.codegen.emitOr(.w64, result_reg, lhs_reg, rhs_reg),
            }

            // Free operand registers if they were temporary
            self.codegen.freeGeneral(lhs_reg);
            self.codegen.freeGeneral(rhs_reg);

            return .{ .general_reg = result_reg };
        }

        // Condition code helpers for cross-architecture support
        fn condEqual() Condition {
            return if (comptime builtin.cpu.arch == .aarch64) .eq else .equal;
        }

        fn condNotEqual() Condition {
            return if (comptime builtin.cpu.arch == .aarch64) .ne else .not_equal;
        }

        fn condLess() Condition {
            return if (comptime builtin.cpu.arch == .aarch64) .lt else .less;
        }

        fn condLessOrEqual() Condition {
            return if (comptime builtin.cpu.arch == .aarch64) .le else .less_or_equal;
        }

        fn condGreater() Condition {
            return if (comptime builtin.cpu.arch == .aarch64) .gt else .greater;
        }

        fn condGreaterOrEqual() Condition {
            return if (comptime builtin.cpu.arch == .aarch64) .ge else .greater_or_equal;
        }

        /// Generate 128-bit integer binary operation
        fn generateI128Binop(
            self: *Self,
            op: MonoExpr.BinOp,
            lhs_loc: ValueLocation,
            rhs_loc: ValueLocation,
            result_layout: layout.Idx,
        ) Error!ValueLocation {
            // For 128-bit operations, we work with the values as pairs of 64-bit words
            // Low word at offset 0, high word at offset 8

            // Get low and high parts of both operands
            const lhs_parts = try self.getI128Parts(lhs_loc);
            const rhs_parts = try self.getI128Parts(rhs_loc);

            // Allocate registers for result
            const result_low = try self.allocTempGeneral();
            const result_high = try self.allocTempGeneral();

            const is_unsigned = result_layout == .u128;

            switch (op) {
                .add => {
                    // 128-bit add: low = lhs_low + rhs_low, high = lhs_high + rhs_high + carry
                    if (comptime builtin.cpu.arch == .aarch64) {
                        // ADDS sets carry flag, ADC adds with carry
                        try self.codegen.emit.addsRegRegReg(.w64, result_low, lhs_parts.low, rhs_parts.low);
                        try self.codegen.emit.adcRegRegReg(.w64, result_high, lhs_parts.high, rhs_parts.high);
                    } else {
                        // x86_64: ADD sets carry, ADC uses it
                        try self.codegen.emit.movRegReg(.w64, result_low, lhs_parts.low);
                        try self.codegen.emit.addRegReg(.w64, result_low, rhs_parts.low);
                        try self.codegen.emit.movRegReg(.w64, result_high, lhs_parts.high);
                        try self.codegen.emit.adcRegReg(.w64, result_high, rhs_parts.high);
                    }
                },
                .sub => {
                    // 128-bit sub: low = lhs_low - rhs_low, high = lhs_high - rhs_high - borrow
                    if (comptime builtin.cpu.arch == .aarch64) {
                        // SUBS sets borrow flag, SBC subtracts with borrow
                        try self.codegen.emit.subsRegRegReg(.w64, result_low, lhs_parts.low, rhs_parts.low);
                        try self.codegen.emit.sbcRegRegReg(.w64, result_high, lhs_parts.high, rhs_parts.high);
                    } else {
                        // x86_64: SUB sets borrow, SBB uses it
                        try self.codegen.emit.movRegReg(.w64, result_low, lhs_parts.low);
                        try self.codegen.emit.subRegReg(.w64, result_low, rhs_parts.low);
                        try self.codegen.emit.movRegReg(.w64, result_high, lhs_parts.high);
                        try self.codegen.emit.sbbRegReg(.w64, result_high, rhs_parts.high);
                    }
                },
                .mul => {
                    if (result_layout == .dec) {
                        // Dec multiplication: call builtin function
                        // mulSaturatedC(RocDec, RocDec) -> RocDec
                        // RocDec is extern struct { num: i128 }
                        try self.callDecMul(lhs_parts, rhs_parts, result_low, result_high);
                    } else {
                        // 128-bit multiply: (a_lo, a_hi) * (b_lo, b_hi)
                        // result_lo = low64(a_lo * b_lo)
                        // result_hi = high64(a_lo * b_lo) + low64(a_lo * b_hi) + low64(a_hi * b_lo)

                        if (comptime builtin.cpu.arch == .aarch64) {
                            // aarch64: Use MUL for low and UMULH for high
                            // 1. a_lo * b_lo -> result_low (low), temp (high via UMULH)
                            try self.codegen.emit.mulRegRegReg(.w64, result_low, lhs_parts.low, rhs_parts.low);
                            try self.codegen.emit.umulhRegRegReg(result_high, lhs_parts.low, rhs_parts.low);

                            // 2. a_lo * b_hi -> temp1 (low part only, add to result_high)
                            const temp1 = try self.allocTempGeneral();
                            try self.codegen.emit.mulRegRegReg(.w64, temp1, lhs_parts.low, rhs_parts.high);
                            try self.codegen.emit.addRegRegReg(.w64, result_high, result_high, temp1);
                            self.codegen.freeGeneral(temp1);

                            // 3. a_hi * b_lo -> temp2 (low part only, add to result_high)
                            const temp2 = try self.allocTempGeneral();
                            try self.codegen.emit.mulRegRegReg(.w64, temp2, rhs_parts.low, lhs_parts.high);
                            try self.codegen.emit.addRegRegReg(.w64, result_high, result_high, temp2);
                            self.codegen.freeGeneral(temp2);
                        } else {
                            // x86_64: Use MUL which gives RDX:RAX = RAX * src
                            // IMPORTANT: MUL clobbers both RAX and RDX. We do 3 MUL operations.
                            // All input/output registers that are RAX or RDX will be clobbered!
                            //
                            // Usage pattern:
                            // Step 1: lhs_parts.low, rhs_parts.low -> clobbers RAX, RDX
                            // Step 2: lhs_parts.low, rhs_parts.high -> clobbers RAX, RDX
                            // Step 3: lhs_parts.high, rhs_parts.low -> clobbers RAX, RDX
                            //
                            // We must save any input in RAX/RDX before they get clobbered.

                            // Mark RAX and RDX as in-use so allocGeneralFor won't return them
                            self.codegen.markRegisterInUse(.RAX);
                            self.codegen.markRegisterInUse(.RDX);

                            // Allocate temp registers for accumulation (guaranteed not RAX/RDX)
                            const temp_low = try self.codegen.allocGeneralFor(0xFFFE);
                            const temp_high = try self.codegen.allocGeneralFor(0xFFFF);

                            // Helper to save a register if it's RAX or RDX
                            const SavedReg = struct {
                                reg: GeneralReg,
                                needs_free: bool,
                            };

                            const saveIfClobbered = struct {
                                fn f(s: *Self, reg: GeneralReg) !SavedReg {
                                    if (reg == .RAX or reg == .RDX) {
                                        const saved = try s.codegen.allocGeneralFor(0xFFFC);
                                        try s.codegen.emit.movRegReg(.w64, saved, reg);
                                        return .{ .reg = saved, .needs_free = true };
                                    }
                                    return .{ .reg = reg, .needs_free = false };
                                }
                            }.f;

                            // Save all inputs that are in RAX/RDX
                            // lhs_parts.low: used in steps 1, 2
                            const lhs_low = try saveIfClobbered(self, lhs_parts.low);
                            // lhs_parts.high: used in step 3
                            const lhs_high = try saveIfClobbered(self, lhs_parts.high);
                            // rhs_parts.low: used in steps 1, 3
                            const rhs_low = try saveIfClobbered(self, rhs_parts.low);
                            // rhs_parts.high: used in step 2
                            const rhs_high = try saveIfClobbered(self, rhs_parts.high);

                            // Restore RAX/RDX to free pool (MUL will use them)
                            self.codegen.freeGeneral(.RAX);
                            self.codegen.freeGeneral(.RDX);

                            // 1. a_lo * b_lo -> RAX (low), RDX (high)
                            try self.codegen.emit.movRegReg(.w64, .RAX, lhs_low.reg);
                            try self.codegen.emit.mulReg(.w64, rhs_low.reg);
                            try self.codegen.emit.movRegReg(.w64, temp_low, .RAX);
                            try self.codegen.emit.movRegReg(.w64, temp_high, .RDX);

                            // 2. a_lo * b_hi -> add low part to temp_high
                            try self.codegen.emit.movRegReg(.w64, .RAX, lhs_low.reg);
                            try self.codegen.emit.mulReg(.w64, rhs_high.reg);
                            try self.codegen.emit.addRegReg(.w64, temp_high, .RAX);

                            // 3. a_hi * b_lo -> add low part to temp_high
                            try self.codegen.emit.movRegReg(.w64, .RAX, lhs_high.reg);
                            try self.codegen.emit.mulReg(.w64, rhs_low.reg);
                            try self.codegen.emit.addRegReg(.w64, temp_high, .RAX);

                            // Move results to actual output registers
                            try self.codegen.emit.movRegReg(.w64, result_low, temp_low);
                            try self.codegen.emit.movRegReg(.w64, result_high, temp_high);

                            // Cleanup temp registers
                            self.codegen.freeGeneral(temp_low);
                            self.codegen.freeGeneral(temp_high);
                            if (lhs_low.needs_free) self.codegen.freeGeneral(lhs_low.reg);
                            if (lhs_high.needs_free) self.codegen.freeGeneral(lhs_high.reg);
                            if (rhs_low.needs_free) self.codegen.freeGeneral(rhs_low.reg);
                            if (rhs_high.needs_free) self.codegen.freeGeneral(rhs_high.reg);
                        }
                    }
                },
                .div => {
                    if (result_layout == .dec) {
                        // Dec division: call builtin function
                        // divC(RocDec, RocDec, *RocOps) -> i128
                        try self.callDecDiv(lhs_parts, rhs_parts, result_low, result_high);
                    } else {
                        // 128-bit integer division: call builtin function
                        try self.callI128DivRem(lhs_parts, rhs_parts, result_low, result_high, is_unsigned, false);
                    }
                },
                .div_trunc => {
                    if (result_layout == .dec) {
                        // Dec truncating division: divide and truncate to whole number
                        // divTruncC(RocDec, RocDec, *RocOps) -> i128
                        try self.callDecDivTrunc(lhs_parts, rhs_parts, result_low, result_high);
                    } else {
                        // 128-bit integer truncating division: same as regular i128 div
                        try self.callI128DivRem(lhs_parts, rhs_parts, result_low, result_high, is_unsigned, false);
                    }
                },
                .mod => {
                    // 128-bit integer remainder: call builtin function
                    try self.callI128DivRem(lhs_parts, rhs_parts, result_low, result_high, is_unsigned, true);
                },
                // Comparison operations for i128/Dec
                .eq, .neq => {
                    // Compare both low and high parts
                    const result_reg = try self.allocTempGeneral();
                    try self.generateI128Equality(lhs_parts, rhs_parts, result_reg, op == .eq);

                    // Free the extra result_high we allocated
                    self.codegen.freeGeneral(result_high);
                    self.codegen.freeGeneral(result_low);
                    self.codegen.freeGeneral(lhs_parts.low);
                    self.codegen.freeGeneral(lhs_parts.high);
                    self.codegen.freeGeneral(rhs_parts.low);
                    self.codegen.freeGeneral(rhs_parts.high);

                    return .{ .general_reg = result_reg };
                },
                .lt, .lte, .gt, .gte => {
                    // i128 comparison: compare high parts first, if equal compare low parts
                    const result_reg = try self.allocTempGeneral();
                    try self.generateI128Comparison(lhs_parts, rhs_parts, result_reg, op, is_unsigned);

                    // Free the extra result_high we allocated
                    self.codegen.freeGeneral(result_high);
                    self.codegen.freeGeneral(result_low);
                    self.codegen.freeGeneral(lhs_parts.low);
                    self.codegen.freeGeneral(lhs_parts.high);
                    self.codegen.freeGeneral(rhs_parts.low);
                    self.codegen.freeGeneral(rhs_parts.high);

                    return .{ .general_reg = result_reg };
                },
                else => {
                    // Boolean ops - use low 64 bits (booleans are 0 or 1)
                    self.codegen.freeGeneral(result_high);
                    self.codegen.freeGeneral(result_low);
                    self.codegen.freeGeneral(lhs_parts.high);
                    self.codegen.freeGeneral(rhs_parts.high);
                    return self.generateIntBinop(op, .{ .general_reg = lhs_parts.low }, .{ .general_reg = rhs_parts.low }, .i64);
                },
            }

            // Free the part registers we loaded
            self.codegen.freeGeneral(lhs_parts.low);
            self.codegen.freeGeneral(lhs_parts.high);
            self.codegen.freeGeneral(rhs_parts.low);
            self.codegen.freeGeneral(rhs_parts.high);

            // Store result to stack and return stack location
            const stack_offset = self.codegen.allocStackSlot(16);
            try self.codegen.emitStoreStack(.w64, stack_offset, result_low);
            try self.codegen.emitStoreStack(.w64, stack_offset + 8, result_high);

            self.codegen.freeGeneral(result_low);
            self.codegen.freeGeneral(result_high);

            return .{ .stack_i128 = stack_offset };
        }

        /// Call Dec multiplication builtin: mulSaturatedC(RocDec, RocDec) -> RocDec
        /// RocDec is extern struct { num: i128 }, so passed/returned as i128
        fn callDecMul(self: *Self, lhs_parts: I128Parts, rhs_parts: I128Parts, result_low: GeneralReg, result_high: GeneralReg) Error!void {
            // Get the address of the Dec multiply function
            const fn_addr = @intFromPtr(&builtins.dec.mulSaturatedC);

            if (comptime builtin.cpu.arch == .aarch64) {
                // aarch64 calling convention for i128:
                // arg1: X0 (low), X1 (high)
                // arg2: X2 (low), X3 (high)
                // return: X0 (low), X1 (high)

                // Load function address FIRST, before setting up arguments
                // This avoids allocating a register that might conflict with X0-X3
                const addr_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(addr_reg, @intCast(fn_addr));

                // Move arguments to correct registers
                try self.codegen.emit.movRegReg(.w64, .X0, lhs_parts.low);
                try self.codegen.emit.movRegReg(.w64, .X1, lhs_parts.high);
                try self.codegen.emit.movRegReg(.w64, .X2, rhs_parts.low);
                try self.codegen.emit.movRegReg(.w64, .X3, rhs_parts.high);

                // Call the function
                try self.codegen.emit.blrReg(addr_reg);
                self.codegen.freeGeneral(addr_reg);

                // Get result from X0, X1
                try self.codegen.emit.movRegReg(.w64, result_low, .X0);
                try self.codegen.emit.movRegReg(.w64, result_high, .X1);
            } else {
                // x86_64 calling convention for i128:
                // arg1: RDI (low), RSI (high)
                // arg2: RDX (low), RCX (high)
                // return: RAX (low), RDX (high)

                // Load function address into R11 first (before clobbering arg regs)
                try self.codegen.emit.movRegImm64(.R11, @intCast(fn_addr));

                const arg_regs = [_]GeneralReg{ .RDI, .RSI, .RDX, .RCX };
                const sources = [_]GeneralReg{ lhs_parts.low, lhs_parts.high, rhs_parts.low, rhs_parts.high };

                // Save sources that would be clobbered before use
                var saved: [4]?GeneralReg = .{ null, null, null, null };
                var next_temp: GeneralReg = .R9;

                for (0..4) |i| {
                    const src = sources[i];
                    for (0..i) |j| {
                        if (src == arg_regs[j]) {
                            var found: ?GeneralReg = null;
                            for (0..i) |k| {
                                if (sources[k] == src and saved[k] != null) {
                                    found = saved[k];
                                    break;
                                }
                            }
                            if (found) |s| {
                                saved[i] = s;
                            } else {
                                try self.codegen.emit.movRegReg(.w64, next_temp, src);
                                saved[i] = next_temp;
                                next_temp = if (next_temp == .R9) .R10 else .RAX;
                            }
                            break;
                        }
                    }
                }

                // Move to argument registers
                for (0..4) |i| {
                    const src = saved[i] orelse sources[i];
                    try self.codegen.emit.movRegReg(.w64, arg_regs[i], src);
                }

                // Call through R11
                try self.codegen.emit.callReg(.R11);

                // Get result from RAX, RDX (handle conflicts)
                if (result_low == .RDX) {
                    try self.codegen.emit.movRegReg(.w64, .R9, .RDX);
                    try self.codegen.emit.movRegReg(.w64, result_low, .RAX);
                    try self.codegen.emit.movRegReg(.w64, result_high, .R9);
                } else if (result_high == .RAX) {
                    try self.codegen.emit.movRegReg(.w64, .R9, .RAX);
                    try self.codegen.emit.movRegReg(.w64, result_high, .RDX);
                    try self.codegen.emit.movRegReg(.w64, result_low, .R9);
                } else {
                    try self.codegen.emit.movRegReg(.w64, result_low, .RAX);
                    try self.codegen.emit.movRegReg(.w64, result_high, .RDX);
                }
            }
        }

        /// Call Dec division builtin: divC(RocDec, RocDec, *RocOps) -> i128
        fn callDecDiv(self: *Self, lhs_parts: I128Parts, rhs_parts: I128Parts, result_low: GeneralReg, result_high: GeneralReg) Error!void {
            // Get the address of the Dec divide function
            const fn_addr = @intFromPtr(&builtins.dec.divC);

            // Get the saved RocOps register
            const roc_ops_reg = self.roc_ops_reg orelse return Error.UnsupportedExpression;

            if (comptime builtin.cpu.arch == .aarch64) {
                // aarch64 calling convention for divC(RocDec, RocDec, *RocOps) -> i128:
                // arg1 (RocDec): X0 (low), X1 (high)
                // arg2 (RocDec): X2 (low), X3 (high)
                // arg3 (*RocOps): X4
                // return: X0 (low), X1 (high)

                // Move arguments to correct registers
                try self.codegen.emit.movRegReg(.w64, .X0, lhs_parts.low);
                try self.codegen.emit.movRegReg(.w64, .X1, lhs_parts.high);
                try self.codegen.emit.movRegReg(.w64, .X2, rhs_parts.low);
                try self.codegen.emit.movRegReg(.w64, .X3, rhs_parts.high);
                try self.codegen.emit.movRegReg(.w64, .X4, roc_ops_reg);

                // Load function address and call
                const addr_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(addr_reg, @intCast(fn_addr));
                try self.codegen.emit.blrReg(addr_reg);
                self.codegen.freeGeneral(addr_reg);

                // Get result from X0, X1
                try self.codegen.emit.movRegReg(.w64, result_low, .X0);
                try self.codegen.emit.movRegReg(.w64, result_high, .X1);
            } else {
                // x86_64 calling convention for divC(RocDec, RocDec, *RocOps) -> i128:
                // arg1 (RocDec): RDI (low), RSI (high)
                // arg2 (RocDec): RDX (low), RCX (high)
                // arg3 (*RocOps): R8
                // return: RAX (low), RDX (high)

                // Load function address into R11 first (before clobbering arg regs)
                try self.codegen.emit.movRegImm64(.R11, @intCast(fn_addr));

                const arg_regs = [_]GeneralReg{ .RDI, .RSI, .RDX, .RCX, .R8 };
                const sources = [_]GeneralReg{ lhs_parts.low, lhs_parts.high, rhs_parts.low, rhs_parts.high, roc_ops_reg };

                // Save sources that would be clobbered before use
                var saved: [5]?GeneralReg = .{ null, null, null, null, null };
                var next_temp: GeneralReg = .R9;

                for (0..5) |i| {
                    const src = sources[i];
                    for (0..i) |j| {
                        if (src == arg_regs[j]) {
                            var found: ?GeneralReg = null;
                            for (0..i) |k| {
                                if (sources[k] == src and saved[k] != null) {
                                    found = saved[k];
                                    break;
                                }
                            }
                            if (found) |s| {
                                saved[i] = s;
                            } else {
                                try self.codegen.emit.movRegReg(.w64, next_temp, src);
                                saved[i] = next_temp;
                                next_temp = if (next_temp == .R9) .R10 else .RAX;
                            }
                            break;
                        }
                    }
                }

                // Move to argument registers
                for (0..5) |i| {
                    const src = saved[i] orelse sources[i];
                    try self.codegen.emit.movRegReg(.w64, arg_regs[i], src);
                }

                // Call through R11
                try self.codegen.emit.callReg(.R11);

                // Get result from RAX, RDX (handle conflicts)
                if (result_low == .RDX) {
                    try self.codegen.emit.movRegReg(.w64, .R9, .RDX);
                    try self.codegen.emit.movRegReg(.w64, result_low, .RAX);
                    try self.codegen.emit.movRegReg(.w64, result_high, .R9);
                } else if (result_high == .RAX) {
                    try self.codegen.emit.movRegReg(.w64, .R9, .RAX);
                    try self.codegen.emit.movRegReg(.w64, result_high, .RDX);
                    try self.codegen.emit.movRegReg(.w64, result_low, .R9);
                } else {
                    try self.codegen.emit.movRegReg(.w64, result_low, .RAX);
                    try self.codegen.emit.movRegReg(.w64, result_high, .RDX);
                }
            }
        }

        /// Call Dec truncating division builtin: divTruncC(RocDec, RocDec, *RocOps) -> i128
        fn callDecDivTrunc(self: *Self, lhs_parts: I128Parts, rhs_parts: I128Parts, result_low: GeneralReg, result_high: GeneralReg) Error!void {
            // Get the address of the Dec truncating divide function
            const fn_addr = @intFromPtr(&builtins.dec.divTruncC);

            // Get the saved RocOps register
            const roc_ops_reg = self.roc_ops_reg orelse return Error.UnsupportedExpression;

            if (comptime builtin.cpu.arch == .aarch64) {
                // aarch64 calling convention for divTruncC(RocDec, RocDec, *RocOps) -> i128:
                // arg1 (RocDec): X0 (low), X1 (high)
                // arg2 (RocDec): X2 (low), X3 (high)
                // arg3 (*RocOps): X4
                // return: X0 (low), X1 (high)

                // Move arguments to correct registers
                try self.codegen.emit.movRegReg(.w64, .X0, lhs_parts.low);
                try self.codegen.emit.movRegReg(.w64, .X1, lhs_parts.high);
                try self.codegen.emit.movRegReg(.w64, .X2, rhs_parts.low);
                try self.codegen.emit.movRegReg(.w64, .X3, rhs_parts.high);
                try self.codegen.emit.movRegReg(.w64, .X4, roc_ops_reg);

                // Load function address and call
                const addr_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(addr_reg, @intCast(fn_addr));
                try self.codegen.emit.blrReg(addr_reg);
                self.codegen.freeGeneral(addr_reg);

                // Get result from X0, X1
                try self.codegen.emit.movRegReg(.w64, result_low, .X0);
                try self.codegen.emit.movRegReg(.w64, result_high, .X1);
            } else {
                // x86_64 calling convention for divTruncC(RocDec, RocDec, *RocOps) -> i128:
                // arg1 (RocDec): RDI (low), RSI (high)
                // arg2 (RocDec): RDX (low), RCX (high)
                // arg3 (*RocOps): R8
                // return: RAX (low), RDX (high)

                // Load function address into R11 first (before clobbering arg regs)
                try self.codegen.emit.movRegImm64(.R11, @intCast(fn_addr));

                const arg_regs = [_]GeneralReg{ .RDI, .RSI, .RDX, .RCX, .R8 };
                const sources = [_]GeneralReg{ lhs_parts.low, lhs_parts.high, rhs_parts.low, rhs_parts.high, roc_ops_reg };

                // Save sources that would be clobbered before use
                var saved: [5]?GeneralReg = .{ null, null, null, null, null };
                var next_temp: GeneralReg = .R9;

                for (0..5) |i| {
                    const src = sources[i];
                    for (0..i) |j| {
                        if (src == arg_regs[j]) {
                            var found: ?GeneralReg = null;
                            for (0..i) |k| {
                                if (sources[k] == src and saved[k] != null) {
                                    found = saved[k];
                                    break;
                                }
                            }
                            if (found) |s| {
                                saved[i] = s;
                            } else {
                                try self.codegen.emit.movRegReg(.w64, next_temp, src);
                                saved[i] = next_temp;
                                next_temp = if (next_temp == .R9) .R10 else .RAX;
                            }
                            break;
                        }
                    }
                }

                // Move to argument registers
                for (0..5) |i| {
                    const src = saved[i] orelse sources[i];
                    try self.codegen.emit.movRegReg(.w64, arg_regs[i], src);
                }

                // Call through R11
                try self.codegen.emit.callReg(.R11);

                // Get result from RAX, RDX (handle conflicts)
                if (result_low == .RDX) {
                    try self.codegen.emit.movRegReg(.w64, .R9, .RDX);
                    try self.codegen.emit.movRegReg(.w64, result_low, .RAX);
                    try self.codegen.emit.movRegReg(.w64, result_high, .R9);
                } else if (result_high == .RAX) {
                    try self.codegen.emit.movRegReg(.w64, .R9, .RAX);
                    try self.codegen.emit.movRegReg(.w64, result_high, .RDX);
                    try self.codegen.emit.movRegReg(.w64, result_low, .R9);
                } else {
                    try self.codegen.emit.movRegReg(.w64, result_low, .RAX);
                    try self.codegen.emit.movRegReg(.w64, result_high, .RDX);
                }
            }
        }

        /// Call i128/u128 division or remainder builtin
        /// Signature: (i128/u128, i128/u128, *RocOps) -> i128/u128
        fn callI128DivRem(
            self: *Self,
            lhs_parts: I128Parts,
            rhs_parts: I128Parts,
            result_low: GeneralReg,
            result_high: GeneralReg,
            is_unsigned: bool,
            is_rem: bool,
        ) Error!void {
            // Get the address of the appropriate builtin function
            const fn_addr: usize = if (is_unsigned)
                if (is_rem) @intFromPtr(&num_remTruncU128) else @intFromPtr(&num_divTruncU128)
            else if (is_rem) @intFromPtr(&num_remTruncI128) else @intFromPtr(&num_divTruncI128);

            // Get the saved RocOps register
            const roc_ops_reg = self.roc_ops_reg orelse return Error.UnsupportedExpression;

            if (comptime builtin.cpu.arch == .aarch64) {
                // aarch64 calling convention for (i128, i128, *RocOps) -> i128:
                // arg1: X0 (low), X1 (high)
                // arg2: X2 (low), X3 (high)
                // arg3: X4
                // return: X0 (low), X1 (high)

                // Move arguments to correct registers
                try self.codegen.emit.movRegReg(.w64, .X0, lhs_parts.low);
                try self.codegen.emit.movRegReg(.w64, .X1, lhs_parts.high);
                try self.codegen.emit.movRegReg(.w64, .X2, rhs_parts.low);
                try self.codegen.emit.movRegReg(.w64, .X3, rhs_parts.high);
                try self.codegen.emit.movRegReg(.w64, .X4, roc_ops_reg);

                // Load function address and call
                const addr_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(addr_reg, @intCast(fn_addr));
                try self.codegen.emit.blrReg(addr_reg);
                self.codegen.freeGeneral(addr_reg);

                // Get result from X0, X1
                try self.codegen.emit.movRegReg(.w64, result_low, .X0);
                try self.codegen.emit.movRegReg(.w64, result_high, .X1);
            } else {
                // x86_64 calling convention for (i128, i128, *RocOps) -> i128:
                // arg1: RDI (low), RSI (high)
                // arg2: RDX (low), RCX (high)
                // arg3: R8
                // return: RAX (low), RDX (high)
                //
                // IMPORTANT: Must handle register conflicts when moving to arg registers.
                // The source registers (from allocGeneralFor) could be any caller-saved
                // register including the argument registers themselves. If a source is
                // an arg register that gets written before the source is read, we'd get
                // wrong values. We save conflicting sources to R9/R10 first.

                // Load function address into R11 (caller-saved, not an arg register)
                try self.codegen.emit.movRegImm64(.R11, @intCast(fn_addr));

                const arg_regs = [_]GeneralReg{ .RDI, .RSI, .RDX, .RCX, .R8 };
                const sources = [_]GeneralReg{ lhs_parts.low, lhs_parts.high, rhs_parts.low, rhs_parts.high, roc_ops_reg };

                // For each source, check if it would be clobbered before use.
                // Source i is clobbered if it equals any of arg_regs[0..i].
                // We save such sources to R9 or R10.
                var saved: [5]?GeneralReg = .{ null, null, null, null, null };
                var next_temp: GeneralReg = .R9;

                for (0..5) |i| {
                    const src = sources[i];
                    for (0..i) |j| {
                        if (src == arg_regs[j]) {
                            // Check if we already saved this one
                            var found_saved: ?GeneralReg = null;
                            for (0..i) |k| {
                                if (sources[k] == src and saved[k] != null) {
                                    found_saved = saved[k];
                                    break;
                                }
                            }
                            if (found_saved) |s| {
                                saved[i] = s;
                            } else {
                                // Save to temp
                                try self.codegen.emit.movRegReg(.w64, next_temp, src);
                                saved[i] = next_temp;
                                next_temp = if (next_temp == .R9) .R10 else .RAX;
                            }
                            break;
                        }
                    }
                }

                // Now move to argument registers
                for (0..5) |i| {
                    const src = saved[i] orelse sources[i];
                    try self.codegen.emit.movRegReg(.w64, arg_regs[i], src);
                }

                // Call through R11
                try self.codegen.emit.callReg(.R11);

                // Get result from RAX, RDX
                // Handle conflict: if result_low is RDX, we'd clobber return high
                if (result_low == .RDX) {
                    try self.codegen.emit.movRegReg(.w64, .R9, .RDX);
                    try self.codegen.emit.movRegReg(.w64, result_low, .RAX);
                    try self.codegen.emit.movRegReg(.w64, result_high, .R9);
                } else if (result_high == .RAX) {
                    try self.codegen.emit.movRegReg(.w64, .R9, .RAX);
                    try self.codegen.emit.movRegReg(.w64, result_high, .RDX);
                    try self.codegen.emit.movRegReg(.w64, result_low, .R9);
                } else {
                    try self.codegen.emit.movRegReg(.w64, result_low, .RAX);
                    try self.codegen.emit.movRegReg(.w64, result_high, .RDX);
                }
            }
        }

        /// Get low and high 64-bit parts of a 128-bit value
        const I128Parts = struct {
            low: GeneralReg,
            high: GeneralReg,
        };

        fn getI128Parts(self: *Self, loc: ValueLocation) Error!I128Parts {
            const low_reg = try self.allocTempGeneral();
            const high_reg = try self.allocTempGeneral();

            switch (loc) {
                .immediate_i128 => |val| {
                    const low: u64 = @truncate(@as(u128, @bitCast(val)));
                    const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);
                    try self.codegen.emitLoadImm(low_reg, @bitCast(low));
                    try self.codegen.emitLoadImm(high_reg, @bitCast(high));
                },
                .stack_i128 => |offset| {
                    try self.codegen.emitLoadStack(.w64, low_reg, offset);
                    try self.codegen.emitLoadStack(.w64, high_reg, offset + 8);
                },
                .immediate_i64 => |val| {
                    // Sign-extend to 128 bits
                    try self.codegen.emitLoadImm(low_reg, val);
                    if (val < 0) {
                        try self.codegen.emitLoadImm(high_reg, -1); // All 1s for sign extension
                    } else {
                        try self.codegen.emitLoadImm(high_reg, 0);
                    }
                },
                .general_reg => |reg| {
                    try self.emitMovRegReg(low_reg, reg);
                    try self.codegen.emitLoadImm(high_reg, 0);
                },
                .stack, .stack_str => |offset| {
                    // 8-byte stack values - sign extend to 128 bits
                    try self.codegen.emitLoadStack(.w64, low_reg, offset);
                    // Sign-extend: check if negative and set high to -1, otherwise 0
                    // For simplicity, assume unsigned and zero-extend
                    try self.codegen.emitLoadImm(high_reg, 0);
                },
                else => {
                    return Error.InvalidLocalLocation;
                },
            }

            return .{ .low = low_reg, .high = high_reg };
        }

        /// Generate i128 equality comparison (eq or neq)
        /// Compares both low and high parts - equal only if both parts match
        fn generateI128Equality(
            self: *Self,
            lhs_parts: I128Parts,
            rhs_parts: I128Parts,
            result_reg: GeneralReg,
            is_eq: bool,
        ) Error!void {
            if (comptime builtin.cpu.arch == .aarch64) {
                // Compare low parts
                try self.codegen.emit.cmpRegReg(.w64, lhs_parts.low, rhs_parts.low);
                // Use CSET to get 1 if equal, 0 if not
                try self.codegen.emit.cset(.w64, result_reg, .eq);

                // Compare high parts
                const temp = try self.allocTempGeneral();
                try self.codegen.emit.cmpRegReg(.w64, lhs_parts.high, rhs_parts.high);
                try self.codegen.emit.cset(.w64, temp, .eq);

                // AND the results: both must be equal
                try self.codegen.emit.andRegRegReg(.w64, result_reg, result_reg, temp);
                self.codegen.freeGeneral(temp);

                // For neq, invert the result
                if (!is_eq) {
                    try self.codegen.emit.eorRegRegImm(.w64, result_reg, result_reg, 1);
                }
            } else {
                // x86_64: compare both parts and combine
                // Compare low parts
                try self.codegen.emit.cmpRegReg(.w64, lhs_parts.low, rhs_parts.low);
                // Set result to 1 if equal
                try self.codegen.emitLoadImm(result_reg, 1);
                const zero = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(zero, 0);
                try self.codegen.emit.cmovcc(.not_equal, .w64, result_reg, zero);

                // Compare high parts
                try self.codegen.emit.cmpRegReg(.w64, lhs_parts.high, rhs_parts.high);
                // If high parts not equal, set to 0
                try self.codegen.emit.cmovcc(.not_equal, .w64, result_reg, zero);

                self.codegen.freeGeneral(zero);

                // For neq, invert the result
                if (!is_eq) {
                    try self.codegen.emit.xorRegImm8(.w64, result_reg, 1);
                }
            }
        }

        /// Generate i128 ordering comparison (lt, lte, gt, gte)
        /// Compares high parts first; if equal, compares low parts
        fn generateI128Comparison(
            self: *Self,
            lhs_parts: I128Parts,
            rhs_parts: I128Parts,
            result_reg: GeneralReg,
            op: MonoExpr.BinOp,
            is_unsigned: bool,
        ) Error!void {
            // Strategy: compare high parts (signed for signed, unsigned for unsigned)
            // If high parts are not equal, use that result
            // If high parts are equal, compare low parts (always unsigned since they're magnitudes)

            if (comptime builtin.cpu.arch == .aarch64) {
                // Compare high parts
                try self.codegen.emit.cmpRegReg(.w64, lhs_parts.high, rhs_parts.high);

                // Get signed/unsigned condition for high part
                // aarch64: cc = unsigned <, cs = unsigned >=, hi = unsigned >, ls = unsigned <=
                const high_cond: Condition = switch (op) {
                    .lt => if (is_unsigned) .cc else .lt,
                    .lte => if (is_unsigned) .ls else .le,
                    .gt => if (is_unsigned) .hi else .gt,
                    .gte => if (is_unsigned) .cs else .ge,
                    else => unreachable,
                };

                // Get unsigned condition for low part (low parts are always unsigned)
                const low_cond: Condition = switch (op) {
                    .lt => .cc,
                    .lte => .ls,
                    .gt => .hi,
                    .gte => .cs,
                    else => unreachable,
                };

                // Result of comparing high parts (for strict inequality case)
                try self.codegen.emit.cset(.w64, result_reg, high_cond);

                // If high parts are equal, we need to check low parts
                const temp = try self.allocTempGeneral();

                // Compare low parts
                try self.codegen.emit.cmpRegReg(.w64, lhs_parts.low, rhs_parts.low);
                try self.codegen.emit.cset(.w64, temp, low_cond);

                // Check if high parts were equal
                try self.codegen.emit.cmpRegReg(.w64, lhs_parts.high, rhs_parts.high);
                // If high parts equal, use low comparison result
                try self.codegen.emit.csel(.w64, result_reg, temp, result_reg, .eq);

                self.codegen.freeGeneral(temp);
            } else {
                // x86_64 implementation
                // Compare high parts first
                try self.codegen.emit.cmpRegReg(.w64, lhs_parts.high, rhs_parts.high);

                // Prepare result values
                const one_reg = try self.allocTempGeneral();
                const zero_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(one_reg, 1);
                try self.codegen.emitLoadImm(zero_reg, 0);

                // Get signed/unsigned condition for high part
                const high_true_cond: Condition = switch (op) {
                    .lt => if (is_unsigned) .below else .less,
                    .lte => if (is_unsigned) .below_or_equal else .less_or_equal,
                    .gt => if (is_unsigned) .above else .greater,
                    .gte => if (is_unsigned) .above_or_equal else .greater_or_equal,
                    else => unreachable,
                };

                // Start with high comparison result
                try self.codegen.emitLoadImm(result_reg, 0);
                try self.codegen.emit.cmovcc(high_true_cond, .w64, result_reg, one_reg);

                // If high parts not equal, we're done - result is set
                // If high parts are equal, need to use low comparison
                // Save high-equal status first

                const temp = try self.allocTempGeneral();

                // Compare high parts again for equality check
                try self.codegen.emit.cmpRegReg(.w64, lhs_parts.high, rhs_parts.high);
                // temp = 1 if high parts equal
                try self.codegen.emitLoadImm(temp, 0);
                try self.codegen.emit.cmovcc(.equal, .w64, temp, one_reg);

                // Now compare low parts (unsigned since they're magnitudes)
                try self.codegen.emit.cmpRegReg(.w64, lhs_parts.low, rhs_parts.low);

                const low_true_cond: Condition = switch (op) {
                    .lt => .below,
                    .lte => .below_or_equal,
                    .gt => .above,
                    .gte => .above_or_equal,
                    else => unreachable,
                };

                // Get low comparison result
                const low_result = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(low_result, 0);
                try self.codegen.emit.cmovcc(low_true_cond, .w64, low_result, one_reg);

                // If high parts were equal, use low result instead
                // test temp, temp; if temp != 0 (high parts equal), use low_result
                try self.codegen.emit.testRegReg(.w64, temp, temp);
                try self.codegen.emit.cmovcc(.not_equal, .w64, result_reg, low_result);

                self.codegen.freeGeneral(temp);
                self.codegen.freeGeneral(low_result);
                self.codegen.freeGeneral(one_reg);
                self.codegen.freeGeneral(zero_reg);
            }
        }

        /// Generate structural comparison for records/tuples
        fn generateStructuralComparison(
            self: *Self,
            lhs_loc: ValueLocation,
            rhs_loc: ValueLocation,
            lhs_expr: MonoExpr,
            op: MonoExpr.BinOp,
        ) Error!ValueLocation {
            // Get element expressions to determine sizes for nested structures
            const elem_exprs: []const MonoExprId = switch (lhs_expr) {
                .record => |r| self.store.getExprSpan(r.fields),
                .tuple => |t| self.store.getExprSpan(t.elems),
                else => unreachable,
            };

            if (elem_exprs.len == 0) {
                // Empty records/tuples are always equal
                return .{ .immediate_i64 = if (op == .eq) 1 else 0 };
            }

            const result_reg = try self.allocTempGeneral();

            // Start with equality result = 1 (true for eq, will be inverted for neq)
            try self.codegen.emitLoadImm(result_reg, 1);

            // Calculate comparison byte offsets using the layout store
            // This must match how generateTuple/generateRecord place elements
            var offsets: [32]i32 = undefined; // Max 32 comparison points
            var offset_count: usize = 0;

            const ls = self.layout_store;

            switch (lhs_expr) {
                .record => |r| {
                    if (ls) |layout_store| {
                        const record_layout = layout_store.getLayout(r.record_layout);
                        if (record_layout.tag == .record) {
                            // Use layout store offsets and sizes to match generateRecord
                            for (0..elem_exprs.len) |i| {
                                const field_offset = layout_store.getRecordFieldOffset(record_layout.data.record.idx, @intCast(i));
                                const field_size = layout_store.getRecordFieldSize(record_layout.data.record.idx, @intCast(i));
                                const field_slots: usize = @max(1, (field_size + 7) / 8);
                                for (0..field_slots) |j| {
                                    offsets[offset_count] = @as(i32, @intCast(field_offset)) + @as(i32, @intCast(j)) * 8;
                                    offset_count += 1;
                                }
                            }
                        } else {
                            // Fallback: 16-byte slots
                            for (0..elem_exprs.len) |i| {
                                offsets[offset_count] = @as(i32, @intCast(i)) * 16;
                                offset_count += 1;
                            }
                        }
                    } else {
                        // No layout store: 16-byte slots
                        for (0..elem_exprs.len) |i| {
                            offsets[offset_count] = @as(i32, @intCast(i)) * 16;
                            offset_count += 1;
                        }
                    }
                },
                .tuple => |t| {
                    if (ls) |layout_store| {
                        const tuple_layout = layout_store.getLayout(t.tuple_layout);
                        if (tuple_layout.tag == .tuple) {
                            // Use layout store offsets to match generateTuple
                            // For nested tuples, we need to compare all 8-byte slots within each element
                            for (0..elem_exprs.len) |i| {
                                const elem_offset = layout_store.getTupleElementOffset(tuple_layout.data.tuple.idx, @intCast(i));
                                const elem_size = layout_store.getTupleElementSize(tuple_layout.data.tuple.idx, @intCast(i));

                                // Use the element size to determine how many 8-byte slots to compare
                                const elem_slots: usize = @max(1, (elem_size + 7) / 8);

                                // Add comparison points for each 8-byte slot within this element
                                for (0..elem_slots) |j| {
                                    offsets[offset_count] = @as(i32, @intCast(elem_offset)) + @as(i32, @intCast(j)) * 8;
                                    offset_count += 1;
                                }
                            }
                        } else {
                            // Fallback: 8-byte slots with nested tuple flattening
                            var current_offset: i32 = 0;
                            for (elem_exprs) |elem_id| {
                                const elem_expr = self.store.getExpr(elem_id);
                                const elem_slots: usize = switch (elem_expr) {
                                    .tuple => |inner_t| self.store.getExprSpan(inner_t.elems).len,
                                    else => 1,
                                };
                                for (0..elem_slots) |_| {
                                    offsets[offset_count] = current_offset;
                                    offset_count += 1;
                                    current_offset += 8;
                                }
                            }
                        }
                    } else {
                        // No layout store: 8-byte slots with nested tuple flattening
                        var current_offset: i32 = 0;
                        for (elem_exprs) |elem_id| {
                            const elem_expr = self.store.getExpr(elem_id);
                            const elem_slots: usize = switch (elem_expr) {
                                .tuple => |inner_t| self.store.getExprSpan(inner_t.elems).len,
                                else => 1,
                            };
                            for (0..elem_slots) |_| {
                                offsets[offset_count] = current_offset;
                                offset_count += 1;
                                current_offset += 8;
                            }
                        }
                    }
                },
                else => unreachable,
            }

            const temp_lhs = try self.allocTempGeneral();
            const temp_rhs = try self.allocTempGeneral();

            // Compare all elements at their respective offsets
            for (0..offset_count) |i| {
                const offset: i32 = offsets[i];

                // Load LHS element
                switch (lhs_loc) {
                    .stack, .stack_str => |base_offset| {
                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emit.ldrRegMemSoff(.w64, temp_lhs, .FP, base_offset + offset);
                        } else {
                            try self.codegen.emit.movRegMem(.w64, temp_lhs, .RBP, base_offset + offset);
                        }
                    },
                    else => {
                        // For single-element, the value IS the element
                        if (i == 0) {
                            const reg = try self.ensureInGeneralReg(lhs_loc);
                            try self.emitMovRegReg(temp_lhs, reg);
                        }
                    },
                }

                // Load RHS element
                switch (rhs_loc) {
                    .stack, .stack_str => |base_offset| {
                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emit.ldrRegMemSoff(.w64, temp_rhs, .FP, base_offset + offset);
                        } else {
                            try self.codegen.emit.movRegMem(.w64, temp_rhs, .RBP, base_offset + offset);
                        }
                    },
                    else => {
                        // For single-element, the value IS the element
                        if (i == 0) {
                            const reg = try self.ensureInGeneralReg(rhs_loc);
                            try self.emitMovRegReg(temp_rhs, reg);
                        }
                    },
                }

                // Compare elements: if not equal, set result to 0
                if (comptime builtin.cpu.arch == .aarch64) {
                    try self.codegen.emit.cmp(.w64, temp_lhs, temp_rhs);
                    // If not equal, clear result register
                    try self.codegen.emit.csel(.w64, result_reg, result_reg, .ZRSP, .eq);
                } else {
                    try self.codegen.emit.cmpRegReg(.w64, temp_lhs, temp_rhs);
                    // Use CMOV to set result to 0 if not equal
                    const zero_reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(zero_reg, 0);
                    try self.codegen.emit.cmovcc(.not_equal, .w64, result_reg, zero_reg);
                    self.codegen.freeGeneral(zero_reg);
                }
            }

            self.codegen.freeGeneral(temp_lhs);
            self.codegen.freeGeneral(temp_rhs);

            // If neq, invert the result
            if (op == .neq) {
                if (comptime builtin.cpu.arch == .aarch64) {
                    try self.codegen.emit.eorRegRegImm(.w64, result_reg, result_reg, 1);
                } else {
                    try self.codegen.emit.xorRegImm8(.w64, result_reg, 1);
                }
            }

            return .{ .general_reg = result_reg };
        }

        /// Generate list comparison (element by element)
        fn generateListComparison(
            self: *Self,
            lhs_loc: ValueLocation,
            rhs_loc: ValueLocation,
            lhs_expr: MonoExpr,
            op: MonoExpr.BinOp,
        ) Error!ValueLocation {
            // Get list elements for element-by-element comparison
            const lhs_list = switch (lhs_expr) {
                .list => |l| l,
                else => return Error.UnsupportedExpression,
            };
            const lhs_elems = self.store.getExprSpan(lhs_list.elems);

            // Determine element size (default to 8 bytes)
            const elem_layout = switch (lhs_expr) {
                .list => |l| l.elem_layout,
                else => .i64,
            };

            // Check if elements are themselves lists by examining the actual elements
            // (elem_layout may not correctly indicate nested lists)
            const is_nested_list = blk: {
                if (lhs_elems.len > 0) {
                    const first_elem = self.store.getExpr(lhs_elems[0]);
                    break :blk (first_elem == .list or first_elem == .empty_list);
                }
                break :blk false;
            };

            // For nested lists, elements are 24-byte structs regardless of elem_layout
            const elem_size: i32 = if (is_nested_list) 24 else switch (elem_layout) {
                .i8, .u8 => 1,
                .i16, .u16 => 2,
                .i32, .u32, .f32 => 4,
                .i64, .u64, .f64, .str => 8,
                .i128, .u128, .dec => 16,
                else => @panic("TODO: list equality for non-scalar element type"),
            };

            const result_reg = try self.allocTempGeneral();

            if (lhs_elems.len == 0) {
                // Empty lists are equal
                try self.codegen.emitLoadImm(result_reg, if (op == .eq) 1 else 0);
                return .{ .general_reg = result_reg };
            }

            // Start with result = 1 (equal)
            try self.codegen.emitLoadImm(result_reg, 1);

            const temp_lhs = try self.allocTempGeneral();
            const temp_rhs = try self.allocTempGeneral();

            // The ptr in each list struct points to the element data
            // Load ptrs first, then compare elements through them

            // Load lhs ptr
            const lhs_ptr_reg = try self.allocTempGeneral();
            switch (lhs_loc) {
                .stack => |base_offset| {
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.ldrRegMemSoff(.w64, lhs_ptr_reg, .FP, base_offset);
                    } else {
                        try self.codegen.emit.movRegMem(.w64, lhs_ptr_reg, .RBP, base_offset);
                    }
                },
                .list_stack => |list_info| {
                    // Load ptr from the list struct
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.ldrRegMemSoff(.w64, lhs_ptr_reg, .FP, list_info.struct_offset);
                    } else {
                        try self.codegen.emit.movRegMem(.w64, lhs_ptr_reg, .RBP, list_info.struct_offset);
                    }
                },
                else => return Error.UnsupportedExpression,
            }

            // Load rhs ptr
            const rhs_ptr_reg = try self.allocTempGeneral();
            switch (rhs_loc) {
                .stack => |base_offset| {
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.ldrRegMemSoff(.w64, rhs_ptr_reg, .FP, base_offset);
                    } else {
                        try self.codegen.emit.movRegMem(.w64, rhs_ptr_reg, .RBP, base_offset);
                    }
                },
                .list_stack => |list_info| {
                    // Load ptr from the list struct
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.ldrRegMemSoff(.w64, rhs_ptr_reg, .FP, list_info.struct_offset);
                    } else {
                        try self.codegen.emit.movRegMem(.w64, rhs_ptr_reg, .RBP, list_info.struct_offset);
                    }
                },
                else => return Error.UnsupportedExpression,
            }

            // Compare each element through the pointers
            if (is_nested_list) {
                // For nested lists, we need to compare the inner list contents
                // Each inner list is a 24-byte struct (ptr, len, capacity)
                // We need to compare lengths first, then compare elements pointed to
                for (0..lhs_elems.len) |i| {
                    const offset: i32 = @as(i32, @intCast(i)) * elem_size;

                    // For nested lists, we need to compare the inner list contents,
                    // not just pointers. Inner lists are stored as (ptr, len) pairs.
                    // Load inner list pointers
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.ldrRegMemSoff(.w64, temp_lhs, lhs_ptr_reg, offset);
                        try self.codegen.emit.ldrRegMemSoff(.w64, temp_rhs, rhs_ptr_reg, offset);
                    } else {
                        try self.codegen.emit.movRegMem(.w64, temp_lhs, lhs_ptr_reg, offset);
                        try self.codegen.emit.movRegMem(.w64, temp_rhs, rhs_ptr_reg, offset);
                    }

                    // Load inner list lengths
                    const inner_len_lhs = try self.allocTempGeneral();
                    const inner_len_rhs = try self.allocTempGeneral();
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.ldrRegMemSoff(.w64, inner_len_lhs, lhs_ptr_reg, offset + 8);
                        try self.codegen.emit.ldrRegMemSoff(.w64, inner_len_rhs, rhs_ptr_reg, offset + 8);
                    } else {
                        try self.codegen.emit.movRegMem(.w64, inner_len_lhs, lhs_ptr_reg, offset + 8);
                        try self.codegen.emit.movRegMem(.w64, inner_len_rhs, rhs_ptr_reg, offset + 8);
                    }

                    // Compare lengths first
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.cmp(.w64, inner_len_lhs, inner_len_rhs);
                        try self.codegen.emit.csel(.w64, result_reg, result_reg, .ZRSP, .eq);
                    } else {
                        try self.codegen.emit.cmpRegReg(.w64, inner_len_lhs, inner_len_rhs);
                        const zero_reg = try self.allocTempGeneral();
                        try self.codegen.emitLoadImm(zero_reg, 0);
                        try self.codegen.emit.cmovcc(.not_equal, .w64, result_reg, zero_reg);
                        self.codegen.freeGeneral(zero_reg);
                    }

                    // Now compare inner list elements
                    // Get the inner list's element info from the expression
                    const inner_list_expr = self.store.getExpr(lhs_elems[i]);
                    const inner_elem_count: usize = switch (inner_list_expr) {
                        .list => |l| self.store.getExprSpan(l.elems).len,
                        .empty_list => 0,
                        else => 0,
                    };
                    const inner_elem_layout = switch (inner_list_expr) {
                        .list => |l| l.elem_layout,
                        else => .i64,
                    };
                    const inner_elem_size: i32 = switch (inner_elem_layout) {
                        .i8, .u8 => 1,
                        .i16, .u16 => 2,
                        .i32, .u32, .f32 => 4,
                        .i64, .u64, .f64, .str => 8,
                        .i128, .u128, .dec => 16,
                        else => 8,
                    };

                    // Compare each inner element
                    // temp_lhs = inner lhs ptr, temp_rhs = inner rhs ptr
                    const inner_temp_lhs = try self.allocTempGeneral();
                    const inner_temp_rhs = try self.allocTempGeneral();
                    for (0..inner_elem_count) |j| {
                        const inner_offset: i32 = @as(i32, @intCast(j)) * inner_elem_size;

                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emit.ldrRegMemSoff(.w64, inner_temp_lhs, temp_lhs, inner_offset);
                            try self.codegen.emit.ldrRegMemSoff(.w64, inner_temp_rhs, temp_rhs, inner_offset);
                            try self.codegen.emit.cmp(.w64, inner_temp_lhs, inner_temp_rhs);
                            try self.codegen.emit.csel(.w64, result_reg, result_reg, .ZRSP, .eq);
                        } else {
                            try self.codegen.emit.movRegMem(.w64, inner_temp_lhs, temp_lhs, inner_offset);
                            try self.codegen.emit.movRegMem(.w64, inner_temp_rhs, temp_rhs, inner_offset);
                            try self.codegen.emit.cmpRegReg(.w64, inner_temp_lhs, inner_temp_rhs);
                            const zero_reg2 = try self.allocTempGeneral();
                            try self.codegen.emitLoadImm(zero_reg2, 0);
                            try self.codegen.emit.cmovcc(.not_equal, .w64, result_reg, zero_reg2);
                            self.codegen.freeGeneral(zero_reg2);
                        }
                    }
                    self.codegen.freeGeneral(inner_temp_lhs);
                    self.codegen.freeGeneral(inner_temp_rhs);
                    self.codegen.freeGeneral(inner_len_lhs);
                    self.codegen.freeGeneral(inner_len_rhs);
                }
            } else {
                // Simple flat list comparison
                for (0..lhs_elems.len) |i| {
                    const offset: i32 = @as(i32, @intCast(i)) * elem_size;

                    // Load lhs element: [lhs_ptr + offset]
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.ldrRegMemSoff(.w64, temp_lhs, lhs_ptr_reg, offset);
                    } else {
                        try self.codegen.emit.movRegMem(.w64, temp_lhs, lhs_ptr_reg, offset);
                    }

                    // Load rhs element: [rhs_ptr + offset]
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.ldrRegMemSoff(.w64, temp_rhs, rhs_ptr_reg, offset);
                    } else {
                        try self.codegen.emit.movRegMem(.w64, temp_rhs, rhs_ptr_reg, offset);
                    }

                    // Compare elements: if not equal, set result to 0
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.cmp(.w64, temp_lhs, temp_rhs);
                        try self.codegen.emit.csel(.w64, result_reg, result_reg, .ZRSP, .eq);
                    } else {
                        try self.codegen.emit.cmpRegReg(.w64, temp_lhs, temp_rhs);
                        // Use CMOV to set result to 0 if not equal
                        const zero_reg = try self.allocTempGeneral();
                        try self.codegen.emitLoadImm(zero_reg, 0);
                        try self.codegen.emit.cmovcc(.not_equal, .w64, result_reg, zero_reg);
                        self.codegen.freeGeneral(zero_reg);
                    }
                }
            }

            self.codegen.freeGeneral(temp_lhs);
            self.codegen.freeGeneral(temp_rhs);
            self.codegen.freeGeneral(lhs_ptr_reg);
            self.codegen.freeGeneral(rhs_ptr_reg);

            // If neq, invert the result
            if (op == .neq) {
                if (comptime builtin.cpu.arch == .aarch64) {
                    try self.codegen.emit.eorRegRegImm(.w64, result_reg, result_reg, 1);
                } else {
                    try self.codegen.emit.xorRegImm8(.w64, result_reg, 1);
                }
            }

            return .{ .general_reg = result_reg };
        }

        /// Generate record comparison using layout information
        /// Used when we have a call/lookup result that returns a record
        fn generateRecordComparisonByLayout(
            self: *Self,
            lhs_loc: ValueLocation,
            rhs_loc: ValueLocation,
            record_layout_idx: layout.Idx,
            op: MonoExpr.BinOp,
        ) Error!ValueLocation {
            const ls = self.layout_store orelse return Error.UnsupportedExpression;
            const stored_layout = ls.getLayout(record_layout_idx);
            if (stored_layout.tag != .record) return Error.UnsupportedExpression;

            const record_idx = stored_layout.data.record.idx.int_idx;
            const record_data = ls.record_data.items.items[record_idx];
            const field_count = record_data.fields.count;
            if (field_count == 0) {
                // Empty records are always equal
                return .{ .immediate_i64 = if (op == .eq) 1 else 0 };
            }


            // Get the record size to ensure values are on stack
            const record_size = ls.layoutSizeAlign(stored_layout).size;

            // Ensure LHS is on stack (small records might be in registers)
            const lhs_stack = switch (lhs_loc) {
                .stack, .stack_str, .stack_i128 => |off| off,
                .general_reg => |reg| blk: {
                    const slot = self.codegen.allocStackSlot(@intCast(record_size));
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.strRegMemSoff(.w64, reg, .FP, slot);
                    } else {
                        try self.codegen.emit.movMemReg(.w64, .RBP, slot, reg);
                    }
                    break :blk slot;
                },
                .immediate_i64 => |val| blk: {
                    const slot = self.codegen.allocStackSlot(@intCast(record_size));
                    const temp = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(temp, val);
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.strRegMemSoff(.w64, temp, .FP, slot);
                    } else {
                        try self.codegen.emit.movMemReg(.w64, .RBP, slot, temp);
                    }
                    self.codegen.freeGeneral(temp);
                    break :blk slot;
                },
                else => return Error.UnsupportedExpression,
            };

            // Ensure RHS is on stack (small records might be in registers)
            const rhs_stack = switch (rhs_loc) {
                .stack, .stack_str, .stack_i128 => |off| off,
                .general_reg => |reg| blk: {
                    const slot = self.codegen.allocStackSlot(@intCast(record_size));
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.strRegMemSoff(.w64, reg, .FP, slot);
                    } else {
                        try self.codegen.emit.movMemReg(.w64, .RBP, slot, reg);
                    }
                    break :blk slot;
                },
                .immediate_i64 => |val| blk: {
                    const slot = self.codegen.allocStackSlot(@intCast(record_size));
                    const temp = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(temp, val);
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.strRegMemSoff(.w64, temp, .FP, slot);
                    } else {
                        try self.codegen.emit.movMemReg(.w64, .RBP, slot, temp);
                    }
                    self.codegen.freeGeneral(temp);
                    break :blk slot;
                },
                else => return Error.UnsupportedExpression,
            };

            const result_reg = try self.allocTempGeneral();
            const temp_lhs = try self.allocTempGeneral();
            const temp_rhs = try self.allocTempGeneral();

            // Compare records field-by-field in 8-byte chunks
            if (comptime builtin.cpu.arch == .aarch64) {
                // Initialize result to true (1 for eq, 0 for ne)
                try self.codegen.emitLoadImm(result_reg, if (op == .eq) @as(i64, 1) else @as(i64, 0));

                var offset: i32 = 0;
                while (offset < @as(i32, @intCast(record_size))) {
                    try self.codegen.emit.ldrRegMemSoff(.w64, temp_lhs, .FP, lhs_stack + offset);
                    try self.codegen.emit.ldrRegMemSoff(.w64, temp_rhs, .FP, rhs_stack + offset);
                    try self.codegen.emit.cmpRegReg(.w64, temp_lhs, temp_rhs);
                    try self.codegen.emit.cset(.w64, temp_lhs, if (op == .eq) .eq else .ne);
                    // AND results together (for eq: all chunks must match)
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.andRegRegReg(.w64, result_reg, result_reg, temp_lhs);
                    }
                    offset += 8;
                }
            } else {
                try self.codegen.emitLoadImm(result_reg, if (op == .eq) @as(i64, 1) else @as(i64, 0));
                var offset: i32 = 0;
                while (offset < @as(i32, @intCast(record_size))) {
                    try self.codegen.emit.movRegMem(.w64, temp_lhs, .RBP, lhs_stack + offset);
                    try self.codegen.emit.movRegMem(.w64, temp_rhs, .RBP, rhs_stack + offset);
                    try self.codegen.emit.cmpRegReg(.w64, temp_lhs, temp_rhs);
                    try self.codegen.emit.setcc(if (op == .eq) .equal else .not_equal, temp_lhs);
                    // AND with 0xFF to get just the byte
                    try self.codegen.emit.andRegImm32(temp_lhs, 0xFF);
                    try self.codegen.emit.andRegRegReg(.w64, result_reg, result_reg, temp_lhs);
                    offset += 8;
                }
            }
            self.codegen.freeGeneral(temp_lhs);
            self.codegen.freeGeneral(temp_rhs);
            return .{ .general_reg = result_reg };
        }

        /// Generate tuple comparison using layout information
        fn generateTupleComparisonByLayout(
            _: *Self,
            _: ValueLocation, // lhs_loc
            _: ValueLocation, // rhs_loc
            _: layout.Idx, // tuple_layout_idx
            _: MonoExpr.BinOp, // op
        ) Error!ValueLocation {
            // TODO: Implement tuple comparison by layout
            return Error.UnsupportedExpression;
        }

        /// Generate list comparison using layout information
        fn generateListComparisonByLayout(
            _: *Self,
            _: ValueLocation, // lhs_loc
            _: ValueLocation, // rhs_loc
            _: layout.Idx, // list_layout_idx
            _: MonoExpr.BinOp, // op
        ) Error!ValueLocation {
            // TODO: Implement list comparison by layout
            return Error.UnsupportedExpression;
        }

        /// Generate floating-point binary operation
        fn generateFloatBinop(
            self: *Self,
            op: MonoExpr.BinOp,
            lhs_loc: ValueLocation,
            rhs_loc: ValueLocation,
        ) Error!ValueLocation {
            // Load LHS into a float register
            const lhs_reg = try self.ensureInFloatReg(lhs_loc);

            // Load RHS into a float register
            const rhs_reg = try self.ensureInFloatReg(rhs_loc);

            // Allocate result register
            const result_reg = try self.codegen.allocFloatFor(0);

            switch (op) {
                .add => try self.codegen.emitAddF64(result_reg, lhs_reg, rhs_reg),
                .sub => try self.codegen.emitSubF64(result_reg, lhs_reg, rhs_reg),
                .mul => try self.codegen.emitMulF64(result_reg, lhs_reg, rhs_reg),
                .div => try self.codegen.emitDivF64(result_reg, lhs_reg, rhs_reg),
                else => {
                    // TODO: PLACEHOLDER - Uses ADD for float comparisons/boolean ops
                    // WHY: Float comparisons require:
                    //   - AArch64: FCMP followed by CSET to materialize condition
                    //   - x86: UCOMISD followed by SETcc to materialize condition
                    // The result should be an integer (0 or 1), not a float!
                    // IMPACT: Float comparisons return garbage (sum of operands as float)
                    // PROPER FIX:
                    //   1. Emit FCMP/UCOMISD to set condition flags
                    //   2. Use CSET (AArch64) or SETcc (x86) to get 0/1 into integer reg
                    //   3. Return that integer register, not a float register
                    try self.codegen.emitAddF64(result_reg, lhs_reg, rhs_reg);
                },
            }

            // Free operand registers
            self.codegen.freeFloat(lhs_reg);
            self.codegen.freeFloat(rhs_reg);

            return .{ .float_reg = result_reg };
        }

        /// Generate code for unary minus
        fn generateUnaryMinus(self: *Self, unary: anytype) Error!ValueLocation {
            const inner_loc = try self.generateExpr(unary.expr);

            // Check if float
            const is_float = switch (unary.result_layout) {
                .f32, .f64 => true,
                else => false,
            };

            // Check if 128-bit type
            const is_i128 = switch (unary.result_layout) {
                .i128, .u128, .dec => true,
                else => false,
            };

            if (is_float) {
                const src_reg = try self.ensureInFloatReg(inner_loc);
                const result_reg = try self.codegen.allocFloatFor(0);
                try self.codegen.emitNegF64(result_reg, src_reg);
                self.codegen.freeFloat(src_reg);
                return .{ .float_reg = result_reg };
            } else if (is_i128) {
                // 128-bit negation: result = 0 - value (using SUBS/SBC or SUB/SBB)
                const parts = try self.getI128Parts(inner_loc);

                const result_low = try self.allocTempGeneral();
                const result_high = try self.allocTempGeneral();

                if (comptime builtin.cpu.arch == .aarch64) {
                    // Negate using NEGS (NEG with flags) and NGC (negate with carry)
                    // NEGS is actually SUBS with XZR as first operand
                    try self.codegen.emit.subsRegRegReg(.w64, result_low, .ZRSP, parts.low);
                    // NGC is SBC with XZR as first operand
                    try self.codegen.emit.sbcRegRegReg(.w64, result_high, .ZRSP, parts.high);
                } else {
                    // x86_64: Load 0, then subtract
                    try self.codegen.emitLoadImm(result_low, 0);
                    try self.codegen.emit.subRegReg(.w64, result_low, parts.low);
                    try self.codegen.emitLoadImm(result_high, 0);
                    try self.codegen.emit.sbbRegReg(.w64, result_high, parts.high);
                }

                self.codegen.freeGeneral(parts.low);
                self.codegen.freeGeneral(parts.high);

                // Store result to stack
                const stack_offset = self.codegen.allocStackSlot(16);
                try self.codegen.emitStoreStack(.w64, stack_offset, result_low);
                try self.codegen.emitStoreStack(.w64, stack_offset + 8, result_high);

                self.codegen.freeGeneral(result_low);
                self.codegen.freeGeneral(result_high);

                return .{ .stack_i128 = stack_offset };
            } else {
                // For 64-bit integers, use NEG
                const reg = try self.ensureInGeneralReg(inner_loc);
                const result_reg = try self.allocTempGeneral();
                try self.codegen.emitNeg(.w64, result_reg, reg);
                self.codegen.freeGeneral(reg);
                return .{ .general_reg = result_reg };
            }
        }

        /// Generate code for unary not
        fn generateUnaryNot(self: *Self, unary: anytype) Error!ValueLocation {
            const inner_loc = try self.generateExpr(unary.expr);

            const reg = try self.ensureInGeneralReg(inner_loc);
            const result_reg = try self.allocTempGeneral();

            // Boolean NOT: XOR with 1 to flip 0↔1
            // 0 XOR 1 = 1 (False -> True)
            // 1 XOR 1 = 0 (True -> False)
            try self.codegen.emitXorImm(.w64, result_reg, reg, 1);

            self.codegen.freeGeneral(reg);
            return .{ .general_reg = result_reg };
        }

        /// Generate code for if-then-else
        fn generateIfThenElse(self: *Self, ite: anytype) Error!ValueLocation {
            const branches = self.store.getIfBranches(ite.branches);

            // Collect jump targets for patching
            var end_patches = std.ArrayList(usize).empty;
            defer end_patches.deinit(self.allocator);

            // Determine result size from layout
            var is_str_result = false;
            var is_list_result = false;
            var result_size: u32 = switch (ite.result_layout) {
                // Scalar types - size based on type
                .i8, .u8, .bool => 1,
                .i16, .u16 => 2,
                .i32, .u32, .f32 => 4,
                .i64, .u64, .f64 => 8,
                .i128, .u128, .dec => 16,
                .str => blk: {
                    is_str_result = true;
                    break :blk 24; // Strings are 24 bytes (ptr, len, capacity)
                },
                else => if (self.layout_store) |ls| blk: {
                    const result_layout = ls.getLayout(ite.result_layout);
                    break :blk switch (result_layout.tag) {
                        .list, .list_of_zst => inner: {
                            is_list_result = true;
                            break :inner 24; // Lists are 24 bytes (ptr, len, capacity)
                        },
                        .tuple => ls.getTupleData(result_layout.data.tuple.idx).size,
                        .record => ls.getRecordData(result_layout.data.record.idx).size,
                        .tag_union => ls.getTagUnionData(result_layout.data.tag_union.idx).size,
                        else => @panic("TODO: if-then-else with unsupported result layout tag"),
                    };
                } else unreachable,
            };

            // Determine storage strategy based on result size
            var result_slot: ?i32 = null;
            var result_reg: ?GeneralReg = null;

            // Generate each branch
            var first_branch = true;
            for (branches) |branch| {
                // Generate condition
                const cond_loc = try self.generateExpr(branch.cond);
                const cond_reg = try self.ensureInGeneralReg(cond_loc);

                // Compare with zero and branch if equal (condition is false)
                const else_patch = try self.emitCmpZeroAndJump(cond_reg);

                self.codegen.freeGeneral(cond_reg);

                // Generate body (true case)
                const body_loc = try self.generateExpr(branch.body);

                // On first branch, determine result storage strategy
                if (first_branch) {
                    first_branch = false;
                    // Detect list results from body_loc when layout check failed
                    // (cross-module layouts may be out of bounds)
                    if (!is_list_result and body_loc == .list_stack) {
                        is_list_result = true;
                        // Update result_size since layout check might have defaulted to 8
                        result_size = 24;
                    }
                    // Use stack for types > 8 bytes (e.g., i128, Dec) or stack-based values
                    if (result_size > 8) {
                        result_slot = self.codegen.allocStackSlot(result_size);
                    } else {
                        switch (body_loc) {
                            .stack, .stack_str, .list_stack => {
                                result_slot = self.codegen.allocStackSlot(result_size);
                            },
                            else => {
                                result_reg = try self.allocTempGeneral();
                            },
                        }
                    }
                }

                // Copy result to the appropriate location
                if (result_slot) |slot| {
                    // Copy from body_loc to result slot using the layout-determined size
                    try self.copyBytesToStackOffset(slot, body_loc, result_size);
                } else if (result_reg) |reg| {
                    const body_reg = try self.ensureInGeneralReg(body_loc);
                    try self.emitMovRegReg(reg, body_reg);
                    self.codegen.freeGeneral(body_reg);
                }

                // Jump to end (skip the else branch)
                const end_patch = try self.codegen.emitJump();
                try end_patches.append(self.allocator, end_patch);

                // Patch the else jump to here (start of else/next branch)
                const current_offset = self.codegen.currentOffset();
                self.codegen.patchJump(else_patch, current_offset);
            }

            // Generate final else
            const else_loc = try self.generateExpr(ite.final_else);

            // Handle case where all branches were composite but else is the first evaluation
            if (result_slot == null and result_reg == null) {
                // Use stack for types > 8 bytes (e.g., i128, Dec) or stack-based values
                if (result_size > 8) {
                    result_slot = self.codegen.allocStackSlot(result_size);
                } else {
                    switch (else_loc) {
                        .stack, .stack_str, .list_stack => {
                            result_slot = self.codegen.allocStackSlot(result_size);
                        },
                        else => {
                            result_reg = try self.allocTempGeneral();
                        },
                    }
                }
            }

            if (result_slot) |slot| {
                // Copy from else_loc to result slot using the layout-determined size
                try self.copyBytesToStackOffset(slot, else_loc, result_size);
            } else if (result_reg) |reg| {
                const else_reg = try self.ensureInGeneralReg(else_loc);
                try self.emitMovRegReg(reg, else_reg);
                self.codegen.freeGeneral(else_reg);
            }

            // Patch all end jumps to here
            const end_offset = self.codegen.currentOffset();
            for (end_patches.items) |patch| {
                self.codegen.patchJump(patch, end_offset);
            }

            // Return the result location - use .stack_str/.list_stack for 24-byte types so nested operations work correctly
            if (result_slot) |slot| {
                if (is_str_result) {
                    return .{ .stack_str = slot };
                }
                if (is_list_result) {
                    return .{ .list_stack = .{
                        .struct_offset = slot,
                        .data_offset = 0, // Data location is stored in the list struct itself
                        .num_elements = 0, // Unknown at compile time
                    } };
                }
                return .{ .stack = slot };
            } else if (result_reg) |reg| {
                return .{ .general_reg = reg };
            } else {
                // Edge case: no branches at all (shouldn't happen)
                return .{ .immediate_i64 = 0 };
            }
        }

        /// Compare register with zero and jump if equal (condition is false)
        /// Returns the patch location for the jump
        fn emitCmpZeroAndJump(self: *Self, reg: GeneralReg) !usize {
            if (comptime builtin.cpu.arch == .aarch64) {
                // cbz reg, 0 (branch if zero, offset will be patched)
                const patch_loc = self.codegen.currentOffset();
                try self.codegen.emit.cbz(.w64, reg, 0);
                return patch_loc;
            } else {
                // cmp reg, 0; je (will be patched)
                try self.codegen.emit.cmpRegImm32(.w64, reg, 0);
                return try self.codegen.emitCondJump(.equal);
            }
        }

        /// Move register to register (architecture-specific)
        fn emitMovRegReg(self: *Self, dst: GeneralReg, src: GeneralReg) !void {
            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.movRegReg(.w64, dst, src);
            } else {
                try self.codegen.emit.movRegReg(.w64, dst, src);
            }
        }

        /// Generate code for when/match expression
        fn generateWhen(self: *Self, when_expr: anytype) Error!ValueLocation {
            // Evaluate the scrutinee (the value being matched)
            const value_loc = try self.generateExpr(when_expr.value);

            // Get the branches
            const branches = self.store.getWhenBranches(when_expr.branches);
            if (branches.len == 0) {
                return Error.UnsupportedExpression;
            }

            // Determine result size to decide between register and stack result
            const ls = self.layout_store orelse return Error.UnsupportedExpression;
            const result_layout_val = ls.getLayout(when_expr.result_layout);
            var result_size: u32 = ls.layoutSizeAlign(result_layout_val).size;
            var use_stack_result = result_size > 8;

            // Get tag union layout info for proper discriminant/payload offsets
            const value_layout_val = ls.getLayout(when_expr.value_layout);

            const tu_disc_offset: i32 = if (value_layout_val.tag == .tag_union) blk: {
                const tu_data = ls.getTagUnionData(value_layout_val.data.tag_union.idx);
                break :blk @intCast(tu_data.discriminant_offset);
            } else 0;

            // Allocate result storage (may be upgraded dynamically below)
            var result_slot: i32 = if (use_stack_result) self.codegen.allocStackSlot(result_size) else 0;
            var result_reg: ?GeneralReg = if (!use_stack_result) try self.allocTempGeneral() else null;

            // Collect jump targets for patching to end
            var end_patches = std.ArrayList(usize).empty;
            defer end_patches.deinit(self.allocator);

            // Generate each branch
            for (branches, 0..) |branch, i| {
                const pattern = self.store.getPattern(branch.pattern);

                // Try to match the pattern
                switch (pattern) {
                    .wildcard => {
                        // Wildcard always matches - generate the body directly
                        const body_loc = try self.generateExpr(branch.body);
                        try self.storeWhenResult(body_loc, &use_stack_result, &result_slot, &result_reg, &result_size);
                        // No more branches needed after wildcard
                        break;
                    },
                    .bind => |bind| {
                        // Bind always matches - bind the value and generate body
                        const symbol_key: u48 = @bitCast(bind.symbol);
                        try self.symbol_locations.put(symbol_key, value_loc);

                        const body_loc = try self.generateExpr(branch.body);
                        try self.storeWhenResult(body_loc, &use_stack_result, &result_slot, &result_reg, &result_size);
                        // No more branches needed after unconditional bind
                        break;
                    },
                    .int_literal => |int_lit| {
                        // Compare value with literal
                        const value_reg = try self.ensureInGeneralReg(value_loc);

                        // Compare with the literal value
                        if (int_lit.value >= std.math.minInt(i32) and int_lit.value <= std.math.maxInt(i32)) {
                            try self.emitCmpImm(value_reg, @intCast(int_lit.value));
                        } else {
                            // Large literal - load to temp register and compare
                            const tmp_reg = try self.allocTempGeneral();
                            try self.loadImm64(tmp_reg, @intCast(int_lit.value));
                            try self.emitCmpRegReg(value_reg, tmp_reg);
                            self.codegen.freeGeneral(tmp_reg);
                        }

                        // Jump to next branch if not equal
                        const is_last_branch = (i == branches.len - 1);
                        var next_patch: ?usize = null;
                        if (!is_last_branch) {
                            next_patch = try self.emitJumpIfNotEqual();
                        }

                        // Pattern matched - generate body
                        const body_loc = try self.generateExpr(branch.body);
                        try self.storeWhenResult(body_loc, &use_stack_result, &result_slot, &result_reg, &result_size);

                        // Jump to end (unless this is the last branch)
                        if (!is_last_branch) {
                            const end_patch = try self.codegen.emitJump();
                            try end_patches.append(self.allocator, end_patch);

                            // Patch the next branch jump to here
                            if (next_patch) |patch| {
                                const current_offset = self.codegen.currentOffset();
                                self.codegen.patchJump(patch, current_offset);
                            }
                        }
                    },
                    .tag => |tag_pattern| {
                        // Match on tag discriminant
                        // Tag unions: payload at offset 0, discriminant at discriminant_offset

                        // Load discriminant based on value location
                        const disc_reg = try self.allocTempGeneral();
                        switch (value_loc) {
                            .stack => |base_offset| {
                                // Load discriminant from correct offset in tag union
                                if (comptime builtin.cpu.arch == .aarch64) {
                                    try self.codegen.emit.ldrRegMemSoff(.w64, disc_reg, .FP, base_offset + tu_disc_offset);
                                } else {
                                    try self.codegen.emit.movRegMem(.w64, disc_reg, .RBP, base_offset + tu_disc_offset);
                                }
                            },
                            .general_reg => |reg| {
                                // Value is directly in register (zero-arg tag case)
                                try self.emitMovRegReg(disc_reg, reg);
                            },
                            .immediate_i64 => |val| {
                                // Immediate discriminant value
                                try self.codegen.emitLoadImm(disc_reg, val);
                            },
                            else => {
                                self.codegen.freeGeneral(disc_reg);
                                return Error.UnsupportedExpression;
                            },
                        }

                        // Compare discriminant with pattern's expected value
                        try self.emitCmpImm(disc_reg, @intCast(tag_pattern.discriminant));
                        self.codegen.freeGeneral(disc_reg);

                        // Jump to next branch if not equal
                        const is_last_branch = (i == branches.len - 1);
                        var next_patch: ?usize = null;
                        if (!is_last_branch) {
                            next_patch = try self.emitJumpIfNotEqual();
                        }

                        // Pattern matched - bind any args if present
                        const args = self.store.getPatternSpan(tag_pattern.args);
                        if (args.len > 0) {
                            // Get variant payload layout for proper binding
                            const variant_payload_layout: ?layout.Idx = if (value_layout_val.tag == .tag_union) vl_blk: {
                                const tu_data = ls.getTagUnionData(value_layout_val.data.tag_union.idx);
                                const variants = ls.getTagUnionVariants(tu_data);
                                if (tag_pattern.discriminant < variants.len) {
                                    const variant = variants.get(tag_pattern.discriminant);
                                    break :vl_blk variant.payload_layout;
                                }
                                break :vl_blk null;
                            } else null;

                            for (args, 0..) |arg_pattern_id, arg_idx| {
                                const arg_pattern = self.store.getPattern(arg_pattern_id);
                                switch (arg_pattern) {
                                    .bind => |arg_bind| {
                                        const symbol_key: u48 = @bitCast(arg_bind.symbol);
                                        switch (value_loc) {
                                            .stack => |base_offset| {
                                                // Payload starts at offset 0 in tag union
                                                const payload_offset = base_offset + @as(i32, @intCast(arg_idx)) * 8;
                                                // Use appropriate ValueLocation based on payload layout
                                                const arg_loc: ValueLocation = if (variant_payload_layout) |pl| plblk: {
                                                    if (pl == .i128 or pl == .u128 or pl == .dec) {
                                                        break :plblk .{ .stack_i128 = payload_offset };
                                                    } else if (pl == .str) {
                                                        break :plblk .{ .stack_str = payload_offset };
                                                    } else {
                                                        const pl_val = ls.getLayout(pl);
                                                        if (pl_val.tag == .list or pl_val.tag == .list_of_zst) {
                                                            break :plblk .{ .list_stack = .{
                                                                .struct_offset = payload_offset,
                                                                .data_offset = 0,
                                                                .num_elements = 0,
                                                            } };
                                                        }
                                                        break :plblk .{ .stack = payload_offset };
                                                    }
                                                } else .{ .stack = payload_offset };
                                                try self.symbol_locations.put(symbol_key, arg_loc);
                                            },
                                            else => {
                                                try self.symbol_locations.put(symbol_key, value_loc);
                                            },
                                        }
                                    },
                                    .wildcard => {},
                                    else => {},
                                }
                            }
                        }

                        // Generate body
                        const body_loc = try self.generateExpr(branch.body);
                        try self.storeWhenResult(body_loc, &use_stack_result, &result_slot, &result_reg, &result_size);

                        // Jump to end (unless this is the last branch)
                        if (!is_last_branch) {
                            const end_patch = try self.codegen.emitJump();
                            try end_patches.append(self.allocator, end_patch);

                            // Patch the next branch jump to here
                            if (next_patch) |patch| {
                                const current_offset = self.codegen.currentOffset();
                                self.codegen.patchJump(patch, current_offset);
                            }
                        }
                    },
                    .list => |list_pattern| {
                        // List pattern matching: check length and bind elements
                        // List layout: ptr at offset 0, len at offset 8, capacity at offset 16
                        // Note: Even for list_stack, elements are on the heap accessed via the pointer

                        const prefix_patterns = self.store.getPatternSpan(list_pattern.prefix);
                        const is_exact_match = list_pattern.rest.isNone();

                        // Get base offset of the list struct (works for both .stack and .list_stack)
                        const base_offset: i32 = switch (value_loc) {
                            .stack => |off| off,
                            .stack_str => |off| off,
                            .list_stack => |list_info| list_info.struct_offset,
                            else => return Error.UnsupportedExpression,
                        };

                        // Load list length from stack (offset 8 from struct base)
                        const len_reg = try self.allocTempGeneral();
                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emit.ldrRegMemSoff(.w64, len_reg, .FP, base_offset + 8);
                        } else {
                            try self.codegen.emit.movRegMem(.w64, len_reg, .RBP, base_offset + 8);
                        }

                        // Compare length with expected
                        const expected_len = @as(i32, @intCast(prefix_patterns.len));
                        try self.emitCmpImm(len_reg, expected_len);
                        self.codegen.freeGeneral(len_reg);

                        // Jump to next branch if length doesn't match
                        const is_last_branch = (i == branches.len - 1);
                        var next_patch: ?usize = null;
                        if (!is_last_branch) {
                            if (is_exact_match) {
                                // Exact match: jump if len != expected
                                next_patch = try self.emitJumpIfNotEqual();
                            } else {
                                // Rest pattern: jump if len < expected (need at least prefix_len elements)
                                next_patch = try self.emitJumpIfLessThan();
                            }
                        }

                        // Length matched - bind prefix elements
                        const elem_layout = ls.getLayout(list_pattern.elem_layout);
                        const elem_size_align = ls.layoutSizeAlign(elem_layout);
                        const elem_size = elem_size_align.size;

                        // Load the data pointer from the list struct (at base_offset)
                        const list_ptr_reg = try self.allocTempGeneral();
                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emit.ldrRegMemSoff(.w64, list_ptr_reg, .FP, base_offset);
                        } else {
                            try self.codegen.emit.movRegMem(.w64, list_ptr_reg, .RBP, base_offset);
                        }

                        // Bind each prefix element by copying from heap to stack
                        for (prefix_patterns, 0..) |elem_pattern_id, elem_idx| {
                            const elem_offset_in_list = @as(i32, @intCast(elem_idx * elem_size));
                            const elem_slot = self.codegen.allocStackSlot(@intCast(elem_size));
                            const temp_reg = try self.allocTempGeneral();

                            if (elem_size <= 8) {
                                if (comptime builtin.cpu.arch == .aarch64) {
                                    try self.codegen.emit.ldrRegMemSoff(.w64, temp_reg, list_ptr_reg, elem_offset_in_list);
                                    try self.codegen.emit.strRegMemSoff(.w64, temp_reg, .FP, elem_slot);
                                } else {
                                    try self.codegen.emit.movRegMem(.w64, temp_reg, list_ptr_reg, elem_offset_in_list);
                                    try self.codegen.emit.movMemReg(.w64, .RBP, elem_slot, temp_reg);
                                }
                            } else {
                                // For larger elements, copy 8 bytes at a time
                                var copied: u32 = 0;
                                while (copied < elem_size) : (copied += 8) {
                                    const src_off = elem_offset_in_list + @as(i32, @intCast(copied));
                                    const dst_off = elem_slot + @as(i32, @intCast(copied));
                                    if (comptime builtin.cpu.arch == .aarch64) {
                                        try self.codegen.emit.ldrRegMemSoff(.w64, temp_reg, list_ptr_reg, src_off);
                                        try self.codegen.emit.strRegMemSoff(.w64, temp_reg, .FP, dst_off);
                                    } else {
                                        try self.codegen.emit.movRegMem(.w64, temp_reg, list_ptr_reg, src_off);
                                        try self.codegen.emit.movMemReg(.w64, .RBP, dst_off, temp_reg);
                                    }
                                }
                            }

                            self.codegen.freeGeneral(temp_reg);
                            try self.bindPattern(elem_pattern_id, .{ .stack = elem_slot });
                        }

                        self.codegen.freeGeneral(list_ptr_reg);

                        // Generate body
                        const body_loc = try self.generateExpr(branch.body);
                        try self.storeWhenResult(body_loc, &use_stack_result, &result_slot, &result_reg, &result_size);

                        // Jump to end (unless this is the last branch)
                        if (!is_last_branch) {
                            const end_patch = try self.codegen.emitJump();
                            try end_patches.append(self.allocator, end_patch);

                            // Patch the next branch jump to here
                            if (next_patch) |patch| {
                                const current_offset = self.codegen.currentOffset();
                                self.codegen.patchJump(patch, current_offset);
                            }
                        }
                    },
                    else => {
                        // Unsupported pattern type (record, tuple destructuring)
                        return Error.UnsupportedExpression;
                    },
                }
            }

            // Patch all end jumps to here
            const end_offset = self.codegen.currentOffset();
            for (end_patches.items) |patch| {
                self.codegen.patchJump(patch, end_offset);
            }

            if (use_stack_result) {
                // Use the declared result layout if it's known (not ZST)
                if (result_layout_val.tag != .zst) {
                    if (when_expr.result_layout == .i128 or when_expr.result_layout == .u128 or when_expr.result_layout == .dec) {
                        return .{ .stack_i128 = result_slot };
                    } else if (when_expr.result_layout == .str) {
                        return .{ .stack_str = result_slot };
                    } else if (result_layout_val.tag == .list or result_layout_val.tag == .list_of_zst) {
                        return .{ .list_stack = .{
                            .struct_offset = result_slot,
                            .data_offset = 0,
                            .num_elements = 0,
                        } };
                    }
                }
                // Fallback: use size-based heuristics (covers dynamic upgrade case)
                if (result_size >= 24) {
                    return .{ .stack_str = result_slot };
                } else if (result_size >= 16) {
                    return .{ .stack_i128 = result_slot };
                }
                return .{ .stack = result_slot };
            }
            return .{ .general_reg = result_reg.? };
        }

        /// Store a when-branch result, dynamically upgrading from register to stack
        /// mode if the body produces a multi-register value (e.g., string, i128, list)
        /// but the declared result layout was ZST/small.
        fn storeWhenResult(
            self: *Self,
            body_loc: ValueLocation,
            use_stack_result: *bool,
            result_slot: *i32,
            result_reg: *?GeneralReg,
            result_size: *u32,
        ) Error!void {
            // Check if we need to upgrade from register to stack mode
            if (!use_stack_result.*) {
                const needed_size: ?u32 = switch (body_loc) {
                    .stack_str => 24,
                    .list_stack => 24,
                    .stack_i128 => 16,
                    .immediate_i128 => 16,
                    else => null,
                };
                if (needed_size) |size| {
                    // Upgrade to stack mode
                    use_stack_result.* = true;
                    result_size.* = size;
                    result_slot.* = self.codegen.allocStackSlot(size);
                    if (result_reg.*) |reg| {
                        self.codegen.freeGeneral(reg);
                        result_reg.* = null;
                    }
                }
            }

            // Store the result
            if (use_stack_result.*) {
                try self.storeResultToSlot(result_slot.*, body_loc, result_size.*);
            } else {
                const body_reg = try self.ensureInGeneralReg(body_loc);
                try self.emitMovRegReg(result_reg.*.?, body_reg);
                self.codegen.freeGeneral(body_reg);
            }
        }

        /// Compare two registers
        fn emitCmpRegReg(self: *Self, lhs: GeneralReg, rhs: GeneralReg) !void {
            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.cmpRegReg(.w64, lhs, rhs);
            } else {
                try self.codegen.emit.cmpRegReg(.w64, lhs, rhs);
            }
        }

        /// Load 64-bit immediate into register
        fn loadImm64(self: *Self, dst: GeneralReg, value: i64) !void {
            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.movRegImm64(dst, @bitCast(value));
            } else {
                try self.codegen.emit.movRegImm64(dst, @bitCast(value));
            }
        }

        /// Generate code for an empty list
        fn generateEmptyList(self: *Self) Error!ValueLocation {
            // Empty list: ptr = null, len = 0, capacity = 0
            // Materialize as a proper 24-byte list struct on the stack so that
            // when passed as a function argument, all 3 registers are set correctly.
            const list_struct_offset: i32 = self.codegen.allocStackSlot(24);
            const zero_reg = try self.allocTempGeneral();
            try self.codegen.emitLoadImm(zero_reg, 0);

            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.strRegMemSoff(.w64, zero_reg, .FP, list_struct_offset);
                try self.codegen.emit.strRegMemSoff(.w64, zero_reg, .FP, list_struct_offset + 8);
                try self.codegen.emit.strRegMemSoff(.w64, zero_reg, .FP, list_struct_offset + 16);
            } else {
                try self.codegen.emit.movMemReg(.w64, .RBP, list_struct_offset, zero_reg);
                try self.codegen.emit.movMemReg(.w64, .RBP, list_struct_offset + 8, zero_reg);
                try self.codegen.emit.movMemReg(.w64, .RBP, list_struct_offset + 16, zero_reg);
            }
            self.codegen.freeGeneral(zero_reg);

            return .{ .list_stack = .{
                .struct_offset = list_struct_offset,
                .data_offset = 0,
                .num_elements = 0,
            } };
        }

        /// Generate code for a list with elements
        fn generateList(self: *Self, list: anytype) Error!ValueLocation {
            const elems = self.store.getExprSpan(list.elems);
            if (elems.len == 0) {
                // Empty list: ptr = null, len = 0, capacity = 0
                const list_struct_offset: i32 = self.codegen.allocStackSlot(24);
                const zero_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(zero_reg, 0);

                if (comptime builtin.cpu.arch == .aarch64) {
                    try self.codegen.emit.strRegMemSoff(.w64, zero_reg, .FP, list_struct_offset);
                    try self.codegen.emit.strRegMemSoff(.w64, zero_reg, .FP, list_struct_offset + 8);
                    try self.codegen.emit.strRegMemSoff(.w64, zero_reg, .FP, list_struct_offset + 16);
                } else {
                    try self.codegen.emit.movMemReg(.w64, .RBP, list_struct_offset, zero_reg);
                    try self.codegen.emit.movMemReg(.w64, .RBP, list_struct_offset + 8, zero_reg);
                    try self.codegen.emit.movMemReg(.w64, .RBP, list_struct_offset + 16, zero_reg);
                }
                self.codegen.freeGeneral(zero_reg);

                return .{ .list_stack = .{
                    .struct_offset = list_struct_offset,
                    .data_offset = 0,
                    .num_elements = 0,
                } };
            }

            // Get element layout from the layout store - required, no fallbacks
            const ls = self.layout_store orelse unreachable;
            const elem_layout_data = ls.getLayout(list.elem_layout);
            const elem_size_align = ls.layoutSizeAlign(elem_layout_data);
            const elem_size: u32 = elem_size_align.size;
            const elem_alignment: u32 = @intCast(elem_size_align.alignment.toByteUnits());

            const num_elems: u32 = @intCast(elems.len);
            const total_data_bytes: usize = @as(usize, elem_size) * @as(usize, num_elems);

            // Determine if elements contain refcounted data
            const elements_refcounted: bool = ls.layoutContainsRefcounted(elem_layout_data);

            // Get the saved RocOps register
            const roc_ops_reg = self.roc_ops_reg orelse return Error.UnsupportedExpression;

            // Call allocateWithRefcountC(data_bytes, element_alignment, elements_refcounted, roc_ops)
            // Returns pointer to allocated memory (refcount is already initialized to 1)
            const fn_addr: usize = @intFromPtr(&allocateWithRefcountC);

            // Allocate stack slot to save the heap pointer (will be clobbered during element generation)
            const heap_ptr_slot: i32 = self.codegen.allocStackSlot(8);

            if (comptime builtin.cpu.arch == .aarch64) {
                // aarch64 calling convention:
                // X0 = data_bytes, X1 = element_alignment, X2 = elements_refcounted, X3 = roc_ops
                // Return: X0 = heap pointer

                try self.codegen.emit.movRegImm64(.X0, @intCast(total_data_bytes));
                try self.codegen.emit.movRegImm64(.X1, @intCast(elem_alignment));
                try self.codegen.emit.movRegImm64(.X2, if (elements_refcounted) 1 else 0);
                try self.codegen.emit.movRegReg(.w64, .X3, roc_ops_reg);

                // Load function address and call
                const addr_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(addr_reg, @intCast(fn_addr));
                try self.codegen.emit.blrReg(addr_reg);
                self.codegen.freeGeneral(addr_reg);

                // Save heap pointer from X0 to stack slot
                try self.codegen.emit.strRegMemSoff(.w64, .X0, .FP, heap_ptr_slot);
            } else {
                // x86_64 calling convention:
                // RDI = data_bytes, RSI = element_alignment, RDX = elements_refcounted, RCX = roc_ops
                // Return: RAX = heap pointer

                // Load function address into R11 first (caller-saved, not an arg register)
                try self.codegen.emit.movRegImm64(.R11, @intCast(fn_addr));

                // Set up arguments
                try self.codegen.emit.movRegImm64(.RDI, @intCast(total_data_bytes));
                try self.codegen.emit.movRegImm64(.RSI, @intCast(elem_alignment));
                try self.codegen.emit.movRegImm64(.RDX, if (elements_refcounted) 1 else 0);
                try self.codegen.emit.movRegReg(.w64, .RCX, roc_ops_reg);

                // Call the function
                try self.codegen.emit.callReg(.R11);

                // Save heap pointer from RAX to stack slot
                try self.codegen.emit.movMemReg(.w64, .RBP, heap_ptr_slot, .RAX);
            }

            // Now store each element to heap memory
            for (elems, 0..) |elem_id, i| {
                const elem_loc = try self.generateExpr(elem_id);
                const elem_heap_offset: i32 = @intCast(@as(usize, i) * @as(usize, elem_size));

                // Load heap pointer from stack slot
                const heap_ptr = try self.allocTempGeneral();
                if (comptime builtin.cpu.arch == .aarch64) {
                    try self.codegen.emit.ldrRegMemSoff(.w64, heap_ptr, .FP, heap_ptr_slot);
                } else {
                    try self.codegen.emit.movRegMem(.w64, heap_ptr, .RBP, heap_ptr_slot);
                }

                // Store element to heap based on its actual location type
                // We must handle different location types differently because the actual
                // size of the value may differ from elem_size (due to type variable resolution)
                switch (elem_loc) {
                    .stack => |src_offset| {
                        // Copy elem_size bytes from stack to heap in 8-byte chunks
                        const temp_reg = try self.allocTempGeneral();
                        var copied: u32 = 0;
                        while (copied < elem_size) : (copied += 8) {
                            const chunk_src = src_offset + @as(i32, @intCast(copied));
                            const chunk_dst = elem_heap_offset + @as(i32, @intCast(copied));
                            if (comptime builtin.cpu.arch == .aarch64) {
                                try self.codegen.emit.ldrRegMemSoff(.w64, temp_reg, .FP, chunk_src);
                                try self.codegen.emit.strRegMemSoff(.w64, temp_reg, heap_ptr, chunk_dst);
                            } else {
                                try self.codegen.emit.movRegMem(.w64, temp_reg, .RBP, chunk_src);
                                try self.codegen.emit.movMemReg(.w64, heap_ptr, chunk_dst, temp_reg);
                            }
                        }
                        self.codegen.freeGeneral(temp_reg);
                    },
                    .list_stack => |list_info| {
                        // For lists, copy the full 24-byte struct
                        const temp_reg = try self.allocTempGeneral();
                        var copied: u32 = 0;
                        while (copied < 24) : (copied += 8) {
                            const chunk_src = list_info.struct_offset + @as(i32, @intCast(copied));
                            const chunk_dst = elem_heap_offset + @as(i32, @intCast(copied));
                            if (comptime builtin.cpu.arch == .aarch64) {
                                try self.codegen.emit.ldrRegMemSoff(.w64, temp_reg, .FP, chunk_src);
                                try self.codegen.emit.strRegMemSoff(.w64, temp_reg, heap_ptr, chunk_dst);
                            } else {
                                try self.codegen.emit.movRegMem(.w64, temp_reg, .RBP, chunk_src);
                                try self.codegen.emit.movMemReg(.w64, heap_ptr, chunk_dst, temp_reg);
                            }
                        }
                        self.codegen.freeGeneral(temp_reg);
                    },
                    .immediate_i128 => |val| {
                        // For i128/Dec immediates, store the full 16 bytes
                        const low: u64 = @truncate(@as(u128, @bitCast(val)));
                        const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);
                        const temp_reg = try self.allocTempGeneral();

                        // Store low 8 bytes
                        try self.codegen.emitLoadImm(temp_reg, @bitCast(low));
                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emit.strRegMemSoff(.w64, temp_reg, heap_ptr, elem_heap_offset);
                        } else {
                            try self.codegen.emit.movMemReg(.w64, heap_ptr, elem_heap_offset, temp_reg);
                        }

                        // Store high 8 bytes
                        try self.codegen.emitLoadImm(temp_reg, @bitCast(high));
                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emit.strRegMemSoff(.w64, temp_reg, heap_ptr, elem_heap_offset + 8);
                        } else {
                            try self.codegen.emit.movMemReg(.w64, heap_ptr, elem_heap_offset + 8, temp_reg);
                        }

                        self.codegen.freeGeneral(temp_reg);
                    },
                    .stack_i128 => |src_offset| {
                        // For i128/Dec stack values, copy the full 16 bytes
                        const temp_reg = try self.allocTempGeneral();

                        // Copy low 8 bytes
                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emit.ldrRegMemSoff(.w64, temp_reg, .FP, src_offset);
                            try self.codegen.emit.strRegMemSoff(.w64, temp_reg, heap_ptr, elem_heap_offset);
                        } else {
                            try self.codegen.emit.movRegMem(.w64, temp_reg, .RBP, src_offset);
                            try self.codegen.emit.movMemReg(.w64, heap_ptr, elem_heap_offset, temp_reg);
                        }

                        // Copy high 8 bytes
                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emit.ldrRegMemSoff(.w64, temp_reg, .FP, src_offset + 8);
                            try self.codegen.emit.strRegMemSoff(.w64, temp_reg, heap_ptr, elem_heap_offset + 8);
                        } else {
                            try self.codegen.emit.movRegMem(.w64, temp_reg, .RBP, src_offset + 8);
                            try self.codegen.emit.movMemReg(.w64, heap_ptr, elem_heap_offset + 8, temp_reg);
                        }

                        self.codegen.freeGeneral(temp_reg);
                    },
                    else => {
                        // For other immediates and register values:
                        // Store 8 bytes from the register, then zero-pad to elem_size if needed
                        const elem_reg = try self.ensureInGeneralReg(elem_loc);
                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emit.strRegMemSoff(.w64, elem_reg, heap_ptr, elem_heap_offset);
                        } else {
                            try self.codegen.emit.movMemReg(.w64, heap_ptr, elem_heap_offset, elem_reg);
                        }
                        self.codegen.freeGeneral(elem_reg);

                        // Zero-pad remaining bytes if elem_size > 8
                        if (elem_size > 8) {
                            const zero_reg = try self.allocTempGeneral();
                            try self.codegen.emitLoadImm(zero_reg, 0);
                            var padded: u32 = 8;
                            while (padded < elem_size) : (padded += 8) {
                                const pad_offset = elem_heap_offset + @as(i32, @intCast(padded));
                                if (comptime builtin.cpu.arch == .aarch64) {
                                    try self.codegen.emit.strRegMemSoff(.w64, zero_reg, heap_ptr, pad_offset);
                                } else {
                                    try self.codegen.emit.movMemReg(.w64, heap_ptr, pad_offset, zero_reg);
                                }
                            }
                            self.codegen.freeGeneral(zero_reg);
                        }
                    },
                }

                self.codegen.freeGeneral(heap_ptr);
            }

            // Create the list struct: (ptr, len, capacity)
            // ptr points to heap memory, len = capacity = num_elems
            const list_struct_offset: i32 = self.codegen.allocStackSlot(24);

            // Load heap pointer and length
            const ptr_reg = try self.allocTempGeneral();
            const len_reg = try self.allocTempGeneral();

            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.ldrRegMemSoff(.w64, ptr_reg, .FP, heap_ptr_slot);
            } else {
                try self.codegen.emit.movRegMem(.w64, ptr_reg, .RBP, heap_ptr_slot);
            }
            try self.codegen.emitLoadImm(len_reg, @intCast(num_elems));

            // Store list struct
            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.strRegMemSoff(.w64, ptr_reg, .FP, list_struct_offset);
                try self.codegen.emit.strRegMemSoff(.w64, len_reg, .FP, list_struct_offset + 8);
                try self.codegen.emit.strRegMemSoff(.w64, len_reg, .FP, list_struct_offset + 16);
            } else {
                try self.codegen.emit.movMemReg(.w64, .RBP, list_struct_offset, ptr_reg);
                try self.codegen.emit.movMemReg(.w64, .RBP, list_struct_offset + 8, len_reg);
                try self.codegen.emit.movMemReg(.w64, .RBP, list_struct_offset + 16, len_reg);
            }

            self.codegen.freeGeneral(ptr_reg);
            self.codegen.freeGeneral(len_reg);

            // Return the list location
            // Note: data_offset is no longer meaningful for heap-allocated lists,
            // but we keep it for compatibility with existing code
            return .{
                .list_stack = .{
                    .struct_offset = list_struct_offset,
                    .data_offset = heap_ptr_slot, // Now points to heap ptr storage on stack
                    .num_elements = num_elems,
                },
            };
        }

        /// Generate code for a record literal
        fn generateRecord(self: *Self, rec: anytype) Error!ValueLocation {
            const ls = self.layout_store orelse return Error.UnsupportedExpression;

            // Validate layout index before use
            if (@intFromEnum(rec.record_layout) >= ls.layouts.len()) {
                std.debug.print("ERROR generateRecord: record_layout={} out of bounds (len={})\n", .{ @intFromEnum(rec.record_layout), ls.layouts.len() });
                return Error.UnsupportedExpression;
            }

            // Get the record layout
            const record_layout = ls.getLayout(rec.record_layout);
            if (record_layout.tag != .record) {
                return Error.UnsupportedExpression;
            }

            const record_data = ls.getRecordData(record_layout.data.record.idx);
            const stack_size = record_data.size;

            // Zero-sized records don't need storage
            if (stack_size == 0) {
                return .{ .immediate_i64 = 0 };
            }

            // Allocate stack space for the record
            const base_offset = self.codegen.allocStackSlot(stack_size);

            // Get field expressions
            const field_exprs = self.store.getExprSpan(rec.fields);

            // Copy each field to its offset within the record
            for (field_exprs, 0..) |field_expr_id, i| {
                const field_offset = ls.getRecordFieldOffset(record_layout.data.record.idx, @intCast(i));
                const field_size = ls.getRecordFieldSize(record_layout.data.record.idx, @intCast(i));
                const field_loc = try self.generateExpr(field_expr_id);
                try self.copyBytesToStackOffset(base_offset + @as(i32, @intCast(field_offset)), field_loc, field_size);
            }

            return .{ .stack = base_offset };
        }

        /// Generate code for field access
        fn generateFieldAccess(self: *Self, access: anytype) Error!ValueLocation {
            const ls = self.layout_store orelse return Error.UnsupportedExpression;

            // Generate code for the record expression
            const record_loc = try self.generateExpr(access.record_expr);

            // Get the record layout to find field offset and size
            const record_layout = ls.getLayout(access.record_layout);
            if (record_layout.tag != .record) {
                // Cross-module layout index mismatch: the record_layout index from
                // a builtin module may map to a different layout in the current module.
                // When field_idx is 0, just return the value as-is (first field = whole value).
                if (access.field_idx == 0) {
                    return record_loc;
                }
                // Any other field access on non-record is a compiler bug
                std.debug.print("BUG: field access on non-record layout\n", .{});
                std.debug.print("  layout tag: {s}\n", .{@tagName(record_layout.tag)});
                std.debug.print("  field_idx: {}\n", .{access.field_idx});
                unreachable;
            }

            const field_offset = ls.getRecordFieldOffset(record_layout.data.record.idx, access.field_idx);
            const field_size = ls.getRecordFieldSize(record_layout.data.record.idx, access.field_idx);

            // Return location pointing to the field within the record
            return switch (record_loc) {
                .stack, .stack_str => |s| {
                    const field_base = s + @as(i32, @intCast(field_offset));
                    // Return stack_i128 for 16-byte fields (Dec/i128/u128)
                    if (field_size == 16) {
                        return .{ .stack_i128 = field_base };
                    }
                    return .{ .stack = field_base };
                },
                .stack_i128 => |s| {
                    // Record itself is i128-sized, field access within it
                    const field_base = s + @as(i32, @intCast(field_offset));
                    if (field_size == 16) {
                        return .{ .stack_i128 = field_base };
                    }
                    return .{ .stack = field_base };
                },
                .general_reg => |reg| {
                    // Record in register - only valid for small records (<=8 bytes)
                    // A record with a 16-byte field cannot fit in a register
                    if (field_size > 8) {
                        return Error.UnsupportedExpression;
                    }
                    if (field_offset == 0) {
                        return .{ .general_reg = reg };
                    } else {
                        const result_reg = try self.allocTempGeneral();
                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emit.lsrRegRegImm(.w64, result_reg, reg, @intCast(field_offset * 8));
                        } else {
                            try self.codegen.emit.movRegReg(.w64, result_reg, reg);
                            try self.codegen.emit.shrRegImm8(.w64, result_reg, @intCast(field_offset * 8));
                        }
                        self.codegen.freeGeneral(reg);
                        return .{ .general_reg = result_reg };
                    }
                },
                .immediate_i64 => |val| {
                    if (field_size > 8) {
                        return Error.UnsupportedExpression;
                    }
                    const shifted = val >> @intCast(field_offset * 8);
                    return .{ .immediate_i64 = shifted };
                },
                else => return Error.UnsupportedExpression,
            };
        }

        /// Generate code for a tuple literal
        fn generateTuple(self: *Self, tup: anytype) Error!ValueLocation {
            const ls = self.layout_store orelse return Error.UnsupportedExpression;

            // Get the tuple layout
            const tuple_layout = ls.getLayout(tup.tuple_layout);
            if (tuple_layout.tag != .tuple) {
                return Error.UnsupportedExpression;
            }

            const tuple_data = ls.getTupleData(tuple_layout.data.tuple.idx);
            const stack_size = tuple_data.size;

            // Zero-sized tuples don't need storage
            if (stack_size == 0) {
                return .{ .immediate_i64 = 0 };
            }

            // Allocate stack space for the tuple
            const base_offset = self.codegen.allocStackSlot(stack_size);

            // Get element expressions
            const elem_exprs = self.store.getExprSpan(tup.elems);

            // Copy each element to its offset within the tuple
            // Use ByOriginalIndex functions because elem_exprs is in source order,
            // but the layout store has elements sorted by alignment
            for (elem_exprs, 0..) |elem_expr_id, i| {
                const elem_offset = ls.getTupleElementOffsetByOriginalIndex(tuple_layout.data.tuple.idx, @intCast(i));
                const elem_size = ls.getTupleElementSizeByOriginalIndex(tuple_layout.data.tuple.idx, @intCast(i));
                const elem_loc = try self.generateExpr(elem_expr_id);
                try self.copyBytesToStackOffset(base_offset + @as(i32, @intCast(elem_offset)), elem_loc, elem_size);
            }

            return .{ .stack = base_offset };
        }

        /// Generate code for tuple element access
        fn generateTupleAccess(self: *Self, access: anytype) Error!ValueLocation {
            const ls = self.layout_store orelse return Error.UnsupportedExpression;

            // Generate code for the tuple expression
            const tuple_loc = try self.generateExpr(access.tuple_expr);

            // Get the tuple layout to find element offset and size
            const tuple_layout = ls.getLayout(access.tuple_layout);
            if (tuple_layout.tag != .tuple) {
                return Error.UnsupportedExpression;
            }

            const elem_offset = ls.getTupleElementOffset(tuple_layout.data.tuple.idx, access.elem_idx);
            const elem_size = ls.getTupleElementSize(tuple_layout.data.tuple.idx, access.elem_idx);

            // Return location pointing to the element within the tuple
            return switch (tuple_loc) {
                .stack, .stack_str => |s| {
                    const elem_base = s + @as(i32, @intCast(elem_offset));
                    // Return stack_i128 for 16-byte elements (Dec/i128/u128)
                    if (elem_size == 16) {
                        return .{ .stack_i128 = elem_base };
                    }
                    return .{ .stack = elem_base };
                },
                .stack_i128 => |s| {
                    // Tuple itself is i128-sized, element access within it
                    const elem_base = s + @as(i32, @intCast(elem_offset));
                    if (elem_size == 16) {
                        return .{ .stack_i128 = elem_base };
                    }
                    return .{ .stack = elem_base };
                },
                .general_reg => |reg| {
                    // Tuple in register - only valid for small tuples (<=8 bytes)
                    // A tuple with a 16-byte element cannot fit in a register
                    if (elem_size > 8) {
                        return Error.UnsupportedExpression;
                    }
                    if (elem_offset == 0) {
                        return .{ .general_reg = reg };
                    } else {
                        const result_reg = try self.allocTempGeneral();
                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emit.lsrRegRegImm(.w64, result_reg, reg, @intCast(elem_offset * 8));
                        } else {
                            try self.codegen.emit.movRegReg(.w64, result_reg, reg);
                            try self.codegen.emit.shrRegImm8(.w64, result_reg, @intCast(elem_offset * 8));
                        }
                        self.codegen.freeGeneral(reg);
                        return .{ .general_reg = result_reg };
                    }
                },
                .immediate_i64 => |val| {
                    if (elem_size > 8) {
                        return Error.UnsupportedExpression;
                    }
                    const shifted = val >> @intCast(elem_offset * 8);
                    return .{ .immediate_i64 = shifted };
                },
                else => return Error.UnsupportedExpression,
            };
        }

        /// Generate code for a zero-argument tag (just discriminant)
        fn generateZeroArgTag(self: *Self, tag: anytype) Error!ValueLocation {
            const ls = self.layout_store orelse return Error.UnsupportedExpression;

            // Get the union layout
            const union_layout = ls.getLayout(tag.union_layout);

            // For simple tags that fit in a register, just return the discriminant
            if (union_layout.tag == .scalar or union_layout.tag == .zst) {
                return .{ .immediate_i64 = tag.discriminant };
            }

            if (union_layout.tag != .tag_union) {
                // Might be a simple enum represented as a scalar
                return .{ .immediate_i64 = tag.discriminant };
            }

            const tu_data = ls.getTagUnionData(union_layout.data.tag_union.idx);
            const stack_size = tu_data.size;

            // For small unions (single discriminant byte), just return the value
            if (stack_size <= 8) {
                return .{ .immediate_i64 = tag.discriminant };
            }

            // For larger unions, allocate space and store discriminant
            const base_offset = self.codegen.allocStackSlot(stack_size);

            // Zero out the union space first
            try self.zeroStackArea(base_offset, stack_size);

            // Store discriminant at its offset
            const disc_offset = tu_data.discriminant_offset;
            const disc_size = tu_data.discriminant_size;
            try self.storeDiscriminant(base_offset + @as(i32, @intCast(disc_offset)), tag.discriminant, disc_size);

            return .{ .stack = base_offset };
        }

        /// Generate code for a tag with payload arguments
        fn generateTag(self: *Self, tag: anytype) Error!ValueLocation {
            const ls = self.layout_store orelse return Error.UnsupportedExpression;

            // Get the union layout
            const union_layout = ls.getLayout(tag.union_layout);
            if (union_layout.tag != .tag_union) {
                return Error.UnsupportedExpression;
            }

            const tu_data = ls.getTagUnionData(union_layout.data.tag_union.idx);
            const stack_size = tu_data.size;

            // Allocate stack space for the tag union
            const base_offset = self.codegen.allocStackSlot(stack_size);

            // Zero out the union space first
            try self.zeroStackArea(base_offset, stack_size);

            // Get argument expressions and store them as payload
            const arg_exprs = self.store.getExprSpan(tag.args);
            var payload_offset: u32 = 0;
            for (arg_exprs) |arg_expr_id| {
                const arg_loc = try self.generateExpr(arg_expr_id);
                try self.copyValueToStackOffset(base_offset + @as(i32, @intCast(payload_offset)), arg_loc);
                // Advance by 8 bytes per field (simplified - should use actual field sizes)
                payload_offset += 8;
            }

            // Store discriminant at its offset
            const disc_offset = tu_data.discriminant_offset;
            const disc_size = tu_data.discriminant_size;
            try self.storeDiscriminant(base_offset + @as(i32, @intCast(disc_offset)), tag.discriminant, disc_size);

            return .{ .stack = base_offset };
        }

        /// Copy a value to a stack offset
        fn copyValueToStackOffset(self: *Self, offset: i32, loc: ValueLocation) Error!void {
            switch (loc) {
                .immediate_i64 => |val| {
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(reg, val);
                    try self.codegen.emitStoreStack(.w64, offset, reg);
                    self.codegen.freeGeneral(reg);
                },
                .general_reg => |reg| {
                    try self.codegen.emitStoreStack(.w64, offset, reg);
                },
                .stack => |src_offset| {
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadStack(.w64, reg, src_offset);
                    try self.codegen.emitStoreStack(.w64, offset, reg);
                    self.codegen.freeGeneral(reg);
                },
                .stack_i128 => |src_offset| {
                    // Copy 16 bytes
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadStack(.w64, reg, src_offset);
                    try self.codegen.emitStoreStack(.w64, offset, reg);
                    try self.codegen.emitLoadStack(.w64, reg, src_offset + 8);
                    try self.codegen.emitStoreStack(.w64, offset + 8, reg);
                    self.codegen.freeGeneral(reg);
                },
                .immediate_i128 => |val| {
                    const low: u64 = @truncate(@as(u128, @bitCast(val)));
                    const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(reg, @bitCast(low));
                    try self.codegen.emitStoreStack(.w64, offset, reg);
                    try self.codegen.emitLoadImm(reg, @bitCast(high));
                    try self.codegen.emitStoreStack(.w64, offset + 8, reg);
                    self.codegen.freeGeneral(reg);
                },
                .float_reg => |reg| {
                    try self.codegen.emitStoreStackF64(offset, reg);
                },
                .immediate_f64 => |val| {
                    const bits: u64 = @bitCast(val);
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(reg, @bitCast(bits));
                    try self.codegen.emitStoreStack(.w64, offset, reg);
                    self.codegen.freeGeneral(reg);
                },
                .stack_str => |src_offset| {
                    // Copy 24-byte RocStr struct
                    const reg = try self.allocTempGeneral();
                    // Copy ptr/data (first 8 bytes)
                    try self.codegen.emitLoadStack(.w64, reg, src_offset);
                    try self.codegen.emitStoreStack(.w64, offset, reg);
                    // Copy len (second 8 bytes)
                    try self.codegen.emitLoadStack(.w64, reg, src_offset + 8);
                    try self.codegen.emitStoreStack(.w64, offset + 8, reg);
                    // Copy capacity/flags (third 8 bytes)
                    try self.codegen.emitLoadStack(.w64, reg, src_offset + 16);
                    try self.codegen.emitStoreStack(.w64, offset + 16, reg);
                    self.codegen.freeGeneral(reg);
                },
                .list_stack => |list_info| {
                    // Copy 24-byte list struct
                    const reg = try self.allocTempGeneral();
                    // Copy ptr (first 8 bytes)
                    try self.codegen.emitLoadStack(.w64, reg, list_info.struct_offset);
                    try self.codegen.emitStoreStack(.w64, offset, reg);
                    // Copy len (second 8 bytes)
                    try self.codegen.emitLoadStack(.w64, reg, list_info.struct_offset + 8);
                    try self.codegen.emitStoreStack(.w64, offset + 8, reg);
                    // Copy capacity (third 8 bytes)
                    try self.codegen.emitLoadStack(.w64, reg, list_info.struct_offset + 16);
                    try self.codegen.emitStoreStack(.w64, offset + 16, reg);
                    self.codegen.freeGeneral(reg);
                },
                .lambda_code => {
                    // lambda_code should not be copied to stack - it's only used for calling
                    unreachable;
                },
                .closure_value => |cv| {
                    // Copy the closure value from stack
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadStack(.w64, reg, cv.stack_offset);
                    try self.codegen.emitStoreStack(.w64, offset, reg);
                    self.codegen.freeGeneral(reg);
                },
            }
        }
        /// Copy a specific number of bytes from a value location to a stack offset
        /// This uses the layout-determined size rather than inferring from ValueLocation type
        fn copyBytesToStackOffset(self: *Self, dest_offset: i32, loc: ValueLocation, size: u32) Error!void {
            // Handle ZST (zero-sized types) - nothing to copy
            if (size == 0) {
                return;
            }

            switch (loc) {
                .immediate_i64 => |val| {
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(reg, val);
                    if (comptime builtin.cpu.arch == .aarch64) {
                        switch (size) {
                            1 => try self.codegen.emitStoreStackByte(dest_offset, reg),
                            2 => try self.codegen.emitStoreStackHalfword(dest_offset, reg),
                            4 => try self.codegen.emitStoreStack(.w32, dest_offset, reg),
                            8 => try self.codegen.emitStoreStack(.w64, dest_offset, reg),
                            16 => {
                                // i64 being stored as Dec (i128) - sign extend
                                try self.codegen.emitStoreStack(.w64, dest_offset, reg);
                                // Store sign extension in high part
                                const high: i64 = if (val < 0) -1 else 0;
                                try self.codegen.emitLoadImm(reg, high);
                                try self.codegen.emitStoreStack(.w64, dest_offset + 8, reg);
                            },
                            24 => {
                                // Empty list (immediate 0) being stored as a 24-byte list struct
                                // An empty list has ptr=0, len=0, capacity=0 (all zeros)
                                std.debug.assert(val == 0);
                                try self.codegen.emitStoreStack(.w64, dest_offset, reg);
                                try self.codegen.emitStoreStack(.w64, dest_offset + 8, reg);
                                try self.codegen.emitStoreStack(.w64, dest_offset + 16, reg);
                            },
                            else => unreachable,
                        }
                    } else {
                        switch (size) {
                            1 => try self.codegen.emitStoreStack(.w8, dest_offset, reg),
                            2 => try self.codegen.emitStoreStack(.w16, dest_offset, reg),
                            4 => try self.codegen.emitStoreStack(.w32, dest_offset, reg),
                            8 => try self.codegen.emitStoreStack(.w64, dest_offset, reg),
                            16 => {
                                // i64 being stored as Dec (i128) - sign extend
                                try self.codegen.emitStoreStack(.w64, dest_offset, reg);
                                // Store sign extension in high part
                                const high: i64 = if (val < 0) -1 else 0;
                                try self.codegen.emitLoadImm(reg, high);
                                try self.codegen.emitStoreStack(.w64, dest_offset + 8, reg);
                            },
                            24 => {
                                // Empty list (immediate 0) being stored as a 24-byte list struct
                                // An empty list has ptr=0, len=0, capacity=0 (all zeros)
                                std.debug.assert(val == 0);
                                try self.codegen.emitStoreStack(.w64, dest_offset, reg);
                                try self.codegen.emitStoreStack(.w64, dest_offset + 8, reg);
                                try self.codegen.emitStoreStack(.w64, dest_offset + 16, reg);
                            },
                            else => unreachable,
                        }
                    }
                    self.codegen.freeGeneral(reg);
                    return;
                },
                .immediate_i128 => |val| {
                    const low: u64 = @truncate(@as(u128, @bitCast(val)));
                    const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);
                    const reg = try self.allocTempGeneral();

                    if (size == 16) {
                        // Full i128 copy
                        try self.codegen.emitLoadImm(reg, @bitCast(low));
                        try self.codegen.emitStoreStack(.w64, dest_offset, reg);
                        try self.codegen.emitLoadImm(reg, @bitCast(high));
                        try self.codegen.emitStoreStack(.w64, dest_offset + 8, reg);
                    } else if (size == 8) {
                        // Truncate to i64 - just store the low 64 bits
                        try self.codegen.emitLoadImm(reg, @bitCast(low));
                        try self.codegen.emitStoreStack(.w64, dest_offset, reg);
                    } else if (size == 4) {
                        // Truncate to i32
                        const low32: u32 = @truncate(low);
                        try self.codegen.emitLoadImm(reg, @as(i64, @bitCast(@as(u64, low32))));
                        try self.codegen.emitStoreStack(.w32, dest_offset, reg);
                    } else {
                        unreachable; // Unsupported size for i128 truncation
                    }

                    self.codegen.freeGeneral(reg);
                    return;
                },
                .general_reg => |reg| {
                    if (size <= 8) {
                        if (comptime builtin.cpu.arch == .aarch64) {
                            switch (size) {
                                1 => try self.codegen.emitStoreStackByte(dest_offset, reg),
                                2 => try self.codegen.emitStoreStackHalfword(dest_offset, reg),
                                4 => try self.codegen.emitStoreStack(.w32, dest_offset, reg),
                                else => try self.codegen.emitStoreStack(.w64, dest_offset, reg),
                            }
                        } else {
                            switch (size) {
                                1 => try self.codegen.emitStoreStack(.w8, dest_offset, reg),
                                2 => try self.codegen.emitStoreStack(.w16, dest_offset, reg),
                                4 => try self.codegen.emitStoreStack(.w32, dest_offset, reg),
                                else => try self.codegen.emitStoreStack(.w64, dest_offset, reg),
                            }
                        }
                    } else {
                        // Large values (> 8 bytes) shouldn't be in a general_reg.
                        // This can happen during inlining when a procedure call returns
                        // a large value. Just store the first 8 bytes.
                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emitStoreStack(.w64, dest_offset, reg);
                        } else {
                            try self.codegen.emitStoreStack(.w64, dest_offset, reg);
                        }
                    }
                    return;
                },
                .stack, .stack_str, .stack_i128, .list_stack => {
                    // Handle stack locations below
                },
                .lambda_code => |lc| {
                    // Store the code offset as an 8-byte pointer value
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(reg, @bitCast(@as(i64, @intCast(lc.code_offset))));
                    try self.codegen.emitStoreStack(.w64, dest_offset, reg);
                    self.codegen.freeGeneral(reg);
                    return;
                },
                .closure_value => |cv| {
                    // Copy the closure value from its stack location
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadStack(.w64, reg, cv.stack_offset);
                    try self.codegen.emitStoreStack(.w64, dest_offset, reg);
                    self.codegen.freeGeneral(reg);
                    return;
                },
                else => {
                    // For other locations, fall through to copyValueToStackOffset
                    try self.copyValueToStackOffset(dest_offset, loc);
                    return;
                },
            }

            // Get the source offset for stack locations
            const src_offset: i32 = switch (loc) {
                .stack => |off| off,
                .stack_str => |off| off,
                .stack_i128 => |off| off,
                .list_stack => |info| info.struct_offset,
                else => unreachable,
            };

            // Copy in 8-byte chunks
            const reg = try self.allocTempGeneral();
            var copied: u32 = 0;
            while (copied < size) {
                try self.codegen.emitLoadStack(.w64, reg, src_offset + @as(i32, @intCast(copied)));
                try self.codegen.emitStoreStack(.w64, dest_offset + @as(i32, @intCast(copied)), reg);
                copied += 8;
            }
            self.codegen.freeGeneral(reg);
        }

        /// Zero out a stack area
        fn zeroStackArea(self: *Self, offset: i32, size: u32) Error!void {
            const reg = try self.allocTempGeneral();
            try self.codegen.emitLoadImm(reg, 0);

            var remaining = size;
            var current_offset = offset;
            while (remaining >= 8) {
                try self.codegen.emitStoreStack(.w64, current_offset, reg);
                current_offset += 8;
                remaining -= 8;
            }
            // Handle remaining bytes (simplified - stores full 8 bytes even for partial)
            if (remaining > 0) {
                try self.codegen.emitStoreStack(.w64, current_offset, reg);
            }

            self.codegen.freeGeneral(reg);
        }

        /// Generate code for a string literal
        fn generateStrLiteral(self: *Self, str_idx: base.StringLiteral.Idx) Error!ValueLocation {
            const str_bytes = self.store.getString(str_idx);

            // Allocate 24 bytes on stack for Roc string representation
            const base_offset = self.codegen.allocStackSlot(24);

            if (str_bytes.len < 24) {
                // Small string optimization: store inline with length in high bit of last byte
                // Format: [data..., length | 0x80] where 0x80 marks it as small string
                var bytes: [24]u8 = .{0} ** 24;
                @memcpy(bytes[0..str_bytes.len], str_bytes);
                bytes[23] = @intCast(str_bytes.len | 0x80); // Set high bit to indicate small string

                // Store as 3 x 8-byte chunks
                const reg = try self.allocTempGeneral();

                const chunk0: u64 = @bitCast(bytes[0..8].*);
                try self.codegen.emitLoadImm(reg, @bitCast(chunk0));
                try self.codegen.emitStoreStack(.w64, base_offset, reg);

                const chunk1: u64 = @bitCast(bytes[8..16].*);
                try self.codegen.emitLoadImm(reg, @bitCast(chunk1));
                try self.codegen.emitStoreStack(.w64, base_offset + 8, reg);

                const chunk2: u64 = @bitCast(bytes[16..24].*);
                try self.codegen.emitLoadImm(reg, @bitCast(chunk2));
                try self.codegen.emitStoreStack(.w64, base_offset + 16, reg);

                self.codegen.freeGeneral(reg);
            } else {
                // Large string: needs heap allocation
                const roc_ops_reg = self.roc_ops_reg orelse return Error.UnsupportedExpression;
                const fn_addr: usize = @intFromPtr(&allocateWithRefcountC);

                // Allocate stack slot to save the heap pointer
                const heap_ptr_slot: i32 = self.codegen.allocStackSlot(8);

                if (comptime builtin.cpu.arch == .aarch64) {
                    // aarch64 calling convention:
                    // X0 = data_bytes, X1 = element_alignment, X2 = elements_refcounted, X3 = roc_ops
                    // Return: X0 = heap pointer

                    try self.codegen.emit.movRegImm64(.X0, @intCast(str_bytes.len));
                    try self.codegen.emit.movRegImm64(.X1, 1); // byte alignment
                    try self.codegen.emit.movRegImm64(.X2, 0); // elements_refcounted = false
                    try self.codegen.emit.movRegReg(.w64, .X3, roc_ops_reg);

                    // Load function address and call
                    const addr_reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(addr_reg, @intCast(fn_addr));
                    try self.codegen.emit.blrReg(addr_reg);
                    self.codegen.freeGeneral(addr_reg);

                    // Save heap pointer from X0 to stack slot
                    try self.codegen.emit.strRegMemSoff(.w64, .X0, .FP, heap_ptr_slot);
                } else {
                    // x86_64 calling convention:
                    // RDI = data_bytes, RSI = element_alignment, RDX = elements_refcounted, RCX = roc_ops
                    // Return: RAX = heap pointer

                    // Load function address into R11 first (caller-saved, not an arg register)
                    try self.codegen.emit.movRegImm64(.R11, @intCast(fn_addr));

                    // Set up arguments
                    try self.codegen.emit.movRegImm64(.RDI, @intCast(str_bytes.len));
                    try self.codegen.emit.movRegImm64(.RSI, 1); // byte alignment
                    try self.codegen.emit.movRegImm64(.RDX, 0); // elements_refcounted = false
                    try self.codegen.emit.movRegReg(.w64, .RCX, roc_ops_reg);

                    // Call the function
                    try self.codegen.emit.callReg(.R11);

                    // Save heap pointer from RAX to stack slot
                    try self.codegen.emit.movMemReg(.w64, .RBP, heap_ptr_slot, .RAX);
                }

                // Copy string bytes to heap memory
                // Load heap pointer, then copy bytes
                const heap_ptr = try self.allocTempGeneral();
                if (comptime builtin.cpu.arch == .aarch64) {
                    try self.codegen.emit.ldrRegMemSoff(.w64, heap_ptr, .FP, heap_ptr_slot);
                } else {
                    try self.codegen.emit.movRegMem(.w64, heap_ptr, .RBP, heap_ptr_slot);
                }

                // Copy string data in 8-byte chunks, then remaining bytes
                var remaining: usize = str_bytes.len;
                var str_offset: usize = 0;
                const temp_reg = try self.allocTempGeneral();

                while (remaining >= 8) {
                    const chunk: u64 = @bitCast(str_bytes[str_offset..][0..8].*);
                    try self.codegen.emitLoadImm(temp_reg, @bitCast(chunk));
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.strRegMemSoff(.w64, temp_reg, heap_ptr, @intCast(str_offset));
                    } else {
                        try self.codegen.emit.movMemReg(.w64, heap_ptr, @intCast(str_offset), temp_reg);
                    }
                    str_offset += 8;
                    remaining -= 8;
                }

                // Handle remaining bytes (1-7 bytes)
                if (remaining > 0) {
                    var last_chunk: u64 = 0;
                    for (0..remaining) |j| {
                        last_chunk |= @as(u64, str_bytes[str_offset + j]) << @intCast(j * 8);
                    }
                    try self.codegen.emitLoadImm(temp_reg, @bitCast(last_chunk));
                    // Store partial - for simplicity, store as full 8 bytes (heap has space)
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.strRegMemSoff(.w64, temp_reg, heap_ptr, @intCast(str_offset));
                    } else {
                        try self.codegen.emit.movMemReg(.w64, heap_ptr, @intCast(str_offset), temp_reg);
                    }
                }

                self.codegen.freeGeneral(temp_reg);
                self.codegen.freeGeneral(heap_ptr);

                // Construct RocStr struct on stack: {pointer, length, capacity}
                // Reload heap pointer for struct construction
                const ptr_reg = try self.allocTempGeneral();
                if (comptime builtin.cpu.arch == .aarch64) {
                    try self.codegen.emit.ldrRegMemSoff(.w64, ptr_reg, .FP, heap_ptr_slot);
                } else {
                    try self.codegen.emit.movRegMem(.w64, ptr_reg, .RBP, heap_ptr_slot);
                }

                // Store pointer (first 8 bytes)
                try self.codegen.emitStoreStack(.w64, base_offset, ptr_reg);

                // Store length (second 8 bytes)
                try self.codegen.emitLoadImm(ptr_reg, @intCast(str_bytes.len));
                try self.codegen.emitStoreStack(.w64, base_offset + 8, ptr_reg);

                // Store capacity (third 8 bytes) - same as length for immutable strings
                // No need to reload, length is still in ptr_reg
                try self.codegen.emitStoreStack(.w64, base_offset + 16, ptr_reg);

                self.codegen.freeGeneral(ptr_reg);
            }

            return .{ .stack_str = base_offset };
        }

        /// Generate code for a for loop over a list
        /// Iterates over each element, binding it to the pattern and executing the body
        fn generateForLoop(self: *Self, for_loop: anytype) Error!ValueLocation {
            // Get the list location
            const list_loc = try self.generateExpr(for_loop.list_expr);

            // Handle empty list represented as immediate 0
            // Empty lists have null pointer and 0 length, so the loop body never executes
            if (list_loc == .immediate_i64 and list_loc.immediate_i64 == 0) {
                // Empty list - loop executes 0 times, just return unit
                return .{ .immediate_i64 = 0 };
            }

            // Get list pointer and length
            const list_base: i32 = switch (list_loc) {
                .stack => |off| off,
                .stack_str => |off| off,
                .list_stack => |list_info| list_info.struct_offset,
                else => return Error.UnsupportedExpression,
            };

            // Get element size from layout - layout store MUST exist at codegen time
            const ls = self.layout_store orelse unreachable;
            const elem_layout = ls.getLayout(for_loop.elem_layout);
            const elem_size: u32 = ls.layoutSizeAlign(elem_layout).size;
            // ZST elements (size 0) are valid - they have no data but we still iterate
            std.debug.assert(elem_size <= 1024 * 1024); // Sanity check: < 1MB

            const is_zst = elem_size == 0;

            // CRITICAL: Store loop state on stack, not in registers!
            // The loop body may call C functions which clobber caller-saved registers.
            // We allocate stack slots for: ptr, len, idx (and elem_slot only for non-ZST)
            const ptr_slot = self.codegen.allocStackSlot(8);
            const len_slot = self.codegen.allocStackSlot(8);
            const idx_slot = self.codegen.allocStackSlot(8);
            // Only allocate element slot for non-ZST elements
            const elem_slot: i32 = if (is_zst) 0 else self.codegen.allocStackSlot(@intCast(elem_size));

            // Initialize loop state on stack
            {
                const temp = try self.allocTempGeneral();
                // Copy ptr from list struct to ptr_slot
                if (comptime builtin.cpu.arch == .aarch64) {
                    try self.codegen.emit.ldrRegMemSoff(.w64, temp, .FP, list_base);
                    try self.codegen.emit.strRegMemSoff(.w64, temp, .FP, ptr_slot);
                    // Copy len from list struct to len_slot
                    try self.codegen.emit.ldrRegMemSoff(.w64, temp, .FP, list_base + 8);
                    try self.codegen.emit.strRegMemSoff(.w64, temp, .FP, len_slot);
                } else {
                    try self.codegen.emit.movRegMem(.w64, temp, .RBP, list_base);
                    try self.codegen.emit.movMemReg(.w64, .RBP, ptr_slot, temp);
                    try self.codegen.emit.movRegMem(.w64, temp, .RBP, list_base + 8);
                    try self.codegen.emit.movMemReg(.w64, .RBP, len_slot, temp);
                }
                // Initialize idx to 0
                try self.codegen.emitLoadImm(temp, 0);
                if (comptime builtin.cpu.arch == .aarch64) {
                    try self.codegen.emit.strRegMemSoff(.w64, temp, .FP, idx_slot);
                } else {
                    try self.codegen.emit.movMemReg(.w64, .RBP, idx_slot, temp);
                }
                self.codegen.freeGeneral(temp);
            }

            // Record loop start position for the backward jump
            const loop_start = self.codegen.currentOffset();

            // Load idx and len from stack, compare
            {
                const idx_reg = try self.allocTempGeneral();
                const len_reg = try self.allocTempGeneral();
                if (comptime builtin.cpu.arch == .aarch64) {
                    try self.codegen.emit.ldrRegMemSoff(.w64, idx_reg, .FP, idx_slot);
                    try self.codegen.emit.ldrRegMemSoff(.w64, len_reg, .FP, len_slot);
                } else {
                    try self.codegen.emit.movRegMem(.w64, idx_reg, .RBP, idx_slot);
                    try self.codegen.emit.movRegMem(.w64, len_reg, .RBP, len_slot);
                }
                try self.emitCmpReg(idx_reg, len_reg);
                self.codegen.freeGeneral(idx_reg);
                self.codegen.freeGeneral(len_reg);
            }

            // Jump to end if index >= length (we'll patch this later)
            const exit_patch = try self.emitJumpIfGreaterOrEqual();

            // Load current element from list[idx] to elem_slot (skip for ZST)
            // Calculate element address: ptr + idx * elem_size
            if (!is_zst) {
                const ptr_reg = try self.allocTempGeneral();
                const idx_reg = try self.allocTempGeneral();
                const addr_reg = try self.allocTempGeneral();

                if (comptime builtin.cpu.arch == .aarch64) {
                    try self.codegen.emit.ldrRegMemSoff(.w64, ptr_reg, .FP, ptr_slot);
                    try self.codegen.emit.ldrRegMemSoff(.w64, idx_reg, .FP, idx_slot);
                } else {
                    try self.codegen.emit.movRegMem(.w64, ptr_reg, .RBP, ptr_slot);
                    try self.codegen.emit.movRegMem(.w64, idx_reg, .RBP, idx_slot);
                }

                try self.codegen.emit.movRegReg(.w64, addr_reg, idx_reg);

                // Multiply by element size
                if (elem_size != 1) {
                    const size_reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(size_reg, elem_size);
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.mulRegRegReg(.w64, addr_reg, addr_reg, size_reg);
                    } else {
                        try self.codegen.emit.imulRegReg(.w64, addr_reg, size_reg);
                    }
                    self.codegen.freeGeneral(size_reg);
                }

                // Add base pointer
                if (comptime builtin.cpu.arch == .aarch64) {
                    try self.codegen.emit.addRegRegReg(.w64, addr_reg, addr_reg, ptr_reg);
                } else {
                    try self.codegen.emit.addRegReg(.w64, addr_reg, ptr_reg);
                }

                // Load element to stack slot
                const temp_reg = try self.allocTempGeneral();
                if (elem_size <= 8) {
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.ldrRegMemSoff(.w64, temp_reg, addr_reg, 0);
                        try self.codegen.emit.strRegMemSoff(.w64, temp_reg, .FP, elem_slot);
                    } else {
                        try self.codegen.emit.movRegMem(.w64, temp_reg, addr_reg, 0);
                        try self.codegen.emit.movMemReg(.w64, .RBP, elem_slot, temp_reg);
                    }
                } else {
                    // For larger elements, copy in 8-byte chunks
                    var copied: u32 = 0;
                    while (copied < elem_size) : (copied += 8) {
                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emit.ldrRegMemSoff(.w64, temp_reg, addr_reg, @intCast(copied));
                            try self.codegen.emit.strRegMemSoff(.w64, temp_reg, .FP, elem_slot + @as(i32, @intCast(copied)));
                        } else {
                            try self.codegen.emit.movRegMem(.w64, temp_reg, addr_reg, @intCast(copied));
                            try self.codegen.emit.movMemReg(.w64, .RBP, elem_slot + @as(i32, @intCast(copied)), temp_reg);
                        }
                    }
                }
                self.codegen.freeGeneral(temp_reg);
                self.codegen.freeGeneral(addr_reg);
                self.codegen.freeGeneral(idx_reg);
                self.codegen.freeGeneral(ptr_reg);
            }

            // Bind the element to the pattern, passing the element layout so list patterns
            // can use the correct inner element size (the pattern's stored elem_layout may be wrong)
            // For ZST elements, bind to immediate 0 (no actual data)
            const elem_loc: ValueLocation = if (is_zst) .{ .immediate_i64 = 0 } else .{ .stack = elem_slot };
            try self.bindPatternWithLayout(for_loop.elem_pattern, elem_loc, for_loop.elem_layout);

            // Execute the body (result is discarded)
            // NOTE: This may call C functions which clobber all caller-saved registers
            {
                _ = try self.generateExpr(for_loop.body);
            }

            // Increment index (load from stack, increment, store back)
            {
                const idx_reg = try self.allocTempGeneral();
                if (comptime builtin.cpu.arch == .aarch64) {
                    try self.codegen.emit.ldrRegMemSoff(.w64, idx_reg, .FP, idx_slot);
                    const one_reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(one_reg, 1);
                    try self.codegen.emit.addRegRegReg(.w64, idx_reg, idx_reg, one_reg);
                    self.codegen.freeGeneral(one_reg);
                    try self.codegen.emit.strRegMemSoff(.w64, idx_reg, .FP, idx_slot);
                } else {
                    try self.codegen.emit.movRegMem(.w64, idx_reg, .RBP, idx_slot);
                    try self.codegen.emit.addRegImm(.w64, idx_reg, 1);
                    try self.codegen.emit.movMemReg(.w64, .RBP, idx_slot, idx_reg);
                }
                self.codegen.freeGeneral(idx_reg);
            }

            // Jump back to loop start
            try self.emitJumpBackward(loop_start);

            // Patch the exit jump to point here
            const loop_exit_offset = self.codegen.currentOffset();
            self.codegen.patchJump(exit_patch, loop_exit_offset);

            // For loops return unit (empty record)
            return .{ .immediate_i64 = 0 };
        }

        /// Generate code for a while loop
        /// Executes body while condition is true
        fn generateWhileLoop(self: *Self, while_loop: anytype) Error!ValueLocation {
            // Record loop start position for the backward jump
            const loop_start = self.codegen.currentOffset();

            // Evaluate condition
            const cond_loc = try self.generateExpr(while_loop.cond);

            // Get condition value into a register for comparison
            const cond_reg = switch (cond_loc) {
                .immediate_i64 => |val| blk: {
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(reg, @intCast(val));
                    break :blk reg;
                },
                .stack => |off| blk: {
                    const reg = try self.allocTempGeneral();
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.ldrRegMemSoff(.w64, reg, .FP, off);
                    } else {
                        try self.codegen.emit.movRegMem(.w64, reg, .RBP, off);
                    }
                    break :blk reg;
                },
                .general_reg => |r| blk: {
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emit.movRegReg(.w64, reg, r);
                    break :blk reg;
                },
                else => return Error.UnsupportedExpression,
            };

            // Compare condition with 0 (false)
            const zero_reg = try self.allocTempGeneral();
            try self.codegen.emitLoadImm(zero_reg, 0);
            try self.emitCmpReg(cond_reg, zero_reg);
            self.codegen.freeGeneral(zero_reg);
            self.codegen.freeGeneral(cond_reg);

            // Jump to end if condition is false (equal to 0)
            const exit_patch = try self.emitJumpIfEqual();

            // Execute the body (result is discarded)
            // NOTE: This may call C functions which clobber all caller-saved registers
            // The body may contain reassignments that update mutable variables
            _ = try self.generateExpr(while_loop.body);

            // Jump back to loop start (to re-evaluate condition)
            try self.emitJumpBackward(loop_start);

            // Patch the exit jump to point here
            const loop_exit_offset = self.codegen.currentOffset();
            self.codegen.patchJump(exit_patch, loop_exit_offset);

            // While loops return unit (empty record)
            return .{ .immediate_i64 = 0 };
        }

        /// Generate code for early return
        fn generateEarlyReturn(self: *Self, er: anytype) Error!ValueLocation {
            // Generate the return value
            const value_loc = try self.generateExpr(er.expr);

            // If we're inside a compileLambdaAsProc, emit a jump to the epilogue
            if (self.early_return_ret_layout) |ret_layout| {
                // Move the value to the return register
                try self.moveToReturnRegisterWithLayout(value_loc, ret_layout);
                // Emit a jump (will be patched to the epilogue location)
                const patch = try self.codegen.emitJump();
                try self.early_return_patches.append(self.allocator, patch);
                // Return a dummy value — this code is unreachable at runtime
                return .{ .immediate_i64 = 0 };
            }

            // Inline lambda case: just return the value (best-effort fallback)
            return value_loc;
        }

        /// Generate code for dbg expression (prints and returns value)
        fn generateDbg(self: *Self, dbg_expr: anytype) Error!ValueLocation {
            _ = try self.generateExpr(dbg_expr.expr);
            @panic("TODO: implement dbg printing");
        }

        /// Generate code for expect expression (assertion)
        fn generateExpect(self: *Self, expect_expr: anytype) Error!ValueLocation {
            _ = try self.generateExpr(expect_expr.cond);
            @panic("TODO: implement expect assertion checking");
        }

        /// Generate code for string concatenation
        fn generateStrConcat(self: *Self, exprs: anytype) Error!ValueLocation {
            const expr_ids = self.store.getExprSpan(exprs);
            if (expr_ids.len == 0) {
                // Empty concat returns empty string
                return try self.generateEmptyString();
            }
            if (expr_ids.len == 1) {
                // Single element, just return it
                return try self.generateExpr(expr_ids[0]);
            }
            @panic("TODO: implement multi-element string concatenation");
        }

        /// Generate an empty string
        fn generateEmptyString(self: *Self) Error!ValueLocation {
            // Empty small string in Roc format: all zeros except byte 23 = 0x80
            // (small string flag set, length 0)
            const str_slot = self.codegen.allocStackSlot(24);
            const zero_reg = try self.allocTempGeneral();
            try self.codegen.emitLoadImm(zero_reg, 0);

            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.strRegMemSoff(.w64, zero_reg, .FP, str_slot);
                try self.codegen.emit.strRegMemSoff(.w64, zero_reg, .FP, str_slot + 8);
            } else {
                try self.codegen.emit.movMemReg(.w64, .RBP, str_slot, zero_reg);
                try self.codegen.emit.movMemReg(.w64, .RBP, str_slot + 8, zero_reg);
            }

            // Byte 23 = 0x80 (small string flag, length 0)
            // In little-endian, bytes 16-23 as u64: 0x80 << 56 = 0x8000000000000000
            const small_str_flag: i64 = @bitCast(@as(u64, 0x80) << 56);
            try self.codegen.emitLoadImm(zero_reg, small_str_flag);
            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.strRegMemSoff(.w64, zero_reg, .FP, str_slot + 16);
            } else {
                try self.codegen.emit.movMemReg(.w64, .RBP, str_slot + 16, zero_reg);
            }

            self.codegen.freeGeneral(zero_reg);
            return .{ .stack_str = str_slot };
        }

        /// Generate code for int_to_str by calling the appropriate C wrapper
        fn generateIntToStr(self: *Self, its: anytype) Error!ValueLocation {
            const val_loc = try self.generateExpr(its.value);
            const fn_addr: usize = switch (its.int_precision) {
                .u8 => @intFromPtr(intToStrC(u8)),
                .i8 => @intFromPtr(intToStrC(i8)),
                .u16 => @intFromPtr(intToStrC(u16)),
                .i16 => @intFromPtr(intToStrC(i16)),
                .u32 => @intFromPtr(intToStrC(u32)),
                .i32 => @intFromPtr(intToStrC(i32)),
                .u64 => @intFromPtr(intToStrC(u64)),
                .i64 => @intFromPtr(intToStrC(i64)),
                .u128 => @intFromPtr(intToStrC(u128)),
                .i128 => @intFromPtr(intToStrC(i128)),
            };
            return try self.callToStrC(fn_addr, val_loc, its.int_precision.size() <= 8);
        }

        /// Generate code for float_to_str by calling the appropriate C wrapper
        fn generateFloatToStr(self: *Self, fts: anytype) Error!ValueLocation {
            const val_loc = try self.generateExpr(fts.value);
            const fn_addr: usize = switch (fts.float_precision) {
                .f32 => @intFromPtr(floatToStrC(f32)),
                .f64 => @intFromPtr(floatToStrC(f64)),
                .dec => @intFromPtr(&decToStrC),
            };
            // Dec is 16 bytes (i128), f32/f64 are 8 bytes or less
            const is_small = fts.float_precision != .dec;
            return try self.callToStrC(fn_addr, val_loc, is_small);
        }

        /// Generate code for dec_to_str by calling the C wrapper
        fn generateDecToStr(self: *Self, expr_id: anytype) Error!ValueLocation {
            const val_loc = try self.generateExpr(expr_id);
            return try self.callToStrC(@intFromPtr(&decToStrC), val_loc, false);
        }

        /// Common helper: call a C wrapper fn(out: *RocStr, value: T, roc_ops: *RocOps)
        /// is_small_value: true if value fits in one register (≤8 bytes), false for 16-byte values
        fn callToStrC(self: *Self, fn_addr: usize, val_loc: ValueLocation, is_small_value: bool) Error!ValueLocation {
            const roc_ops_reg = self.roc_ops_reg orelse return Error.UnsupportedExpression;

            // Allocate stack space for result (RocStr = 24 bytes)
            const result_offset = self.codegen.allocStackSlot(24);

            if (comptime builtin.cpu.arch == .aarch64) {
                // aarch64 C calling convention: X0=out, X1=value (X1+X2 for 16-byte), X2/X3=roc_ops
                // Save value to a temp register first since ensureInGeneralReg might return X0
                if (is_small_value) {
                    const val_reg = try self.ensureInGeneralReg(val_loc);
                    try self.codegen.emit.movRegReg(.w64, .X1, val_reg);
                    self.codegen.freeGeneral(val_reg);
                    // X2 = roc_ops
                    try self.codegen.emit.movRegReg(.w64, .X2, roc_ops_reg);
                } else {
                    // 16-byte value (Dec/i128): needs X1 and X2, roc_ops goes in X3
                    switch (val_loc) {
                        .stack_i128, .stack => |offset| {
                            try self.codegen.emitLoadStack(.w64, .X1, offset);
                            try self.codegen.emitLoadStack(.w64, .X2, offset + 8);
                        },
                        .immediate_i128 => |val| {
                            const low: u64 = @truncate(@as(u128, @bitCast(val)));
                            const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);
                            try self.codegen.emitLoadImm(.X1, @bitCast(low));
                            try self.codegen.emitLoadImm(.X2, @bitCast(high));
                        },
                        else => {
                            const val_reg = try self.ensureInGeneralReg(val_loc);
                            try self.codegen.emit.movRegReg(.w64, .X1, val_reg);
                            self.codegen.freeGeneral(val_reg);
                            try self.codegen.emitLoadImm(.X2, 0);
                        },
                    }
                    // X3 = roc_ops
                    try self.codegen.emit.movRegReg(.w64, .X3, roc_ops_reg);
                }

                // X0 = output pointer (FP + result_offset)
                try self.codegen.emit.movRegImm64(.X0, @bitCast(@as(i64, result_offset)));
                try self.codegen.emit.addRegRegReg(.w64, .X0, .FP, .X0);

                // Call
                try self.codegen.emitLoadImm(.X9, @intCast(fn_addr));
                try self.codegen.emit.blrReg(.X9);
            } else {
                // x86_64 C calling convention: RDI=out, RSI=value (RSI+RDX for 16-byte), RDX/RCX=roc_ops
                if (is_small_value) {
                    const val_reg = try self.ensureInGeneralReg(val_loc);
                    try self.codegen.emit.movRegReg(.w64, .RSI, val_reg);
                    self.codegen.freeGeneral(val_reg);
                    // RDX = roc_ops
                    try self.codegen.emit.movRegReg(.w64, .RDX, roc_ops_reg);
                } else {
                    // 16-byte value: RSI + RDX, roc_ops goes in RCX
                    switch (val_loc) {
                        .stack_i128, .stack => |offset| {
                            try self.codegen.emitLoadStack(.w64, .RSI, offset);
                            try self.codegen.emitLoadStack(.w64, .RDX, offset + 8);
                        },
                        .immediate_i128 => |val| {
                            const low: u64 = @truncate(@as(u128, @bitCast(val)));
                            const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);
                            try self.codegen.emitLoadImm(.RSI, @bitCast(low));
                            try self.codegen.emitLoadImm(.RDX, @bitCast(high));
                        },
                        else => {
                            const val_reg = try self.ensureInGeneralReg(val_loc);
                            try self.codegen.emit.movRegReg(.w64, .RSI, val_reg);
                            self.codegen.freeGeneral(val_reg);
                            try self.codegen.emitLoadImm(.RDX, 0);
                        },
                    }
                    // RCX = roc_ops
                    try self.codegen.emit.movRegReg(.w64, .RCX, roc_ops_reg);
                }

                // RDI = output pointer
                try self.codegen.emit.leaRegMem(.RDI, .RBP, result_offset);

                // Call
                try self.codegen.emitLoadImm(.R11, @intCast(fn_addr));
                try self.codegen.emit.callReg(.R11);
            }

            return .{ .stack_str = result_offset };
        }

        /// Generate code for str_escape_and_quote
        fn generateStrEscapeAndQuote(self: *Self, expr_id: anytype) Error!ValueLocation {
            _ = self;
            _ = expr_id;
            @panic("TODO: implement str_escape_and_quote");
        }

        /// Generate code for discriminant switch
        fn generateDiscriminantSwitch(self: *Self, ds: anytype) Error!ValueLocation {
            // Get the value and read its discriminant
            const value_loc = try self.generateExpr(ds.value);

            const ls = self.layout_store orelse return Error.UnsupportedExpression;
            const union_layout = ls.getLayout(ds.union_layout);
            if (union_layout.tag != .tag_union) return Error.UnsupportedExpression;

            const tu_data = ls.getTagUnionData(union_layout.data.tag_union.idx);
            const disc_offset = tu_data.discriminant_offset;

            // Load discriminant value
            const disc_reg = try self.allocTempGeneral();
            const base_offset: i32 = switch (value_loc) {
                .stack => |off| off,
                .stack_str => |off| off,
                else => return Error.UnsupportedExpression,
            };

            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.ldrRegMemSoff(.w64, disc_reg, .FP, base_offset + @as(i32, @intCast(disc_offset)));
            } else {
                try self.codegen.emit.movRegMem(.w64, disc_reg, .RBP, base_offset + @as(i32, @intCast(disc_offset)));
            }

            // Get the branches
            const branches = self.store.getExprSpan(ds.branches);
            if (branches.len == 0) return Error.UnsupportedExpression;

            // For single branch, just return it
            if (branches.len == 1) {
                self.codegen.freeGeneral(disc_reg);
                return try self.generateExpr(branches[0]);
            }

            // TODO: Implement full switch with jump table for many branches
            // For now, use if-else chain for small number of branches

            // Determine result size from the tag union's variant payloads
            // The result could be Dec (16 bytes), String (24 bytes), or smaller types
            const result_slot_size: u32 = blk: {
                const variants = ls.getTagUnionVariants(tu_data);
                var max_size: u32 = 8;
                for (0..variants.len) |vi| {
                    const variant = variants.get(vi);
                    const vl = ls.getLayout(variant.payload_layout);
                    const vs = ls.layoutSizeAlign(vl).size;
                    if (vs > max_size) max_size = vs;
                }
                break :blk max_size;
            };

            // Allocate result slot sized for the largest payload
            const result_slot = self.codegen.allocStackSlot(result_slot_size);
            var exit_patches = std.ArrayList(usize).empty;
            defer exit_patches.deinit(self.allocator);

            // Track what kind of value was stored to determine return type
            var result_is_i128 = false;

            for (branches, 0..) |branch_expr, i| {
                if (i < branches.len - 1) {
                    // Compare discriminant with branch index
                    try self.emitCmpImm(disc_reg, @intCast(i));
                    const skip_patch = try self.emitJumpIfNotEqual();

                    // Generate branch body
                    const branch_loc = try self.generateExpr(branch_expr);
                    if (branch_loc == .stack_i128 or branch_loc == .immediate_i128) result_is_i128 = true;
                    try self.storeResultToSlot(result_slot, branch_loc, result_slot_size);

                    // Jump to end
                    const exit_patch = try self.emitJumpUnconditional();
                    try exit_patches.append(self.allocator, exit_patch);

                    // Patch the skip jump to current location
                    const skip_offset = self.codegen.currentOffset();
                    self.codegen.patchJump(skip_patch, skip_offset);
                } else {
                    // Last branch is the default
                    const branch_loc = try self.generateExpr(branch_expr);
                    if (branch_loc == .stack_i128 or branch_loc == .immediate_i128) result_is_i128 = true;
                    try self.storeResultToSlot(result_slot, branch_loc, result_slot_size);
                }
            }

            // Patch all exit jumps to current location
            const end_offset = self.codegen.currentOffset();
            for (exit_patches.items) |patch| {
                self.codegen.patchJump(patch, end_offset);
            }

            self.codegen.freeGeneral(disc_reg);

            // Return with appropriate value location type
            if (result_is_i128 or result_slot_size == 16) {
                return .{ .stack_i128 = result_slot };
            } else {
                return .{ .stack = result_slot };
            }
        }

        /// Helper to store a result to a stack slot
        fn storeResultToSlot(self: *Self, slot: i32, loc: ValueLocation, slot_size: u32) Error!void {
            const temp_reg = try self.allocTempGeneral();
            switch (loc) {
                .immediate_i64 => |val| {
                    try self.codegen.emitLoadImm(temp_reg, @bitCast(val));
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.strRegMemSoff(.w64, temp_reg, .FP, slot);
                    } else {
                        try self.codegen.emit.movMemReg(.w64, .RBP, slot, temp_reg);
                    }
                },
                .immediate_i128 => |val| {
                    // Store low 64 bits
                    try self.codegen.emitLoadImm(temp_reg, @bitCast(@as(i64, @truncate(val))));
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.strRegMemSoff(.w64, temp_reg, .FP, slot);
                    } else {
                        try self.codegen.emit.movMemReg(.w64, .RBP, slot, temp_reg);
                    }
                    // Store high 64 bits
                    try self.codegen.emitLoadImm(temp_reg, @bitCast(@as(i64, @truncate(val >> 64))));
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.strRegMemSoff(.w64, temp_reg, .FP, slot + 8);
                    } else {
                        try self.codegen.emit.movMemReg(.w64, .RBP, slot + 8, temp_reg);
                    }
                },
                .stack => |off| {
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.ldrRegMemSoff(.w64, temp_reg, .FP, off);
                        try self.codegen.emit.strRegMemSoff(.w64, temp_reg, .FP, slot);
                    } else {
                        try self.codegen.emit.movRegMem(.w64, temp_reg, .RBP, off);
                        try self.codegen.emit.movMemReg(.w64, .RBP, slot, temp_reg);
                    }
                },
                .stack_i128 => |off| {
                    // Copy 16 bytes (two 8-byte chunks)
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.ldrRegMemSoff(.w64, temp_reg, .FP, off);
                        try self.codegen.emit.strRegMemSoff(.w64, temp_reg, .FP, slot);
                        try self.codegen.emit.ldrRegMemSoff(.w64, temp_reg, .FP, off + 8);
                        try self.codegen.emit.strRegMemSoff(.w64, temp_reg, .FP, slot + 8);
                    } else {
                        try self.codegen.emit.movRegMem(.w64, temp_reg, .RBP, off);
                        try self.codegen.emit.movMemReg(.w64, .RBP, slot, temp_reg);
                        try self.codegen.emit.movRegMem(.w64, temp_reg, .RBP, off + 8);
                        try self.codegen.emit.movMemReg(.w64, .RBP, slot + 8, temp_reg);
                    }
                },
                .general_reg => |reg| {
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.strRegMemSoff(.w64, reg, .FP, slot);
                    } else {
                        try self.codegen.emit.movMemReg(.w64, .RBP, slot, reg);
                    }
                },
                else => {
                    // For other types, try a generic copy using slot_size
                    var offset: u32 = 0;
                    while (offset < slot_size) : (offset += 8) {
                        const src_off = switch (loc) {
                            .stack_str => |off| off + @as(i32, @intCast(offset)),
                            .list_stack => |ls_info| ls_info.struct_offset + @as(i32, @intCast(offset)),
                            else => break,
                        };
                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emit.ldrRegMemSoff(.w64, temp_reg, .FP, src_off);
                            try self.codegen.emit.strRegMemSoff(.w64, temp_reg, .FP, slot + @as(i32, @intCast(offset)));
                        } else {
                            try self.codegen.emit.movRegMem(.w64, temp_reg, .RBP, src_off);
                            try self.codegen.emit.movMemReg(.w64, .RBP, slot + @as(i32, @intCast(offset)), temp_reg);
                        }
                    }
                },
            }
            self.codegen.freeGeneral(temp_reg);
        }

        /// Emit a compare of two registers
        fn emitCmpReg(self: *Self, reg1: GeneralReg, reg2: GeneralReg) Error!void {
            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.cmpRegReg(.w64, reg1, reg2);
            } else {
                try self.codegen.emit.cmpRegReg(.w64, reg1, reg2);
            }
        }

        /// Emit a jump if greater or equal (for unsigned comparison)
        fn emitJumpIfGreaterOrEqual(self: *Self) Error!usize {
            if (comptime builtin.cpu.arch == .aarch64) {
                // B.CS (branch if carry set = unsigned higher or same) with placeholder offset
                const patch_loc = self.codegen.currentOffset();
                try self.codegen.emit.bcond(.cs, 0);
                return patch_loc;
            } else {
                // JAE (jump if unsigned above or equal) with placeholder offset
                const patch_loc = self.codegen.currentOffset() + 2;
                try self.codegen.emit.jae(@bitCast(@as(i32, 0)));
                return patch_loc;
            }
        }

        /// Emit an unconditional jump
        fn emitJumpUnconditional(self: *Self) Error!usize {
            return try self.codegen.emitJump();
        }

        /// Emit a backward jump to a known location
        fn emitJumpBackward(self: *Self, target: usize) Error!void {
            const current = self.codegen.currentOffset();
            // Calculate offset - need to account for instruction encoding
            if (comptime builtin.cpu.arch == .aarch64) {
                // aarch64 b instruction: offset is in words (4 bytes), relative to PC
                const byte_offset = @as(i32, @intCast(target)) - @as(i32, @intCast(current));
                try self.codegen.emit.b(byte_offset);
            } else {
                // x86_64: jmp rel32 - offset is relative to end of instruction
                const inst_size: i32 = 5; // JMP rel32 is 5 bytes
                const byte_offset = @as(i32, @intCast(target)) - @as(i32, @intCast(current)) - inst_size;
                try self.codegen.emit.jmpRel32(byte_offset);
            }
        }

        /// Store a discriminant value at the given offset
        fn storeDiscriminant(self: *Self, offset: i32, value: u16, disc_size: u8) Error!void {
            const reg = try self.allocTempGeneral();
            try self.codegen.emitLoadImm(reg, value);

            // Store appropriate size - architecture specific
            if (comptime builtin.cpu.arch == .aarch64) {
                // aarch64 only has .w32 and .w64 for emitStoreStack, use direct emit for smaller sizes
                switch (disc_size) {
                    1 => {
                        // Use strb for 1-byte store
                        if (offset >= 0 and offset <= 4095) {
                            try self.codegen.emit.strbRegMem(reg, .FP, @intCast(offset));
                        } else {
                            // For negative/large offsets, compute address first
                            try self.codegen.emit.movRegImm64(.IP0, @bitCast(@as(i64, offset)));
                            try self.codegen.emit.addRegRegReg(.w64, .IP0, .FP, .IP0);
                            try self.codegen.emit.strbRegMem(reg, .IP0, 0);
                        }
                    },
                    2 => {
                        // Use strh for 2-byte store
                        if (offset >= 0 and offset <= 8190) {
                            try self.codegen.emit.strhRegMem(reg, .FP, @intCast(@as(u32, @intCast(offset)) >> 1));
                        } else {
                            try self.codegen.emit.movRegImm64(.IP0, @bitCast(@as(i64, offset)));
                            try self.codegen.emit.addRegRegReg(.w64, .IP0, .FP, .IP0);
                            try self.codegen.emit.strhRegMem(reg, .IP0, 0);
                        }
                    },
                    else => {
                        // 4 or 8 bytes - use standard store
                        try self.codegen.emitStoreStack(.w64, offset, reg);
                    },
                }
            } else {
                // x86_64 supports all widths
                const width: x86_64.RegisterWidth = switch (disc_size) {
                    1 => .w8,
                    2 => .w16,
                    4 => .w32,
                    else => .w64,
                };
                try self.codegen.emitStoreStack(width, offset, reg);
            }

            self.codegen.freeGeneral(reg);
        }

        /// Generate code for a block
        fn generateBlock(self: *Self, block: anytype) Error!ValueLocation {
            const stmts = self.store.getStmts(block.stmts);

            // Process each statement
            for (stmts) |stmt| {
                // Generate code for the expression
                const expr_loc = try self.generateExpr(stmt.expr);
                // Get the expression's layout (for mutable variable binding with correct size)
                const expr_layout = self.getExprLayout(stmt.expr);
                // Bind the result to the pattern, using expr layout for mutable vars
                try self.bindPatternWithLayout(stmt.pattern, expr_loc, expr_layout);
            }

            // Generate the final expression
            return self.generateExpr(block.final_expr);
        }

        /// Bind a value to a pattern
        /// expr_layout_override: Optional layout from the expression being bound. If provided,
        /// this is used for mutable variables instead of the pattern's layout_idx (which may be wrong).
        fn bindPattern(self: *Self, pattern_id: MonoPatternId, value_loc: ValueLocation) Error!void {
            return self.bindPatternWithLayout(pattern_id, value_loc, null);
        }

        /// Bind a value to a pattern with an optional expression layout override
        fn bindPatternWithLayout(self: *Self, pattern_id: MonoPatternId, value_loc: ValueLocation, expr_layout_override: ?layout.Idx) Error!void {
            const pattern = self.store.getPattern(pattern_id);

            switch (pattern) {
                .bind => |bind| {
                    const symbol_key: u48 = @bitCast(bind.symbol);

                    // Check if this is a reassignable (mutable) variable
                    if (bind.symbol.ident_idx.attributes.reassignable) {
                        // Mutable variables need fixed stack slots for runtime updates
                        if (self.mutable_var_slots.get(symbol_key)) |var_info| {
                            // Re-binding: copy new value to the fixed slot at runtime
                            try self.copyBytesToStackOffset(var_info.slot, value_loc, var_info.size);
                            // symbol_locations already points to the fixed slot, don't change it
                        } else {
                            // First binding: allocate a fixed slot and copy value there
                            const ls = self.layout_store orelse unreachable;

                            // Use the pattern's layout for the mutable variable size.
                            // The pattern's layout_idx reflects the variable's declared/inferred type
                            // which includes all unification constraints (e.g., from both the initial
                            // assignment and subsequent rebindings). The expression's layout
                            // (expr_layout_override) can be wrong when the expression is wrapped in
                            // a call whose ret_layout wasn't properly resolved (e.g., List.with_capacity
                            // wrapped in a call with ret_layout=u8 instead of List).
                            const size: u32 = blk: {
                                const layout_val = ls.getLayout(bind.layout_idx);
                                break :blk ls.layoutSizeAlign(layout_val).size;
                            };

                            // Allocate a fixed stack slot for this mutable variable
                            const fixed_slot = self.codegen.allocStackSlot(size);

                            // Copy the initial value to the fixed slot
                            try self.copyBytesToStackOffset(fixed_slot, value_loc, size);

                            // Record the fixed slot info for future re-bindings
                            try self.mutable_var_slots.put(symbol_key, .{ .slot = fixed_slot, .size = size });

                            // Point symbol_locations to the fixed slot
                            // IMPORTANT: Preserve the correct ValueLocation type so that
                            // subsequent code correctly handles multi-register values
                            if (value_loc == .list_stack) {
                                try self.symbol_locations.put(symbol_key, .{ .list_stack = .{
                                    .struct_offset = fixed_slot,
                                    .data_offset = 0,
                                    .num_elements = 0,
                                } });
                            } else if (value_loc == .stack_str or size == 24) {
                                // Strings are 24 bytes - preserve .stack_str so return handling
                                // loads all 3 registers (ptr, len, capacity)
                                try self.symbol_locations.put(symbol_key, .{ .stack_str = fixed_slot });
                            } else if (value_loc == .stack_i128 or size == 16) {
                                // Dec/i128 values are 16 bytes - preserve .stack_i128
                                try self.symbol_locations.put(symbol_key, .{ .stack_i128 = fixed_slot });
                            } else {
                                try self.symbol_locations.put(symbol_key, .{ .stack = fixed_slot });
                            }
                        }
                    } else {
                        // Non-mutable: just record the location as before
                        try self.symbol_locations.put(symbol_key, value_loc);
                    }
                },
                .wildcard => {
                    // Ignore the value
                },
                .record => |rec| {
                    // Record destructuring: bind each field pattern
                    const ls = self.layout_store orelse return;
                    const record_layout = ls.getLayout(rec.record_layout);
                    if (record_layout.tag != .record) return;

                    const field_patterns = self.store.getPatternSpan(rec.fields);

                    // Get the base offset of the record
                    const base_offset: i32 = switch (value_loc) {
                        .stack => |off| off,
                        .stack_str => |off| off,
                        else => return, // Can't destructure non-stack values
                    };

                    // Bind each field
                    for (field_patterns, 0..) |field_pattern_id, i| {
                        const field_offset = ls.getRecordFieldOffset(record_layout.data.record.idx, @intCast(i));
                        const field_size = ls.getRecordFieldSize(record_layout.data.record.idx, @intCast(i));

                        // Create a location for the field
                        const field_loc: ValueLocation = if (field_size > 8)
                            .{ .stack = base_offset + @as(i32, @intCast(field_offset)) }
                        else
                            .{ .stack = base_offset + @as(i32, @intCast(field_offset)) };

                        try self.bindPattern(field_pattern_id, field_loc);
                    }
                },
                .tuple => |tup| {
                    // Tuple destructuring: bind each element pattern
                    const ls = self.layout_store orelse return;
                    const tuple_layout = ls.getLayout(tup.tuple_layout);
                    if (tuple_layout.tag != .tuple) return;

                    const elem_patterns = self.store.getPatternSpan(tup.elems);

                    // Get the base offset of the tuple
                    const base_offset: i32 = switch (value_loc) {
                        .stack => |off| off,
                        .stack_str => |off| off,
                        else => return, // Can't destructure non-stack values
                    };

                    // Bind each element
                    for (elem_patterns, 0..) |elem_pattern_id, i| {
                        const elem_offset = ls.getTupleElementOffset(tuple_layout.data.tuple.idx, @intCast(i));

                        // Create a location for the element
                        const elem_loc: ValueLocation = .{ .stack = base_offset + @as(i32, @intCast(elem_offset)) };

                        try self.bindPattern(elem_pattern_id, elem_loc);
                    }
                },
                .as_pattern => |as_pat| {
                    // As-pattern: bind the symbol AND recursively bind the inner pattern
                    const symbol_key: u48 = @bitCast(as_pat.symbol);
                    try self.symbol_locations.put(symbol_key, value_loc);

                    // Also bind the inner pattern
                    if (!as_pat.inner.isNone()) {
                        try self.bindPattern(as_pat.inner, value_loc);
                    }
                },
                .list => |lst| {
                    // List destructuring: bind prefix elements and optional rest
                    // Get the base offset of the list struct (ptr, len, capacity)
                    const base_offset: i32 = switch (value_loc) {
                        .stack => |off| off,
                        .stack_str => |off| off,
                        .list_stack => |list_info| list_info.struct_offset,
                        else => return,
                    };

                    const prefix_patterns = self.store.getPatternSpan(lst.prefix);

                    // For each prefix element, we need to load from the list data
                    // List layout: ptr at offset 0, len at offset 8, capacity at offset 16
                    // Elements are at ptr[0], ptr[1], etc.

                    // Get element size from layout
                    // Use element layout from the expression override if it's a list,
                    // otherwise use the pattern's element layout.
                    const ls = self.layout_store orelse return;
                    const elem_layout = if (expr_layout_override) |override_idx| blk: {
                        const override_layout = ls.getLayout(override_idx);
                        if (override_layout.tag == .list) {
                            break :blk ls.getLayout(override_layout.data.list);
                        }
                        break :blk ls.getLayout(lst.elem_layout);
                    } else ls.getLayout(lst.elem_layout);
                    const elem_size_align = ls.layoutSizeAlign(elem_layout);
                    const elem_size = elem_size_align.size;

                    // Load list pointer to a register
                    const list_ptr_reg = try self.allocTempGeneral();
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.ldrRegMemSoff(.w64, list_ptr_reg, .FP, base_offset);
                    } else {
                        try self.codegen.emit.movRegMem(.w64, list_ptr_reg, .RBP, base_offset);
                    }

                    // Bind each prefix element
                    for (prefix_patterns, 0..) |elem_pattern_id, i| {
                        // Allocate stack space for this element
                        const elem_slot = self.codegen.allocStackSlot(@intCast(elem_size));

                        // Copy element from list to stack
                        const elem_offset_in_list = @as(i32, @intCast(i * elem_size));
                        const temp_reg = try self.allocTempGeneral();

                        if (elem_size <= 8) {
                            // Load element from list[i] to temp
                            if (comptime builtin.cpu.arch == .aarch64) {
                                try self.codegen.emit.ldrRegMemSoff(.w64, temp_reg, list_ptr_reg, elem_offset_in_list);
                                try self.codegen.emit.strRegMemSoff(.w64, temp_reg, .FP, elem_slot);
                            } else {
                                try self.codegen.emit.movRegMem(.w64, temp_reg, list_ptr_reg, elem_offset_in_list);
                                try self.codegen.emit.movMemReg(.w64, .RBP, elem_slot, temp_reg);
                            }
                        } else {
                            // For larger elements, copy 8 bytes at a time
                            var copied: u32 = 0;
                            while (copied < elem_size) : (copied += 8) {
                                const src_off = elem_offset_in_list + @as(i32, @intCast(copied));
                                const dst_off = elem_slot + @as(i32, @intCast(copied));
                                if (comptime builtin.cpu.arch == .aarch64) {
                                    try self.codegen.emit.ldrRegMemSoff(.w64, temp_reg, list_ptr_reg, src_off);
                                    try self.codegen.emit.strRegMemSoff(.w64, temp_reg, .FP, dst_off);
                                } else {
                                    try self.codegen.emit.movRegMem(.w64, temp_reg, list_ptr_reg, src_off);
                                    try self.codegen.emit.movMemReg(.w64, .RBP, dst_off, temp_reg);
                                }
                            }
                        }

                        self.codegen.freeGeneral(temp_reg);

                        // Bind the element pattern to the stack slot
                        // Use stack_i128 for 16-byte elements (Dec/i128) so that i128 operations
                        // know to load the full 16 bytes
                        const elem_loc: ValueLocation = if (elem_size == 16)
                            .{ .stack_i128 = elem_slot }
                        else
                            .{ .stack = elem_slot };
                        try self.bindPattern(elem_pattern_id, elem_loc);
                    }

                    // Handle rest pattern (the remaining list after prefix)
                    if (!lst.rest.isNone()) {
                        // Create a new RocList for the remaining elements
                        // RocList layout: bytes (ptr), length (usize), capacity_or_alloc_ptr (usize)
                        const rest_slot = self.codegen.allocStackSlot(24);

                        const prefix_count = @as(u32, @intCast(prefix_patterns.len));
                        const prefix_byte_offset = prefix_count * elem_size;

                        // Calculate rest pointer: original_ptr + prefix_len * elem_size
                        const rest_ptr_reg = try self.allocTempGeneral();
                        if (prefix_byte_offset == 0) {
                            // No offset needed, just copy the pointer
                            if (comptime builtin.cpu.arch == .aarch64) {
                                try self.codegen.emit.movRegReg(.w64, rest_ptr_reg, list_ptr_reg);
                            } else {
                                try self.codegen.emit.movRegReg(.w64, rest_ptr_reg, list_ptr_reg);
                            }
                        } else {
                            // Add offset to pointer: rest_ptr = list_ptr + prefix_byte_offset
                            if (comptime builtin.cpu.arch == .aarch64) {
                                try self.codegen.emit.addRegRegImm12(.w64, rest_ptr_reg, list_ptr_reg, @intCast(prefix_byte_offset));
                            } else {
                                try self.codegen.emit.movRegReg(.w64, rest_ptr_reg, list_ptr_reg);
                                try self.codegen.emit.addRegImm32(.w64, rest_ptr_reg, @intCast(prefix_byte_offset));
                            }
                        }

                        // Store rest pointer at rest_slot + 0
                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emit.strRegMemSoff(.w64, rest_ptr_reg, .FP, rest_slot);
                        } else {
                            try self.codegen.emit.movMemReg(.w64, .RBP, rest_slot, rest_ptr_reg);
                        }
                        self.codegen.freeGeneral(rest_ptr_reg);

                        // Load original length from base_offset + 8
                        const len_reg = try self.allocTempGeneral();
                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emit.ldrRegMemSoff(.w64, len_reg, .FP, base_offset + 8);
                        } else {
                            try self.codegen.emit.movRegMem(.w64, len_reg, .RBP, base_offset + 8);
                        }

                        // Calculate rest length: original_length - prefix_count
                        if (prefix_count > 0) {
                            if (comptime builtin.cpu.arch == .aarch64) {
                                try self.codegen.emit.subRegRegImm12(.w64, len_reg, len_reg, @intCast(prefix_count));
                            } else {
                                try self.codegen.emit.subRegImm32(.w64, len_reg, @intCast(prefix_count));
                            }
                        }

                        // Store rest length at rest_slot + 8
                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emit.strRegMemSoff(.w64, len_reg, .FP, rest_slot + 8);
                        } else {
                            try self.codegen.emit.movMemReg(.w64, .RBP, rest_slot + 8, len_reg);
                        }

                        // For capacity, use the same length (this is a slice view, not a copy)
                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emit.strRegMemSoff(.w64, len_reg, .FP, rest_slot + 16);
                        } else {
                            try self.codegen.emit.movMemReg(.w64, .RBP, rest_slot + 16, len_reg);
                        }
                        self.codegen.freeGeneral(len_reg);

                        // Bind the rest pattern to the new list slot
                        try self.bindPattern(lst.rest, .{ .stack = rest_slot });
                    }

                    self.codegen.freeGeneral(list_ptr_reg);
                },
                .tag => |tag_pat| {
                    // Tag destructuring: bind payload patterns
                    // For lambda parameters, the tag match is already known, just bind the payload
                    const arg_patterns = self.store.getPatternSpan(tag_pat.args);
                    if (arg_patterns.len == 0) return;

                    const ls = self.layout_store orelse return;
                    const union_layout = ls.getLayout(tag_pat.union_layout);
                    if (union_layout.tag != .tag_union) return;

                    const tu_data = ls.getTagUnionData(union_layout.data.tag_union.idx);
                    // Payload is always at offset 0, discriminant comes after
                    const payload_offset: i32 = 0;

                    // Get the base offset
                    const base_offset: i32 = switch (value_loc) {
                        .stack => |off| off,
                        .stack_str => |off| off,
                        else => return,
                    };

                    // Get the variant's payload layout to determine element offsets
                    const variants = ls.getTagUnionVariants(tu_data);
                    const variant = variants.get(tag_pat.discriminant);
                    const payload_layout = ls.getLayout(variant.payload_layout);

                    // For tags with single arg, bind directly at payload offset
                    // For tags with multiple args (tuples), need to use tuple element offsets
                    if (arg_patterns.len == 1) {
                        // Use the correct value location type based on payload layout
                        const payload_loc_offset = base_offset + payload_offset;
                        const arg_loc: ValueLocation = if (variant.payload_layout == .i128 or
                            variant.payload_layout == .u128 or variant.payload_layout == .dec)
                            .{ .stack_i128 = payload_loc_offset }
                        else if (variant.payload_layout == .str)
                            .{ .stack_str = payload_loc_offset }
                        else if (payload_layout.tag == .list or payload_layout.tag == .list_of_zst)
                            .{ .list_stack = .{
                                .struct_offset = payload_loc_offset,
                                .data_offset = 0,
                                .num_elements = 0,
                            } }
                        else
                            .{ .stack = payload_loc_offset };
                        try self.bindPattern(arg_patterns[0], arg_loc);
                    } else {
                        // Multiple args means payload is a tuple - get offsets from tuple layout
                        if (payload_layout.tag == .tuple) {
                            for (arg_patterns, 0..) |arg_pattern_id, i| {
                                const tuple_elem_offset = ls.getTupleElementOffset(payload_layout.data.tuple.idx, @intCast(i));
                                const arg_offset = base_offset + payload_offset + @as(i32, @intCast(tuple_elem_offset));
                                try self.bindPattern(arg_pattern_id, .{ .stack = arg_offset });
                            }
                        } else {
                            // Payload is not a tuple but we have multiple patterns - this shouldn't happen
                            // but handle gracefully by treating each pattern as having the same location
                            for (arg_patterns) |arg_pattern_id| {
                                const arg_loc: ValueLocation = .{ .stack = base_offset + payload_offset };
                                try self.bindPattern(arg_pattern_id, arg_loc);
                            }
                        }
                    }
                },
                else => {
                    // Literal patterns (int_literal, float_literal, str_literal) don't bind anything
                    // They are used for matching in when expressions, not for binding
                },
            }
        }

        /// Get the register used for argument N in the calling convention
        fn getArgumentRegister(_: *Self, index: u8) GeneralReg {
            if (comptime builtin.cpu.arch == .aarch64) {
                // AArch64: X0-X7 for arguments
                if (index >= 8) {
                    std.debug.print("BUG: getArgumentRegister called with index {} >= 8 (only X0-X7 available)\n", .{index});
                    unreachable;
                }
                return @enumFromInt(index);
            } else {
                // x86_64 System V: RDI, RSI, RDX, RCX, R8, R9
                const arg_regs = [_]x86_64.GeneralReg{ .RDI, .RSI, .RDX, .RCX, .R8, .R9 };
                if (index >= arg_regs.len) {
                    std.debug.print("BUG: getArgumentRegister called with index {} >= 6 (only 6 arg regs available)\n", .{index});
                    unreachable;
                }
                return arg_regs[index];
            }
        }

        /// Get the register used for return values
        fn getReturnRegister(_: *Self) GeneralReg {
            if (comptime builtin.cpu.arch == .aarch64) {
                return .X0;
            } else {
                return .RAX;
            }
        }

        /// Emit a call instruction to a specific code offset
        fn emitCallToOffset(self: *Self, target_offset: usize) !void {
            const current = self.codegen.currentOffset();
            // Calculate relative byte offset (can be negative for backward call)
            const rel_offset: i32 = @intCast(@as(i64, @intCast(target_offset)) - @as(i64, @intCast(current)));

            if (comptime builtin.cpu.arch == .aarch64) {
                // BL instruction expects byte offset (it divides by 4 internally)
                try self.codegen.emit.bl(rel_offset);
            } else {
                // x86_64: CALL rel32
                // Offset is relative to instruction after the call (current + 5)
                const call_rel = rel_offset - 5;
                try self.codegen.emit.call(@bitCast(call_rel));
            }
        }

        /// Generate code for a function call
        fn generateCall(self: *Self, call: anytype) Error!ValueLocation {
            // Get the function expression
            const fn_expr = self.store.getExpr(call.fn_expr);

            return switch (fn_expr) {
                // Direct lambda call: compile as procedure and call
                .lambda => |lambda| {
                    const code_offset = self.compileLambdaAsProc(call.fn_expr, lambda) catch {
                        return try self.callLambdaBodyDirect(lambda, call.args);
                    };
                    return try self.generateCallToLambda(code_offset, call.args, call.ret_layout);
                },

                // Direct closure call: compile inner lambda as procedure and call
                .closure => |closure| {
                    const inner = self.store.getExpr(closure.lambda);
                    if (inner == .lambda) {
                        const code_offset = self.compileLambdaAsProc(closure.lambda, inner.lambda) catch {
                            return try self.callLambdaBodyDirect(inner.lambda, call.args);
                        };
                        return try self.generateCallToLambda(code_offset, call.args, call.ret_layout);
                    }
                    return Error.UnsupportedExpression;
                },

                // Chained calls: evaluate inner call, then call result with outer args
                .call => |inner_call| {
                    const inner_result = try self.generateCall(inner_call);
                    if (inner_result == .lambda_code) {
                        return try self.generateCallToLambda(
                            inner_result.lambda_code.code_offset,
                            call.args,
                            call.ret_layout,
                        );
                    }
                    if (inner_result == .closure_value) {
                        return try self.generateClosureDispatch(inner_result.closure_value, call.args, call.ret_layout);
                    }
                    return Error.UnsupportedExpression;
                },

                // Block calls: evaluate block, if it returns lambda_code or closure_value, call it
                .block => |block| {
                    const block_result = try self.generateBlock(block);
                    if (block_result == .lambda_code) {
                        return try self.generateCallToLambda(
                            block_result.lambda_code.code_offset,
                            call.args,
                            call.ret_layout,
                        );
                    }
                    if (block_result == .closure_value) {
                        return try self.generateClosureDispatch(block_result.closure_value, call.args, call.ret_layout);
                    }
                    return Error.UnsupportedExpression;
                },

                // Lookup a function and call it
                .lookup => |lookup| {
                    // Check if the symbol is bound to a lambda_code or closure_value location
                    const symbol_key: u48 = @bitCast(lookup.symbol);
                    if (self.symbol_locations.get(symbol_key)) |loc| {
                        switch (loc) {
                            .lambda_code => |lc| {
                                return try self.generateCallToLambda(
                                    lc.code_offset,
                                    call.args,
                                    call.ret_layout,
                                );
                            },
                            .closure_value => |cv| {
                                return try self.generateClosureDispatch(cv, call.args, call.ret_layout);
                            },
                            else => {},
                        }
                    }
                    return try self.generateLookupCall(lookup, call.args, call.ret_layout);
                },

                else => return Error.UnsupportedExpression,
            };
        }

        /// Generate code for a closure expression.
        /// Handles different closure representations based on the lambda set.
        fn generateClosure(self: *Self, closure: anytype) Error!ValueLocation {
            switch (closure.representation) {
                .enum_dispatch => |repr| {
                    // Multiple functions, no captures - just store the tag byte
                    // Use 8-byte slot for simplicity (tag is only 1 byte but stack alignment matters)
                    const slot = self.codegen.allocStackSlot(8);
                    const temp = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(temp, repr.tag);
                    // Store as 64-bit value (low byte is the tag, rest is padding)
                    try self.codegen.emitStoreStack(.w64, slot, temp);
                    self.codegen.freeGeneral(temp);
                    return .{ .closure_value = .{
                        .stack_offset = slot,
                        .representation = closure.representation,
                        .lambda = closure.lambda,
                        .captures = closure.captures,
                    } };
                },
                .union_repr => |repr| {
                    // Multiple functions with captures - store tag + captures as tagged union
                    const ls = self.layout_store orelse return Error.UnsupportedExpression;
                    const union_layout = ls.getLayout(repr.union_layout);
                    const union_size = ls.layoutSizeAlign(union_layout).size;
                    const slot = self.codegen.allocStackSlot(@intCast(union_size));
                    // Store tag at offset 0 as 64-bit (low 16 bits are the tag)
                    const temp = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(temp, repr.tag);
                    try self.codegen.emitStoreStack(.w64, slot, temp);
                    self.codegen.freeGeneral(temp);
                    // Materialize captures at payload offset (+8 for tag with padding)
                    try self.materializeCaptures(closure.captures, slot + 8);
                    return .{ .closure_value = .{
                        .stack_offset = slot,
                        .representation = closure.representation,
                        .lambda = closure.lambda,
                        .captures = closure.captures,
                    } };
                },
                .unwrapped_capture => {
                    // Single function with one capture - materialize capture to stack
                    // and store as closure_value for dispatch at call sites.
                    const ls = self.layout_store orelse return Error.UnsupportedExpression;
                    const capture_layout = ls.getLayout(closure.representation.unwrapped_capture.capture_layout);
                    const capture_size = ls.layoutSizeAlign(capture_layout).size;
                    const slot = self.codegen.allocStackSlot(@intCast(capture_size));
                    try self.materializeCaptures(closure.captures, slot);
                    return .{ .closure_value = .{
                        .stack_offset = slot,
                        .representation = closure.representation,
                        .lambda = closure.lambda,
                        .captures = closure.captures,
                    } };
                },
                .struct_captures => |sc| {
                    // Single function with multiple captures - materialize to struct on stack.
                    const ls = self.layout_store orelse return Error.UnsupportedExpression;
                    const struct_layout = ls.getLayout(sc.struct_layout);
                    const struct_size = ls.layoutSizeAlign(struct_layout).size;
                    const slot = self.codegen.allocStackSlot(@intCast(struct_size));
                    try self.materializeCaptures(closure.captures, slot);
                    return .{ .closure_value = .{
                        .stack_offset = slot,
                        .representation = closure.representation,
                        .lambda = closure.lambda,
                        .captures = closure.captures,
                    } };
                },
                .direct_call => {
                    // Direct call - compile as procedure for direct calling
                    const inner = self.store.getExpr(closure.lambda);
                    if (inner != .lambda) return Error.UnsupportedExpression;
                    const code_offset = try self.compileLambdaAsProc(closure.lambda, inner.lambda);
                    return .{ .lambda_code = .{
                        .code_offset = code_offset,
                        .ret_layout = inner.lambda.ret_layout,
                    } };
                },
            }
        }

        /// Materialize captured values to a stack location.
        /// Used when creating closure values with captures.
        fn materializeCaptures(self: *Self, captures_span: mono.MonoIR.MonoCaptureSpan, base_offset: i32) Error!void {
            const captures = self.store.getCaptures(captures_span);
            var offset: i32 = 0;
            for (captures) |capture| {
                const symbol_key: u48 = @bitCast(capture.symbol);
                if (self.symbol_locations.get(symbol_key)) |capture_loc| {
                    // Get size of this capture
                    const ls = self.layout_store orelse return Error.UnsupportedExpression;
                    const capture_layout = ls.getLayout(capture.layout_idx);
                    const capture_size = ls.layoutSizeAlign(capture_layout).size;
                    const is_i128 = (capture_size >= 16);

                    // Copy value to the struct offset
                    switch (capture_loc) {
                        .immediate_i128 => |val| {
                            // Store 128-bit immediate as two 64-bit halves
                            const low: u64 = @truncate(@as(u128, @bitCast(val)));
                            const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);
                            const temp = try self.allocTempGeneral();
                            try self.codegen.emitLoadImm(temp, @bitCast(low));
                            try self.codegen.emitStoreStack(.w64, base_offset + offset, temp);
                            try self.codegen.emitLoadImm(temp, @bitCast(high));
                            try self.codegen.emitStoreStack(.w64, base_offset + offset + 8, temp);
                            self.codegen.freeGeneral(temp);
                        },
                        .stack_i128 => |src_offset| {
                            // Copy both 64-bit halves
                            const temp = try self.allocTempGeneral();
                            try self.codegen.emitLoadStack(.w64, temp, src_offset);
                            try self.codegen.emitStoreStack(.w64, base_offset + offset, temp);
                            try self.codegen.emitLoadStack(.w64, temp, src_offset + 8);
                            try self.codegen.emitStoreStack(.w64, base_offset + offset + 8, temp);
                            self.codegen.freeGeneral(temp);
                        },
                        .general_reg => |reg| {
                            try self.codegen.emitStoreStack(.w64, base_offset + offset, reg);
                            if (is_i128) {
                                // Zero-extend to 128 bits
                                const temp = try self.allocTempGeneral();
                                try self.codegen.emitLoadImm(temp, 0);
                                try self.codegen.emitStoreStack(.w64, base_offset + offset + 8, temp);
                                self.codegen.freeGeneral(temp);
                            }
                        },
                        .stack => |src_offset| {
                            const temp = try self.allocTempGeneral();
                            try self.codegen.emitLoadStack(.w64, temp, src_offset);
                            try self.codegen.emitStoreStack(.w64, base_offset + offset, temp);
                            if (is_i128) {
                                try self.codegen.emitLoadStack(.w64, temp, src_offset + 8);
                                try self.codegen.emitStoreStack(.w64, base_offset + offset + 8, temp);
                            }
                            self.codegen.freeGeneral(temp);
                        },
                        .immediate_i64 => |val| {
                            const temp = try self.allocTempGeneral();
                            try self.codegen.emitLoadImm(temp, @bitCast(val));
                            try self.codegen.emitStoreStack(.w64, base_offset + offset, temp);
                            if (is_i128) {
                                // Sign-extend to 128 bits
                                const sign_ext: i64 = if (val < 0) -1 else 0;
                                try self.codegen.emitLoadImm(temp, sign_ext);
                                try self.codegen.emitStoreStack(.w64, base_offset + offset + 8, temp);
                            }
                            self.codegen.freeGeneral(temp);
                        },
                        else => {
                            const slot = try self.ensureOnStack(capture_loc, if (is_i128) 16 else 8);
                            const temp = try self.allocTempGeneral();
                            try self.codegen.emitLoadStack(.w64, temp, slot);
                            try self.codegen.emitStoreStack(.w64, base_offset + offset, temp);
                            if (is_i128) {
                                try self.codegen.emitLoadStack(.w64, temp, slot + 8);
                                try self.codegen.emitStoreStack(.w64, base_offset + offset + 8, temp);
                            }
                            self.codegen.freeGeneral(temp);
                        },
                    }
                    offset += @intCast(capture_size);
                } else {
                    return Error.LocalNotFound;
                }
            }
        }

        /// Generate code for dispatching a closure call.
        /// Handles the different closure representations with appropriate dispatch.
        fn generateClosureDispatch(
            self: *Self,
            cv: anytype,
            args_span: anytype,
            ret_layout: layout.Idx,
        ) Error!ValueLocation {
            switch (cv.representation) {
                .enum_dispatch => |repr| {
                    return try self.dispatchEnumClosure(cv.stack_offset, repr.lambda_set, args_span, ret_layout);
                },
                .union_repr => |repr| {
                    return try self.dispatchUnionClosure(cv.stack_offset, repr, args_span, ret_layout);
                },
                .unwrapped_capture => {
                    // Single function - call directly with the captured value
                    return try self.callSingleClosureWithCaptures(cv, args_span, ret_layout);
                },
                .struct_captures => {
                    // Single function - call directly with captures struct
                    return try self.callSingleClosureWithCaptures(cv, args_span, ret_layout);
                },
                .direct_call => {
                    // Lambda that couldn't be compiled as proc (e.g., captures
                    // mutable variables). Evaluate body directly in current scope.
                    const lambda_expr = self.store.getExpr(cv.lambda);
                    const lambda = switch (lambda_expr) {
                        .lambda => |l| l,
                        .closure => |c| blk: {
                            const inner = self.store.getExpr(c.lambda);
                            if (inner == .lambda) break :blk inner.lambda;
                            return Error.UnsupportedExpression;
                        },
                        else => return Error.UnsupportedExpression,
                    };
                    return try self.callLambdaBodyDirect(lambda, args_span);
                },
            }
        }

        /// Dispatch an enum closure (multiple functions, no captures).
        /// Generates a switch on the tag to dispatch to the correct function.
        fn dispatchEnumClosure(
            self: *Self,
            tag_offset: i32,
            lambda_set: mono.LambdaSetMemberSpan,
            args_span: anytype,
            ret_layout: layout.Idx,
        ) Error!ValueLocation {
            const members = self.store.getLambdaSetMembers(lambda_set);

            if (members.len == 0) {
                return Error.UnsupportedExpression;
            }

            if (members.len == 1) {
                // Single function - no dispatch needed
                const member = members[0];
                return try self.compileLambdaAndCall(member.lambda_body, args_span, ret_layout);
            }

            // Load tag from stack (stored as 64-bit value, tag is in low byte)
            const tag_reg = try self.allocTempGeneral();
            try self.codegen.emitLoadStack(.w64, tag_reg, tag_offset);

            // Allocate result slot
            const result_slot = self.codegen.allocStackSlot(8);

            // Track end jumps for patching
            var end_jumps = std.ArrayList(usize).empty;
            defer end_jumps.deinit(self.allocator);

            for (members, 0..) |member, i| {
                const is_last = (i == members.len - 1);

                if (!is_last) {
                    // Compare tag with this member's tag
                    try self.emitCmpImm(tag_reg, member.tag);
                    const skip_jump = try self.emitJumpIfNotEqual();

                    // Generate code for this branch
                    const result = try self.compileLambdaAndCall(member.lambda_body, args_span, ret_layout);
                    try self.copyToStackSlot(result_slot, result, ret_layout);

                    // Jump to end
                    try end_jumps.append(self.allocator, try self.codegen.emitJump());

                    // Patch skip_jump to here
                    self.codegen.patchJump(skip_jump, self.codegen.currentOffset());
                } else {
                    // Last case - no comparison needed (fallthrough)
                    const result = try self.compileLambdaAndCall(member.lambda_body, args_span, ret_layout);
                    try self.copyToStackSlot(result_slot, result, ret_layout);
                }
            }

            // Patch all end jumps to current location
            for (end_jumps.items) |jump| {
                self.codegen.patchJump(jump, self.codegen.currentOffset());
            }

            self.codegen.freeGeneral(tag_reg);
            return .{ .stack = result_slot };
        }

        /// Dispatch a union closure (multiple functions, some with captures).
        fn dispatchUnionClosure(
            self: *Self,
            union_offset: i32,
            repr: anytype,
            args_span: anytype,
            ret_layout: layout.Idx,
        ) Error!ValueLocation {
            const members = self.store.getLambdaSetMembers(repr.lambda_set);

            if (members.len == 0) {
                return Error.UnsupportedExpression;
            }

            if (members.len == 1) {
                // Single function - call with captures from payload
                const member = members[0];
                // Captures start at offset +8 (after tag with padding)
                return try self.compileLambdaAndCallWithCaptures(member, union_offset + 8, args_span, ret_layout);
            }

            // Load tag from stack (stored as 64-bit value, tag is in low bits)
            const tag_reg = try self.allocTempGeneral();
            try self.codegen.emitLoadStack(.w64, tag_reg, union_offset);

            // Allocate result slot
            const result_slot = self.codegen.allocStackSlot(8);

            // Track end jumps for patching
            var end_jumps = std.ArrayList(usize).empty;
            defer end_jumps.deinit(self.allocator);

            for (members, 0..) |member, i| {
                const is_last = (i == members.len - 1);

                if (!is_last) {
                    // Compare tag with this member's tag
                    try self.emitCmpImm(tag_reg, member.tag);
                    const skip_jump = try self.emitJumpIfNotEqual();

                    // Generate code for this branch (captures at +8 after tag with padding)
                    const result = try self.compileLambdaAndCallWithCaptures(member, union_offset + 8, args_span, ret_layout);
                    try self.copyToStackSlot(result_slot, result, ret_layout);

                    // Jump to end
                    try end_jumps.append(self.allocator, try self.codegen.emitJump());

                    // Patch skip_jump to here
                    self.codegen.patchJump(skip_jump, self.codegen.currentOffset());
                } else {
                    // Last case - no comparison needed (fallthrough)
                    const result = try self.compileLambdaAndCallWithCaptures(member, union_offset + 8, args_span, ret_layout);
                    try self.copyToStackSlot(result_slot, result, ret_layout);
                }
            }

            // Patch all end jumps to current location
            for (end_jumps.items) |jump| {
                self.codegen.patchJump(jump, self.codegen.currentOffset());
            }

            self.codegen.freeGeneral(tag_reg);
            return .{ .stack = result_slot };
        }

        /// Call a single closure (unwrapped_capture or struct_captures) by binding
        /// its captures to symbol_locations and evaluating the lambda body directly.
        fn callSingleClosureWithCaptures(
            self: *Self,
            cv: anytype,
            args_span: anytype,
            ret_layout: layout.Idx,
        ) Error!ValueLocation {
            _ = ret_layout;

            // Bind captures from the closure's stack data to their symbols
            const captures = self.store.getCaptures(cv.captures);
            var offset: i32 = 0;
            for (captures) |capture| {
                const symbol_key: u48 = @bitCast(capture.symbol);
                const ls = self.layout_store orelse return Error.UnsupportedExpression;
                const capture_layout = ls.getLayout(capture.layout_idx);
                const capture_size = ls.layoutSizeAlign(capture_layout).size;
                // Use stack_i128 for 128-bit values (Dec, i128, u128)
                if (capture_size >= 16) {
                    try self.symbol_locations.put(symbol_key, .{ .stack_i128 = cv.stack_offset + offset });
                } else {
                    try self.symbol_locations.put(symbol_key, .{ .stack = cv.stack_offset + offset });
                }
                offset += @intCast(capture_size);
            }

            // Get the lambda body and evaluate it with captures in scope
            const lambda_expr = self.store.getExpr(cv.lambda);
            const lambda = switch (lambda_expr) {
                .lambda => |l| l,
                .closure => |c| blk: {
                    const inner = self.store.getExpr(c.lambda);
                    if (inner == .lambda) break :blk inner.lambda;
                    return Error.UnsupportedExpression;
                },
                else => return Error.UnsupportedExpression,
            };

            return try self.callLambdaBodyDirect(lambda, args_span);
        }

        /// Call a lambda by binding its parameters to arguments and evaluating its body
        /// directly in the current scope. Used when a lambda can't be compiled as a
        /// standalone procedure (e.g., it captures variables from the enclosing scope).
        fn callLambdaBodyDirect(self: *Self, lambda: anytype, args_span: anytype) Error!ValueLocation {
            const args = self.store.getExprSpan(args_span);
            const params = self.store.getPatternSpan(lambda.params);
            for (params, 0..) |pattern_id, i| {
                if (i >= args.len) break;
                const arg_loc = try self.generateExpr(args[i]);
                try self.bindPattern(pattern_id, arg_loc);
            }
            return try self.generateExpr(lambda.body);
        }

        /// Compile a lambda body expression as a procedure and call it.
        fn compileLambdaAndCall(
            self: *Self,
            lambda_body: mono.MonoExprId,
            args_span: anytype,
            ret_layout: layout.Idx,
        ) Error!ValueLocation {
            const lambda_expr = self.store.getExpr(lambda_body);
            switch (lambda_expr) {
                .lambda => |lambda| {
                    const code_offset = self.compileLambdaAsProc(lambda_body, lambda) catch {
                        return try self.callLambdaBodyDirect(lambda, args_span);
                    };
                    return try self.generateCallToLambda(code_offset, args_span, ret_layout);
                },
                .closure => |closure| {
                    const inner = self.store.getExpr(closure.lambda);
                    if (inner == .lambda) {
                        const code_offset = self.compileLambdaAsProc(closure.lambda, inner.lambda) catch {
                            return try self.callLambdaBodyDirect(inner.lambda, args_span);
                        };
                        return try self.generateCallToLambda(code_offset, args_span, ret_layout);
                    }
                    return Error.UnsupportedExpression;
                },
                else => return Error.UnsupportedExpression,
            }
        }

        /// Compile a lambda and call it, binding captures from a stack location.
        /// Binds captures to symbol_locations, then evaluates the lambda body directly
        /// (captures need to stay in scope, so we don't compile as a separate procedure).
        fn compileLambdaAndCallWithCaptures(
            self: *Self,
            member: mono.LambdaSetMember,
            captures_offset: i32,
            args_span: anytype,
            ret_layout: layout.Idx,
        ) Error!ValueLocation {
            _ = ret_layout;

            // Bind captures from the stack to their symbols
            const captures = self.store.getCaptures(member.captures);
            var offset: i32 = 0;
            for (captures) |capture| {
                const symbol_key: u48 = @bitCast(capture.symbol);
                const ls = self.layout_store orelse return Error.UnsupportedExpression;
                const capture_layout = ls.getLayout(capture.layout_idx);
                const capture_size = ls.layoutSizeAlign(capture_layout).size;
                try self.symbol_locations.put(symbol_key, .{ .stack = captures_offset + offset });
                offset += @intCast(capture_size);
            }

            // Get the lambda and evaluate directly with captures in scope
            const lambda_expr = self.store.getExpr(member.lambda_body);
            const lambda = switch (lambda_expr) {
                .lambda => |l| l,
                .closure => |c| blk: {
                    const inner = self.store.getExpr(c.lambda);
                    if (inner == .lambda) break :blk inner.lambda;
                    return Error.UnsupportedExpression;
                },
                else => return Error.UnsupportedExpression,
            };

            return try self.callLambdaBodyDirect(lambda, args_span);
        }

        /// Copy a value location to a stack slot.
        fn copyToStackSlot(self: *Self, slot: i32, loc: ValueLocation, ret_layout: layout.Idx) Error!void {
            _ = ret_layout;
            switch (loc) {
                .general_reg => |reg| {
                    try self.codegen.emitStoreStack(.w64, slot, reg);
                },
                .stack => |src_offset| {
                    const temp = try self.allocTempGeneral();
                    try self.codegen.emitLoadStack(.w64, temp, src_offset);
                    try self.codegen.emitStoreStack(.w64, slot, temp);
                    self.codegen.freeGeneral(temp);
                },
                .immediate_i64 => |val| {
                    const temp = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(temp, @bitCast(val));
                    try self.codegen.emitStoreStack(.w64, slot, temp);
                    self.codegen.freeGeneral(temp);
                },
                else => {
                    // For other types, load to temp and store
                    const temp_slot = try self.ensureOnStack(loc, 8);
                    const temp = try self.allocTempGeneral();
                    try self.codegen.emitLoadStack(.w64, temp, temp_slot);
                    try self.codegen.emitStoreStack(.w64, slot, temp);
                    self.codegen.freeGeneral(temp);
                },
            }
        }

        /// Emit compare immediate instruction
        fn emitCmpImm(self: *Self, reg: GeneralReg, value: i64) !void {
            if (comptime builtin.cpu.arch == .aarch64) {
                // CMP reg, #imm12
                try self.codegen.emit.cmpRegImm12(.w64, reg, @intCast(value));
            } else {
                // x86_64: CMP reg, imm32
                // Load immediate into temporary register and compare
                const temp = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(temp, value);
                try self.codegen.emit.cmpRegReg(.w64, reg, temp);
                self.codegen.freeGeneral(temp);
            }
        }

        /// Emit jump if not equal (after comparison)
        ///
        /// BRANCH PATCHING MECHANISM:
        /// When generating switch dispatch, we don't know the jump target offset until
        /// we've generated the code for the branch body. So we:
        /// 1. Emit the branch instruction with offset=0 (placeholder)
        /// 2. Record the instruction's location (patch_loc)
        /// 3. Generate the branch body code
        /// 4. Calculate the actual offset: current_offset - patch_loc
        /// 5. Patch the instruction at patch_loc with the real offset
        ///
        /// WHY OFFSET 0 IS SAFE:
        /// Offset 0 means "jump to the next instruction" which is harmless if we
        /// somehow fail to patch. But in normal operation, codegen.patchJump()
        /// overwrites the placeholder before execution.
        ///
        /// RETURNS: The patch location (where the displacement bytes are) for later patching.
        fn emitJumpIfNotEqual(self: *Self) !usize {
            if (comptime builtin.cpu.arch == .aarch64) {
                // B.NE (branch if not equal) with placeholder offset
                // On aarch64, the entire 4-byte instruction encodes the offset
                const patch_loc = self.codegen.currentOffset();
                try self.codegen.emit.bcond(.ne, 0);
                return patch_loc;
            } else {
                // JNE (jump if not equal) with placeholder offset
                // x86_64: JNE rel32 is 0F 85 xx xx xx xx (6 bytes)
                // The displacement starts at offset +2, so patch_loc = currentOffset + 2
                const patch_loc = self.codegen.currentOffset() + 2;
                try self.codegen.emit.jne(@bitCast(@as(i32, 0)));
                return patch_loc;
            }
        }

        /// Emit a conditional jump for unsigned less than (for list length comparisons)
        fn emitJumpIfLessThan(self: *Self) !usize {
            if (comptime builtin.cpu.arch == .aarch64) {
                // B.CC (branch if carry clear = unsigned less than) with placeholder offset
                const patch_loc = self.codegen.currentOffset();
                try self.codegen.emit.bcond(.cc, 0);
                return patch_loc;
            } else {
                // JB (jump if below = unsigned less than) with placeholder offset
                const patch_loc = self.codegen.currentOffset() + 2;
                try self.codegen.emit.jccRel32(.below, @bitCast(@as(i32, 0)));
                return patch_loc;
            }
        }

        /// Emit a conditional jump if equal (for while loop false condition check)
        fn emitJumpIfEqual(self: *Self) !usize {
            if (comptime builtin.cpu.arch == .aarch64) {
                // B.EQ (branch if equal) with placeholder offset
                const patch_loc = self.codegen.currentOffset();
                try self.codegen.emit.bcond(.eq, 0);
                return patch_loc;
            } else {
                // JE (jump if equal) with placeholder offset
                const patch_loc = self.codegen.currentOffset() + 2;
                try self.codegen.emit.jccRel32(.equal, @bitCast(@as(i32, 0)));
                return patch_loc;
            }
        }

        /// Generate code for calling a looked-up function definition.
        fn generateLookupCall(self: *Self, lookup: anytype, args_span: anytype, ret_layout: layout.Idx) Error!ValueLocation {
            const symbol_key: u48 = @bitCast(lookup.symbol);

            // Check if the function was compiled as a procedure
            if (self.proc_registry.get(symbol_key)) |proc| {
                return try self.generateCallToCompiledProc(proc, args_span, ret_layout);
            }

            // Look up the function in top-level definitions
            if (self.store.getSymbolDef(lookup.symbol)) |def_expr_id| {
                const def_expr = self.store.getExpr(def_expr_id);

                return switch (def_expr) {
                    .lambda => |lambda| {
                        // Low_level wrapper lambdas are evaluated directly in the
                        // caller's scope. Compiling them as separate procedures causes
                        // issues with stack frame management for C function calls.
                        const body_expr = self.store.getExpr(lambda.body);
                        if (body_expr == .low_level) {
                            return try self.callLambdaBodyDirect(lambda, args_span);
                        }
                        const offset = self.compileLambdaAsProc(def_expr_id, lambda) catch {
                            return try self.callLambdaBodyDirect(lambda, args_span);
                        };
                        return try self.generateCallToLambda(offset, args_span, ret_layout);
                    },
                    .closure => |closure| {
                        const inner = self.store.getExpr(closure.lambda);
                        if (inner == .lambda) {
                            const inner_body = self.store.getExpr(inner.lambda.body);
                            if (inner_body == .low_level) {
                                return try self.callLambdaBodyDirect(inner.lambda, args_span);
                            }
                            const offset = self.compileLambdaAsProc(closure.lambda, inner.lambda) catch {
                                return try self.callLambdaBodyDirect(inner.lambda, args_span);
                            };
                            return try self.generateCallToLambda(offset, args_span, ret_layout);
                        }
                        return Error.UnsupportedExpression;
                    },
                    else => return Error.UnsupportedExpression,
                };
            }

            return Error.LocalNotFound;
        }

        /// Generate a call to an already-compiled procedure.
        /// This is used for recursive functions that were compiled via compileAllProcs.
        fn generateCallToCompiledProc(self: *Self, proc: CompiledProc, args_span: anytype, ret_layout: layout.Idx) Error!ValueLocation {
            // Evaluate arguments and place them in argument registers
            // Track register index separately since some args use multiple registers
            const args = self.store.getExprSpan(args_span);
            var reg_idx: u8 = 0;

            for (args) |arg_id| {
                const arg_loc = try self.generateExpr(arg_id);
                const arg_layout = self.getExprLayout(arg_id);

                // Handle i128/Dec arguments (need two registers)
                // Check both the value location AND the argument layout, because
                // mutable variables store Dec/i128 values as .stack (not .stack_i128)
                const is_i128_arg = (arg_loc == .stack_i128 or arg_loc == .immediate_i128) or
                    (arg_loc == .stack and arg_layout != null and
                    (arg_layout.? == .dec or arg_layout.? == .i128 or arg_layout.? == .u128));
                if (is_i128_arg) {
                    const low_reg = self.getArgumentRegister(reg_idx);
                    const high_reg = self.getArgumentRegister(reg_idx + 1);
                    switch (arg_loc) {
                        .stack_i128, .stack => |offset| {
                            if (comptime builtin.cpu.arch == .aarch64) {
                                try self.codegen.emitLoadStack(.w64, low_reg, offset);
                                try self.codegen.emitLoadStack(.w64, high_reg, offset + 8);
                            } else {
                                try self.codegen.emitLoadStack(.w64, low_reg, offset);
                                try self.codegen.emitLoadStack(.w64, high_reg, offset + 8);
                            }
                        },
                        .immediate_i128 => |val| {
                            const low: u64 = @truncate(@as(u128, @bitCast(val)));
                            const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);
                            try self.codegen.emitLoadImm(low_reg, @bitCast(low));
                            try self.codegen.emitLoadImm(high_reg, @bitCast(high));
                        },
                        else => unreachable,
                    }
                    reg_idx += 2;
                } else {
                    // Check if this is a list argument (24 bytes)
                    const is_list = if (arg_layout) |al| blk: {
                        if (self.layout_store) |ls| {
                            const layout_val = ls.getLayout(al);
                            break :blk layout_val.tag == .list or layout_val.tag == .list_of_zst;
                        }
                        break :blk false;
                    } else (arg_loc == .list_stack);

                    if (is_list) {
                        // List arguments need 3 registers
                        const offset: i32 = switch (arg_loc) {
                            .stack => |off| off,
                            .list_stack => |li| li.struct_offset,
                            else => unreachable, // Lists must always be on the stack
                        };

                        const reg0 = self.getArgumentRegister(reg_idx);
                        const reg1 = self.getArgumentRegister(reg_idx + 1);
                        const reg2 = self.getArgumentRegister(reg_idx + 2);

                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emit.ldrRegMemSoff(.w64, reg0, .FP, offset);
                            try self.codegen.emit.ldrRegMemSoff(.w64, reg1, .FP, offset + 8);
                            try self.codegen.emit.ldrRegMemSoff(.w64, reg2, .FP, offset + 16);
                        } else {
                            try self.codegen.emit.movRegMem(.w64, reg0, .RBP, offset);
                            try self.codegen.emit.movRegMem(.w64, reg1, .RBP, offset + 8);
                            try self.codegen.emit.movRegMem(.w64, reg2, .RBP, offset + 16);
                        }
                        reg_idx += 3;
                    } else if (arg_layout != null and self.layout_store != null) blk: {
                        // Check if this is a record > 8 bytes that needs multiple registers
                        const ls = self.layout_store.?;
                        const layout_val = ls.getLayout(arg_layout.?);
                        if (layout_val.tag == .record) {
                            const size = ls.layoutSizeAlign(layout_val).size;
                            if (size > 8) {
                                const offset: i32 = switch (arg_loc) {
                                    .stack => |off| off,
                                    else => break :blk,
                                };
                                const num_regs: u8 = @intCast((size + 7) / 8);
                                var ri: u8 = 0;
                                while (ri < num_regs) : (ri += 1) {
                                    const r = self.getArgumentRegister(reg_idx + ri);
                                    try self.codegen.emitLoadStack(.w64, r, offset + @as(i32, ri) * 8);
                                }
                                reg_idx += num_regs;
                                continue;
                            }
                        }
                        // Default: single register
                        const arg_reg = self.getArgumentRegister(reg_idx);
                        try self.moveToReg(arg_loc, arg_reg);
                        reg_idx += 1;
                    } else {
                        const arg_reg = self.getArgumentRegister(reg_idx);
                        try self.moveToReg(arg_loc, arg_reg);
                        reg_idx += 1;
                    }
                }
            }

            // Emit the call instruction
            try self.emitCallToOffset(proc.code_start);

            // Handle i128/Dec return values (returned in two registers)
            if (ret_layout == .i128 or ret_layout == .u128 or ret_layout == .dec) {
                const stack_offset = self.codegen.allocStackSlot(16);
                if (comptime builtin.cpu.arch == .aarch64) {
                    try self.codegen.emitStoreStack(.w64, stack_offset, .X0);
                    try self.codegen.emitStoreStack(.w64, stack_offset + 8, .X1);
                } else {
                    try self.codegen.emitStoreStack(.w64, stack_offset, .RAX);
                    try self.codegen.emitStoreStack(.w64, stack_offset + 8, .RDX);
                }
                return .{ .stack_i128 = stack_offset };
            }

            // Check if return type is a string (24 bytes)
            if (ret_layout == .str) {
                const stack_offset = self.codegen.allocStackSlot(24);
                if (comptime builtin.cpu.arch == .aarch64) {
                    try self.codegen.emit.strRegMemSoff(.w64, .X0, .FP, stack_offset);
                    try self.codegen.emit.strRegMemSoff(.w64, .X1, .FP, stack_offset + 8);
                    try self.codegen.emit.strRegMemSoff(.w64, .X2, .FP, stack_offset + 16);
                } else {
                    try self.codegen.emit.movMemReg(.w64, .RBP, stack_offset, .RAX);
                    try self.codegen.emit.movMemReg(.w64, .RBP, stack_offset + 8, .RDX);
                    try self.codegen.emit.movMemReg(.w64, .RBP, stack_offset + 16, .RCX);
                }
                return .{ .stack_str = stack_offset };
            }

            // Check if return type is a list (24 bytes)
            const is_list_return = if (self.layout_store) |ls| blk: {
                const layout_val = ls.getLayout(ret_layout);
                break :blk layout_val.tag == .list or layout_val.tag == .list_of_zst;
            } else false;

            if (is_list_return) {
                // List return (24 bytes) - save X0/X1/X2 to stack
                // Use .list_stack so recursive calls properly detect this as a list argument
                // (the fallback check at arg_loc == .list_stack needs this)
                const stack_offset = self.codegen.allocStackSlot(24);
                if (comptime builtin.cpu.arch == .aarch64) {
                    try self.codegen.emit.strRegMemSoff(.w64, .X0, .FP, stack_offset);
                    try self.codegen.emit.strRegMemSoff(.w64, .X1, .FP, stack_offset + 8);
                    try self.codegen.emit.strRegMemSoff(.w64, .X2, .FP, stack_offset + 16);
                } else {
                    try self.codegen.emit.movMemReg(.w64, .RBP, stack_offset, .RAX);
                    try self.codegen.emit.movMemReg(.w64, .RBP, stack_offset + 8, .RDX);
                    try self.codegen.emit.movMemReg(.w64, .RBP, stack_offset + 16, .RCX);
                }
                return .{ .list_stack = .{
                    .struct_offset = stack_offset,
                    .data_offset = 0, // Data location is stored in the list struct itself
                    .num_elements = 0, // Unknown at compile time for returned lists
                } };
            }

            // Check if return type is a multi-register struct (record, tag_union, tuple > 8 bytes)
            if (self.layout_store) |ls| {
                const layout_val = ls.getLayout(ret_layout);
                if (layout_val.tag == .record or layout_val.tag == .tag_union or layout_val.tag == .tuple) {
                    const size_align = ls.layoutSizeAlign(layout_val);
                    if (size_align.size > 8) {
                        // Large struct return - save multiple registers to stack
                        const stack_offset = self.codegen.allocStackSlot(size_align.size);
                        const num_regs = (size_align.size + 7) / 8;
                        if (comptime builtin.cpu.arch == .aarch64) {
                            const regs = [_]@TypeOf(GeneralReg.X0){ .X0, .X1, .X2, .X3 };
                            for (0..@min(num_regs, 4)) |i| {
                                try self.codegen.emit.strRegMemSoff(.w64, regs[i], .FP, stack_offset + @as(i32, @intCast(i * 8)));
                            }
                        } else {
                            const regs = [_]@TypeOf(GeneralReg.RAX){ .RAX, .RDX, .RCX, .R8 };
                            for (0..@min(num_regs, 4)) |i| {
                                try self.codegen.emit.movMemReg(.w64, .RBP, stack_offset + @as(i32, @intCast(i * 8)), regs[i]);
                            }
                        }
                        return .{ .stack = stack_offset };
                    }
                }
            }

            const ret_reg = self.getReturnRegister();
            return .{ .general_reg = ret_reg };
        }

        /// Move a value to a specific register
        fn moveToReg(self: *Self, loc: ValueLocation, target_reg: GeneralReg) Error!void {
            switch (loc) {
                .general_reg => |src_reg| {
                    if (src_reg != target_reg) {
                        try self.emitMovRegReg(target_reg, src_reg);
                    }
                },
                .immediate_i64 => |val| {
                    try self.codegen.emitLoadImm(target_reg, val);
                },
                .immediate_i128 => |val| {
                    // Only load low 64 bits into single register
                    const low: i64 = @truncate(val);
                    try self.codegen.emitLoadImm(target_reg, low);
                },
                .stack => |offset| {
                    try self.codegen.emitLoadStack(.w64, target_reg, offset);
                },
                .stack_i128 => |offset| {
                    // Only load low 64 bits
                    try self.codegen.emitLoadStack(.w64, target_reg, offset);
                },
                .stack_str => |offset| {
                    // Load ptr/data (first 8 bytes of string struct)
                    try self.codegen.emitLoadStack(.w64, target_reg, offset);
                },
                .list_stack => |list_info| {
                    // Load ptr (first 8 bytes of list struct)
                    try self.codegen.emitLoadStack(.w64, target_reg, list_info.struct_offset);
                },
                .lambda_code => |lc| {
                    // Load the code offset into the register - used when passing lambdas as arguments
                    try self.codegen.emitLoadImm(target_reg, @bitCast(@as(i64, @intCast(lc.code_offset))));
                },
                .closure_value => |cv| {
                    // Load the closure value from stack
                    try self.codegen.emitLoadStack(.w64, target_reg, cv.stack_offset);
                },
                .float_reg, .immediate_f64 => {
                    return Error.InvalidLocalLocation;
                },
            }
        }

        /// Allocate a general register with a unique temporary local ID.
        /// Use this for temporary registers that don't correspond to real local variables.
        /// This prevents register ownership conflicts that can corrupt spill tracking.
        fn allocTempGeneral(self: *Self) Error!GeneralReg {
            const local_id = self.next_temp_local;
            self.next_temp_local +%= 1;
            return self.codegen.allocGeneralFor(local_id);
        }

        /// Ensure a value location is on the stack, spilling if needed. Returns stack offset.
        fn ensureOnStack(self: *Self, loc: ValueLocation, size: u32) Error!i32 {
            return switch (loc) {
                .stack, .stack_i128, .stack_str => |off| off,
                .list_stack => |info| info.struct_offset,
                .general_reg => |reg| blk: {
                    const slot = self.codegen.allocStackSlot(@intCast(size));
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.strRegMemSoff(.w64, reg, .FP, slot);
                    } else {
                        try self.codegen.emit.movMemReg(.w64, .RBP, slot, reg);
                    }
                    self.codegen.freeGeneral(reg);
                    break :blk slot;
                },
                .immediate_i64 => |val| blk: {
                    const slot = self.codegen.allocStackSlot(8);
                    const temp = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(temp, val);
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.strRegMemSoff(.w64, temp, .FP, slot);
                    } else {
                        try self.codegen.emit.movMemReg(.w64, .RBP, slot, temp);
                    }
                    self.codegen.freeGeneral(temp);
                    break :blk slot;
                },
                .immediate_i128 => |val| blk: {
                    // Store 128-bit immediate to stack
                    const slot = self.codegen.allocStackSlot(16);
                    const temp = try self.allocTempGeneral();
                    const low: u64 = @truncate(@as(u128, @bitCast(val)));
                    const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);
                    try self.codegen.emitLoadImm(temp, @bitCast(low));
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.strRegMemSoff(.w64, temp, .FP, slot);
                    } else {
                        try self.codegen.emit.movMemReg(.w64, .RBP, slot, temp);
                    }
                    try self.codegen.emitLoadImm(temp, @bitCast(high));
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.strRegMemSoff(.w64, temp, .FP, slot + 8);
                    } else {
                        try self.codegen.emit.movMemReg(.w64, .RBP, slot + 8, temp);
                    }
                    self.codegen.freeGeneral(temp);
                    break :blk slot;
                },
                else => {
                    std.debug.print("BUG: ensureOnStack unsupported loc: {s}\n", .{@tagName(loc)});
                    unreachable;
                },
            };
        }

        /// Ensure a value is in a general-purpose register
        fn ensureInGeneralReg(self: *Self, loc: ValueLocation) Error!GeneralReg {
            switch (loc) {
                .general_reg => |reg| return reg,
                .immediate_i64 => |val| {
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadImm(reg, val);
                    return reg;
                },
                .immediate_i128 => |val| {
                    // Only load low 64 bits
                    const reg = try self.allocTempGeneral();
                    const low: i64 = @truncate(val);
                    try self.codegen.emitLoadImm(reg, low);
                    return reg;
                },
                .stack => |offset| {
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadStack(.w64, reg, offset);
                    return reg;
                },
                .stack_i128 => |offset| {
                    // Only load low 64 bits
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadStack(.w64, reg, offset);
                    return reg;
                },
                .stack_str => |offset| {
                    // Load ptr/data (first 8 bytes of string struct)
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadStack(.w64, reg, offset);
                    return reg;
                },
                .list_stack => |list_info| {
                    // Load ptr (first 8 bytes of list struct)
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadStack(.w64, reg, list_info.struct_offset);
                    return reg;
                },
                .closure_value => |cv| {
                    // Load the closure value from stack
                    const reg = try self.allocTempGeneral();
                    try self.codegen.emitLoadStack(.w64, reg, cv.stack_offset);
                    return reg;
                },
                .float_reg, .immediate_f64, .lambda_code => {
                    // Convert float to int or lambda_code to register - this shouldn't happen in normal code
                    return Error.InvalidLocalLocation;
                },
            }
        }

        /// Ensure a value is in a floating-point register
        fn ensureInFloatReg(self: *Self, loc: ValueLocation) Error!FloatReg {
            switch (loc) {
                .float_reg => |reg| return reg,
                .immediate_f64 => |val| {
                    const reg = self.codegen.allocFloat() orelse return Error.NoRegisterToSpill;
                    const bits: u64 = @bitCast(val);

                    if (bits == 0) {
                        // Special case: 0.0 can be loaded efficiently
                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emit.fmovFloatFromGen(.double, reg, .ZRSP);
                        } else {
                            try self.codegen.emit.xorpdRegReg(reg, reg);
                        }
                    } else {
                        if (comptime builtin.cpu.arch == .aarch64) {
                            // Load bits into scratch register, then FMOV to float register
                            try self.codegen.emit.movRegImm64(.IP0, @bitCast(bits));
                            try self.codegen.emit.fmovFloatFromGen(.double, reg, .IP0);
                        } else {
                            // x86_64: Store bits to stack, then load into float register
                            // Use a temporary stack slot
                            const stack_offset: i32 = -16; // Below any local variables
                            try self.codegen.emit.movRegImm64(.R11, @bitCast(bits));
                            try self.codegen.emit.movMemReg(.w64, .RBP, stack_offset, .R11);
                            try self.codegen.emit.movsdRegMem(reg, .RBP, stack_offset);
                        }
                    }
                    return reg;
                },
                .stack => |offset| {
                    const reg = self.codegen.allocFloat() orelse return Error.NoRegisterToSpill;
                    try self.codegen.emitLoadStackF64(reg, offset);
                    return reg;
                },
                .general_reg, .immediate_i64, .immediate_i128, .stack_i128, .stack_str, .list_stack, .lambda_code, .closure_value => {
                    // Convert int to float or lambda_code/closure_value - not supported
                    return Error.InvalidLocalLocation;
                },
            }
        }

        /// Store the result to the output buffer pointed to by a saved register
        /// This is used when the original result pointer (X0/RDI) may have been clobbered
        fn storeResultToSavedPtr(self: *Self, loc: ValueLocation, result_layout: layout.Idx, saved_ptr_reg: GeneralReg, tuple_len: usize) Error!void {
            // Handle tuples specially - copy all elements from stack to result buffer
            if (tuple_len > 1) {
                switch (loc) {
                    .stack => |base_offset| {
                        // Use layout store for accurate element offsets and sizes
                        if (self.layout_store) |ls| {
                            const tuple_layout = ls.getLayout(result_layout);
                            if (tuple_layout.tag == .tuple) {
                                const tuple_data = ls.getTupleData(tuple_layout.data.tuple.idx);
                                const total_size = tuple_data.size;

                                // Copy entire tuple as 8-byte chunks
                                const temp_reg = try self.allocTempGeneral();
                                var copied: u32 = 0;

                                while (copied < total_size) {
                                    const stack_offset = base_offset + @as(i32, @intCast(copied));
                                    const buf_offset: i32 = @as(i32, @intCast(copied));

                                    // Load from stack
                                    if (comptime builtin.cpu.arch == .aarch64) {
                                        try self.codegen.emit.ldrRegMemSoff(.w64, temp_reg, .FP, stack_offset);
                                    } else {
                                        try self.codegen.emit.movRegMem(.w64, temp_reg, .RBP, stack_offset);
                                    }

                                    // Store to result buffer
                                    if (comptime builtin.cpu.arch == .aarch64) {
                                        try self.codegen.emit.strRegMemSoff(.w64, temp_reg, saved_ptr_reg, buf_offset);
                                    } else {
                                        try self.codegen.emit.movMemReg(.w64, saved_ptr_reg, buf_offset, temp_reg);
                                    }

                                    copied += 8;
                                }

                                self.codegen.freeGeneral(temp_reg);
                                return;
                            }
                        }

                        // Fallback: copy tuple_len * 8 bytes
                        const temp_reg = try self.allocTempGeneral();
                        for (0..tuple_len) |i| {
                            const stack_offset = base_offset + @as(i32, @intCast(i)) * 8;
                            const buf_offset: i32 = @as(i32, @intCast(i)) * 8;

                            if (comptime builtin.cpu.arch == .aarch64) {
                                try self.codegen.emit.ldrRegMemSoff(.w64, temp_reg, .FP, stack_offset);
                                try self.codegen.emit.strRegMemSoff(.w64, temp_reg, saved_ptr_reg, buf_offset);
                            } else {
                                try self.codegen.emit.movRegMem(.w64, temp_reg, .RBP, stack_offset);
                                try self.codegen.emit.movMemReg(.w64, saved_ptr_reg, buf_offset, temp_reg);
                            }
                        }
                        self.codegen.freeGeneral(temp_reg);
                        return;
                    },
                    else => {
                        // Fallback - just store the single value
                    },
                }
            }

            switch (result_layout) {
                .i64, .i32, .i16, .i8, .u64, .u32, .u16, .u8, .bool => {
                    const reg = try self.ensureInGeneralReg(loc);
                    try self.emitStoreToMem(saved_ptr_reg, reg);
                },
                .f64 => {
                    switch (loc) {
                        .float_reg => |reg| {
                            try self.emitStoreFloatToMem(saved_ptr_reg, reg);
                        },
                        .immediate_f64 => |val| {
                            const bits: i64 = @bitCast(val);
                            const reg = try self.allocTempGeneral();
                            try self.codegen.emitLoadImm(reg, bits);
                            try self.emitStoreToMem(saved_ptr_reg, reg);
                            self.codegen.freeGeneral(reg);
                        },
                        else => {
                            const reg = try self.ensureInGeneralReg(loc);
                            try self.emitStoreToMem(saved_ptr_reg, reg);
                        },
                    }
                },
                .f32 => {
                    // F32: Convert from F64 and store 4 bytes
                    switch (loc) {
                        .float_reg => |reg| {
                            // Convert F64 to F32, then store 4 bytes
                            if (comptime builtin.cpu.arch == .aarch64) {
                                try self.codegen.emit.fcvtFloatFloat(.single, reg, .double, reg);
                                try self.codegen.emit.fstrRegMemUoff(.single, reg, saved_ptr_reg, 0);
                            } else {
                                try self.codegen.emit.cvtsd2ssRegReg(reg, reg);
                                try self.codegen.emit.movssMemReg(saved_ptr_reg, 0, reg);
                            }
                        },
                        .immediate_f64 => |val| {
                            // Convert to f32 bits and store 4 bytes
                            const f32_val: f32 = @floatCast(val);
                            const bits: u32 = @bitCast(f32_val);
                            const reg = try self.allocTempGeneral();
                            try self.codegen.emitLoadImm(reg, @as(i64, bits));
                            if (comptime builtin.cpu.arch == .aarch64) {
                                try self.codegen.emit.strRegMemUoff(.w32, reg, saved_ptr_reg, 0);
                            } else {
                                try self.codegen.emit.movMemReg(.w32, saved_ptr_reg, 0, reg);
                            }
                            self.codegen.freeGeneral(reg);
                        },
                        else => {
                            // Store 4 bytes from general register
                            const reg = try self.ensureInGeneralReg(loc);
                            if (comptime builtin.cpu.arch == .aarch64) {
                                try self.codegen.emit.strRegMemUoff(.w32, reg, saved_ptr_reg, 0);
                            } else {
                                try self.codegen.emit.movMemReg(.w32, saved_ptr_reg, 0, reg);
                            }
                        },
                    }
                },
                .i128, .u128, .dec => {
                    try self.storeI128ToMem(saved_ptr_reg, loc);
                },
                .str => {
                    // Strings are 24 bytes (ptr, len, capacity) - same as lists
                    switch (loc) {
                        .stack, .stack_str => |stack_offset| {
                            // Copy 24-byte RocStr struct from stack to result buffer
                            const temp_reg = try self.allocTempGeneral();

                            // Copy all 24 bytes (3 x 8-byte words)
                            if (comptime builtin.cpu.arch == .aarch64) {
                                try self.codegen.emit.ldrRegMemSoff(.w64, temp_reg, .FP, stack_offset);
                                try self.codegen.emit.strRegMemSoff(.w64, temp_reg, saved_ptr_reg, 0);
                                try self.codegen.emit.ldrRegMemSoff(.w64, temp_reg, .FP, stack_offset + 8);
                                try self.codegen.emit.strRegMemSoff(.w64, temp_reg, saved_ptr_reg, 8);
                                try self.codegen.emit.ldrRegMemSoff(.w64, temp_reg, .FP, stack_offset + 16);
                                try self.codegen.emit.strRegMemSoff(.w64, temp_reg, saved_ptr_reg, 16);
                            } else {
                                try self.codegen.emit.movRegMem(.w64, temp_reg, .RBP, stack_offset);
                                try self.codegen.emit.movMemReg(.w64, saved_ptr_reg, 0, temp_reg);
                                try self.codegen.emit.movRegMem(.w64, temp_reg, .RBP, stack_offset + 8);
                                try self.codegen.emit.movMemReg(.w64, saved_ptr_reg, 8, temp_reg);
                                try self.codegen.emit.movRegMem(.w64, temp_reg, .RBP, stack_offset + 16);
                                try self.codegen.emit.movMemReg(.w64, saved_ptr_reg, 16, temp_reg);
                            }

                            self.codegen.freeGeneral(temp_reg);
                        },
                        else => {
                            // Fallback for non-stack string location
                            const reg = try self.ensureInGeneralReg(loc);
                            try self.emitStoreToMem(saved_ptr_reg, reg);
                        },
                    }
                },
                else => {
                    // Check if this is a composite type (record/tuple/list) via layout store
                    if (self.layout_store) |ls| {
                        const layout_val = ls.getLayout(result_layout);
                        switch (layout_val.tag) {
                            .record => {
                                const record_data = ls.getRecordData(layout_val.data.record.idx);
                                try self.copyStackToPtr(loc, saved_ptr_reg, record_data.size);
                                return;
                            },
                            .tuple => {
                                const tuple_data = ls.getTupleData(layout_val.data.tuple.idx);
                                try self.copyStackToPtr(loc, saved_ptr_reg, tuple_data.size);
                                return;
                            },
                            .tag_union => {
                                const tu_data = ls.getTagUnionData(layout_val.data.tag_union.idx);
                                try self.copyStackToPtr(loc, saved_ptr_reg, tu_data.size);
                                return;
                            },
                            .list, .list_of_zst => {
                                // Lists are 24-byte structs (ptr, len, capacity)
                                try self.copyStackToPtr(loc, saved_ptr_reg, 24);
                                return;
                            },
                            .scalar => {
                                const sa = ls.layoutSizeAlign(layout_val);
                                if (sa.size == 24) {
                                    // Str: 24-byte struct (ptr, len, capacity)
                                    try self.copyStackToPtr(loc, saved_ptr_reg, 24);
                                } else if (sa.size == 16) {
                                    // i128/u128/Dec
                                    try self.storeI128ToMem(saved_ptr_reg, loc);
                                } else if (sa.size > 0) {
                                    // Small scalars (1-8 bytes)
                                    const reg = try self.ensureInGeneralReg(loc);
                                    try self.emitStoreToMem(saved_ptr_reg, reg);
                                }
                                return;
                            },
                            else => @panic("TODO: storeResultToSavedPtr for unsupported layout tag"),
                        }
                    } else {
                        unreachable; // non-scalar layout must have layout store
                    }
                },
            }
        }

        /// Copy bytes from stack location to memory pointed to by ptr_reg
        fn copyStackToPtr(self: *Self, loc: ValueLocation, ptr_reg: GeneralReg, size: u32) Error!void {
            switch (loc) {
                .stack => |stack_offset| {
                    // Copy size bytes from stack to destination
                    const temp_reg = try self.allocTempGeneral();
                    var remaining = size;
                    var src_offset: i32 = stack_offset;
                    var dst_offset: i32 = 0;

                    // Copy 8 bytes at a time
                    while (remaining >= 8) {
                        try self.codegen.emitLoadStack(.w64, temp_reg, src_offset);
                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emit.strRegMemUoff(.w64, temp_reg, ptr_reg, @intCast(@as(u32, @intCast(dst_offset)) >> 3));
                        } else {
                            try self.codegen.emit.movMemReg(.w64, ptr_reg, dst_offset, temp_reg);
                        }
                        src_offset += 8;
                        dst_offset += 8;
                        remaining -= 8;
                    }

                    // Handle remaining bytes (4, 2, 1)
                    if (remaining >= 4) {
                        try self.codegen.emitLoadStack(.w32, temp_reg, src_offset);
                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emit.strRegMemUoff(.w32, temp_reg, ptr_reg, @intCast(@as(u32, @intCast(dst_offset)) >> 2));
                        } else {
                            try self.codegen.emit.movMemReg(.w32, ptr_reg, dst_offset, temp_reg);
                        }
                        src_offset += 4;
                        dst_offset += 4;
                        remaining -= 4;
                    }

                    self.codegen.freeGeneral(temp_reg);
                },
                .list_stack => |list_info| {
                    // Copy 24 bytes from list struct on stack to destination
                    const temp_reg = try self.allocTempGeneral();
                    var remaining = size;
                    var src_offset: i32 = list_info.struct_offset;
                    var dst_offset: i32 = 0;

                    // Copy 8 bytes at a time
                    while (remaining >= 8) {
                        try self.codegen.emitLoadStack(.w64, temp_reg, src_offset);
                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emit.strRegMemUoff(.w64, temp_reg, ptr_reg, @intCast(@as(u32, @intCast(dst_offset)) >> 3));
                        } else {
                            try self.codegen.emit.movMemReg(.w64, ptr_reg, dst_offset, temp_reg);
                        }
                        src_offset += 8;
                        dst_offset += 8;
                        remaining -= 8;
                    }

                    self.codegen.freeGeneral(temp_reg);
                },
                else => {
                    // Not a stack location - try to store as single value
                    const reg = try self.ensureInGeneralReg(loc);
                    try self.emitStoreToMem(ptr_reg, reg);
                },
            }
        }

        /// Store 128-bit value to memory at [ptr_reg]
        fn storeI128ToMem(self: *Self, ptr_reg: GeneralReg, loc: ValueLocation) Error!void {
            switch (loc) {
                .immediate_i128 => |val| {
                    // Store low 64 bits, then high 64 bits
                    const low: u64 = @truncate(@as(u128, @bitCast(val)));
                    const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);

                    const reg = try self.allocTempGeneral();

                    // Store low 64 bits at [ptr]
                    try self.codegen.emitLoadImm(reg, @bitCast(low));
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.strRegMemUoff(.w64, reg, ptr_reg, 0);
                    } else {
                        try self.codegen.emit.movMemReg(.w64, ptr_reg, 0, reg);
                    }

                    // Store high 64 bits at [ptr + 8]
                    try self.codegen.emitLoadImm(reg, @bitCast(high));
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.strRegMemUoff(.w64, reg, ptr_reg, 1); // offset 1 = 8 bytes for u64
                    } else {
                        try self.codegen.emit.movMemReg(.w64, ptr_reg, 8, reg);
                    }

                    self.codegen.freeGeneral(reg);
                },
                .stack_i128, .stack, .stack_str => |offset| {
                    // Copy 16 bytes from stack to destination
                    const reg = try self.allocTempGeneral();

                    // Load low 64 bits from stack, store to dest
                    try self.codegen.emitLoadStack(.w64, reg, offset);
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.strRegMemUoff(.w64, reg, ptr_reg, 0);
                    } else {
                        try self.codegen.emit.movMemReg(.w64, ptr_reg, 0, reg);
                    }

                    // Load high 64 bits from stack, store to dest
                    try self.codegen.emitLoadStack(.w64, reg, offset + 8);
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.strRegMemUoff(.w64, reg, ptr_reg, 1);
                    } else {
                        try self.codegen.emit.movMemReg(.w64, ptr_reg, 8, reg);
                    }

                    self.codegen.freeGeneral(reg);
                },
                .immediate_i64 => |val| {
                    // Sign-extend i64 to i128 and store
                    const val_i128: i128 = val;
                    const low: u64 = @truncate(@as(u128, @bitCast(val_i128)));
                    const high: u64 = @truncate(@as(u128, @bitCast(val_i128)) >> 64);

                    const reg = try self.allocTempGeneral();

                    try self.codegen.emitLoadImm(reg, @bitCast(low));
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.strRegMemUoff(.w64, reg, ptr_reg, 0);
                    } else {
                        try self.codegen.emit.movMemReg(.w64, ptr_reg, 0, reg);
                    }

                    try self.codegen.emitLoadImm(reg, @bitCast(high));
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.strRegMemUoff(.w64, reg, ptr_reg, 1);
                    } else {
                        try self.codegen.emit.movMemReg(.w64, ptr_reg, 8, reg);
                    }

                    self.codegen.freeGeneral(reg);
                },
                .general_reg => |reg| {
                    // Only have low 64 bits in register - this is a bug indicator,
                    // but handle gracefully by storing low and zeroing high
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.strRegMemUoff(.w64, reg, ptr_reg, 0);
                        try self.codegen.emit.strRegMemUoff(.w64, .ZRSP, ptr_reg, 1);
                    } else {
                        try self.codegen.emit.movMemReg(.w64, ptr_reg, 0, reg);
                        const zero_reg = try self.allocTempGeneral();
                        try self.codegen.emitLoadImm(zero_reg, 0);
                        try self.codegen.emit.movMemReg(.w64, ptr_reg, 8, zero_reg);
                        self.codegen.freeGeneral(zero_reg);
                    }
                },
                else => {
                    // Unknown location type - shouldn't happen for i128
                    unreachable;
                },
            }
        }

        /// Store general register to memory at [ptr_reg] (architecture-specific)
        fn emitStoreToMem(self: *Self, ptr_reg: anytype, src_reg: GeneralReg) !void {
            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.strRegMemUoff(.w64, src_reg, ptr_reg, 0);
            } else {
                try self.codegen.emit.movMemReg(.w64, ptr_reg, 0, src_reg);
            }
        }

        /// Store float register to memory at [ptr_reg] (architecture-specific)
        fn emitStoreFloatToMem(self: *Self, ptr_reg: anytype, src_reg: FloatReg) !void {
            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.fstrRegMemUoff(.double, src_reg, ptr_reg, 0);
            } else {
                try self.codegen.emit.movsdMemReg(ptr_reg, 0, src_reg);
            }
        }

        /// Compile all procedures first, before generating any calls.
        /// This ensures all call targets are known before we need to patch calls.
        pub fn compileAllProcs(self: *Self, procs: []const MonoProc) Error!void {
            for (procs) |proc| {
                try self.compileProc(proc);
            }
        }

        /// Compile a single procedure as a complete unit.
        /// Generates prologue, body, and epilogue (including RET).
        fn compileProc(self: *Self, proc: MonoProc) Error!void {
            const code_start = self.codegen.currentOffset();
            const key: u48 = @bitCast(proc.name);

            // CRITICAL: Register the procedure BEFORE generating the body
            // so that recursive calls within the body can find this procedure.
            // We'll update code_end after generation is complete.
            try self.proc_registry.put(key, .{
                .code_start = code_start,
                .code_end = 0, // Placeholder, updated below
                .name = proc.name,
            });

            // Save current state - procedure has its own scope that shouldn't pollute caller
            const saved_stack_offset = self.codegen.stack_offset;
            var saved_symbol_locations = self.symbol_locations.clone() catch return Error.OutOfMemory;
            defer saved_symbol_locations.deinit();
            var saved_mutable_var_slots = self.mutable_var_slots.clone() catch return Error.OutOfMemory;
            defer saved_mutable_var_slots.deinit();

            // Clear state for procedure's scope
            self.symbol_locations.clearRetainingCapacity();
            self.mutable_var_slots.clearRetainingCapacity();

            // Generate function prologue (save frame, allocate stack)
            try self.emitPrologue();

            // Set up recursive context
            const old_recursive_symbol = self.current_recursive_symbol;
            const old_recursive_join_point = self.current_recursive_join_point;

            switch (proc.is_self_recursive) {
                .self_recursive => |join_point_id| {
                    self.current_recursive_symbol = proc.name;
                    self.current_recursive_join_point = join_point_id;
                },
                .not_self_recursive => {},
            }

            // Bind parameters to argument registers
            try self.bindProcParams(proc.args, proc.arg_layouts);

            // Generate the body (control flow statements)
            // Note: .ret statements in the body will emit epilogue+ret
            try self.generateStmt(proc.body);

            // Restore recursive context
            self.current_recursive_symbol = old_recursive_symbol;
            self.current_recursive_join_point = old_recursive_join_point;

            // Update the code_end now that generation is complete
            const code_end = self.codegen.currentOffset();
            if (self.proc_registry.getPtr(key)) |entry| {
                entry.code_end = code_end;
            }

            // Restore state
            self.codegen.stack_offset = saved_stack_offset;
            self.symbol_locations.deinit();
            self.symbol_locations = saved_symbol_locations.clone() catch return Error.OutOfMemory;
            self.mutable_var_slots.deinit();
            self.mutable_var_slots = saved_mutable_var_slots.clone() catch return Error.OutOfMemory;
        }

        /// Compile a lambda expression as a standalone procedure.
        /// Returns the code offset where the procedure starts.
        /// If the lambda was already compiled, returns the cached offset.
        fn compileLambdaAsProc(self: *Self, lambda_expr_id: MonoExprId, lambda: anytype) Error!usize {
            const key = @intFromEnum(lambda_expr_id);

            // Check if already compiled
            if (self.compiled_lambdas.get(key)) |code_offset| {
                return code_offset;
            }

            // Emit a jump over the lambda code to prevent fall-through
            // The lambda code is emitted inline, so we need to skip it during normal execution
            const skip_jump = try self.codegen.emitJump();
            // Record the start offset (after the jump, this is where calls will land)
            const code_start = self.codegen.currentOffset();

            // Register before generating (for potential recursive calls)
            try self.compiled_lambdas.put(key, code_start);

            // Save current state - both stack offset AND symbol locations
            // IMPORTANT: We must save symbol_locations because the procedure has its own
            // parameter bindings that shouldn't pollute the caller's symbol map
            const saved_stack_offset = self.codegen.stack_offset;
            var saved_symbol_locations = self.symbol_locations.clone() catch return Error.OutOfMemory;
            defer saved_symbol_locations.deinit();
            var saved_mutable_var_slots = self.mutable_var_slots.clone() catch return Error.OutOfMemory;
            defer saved_mutable_var_slots.deinit();

            // Clear state for the procedure's scope
            self.symbol_locations.clearRetainingCapacity();
            self.mutable_var_slots.clearRetainingCapacity();

            // Save early return state before generating body
            const saved_early_return_ret_layout = self.early_return_ret_layout;
            const saved_early_return_patches_len = self.early_return_patches.items.len;

            // Restore state on error so callers can fall back to callLambdaBodyDirect
            errdefer {
                self.codegen.stack_offset = saved_stack_offset;
                self.symbol_locations.deinit();
                self.symbol_locations = saved_symbol_locations.clone() catch unreachable;
                self.mutable_var_slots.deinit();
                self.mutable_var_slots = saved_mutable_var_slots.clone() catch unreachable;
                _ = self.compiled_lambdas.remove(key);
                self.codegen.patchJump(skip_jump, self.codegen.currentOffset());
                self.early_return_ret_layout = saved_early_return_ret_layout;
                self.early_return_patches.shrinkRetainingCapacity(saved_early_return_patches_len);
            }

            // Emit prologue
            try self.emitPrologue();

            // Bind parameters from argument registers
            try self.bindLambdaParams(lambda.params);

            // Set early return state so generateEarlyReturn can emit jumps
            self.early_return_ret_layout = lambda.ret_layout;

            // Generate the body
            const result_loc = try self.generateExpr(lambda.body);

            // Move result to return register if needed
            // Pass the return layout so we can handle records > 8 bytes
            try self.moveToReturnRegisterWithLayout(result_loc, lambda.ret_layout);

            // Record epilogue location for early return patches
            const epilogue_offset = self.codegen.currentOffset();

            // Emit epilogue and return
            try self.emitEpilogue();

            // Patch all early return jumps to point to the epilogue
            for (self.early_return_patches.items[saved_early_return_patches_len..]) |patch| {
                self.codegen.patchJump(patch, epilogue_offset);
            }
            // Restore early return state (trim patches back)
            self.early_return_patches.shrinkRetainingCapacity(saved_early_return_patches_len);
            self.early_return_ret_layout = saved_early_return_ret_layout;

            // Restore state
            self.codegen.stack_offset = saved_stack_offset;
            self.symbol_locations.deinit();
            self.symbol_locations = saved_symbol_locations.clone() catch return Error.OutOfMemory;
            self.mutable_var_slots.deinit();
            self.mutable_var_slots = saved_mutable_var_slots.clone() catch return Error.OutOfMemory;

            // Patch the skip jump to point here (after the lambda code)
            const after_lambda = self.codegen.currentOffset();
            self.codegen.patchJump(skip_jump, after_lambda);

            return code_start;
        }

        /// Bind lambda parameters from argument registers.
        /// Similar to bindProcParams but works with pattern spans.
        const max_arg_regs: u8 = if (builtin.cpu.arch == .aarch64) 8 else 6;

        fn bindLambdaParams(self: *Self, params: mono.MonoPatternSpan) Error!void {
            const pattern_ids = self.store.getPatternSpan(params);
            var reg_idx: u8 = 0;

            for (pattern_ids) |pattern_id| {
                if (reg_idx >= max_arg_regs) return Error.UnsupportedExpression;
                const pattern = self.store.getPattern(pattern_id);
                switch (pattern) {
                    .bind => |bind| {
                        const symbol_key: u48 = @bitCast(bind.symbol);

                        // Check if this is a string parameter (24 bytes, 3 registers)
                        if (bind.layout_idx == .str) {
                            const arg_reg0 = self.getArgumentRegister(reg_idx);
                            const arg_reg1 = self.getArgumentRegister(reg_idx + 1);
                            const arg_reg2 = self.getArgumentRegister(reg_idx + 2);

                            const stack_offset = self.codegen.allocStackSlot(24);
                            try self.codegen.emitStoreStack(.w64, stack_offset, arg_reg0);
                            try self.codegen.emitStoreStack(.w64, stack_offset + 8, arg_reg1);
                            try self.codegen.emitStoreStack(.w64, stack_offset + 16, arg_reg2);

                            try self.symbol_locations.put(symbol_key, .{ .stack_str = stack_offset });
                            reg_idx += 3;
                        }
                        // Check if this is an i128/Dec parameter (16 bytes, 2 registers)
                        else if (bind.layout_idx == .i128 or bind.layout_idx == .u128 or bind.layout_idx == .dec) {
                            const arg_reg0 = self.getArgumentRegister(reg_idx);
                            const arg_reg1 = self.getArgumentRegister(reg_idx + 1);

                            const stack_offset = self.codegen.allocStackSlot(16);
                            try self.codegen.emitStoreStack(.w64, stack_offset, arg_reg0);
                            try self.codegen.emitStoreStack(.w64, stack_offset + 8, arg_reg1);

                            try self.symbol_locations.put(symbol_key, .{ .stack_i128 = stack_offset });
                            reg_idx += 2;
                        }
                        // Check if this is a list, record, tag union, or tuple parameter needing multiple registers
                        else if (self.layout_store) |ls| {
                            if (@intFromEnum(bind.layout_idx) < ls.layouts.len()) {
                                const layout_val = ls.getLayout(bind.layout_idx);
                                if (layout_val.tag == .list or layout_val.tag == .list_of_zst) {
                                    const arg_reg0 = self.getArgumentRegister(reg_idx);
                                    const arg_reg1 = self.getArgumentRegister(reg_idx + 1);
                                    const arg_reg2 = self.getArgumentRegister(reg_idx + 2);

                                    const stack_offset = self.codegen.allocStackSlot(24);
                                    try self.codegen.emitStoreStack(.w64, stack_offset, arg_reg0);
                                    try self.codegen.emitStoreStack(.w64, stack_offset + 8, arg_reg1);
                                    try self.codegen.emitStoreStack(.w64, stack_offset + 16, arg_reg2);

                                    try self.symbol_locations.put(symbol_key, .{ .list_stack = .{
                                        .struct_offset = stack_offset,
                                        .data_offset = 0,
                                        .num_elements = 0,
                                    } });
                                    reg_idx += 3;
                                    continue;
                                }
                                // Record, tag union, and tuple parameters > 8 bytes need multiple registers
                                if (layout_val.tag == .record or layout_val.tag == .tag_union or layout_val.tag == .tuple) {
                                    const size = ls.layoutSizeAlign(layout_val).size;
                                    if (size > 8) {
                                        const num_regs: u8 = @intCast((size + 7) / 8);
                                        if (reg_idx + num_regs > max_arg_regs) return Error.UnsupportedExpression;
                                        const stack_offset = self.codegen.allocStackSlot(@intCast(size));
                                        var ri: u8 = 0;
                                        while (ri < num_regs) : (ri += 1) {
                                            const arg_r = self.getArgumentRegister(reg_idx + ri);
                                            try self.codegen.emitStoreStack(.w64, stack_offset + @as(i32, ri) * 8, arg_r);
                                        }
                                        try self.symbol_locations.put(symbol_key, .{ .stack = stack_offset });
                                        reg_idx += num_regs;
                                        continue;
                                    }
                                }
                            }
                            // Default: single 8-byte value
                            const arg_reg = self.getArgumentRegister(reg_idx);
                            const stack_offset = self.codegen.allocStackSlot(8);
                            try self.codegen.emitStoreStack(.w64, stack_offset, arg_reg);
                            try self.symbol_locations.put(symbol_key, .{ .stack = stack_offset });
                            reg_idx += 1;
                        } else {
                            // Default: single 8-byte value
                            const arg_reg = self.getArgumentRegister(reg_idx);
                            const stack_offset = self.codegen.allocStackSlot(8);
                            try self.codegen.emitStoreStack(.w64, stack_offset, arg_reg);
                            try self.symbol_locations.put(symbol_key, .{ .stack = stack_offset });
                            reg_idx += 1;
                        }
                    },
                    .wildcard => {
                        // Skip this argument register
                        reg_idx += 1;
                    },
                    .record => |rec| {
                        // Record destructuring: store registers to stack, then delegate to bindPattern
                        const ls = self.layout_store orelse return Error.UnsupportedExpression;
                        const record_layout = ls.getLayout(rec.record_layout);
                        const size = ls.layoutSizeAlign(record_layout).size;
                        const num_regs: u8 = @intCast((size + 7) / 8);
                        if (reg_idx + num_regs > max_arg_regs) return Error.UnsupportedExpression;
                        const stack_offset = self.codegen.allocStackSlot(@intCast(size));
                        var ri: u8 = 0;
                        while (ri < num_regs) : (ri += 1) {
                            const arg_r = self.getArgumentRegister(reg_idx + ri);
                            try self.codegen.emitStoreStack(.w64, stack_offset + @as(i32, ri) * 8, arg_r);
                        }
                        reg_idx += num_regs;
                        try self.bindPattern(pattern_id, .{ .stack = stack_offset });
                    },
                    .list => {
                        // List destructuring: lists are 24 bytes (ptr, len, capacity) = 3 registers
                        if (reg_idx + 3 > max_arg_regs) return Error.UnsupportedExpression;
                        const stack_offset = self.codegen.allocStackSlot(24);
                        const arg_reg0 = self.getArgumentRegister(reg_idx);
                        const arg_reg1 = self.getArgumentRegister(reg_idx + 1);
                        const arg_reg2 = self.getArgumentRegister(reg_idx + 2);
                        try self.codegen.emitStoreStack(.w64, stack_offset, arg_reg0);
                        try self.codegen.emitStoreStack(.w64, stack_offset + 8, arg_reg1);
                        try self.codegen.emitStoreStack(.w64, stack_offset + 16, arg_reg2);
                        reg_idx += 3;
                        try self.bindPattern(pattern_id, .{ .stack = stack_offset });
                    },
                    .tuple => |tup| {
                        // Tuple destructuring: store registers to stack, then delegate to bindPattern
                        const ls = self.layout_store orelse return Error.UnsupportedExpression;
                        const tuple_layout = ls.getLayout(tup.tuple_layout);
                        const size = ls.layoutSizeAlign(tuple_layout).size;
                        const num_regs: u8 = @intCast((size + 7) / 8);
                        if (reg_idx + num_regs > max_arg_regs) return Error.UnsupportedExpression;
                        const stack_offset = self.codegen.allocStackSlot(@intCast(size));
                        var ri: u8 = 0;
                        while (ri < num_regs) : (ri += 1) {
                            const arg_r = self.getArgumentRegister(reg_idx + ri);
                            try self.codegen.emitStoreStack(.w64, stack_offset + @as(i32, ri) * 8, arg_r);
                        }
                        reg_idx += num_regs;
                        try self.bindPattern(pattern_id, .{ .stack = stack_offset });
                    },
                    else => {
                        // For now, skip complex patterns
                        reg_idx += 1;
                    },
                }
            }
        }

        /// Move a value to the return register (X0 on aarch64, RAX on x86_64)
        /// Move a value to the return register(s), using layout information for proper sizing.
        fn moveToReturnRegisterWithLayout(self: *Self, loc: ValueLocation, ret_layout: layout.Idx) Error!void {
            // First check if the layout tells us this is a multi-register type > 8 bytes
            if (self.layout_store) |ls| {
                const layout_val = ls.getLayout(ret_layout);

                // Lists and strings are always 24 bytes (3 registers)
                if (layout_val.tag == .list or layout_val.tag == .list_of_zst) {
                    const stack_offset: i32 = switch (loc) {
                        .stack => |off| off,
                        .list_stack => |info| info.struct_offset,
                        else => return self.moveToReturnRegister(loc),
                    };
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emitLoadStack(.w64, .X0, stack_offset);
                        try self.codegen.emitLoadStack(.w64, .X1, stack_offset + 8);
                        try self.codegen.emitLoadStack(.w64, .X2, stack_offset + 16);
                    } else {
                        try self.codegen.emitLoadStack(.w64, .RAX, stack_offset);
                        try self.codegen.emitLoadStack(.w64, .RDX, stack_offset + 8);
                        try self.codegen.emitLoadStack(.w64, .RCX, stack_offset + 16);
                    }
                    return;
                }

                if (layout_val.tag == .record or layout_val.tag == .tag_union or layout_val.tag == .tuple) {
                    const size_align = ls.layoutSizeAlign(layout_val);
                    if (size_align.size > 8) {
                        // Large struct - need to return in multiple registers
                        const stack_offset: i32 = switch (loc) {
                            .stack => |off| off,
                            else => {
                                // For non-stack locations, fall through to regular handling
                                return self.moveToReturnRegister(loc);
                            },
                        };
                        const num_regs = (size_align.size + 7) / 8;
                        if (comptime builtin.cpu.arch == .aarch64) {
                            const regs = [_]@TypeOf(GeneralReg.X0){ .X0, .X1, .X2, .X3 };
                            for (0..@min(num_regs, 4)) |i| {
                                try self.codegen.emitLoadStack(.w64, regs[i], stack_offset + @as(i32, @intCast(i * 8)));
                            }
                        } else {
                            const regs = [_]@TypeOf(GeneralReg.RAX){ .RAX, .RDX, .RCX, .R8 };
                            for (0..@min(num_regs, 4)) |i| {
                                try self.codegen.emitLoadStack(.w64, regs[i], stack_offset + @as(i32, @intCast(i * 8)));
                            }
                        }
                        return;
                    }
                }
            }
            // Fall back to regular handling
            return self.moveToReturnRegister(loc);
        }

        fn moveToReturnRegister(self: *Self, loc: ValueLocation) Error!void {
            const ret_reg = self.getReturnRegister();
            switch (loc) {
                .general_reg => |reg| {
                    if (reg != ret_reg) {
                        try self.codegen.emit.movRegReg(.w64, ret_reg, reg);
                    }
                },
                .stack => |offset| {
                    try self.codegen.emitLoadStack(.w64, ret_reg, offset);
                },
                .immediate_i64 => |val| {
                    try self.codegen.emitLoadImm(ret_reg, @bitCast(val));
                },
                .stack_str => |offset| {
                    // String return (24 bytes) - load into X0/X1/X2 or RAX/RDX/RCX
                    try self.codegen.emitLoadStack(.w64, ret_reg, offset);
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emitLoadStack(.w64, .X1, offset + 8);
                        try self.codegen.emitLoadStack(.w64, .X2, offset + 16);
                    } else {
                        try self.codegen.emitLoadStack(.w64, .RDX, offset + 8);
                        try self.codegen.emitLoadStack(.w64, .RCX, offset + 16);
                    }
                },
                .list_stack => |info| {
                    // List return (24 bytes) - load into X0/X1/X2 or RAX/RDX/RCX
                    try self.codegen.emitLoadStack(.w64, ret_reg, info.struct_offset);
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emitLoadStack(.w64, .X1, info.struct_offset + 8);
                        try self.codegen.emitLoadStack(.w64, .X2, info.struct_offset + 16);
                    } else {
                        try self.codegen.emitLoadStack(.w64, .RDX, info.struct_offset + 8);
                        try self.codegen.emitLoadStack(.w64, .RCX, info.struct_offset + 16);
                    }
                },
                .stack_i128 => |offset| {
                    // For i128/Dec return values, load both halves
                    // X0 = low 64 bits, X1 = high 64 bits
                    try self.codegen.emitLoadStack(.w64, ret_reg, offset);
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emitLoadStack(.w64, .X1, offset + 8);
                    } else {
                        try self.codegen.emitLoadStack(.w64, .RDX, offset + 8);
                    }
                },
                .immediate_i128 => |val| {
                    // Load low 64 bits to X0, high 64 bits to X1
                    const low: i64 = @truncate(val);
                    const high: i64 = @truncate(val >> 64);
                    try self.codegen.emitLoadImm(ret_reg, low);
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emitLoadImm(.X1, high);
                    } else {
                        try self.codegen.emitLoadImm(.RDX, high);
                    }
                },
                .lambda_code => |lc| {
                    // Return lambda code location: code_offset in X0/RAX, ret_layout in X1/RDX
                    try self.codegen.emitLoadImm(ret_reg, @bitCast(@as(i64, @intCast(lc.code_offset))));
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emitLoadImm(.X1, @intFromEnum(lc.ret_layout));
                    } else {
                        try self.codegen.emitLoadImm(.RDX, @intFromEnum(lc.ret_layout));
                    }
                },
                .closure_value => {
                    // Can't return a closure_value from a compiled procedure - the
                    // closure's capture data lives on this procedure's stack frame
                    // which is deallocated on return.
                    return Error.UnsupportedExpression;
                },
                else => {
                    // For other types (like float_reg), try to handle appropriately
                },
            }
        }

        /// Generate a call to a compiled lambda procedure.
        /// Puts arguments in registers and emits a call instruction.
        ///
        /// Uses a two-pass approach to avoid register clobbering:
        /// 1. First generate all argument expressions (which may trigger nested calls
        ///    or allocate temp registers)
        /// 2. Then load the stable results into argument registers
        ///
        /// This prevents a bug where generating arg[1] could clobber argument
        /// registers (X2-X7) that were already loaded with arg[0]'s data.
        fn generateCallToLambda(self: *Self, code_offset: usize, args_span: anytype, ret_layout: layout.Idx) Error!ValueLocation {
            const args = self.store.getExprSpan(args_span);

            // Pass 1: Generate all argument expressions and ensure results are
            // in stable locations (stack or immediate). This prevents subsequent
            // arg generation from clobbering earlier results via temp registers.
            const ArgInfo = struct {
                loc: ValueLocation,
                layout_idx: ?layout.Idx,
            };
            var arg_infos: [8]ArgInfo = undefined;
            for (args, 0..) |arg_id, i| {
                if (i >= 8) break;
                const arg_loc = try self.generateExpr(arg_id);
                const arg_layout = self.getExprLayout(arg_id);

                arg_infos[i] = .{ .loc = arg_loc, .layout_idx = arg_layout };
            }

            // Pass 2: Load all argument values into registers from stable locations
            var reg_idx: u8 = 0;
            for (0..args.len) |i| {
                if (i >= 8) break;
                const arg_loc = arg_infos[i].loc;
                const arg_layout = arg_infos[i].layout_idx;

                // Handle i128/Dec arguments (need two registers)
                const is_i128_arg = (arg_loc == .stack_i128 or arg_loc == .immediate_i128) or
                    (arg_loc == .stack and arg_layout != null and
                    (arg_layout.? == .dec or arg_layout.? == .i128 or arg_layout.? == .u128));
                if (is_i128_arg) {
                    const low_reg = self.getArgumentRegister(reg_idx);
                    const high_reg = self.getArgumentRegister(reg_idx + 1);
                    switch (arg_loc) {
                        .stack_i128, .stack => |offset| {
                            if (comptime builtin.cpu.arch == .aarch64) {
                                try self.codegen.emitLoadStack(.w64, low_reg, offset);
                                try self.codegen.emitLoadStack(.w64, high_reg, offset + 8);
                            } else {
                                try self.codegen.emitLoadStack(.w64, low_reg, offset);
                                try self.codegen.emitLoadStack(.w64, high_reg, offset + 8);
                            }
                        },
                        .immediate_i128 => |val| {
                            const low: u64 = @truncate(@as(u128, @bitCast(val)));
                            const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);
                            try self.codegen.emitLoadImm(low_reg, @bitCast(low));
                            try self.codegen.emitLoadImm(high_reg, @bitCast(high));
                        },
                        else => unreachable,
                    }
                    reg_idx += 2;
                    continue;
                }

                const arg_reg = self.getArgumentRegister(reg_idx);

                switch (arg_loc) {
                    .general_reg => |reg| {
                        // Should not happen after spilling in pass 1, but handle anyway
                        if (reg != arg_reg) {
                            try self.codegen.emit.movRegReg(.w64, arg_reg, reg);
                        }
                        reg_idx += 1;
                    },
                    .stack => |offset| {
                        // Check if this is a multi-register struct (record, tag_union, tuple > 8 bytes)
                        var handled_as_record = false;
                        if (arg_layout != null and self.layout_store != null) {
                            const ls = self.layout_store.?;
                            const layout_val = ls.getLayout(arg_layout.?);
                            if (layout_val.tag == .record or layout_val.tag == .tag_union or layout_val.tag == .tuple) {
                                const size = ls.layoutSizeAlign(layout_val).size;
                                if (size > 8) {
                                    const num_regs: u8 = @intCast((size + 7) / 8);
                                    var ri: u8 = 0;
                                    while (ri < num_regs) : (ri += 1) {
                                        const r = self.getArgumentRegister(reg_idx + ri);
                                        try self.codegen.emitLoadStack(.w64, r, offset + @as(i32, ri) * 8);
                                    }
                                    reg_idx += num_regs;
                                    handled_as_record = true;
                                }
                            }
                        }
                        if (!handled_as_record) {
                            try self.codegen.emitLoadStack(.w64, arg_reg, offset);
                            reg_idx += 1;
                        }
                    },
                    .immediate_i64 => |val| {
                        try self.codegen.emitLoadImm(arg_reg, @bitCast(val));
                        reg_idx += 1;
                    },
                    .stack_str => |offset| {
                        // Strings need 3 registers (24 bytes: ptr/data, len, capacity)
                        const arg_reg0 = self.getArgumentRegister(reg_idx);
                        const arg_reg1 = self.getArgumentRegister(reg_idx + 1);
                        const arg_reg2 = self.getArgumentRegister(reg_idx + 2);
                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emit.ldrRegMemSoff(.w64, arg_reg0, .FP, offset);
                            try self.codegen.emit.ldrRegMemSoff(.w64, arg_reg1, .FP, offset + 8);
                            try self.codegen.emit.ldrRegMemSoff(.w64, arg_reg2, .FP, offset + 16);
                        } else {
                            try self.codegen.emit.movRegMem(.w64, arg_reg0, .RBP, offset);
                            try self.codegen.emit.movRegMem(.w64, arg_reg1, .RBP, offset + 8);
                            try self.codegen.emit.movRegMem(.w64, arg_reg2, .RBP, offset + 16);
                        }
                        reg_idx += 3;
                    },
                    .list_stack => |info| {
                        // Lists need 3 registers (24 bytes: ptr, len, capacity)
                        const arg_reg0 = self.getArgumentRegister(reg_idx);
                        const arg_reg1 = self.getArgumentRegister(reg_idx + 1);
                        const arg_reg2 = self.getArgumentRegister(reg_idx + 2);
                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emit.ldrRegMemSoff(.w64, arg_reg0, .FP, info.struct_offset);
                            try self.codegen.emit.ldrRegMemSoff(.w64, arg_reg1, .FP, info.struct_offset + 8);
                            try self.codegen.emit.ldrRegMemSoff(.w64, arg_reg2, .FP, info.struct_offset + 16);
                        } else {
                            try self.codegen.emit.movRegMem(.w64, arg_reg0, .RBP, info.struct_offset);
                            try self.codegen.emit.movRegMem(.w64, arg_reg1, .RBP, info.struct_offset + 8);
                            try self.codegen.emit.movRegMem(.w64, arg_reg2, .RBP, info.struct_offset + 16);
                        }
                        reg_idx += 3;
                    },
                    .stack_i128, .immediate_i128 => {
                        // i128 needs 2 registers (handled by is_i128_arg above, but kept for exhaustiveness)
                        const arg_reg0 = self.getArgumentRegister(reg_idx);
                        const arg_reg1 = self.getArgumentRegister(reg_idx + 1);
                        switch (arg_loc) {
                            .stack_i128 => |offset| {
                                if (comptime builtin.cpu.arch == .aarch64) {
                                    try self.codegen.emit.ldrRegMemSoff(.w64, arg_reg0, .FP, offset);
                                    try self.codegen.emit.ldrRegMemSoff(.w64, arg_reg1, .FP, offset + 8);
                                } else {
                                    try self.codegen.emit.movRegMem(.w64, arg_reg0, .RBP, offset);
                                    try self.codegen.emit.movRegMem(.w64, arg_reg1, .RBP, offset + 8);
                                }
                            },
                            .immediate_i128 => |val| {
                                const low: u64 = @truncate(@as(u128, @bitCast(val)));
                                const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);
                                try self.codegen.emitLoadImm(arg_reg0, @bitCast(low));
                                try self.codegen.emitLoadImm(arg_reg1, @bitCast(high));
                            },
                            else => unreachable,
                        }
                        reg_idx += 2;
                    },
                    .lambda_code => |lc| {
                        // For lambda_code, pass the code offset as a pointer-sized value
                        try self.codegen.emitLoadImm(arg_reg, @bitCast(@as(i64, @intCast(lc.code_offset))));
                        reg_idx += 1;
                    },
                    .closure_value => |cv| {
                        // For closure_value, pass the stack offset pointer
                        try self.codegen.emitLoadStack(.w64, arg_reg, cv.stack_offset);
                        reg_idx += 1;
                    },
                    else => {
                        // For other types (float_reg, immediate_f64), skip
                        reg_idx += 1;
                    },
                }
            }

            // Emit call to the procedure
            try self.emitCallToOffset(code_offset);

            // Handle i128/Dec return values (returned in two registers)
            if (ret_layout == .i128 or ret_layout == .u128 or ret_layout == .dec) {
                const stack_offset = self.codegen.allocStackSlot(16);
                if (comptime builtin.cpu.arch == .aarch64) {
                    try self.codegen.emitStoreStack(.w64, stack_offset, .X0);
                    try self.codegen.emitStoreStack(.w64, stack_offset + 8, .X1);
                } else {
                    try self.codegen.emitStoreStack(.w64, stack_offset, .RAX);
                    try self.codegen.emitStoreStack(.w64, stack_offset + 8, .RDX);
                }
                return .{ .stack_i128 = stack_offset };
            }

            // Check if return type is a string (24 bytes)
            if (ret_layout == .str) {
                // String return (24 bytes) - save X0/X1/X2 to stack
                const stack_offset = self.codegen.allocStackSlot(24);
                if (comptime builtin.cpu.arch == .aarch64) {
                    try self.codegen.emit.strRegMemSoff(.w64, .X0, .FP, stack_offset);
                    try self.codegen.emit.strRegMemSoff(.w64, .X1, .FP, stack_offset + 8);
                    try self.codegen.emit.strRegMemSoff(.w64, .X2, .FP, stack_offset + 16);
                } else {
                    try self.codegen.emit.movMemReg(.w64, .RBP, stack_offset, .RAX);
                    try self.codegen.emit.movMemReg(.w64, .RBP, stack_offset + 8, .RDX);
                    try self.codegen.emit.movMemReg(.w64, .RBP, stack_offset + 16, .RCX);
                }
                return .{ .stack_str = stack_offset };
            }

            // Check if return type is a list (24 bytes)
            const is_list_return = if (self.layout_store) |ls| blk: {
                const layout_val = ls.getLayout(ret_layout);
                break :blk layout_val.tag == .list or layout_val.tag == .list_of_zst;
            } else false;

            if (is_list_return) {
                // List return (24 bytes) - save X0/X1/X2 to stack
                const stack_offset = self.codegen.allocStackSlot(24);
                if (comptime builtin.cpu.arch == .aarch64) {
                    try self.codegen.emit.strRegMemSoff(.w64, .X0, .FP, stack_offset);
                    try self.codegen.emit.strRegMemSoff(.w64, .X1, .FP, stack_offset + 8);
                    try self.codegen.emit.strRegMemSoff(.w64, .X2, .FP, stack_offset + 16);
                } else {
                    try self.codegen.emit.movMemReg(.w64, .RBP, stack_offset, .RAX);
                    try self.codegen.emit.movMemReg(.w64, .RBP, stack_offset + 8, .RDX);
                    try self.codegen.emit.movMemReg(.w64, .RBP, stack_offset + 16, .RCX);
                }
                return .{ .list_stack = .{
                    .struct_offset = stack_offset,
                    .data_offset = 0,
                    .num_elements = 0,
                } };
            }

            // Check if return type is a multi-register struct (record, tag_union, tuple > 8 bytes)
            if (self.layout_store) |ls| {
                const layout_val = ls.getLayout(ret_layout);
                if (layout_val.tag == .record or layout_val.tag == .tag_union or layout_val.tag == .tuple) {
                    const size_align = ls.layoutSizeAlign(layout_val);
                    if (size_align.size > 8) {
                        // Large struct return - save multiple registers to stack
                        const stack_offset = self.codegen.allocStackSlot(size_align.size);
                        const num_regs = (size_align.size + 7) / 8;
                        if (comptime builtin.cpu.arch == .aarch64) {
                            // Save X0, X1, X2, X3... to stack based on size
                            const regs = [_]@TypeOf(GeneralReg.X0){ .X0, .X1, .X2, .X3 };
                            for (0..@min(num_regs, 4)) |i| {
                                try self.codegen.emit.strRegMemSoff(.w64, regs[i], .FP, stack_offset + @as(i32, @intCast(i * 8)));
                            }
                        } else {
                            // x86_64: RAX, RDX, RCX, R8
                            const regs = [_]@TypeOf(GeneralReg.RAX){ .RAX, .RDX, .RCX, .R8 };
                            for (0..@min(num_regs, 4)) |i| {
                                try self.codegen.emit.movMemReg(.w64, .RBP, stack_offset + @as(i32, @intCast(i * 8)), regs[i]);
                            }
                        }
                        return .{ .stack = stack_offset };
                    }
                }
            }

            const ret_reg = self.getReturnRegister();
            return .{ .general_reg = ret_reg };
        }

        /// Fixed stack frame size for procedures (includes space for spills)
        /// Note: On aarch64, stp/ldp use 7-bit signed scaled offsets.
        /// Max frame size is 63 * 8 = 504 bytes. We use 256 bytes for locals
        /// to handle records with multiple fields (16 bytes each) and nested
        /// structures without stack overflow.
        const PROC_STACK_SIZE: i32 = 256;

        /// Emit function prologue (architecture-specific).
        /// Sets up the stack frame for the function, including space for local variables.
        fn emitPrologue(self: *Self) Error!void {
            if (comptime builtin.cpu.arch == .aarch64) {
                // AArch64 prologue:
                // stp x29, x30, [sp, #-(16+STACK_SIZE)]!  ; Save FP/LR and allocate stack
                // mov x29, sp                             ; Set up new frame pointer
                // Total frame = 16 (FP/LR) + PROC_STACK_SIZE (locals)
                // stp offset is in units of 8 bytes (scaled)
                const total_frame = 16 + PROC_STACK_SIZE;
                const scaled_offset: i7 = @intCast(@divExact(-total_frame, 8));
                try self.codegen.emit.stpPreIndex(.w64, .FP, .LR, .ZRSP, scaled_offset);
                try self.codegen.emit.movRegReg(.w64, .FP, .ZRSP);
                // Reset stack_offset to account for the pre-allocated space
                // Stack slots start at FP+16 (above saved FP/LR) and go up
                self.codegen.stack_offset = 16; // First slot at [FP+16]
            } else {
                // x86_64 prologue:
                // push rbp                    ; Save frame pointer
                // mov rbp, rsp                ; Set up new frame pointer
                // sub rsp, PROC_STACK_SIZE   ; Allocate stack space
                try self.codegen.emit.push(.RBP);
                try self.codegen.emit.movRegReg(.w64, .RBP, .RSP);
                try self.codegen.emit.subRegImm32(.w64, .RSP, PROC_STACK_SIZE);
                // Stack slots are at negative offsets from RBP
                self.codegen.stack_offset = 0; // Will go negative
            }
        }

        /// Emit function epilogue (architecture-specific).
        /// Tears down the stack frame and returns.
        fn emitEpilogue(self: *Self) Error!void {
            if (comptime builtin.cpu.arch == .aarch64) {
                // AArch64 epilogue:
                // ldp x29, x30, [sp], #(16+STACK_SIZE)  ; Restore FP/LR and deallocate
                // ret                                   ; Return to caller
                // ldp offset is in units of 8 bytes (scaled)
                const total_frame = 16 + PROC_STACK_SIZE;
                const scaled_offset: i7 = @intCast(@divExact(total_frame, 8));
                try self.codegen.emit.ldpPostIndex(.w64, .FP, .LR, .ZRSP, scaled_offset);
                try self.codegen.emit.ret();
            } else {
                // x86_64 epilogue:
                // mov rsp, rbp                ; Restore stack pointer (deallocates locals)
                // pop rbp                     ; Restore frame pointer
                // ret                          ; Return to caller
                try self.codegen.emit.movRegReg(.w64, .RSP, .RBP);
                try self.codegen.emit.pop(.RBP);
                try self.codegen.emit.ret();
            }
        }

        /// Emit prologue for main expression code.
        /// Sets up frame pointer and saves callee-saved registers.
        /// The frame pointer is REQUIRED because emitStoreStack/emitLoadStack use FP-relative addressing.
        fn emitMainPrologue(self: *Self) Error!void {
            if (comptime builtin.cpu.arch == .aarch64) {
                // First, save FP and LR and establish frame pointer.
                // This is REQUIRED because stack slot accesses use FP-relative addressing.
                // stp x29, x30, [sp, #-16]!  (push FP and LR)
                try self.codegen.emit.stpPreIndex(.w64, .FP, .LR, .ZRSP, -2);
                // mov x29, sp  (establish frame pointer)
                try self.codegen.emit.movRegReg(.w64, .FP, .ZRSP);

                // Now save X19 and X20 (callee-saved) which we use for result ptr and RocOps ptr.
                // stp x19, x20, [sp, #-16]!
                try self.codegen.emit.stpPreIndex(.w64, .X19, .X20, .ZRSP, -2);

                // CRITICAL: Allocate stack space for local variables BEFORE they're used.
                // Without this, stack slots would be below SP and could get corrupted
                // when we call builtin functions. After X19/X20 save, SP = FP - 16.
                // We need enough space for inlined builtins like List.map which can use 400+ bytes.
                const MAIN_STACK_SIZE: u12 = 1024;
                try self.codegen.emit.subRegRegImm12(.w64, .ZRSP, .ZRSP, MAIN_STACK_SIZE);

                // Initialize stack_offset to account for saved X19/X20 at [FP-16].
                // allocStackSlot decrements stack_offset and returns the new value.
                // With stack_offset = -16, first allocation of 16 bytes returns -32,
                // which is below the saved registers and won't corrupt them.
                self.codegen.stack_offset = -16;
            } else {
                // First, set up frame pointer.
                // This is REQUIRED because stack slot accesses use RBP-relative addressing.
                try self.codegen.emit.push(.RBP);
                try self.codegen.emit.movRegReg(.w64, .RBP, .RSP);

                // Save RBX and R12 (callee-saved) which we use for result ptr and RocOps ptr
                try self.codegen.emit.push(.RBX);
                try self.codegen.emit.push(.R12);

                // CRITICAL: Allocate stack space for local variables BEFORE they're used.
                // Without this, stack slots would be in the red zone and get corrupted
                // when we call builtin functions. After the pushes, RSP = RBP - 16.
                // Subtracting MAIN_STACK_SIZE gives RSP = RBP - 16 - MAIN_STACK_SIZE.
                // We need enough space for inlined builtins like List.map which can use 400+ bytes.
                const MAIN_STACK_SIZE: i32 = 1024;
                try self.codegen.emit.subRegImm32(.w64, .RSP, MAIN_STACK_SIZE);

                // Initialize stack_offset to account for saved RBX at [RBP-8]
                // and R12 at [RBP-16]. With stack_offset = -16, first allocation returns -32.
                self.codegen.stack_offset = -16;
            }
        }

        /// Emit epilogue for main expression code.
        /// Restores callee-saved registers and frame pointer, then returns.
        fn emitMainEpilogue(self: *Self) Error!void {
            if (comptime builtin.cpu.arch == .aarch64) {
                // Deallocate local variable stack space (must match MAIN_STACK_SIZE in prologue)
                const MAIN_STACK_SIZE: u12 = 1024;
                try self.codegen.emit.addRegRegImm12(.w64, .ZRSP, .ZRSP, MAIN_STACK_SIZE);
                // Restore X19 and X20
                // ldp x19, x20, [sp], #16
                try self.codegen.emit.ldpPostIndex(.w64, .X19, .X20, .ZRSP, 2);
                // Restore FP and LR
                // ldp x29, x30, [sp], #16
                try self.codegen.emit.ldpPostIndex(.w64, .FP, .LR, .ZRSP, 2);
                try self.codegen.emit.ret();
            } else {
                // Deallocate local variable stack space (must match MAIN_STACK_SIZE in prologue)
                const MAIN_STACK_SIZE: i32 = 1024;
                try self.codegen.emit.addRegImm32(.w64, .RSP, MAIN_STACK_SIZE);
                // Restore R12 and RBX (in reverse order of push)
                try self.codegen.emit.pop(.R12);
                try self.codegen.emit.pop(.RBX);
                // Restore frame pointer
                try self.codegen.emit.pop(.RBP);
                try self.codegen.emit.ret();
            }
        }

        /// Bind procedure parameters to argument registers
        fn bindProcParams(self: *Self, params: mono.MonoPatternSpan, param_layouts: LayoutIdxSpan) Error!void {
            const pattern_ids = self.store.getPatternSpan(params);
            const layouts = self.store.getLayoutIdxSpan(param_layouts);

            // Track current register index separately from parameter index
            // because 128-bit parameters consume 2 registers
            var reg_idx: u8 = 0;

            for (pattern_ids, 0..) |pattern_id, param_idx| {
                const pattern = self.store.getPattern(pattern_id);
                switch (pattern) {
                    .bind => |bind| {
                        const symbol_key: u48 = @bitCast(bind.symbol);

                        // Check if this parameter is a 128-bit type
                        const is_128bit = if (param_idx < layouts.len) blk: {
                            const param_layout = layouts[param_idx];
                            break :blk param_layout == .i128 or param_layout == .u128 or param_layout == .dec;
                        } else false;

                        if (is_128bit) {
                            // 128-bit types need to be spilled to stack
                            // They arrive in two consecutive registers (e.g., X0+X1)
                            if (comptime builtin.cpu.arch == .aarch64) {
                                // For aarch64, 128-bit values must be 16-byte aligned
                                // and use an even-odd register pair
                                if (reg_idx % 2 != 0) {
                                    reg_idx += 1; // Skip to even register
                                }

                                const low_reg = self.getArgumentRegister(reg_idx);
                                const high_reg = self.getArgumentRegister(reg_idx + 1);

                                // Allocate 16-byte stack slot
                                const stack_offset = self.codegen.allocStack(16);

                                // Store both registers to stack
                                try self.codegen.emitStoreStack(.w64, stack_offset, low_reg);
                                try self.codegen.emitStoreStack(.w64, stack_offset + 8, high_reg);

                                // Track as stack_i128
                                try self.symbol_locations.put(symbol_key, .{ .stack_i128 = stack_offset });

                                // Mark both registers as NOT in use since we spilled them
                                // (Actually, don't mark them in use since we moved to stack)
                                reg_idx += 2;
                            } else {
                                // x86_64: 128-bit values are typically passed on stack or in RDI+RSI
                                // For now, handle like aarch64
                                const low_reg = self.getArgumentRegister(reg_idx);
                                const high_reg = self.getArgumentRegister(reg_idx + 1);

                                const stack_offset = self.codegen.allocStack(16);
                                try self.codegen.emitStoreStack(.w64, stack_offset, low_reg);
                                try self.codegen.emitStoreStack(.w64, stack_offset + 8, high_reg);

                                try self.symbol_locations.put(symbol_key, .{ .stack_i128 = stack_offset });
                                reg_idx += 2;
                            }
                        } else {
                            // Check if this is a list type (24 bytes)
                            const is_list = if (param_idx < layouts.len) blk: {
                                const param_layout = layouts[param_idx];
                                if (self.layout_store) |ls| {
                                    // Bounds check for cross-module layouts
                                    if (@intFromEnum(param_layout) >= ls.layouts.len()) {
                                        break :blk false;
                                    }
                                    const layout_val = ls.getLayout(param_layout);
                                    break :blk layout_val.tag == .list or layout_val.tag == .list_of_zst;
                                }
                                break :blk false;
                            } else false;

                            if (is_list) {
                                // List types arrive in 3 consecutive registers (X0+X1+X2)
                                // Spill to 24-byte stack slot
                                const stack_offset = self.codegen.allocStackSlot(24);

                                if (comptime builtin.cpu.arch == .aarch64) {
                                    const reg0 = self.getArgumentRegister(reg_idx);
                                    const reg1 = self.getArgumentRegister(reg_idx + 1);
                                    const reg2 = self.getArgumentRegister(reg_idx + 2);

                                    try self.codegen.emit.strRegMemSoff(.w64, reg0, .FP, stack_offset);
                                    try self.codegen.emit.strRegMemSoff(.w64, reg1, .FP, stack_offset + 8);
                                    try self.codegen.emit.strRegMemSoff(.w64, reg2, .FP, stack_offset + 16);
                                } else {
                                    const reg0 = self.getArgumentRegister(reg_idx);
                                    const reg1 = self.getArgumentRegister(reg_idx + 1);
                                    const reg2 = self.getArgumentRegister(reg_idx + 2);

                                    try self.codegen.emit.movMemReg(.w64, .RBP, stack_offset, reg0);
                                    try self.codegen.emit.movMemReg(.w64, .RBP, stack_offset + 8, reg1);
                                    try self.codegen.emit.movMemReg(.w64, .RBP, stack_offset + 16, reg2);
                                }

                                // Store as .list_stack so that when this parameter is used as an argument
                                // or returned, it's properly detected as a list
                                try self.symbol_locations.put(symbol_key, .{ .list_stack = .{
                                    .struct_offset = stack_offset,
                                    .data_offset = 0, // Data location is stored in the list struct itself
                                    .num_elements = 0, // Unknown at compile time
                                } });
                                reg_idx += 3;
                            } else {
                                // Normal 64-bit or smaller parameter — spill to stack
                                const arg_reg = self.getArgumentRegister(reg_idx);
                                const stack_offset = self.codegen.allocStackSlot(8);
                                try self.codegen.emitStoreStack(.w64, stack_offset, arg_reg);
                                try self.symbol_locations.put(symbol_key, .{ .stack = stack_offset });
                                reg_idx += 1;
                            }
                        }
                    },
                    else => {
                        // Complex parameter patterns not yet supported
                    },
                }
            }
        }

        /// Generate code for a control flow statement
        fn generateStmt(self: *Self, stmt_id: CFStmtId) Error!void {
            const stmt = self.store.getCFStmt(stmt_id);

            switch (stmt) {
                .let_stmt => |let_s| {
                    // Evaluate the value
                    const value_loc = try self.generateExpr(let_s.value);
                    // Bind to pattern
                    try self.bindPattern(let_s.pattern, value_loc);
                    // Continue with next statement
                    try self.generateStmt(let_s.next);
                },

                .join => |j| {
                    // Store param layouts and patterns for this join point (needed by rebindJoinPointParams)
                    const jp_key = @intFromEnum(j.id);
                    try self.join_point_param_layouts.put(jp_key, j.param_layouts);
                    try self.join_point_param_patterns.put(jp_key, j.params);

                    // Set up storage for join point parameters (they'll be rebound on each jump)
                    try self.setupJoinPointParams(j.id, j.params, j.param_layouts);
                    if (!self.join_point_jumps.contains(jp_key)) {
                        try self.join_point_jumps.put(jp_key, std.ArrayList(JumpRecord).empty);
                    }

                    // Generate REMAINDER first (code that eventually jumps TO the join point)
                    try self.generateStmt(j.remainder);

                    // Record where join point body starts (this is where jumps will target)
                    const join_location = self.codegen.currentOffset();
                    try self.join_points.put(jp_key, join_location);

                    // Generate BODY (what happens when jumped to)
                    try self.generateStmt(j.body);

                    // Patch all jumps to this join point
                    if (self.join_point_jumps.get(jp_key)) |jumps| {
                        for (jumps.items) |jump_record| {
                            self.codegen.patchJump(jump_record.location, join_location);
                        }
                    }
                },

                .jump => |jmp| {
                    // Evaluate all arguments first (before rebinding, in case args reference params)
                    const args = self.store.getExprSpan(jmp.args);
                    var arg_locs: std.ArrayListUnmanaged(ValueLocation) = .empty;
                    defer arg_locs.deinit(self.allocator);

                    for (args) |arg_id| {
                        const loc = try self.generateExpr(arg_id);
                        try arg_locs.append(self.allocator, loc);
                    }

                    // Rebind join point parameters to new argument values
                    try self.rebindJoinPointParams(jmp.target, arg_locs.items);

                    // Emit jump instruction with placeholder offset
                    const jump_location = self.codegen.currentOffset();
                    try self.emitJumpPlaceholder();

                    // Record for patching
                    const jp_key = @intFromEnum(jmp.target);
                    if (self.join_point_jumps.getPtr(jp_key)) |jumps| {
                        try jumps.append(self.allocator, .{ .location = jump_location });
                    }
                },

                .ret => |r| {
                    // Evaluate the return value
                    const value_loc = try self.generateExpr(r.value);


                    // Handle i128/Dec return values specially (need two registers)
                    if (value_loc == .stack_i128 or value_loc == .immediate_i128) {
                        if (comptime builtin.cpu.arch == .aarch64) {
                            // aarch64: return i128 in X0 (low), X1 (high)
                            switch (value_loc) {
                                .stack_i128 => |offset| {
                                    try self.codegen.emitLoadStack(.w64, .X0, offset);
                                    try self.codegen.emitLoadStack(.w64, .X1, offset + 8);
                                },
                                .immediate_i128 => |val| {
                                    const low: u64 = @truncate(@as(u128, @bitCast(val)));
                                    const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);
                                    try self.codegen.emitLoadImm(.X0, @bitCast(low));
                                    try self.codegen.emitLoadImm(.X1, @bitCast(high));
                                },
                                else => unreachable,
                            }
                        } else {
                            // x86_64: return i128 in RAX (low), RDX (high)
                            switch (value_loc) {
                                .stack_i128 => |offset| {
                                    try self.codegen.emitLoadStack(.w64, .RAX, offset);
                                    try self.codegen.emitLoadStack(.w64, .RDX, offset + 8);
                                },
                                .immediate_i128 => |val| {
                                    const low: u64 = @truncate(@as(u128, @bitCast(val)));
                                    const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);
                                    try self.codegen.emitLoadImm(.RAX, @bitCast(low));
                                    try self.codegen.emitLoadImm(.RDX, @bitCast(high));
                                },
                                else => unreachable,
                            }
                        }
                    } else if (value_loc == .list_stack) {
                        // List return (24 bytes) - return in X0, X1, X2 (aarch64) or RAX, RDX, RCX (x86_64)
                        const offset = value_loc.list_stack.struct_offset;
                        if (comptime builtin.cpu.arch == .aarch64) {
                            try self.codegen.emitLoadStack(.w64, .X0, offset);
                            try self.codegen.emitLoadStack(.w64, .X1, offset + 8);
                            try self.codegen.emitLoadStack(.w64, .X2, offset + 16);
                        } else {
                            try self.codegen.emitLoadStack(.w64, .RAX, offset);
                            try self.codegen.emitLoadStack(.w64, .RDX, offset + 8);
                            try self.codegen.emitLoadStack(.w64, .RCX, offset + 16);
                        }
                    } else if (value_loc == .stack) {
                        // Check expression layout for multi-register returns
                        var is_list = false;
                        var is_i128 = false;
                        var is_large_record = false;
                        var record_size: u32 = 0;
                        const expr_layout_opt = self.getExprLayout(r.value);
                        if (self.layout_store) |ls| {
                            if (expr_layout_opt) |ret_layout| {
                                const layout_val = ls.getLayout(ret_layout);
                                is_list = layout_val.tag == .list or layout_val.tag == .list_of_zst;
                                is_i128 = ret_layout == .i128 or ret_layout == .u128 or ret_layout == .dec;
                                if (layout_val.tag == .record or layout_val.tag == .tag_union or layout_val.tag == .tuple) {
                                    const sa = ls.layoutSizeAlign(layout_val);
                                    if (sa.size > 8) {
                                        is_large_record = true;
                                        record_size = sa.size;
                                    }
                                }
                            }
                        }

                        if (is_list) {
                            // List return (24 bytes) from .stack location
                            const offset = value_loc.stack;
                            if (comptime builtin.cpu.arch == .aarch64) {
                                try self.codegen.emitLoadStack(.w64, .X0, offset);
                                try self.codegen.emitLoadStack(.w64, .X1, offset + 8);
                                try self.codegen.emitLoadStack(.w64, .X2, offset + 16);
                            } else {
                                try self.codegen.emitLoadStack(.w64, .RAX, offset);
                                try self.codegen.emitLoadStack(.w64, .RDX, offset + 8);
                                try self.codegen.emitLoadStack(.w64, .RCX, offset + 16);
                            }
                        } else if (is_i128) {
                            // i128/Dec return (16 bytes) from .stack location
                            const offset = value_loc.stack;
                            if (comptime builtin.cpu.arch == .aarch64) {
                                try self.codegen.emitLoadStack(.w64, .X0, offset);
                                try self.codegen.emitLoadStack(.w64, .X1, offset + 8);
                            } else {
                                try self.codegen.emitLoadStack(.w64, .RAX, offset);
                                try self.codegen.emitLoadStack(.w64, .RDX, offset + 8);
                            }
                        } else if (is_large_record) {
                            // Large record return - load into multiple registers
                            const offset = value_loc.stack;
                            const num_regs = (record_size + 7) / 8;
                            if (comptime builtin.cpu.arch == .aarch64) {
                                const regs = [_]@TypeOf(GeneralReg.X0){ .X0, .X1, .X2, .X3 };
                                for (0..@min(num_regs, 4)) |i| {
                                    try self.codegen.emitLoadStack(.w64, regs[i], offset + @as(i32, @intCast(i * 8)));
                                }
                            } else {
                                const regs = [_]@TypeOf(GeneralReg.RAX){ .RAX, .RDX, .RCX, .R8 };
                                for (0..@min(num_regs, 4)) |i| {
                                    try self.codegen.emitLoadStack(.w64, regs[i], offset + @as(i32, @intCast(i * 8)));
                                }
                            }
                        } else {
                            // Normal 64-bit return from stack
                            const return_reg = self.getReturnRegister();
                            const value_reg = try self.ensureInGeneralReg(value_loc);
                            if (value_reg != return_reg) {
                                try self.emitMovRegReg(return_reg, value_reg);
                            }
                        }
                    } else {
                        // Move to return register (64-bit values)
                        const return_reg = self.getReturnRegister();
                        const value_reg = try self.ensureInGeneralReg(value_loc);
                        if (value_reg != return_reg) {
                            try self.emitMovRegReg(return_reg, value_reg);
                        }
                    }
                    // Emit epilogue (restores frame and returns)
                    try self.emitEpilogue();
                },

                .expr_stmt => |e| {
                    // Evaluate expression for side effects
                    _ = try self.generateExpr(e.value);
                    // Continue with next
                    try self.generateStmt(e.next);
                },

                .switch_stmt => |sw| {
                    try self.generateSwitchStmt(sw);
                },
            }
        }

        /// Set up storage locations for join point parameters
        fn setupJoinPointParams(self: *Self, _: JoinPointId, params: mono.MonoPatternSpan, param_layouts: LayoutIdxSpan) Error!void {
            const pattern_ids = self.store.getPatternSpan(params);
            const layouts = self.store.getLayoutIdxSpan(param_layouts);

            var reg_idx: u8 = 0;

            // For each parameter, allocate a register or stack slot
            for (pattern_ids, 0..) |pattern_id, param_idx| {
                const pattern = self.store.getPattern(pattern_id);
                switch (pattern) {
                    .bind => |bind| {
                        const symbol_key: u48 = @bitCast(bind.symbol);

                        // Check if this parameter is a 128-bit type
                        const is_128bit = if (param_idx < layouts.len) blk: {
                            const param_layout = layouts[param_idx];
                            break :blk param_layout == .i128 or param_layout == .u128 or param_layout == .dec;
                        } else false;

                        if (is_128bit) {
                            // 128-bit types need two consecutive registers
                            const low_reg = self.getArgumentRegister(reg_idx);
                            const high_reg = self.getArgumentRegister(reg_idx + 1);

                            // Allocate 16-byte stack slot
                            const stack_offset = self.codegen.allocStack(16);

                            // Store both registers to stack
                            try self.codegen.emitStoreStack(.w64, stack_offset, low_reg);
                            try self.codegen.emitStoreStack(.w64, stack_offset + 8, high_reg);

                            // Track as stack_i128
                            try self.symbol_locations.put(symbol_key, .{ .stack_i128 = stack_offset });
                            reg_idx += 2;
                        } else {
                            // Check if this is a list type (24 bytes)
                            const is_list = if (param_idx < layouts.len) blk: {
                                const param_layout = layouts[param_idx];
                                if (self.layout_store) |ls| {
                                    // Bounds check for cross-module layouts
                                    if (@intFromEnum(param_layout) >= ls.layouts.len()) {
                                        break :blk false;
                                    }
                                    const layout_val = ls.getLayout(param_layout);
                                    break :blk layout_val.tag == .list or layout_val.tag == .list_of_zst;
                                }
                                break :blk false;
                            } else false;

                            if (is_list) {
                                // List types need 3 consecutive registers
                                const stack_offset = self.codegen.allocStackSlot(24);

                                if (comptime builtin.cpu.arch == .aarch64) {
                                    const reg0 = self.getArgumentRegister(reg_idx);
                                    const reg1 = self.getArgumentRegister(reg_idx + 1);
                                    const reg2 = self.getArgumentRegister(reg_idx + 2);

                                    try self.codegen.emit.strRegMemSoff(.w64, reg0, .FP, stack_offset);
                                    try self.codegen.emit.strRegMemSoff(.w64, reg1, .FP, stack_offset + 8);
                                    try self.codegen.emit.strRegMemSoff(.w64, reg2, .FP, stack_offset + 16);
                                } else {
                                    const reg0 = self.getArgumentRegister(reg_idx);
                                    const reg1 = self.getArgumentRegister(reg_idx + 1);
                                    const reg2 = self.getArgumentRegister(reg_idx + 2);

                                    try self.codegen.emit.movMemReg(.w64, .RBP, stack_offset, reg0);
                                    try self.codegen.emit.movMemReg(.w64, .RBP, stack_offset + 8, reg1);
                                    try self.codegen.emit.movMemReg(.w64, .RBP, stack_offset + 16, reg2);
                                }

                                // Store as .list_stack so that when this parameter is used as an argument
                                // or returned, it's properly detected as a list
                                try self.symbol_locations.put(symbol_key, .{ .list_stack = .{
                                    .struct_offset = stack_offset,
                                    .data_offset = 0, // Data location is stored in the list struct itself
                                    .num_elements = 0, // Unknown at compile time
                                } });
                                reg_idx += 3;
                            } else {
                                // Normal 64-bit or smaller parameter — spill to stack
                                const arg_reg = self.getArgumentRegister(reg_idx);
                                const stack_offset = self.codegen.allocStackSlot(8);
                                try self.codegen.emitStoreStack(.w64, stack_offset, arg_reg);
                                try self.symbol_locations.put(symbol_key, .{ .stack = stack_offset });
                                reg_idx += 1;
                            }
                        }
                    },
                    else => unreachable, // Join point params must be simple bindings
                }
            }
        }

        /// Rebind join point parameters to new argument values (for jump)
        /// This writes the new values directly to the stack slots used by symbol_locations,
        /// so that the join point body can read the updated values.
        fn rebindJoinPointParams(self: *Self, target: JoinPointId, arg_locs: []const ValueLocation) Error!void {
            const jp_key = @intFromEnum(target);
            const param_layouts_span = self.join_point_param_layouts.get(jp_key) orelse unreachable;
            const param_patterns_span = self.join_point_param_patterns.get(jp_key) orelse unreachable;
            const layouts = self.store.getLayoutIdxSpan(param_layouts_span);
            const pattern_ids = self.store.getPatternSpan(param_patterns_span);

            // Copy new argument values to the stack slots used by symbol_locations
            for (arg_locs, 0..) |loc, param_idx| {
                if (param_idx >= pattern_ids.len) continue;

                const pattern = self.store.getPattern(pattern_ids[param_idx]);
                const symbol_key: u48 = switch (pattern) {
                    .bind => |bind| @bitCast(bind.symbol),
                    else => continue, // Skip non-bind patterns
                };

                // Get the destination location (where the join point body will read from)
                const dst_loc = self.symbol_locations.get(symbol_key) orelse continue;

                // Determine if this is a list type
                const is_list = if (param_idx < layouts.len) blk: {
                    const param_layout = layouts[param_idx];
                    if (self.layout_store) |ls| {
                        if (@intFromEnum(param_layout) >= ls.layouts.len()) {
                            break :blk (loc == .list_stack or dst_loc == .list_stack);
                        }
                        const layout_val = ls.getLayout(param_layout);
                        break :blk layout_val.tag == .list or layout_val.tag == .list_of_zst;
                    }
                    break :blk (loc == .list_stack or dst_loc == .list_stack);
                } else (loc == .list_stack or dst_loc == .list_stack);

                // Get the destination stack offset
                // All join point parameters should be on the stack (set up in setupJoinPointParams)
                const dst_offset: i32 = switch (dst_loc) {
                    .stack => |off| off,
                    .list_stack => |ls_info| ls_info.struct_offset,
                    .stack_i128 => |off| off,
                    else => unreachable, // Join point params must be on stack
                };

                // Copy the value to the destination
                if (is_list) {
                    // Copy 24 bytes (list struct)
                    const src_offset: i32 = switch (loc) {
                        .stack => |off| off,
                        .list_stack => |ls_info| ls_info.struct_offset,
                        else => unreachable, // Lists must always be on the stack
                    };

                    // Skip copy if source and destination are the same
                    if (src_offset == dst_offset) continue;

                    // Copy from src stack to dst stack (24 bytes)
                    const temp_reg = try self.allocTempGeneral();
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.ldrRegMemSoff(.w64, temp_reg, .FP, src_offset);
                        try self.codegen.emit.strRegMemSoff(.w64, temp_reg, .FP, dst_offset);
                        try self.codegen.emit.ldrRegMemSoff(.w64, temp_reg, .FP, src_offset + 8);
                        try self.codegen.emit.strRegMemSoff(.w64, temp_reg, .FP, dst_offset + 8);
                        try self.codegen.emit.ldrRegMemSoff(.w64, temp_reg, .FP, src_offset + 16);
                        try self.codegen.emit.strRegMemSoff(.w64, temp_reg, .FP, dst_offset + 16);
                    } else {
                        try self.codegen.emit.movRegMem(.w64, temp_reg, .RBP, src_offset);
                        try self.codegen.emit.movMemReg(.w64, .RBP, dst_offset, temp_reg);
                        try self.codegen.emit.movRegMem(.w64, temp_reg, .RBP, src_offset + 8);
                        try self.codegen.emit.movMemReg(.w64, .RBP, dst_offset + 8, temp_reg);
                        try self.codegen.emit.movRegMem(.w64, temp_reg, .RBP, src_offset + 16);
                        try self.codegen.emit.movMemReg(.w64, .RBP, dst_offset + 16, temp_reg);
                    }
                    self.codegen.freeGeneral(temp_reg);
                } else if (dst_loc == .stack_i128) {
                    // Copy 16 bytes (i128)
                    switch (loc) {
                        .stack_i128 => |src_offset| {
                            const temp_reg = try self.allocTempGeneral();
                            if (comptime builtin.cpu.arch == .aarch64) {
                                try self.codegen.emit.ldrRegMemSoff(.w64, temp_reg, .FP, src_offset);
                                try self.codegen.emit.strRegMemSoff(.w64, temp_reg, .FP, dst_offset);
                                try self.codegen.emit.ldrRegMemSoff(.w64, temp_reg, .FP, src_offset + 8);
                                try self.codegen.emit.strRegMemSoff(.w64, temp_reg, .FP, dst_offset + 8);
                            } else {
                                try self.codegen.emit.movRegMem(.w64, temp_reg, .RBP, src_offset);
                                try self.codegen.emit.movMemReg(.w64, .RBP, dst_offset, temp_reg);
                                try self.codegen.emit.movRegMem(.w64, temp_reg, .RBP, src_offset + 8);
                                try self.codegen.emit.movMemReg(.w64, .RBP, dst_offset + 8, temp_reg);
                            }
                            self.codegen.freeGeneral(temp_reg);
                        },
                        .immediate_i128 => |val| {
                            const low: u64 = @truncate(@as(u128, @bitCast(val)));
                            const high: u64 = @truncate(@as(u128, @bitCast(val)) >> 64);
                            const temp_reg = try self.allocTempGeneral();
                            try self.codegen.emitLoadImm(temp_reg, @bitCast(low));
                            try self.codegen.emitStoreStack(.w64, dst_offset, temp_reg);
                            try self.codegen.emitLoadImm(temp_reg, @bitCast(high));
                            try self.codegen.emitStoreStack(.w64, dst_offset + 8, temp_reg);
                            self.codegen.freeGeneral(temp_reg);
                        },
                        else => unreachable, // i128 values must be in stack_i128 or immediate_i128
                    }
                } else {
                    // Copy 8 bytes (normal value)
                    const src_reg = try self.ensureInGeneralReg(loc);
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.strRegMemSoff(.w64, src_reg, .FP, dst_offset);
                    } else {
                        try self.codegen.emit.movMemReg(.w64, .RBP, dst_offset, src_reg);
                    }
                }
            }
        }

        /// Emit a jump placeholder (will be patched later)
        fn emitJumpPlaceholder(self: *Self) Error!void {
            if (comptime builtin.cpu.arch == .aarch64) {
                // B instruction with offset 0 (will be patched)
                try self.codegen.emit.b(0);
            } else {
                // JMP rel32 with offset 0 (will be patched)
                try self.codegen.emit.jmp(0);
            }
        }

        /// Generate code for a switch statement
        fn generateSwitchStmt(self: *Self, sw: anytype) Error!void {
            // Evaluate condition
            const cond_loc = try self.generateExpr(sw.cond);
            const cond_reg = try self.ensureInGeneralReg(cond_loc);

            const branches = self.store.getCFSwitchBranches(sw.branches);

            // For single branch (bool switch): compare and branch
            if (branches.len == 1) {
                const branch = branches[0];

                // Compare with branch value and jump if NOT equal (to default)
                if (comptime builtin.cpu.arch == .aarch64) {
                    try self.codegen.emit.cmpRegImm12(.w64, cond_reg, @intCast(branch.value));
                } else {
                    try self.codegen.emit.cmpRegImm32(.w64, cond_reg, @intCast(branch.value));
                }

                // Jump to default if not equal
                const else_patch = try self.emitJumpIfNotEqual();

                self.codegen.freeGeneral(cond_reg);

                // Generate branch body (recursively generates statements)
                try self.generateStmt(branch.body);

                // Patch else jump to here
                const else_offset = self.codegen.currentOffset();
                self.codegen.patchJump(else_patch, else_offset);

                // Generate default branch
                try self.generateStmt(sw.default_branch);
            } else {
                // Multiple branches - generate cascading comparisons
                var end_patches = std.ArrayList(usize).empty;
                defer end_patches.deinit(self.allocator);

                for (branches, 0..) |branch, i| {
                    if (i < branches.len - 1) {
                        // Compare and skip if not match
                        try self.emitCmpImm(cond_reg, @intCast(branch.value));
                        const skip_patch = try self.emitJumpIfNotEqual();

                        // Generate branch body
                        try self.generateStmt(branch.body);

                        // Jump to end
                        const end_patch = try self.codegen.emitJump();
                        try end_patches.append(self.allocator, end_patch);

                        // Patch skip
                        const skip_offset = self.codegen.currentOffset();
                        self.codegen.patchJump(skip_patch, skip_offset);
                    } else {
                        // Last branch before default
                        try self.emitCmpImm(cond_reg, @intCast(branch.value));
                        const skip_patch = try self.emitJumpIfNotEqual();

                        try self.generateStmt(branch.body);

                        const end_patch = try self.codegen.emitJump();
                        try end_patches.append(self.allocator, end_patch);

                        self.codegen.patchJump(skip_patch, self.codegen.currentOffset());
                    }
                }

                self.codegen.freeGeneral(cond_reg);

                // Generate default branch
                try self.generateStmt(sw.default_branch);

                // Patch all end jumps
                const end_offset = self.codegen.currentOffset();
                for (end_patches.items) |patch| {
                    self.codegen.patchJump(patch, end_offset);
                }
            }
        }

        /// Patch all pending calls after all procedures are compiled
        /// Generate code for incref operation
        /// Increments the reference count of a heap-allocated value
        fn generateIncref(self: *Self, rc_op: anytype) Error!ValueLocation {
            // First generate the value expression
            const value_loc = try self.generateExpr(rc_op.value);

            // Check if we have a layout store to determine the type
            const ls = self.layout_store orelse return value_loc;

            // Get the layout to check if it's a heap-allocated type
            const layout_val = ls.getLayout(rc_op.layout_idx);

            // Only incref heap-allocated types: list, str (large), box
            switch (layout_val.tag) {
                .list, .list_of_zst => {
                    // Lists always have heap-allocated data
                    try self.emitListIncref(value_loc, rc_op.count);
                },
                .scalar => {
                    // Check if it's a string
                    if (layout_val.data.scalar.tag == .str) {
                        // Strings use SSO - only incref if large string
                        try self.emitStrIncref(value_loc, rc_op.count);
                    }
                    // Other scalars don't need incref
                },
                .box, .box_of_zst => {
                    // Boxes are always heap-allocated
                    try self.emitBoxIncref(value_loc, rc_op.count);
                },
                else => {
                    // Records, tuples, tag unions, closures, zst don't need RC at the top level
                    // (their heap-allocated fields are handled separately)
                },
            }

            return value_loc;
        }

        /// Generate code for decref operation
        /// Decrements the reference count and frees if it reaches zero
        fn generateDecref(self: *Self, rc_op: anytype) Error!ValueLocation {
            // First generate the value expression
            const value_loc = try self.generateExpr(rc_op.value);

            // Check if we have a layout store to determine the type
            const ls = self.layout_store orelse return value_loc;

            // Get the layout to check if it's a heap-allocated type
            const layout_val = ls.getLayout(rc_op.layout_idx);

            // Only decref heap-allocated types: list, str (large), box
            switch (layout_val.tag) {
                .list, .list_of_zst => {
                    // Lists always have heap-allocated data
                    try self.emitListDecref(value_loc);
                },
                .scalar => {
                    // Check if it's a string
                    if (layout_val.data.scalar.tag == .str) {
                        // Strings use SSO - only decref if large string
                        try self.emitStrDecref(value_loc);
                    }
                    // Other scalars don't need decref
                },
                .box, .box_of_zst => {
                    // Boxes are always heap-allocated
                    try self.emitBoxDecref(value_loc);
                },
                else => {
                    // Records, tuples, tag unions, closures, zst don't need RC at the top level
                },
            }

            return value_loc;
        }

        /// Generate code for free operation
        /// Directly frees memory without checking refcount
        fn generateFree(self: *Self, rc_op: anytype) Error!ValueLocation {
            // First generate the value expression
            const value_loc = try self.generateExpr(rc_op.value);

            // Check if we have a layout store to determine the type
            const ls = self.layout_store orelse return value_loc;

            // Get the layout to check if it's a heap-allocated type
            const layout_val = ls.getLayout(rc_op.layout_idx);

            // Only free heap-allocated types: list, str (large), box
            switch (layout_val.tag) {
                .list, .list_of_zst => {
                    try self.emitListFree(value_loc);
                },
                .scalar => {
                    if (layout_val.data.scalar.tag == .str) {
                        try self.emitStrFree(value_loc);
                    }
                },
                .box, .box_of_zst => {
                    try self.emitBoxFree(value_loc);
                },
                else => {},
            }

            return value_loc;
        }

        /// Emit incref for a list value
        fn emitListIncref(self: *Self, value_loc: ValueLocation, count: u16) Error!void {
            const roc_ops_reg = self.roc_ops_reg orelse return;
            const fn_addr: usize = @intFromPtr(&increfDataPtrC);

            // Get the data pointer from the list struct (offset 0)
            const ptr_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(ptr_reg);

            switch (value_loc) {
                .stack => |offset| {
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.ldrRegMemSoff(.w64, ptr_reg, .FP, offset);
                    } else {
                        try self.codegen.emit.movRegMem(.w64, ptr_reg, .RBP, offset);
                    }
                },
                .list_stack => |info| {
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.ldrRegMemSoff(.w64, ptr_reg, .FP, info.struct_offset);
                    } else {
                        try self.codegen.emit.movRegMem(.w64, ptr_reg, .RBP, info.struct_offset);
                    }
                },
                else => return, // Can't incref non-stack values
            }

            // Call increfDataPtrC(ptr, count, roc_ops)
            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.movRegReg(.w64, .X0, ptr_reg);
                try self.codegen.emit.movRegImm64(.X1, @intCast(count));
                try self.codegen.emit.movRegReg(.w64, .X2, roc_ops_reg);

                const addr_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(addr_reg, @intCast(fn_addr));
                try self.codegen.emit.blrReg(addr_reg);
                self.codegen.freeGeneral(addr_reg);
            } else {
                try self.codegen.emit.movRegImm64(.R11, @intCast(fn_addr));
                try self.codegen.emit.movRegReg(.w64, .RDI, ptr_reg);
                try self.codegen.emit.movRegImm64(.RSI, @intCast(count));
                try self.codegen.emit.movRegReg(.w64, .RDX, roc_ops_reg);
                try self.codegen.emit.callReg(.R11);
            }
        }

        /// Emit decref for a list value
        fn emitListDecref(self: *Self, value_loc: ValueLocation) Error!void {
            const roc_ops_reg = self.roc_ops_reg orelse return;
            const fn_addr: usize = @intFromPtr(&decrefDataPtrC);

            // Get the data pointer from the list struct (offset 0)
            const ptr_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(ptr_reg);

            switch (value_loc) {
                .stack => |offset| {
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.ldrRegMemSoff(.w64, ptr_reg, .FP, offset);
                    } else {
                        try self.codegen.emit.movRegMem(.w64, ptr_reg, .RBP, offset);
                    }
                },
                .list_stack => |info| {
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.ldrRegMemSoff(.w64, ptr_reg, .FP, info.struct_offset);
                    } else {
                        try self.codegen.emit.movRegMem(.w64, ptr_reg, .RBP, info.struct_offset);
                    }
                },
                else => return,
            }

            // Call decrefDataPtrC(ptr, alignment, elements_refcounted, roc_ops)
            // Lists have 8-byte alignment by default
            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.movRegReg(.w64, .X0, ptr_reg);
                try self.codegen.emit.movRegImm64(.X1, 8); // alignment
                try self.codegen.emit.movRegImm64(.X2, 0); // elements_refcounted = false
                try self.codegen.emit.movRegReg(.w64, .X3, roc_ops_reg);

                const addr_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(addr_reg, @intCast(fn_addr));
                try self.codegen.emit.blrReg(addr_reg);
                self.codegen.freeGeneral(addr_reg);
            } else {
                try self.codegen.emit.movRegImm64(.R11, @intCast(fn_addr));
                try self.codegen.emit.movRegReg(.w64, .RDI, ptr_reg);
                try self.codegen.emit.movRegImm64(.RSI, 8); // alignment
                try self.codegen.emit.movRegImm64(.RDX, 0); // elements_refcounted
                try self.codegen.emit.movRegReg(.w64, .RCX, roc_ops_reg);
                try self.codegen.emit.callReg(.R11);
            }
        }

        /// Emit free for a list value
        fn emitListFree(self: *Self, value_loc: ValueLocation) Error!void {
            const roc_ops_reg = self.roc_ops_reg orelse return;
            const fn_addr: usize = @intFromPtr(&freeDataPtrC);

            const ptr_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(ptr_reg);

            switch (value_loc) {
                .stack => |offset| {
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.ldrRegMemSoff(.w64, ptr_reg, .FP, offset);
                    } else {
                        try self.codegen.emit.movRegMem(.w64, ptr_reg, .RBP, offset);
                    }
                },
                .list_stack => |info| {
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.ldrRegMemSoff(.w64, ptr_reg, .FP, info.struct_offset);
                    } else {
                        try self.codegen.emit.movRegMem(.w64, ptr_reg, .RBP, info.struct_offset);
                    }
                },
                else => return,
            }

            // Call freeDataPtrC(ptr, alignment, elements_refcounted, roc_ops)
            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.movRegReg(.w64, .X0, ptr_reg);
                try self.codegen.emit.movRegImm64(.X1, 8);
                try self.codegen.emit.movRegImm64(.X2, 0);
                try self.codegen.emit.movRegReg(.w64, .X3, roc_ops_reg);

                const addr_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(addr_reg, @intCast(fn_addr));
                try self.codegen.emit.blrReg(addr_reg);
                self.codegen.freeGeneral(addr_reg);
            } else {
                try self.codegen.emit.movRegImm64(.R11, @intCast(fn_addr));
                try self.codegen.emit.movRegReg(.w64, .RDI, ptr_reg);
                try self.codegen.emit.movRegImm64(.RSI, 8);
                try self.codegen.emit.movRegImm64(.RDX, 0);
                try self.codegen.emit.movRegReg(.w64, .RCX, roc_ops_reg);
                try self.codegen.emit.callReg(.R11);
            }
        }

        /// Emit incref for a string value
        /// Strings use SSO, so we need to check if it's a large string first
        fn emitStrIncref(self: *Self, value_loc: ValueLocation, count: u16) Error!void {
            const roc_ops_reg = self.roc_ops_reg orelse return;
            const fn_addr: usize = @intFromPtr(&increfDataPtrC);

            // String struct: bytes (offset 0), length (offset 8), capacity_or_alloc_ptr (offset 16)
            // Small string detection: capacity_or_alloc_ptr has high bit set (negative when signed)

            const base_offset: i32 = switch (value_loc) {
                .stack => |offset| offset,
                .stack_str => |offset| offset,
                else => return,
            };

            // Load capacity_or_alloc_ptr to check for small string
            const cap_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(cap_reg);

            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.ldrRegMemSoff(.w64, cap_reg, .FP, base_offset + 16);
            } else {
                try self.codegen.emit.movRegMem(.w64, cap_reg, .RBP, base_offset + 16);
            }

            // Check if small string (high bit set = negative)
            // If negative, skip the incref
            const skip_patch = blk: {
                if (comptime builtin.cpu.arch == .aarch64) {
                    // Compare with 0 and branch if negative (mi = minus/negative)
                    try self.codegen.emit.cmpRegImm12(.w64, cap_reg, 0);
                    const patch_loc = self.codegen.currentOffset();
                    try self.codegen.emit.bcond(.mi, 0);
                    break :blk patch_loc;
                } else {
                    // Test the sign bit and jump if sign flag set (negative)
                    try self.codegen.emit.testRegReg(.w64, cap_reg, cap_reg);
                    break :blk try self.codegen.emitCondJump(.sign);
                }
            };

            // Not a small string - load the bytes pointer and call incref
            const ptr_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(ptr_reg);

            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.ldrRegMemSoff(.w64, ptr_reg, .FP, base_offset);
            } else {
                try self.codegen.emit.movRegMem(.w64, ptr_reg, .RBP, base_offset);
            }

            // Call increfDataPtrC(ptr, count, roc_ops)
            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.movRegReg(.w64, .X0, ptr_reg);
                try self.codegen.emit.movRegImm64(.X1, @intCast(count));
                try self.codegen.emit.movRegReg(.w64, .X2, roc_ops_reg);

                const addr_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(addr_reg, @intCast(fn_addr));
                try self.codegen.emit.blrReg(addr_reg);
                self.codegen.freeGeneral(addr_reg);
            } else {
                try self.codegen.emit.movRegImm64(.R11, @intCast(fn_addr));
                try self.codegen.emit.movRegReg(.w64, .RDI, ptr_reg);
                try self.codegen.emit.movRegImm64(.RSI, @intCast(count));
                try self.codegen.emit.movRegReg(.w64, .RDX, roc_ops_reg);
                try self.codegen.emit.callReg(.R11);
            }

            // Patch the skip jump to here
            self.codegen.patchJump(skip_patch, self.codegen.currentOffset());
        }

        /// Emit decref for a string value
        fn emitStrDecref(self: *Self, value_loc: ValueLocation) Error!void {
            const roc_ops_reg = self.roc_ops_reg orelse return;
            const fn_addr: usize = @intFromPtr(&decrefDataPtrC);

            const base_offset: i32 = switch (value_loc) {
                .stack => |offset| offset,
                .stack_str => |offset| offset,
                else => return,
            };

            // Load capacity_or_alloc_ptr to check for small string
            const cap_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(cap_reg);

            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.ldrRegMemSoff(.w64, cap_reg, .FP, base_offset + 16);
            } else {
                try self.codegen.emit.movRegMem(.w64, cap_reg, .RBP, base_offset + 16);
            }

            // Check if small string (high bit set = negative)
            const skip_patch = blk: {
                if (comptime builtin.cpu.arch == .aarch64) {
                    try self.codegen.emit.cmpRegImm12(.w64, cap_reg, 0);
                    const patch_loc = self.codegen.currentOffset();
                    try self.codegen.emit.bcond(.mi, 0);
                    break :blk patch_loc;
                } else {
                    try self.codegen.emit.testRegReg(.w64, cap_reg, cap_reg);
                    break :blk try self.codegen.emitCondJump(.sign);
                }
            };

            // Not a small string - load the bytes pointer and call decref
            const ptr_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(ptr_reg);

            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.ldrRegMemSoff(.w64, ptr_reg, .FP, base_offset);
            } else {
                try self.codegen.emit.movRegMem(.w64, ptr_reg, .RBP, base_offset);
            }

            // Call decrefDataPtrC(ptr, alignment, elements_refcounted, roc_ops)
            // Strings have 1-byte alignment for the data
            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.movRegReg(.w64, .X0, ptr_reg);
                try self.codegen.emit.movRegImm64(.X1, 1); // alignment
                try self.codegen.emit.movRegImm64(.X2, 0); // elements_refcounted = false
                try self.codegen.emit.movRegReg(.w64, .X3, roc_ops_reg);

                const addr_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(addr_reg, @intCast(fn_addr));
                try self.codegen.emit.blrReg(addr_reg);
                self.codegen.freeGeneral(addr_reg);
            } else {
                try self.codegen.emit.movRegImm64(.R11, @intCast(fn_addr));
                try self.codegen.emit.movRegReg(.w64, .RDI, ptr_reg);
                try self.codegen.emit.movRegImm64(.RSI, 1);
                try self.codegen.emit.movRegImm64(.RDX, 0);
                try self.codegen.emit.movRegReg(.w64, .RCX, roc_ops_reg);
                try self.codegen.emit.callReg(.R11);
            }

            // Patch the skip jump to here
            self.codegen.patchJump(skip_patch, self.codegen.currentOffset());
        }

        /// Emit free for a string value
        fn emitStrFree(self: *Self, value_loc: ValueLocation) Error!void {
            const roc_ops_reg = self.roc_ops_reg orelse return;
            const fn_addr: usize = @intFromPtr(&freeDataPtrC);

            const base_offset: i32 = switch (value_loc) {
                .stack => |offset| offset,
                .stack_str => |offset| offset,
                else => return,
            };

            // Load capacity_or_alloc_ptr to check for small string
            const cap_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(cap_reg);

            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.ldrRegMemSoff(.w64, cap_reg, .FP, base_offset + 16);
            } else {
                try self.codegen.emit.movRegMem(.w64, cap_reg, .RBP, base_offset + 16);
            }

            // Check if small string (high bit set = negative)
            const skip_patch = blk: {
                if (comptime builtin.cpu.arch == .aarch64) {
                    try self.codegen.emit.cmpRegImm12(.w64, cap_reg, 0);
                    const patch_loc = self.codegen.currentOffset();
                    try self.codegen.emit.bcond(.mi, 0);
                    break :blk patch_loc;
                } else {
                    try self.codegen.emit.testRegReg(.w64, cap_reg, cap_reg);
                    break :blk try self.codegen.emitCondJump(.sign);
                }
            };

            // Not a small string - load the bytes pointer and call free
            const ptr_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(ptr_reg);

            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.ldrRegMemSoff(.w64, ptr_reg, .FP, base_offset);
            } else {
                try self.codegen.emit.movRegMem(.w64, ptr_reg, .RBP, base_offset);
            }

            // Call freeDataPtrC(ptr, alignment, elements_refcounted, roc_ops)
            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.movRegReg(.w64, .X0, ptr_reg);
                try self.codegen.emit.movRegImm64(.X1, 1);
                try self.codegen.emit.movRegImm64(.X2, 0);
                try self.codegen.emit.movRegReg(.w64, .X3, roc_ops_reg);

                const addr_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(addr_reg, @intCast(fn_addr));
                try self.codegen.emit.blrReg(addr_reg);
                self.codegen.freeGeneral(addr_reg);
            } else {
                try self.codegen.emit.movRegImm64(.R11, @intCast(fn_addr));
                try self.codegen.emit.movRegReg(.w64, .RDI, ptr_reg);
                try self.codegen.emit.movRegImm64(.RSI, 1);
                try self.codegen.emit.movRegImm64(.RDX, 0);
                try self.codegen.emit.movRegReg(.w64, .RCX, roc_ops_reg);
                try self.codegen.emit.callReg(.R11);
            }

            // Patch the skip jump to here
            self.codegen.patchJump(skip_patch, self.codegen.currentOffset());
        }

        /// Emit incref for a box value
        fn emitBoxIncref(self: *Self, value_loc: ValueLocation, count: u16) Error!void {
            const roc_ops_reg = self.roc_ops_reg orelse return;
            const fn_addr: usize = @intFromPtr(&increfDataPtrC);

            // Box is just a pointer
            const ptr_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(ptr_reg);

            switch (value_loc) {
                .stack => |offset| {
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.ldrRegMemSoff(.w64, ptr_reg, .FP, offset);
                    } else {
                        try self.codegen.emit.movRegMem(.w64, ptr_reg, .RBP, offset);
                    }
                },
                .general_reg => |r| {
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.movRegReg(.w64, ptr_reg, r);
                    } else {
                        try self.codegen.emit.movRegReg(.w64, ptr_reg, r);
                    }
                },
                else => return,
            }

            // Call increfDataPtrC(ptr, count, roc_ops)
            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.movRegReg(.w64, .X0, ptr_reg);
                try self.codegen.emit.movRegImm64(.X1, @intCast(count));
                try self.codegen.emit.movRegReg(.w64, .X2, roc_ops_reg);

                const addr_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(addr_reg, @intCast(fn_addr));
                try self.codegen.emit.blrReg(addr_reg);
                self.codegen.freeGeneral(addr_reg);
            } else {
                try self.codegen.emit.movRegImm64(.R11, @intCast(fn_addr));
                try self.codegen.emit.movRegReg(.w64, .RDI, ptr_reg);
                try self.codegen.emit.movRegImm64(.RSI, @intCast(count));
                try self.codegen.emit.movRegReg(.w64, .RDX, roc_ops_reg);
                try self.codegen.emit.callReg(.R11);
            }
        }

        /// Emit decref for a box value
        fn emitBoxDecref(self: *Self, value_loc: ValueLocation) Error!void {
            const roc_ops_reg = self.roc_ops_reg orelse return;
            const fn_addr: usize = @intFromPtr(&decrefDataPtrC);

            const ptr_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(ptr_reg);

            switch (value_loc) {
                .stack => |offset| {
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.ldrRegMemSoff(.w64, ptr_reg, .FP, offset);
                    } else {
                        try self.codegen.emit.movRegMem(.w64, ptr_reg, .RBP, offset);
                    }
                },
                .general_reg => |r| {
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.movRegReg(.w64, ptr_reg, r);
                    } else {
                        try self.codegen.emit.movRegReg(.w64, ptr_reg, r);
                    }
                },
                else => return,
            }

            // Call decrefDataPtrC(ptr, alignment, elements_refcounted, roc_ops)
            // Boxes use 8-byte alignment
            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.movRegReg(.w64, .X0, ptr_reg);
                try self.codegen.emit.movRegImm64(.X1, 8);
                try self.codegen.emit.movRegImm64(.X2, 0);
                try self.codegen.emit.movRegReg(.w64, .X3, roc_ops_reg);

                const addr_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(addr_reg, @intCast(fn_addr));
                try self.codegen.emit.blrReg(addr_reg);
                self.codegen.freeGeneral(addr_reg);
            } else {
                try self.codegen.emit.movRegImm64(.R11, @intCast(fn_addr));
                try self.codegen.emit.movRegReg(.w64, .RDI, ptr_reg);
                try self.codegen.emit.movRegImm64(.RSI, 8);
                try self.codegen.emit.movRegImm64(.RDX, 0);
                try self.codegen.emit.movRegReg(.w64, .RCX, roc_ops_reg);
                try self.codegen.emit.callReg(.R11);
            }
        }

        /// Emit free for a box value
        fn emitBoxFree(self: *Self, value_loc: ValueLocation) Error!void {
            const roc_ops_reg = self.roc_ops_reg orelse return;
            const fn_addr: usize = @intFromPtr(&freeDataPtrC);

            const ptr_reg = try self.allocTempGeneral();
            defer self.codegen.freeGeneral(ptr_reg);

            switch (value_loc) {
                .stack => |offset| {
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.ldrRegMemSoff(.w64, ptr_reg, .FP, offset);
                    } else {
                        try self.codegen.emit.movRegMem(.w64, ptr_reg, .RBP, offset);
                    }
                },
                .general_reg => |r| {
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.codegen.emit.movRegReg(.w64, ptr_reg, r);
                    } else {
                        try self.codegen.emit.movRegReg(.w64, ptr_reg, r);
                    }
                },
                else => return,
            }

            // Call freeDataPtrC(ptr, alignment, elements_refcounted, roc_ops)
            if (comptime builtin.cpu.arch == .aarch64) {
                try self.codegen.emit.movRegReg(.w64, .X0, ptr_reg);
                try self.codegen.emit.movRegImm64(.X1, 8);
                try self.codegen.emit.movRegImm64(.X2, 0);
                try self.codegen.emit.movRegReg(.w64, .X3, roc_ops_reg);

                const addr_reg = try self.allocTempGeneral();
                try self.codegen.emitLoadImm(addr_reg, @intCast(fn_addr));
                try self.codegen.emit.blrReg(addr_reg);
                self.codegen.freeGeneral(addr_reg);
            } else {
                try self.codegen.emit.movRegImm64(.R11, @intCast(fn_addr));
                try self.codegen.emit.movRegReg(.w64, .RDI, ptr_reg);
                try self.codegen.emit.movRegImm64(.RSI, 8);
                try self.codegen.emit.movRegImm64(.RDX, 0);
                try self.codegen.emit.movRegReg(.w64, .RCX, roc_ops_reg);
                try self.codegen.emit.callReg(.R11);
            }
        }

        pub fn patchPendingCalls(self: *Self) Error!void {
            for (self.pending_calls.items) |pending| {
                const key: u48 = @bitCast(pending.target_symbol);
                const proc = self.proc_registry.get(key) orelse {
                    return Error.LocalNotFound; // Function not found
                };
                self.patchCallTarget(pending.call_site, proc.code_start);
            }
        }

        /// Patch a call instruction to target a specific offset
        fn patchCallTarget(self: *Self, call_site: usize, target_offset: usize) void {
            const rel_offset: i32 = @intCast(@as(i64, @intCast(target_offset)) - @as(i64, @intCast(call_site)));

            if (comptime builtin.cpu.arch == .aarch64) {
                // BL instruction: patch the immediate offset
                // BL uses 26-bit signed offset in instructions (multiply by 4)
                const instr_offset = @divTrunc(rel_offset, 4);
                self.codegen.patchBL(call_site, instr_offset);
            } else {
                // CALL rel32: patch the 32-bit relative offset
                // Offset is relative to instruction after CALL (call_site + 5)
                const call_rel = rel_offset - 5;
                self.codegen.patchCall(call_site, call_rel);
            }
        }
    };
}

/// Select the appropriate code generator based on target architecture
pub const MonoExprCodeGen = if (builtin.cpu.arch == .aarch64)
    MonoExprCodeGenFor(aarch64.CodeGen.AArch64CodeGen, aarch64.GeneralReg, aarch64.FloatReg, aarch64.Emit.Condition)
else if (builtin.cpu.arch == .x86_64)
    MonoExprCodeGenFor(x86_64.CodeGen.SystemVCodeGen, x86_64.GeneralReg, x86_64.FloatReg, x86_64.Emit.Condition)
else
    UnsupportedArchCodeGen;

/// Stub code generator for unsupported architectures.
/// This allows the code to compile for cross-compilation targets like 32-bit ARM/x86,
/// but will error at runtime if actually used.
pub const UnsupportedArchCodeGen = struct {
    const Self = @This();

    pub const Error = error{
        UnsupportedArchitecture,
        UnsupportedExpression,
        OutOfMemory,
    };

    pub const CodeResult = struct {
        code: []const u8,
        entry_offset: usize,
    };

    allocator: Allocator,

    pub fn init(
        allocator: Allocator,
        _: *const MonoExprStore,
        _: ?*const LayoutStore,
        _: ?*StaticDataInterner,
    ) Self {
        return .{ .allocator = allocator };
    }

    pub fn deinit(_: *Self) void {}

    pub fn compileAllProcs(_: *Self, _: anytype) Error!void {
        return error.UnsupportedArchitecture;
    }

    pub fn generateCode(_: *Self, _: anytype, _: anytype, _: anytype) Error!CodeResult {
        return error.UnsupportedArchitecture;
    }

    pub fn generateExpr(_: *Self, _: anytype) Error!void {
        return error.UnsupportedArchitecture;
    }

    pub fn generateProc(_: *Self, _: anytype) Error!void {
        return error.UnsupportedArchitecture;
    }

    pub fn finalize(_: *Self) Error![]const u8 {
        return error.UnsupportedArchitecture;
    }

    pub fn getCode(_: *const Self) []const u8 {
        return &[_]u8{};
    }
};

// Tests

test "code generator initialization" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = MonoExprStore.init(allocator);
    defer store.deinit();

    var codegen = MonoExprCodeGen.init(allocator, &store, null, null);
    defer codegen.deinit();
}

test "generate i64 literal" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = MonoExprStore.init(allocator);
    defer store.deinit();

    // Add an i64 literal
    const expr_id = try store.addExpr(.{ .i64_literal = 42 }, base.Region.zero());

    var codegen = MonoExprCodeGen.init(allocator, &store, null, null);
    defer codegen.deinit();

    const result = try codegen.generateCode(expr_id, .i64, 1);
    defer allocator.free(result.code);

    // Should have generated some code
    try std.testing.expect(result.code.len > 0);
}

test "generate bool literal" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = MonoExprStore.init(allocator);
    defer store.deinit();

    const expr_id = try store.addExpr(.{ .bool_literal = true }, base.Region.zero());

    var codegen = MonoExprCodeGen.init(allocator, &store, null, null);
    defer codegen.deinit();

    const result = try codegen.generateCode(expr_id, .bool, 1);
    defer allocator.free(result.code);

    try std.testing.expect(result.code.len > 0);
}

test "generate addition" {
    if (comptime builtin.cpu.arch != .x86_64 and builtin.cpu.arch != .aarch64) {
        return error.SkipZigTest;
    }

    const allocator = std.testing.allocator;
    var store = MonoExprStore.init(allocator);
    defer store.deinit();

    // Create: 1 + 2
    const lhs_id = try store.addExpr(.{ .i64_literal = 1 }, base.Region.zero());
    const rhs_id = try store.addExpr(.{ .i64_literal = 2 }, base.Region.zero());
    const add_id = try store.addExpr(.{ .binop = .{
        .op = .add,
        .lhs = lhs_id,
        .rhs = rhs_id,
        .result_layout = .i64,
    } }, base.Region.zero());

    var codegen = MonoExprCodeGen.init(allocator, &store, null, null);
    defer codegen.deinit();

    const result = try codegen.generateCode(add_id, .i64, 1);
    defer allocator.free(result.code);

    try std.testing.expect(result.code.len > 0);
}
