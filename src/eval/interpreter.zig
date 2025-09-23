//! Type-Carrying Interpreter for Roc
//!
//! This interpreter uses a parallel stack architecture where:
//! - Value stack contains raw, unboxed bytes
//! - Type stack contains type variables at corresponding indices
//! - A single runtime type store unifies types across all modules
//!
//! This design enables correct polymorphism while maintaining performance.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const types = @import("types");
const can = @import("can");
const builtins = @import("builtins");
const collections = @import("collections");
const check = @import("check");
const layout = @import("layout");
const build_options = @import("build_options");
const stack = @import("stack.zig");

// Core type aliases
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const RocOps = builtins.host_abi.RocOps;
const RocList = builtins.list.RocList;
const RocStr = builtins.str.RocStr;
const strConcat = builtins.str.strConcat;
const RocDec = builtins.dec.RocDec;
const Layout = layout.Layout;
const Expr = CIR.Expr;
const Pattern = CIR.Pattern;
const Ident = base.Ident;
const Var = types.Var;
const Content = types.Content;
const TypesStore = types.Store;

const target_usize = base.target.Target.native.target_usize;
const DEBUG_ENABLED = build_options.trace_eval;

/// Errors that can occur during evaluation
pub const EvalError = error{
    Crash,
    OutOfMemory,
    StackOverflow,
    InvalidStackState,
    TypeMismatch,
    UnboundVariable,
    UnificationFailed,
    LayoutConversionFailed,
    InvalidFieldAccess,
    InvalidTagUnion,
    DivisionByZero,
    IntegerOverflow,
    IntegerUnderflow,
    InvalidNumericOp,
    LayoutError,
    UnexpectedWorkItem,
    Overflow,
    InvalidCallStack,
    UnsupportedScalarType,
};

/// Key for mapping compile-time type variables to runtime ones
const VarTranslationKey = struct {
    module_id: usize, // Pointer to module as ID
    compile_var: Var,
};

/// A value on the stack with its position
pub const StackValue = struct {
    /// Pointer to the value's bytes in stack memory
    ptr: ?*anyopaque,
    /// The type variable for this value
    type_var: Var,

    pub fn asI128(self: StackValue, interp: *const Interpreter) i128 {
        const mutable_interp = @constCast(interp);
        const layout_val = mutable_interp.getLayoutForVar(self.type_var) catch unreachable;
        if (layout_val.tag != .scalar) unreachable;
        const scalar = layout_val.data.scalar;
        if (scalar.tag != .int) unreachable;

        return switch (scalar.data.int) {
            .i8 => @as(i128, @as(*i8, @ptrCast(@alignCast(self.ptr))).*),
            .i16 => @as(i128, @as(*i16, @ptrCast(@alignCast(self.ptr))).*),
            .i32 => @as(i128, @as(*i32, @ptrCast(@alignCast(self.ptr))).*),
            .i64 => @as(i128, @as(*i64, @ptrCast(@alignCast(self.ptr))).*),
            .i128 => @as(*i128, @ptrCast(@alignCast(self.ptr))).*,
            .u8 => @as(i128, @as(*u8, @ptrCast(@alignCast(self.ptr))).*),
            .u16 => @as(i128, @as(*u16, @ptrCast(@alignCast(self.ptr))).*),
            .u32 => @as(i128, @as(*u32, @ptrCast(@alignCast(self.ptr))).*),
            .u64 => @as(i128, @as(*u64, @ptrCast(@alignCast(self.ptr))).*),
            .u128 => @intCast(@as(*u128, @ptrCast(@alignCast(self.ptr))).*),
        };
    }

    pub fn asBool(self: StackValue) bool {
        const byte_ptr: *u8 = @ptrCast(@alignCast(self.ptr));
        return byte_ptr.* != 0;
    }

    pub fn asStr(self: StackValue) RocStr {
        return @as(*RocStr, @ptrCast(@alignCast(self.ptr))).*;
    }
};

/// Internal representation of a value on the stack
const InternalStackValue = struct {
    /// Offset in the stack memory
    offset: usize,
    /// Size in bytes
    size: usize,
    /// Alignment requirement
    alignment: std.mem.Alignment,
};

/// Work item types for the work stack
const WorkItemKind = enum {
    w_eval_expr,
    w_lambda_call,
    w_lambda_return,
    w_binop_add,
    w_binop_sub,
    w_binop_mul,
    w_binop_div,
    w_binop_div_trunc,
    w_binop_rem,
    w_binop_eq,
    w_binop_ne,
    w_binop_gt,
    w_binop_lt,
    w_binop_ge,
    w_binop_le,
    w_binop_and,
    w_binop_or,
    w_unary_minus,
    w_unary_not,
    w_eval_record_fields,
    w_dot_access,
    w_crash,
    w_str_interpolate_combine,
    w_str_interpolate_segments,
    w_eval_tuple_elements,
    w_let_bind,
    w_block_cleanup,
    w_if_then_else,
    w_when_branch,
};

/// Block cleanup data
const BlockCleanup = struct {
    bindings_to_keep: u32,
    values_to_keep: u32,
};

/// If-then-else data
const IfData = struct {
    then_branch: CIR.Expr.Idx,
    else_branch: CIR.Expr.Idx,
};

/// When branch data
const WhenData = struct {
    branches: CIR.Expr.Match.Branch.Span,
    current_branch: u32,
};

/// Work item for the evaluation stack
const WorkItem = struct {
    kind: WorkItemKind,
    expr_idx: CIR.Expr.Idx,
    module_ctx: *ModuleEnv,
    extra: union {
        none: void,
        arg_count: u32,
        current_field_idx: u32,
        current_element_idx: u32,
        dot_access_field_name: Ident.Idx,
        crash_msg: base.StringLiteral.Idx,
        segment_count: u32,
        decl_pattern_idx: CIR.Pattern.Idx,
        block_cleanup: BlockCleanup,
        if_data: IfData,
        when_data: WhenData,
    },
};

/// Binding of a pattern to a value
const Binding = struct {
    pattern_idx: CIR.Pattern.Idx,
    value: StackValue,

    fn cleanup(self: Binding, roc_ops: *RocOps) void {
        _ = self;
        _ = roc_ops;
    }
};

/// Call frame for function calls
const CallFrame = struct {
    /// Index in value stack where this frame starts
    value_base: usize,
    /// Index in type stack where this frame starts
    type_base: usize,
    /// Number of arguments
    arg_count: u32,
    /// Return address (work stack index)
    return_addr: usize,
    /// Module where the function was defined
    module_ctx: *ModuleEnv,
    /// Saved bindings count
    saved_bindings_count: usize,
};

/// The type-carrying interpreter
pub const Interpreter = struct {
    /// Memory allocator
    allocator: std.mem.Allocator,
    /// Current module environment
    env: *ModuleEnv,
    /// Other modules for cross-module calls
    other_modules: []const *ModuleEnv,

    /// Raw value bytes storage
    stack_memory: *stack.Stack,
    /// Parallel type information
    type_stack: std.ArrayList(Var),
    /// Values metadata (offset, size, alignment)
    value_stack: std.ArrayList(InternalStackValue),

    /// Single runtime type store for all modules
    runtime_types: *TypesStore,
    /// Maps compile-time vars to runtime vars
    var_translation: std.AutoHashMap(VarTranslationKey, Var),
    /// Cache of type var to layout conversions
    layout_cache: std.AutoHashMap(Var, Layout),
    /// Store for creating compound layouts
    layout_store: *layout.Store,

    /// Unification scratch space
    unify_scratch: *check.unifier.Scratch,
    /// Occurs check scratch space
    occurs_scratch: *check.occurs.Scratch,
    /// Problem store for unification (unused at runtime)
    problem_store: *check.problem.Store,
    /// Snapshot store for unification (unused at runtime)
    snapshot_store: *check.snapshot.Store,

    /// Work queue for evaluation
    work_stack: std.ArrayList(WorkItem),
    /// Local bindings
    bindings_stack: std.ArrayList(Binding),
    /// Call frames
    frame_stack: std.ArrayList(CallFrame),

    /// Debug tracing
    trace_indent: u32,
    trace_writer: ?std.io.AnyWriter,

    /// Crash state
    has_crashed: bool,

    /// Cached result type for eval return value
    cached_result_type: ?Var,
    crash_message: ?[]const u8,

    pub fn init(
        allocator: std.mem.Allocator,
        cir: *ModuleEnv,
        stack_memory: *stack.Stack,
        layout_store: *layout.Store,
        type_store_unused: *types.Store, // Kept for API compatibility
    ) !Interpreter {
        _ = type_store_unused;
        return initWithModules(allocator, cir, &.{}, stack_memory, layout_store, null);
    }

    pub fn initWithModules(
        allocator: std.mem.Allocator,
        cir: *ModuleEnv,
        other_modules: []const *ModuleEnv,
        stack_memory: *stack.Stack,
        layout_store: *layout.Store,
        type_store_unused: ?*types.Store,
    ) !Interpreter {
        _ = type_store_unused;

        // Create runtime type store
        const runtime_types = try allocator.create(TypesStore);
        runtime_types.* = try TypesStore.init(allocator);

        // Create unification scratch spaces
        const unify_scratch = try allocator.create(check.unifier.Scratch);
        unify_scratch.* = try check.unifier.Scratch.init(allocator);

        const occurs_scratch = try allocator.create(check.occurs.Scratch);
        occurs_scratch.* = try check.occurs.Scratch.init(allocator);

        const problem_store = try allocator.create(check.problem.Store);
        problem_store.* = .{}; // Default initialization

        const snapshot_store = try allocator.create(check.snapshot.Store);
        snapshot_store.* = try check.snapshot.Store.initCapacity(allocator, 16);

        return Interpreter{
            .allocator = allocator,
            .env = cir,
            .other_modules = other_modules,
            .stack_memory = stack_memory,
            .type_stack = try std.ArrayList(Var).initCapacity(allocator, 128),
            .value_stack = try std.ArrayList(InternalStackValue).initCapacity(allocator, 128),
            .runtime_types = runtime_types,
            .var_translation = std.AutoHashMap(VarTranslationKey, Var).init(allocator),
            .layout_cache = std.AutoHashMap(Var, Layout).init(allocator),
            .layout_store = layout_store,
            .unify_scratch = unify_scratch,
            .occurs_scratch = occurs_scratch,
            .problem_store = problem_store,
            .snapshot_store = snapshot_store,
            .work_stack = try std.ArrayList(WorkItem).initCapacity(allocator, 128),
            .bindings_stack = try std.ArrayList(Binding).initCapacity(allocator, 128),
            .frame_stack = try std.ArrayList(CallFrame).initCapacity(allocator, 128),
            .trace_indent = 0,
            .trace_writer = null,
            .has_crashed = false,
            .cached_result_type = null,
            .crash_message = null,
        };
    }

    pub fn deinit(self: *Interpreter, roc_ops: *RocOps) void {
        // Clean up bindings
        for (self.bindings_stack.items) |binding| {
            binding.cleanup(roc_ops);
        }

        // Clean up runtime types
        self.runtime_types.deinit();
        self.allocator.destroy(self.runtime_types);

        // Clean up scratch spaces
        self.unify_scratch.deinit();
        self.allocator.destroy(self.unify_scratch);

        self.occurs_scratch.deinit();
        self.allocator.destroy(self.occurs_scratch);

        self.problem_store.deinit(self.allocator);
        self.allocator.destroy(self.problem_store);

        self.snapshot_store.deinit();
        self.allocator.destroy(self.snapshot_store);

        // Clean up maps and stacks
        self.var_translation.deinit();
        self.layout_cache.deinit();
        self.type_stack.deinit();
        self.value_stack.deinit();
        self.work_stack.deinit();
        self.bindings_stack.deinit();
        self.frame_stack.deinit();
    }

    /// Translate a compile-time type variable to runtime type store
    fn translateTypeVar(self: *Interpreter, module: *ModuleEnv, compile_var: Var) !Var {
        const key = VarTranslationKey{
            .module_id = @intFromPtr(module),
            .compile_var = compile_var,
        };

        // Check cache
        if (self.var_translation.get(key)) |runtime_var| {
            return runtime_var;
        }

        // Resolve and translate the type
        const resolved = module.types.resolveVar(compile_var);
        const runtime_var = try self.translateContent(module, resolved.desc.content);

        // Cache the translation
        try self.var_translation.put(key, runtime_var);
        return runtime_var;
    }

    /// Translate type content to runtime store
    fn translateContent(self: *Interpreter, module: *ModuleEnv, content: Content) !Var {
        return switch (content) {
            .flex_var => |name| {
                const runtime_var = try self.runtime_types.fresh();
                if (name) |n| {
                    try self.runtime_types.setVarContent(runtime_var, .{ .flex_var = n });
                }
                return runtime_var;
            },
            .rigid_var => |name| try self.runtime_types.freshFromContent(.{ .rigid_var = name }),
            .err => try self.runtime_types.freshFromContent(.err),
            .alias => |alias| {
                // Translate the aliased type
                const vars = try self.translateVarSlice(module, module.types.sliceVars(alias.vars.nonempty));

                // Store the vars in runtime types
                const start_idx = self.runtime_types.vars.len();
                for (vars) |v| {
                    _ = try self.runtime_types.vars.append(self.allocator, v);
                }

                const vars_range = Var.SafeList.NonEmptyRange{
                    .nonempty = .{
                        .start = @enumFromInt(start_idx),
                        .count = @intCast(vars.len),
                    },
                };

                return try self.runtime_types.freshFromContent(.{
                    .alias = .{
                        .ident = alias.ident,
                        .vars = vars_range,
                    },
                });
            },
            .structure => |structure| try self.translateStructure(module, structure),
        };
    }

    /// Translate a type structure to runtime store
    fn translateStructure(self: *Interpreter, module: *ModuleEnv, structure: types.FlatType) error{OutOfMemory}!Var {
        const flat_type = switch (structure) {
            .fn_pure, .fn_effectful, .fn_unbound => |func| blk: {
                const args = try self.translateVarSlice(module, module.types.sliceVars(func.args));
                const ret = try self.translateTypeVar(module, func.ret);

                // Store args in runtime types
                const start_idx = self.runtime_types.vars.len();
                for (args) |arg| {
                    _ = try self.runtime_types.vars.append(self.allocator, arg);
                }
                const args_range = Var.SafeList.Range{
                    .start = @enumFromInt(start_idx),
                    .count = @intCast(args.len),
                };

                const func_content = types.Func{
                    .args = args_range,
                    .ret = ret,
                    .needs_instantiation = func.needs_instantiation,
                };

                break :blk switch (structure) {
                    .fn_pure => types.FlatType{ .fn_pure = func_content },
                    .fn_effectful => types.FlatType{ .fn_effectful = func_content },
                    .fn_unbound => types.FlatType{ .fn_unbound = func_content },
                    else => unreachable,
                };
            },
            .record => |record| blk: {
                const fields = module.types.getRecordFieldsSlice(record.fields);
                const fields_len = fields.len;
                const field_names = fields.items(.name);
                const field_types = fields.items(.var_);

                // Store fields in runtime types
                const fields_start_idx = self.runtime_types.record_fields.len();
                for (0..fields_len) |i| {
                    const translated_var = try self.translateTypeVar(module, field_types[i]);
                    _ = try self.runtime_types.record_fields.append(self.allocator, .{
                        .name = field_names[i],
                        .var_ = translated_var,
                    });
                }
                const runtime_fields_range = types.RecordField.SafeMultiList.Range{
                    .start = @enumFromInt(fields_start_idx),
                    .count = @intCast(fields_len),
                };
                const ext = try self.translateTypeVar(module, record.ext);

                const record_content = types.Record{
                    .fields = runtime_fields_range,
                    .ext = ext,
                };

                break :blk types.FlatType{ .record = record_content };
            },
            .record_unbound => |fields_range| blk: {
                const fields = module.types.getRecordFieldsSlice(fields_range);
                const fields_len = fields.len;
                const field_names = fields.items(.name);
                const field_types = fields.items(.var_);

                // Store fields in runtime types
                const fields_start_idx = self.runtime_types.record_fields.len();
                for (0..fields_len) |i| {
                    const translated_var = try self.translateTypeVar(module, field_types[i]);
                    _ = try self.runtime_types.record_fields.append(self.allocator, .{
                        .name = field_names[i],
                        .var_ = translated_var,
                    });
                }
                const runtime_fields_range = types.RecordField.SafeMultiList.Range{
                    .start = @enumFromInt(fields_start_idx),
                    .count = @intCast(fields_len),
                };
                // record_unbound is just the fields, no ext
                break :blk types.FlatType{ .record_unbound = runtime_fields_range };
            },
            .tag_union => |tag_union| blk: {
                const tags = module.types.getTagsSlice(tag_union.tags);
                const tags_len = tags.len;
                var runtime_tags = try self.allocator.alloc(types.Tag, tags_len);
                const tag_names = tags.items(.name);
                const tag_args = tags.items(.args);
                for (0..tags_len) |i| {
                    const tag_name = tag_names[i];
                    const tag_args_range = tag_args[i];
                    const payloads = module.types.sliceVars(tag_args_range);
                    const runtime_payloads = try self.translateVarSlice(module, payloads);

                    // Store payloads in runtime types
                    const start_idx = self.runtime_types.vars.len();
                    for (runtime_payloads) |payload| {
                        _ = try self.runtime_types.vars.append(self.allocator, payload);
                    }
                    const runtime_args_range = Var.SafeList.Range{
                        .start = @enumFromInt(start_idx),
                        .count = @intCast(runtime_payloads.len),
                    };

                    runtime_tags[i] = types.Tag{
                        .name = tag_name,
                        .args = runtime_args_range,
                    };
                }
                const ext = try self.translateTypeVar(module, tag_union.ext);

                // Store tags in runtime types
                const tags_range = try self.runtime_types.appendTags(runtime_tags);

                break :blk types.FlatType{
                    .tag_union = types.TagUnion{
                        .tags = tags_range,
                        .ext = ext,
                    },
                };
            },
            .tuple => |tuple| blk: {
                const elems = try self.translateVarSlice(module, module.types.sliceVars(tuple.elems));

                // Store elems in runtime types
                const start_idx = self.runtime_types.vars.len();
                for (elems) |elem| {
                    _ = try self.runtime_types.vars.append(self.allocator, elem);
                }
                const elems_range = Var.SafeList.Range{
                    .start = @enumFromInt(start_idx),
                    .count = @intCast(elems.len),
                };

                break :blk types.FlatType{
                    .tuple = .{
                        .elems = elems_range,
                    },
                };
            },
            .str => types.FlatType.str,
            .list => |elem| types.FlatType{ .list = try self.translateTypeVar(module, elem) },
            .box => |elem| types.FlatType{ .box = try self.translateTypeVar(module, elem) },
            .nominal_type => |nominal| blk: {
                const args = try self.translateVarSlice(module, module.types.sliceVars(nominal.vars.nonempty));

                // Store args in runtime types
                const start_idx = self.runtime_types.vars.len();
                for (args) |arg| {
                    _ = try self.runtime_types.vars.append(self.allocator, arg);
                }
                const args_range = Var.SafeList.NonEmptyRange{
                    .nonempty = .{
                        .start = @enumFromInt(start_idx),
                        .count = @intCast(args.len),
                    },
                };

                break :blk types.FlatType{
                    .nominal_type = .{
                        .ident = nominal.ident,
                        .vars = args_range,
                        .origin_module = nominal.origin_module,
                    },
                };
            },
            .num => |num| types.FlatType{ .num = num },
            .list_unbound => types.FlatType.list_unbound,
            .record_poly => |rec_poly| blk: {
                const record_fields = module.types.getRecordFieldsSlice(rec_poly.record.fields);
                const fields_len = record_fields.len;
                const field_names = record_fields.items(.name);
                const field_types = record_fields.items(.var_);

                // Store fields in runtime types
                const fields_start_idx = self.runtime_types.record_fields.len();
                for (0..fields_len) |i| {
                    const translated_var = try self.translateTypeVar(module, field_types[i]);
                    _ = try self.runtime_types.record_fields.append(self.allocator, .{
                        .name = field_names[i],
                        .var_ = translated_var,
                    });
                }
                const runtime_fields_range = types.RecordField.SafeMultiList.Range{
                    .start = @enumFromInt(fields_start_idx),
                    .count = @intCast(fields_len),
                };

                const ext = try self.translateTypeVar(module, rec_poly.record.ext);
                const var_ = try self.translateTypeVar(module, rec_poly.var_);

                break :blk types.FlatType{
                    .record_poly = .{
                        .record = types.Record{
                            .fields = runtime_fields_range,
                            .ext = ext,
                        },
                        .var_ = var_,
                    },
                };
            },
            .empty_record => types.FlatType.empty_record,
            .empty_tag_union => types.FlatType.empty_tag_union,
        };

        return try self.runtime_types.freshFromContent(.{ .structure = flat_type });
    }

    /// Translate a slice of type variables
    fn translateVarSlice(self: *Interpreter, module: *ModuleEnv, vars: []const Var) error{OutOfMemory}![]Var {
        var result = try self.allocator.alloc(Var, vars.len);
        for (vars, 0..) |v, i| {
            result[i] = try self.translateTypeVar(module, v);
        }
        return result;
    }

    /// Get layout for a type variable (with caching)
    pub fn getLayoutForVar(self: *Interpreter, type_var: Var) !Layout {
        // Check cache
        if (self.layout_cache.get(type_var)) |cached| {
            return cached;
        }

        // Convert type to layout
        const resolved = self.runtime_types.resolveVar(type_var);
        const layout_val = try self.typeToLayout(resolved.desc.content);

        // Cache it
        try self.layout_cache.put(type_var, layout_val);
        return layout_val;
    }


    /// Convert type content to layout
    fn typeToLayout(self: *Interpreter, content: Content) EvalError!Layout {
        return switch (content) {
            .structure => |structure| switch (structure) {
                .num => |num| switch (num) {
                    .int_precision => |prec| switch (prec) {
                        .i8 => Layout.int(.i8),
                        .i16 => Layout.int(.i16),
                        .i32 => Layout.int(.i32),
                        .i64 => Layout.int(.i64),
                        .i128 => Layout.int(.i128),
                        .u8 => Layout.int(.u8),
                        .u16 => Layout.int(.u16),
                        .u32 => Layout.int(.u32),
                        .u64 => Layout.int(.u64),
                        .u128 => Layout.int(.u128),
                    },
                    .frac_precision => |prec| switch (prec) {
                        .f32 => Layout.frac(.f32),
                        .f64 => Layout.frac(.f64),
                        .dec => Layout.frac(.dec),
                    },
                    .num_compact => Layout.int(.i64), // Default
                    .int_poly, .num_poly, .int_unbound, .num_unbound, .frac_unbound, .frac_poly => {
                        // Default to i64 for unresolved numeric types
                        return Layout.int(.i64);
                    },
                },
                .str => Layout.str(),
                .list => |elem_var| {
                    const elem_layout = try self.getLayoutForVar(elem_var);
                    const elem_idx = try self.layout_store.insertLayout(elem_layout);
                    const list_idx = try self.layout_store.insertList(elem_idx);
                    return self.layout_store.getLayout(list_idx);
                },
                .box => |elem_var| {
                    const elem_layout = try self.getLayoutForVar(elem_var);
                    const elem_idx = try self.layout_store.insertLayout(elem_layout);
                    const box_idx = try self.layout_store.insertBox(elem_idx);
                    return self.layout_store.getLayout(box_idx);
                },
                .record => |record| {
                    _ = record.fields;
                    // For now, return a simple placeholder for records
                    return Layout.int(.u64);
                },
                .record_unbound => |fields_range| {
                    _ = fields_range;
                    // For now, return a simple placeholder for records
                    return Layout.int(.u64);
                },
                .tag_union => |_| {
                    // For tag unions, we need a more complex layout representation
                    // For now, return a simple scalar as placeholder
                    // TODO: Implement proper tag union layout
                    return Layout.int(.u64);
                },
                .tuple => |tuple| {
                    // Get layouts for all tuple elements
                    const elems = self.runtime_types.sliceVars(tuple.elems);
                    var elem_layouts = try self.allocator.alloc(layout.Idx, elems.len);
                    defer self.allocator.free(elem_layouts);

                    var max_alignment: std.mem.Alignment = .@"1";
                    var current_offset: u32 = 0;

                    // Calculate layouts and alignment for each element
                    for (elems, 0..) |elem, i| {
                        const elem_layout = try self.getLayoutForVar(elem);
                        const elem_idx = try self.layout_store.insertLayout(elem_layout);
                        elem_layouts[i] = elem_idx;

                        const elem_align = elem_layout.alignment(base.target.Target.native.target_usize);
                        max_alignment = max_alignment.max(elem_align);

                        // Align offset and add size
                        current_offset = @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(elem_align.toByteUnits()))));
                        current_offset += self.layout_store.layoutSize(elem_layout);
                    }

                    // Store tuple fields
                    const fields_start_int = self.layout_store.tuple_fields.len();
                    for (elem_layouts, 0..) |elem_idx, i| {
                        _ = try self.layout_store.tuple_fields.append(self.allocator, .{
                            .index = @intCast(i),
                            .layout = elem_idx,
                        });
                    }

                    // Create the tuple data
                    const total_size = @as(u32, @intCast(std.mem.alignForward(u32, current_offset, @as(u32, @intCast(max_alignment.toByteUnits())))));
                    const fields_range = collections.NonEmptyRange{
                        .start = @intCast(fields_start_int),
                        .count = @intCast(elem_layouts.len),
                    };

                    const tuple_idx = layout.TupleIdx{ .int_idx = @intCast(self.layout_store.tuple_data.len()) };
                    _ = try self.layout_store.tuple_data.append(self.allocator, .{
                        .size = total_size,
                        .fields = fields_range,
                    });

                    return Layout.tuple(max_alignment, tuple_idx);
                },
                .fn_pure, .fn_effectful, .fn_unbound => Layout.opaquePtr(), // Functions are opaque pointers
                .nominal_type => |_| {
                    // For nominal types like Bool, return a bool layout
                    // TODO: Properly distinguish between different nominal types
                    return Layout.boolType();
                },
                .list_unbound => error.LayoutConversionFailed,
                .record_poly => |rec_poly| {
                    // For polymorphic records, use the base record
                    const fields = self.runtime_types.getRecordFieldsSlice(rec_poly.record.fields);
                    var layout_fields = try self.allocator.alloc(layout.RecordField, fields.len);
                    const field_names = fields.items(.name);
                    const field_types = fields.items(.var_);
                    for (0..fields.len) |i| {
                        const field_layout = try self.getLayoutForVar(field_types[i]);
                        const field_idx = try self.layout_store.insertLayout(field_layout);
                        layout_fields[i] = .{
                            .name = field_names[i],
                            .layout = field_idx,
                        };
                    }

                    // Similar to tuple, need to create proper record layout
                    // For now, return a simple placeholder
                    return Layout.int(.u64);
                },
                .empty_tag_union => Layout.boxOfZst(), // Zero-sized type
                .empty_record => Layout.boxOfZst(), // Zero-sized type
            },
            .flex_var, .rigid_var => error.LayoutConversionFailed,
            .err => error.TypeMismatch,
            .alias => |alias| {
                // Resolve the first type variable in the alias to get the actual type
                const vars = self.runtime_types.sliceVars(alias.vars.nonempty);
                if (vars.len > 0) {
                    const first_var = vars[0];
                    const resolved = self.runtime_types.resolveVar(first_var);
                    return self.typeToLayout(resolved.desc.content);
                }
                return error.LayoutConversionFailed;
            },
        };
    }

    /// Push a value onto both stacks
    fn pushValue(self: *Interpreter, bytes: []const u8, type_var: Var) !StackValue {
        // Get layout for alignment calculation
        const layout_val = try self.getLayoutForVar(type_var);
        const alignment = layout_val.alignment(target_usize);
        const size = self.layout_store.layoutSize(layout_val);

        // Allocate space on stack
        const ptr = try self.stack_memory.alloca(@intCast(size), alignment);

        // Calculate offset from start of stack
        const offset = @intFromPtr(ptr) - @intFromPtr(self.stack_memory.start);

        // Copy bytes
        @memcpy(@as([*]u8, @ptrCast(ptr))[0..bytes.len], bytes);

        // Push type
        try self.type_stack.append(type_var);

        // Push value metadata
        try self.value_stack.append(.{
            .offset = offset,
            .size = size,
            .alignment = alignment,
        });

        return StackValue{
            .ptr = ptr,
            .type_var = type_var,
        };
    }

    /// Pop a value from both stacks
    fn popStackValue(self: *Interpreter) !StackValue {
        if (self.value_stack.items.len == 0) {
            return error.InvalidStackState;
        }

        const value = self.value_stack.pop() orelse return error.InvalidStackState;
        const type_var = self.type_stack.pop() orelse return error.InvalidStackState;

        const ptr = self.getStackPtr(value.offset);
        self.stack_memory.restore(@ptrCast(ptr));

        return StackValue{
            .ptr = ptr,
            .type_var = type_var,
        };
    }

    /// Get a pointer to the stack memory at the given offset
    fn getStackPtr(self: *const Interpreter, offset: usize) *anyopaque {
        const ptr = @as([*]u8, @ptrCast(self.stack_memory.start)) + offset;
        return @ptrCast(ptr);
    }

    /// Peek at a value on the stack
    fn peekStackValue(self: *Interpreter, depth: usize) !StackValue {
        const index = self.value_stack.items.len - depth;
        const value = self.value_stack.items[index];

        // Get pointer to the value in stack memory
        const ptr = @as([*]u8, @ptrCast(self.stack_memory.start)) + value.offset;

        const type_var = self.type_stack.items[index];
        return StackValue{
            .ptr = @ptrCast(ptr),
            .type_var = type_var,
        };
    }

    /// Schedule work to be done
    fn schedule_work(self: *Interpreter, item: WorkItem) void {
        self.work_stack.append(item) catch unreachable;
    }

    /// Main evaluation entry point
    pub fn eval(self: *Interpreter, expr_idx: CIR.Expr.Idx, roc_ops: *RocOps) EvalError!StackValue {
        // Clear stacks
        self.work_stack.clearRetainingCapacity();
        self.value_stack.clearRetainingCapacity();
        self.type_stack.clearRetainingCapacity();
        self.bindings_stack.clearRetainingCapacity();
        self.frame_stack.clearRetainingCapacity();
        self.has_crashed = false;

        // Schedule initial expression
        self.schedule_work(.{
            .kind = .w_eval_expr,
            .expr_idx = expr_idx,
            .module_ctx = self.env,
            .extra = .{ .none = {} },
        });

        // Process work items
        while (self.work_stack.pop()) |work| {
            if (self.has_crashed) {
                return error.Crash;
            }

            try self.processWorkItem(work, roc_ops);
        }

        // Get final result before clearing stacks
        if (self.value_stack.items.len == 0) {
            return error.InvalidStackState;
        }

        const final_value = self.value_stack.items[self.value_stack.items.len - 1];
        const final_type_idx = self.type_stack.items.len - 1;

        // Cache the result type for later use
        self.cached_result_type = self.type_stack.items[final_type_idx];

        const ptr = self.getStackPtr(final_value.offset);

        return StackValue{
            .ptr = ptr,
            .type_var = self.cached_result_type.?,
        };
    }

    /// Process a single work item
    fn processWorkItem(self: *Interpreter, work: WorkItem, roc_ops: *RocOps) !void {
        switch (work.kind) {
            .w_eval_expr => try self.evalExpr(work.expr_idx, work.module_ctx),
            .w_lambda_call => try self.handleLambdaCall(work.expr_idx, work.extra.arg_count, roc_ops),
            .w_lambda_return => try self.handleLambdaReturn(roc_ops),
            .w_binop_add => try self.completeBinop(.add, roc_ops),
            .w_binop_sub => try self.completeBinop(.sub, roc_ops),
            .w_binop_mul => try self.completeBinop(.mul, roc_ops),
            .w_binop_div => try self.completeBinop(.div, roc_ops),
            .w_binop_div_trunc => try self.completeBinop(.div_trunc, roc_ops),
            .w_binop_rem => try self.completeBinop(.rem, roc_ops),
            .w_binop_eq => try self.completeBinop(.eq, roc_ops),
            .w_binop_ne => try self.completeBinop(.ne, roc_ops),
            .w_binop_gt => try self.completeBinop(.gt, roc_ops),
            .w_binop_lt => try self.completeBinop(.lt, roc_ops),
            .w_binop_ge => try self.completeBinop(.ge, roc_ops),
            .w_binop_le => try self.completeBinop(.le, roc_ops),
            .w_binop_and => try self.completeBinop(.bool_and, roc_ops),
            .w_binop_or => try self.completeBinop(.bool_or, roc_ops),
            .w_unary_minus => try self.completeUnaryMinus(),
            .w_unary_not => try self.completeUnaryNot(),
            .w_eval_record_fields => try self.handleRecordFields(work.expr_idx, work.extra.current_field_idx),
            .w_dot_access => try self.handleDotAccess(work.extra.dot_access_field_name),
            .w_crash => {
                const msg = work.module_ctx.getString(work.extra.crash_msg);
                roc_ops.crash(msg);
                self.has_crashed = true;
                self.crash_message = msg;
            },
            .w_str_interpolate_combine => try self.handleStringInterpolateCombine(work.extra.segment_count, roc_ops),
            .w_str_interpolate_segments => {},
            .w_eval_tuple_elements => try self.evaluateTuple(work.expr_idx, work.extra.current_element_idx, roc_ops),
            .w_let_bind => try self.handleLetBind(work.extra.decl_pattern_idx),
            .w_block_cleanup => {
                const cleanup_data = work.extra.block_cleanup;
                try self.handleBlockCleanup(cleanup_data, roc_ops);
            },
            .w_if_then_else => try self.handleIfThenElse(work.extra.if_data),
            .w_when_branch => try self.handleWhenBranch(work.extra.when_data, work.module_ctx),
        }
    }

    /// Evaluate an expression
    fn evalExpr(self: *Interpreter, expr_idx: CIR.Expr.Idx, module_ctx: *ModuleEnv) !void {
        const expr = module_ctx.store.getExpr(expr_idx);

        switch (expr) {
            .e_lookup_local => |local| {
                const pattern_idx = local.pattern_idx;
                // Look up the binding
                if (self.lookupBinding(pattern_idx)) |value| {
                    // Get the type from the binding
                    const type_var = value.type_var;
                    const layout_val = try self.getLayoutForVar(type_var);
                    const size = self.layout_store.layoutSize(layout_val);

                    // Copy the value
                    const bytes = @as([*]const u8, @ptrCast(value.ptr))[0..size];
                    _ = try self.pushValue(bytes, type_var);
                } else if (try self.lookupCapture(pattern_idx)) |value| {
                    // Get the type from the capture
                    const type_var = value.type_var;
                    const layout_val = try self.getLayoutForVar(type_var);
                    const size = self.layout_store.layoutSize(layout_val);

                    // Copy the value
                    const bytes = @as([*]const u8, @ptrCast(value.ptr))[0..size];
                    _ = try self.pushValue(bytes, type_var);
                } else {
                    return error.UnboundVariable;
                }
            },
            .e_int => |int_expr| {
                // Get the type variable for this expression
                const compile_var = ModuleEnv.varFrom(expr_idx);
                const runtime_var = try self.translateTypeVar(module_ctx, compile_var);

                // Determine the concrete numeric type
                const resolved = self.runtime_types.resolveVar(runtime_var);
                const num_type = if (resolved.desc.content == .structure)
                    resolved.desc.content.structure.num
                else
                    types.Num{ .int_precision = .i64 }; // Default

                // Convert to bytes based on type
                const int_val = int_expr.value.toI128();
                switch (num_type) {
                    .int_precision => |prec| switch (prec) {
                        .i8 => {
                            const val = @as(i8, @intCast(int_val));
                            _ = try self.pushValue(std.mem.asBytes(&val), runtime_var);
                        },
                        .i16 => {
                            const val = @as(i16, @intCast(int_val));
                            _ = try self.pushValue(std.mem.asBytes(&val), runtime_var);
                        },
                        .i32 => {
                            const val = @as(i32, @intCast(int_val));
                            _ = try self.pushValue(std.mem.asBytes(&val), runtime_var);
                        },
                        .i64 => {
                            const val = @as(i64, @intCast(int_val));
                            _ = try self.pushValue(std.mem.asBytes(&val), runtime_var);
                        },
                        .i128 => {
                            const val = int_val;
                            _ = try self.pushValue(std.mem.asBytes(&val), runtime_var);
                        },
                        .u8 => {
                            const val = @as(u8, @intCast(int_val));
                            _ = try self.pushValue(std.mem.asBytes(&val), runtime_var);
                        },
                        .u16 => {
                            const val = @as(u16, @intCast(int_val));
                            _ = try self.pushValue(std.mem.asBytes(&val), runtime_var);
                        },
                        .u32 => {
                            const val = @as(u32, @intCast(int_val));
                            _ = try self.pushValue(std.mem.asBytes(&val), runtime_var);
                        },
                        .u64 => {
                            const val = @as(u64, @intCast(int_val));
                            _ = try self.pushValue(std.mem.asBytes(&val), runtime_var);
                        },
                        .u128 => {
                            const val = @as(u128, @intCast(int_val));
                            _ = try self.pushValue(std.mem.asBytes(&val), runtime_var);
                        },
                    },
                    else => {
                        // Default for unbound types
                        const val = @as(i64, @intCast(int_val));
                        _ = try self.pushValue(std.mem.asBytes(&val), runtime_var);
                    },
                }
            },
            .e_frac_f32 => |frac| {
                const compile_var = ModuleEnv.varFrom(expr_idx);
                const runtime_var = try self.translateTypeVar(module_ctx, compile_var);
                const val = frac.value;
                _ = try self.pushValue(std.mem.asBytes(&val), runtime_var);
            },
            .e_frac_f64 => |frac| {
                const compile_var = ModuleEnv.varFrom(expr_idx);
                const runtime_var = try self.translateTypeVar(module_ctx, compile_var);
                const val = frac.value;
                _ = try self.pushValue(std.mem.asBytes(&val), runtime_var);
            },
            .e_frac_dec => |frac| {
                const compile_var = ModuleEnv.varFrom(expr_idx);
                const runtime_var = try self.translateTypeVar(module_ctx, compile_var);

                // For Dec type, we just store the RocDec value
                _ = try self.pushValue(std.mem.asBytes(&frac.value), runtime_var);
            },
            .e_str_segment => |segment| {
                const compile_var = ModuleEnv.varFrom(expr_idx);
                const runtime_var = try self.translateTypeVar(module_ctx, compile_var);

                const str_literal = module_ctx.getString(segment.literal);

                // Create a RocStr from the string literal
                var roc_str: RocStr = undefined;
                const SMALL_STRING_SIZE = @sizeOf(RocStr);

                if (str_literal.len < SMALL_STRING_SIZE) {
                    // Small string - store inline
                    roc_str = RocStr.empty();
                    const str_bytes = @as([*]u8, @ptrCast(&roc_str));
                    @memcpy(str_bytes[0..str_literal.len], str_literal);
                    // Set the length in the last byte with the small string flag
                    str_bytes[SMALL_STRING_SIZE - 1] = @as(u8, @intCast(str_literal.len)) | 0b1000_0000;
                } else {
                    // Big string - use heap allocation
                    roc_str = RocStr{
                        .bytes = if (str_literal.len > 0) @constCast(str_literal.ptr) else null,
                        .length = str_literal.len,
                        .capacity_or_alloc_ptr = str_literal.len,
                    };
                }
                _ = try self.pushValue(std.mem.asBytes(&roc_str), runtime_var);
            },
            .e_call => |call| {
                const all_exprs = module_ctx.store.sliceExpr(call.args);
                if (all_exprs.len == 0) {
                    return error.TypeMismatch;
                }

                const function_expr = all_exprs[0];
                const arg_exprs = all_exprs[1..];
                const arg_count: u32 = @intCast(arg_exprs.len);

                // Schedule lambda call
                self.schedule_work(.{
                    .kind = .w_lambda_call,
                    .expr_idx = expr_idx,
                    .module_ctx = module_ctx,
                    .extra = .{ .arg_count = arg_count },
                });

                // Schedule argument evaluations in reverse
                var i = arg_exprs.len;
                while (i > 0) {
                    i -= 1;
                    self.schedule_work(.{
                        .kind = .w_eval_expr,
                        .expr_idx = arg_exprs[i],
                        .module_ctx = module_ctx,
                        .extra = .{ .none = {} },
                    });
                }

                // Schedule function evaluation
                self.schedule_work(.{
                    .kind = .w_eval_expr,
                    .expr_idx = function_expr,
                    .module_ctx = module_ctx,
                    .extra = .{ .none = {} },
                });
            },
            .e_binop => |binop| {
                const work_kind = switch (binop.op) {
                    .add => WorkItemKind.w_binop_add,
                    .sub => WorkItemKind.w_binop_sub,
                    .mul => WorkItemKind.w_binop_mul,
                    .div => WorkItemKind.w_binop_div,
                    .div_trunc => WorkItemKind.w_binop_div_trunc,
                    .rem => WorkItemKind.w_binop_rem,
                    .pow => WorkItemKind.w_binop_mul, // Use mul as placeholder for pow
                    .eq => WorkItemKind.w_binop_eq,
                    .ne => WorkItemKind.w_binop_ne,
                    .gt => WorkItemKind.w_binop_gt,
                    .lt => WorkItemKind.w_binop_lt,
                    .ge => WorkItemKind.w_binop_ge,
                    .le => WorkItemKind.w_binop_le,
                    .@"and" => WorkItemKind.w_binop_and,
                    .@"or" => WorkItemKind.w_binop_or,
                    .pipe_forward => WorkItemKind.w_binop_add, // Use add as placeholder
                    .null_coalesce => WorkItemKind.w_binop_or, // Use or as placeholder
                };

                self.schedule_work(.{
                    .kind = work_kind,
                    .expr_idx = expr_idx,
                    .module_ctx = module_ctx,
                    .extra = .{ .none = {} },
                });

                // Schedule operands (right then left for correct stack order)
                self.schedule_work(.{
                    .kind = .w_eval_expr,
                    .expr_idx = binop.rhs,
                    .module_ctx = module_ctx,
                    .extra = .{ .none = {} },
                });
                self.schedule_work(.{
                    .kind = .w_eval_expr,
                    .expr_idx = binop.lhs,
                    .module_ctx = module_ctx,
                    .extra = .{ .none = {} },
                });
            },
            .e_unary_minus => |minus| {
                self.schedule_work(.{
                    .kind = .w_unary_minus,
                    .expr_idx = expr_idx,
                    .module_ctx = module_ctx,
                    .extra = .{ .none = {} },
                });
                self.schedule_work(.{
                    .kind = .w_eval_expr,
                    .expr_idx = minus.expr,
                    .module_ctx = module_ctx,
                    .extra = .{ .none = {} },
                });
            },
            .e_lambda => |lambda| {
                // Create closure
                const closure = try self.createClosure(lambda, module_ctx);

                // Get type
                const compile_var = ModuleEnv.varFrom(expr_idx);
                const runtime_var = try self.translateTypeVar(module_ctx, compile_var);

                _ = try self.pushValue(std.mem.asBytes(&closure), runtime_var);
            },
            .e_record => |record| {
                const field_idxs = module_ctx.store.sliceRecordFields(record.fields);

                if (field_idxs.len == 0) {
                    // Empty record
                    const compile_var = ModuleEnv.varFrom(expr_idx);
                    const runtime_var = try self.translateTypeVar(module_ctx, compile_var);
                    _ = try self.pushValue(&[_]u8{}, runtime_var);
                } else {
                    // Schedule field evaluation
                    self.schedule_work(.{
                        .kind = .w_eval_record_fields,
                        .expr_idx = expr_idx,
                        .module_ctx = module_ctx,
                        .extra = .{ .current_field_idx = 0 },
                    });

                    // Evaluate all field values in reverse
                    var i = field_idxs.len;
                    while (i > 0) {
                        i -= 1;
                        const field = module_ctx.store.getRecordField(field_idxs[i]);
                        self.schedule_work(.{
                            .kind = .w_eval_expr,
                            .expr_idx = field.value,
                            .module_ctx = module_ctx,
                            .extra = .{ .none = {} },
                        });
                    }
                }
            },
            .e_dot_access => |access| {
                self.schedule_work(.{
                    .kind = .w_dot_access,
                    .expr_idx = expr_idx,
                    .module_ctx = module_ctx,
                    .extra = .{ .dot_access_field_name = access.field_name },
                });

                self.schedule_work(.{
                    .kind = .w_eval_expr,
                    .expr_idx = access.receiver,
                    .module_ctx = module_ctx,
                    .extra = .{ .none = {} },
                });
            },
            .e_tag => |tag| {
                const payloads = module_ctx.store.sliceExpr(tag.args);

                // Get type
                const compile_var = ModuleEnv.varFrom(expr_idx);
                const runtime_var = try self.translateTypeVar(module_ctx, compile_var);

                // Allocate tag union
                const layout_val = try self.getLayoutForVar(runtime_var);
                const size = self.layout_store.layoutSize(layout_val);
                const align_val = layout_val.alignment(target_usize);

                const ptr = try self.stack_memory.alloca(@intCast(size), align_val);

                // Set discriminant
                const tag_idx = try self.findTagIndex(runtime_var, tag.name);
                @as(*u16, @ptrCast(@alignCast(ptr))).* = @intCast(tag_idx);

                // Copy payload values
                _ = 2; // payload_offset: After discriminant
                for (payloads) |payload_expr| {
                    // This is simplified - real implementation would evaluate payloads properly
                    _ = payload_expr;
                }

                _ = try self.pushValue(@as([*]u8, @ptrCast(ptr))[0..size], runtime_var);
            },
            .e_if => |if_expr| {
                // Get first branch (simplified - real implementation would handle multiple branches)
                const branches = module_ctx.store.sliceIfBranches(if_expr.branches);
                if (branches.len > 0) {
                    const first_branch = module_ctx.store.getIfBranch(branches[0]);

                    // Schedule if-then-else evaluation
                    self.schedule_work(.{
                        .kind = .w_if_then_else,
                        .expr_idx = expr_idx,
                        .module_ctx = module_ctx,
                        .extra = .{
                            .if_data = .{
                                .then_branch = first_branch.body,
                                .else_branch = if_expr.final_else,
                            },
                        },
                    });

                    // Evaluate condition
                    self.schedule_work(.{
                        .kind = .w_eval_expr,
                        .expr_idx = first_branch.cond,
                        .module_ctx = module_ctx,
                        .extra = .{ .none = {} },
                    });
                }
            },
            .e_match => |match| {
                // Schedule when evaluation
                self.schedule_work(.{
                    .kind = .w_when_branch,
                    .expr_idx = expr_idx,
                    .module_ctx = module_ctx,
                    .extra = .{
                        .when_data = .{
                            .branches = match.branches,
                            .current_branch = 0,
                        },
                    },
                });

                // Evaluate scrutinee
                self.schedule_work(.{
                    .kind = .w_eval_expr,
                    .expr_idx = match.cond,
                    .module_ctx = module_ctx,
                    .extra = .{ .none = {} },
                });
            },
            .e_block => |block| {
                // Schedule body evaluation and cleanup
                const bindings_before = self.bindings_stack.items.len;
                const values_before = self.value_stack.items.len;

                self.schedule_work(.{
                    .kind = .w_block_cleanup,
                    .expr_idx = expr_idx,
                    .module_ctx = module_ctx,
                    .extra = .{
                        .block_cleanup = .{
                            .bindings_to_keep = @intCast(bindings_before),
                            .values_to_keep = @intCast(values_before),
                        },
                    },
                });

                // Schedule body
                self.schedule_work(.{
                    .kind = .w_eval_expr,
                    .expr_idx = block.final_expr,
                    .module_ctx = module_ctx,
                    .extra = .{ .none = {} },
                });

                // Schedule bindings
                const stmts = module_ctx.store.sliceStatements(block.stmts);
                var i = stmts.len;
                while (i > 0) {
                    i -= 1;
                    const stmt = module_ctx.store.getStatement(stmts[i]);
                    switch (stmt) {
                        .s_decl => |decl| {
                            self.schedule_work(.{
                                .kind = .w_let_bind,
                                .expr_idx = decl.expr,
                                .module_ctx = module_ctx,
                                .extra = .{ .decl_pattern_idx = decl.pattern },
                            });

                            self.schedule_work(.{
                                .kind = .w_eval_expr,
                                .expr_idx = decl.expr,
                                .module_ctx = module_ctx,
                                .extra = .{ .none = {} },
                            });
                        },
                        // Skip other statements for now
                        .s_var, .s_reassign, .s_crash, .s_dbg, .s_expr, .s_expect, .s_for, .s_runtime_error, .s_return, .s_import, .s_alias_decl, .s_nominal_decl, .s_type_anno => {},
                    }
                }
            },
            .e_tuple => |tuple| {
                const elems = module_ctx.store.sliceExpr(tuple.elems);

                if (elems.len == 0) {
                    // Empty tuple (unit)
                    const compile_var = ModuleEnv.varFrom(expr_idx);
                    const runtime_var = try self.translateTypeVar(module_ctx, compile_var);
                    _ = try self.pushValue(&[_]u8{}, runtime_var);
                } else {
                    // Schedule tuple element evaluation
                    self.schedule_work(.{
                        .kind = .w_eval_tuple_elements,
                        .expr_idx = expr_idx,
                        .module_ctx = module_ctx,
                        .extra = .{ .current_element_idx = 0 },
                    });

                    // Evaluate elements in reverse
                    var i = elems.len;
                    while (i > 0) {
                        i -= 1;
                        self.schedule_work(.{
                            .kind = .w_eval_expr,
                            .expr_idx = elems[i],
                            .module_ctx = module_ctx,
                            .extra = .{ .none = {} },
                        });
                    }
                }
            },
            .e_crash => |crash| {
                self.schedule_work(.{
                    .kind = .w_crash,
                    .expr_idx = expr_idx,
                    .module_ctx = module_ctx,
                    .extra = .{ .crash_msg = crash.msg },
                });
            },
            .e_str => |str| {
                const segments = module_ctx.store.sliceExpr(str.span);

                if (segments.len == 1) {
                    // Optimization: single segment strings don't need combining
                    self.schedule_work(.{
                        .kind = .w_eval_expr,
                        .expr_idx = segments[0],
                        .module_ctx = module_ctx,
                        .extra = .{ .none = {} },
                    });
                } else {
                    self.schedule_work(.{
                        .kind = .w_str_interpolate_combine,
                        .expr_idx = expr_idx,
                        .module_ctx = module_ctx,
                        .extra = .{ .segment_count = @intCast(segments.len) },
                    });

                    // Mark segments boundary
                    self.schedule_work(.{
                        .kind = .w_str_interpolate_segments,
                        .expr_idx = expr_idx,
                        .module_ctx = module_ctx,
                        .extra = .{ .none = {} },
                    });

                    // Evaluate segments in reverse
                    var i = segments.len;
                    while (i > 0) {
                        i -= 1;
                        self.schedule_work(.{
                            .kind = .w_eval_expr,
                            .expr_idx = segments[i],
                            .module_ctx = module_ctx,
                            .extra = .{ .none = {} },
                        });
                    }
                }
            },
            .e_lookup_external => |_| {
                // External lookups are complex - simplified for now
                const compile_var = ModuleEnv.varFrom(expr_idx);
                const runtime_var = try self.translateTypeVar(module_ctx, compile_var);
                _ = try self.pushValue(&[_]u8{0}, runtime_var);
            },
            .e_zero_argument_tag => |tag| {
                // Handle zero-argument tags like True, False
                const compile_var = ModuleEnv.varFrom(expr_idx);
                const runtime_var = try self.translateTypeVar(module_ctx, compile_var);

                // Get the tag name - try both name and closure_name
                const tag_name = module_ctx.getIdent(tag.name);
                const closure_name = module_ctx.getIdent(tag.closure_name);

                // For Bool type: False = 0, True = 1
                // Check both possible names
                const is_true = std.mem.eql(u8, tag_name, "True") or
                               std.mem.eql(u8, closure_name, "True");
                const tag_value: u8 = if (is_true) 1 else 0;
                _ = try self.pushValue(&[_]u8{tag_value}, runtime_var);
            },
            .e_unary_not => |unary| {
                // Schedule completion and operand evaluation
                self.schedule_work(.{
                    .kind = .w_unary_not,
                    .expr_idx = expr_idx,
                    .module_ctx = module_ctx,
                    .extra = .{ .none = {} },
                });
                self.schedule_work(.{
                    .kind = .w_eval_expr,
                    .expr_idx = unary.expr,
                    .module_ctx = module_ctx,
                    .extra = .{ .none = {} },
                });
            },
            .e_empty_list, .e_empty_record, .e_list, .e_closure, .e_dbg, .e_expect, .e_runtime_error, .e_nominal, .e_nominal_external, .e_ellipsis, .e_dec_small => {
                // Not implemented yet - push placeholder
                const compile_var = ModuleEnv.varFrom(expr_idx);
                const runtime_var = try self.translateTypeVar(module_ctx, compile_var);
                _ = try self.pushValue(&[_]u8{0}, runtime_var);
            },
        }
    }

    /// Handle lambda call
    fn handleLambdaCall(self: *Interpreter, expr_idx: CIR.Expr.Idx, arg_count: u32, roc_ops: *RocOps) !void {
        _ = expr_idx;

        // Get closure from stack
        const closure_value = try self.peekStackValue(1);
        const closure_layout = try self.getLayoutForVar(closure_value.type_var);

        if (closure_layout.tag != .closure) {
            return error.InvalidStackState;
        }

        const closure: *const layout.Closure = @ptrCast(@alignCast(closure_value.ptr.?));

        // Get the module where the closure was defined
        const closure_module = if (closure.module_ptr) |ptr|
            @as(*ModuleEnv, @ptrCast(@alignCast(ptr)))
        else
            self.env;

        // Create call frame
        const frame = CallFrame{
            .value_base = self.value_stack.items.len - arg_count - 1,
            .type_base = self.type_stack.items.len - arg_count - 1,
            .arg_count = arg_count,
            .return_addr = self.work_stack.items.len,
            .module_ctx = closure_module,
            .saved_bindings_count = self.bindings_stack.items.len,
        };
        try self.frame_stack.append(frame);

        // Get lambda expression
        const lambda_expr = closure_module.store.getExpr(closure.lambda_expr_idx);
        const lambda = lambda_expr.e_lambda;

        // Perform runtime type unification for polymorphic functions
        const lambda_type_var = try self.translateTypeVar(closure_module, ModuleEnv.varFrom(closure.lambda_expr_idx));
        const resolved_lambda = self.runtime_types.resolveVar(lambda_type_var);

        if (resolved_lambda.desc.content == .structure) {
            const func = switch (resolved_lambda.desc.content.structure) {
                .fn_pure, .fn_effectful, .fn_unbound => |f| f,
                else => return error.TypeMismatch,
            };

            // Unify parameter types with argument types
            const param_types = self.runtime_types.sliceVars(func.args);
            for (param_types, 0..) |param_type, i| {
                const arg_type = self.type_stack.items[frame.type_base + 1 + i]; // +1 for closure

                // Unify parameter with argument
                const result = try check.unifier.unifyWithContext(
                    closure_module,
                    self.runtime_types,
                    self.problem_store,
                    self.snapshot_store,
                    self.unify_scratch,
                    self.occurs_scratch,
                    param_type,
                    arg_type,
                    false,
                );

                if (!result.isOk()) {
                    return error.UnificationFailed;
                }
            }
        }

        // Bind parameters
        const patterns = closure_module.store.slicePatterns(lambda.args);
        for (patterns, 0..) |pattern_idx, i| {
            const arg_value = self.value_stack.items[frame.value_base + 1 + i]; // +1 to skip closure
            const arg_type = self.type_stack.items[frame.type_base + 1 + i];

            try self.bindings_stack.append(.{
                .pattern_idx = pattern_idx,
                .value = StackValue{
                    .ptr = @ptrCast(@as([*]u8, @ptrCast(self.stack_memory.start)) + arg_value.offset),
                    .type_var = arg_type,
                },
            });
        }

        // Bind captures (simplified - real implementation would bind actual captures)

        // Schedule return and body evaluation
        self.schedule_work(.{
            .kind = .w_lambda_return,
            .expr_idx = closure.lambda_expr_idx,
            .module_ctx = closure_module,
            .extra = .{ .none = {} },
        });

        self.schedule_work(.{
            .kind = .w_eval_expr,
            .expr_idx = lambda.body,
            .module_ctx = closure_module,
            .extra = .{ .none = {} },
        });

        _ = roc_ops;
    }

    /// Handle lambda return
    fn handleLambdaReturn(self: *Interpreter, roc_ops: *RocOps) !void {
        _ = roc_ops;

        const frame = self.frame_stack.pop() orelse return error.InvalidCallStack;

        // Get return value
        const return_value = try self.popStackValue();

        // Clean up arguments and closure
        var i: usize = 0;
        while (i <= frame.arg_count) : (i += 1) {
            _ = self.value_stack.pop();
            _ = self.type_stack.pop();
        }
        // Stack memory is managed by restore calls

        // Restore bindings
        self.bindings_stack.shrinkRetainingCapacity(frame.saved_bindings_count);

        // Push return value with its type
        const return_type = return_value.type_var;
        const return_layout = try self.getLayoutForVar(return_type);
        const size = self.layout_store.layoutSize(return_layout);

        const bytes = @as([*]const u8, @ptrCast(return_value.ptr))[0..size];
        _ = try self.pushValue(bytes, return_type);
    }

    /// Complete binary operation
    fn completeBinop(self: *Interpreter, op: enum {
        add, sub, mul, div, div_trunc, rem,
        eq, ne, gt, lt, ge, le,
        bool_and, bool_or
    }, roc_ops: *RocOps) !void {
        _ = roc_ops;

        const rhs = try self.popStackValue();
        const lhs = try self.popStackValue();

        // Get types
        const lhs_type = lhs.type_var;
        const rhs_type = rhs.type_var;

        // Unify types
        const result = try check.unifier.unifyWithContext(
            self.env,
            self.runtime_types,
            self.problem_store,
            self.snapshot_store,
            self.unify_scratch,
            self.occurs_scratch,
            lhs_type,
            rhs_type,
            false,
        );

        if (!result.isOk()) {
            return error.UnificationFailed;
        }

        // Determine result type and perform operation
        const result_type = lhs_type; // After unification, both types are the same
        const layout_val = try self.getLayoutForVar(result_type);

        // Perform operation based on layout
        switch (layout_val.tag) {
            .scalar => {
                const scalar = layout_val.data.scalar;
                switch (scalar.tag) {
                    .int => {
                        const lhs_val = @as(*i64, @ptrCast(@alignCast(lhs.ptr))).*;
                        const rhs_val = @as(*i64, @ptrCast(@alignCast(rhs.ptr))).*;

                const result_val: i64 = switch (op) {
                    .add => try std.math.add(i64, lhs_val, rhs_val),
                    .sub => try std.math.sub(i64, lhs_val, rhs_val),
                    .mul => try std.math.mul(i64, lhs_val, rhs_val),
                    .div => @divTrunc(lhs_val, rhs_val),
                    .div_trunc => @divTrunc(lhs_val, rhs_val),
                    .rem => @rem(lhs_val, rhs_val),
                    .eq => if (lhs_val == rhs_val) 1 else 0,
                    .ne => if (lhs_val != rhs_val) 1 else 0,
                    .gt => if (lhs_val > rhs_val) 1 else 0,
                    .lt => if (lhs_val < rhs_val) 1 else 0,
                    .ge => if (lhs_val >= rhs_val) 1 else 0,
                    .le => if (lhs_val <= rhs_val) 1 else 0,
                    else => return error.InvalidNumericOp,
                };

                // Push result with appropriate type
                const result_type_final = if (op == .eq or op == .ne or op == .gt or
                                            op == .lt or op == .ge or op == .le)
                    try self.runtime_types.freshFromContent(.{ .structure = .{ .num = .{ .int_precision = .u8 } } }) // Bool
                else
                    result_type;

                _ = try self.pushValue(std.mem.asBytes(&result_val), result_type_final);
                    },
                    .bool => {
                        const lhs_val = @as(*u8, @ptrCast(@alignCast(lhs.ptr))).* != 0;
                        const rhs_val = @as(*u8, @ptrCast(@alignCast(rhs.ptr))).* != 0;

                        const result_val: u8 = switch (op) {
                            .bool_and => if (lhs_val and rhs_val) 1 else 0,
                            .bool_or => if (lhs_val or rhs_val) 1 else 0,
                            .eq => if (lhs_val == rhs_val) 1 else 0,
                            .ne => if (lhs_val != rhs_val) 1 else 0,
                            else => return error.InvalidNumericOp,
                        };

                        _ = try self.pushValue(&[_]u8{result_val}, result_type);
                    },
                    .str => {
                        // String comparison
                        const lhs_str = @as(*RocStr, @ptrCast(@alignCast(lhs.ptr))).*;
                        const rhs_str = @as(*RocStr, @ptrCast(@alignCast(rhs.ptr))).*;

                        const result_val: u8 = switch (op) {
                            .eq => if (lhs_str.eq(rhs_str)) 1 else 0,
                            .ne => if (!lhs_str.eq(rhs_str)) 1 else 0,
                            else => return error.InvalidNumericOp,
                        };

                        // Result type is boolean - using nominal type for Bool
                        const bool_type = try self.runtime_types.freshFromContent(.{
                            .structure = .{ .nominal_type = .{
                                .ident = .{ .ident_idx = .{
                                    .attributes = .{ .effectful = false, .ignored = false, .reassignable = false },
                                    .idx = 0
                                } }, // Bool identifier placeholder
                                .vars = .{ .nonempty = .{ .start = @enumFromInt(0), .count = 0 } },
                                .origin_module = .{
                                    .attributes = .{ .effectful = false, .ignored = false, .reassignable = false },
                                    .idx = 0
                                },
                            } }
                        });
                        _ = try self.pushValue(&[_]u8{result_val}, bool_type);
                    },
                    else => return error.UnsupportedScalarType,
                }
            },
            else => {
                // Handle other numeric types similarly
                return error.InvalidNumericOp;
            },
        }
    }

    /// Complete unary minus
    fn completeUnaryMinus(self: *Interpreter) !void {
        const val = try self.popStackValue();
        const type_var = val.type_var;
        const layout_val = try self.getLayoutForVar(type_var);

        switch (layout_val.tag) {
            .scalar => {
                const scalar = layout_val.data.scalar;
                switch (scalar.tag) {
                    .int => {
                        const num = @as(*i64, @ptrCast(@alignCast(val.ptr))).*;
                        const result = try std.math.negate(num);
                        _ = try self.pushValue(std.mem.asBytes(&result), type_var);
                    },
                    .frac => {
                        const num = @as(*f64, @ptrCast(@alignCast(val.ptr))).*;
                        const result = -num;
                        _ = try self.pushValue(std.mem.asBytes(&result), type_var);
                    },
                    else => return error.InvalidNumericOp,
                }
            },
            else => return error.InvalidNumericOp,
        }
    }

    /// Complete unary not
    fn completeUnaryNot(self: *Interpreter) !void {
        const val = try self.popStackValue();
        const type_var = val.type_var;

        const bool_val = @as(*u8, @ptrCast(@alignCast(val.ptr))).* != 0;
        const result: u8 = if (bool_val) 0 else 1;

        _ = try self.pushValue(&[_]u8{result}, type_var);
    }

    /// Handle record field evaluation
    fn handleRecordFields(self: *Interpreter, expr_idx: CIR.Expr.Idx, current_field: u32) !void {
        _ = current_field;

        const expr = self.env.store.getExpr(expr_idx);
        const record = expr.e_record;

        const record_fields = self.env.store.sliceRecordFields(record.fields);

        // Get record type
        const compile_var = ModuleEnv.varFrom(expr_idx);
        const runtime_var = try self.translateTypeVar(self.env, compile_var);
        const layout_val = try self.getLayoutForVar(runtime_var);

        // Allocate record memory
        const size = self.layout_store.layoutSize(layout_val);
        const align_val = layout_val.alignment(target_usize);
        const ptr = try self.stack_memory.alloca(@intCast(size), align_val);

        // Copy field values
        var field_offset: usize = 0;
        for (record_fields, 0..) |field_idx, i| {
            const field = self.env.store.getRecordField(field_idx);
            _ = field.name;
            const field_value = self.value_stack.items[self.value_stack.items.len - record_fields.len + i];
            const field_size = field_value.size;

            const src = self.getStackPtr(field_value.offset);
            const dst = @as([*]u8, @ptrCast(ptr)) + field_offset;
            @memcpy(dst[0..field_size], @as([*]const u8, @ptrCast(src))[0..field_size]);

            field_offset += field_size;
        }

        // Pop field values
        for (record_fields) |_| {
            _ = self.value_stack.pop();
            _ = self.type_stack.pop();
        }
        // Stack memory is managed by restore calls

        // Push record
        _ = try self.pushValue(@as([*]const u8, @ptrCast(ptr))[0..size], runtime_var);
    }

    /// Handle dot access
    fn handleDotAccess(self: *Interpreter, field_name: Ident.Idx) !void {
        const record_value = try self.popStackValue();
        const record_type = record_value.type_var;

        // Get record structure
        const resolved = self.runtime_types.resolveVar(record_type);
        const fields_range = switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .record => |r| r.fields,
                .record_unbound => |r| r,
                else => return error.InvalidFieldAccess,
            },
            else => return error.InvalidFieldAccess,
        };

        // Find field
        const fields = self.runtime_types.getRecordFieldsSlice(fields_range);
        var field_offset: usize = 0;
        var field_type: ?Var = null;
        var field_size: usize = 0;

        const field_names = fields.items(.name);
        const field_types = fields.items(.var_);

        for (field_names, field_types) |name, typ| {
            if (name == field_name) {
                field_type = typ;
                const field_layout = try self.getLayoutForVar(typ);
                field_size = self.layout_store.layoutSize(field_layout);
                break;
            }
            const this_layout = try self.getLayoutForVar(typ);
            field_offset += self.layout_store.layoutSize(this_layout);
        } else {
            return error.InvalidFieldAccess;
        }

        // Copy field value
        const src = @as([*]const u8, @ptrCast(record_value.ptr)) + field_offset;
        _ = try self.pushValue(src[0..field_size], field_type.?);
    }

    /// Handle string interpolation combine
    fn handleStringInterpolateCombine(self: *Interpreter, segment_count: u32, roc_ops: *RocOps) !void {
        // Pop segments and combine
        var combined = RocStr.empty();

        var i: u32 = 0;
        while (i < segment_count) : (i += 1) {
            const segment = try self.popStackValue();
            const str = @as(*RocStr, @ptrCast(@alignCast(segment.ptr))).*;
            combined = strConcat(combined, str, roc_ops);
        }

        // Get string type
        const str_type = try self.runtime_types.freshFromContent(.{
            .structure = .str,
        });

        _ = try self.pushValue(std.mem.asBytes(&combined), str_type);
    }

    /// Evaluate tuple elements
    fn evaluateTuple(self: *Interpreter, expr_idx: CIR.Expr.Idx, current_elem: u32, roc_ops: *RocOps) !void {
        _ = current_elem;
        _ = roc_ops;

        const expr = self.env.store.getExpr(expr_idx);
        const tuple = expr.e_tuple;
        const elems = self.env.store.sliceExpr(tuple.elems);

        // Get tuple type
        const compile_var = ModuleEnv.varFrom(expr_idx);
        const runtime_var = try self.translateTypeVar(self.env, compile_var);
        const layout_val = try self.getLayoutForVar(runtime_var);

        // Allocate tuple memory
        const size = self.layout_store.layoutSize(layout_val);
        const align_val = layout_val.alignment(target_usize);
        const ptr = try self.stack_memory.alloca(@intCast(size), align_val);

        // Copy elements
        var elem_offset: usize = 0;
        for (elems, 0..) |_, i| {
            const elem_value = self.value_stack.items[self.value_stack.items.len - elems.len + i];
            const elem_size = elem_value.size;

            const src = self.getStackPtr(elem_value.offset);
            const dst = @as([*]u8, @ptrCast(ptr)) + elem_offset;
            @memcpy(dst[0..elem_size], @as([*]const u8, @ptrCast(src))[0..elem_size]);

            elem_offset += elem_size;
        }

        // Pop element values
        for (elems) |_| {
            _ = self.value_stack.pop();
            _ = self.type_stack.pop();
        }
        // Stack memory is managed by restore calls

        // Push tuple
        _ = try self.pushValue(@as([*]const u8, @ptrCast(ptr))[0..size], runtime_var);
    }

    /// Handle let binding
    fn handleLetBind(self: *Interpreter, pattern_idx: CIR.Pattern.Idx) !void {
        const value = try self.peekStackValue(1);

        try self.bindings_stack.append(.{
            .pattern_idx = pattern_idx,
            .value = value,
        });
    }

    /// Handle block cleanup
    fn handleBlockCleanup(self: *Interpreter, cleanup: BlockCleanup, roc_ops: *RocOps) !void {
        _ = roc_ops;

        // Get result value
        const result = try self.popStackValue();
        const result_type = result.type_var;
        const result_layout = try self.getLayoutForVar(result_type);
        const size = self.layout_store.layoutSize(result_layout);

        // Save result
        const temp = try self.allocator.alloc(u8, size);
        defer self.allocator.free(temp);
        @memcpy(temp, @as([*]const u8, @ptrCast(result.ptr))[0..size]);

        // Clean up bindings
        self.bindings_stack.shrinkRetainingCapacity(cleanup.bindings_to_keep);

        // Clean up values
        while (self.value_stack.items.len > cleanup.values_to_keep) {
            _ = self.value_stack.pop();
            _ = self.type_stack.pop();
        }

        // Push result back
        _ = try self.pushValue(temp, result_type);
    }

    /// Handle if-then-else
    fn handleIfThenElse(self: *Interpreter, data: IfData) !void {
        const cond = try self.popStackValue();
        const cond_val = @as(*u8, @ptrCast(@alignCast(cond.ptr))).* != 0;

        if (cond_val) {
            self.schedule_work(.{
                .kind = .w_eval_expr,
                .expr_idx = data.then_branch,
                .module_ctx = self.env,
                .extra = .{ .none = {} },
            });
        } else {
            self.schedule_work(.{
                .kind = .w_eval_expr,
                .expr_idx = data.else_branch,
                .module_ctx = self.env,
                .extra = .{ .none = {} },
            });
        }
    }

    /// Handle when branch
    fn handleWhenBranch(self: *Interpreter, data: WhenData, module_ctx: *ModuleEnv) !void {
        const scrutinee = try self.peekStackValue(1);
        const branches = module_ctx.store.sliceMatchBranches(data.branches);

        // Try to match branches
        for (branches) |branch_idx| {
            const branch = module_ctx.store.getMatchBranch(branch_idx);
            const branch_patterns = module_ctx.store.sliceMatchBranchPatterns(branch.patterns);

            // Check if pattern matches
            if (branch_patterns.len > 0) {
                const branch_pattern = module_ctx.store.getMatchBranchPattern(branch_patterns[0]);
                if (try self.matchesPattern(branch_pattern.pattern, scrutinee, module_ctx)) {
                    // Pop scrutinee
                    _ = try self.popStackValue();

                    // Evaluate branch body
                    self.schedule_work(.{
                        .kind = .w_eval_expr,
                        .expr_idx = branch.value,
                        .module_ctx = module_ctx,
                        .extra = .{ .none = {} },
                    });
                    return;
                }
            }
        }

        return error.TypeMismatch; // No match found
    }

    /// Check if a pattern matches a value
    fn matchesPattern(self: *Interpreter, pattern_idx: CIR.Pattern.Idx, value: StackValue, module_ctx: *const ModuleEnv) !bool {
        const pattern = module_ctx.store.getPattern(pattern_idx);

        switch (pattern) {
            .underscore => return true,
            .assign => return true, // Would bind the value
            .int_literal => |lit| {
                const val = value.asI128(self);
                // TODO: Properly handle IntValue comparison
                _ = lit;
                _ = val;
                return false;
            },
            .str_literal => |lit| {
                const val = value.asStr();
                const expected = module_ctx.getString(lit.literal);
                // TODO: Properly compare strings with RocStr
                const val_slice = if (val.bytes) |bytes| bytes[0..val.length] else &[_]u8{};
                return std.mem.eql(u8, val_slice, expected);
            },
            .applied_tag => |_| {
                // TODO: Properly handle applied tag pattern matching
                return false;
            },
            else => return false,
        }
    }

    /// Create a closure
    fn createClosure(_: *Interpreter, lambda: Expr.Lambda, module_ctx: *ModuleEnv) !layout.Closure {
        return layout.Closure{
            .body_idx = lambda.body,
            .params = lambda.args,
            .captures_pattern_idx = @enumFromInt(0), // Simplified - no captures for now
            .captures_layout_idx = @enumFromInt(0), // Simplified
            .lambda_expr_idx = @enumFromInt(0), // Simplified
            .module_ptr = module_ctx,
        };
    }

    /// Look up a binding
    fn lookupBinding(self: *Interpreter, pattern_idx: CIR.Pattern.Idx) ?StackValue {
        var i = self.bindings_stack.items.len;
        while (i > 0) {
            i -= 1;
            const binding = self.bindings_stack.items[i];
            if (binding.pattern_idx == pattern_idx) {
                return binding.value;
            }
        }
        return null;
    }

    /// Look up a capture
    fn lookupCapture(_: *Interpreter, pattern_idx: CIR.Pattern.Idx) !?StackValue {
        _ = pattern_idx;
        // Simplified - real implementation would look up captures properly
        return null;
    }

    /// Find module by name
    fn findModule(self: *const Interpreter, module_name: []const u8) *ModuleEnv {
        for (self.other_modules) |mod| {
            if (std.mem.eql(u8, mod.module_name, module_name)) {
                return mod;
            }
        }
        return self.env; // Default to current module
    }

    /// Find tag index in a union
    fn findTagIndex(self: *Interpreter, union_type: Var, tag_name: Ident.Idx) !u16 {
        const resolved = self.runtime_types.resolveVar(union_type);
        const tag_union = switch (resolved.desc.content) {
            .structure => |s| switch (s) {
                .tag_union => |t| t,
                else => return error.InvalidTagUnion,
            },
            else => return error.InvalidTagUnion,
        };

        const tags = self.runtime_types.getTagsSlice(tag_union.tags);
        const tag_names = tags.items(.name);
        for (tag_names, 0..) |tag_name_curr, i| {
            if (tag_name_curr == tag_name) {
                return @intCast(i);
            }
        }
        return error.InvalidTagUnion;
    }

    /// Find tag index by pattern
    fn findTagIndexByPattern(self: *Interpreter, pattern_idx: CIR.Pattern.Idx, module_ctx: *const ModuleEnv) !u16 {
        const pattern = module_ctx.store.getPattern(pattern_idx);
        const tag_pattern = pattern.tag;
        return self.findTagIndex(
            try self.translateTypeVar(module_ctx, ModuleEnv.varFrom(@enumFromInt(@intFromEnum(pattern_idx)))),
            tag_pattern.name,
        );
    }

    /// Get crash message
    pub fn getCrashMsg(self: *const Interpreter) ?[]const u8 {
        return self.crash_message;
    }

    /// Start debug tracing
    pub fn startTrace(self: *Interpreter, writer: std.io.AnyWriter) void {
        self.trace_writer = writer;
        self.trace_indent = 0;
    }

    /// End debug tracing
    pub fn endTrace(self: *Interpreter) void {
        self.trace_writer = null;
        self.trace_indent = 0;
    }
};

