//! Lambda Set Inference Pass
//!
//! This pass runs AFTER LambdaLifter and BEFORE ClosureTransformer to coordinate
//! cross-module closure handling. It:
//!
//! 1. Collects all lifted closures from all modules
//! 2. Assigns globally unique module-qualified names (e.g., "UserModule.#1_addX")
//! 3. Builds lambda set mappings for cross-module dispatch
//!
//! ## Cross-Module Problem
//!
//! When `List.fold` receives a closure from user code:
//! ```roc
//! List.fold([1, 2, 3], {sum: 0, count: 0}, |acc, item| {...})
//! ```
//!
//! Inside `List.fold`, when it calls `step($state, item)`:
//! - `step` is a local parameter bound to a `CIR.Expr.Idx`
//! - That index points to the closure in the **user's module** store
//! - Without coordination, code generation looks it up in the **List module** store → crash
//!
//! ## Solution
//!
//! This pass is the single coordination point that sees all modules. It assigns
//! globally unique closure names so that after defunctionalization, closures become
//! tag values (not raw expr indices) that can be safely passed between modules.
//!
//! ## Pipeline Order
//!
//! ```
//! Parse → Canonicalize → Type Solve → Monomorphization → Lambda Lifting
//!                                                              ↓
//!                                                    Lambda Set Inference (THIS)
//!                                                              ↓
//!                                                    Defunctionalization → IR
//! ```

const std = @import("std");
const base = @import("base");

const ModuleEnv = @import("ModuleEnv.zig");
const CIR = @import("CIR.zig");
const Expr = CIR.Expr;

const Self = @This();

/// Allocator for internal structures
allocator: std.mem.Allocator,

/// Maps (module_name_idx, local_closure_id) → globally unique Ident.Idx
/// The Ident is module-qualified: "UserModule.#1_addX"
closure_names: std.AutoHashMap(ClosureKey, base.Ident.Idx),

/// Maps call site → set of possible closures that could be called
/// Used by ClosureTransformer to generate match arms for cross-module dispatch
lambda_sets: std.AutoHashMap(CallSiteKey, std.ArrayList(ClosureKey)),

/// Global counter for ensuring unique closure IDs across modules
global_closure_counter: u32,

/// Collected closure info from all modules for later lookup
collected_closures: std.ArrayList(CollectedClosure),

/// Key for identifying a closure within a specific module
pub const ClosureKey = struct {
    /// Module name identifier (from ModuleEnv.module_name_idx)
    module_name_idx: base.Ident.Idx,
    /// Local closure ID within that module (the closure_counter value when it was created)
    local_closure_id: u32,

    pub fn eql(a: ClosureKey, b: ClosureKey) bool {
        return @as(u32, @bitCast(a.module_name_idx)) == @as(u32, @bitCast(b.module_name_idx)) and
            a.local_closure_id == b.local_closure_id;
    }

    pub fn hash(key: ClosureKey) u64 {
        var hasher = std.hash.Wyhash.init(0);
        const module_bits = @as(u32, @bitCast(key.module_name_idx));
        hasher.update(std.mem.asBytes(&module_bits));
        hasher.update(std.mem.asBytes(&key.local_closure_id));
        return hasher.final();
    }
};

/// Key for identifying a call site that may call closures from different modules
pub const CallSiteKey = struct {
    /// Module where the call site is located
    module_name_idx: base.Ident.Idx,
    /// The expression index of the call site
    expr_idx: Expr.Idx,
};

/// Information about a collected closure
pub const CollectedClosure = struct {
    /// The module this closure belongs to
    module_name_idx: base.Ident.Idx,
    /// Local closure ID within the module
    local_closure_id: u32,
    /// The original local tag name (e.g., "#1_addX")
    local_tag_name: base.Ident.Idx,
    /// The globally unique tag name (e.g., "UserModule.#1_addX")
    global_tag_name: base.Ident.Idx,
    /// The closure expression index in the source module
    closure_expr_idx: Expr.Idx,
};

/// Initialize the lambda set inference pass
pub fn init(allocator: std.mem.Allocator) Self {
    return Self{
        .allocator = allocator,
        .closure_names = std.AutoHashMap(ClosureKey, base.Ident.Idx).init(allocator),
        .lambda_sets = std.AutoHashMap(CallSiteKey, std.ArrayList(ClosureKey)).init(allocator),
        .global_closure_counter = 0,
        .collected_closures = std.ArrayList(CollectedClosure).empty,
    };
}

/// Free resources
pub fn deinit(self: *Self) void {
    self.closure_names.deinit();
    // Free lambda set ArrayLists
    var iter = self.lambda_sets.valueIterator();
    while (iter.next()) |list| {
        list.deinit(self.allocator);
    }
    self.lambda_sets.deinit();
    self.collected_closures.deinit(self.allocator);
}

/// Process all modules, collecting closure info and assigning globally unique names.
///
/// This is the main entry point. It:
/// 1. Pass 1: Collect all closures from all modules, assign module-qualified names
/// 2. Pass 2: Build lambda sets for call sites with closure parameters
///
/// After this runs, ClosureTransformer can use getGlobalClosureName() to get
/// the globally unique name for any closure.
pub fn inferAll(self: *Self, modules: []*ModuleEnv) !void {
    // Pass 1: Collect all closures, assign module-qualified names
    for (modules) |module| {
        try self.collectClosures(module);
    }

    // Pass 2: Build lambda sets for call sites with closure parameters
    // This identifies where cross-module dispatch might be needed
    for (modules) |module| {
        try self.buildLambdaSets(module);
    }
}

/// Collect closures from a single module.
/// Walks the module's CIR looking for closure expressions and assigns global names.
fn collectClosures(self: *Self, module: *ModuleEnv) std.mem.Allocator.Error!void {
    // Get all statements from the module
    const stmts = module.store.sliceStatements(module.all_statements);

    for (stmts) |stmt_idx| {
        try self.collectClosuresFromStatement(module, stmt_idx);
    }
}

/// Recursively collect closures from a statement
fn collectClosuresFromStatement(self: *Self, module: *ModuleEnv, stmt_idx: CIR.Statement.Idx) std.mem.Allocator.Error!void {
    const stmt = module.store.getStatement(stmt_idx);

    switch (stmt) {
        .s_decl => |decl| {
            try self.collectClosuresFromExpr(module, decl.expr);
        },
        .s_var => |var_stmt| {
            try self.collectClosuresFromExpr(module, var_stmt.expr);
        },
        else => {},
    }
}

/// Recursively collect closures from an expression
fn collectClosuresFromExpr(self: *Self, module: *ModuleEnv, expr_idx: Expr.Idx) std.mem.Allocator.Error!void {
    const expr = module.store.getExpr(expr_idx);

    switch (expr) {
        .e_closure => |closure| {
            // Found a closure - register it with a globally unique name
            try self.registerClosure(module, expr_idx, closure);

            // Also check the lambda body for nested closures
            try self.collectClosuresFromExpr(module, closure.lambda_idx);
        },
        .e_lambda => |lambda| {
            try self.collectClosuresFromExpr(module, lambda.body);
        },
        .e_call => |call| {
            try self.collectClosuresFromExpr(module, call.func);
            const args = module.store.sliceExpr(call.args);
            for (args) |arg_idx| {
                try self.collectClosuresFromExpr(module, arg_idx);
            }
        },
        .e_block => |block| {
            const block_stmts = module.store.sliceStatements(block.stmts);
            for (block_stmts) |stmt_idx| {
                try self.collectClosuresFromStatement(module, stmt_idx);
            }
            try self.collectClosuresFromExpr(module, block.final_expr);
        },
        .e_if => |if_expr| {
            const branches = module.store.sliceIfBranches(if_expr.branches);
            for (branches) |branch_idx| {
                const branch = module.store.getIfBranch(branch_idx);
                try self.collectClosuresFromExpr(module, branch.cond);
                try self.collectClosuresFromExpr(module, branch.body);
            }
            try self.collectClosuresFromExpr(module, if_expr.final_else);
        },
        .e_match => |match| {
            try self.collectClosuresFromExpr(module, match.cond);
            const branches = module.store.sliceMatchBranches(match.branches);
            for (branches) |branch_idx| {
                const branch = module.store.getMatchBranch(branch_idx);
                if (branch.guard) |guard| {
                    try self.collectClosuresFromExpr(module, guard);
                }
                try self.collectClosuresFromExpr(module, branch.value);
            }
        },
        .e_list => |list| {
            const elems = module.store.sliceExpr(list.elems);
            for (elems) |elem_idx| {
                try self.collectClosuresFromExpr(module, elem_idx);
            }
        },
        .e_tuple => |tuple| {
            const elems = module.store.sliceExpr(tuple.elems);
            for (elems) |elem_idx| {
                try self.collectClosuresFromExpr(module, elem_idx);
            }
        },
        .e_record => |record| {
            const fields = module.store.sliceRecordFields(record.fields);
            for (fields) |field_idx| {
                const field = module.store.getRecordField(field_idx);
                try self.collectClosuresFromExpr(module, field.value);
            }
            if (record.ext) |ext| {
                try self.collectClosuresFromExpr(module, ext);
            }
        },
        .e_tag => |tag| {
            const args = module.store.sliceExpr(tag.args);
            for (args) |arg_idx| {
                try self.collectClosuresFromExpr(module, arg_idx);
            }
        },
        .e_binop => |binop| {
            try self.collectClosuresFromExpr(module, binop.lhs);
            try self.collectClosuresFromExpr(module, binop.rhs);
        },
        .e_unary_minus => |unary| {
            try self.collectClosuresFromExpr(module, unary.expr);
        },
        .e_unary_not => |unary| {
            try self.collectClosuresFromExpr(module, unary.expr);
        },
        .e_dot_access => |dot| {
            try self.collectClosuresFromExpr(module, dot.receiver);
            if (dot.args) |args_span| {
                const args = module.store.sliceExpr(args_span);
                for (args) |arg_idx| {
                    try self.collectClosuresFromExpr(module, arg_idx);
                }
            }
        },
        .e_nominal => |nom| {
            try self.collectClosuresFromExpr(module, nom.backing_expr);
        },
        .e_nominal_external => |nom| {
            try self.collectClosuresFromExpr(module, nom.backing_expr);
        },
        // Terminal expressions that don't contain nested closures
        .e_num,
        .e_str,
        .e_frac_f32,
        .e_frac_f64,
        .e_dec,
        .e_dec_small,
        .e_typed_int,
        .e_typed_frac,
        .e_str_segment,
        .e_lookup_local,
        .e_lookup_external,
        .e_lookup_required,
        .e_zero_argument_tag,
        .e_empty_record,
        .e_empty_list,
        .e_crash,
        .e_runtime_error,
        .e_dbg,
        .e_expect,
        .e_for,
        .e_hosted_lambda,
        .e_low_level_lambda,
        .e_type_var_dispatch,
        .e_ellipsis,
        .e_anno_only,
        .e_return,
        .e_incref,
        .e_decref,
        .e_free,
        => {},
    }
}

/// Register a closure with a globally unique name
fn registerClosure(self: *Self, module: *ModuleEnv, closure_expr_idx: Expr.Idx, closure: Expr.Closure) std.mem.Allocator.Error!void {
    _ = closure; // We use the expr_idx to identify the closure

    // Assign a global ID
    self.global_closure_counter += 1;
    const local_closure_id = self.global_closure_counter;

    // Create the closure key
    const key = ClosureKey{
        .module_name_idx = module.module_name_idx,
        .local_closure_id = local_closure_id,
    };

    // Generate the local tag name (for now, use a simple counter-based name)
    // The ClosureTransformer will generate actual names with hints
    const local_name = try std.fmt.allocPrint(
        self.allocator,
        "#{d}",
        .{local_closure_id},
    );
    defer self.allocator.free(local_name);
    const local_tag_name = try module.insertIdent(base.Ident.for_text(local_name));

    // Generate the globally unique module-qualified name
    const global_name = try self.generateGlobalName(module, local_tag_name);

    // Store the mapping
    try self.closure_names.put(key, global_name);

    // Also store in collected_closures for later iteration
    try self.collected_closures.append(self.allocator, .{
        .module_name_idx = module.module_name_idx,
        .local_closure_id = local_closure_id,
        .local_tag_name = local_tag_name,
        .global_tag_name = global_name,
        .closure_expr_idx = closure_expr_idx,
    });
}

/// Generate a module-qualified closure name.
/// Format: "ModuleName.#N_funcName" where N is globally unique
fn generateGlobalName(
    self: *Self,
    module: *ModuleEnv,
    local_name: base.Ident.Idx,
) std.mem.Allocator.Error!base.Ident.Idx {
    const module_name = module.module_name;
    const local_name_str = module.getIdent(local_name);

    // Format: "ModuleName.#N_funcName" or "ModuleName.#N" if no func name hint
    const qualified = try std.fmt.allocPrint(
        self.allocator,
        "{s}.{s}",
        .{ module_name, local_name_str },
    );
    defer self.allocator.free(qualified);
    return try module.insertIdent(base.Ident.for_text(qualified));
}

/// Build lambda sets for call sites.
/// Identifies function parameters that may receive closures from other modules.
fn buildLambdaSets(self: *Self, module: *ModuleEnv) std.mem.Allocator.Error!void {
    // For now, this is a simplified implementation.
    // A full implementation would analyze call graphs to determine
    // which call sites might receive closures from multiple modules.
    //
    // The key insight is that higher-order functions like List.fold
    // have parameters (like `step`) that can receive closures from any module.
    // When we see a call to such a function, we need to record that the
    // call site's lambda set includes the passed closure.

    const stmts = module.store.sliceStatements(module.all_statements);
    for (stmts) |stmt_idx| {
        try self.buildLambdaSetsFromStatement(module, stmt_idx);
    }
}

/// Recursively build lambda sets from a statement
fn buildLambdaSetsFromStatement(self: *Self, module: *ModuleEnv, stmt_idx: CIR.Statement.Idx) std.mem.Allocator.Error!void {
    const stmt = module.store.getStatement(stmt_idx);

    switch (stmt) {
        .s_decl => |decl| {
            try self.buildLambdaSetsFromExpr(module, decl.expr);
        },
        .s_var => |var_stmt| {
            try self.buildLambdaSetsFromExpr(module, var_stmt.expr);
        },
        else => {},
    }
}

/// Recursively build lambda sets from an expression
fn buildLambdaSetsFromExpr(self: *Self, module: *ModuleEnv, expr_idx: Expr.Idx) std.mem.Allocator.Error!void {
    const expr = module.store.getExpr(expr_idx);

    switch (expr) {
        .e_call => |call| {
            // Check if any arguments are closures
            const args = module.store.sliceExpr(call.args);
            for (args) |arg_idx| {
                const arg_expr = module.store.getExpr(arg_idx);
                if (arg_expr == .e_closure) {
                    // This call site receives a closure - record it
                    const call_key = CallSiteKey{
                        .module_name_idx = module.module_name_idx,
                        .expr_idx = expr_idx,
                    };

                    // Find which closure this is
                    for (self.collected_closures.items) |closure_info| {
                        if (@intFromEnum(closure_info.closure_expr_idx) == @intFromEnum(arg_idx) and
                            @as(u32, @bitCast(closure_info.module_name_idx)) == @as(u32, @bitCast(module.module_name_idx)))
                        {
                            const closure_key = ClosureKey{
                                .module_name_idx = closure_info.module_name_idx,
                                .local_closure_id = closure_info.local_closure_id,
                            };

                            const gop = try self.lambda_sets.getOrPut(call_key);
                            if (!gop.found_existing) {
                                gop.value_ptr.* = std.ArrayList(ClosureKey).empty;
                            }
                            try gop.value_ptr.append(self.allocator, closure_key);
                            break;
                        }
                    }
                }
            }

            // Recurse into function and arguments
            try self.buildLambdaSetsFromExpr(module, call.func);
            for (args) |arg_idx| {
                try self.buildLambdaSetsFromExpr(module, arg_idx);
            }
        },
        .e_block => |block| {
            const block_stmts = module.store.sliceStatements(block.stmts);
            for (block_stmts) |stmt_idx| {
                try self.buildLambdaSetsFromStatement(module, stmt_idx);
            }
            try self.buildLambdaSetsFromExpr(module, block.final_expr);
        },
        .e_lambda => |lambda| {
            try self.buildLambdaSetsFromExpr(module, lambda.body);
        },
        .e_closure => |closure| {
            try self.buildLambdaSetsFromExpr(module, closure.lambda_idx);
        },
        .e_if => |if_expr| {
            const branches = module.store.sliceIfBranches(if_expr.branches);
            for (branches) |branch_idx| {
                const branch = module.store.getIfBranch(branch_idx);
                try self.buildLambdaSetsFromExpr(module, branch.cond);
                try self.buildLambdaSetsFromExpr(module, branch.body);
            }
            try self.buildLambdaSetsFromExpr(module, if_expr.final_else);
        },
        .e_match => |match| {
            try self.buildLambdaSetsFromExpr(module, match.cond);
            const branches = module.store.sliceMatchBranches(match.branches);
            for (branches) |branch_idx| {
                const branch = module.store.getMatchBranch(branch_idx);
                if (branch.guard) |guard| {
                    try self.buildLambdaSetsFromExpr(module, guard);
                }
                try self.buildLambdaSetsFromExpr(module, branch.value);
            }
        },
        .e_binop => |binop| {
            try self.buildLambdaSetsFromExpr(module, binop.lhs);
            try self.buildLambdaSetsFromExpr(module, binop.rhs);
        },
        // Other expressions don't affect lambda sets directly
        else => {},
    }
}

/// Get the globally unique name for a closure.
/// Used by ClosureTransformer to get the module-qualified tag name.
pub fn getGlobalClosureName(self: *const Self, key: ClosureKey) ?base.Ident.Idx {
    return self.closure_names.get(key);
}

/// Get the lambda set for a call site.
/// Returns the list of closure keys that might be called at this site.
pub fn getLambdaSet(self: *const Self, key: CallSiteKey) ?[]const ClosureKey {
    if (self.lambda_sets.get(key)) |list| {
        return list.items;
    }
    return null;
}

/// Check if a closure exists in our registry
pub fn hasClosureFor(self: *const Self, module_name_idx: base.Ident.Idx, closure_id: u32) bool {
    const key = ClosureKey{
        .module_name_idx = module_name_idx,
        .local_closure_id = closure_id,
    };
    return self.closure_names.contains(key);
}

/// Get iterator over all collected closures
pub fn getCollectedClosures(self: *const Self) []const CollectedClosure {
    return self.collected_closures.items;
}

// Tests

test "LambdaSetInference: init and deinit" {
    const allocator = std.testing.allocator;
    var inference = Self.init(allocator);
    defer inference.deinit();

    try std.testing.expectEqual(@as(u32, 0), inference.global_closure_counter);
    try std.testing.expectEqual(@as(usize, 0), inference.collected_closures.items.len);
}

test "LambdaSetInference: ClosureKey equality" {
    const attrs = base.Ident.Attributes{ .effectful = false, .ignored = false, .reassignable = false };
    const key1 = ClosureKey{
        .module_name_idx = .{ .attributes = attrs, .idx = 1 },
        .local_closure_id = 42,
    };
    const key2 = ClosureKey{
        .module_name_idx = .{ .attributes = attrs, .idx = 1 },
        .local_closure_id = 42,
    };
    const key3 = ClosureKey{
        .module_name_idx = .{ .attributes = attrs, .idx = 2 },
        .local_closure_id = 42,
    };

    try std.testing.expect(key1.eql(key2));
    try std.testing.expect(!key1.eql(key3));
}
