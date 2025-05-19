const std = @import("std");
const testing = std.testing;
const host = @import("../host_abi.zig");
const can = @import("../check/canonicalize.zig");
const eval_env = @import("./eval_env.zig");
const safe_list = @import("../collections/safe_list.zig");
const typ = @import("../types/types.zig");
const base = @import("../base.zig");
// const builtin_str = @import("../../crates/compiler/builtins/bitcode/src/str.zig");

const Ident = base.Ident;
const SafeList = safe_list.SafeList;
const StringLiteralStore = base.StringLiteral.Store;
// const RocStr = builtin_str.RocStr;

/// An operation for the interpreter to perform, using
/// the current contents of various stacks as arguments.
const Op = enum {
    eval,
    branch,
    assign,
    begin_scope,
    end_scope,
    crash,
};

/// Error that can happen when evaluating.
const EvalErr = error{
    NotABool,
    OutOfBounds,
    NotAList,
    NotARecord,
    DivideByZero,
    IdentNotFound,
    NotInBackpack,
    ParseError,
    TooBig,
    InvalidRange,
    BadLayout,
};

// eval's caller knows which of these it's receiving based on inferred type of the value.
pub const EvalAnswer = union {
    str: []u8, // TODO RocStr,
    bool: bool,
    // boxed_record: Record, // must be boxed bc size can't be known at compile time
};

/// Returns a Roc runtime value which has been allocated using ops.roc_alloc (and friends).
/// It's guaranteed to have the same layout that it would have when built using optimizations
/// (including lambda set specializations etc.)
pub fn eval(
    scratch: std.mem.Allocator,
    roc_ops: eval_env.EvalRocOps,
    can_node_idx: NodeIdx,
    can_ir: *const can.IR,
    // type_store: *const typ.Store,
) EvalErr!TypedVal {
    var state = State.init(
        scratch,
        roc_ops,
        can_ir,
        // type_store,
    );

    return state.eval(can_node_idx);
}

const NodeIdx = u32;

const TypedVal = struct {
    type_var: u32, // TODO TypeVar
    value: EvalAnswer,

    fn asBool(_: TypedVal) !bool {
        // TODO probably we should have type_stack and val_stack instead of this, for better SoA.
        @panic("TODO need to have asBool take in Subs and then check to see if this type val is a bool.");
    }
};

const Scope = struct {
    fn insert(_: Scope, _: Ident, _: TypedVal) void {
        // TODO
    }

    fn begin(_: Scope) void {
        // TODO
    }

    fn end(_: Scope) void {
        // TODO
    }
};

/// The state of the in-progress evaluation
const State = struct {
    roc_ops: eval_env.EvalRocOps,
    can_ir: *const can.IR,
    // type_store: *const typ.Store,
    op_stack: SafeList(Op),
    idx_stack: SafeList(NodeIdx),
    val_stack: SafeList(TypedVal),
    ident_stack: SafeList(Ident),
    scope: Scope,

    fn init(
        allocator: std.mem.Allocator,
        roc_ops: eval_env.EvalRocOps,
        can_ir: *const can.IR,
        // type_store: *const typ.Store,
    ) State {
        return State{
            .roc_ops = roc_ops,
            .can_ir = can_ir,
            // .type_store = type_store,
            .scope = .{},
            .op_stack = SafeList(Op).initCapacity(allocator, 128),
            .idx_stack = SafeList(NodeIdx).initCapacity(allocator, 128),
            .val_stack = SafeList(TypedVal).initCapacity(allocator, 128),
            .ident_stack = SafeList(Ident).initCapacity(allocator, 128),
        };
    }

    fn exprAt(_: *State, _: NodeIdx) can.IR.Expr {
        return .empty_record; // TODO actually get the expr
    }

    fn eval(self: *State, initial_idx: NodeIdx) EvalErr!TypedVal {
        // First, eval the node we were given. This may add to op_stack.
        try self.evalExpr(self.exprAt(initial_idx));

        while (self.op_stack.pop()) |op| {
            switch (op) {
                .branch => {
                    const then_idx = self.idx_stack.pop() orelse unreachable;
                    const else_idx = self.idx_stack.pop() orelse unreachable;
                    const cond_val = self.val_stack.pop() orelse unreachable;
                    const cond = try cond_val.asBool();
                    const expr = self.exprAt(if (cond) then_idx else else_idx);

                    try self.evalExpr(expr);
                },
                .assign => {
                    self.scope.insert(
                        self.ident_stack.pop() orelse unreachable,
                        self.val_stack.pop() orelse unreachable,
                    );
                },
                .begin_scope => {
                    self.scope.begin();
                },
                .end_scope => {
                    self.scope.end();
                },
                .crash => {
                    const roc_str: []u8 = (self.val_stack.pop() orelse unreachable).value.str;
                    const roc_crashed: host.RocCrashed = .{
                        .utf8_bytes = roc_str.ptr,
                        .len = roc_str.len,
                    };
                    self.roc_ops.roc_crashed(&roc_crashed, &self.roc_ops.env);

                    // crashed() should have unwound the stack.
                    unreachable;
                },
                .eval => {
                    try self.evalExpr(self.exprAt(self.idx_stack.pop() orelse unreachable));
                },
            }
        }

        // At this point, val_stack should have exactly one item in it.
        // TODO report an interpreter bug if there are 0 or 2+ vals on the stack.
        return self.val_stack.pop() orelse unreachable;
    }

    /// Evaluate a single expr (a canonical IR node).
    fn evalExpr(_: *State, expr: can.IR.Expr) EvalErr!void {
        switch (expr) {
            // .if_then_else => |if_then_else| {
            //     // first, eval the condition
            //     self.op_stack.push(Op.eval);
            //     self.idx_stack.push(if_then_else.condIdx());

            //     // next, do an if/else branch
            //     self.op_stack.push(Op.branch);
            //     self.idx_stack.push(if_then_else.thenIdx());
            //     self.idx_stack.push(if_then_else.elseIdx());
            // },
            // .block => |block| {
            //     // blocks get their own scope for their assignments
            //     self.op_stack.push(Op.begin_scope);

            //     // run all the statements preceding the expr
            //     for (block.stmts) |stmt| {
            //         switch (stmt) {
            //             .assign => |assignment| {
            //                 // eval the expr that's being assigned to name
            //                 self.op_stack.push(Op.eval);
            //                 self.idx_stack.push(assignment.idx);

            //                 // assign it to name
            //                 self.op_stack.push(Op.assign);
            //                 self.ident_stack.push(assignment.name);
            //             },
            //         }
            //     }

            //     // eval the expr at the end of the block
            //     self.op_stack.push(Op.eval);
            //     self.idx_stack.push(block.expr);

            //     self.op_stack.push(Op.end_scope);
            // },
            .num => |num| {
                _ = num;
                // num_var: TypeIdx,
                // literal: StringLiteral.Idx,
                // value: IntValue,
                // bound: types.num.Bound.Num,
            },
            .int => |int| {
                // Int and Float store a variable to generate better error messages
                _ = int;
                // num_var: TypeIdx,
                // precision_var: TypeIdx,
                // literal: StringLiteral.Idx,
                // value: IntValue,
                // bound: types.num.Bound.Int,
            },
            .float => |float| {
                _ = float;
                // num_var: TypeIdx,
                // precision_var: TypeIdx,
                // literal: StringLiteral.Idx,
                // value: f64,
                // bound: types.num.Bound.Float,
            },
            .str => |idx| {
                _ = idx; // TODO
                // const str: []u8 = self.str_literal_store.get(idx);
                // const roc_str = try RocStr.init(self.roc_ops.allocator(), str);
                // const roc_str = self.roc_ops.allocator().dupe(u8, str) catch unreachable;

                // self.val_store.push_str(roc_str);
            },
            .single_quote => |single_quote| {
                // Number variable, precision variable, value, bound
                _ = single_quote;
                // num_var: TypeIdx,
                // precision_var: TypeIdx,
                // value: u32,
                // bound: types.num.Bound.SingleQuote,
            },
            .list => |list| {
                _ = list;
                // elem_var: TypeIdx,
                // elems: ExprAtRegion.Range,
            },
            .@"var" => |var_| {
                _ = var_;
                // ident: Ident.Idx,
                // type_var: TypeIdx,
            },
            .when => |when_idx| {
                _ = when_idx;
            },
            .@"if" => |if_| {
                _ = if_;
                // cond_var: TypeIdx,
                // branch_var: TypeIdx,
                // branches: IfBranch.Range,
                // final_else: ExprAtRegion.Idx,
            },
            .let => |let| {
                _ = let;
                // defs: Def.Range,
                // cont: ExprAtRegion.Idx,
                // cycle_mark: IllegalCycleMark,
            },
            .call => |call| {
                // This is *only* for calling functions, not for tag application.
                // The Tag variant contains any applied values inside it.
                _ = call;
                // TODO:
                // Box<(Variable, Loc<Expr>, Variable, Variable)>,
                // args: TypedExprAtRegion.Range,
                // called_via: base.CalledVia,
            },
            .record => |rec| {
                _ = rec;
                // record_var: TypeIdx,
                // TODO:
                // fields: SendMap<Lowercase, Field>,
            },
            .empty_record => {
                // TODO
            },
            .compilation_error => |problem_idx| {
                _ = problem_idx;
                // TODO
            },
            .crash => |crash| {
                _ = crash;
                // msg: ExprAtRegion.Idx,
                // ret_var: TypeIdx,
            },
            .record_access => |rec| {
                _ = rec;
                // Look up exactly one field on a record, e.g. (expr).foo.
                // record_var: TypeIdx,
                // ext_var: TypeIdx,
                // field_var: TypeIdx,
                // loc_expr: ExprAtRegion.Idx,
                // field: Ident.Idx,
            },
            .tag => |tag| {
                _ = tag;
                // tag_union_var: TypeIdx,
                // ext_var: TypeIdx,
                // name: Ident.Idx,
                // args: TypedExprAtRegion.Range,
            },
            .zero_argument_tag => |tag| {
                _ = tag;
                // closure_name: Ident.Idx,
                // variant_var: TypeIdx,
                // ext_var: TypeIdx,
                // name: Ident.Idx,
            },
        }
    }
};

test "Hello World" {
    const scratch = testing.allocator;
    var ir = can.IR.init(scratch);
    defer ir.deinit();
    const roc_ops = eval_env.testingEvalRocOps(scratch);
    // const answer = try eval(
    //     scratch,
    //     roc_ops,
    //     0, // TODO make this be the index to the can IR node of the string
    //     &ir,
    //     // type_store: *const typ.Store,
    // );

    _ = roc_ops;

    // // TODO assert type
    // try testing.expectEqualStrings(answer.value.str, "Hello World");
}
