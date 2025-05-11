const std = @import("std");
const host = @import("../host_abi.zig");
const safe_list = @import("../collections/safe_list.zig");

const SafeList = safe_list.SafeList;

const LogEntry = union(enum) {
    dbg: [*]u8,
    expect_failed: [*]u8,
};

const Env = struct {
    gpa: std.mem.Allocator,
    log: SafeList(LogEntry, usize),
};
const RocOps = host.RocOps(Env, struct {});

pub const RuntimeVal = struct {
    /// The layout of this value in memory.
    layout: Layout,
    /// A pointer to the value itself.
    data: *anyopaque,

    /// The layout of the runtime value. Since we aren't performing specialization ahead of time
    /// when doing these evaluations, we assemble this layout on the fly as we evaluate.
    pub const Layout = union(enum) {
        bool,
        str,
        tuple: SafeList(Layout, u16),
        list: *Layout,
        set: *Layout,
        box: *Layout,
    };
};

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

/// Returns a Roc runtime value which has been allocated using ops.roc_alloc (and friends).
/// It's guaranteed to have the same layout that it would have when built using optimizations
/// (including lambda set specializations etc.)
pub fn eval(
    scratch: std.mem.Allocator,
    roc_ops: RocOps,
    can_node_idx: IR.NodeIdx,
    can_node_store: IR.Store,
    subs: Subs,
) EvalErr!RuntimeVal {
    scratch.reset();
    const state = State.init(scratch, roc_ops, can_node_store, subs);

    return state.eval(can_node_idx);
}

/// The state of the in-progress evaluation
const State = struct {
    roc_ops: RocOps,
    can_node_store: IR.Store,
    subs: Subs,
    op_stack: SafeList(Op, u32),
    idx_stack: SafeList(Node.Idx, u32),
    val_stack: SafeList(RuntimeVal, u32),
    ident_stack: SafeList(Ident, u32),

    fn init(
        allocator: std.mem.Allocator,
        roc_ops: RocOps,
        can_node_store: IR.Store,
        subs: Subs,
    ) State {
        return State{
            .roc_ops = roc_ops,
            .can_node_store = can_node_store,
            .subs = subs,
            .op_stack = SafeList(Op).initCapacity(allocator, 128),
            .idx_stack = SafeList(Node.Idx).initCapacity(allocator, 128),
            .val_stack = SafeList(RuntimeVal).initCapacity(allocator, 128),
            .ident_stack = SafeList(Ident).initCapacity(allocator, 128),
        };
    }

    fn eval(self: *const State, initial_idx: Node.Idx) EvalErr!RuntimeVal {
        // first, eval the node we were given.
        try self.evalExpr(self.exprAt(initial_idx.toArg()));

        while (self.op_stack.pop()) |op| {
            switch (op) {
                .branch => {
                    const then_idx = self.idx_stack.pop() orelse unreachable;
                    const else_idx = self.idx_stack.pop() orelse unreachable;
                    const cond_val = self.val_stack.pop() orelse unreachable;
                    const cond = try cond_val.asBool();
                    const expr = self.exprAt(if (cond) then_idx, else_idx);

                    try self.evalExpr(expr);
                },
                .assign => {
                    self.scope.insert(self.ident_stack.pop(), self.val_stack.pop());
                },
                .begin_scope => {
                    self.scope.begin();
                },
                .end_scope => {
                    self.scope.end();
                },
                .crash => {
                    self.roc_ops.crashed();

                    // crashed() should have unwound the stack.
                    unreachable;
                },
                .eval => {
                    try self.evalExpr(self.exprAt(self.idx_stack.pop()));
                },
            }
        }

        // At this point, val_stack should have exactly one item in it.
        // TODO report an interpreter bug if there are 0 or 2+ vals on the stack.
        return self.val_stack.pop();
    }

    /// Evaluate a single expr (a canonical IR node).
    fn evalExpr(self: *State, expr: Expr) EvalErr!void {
        switch (expr) {
            .if_then_else => |if_then_else| {
                // first, eval the condition
                op_stack.push(Op.eval);
                idx_stack.push(if_then_else.condIdx());

                // next, do an if/else branch
                op_stack.push(Op.branch);
                idx_stack.push(if_then_else.thenIdx());
                idx_stack.push(if_then_else.elseIdx());
            },
            .block => |block| {
                // blocks get their own scope for their assignments
                op_stack.push(Op.begin_scope);

                // run all the statements preceding the expr
                for (block.stmts) |stmt| {
                    switch (stmt) {
                        .assign => |name, idx| {
                            // eval the expr that's being assigned to name
                            op_stack.push(Op.eval);
                            idx_stack.push(idx);

                            // assign it to name
                            op_stack.push(Op.assign);
                            ident_stack.push(name);
                        },
                    }
                }

                // eval the expr at the end of the block
                op_stack.push(Op.eval);
                idx_stack.push(block.expr);

                op_stack.push(Op.end_scope);
            },
        }
    }
};
