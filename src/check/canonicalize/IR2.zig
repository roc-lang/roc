const std = @import("std");

pub const TypeVar = struct { t_var: i24 };

pub const Store = struct {
    nodes: [*]Node,

    pub fn if_node(self: *const Store, idx: Node.IfIdx) If {
        const cond_idx = idx.adjacent(1);
        const else_idx = self.nodes.at(idx);

        return .{
            .cond_idx = cond_idx,
            .cond_var = cond_idx.asVar(),
            .else_idx = else_idx,
            .else_var = else_idx.asVar(),
        };
    }

    pub fn eval(
        self: *const Store,
        gpa: std.mem.Allocator,
        scratch: std.mem.Allocator,
        initial_idx: Node.Idx,
    ) EvalErr!RuntimeVal {
        var op_stack = std.ArrayList(Op).initCapacity(scratch, 1024);
        var idx_stack = std.ArrayList(Node.Idx).initCapacity(scratch, 1024);
        var val_stack = std.ArrayList(RuntimeVal).initCapacity(scratch, 1024);
        var ident_stack = std.ArrayList(Ident).initCapacity(scratch, 1024);

        // first, eval the node we were given.
        op_stack.push(Op.eval);
        idx_stack.push(initial_idx.toArg());

        while (!op_stack.isEmpty()) {
            switch (op_stack.pop()) {
                .branch => {
                    const then_idx = idx_stack.pop();
                    const else_idx = idx_stack.pop();
                    const cond = try val_stack.pop().asBool();
                    idx_stack.push(if (cond) then_idx, else_idx);
                    op_stack.push(Op.eval);
                },
                .assign => {
                    scope.insert(ident_stack.pop(), val_stack.pop());
                },
                .begin_scope => {
                    // TODO
                },
                .end_scope => {
                    // TODO
                },
                .crash => {
                    // TODO return a crash error along with the string.
                },
                .eval => {
                    const idx = idx_stack.pop();

                    switch (self.exprAt(idx)) {
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
                },
            }
        }

        // At this point, val_stack should have exactly one item in it.
        // TODO report an interpreter bug if there are 0 or 2+ vals on the stack.
        return try val_stack.pop();
    }
};

/// A value at runtime, such as a boolean or a record.
///
/// These are unmanaged; an allocator must be passed in for any heap operation.
pub const RuntimeVal = union(enum) {

    ////////////////////////////////////////////////////
    // TODO actually this is all wrong. What needs to happen here
    // is that we actually share code with builtins and code gen
    // to put the real bytes in memory that a host would see,
    // because these will literally be passed to hosts!
    // so, any extra metadata (e.g. about types) needs to be stored in a side table.
    ////////////////////////////////////////////////////

    bool: bool,
    heap: *const Refcounted, // TODO do ptr tagging so RuntimeVal is a usize even on wasm

    pub fn deinit(self: *RuntimeVal, allocator: std.mem.Allocator) void {
        switch (self) {
            .bool => {},
            .heap => |refcounted| {
                refcounted.decref();
            },
        }
    }

    pub fn asBool(self: RuntimeVal) TypeMismatch!bool {
        switch (self) {
            bool => {
                return self.bool;
            },
            _ => {
                // TODO err TypeMismatch
            },
        }
    }
};

pub const Refcounted = struct {
    refcount: u32,
    // TODO the refcounted payload

    pub const Record = struct {
        fields: std.ArrayListUnmanaged(RuntimeVal.Record.Field),

        pub const Field = struct {
            name: InternedStrId,
            val: RuntimeVal,
        };
    };

    pub fn incref(self: *Refcounted) void {
        self.refcount += 1;

        // TODO if this is a record, recursively incref all its fields, etc.
    }

    pub fn decref(self: *Refcounted, allocator: std.mem.Allocator) void {
        // TODO if this is a record, recursively decref all its fields, etc.

        if (self.refcount == 1) {
            self.deinit();
            allocator.free(self);
        } else {
            self.refcount -= 1;
        }
    }

    pub fn deinit(self: Refcounted, allocator: std.mem.Allocator) void {
        for (self.fields) |field| {
            field.val.deinit(allocator);
        }
    }
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
