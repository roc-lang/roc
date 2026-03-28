//! Tail recursion detection and transformation for statement-only LIR.
//!
//! Tail-recursive calls are represented directly in statement form:
//! `assign_call tmp = f(args); ret tmp`
//!
//! This pass rewrites that shape to:
//! `jump join_point(args)`
//! and wraps the transformed body in:
//! `join join_point(params) = body in jump join_point(params)`

const std = @import("std");

const ir = @import("LIR.zig");
const LirStore = @import("LirStore.zig");

const Allocator = std.mem.Allocator;
const CFStmt = ir.CFStmt;
const CFStmtId = ir.CFStmtId;
const JoinPointId = ir.JoinPointId;
const LocalSpan = ir.LocalSpan;
const Symbol = ir.Symbol;

/// Tail-recursion rewrite state for one target proc body.
pub const TailRecursionPass = struct {
    store: *LirStore,
    target_symbol: Symbol,
    join_point_id: JoinPointId,
    allocator: Allocator,
    found_tail_call: bool = false,

    /// Initializes a tail-recursion pass for one proc body.
    pub fn init(
        store: *LirStore,
        target_symbol: Symbol,
        join_point_id: JoinPointId,
        allocator: Allocator,
    ) TailRecursionPass {
        return .{
            .store = store,
            .target_symbol = target_symbol,
            .join_point_id = join_point_id,
            .allocator = allocator,
        };
    }

    fn isTailCallToTarget(self: *TailRecursionPass, assign: @FieldType(CFStmt, "assign_call")) bool {
        const proc = self.store.getProcSpec(assign.proc);
        return proc.name.eql(self.target_symbol);
    }

    fn transformAssignLike(self: *TailRecursionPass, stmt: CFStmt, next_raw: CFStmtId) Allocator.Error!CFStmtId {
        const next = try self.transformStmt(next_raw);
        return switch (stmt) {
            .assign_ref => |assign| self.store.addCFStmt(.{ .assign_ref = .{
                .target = assign.target,
                .result = assign.result,
                .op = assign.op,
                .next = next,
            } }),
            .assign_literal => |assign| self.store.addCFStmt(.{ .assign_literal = .{
                .target = assign.target,
                .result = assign.result,
                .value = assign.value,
                .next = next,
            } }),
            .assign_call => |assign| self.store.addCFStmt(.{ .assign_call = .{
                .target = assign.target,
                .result = assign.result,
                .proc = assign.proc,
                .args = assign.args,
                .next = next,
            } }),
            .assign_low_level => |assign| self.store.addCFStmt(.{ .assign_low_level = .{
                .target = assign.target,
                .result = assign.result,
                .op = assign.op,
                .args = assign.args,
                .next = next,
            } }),
            .assign_list => |assign| self.store.addCFStmt(.{ .assign_list = .{
                .target = assign.target,
                .result = assign.result,
                .elems = assign.elems,
                .next = next,
            } }),
            .assign_struct => |assign| self.store.addCFStmt(.{ .assign_struct = .{
                .target = assign.target,
                .result = assign.result,
                .fields = assign.fields,
                .next = next,
            } }),
            .assign_tag => |assign| self.store.addCFStmt(.{ .assign_tag = .{
                .target = assign.target,
                .result = assign.result,
                .discriminant = assign.discriminant,
                .args = assign.args,
                .next = next,
            } }),
            .assign_symbol => |assign| self.store.addCFStmt(.{ .assign_symbol = .{
                .target = assign.target,
                .symbol = assign.symbol,
                .next = next,
            } }),
            else => unreachable,
        };
    }

    /// Transforms a statement graph, rewriting tail-recursive call/return pairs into jumps.
    pub fn transformStmt(self: *TailRecursionPass, stmt_id: CFStmtId) Allocator.Error!CFStmtId {
        const stmt = self.store.getCFStmt(stmt_id);
        return switch (stmt) {
            .assign_call => |assign| blk: {
                if (self.isTailCallToTarget(assign)) {
                    const next_stmt = self.store.getCFStmt(assign.next);
                    if (next_stmt == .ret and next_stmt.ret.value == assign.target) {
                        self.found_tail_call = true;
                        break :blk try self.store.addCFStmt(.{ .jump = .{
                            .target = self.join_point_id,
                            .args = assign.args,
                        } });
                    }
                }
                break :blk try self.transformAssignLike(stmt, assign.next);
            },
            .assign_symbol => |assign| try self.transformAssignLike(stmt, assign.next),
            .assign_ref => |assign| try self.transformAssignLike(stmt, assign.next),
            .assign_literal => |assign| try self.transformAssignLike(stmt, assign.next),
            .assign_low_level => |assign| try self.transformAssignLike(stmt, assign.next),
            .assign_list => |assign| try self.transformAssignLike(stmt, assign.next),
            .assign_struct => |assign| try self.transformAssignLike(stmt, assign.next),
            .assign_tag => |assign| try self.transformAssignLike(stmt, assign.next),
            .incref, .decref, .free, .scope_exit, .jump, .ret, .crash, .runtime_error => stmt_id,
            .switch_stmt => |switch_stmt| blk: {
                var rewritten_branches: std.ArrayListUnmanaged(ir.CFSwitchBranch) = .empty;
                defer rewritten_branches.deinit(self.allocator);

                for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                    try rewritten_branches.append(self.allocator, .{
                        .value = branch.value,
                        .body = try self.transformStmt(branch.body),
                    });
                }

                const branch_span = try self.store.addCFSwitchBranches(rewritten_branches.items);
                break :blk try self.store.addCFStmt(.{ .switch_stmt = .{
                    .cond = switch_stmt.cond,
                    .branches = branch_span,
                    .default_branch = try self.transformStmt(switch_stmt.default_branch),
                } });
            },
            .borrow_scope => |scope| try self.store.addCFStmt(.{ .borrow_scope = .{
                .id = scope.id,
                .body = try self.transformStmt(scope.body),
                .remainder = try self.transformStmt(scope.remainder),
            } }),
            .join => |join| try self.store.addCFStmt(.{ .join = .{
                .id = join.id,
                .params = join.params,
                .body = try self.transformStmt(join.body),
                .remainder = try self.transformStmt(join.remainder),
            } }),
        };
    }
};

/// Rewrites a proc body into an explicit `join`/`jump` loop when tail recursion is present.
pub fn makeTailRecursive(
    store: *LirStore,
    proc_symbol: Symbol,
    join_point_id: JoinPointId,
    body: CFStmtId,
    params: LocalSpan,
    allocator: Allocator,
) Allocator.Error!?CFStmtId {
    var pass = TailRecursionPass.init(store, proc_symbol, join_point_id, allocator);
    const transformed_body = try pass.transformStmt(body);

    if (!pass.found_tail_call) return null;

    const initial_jump = try store.addCFStmt(.{ .jump = .{
        .target = join_point_id,
        .args = params,
    } });

    return try store.addCFStmt(.{ .join = .{
        .id = join_point_id,
        .params = params,
        .body = transformed_body,
        .remainder = initial_jump,
    } });
}

test "TailRecursionPass initialization" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();

    const symbol = Symbol.fromRaw(42);
    const join_id: JoinPointId = @enumFromInt(1);

    var pass = TailRecursionPass.init(&store, symbol, join_id, allocator);
    try std.testing.expect(!pass.found_tail_call);
    try std.testing.expect(pass.target_symbol.eql(symbol));
}

test "TailRecursionPass transforms assign_call/ret into jump" {
    const allocator = std.testing.allocator;
    const i64_layout = @import("layout").Idx.i64;

    var store = LirStore.init(allocator);
    defer store.deinit();

    const proc_symbol = Symbol.fromRaw(100);
    const arg_local = try store.addLocal(.{ .source_symbol = Symbol.fromRaw(101), .layout_idx = i64_layout });
    const temp_local = try store.addLocal(.{ .source_symbol = Symbol.fromRaw(102), .layout_idx = i64_layout });
    const join_id: JoinPointId = @enumFromInt(7);
    const sentinel_msg = try store.insertString("TailRecursion test unresolved proc body sentinel should never execute");
    const sentinel_body = try store.addCFStmt(.{ .crash = .{ .msg = sentinel_msg } });

    const proc_id = try store.addProcSpec(.{
        .name = proc_symbol,
        .args = LocalSpan.empty(),
        .body = sentinel_body,
        .ret_layout = i64_layout,
        .result_contract = .fresh,
        .hosted = null,
    });

    const args = try store.addLocalSpan(&.{arg_local});
    const ret_stmt = try store.addCFStmt(.{ .ret = .{
        .value = temp_local,
    } });
    const call_stmt = try store.addCFStmt(.{ .assign_call = .{
        .target = temp_local,
        .result = .fresh,
        .proc = proc_id,
        .args = args,
        .next = ret_stmt,
    } });

    const transformed = try makeTailRecursive(
        &store,
        proc_symbol,
        join_id,
        call_stmt,
        args,
        allocator,
    );

    try std.testing.expect(transformed != null);
}
