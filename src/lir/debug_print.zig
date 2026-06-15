//! Compact textual dump of LIR procs.
//!
//! Used by the TRMC golden tests (before/after transform diffs) and the
//! `-Dprint-ir-after-trmc` debug build option. The format aims for stability
//! and readability, not completeness — it is not a serialization format.

const std = @import("std");
const core = @import("lir_core");
const layout_mod = @import("layout");

const LIR = core.LIR;
const LirStore = core.LirStore;

/// Errors produced while printing: allocation for the visited set, or the
/// writer rejecting output.
pub const Error = std.mem.Allocator.Error || error{WriteFailed};

/// Write a single proc as indented text.
pub fn writeProc(
    gpa: std.mem.Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    proc_id: LIR.LirProcSpecId,
    writer: *std.Io.Writer,
) Error!void {
    const proc = store.getProcSpec(proc_id);

    try writer.print("proc p{d} args=[", .{@intFromEnum(proc_id)});
    const args = store.getLocalSpan(proc.args);
    for (args, 0..) |arg, i| {
        if (i > 0) try writer.writeAll(", ");
        try writeTypedLocal(store, layouts, arg, writer);
    }
    try writer.writeAll("] ret=");
    try writeLayout(layouts, proc.ret_layout, writer);
    if (proc.tail_transform != .none) {
        try writer.print(" transform={s}", .{@tagName(proc.tail_transform)});
    }
    try writer.writeAll("\n");

    if (proc.body) |body| {
        var printer = Printer{ .store = store, .layouts = layouts };
        defer printer.visited.deinit(gpa);
        try printer.writeChainInner(gpa, body, 1, writer);
    } else {
        try writer.writeAll("  <no body>\n");
    }
}

const Printer = struct {
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    // Lowered LIR statement graphs are DAGs — linear tails can be shared by
    // multiple predecessors. This prints each shared tail once (later arrivals
    // get an <<already printed>> marker) and guards malformed (cyclic) input
    // from looping forever.
    visited: std.AutoHashMapUnmanaged(LIR.CFStmtId, void) = .{},

    fn writeChainInner(self: *Printer, gpa: std.mem.Allocator, start: LIR.CFStmtId, indent: usize, writer: *std.Io.Writer) Error!void {
        var current = start;
        while (true) {
            const entry = try self.visited.getOrPut(gpa, current);
            if (entry.found_existing) {
                try writeIndent(indent, writer);
                try writer.print("<<already printed s{d}>>\n", .{@intFromEnum(current)});
                return;
            }

            const stmt = self.store.getCFStmt(current);
            switch (stmt) {
                .assign_ref => |s| {
                    try self.writeTarget(s.target, indent, writer);
                    switch (s.op) {
                        .local => |src| try writer.print("ref.local l{d}", .{@intFromEnum(src)}),
                        .discriminant => |d| try writer.print("ref.discriminant l{d}", .{@intFromEnum(d.source)}),
                        .field => |f| try writer.print("ref.field l{d}[{d}]", .{ @intFromEnum(f.source), f.field_idx }),
                        .tag_payload => |t| try writer.print("ref.tag_payload l{d} v{d}[{d}]", .{ @intFromEnum(t.source), t.variant_index, t.payload_idx }),
                        .tag_payload_struct => |t| try writer.print("ref.tag_payload_struct l{d} v{d}", .{ @intFromEnum(t.source), t.variant_index }),
                        .list_reinterpret => |l| try writer.print("ref.list_reinterpret l{d}", .{@intFromEnum(l.backing_ref)}),
                        .nominal => |n| try writer.print("ref.nominal l{d}", .{@intFromEnum(n.backing_ref)}),
                    }
                    try writer.writeAll("\n");
                    current = s.next;
                },
                .assign_literal => |s| {
                    try self.writeTarget(s.target, indent, writer);
                    switch (s.value) {
                        .i64_literal => |l| try writer.print("literal {d}", .{l.value}),
                        .i128_literal => |l| try writer.print("literal {d}", .{l.value}),
                        .f64_literal => |f| try writer.print("literal f64 {d}", .{f}),
                        .f32_literal => |f| try writer.print("literal f32 {d}", .{f}),
                        .dec_literal => |d| try writer.print("literal dec {d}", .{d}),
                        .str_literal => try writer.writeAll("literal str"),
                        .null_ptr => try writer.writeAll("literal null_ptr"),
                        .proc_ref => |p| try writer.print("literal proc_ref p{d}", .{@intFromEnum(p)}),
                    }
                    try writer.writeAll("\n");
                    current = s.next;
                },
                .assign_call => |s| {
                    try self.writeTarget(s.target, indent, writer);
                    try writer.print("call p{d}(", .{@intFromEnum(s.proc)});
                    try self.writeLocals(s.args, writer);
                    try writer.writeAll(")\n");
                    current = s.next;
                },
                .assign_call_erased => |s| {
                    try self.writeTarget(s.target, indent, writer);
                    try writer.print("call_erased l{d}(", .{@intFromEnum(s.closure)});
                    try self.writeLocals(s.args, writer);
                    try writer.writeAll(")\n");
                    current = s.next;
                },
                .assign_packed_erased_fn => |s| {
                    try self.writeTarget(s.target, indent, writer);
                    try writer.print("packed_erased_fn p{d}\n", .{@intFromEnum(s.proc)});
                    current = s.next;
                },
                .assign_low_level => |s| {
                    try self.writeTarget(s.target, indent, writer);
                    try writer.print("low_level {s}(", .{@tagName(s.op)});
                    try self.writeLocals(s.args, writer);
                    try writer.writeAll(")\n");
                    current = s.next;
                },
                .assign_list => |s| {
                    try self.writeTarget(s.target, indent, writer);
                    try writer.writeAll("list(");
                    try self.writeLocals(s.elems, writer);
                    try writer.writeAll(")\n");
                    current = s.next;
                },
                .assign_struct => |s| {
                    try self.writeTarget(s.target, indent, writer);
                    try writer.writeAll("struct(");
                    try self.writeLocals(s.fields, writer);
                    try writer.writeAll(")\n");
                    current = s.next;
                },
                .assign_tag => |s| {
                    try self.writeTarget(s.target, indent, writer);
                    try writer.print("tag v{d} d{d}", .{ s.variant_index, s.discriminant });
                    if (s.payload) |payload| try writer.print(" (l{d})", .{@intFromEnum(payload)});
                    try writer.writeAll("\n");
                    current = s.next;
                },
                .set_local => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("set l{d} := l{d} ({s})\n", .{ @intFromEnum(s.target), @intFromEnum(s.value), @tagName(s.mode) });
                    current = s.next;
                },
                .debug => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("debug l{d}\n", .{@intFromEnum(s.message)});
                    current = s.next;
                },
                .expect => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("expect l{d}\n", .{@intFromEnum(s.condition)});
                    current = s.next;
                },
                .comptime_branch_taken => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("comptime_branch_taken site={d} branch={d}\n", .{ @intFromEnum(s.site), s.branch_index });
                    current = s.next;
                },
                .expect_err => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("expect_err l{d}\n", .{@intFromEnum(s.message)});
                    return;
                },
                .incref => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("incref l{d} x{d}\n", .{ @intFromEnum(s.value), s.count });
                    current = s.next;
                },
                .decref => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("decref l{d}\n", .{@intFromEnum(s.value)});
                    current = s.next;
                },
                .free => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("free l{d}\n", .{@intFromEnum(s.value)});
                    current = s.next;
                },
                .switch_stmt => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("switch l{d}\n", .{@intFromEnum(s.cond)});
                    for (self.store.getCFSwitchBranches(s.branches)) |branch| {
                        try writeIndent(indent + 1, writer);
                        try writer.print("case {d}:\n", .{branch.value});
                        try self.writeChainInner(gpa, branch.body, indent + 2, writer);
                    }
                    try writeIndent(indent + 1, writer);
                    try writer.writeAll("default:\n");
                    try self.writeChainInner(gpa, s.default_branch, indent + 2, writer);
                    if (s.continuation) |continuation| {
                        try writeIndent(indent + 1, writer);
                        try writer.writeAll("continuation:\n");
                        try self.writeChainInner(gpa, continuation, indent + 2, writer);
                    }
                    return;
                },
                .str_match => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("str_match l{d} prefix_len={d} end={s}\n", .{ @intFromEnum(s.source), s.prefix.len, @tagName(s.end) });
                    for (self.store.getStrMatchSteps(s.steps), 0..) |step, index| {
                        try writeIndent(indent + 1, writer);
                        try writer.print("step {d} capture=", .{index});
                        switch (step.capture) {
                            .discard => try writer.writeAll("_"),
                            .view => |local| try writer.print("view l{d}", .{@intFromEnum(local)}),
                        }
                        try writer.print(" delimiter_len={d}\n", .{step.delimiter.len});
                    }
                    try writeIndent(indent + 1, writer);
                    try writer.writeAll("match:\n");
                    try self.writeChainInner(gpa, s.on_match, indent + 2, writer);
                    try writeIndent(indent + 1, writer);
                    try writer.writeAll("miss:\n");
                    try self.writeChainInner(gpa, s.on_miss, indent + 2, writer);
                    return;
                },
                .join => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("join j{d} params=[", .{@intFromEnum(s.id)});
                    try self.writeLocals(s.params, writer);
                    try writer.writeAll("]\n");
                    try writeIndent(indent + 1, writer);
                    try writer.writeAll("remainder:\n");
                    try self.writeChainInner(gpa, s.remainder, indent + 2, writer);
                    try writeIndent(indent + 1, writer);
                    try writer.writeAll("body:\n");
                    try self.writeChainInner(gpa, s.body, indent + 2, writer);
                    return;
                },
                .jump => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("jump j{d}\n", .{@intFromEnum(s.target)});
                    return;
                },
                .ret => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("ret l{d}\n", .{@intFromEnum(s.value)});
                    return;
                },
                .crash => {
                    try writeIndent(indent, writer);
                    try writer.writeAll("crash\n");
                    return;
                },
                .runtime_error => {
                    try writeIndent(indent, writer);
                    try writer.writeAll("runtime_error\n");
                    return;
                },
                .comptime_exhaustiveness_failed => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("comptime_exhaustiveness_failed site={d}\n", .{@intFromEnum(s.site)});
                    return;
                },
                .loop_continue => {
                    try writeIndent(indent, writer);
                    try writer.writeAll("loop_continue\n");
                    return;
                },
                .loop_break => {
                    try writeIndent(indent, writer);
                    try writer.writeAll("loop_break\n");
                    return;
                },
            }
        }
    }

    fn writeTarget(self: *Printer, target: LIR.LocalId, indent: usize, writer: *std.Io.Writer) Error!void {
        try writeIndent(indent, writer);
        try writeTypedLocal(self.store, self.layouts, target, writer);
        try writer.writeAll(" = ");
    }

    fn writeLocals(self: *Printer, span: LIR.LocalSpan, writer: *std.Io.Writer) Error!void {
        for (self.store.getLocalSpan(span), 0..) |local, i| {
            if (i > 0) try writer.writeAll(", ");
            try writer.print("l{d}", .{@intFromEnum(local)});
        }
    }
};

fn writeTypedLocal(
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    local: LIR.LocalId,
    writer: *std.Io.Writer,
) Error!void {
    try writer.print("l{d}:", .{@intFromEnum(local)});
    try writeLayout(layouts, store.getLocal(local).layout_idx, writer);
}

fn writeLayout(layouts: *const layout_mod.Store, idx: layout_mod.Idx, writer: *std.Io.Writer) Error!void {
    const raw = @intFromEnum(idx);
    const sentinel_names = [_][]const u8{
        "bool", "str", "u8",   "i8",   "u16", "i16", "u32", "i32",
        "u64",  "i64", "u128", "i128", "f32", "f64", "dec", "opaque_ptr",
        "zst",
    };
    if (raw < sentinel_names.len) {
        try writer.writeAll(sentinel_names[raw]);
        return;
    }
    try writer.print("{s}#{d}", .{ @tagName(layouts.getLayout(idx).tag), raw });
}

fn writeIndent(indent: usize, writer: *std.Io.Writer) Error!void {
    for (0..indent) |_| try writer.writeAll("  ");
}
