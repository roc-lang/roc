//! Compact textual dump of LIR procs.
//!
//! Used by the TRMC golden tests (before/after transform diffs) and the
//! `-Dprint-ir-after-trmc` debug build option. The format aims for stability
//! and readability, not completeness—it is not a serialization format.

const std = @import("std");
const collections = @import("collections");
const core = @import("lir_core");
const layout_mod = @import("layout");

const LIR = core.LIR;
const LirStore = core.LirStore;
const DynOps = core.DynOps;
const GuardedList = collections.GuardedList;

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

    try writer.print("proc p{d} args=[", .{@backingInt(proc_id)});
    const args = store.getLocalSpan(proc.args);
    for (0..args.len) |i| {
        const arg = GuardedList.at(args, i);
        if (i > 0) try writer.writeAll(", ");
        try writeTypedLocal(store, layouts, arg, writer);
    }
    try writer.writeAll("] ret=");
    try writeLayout(layouts, proc.ret_layout, writer);
    if (proc.tail_transform != .none) {
        try writer.print(" transform={s}", .{@tagName(proc.tail_transform)});
    }
    if (proc.tail_group) |group| try writer.print(" tail_group={d}", .{@backingInt(group)});
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
    // Lowered LIR statement graphs are DAGs—linear tails can be shared by
    // multiple predecessors. This prints each shared tail once (later arrivals
    // get an <<already printed>> marker) and guards malformed (cyclic) input
    // from looping forever.
    visited: collections.DenseMapUnmanaged(LIR.CFStmtId, void) = .{},

    fn writeChainInner(self: *Printer, gpa: std.mem.Allocator, start: LIR.CFStmtId, indent: usize, writer: *std.Io.Writer) Error!void {
        var current = start;
        while (true) {
            const entry = try self.visited.getOrPut(gpa, current);
            if (entry.found_existing) {
                try writeIndent(indent, writer);
                try writer.writeAll("<<already printed>>\n");
                return;
            }

            const stmt = self.store.getCFStmtImmediate(current);
            switch (stmt.*) {
                .assign_ref => |s| {
                    try self.writeTarget(s.target, indent, writer);
                    switch (s.op.kind) {
                        .local => try writer.print("ref.local l{d}", .{@backingInt(s.op.source)}),
                        .discriminant => try writer.print("ref.discriminant l{d}", .{@backingInt(s.op.source)}),
                        .field => try writer.print("ref.field l{d}[{d}]", .{ @backingInt(s.op.source), s.op.index }),
                        .tag_payload => try writer.print("ref.tag_payload l{d} v{d}[{d}]", .{ @backingInt(s.op.source), s.op.variant_index, s.op.index }),
                        .tag_payload_struct => try writer.print("ref.tag_payload_struct l{d} v{d}", .{ @backingInt(s.op.source), s.op.variant_index }),
                        .list_reinterpret => try writer.print("ref.list_reinterpret l{d}", .{@backingInt(s.op.source)}),
                        .nominal => try writer.print("ref.nominal l{d}", .{@backingInt(s.op.source)}),
                    }
                    if (self.store.stmtRowOrDefault(s.extra).take_kind == .take) try writer.writeAll(" take");
                    const absent_fields = self.store.getU32Span(self.store.stmtRowOrDefault(s.extra).residual_shell_absent_fields);
                    if (absent_fields.len != 0) {
                        try writer.writeAll(" shell_absent={");
                        for (0..absent_fields.len) |index| {
                            if (index != 0) try writer.writeAll(",");
                            try writer.print("{d}", .{GuardedList.at(absent_fields, index)});
                        }
                        try writer.writeAll("}");
                    }
                    try writer.writeAll("\n");
                    current = s.next;
                },
                .assign_literal => |s| {
                    try self.writeTarget(s.target, indent, writer);
                    switch (s.value) {
                        .i64_literal => |l| try writer.print("literal {d}", .{l.value}),
                        .i128_literal => |l_id| blk_l: {
                            const l = self.store.stmtRow(l_id);
                            break :blk_l try writer.print("literal {d}", .{l.value});
                        },
                        .f64_literal => |f| try writer.print("literal f64 {d}", .{f}),
                        .f32_literal => |f| try writer.print("literal f32 {d}", .{f}),
                        .dec_literal => |d| try writer.print("literal dec {d}", .{d}),
                        .str_literal => try writer.writeAll("literal str"),
                        .static_data => |read| try writer.print("literal static_data s{d}", .{@backingInt(read.id)}),
                        .bytes_literal => try writer.writeAll("literal bytes"),
                        .proc_ref => |p| try writer.print("literal proc_ref p{d}", .{@backingInt(p)}),
                    }
                    const extra = self.store.literalExtra(s.value);
                    if (extra.fresh_alternative) |proc| try writer.print(" fresh=p{d}", .{@backingInt(proc)});
                    if (extra.list_len) |len| try writer.print(" len={d}", .{len});
                    try writer.writeAll("\n");
                    current = s.next;
                },
                .init_uninitialized => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("init_uninitialized l{d}\n", .{@backingInt(s.target)});
                    current = s.next;
                },
                .assign_call => |s| {
                    try self.writeTarget(s.target, indent, writer);
                    try writer.print("call p{d}(", .{@backingInt(s.proc)});
                    try self.writeLocals(s.args, writer);
                    try writer.writeByte(')');
                    const extra = self.store.stmtRowOrDefault(s.extra);
                    if (extra.is_cold) try writer.writeAll(" cold");
                    if (extra.replaces_frame) try writer.writeAll(" replaces_frame");
                    if (extra.drive != .none) try writer.print(" drive={s}", .{@tagName(extra.drive)});
                    try writer.writeByte('\n');
                    current = s.next;
                },
                .assign_call_erased => |inline_s| {
                    const s = self.store.stmtRow(inline_s.data);
                    try self.writeTarget(inline_s.target, indent, writer);
                    try writer.print("call_erased l{d}(", .{@backingInt(s.closure)});
                    try self.writeLocals(s.args, writer);
                    try writer.writeByte(')');
                    if (s.reuse_closure) try writer.writeAll(" reuse_closure");
                    if (s.reuse_source) |reuse_source| {
                        try writer.print(" reuse_source=l{d}", .{@backingInt(reuse_source)});
                    }
                    if (s.deferred) try writer.writeAll(" deferred");
                    if (s.drive != .none) try writer.print(" drive={s}", .{@tagName(s.drive)});
                    try writer.writeByte('\n');
                    current = inline_s.next;
                },
                .assign_packed_erased_fn => |s_stmt| {
                    const s = self.store.stmtRow(s_stmt.data);
                    try self.writeTarget(s_stmt.target, indent, writer);
                    try writer.print("packed_erased_fn p{d}", .{@backingInt(s.proc)});
                    if (s.capture) |capture| {
                        try writer.print(" capture=l{d}", .{@backingInt(capture)});
                    }
                    if (s.capture_layout) |capture_layout| {
                        try writer.writeAll(" capture_layout=");
                        try writeLayout(self.layouts, capture_layout, writer);
                    }
                    try writer.print(" on_drop={s}", .{@tagName(s.on_drop)});
                    if (s.reuse) |reuse| {
                        try writer.print(" reuse=l{d}", .{@backingInt(reuse)});
                        if (s.reuse_unique) try writer.writeAll(" unique");
                    }
                    try writer.writeByte('\n');
                    current = s_stmt.next;
                },
                .assign_low_level => |s| {
                    try self.writeTarget(s.target, indent, writer);
                    try writer.print("low_level {s}(", .{@tagName(s.op)});
                    try self.writeLocals(s.args, writer);
                    try writer.writeAll(")");
                    if (self.store.stmtRowOrDefault(s.extra).unique_args != 0) try writer.print(" unique=0x{x}", .{self.store.stmtRowOrDefault(s.extra).unique_args});
                    if (self.store.stmtRowOrDefault(s.extra).simd_concat_count) |count| try writer.print(" concat_count={d}", .{count});
                    try writer.writeAll("\n");
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
                    if (s.payload) |payload| try writer.print(" (l{d})", .{@backingInt(payload)});
                    try writer.writeAll("\n");
                    current = s.next;
                },
                .store_struct => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("store_struct l{d} layout=", .{@backingInt(s.dest)});
                    try writeLayout(self.layouts, s.struct_layout, writer);
                    try writer.writeAll("(");
                    try self.writeLocals(s.fields, writer);
                    try writer.writeAll(")\n");
                    current = s.next;
                },
                .store_tag => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("store_tag l{d} layout=", .{@backingInt(s.dest)});
                    try writeLayout(self.layouts, s.tag_layout, writer);
                    try writer.print(" v{d} d{d}", .{ s.variant_index, s.discriminant });
                    if (s.payload) |payload| try writer.print(" (l{d})", .{@backingInt(payload)});
                    try writer.writeAll("\n");
                    current = s.next;
                },
                .set_local => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("set l{d} := l{d} ({s})\n", .{ @backingInt(s.target), @backingInt(s.value), @tagName(s.mode) });
                    current = s.next;
                },
                .debug => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("debug l{d}\n", .{@backingInt(s.message)});
                    current = s.next;
                },
                .expect => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("expect l{d}\n", .{@backingInt(s.condition)});
                    current = s.next;
                },
                .expect_err => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("expect_err l{d}\n", .{@backingInt(s.message)});
                    return;
                },
                .incref => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("incref l{d} x{d} ", .{ @backingInt(s.value), s.count });
                    try writeRcHelper(self.store.rcHelper(s.rc), writer);
                    try writeAtomicity(s.atomicity, writer);
                    try writer.writeAll("\n");
                    current = s.next;
                },
                .decref => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("decref l{d} ", .{@backingInt(s.value)});
                    try writeRcHelper(self.store.rcHelper(s.rc), writer);
                    try writeAtomicity(s.atomicity, writer);
                    try writer.writeAll("\n");
                    current = s.next;
                },
                .decref_if_initialized => |inline_s| {
                    const s = self.store.stmtRow(inline_s.data);
                    try writeIndent(indent, writer);
                    try writer.print("decref_if_initialized cond=l{d} mask=0x{x} value=l{d} ", .{
                        @backingInt(inline_s.cond),
                        s.cond_mask,
                        @backingInt(inline_s.value),
                    });
                    try writeRcHelper(s.rc, writer);
                    try writeAtomicity(s.atomicity, writer);
                    try writer.writeAll("\n");
                    current = inline_s.next;
                },
                .free => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("free l{d} ", .{@backingInt(s.value)});
                    try writeRcHelper(self.store.rcHelper(s.rc), writer);
                    try writeAtomicity(s.atomicity, writer);
                    try writer.writeAll("\n");
                    current = s.next;
                },
                .switch_stmt => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("switch l{d} default_cold={}\n", .{
                        @backingInt(s.cond),
                        s.default_is_cold,
                    });
                    const branches = self.store.getCFSwitchBranches(s.branches);
                    for (0..branches.len) |branch_index| {
                        const branch = GuardedList.at(branches, branch_index);
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
                .switch_initialized_payload => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("switch_initialized_payload cond=l{d} mask=0x{x} payload=l{d} uninitialized_cold={}\n", .{
                        @backingInt(s.cond),
                        s.cond_mask,
                        @backingInt(s.payload),
                        s.uninitialized_is_cold,
                    });
                    try writeIndent(indent + 1, writer);
                    try writer.writeAll("initialized:\n");
                    try self.writeChainInner(gpa, s.initialized_branch, indent + 2, writer);
                    try writeIndent(indent + 1, writer);
                    try writer.writeAll("uninitialized:\n");
                    try self.writeChainInner(gpa, s.uninitialized_branch, indent + 2, writer);
                    return;
                },
                .str_match => |s_stmt| {
                    const s = self.store.stmtRow(s_stmt.data);
                    try writeIndent(indent, writer);
                    try writer.print("str_match l{d} prefix_len={d} end={s}\n", .{ @backingInt(s_stmt.source), s.prefix.len, @tagName(s.end) });
                    const steps = self.store.getStrMatchSteps(s.steps);
                    for (0..steps.len) |index| {
                        const step = GuardedList.at(steps, index);
                        try writeIndent(indent + 1, writer);
                        try writer.print("step {d} capture=", .{index});
                        switch (step.capture) {
                            .discard => try writer.writeAll("_"),
                            .view => |local| try writer.print("view l{d}", .{@backingInt(local)}),
                        }
                        try writer.print(" delimiter_len={d}\n", .{step.delimiter.len});
                    }
                    try writeIndent(indent + 1, writer);
                    try writer.writeAll("match:\n");
                    try self.writeChainInner(gpa, s_stmt.on_match, indent + 2, writer);
                    try writeIndent(indent + 1, writer);
                    try writer.writeAll("miss:\n");
                    try self.writeChainInner(gpa, s_stmt.on_miss, indent + 2, writer);
                    return;
                },
                .str_match_set => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("str_match_set l{d} arms={d}\n", .{ @backingInt(s.source), s.arms.len });
                    const arms = self.store.getStrMatchArms(s.arms);
                    for (0..arms.len) |arm_index| {
                        const arm = GuardedList.at(arms, arm_index);
                        try writeIndent(indent + 1, writer);
                        try writer.print("arm {d} prefix_len={d} end={s}\n", .{ arm_index, arm.prefix.len, @tagName(arm.end) });
                        const steps = self.store.getStrMatchSteps(arm.steps);
                        for (0..steps.len) |step_index| {
                            const step = GuardedList.at(steps, step_index);
                            try writeIndent(indent + 2, writer);
                            try writer.print("step {d} capture=", .{step_index});
                            switch (step.capture) {
                                .discard => try writer.writeAll("_"),
                                .view => |local| try writer.print("view l{d}", .{@backingInt(local)}),
                            }
                            try writer.print(" delimiter_len={d}\n", .{step.delimiter.len});
                        }
                        try writeIndent(indent + 2, writer);
                        try writer.writeAll("match:\n");
                        try self.writeChainInner(gpa, arm.on_match, indent + 3, writer);
                    }
                    try writeIndent(indent + 1, writer);
                    try writer.writeAll("miss:\n");
                    try self.writeChainInner(gpa, s.on_miss, indent + 2, writer);
                    return;
                },
                .join => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("join j{d} params=[", .{@backingInt(s.id)});
                    try self.writeLocals(s.params, writer);
                    try writer.writeAll("]");
                    if (!self.store.stmtRowOrDefault(s.extra).retained.isEmpty()) {
                        try writer.writeAll(" retained=[");
                        try self.writeLocals(self.store.stmtRowOrDefault(s.extra).retained, writer);
                        try writer.writeAll("]");
                    }
                    const extra = self.store.stmtRowOrDefault(s.extra);
                    if (!extra.maybe_uninitialized_params.isEmpty()) {
                        try writer.writeAll(" maybe_uninitialized=[");
                        try self.writeLocals(extra.maybe_uninitialized_params, writer);
                        try writer.writeAll("]");
                        try writer.writeAll(" conditions=[");
                        try self.writeLocals(extra.maybe_uninitialized_conditions, writer);
                        try writer.writeAll("]");
                        try writer.writeAll(" masks=[");
                        const masks = self.store.getU64Span(extra.maybe_uninitialized_condition_masks);
                        for (0..masks.len) |index| {
                            const mask = GuardedList.at(masks, index);
                            if (index > 0) try writer.writeAll(", ");
                            try writer.print("0x{x}", .{mask});
                        }
                        try writer.writeAll("]");
                    }
                    try writer.writeAll("\n");
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
                    try writer.print("jump j{d}\n", .{@backingInt(s.target)});
                    return;
                },
                .ret => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("ret l{d}\n", .{@backingInt(s.value)});
                    return;
                },
                .crash => |s| {
                    try writeIndent(indent, writer);
                    switch (s.msg) {
                        .literal => try writer.writeAll("crash\n"),
                        .local => |local| try writer.print("crash l{d}\n", .{@backingInt(local)}),
                    }
                    return;
                },
                .dyn => |s| {
                    if (s.target) |target| {
                        try self.writeTarget(target, indent, writer);
                    } else {
                        try writeIndent(indent, writer);
                    }
                    try self.writeDynOp(self.store.stmtRow(s.data), writer);
                    try writer.writeAll("\n");
                    current = s.next;
                },
                .runtime_error => {
                    try writeIndent(indent, writer);
                    try writer.writeAll("runtime_error\n");
                    return;
                },
                .comptime_exhaustiveness_failed => |s| {
                    try writeIndent(indent, writer);
                    try writer.print("comptime_exhaustiveness_failed owner={d} checked_site={?d}\n", .{ @backingInt(s.owner), if (s.checked_site) |site| @backingInt(site) else null });
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

    fn writeDynOp(self: *Printer, op: LIR.DynOp, writer: *std.Io.Writer) Error!void {
        try writer.print("dyn.{s}", .{@tagName(op)});
        switch (op) {
            .desc_tag_union => |desc| {
                try writer.writeAll(" names(");
                try self.writeLocals(desc.names, writer);
                try writer.writeAll(") counts(");
                const counts = self.store.getU32Span(desc.payload_counts);
                for (0..counts.len) |i| {
                    if (i > 0) try writer.writeAll(", ");
                    try writer.print("{d}", .{GuardedList.at(counts, i)});
                }
                try writer.writeAll(") payloads(");
                try self.writeLocals(desc.payloads, writer);
                try writer.writeAll(")");
                if (desc.ext) |ext| try writer.print(" ext(l{d})", .{@backingInt(ext)});
                return;
            },
            .dict_compose => |dict| try writer.print(" p{d}", .{@backingInt(dict.worker)}),
            .closure => |closure| try writer.print(" p{d}", .{@backingInt(closure.proc)}),
            .dict_method => |method| try writer.print(" #{d}", .{method.index}),
            .field_addr => |field| try writer.print(" #{d}", .{field.field_index}),
            .tag_payload_addr => |payload| try writer.print(" #{d}", .{payload.payload_index}),
            .desc_nominal => |nominal| if (nominal.inspect_opaque) try writer.writeAll(" opaque"),
            .frame_alloc, .desc_list, .desc_box, .desc_record, .desc_tuple, .desc_presence_slot, .desc_args, .desc_recursive_box, .desc_recursive_open, .desc_recursive_close, .dict_record, .copy, .field_addr_named, .list_item_addr, .box_payload_addr, .tag_discriminant, .tag_discriminant_of, .record, .record_update, .tag, .list, .box, .slot_alloc, .slot_store, .eq, .hash, .inspect, .call_erased, .view, .adapt => {},
        }
        const operands = DynOps.Operands.of(op);
        for (operands.singles[0..operands.single_count]) |local| try writer.print(" l{d}", .{@backingInt(local)});
        for (operands.spans[0..operands.span_count]) |span| {
            try writer.writeAll(" (");
            try self.writeLocals(span, writer);
            try writer.writeAll(")");
        }
    }

    fn writeLocals(self: *Printer, span: LIR.LocalSpan, writer: *std.Io.Writer) Error!void {
        const locals = self.store.getLocalSpan(span);
        for (0..locals.len) |i| {
            const local = GuardedList.at(locals, i);
            if (i > 0) try writer.writeAll(", ");
            try writer.print("l{d}", .{@backingInt(local)});
        }
    }
};

fn writeTypedLocal(
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    local: LIR.LocalId,
    writer: *std.Io.Writer,
) Error!void {
    try writer.print("l{d}:", .{@backingInt(local)});
    const value = store.getLocal(local);
    if (value.dyn) |dyn| {
        try writer.print("dyn({s},l{d})", .{ @tagName(dyn.storage), @backingInt(dyn.desc) });
        return;
    }
    try writeLayout(layouts, value.layout_idx, writer);
}

fn writeLayout(layouts: *const layout_mod.Store, idx: layout_mod.Idx, writer: *std.Io.Writer) Error!void {
    const raw = @backingInt(idx);
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

/// Atomic is the default every consumer may assume, so only a
/// single-threaded count update is marked.
fn writeAtomicity(atomicity: LIR.RcAtomicity, writer: *std.Io.Writer) Error!void {
    if (atomicity == .single_thread) try writer.writeAll(" single_thread");
}

fn writeRcHelper(helper: LIR.RcHelper, writer: *std.Io.Writer) Error!void {
    switch (helper) {
        .concrete => |rc| try writer.print("rc=concrete({s},{d})", .{ @tagName(rc.op), @backingInt(rc.layout_idx) }),
    }
}

fn writeIndent(indent: usize, writer: *std.Io.Writer) Error!void {
    for (0..indent) |_| try writer.writeAll("  ");
}

/// A fingerprint of a proc's LIR that is the same in every program that
/// lowers the same procedure the same way: the debug text with every
/// program-numbered reference renamed. A referenced proc is named by its
/// content identity; locals, joins, static data, layouts, and the remaining
/// store-numbered ids are renumbered in order
/// of first appearance, each in its own namespace. The object cache's
/// diagnostics compare these across programs.
pub fn procFingerprint(
    gpa: std.mem.Allocator,
    store: *const LirStore,
    layouts: *const layout_mod.Store,
    proc_id: LIR.LirProcSpecId,
) Error!u64 {
    var text: std.Io.Writer.Allocating = .init(gpa);
    defer text.deinit();
    try writeProc(gpa, store, layouts, proc_id, &text.writer);
    const source = text.written();

    var canonical: std.Io.Writer.Allocating = .init(gpa);
    defer canonical.deinit();
    var names = std.StringHashMap(std.AutoHashMap(u64, u32)).init(gpa);
    defer {
        var iter = names.valueIterator();
        while (iter.next()) |map| map.deinit();
        names.deinit();
    }

    var index: usize = 0;
    while (index < source.len) {
        const at_boundary = index == 0 or !isIdentByte(source[index - 1]);
        if (at_boundary) {
            if (try canonicalReference(gpa, store, source, index, &names, &canonical.writer)) |next| {
                index = next;
                continue;
            }
        }
        try canonical.writer.writeByte(source[index]);
        index += 1;
    }
    return std.hash.Wyhash.hash(0, canonical.written());
}

fn isIdentByte(byte: u8) bool {
    return std.ascii.isAlphanumeric(byte) or byte == '_';
}

fn digitsEnd(source: []const u8, start: usize) usize {
    var end = start;
    while (end < source.len and std.ascii.isDigit(source[end])) end += 1;
    return end;
}

/// Rewrites the program-numbered reference starting at `start`, if one does,
/// and returns where it ends.
fn canonicalReference(
    gpa: std.mem.Allocator,
    store: *const LirStore,
    source: []const u8,
    start: usize,
    names: *std.StringHashMap(std.AutoHashMap(u64, u32)),
    writer: *std.Io.Writer,
) Error!?usize {
    // `l12`, `j3`, `p7`, and `static_data s4`.
    const letter = source[start];
    if (letter == 'l' or letter == 'j' or letter == 'p' or
        (letter == 's' and std.mem.endsWith(u8, source[0..start], "static_data ")))
    {
        const end = digitsEnd(source, start + 1);
        if (end > start + 1 and (end == source.len or !isIdentByte(source[end]))) {
            const value = std.fmt.parseInt(u64, source[start + 1 .. end], 10) catch return null;
            if (letter == 'p') {
                const identity = store.getProcSpecImmediate(@fromBackingInt(@intCast(@as(u32, @intCast(value))))).identity;
                try writer.print("p{s}", .{&identity.symbolHex()});
            } else {
                try writer.print("{c}{d}", .{ letter, try canonicalId(gpa, names, source[start .. start + 1], value) });
            }
            return end;
        }
    }
    // `list#26` and `rc=concrete(decref,26)`.
    var word_end = start;
    while (word_end < source.len and isIdentByte(source[word_end])) word_end += 1;
    const word = source[start..word_end];
    if (word.len == 0) return null;
    if (word_end < source.len and source[word_end] == '#') {
        const end = digitsEnd(source, word_end + 1);
        if (end == word_end + 1) return null;
        const value = std.fmt.parseInt(u64, source[word_end + 1 .. end], 10) catch return null;
        try writer.print("{s}#{d}", .{ word, try canonicalId(gpa, names, "layout", value) });
        return end;
    }
    if (std.mem.eql(u8, word, "concrete") and word_end < source.len and source[word_end] == '(') {
        const comma = std.mem.findScalarPos(u8, source, word_end, ',') orelse return null;
        const end = digitsEnd(source, comma + 1);
        if (end == comma + 1) return null;
        const value = std.fmt.parseInt(u64, source[comma + 1 .. end], 10) catch return null;
        try writer.print("{s},{d}", .{ source[start..comma], try canonicalId(gpa, names, "layout", value) });
        return end;
    }
    // `name=`, `adapter=`, `method=`, and `site=` carry store ids.
    for ([_][]const u8{ "name", "adapter", "method", "site" }) |key| {
        if (!std.mem.eql(u8, word, key) or word_end >= source.len or source[word_end] != '=') continue;
        const end = digitsEnd(source, word_end + 1);
        if (end == word_end + 1) return null;
        const value = std.fmt.parseInt(u64, source[word_end + 1 .. end], 10) catch return null;
        try writer.print("{s}={d}", .{ key, try canonicalId(gpa, names, key, value) });
        return end;
    }
    return null;
}

fn canonicalId(
    gpa: std.mem.Allocator,
    names: *std.StringHashMap(std.AutoHashMap(u64, u32)),
    namespace: []const u8,
    value: u64,
) Error!u32 {
    const space = try names.getOrPut(namespace);
    if (!space.found_existing) space.value_ptr.* = std.AutoHashMap(u64, u32).init(gpa);
    const entry = try space.value_ptr.getOrPut(value);
    if (!entry.found_existing) entry.value_ptr.* = @intCast(space.value_ptr.count() - 1);
    return entry.value_ptr.*;
}

test "proc fingerprint ignores program numbering but not content" {
    const allocator = std.testing.allocator;
    var layouts = try layout_mod.Store.init(allocator, .u64);
    defer layouts.deinit();

    const Program = struct {
        fn build(store: *LirStore, padding: usize, callee_identity: u8) std.mem.Allocator.Error!LIR.LirProcSpecId {
            // Unrelated locals and procs first shift every program-wide number.
            for (0..padding) |_| _ = try store.addLocal(.{ .layout_idx = .u64 });
            for (0..padding) |index| {
                _ = try store.addProcSpec(.{
                    .name = .none,
                    .identity = LIR.ProcIdentity.forTest(@intCast(100 + index)),
                    .args = .empty(),
                    .body = null,
                    .ret_layout = .u64,
                }, .none);
            }
            const callee = try store.addProcSpec(.{
                .name = .none,
                .identity = LIR.ProcIdentity.forTest(callee_identity),
                .args = .empty(),
                .body = null,
                .ret_layout = .str,
            }, .none);
            const value = try store.addLocal(.{ .layout_idx = .str });
            const ret = try store.addCFStmt(.{ .ret = .{ .value = value } }, .test_fixture);
            const call = try store.addCFStmt(.{ .assign_call = .{
                .target = value,
                .proc = callee,
                .args = try store.addLocalSpan(&.{}),
                .next = ret,
            } }, .test_fixture);
            return try store.addProcSpec(.{
                .name = .none,
                .identity = LIR.ProcIdentity.forTest(1),
                .args = .empty(),
                .body = call,
                .ret_layout = .str,
            }, .none);
        }
    };

    var first = LirStore.init(allocator);
    defer first.deinit();
    var second = LirStore.init(allocator);
    defer second.deinit();
    var other_callee = LirStore.init(allocator);
    defer other_callee.deinit();
    const first_proc = try Program.build(&first, 0, 7);
    const second_proc = try Program.build(&second, 3, 7);
    const other_proc = try Program.build(&other_callee, 0, 8);

    const first_print = try procFingerprint(allocator, &first, &layouts, first_proc);
    try std.testing.expectEqual(first_print, try procFingerprint(allocator, &second, &layouts, second_proc));
    try std.testing.expect(first_print != try procFingerprint(allocator, &other_callee, &layouts, other_proc));
}
