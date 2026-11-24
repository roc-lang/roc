//! Per-pass LIR dumps for the lir-passes model in `models/lir-passes` (`ROC_LIR_PASS_DUMP`).
//!
//! The format is specified in `models/lir-passes/FORMAT.md`.
//!
//! Developer diagnostic only: compiled to nothing in a compiler that does not
//! read developer diagnostics. Each dump is one block in a
//! line-oriented format (one statement per line, ids not pointers), appended
//! to the file the variable names with one write per block, so blocks from
//! parallel workers never interleave:
//!
//!   @@LPD begin <kind> run=<r> stage=<s> label=<label> proc=<p|*>
//!   ... L/P/V/S lines ...
//!   @@LPD end
//!
//! `kind` is `program` (every procedure, at a stage boundary) or `phase`
//! (one procedure before and after one procedure-local phase that changed
//! it, as `pre`/`post` sections).

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const core = @import("lir_core");
const layout_mod = @import("layout");

const LIR = core.LIR;
const LirStore = core.LirStore;
const GuardedList = collections.GuardedList;

/// Writing a dump fails only when its writer or an allocation fails.
const DumpError = std.Io.Writer.Error || std.mem.Allocator.Error;
const Allocator = std.mem.Allocator;

/// Whether dumps are requested in this process.
pub fn enabled() bool {
    if (!base.dev_diagnostics.available) return false;
    return base.dev_diagnostics.get(.lir_pass_dump) != null;
}

var run_counter = std.atomic.Value(u32).init(0);
var stage_counter = std.atomic.Value(u32).init(0);
/// The run and stage of the most recent program dump; phase dumps carry it
/// so a model can resolve callees from the program dump before the phase.
pub var current_run = std.atomic.Value(u32).init(0);
/// The stage of the most recent program dump; see `current_run`.
pub var current_stage = std.atomic.Value(u32).init(0);

/// Start a new lowering run.
pub fn beginRun() void {
    if (!enabled()) return;
    current_run.store(run_counter.fetchAdd(1, .monotonic) + 1, .monotonic);
}

/// Dump every procedure at a stage boundary.
pub fn program(result: *const core.Program.Result, label: []const u8) void {
    if (!enabled()) return;
    const store = &result.store;
    const layouts = &result.layouts;
    const stage = stage_counter.fetchAdd(1, .monotonic) + 1;
    current_stage.store(stage, .monotonic);
    var buffer: std.Io.Writer.Allocating = .init(std.heap.page_allocator);
    defer buffer.deinit();
    const w = &buffer.writer;
    writeAll(store, layouts, label, stage, w) catch return;
    writeStaticData(result, w) catch return;
    emit(buffer.written());
}

/// Append one whole block to the file `ROC_LIR_PASS_DUMP` names, with a
/// single `write` on an `O_APPEND` descriptor so blocks from parallel workers
/// never interleave with each other or with the compiler's own output.
fn emit(bytes: []const u8) void {
    const path = base.dev_diagnostics.get(.lir_pass_dump) orelse return;
    var path_buffer: [4096]u8 = undefined;
    const path_z = std.fmt.bufPrintSentinel(&path_buffer, "{s}", .{path}, 0) catch return;
    const fd = std.c.open(path_z, .{ .ACCMODE = .WRONLY, .CREAT = true, .APPEND = true, .CLOEXEC = true }, @as(std.c.mode_t, 0o644));
    if (fd < 0) return;
    defer _ = std.c.close(fd);
    var written: usize = 0;
    while (written < bytes.len) {
        const result = std.c.write(fd, bytes[written..].ptr, bytes.len - written);
        if (result <= 0) return;
        written += @intCast(result);
    }
}

fn writeAll(store: *const LirStore, layouts: *const layout_mod.Store, label: []const u8, stage: u32, w: *std.Io.Writer) DumpError!void {
    try w.print("@@LPD begin program run={d} stage={d} label={s} proc=*\n", .{ current_run.load(.monotonic), stage, label });
    var seen = LayoutSet.init(std.heap.page_allocator);
    defer seen.deinit();
    for (0..store.procSpecCount()) |index| {
        const proc_id: LIR.LirProcSpecId = @fromBackingInt(@intCast(@as(u32, @intCast(index))));
        try writeProc(store, proc_id, w, &seen);
    }
    try writeLayouts(layouts, &seen, w);
}

/// One `D <slot> <layout> ...` line per static data slot: its initializer
/// procedure, its explicit parts, or `opaque` for a value only compile-time
/// evaluation or a ConstStore node supplies.
fn writeStaticData(result: *const core.Program.Result, w: *std.Io.Writer) DumpError!void {
    for (result.static_data_values.items, 0..) |value, slot| {
        try w.print("D {d} {d} ", .{ slot, @backingInt(value.layout_idx) });
        if (value.compile_time_root != null or value.stored != null or value.representation_of != null) {
            try w.writeAll("opaque\n");
            continue;
        }
        if (value.parts) |parts| {
            try w.writeAll("parts ");
            switch (parts.kind) {
                .word => |word| try w.print("word {d}", .{word}),
                .product => try w.writeAll("product 0"),
                .list => |cap| try w.print("list {d}", .{cap}),
                .box => try w.writeAll("box 0"),
                .tag => |disc| try w.print("tag {d}", .{disc}),
                .callable => |c| try w.print("callable {d}", .{@backingInt(c.proc)}),
            }
            for (parts.children) |child| try w.print(" {d}", .{@backingInt(child)});
            try w.writeAll("\n");
            continue;
        }
        if (value.initializer.get()) |proc| {
            try w.print("init {d}\n", .{@backingInt(proc)});
            continue;
        }
        if (value.callable.get()) |proc| {
            try w.print("callable {d}\n", .{@backingInt(proc)});
            continue;
        }
        try w.writeAll("opaque\n");
    }
    try w.writeAll("@@LPD end\n");
}

/// Text of one procedure (for phase pre/post comparison).
pub fn procText(allocator: Allocator, store: *const LirStore, layouts: *const layout_mod.Store, proc_id: LIR.LirProcSpecId) ?[]u8 {
    var buffer: std.Io.Writer.Allocating = .init(allocator);
    errdefer buffer.deinit();
    var seen = LayoutSet.init(allocator);
    defer seen.deinit();
    writeProc(store, proc_id, &buffer.writer, &seen) catch {
        buffer.deinit();
        return null;
    };
    writeLayouts(layouts, &seen, &buffer.writer) catch {
        buffer.deinit();
        return null;
    };
    return buffer.toOwnedSlice() catch null;
}

/// Emit a phase dump when the phase changed the procedure's text.
pub fn phase(phase_name: []const u8, proc_id: LIR.LirProcSpecId, pre: []const u8, post: []const u8) void {
    if (std.mem.eql(u8, pre, post)) return;
    const text = std.fmt.allocPrint(std.heap.page_allocator, "@@LPD begin phase run={d} stage={d} label={s} proc={d}\n@@LPD pre\n{s}@@LPD post\n{s}@@LPD end\n", .{
        current_run.load(.monotonic),
        current_stage.load(.monotonic),
        phase_name,
        @backingInt(proc_id),
        pre,
        post,
    }) catch return;
    defer std.heap.page_allocator.free(text);
    emit(text);
}

/// An insertion-ordered set of ids on the page allocator.
const IdSet = struct {
    map: std.AutoArrayHashMapUnmanaged(u32, void) = .empty,

    fn init(_: Allocator) IdSet {
        return .{};
    }

    fn deinit(self: *IdSet) void {
        self.map.deinit(std.heap.page_allocator);
    }

    fn put(self: *IdSet, key: u32, _: void) Allocator.Error!void {
        try self.map.put(std.heap.page_allocator, key, {});
    }

    fn count(self: *const IdSet) usize {
        return self.map.count();
    }

    fn keys(self: *const IdSet) []const u32 {
        return self.map.keys();
    }
};

const LayoutSet = IdSet;

fn noteLayout(seen: *LayoutSet, idx: layout_mod.Idx) DumpError!void {
    try seen.put(@intCast(@backingInt(idx)), {});
}

fn writeLayouts(layouts: *const layout_mod.Store, seen: *LayoutSet, w: *std.Io.Writer) DumpError!void {
    var i: usize = 0;
    // `seen` grows while we walk it: children are appended and visited.
    while (i < seen.count()) : (i += 1) {
        const raw = seen.keys()[i];
        const idx: layout_mod.Idx = @fromBackingInt(@intCast(raw));
        const l = layouts.getLayout(idx);
        try w.print("L {d} ", .{raw});
        switch (l.tag) {
            .scalar => {
                const s = l.getScalar();
                switch (s.tag) {
                    .str => try w.writeAll("str"),
                    .int => try w.print("int {s}", .{@tagName(s.getInt())}),
                    .frac => try w.print("frac {s}", .{@tagName(s.getFrac())}),
                    .opaque_ptr => try w.writeAll("opaque_ptr"),
                    .vector => try w.print("vector {s}", .{@tagName(s.getVector())}),
                }
            },
            .box, .list, .ptr => {
                const child = l.getIdx();
                try w.print("{s} {d}", .{ @tagName(l.tag), @backingInt(child) });
                try noteLayout(seen, child);
            },
            .box_of_zst, .list_of_zst, .zst, .erased_callable, .hole, .dynamic => try w.writeAll(@tagName(l.tag)),
            .closure => {
                const child = l.getClosure().captures_layout_idx;
                try w.print("closure {d}", .{@backingInt(child)});
                try noteLayout(seen, child);
            },
            .struct_ => {
                const info = layouts.getStructInfo(l);
                try w.writeAll("struct");
                for (0..info.fields.len) |fi| {
                    const field = info.fields.get(@intCast(fi));
                    if (field.is_padding) continue;
                    try w.print(" {d}:{d}", .{ field.index, @backingInt(field.layout) });
                    try noteLayout(seen, field.layout);
                }
            },
            .tag_union => {
                const data = layouts.getTagUnionData(l.getTagUnion().idx);
                const variants = layouts.getTagUnionVariants(data);
                try w.print("tag_union disc_size={d}", .{data.discriminant_size});
                for (0..variants.len) |vi| {
                    const payload = variants.get(@intCast(vi)).payload_layout;
                    try w.print(" {d}", .{@backingInt(payload)});
                    try noteLayout(seen, payload);
                }
            },
        }
        try w.writeAll("\n");
    }
}

fn hex(w: *std.Io.Writer, bytes: []const u8) DumpError!void {
    if (bytes.len == 0) {
        try w.writeAll("-");
        return;
    }
    for (bytes) |b| try w.print("{x:0>2}", .{b});
}

fn locals(store: *const LirStore, span: LIR.LocalSpan, w: *std.Io.Writer) DumpError!void {
    const items = store.getLocalSpan(span);
    if (items.len == 0) {
        try w.writeAll("-");
        return;
    }
    for (0..items.len) |i| {
        if (i > 0) try w.writeAll(",");
        try w.print("{d}", .{@backingInt(GuardedList.at(items, i))});
    }
}

fn optLocal(w: *std.Io.Writer, local: ?LIR.LocalId) DumpError!void {
    if (local) |l| try w.print("{d}", .{@backingInt(l)}) else try w.writeAll("-");
}

const StmtSet = IdSet;
const LocalSet = IdSet;

fn writeProc(store: *const LirStore, proc_id: LIR.LirProcSpecId, w: *std.Io.Writer, seen_layouts: *LayoutSet) DumpError!void {
    const proc = store.getProcSpec(proc_id);
    const name = store.procDebugName(proc_id) orelse "-";
    try w.print("P {d} name=", .{@backingInt(proc_id)});
    for (name) |c| try w.writeByte(if (c == ' ' or c == '\n') '_' else c);
    try w.writeAll(" args=");
    try locals(store, proc.args, w);
    try w.print(" ret={d}", .{@backingInt(proc.ret_layout)});
    try noteLayout(seen_layouts, proc.ret_layout);
    if (proc.body.get()) |body| try w.print(" body={d}", .{@backingInt(body)}) else try w.writeAll(" body=-");
    try w.print(" abi={s} hosted={d} static_init={d} transform={s}", .{
        @tagName(proc.abi),
        @intFromBool(proc.hosted.get() != null),
        @intFromBool(proc.is_static_initializer),
        @tagName(proc.tail_transform),
    });
    try w.print(" erased_capture={d} dyn_result={d}\n", .{ @intFromBool(proc.erased_capture_arg.get() != null), @intFromBool(proc.dyn_result.get() != null) });

    var used_locals = LocalSet.init(std.heap.page_allocator);
    defer used_locals.deinit();
    for (0..store.getLocalSpan(proc.args).len) |i| try used_locals.put(@backingInt(GuardedList.at(store.getLocalSpan(proc.args), i)), {});

    const body = proc.body.get() orelse return;
    var stmts = StmtSet.init(std.heap.page_allocator);
    defer stmts.deinit();
    try stmts.put(@backingInt(body), {});
    var i: usize = 0;
    while (i < stmts.count()) : (i += 1) {
        const id: LIR.CFStmtId = @fromBackingInt(stmts.keys()[i]);
        try writeStmt(store, id, w, &stmts, &used_locals, seen_layouts);
    }
    for (used_locals.keys()) |raw| {
        const local_id: LIR.LocalId = @fromBackingInt(raw);
        const local = store.getLocal(local_id);
        try w.print("V {d} {d}", .{ raw, @backingInt(local.layout_idx) });
        if (local.dyn != null) try w.writeAll(" dyn");
        try w.writeAll("\n");
        try noteLayout(seen_layouts, local.layout_idx);
    }
}

fn next(stmts: *StmtSet, id: LIR.CFStmtId, w: *std.Io.Writer) DumpError!void {
    try stmts.put(@backingInt(id), {});
    try w.print(" {d}", .{@backingInt(id)});
}

fn useLocal(set: *LocalSet, l: LIR.LocalId) DumpError!void {
    try set.put(@backingInt(l), {});
}

fn useSpan(store: *const LirStore, set: *LocalSet, span: LIR.LocalSpan) DumpError!void {
    const items = store.getLocalSpan(span);
    for (0..items.len) |i| try useLocal(set, GuardedList.at(items, i));
}

fn writeStmt(
    store: *const LirStore,
    id: LIR.CFStmtId,
    w: *std.Io.Writer,
    stmts: *StmtSet,
    used: *LocalSet,
    seen_layouts: *LayoutSet,
) DumpError!void {
    try w.print("S {d} ", .{@backingInt(id)});
    switch (store.getCFStmtImmediate(id).*) {
        .assign_ref => |s| {
            try useLocal(used, s.target);
            try useLocal(used, s.op.source);
            try w.print("ref {d} {s} {d} {d} {d} {d}", .{ @backingInt(s.target), @tagName(s.op.kind), @backingInt(s.op.source), s.op.index, s.op.variant_index, s.op.tag_discriminant });
            try next(stmts, s.next, w);
        },
        .assign_literal => |s| {
            try useLocal(used, s.target);
            try w.print("lit {d} ", .{@backingInt(s.target)});
            switch (s.value) {
                .i64_literal => |l| try w.print("int {d}", .{l.value}),
                .i128_literal => |row| try w.print("int {d}", .{store.stmtRow(row).value}),
                .f64_literal => |f| try w.print("f64 {x}", .{@as(u64, @bitCast(f))}),
                .f32_literal => |f| try w.print("f32 {x}", .{@as(u32, @bitCast(f))}),
                .dec_literal => |d| try w.print("dec {d}", .{d}),
                .str_literal => |lit| {
                    try w.writeAll("str ");
                    try hex(w, store.getStringLiteral(lit));
                },
                .static_data => |read| try w.print("static {d}", .{@backingInt(read.id)}),
                .bytes_literal => |lit| {
                    try w.print("bytes {d} ", .{lit.len});
                    try hex(w, store.getStringLiteral(lit.bytes));
                },
                .proc_ref => |p| try w.print("proc_ref {d}", .{@backingInt(p)}),
            }
            try next(stmts, s.next, w);
        },
        .init_uninitialized => |s| {
            try useLocal(used, s.target);
            try w.print("uninit {d}", .{@backingInt(s.target)});
            try next(stmts, s.next, w);
        },
        .assign_call => |s| {
            try useLocal(used, s.target);
            try useSpan(store, used, s.args);
            try w.print("call {d} {d} ", .{ @backingInt(s.target), @backingInt(s.proc) });
            try locals(store, s.args, w);
            try next(stmts, s.next, w);
            try w.print(" tail={d}", .{@intFromBool(s.tail_call.isSite())});
        },
        .assign_call_erased => |s| {
            const row = store.stmtRow(s.data);
            try useLocal(used, s.target);
            try useLocal(used, row.closure);
            try useSpan(store, used, row.args);
            try w.print("call_erased {d} {d} ", .{ @backingInt(s.target), @backingInt(row.closure) });
            try locals(store, row.args, w);
            try next(stmts, s.next, w);
        },
        .assign_packed_erased_fn => |s| {
            const row = store.stmtRow(s.data);
            try useLocal(used, s.target);
            if (row.capture.get()) |c| try useLocal(used, c);
            try w.print("packed_fn {d} {d} ", .{ @backingInt(s.target), @backingInt(row.proc) });
            try optLocal(w, row.capture.get());
            try next(stmts, s.next, w);
        },
        .assign_low_level => |s| {
            try useLocal(used, s.target);
            try useSpan(store, used, s.args);
            try w.print("ll {d} {s} ", .{ @backingInt(s.target), @tagName(s.op) });
            try locals(store, s.args, w);
            try next(stmts, s.next, w);
            if (store.stmtRowOrDefault(s.extra).dyn_elem_desc) try w.writeAll(" dyn_elem_desc");
        },
        .assign_list => |s| {
            try useLocal(used, s.target);
            try useSpan(store, used, s.elems);
            try w.print("list {d} ", .{@backingInt(s.target)});
            try locals(store, s.elems, w);
            try next(stmts, s.next, w);
        },
        .assign_struct => |s| {
            try useLocal(used, s.target);
            try useSpan(store, used, s.fields);
            try w.print("struct {d} ", .{@backingInt(s.target)});
            try locals(store, s.fields, w);
            try next(stmts, s.next, w);
        },
        .assign_tag => |s| {
            try useLocal(used, s.target);
            if (s.payload) |p| try useLocal(used, p);
            try w.print("tag {d} {d} {d} ", .{ @backingInt(s.target), s.variant_index, s.discriminant });
            try optLocal(w, s.payload);
            try next(stmts, s.next, w);
        },
        .store_struct => |s| {
            try useLocal(used, s.dest);
            try useSpan(store, used, s.fields);
            try noteLayout(seen_layouts, s.struct_layout);
            try w.print("store_struct {d} {d} ", .{ @backingInt(s.dest), @backingInt(s.struct_layout) });
            try locals(store, s.fields, w);
            try next(stmts, s.next, w);
        },
        .store_tag => |s| {
            try useLocal(used, s.dest);
            if (s.payload) |p| try useLocal(used, p);
            try noteLayout(seen_layouts, s.tag_layout);
            try w.print("store_tag {d} {d} {d} {d} ", .{ @backingInt(s.dest), @backingInt(s.tag_layout), s.variant_index, s.discriminant });
            try optLocal(w, s.payload);
            try next(stmts, s.next, w);
        },
        .set_local => |s| {
            try useLocal(used, s.target);
            try useLocal(used, s.value);
            try w.print("set {d} {d} {s}", .{ @backingInt(s.target), @backingInt(s.value), @tagName(s.mode) });
            try next(stmts, s.next, w);
        },
        .debug => |s| {
            try useLocal(used, s.message);
            try w.print("debug {d}", .{@backingInt(s.message)});
            try next(stmts, s.next, w);
        },
        .expect => |s| {
            try useLocal(used, s.condition);
            try w.print("expect {d}", .{@backingInt(s.condition)});
            try next(stmts, s.next, w);
        },
        .expect_err => |s| {
            try useLocal(used, s.message);
            try w.print("expect_err {d}", .{@backingInt(s.message)});
        },
        .dyn => |s| {
            if (s.target) |t| try useLocal(used, t);
            try w.print("dyn ", .{});
            try optLocal(w, s.target);
            try w.print(" {s}", .{@tagName(store.stmtRow(s.data))});
            try next(stmts, s.next, w);
        },
        .runtime_error => try w.writeAll("runtime_error"),
        .comptime_exhaustiveness_failed => try w.writeAll("comptime_exhaustiveness_failed"),
        .incref => |s| {
            try w.print("rc incref {d}", .{@backingInt(s.value)});
            try next(stmts, s.next, w);
        },
        .decref => |s| {
            try w.print("rc decref {d}", .{@backingInt(s.value)});
            try next(stmts, s.next, w);
        },
        .decref_if_initialized => |s| {
            try w.print("rc decref_if_initialized {d}", .{@backingInt(s.value)});
            try next(stmts, s.next, w);
        },
        .free => |s| {
            try w.print("rc free {d}", .{@backingInt(s.value)});
            try next(stmts, s.next, w);
        },
        .switch_stmt => |s| {
            try useLocal(used, s.cond);
            try w.print("switch {d}", .{@backingInt(s.cond)});
            try next(stmts, s.default_branch, w);
            if (s.continuation) |c| try next(stmts, c, w) else try w.writeAll(" -");
            const branches = store.getCFSwitchBranches(s.branches);
            for (0..branches.len) |bi| {
                const branch = GuardedList.at(branches, bi);
                try w.print(" {d}:{d}", .{ branch.value, @backingInt(branch.body) });
                try stmts.put(@backingInt(branch.body), {});
            }
        },
        .switch_initialized_payload => |s| {
            try useLocal(used, s.cond);
            try useLocal(used, s.payload);
            try w.print("sip {d} {d} {d}", .{ @backingInt(s.cond), s.cond_mask, @backingInt(s.payload) });
            try next(stmts, s.initialized_branch, w);
            try next(stmts, s.uninitialized_branch, w);
        },
        .str_match => |s| {
            const row = store.stmtRow(s.data);
            try useLocal(used, s.source);
            try w.print("str_match {d}", .{@backingInt(s.source)});
            try next(stmts, s.on_match, w);
            try next(stmts, s.on_miss, w);
            try w.writeAll(" arm ");
            try writeArm(store, used, row.prefix, row.steps, row.end, w);
        },
        .str_match_set => |s| {
            try useLocal(used, s.source);
            try w.print("str_match_set {d}", .{@backingInt(s.source)});
            try next(stmts, s.on_miss, w);
            const arms = store.getStrMatchArms(s.arms);
            for (0..arms.len) |ai| {
                const arm = GuardedList.at(arms, ai);
                try w.writeAll(" arm");
                try next(stmts, arm.on_match, w);
                try w.writeAll(" ");
                try writeArm(store, used, arm.prefix, arm.steps, arm.end, w);
            }
        },
        .join => |s| {
            try useSpan(store, used, s.params);
            try w.print("join {d} ", .{@backingInt(s.id)});
            try locals(store, s.params, w);
            try next(stmts, s.body, w);
            try next(stmts, s.remainder, w);
        },
        .jump => |s| try w.print("jump {d}", .{@backingInt(s.target)}),
        .ret => |s| {
            try useLocal(used, s.value);
            try w.print("ret {d}", .{@backingInt(s.value)});
        },
        .crash => |s| switch (s.msg) {
            .literal => |lit| {
                try w.writeAll("crash lit ");
                try hex(w, store.getString(lit));
            },
            .local => |l| {
                try useLocal(used, l);
                try w.print("crash local {d}", .{@backingInt(l)});
            },
        },
    }
    try w.writeAll("\n");
}

fn writeArm(store: *const LirStore, used: *LocalSet, prefix: LIR.StrLiteral, steps_span: LIR.StrMatchStepSpan, end: LIR.StrPatternEnd, w: *std.Io.Writer) DumpError!void {
    try hex(w, store.getStringLiteral(prefix));
    try w.print(" {s} ", .{@tagName(end)});
    const steps = store.getStrMatchSteps(steps_span);
    if (steps.len == 0) try w.writeAll("-");
    for (0..steps.len) |i| {
        const step = GuardedList.at(steps, i);
        if (i > 0) try w.writeAll(",");
        switch (step.capture) {
            .discard => try w.writeAll("_"),
            .view => |l| {
                try useLocal(used, l);
                try w.print("{d}", .{@backingInt(l)});
            },
        }
        try w.writeAll(":");
        try hex(w, store.getStringLiteral(step.delimiter));
    }
}
