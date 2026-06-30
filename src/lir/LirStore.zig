//! Flat storage for statement-only, local-centric LIR.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const layout = @import("layout");

const lir_defs = @import("LIR.zig");

const Allocator = std.mem.Allocator;

const CFStmt = lir_defs.CFStmt;
const CFStmtId = lir_defs.CFStmtId;
const CFSwitchBranch = lir_defs.CFSwitchBranch;
const CFSwitchBranchSpan = lir_defs.CFSwitchBranchSpan;
const JoinPoint = lir_defs.JoinPoint;
const JoinPointSpan = lir_defs.JoinPointSpan;
const LirProcSpec = lir_defs.LirProcSpec;
const LirProcSpecId = lir_defs.LirProcSpecId;
const Local = lir_defs.Local;
const LocalId = lir_defs.LocalId;
const LocalSpan = lir_defs.LocalSpan;
const StrMatchArm = lir_defs.StrMatchArm;
const StrMatchArmSpan = lir_defs.StrMatchArmSpan;
const StrMatchStep = lir_defs.StrMatchStep;
const StrMatchStepSpan = lir_defs.StrMatchStepSpan;
const Symbol = lir_defs.Symbol;
const LirPattern = lir_defs.LirPattern;
const LirPatternId = lir_defs.LirPatternId;
const LirPatternSpan = lir_defs.LirPatternSpan;
const U64Span = lir_defs.U64Span;

/// Source-level name to use when presenting a specialized LIR proc in debug output.
pub const ProcDebugName = extern struct {
    proc: u32,
    string: base.StringLiteral.Idx,
};

const Self = @This();

cf_stmts: std.ArrayList(CFStmt),
cf_switch_branches: std.ArrayList(CFSwitchBranch),
str_match_steps: std.ArrayList(StrMatchStep),
str_match_arms: std.ArrayList(StrMatchArm),
join_points: std.ArrayList(JoinPoint),
locals: std.ArrayList(Local),
local_ids: std.ArrayList(LocalId),
u64s: std.ArrayList(u64),
proc_specs: std.ArrayList(LirProcSpec),
strings: base.StringLiteral.Store,
string_builder: base.StringLiteral.BuilderState,
strings_insertable: bool,
allocator: Allocator,
next_synthetic_symbol: u64,
patterns: std.ArrayList(LirPattern),
pattern_ids: std.ArrayList(LirPatternId),
/// Source file table (module display names) for `SourceLoc.file`, flattened
/// as concatenated bytes plus per-entry end offsets so it can be mapped
/// zero-copy from a LIR image.
source_file_bytes: std.ArrayList(u8),
source_file_ends: std.ArrayList(u32),
/// Source location per statement, parallel to `cf_stmts`. Reference-count
/// statements always record `SourceLoc.none`; they have no source counterpart.
cf_stmt_locs: std.ArrayList(base.SourceLoc),
/// Checked source region per statement, parallel to `cf_stmts`. Reference-count
/// statements always record `Region.zero`; they have no source counterpart.
cf_stmt_regions: std.ArrayList(base.Region),
/// Source location per proc, parallel to `proc_specs`.
proc_locs: std.ArrayList(base.SourceLoc),
/// Source-level debug names for procs that have source names.
proc_debug_names: std.ArrayList(ProcDebugName),
/// Source-level name per local, parallel to `locals`: an index into
/// `strings`, or `no_local_name` for compiler-generated temporaries.
local_names: std.ArrayList(u32),
/// Ambient location recorded by `addCFStmt`/`addProcSpec`. Lowering sets
/// this on entry to each source node it lowers.
current_loc: base.SourceLoc,
/// Ambient checked source region recorded by `addCFStmt`.
current_region: base.Region,

/// Initializes empty storage for statement-only LIR.
pub fn init(allocator: Allocator) Self {
    return .{
        .cf_stmts = std.ArrayList(CFStmt).empty,
        .cf_switch_branches = std.ArrayList(CFSwitchBranch).empty,
        .str_match_steps = std.ArrayList(StrMatchStep).empty,
        .str_match_arms = std.ArrayList(StrMatchArm).empty,
        .join_points = std.ArrayList(JoinPoint).empty,
        .locals = std.ArrayList(Local).empty,
        .local_ids = std.ArrayList(LocalId).empty,
        .u64s = std.ArrayList(u64).empty,
        .proc_specs = std.ArrayList(LirProcSpec).empty,
        .strings = base.StringLiteral.Store{},
        .string_builder = .{},
        .strings_insertable = true,
        .allocator = allocator,
        .next_synthetic_symbol = 0xf000_0000_0000_0000,
        .patterns = std.ArrayList(LirPattern).empty,
        .pattern_ids = std.ArrayList(LirPatternId).empty,
        .source_file_bytes = std.ArrayList(u8).empty,
        .source_file_ends = std.ArrayList(u32).empty,
        .cf_stmt_locs = std.ArrayList(base.SourceLoc).empty,
        .cf_stmt_regions = std.ArrayList(base.Region).empty,
        .proc_locs = std.ArrayList(base.SourceLoc).empty,
        .proc_debug_names = std.ArrayList(ProcDebugName).empty,
        .local_names = std.ArrayList(u32).empty,
        .current_loc = base.SourceLoc.none,
        .current_region = base.Region.zero(),
    };
}

/// Releases all storage owned by this LIR store.
pub fn deinit(self: *Self) void {
    self.cf_stmts.deinit(self.allocator);
    self.cf_switch_branches.deinit(self.allocator);
    self.str_match_steps.deinit(self.allocator);
    self.str_match_arms.deinit(self.allocator);
    self.join_points.deinit(self.allocator);
    self.locals.deinit(self.allocator);
    self.local_ids.deinit(self.allocator);
    self.u64s.deinit(self.allocator);
    self.proc_specs.deinit(self.allocator);
    self.string_builder.deinit(self.allocator);
    self.strings.deinit(self.allocator);
    self.patterns.deinit(self.allocator);
    self.pattern_ids.deinit(self.allocator);
    self.source_file_bytes.deinit(self.allocator);
    self.source_file_ends.deinit(self.allocator);
    self.cf_stmt_locs.deinit(self.allocator);
    self.cf_stmt_regions.deinit(self.allocator);
    self.proc_locs.deinit(self.allocator);
    self.proc_debug_names.deinit(self.allocator);
    self.local_names.deinit(self.allocator);
}

/// Sentinel in `local_names` for locals with no source-level name.
pub const no_local_name: u32 = std.math.maxInt(u32);

/// Record the source-level name of a local (empty means none).
pub fn setLocalName(self: *Self, id: LocalId, name: []const u8) Allocator.Error!void {
    if (name.len == 0) return;
    const idx = try self.insertString(name);
    self.local_names.items[@intFromEnum(id)] = @intFromEnum(idx);
}

/// Source-level name of a local, or null for compiler-generated temporaries.
pub fn localName(self: *const Self, id: LocalId) ?[]const u8 {
    const raw = self.local_names.items[@intFromEnum(id)];
    if (raw == no_local_name) return null;
    return self.getString(@enumFromInt(raw));
}

/// Record the source-level debug name of a proc.
pub fn setProcDebugName(self: *Self, id: LirProcSpecId, name: []const u8) Allocator.Error!void {
    if (name.len == 0) return;
    try self.setProcDebugNameIndex(id, try self.insertString(name));
}

/// Copy proc source metadata from one proc to another, for compiler-generated variants.
pub fn copyProcDebugInfo(self: *Self, dst: LirProcSpecId, src: LirProcSpecId) Allocator.Error!void {
    self.proc_locs.items[@intFromEnum(dst)] = self.proc_locs.items[@intFromEnum(src)];
    if (self.procDebugNameIndex(src)) |idx| {
        try self.setProcDebugNameIndex(dst, idx);
    }
}

/// Source-level debug name of a proc, or null for compiler-generated procs.
pub fn procDebugName(self: *const Self, id: LirProcSpecId) ?[]const u8 {
    const idx = self.procDebugNameIndex(id) orelse return null;
    return self.getString(idx);
}

fn procDebugNameIndex(self: *const Self, id: LirProcSpecId) ?base.StringLiteral.Idx {
    const proc = @intFromEnum(id);
    for (self.proc_debug_names.items) |entry| {
        if (entry.proc == proc) return entry.string;
    }
    return null;
}

fn setProcDebugNameIndex(self: *Self, id: LirProcSpecId, string: base.StringLiteral.Idx) Allocator.Error!void {
    const proc = @intFromEnum(id);
    for (self.proc_debug_names.items) |*entry| {
        if (entry.proc == proc) {
            entry.string = string;
            return;
        }
    }
    try self.proc_debug_names.append(self.allocator, .{ .proc = proc, .string = string });
}

/// Copies the source file table from a lowering stage's program.
pub fn setSourceFiles(self: *Self, files: []const []const u8) Allocator.Error!void {
    std.debug.assert(self.source_file_ends.items.len == 0);
    for (files) |file| {
        try self.source_file_bytes.appendSlice(self.allocator, file);
        try self.source_file_ends.append(self.allocator, @intCast(self.source_file_bytes.items.len));
    }
}

/// Number of entries in the source file table.
pub fn sourceFileCount(self: *const Self) u32 {
    return @intCast(self.source_file_ends.items.len);
}

/// Name of one source file table entry.
pub fn sourceFileName(self: *const Self, file: u32) []const u8 {
    const end = self.source_file_ends.items[file];
    const start = if (file == 0) 0 else self.source_file_ends.items[file - 1];
    return self.source_file_bytes.items[start..end];
}

/// Source location of a statement.
pub fn stmtLoc(self: *const Self, id: CFStmtId) base.SourceLoc {
    return self.cf_stmt_locs.items[@intFromEnum(id)];
}

/// Checked source region of a statement.
pub fn stmtRegion(self: *const Self, id: CFStmtId) base.Region {
    return self.cf_stmt_regions.items[@intFromEnum(id)];
}

/// Source location of a proc.
pub fn procLoc(self: *const Self, id: LirProcSpecId) base.SourceLoc {
    return self.proc_locs.items[@intFromEnum(id)];
}

/// Appends a pattern and returns its id.
pub fn addPattern(self: *Self, pattern: LirPattern) Allocator.Error!LirPatternId {
    const id: LirPatternId = @enumFromInt(self.patterns.items.len);
    try self.patterns.append(self.allocator, pattern);
    return id;
}

/// Returns the pattern for a given id.
pub fn getPattern(self: *const Self, id: LirPatternId) LirPattern {
    return self.patterns.items[@intFromEnum(id)];
}

/// Appends a slice of pattern ids and returns the span.
pub fn addPatternSpan(self: *Self, ids: []const LirPatternId) Allocator.Error!LirPatternSpan {
    const start: u32 = @intCast(self.pattern_ids.items.len);
    try self.pattern_ids.appendSlice(self.allocator, ids);
    return .{ .start = start, .len = @intCast(ids.len) };
}

/// Returns the pattern ids for a given span.
pub fn getPatternSpan(self: *const Self, span: LirPatternSpan) []const LirPatternId {
    if (span.len == 0) return &.{};
    return self.pattern_ids.items[span.start .. span.start + span.len];
}

/// Returns a fresh synthetic symbol for compiler-generated locals and procs.
pub fn freshSyntheticSymbol(self: *Self) Symbol {
    const symbol = Symbol.fromRaw(self.next_synthetic_symbol);
    self.next_synthetic_symbol += 1;
    return symbol;
}

/// Interns a string literal in the store-level string table.
pub fn insertString(self: *Self, text: []const u8) Allocator.Error!base.StringLiteral.Idx {
    self.assertStringsInsertable();
    return self.string_builder.insert(&self.strings, self.allocator, text);
}

/// Interns string backing bytes and returns a literal view into them.
pub fn insertStringView(
    self: *Self,
    backing: []const u8,
    offset: u32,
    len: u32,
) Allocator.Error!lir_defs.StrLiteral {
    const offset_usize: usize = offset;
    const len_usize: usize = len;
    if (offset_usize > backing.len or len_usize > backing.len - offset_usize) {
        if (builtin.mode == .Debug) {
            std.debug.panic("LirStore invariant violated: string literal view exceeded backing bytes", .{});
        }
        unreachable;
    }

    return .{
        .backing = try self.insertString(backing),
        .offset = offset,
        .len = len,
    };
}

/// Returns the text for an interned string literal.
pub fn getString(self: *const Self, idx: base.StringLiteral.Idx) []const u8 {
    return self.strings.get(idx);
}

/// Returns the bytes used by one string literal view.
pub fn getStringLiteral(self: *const Self, literal: lir_defs.StrLiteral) []const u8 {
    const backing = self.getString(literal.backing);
    const offset: usize = literal.offset;
    const len: usize = literal.len;
    if (offset > backing.len or len > backing.len - offset) {
        if (builtin.mode == .Debug) {
            std.debug.panic("LirStore invariant violated: string literal view exceeded stored backing bytes", .{});
        }
        unreachable;
    }
    return backing[offset..][0..len];
}

/// Returns the full backing bytes for one string literal view.
pub fn getStringLiteralBacking(self: *const Self, literal: lir_defs.StrLiteral) []const u8 {
    return self.getString(literal.backing);
}

fn assertStringsInsertable(self: *const Self) void {
    if (self.strings_insertable) return;

    if (comptime builtin.mode == .Debug) {
        std.debug.panic("LirStore invariant violated: attempted to insert into frozen string literal store", .{});
    }
    unreachable;
}

/// Registers one LIR local and returns its id.
pub fn addLocal(self: *Self, local: Local) Allocator.Error!LocalId {
    const idx = self.locals.items.len;
    try self.locals.append(self.allocator, local);
    try self.local_names.append(self.allocator, no_local_name);
    return @enumFromInt(@as(u32, @intCast(idx)));
}

/// Returns one stored LIR local.
pub fn getLocal(self: *const Self, id: LocalId) Local {
    return self.locals.items[@intFromEnum(id)];
}

/// Returns a mutable pointer to one stored LIR local.
pub fn getLocalPtr(self: *Self, id: LocalId) *Local {
    return &self.locals.items[@intFromEnum(id)];
}

/// Stores local ids and returns the corresponding flat-storage span.
pub fn addLocalSpan(self: *Self, ids: []const LocalId) Allocator.Error!LocalSpan {
    if (ids.len == 0) return LocalSpan.empty();

    const start = @as(u32, @intCast(self.local_ids.items.len));
    try self.local_ids.appendSlice(self.allocator, ids);
    return .{ .start = start, .len = @intCast(ids.len) };
}

/// Resolves a local-id span to its stored slice.
pub fn getLocalSpan(self: *const Self, span: LocalSpan) []const LocalId {
    if (span.len == 0) return &.{};
    if (builtin.mode == .Debug) {
        const end = @as(u64, span.start) + @as(u64, span.len);
        if (end > self.local_ids.items.len) {
            std.debug.panic(
                "LirStore invariant violated: local-id span start={d} len={d} exceeds local-id storage len={d}",
                .{ span.start, span.len, self.local_ids.items.len },
            );
        }
    }
    return self.local_ids.items[span.start..][0..span.len];
}

/// Stores u64 values and returns the corresponding flat-storage span.
pub fn addU64Span(self: *Self, values: []const u64) Allocator.Error!U64Span {
    if (values.len == 0) return U64Span.empty();

    const start = @as(u32, @intCast(self.u64s.items.len));
    try self.u64s.appendSlice(self.allocator, values);
    return .{ .start = start, .len = @intCast(values.len) };
}

/// Resolves a u64 span to its stored slice.
pub fn getU64Span(self: *const Self, span: U64Span) []const u64 {
    if (span.len == 0) return &.{};
    if (builtin.mode == .Debug) {
        const end = @as(u64, span.start) + @as(u64, span.len);
        if (end > self.u64s.items.len) {
            std.debug.panic(
                "LirStore invariant violated: u64 span start={d} len={d} exceeds u64 storage len={d}",
                .{ span.start, span.len, self.u64s.items.len },
            );
        }
    }
    return self.u64s.items[span.start..][0..span.len];
}

/// Appends a statement/control-flow node and returns its id.
pub fn addCFStmt(self: *Self, stmt: CFStmt) Allocator.Error!CFStmtId {
    const idx = self.cf_stmts.items.len;
    try self.cf_stmts.append(self.allocator, stmt);
    const has_source = switch (stmt) {
        .incref,
        .decref,
        .decref_if_initialized,
        .free,
        => false,

        .init_uninitialized,
        .assign_ref,
        .assign_literal,
        .assign_call,
        .assign_call_erased,
        .assign_packed_erased_fn,
        .assign_low_level,
        .assign_list,
        .assign_struct,
        .assign_tag,
        .set_local,
        .debug,
        .expect,
        .expect_err,
        .runtime_error,
        .comptime_exhaustiveness_failed,
        .comptime_branch_taken,
        .switch_stmt,
        .switch_initialized_payload,
        .str_match,
        .str_match_set,
        .loop_continue,
        .loop_break,
        .join,
        .jump,
        .ret,
        .crash,
        => true,
    };
    const loc = if (has_source) self.current_loc else base.SourceLoc.none;
    const region = if (has_source) self.current_region else base.Region.zero();
    try self.cf_stmt_locs.append(self.allocator, loc);
    try self.cf_stmt_regions.append(self.allocator, region);
    return @enumFromInt(@as(u32, @intCast(idx)));
}

/// Returns the stored statement for the given id.
pub fn getCFStmt(self: *const Self, id: CFStmtId) CFStmt {
    self.verifyCFStmtId(id);
    return self.cf_stmts.items[@intFromEnum(id)];
}

/// Returns a mutable pointer to the stored statement for the given id.
pub fn getCFStmtPtr(self: *Self, id: CFStmtId) *CFStmt {
    self.verifyCFStmtId(id);
    return &self.cf_stmts.items[@intFromEnum(id)];
}

fn verifyCFStmtId(self: *const Self, id: CFStmtId) void {
    if (builtin.mode == .Debug) {
        const idx = @intFromEnum(id);
        if (idx >= self.cf_stmts.items.len) {
            std.debug.panic(
                "LirStore invariant violated: statement id {d} exceeds statement storage len {d}",
                .{ idx, self.cf_stmts.items.len },
            );
        }
    }
}

/// Appends switch branches and returns the corresponding flat-storage span.
pub fn addCFSwitchBranches(self: *Self, branches: []const CFSwitchBranch) Allocator.Error!CFSwitchBranchSpan {
    if (branches.len == 0) return CFSwitchBranchSpan.empty();

    const start = @as(u32, @intCast(self.cf_switch_branches.items.len));
    try self.cf_switch_branches.appendSlice(self.allocator, branches);
    return .{ .start = start, .len = @intCast(branches.len) };
}

/// Resolves a switch-branch span to its stored slice.
pub fn getCFSwitchBranches(self: *const Self, span: CFSwitchBranchSpan) []const CFSwitchBranch {
    if (span.len == 0) return &.{};
    if (builtin.mode == .Debug) {
        const end = @as(u64, span.start) + @as(u64, span.len);
        if (end > self.cf_switch_branches.items.len) {
            std.debug.panic(
                "LirStore invariant violated: switch-branch span start={d} len={d} exceeds switch-branch storage len={d}",
                .{ span.start, span.len, self.cf_switch_branches.items.len },
            );
        }
    }
    return self.cf_switch_branches.items[span.start..][0..span.len];
}

/// Resolves a switch-branch span to its stored mutable slice.
pub fn getCFSwitchBranchesMut(self: *Self, span: CFSwitchBranchSpan) []CFSwitchBranch {
    if (span.len == 0) return self.cf_switch_branches.items[0..0];
    if (builtin.mode == .Debug) {
        const end = @as(u64, span.start) + @as(u64, span.len);
        if (end > self.cf_switch_branches.items.len) {
            std.debug.panic(
                "LirStore invariant violated: mutable switch-branch span start={d} len={d} exceeds switch-branch storage len={d}",
                .{ span.start, span.len, self.cf_switch_branches.items.len },
            );
        }
    }
    return self.cf_switch_branches.items[span.start..][0..span.len];
}

/// Appends string-match steps and returns the corresponding flat-storage span.
pub fn addStrMatchSteps(self: *Self, steps: []const StrMatchStep) Allocator.Error!StrMatchStepSpan {
    if (steps.len == 0) return StrMatchStepSpan.empty();

    const start = @as(u32, @intCast(self.str_match_steps.items.len));
    try self.str_match_steps.appendSlice(self.allocator, steps);
    return .{ .start = start, .len = @intCast(steps.len) };
}

/// Resolves a string-match-step span to its stored slice.
pub fn getStrMatchSteps(self: *const Self, span: StrMatchStepSpan) []const StrMatchStep {
    if (span.len == 0) return &.{};
    if (builtin.mode == .Debug) {
        const end = @as(u64, span.start) + @as(u64, span.len);
        if (end > self.str_match_steps.items.len) {
            std.debug.panic(
                "LirStore invariant violated: string-match-step span start={d} len={d} exceeds string-match-step storage len={d}",
                .{ span.start, span.len, self.str_match_steps.items.len },
            );
        }
    }
    return self.str_match_steps.items[span.start..][0..span.len];
}

/// Appends string-match arms and returns the corresponding flat-storage span.
pub fn addStrMatchArms(self: *Self, arms: []const StrMatchArm) Allocator.Error!StrMatchArmSpan {
    if (arms.len == 0) return StrMatchArmSpan.empty();

    const start = @as(u32, @intCast(self.str_match_arms.items.len));
    try self.str_match_arms.appendSlice(self.allocator, arms);
    return .{ .start = start, .len = @intCast(arms.len) };
}

/// Resolves a string-match-arm span to its stored slice.
pub fn getStrMatchArms(self: *const Self, span: StrMatchArmSpan) []const StrMatchArm {
    if (span.len == 0) return &.{};
    if (builtin.mode == .Debug) {
        const end = @as(u64, span.start) + @as(u64, span.len);
        if (end > self.str_match_arms.items.len) {
            std.debug.panic(
                "LirStore invariant violated: string-match-arm span start={d} len={d} exceeds string-match-arm storage len={d}",
                .{ span.start, span.len, self.str_match_arms.items.len },
            );
        }
    }
    return self.str_match_arms.items[span.start..][0..span.len];
}

/// Resolves a string-match-arm span to its stored mutable slice.
pub fn getStrMatchArmsMut(self: *Self, span: StrMatchArmSpan) []StrMatchArm {
    if (span.len == 0) return self.str_match_arms.items[0..0];
    if (builtin.mode == .Debug) {
        const end = @as(u64, span.start) + @as(u64, span.len);
        if (end > self.str_match_arms.items.len) {
            std.debug.panic(
                "LirStore invariant violated: mutable string-match-arm span start={d} len={d} exceeds string-match-arm storage len={d}",
                .{ span.start, span.len, self.str_match_arms.items.len },
            );
        }
    }
    return self.str_match_arms.items[span.start..][0..span.len];
}

/// Appends join-point entries and returns the corresponding flat-storage span.
pub fn addJoinPointSpan(self: *Self, join_points: []const JoinPoint) Allocator.Error!JoinPointSpan {
    if (join_points.len == 0) return JoinPointSpan.empty();

    const start = @as(u32, @intCast(self.join_points.items.len));
    try self.join_points.appendSlice(self.allocator, join_points);
    return .{ .start = start, .len = @intCast(join_points.len) };
}

/// Resolves a join-point span to its stored slice.
pub fn getJoinPointSpan(self: *const Self, span: JoinPointSpan) []const JoinPoint {
    if (span.len == 0) return &.{};
    if (builtin.mode == .Debug) {
        const end = @as(u64, span.start) + @as(u64, span.len);
        if (end > self.join_points.items.len) {
            std.debug.panic(
                "LirStore invariant violated: join-point span start={d} len={d} exceeds join-point storage len={d}",
                .{ span.start, span.len, self.join_points.items.len },
            );
        }
    }
    return self.join_points.items[span.start..][0..span.len];
}

/// Resolves a join-point span to its stored mutable slice.
pub fn getJoinPointSpanMut(self: *Self, span: JoinPointSpan) []JoinPoint {
    if (span.len == 0) return self.join_points.items[0..0];
    if (builtin.mode == .Debug) {
        const end = @as(u64, span.start) + @as(u64, span.len);
        if (end > self.join_points.items.len) {
            std.debug.panic(
                "LirStore invariant violated: mutable join-point span start={d} len={d} exceeds join-point storage len={d}",
                .{ span.start, span.len, self.join_points.items.len },
            );
        }
    }
    return self.join_points.items[span.start..][0..span.len];
}

/// Appends a proc specification and returns its id.
pub fn addProcSpec(self: *Self, proc: LirProcSpec) Allocator.Error!LirProcSpecId {
    const idx = self.proc_specs.items.len;
    try self.proc_specs.append(self.allocator, proc);
    try self.proc_locs.append(self.allocator, self.current_loc);
    return @enumFromInt(@as(u32, @intCast(idx)));
}

/// Returns the stored proc specification for the given id.
pub fn getProcSpec(self: *const Self, idx: LirProcSpecId) LirProcSpec {
    return self.proc_specs.items[@intFromEnum(idx)];
}

/// Returns a mutable pointer to the stored proc specification for the given id.
pub fn getProcSpecPtr(self: *Self, idx: LirProcSpecId) *LirProcSpec {
    return &self.proc_specs.items[@intFromEnum(idx)];
}

/// Returns all stored proc specifications.
pub fn getProcSpecs(self: *const Self) []const LirProcSpec {
    return self.proc_specs.items;
}

/// Reports whether any local in a span has a layout that requires stack probing.
pub fn localSpanNeedsStackProbe(self: *const Self, layouts: *const layout.Store, span: LocalSpan) bool {
    for (self.getLocalSpan(span)) |local| {
        if (lir_defs.layoutNeedsStackProbe(layouts, self.getLocal(local).layout_idx)) return true;
    }
    return false;
}

/// Reports whether a proc's args, frame locals, or return layout require stack probing.
pub fn procNeedsStackProbe(self: *const Self, layouts: *const layout.Store, proc: LirProcSpec) bool {
    if (self.localSpanNeedsStackProbe(layouts, proc.args)) return true;
    if (self.localSpanNeedsStackProbe(layouts, proc.frame_locals)) return true;
    if (lir_defs.layoutNeedsStackProbe(layouts, proc.ret_layout)) return true;
    return false;
}
