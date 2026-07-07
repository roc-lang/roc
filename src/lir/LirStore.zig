//! Flat storage for statement-only, local-centric LIR.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const collections = @import("collections");
const layout = @import("layout");

const lir_defs = @import("LIR.zig");

const Allocator = std.mem.Allocator;
pub const GuardedList = collections.GuardedList;

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

/// Guarded immutable span borrow for a named `LirStore` backing list.
pub fn StoreSpanBorrow(comptime T: type, comptime field_name: []const u8) type {
    return GuardedList.BorrowSpan(T, "LirStore." ++ field_name);
}

/// Guarded mutable span borrow for a named `LirStore` backing list.
pub fn StoreSpanBorrowMut(comptime T: type, comptime field_name: []const u8) type {
    return GuardedList.BorrowSpanMut(T, "LirStore." ++ field_name);
}

cf_stmts: GuardedList.List(CFStmt, "LirStore.cf_stmts"),
cf_switch_branches: GuardedList.List(CFSwitchBranch, "LirStore.cf_switch_branches"),
str_match_steps: GuardedList.List(StrMatchStep, "LirStore.str_match_steps"),
str_match_arms: GuardedList.List(StrMatchArm, "LirStore.str_match_arms"),
join_points: GuardedList.List(JoinPoint, "LirStore.join_points"),
locals: GuardedList.List(Local, "LirStore.locals"),
local_ids: GuardedList.List(LocalId, "LirStore.local_ids"),
u64s: GuardedList.List(u64, "LirStore.u64s"),
proc_specs: GuardedList.List(LirProcSpec, "LirStore.proc_specs"),
strings: base.StringLiteral.Store,
string_builder: base.StringLiteral.BuilderState,
strings_insertable: bool,
allocator: Allocator,
next_synthetic_symbol: u64,
patterns: GuardedList.List(LirPattern, "LirStore.patterns"),
pattern_ids: GuardedList.List(LirPatternId, "LirStore.pattern_ids"),
/// Source file table (module display names) for `SourceLoc.file`, flattened
/// as concatenated bytes plus per-entry end offsets so it can be mapped
/// zero-copy from a LIR image.
source_file_bytes: GuardedList.List(u8, "LirStore.source_file_bytes"),
source_file_ends: GuardedList.List(u32, "LirStore.source_file_ends"),
/// Source location per statement, parallel to `cf_stmts`. Reference-count
/// statements always record `SourceLoc.none`; they have no source counterpart.
cf_stmt_locs: GuardedList.List(base.SourceLoc, "LirStore.cf_stmt_locs"),
/// Checked source region per statement, parallel to `cf_stmts`. Reference-count
/// statements always record `Region.zero`; they have no source counterpart.
cf_stmt_regions: GuardedList.List(base.Region, "LirStore.cf_stmt_regions"),
/// Source location per proc, parallel to `proc_specs`.
proc_locs: GuardedList.List(base.SourceLoc, "LirStore.proc_locs"),
/// Source-level debug names for procs that have source names.
proc_debug_names: GuardedList.List(ProcDebugName, "LirStore.proc_debug_names"),
/// Source-level name per local, parallel to `locals`: an index into
/// `strings`, or `no_local_name` for compiler-generated temporaries.
local_names: GuardedList.List(u32, "LirStore.local_names"),
/// Ambient location recorded by `addCFStmt`/`addProcSpec`. Lowering sets
/// this on entry to each source node it lowers.
current_loc: base.SourceLoc,
/// Ambient checked source region recorded by `addCFStmt`.
current_region: base.Region,

/// Initializes empty storage for statement-only LIR.
pub fn init(allocator: Allocator) Self {
    return .{
        .cf_stmts = .empty,
        .cf_switch_branches = .empty,
        .str_match_steps = .empty,
        .str_match_arms = .empty,
        .join_points = .empty,
        .locals = .empty,
        .local_ids = .empty,
        .u64s = .empty,
        .proc_specs = .empty,
        .strings = base.StringLiteral.Store{},
        .string_builder = .{},
        .strings_insertable = true,
        .allocator = allocator,
        .next_synthetic_symbol = 0xf000_0000_0000_0000,
        .patterns = .empty,
        .pattern_ids = .empty,
        .source_file_bytes = .empty,
        .source_file_ends = .empty,
        .cf_stmt_locs = .empty,
        .cf_stmt_regions = .empty,
        .proc_locs = .empty,
        .proc_debug_names = .empty,
        .local_names = .empty,
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
    self.local_names.set(@intFromEnum(id), @intFromEnum(idx));
}

/// Source-level name of a local, or null for compiler-generated temporaries.
pub fn localName(self: *const Self, id: LocalId) ?[]const u8 {
    const raw = self.local_names.get(@intFromEnum(id));
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
    self.proc_locs.set(@intFromEnum(dst), self.proc_locs.get(@intFromEnum(src)));
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
    for (self.proc_debug_names.unsafeRawItemsForView()) |entry| {
        if (entry.proc == proc) return entry.string;
    }
    return null;
}

fn setProcDebugNameIndex(self: *Self, id: LirProcSpecId, string: base.StringLiteral.Idx) Allocator.Error!void {
    const proc = @intFromEnum(id);
    for (self.proc_debug_names.unsafeRawItemsMutForStore()) |*entry| {
        if (entry.proc == proc) {
            entry.string = string;
            return;
        }
    }
    try self.proc_debug_names.append(self.allocator, .{ .proc = proc, .string = string });
}

/// Copies the source file table from a lowering stage's program.
pub fn setSourceFiles(self: *Self, files: []const []const u8) Allocator.Error!void {
    std.debug.assert(self.source_file_ends.len() == 0);
    for (files) |file| {
        try self.source_file_bytes.appendSlice(self.allocator, file);
        try self.source_file_ends.append(self.allocator, @intCast(self.source_file_bytes.len()));
    }
}

/// Number of entries in the source file table.
pub fn sourceFileCount(self: *const Self) u32 {
    return @intCast(self.source_file_ends.len());
}

/// Name of one source file table entry.
pub fn sourceFileName(self: *const Self, file: u32) []const u8 {
    const end = self.source_file_ends.get(file);
    const start = if (file == 0) 0 else self.source_file_ends.get(file - 1);
    return self.source_file_bytes.unsafeRawItemsForView()[start..end];
}

/// Source location of a statement.
pub fn stmtLoc(self: *const Self, id: CFStmtId) base.SourceLoc {
    return self.cf_stmt_locs.get(@intFromEnum(id));
}

/// Checked source region of a statement.
pub fn stmtRegion(self: *const Self, id: CFStmtId) base.Region {
    return self.cf_stmt_regions.get(@intFromEnum(id));
}

/// Source location of a proc.
pub fn procLoc(self: *const Self, id: LirProcSpecId) base.SourceLoc {
    return self.proc_locs.get(@intFromEnum(id));
}

/// Appends a pattern and returns its id.
pub fn addPattern(self: *Self, pattern: LirPattern) Allocator.Error!LirPatternId {
    const id: LirPatternId = @enumFromInt(self.patterns.len());
    try self.patterns.append(self.allocator, pattern);
    return id;
}

/// Returns the pattern for a given id.
pub fn getPattern(self: *const Self, id: LirPatternId) LirPattern {
    return self.patterns.get(@intFromEnum(id));
}

/// Number of stored patterns.
pub fn patternCount(self: *const Self) usize {
    return self.patterns.len();
}

/// Returns all stored patterns.
pub fn getPatterns(self: *const Self) []const LirPattern {
    return self.patterns.unsafeRawItemsForView();
}

/// Appends a slice of pattern ids and returns the span.
pub fn addPatternSpan(self: *Self, ids: []const LirPatternId) Allocator.Error!LirPatternSpan {
    const start: u32 = @intCast(self.pattern_ids.len());
    try self.pattern_ids.appendSlice(self.allocator, ids);
    return .{ .start = start, .len = @intCast(ids.len) };
}

/// Returns the pattern ids for a given span.
pub fn getPatternSpan(self: *const Self, span: LirPatternSpan) StoreSpanBorrow(LirPatternId, "pattern_ids") {
    return self.pattern_ids.borrowSpan(span.start, span.len);
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
    const idx = self.locals.len();
    try self.locals.append(self.allocator, local);
    try self.local_names.append(self.allocator, no_local_name);
    return @enumFromInt(@as(u32, @intCast(idx)));
}

/// Number of stored LIR locals.
pub fn localCount(self: *const Self) usize {
    return self.locals.len();
}

/// Returns all stored LIR locals.
pub fn getLocals(self: *const Self) []const Local {
    return self.locals.unsafeRawItemsForView();
}

/// Returns one stored LIR local.
pub fn getLocal(self: *const Self, id: LocalId) Local {
    return self.locals.get(@intFromEnum(id));
}

/// Returns a mutable pointer to one stored LIR local.
pub fn getLocalPtr(self: *Self, id: LocalId) *Local {
    return self.locals.getPtrImmediate(@intFromEnum(id));
}

/// Stores local ids and returns the corresponding flat-storage span.
pub fn addLocalSpan(self: *Self, ids: []const LocalId) Allocator.Error!LocalSpan {
    if (ids.len == 0) return LocalSpan.empty();

    const start = @as(u32, @intCast(self.local_ids.len()));
    try self.local_ids.appendSlice(self.allocator, ids);
    return .{ .start = start, .len = @intCast(ids.len) };
}

/// Resolves a local-id span to its stored slice.
pub fn getLocalSpan(self: *const Self, span: LocalSpan) StoreSpanBorrow(LocalId, "local_ids") {
    return self.local_ids.borrowSpan(span.start, span.len);
}

/// Stores u64 values and returns the corresponding flat-storage span.
pub fn addU64Span(self: *Self, values: []const u64) Allocator.Error!U64Span {
    if (values.len == 0) return U64Span.empty();

    const start = @as(u32, @intCast(self.u64s.len()));
    try self.u64s.appendSlice(self.allocator, values);
    return .{ .start = start, .len = @intCast(values.len) };
}

/// Resolves a u64 span to its stored slice.
pub fn getU64Span(self: *const Self, span: U64Span) StoreSpanBorrow(u64, "u64s") {
    return self.u64s.borrowSpan(span.start, span.len);
}

/// Appends a statement/control-flow node and returns its id.
pub fn addCFStmt(self: *Self, stmt: CFStmt) Allocator.Error!CFStmtId {
    const idx = self.cf_stmts.len();
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

/// Number of stored control-flow statements.
pub fn cfStmtCount(self: *const Self) usize {
    return self.cf_stmts.len();
}

/// Returns all stored control-flow statements.
pub fn getCFStmts(self: *const Self) []const CFStmt {
    return self.cf_stmts.unsafeRawItemsForView();
}

/// Number of stored statement source-location entries.
pub fn cfStmtLocCount(self: *const Self) usize {
    return self.cf_stmt_locs.len();
}

/// Returns all stored statement source-location entries.
pub fn getCFStmtLocs(self: *const Self) []const base.SourceLoc {
    return self.cf_stmt_locs.unsafeRawItemsForView();
}

/// Number of stored statement source-region entries.
pub fn cfStmtRegionCount(self: *const Self) usize {
    return self.cf_stmt_regions.len();
}

/// Returns all stored statement source-region entries.
pub fn getCFStmtRegions(self: *const Self) []const base.Region {
    return self.cf_stmt_regions.unsafeRawItemsForView();
}

/// Returns the stored statement for the given id.
pub fn getCFStmt(self: *const Self, id: CFStmtId) CFStmt {
    self.verifyCFStmtId(id);
    return self.cf_stmts.get(@intFromEnum(id));
}

/// Returns a mutable pointer to the stored statement for the given id.
pub fn getCFStmtPtr(self: *Self, id: CFStmtId) *CFStmt {
    self.verifyCFStmtId(id);
    return self.cf_stmts.getPtrImmediate(@intFromEnum(id));
}

fn verifyCFStmtId(self: *const Self, id: CFStmtId) void {
    if (builtin.mode == .Debug) {
        const idx = @intFromEnum(id);
        if (idx >= self.cf_stmts.len()) {
            std.debug.panic(
                "LirStore invariant violated: statement id {d} exceeds statement storage len {d}",
                .{ idx, self.cf_stmts.len() },
            );
        }
    }
}

/// Appends switch branches and returns the corresponding flat-storage span.
pub fn addCFSwitchBranches(self: *Self, branches: []const CFSwitchBranch) Allocator.Error!CFSwitchBranchSpan {
    if (branches.len == 0) return CFSwitchBranchSpan.empty();

    const start = @as(u32, @intCast(self.cf_switch_branches.len()));
    try self.cf_switch_branches.appendSlice(self.allocator, branches);
    return .{ .start = start, .len = @intCast(branches.len) };
}

/// Resolves a switch-branch span to its stored slice.
pub fn getCFSwitchBranches(self: *const Self, span: CFSwitchBranchSpan) StoreSpanBorrow(CFSwitchBranch, "cf_switch_branches") {
    return self.cf_switch_branches.borrowSpan(span.start, span.len);
}

/// Resolves a switch-branch span to its stored mutable slice.
pub fn getCFSwitchBranchesMut(self: *Self, span: CFSwitchBranchSpan) StoreSpanBorrowMut(CFSwitchBranch, "cf_switch_branches") {
    return self.cf_switch_branches.borrowSpanMut(span.start, span.len);
}

/// Appends string-match steps and returns the corresponding flat-storage span.
pub fn addStrMatchSteps(self: *Self, steps: []const StrMatchStep) Allocator.Error!StrMatchStepSpan {
    if (steps.len == 0) return StrMatchStepSpan.empty();

    const start = @as(u32, @intCast(self.str_match_steps.len()));
    try self.str_match_steps.appendSlice(self.allocator, steps);
    return .{ .start = start, .len = @intCast(steps.len) };
}

/// Resolves a string-match-step span to its stored slice.
pub fn getStrMatchSteps(self: *const Self, span: StrMatchStepSpan) StoreSpanBorrow(StrMatchStep, "str_match_steps") {
    return self.str_match_steps.borrowSpan(span.start, span.len);
}

/// Appends string-match arms and returns the corresponding flat-storage span.
pub fn addStrMatchArms(self: *Self, arms: []const StrMatchArm) Allocator.Error!StrMatchArmSpan {
    if (arms.len == 0) return StrMatchArmSpan.empty();

    const start = @as(u32, @intCast(self.str_match_arms.len()));
    try self.str_match_arms.appendSlice(self.allocator, arms);
    return .{ .start = start, .len = @intCast(arms.len) };
}

/// Resolves a string-match-arm span to its stored slice.
pub fn getStrMatchArms(self: *const Self, span: StrMatchArmSpan) StoreSpanBorrow(StrMatchArm, "str_match_arms") {
    return self.str_match_arms.borrowSpan(span.start, span.len);
}

/// Resolves a string-match-arm span to its stored mutable slice.
pub fn getStrMatchArmsMut(self: *Self, span: StrMatchArmSpan) StoreSpanBorrowMut(StrMatchArm, "str_match_arms") {
    return self.str_match_arms.borrowSpanMut(span.start, span.len);
}

/// Appends join-point entries and returns the corresponding flat-storage span.
pub fn addJoinPointSpan(self: *Self, join_points: []const JoinPoint) Allocator.Error!JoinPointSpan {
    if (join_points.len == 0) return JoinPointSpan.empty();

    const start = @as(u32, @intCast(self.join_points.len()));
    try self.join_points.appendSlice(self.allocator, join_points);
    return .{ .start = start, .len = @intCast(join_points.len) };
}

/// Resolves a join-point span to its stored slice.
pub fn getJoinPointSpan(self: *const Self, span: JoinPointSpan) StoreSpanBorrow(JoinPoint, "join_points") {
    return self.join_points.borrowSpan(span.start, span.len);
}

/// Resolves a join-point span to its stored mutable slice.
pub fn getJoinPointSpanMut(self: *Self, span: JoinPointSpan) StoreSpanBorrowMut(JoinPoint, "join_points") {
    return self.join_points.borrowSpanMut(span.start, span.len);
}

/// Appends a proc specification and returns its id.
pub fn addProcSpec(self: *Self, proc: LirProcSpec) Allocator.Error!LirProcSpecId {
    const idx = self.proc_specs.len();
    try self.proc_specs.append(self.allocator, proc);
    try self.proc_locs.append(self.allocator, self.current_loc);
    return @enumFromInt(@as(u32, @intCast(idx)));
}

/// Number of stored proc specifications.
pub fn procSpecCount(self: *const Self) usize {
    return self.proc_specs.len();
}

/// Number of stored proc source-location entries.
pub fn procLocCount(self: *const Self) usize {
    return self.proc_locs.len();
}

/// Returns all stored proc source-location entries.
pub fn getProcLocs(self: *const Self) []const base.SourceLoc {
    return self.proc_locs.unsafeRawItemsForView();
}

/// Number of stored proc debug-name entries.
pub fn procDebugNameCount(self: *const Self) usize {
    return self.proc_debug_names.len();
}

/// Returns all stored proc debug-name entries.
pub fn getProcDebugNames(self: *const Self) []const ProcDebugName {
    return self.proc_debug_names.unsafeRawItemsForView();
}

/// Number of stored local-name entries.
pub fn localNameCount(self: *const Self) usize {
    return self.local_names.len();
}

/// Returns all raw local-name table entries.
pub fn getLocalNamesRaw(self: *const Self) []const u32 {
    return self.local_names.unsafeRawItemsForView();
}

/// Returns the stored proc specification for the given id.
pub fn getProcSpec(self: *const Self, idx: LirProcSpecId) LirProcSpec {
    return self.proc_specs.get(@intFromEnum(idx));
}

/// Updates the body for a stored proc specification.
pub fn setProcSpecBody(self: *Self, idx: LirProcSpecId, body: ?CFStmtId) void {
    self.proc_specs.getPtrImmediate(@intFromEnum(idx)).body = body;
}

/// Updates the final join-point span for a stored proc specification.
pub fn setProcSpecJoinPoints(self: *Self, idx: LirProcSpecId, join_points: JoinPointSpan) void {
    self.proc_specs.getPtrImmediate(@intFromEnum(idx)).join_points = join_points;
}

/// Updates body and final join points after all fallible/appending work has completed.
pub fn setProcSpecBodyAndJoinPoints(self: *Self, idx: LirProcSpecId, body: ?CFStmtId, join_points: JoinPointSpan) void {
    const proc = self.proc_specs.getPtrImmediate(@intFromEnum(idx));
    proc.body = body;
    proc.join_points = join_points;
}

/// Returns a mutable pointer to the stored proc specification for the given id.
pub fn getProcSpecPtr(self: *Self, idx: LirProcSpecId) *LirProcSpec {
    return self.proc_specs.getPtrImmediate(@intFromEnum(idx));
}

/// Returns all stored proc specifications.
pub fn getProcSpecs(self: *const Self) []const LirProcSpec {
    return self.proc_specs.unsafeRawItemsForView();
}

/// Returns one stored proc debug-name entry.
pub fn getProcDebugName(self: *const Self, index: usize) ProcDebugName {
    return self.proc_debug_names.get(index);
}

/// Returns the raw local-name table entry for the given local id.
pub fn getLocalNameRaw(self: *const Self, id: LocalId) u32 {
    return self.local_names.get(@intFromEnum(id));
}

/// Remaps proc debug-name entries and drops names for pruned procs.
pub fn compactProcDebugNames(self: *Self, old_to_new: []const ?LirProcSpecId) void {
    var write: usize = 0;
    const names = self.proc_debug_names.unsafeRawItemsMutForStore();
    for (names) |entry| {
        if (entry.proc >= old_to_new.len) continue;
        const new_proc = old_to_new[entry.proc] orelse continue;
        names[write] = .{
            .proc = @intFromEnum(new_proc),
            .string = entry.string,
        };
        write += 1;
    }
    self.proc_debug_names.shrinkRetainingCapacity(write);
}

/// Compacts proc specs and their parallel source-location table in place.
pub fn compactProcSpecs(self: *Self, reachable: []const bool) void {
    std.debug.assert(reachable.len == self.proc_specs.len());
    std.debug.assert(self.proc_specs.len() == self.proc_locs.len());

    var write: usize = 0;
    const proc_specs = self.proc_specs.unsafeRawItemsMutForStore();
    const proc_locs = self.proc_locs.unsafeRawItemsMutForStore();
    for (proc_specs, proc_locs, 0..) |proc, loc, index| {
        if (!reachable[index]) continue;
        proc_specs[write] = proc;
        proc_locs[write] = loc;
        write += 1;
    }
    self.proc_specs.shrinkRetainingCapacity(write);
    self.proc_locs.shrinkRetainingCapacity(write);
}

/// Compacts control-flow statements and their parallel debug metadata in place.
pub fn compactCFStmts(self: *Self, reachable: []const bool) void {
    std.debug.assert(reachable.len == self.cf_stmts.len());
    std.debug.assert(self.cf_stmts.len() == self.cf_stmt_locs.len());
    std.debug.assert(self.cf_stmts.len() == self.cf_stmt_regions.len());

    var write: usize = 0;
    const cf_stmts = self.cf_stmts.unsafeRawItemsMutForStore();
    const cf_stmt_locs = self.cf_stmt_locs.unsafeRawItemsMutForStore();
    const cf_stmt_regions = self.cf_stmt_regions.unsafeRawItemsMutForStore();
    for (cf_stmts, cf_stmt_locs, cf_stmt_regions, 0..) |stmt, loc, region, index| {
        if (!reachable[index]) continue;
        cf_stmts[write] = stmt;
        cf_stmt_locs[write] = loc;
        cf_stmt_regions[write] = region;
        write += 1;
    }
    self.cf_stmts.shrinkRetainingCapacity(write);
    self.cf_stmt_locs.shrinkRetainingCapacity(write);
    self.cf_stmt_regions.shrinkRetainingCapacity(write);
}

/// Reports whether any local in a span has a layout that requires stack probing.
pub fn localSpanNeedsStackProbe(self: *const Self, layouts: *const layout.Store, span: LocalSpan) bool {
    const locals = self.getLocalSpan(span);
    for (0..locals.len) |index| {
        const local = GuardedList.at(locals, index);
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
