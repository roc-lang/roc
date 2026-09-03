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
const InlineScope = lir_defs.InlineScope;
const InlineScopeId = lir_defs.InlineScopeId;
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
const U32Span = lir_defs.U32Span;
const ErasedCallArgsPlan = lir_defs.ErasedCallArgsPlan;
const ErasedCallArgsPlanId = lir_defs.ErasedCallArgsPlanId;

/// Source-level name to use when presenting a specialized LIR proc in debug output.
pub const ProcDebugName = extern struct {
    proc: u32,
    string: base.StringLiteral.Idx,
};

const Self = @This();

/// Lengths of the coordinator-owned prefix visible to a body worker.
/// Capturing these lengths before lowering makes the subsequently-added suffix
/// an independently appendable body shard.
pub const BodyPrefix = struct {
    cf_stmts: u32,
    cf_switch_branches: u32,
    str_match_steps: u32,
    str_match_arms: u32,
    join_points: u32,
    locals: u32,
    local_ids: u32,
    u64s: u32,
    u32s: u32,
    erased_call_arg_plans: u32,
    patterns: u32,
    pattern_ids: u32,
    inline_scopes: u32,
    strings: u32,
    proc_specs: u32,
    proc_locs: u32,
    proc_debug_names: u32,
    source_file_bytes: u32,
    source_file_ends: u32,
    source_file_qualified_bytes: u32,
    source_file_qualified_ends: u32,
};

/// A body-owned suffix of a private store. The store must outlive this view and
/// must not be mutated between capture and append.
pub const BodyShard = struct {
    store: *const Self,
    prefix: BodyPrefix,
};

/// Base indices used to translate references from a private shard to the
/// coordinator store.
pub const BodyRelocation = struct {
    cf_stmts: u32,
    cf_switch_branches: u32,
    str_match_steps: u32,
    str_match_arms: u32,
    join_points: u32,
    locals: u32,
    local_ids: u32,
    u64s: u32,
    u32s: u32,
    erased_call_arg_plans: u32,
    patterns: u32,
    pattern_ids: u32,

    pub fn local(self: BodyRelocation, prefix: BodyPrefix, id: LocalId) LocalId {
        return relocateBodyValue(LocalId, id, prefix, self);
    }

    pub fn localSpan(self: BodyRelocation, prefix: BodyPrefix, span: LocalSpan) LocalSpan {
        return relocateBodyValue(LocalSpan, span, prefix, self);
    }
};

/// Coordinator identities assigned while appending one body shard.
pub const AppendedBody = struct {
    relocation: BodyRelocation,
    root: ?CFStmtId,
    frame_locals: LocalSpan,
};

/// Failures while validating or appending a private body suffix.
pub const AppendBodyError = Allocator.Error || error{
    /// String or inline-scope interning changed after the frozen prefix.
    /// The coordinator may retry this body through serial lowering.
    UnsupportedShardMetadata,
    InvalidBodyPrefix,
};

fn ownStringEntryCount(self: *const Self) u32 {
    var iterator = self.strings.iterator();
    var count: u32 = 0;
    while (iterator.next() != null) count += 1;
    return count;
}

fn stringEntryCount(self: *const Self) u32 {
    if (self.body_coordinator) |coordinator| return coordinator.stringEntryCount();
    return self.ownStringEntryCount();
}

/// Captures the frozen prefix before a worker starts lowering a body.
pub fn captureBodyPrefix(self: *const Self) BodyPrefix {
    if (self.body_coordinator != null) return self.body_prefix;
    return .{
        .cf_stmts = @intCast(self.cf_stmts.len()),
        .cf_switch_branches = @intCast(self.cf_switch_branches.len()),
        .str_match_steps = @intCast(self.str_match_steps.len()),
        .str_match_arms = @intCast(self.str_match_arms.len()),
        .join_points = @intCast(self.join_points.len()),
        .locals = @intCast(self.locals.len()),
        .local_ids = @intCast(self.local_ids.len()),
        .u64s = @intCast(self.u64s.len()),
        .u32s = @intCast(self.u32s.len()),
        .erased_call_arg_plans = @intCast(self.erased_call_arg_plans.len()),
        .patterns = @intCast(self.patterns.len()),
        .pattern_ids = @intCast(self.pattern_ids.len()),
        .inline_scopes = @intCast(self.inline_scopes.len()),
        .strings = self.stringEntryCount(),
        .proc_specs = @intCast(self.proc_specs.len()),
        .proc_locs = @intCast(self.proc_locs.len()),
        .proc_debug_names = @intCast(self.proc_debug_names.len()),
        .source_file_bytes = @intCast(self.source_file_bytes.len()),
        .source_file_ends = @intCast(self.source_file_ends.len()),
        .source_file_qualified_bytes = @intCast(self.source_file_qualified_bytes.len()),
        .source_file_qualified_ends = @intCast(self.source_file_qualified_ends.len()),
    };
}

/// Creates an isolated body worker store which reads its frozen prefix from the
/// coordinator. Worker-owned lists therefore contain only body suffix data.
pub fn cloneForBodyShard(self: *const Self, allocator: Allocator) Allocator.Error!Self {
    var result = Self.init(allocator);
    result.body_coordinator = self;
    result.body_prefix = self.captureBodyPrefix();
    result.strings_insertable = false;
    result.next_synthetic_symbol = self.next_synthetic_symbol;
    result.current_loc = self.current_loc;
    result.current_region = self.current_region;
    result.current_inline_scope = self.current_inline_scope;
    return result;
}

/// Captures the suffix added since `prefix`.
pub fn captureBodyShard(self: *const Self, prefix: BodyPrefix) AppendBodyError!BodyShard {
    if (self.body_coordinator != null) {
        if (!std.meta.eql(prefix, self.body_prefix)) return error.InvalidBodyPrefix;
        if (self.inline_scopes.len() != 0 or
            self.ownStringEntryCount() != 0 or
            self.proc_specs.len() != 0 or
            self.proc_locs.len() != 0 or
            self.proc_debug_names.len() != 0 or
            self.source_file_bytes.len() != 0 or
            self.source_file_ends.len() != 0 or
            self.source_file_qualified_bytes.len() != 0 or
            self.source_file_qualified_ends.len() != 0)
        {
            return error.UnsupportedShardMetadata;
        }
        return .{ .store = self, .prefix = prefix };
    }
    if (prefix.cf_stmts > self.cf_stmts.len() or
        prefix.cf_switch_branches > self.cf_switch_branches.len() or
        prefix.str_match_steps > self.str_match_steps.len() or
        prefix.str_match_arms > self.str_match_arms.len() or
        prefix.join_points > self.join_points.len() or
        prefix.locals > self.locals.len() or
        prefix.local_ids > self.local_ids.len() or
        prefix.u64s > self.u64s.len() or
        prefix.u32s > self.u32s.len() or
        prefix.erased_call_arg_plans > self.erased_call_arg_plans.len() or
        prefix.patterns > self.patterns.len() or
        prefix.pattern_ids > self.pattern_ids.len() or
        prefix.inline_scopes > self.inline_scopes.len() or
        prefix.strings > self.stringEntryCount() or
        prefix.proc_specs > self.proc_specs.len() or
        prefix.proc_locs > self.proc_locs.len() or
        prefix.proc_debug_names > self.proc_debug_names.len() or
        prefix.source_file_bytes > self.source_file_bytes.len() or
        prefix.source_file_ends > self.source_file_ends.len() or
        prefix.source_file_qualified_bytes > self.source_file_qualified_bytes.len() or
        prefix.source_file_qualified_ends > self.source_file_qualified_ends.len())
    {
        return error.InvalidBodyPrefix;
    }
    if (prefix.inline_scopes != self.inline_scopes.len() or
        prefix.strings != self.stringEntryCount() or
        prefix.proc_specs != self.proc_specs.len() or
        prefix.proc_locs != self.proc_locs.len() or
        prefix.proc_debug_names != self.proc_debug_names.len() or
        prefix.source_file_bytes != self.source_file_bytes.len() or
        prefix.source_file_ends != self.source_file_ends.len() or
        prefix.source_file_qualified_bytes != self.source_file_qualified_bytes.len() or
        prefix.source_file_qualified_ends != self.source_file_qualified_ends.len())
    {
        return error.UnsupportedShardMetadata;
    }
    return .{ .store = self, .prefix = prefix };
}

fn movedIndex(value: u32, prefix: u32, destination: u32) u32 {
    return if (value < prefix) value else destination + (value - prefix);
}

fn relocateBodyValue(comptime T: type, value: T, prefix: BodyPrefix, bases: BodyRelocation) T {
    if (T == LocalId) return @enumFromInt(movedIndex(@intFromEnum(value), prefix.locals, bases.locals));
    if (T == CFStmtId) return @enumFromInt(movedIndex(@intFromEnum(value), prefix.cf_stmts, bases.cf_stmts));
    if (T == ErasedCallArgsPlanId) return @enumFromInt(movedIndex(@intFromEnum(value), prefix.erased_call_arg_plans, bases.erased_call_arg_plans));
    if (T == LirPatternId) {
        if (value == LirPatternId.none) return value;
        return @enumFromInt(movedIndex(@intFromEnum(value), prefix.patterns, bases.patterns));
    }
    if (T == InlineScopeId) return value;
    if (T == LocalSpan) return if (value.len == 0) value else .{ .start = movedIndex(value.start, prefix.local_ids, bases.local_ids), .len = value.len };
    if (T == CFSwitchBranchSpan) return if (value.len == 0) value else .{ .start = movedIndex(value.start, prefix.cf_switch_branches, bases.cf_switch_branches), .len = value.len };
    if (T == StrMatchStepSpan) return if (value.len == 0) value else .{ .start = movedIndex(value.start, prefix.str_match_steps, bases.str_match_steps), .len = value.len };
    if (T == StrMatchArmSpan) return if (value.len == 0) value else .{ .start = movedIndex(value.start, prefix.str_match_arms, bases.str_match_arms), .len = value.len };
    if (T == JoinPointSpan) return if (value.len == 0) value else .{ .start = movedIndex(value.start, prefix.join_points, bases.join_points), .len = value.len };
    if (T == U64Span) return if (value.len == 0) value else .{ .start = movedIndex(value.start, prefix.u64s, bases.u64s), .len = value.len };
    if (T == U32Span) return if (value.len == 0) value else .{ .start = movedIndex(value.start, prefix.u32s, bases.u32s), .len = value.len };
    if (T == LirPatternSpan) return if (value.len == 0) value else .{ .start = movedIndex(value.start, prefix.pattern_ids, bases.pattern_ids), .len = value.len };

    const type_info = @typeInfo(T);
    if (comptime std.meta.activeTag(type_info) == .optional) {
        if (value) |payload| {
            return relocateBodyValue(type_info.optional.child, payload, prefix, bases);
        }
        return value;
    }
    if (comptime std.meta.activeTag(type_info) == .@"struct") {
        var result = value;
        inline for (type_info.@"struct".fields) |field| {
            @field(result, field.name) = relocateBodyValue(field.type, @field(value, field.name), prefix, bases);
        }
        return result;
    }
    if (comptime std.meta.activeTag(type_info) == .@"union") {
        const tag_type = type_info.@"union".tag_type orelse return value;
        const active_tag = std.meta.activeTag(value);
        inline for (type_info.@"union".fields) |field| {
            if (active_tag == @field(tag_type, field.name)) {
                const payload = @field(value, field.name);
                return @unionInit(T, field.name, relocateBodyValue(field.type, payload, prefix, bases));
            }
        }
        unreachable;
    }
    return value;
}

/// Deterministically appends one body suffix. All capacity is acquired before
/// any logical mutation, so allocation failure leaves destination lengths and
/// contents unchanged.
pub fn appendBodyShard(
    self: *Self,
    shard: BodyShard,
    root: ?CFStmtId,
    frame_locals: LocalSpan,
) AppendBodyError!AppendedBody {
    const source = shard.store;
    const prefix = shard.prefix;
    const source_prefix: BodyPrefix = if (source.body_coordinator != null)
        std.mem.zeroes(BodyPrefix)
    else
        prefix;
    // Revalidate because a BodyShard is a borrowed suffix view.
    _ = try source.captureBodyShard(prefix);
    if (source.cf_stmts.len() != source.cf_stmt_locs.len() or
        source.cf_stmts.len() != source.cf_stmt_regions.len() or
        source.cf_stmts.len() != source.cf_stmt_inline_scopes.len() or
        source.locals.len() != source.local_names.len())
    {
        return error.InvalidBodyPrefix;
    }

    const bases: BodyRelocation = .{
        .cf_stmts = @intCast(self.cf_stmts.len()),
        .cf_switch_branches = @intCast(self.cf_switch_branches.len()),
        .str_match_steps = @intCast(self.str_match_steps.len()),
        .str_match_arms = @intCast(self.str_match_arms.len()),
        .join_points = @intCast(self.join_points.len()),
        .locals = @intCast(self.locals.len()),
        .local_ids = @intCast(self.local_ids.len()),
        .u64s = @intCast(self.u64s.len()),
        .u32s = @intCast(self.u32s.len()),
        .erased_call_arg_plans = @intCast(self.erased_call_arg_plans.len()),
        .patterns = @intCast(self.patterns.len()),
        .pattern_ids = @intCast(self.pattern_ids.len()),
    };
    const stmt_len = source.cf_stmts.len() - source_prefix.cf_stmts;
    const local_len = source.locals.len() - source_prefix.locals;

    // Reserve every destination before the first append. Parallel metadata
    // arrays are reserved and appended alongside their owner arrays.
    try self.cf_stmts.ensureUnusedCapacity(self.allocator, stmt_len);
    try self.cf_stmt_locs.ensureUnusedCapacity(self.allocator, stmt_len);
    try self.cf_stmt_regions.ensureUnusedCapacity(self.allocator, stmt_len);
    try self.cf_stmt_inline_scopes.ensureUnusedCapacity(self.allocator, stmt_len);
    try self.cf_switch_branches.ensureUnusedCapacity(self.allocator, source.cf_switch_branches.len() - source_prefix.cf_switch_branches);
    try self.str_match_steps.ensureUnusedCapacity(self.allocator, source.str_match_steps.len() - source_prefix.str_match_steps);
    try self.str_match_arms.ensureUnusedCapacity(self.allocator, source.str_match_arms.len() - source_prefix.str_match_arms);
    try self.join_points.ensureUnusedCapacity(self.allocator, source.join_points.len() - source_prefix.join_points);
    try self.locals.ensureUnusedCapacity(self.allocator, local_len);
    try self.local_names.ensureUnusedCapacity(self.allocator, local_len);
    try self.local_ids.ensureUnusedCapacity(self.allocator, source.local_ids.len() - source_prefix.local_ids);
    try self.u64s.ensureUnusedCapacity(self.allocator, source.u64s.len() - source_prefix.u64s);
    try self.u32s.ensureUnusedCapacity(self.allocator, source.u32s.len() - source_prefix.u32s);
    try self.erased_call_arg_plans.ensureUnusedCapacity(self.allocator, source.erased_call_arg_plans.len() - source_prefix.erased_call_arg_plans);
    try self.patterns.ensureUnusedCapacity(self.allocator, source.patterns.len() - source_prefix.patterns);
    try self.pattern_ids.ensureUnusedCapacity(self.allocator, source.pattern_ids.len() - source_prefix.pattern_ids);

    const local_ids = source.local_ids.unsafeRawItemsForView();
    for (local_ids[source_prefix.local_ids..]) |item| try self.local_ids.append(self.allocator, relocateBodyValue(LocalId, item, prefix, bases));
    try self.u64s.appendSlice(self.allocator, source.u64s.unsafeRawItemsForView()[source_prefix.u64s..]);
    try self.u32s.appendSlice(self.allocator, source.u32s.unsafeRawItemsForView()[source_prefix.u32s..]);
    for (source.erased_call_arg_plans.unsafeRawItemsForView()[source_prefix.erased_call_arg_plans..]) |item| try self.erased_call_arg_plans.append(self.allocator, relocateBodyValue(ErasedCallArgsPlan, item, prefix, bases));
    for (source.pattern_ids.unsafeRawItemsForView()[source_prefix.pattern_ids..]) |item| try self.pattern_ids.append(self.allocator, relocateBodyValue(LirPatternId, item, prefix, bases));
    for (source.patterns.unsafeRawItemsForView()[source_prefix.patterns..]) |item| try self.patterns.append(self.allocator, relocateBodyValue(LirPattern, item, prefix, bases));
    for (source.locals.unsafeRawItemsForView()[source_prefix.locals..]) |item| try self.locals.append(self.allocator, relocateBodyValue(Local, item, prefix, bases));
    try self.local_names.appendSlice(self.allocator, source.local_names.unsafeRawItemsForView()[source_prefix.locals..]);
    for (source.cf_switch_branches.unsafeRawItemsForView()[source_prefix.cf_switch_branches..]) |item| try self.cf_switch_branches.append(self.allocator, relocateBodyValue(CFSwitchBranch, item, prefix, bases));
    for (source.str_match_steps.unsafeRawItemsForView()[source_prefix.str_match_steps..]) |item| try self.str_match_steps.append(self.allocator, relocateBodyValue(StrMatchStep, item, prefix, bases));
    for (source.str_match_arms.unsafeRawItemsForView()[source_prefix.str_match_arms..]) |item| try self.str_match_arms.append(self.allocator, relocateBodyValue(StrMatchArm, item, prefix, bases));
    for (source.join_points.unsafeRawItemsForView()[source_prefix.join_points..]) |item| try self.join_points.append(self.allocator, relocateBodyValue(JoinPoint, item, prefix, bases));
    for (source.cf_stmts.unsafeRawItemsForView()[source_prefix.cf_stmts..]) |item| try self.cf_stmts.append(self.allocator, relocateBodyValue(CFStmt, item, prefix, bases));
    try self.cf_stmt_locs.appendSlice(self.allocator, source.cf_stmt_locs.unsafeRawItemsForView()[source_prefix.cf_stmts..]);
    try self.cf_stmt_regions.appendSlice(self.allocator, source.cf_stmt_regions.unsafeRawItemsForView()[source_prefix.cf_stmts..]);
    try self.cf_stmt_inline_scopes.appendSlice(self.allocator, source.cf_stmt_inline_scopes.unsafeRawItemsForView()[source_prefix.cf_stmts..]);

    return .{
        .relocation = bases,
        .root = if (root) |id| relocateBodyValue(CFStmtId, id, prefix, bases) else null,
        .frame_locals = relocateBodyValue(LocalSpan, frame_locals, prefix, bases),
    };
}

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
u32s: GuardedList.List(u32, "LirStore.u32s"),
erased_call_arg_plans: GuardedList.List(ErasedCallArgsPlan, "LirStore.erased_call_arg_plans"),
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
/// Package-qualified module identity per source file table entry (e.g.
/// `pf.Utils`), flattened exactly like `source_file_bytes`/`source_file_ends`.
/// Module identity comparisons ("does this failed statement belong to the
/// finalized module?") must use these, never the display names: two packages
/// may both contain a module with the same bare name.
source_file_qualified_bytes: GuardedList.List(u8, "LirStore.source_file_qualified_bytes"),
source_file_qualified_ends: GuardedList.List(u32, "LirStore.source_file_qualified_ends"),
/// Source location per statement, parallel to `cf_stmts`. Reference-count
/// statements always record `SourceLoc.none`; they have no source counterpart.
cf_stmt_locs: GuardedList.List(base.SourceLoc, "LirStore.cf_stmt_locs"),
/// Checked source region per statement, parallel to `cf_stmts`. Reference-count
/// statements always record `Region.zero`; they have no source counterpart.
cf_stmt_regions: GuardedList.List(base.Region, "LirStore.cf_stmt_regions"),
/// Virtual inline scope per statement, parallel to `cf_stmts`.
cf_stmt_inline_scopes: GuardedList.List(InlineScopeId, "LirStore.cf_stmt_inline_scopes"),
/// Interned virtual source-frame graph.
inline_scopes: GuardedList.List(InlineScope, "LirStore.inline_scopes"),
/// Immutable coordinator storage visible to a body worker. A null pointer
/// denotes a normal coordinator store.
body_coordinator: ?*const Self,
/// Logical lengths occupied by `body_coordinator` in a body worker.
body_prefix: BodyPrefix,
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
/// Ambient virtual source frame recorded by `addCFStmt`.
current_inline_scope: InlineScopeId,

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
        .u32s = .empty,
        .erased_call_arg_plans = .empty,
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
        .source_file_qualified_bytes = .empty,
        .source_file_qualified_ends = .empty,
        .cf_stmt_locs = .empty,
        .cf_stmt_regions = .empty,
        .cf_stmt_inline_scopes = .empty,
        .inline_scopes = .empty,
        .body_coordinator = null,
        .body_prefix = std.mem.zeroes(BodyPrefix),
        .proc_locs = .empty,
        .proc_debug_names = .empty,
        .local_names = .empty,
        .current_loc = base.SourceLoc.none,
        .current_region = base.Region.zero(),
        .current_inline_scope = InlineScopeId.none,
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
    self.u32s.deinit(self.allocator);
    self.erased_call_arg_plans.deinit(self.allocator);
    self.proc_specs.deinit(self.allocator);
    self.string_builder.deinit(self.allocator);
    self.strings.deinit(self.allocator);
    self.patterns.deinit(self.allocator);
    self.pattern_ids.deinit(self.allocator);
    self.source_file_bytes.deinit(self.allocator);
    self.source_file_ends.deinit(self.allocator);
    self.source_file_qualified_bytes.deinit(self.allocator);
    self.source_file_qualified_ends.deinit(self.allocator);
    self.cf_stmt_locs.deinit(self.allocator);
    self.cf_stmt_regions.deinit(self.allocator);
    self.cf_stmt_inline_scopes.deinit(self.allocator);
    self.inline_scopes.deinit(self.allocator);
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
    const raw = self.getLocalNameRaw(id);
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

/// Stored debug-name string id for a proc, used when a procedure boundary is
/// replaced by an explicit virtual inline frame.
pub fn procDebugNameString(self: *const Self, id: LirProcSpecId) base.StringLiteral.Idx {
    return self.procDebugNameIndex(id) orelse base.StringLiteral.Idx.none;
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
pub fn setSourceFiles(self: *Self, files: []const base.SourceFileEntry) Allocator.Error!void {
    std.debug.assert(self.source_file_ends.len() == 0);
    std.debug.assert(self.source_file_qualified_ends.len() == 0);
    for (files) |file| {
        try self.source_file_bytes.appendSlice(self.allocator, file.name);
        try self.source_file_ends.append(self.allocator, @intCast(self.source_file_bytes.len()));
        try self.source_file_qualified_bytes.appendSlice(self.allocator, file.qualified_name);
        try self.source_file_qualified_ends.append(self.allocator, @intCast(self.source_file_qualified_bytes.len()));
    }
}

/// Number of entries in the source file table.
pub fn sourceFileCount(self: *const Self) u32 {
    if (self.body_coordinator) |coordinator| return coordinator.sourceFileCount();
    return @intCast(self.source_file_ends.len());
}

/// Display name of one source file table entry.
pub fn sourceFileName(self: *const Self, file: u32) []const u8 {
    if (self.body_coordinator) |coordinator| return coordinator.sourceFileName(file);
    const end = self.source_file_ends.get(file);
    const start = if (file == 0) 0 else self.source_file_ends.get(file - 1);
    return self.source_file_bytes.unsafeRawItemsForView()[start..end];
}

/// Package-qualified module identity of one source file table entry. Use
/// this (not `sourceFileName`) whenever a location's owning module is
/// compared against another module: bare names collide across packages.
pub fn sourceFileQualifiedName(self: *const Self, file: u32) []const u8 {
    if (self.body_coordinator) |coordinator| return coordinator.sourceFileQualifiedName(file);
    const end = self.source_file_qualified_ends.get(file);
    const start = if (file == 0) 0 else self.source_file_qualified_ends.get(file - 1);
    return self.source_file_qualified_bytes.unsafeRawItemsForView()[start..end];
}

/// Source location of a statement.
pub fn stmtLoc(self: *const Self, id: CFStmtId) base.SourceLoc {
    const index = @intFromEnum(id);
    if (self.body_coordinator) |coordinator| {
        if (index < self.body_prefix.cf_stmts) return coordinator.stmtLoc(id);
        return self.cf_stmt_locs.get(index - self.body_prefix.cf_stmts);
    }
    return self.cf_stmt_locs.get(index);
}

/// Virtual source frame associated with a statement.
pub fn stmtInlineScope(self: *const Self, id: CFStmtId) InlineScopeId {
    const index = @intFromEnum(id);
    if (self.body_coordinator) |coordinator| {
        if (index < self.body_prefix.cf_stmts) return coordinator.stmtInlineScope(id);
        return self.cf_stmt_inline_scopes.get(index - self.body_prefix.cf_stmts);
    }
    return self.cf_stmt_inline_scopes.get(index);
}

/// Retrieve one virtual source frame.
pub fn inlineScope(self: *const Self, id: InlineScopeId) InlineScope {
    if (self.body_coordinator) |coordinator| return coordinator.inlineScope(id);
    return self.inline_scopes.get(@intFromEnum(id));
}

/// Number of virtual source frames.
pub fn inlineScopeCount(self: *const Self) usize {
    if (self.body_coordinator) |coordinator| return coordinator.inlineScopeCount();
    return self.inline_scopes.len();
}

/// Intern one virtual source frame and return its identifier.
pub fn addInlineScope(self: *Self, scope: InlineScope) Allocator.Error!InlineScopeId {
    self.assertBodyMetadataImmutable();
    const id: InlineScopeId = @enumFromInt(@as(u32, @intCast(self.inline_scopes.len())));
    try self.inline_scopes.append(self.allocator, scope);
    return id;
}

/// Checked source region of a statement.
pub fn stmtRegion(self: *const Self, id: CFStmtId) base.Region {
    const index = @intFromEnum(id);
    if (self.body_coordinator) |coordinator| {
        if (index < self.body_prefix.cf_stmts) return coordinator.stmtRegion(id);
        return self.cf_stmt_regions.get(index - self.body_prefix.cf_stmts);
    }
    return self.cf_stmt_regions.get(index);
}

/// Source location of a proc.
pub fn procLoc(self: *const Self, id: LirProcSpecId) base.SourceLoc {
    if (self.body_coordinator) |coordinator| return coordinator.procLoc(id);
    return self.proc_locs.get(@intFromEnum(id));
}

/// Appends a pattern and returns its id.
pub fn addPattern(self: *Self, pattern: LirPattern) Allocator.Error!LirPatternId {
    const id: LirPatternId = @enumFromInt(self.patterns.len() + if (self.body_coordinator != null) self.body_prefix.patterns else 0);
    try self.patterns.append(self.allocator, pattern);
    return id;
}

/// Returns the pattern for a given id.
pub fn getPattern(self: *const Self, id: LirPatternId) LirPattern {
    const index = @intFromEnum(id);
    if (self.body_coordinator) |coordinator| {
        if (index < self.body_prefix.patterns) return coordinator.getPattern(id);
        return self.patterns.get(index - self.body_prefix.patterns);
    }
    return self.patterns.get(index);
}

/// Number of stored patterns.
pub fn patternCount(self: *const Self) usize {
    return self.patterns.len() + if (self.body_coordinator != null) self.body_prefix.patterns else 0;
}

/// Returns all stored patterns.
pub fn getPatterns(self: *const Self) []const LirPattern {
    return self.patterns.unsafeRawItemsForView();
}

/// Appends a slice of pattern ids and returns the span.
pub fn addPatternSpan(self: *Self, ids: []const LirPatternId) Allocator.Error!LirPatternSpan {
    const start: u32 = @intCast(self.pattern_ids.len() + if (self.body_coordinator != null) self.body_prefix.pattern_ids else 0);
    try self.pattern_ids.appendSlice(self.allocator, ids);
    return .{ .start = start, .len = @intCast(ids.len) };
}

/// Returns the pattern ids for a given span.
pub fn getPatternSpan(self: *const Self, span: LirPatternSpan) StoreSpanBorrow(LirPatternId, "pattern_ids") {
    if (self.body_coordinator) |coordinator| {
        if (span.start < self.body_prefix.pattern_ids) return coordinator.getPatternSpan(span);
        return self.pattern_ids.borrowSpan(span.start - self.body_prefix.pattern_ids, span.len);
    }
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
    return try self.insertStringAligned(text, 1);
}

/// Interns string backing bytes with the requested minimum alignment.
pub fn insertStringAligned(self: *Self, text: []const u8, alignment: u32) Allocator.Error!base.StringLiteral.Idx {
    self.assertStringsInsertable();
    return self.string_builder.insertAligned(&self.strings, self.allocator, text, alignment);
}

/// Interns string backing bytes and returns a literal view into them.
pub fn insertStringView(
    self: *Self,
    backing: []const u8,
    offset: u32,
    len: u32,
) Allocator.Error!lir_defs.StrLiteral {
    return try self.insertStringViewAligned(backing, offset, len, 1);
}

/// Interns shared literal backing bytes, raising their runtime alignment to the
/// maximum required by every string or packed-list view.
pub fn insertStringViewAligned(
    self: *Self,
    backing: []const u8,
    offset: u32,
    len: u32,
    alignment: u32,
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
        .backing = try self.insertStringAligned(backing, alignment),
        .offset = offset,
        .len = len,
    };
}

/// Returns the text for an interned string literal.
pub fn getString(self: *const Self, idx: base.StringLiteral.Idx) []const u8 {
    if (self.body_coordinator) |coordinator| return coordinator.getString(idx);
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

fn assertBodyMetadataImmutable(self: *const Self) void {
    if (self.body_coordinator == null) return;
    if (comptime builtin.mode == .Debug) {
        std.debug.panic("LirStore invariant violated: attempted to mutate coordinator metadata from body worker", .{});
    }
    unreachable;
}

/// Registers one LIR local and returns its id.
pub fn addLocal(self: *Self, local: Local) Allocator.Error!LocalId {
    const idx = self.locals.len() + if (self.body_coordinator != null) self.body_prefix.locals else 0;
    try self.locals.append(self.allocator, local);
    try self.local_names.append(self.allocator, no_local_name);
    return @enumFromInt(@as(u32, @intCast(idx)));
}

/// Number of stored LIR locals.
pub fn localCount(self: *const Self) usize {
    return self.locals.len() + if (self.body_coordinator != null) self.body_prefix.locals else 0;
}

/// Returns all stored LIR locals.
pub fn getLocals(self: *const Self) []const Local {
    return self.locals.unsafeRawItemsForView();
}

/// Returns one stored LIR local.
pub fn getLocal(self: *const Self, id: LocalId) Local {
    const index = @intFromEnum(id);
    if (self.body_coordinator) |coordinator| {
        if (index < self.body_prefix.locals) return coordinator.getLocal(id);
        return self.locals.get(index - self.body_prefix.locals);
    }
    return self.locals.get(index);
}

/// Returns a mutable pointer to one stored LIR local.
pub fn getLocalPtr(self: *Self, id: LocalId) *Local {
    const index = @intFromEnum(id);
    if (self.body_coordinator != null and index < self.body_prefix.locals) {
        self.assertBodyMetadataImmutable();
    }
    return self.locals.getPtrImmediate(index - if (self.body_coordinator != null) self.body_prefix.locals else 0);
}

/// Records the boxy descriptor governing a local's runtime payload.
pub fn setLocalBoxyDesc(self: *Self, id: LocalId, desc: lir_defs.BoxyDescRef) void {
    const local = self.getLocalPtr(id);
    if (local.boxy_desc) |existing| {
        if (!std.meta.eql(existing, desc)) {
            std.debug.panic(
                "LIR store invariant violated: local {d} was assigned two different boxy descriptors: existing={any} new={any}",
                .{ @intFromEnum(id), existing, desc },
            );
        }
        return;
    }
    local.boxy_desc = desc;
}

/// Attaches descriptor metadata without mutating existing LIR statements.
/// Stores local ids and returns the corresponding flat-storage span.
pub fn addLocalSpan(self: *Self, ids: []const LocalId) Allocator.Error!LocalSpan {
    if (ids.len == 0) return LocalSpan.empty();

    const start = @as(u32, @intCast(self.local_ids.len() + if (self.body_coordinator != null) self.body_prefix.local_ids else 0));
    try self.local_ids.appendSlice(self.allocator, ids);
    return .{ .start = start, .len = @intCast(ids.len) };
}

/// Resolves a local-id span to its stored slice.
pub fn getLocalSpan(self: *const Self, span: LocalSpan) StoreSpanBorrow(LocalId, "local_ids") {
    if (self.body_coordinator) |coordinator| {
        if (span.start < self.body_prefix.local_ids) return coordinator.getLocalSpan(span);
        return self.local_ids.borrowSpan(span.start - self.body_prefix.local_ids, span.len);
    }
    return self.local_ids.borrowSpan(span.start, span.len);
}

/// Stores u64 values and returns the corresponding flat-storage span.
pub fn addU64Span(self: *Self, values: []const u64) Allocator.Error!U64Span {
    if (values.len == 0) return U64Span.empty();

    const start = @as(u32, @intCast(self.u64s.len() + if (self.body_coordinator != null) self.body_prefix.u64s else 0));
    try self.u64s.appendSlice(self.allocator, values);
    return .{ .start = start, .len = @intCast(values.len) };
}

/// Resolves a u64 span to its stored slice.
pub fn getU64Span(self: *const Self, span: U64Span) StoreSpanBorrow(u64, "u64s") {
    if (self.body_coordinator) |coordinator| {
        if (span.start < self.body_prefix.u64s) return coordinator.getU64Span(span);
        return self.u64s.borrowSpan(span.start - self.body_prefix.u64s, span.len);
    }
    return self.u64s.borrowSpan(span.start, span.len);
}

/// Stores u32 values and returns the corresponding flat-storage span.
pub fn addU32Span(self: *Self, values: []const u32) Allocator.Error!U32Span {
    if (values.len == 0) return U32Span.empty();
    const start: u32 = @intCast(self.u32s.len() + if (self.body_coordinator != null) self.body_prefix.u32s else 0);
    try self.u32s.appendSlice(self.allocator, values);
    return .{ .start = start, .len = @intCast(values.len) };
}

/// Resolves a u32 span to its stored slice.
pub fn getU32Span(self: *const Self, span: U32Span) StoreSpanBorrow(u32, "u32s") {
    if (self.body_coordinator) |coordinator| {
        if (span.start < self.body_prefix.u32s) return coordinator.getU32Span(span);
        return self.u32s.borrowSpan(span.start - self.body_prefix.u32s, span.len);
    }
    return self.u32s.borrowSpan(span.start, span.len);
}

/// Intern the canonical erased-call argument layout for an ordered signature.
pub fn internErasedCallArgsPlan(
    self: *Self,
    layouts: *const layout.Store,
    arg_layouts: []const layout.Idx,
) Allocator.Error!ErasedCallArgsPlanId {
    const offsets = try self.allocator.alloc(u32, arg_layouts.len);
    defer self.allocator.free(offsets);
    const metrics = layout.erased_call_abi.plan(layouts, arg_layouts, offsets);

    const prefix_plans = if (self.body_coordinator) |coordinator|
        coordinator.erased_call_arg_plans.unsafeRawItemsForView()
    else
        &.{};
    for (prefix_plans, 0..) |existing, index| {
        const existing_offsets = if (self.body_coordinator) |coordinator|
            coordinator.u32s.unsafeRawItemsForView()[existing.offsets.start..][0..existing.offsets.len]
        else
            self.u32s.unsafeRawItemsForView()[existing.offsets.start..][0..existing.offsets.len];
        if (existing.size == metrics.size and
            existing.alignment == metrics.alignment and
            std.mem.eql(u32, existing_offsets, offsets))
        {
            return @enumFromInt(@as(u32, @intCast(index)));
        }
    }
    for (self.erased_call_arg_plans.unsafeRawItemsForView(), 0..) |existing, suffix_index| {
        const suffix_start = existing.offsets.start - if (self.body_coordinator != null) self.body_prefix.u32s else 0;
        const existing_offsets = self.u32s.unsafeRawItemsForView()[suffix_start..][0..existing.offsets.len];
        if (existing.size == metrics.size and
            existing.alignment == metrics.alignment and
            std.mem.eql(u32, existing_offsets, offsets))
        {
            return @enumFromInt(@as(u32, @intCast(suffix_index + prefix_plans.len)));
        }
    }

    const id: ErasedCallArgsPlanId = @enumFromInt(@as(u32, @intCast(self.erased_call_arg_plans.len() + prefix_plans.len)));
    try self.erased_call_arg_plans.append(self.allocator, .{
        .offsets = try self.addU32Span(offsets),
        .size = metrics.size,
        .alignment = metrics.alignment,
    });
    return id;
}

/// Return an interned erased-call argument layout plan.
pub fn getErasedCallArgsPlan(self: *const Self, id: ErasedCallArgsPlanId) ErasedCallArgsPlan {
    const index = @intFromEnum(id);
    if (self.body_coordinator) |coordinator| {
        if (index < self.body_prefix.erased_call_arg_plans) return coordinator.getErasedCallArgsPlan(id);
        return self.erased_call_arg_plans.get(index - self.body_prefix.erased_call_arg_plans);
    }
    return self.erased_call_arg_plans.get(index);
}

/// Return the number of interned erased-call argument layout plans.
pub fn erasedCallArgsPlanCount(self: *const Self) usize {
    return self.erased_call_arg_plans.len() + if (self.body_coordinator != null) self.body_prefix.erased_call_arg_plans else 0;
}

/// Borrow the ordered field offsets named by an erased-call argument layout plan.
pub fn getErasedCallArgOffsets(self: *const Self, plan: ErasedCallArgsPlan) StoreSpanBorrow(u32, "u32s") {
    return self.getU32Span(plan.offsets);
}

/// Appends a statement/control-flow node and returns its id.
pub fn addCFStmt(self: *Self, stmt: CFStmt) Allocator.Error!CFStmtId {
    const idx = self.cf_stmts.len() + if (self.body_coordinator != null) self.body_prefix.cf_stmts else 0;
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
        .assign_boxy_desc_ref,
        .assign_boxy_dict_ref,
        .assign_boxy_box,
        .assign_boxy_reuse_box,
        .assign_boxy_unbox,
        .assign_boxy_adapt,
        .assign_boxy_inspect,
        .assign_boxy_eq,
        .assign_boxy_tag,
        .assign_boxy_tag_payload,
        .boxy_tag_match,
        .assign_call_dict,
        .assign_low_level,
        .assign_list,
        .assign_struct,
        .assign_tag,
        .store_struct,
        .store_tag,
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
    const inline_scope = if (has_source) self.current_inline_scope else InlineScopeId.none;
    try self.cf_stmt_locs.append(self.allocator, loc);
    try self.cf_stmt_regions.append(self.allocator, region);
    try self.cf_stmt_inline_scopes.append(self.allocator, inline_scope);
    return @enumFromInt(@as(u32, @intCast(idx)));
}

/// Number of stored control-flow statements.
pub fn cfStmtCount(self: *const Self) usize {
    return self.cf_stmts.len() + if (self.body_coordinator != null) self.body_prefix.cf_stmts else 0;
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
    const index = @intFromEnum(id);
    if (self.body_coordinator) |coordinator| {
        if (index < self.body_prefix.cf_stmts) return coordinator.getCFStmt(id);
        return self.cf_stmts.get(index - self.body_prefix.cf_stmts);
    }
    return self.cf_stmts.get(index);
}

/// Returns a mutable pointer to the stored statement for the given id.
pub fn getCFStmtPtr(self: *Self, id: CFStmtId) *CFStmt {
    self.verifyCFStmtId(id);
    const index = @intFromEnum(id);
    if (self.body_coordinator != null and index < self.body_prefix.cf_stmts) {
        self.assertBodyMetadataImmutable();
    }
    return self.cf_stmts.getPtrImmediate(index - if (self.body_coordinator != null) self.body_prefix.cf_stmts else 0);
}

fn verifyCFStmtId(self: *const Self, id: CFStmtId) void {
    if (builtin.mode == .Debug) {
        const idx = @intFromEnum(id);
        if (idx >= self.cfStmtCount()) {
            std.debug.panic(
                "LirStore invariant violated: statement id {d} exceeds statement storage len {d}",
                .{ idx, self.cfStmtCount() },
            );
        }
    }
}

/// Appends switch branches and returns the corresponding flat-storage span.
pub fn addCFSwitchBranches(self: *Self, branches: []const CFSwitchBranch) Allocator.Error!CFSwitchBranchSpan {
    if (branches.len == 0) return CFSwitchBranchSpan.empty();

    const start = @as(u32, @intCast(self.cf_switch_branches.len() + if (self.body_coordinator != null) self.body_prefix.cf_switch_branches else 0));
    try self.cf_switch_branches.appendSlice(self.allocator, branches);
    return .{ .start = start, .len = @intCast(branches.len) };
}

/// Resolves a switch-branch span to its stored slice.
pub fn getCFSwitchBranches(self: *const Self, span: CFSwitchBranchSpan) StoreSpanBorrow(CFSwitchBranch, "cf_switch_branches") {
    if (self.body_coordinator) |coordinator| {
        if (span.start < self.body_prefix.cf_switch_branches) return coordinator.getCFSwitchBranches(span);
        return self.cf_switch_branches.borrowSpan(span.start - self.body_prefix.cf_switch_branches, span.len);
    }
    return self.cf_switch_branches.borrowSpan(span.start, span.len);
}

/// Resolves a switch-branch span to its stored mutable slice.
pub fn getCFSwitchBranchesMut(self: *Self, span: CFSwitchBranchSpan) StoreSpanBorrowMut(CFSwitchBranch, "cf_switch_branches") {
    if (self.body_coordinator != null and span.start < self.body_prefix.cf_switch_branches) {
        self.assertBodyMetadataImmutable();
    }
    return self.cf_switch_branches.borrowSpanMut(span.start - if (self.body_coordinator != null) self.body_prefix.cf_switch_branches else 0, span.len);
}

/// Appends string-match steps and returns the corresponding flat-storage span.
pub fn addStrMatchSteps(self: *Self, steps: []const StrMatchStep) Allocator.Error!StrMatchStepSpan {
    if (steps.len == 0) return StrMatchStepSpan.empty();

    const start = @as(u32, @intCast(self.str_match_steps.len() + if (self.body_coordinator != null) self.body_prefix.str_match_steps else 0));
    try self.str_match_steps.appendSlice(self.allocator, steps);
    return .{ .start = start, .len = @intCast(steps.len) };
}

/// Resolves a string-match-step span to its stored slice.
pub fn getStrMatchSteps(self: *const Self, span: StrMatchStepSpan) StoreSpanBorrow(StrMatchStep, "str_match_steps") {
    if (self.body_coordinator) |coordinator| {
        if (span.start < self.body_prefix.str_match_steps) return coordinator.getStrMatchSteps(span);
        return self.str_match_steps.borrowSpan(span.start - self.body_prefix.str_match_steps, span.len);
    }
    return self.str_match_steps.borrowSpan(span.start, span.len);
}

/// Appends string-match arms and returns the corresponding flat-storage span.
pub fn addStrMatchArms(self: *Self, arms: []const StrMatchArm) Allocator.Error!StrMatchArmSpan {
    if (arms.len == 0) return StrMatchArmSpan.empty();

    const start = @as(u32, @intCast(self.str_match_arms.len() + if (self.body_coordinator != null) self.body_prefix.str_match_arms else 0));
    try self.str_match_arms.appendSlice(self.allocator, arms);
    return .{ .start = start, .len = @intCast(arms.len) };
}

/// Resolves a string-match-arm span to its stored slice.
pub fn getStrMatchArms(self: *const Self, span: StrMatchArmSpan) StoreSpanBorrow(StrMatchArm, "str_match_arms") {
    if (self.body_coordinator) |coordinator| {
        if (span.start < self.body_prefix.str_match_arms) return coordinator.getStrMatchArms(span);
        return self.str_match_arms.borrowSpan(span.start - self.body_prefix.str_match_arms, span.len);
    }
    return self.str_match_arms.borrowSpan(span.start, span.len);
}

/// Resolves a string-match-arm span to its stored mutable slice.
pub fn getStrMatchArmsMut(self: *Self, span: StrMatchArmSpan) StoreSpanBorrowMut(StrMatchArm, "str_match_arms") {
    if (self.body_coordinator != null and span.start < self.body_prefix.str_match_arms) {
        self.assertBodyMetadataImmutable();
    }
    return self.str_match_arms.borrowSpanMut(span.start - if (self.body_coordinator != null) self.body_prefix.str_match_arms else 0, span.len);
}

/// Appends join-point entries and returns the corresponding flat-storage span.
pub fn addJoinPointSpan(self: *Self, join_points: []const JoinPoint) Allocator.Error!JoinPointSpan {
    if (join_points.len == 0) return JoinPointSpan.empty();

    const start = @as(u32, @intCast(self.join_points.len() + if (self.body_coordinator != null) self.body_prefix.join_points else 0));
    try self.join_points.appendSlice(self.allocator, join_points);
    return .{ .start = start, .len = @intCast(join_points.len) };
}

/// Resolves a join-point span to its stored slice.
pub fn getJoinPointSpan(self: *const Self, span: JoinPointSpan) StoreSpanBorrow(JoinPoint, "join_points") {
    if (self.body_coordinator) |coordinator| {
        if (span.start < self.body_prefix.join_points) return coordinator.getJoinPointSpan(span);
        return self.join_points.borrowSpan(span.start - self.body_prefix.join_points, span.len);
    }
    return self.join_points.borrowSpan(span.start, span.len);
}

/// Resolves a join-point span to its stored mutable slice.
pub fn getJoinPointSpanMut(self: *Self, span: JoinPointSpan) StoreSpanBorrowMut(JoinPoint, "join_points") {
    if (self.body_coordinator != null and span.start < self.body_prefix.join_points) {
        self.assertBodyMetadataImmutable();
    }
    return self.join_points.borrowSpanMut(span.start - if (self.body_coordinator != null) self.body_prefix.join_points else 0, span.len);
}

/// Appends a proc specification and returns its id.
pub fn addProcSpec(self: *Self, proc: LirProcSpec) Allocator.Error!LirProcSpecId {
    self.assertBodyMetadataImmutable();
    const idx = self.proc_specs.len();
    try self.proc_specs.append(self.allocator, proc);
    try self.proc_locs.append(self.allocator, self.current_loc);
    return @enumFromInt(@as(u32, @intCast(idx)));
}

/// Number of stored proc specifications.
pub fn procSpecCount(self: *const Self) usize {
    return self.proc_specs.len() + if (self.body_coordinator != null) self.body_prefix.proc_specs else 0;
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
    const index = @intFromEnum(idx);
    if (self.body_coordinator) |coordinator| return coordinator.getProcSpec(idx);
    return self.proc_specs.get(index);
}

/// Updates the body for a stored proc specification.
pub fn setProcSpecBody(self: *Self, idx: LirProcSpecId, body: ?CFStmtId) void {
    self.assertBodyMetadataImmutable();
    self.proc_specs.getPtrImmediate(@intFromEnum(idx)).body = body;
}

/// Updates the final join-point span for a stored proc specification.
pub fn setProcSpecJoinPoints(self: *Self, idx: LirProcSpecId, join_points: JoinPointSpan) void {
    self.assertBodyMetadataImmutable();
    self.proc_specs.getPtrImmediate(@intFromEnum(idx)).join_points = join_points;
}

/// Updates body and final join points after all fallible/appending work has completed.
pub fn setProcSpecBodyAndJoinPoints(self: *Self, idx: LirProcSpecId, body: ?CFStmtId, join_points: JoinPointSpan) void {
    self.assertBodyMetadataImmutable();
    const proc = self.proc_specs.getPtrImmediate(@intFromEnum(idx));
    proc.body = body;
    proc.join_points = join_points;
}

/// Returns a mutable pointer to the stored proc specification for the given id.
pub fn getProcSpecPtr(self: *Self, idx: LirProcSpecId) *LirProcSpec {
    self.assertBodyMetadataImmutable();
    return self.proc_specs.getPtrImmediate(@intFromEnum(idx));
}

/// Returns all stored proc specifications.
pub fn getProcSpecs(self: *const Self) []const LirProcSpec {
    return self.proc_specs.unsafeRawItemsForView();
}

test "body shard relocates nonzero local and body suffixes" {
    var coordinator = Self.init(std.testing.allocator);
    defer coordinator.deinit();
    const global = try coordinator.addLocal(.{ .layout_idx = .zst });
    _ = try coordinator.addLocalSpan(&.{global});
    const global_pattern = try coordinator.addPattern(.{ .wildcard = .{ .layout_idx = .zst } });
    _ = try coordinator.addPatternSpan(&.{global_pattern});
    const body_name = try coordinator.insertString("body_local");
    const body_inline_scope = try coordinator.addInlineScope(.{
        .source_symbol = Symbol.fromRaw(123),
        .source_name = body_name,
        .source_loc = .{ .file = 1, .line = 2, .column = 3 },
        .call_site = .{ .file = 4, .line = 5, .column = 6 },
        .parent = .none,
    });

    var worker = try coordinator.cloneForBodyShard(std.testing.allocator);
    defer worker.deinit();
    const prefix = worker.captureBodyPrefix();
    try std.testing.expectEqual(@as(u32, 0), prefix.cf_stmts);
    try std.testing.expectEqual(@as(u32, 1), prefix.locals);

    const body_local = try worker.addLocal(.{ .layout_idx = .zst });
    worker.local_names.set(@intFromEnum(body_local) - prefix.locals, @intFromEnum(body_name));
    const frame = try worker.addLocalSpan(&.{ body_local, global });
    const masks = try worker.addU64Span(&.{9});
    const offsets = try worker.addU32Span(&.{ 0, 8 });
    try worker.erased_call_arg_plans.append(worker.allocator, .{
        .offsets = offsets,
        .size = 16,
        .alignment = 8,
    });
    const body_loc: base.SourceLoc = .{ .file = 7, .line = 8, .column = 9 };
    const body_region = base.Region.from_raw_offsets(10, 20);
    worker.current_loc = body_loc;
    worker.current_region = body_region;
    worker.current_inline_scope = body_inline_scope;
    const ret = try worker.addCFStmt(.{ .ret = .{ .value = body_local } });
    const branches = try worker.addCFSwitchBranches(&.{.{ .value = 1, .body = ret }});
    const steps = try worker.addStrMatchSteps(&.{.{
        .capture = .{ .view = body_local },
        .delimiter = .{ .backing = .none, .offset = 0, .len = 0 },
    }});
    const arms = try worker.addStrMatchArms(&.{.{
        .prefix = .{ .backing = .none, .offset = 0, .len = 0 },
        .steps = steps,
        .end = .exact,
        .on_match = ret,
    }});
    const join_points = try worker.addJoinPointSpan(&.{.{
        .id = @enumFromInt(7),
        .params = frame,
        .body = ret,
    }});
    const child_pattern = try worker.addPattern(.{ .wildcard = .{ .layout_idx = .zst } });
    const pattern_args = try worker.addPatternSpan(&.{child_pattern});
    const parent_pattern = try worker.addPattern(.{ .tag = .{
        .discriminant = 1,
        .union_layout = .zst,
        .args = pattern_args,
    } });
    const pattern_ids = try worker.addPatternSpan(&.{ parent_pattern, child_pattern });

    const shard = try worker.captureBodyShard(prefix);
    // Give every destination body space a nonzero base.
    const destination_local = try coordinator.addLocal(.{ .layout_idx = .zst });
    _ = try coordinator.addLocalSpan(&.{destination_local});
    _ = try coordinator.addU64Span(&.{3});
    _ = try coordinator.addU32Span(&.{4});
    const destination_stmt = try coordinator.addCFStmt(.{ .ret = .{ .value = destination_local } });
    _ = try coordinator.addCFSwitchBranches(&.{.{ .value = 0, .body = destination_stmt }});
    _ = try coordinator.addStrMatchSteps(&.{.{
        .capture = .discard,
        .delimiter = .{ .backing = .none, .offset = 0, .len = 0 },
    }});
    _ = try coordinator.addStrMatchArms(&.{.{
        .prefix = .{ .backing = .none, .offset = 0, .len = 0 },
        .steps = .{ .start = 0, .len = 1 },
        .end = .tail,
        .on_match = destination_stmt,
    }});
    _ = try coordinator.addJoinPointSpan(&.{.{ .id = @enumFromInt(99), .params = .empty(), .body = destination_stmt }});
    const destination_pattern = try coordinator.addPattern(.{ .wildcard = .{ .layout_idx = .zst } });
    _ = try coordinator.addPatternSpan(&.{destination_pattern});

    const appended = try coordinator.appendBodyShard(shard, ret, frame);
    try std.testing.expectEqual(@as(u32, 1), @intFromEnum(appended.root.?));
    try std.testing.expectEqual(@as(u32, 2), appended.frame_locals.start);
    const relocated_ret = coordinator.getCFStmt(appended.root.?);
    const relocated_body_local = relocated_ret.ret.value;
    try std.testing.expectEqual(@as(u32, 2), @intFromEnum(relocated_body_local));
    try std.testing.expectEqual(@intFromEnum(body_name), coordinator.getLocalNameRaw(relocated_body_local));
    try std.testing.expectEqualStrings("body_local", coordinator.localName(relocated_body_local).?);
    try std.testing.expectEqual(body_loc, coordinator.stmtLoc(appended.root.?));
    try std.testing.expectEqual(body_region, coordinator.stmtRegion(appended.root.?));
    try std.testing.expectEqual(body_inline_scope, coordinator.stmtInlineScope(appended.root.?));
    const relocated_frame = coordinator.getLocalSpan(appended.frame_locals);
    try std.testing.expectEqual(@as(u32, 2), @intFromEnum(relocated_frame.at(0)));
    try std.testing.expectEqual(global, relocated_frame.at(1));
    const relocated_branch = coordinator.getCFSwitchBranches(.{ .start = appended.relocation.cf_switch_branches, .len = branches.len }).at(0);
    try std.testing.expectEqual(@as(u64, 1), relocated_branch.value);
    try std.testing.expectEqual(appended.root.?, relocated_branch.body);
    const relocated_step = coordinator.getStrMatchSteps(.{ .start = appended.relocation.str_match_steps, .len = steps.len }).at(0);
    try std.testing.expectEqual(@as(u32, 2), @intFromEnum(relocated_step.capture.view));
    const relocated_arm = coordinator.getStrMatchArms(.{ .start = appended.relocation.str_match_arms, .len = arms.len }).at(0);
    try std.testing.expectEqual(appended.relocation.str_match_steps, relocated_arm.steps.start);
    try std.testing.expectEqual(appended.root.?, relocated_arm.on_match);
    const relocated_join = coordinator.getJoinPointSpan(.{ .start = appended.relocation.join_points, .len = join_points.len }).at(0);
    try std.testing.expectEqual(appended.frame_locals, relocated_join.params);
    try std.testing.expectEqual(appended.root.?, relocated_join.body);
    const relocated_plan = coordinator.erased_call_arg_plans.get(appended.relocation.erased_call_arg_plans);
    try std.testing.expectEqual(appended.relocation.u32s, relocated_plan.offsets.start);
    const relocated_offsets = coordinator.getErasedCallArgOffsets(relocated_plan);
    try std.testing.expectEqual(@as(u32, 0), relocated_offsets.at(0));
    try std.testing.expectEqual(@as(u32, 8), relocated_offsets.at(1));
    try std.testing.expectEqual(
        @as(u64, 9),
        coordinator.getU64Span(.{ .start = appended.relocation.u64s, .len = masks.len }).at(0),
    );
    const relocated_pattern_ids = coordinator.getPatternSpan(.{ .start = appended.relocation.pattern_ids + 1, .len = pattern_ids.len });
    try std.testing.expectEqual(@as(u32, 3), @intFromEnum(relocated_pattern_ids.at(0)));
    try std.testing.expectEqual(@as(u32, 2), @intFromEnum(relocated_pattern_ids.at(1)));
    const relocated_parent = coordinator.getPattern(relocated_pattern_ids.at(0)).tag;
    try std.testing.expectEqual(appended.relocation.pattern_ids, relocated_parent.args.start);
    try std.testing.expectEqual(relocated_pattern_ids.at(1), coordinator.getPatternSpan(relocated_parent.args).at(0));
    try std.testing.expectEqual(coordinator.cfStmtCount(), coordinator.cfStmtLocCount());
    try std.testing.expectEqual(coordinator.localCount(), coordinator.local_names.len());
}

test "body shard append preserves destination on every reserve-stage allocation failure" {
    const Helper = struct {
        fn run(fail_index: usize) (Allocator.Error || error{TestExpectedEqual})!bool {
            var source = Self.init(std.testing.allocator);
            defer source.deinit();
            const source_prefix = source.captureBodyPrefix();
            var destination = Self.init(std.testing.allocator);
            defer destination.deinit();

            const locals = [_]Local{.{ .layout_idx = .zst }} ** 9;
            const u64s = [_]u64{7} ** 9;
            const u32s = [_]u32{11} ** 9;
            const steps = [_]StrMatchStep{.{ .capture = .discard, .delimiter = .{ .backing = .none, .offset = 0, .len = 0 } }} ** 9;

            var local_ids: [locals.len]LocalId = undefined;
            for (locals, 0..) |local, index| local_ids[index] = try source.addLocal(local);
            _ = try source.addLocalSpan(&local_ids);
            _ = try source.addU64Span(&u64s);
            const offsets = try source.addU32Span(&u32s);
            for (0..9) |_| try source.erased_call_arg_plans.append(source.allocator, .{ .offsets = offsets, .size = 4, .alignment = 4 });
            var pattern_ids: [9]LirPatternId = undefined;
            for (&pattern_ids) |*pattern_id| {
                pattern_id.* = try source.addPattern(.{ .wildcard = .{ .layout_idx = .zst } });
            }
            _ = try source.addPatternSpan(&pattern_ids);
            var source_stmt: CFStmtId = undefined;
            for (0..9) |index| {
                const stmt = try source.addCFStmt(.{ .ret = .{ .value = local_ids[0] } });
                if (index == 0) source_stmt = stmt;
            }
            _ = try source.addCFSwitchBranches(&([_]CFSwitchBranch{.{ .value = 1, .body = source_stmt }} ** 9));
            _ = try source.addStrMatchSteps(&steps);
            _ = try source.addStrMatchArms(&([_]StrMatchArm{.{
                .prefix = .{ .backing = .none, .offset = 0, .len = 0 },
                .steps = .{ .start = 0, .len = 1 },
                .end = .exact,
                .on_match = source_stmt,
            }} ** 9));
            _ = try source.addJoinPointSpan(&([_]JoinPoint{.{
                .id = @enumFromInt(1),
                .params = .empty(),
                .body = source_stmt,
            }} ** 9));

            const destination_local = try destination.addLocal(.{ .layout_idx = .zst });
            _ = try destination.addLocalSpan(&.{destination_local});
            _ = try destination.addU64Span(&.{3});
            _ = try destination.addU32Span(&.{5});
            try destination.erased_call_arg_plans.append(destination.allocator, .{ .offsets = .{ .start = 0, .len = 1 }, .size = 4, .alignment = 4 });
            const destination_pattern = try destination.addPattern(.{ .wildcard = .{ .layout_idx = .zst } });
            _ = try destination.addPatternSpan(&.{destination_pattern});
            const destination_stmt = try destination.addCFStmt(.{ .ret = .{ .value = destination_local } });
            _ = try destination.addCFSwitchBranches(&.{.{ .value = 2, .body = destination_stmt }});
            _ = try destination.addStrMatchSteps(&.{.{ .capture = .discard, .delimiter = .{ .backing = .none, .offset = 0, .len = 0 } }});
            _ = try destination.addStrMatchArms(&.{.{
                .prefix = .{ .backing = .none, .offset = 0, .len = 0 },
                .steps = .{ .start = 0, .len = 1 },
                .end = .exact,
                .on_match = destination_stmt,
            }});
            _ = try destination.addJoinPointSpan(&.{.{
                .id = @enumFromInt(2),
                .params = .empty(),
                .body = destination_stmt,
            }});

            const shard = source.captureBodyShard(source_prefix) catch unreachable;
            var failing_allocator = std.testing.FailingAllocator.init(std.testing.allocator, .{
                .fail_index = fail_index,
            });
            destination.allocator = failing_allocator.allocator();
            defer destination.allocator = std.testing.allocator;
            _ = destination.appendBodyShard(shard, null, .empty()) catch |err| {
                try std.testing.expectEqual(error.OutOfMemory, err);
                try std.testing.expectEqual(@as(usize, 1), destination.cf_stmts.len());
                try std.testing.expectEqual(@as(usize, 1), destination.cf_stmt_locs.len());
                try std.testing.expectEqual(@as(usize, 1), destination.cf_stmt_regions.len());
                try std.testing.expectEqual(@as(usize, 1), destination.cf_stmt_inline_scopes.len());
                try std.testing.expectEqual(@as(usize, 1), destination.cf_switch_branches.len());
                try std.testing.expectEqual(@as(usize, 1), destination.str_match_steps.len());
                try std.testing.expectEqual(@as(usize, 1), destination.str_match_arms.len());
                try std.testing.expectEqual(@as(usize, 1), destination.join_points.len());
                try std.testing.expectEqual(@as(usize, 1), destination.locals.len());
                try std.testing.expectEqual(@as(usize, 1), destination.local_names.len());
                try std.testing.expectEqual(@as(usize, 1), destination.local_ids.len());
                try std.testing.expectEqual(@as(usize, 1), destination.u64s.len());
                try std.testing.expectEqual(@as(usize, 1), destination.u32s.len());
                try std.testing.expectEqual(@as(usize, 1), destination.erased_call_arg_plans.len());
                try std.testing.expectEqual(@as(usize, 1), destination.patterns.len());
                try std.testing.expectEqual(@as(usize, 1), destination.pattern_ids.len());
                try std.testing.expectEqual(@as(u64, 2), destination.cf_switch_branches.get(0).value);
                try std.testing.expectEqual(@as(u64, 3), destination.u64s.get(0));
                try std.testing.expectEqual(@as(u32, 5), destination.u32s.get(0));
                try std.testing.expectEqual(destination_local, destination.cf_stmts.get(0).ret.value);
                return true;
            };
            return false;
        }
    };
    var fail_index: usize = 0;
    while (try Helper.run(fail_index)) : (fail_index += 1) {}
    try std.testing.expectEqual(@as(usize, 8), fail_index);
}

test "body shard reads coordinator prefix without copying it" {
    var coordinator = Self.init(std.testing.allocator);
    defer coordinator.deinit();

    const global = try coordinator.addLocal(.{ .layout_idx = .zst });
    try coordinator.setLocalName(global, "global");
    const global_span = try coordinator.addLocalSpan(&.{global});
    const global_stmt = try coordinator.addCFStmt(.{ .ret = .{ .value = global } });
    const global_offsets = try coordinator.addU32Span(&.{4});
    const global_plan: ErasedCallArgsPlanId = @enumFromInt(@as(u32, @intCast(coordinator.erased_call_arg_plans.len())));
    try coordinator.erased_call_arg_plans.append(coordinator.allocator, .{
        .offsets = global_offsets,
        .size = 4,
        .alignment = 4,
    });

    var worker = try coordinator.cloneForBodyShard(std.testing.allocator);
    defer worker.deinit();

    try std.testing.expectEqual(@as(usize, 0), worker.locals.len());
    try std.testing.expectEqual(@as(usize, 0), worker.local_ids.len());
    try std.testing.expectEqual(@as(usize, 0), worker.cf_stmts.len());
    try std.testing.expectEqual(global, worker.getLocalSpan(global_span).at(0));
    try std.testing.expectEqual(global, worker.getCFStmt(global_stmt).ret.value);
    try std.testing.expectEqualStrings("global", worker.localName(global).?);
    try std.testing.expectEqual(coordinator.getLocalNameRaw(global), worker.getLocalNameRaw(global));
    try std.testing.expectEqual(@as(u32, 4), worker.getErasedCallArgOffsets(worker.getErasedCallArgsPlan(global_plan)).at(0));

    const suffix_local = try worker.addLocal(.{ .layout_idx = .zst });
    const suffix_span = try worker.addLocalSpan(&.{suffix_local});
    const suffix_stmt = try worker.addCFStmt(.{ .ret = .{ .value = suffix_local } });
    try std.testing.expectEqual(@as(u32, 1), @intFromEnum(suffix_local));
    try std.testing.expectEqual(@as(u32, 1), suffix_span.start);
    try std.testing.expectEqual(@as(u32, 1), @intFromEnum(suffix_stmt));
    try std.testing.expectEqual(suffix_local, worker.getLocalSpan(suffix_span).at(0));
    try std.testing.expectEqual(suffix_local, worker.getCFStmt(suffix_stmt).ret.value);
}

/// Returns one stored proc debug-name entry.
pub fn getProcDebugName(self: *const Self, index: usize) ProcDebugName {
    return self.proc_debug_names.get(index);
}

/// Returns the raw local-name table entry for the given local id.
pub fn getLocalNameRaw(self: *const Self, id: LocalId) u32 {
    const index = @intFromEnum(id);
    if (self.body_coordinator) |coordinator| {
        if (index < self.body_prefix.locals) return coordinator.getLocalNameRaw(id);
        return self.local_names.get(index - self.body_prefix.locals);
    }
    return self.local_names.get(index);
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
    std.debug.assert(self.cf_stmts.len() == self.cf_stmt_inline_scopes.len());

    var write: usize = 0;
    const cf_stmts = self.cf_stmts.unsafeRawItemsMutForStore();
    const cf_stmt_locs = self.cf_stmt_locs.unsafeRawItemsMutForStore();
    const cf_stmt_regions = self.cf_stmt_regions.unsafeRawItemsMutForStore();
    const cf_stmt_inline_scopes = self.cf_stmt_inline_scopes.unsafeRawItemsMutForStore();
    for (cf_stmts, cf_stmt_locs, cf_stmt_regions, cf_stmt_inline_scopes, 0..) |stmt, loc, region, inline_scope, index| {
        if (!reachable[index]) continue;
        cf_stmts[write] = stmt;
        cf_stmt_locs[write] = loc;
        cf_stmt_regions[write] = region;
        cf_stmt_inline_scopes[write] = inline_scope;
        write += 1;
    }
    self.cf_stmts.shrinkRetainingCapacity(write);
    self.cf_stmt_locs.shrinkRetainingCapacity(write);
    self.cf_stmt_regions.shrinkRetainingCapacity(write);
    self.cf_stmt_inline_scopes.shrinkRetainingCapacity(write);
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

test "source file table stores display and package-qualified names per entry" {
    const gpa = std.testing.allocator;
    var store = Self.init(gpa);
    defer store.deinit();

    // Two modules with the SAME bare name from different packages must stay
    // distinguishable through their qualified names: the provenance
    // comparison in compile-time failure reporting matches by qualified
    // identity, never by display name.
    try store.setSourceFiles(&.{
        .{ .name = "Cfg", .qualified_name = "app.Cfg" },
        .{ .name = "Cfg", .qualified_name = "pf.Cfg" },
        .{ .name = "Utils", .qualified_name = "app.Utils" },
    });

    try std.testing.expectEqual(@as(u32, 3), store.sourceFileCount());
    try std.testing.expectEqualStrings("Cfg", store.sourceFileName(0));
    try std.testing.expectEqualStrings("Cfg", store.sourceFileName(1));
    try std.testing.expectEqualStrings("Utils", store.sourceFileName(2));
    try std.testing.expectEqualStrings("app.Cfg", store.sourceFileQualifiedName(0));
    try std.testing.expectEqualStrings("pf.Cfg", store.sourceFileQualifiedName(1));
    try std.testing.expectEqualStrings("app.Utils", store.sourceFileQualifiedName(2));
}
