//! Flat storage for statement-only, local-centric LIR.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");

const ir = @import("LIR.zig");

const Allocator = std.mem.Allocator;

const CFStmt = ir.CFStmt;
const CFStmtId = ir.CFStmtId;
const CFSwitchBranch = ir.CFSwitchBranch;
const CFSwitchBranchSpan = ir.CFSwitchBranchSpan;
const LirProcSpec = ir.LirProcSpec;
const LirProcSpecId = ir.LirProcSpecId;
const Local = ir.Local;
const LocalId = ir.LocalId;
const LocalSpan = ir.LocalSpan;
const RefProjection = ir.RefProjection;
const RefProjectionSpan = ir.RefProjectionSpan;
const Symbol = ir.Symbol;

const Self = @This();

cf_stmts: std.ArrayList(CFStmt),
cf_switch_branches: std.ArrayList(CFSwitchBranch),
locals: std.ArrayList(Local),
local_ids: std.ArrayList(LocalId),
ref_projections: std.ArrayList(RefProjection),
proc_specs: std.ArrayList(LirProcSpec),
strings: base.StringLiteral.Store,
allocator: Allocator,
next_synthetic_symbol: u64,

/// Initializes empty storage for statement-only LIR.
pub fn init(allocator: Allocator) Self {
    return .{
        .cf_stmts = std.ArrayList(CFStmt).empty,
        .cf_switch_branches = std.ArrayList(CFSwitchBranch).empty,
        .locals = std.ArrayList(Local).empty,
        .local_ids = std.ArrayList(LocalId).empty,
        .ref_projections = std.ArrayList(RefProjection).empty,
        .proc_specs = std.ArrayList(LirProcSpec).empty,
        .strings = base.StringLiteral.Store{},
        .allocator = allocator,
        .next_synthetic_symbol = 0xf000_0000_0000_0000,
    };
}

/// Releases all storage owned by this LIR store.
pub fn deinit(self: *Self) void {
    self.cf_stmts.deinit(self.allocator);
    self.cf_switch_branches.deinit(self.allocator);
    self.locals.deinit(self.allocator);
    self.local_ids.deinit(self.allocator);
    self.ref_projections.deinit(self.allocator);
    self.proc_specs.deinit(self.allocator);
    self.strings.deinit(self.allocator);
}

/// Returns a fresh synthetic symbol for compiler-generated locals and procs.
pub fn freshSyntheticSymbol(self: *Self) Symbol {
    const symbol = Symbol.fromRaw(self.next_synthetic_symbol);
    self.next_synthetic_symbol += 1;
    return symbol;
}

/// Interns a string literal in the store-level string table.
pub fn insertString(self: *Self, text: []const u8) Allocator.Error!base.StringLiteral.Idx {
    return self.strings.insert(self.allocator, text);
}

/// Returns the text for an interned string literal.
pub fn getString(self: *const Self, idx: base.StringLiteral.Idx) []const u8 {
    return self.strings.get(idx);
}

/// Registers one LIR local and returns its id.
pub fn addLocal(self: *Self, local: Local) Allocator.Error!LocalId {
    const idx = self.locals.items.len;
    try self.locals.append(self.allocator, local);
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

/// Appends ref projections and returns the corresponding flat-storage span.
pub fn addRefProjectionSpan(self: *Self, projections: []const RefProjection) Allocator.Error!RefProjectionSpan {
    if (projections.len == 0) return RefProjectionSpan.empty();

    const start = @as(u32, @intCast(self.ref_projections.items.len));
    try self.ref_projections.appendSlice(self.allocator, projections);
    return .{ .start = start, .len = @intCast(projections.len) };
}

/// Resolves a ref-projection span to its stored slice.
pub fn getRefProjectionSpan(self: *const Self, span: RefProjectionSpan) []const RefProjection {
    if (span.len == 0) return &.{};
    if (builtin.mode == .Debug) {
        const end = @as(u64, span.start) + @as(u64, span.len);
        if (end > self.ref_projections.items.len) {
            std.debug.panic(
                "LirStore invariant violated: ref-projection span start={d} len={d} exceeds ref-projection storage len={d}",
                .{ span.start, span.len, self.ref_projections.items.len },
            );
        }
    }
    return self.ref_projections.items[span.start..][0..span.len];
}

/// Appends a statement/control-flow node and returns its id.
pub fn addCFStmt(self: *Self, stmt: CFStmt) Allocator.Error!CFStmtId {
    const idx = self.cf_stmts.items.len;
    try self.cf_stmts.append(self.allocator, stmt);
    return @enumFromInt(@as(u32, @intCast(idx)));
}

/// Returns the stored statement for the given id.
pub fn getCFStmt(self: *const Self, id: CFStmtId) CFStmt {
    return self.cf_stmts.items[@intFromEnum(id)];
}

/// Returns a mutable pointer to the stored statement for the given id.
pub fn getCFStmtPtr(self: *Self, id: CFStmtId) *CFStmt {
    return &self.cf_stmts.items[@intFromEnum(id)];
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

/// Appends a proc specification and returns its id.
pub fn addProcSpec(self: *Self, proc: LirProcSpec) Allocator.Error!LirProcSpecId {
    const idx = self.proc_specs.items.len;
    try self.proc_specs.append(self.allocator, proc);
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
