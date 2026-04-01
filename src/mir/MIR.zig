//! Monomorphic Intermediate Representation (MIR)
//!
//! MIR sits between CIR (Canonical IR, per-module, polymorphic) and LIR
//! (layout- and backend-oriented). This strongest-form MIR is:
//!
//! - **Monomorphic**: every local, lambda, and top-level def has a concrete
//!   monotype
//! - **Statement-only**: nested value expressions are gone; computation is
//!   explicit local-defining statements
//! - **Lambda-aware**: lambdas and closures are still explicit MIR concepts
//! - **Pre-layout**: MIR knows monotypes, not layouts
//! - **Local-centric**: executable flow uses compact local ids; global symbols
//!   only appear when materialized into locals
//!
//! Control flow is explicit with statements, joins, jumps, returns, crashes,
//! and borrow scopes. `if` is expected to be lowered to explicit switch control
//! flow before values reach strongest-form MIR. Destructuring is expected to be
//! lowered into explicit local-defining statements instead of remaining as MIR
//! patterns.

const std = @import("std");
const base = @import("base");
const builtins = @import("builtins");
const Monotype = @import("corecir").Monotype;

const Ident = base.Ident;
const StringLiteral = base.StringLiteral;
const Region = base.Region;
const Allocator = std.mem.Allocator;

const CIR = @import("can").CIR;

/// Global identifier (opaque 64-bit id).
pub const Symbol = packed struct(u64) {
    id: u64,

    comptime {
        std.debug.assert(@sizeOf(Symbol) == @sizeOf(u64));
        std.debug.assert(@alignOf(Symbol) == @alignOf(u64));
    }

    /// Builds one symbol from its raw 64-bit identity.
    pub fn fromRaw(id: u64) Symbol {
        return .{ .id = id };
    }

    /// Returns the raw 64-bit identity of this symbol.
    pub fn raw(self: Symbol) u64 {
        return self.id;
    }

    /// Reports whether two symbols are identical.
    pub fn eql(a: Symbol, b: Symbol) bool {
        return a.id == b.id;
    }

    /// Hashes this symbol by its raw identity.
    pub fn hash(self: Symbol) u64 {
        return self.id;
    }

    pub const none: Symbol = .{ .id = std.math.maxInt(u64) };

    /// Reports whether this is the distinguished sentinel symbol.
    pub fn isNone(self: Symbol) bool {
        return self.id == none.id;
    }
};

/// Index into Store.locals.
pub const LocalId = enum(u32) {
    _,
};

/// Index into Store.cf_stmts.
pub const CFStmtId = enum(u32) {
    _,
};

/// Index into Store.lambdas.
pub const LambdaId = enum(u32) {
    _,
};

/// Index into Store.const_defs.
pub const ConstDefId = enum(u32) {
    _,
};

/// Join point identity for explicit control-flow merges.
pub const JoinPointId = enum(u32) {
    _,
};

/// Borrow-scope identity for explicit lexical borrow regions.
pub const BorrowScopeId = enum(u32) {
    _,
};

/// One explicitly typed MIR local.
pub const ExactCallable = struct {
    lambda: LambdaId,
    requires_hidden_capture: bool,
};

/// One explicitly typed MIR local.
pub const Local = struct {
    monotype: Monotype.Idx,
    reassignable: bool = false,
};

/// One canonical definition for a MIR local.
pub const LocalDefKind = union(enum) {
    param: struct {
        lambda: LambdaId,
        index: u16,
    },
    captures_param: struct {
        lambda: LambdaId,
    },
    join_param: struct {
        join: JoinPointId,
        index: u16,
    },
    symbol: Symbol,
    ref: RefOp,
    literal: LiteralValue,
    lambda: LambdaId,
    closure: struct {
        lambda: LambdaId,
        captures: LocalSpan,
    },
    call: struct {
        callee: LocalId,
        exact_lambda: LambdaId,
        exact_requires_hidden_capture: bool,
        args: LocalSpan,
    },
    low_level: struct {
        op: CIR.Expr.LowLevel,
        args: LocalSpan,
    },
    list: LocalSpan,
    struct_: LocalSpan,
    tag: struct {
        name: Monotype.Name,
        args: LocalSpan,
    },
};

/// Canonical semantic record for one MIR local.
pub const LocalDef = struct {
    kind: ?LocalDefKind = null,
    exact_callable: ?ExactCallable = null,
};

/// Span of LocalId values stored in Store.local_ids.
pub const LocalSpan = extern struct {
    start: u32,
    len: u16,

    /// Returns an empty local span.
    pub fn empty() LocalSpan {
        return .{ .start = 0, .len = 0 };
    }

    /// Reports whether this span is empty.
    pub fn isEmpty(self: LocalSpan) bool {
        return self.len == 0;
    }
};

/// One reference-producing MIR operation.
pub const ProjectionOwnership = enum {
    borrow,
    move,
};

/// One reference-producing MIR operation.
pub const RefOp = union(enum) {
    local: LocalId,
    discriminant: struct {
        source: LocalId,
    },
    field: struct {
        source: LocalId,
        field_idx: u32,
        ownership: ProjectionOwnership = .borrow,
    },
    tag_payload: struct {
        source: LocalId,
        payload_idx: u32,
        tag_discriminant: u32,
        ownership: ProjectionOwnership = .borrow,
    },
    nominal: struct {
        backing: LocalId,
    },
};

/// One switch branch keyed by an explicit integer test value.
pub const SwitchBranch = struct {
    value: u64,
    body: CFStmtId,
};

/// Span of SwitchBranch values stored in Store.switch_branches.
pub const SwitchBranchSpan = extern struct {
    start: u32,
    len: u16,

    /// Returns an empty switch-branch span.
    pub fn empty() SwitchBranchSpan {
        return .{ .start = 0, .len = 0 };
    }

    /// Reports whether this span is empty.
    pub fn isEmpty(self: SwitchBranchSpan) bool {
        return self.len == 0;
    }
};

/// Whether a lambda is recursive and whether that recursion is tail-recursive.
pub const LambdaRecursion = enum {
    not_recursive,
    recursive,
    tail_recursive,
};

/// Hosted lambda metadata used to link lambda-backed calls to platform implementations.
pub const HostedLambda = struct {
    symbol_name: Ident.Idx,
    index: u32,
};

/// One lowered MIR lambda body.
pub const Lambda = struct {
    fn_monotype: Monotype.Idx,
    params: LocalSpan,
    body: CFStmtId,
    ret_monotype: Monotype.Idx,
    debug_name: Symbol,
    source_region: Region,
    captures_param: ?LocalId = null,
    recursion: LambdaRecursion,
    hosted: ?HostedLambda = null,
};

/// One top-level constant definition.
pub const ConstDef = struct {
    symbol: Symbol,
    body: CFStmtId,
    monotype: Monotype.Idx,
    source_region: Region,
};

/// One MIR literal value.
pub const LiteralValue = union(enum) {
    int: CIR.IntValue,
    frac_f32: f32,
    frac_f64: f64,
    dec: builtins.dec.RocDec,
    str: StringLiteral.Idx,
};

/// Runtime-error reasons that remain explicit in MIR.
pub const RuntimeError = union(enum) {
    can_diagnostic: CIR.Diagnostic.Idx,
    type_error,
    ellipsis,
    anno_only,
};

/// Single canonical statement/control-flow language for strongest-form MIR.
pub const CFStmt = union(enum) {
    assign_symbol: struct {
        target: LocalId,
        symbol: Symbol,
        next: CFStmtId,
    },
    assign_ref: struct {
        target: LocalId,
        op: RefOp,
        next: CFStmtId,
    },
    assign_literal: struct {
        target: LocalId,
        literal: LiteralValue,
        next: CFStmtId,
    },
    assign_lambda: struct {
        target: LocalId,
        lambda: LambdaId,
        next: CFStmtId,
    },
    assign_closure: struct {
        target: LocalId,
        lambda: LambdaId,
        captures: LocalSpan,
        next: CFStmtId,
    },
    assign_call: struct {
        target: LocalId,
        callee: LocalId,
        exact_lambda: LambdaId,
        exact_requires_hidden_capture: bool,
        args: LocalSpan,
        next: CFStmtId,
    },
    assign_low_level: struct {
        target: LocalId,
        op: CIR.Expr.LowLevel,
        args: LocalSpan,
        next: CFStmtId,
    },
    assign_list: struct {
        target: LocalId,
        elems: LocalSpan,
        next: CFStmtId,
    },
    assign_struct: struct {
        target: LocalId,
        fields: LocalSpan,
        next: CFStmtId,
    },
    assign_tag: struct {
        target: LocalId,
        name: Monotype.Name,
        args: LocalSpan,
        next: CFStmtId,
    },
    debug: struct {
        value: LocalId,
        next: CFStmtId,
    },
    expect: struct {
        condition: LocalId,
        next: CFStmtId,
    },
    runtime_error: RuntimeError,
    switch_stmt: struct {
        scrutinee: LocalId,
        branches: SwitchBranchSpan,
        default_branch: CFStmtId,
    },
    borrow_scope: struct {
        id: BorrowScopeId,
        body: CFStmtId,
        remainder: CFStmtId,
    },
    scope_exit: struct {
        id: BorrowScopeId,
    },
    join: struct {
        id: JoinPointId,
        params: LocalSpan,
        body: CFStmtId,
        remainder: CFStmtId,
    },
    jump: struct {
        id: JoinPointId,
        args: LocalSpan,
    },
    ret: struct {
        value: LocalId,
    },
    crash: StringLiteral.Idx,
};

/// Flat storage for strongest-form MIR.
pub const Store = struct {
    const EntryState = enum {
        reserved,
        finalized,
    };

    cf_stmts: std.ArrayListUnmanaged(CFStmt),
    cf_stmt_states: std.ArrayListUnmanaged(EntryState),
    switch_branches: std.ArrayListUnmanaged(SwitchBranch),
    locals: std.ArrayListUnmanaged(Local),
    local_defs: std.ArrayListUnmanaged(LocalDef),
    local_ids: std.ArrayListUnmanaged(LocalId),
    lambdas: std.ArrayListUnmanaged(Lambda),
    lambda_states: std.ArrayListUnmanaged(EntryState),
    const_defs: std.ArrayListUnmanaged(ConstDef),
    monotype_store: Monotype.Store,
    const_defs_by_symbol: std.AutoHashMapUnmanaged(u64, ConstDefId),
    strings: StringLiteral.Store,

    /// Initializes an empty MIR store.
    pub fn init(allocator: Allocator) Allocator.Error!Store {
        return .{
            .cf_stmts = .empty,
            .cf_stmt_states = .empty,
            .switch_branches = .empty,
            .locals = .empty,
            .local_defs = .empty,
            .local_ids = .empty,
            .lambdas = .empty,
            .lambda_states = .empty,
            .const_defs = .empty,
            .monotype_store = try Monotype.Store.init(allocator),
            .const_defs_by_symbol = .empty,
            .strings = .{},
        };
    }

    /// Releases all storage owned by this MIR store.
    pub fn deinit(self: *Store, allocator: Allocator) void {
        self.cf_stmts.deinit(allocator);
        self.cf_stmt_states.deinit(allocator);
        self.switch_branches.deinit(allocator);
        self.locals.deinit(allocator);
        self.local_defs.deinit(allocator);
        self.local_ids.deinit(allocator);
        self.lambdas.deinit(allocator);
        self.lambda_states.deinit(allocator);
        self.const_defs.deinit(allocator);
        self.monotype_store.deinit(allocator);
        self.const_defs_by_symbol.deinit(allocator);
        self.strings.deinit(allocator);
    }

    /// Interns a string literal in the store-level string table.
    pub fn insertString(self: *Store, allocator: Allocator, text: []const u8) Allocator.Error!StringLiteral.Idx {
        return self.strings.insert(allocator, text);
    }

    /// Returns the text for an interned string literal.
    pub fn getString(self: *const Store, idx: StringLiteral.Idx) []const u8 {
        return self.strings.get(idx);
    }

    /// Registers one MIR local and returns its id.
    pub fn addLocal(self: *Store, allocator: Allocator, local: Local) Allocator.Error!LocalId {
        const idx: u32 = @intCast(self.locals.items.len);
        try self.locals.append(allocator, local);
        try self.local_defs.append(allocator, .{});
        return @enumFromInt(idx);
    }

    /// Returns one MIR local by id.
    pub fn getLocal(self: *const Store, id: LocalId) Local {
        return self.locals.items[@intFromEnum(id)];
    }

    /// Returns a mutable pointer to one MIR local.
    pub fn getLocalPtr(self: *Store, id: LocalId) *Local {
        return &self.locals.items[@intFromEnum(id)];
    }

    /// Returns the canonical definition for one MIR local.
    pub fn getLocalDef(self: *const Store, id: LocalId) LocalDefKind {
        return self.local_defs.items[@intFromEnum(id)].kind orelse std.debug.panic(
            "MIR local {d} has no recorded local definition",
            .{@intFromEnum(id)},
        );
    }

    /// Returns the canonical definition for one MIR local if it was recorded.
    pub fn getLocalDefOpt(self: *const Store, id: LocalId) ?LocalDefKind {
        return self.local_defs.items[@intFromEnum(id)].kind;
    }

    pub fn getLocalDefRecord(self: *const Store, id: LocalId) LocalDef {
        return self.local_defs.items[@intFromEnum(id)];
    }

    pub fn getLocalDefRecordPtr(self: *Store, id: LocalId) *LocalDef {
        return &self.local_defs.items[@intFromEnum(id)];
    }

    /// Stores a local-id slice and returns its span.
    pub fn addLocalSpan(self: *Store, allocator: Allocator, ids: []const LocalId) Allocator.Error!LocalSpan {
        if (ids.len == 0) return LocalSpan.empty();
        const start: u32 = @intCast(self.local_ids.items.len);
        try self.local_ids.appendSlice(allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    /// Resolves one stored local-id span.
    pub fn getLocalSpan(self: *const Store, span: LocalSpan) []const LocalId {
        if (span.len == 0) return &.{};
        return self.local_ids.items[span.start..][0..span.len];
    }

    pub fn reserveCFStmt(self: *Store, allocator: Allocator) Allocator.Error!CFStmtId {
        const idx: u32 = @intCast(self.cf_stmts.items.len);
        try self.cf_stmts.append(allocator, .{ .runtime_error = .type_error });
        try self.cf_stmt_states.append(allocator, .reserved);
        return @enumFromInt(idx);
    }

    pub fn finalizeCFStmt(self: *Store, id: CFStmtId, stmt: CFStmt) Allocator.Error!void {
        const idx = @intFromEnum(id);
        const state = &self.cf_stmt_states.items[idx];
        if (state.* != .reserved) {
            std.debug.panic(
                "MIR invariant violated: finalizeCFStmt expected reserved stmt {d}, found state {s}",
                .{ idx, @tagName(state.*) },
            );
        }
        self.cf_stmts.items[idx] = stmt;
        state.* = .finalized;
        try self.recordCFStmtLocalDefs(id, stmt);
    }

    /// Appends one finalized MIR statement and returns its id.
    pub fn addCFStmt(self: *Store, allocator: Allocator, stmt: CFStmt) Allocator.Error!CFStmtId {
        const id = try self.reserveCFStmt(allocator);
        try self.finalizeCFStmt(id, stmt);
        return id;
    }

    /// Returns one MIR statement by id.
    pub fn getCFStmt(self: *const Store, id: CFStmtId) CFStmt {
        self.assertCFStmtFinalized(id);
        return self.cf_stmts.items[@intFromEnum(id)];
    }

    /// Stores switch branches and returns their span.
    pub fn addSwitchBranches(self: *Store, allocator: Allocator, branches: []const SwitchBranch) Allocator.Error!SwitchBranchSpan {
        if (branches.len == 0) return SwitchBranchSpan.empty();
        const start: u32 = @intCast(self.switch_branches.items.len);
        try self.switch_branches.appendSlice(allocator, branches);
        return .{ .start = start, .len = @intCast(branches.len) };
    }

    /// Resolves one stored switch-branch span.
    pub fn getSwitchBranches(self: *const Store, span: SwitchBranchSpan) []const SwitchBranch {
        if (span.len == 0) return &.{};
        return self.switch_branches.items[span.start..][0..span.len];
    }

    pub fn reserveLambda(self: *Store, allocator: Allocator) Allocator.Error!LambdaId {
        const idx: u32 = @intCast(self.lambdas.items.len);
        try self.lambdas.append(allocator, .{
            .fn_monotype = .none,
            .params = .empty(),
            .body = @enumFromInt(0),
            .ret_monotype = .none,
            .debug_name = .none,
            .source_region = .zero(),
            .captures_param = null,
            .recursion = .not_recursive,
            .hosted = null,
        });
        try self.lambda_states.append(allocator, .reserved);
        return @enumFromInt(idx);
    }

    pub fn finalizeLambda(self: *Store, id: LambdaId, lambda: Lambda) Allocator.Error!void {
        const idx = @intFromEnum(id);
        const state = &self.lambda_states.items[idx];
        if (state.* != .reserved) {
            std.debug.panic(
                "MIR invariant violated: finalizeLambda expected reserved lambda {d}, found state {s}",
                .{ idx, @tagName(state.*) },
            );
        }
        self.lambdas.items[idx] = lambda;
        state.* = .finalized;
        try self.recordLambdaLocalDefs(id, lambda);
    }

    /// Registers one finalized MIR lambda and returns its id.
    pub fn addLambda(self: *Store, allocator: Allocator, lambda: Lambda) Allocator.Error!LambdaId {
        const id = try self.reserveLambda(allocator);
        try self.finalizeLambda(id, lambda);
        return id;
    }

    /// Returns one MIR lambda by id.
    pub fn getLambda(self: *const Store, id: LambdaId) Lambda {
        self.assertLambdaFinalized(id);
        return self.lambdas.items[@intFromEnum(id)];
    }

    /// Returns one MIR lambda by id regardless of whether it is still reserved.
    /// Lowering uses this for reserved callable prototypes before body finalization.
    pub fn getLambdaAnyState(self: *const Store, id: LambdaId) Lambda {
        return self.lambdas.items[@intFromEnum(id)];
    }

    /// Installs a reserved lambda prototype before final body/callable-bindings finalization.
    pub fn installReservedLambdaPrototype(self: *Store, id: LambdaId, lambda: Lambda) Allocator.Error!void {
        const idx = @intFromEnum(id);
        const state = self.lambda_states.items[idx];
        if (state != .reserved) {
            std.debug.panic(
                "MIR invariant violated: installReservedLambdaPrototype expected reserved lambda {d}, found state {s}",
                .{ idx, @tagName(state) },
            );
        }
        self.lambdas.items[idx] = lambda;
        try self.recordLambdaLocalDefs(id, lambda);
    }

    /// Returns all stored MIR lambdas.
    pub fn getLambdas(self: *const Store) []const Lambda {
        return self.lambdas.items;
    }

    /// Registers one top-level constant definition.
    pub fn addConstDef(self: *Store, allocator: Allocator, def: ConstDef) Allocator.Error!ConstDefId {
        const idx: u32 = @intCast(self.const_defs.items.len);
        try self.const_defs.append(allocator, def);
        return @enumFromInt(idx);
    }

    /// Returns one top-level constant definition.
    pub fn getConstDef(self: *const Store, id: ConstDefId) ConstDef {
        return self.const_defs.items[@intFromEnum(id)];
    }

    /// Returns all stored top-level constant definitions.
    pub fn getConstDefs(self: *const Store) []const ConstDef {
        return self.const_defs.items;
    }

    /// Registers one constant definition under its global symbol.
    pub fn registerConstDef(self: *Store, allocator: Allocator, def: ConstDef) Allocator.Error!ConstDefId {
        const key = def.symbol.raw();
        const gop = try self.const_defs_by_symbol.getOrPut(allocator, key);
        if (gop.found_existing) {
            std.debug.panic(
                "MIR duplicate constant definition for symbol key {d}",
                .{key},
            );
        }

        const id = try self.addConstDef(allocator, def);
        gop.value_ptr.* = id;
        return id;
    }

    /// Resolves one registered constant definition by symbol.
    pub fn getConstDefForSymbol(self: *const Store, symbol: Symbol) ?ConstDefId {
        return self.const_defs_by_symbol.get(symbol.raw());
    }

    pub fn setLocalExactCallable(self: *Store, local: LocalId, exact_callable: ?ExactCallable) void {
        self.local_defs.items[@intFromEnum(local)].exact_callable = exact_callable;
    }

    pub fn getLocalExactCallable(self: *const Store, local: LocalId) ?ExactCallable {
        return self.local_defs.items[@intFromEnum(local)].exact_callable;
    }

    fn recordLocalDef(self: *Store, local: LocalId, def: LocalDefKind) Allocator.Error!void {
        const slot = &self.local_defs.items[@intFromEnum(local)];
        if (slot.kind) |existing| {
            if (std.meta.eql(existing, def)) return;
            std.debug.panic(
                "MIR local {d} received multiple recorded definitions; existing={s} new={s}",
                .{
                    @intFromEnum(local),
                    @tagName(existing),
                    @tagName(def),
                },
            );
        }
        slot.kind = def;
    }

    fn assertCFStmtFinalized(self: *const Store, id: CFStmtId) void {
        const state = self.cf_stmt_states.items[@intFromEnum(id)];
        if (state != .finalized) {
            std.debug.panic(
                "MIR invariant violated: accessed reserved CF stmt {d}",
                .{@intFromEnum(id)},
            );
        }
    }

    fn assertLambdaFinalized(self: *const Store, id: LambdaId) void {
        const state = self.lambda_states.items[@intFromEnum(id)];
        if (state != .finalized) {
            std.debug.panic(
                "MIR invariant violated: accessed reserved lambda {d}",
                .{@intFromEnum(id)},
            );
        }
    }

    fn recordCFStmtLocalDefs(self: *Store, stmt_id: CFStmtId, stmt: CFStmt) Allocator.Error!void {
        switch (stmt) {
            .assign_symbol => |assign| try self.recordLocalDef(assign.target, .{ .symbol = assign.symbol }),
            .assign_ref => |assign| try self.recordLocalDef(assign.target, .{ .ref = assign.op }),
            .assign_literal => |assign| try self.recordLocalDef(assign.target, .{ .literal = assign.literal }),
            .assign_lambda => |assign| try self.recordLocalDef(assign.target, .{ .lambda = assign.lambda }),
            .assign_closure => |assign| try self.recordLocalDef(assign.target, .{ .closure = .{
                .lambda = assign.lambda,
                .captures = assign.captures,
            } }),
            .assign_call => |assign| try self.recordLocalDef(assign.target, .{ .call = .{
                .callee = assign.callee,
                .exact_lambda = assign.exact_lambda,
                .exact_requires_hidden_capture = assign.exact_requires_hidden_capture,
                .args = assign.args,
            } }),
            .assign_low_level => |assign| try self.recordLocalDef(assign.target, .{ .low_level = .{
                .op = assign.op,
                .args = assign.args,
            } }),
            .assign_list => |assign| try self.recordLocalDef(assign.target, .{ .list = assign.elems }),
            .assign_struct => |assign| try self.recordLocalDef(assign.target, .{ .struct_ = assign.fields }),
            .assign_tag => |assign| try self.recordLocalDef(assign.target, .{ .tag = .{
                .name = assign.name,
                .args = assign.args,
            } }),
            .join => |join_stmt| {
                for (self.getLocalSpan(join_stmt.params), 0..) |param, i| {
                    try self.recordLocalDef(param, .{ .join_param = .{
                        .join = join_stmt.id,
                        .index = @intCast(i),
                    } });
                }
            },
            .debug,
            .expect,
            .runtime_error,
            .switch_stmt,
            .borrow_scope,
            .scope_exit,
            .jump,
            .ret,
            .crash,
            => {},
        }

        _ = stmt_id;
    }

    fn recordLambdaLocalDefs(self: *Store, lambda_id: LambdaId, lambda: Lambda) Allocator.Error!void {
        for (self.getLocalSpan(lambda.params), 0..) |param, i| {
            try self.recordLocalDef(param, .{ .param = .{
                .lambda = lambda_id,
                .index = @intCast(i),
            } });
        }

        if (lambda.captures_param) |captures_param| {
            try self.recordLocalDef(captures_param, .{ .captures_param = .{
                .lambda = lambda_id,
            } });
        }
    }
};
