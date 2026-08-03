//! Decision-tree match compiler shared by both LIR lowerers.
//!
//! This module turns a `match` (a list of branches, each a pattern plus an
//! optional guard and a body) into an explicit decision tree that the host
//! lowerer then emits as LIR: multiway `switch_stmt`s on tag discriminants and
//! integer values, `str_match_set`s for string arms, length-bucketed switches
//! for list patterns, and equality chains for wide or non-switchable literals.
//! Each scrutinee position ("occurrence") is read into at most one LIR local
//! per dominating scope: one discriminant read per tested tag position, one
//! field read per destructured position, one length read per list position.
//!
//! ## The sharing invariant
//!
//! Monotype is a DAG: an expression id referenced from multiple positions is
//! RE-LOWERED at each reference, so anything the tree wants to share must go
//! through LIR join points (`join`/`jump`), never through re-lowering. PR 9707
//! removed the one known violator (the list-pattern desugarer) after measuring
//! ~(elements+1)^branches statement blowup. This compiler is structured so the
//! invariant holds by construction:
//!
//! - Rows (branches) are never duplicated during specialization. A row whose
//!   pattern does not test the selected occurrence ends the current test
//!   group instead of being copied into every arm; the rows below the group
//!   are compiled once and reached through a single exit join.
//! - Each branch body and guard is therefore lowered exactly once. Bodies end
//!   with a jump to the match's result join; guard failures fall through to
//!   the residual tree for the rows below the guarded row, re-entering the
//!   tree without re-testing columns already known.
//!
//! Because rows never duplicate, the emitted statement count is O(total
//! pattern size) with a small constant; a debug lint in the emitter asserts a
//! hard multiplier bound per match so exponential regressions fail loudly.
//!
//! ## Exhaustiveness
//!
//! Lowering consumes the checker's committed verdicts and never re-derives
//! them. A closed match manifests structurally: the committed union layout's
//! variant set is exactly what `closeExhaustiveVars` left, so a tag test whose
//! arms cover every variant of the occurrence type emits its last arm as the
//! switch default and no failure terminal. Open matches keep the terminal the
//! chain used: `comptime_exhaustiveness_failed` when the match has a comptime
//! site, `runtime_error` otherwise.

const std = @import("std");

/// Pattern kinds the accessor context reports. This is the module's neutral
/// view of `PatData` across Monotype Lifted and Lambda Mono inputs; `callable`
/// only occurs for Lambda Mono.
pub const PatKind = enum {
    bind,
    wildcard,
    as_pattern,
    record,
    tuple,
    list,
    nominal,
    tag,
    callable,
    int_lit,
    dec_lit,
    frac_f32_lit,
    frac_f64_lit,
    str_lit,
    str_pattern,
};

/// What a test node switches on.
pub const TestKind = enum {
    /// Tag-union discriminant; arms keyed by variant index.
    tag,
    /// Lambda Mono callable variant; dispatches like `tag`.
    callable,
    /// Integer scrutinee whose layout fits a `switch_stmt` (size <= 8 bytes);
    /// arm keys are the value zero-extended at layout width.
    int_switch,
    /// Sequential equality tests sharing one scrutinee local: wide integers,
    /// Dec, floats (which must keep IEEE `==` comparison behavior), and string literals
    /// when they cannot be string-set arms.
    eq_chain,
    /// `str_match_set` arms (string interpolation patterns, and string
    /// literals as exact arms).
    str_set,
    /// List length dispatch: exact lengths as switch arms.
    list_len,
};

/// One step from a parent occurrence to a child occurrence.
pub const Step = union(enum) {
    /// The scrutinee itself.
    root,
    /// Record field or tuple item by committed index.
    field: u16,
    /// Tag payload `index` of `variant`; `single` payloads extract through
    /// `tag_payload_struct` rather than `tag_payload`.
    tag_payload: struct { variant: u16, index: u16, single: bool },
    /// Callable payload of `variant` (Lambda Mono only).
    callable_payload: struct { variant: u16 },
    /// List element at a fixed index from the front.
    list_elem_front: u32,
    /// List element at `n` from the back (index `len - n` at runtime).
    list_elem_back: u32,
    /// The rest slice of a list pattern: everything between `front` fixed
    /// elements and `back` fixed elements.
    list_rest: struct { front: u32, back: u32 },
    /// The backing value of a nominal pattern (PR 9849 boundary rules).
    nominal_backing,
    /// Capture `index` of a string-set arm with the given shape key.
    str_capture: struct { shape: u32, index: u16 },
};

/// Interned occurrence id; `root` is always index 0.
pub const OccId = enum(u32) {
    root = 0,
    _,

    pub fn idx(self: OccId) u32 {
        return @intFromEnum(self);
    }
};

/// Decision-tree compiler generic over an accessor context.
///
/// `Ctx` supplies pattern access, type queries, and constructor identity:
///
/// ```
/// const PatId/TypeId/ExprId/LocalId  — input IR id types
/// patKind(pat) PatKind
/// bindLocal(pat) LocalId                                  // .bind
/// asInfo(pat) struct { pattern: PatId, local: LocalId }   // .as_pattern
/// recordDestructCount(pat) u16
/// recordDestruct(pat, ty, i) LowerError!SubPat            // committed field index
/// tupleItemCount(pat) u16
/// tupleItem(pat, ty, i) LowerError!SubPat
/// nominalInner(pat, ty) LowerError!SubPat                 // backing pattern + type
/// tagVariant(pat, ty) u16
/// tagPayloadCount(pat) u16
/// tagPayload(pat, ty, i) LowerError!SubPat
/// tagVariantCount(ty) ?u32                                // null: unknown, never exhaustive
/// callableVariant(pat, ty) u16                            // Lambda Mono only
/// callablePayload(pat, ty) LowerError!?SubPat
/// callableVariantCount(ty) ?u32
/// listView(pat) ListPatView
/// listElemPat(pat, i) PatId
/// listElemTy(ty) TypeId
/// ctorKey(pat, ty) LowerError!u128  // identity within a TestKind (see TestKind
///                                  // docs); string shape ids must fit in u32
/// const LowerError               // error set including OutOfMemory
/// intSwitchValue(pat, ty) ?u64 // null: integer literal must use eq_chain
/// strLitIsSetArm() bool        // whether str_lit unifies with str_pattern arms
/// strCaptureCount(pat) u16     // .str_pattern capture steps
/// strCapturePat(pat, i) ?PatId
/// ```
///
/// where `SubPat = struct { index: u16, ty: TypeId, pat: PatId }` (field
/// names, not the concrete type, are what matters).
pub fn Compiler(comptime Ctx: type) type {
    return struct {
        pub const PatId = Ctx.PatId;
        pub const TypeId = Ctx.TypeId;
        pub const ExprId = Ctx.ExprId;
        pub const LocalId = Ctx.LocalId;

        /// A match branch, in source order.
        pub const Branch = struct {
            pat: PatId,
            guard: ?ExprId,
            body: ExprId,
            branch_index: u32,
        };

        /// A pattern binding: assign the value at `occ` to `local` when the
        /// row's tests have all passed (or before its guard runs).
        pub const Bind = struct {
            occ: OccId,
            ty: TypeId,
            local: LocalId,
        };

        /// A refutable column: the value at `occ` must match `pat`.
        pub const Col = struct {
            occ: OccId,
            ty: TypeId,
            pat: PatId,
        };

        pub const Leaf = struct {
            binds: []const Bind,
            body: ExprId,
            branch_index: u32,
        };

        pub const GuardNode = struct {
            binds: []const Bind,
            guard: ExprId,
            body: ExprId,
            branch_index: u32,
            /// Compiled residual for the rows below this one; taken when the
            /// guard evaluates to false. Never re-tests known columns.
            otherwise: *Tree,
        };

        pub const Arm = struct {
            /// Constructor identity within the test's kind.
            key: u128,
            /// Representative pattern for emission (literal value, string
            /// shape). All rows merged into this arm share the identity.
            example: PatId,
            example_ty: TypeId,
            subtree: *Tree,
        };

        pub const TestNode = struct {
            occ: OccId,
            ty: TypeId,
            kind: TestKind,
            arms: []Arm,
            /// Taken when no arm matches; null iff `exhaustive`.
            default: ?*Tree,
            /// Arms cover the occurrence type's full variant set (checker
            /// verdict made structural); emission turns the last arm into the
            /// switch default.
            exhaustive: bool,
        };

        /// A single rest-pattern list row: `len >= min_len` guards the
        /// specialized row; both the length-test failure and any element-test
        /// failure continue with `otherwise`.
        pub const LenCheck = struct {
            occ: OccId,
            ty: TypeId,
            min_len: u64,
            then: *Tree,
            otherwise: *Tree,
        };

        /// A shared continuation: `inner` is a test whose misses jump to the
        /// exit; `cont` is compiled once and emitted as the body of one LIR
        /// join point.
        pub const ExitJoin = struct {
            id: u32,
            cont: *Tree,
            inner: *Tree,
        };

        pub const Tree = union(enum) {
            leaf: Leaf,
            guard: GuardNode,
            test_: TestNode,
            len_check: LenCheck,
            exit_join: ExitJoin,
            /// Jump to the exit join with this id.
            exit_: u32,
            /// Open-match failure terminal.
            fail,
        };

        pub const OccEntry = struct {
            parent: OccId,
            step: Step,
            ty: TypeId,
        };

        pub const Stats = struct {
            /// Total normalized pattern nodes across all branches (the "total
            /// pattern size" of the statement-count lint).
            pattern_nodes: u32 = 0,
            /// References to the failure terminal in the built tree.
            fail_refs: u32 = 0,
        };

        pub const BuildResult = struct {
            tree: *Tree,
            occs: []const OccEntry,
            stats: Stats,
        };

        const OccDigest = struct {
            parent: u32,
            tag: u8,
            a: u32,
            b: u32,
        };

        const Row = struct {
            cols: []const Col,
            binds: []const Bind,
            guard: ?ExprId,
            body: ExprId,
            branch_index: u32,
        };

        const Miss = union(enum) {
            fail,
            exit_: u32,
        };

        const ExitState = struct {
            cont: *Tree,
            refs: u32,
        };

        const Builder = struct {
            arena: std.mem.Allocator,
            ctx: Ctx,
            occ_entries: std.ArrayList(OccEntry),
            occ_map: std.AutoHashMap(OccDigest, u32),
            exits: std.ArrayList(ExitState),
            stats: Stats,

            fn intern(self: *Builder, parent: OccId, step: Step, ty: TypeId) error{OutOfMemory}!OccId {
                const key = occDigest(parent, step);
                const gop = try self.occ_map.getOrPut(key);
                if (!gop.found_existing) {
                    gop.value_ptr.* = @intCast(self.occ_entries.items.len);
                    try self.occ_entries.append(self.arena, .{ .parent = parent, .step = step, .ty = ty });
                }
                return @enumFromInt(gop.value_ptr.*);
            }

            fn occDigest(parent: OccId, step: Step) OccDigest {
                return switch (step) {
                    .root => .{ .parent = parent.idx(), .tag = 0, .a = 0, .b = 0 },
                    .field => |i| .{ .parent = parent.idx(), .tag = 1, .a = i, .b = 0 },
                    .tag_payload => |p| .{ .parent = parent.idx(), .tag = 2, .a = p.variant, .b = (@as(u32, p.index) << 1) | @intFromBool(p.single) },
                    .callable_payload => |p| .{ .parent = parent.idx(), .tag = 3, .a = p.variant, .b = 0 },
                    .list_elem_front => |i| .{ .parent = parent.idx(), .tag = 4, .a = i, .b = 0 },
                    .list_elem_back => |i| .{ .parent = parent.idx(), .tag = 5, .a = i, .b = 0 },
                    .list_rest => |r| .{ .parent = parent.idx(), .tag = 6, .a = r.front, .b = r.back },
                    .nominal_backing => .{ .parent = parent.idx(), .tag = 7, .a = 0, .b = 0 },
                    .str_capture => |c| .{ .parent = parent.idx(), .tag = 8, .a = c.shape, .b = c.index },
                };
            }

            fn mk(self: *Builder, tree: Tree) error{OutOfMemory}!*Tree {
                const node = try self.arena.create(Tree);
                node.* = tree;
                return node;
            }

            /// Expand `pat` at `occ` into refutable columns and bindings.
            /// Irrefutable structure (binds, wildcards, as-patterns, records,
            /// tuples, nominal wrappers, and rest-only list patterns)
            /// disappears here; only patterns that require a test remain as
            /// columns.
            fn normalize(
                self: *Builder,
                occ: OccId,
                ty: TypeId,
                pat: PatId,
                cols: *std.ArrayList(Col),
                binds: *std.ArrayList(Bind),
            ) Ctx.LowerError!void {
                self.stats.pattern_nodes += 1;
                switch (self.ctx.patKind(pat)) {
                    .bind => try binds.append(self.arena, .{ .occ = occ, .ty = ty, .local = self.ctx.bindLocal(pat) }),
                    .wildcard => {},
                    .as_pattern => {
                        const info = self.ctx.asInfo(pat);
                        try binds.append(self.arena, .{ .occ = occ, .ty = ty, .local = info.local });
                        try self.normalize(occ, ty, info.pattern, cols, binds);
                    },
                    .record => {
                        const count = self.ctx.recordDestructCount(pat);
                        var i: u16 = 0;
                        while (i < count) : (i += 1) {
                            const sub = try self.ctx.recordDestruct(pat, ty, i);
                            const child = try self.intern(occ, .{ .field = sub.index }, sub.ty);
                            try self.normalize(child, sub.ty, sub.pat, cols, binds);
                        }
                    },
                    .tuple => {
                        const count = self.ctx.tupleItemCount(pat);
                        var i: u16 = 0;
                        while (i < count) : (i += 1) {
                            const sub = try self.ctx.tupleItem(pat, ty, i);
                            const child = try self.intern(occ, .{ .field = sub.index }, sub.ty);
                            try self.normalize(child, sub.ty, sub.pat, cols, binds);
                        }
                    },
                    .nominal => {
                        const sub = try self.ctx.nominalInner(pat, ty);
                        const child = try self.intern(occ, .nominal_backing, sub.ty);
                        try self.normalize(child, sub.ty, sub.pat, cols, binds);
                    },
                    .list => {
                        const view = self.ctx.listView(pat);
                        if (view.fixed_count == 0) {
                            if (view.rest) |rest| {
                                // `[..]` / `[.. as r]`: irrefutable; the rest
                                // IS the whole scrutinee list.
                                if (rest.pattern) |rest_pat| {
                                    try self.normalize(occ, ty, rest_pat, cols, binds);
                                }
                                return;
                            }
                        }
                        try cols.append(self.arena, .{ .occ = occ, .ty = ty, .pat = pat });
                    },
                    .tag, .callable, .int_lit, .dec_lit, .frac_f32_lit, .frac_f64_lit, .str_lit, .str_pattern => {
                        try cols.append(self.arena, .{ .occ = occ, .ty = ty, .pat = pat });
                    },
                }
            }

            fn missTree(self: *Builder, miss: Miss) error{OutOfMemory}!*Tree {
                switch (miss) {
                    .fail => {
                        self.stats.fail_refs += 1;
                        return try self.mk(.fail);
                    },
                    .exit_ => |id| {
                        self.exits.items[id].refs += 1;
                        return try self.mk(.{ .exit_ = id });
                    },
                }
            }

            fn colAt(row: Row, occ: OccId) ?Col {
                for (row.cols) |col| {
                    if (col.occ == occ) return col;
                }
                return null;
            }

            fn colsWithout(self: *Builder, row: Row, occ: OccId) Ctx.LowerError![]const Col {
                var out: std.ArrayList(Col) = .empty;
                try out.ensureTotalCapacityPrecise(self.arena, row.cols.len -| 1);
                for (row.cols) |col| {
                    if (col.occ != occ) try out.append(self.arena, col);
                }
                return out.items;
            }

            fn testKindOf(self: *Builder, col: Col) TestKind {
                return switch (self.ctx.patKind(col.pat)) {
                    .tag => .tag,
                    .callable => .callable,
                    .int_lit => if (self.ctx.intSwitchValue(col.pat, col.ty) != null) .int_switch else .eq_chain,
                    .dec_lit, .frac_f32_lit, .frac_f64_lit => .eq_chain,
                    .str_lit => if (self.ctx.strLitIsSetArm()) .str_set else .eq_chain,
                    .str_pattern => .str_set,
                    .list => .list_len,
                    .bind, .wildcard, .as_pattern, .record, .tuple, .nominal => unreachable, // normalized away
                };
            }

            fn isRestList(self: *Builder, pat: PatId) bool {
                if (self.ctx.patKind(pat) != .list) return false;
                return self.ctx.listView(pat).rest != null;
            }

            /// Length of the test group at `occ` starting at `rows[0]`: the
            /// maximal prefix of rows that all test `occ` with the same kind.
            /// A row without a column at `occ` (a wildcard there), a row whose
            /// column has a different kind, and a rest-pattern list row all
            /// end the group (rest rows become `len_check` nodes instead so
            /// rows never duplicate across arms).
            fn runLength(self: *Builder, rows: []const Row, occ: OccId, kind: TestKind) usize {
                var n: usize = 0;
                while (n < rows.len) : (n += 1) {
                    const col = colAt(rows[n], occ) orelse break;
                    if (self.testKindOf(col) != kind) break;
                    if (kind == .list_len and self.isRestList(col.pat)) break;
                }
                return n;
            }

            /// Pick the occurrence to test next: the column of `rows[0]` with
            /// the longest same-kind run down the matrix (ties broken by
            /// first-column order). Longer runs mean more rows dispatched by
            /// one multiway test. Measured against plain first-column
            /// selection on the generated corpus
            /// (src/compile/test/match_corpus_test.zig) the two produced
            /// identical statement totals — realistic matrices rarely give
            /// row 0 multiple refutable columns with different run lengths —
            /// so run length is kept because it dominates first-column by
            /// construction whenever they do differ.
            fn selectColumn(self: *Builder, rows: []const Row) Col {
                const row0 = rows[0];
                var best = row0.cols[0];
                var best_run: usize = 0;
                for (row0.cols) |col| {
                    const kind = self.testKindOf(col);
                    var run = self.runLength(rows, col.occ, kind);
                    if (kind == .list_len and run == 0 and self.isRestList(col.pat)) {
                        // A leading rest row forms its own len_check segment.
                        run = 1;
                    }
                    if (run > best_run) {
                        best = col;
                        best_run = run;
                    }
                }
                return best;
            }

            /// Specialize `row` for tag/callable arm membership: replace the
            /// tested column with payload sub-columns.
            fn specTagRow(self: *Builder, row: Row, col: Col, kind: TestKind) Ctx.LowerError!Row {
                var cols: std.ArrayList(Col) = .empty;
                var binds: std.ArrayList(Bind) = .empty;
                try binds.appendSlice(self.arena, row.binds);

                switch (kind) {
                    .tag => {
                        const variant = self.ctx.tagVariant(col.pat, col.ty);
                        const count = self.ctx.tagPayloadCount(col.pat);
                        const single = count == 1;
                        var i: u16 = 0;
                        while (i < count) : (i += 1) {
                            const sub = try self.ctx.tagPayload(col.pat, col.ty, i);
                            const child = try self.intern(col.occ, .{ .tag_payload = .{
                                .variant = variant,
                                .index = sub.index,
                                .single = single,
                            } }, sub.ty);
                            try self.normalize(child, sub.ty, sub.pat, &cols, &binds);
                        }
                    },
                    .callable => {
                        const variant = self.ctx.callableVariant(col.pat, col.ty);
                        if (try self.ctx.callablePayload(col.pat, col.ty)) |sub| {
                            const child = try self.intern(col.occ, .{ .callable_payload = .{ .variant = variant } }, sub.ty);
                            try self.normalize(child, sub.ty, sub.pat, &cols, &binds);
                        }
                    },
                    else => unreachable,
                }

                for (row.cols) |c| {
                    if (c.occ != col.occ) try cols.append(self.arena, c);
                }
                return .{
                    .cols = cols.items,
                    .binds = binds.items,
                    .guard = row.guard,
                    .body = row.body,
                    .branch_index = row.branch_index,
                };
            }

            /// Specialize a string row inside a str_set arm: capture patterns
            /// become columns/binds at str_capture occurrences. Literal string
            /// arms have no captures and just drop the column.
            fn specStrRow(self: *Builder, row: Row, col: Col, shape: u32) Ctx.LowerError!Row {
                var cols: std.ArrayList(Col) = .empty;
                var binds: std.ArrayList(Bind) = .empty;
                try binds.appendSlice(self.arena, row.binds);

                if (self.ctx.patKind(col.pat) == .str_pattern) {
                    const count = self.ctx.strCaptureCount(col.pat);
                    var i: u16 = 0;
                    while (i < count) : (i += 1) {
                        if (self.ctx.strCapturePat(col.pat, i)) |capture| {
                            const child = try self.intern(col.occ, .{ .str_capture = .{ .shape = shape, .index = i } }, col.ty);
                            try self.normalize(child, col.ty, capture, &cols, &binds);
                        }
                    }
                }

                for (row.cols) |c| {
                    if (c.occ != col.occ) try cols.append(self.arena, c);
                }
                return .{
                    .cols = cols.items,
                    .binds = binds.items,
                    .guard = row.guard,
                    .body = row.body,
                    .branch_index = row.branch_index,
                };
            }

            /// Specialize a literal row (int/dec/frac/str_lit equality arm):
            /// the matched literal imposes no sub-structure, so the column
            /// just drops.
            fn specLiteralRow(self: *Builder, row: Row, col: Col) Ctx.LowerError!Row {
                return .{
                    .cols = try self.colsWithout(row, col.occ),
                    .binds = row.binds,
                    .guard = row.guard,
                    .body = row.body,
                    .branch_index = row.branch_index,
                };
            }

            /// Specialize an exact-length list row inside a length arm:
            /// element patterns become columns at element occurrences. With
            /// the length known exactly, back-relative elements canonicalize
            /// to front indices so rows agree on occurrence identity.
            fn specListExactRow(self: *Builder, row: Row, col: Col, exact_len: u64) Ctx.LowerError!Row {
                var cols: std.ArrayList(Col) = .empty;
                var binds: std.ArrayList(Bind) = .empty;
                try binds.appendSlice(self.arena, row.binds);
                try self.specListInto(row, col, .{ .exact = exact_len }, &cols, &binds);
                return .{
                    .cols = cols.items,
                    .binds = binds.items,
                    .guard = row.guard,
                    .body = row.body,
                    .branch_index = row.branch_index,
                };
            }

            /// Specialize a rest-pattern list row under its `len >= k` check:
            /// elements before the rest index are front-relative, elements
            /// after it are back-relative, and the rest slice binds between
            /// them.
            fn specListRestRow(self: *Builder, row: Row, col: Col) Ctx.LowerError!Row {
                var cols: std.ArrayList(Col) = .empty;
                var binds: std.ArrayList(Bind) = .empty;
                try binds.appendSlice(self.arena, row.binds);
                try self.specListInto(row, col, .rest_relative, &cols, &binds);
                return .{
                    .cols = cols.items,
                    .binds = binds.items,
                    .guard = row.guard,
                    .body = row.body,
                    .branch_index = row.branch_index,
                };
            }

            const ListIndexing = union(enum) {
                exact: u64,
                rest_relative,
            };

            fn specListInto(
                self: *Builder,
                row: Row,
                col: Col,
                indexing: ListIndexing,
                cols: *std.ArrayList(Col),
                binds: *std.ArrayList(Bind),
            ) Ctx.LowerError!void {
                const view = self.ctx.listView(col.pat);
                const elem_ty = self.ctx.listElemTy(col.ty);
                const rest_index: ?u32 = if (view.rest) |rest| rest.index else null;

                var i: u32 = 0;
                while (i < view.fixed_count) : (i += 1) {
                    const from_back = if (rest_index) |ri| i >= ri else false;
                    const step: Step = if (!from_back)
                        .{ .list_elem_front = i }
                    else switch (indexing) {
                        // Element i sits (fixed_count - i) from the end; with
                        // the length known exactly that is a fixed front
                        // index, letting arms share element occurrences.
                        .exact => |n| .{ .list_elem_front = @intCast(n - (view.fixed_count - i)) },
                        .rest_relative => .{ .list_elem_back = view.fixed_count - i },
                    };
                    const child = try self.intern(col.occ, step, elem_ty);
                    try self.normalize(child, elem_ty, self.ctx.listElemPat(col.pat, i), cols, binds);
                }

                if (view.rest) |rest| {
                    if (rest.pattern) |rest_pat| {
                        const front = rest_index.?;
                        const back = view.fixed_count - front;
                        const child = try self.intern(col.occ, .{ .list_rest = .{ .front = front, .back = back } }, col.ty);
                        try self.normalize(child, col.ty, rest_pat, cols, binds);
                    }
                }

                for (row.cols) |c| {
                    if (c.occ != col.occ) try cols.append(self.arena, c);
                }
            }

            /// Compile `rows` against `miss`. Rows are consumed front-to-back
            /// as segments (leaf/guard runs, test groups, single rest-row
            /// length checks); segments compose bottom-up so the recursion
            /// depth tracks pattern nesting, not branch count.
            fn compile(self: *Builder, rows: []const Row, miss: Miss) Ctx.LowerError!*Tree {
                const segments = try self.partition(rows);
                var acc: ?*Tree = null;
                var i = segments.len;
                while (i > 0) {
                    i -= 1;
                    const seg = segments[i];
                    acc = try self.buildSegment(seg.kind, rows[seg.start..seg.end], acc, miss);
                }
                return acc orelse try self.missTree(miss);
            }

            const Segment = struct {
                start: usize,
                end: usize,
                kind: SegmentKind,
            };

            const SegmentKind = union(enum) {
                /// Rows with no remaining columns: a run of guarded leaves
                /// optionally ending in an unguarded (terminal) leaf.
                leaves,
                /// A multiway test group on one occurrence.
                group: struct { occ: OccId, ty: TypeId, kind: TestKind },
                /// A single rest-pattern list row.
                rest_row: struct { occ: OccId, ty: TypeId },
            };

            fn partition(self: *Builder, rows: []const Row) Ctx.LowerError![]const Segment {
                var segments: std.ArrayList(Segment) = .empty;
                var i: usize = 0;
                while (i < rows.len) {
                    const row = rows[i];
                    if (row.cols.len == 0) {
                        var end = i;
                        var terminal = false;
                        while (end < rows.len and rows[end].cols.len == 0) {
                            const unguarded = rows[end].guard == null;
                            end += 1;
                            if (unguarded) {
                                terminal = true;
                                break;
                            }
                        }
                        try segments.append(self.arena, .{ .start = i, .end = end, .kind = .leaves });
                        if (terminal) return segments.items; // rows below are unreachable here
                        i = end;
                        continue;
                    }

                    const col = self.selectColumn(rows[i..]);
                    const kind = self.testKindOf(col);
                    if (kind == .list_len and self.isRestList(col.pat)) {
                        try segments.append(self.arena, .{ .start = i, .end = i + 1, .kind = .{ .rest_row = .{ .occ = col.occ, .ty = col.ty } } });
                        i += 1;
                        continue;
                    }
                    const run = self.runLength(rows[i..], col.occ, kind);
                    std.debug.assert(run > 0);
                    try segments.append(self.arena, .{ .start = i, .end = i + run, .kind = .{ .group = .{ .occ = col.occ, .ty = col.ty, .kind = kind } } });
                    i += run;
                }
                return segments.items;
            }

            /// Build one segment. `below` is the already-compiled tree for
            /// the rows after this segment (null when this segment is last),
            /// and `miss` is where matching continues when the whole
            /// remainder is exhausted.
            fn buildSegment(self: *Builder, seg_kind: SegmentKind, rows: []const Row, below: ?*Tree, miss: Miss) Ctx.LowerError!*Tree {
                switch (seg_kind) {
                    .leaves => {
                        var acc = below;
                        var i = rows.len;
                        while (i > 0) {
                            i -= 1;
                            const row = rows[i];
                            if (row.guard) |guard| {
                                const otherwise = acc orelse try self.missTree(miss);
                                acc = try self.mk(.{ .guard = .{
                                    .binds = row.binds,
                                    .guard = guard,
                                    .body = row.body,
                                    .branch_index = row.branch_index,
                                    .otherwise = otherwise,
                                } });
                            } else {
                                acc = try self.mk(.{ .leaf = .{
                                    .binds = row.binds,
                                    .body = row.body,
                                    .branch_index = row.branch_index,
                                } });
                            }
                        }
                        return acc.?;
                    },
                    .rest_row => |info| return try self.buildRestRow(info.occ, rows[0], below, miss),
                    .group => |info| return try self.buildGroup(info.occ, info.ty, info.kind, rows, below, miss),
                }
            }

            fn buildRestRow(self: *Builder, occ: OccId, row: Row, below: ?*Tree, miss: Miss) Ctx.LowerError!*Tree {
                const col = colAt(row, occ).?;
                const view = self.ctx.listView(col.pat);
                std.debug.assert(view.rest != null);

                if (below) |cont| {
                    const exit_id: u32 = @intCast(self.exits.items.len);
                    try self.exits.append(self.arena, .{ .cont = cont, .refs = 0 });
                    const spec = try self.specListRestRow(row, col);
                    const then = try self.compile(&.{spec}, .{ .exit_ = exit_id });
                    const otherwise = try self.missTree(.{ .exit_ = exit_id });
                    const node = try self.mk(.{ .len_check = .{
                        .occ = col.occ,
                        .ty = col.ty,
                        .min_len = view.fixed_count,
                        .then = then,
                        .otherwise = otherwise,
                    } });
                    return try self.wrapExit(exit_id, node);
                }

                const spec = try self.specListRestRow(row, col);
                const then = try self.compile(&.{spec}, miss);
                const otherwise = try self.missTree(miss);
                return try self.mk(.{ .len_check = .{
                    .occ = col.occ,
                    .ty = col.ty,
                    .min_len = view.fixed_count,
                    .then = then,
                    .otherwise = otherwise,
                } });
            }

            /// Wrap `inner` in an exit join when its continuation is
            /// referenced; splice the continuation inline when the join would
            /// have a single default-position reference.
            fn wrapExit(self: *Builder, exit_id: u32, inner: *Tree) Ctx.LowerError!*Tree {
                const state = self.exits.items[exit_id];
                if (state.refs == 0) return inner;
                if (state.refs == 1) {
                    // Splice into the unique reference site when it is a
                    // directly-owned default/otherwise slot of `inner`.
                    switch (inner.*) {
                        .test_ => |*t| if (t.default) |d| {
                            if (d.* == .exit_ and d.exit_ == exit_id) {
                                t.default = state.cont;
                                return inner;
                            }
                        },
                        .len_check => |*lc| {
                            if (lc.otherwise.* == .exit_ and lc.otherwise.exit_ == exit_id) {
                                lc.otherwise = state.cont;
                                return inner;
                            }
                        },
                        else => {},
                    }
                }
                return try self.mk(.{ .exit_join = .{ .id = exit_id, .cont = state.cont, .inner = inner } });
            }

            fn buildGroup(self: *Builder, occ: OccId, occ_ty: TypeId, kind: TestKind, rows: []const Row, below: ?*Tree, miss: Miss) Ctx.LowerError!*Tree {
                // Arm collection preserves first-appearance order; rows with
                // the same constructor identity merge into one arm in source
                // order, which is what makes guard fallthrough inside an arm
                // (including duplicate string patterns) retry later rows
                // instead of skipping them.
                var arm_keys: std.ArrayList(u128) = .empty;
                var arm_examples: std.ArrayList(Col) = .empty;
                var arm_rows: std.ArrayList(std.ArrayList(Row)) = .empty;
                var key_index: std.AutoHashMap(u128, usize) = .init(self.arena);

                for (rows) |row| {
                    const col = colAt(row, occ).?;
                    const key = try self.ctx.ctorKey(col.pat, col.ty);
                    const gop = try key_index.getOrPut(key);
                    if (!gop.found_existing) {
                        gop.value_ptr.* = arm_keys.items.len;
                        try arm_keys.append(self.arena, key);
                        try arm_examples.append(self.arena, col);
                        try arm_rows.append(self.arena, .empty);
                    }
                    const spec = switch (kind) {
                        .tag, .callable => try self.specTagRow(row, col, kind),
                        .int_switch, .eq_chain => try self.specLiteralRow(row, col),
                        .str_set => try self.specStrRow(row, col, @truncate(key)),
                        .list_len => try self.specListExactRow(row, col, @truncate(key)),
                    };
                    try arm_rows.items[gop.value_ptr.*].append(self.arena, spec);
                }

                const exit_id: u32 = @intCast(self.exits.items.len);
                try self.exits.append(self.arena, .{ .cont = undefined, .refs = 0 });
                const have_below = below != null;
                const arm_miss: Miss = if (have_below) .{ .exit_ = exit_id } else miss;

                const arms = try self.arena.alloc(Arm, arm_keys.items.len);
                for (arms, arm_keys.items, arm_examples.items, arm_rows.items) |*arm, key, example, rows_list| {
                    arm.* = .{
                        .key = key,
                        .example = example.pat,
                        .example_ty = example.ty,
                        .subtree = try self.compile(rows_list.items, arm_miss),
                    };
                }

                const exhaustive = switch (kind) {
                    .tag => if (self.ctx.tagVariantCount(occ_ty)) |count| arms.len == count else false,
                    .callable => if (self.ctx.callableVariantCount(occ_ty)) |count| arms.len == count else false,
                    .int_switch, .eq_chain, .str_set, .list_len => false,
                };

                const default: ?*Tree = if (exhaustive) null else if (have_below)
                    try self.missTree(.{ .exit_ = exit_id })
                else
                    try self.missTree(miss);

                const node = try self.mk(.{ .test_ = .{
                    .occ = occ,
                    .ty = occ_ty,
                    .kind = kind,
                    .arms = arms,
                    .default = default,
                    .exhaustive = exhaustive,
                } });

                if (!have_below) return node;
                self.exits.items[exit_id].cont = below.?;
                return try self.wrapExit(exit_id, node);
            }
        };

        /// Build the decision tree for one match. All allocations go through
        /// `arena`, which the caller frees wholesale after emission.
        pub fn build(
            arena: std.mem.Allocator,
            ctx: Ctx,
            scrutinee_ty: TypeId,
            branches: []const Branch,
        ) Ctx.LowerError!BuildResult {
            var builder = Builder{
                .arena = arena,
                .ctx = ctx,
                .occ_entries = .empty,
                .occ_map = .init(arena),
                .exits = .empty,
                .stats = .{},
            };
            try builder.occ_entries.append(arena, .{ .parent = .root, .step = .root, .ty = scrutinee_ty });

            const rows = try arena.alloc(Row, branches.len);
            for (branches, rows) |branch, *row| {
                var cols: std.ArrayList(Col) = .empty;
                var binds: std.ArrayList(Bind) = .empty;
                try builder.normalize(.root, scrutinee_ty, branch.pat, &cols, &binds);
                row.* = .{
                    .cols = cols.items,
                    .binds = binds.items,
                    .guard = branch.guard,
                    .body = branch.body,
                    .branch_index = branch.branch_index,
                };
            }

            const tree = try builder.compile(rows, .fail);
            return .{
                .tree = tree,
                .occs = builder.occ_entries.items,
                .stats = builder.stats,
            };
        }

        /// Emit a built tree as LIR. Beyond the construction interface, `Ctx`
        /// must provide the emission surface:
        ///
        /// ```
        /// const LirLocal / CFStmtId / JoinPointId / StrArm / LowerError
        /// stmtCount() usize
        /// freshJoinPointId() JoinPointId
        /// joinJump(JoinPointId) CFStmtId
        /// addExitJoin(id, body, remainder) CFStmtId      // empty params
        /// failTerminal() CFStmtId                        // per checker verdict
        /// lirLocalForOcc(step: Step, ty, parent: ?LirLocal) LirLocal
        /// lirLocalU16/lirLocalU64() LirLocal
        /// isZstLirLocal(LirLocal) bool
        /// readDiscriminant(target, source, next) CFStmtId
        /// readField(target, ty, source, index, next) CFStmtId
        /// readTagPayload(target, ty, source, variant, index, single, next) CFStmtId
        /// readCallablePayload(target, ty, source, variant, next) CFStmtId
        /// readListLen(target, source, next) CFStmtId
        /// readListElemFront(target, source, index, next) CFStmtId
        /// readListElemBack(target, source, len_local, back, next) CFStmtId
        /// readListRest(target, source, len_local, front, back, next) CFStmtId
        /// readNominalBacking(target, backing_ty, source, nominal_ty, next) CFStmtId
        /// switchStmt(cond, values: []const u64, bodies: []const CFStmtId, default) CFStmtId
        /// lenGteTest(len_local, min, then, els) CFStmtId
        /// literalEqTest(source, ty, pat, on_match, on_miss) CFStmtId
        /// strArmCaptureLocals(pat) []const ?LirLocal     // arena-allocated
        /// buildStrArm(pat, capture_locals, on_match) StrArm
        /// strMatchSet(source, arms, on_miss) CFStmtId
        /// bindPatternLocal(local, ty, source, next) CFStmtId
        /// lowerBody(body, next) CFStmtId                 // into the result local
        /// guardTemp(guard) LirLocal
        /// lowerGuard(cond, guard, next) CFStmtId
        /// boolSwitch(cond, then, els) CFStmtId
        /// ```
        const CtxStmt = Ctx.CFStmtId;
        const CtxLocal = Ctx.LirLocal;

        /// Statement-count lint bound: emitted match-machinery statements must
        /// stay within `LINT_MULT * pattern_nodes + LINT_BASE`. Rows never
        /// duplicate, so every pattern node contributes a bounded number of
        /// statements (list rest extraction is the worst at ~8); exceeding the
        /// bound means the linear-size guarantee regressed.
        pub const LINT_MULT = 16;
        pub const LINT_BASE = 24;

        const OccUse = struct {
            value: bool = false,
            disc: bool = false,
            len: bool = false,
        };

        const EmitLocals = struct {
            value: ?CtxLocal = null,
            disc: ?CtxLocal = null,
            len: ?CtxLocal = null,
        };

        const Extraction = struct {
            occ: OccId,
            what: enum { value, disc, len },
        };

        /// The scope that must be entered before an occurrence's value can be
        /// extracted: the last constructor-dependent step on its path.
        const Scope = union(enum) {
            /// Safe from the match entry (fields, nominal backings, and their
            /// derived reads).
            top,
            /// Inside the arm for `variant` of the tag test on `occ`.
            tag_arm: struct { occ: OccId, variant: u16 },
            callable_arm: struct { occ: OccId, variant: u16 },
            /// Inside a list-length scope on `occ` that guarantees at least
            /// `min_len` elements.
            list_len: struct { occ: OccId, min_len: u64 },
            /// Inside the string-set arm with `shape` on `occ`. Capture
            /// locals come from the arm itself, not from extraction.
            str_arm: struct { occ: OccId, shape: u32 },
        };

        pub const Emitter = struct {
            arena: std.mem.Allocator,
            ctx: Ctx,
            occs: []const OccEntry,
            tree_stats: Stats,
            done: Ctx.JoinPointId,
            uses: std.AutoHashMap(OccId, OccUse),
            exit_joins: std.AutoHashMap(u32, Ctx.JoinPointId),
            /// Statements added by delegated body/guard lowering, excluded
            /// from the lint's machinery count.
            delegated_stmts: usize,

            const Env = struct {
                locals: std.AutoHashMapUnmanaged(OccId, EmitLocals),

                fn clone(self: *const Env, arena: std.mem.Allocator) error{OutOfMemory}!Env {
                    return .{ .locals = try self.locals.clone(arena) };
                }
            };

            /// Emit `tree` and return the entry statement. `scrutinee` is the
            /// already-allocated local the caller evaluates the scrutinee
            /// into; bodies are lowered into the caller's result local and
            /// jump to `done`.
            pub fn emitMatch(
                arena: std.mem.Allocator,
                ctx: Ctx,
                result: BuildResult,
                scrutinee: CtxLocal,
                done: Ctx.JoinPointId,
            ) Ctx.LowerError!CtxStmt {
                var self = Emitter{
                    .arena = arena,
                    .ctx = ctx,
                    .occs = result.occs,
                    .tree_stats = result.stats,
                    .done = done,
                    .uses = .init(arena),
                    .exit_joins = .init(arena),
                    .delegated_stmts = 0,
                };
                try self.collectUses(result.tree);

                var env = Env{ .locals = .empty };
                try env.locals.put(arena, .root, .{ .value = scrutinee });

                const before = self.ctx.stmtCount();
                var entry_extractions: std.ArrayList(Extraction) = .empty;
                try self.enterScope(&env, .top, &entry_extractions);
                var body = try self.emitTree(result.tree, &env);
                body = try self.emitExtractions(entry_extractions.items, &env, body);

                if (std.debug.runtime_safety) {
                    const machinery = (self.ctx.stmtCount() - before) - self.delegated_stmts;
                    const bound = @as(usize, LINT_MULT) * @as(usize, result.stats.pattern_nodes) + LINT_BASE;
                    if (machinery > bound) {
                        std.debug.panic(
                            "match_tree emitted {d} machinery statements for {d} pattern nodes (bound {d}); the linear-size guarantee regressed",
                            .{ machinery, result.stats.pattern_nodes, bound },
                        );
                    }
                }
                return body;
            }

            fn occEntry(self: *const Emitter, occ: OccId) OccEntry {
                return self.occs[occ.idx()];
            }

            fn markUse(self: *Emitter, occ: OccId, comptime what: enum { value, disc, len }) error{OutOfMemory}!void {
                // A derived or extracted read needs its parent's value.
                const gop = try self.uses.getOrPut(occ);
                if (!gop.found_existing) gop.value_ptr.* = .{};
                switch (what) {
                    .value => gop.value_ptr.value = true,
                    .disc => gop.value_ptr.disc = true,
                    .len => gop.value_ptr.len = true,
                }
                const entry = self.occEntry(occ);
                if (occ != .root) try self.markUse(entry.parent, .value);
                // Back-relative and rest reads need the parent list's length.
                switch (entry.step) {
                    .list_elem_back, .list_rest => try self.markUse(entry.parent, .len),
                    else => {},
                }
            }

            fn collectUses(self: *Emitter, tree: *const Tree) error{OutOfMemory}!void {
                switch (tree.*) {
                    .leaf => |leaf| for (leaf.binds) |bind| try self.markUse(bind.occ, .value),
                    .guard => |g| {
                        for (g.binds) |bind| try self.markUse(bind.occ, .value);
                        try self.collectUses(g.otherwise);
                    },
                    .test_ => |t| {
                        switch (t.kind) {
                            // A single-arm exhaustive discriminant test emits
                            // no switch at all, so it needs no discriminant
                            // read (payload extraction marks the value on its
                            // own).
                            .tag, .callable => if (!(t.exhaustive and t.arms.len == 1)) try self.markUse(t.occ, .disc),
                            .int_switch, .eq_chain, .str_set => try self.markUse(t.occ, .value),
                            .list_len => try self.markUse(t.occ, .len),
                        }
                        for (t.arms) |arm| try self.collectUses(arm.subtree);
                        if (t.default) |d| try self.collectUses(d);
                    },
                    .len_check => |lc| {
                        try self.markUse(lc.occ, .len);
                        try self.collectUses(lc.then);
                        try self.collectUses(lc.otherwise);
                    },
                    .exit_join => |j| {
                        try self.collectUses(j.cont);
                        try self.collectUses(j.inner);
                    },
                    .exit_, .fail => {},
                }
            }

            /// The scope in which `occ`'s value becomes extractable.
            fn establishingScope(self: *const Emitter, occ: OccId) Scope {
                var walk = occ;
                while (walk != .root) {
                    const entry = self.occEntry(walk);
                    switch (entry.step) {
                        .tag_payload => |p| return .{ .tag_arm = .{ .occ = entry.parent, .variant = p.variant } },
                        .callable_payload => |p| return .{ .callable_arm = .{ .occ = entry.parent, .variant = p.variant } },
                        .list_elem_front => |i| return .{ .list_len = .{ .occ = entry.parent, .min_len = @as(u64, i) + 1 } },
                        .list_elem_back => |j| return .{ .list_len = .{ .occ = entry.parent, .min_len = j } },
                        .list_rest => |r| return .{ .list_len = .{ .occ = entry.parent, .min_len = @as(u64, r.front) + r.back } },
                        .str_capture => |c| return .{ .str_arm = .{ .occ = entry.parent, .shape = c.shape } },
                        .root, .field, .nominal_backing => walk = entry.parent,
                    }
                }
                return .top;
            }

            fn scopeSatisfies(scope: Scope, needed: Scope) bool {
                return switch (needed) {
                    .top => scope == .top,
                    .tag_arm => |n| scope == .tag_arm and scope.tag_arm.occ == n.occ and scope.tag_arm.variant == n.variant,
                    .callable_arm => |n| scope == .callable_arm and scope.callable_arm.occ == n.occ and scope.callable_arm.variant == n.variant,
                    .list_len => |n| scope == .list_len and scope.list_len.occ == n.occ and scope.list_len.min_len >= n.min_len,
                    .str_arm => |n| scope == .str_arm and scope.str_arm.occ == n.occ and scope.str_arm.shape == n.shape,
                };
            }

            /// Materialize every used occurrence whose establishing scope is
            /// satisfied by `scope`: allocate locals into `env` now (so the
            /// subtree can reference them) and record extraction descriptors;
            /// the caller prepends the actual statements with
            /// `emitExtractions` after the subtree is built.
            fn enterScope(
                self: *Emitter,
                env: *Env,
                scope: Scope,
                extractions: *std.ArrayList(Extraction),
            ) Ctx.LowerError!void {
                // Deterministic iteration: walk occs in interning order.
                for (0..self.occs.len) |i| {
                    const occ: OccId = @enumFromInt(i);
                    const use = self.uses.get(occ) orelse continue;
                    if (!scopeSatisfies(scope, self.establishingScope(occ))) continue;
                    try self.materialize(env, occ, use, extractions);
                }
            }

            fn materialize(
                self: *Emitter,
                env: *Env,
                occ: OccId,
                use: OccUse,
                extractions: *std.ArrayList(Extraction),
            ) Ctx.LowerError!void {
                const gop = try env.locals.getOrPut(self.arena, occ);
                if (!gop.found_existing) gop.value_ptr.* = .{};
                const entry = self.occEntry(occ);

                if ((use.value or use.disc or use.len) and gop.value_ptr.value == null) {
                    // String captures get their locals from the enclosing
                    // string arm, which registers them before entering the
                    // subtree; reaching here for one is a compiler bug.
                    std.debug.assert(entry.step != .str_capture);
                    // Parents materialize before children (interning order),
                    // so the parent's value local is already available for
                    // layout-derived allocations (callable payloads).
                    const parent = if (occ == .root) null else env.locals.get(entry.parent).?.value;
                    gop.value_ptr.value = try self.ctx.lirLocalForOcc(entry.step, entry.ty, parent);
                    try extractions.append(self.arena, .{ .occ = occ, .what = .value });
                }
                if (use.disc and gop.value_ptr.disc == null) {
                    gop.value_ptr.disc = try self.ctx.lirLocalU16();
                    try extractions.append(self.arena, .{ .occ = occ, .what = .disc });
                }
                if (use.len and gop.value_ptr.len == null) {
                    gop.value_ptr.len = try self.ctx.lirLocalU64();
                    try extractions.append(self.arena, .{ .occ = occ, .what = .len });
                }
            }

            /// Prepend extraction statements before `next`, in reverse of the
            /// recorded (dependency) order so parents extract first.
            fn emitExtractions(
                self: *Emitter,
                extractions: []const Extraction,
                env: *Env,
                next: CtxStmt,
            ) Ctx.LowerError!CtxStmt {
                var current = next;
                var i = extractions.len;
                while (i > 0) {
                    i -= 1;
                    const ex = extractions[i];
                    const entry = self.occEntry(ex.occ);
                    const locals = env.locals.get(ex.occ).?;
                    switch (ex.what) {
                        .disc => {
                            const source = self.valueLocal(env, ex.occ);
                            current = try self.ctx.readDiscriminant(locals.disc.?, source, current);
                        },
                        .len => {
                            const source = self.valueLocal(env, ex.occ);
                            current = try self.ctx.readListLen(locals.len.?, source, current);
                        },
                        .value => {
                            const target = locals.value.?;
                            if (self.ctx.isZstLirLocal(target)) continue;
                            const parent = self.valueLocal(env, entry.parent);
                            current = switch (entry.step) {
                                .root => current, // pre-seeded scrutinee
                                .field => |index| try self.ctx.readField(target, entry.ty, parent, index, current),
                                .tag_payload => |p| try self.ctx.readTagPayload(target, entry.ty, parent, p.variant, p.index, p.single, current),
                                .callable_payload => |p| try self.ctx.readCallablePayload(target, entry.ty, parent, p.variant, current),
                                .list_elem_front => |index| try self.ctx.readListElemFront(target, parent, index, current),
                                .list_elem_back => |back| try self.ctx.readListElemBack(target, parent, self.lenLocal(env, entry.parent), back, current),
                                .list_rest => |r| try self.ctx.readListRest(target, parent, self.lenLocal(env, entry.parent), r.front, r.back, current),
                                .nominal_backing => try self.ctx.readNominalBacking(target, entry.ty, parent, self.occEntry(entry.parent).ty, current),
                                .str_capture => unreachable, // registered by the string arm
                            };
                        },
                    }
                }
                return current;
            }

            fn valueLocal(_: *const Emitter, env: *Env, occ: OccId) CtxLocal {
                return (env.locals.get(occ) orelse unreachable).value orelse unreachable;
            }

            fn lenLocal(_: *const Emitter, env: *Env, occ: OccId) CtxLocal {
                return (env.locals.get(occ) orelse unreachable).len orelse unreachable;
            }

            fn discLocal(_: *const Emitter, env: *Env, occ: OccId) CtxLocal {
                return (env.locals.get(occ) orelse unreachable).disc orelse unreachable;
            }

            fn emitBinds(self: *Emitter, binds: []const Bind, env: *Env, next: CtxStmt) Ctx.LowerError!CtxStmt {
                var current = next;
                var i = binds.len;
                while (i > 0) {
                    i -= 1;
                    const bind = binds[i];
                    current = try self.ctx.bindPatternLocal(bind.local, bind.ty, self.valueLocal(env, bind.occ), current);
                }
                return current;
            }

            fn lowerBodyCounted(self: *Emitter, body: ExprId, next: CtxStmt) Ctx.LowerError!CtxStmt {
                const before = self.ctx.stmtCount();
                const result = try self.ctx.lowerBody(body, next);
                self.delegated_stmts += self.ctx.stmtCount() - before;
                return result;
            }

            fn emitTree(self: *Emitter, tree: *const Tree, env: *Env) Ctx.LowerError!CtxStmt {
                switch (tree.*) {
                    .leaf => |leaf| {
                        const body = try self.lowerBodyCounted(leaf.body, try self.ctx.joinJump(self.done));
                        return try self.emitBinds(leaf.binds, env, body);
                    },
                    .guard => |g| {
                        const body = try self.lowerBodyCounted(g.body, try self.ctx.joinJump(self.done));
                        const otherwise = try self.emitTree(g.otherwise, env);
                        const cond = try self.ctx.guardTemp(g.guard);
                        const guard_switch = try self.ctx.boolSwitch(cond, body, otherwise);
                        const before = self.ctx.stmtCount();
                        const guarded = try self.ctx.lowerGuard(cond, g.guard, guard_switch);
                        self.delegated_stmts += self.ctx.stmtCount() - before;
                        return try self.emitBinds(g.binds, env, guarded);
                    },
                    .test_ => |t| return try self.emitTest(t, env),
                    .len_check => |lc| {
                        var then_env = try env.clone(self.arena);
                        var extractions: std.ArrayList(Extraction) = .empty;
                        try self.enterScope(&then_env, .{ .list_len = .{ .occ = lc.occ, .min_len = lc.min_len } }, &extractions);
                        var then = try self.emitTree(lc.then, &then_env);
                        then = try self.emitExtractions(extractions.items, &then_env, then);
                        const otherwise = try self.emitTree(lc.otherwise, env);
                        return try self.ctx.lenGteTest(self.lenLocal(env, lc.occ), lc.min_len, then, otherwise);
                    },
                    .exit_join => |j| {
                        const jp = self.ctx.freshJoinPointId();
                        try self.exit_joins.put(j.id, jp);
                        const cont = try self.emitTree(j.cont, env);
                        const inner = try self.emitTree(j.inner, env);
                        return try self.ctx.addExitJoin(jp, cont, inner);
                    },
                    .exit_ => |id| return try self.ctx.joinJump(self.exit_joins.get(id).?),
                    .fail => return try self.ctx.failTerminal(),
                }
            }

            fn emitTest(self: *Emitter, t: TestNode, env: *Env) Ctx.LowerError!CtxStmt {
                switch (t.kind) {
                    .tag, .callable => return try self.emitDiscTest(t, env),
                    .int_switch => return try self.emitIntSwitch(t, env),
                    .eq_chain => return try self.emitEqChain(t, env),
                    .str_set => return try self.emitStrSet(t, env),
                    .list_len => return try self.emitLenSwitch(t, env),
                }
            }

            fn emitArm(self: *Emitter, t: TestNode, arm: Arm, env: *Env) Ctx.LowerError!CtxStmt {
                var arm_env = try env.clone(self.arena);
                var extractions: std.ArrayList(Extraction) = .empty;
                const scope: Scope = switch (t.kind) {
                    .tag => .{ .tag_arm = .{ .occ = t.occ, .variant = @intCast(arm.key) } },
                    .callable => .{ .callable_arm = .{ .occ = t.occ, .variant = @intCast(arm.key) } },
                    .list_len => .{ .list_len = .{ .occ = t.occ, .min_len = @truncate(arm.key) } },
                    .int_switch, .eq_chain => {
                        // Literal arms impose no sub-structure; no new scope.
                        return try self.emitTree(arm.subtree, env);
                    },
                    .str_set => unreachable, // handled by emitStrSet
                };
                try self.enterScope(&arm_env, scope, &extractions);
                var body = try self.emitTree(arm.subtree, &arm_env);
                body = try self.emitExtractions(extractions.items, &arm_env, body);
                return body;
            }

            fn emitDiscTest(self: *Emitter, t: TestNode, env: *Env) Ctx.LowerError!CtxStmt {
                if (t.exhaustive and t.arms.len == 1) {
                    // The union has exactly one variant and the sole arm
                    // covers it: no dispatch, no discriminant read (a
                    // zero-branch switch is also rejected by the dev
                    // backend).
                    return try self.emitArm(t, t.arms[0], env);
                }
                const source = self.valueLocal(env, t.occ);
                if (self.ctx.isZstLirLocal(source)) {
                    // A ZST scrutinee has exactly one possible variant; no
                    // dispatch is needed (mirrors the chain's ZST fast path).
                    std.debug.assert(t.arms.len == 1);
                    return try self.emitArm(t, t.arms[0], env);
                }
                const arm_count = if (t.exhaustive) t.arms.len - 1 else t.arms.len;
                const values = try self.arena.alloc(u64, arm_count);
                const bodies = try self.arena.alloc(CtxStmt, arm_count);
                for (t.arms[0..arm_count], values, bodies) |arm, *value, *body| {
                    value.* = @intCast(arm.key);
                    body.* = try self.emitArm(t, arm, env);
                }
                const default = if (t.exhaustive)
                    try self.emitArm(t, t.arms[t.arms.len - 1], env)
                else
                    try self.emitTree(t.default.?, env);
                return try self.ctx.switchStmt(self.discLocal(env, t.occ), values, bodies, default);
            }

            fn emitIntSwitch(self: *Emitter, t: TestNode, env: *Env) Ctx.LowerError!CtxStmt {
                const values = try self.arena.alloc(u64, t.arms.len);
                const bodies = try self.arena.alloc(CtxStmt, t.arms.len);
                for (t.arms, values, bodies) |arm, *value, *body| {
                    value.* = self.ctx.intSwitchValue(arm.example, arm.example_ty).?;
                    body.* = try self.emitArm(t, arm, env);
                }
                const default = try self.emitTree(t.default.?, env);
                return try self.ctx.switchStmt(self.valueLocal(env, t.occ), values, bodies, default);
            }

            fn emitEqChain(self: *Emitter, t: TestNode, env: *Env) Ctx.LowerError!CtxStmt {
                const source = self.valueLocal(env, t.occ);
                var current = try self.emitTree(t.default.?, env);
                var i = t.arms.len;
                while (i > 0) {
                    i -= 1;
                    const arm = t.arms[i];
                    const body = try self.emitArm(t, arm, env);
                    current = try self.ctx.literalEqTest(source, arm.example_ty, arm.example, body, current);
                }
                return current;
            }

            fn emitStrSet(self: *Emitter, t: TestNode, env: *Env) Ctx.LowerError!CtxStmt {
                const arms = try self.arena.alloc(Ctx.StrArm, t.arms.len);
                for (t.arms, arms) |arm, *out| {
                    var arm_env = try env.clone(self.arena);
                    const capture_locals = try self.ctx.strArmCaptureLocals(arm.example);
                    // Register capture locals under their occurrences before
                    // entering the subtree; only interned (used) captures
                    // matter.
                    const shape: u32 = @truncate(arm.key);
                    for (capture_locals, 0..) |maybe_local, step_index| {
                        const local = maybe_local orelse continue;
                        if (self.lookupOcc(t.occ, .{ .str_capture = .{ .shape = shape, .index = @intCast(step_index) } })) |occ| {
                            try arm_env.locals.put(self.arena, occ, .{ .value = local });
                        }
                    }
                    var extractions: std.ArrayList(Extraction) = .empty;
                    try self.enterScope(&arm_env, .{ .str_arm = .{ .occ = t.occ, .shape = shape } }, &extractions);
                    var body = try self.emitTree(arm.subtree, &arm_env);
                    body = try self.emitExtractions(extractions.items, &arm_env, body);
                    out.* = try self.ctx.buildStrArm(arm.example, capture_locals, body);
                }
                const on_miss = try self.emitTree(t.default.?, env);
                return try self.ctx.strMatchSet(self.valueLocal(env, t.occ), arms, on_miss);
            }

            fn emitLenSwitch(self: *Emitter, t: TestNode, env: *Env) Ctx.LowerError!CtxStmt {
                const values = try self.arena.alloc(u64, t.arms.len);
                const bodies = try self.arena.alloc(CtxStmt, t.arms.len);
                for (t.arms, values, bodies) |arm, *value, *body| {
                    value.* = @truncate(arm.key);
                    body.* = try self.emitArm(t, arm, env);
                }
                const default = try self.emitTree(t.default.?, env);
                return try self.ctx.switchStmt(self.lenLocal(env, t.occ), values, bodies, default);
            }

            /// Look up an interned occurrence without creating it. Returns
            /// null when the capture was never referenced by any row.
            fn lookupOcc(self: *const Emitter, parent: OccId, step: Step) ?OccId {
                for (self.occs, 0..) |entry, i| {
                    if (entry.parent != parent) continue;
                    if (std.meta.eql(entry.step, step)) return @enumFromInt(i);
                }
                return null;
            }
        };
    };
}

// Construction unit tests over a mock accessor context.

const MockPat = union(enum) {
    bind: u32,
    wildcard,
    as_pattern: struct { pattern: u32, local: u32 },
    record: []const MockSub,
    tuple: []const MockSub,
    nominal: MockSub,
    tag: struct { variant: u16, payloads: []const MockSub },
    int_lit: struct { value: i64, switchable: bool = true },
    dec_lit: i128,
    str_lit: u32,
    str_pattern: struct { shape: u32, captures: []const ?u32 },
    list: struct { elems: []const u32, rest: ?struct { index: u32, pattern: ?u32 } },
};

const MockSub = struct { index: u16, ty: u32, pat: u32 };

const MockCtx = struct {
    pats: []const MockPat,
    variant_counts: []const ?u32 = &.{},

    pub const PatId = u32;
    pub const TypeId = u32;
    pub const ExprId = u32;
    pub const LocalId = u32;
    pub const LowerError = error{OutOfMemory};

    fn get(self: MockCtx, pat: u32) MockPat {
        return self.pats[pat];
    }

    pub fn patKind(self: MockCtx, pat: u32) PatKind {
        return switch (self.get(pat)) {
            .bind => .bind,
            .wildcard => .wildcard,
            .as_pattern => .as_pattern,
            .record => .record,
            .tuple => .tuple,
            .nominal => .nominal,
            .tag => .tag,
            .int_lit => .int_lit,
            .dec_lit => .dec_lit,
            .str_lit => .str_lit,
            .str_pattern => .str_pattern,
            .list => .list,
        };
    }

    pub fn bindLocal(self: MockCtx, pat: u32) u32 {
        return self.get(pat).bind;
    }

    pub fn asInfo(self: MockCtx, pat: u32) struct { pattern: u32, local: u32 } {
        const info = self.get(pat).as_pattern;
        return .{ .pattern = info.pattern, .local = info.local };
    }

    pub fn recordDestructCount(self: MockCtx, pat: u32) u16 {
        return @intCast(self.get(pat).record.len);
    }

    pub fn recordDestruct(self: MockCtx, pat: u32, _: u32, i: u16) LowerError!MockSub {
        return self.get(pat).record[i];
    }

    pub fn tupleItemCount(self: MockCtx, pat: u32) u16 {
        return @intCast(self.get(pat).tuple.len);
    }

    pub fn tupleItem(self: MockCtx, pat: u32, _: u32, i: u16) LowerError!MockSub {
        return self.get(pat).tuple[i];
    }

    pub fn nominalInner(self: MockCtx, pat: u32, _: u32) LowerError!MockSub {
        return self.get(pat).nominal;
    }

    pub fn tagVariant(self: MockCtx, pat: u32, _: u32) u16 {
        return self.get(pat).tag.variant;
    }

    pub fn tagPayloadCount(self: MockCtx, pat: u32) u16 {
        return @intCast(self.get(pat).tag.payloads.len);
    }

    pub fn tagPayload(self: MockCtx, pat: u32, _: u32, i: u16) LowerError!MockSub {
        return self.get(pat).tag.payloads[i];
    }

    pub fn tagVariantCount(self: MockCtx, ty: u32) ?u32 {
        if (ty < self.variant_counts.len) return self.variant_counts[ty];
        return null;
    }

    pub fn callableVariant(_: MockCtx, _: u32, _: u32) u16 {
        unreachable;
    }

    pub fn callablePayload(_: MockCtx, _: u32, _: u32) LowerError!?MockSub {
        unreachable;
    }

    pub fn callableVariantCount(_: MockCtx, _: u32) ?u32 {
        return null;
    }

    pub const ListPatView = struct {
        fixed_count: u32,
        rest: ?struct { index: u32, pattern: ?u32 },
    };

    pub fn listView(self: MockCtx, pat: u32) ListPatView {
        const list = self.get(pat).list;
        return .{ .fixed_count = @intCast(list.elems.len), .rest = if (list.rest) |r| .{ .index = r.index, .pattern = r.pattern } else null };
    }

    pub fn listElemPat(self: MockCtx, pat: u32, i: u32) u32 {
        return self.get(pat).list.elems[i];
    }

    pub fn listElemTy(_: MockCtx, ty: u32) u32 {
        return ty + 100; // arbitrary distinct element type id
    }

    pub fn ctorKey(self: MockCtx, pat: u32, _: u32) LowerError!u128 {
        return switch (self.get(pat)) {
            .tag => |t| t.variant,
            .int_lit => |i| @bitCast(@as(i128, i.value)),
            .dec_lit => |d| @bitCast(d),
            .str_lit => |s| s,
            .str_pattern => |s| s.shape,
            .list => |l| l.elems.len,
            else => unreachable,
        };
    }

    pub fn intSwitchValue(self: MockCtx, pat: u32, _: u32) ?u64 {
        const lit = self.get(pat).int_lit;
        if (!lit.switchable) return null;
        return @bitCast(lit.value);
    }

    pub fn strLitIsSetArm(_: MockCtx) bool {
        return true;
    }

    pub fn strCaptureCount(self: MockCtx, pat: u32) u16 {
        return switch (self.get(pat)) {
            .str_pattern => |s| @intCast(s.captures.len),
            else => 0,
        };
    }

    pub fn strCapturePat(self: MockCtx, pat: u32, i: u16) ?u32 {
        return self.get(pat).str_pattern.captures[i];
    }
};

const TestCompiler = Compiler(MockCtx);

fn buildMock(
    arena: std.mem.Allocator,
    ctx: MockCtx,
    scrutinee_ty: u32,
    branches: []const TestCompiler.Branch,
) error{OutOfMemory}!TestCompiler.BuildResult {
    return try TestCompiler.build(arena, ctx, scrutinee_ty, branches);
}

fn mockBranches(comptime n: usize, pats: [n]u32) [n]TestCompiler.Branch {
    var out: [n]TestCompiler.Branch = undefined;
    for (pats, 0..) |pat, i| {
        out[i] = .{ .pat = pat, .guard = null, .body = @intCast(i), .branch_index = @intCast(i) };
    }
    return out;
}

test "N tag branches with wildcard become one multiway test" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const ctx = MockCtx{
        .pats = &.{
            .{ .tag = .{ .variant = 0, .payloads = &.{} } },
            .{ .tag = .{ .variant = 1, .payloads = &.{} } },
            .{ .tag = .{ .variant = 2, .payloads = &.{} } },
            .wildcard,
        },
        .variant_counts = &.{null}, // scrutinee ty 0: open (variant count unknown)
    };
    const branches = mockBranches(4, .{ 0, 1, 2, 3 });
    const result = try buildMock(arena, ctx, 0, &branches);

    // One test node with three arms; the default is the spliced wildcard leaf.
    const node = result.tree.test_;
    try std.testing.expectEqual(TestKind.tag, node.kind);
    try std.testing.expectEqual(@as(usize, 3), node.arms.len);
    try std.testing.expect(!node.exhaustive);
    try std.testing.expectEqual(@as(u32, 3), node.default.?.leaf.branch_index);
    for (node.arms, 0..) |arm, i| {
        try std.testing.expectEqual(@as(u128, i), arm.key);
        try std.testing.expectEqual(@as(u32, @intCast(i)), arm.subtree.leaf.branch_index);
    }
    try std.testing.expectEqual(@as(u32, 0), result.stats.fail_refs);
}

test "closed tag match is exhaustive with no default and no fail" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const ctx = MockCtx{
        .pats = &.{
            .{ .tag = .{ .variant = 0, .payloads = &.{} } },
            .{ .tag = .{ .variant = 1, .payloads = &.{} } },
        },
        .variant_counts = &.{2},
    };
    const branches = mockBranches(2, .{ 0, 1 });
    const result = try buildMock(arena, ctx, 0, &branches);

    const node = result.tree.test_;
    try std.testing.expect(node.exhaustive);
    try std.testing.expect(node.default == null);
    try std.testing.expectEqual(@as(u32, 0), result.stats.fail_refs);
}

test "open tag match without wildcard keeps a fail default" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const ctx = MockCtx{
        .pats = &.{
            .{ .tag = .{ .variant = 0, .payloads = &.{} } },
            .{ .tag = .{ .variant = 1, .payloads = &.{} } },
        },
        .variant_counts = &.{3},
    };
    const branches = mockBranches(2, .{ 0, 1 });
    const result = try buildMock(arena, ctx, 0, &branches);

    const node = result.tree.test_;
    try std.testing.expect(!node.exhaustive);
    try std.testing.expect(node.default.?.* == .fail);
    try std.testing.expectEqual(@as(u32, 1), result.stats.fail_refs);
}

test "duplicate constructors merge into one arm preserving row order" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    // match { A if g => 0, A => 1, B => 2 }
    const ctx = MockCtx{
        .pats = &.{
            .{ .tag = .{ .variant = 0, .payloads = &.{} } },
            .{ .tag = .{ .variant = 0, .payloads = &.{} } },
            .{ .tag = .{ .variant = 1, .payloads = &.{} } },
        },
        .variant_counts = &.{2},
    };
    var branches = mockBranches(3, .{ 0, 1, 2 });
    branches[0].guard = 77;
    const result = try buildMock(arena, ctx, 0, &branches);

    const node = result.tree.test_;
    try std.testing.expectEqual(@as(usize, 2), node.arms.len);
    try std.testing.expect(node.exhaustive);
    // Arm A: guard node for row 0 whose otherwise is row 1's leaf.
    const arm_a = node.arms[0].subtree.guard;
    try std.testing.expectEqual(@as(u32, 0), arm_a.branch_index);
    try std.testing.expectEqual(@as(u32, 77), arm_a.guard);
    try std.testing.expectEqual(@as(u32, 1), arm_a.otherwise.leaf.branch_index);
    try std.testing.expectEqual(@as(u32, 2), node.arms[1].subtree.leaf.branch_index);
}

test "guarded wildcard row splits tag groups and shares the continuation" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    // match { A => 0, _ if g => 1, B => 2, _ => 3 }
    const ctx = MockCtx{
        .pats = &.{
            .{ .tag = .{ .variant = 0, .payloads = &.{} } },
            .wildcard,
            .{ .tag = .{ .variant = 1, .payloads = &.{} } },
            .wildcard,
        },
        .variant_counts = &.{3},
    };
    var branches = mockBranches(4, .{ 0, 1, 2, 3 });
    branches[1].guard = 42;
    const result = try buildMock(arena, ctx, 0, &branches);

    // Group 1 is a one-arm tag test; its sole miss reference is the default,
    // so the continuation (the guard row and everything below) splices in
    // directly with no exit join. The guard's otherwise chains to the second
    // tag test with the final wildcard as its default.
    const first = result.tree.test_;
    try std.testing.expectEqual(@as(usize, 1), first.arms.len);
    try std.testing.expectEqual(@as(u32, 0), first.arms[0].subtree.leaf.branch_index);

    const guard = first.default.?.guard;
    try std.testing.expectEqual(@as(u32, 1), guard.branch_index);
    const second = guard.otherwise.test_;
    try std.testing.expectEqual(@as(usize, 1), second.arms.len);
    try std.testing.expectEqual(@as(u32, 2), second.arms[0].subtree.leaf.branch_index);
    try std.testing.expectEqual(@as(u32, 3), second.default.?.leaf.branch_index);
    try std.testing.expectEqual(@as(u32, 0), result.stats.fail_refs);
}

test "tag payloads specialize into payload occurrence columns" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    // match { Ok(1) => 0, Ok(x) => 1, Err => 2 } — nested int test inside Ok.
    const ctx = MockCtx{
        .pats = &.{
            .{ .tag = .{ .variant = 0, .payloads = &.{.{ .index = 0, .ty = 9, .pat = 3 }} } },
            .{ .tag = .{ .variant = 0, .payloads = &.{.{ .index = 0, .ty = 9, .pat = 4 }} } },
            .{ .tag = .{ .variant = 1, .payloads = &.{} } },
            .{ .int_lit = .{ .value = 1 } },
            .{ .bind = 5 },
        },
        .variant_counts = &.{2},
    };
    const branches = mockBranches(3, .{ 0, 1, 2 });
    const result = try buildMock(arena, ctx, 0, &branches);

    const node = result.tree.test_;
    try std.testing.expect(node.exhaustive);
    const ok_arm = node.arms[0].subtree.test_;
    try std.testing.expectEqual(TestKind.int_switch, ok_arm.kind);
    try std.testing.expectEqual(@as(usize, 1), ok_arm.arms.len);
    try std.testing.expectEqual(@as(u32, 0), ok_arm.arms[0].subtree.leaf.branch_index);
    // Default of the inner int test is the spliced Ok(x) leaf with its bind.
    const fallback = ok_arm.default.?.leaf;
    try std.testing.expectEqual(@as(u32, 1), fallback.branch_index);
    try std.testing.expectEqual(@as(usize, 1), fallback.binds.len);
    try std.testing.expectEqual(@as(u32, 5), fallback.binds[0].local);
    // The payload occurrence was interned under the scrutinee root.
    const payload_occ = result.occs[fallback.binds[0].occ.idx()];
    try std.testing.expectEqual(OccId.root, payload_occ.parent);
    try std.testing.expectEqual(@as(u16, 0), payload_occ.step.tag_payload.variant);
    try std.testing.expect(payload_occ.step.tag_payload.single);
}

test "string arms with identical shapes merge and retry on guard failure" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    // match { "${a}!" if g => 0, "${b}!" => 1, "x" => 2, _ => 3 }
    const ctx = MockCtx{
        .pats = &.{
            .{ .str_pattern = .{ .shape = 7, .captures = &.{0xA} } },
            .{ .str_pattern = .{ .shape = 7, .captures = &.{0xB} } },
            .{ .str_lit = 3 },
            .wildcard,
            // capture binds (ids 4/5 unused; captures reference pat ids 10/11 below)
        },
        .variant_counts = &.{},
    };
    // Extend pats with capture bind patterns at ids 0xA and 0xB.
    var pats: [12]MockPat = undefined;
    for (ctx.pats, 0..) |p, i| pats[i] = p;
    pats[4] = .wildcard;
    pats[5] = .wildcard;
    pats[6] = .wildcard;
    pats[7] = .wildcard;
    pats[8] = .wildcard;
    pats[9] = .wildcard;
    pats[0xA] = .{ .bind = 100 };
    pats[0xB] = .{ .bind = 101 };
    const full_ctx = MockCtx{ .pats = &pats, .variant_counts = &.{} };

    var branches = mockBranches(4, .{ 0, 1, 2, 3 });
    branches[0].guard = 55;
    const result = try buildMock(arena, full_ctx, 0, &branches);

    const node = result.tree.test_;
    try std.testing.expectEqual(TestKind.str_set, node.kind);
    try std.testing.expectEqual(@as(usize, 2), node.arms.len);
    // First arm: shape 7; rows 0 (guarded) then 1 merged in order.
    const first_arm = node.arms[0].subtree.guard;
    try std.testing.expectEqual(@as(u32, 0), first_arm.branch_index);
    try std.testing.expectEqual(@as(u32, 1), first_arm.otherwise.leaf.branch_index);
    try std.testing.expectEqual(@as(usize, 1), first_arm.otherwise.leaf.binds.len);
    try std.testing.expectEqual(@as(u32, 101), first_arm.otherwise.leaf.binds[0].local);
    // Guard failure retries the SAME arm's later row — not the group default.
    try std.testing.expectEqual(@as(u32, 2), node.arms[1].subtree.leaf.branch_index);
    try std.testing.expectEqual(@as(u32, 3), node.default.?.leaf.branch_index);
}

test "list exact lengths bucket into one length test with rest row below" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    // match { [] => 0, [_] => 1, [_, ..] => 2, [] => dead }
    const ctx = MockCtx{
        .pats = &.{
            .{ .list = .{ .elems = &.{}, .rest = null } },
            .{ .list = .{ .elems = &.{4}, .rest = null } },
            .{ .list = .{ .elems = &.{4}, .rest = .{ .index = 1, .pattern = null } } },
            .{ .list = .{ .elems = &.{}, .rest = null } },
            .wildcard,
        },
    };
    const branches = mockBranches(4, .{ 0, 1, 2, 3 });
    const result = try buildMock(arena, ctx, 0, &branches);

    // Group 1: exact lengths 0 and 1; the rest row breaks the group and
    // becomes a len_check spliced in as the group default (single miss
    // reference, no join). The trailing duplicate [] row lands in its own
    // dead group below the rest row.
    const node = result.tree.test_;
    try std.testing.expectEqual(TestKind.list_len, node.kind);
    try std.testing.expectEqual(@as(usize, 2), node.arms.len);
    try std.testing.expectEqual(@as(u128, 0), node.arms[0].key);
    try std.testing.expectEqual(@as(u32, 0), node.arms[0].subtree.leaf.branch_index);
    try std.testing.expectEqual(@as(u128, 1), node.arms[1].key);
    try std.testing.expectEqual(@as(u32, 1), node.arms[1].subtree.leaf.branch_index);

    const rest = node.default.?.len_check;
    try std.testing.expectEqual(@as(u64, 1), rest.min_len);
    try std.testing.expectEqual(@as(u32, 2), rest.then.leaf.branch_index);
    // The dead trailing [] group still hangs off the rest row's otherwise.
    const dead = rest.otherwise.test_;
    try std.testing.expectEqual(@as(u32, 3), dead.arms[0].subtree.leaf.branch_index);
    try std.testing.expect(dead.default.?.* == .fail);
}

test "leading rest row forms a len_check with the remaining rows below" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    // match { [x, ..] => 0, [] => 1 }
    const ctx = MockCtx{
        .pats = &.{
            .{ .list = .{ .elems = &.{2}, .rest = .{ .index = 1, .pattern = null } } },
            .{ .list = .{ .elems = &.{}, .rest = null } },
            .{ .bind = 9 },
        },
    };
    const branches = mockBranches(2, .{ 0, 1 });
    const result = try buildMock(arena, ctx, 0, &branches);

    // The rest row's then-branch never misses, so the continuation splices
    // into the otherwise slot with no join.
    const check = result.tree.len_check;
    try std.testing.expectEqual(@as(u64, 1), check.min_len);
    const then_leaf = check.then.leaf;
    try std.testing.expectEqual(@as(u32, 0), then_leaf.branch_index);
    try std.testing.expectEqual(@as(usize, 1), then_leaf.binds.len);
    const below = check.otherwise.test_;
    try std.testing.expectEqual(TestKind.list_len, below.kind);
    try std.testing.expectEqual(@as(u32, 1), below.arms[0].subtree.leaf.branch_index);
}

test "continuation referenced from two miss sites keeps its exit join" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    // match { Ok(1) => 0, _ => 1 }: the wildcard row is reachable both from
    // the tag test's default (scrutinee is Err) and from the inner literal
    // test's miss (scrutinee is Ok(n), n != 1). The continuation must compile
    // once behind an exit join — re-lowering it would violate the sharing
    // invariant.
    const ctx = MockCtx{
        .pats = &.{
            .{ .tag = .{ .variant = 0, .payloads = &.{.{ .index = 0, .ty = 9, .pat = 2 }} } },
            .wildcard,
            .{ .int_lit = .{ .value = 1 } },
        },
        .variant_counts = &.{2},
    };
    const branches = mockBranches(2, .{ 0, 1 });
    const result = try buildMock(arena, ctx, 0, &branches);

    const join = result.tree.exit_join;
    try std.testing.expectEqual(@as(u32, 1), join.cont.leaf.branch_index);
    const tag_test = join.inner.test_;
    try std.testing.expect(tag_test.default.?.* == .exit_);
    const inner = tag_test.arms[0].subtree.test_;
    try std.testing.expectEqual(TestKind.int_switch, inner.kind);
    try std.testing.expect(inner.default.?.* == .exit_);
    try std.testing.expectEqual(@as(u32, 0), result.stats.fail_refs);
}

test "unguarded irrefutable row makes later rows dead" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const ctx = MockCtx{
        .pats = &.{
            .{ .bind = 1 },
            .{ .tag = .{ .variant = 0, .payloads = &.{} } },
        },
        .variant_counts = &.{2},
    };
    const branches = mockBranches(2, .{ 0, 1 });
    const result = try buildMock(arena, ctx, 0, &branches);

    const leaf = result.tree.leaf;
    try std.testing.expectEqual(@as(u32, 0), leaf.branch_index);
    try std.testing.expectEqual(@as(u32, 0), result.stats.fail_refs);
}

test "records and tuples destructure without tests" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    // match { { x: 1, y } => 0, { x, y: 2 } => 1, { x, y } => 2 }
    const ctx = MockCtx{
        .pats = &.{
            .{ .record = &.{ .{ .index = 0, .ty = 5, .pat = 3 }, .{ .index = 1, .ty = 5, .pat = 4 } } },
            .{ .record = &.{ .{ .index = 0, .ty = 5, .pat = 5 }, .{ .index = 1, .ty = 5, .pat = 6 } } },
            .{ .record = &.{ .{ .index = 0, .ty = 5, .pat = 7 }, .{ .index = 1, .ty = 5, .pat = 8 } } },
            .{ .int_lit = .{ .value = 1 } }, // row0 field x
            .{ .bind = 20 }, // row0 field y
            .{ .bind = 21 }, // row1 field x
            .{ .int_lit = .{ .value = 2 } }, // row1 field y
            .{ .bind = 22 }, // row2 field x
            .{ .bind = 23 }, // row2 field y
        },
    };
    const branches = mockBranches(3, .{ 0, 1, 2 });
    const result = try buildMock(arena, ctx, 0, &branches);

    // First test: field 0 (x == 1); its default splices to the field-1 test
    // (y == 2) with the all-binds row as that test's default.
    const x_test = result.tree.test_;
    try std.testing.expectEqual(TestKind.int_switch, x_test.kind);
    const x_occ = result.occs[x_test.occ.idx()];
    try std.testing.expectEqual(@as(u16, 0), x_occ.step.field);
    try std.testing.expectEqual(@as(u32, 0), x_test.arms[0].subtree.leaf.branch_index);

    const y_test = x_test.default.?.test_;
    const y_occ = result.occs[y_test.occ.idx()];
    try std.testing.expectEqual(@as(u16, 1), y_occ.step.field);
    try std.testing.expectEqual(@as(u32, 1), y_test.arms[0].subtree.leaf.branch_index);
    try std.testing.expectEqual(@as(u32, 2), y_test.default.?.leaf.branch_index);
    try std.testing.expectEqual(@as(u32, 0), result.stats.fail_refs);
}

test "column selection prefers the longer run" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    // Two-column matrix via tuples: rows (A, 1) / (B, 1) / (C, 1): the first
    // column runs 3 deep, the second (int) also runs 3 — first wins ties.
    // Make column 2 run longer: (A, 1) / (_, 2) / would break col0... instead:
    // rows: (A, 1), (B, 2), (B, 3): col0 run = 3, col1 run = 3; tie -> col0.
    const ctx = MockCtx{
        .pats = &.{
            .{ .tuple = &.{ .{ .index = 0, .ty = 1, .pat = 3 }, .{ .index = 1, .ty = 2, .pat = 6 } } },
            .{ .tuple = &.{ .{ .index = 0, .ty = 1, .pat = 4 }, .{ .index = 1, .ty = 2, .pat = 7 } } },
            .{ .tuple = &.{ .{ .index = 0, .ty = 1, .pat = 5 }, .{ .index = 1, .ty = 2, .pat = 8 } } },
            .{ .tag = .{ .variant = 0, .payloads = &.{} } },
            .{ .tag = .{ .variant = 1, .payloads = &.{} } },
            .{ .tag = .{ .variant = 1, .payloads = &.{} } },
            .{ .int_lit = .{ .value = 1 } },
            .{ .int_lit = .{ .value = 2 } },
            .{ .int_lit = .{ .value = 3 } },
        },
        .variant_counts = &.{ null, 2 },
    };
    const branches = mockBranches(3, .{ 0, 1, 2 });
    const result = try buildMock(arena, ctx, 1, &branches);

    const node = result.tree.test_;
    try std.testing.expectEqual(TestKind.tag, node.kind);
    const occ = result.occs[node.occ.idx()];
    try std.testing.expectEqual(@as(u16, 0), occ.step.field);
    // Arm B contains rows 1 and 2, distinguished by the int column inside.
    const arm_b = node.arms[1].subtree.test_;
    try std.testing.expectEqual(TestKind.int_switch, arm_b.kind);
    try std.testing.expectEqual(@as(usize, 2), arm_b.arms.len);
}

test "non-switchable ints fall back to eq_chain kind" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const ctx = MockCtx{
        .pats = &.{
            .{ .int_lit = .{ .value = 1, .switchable = false } },
            .{ .int_lit = .{ .value = 2, .switchable = false } },
            .wildcard,
        },
    };
    const branches = mockBranches(3, .{ 0, 1, 2 });
    const result = try buildMock(arena, ctx, 0, &branches);

    const node = result.tree.test_;
    try std.testing.expectEqual(TestKind.eq_chain, node.kind);
    try std.testing.expectEqual(@as(usize, 2), node.arms.len);
    try std.testing.expectEqual(@as(u32, 2), node.default.?.leaf.branch_index);
}

test "pattern node stats count normalized nodes" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const ctx = MockCtx{
        .pats = &.{
            .{ .tag = .{ .variant = 0, .payloads = &.{.{ .index = 0, .ty = 9, .pat = 1 }} } },
            .{ .bind = 4 },
            .wildcard,
        },
        .variant_counts = &.{2},
    };
    const branches = mockBranches(2, .{ 0, 2 });
    const result = try buildMock(arena, ctx, 0, &branches);
    // Row 0: tag col (1 node at normalize) + payload bind (1 node during
    // specialization); row 1: wildcard (1 node).
    try std.testing.expectEqual(@as(u32, 3), result.stats.pattern_nodes);
}
