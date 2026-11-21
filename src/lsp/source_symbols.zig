//! Source occurrences for LSP refactoring. Checked CIR supplies binding identity;
//! the parser supplies exact written tokens, never canonical qualified spellings.
//! This index is built only by the language server and never enters compiler caches.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const parse = @import("parse");
const collections = @import("collections");
const queries = @import("cir_queries.zig");
const position = @import("position.zig");
const module_lookup = @import("module_lookup.zig");
const CirVisitor = @import("cir_visitor.zig").CirVisitor;
const VisitAction = @import("cir_visitor.zig").VisitAction;
const Allocator = std.mem.Allocator;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const Region = base.Region;

/// Where a method call's implementation lives, as far as a source rename can tell.
pub const MethodTarget = union(enum) {
    local: CIR.Pattern.Idx,
    external,
    unresolved,
};

/// Resolves a receiver method call to the binding that implements it.
pub const ResolveMethod = *const fn (env: *ModuleEnv, receiver: CIR.Expr.Idx, method_name: base.Ident.Idx) MethodTarget;

/// A renameable binding's source name and the region where it is declared.
pub const Binding = struct {
    name: []const u8,
    region: Region,
    associated: bool,
};

/// One written occurrence of a binding's name in the document source.
pub const Occurrence = struct {
    region: Region,
    pattern: CIR.Pattern.Idx,
    declaration: bool,

    fn lessThan(_: void, a: Occurrence, b: Occurrence) bool {
        if (a.region.start.offset != b.region.start.offset) return a.region.start.offset < b.region.start.offset;
        if (a.region.end.offset != b.region.end.offset) return a.region.end.offset < b.region.end.offset;
        if (a.pattern != b.pattern) return @backingInt(a.pattern) < @backingInt(b.pattern);
        return @intFromBool(a.declaration) < @intFromBool(b.declaration);
    }
};

/// An immutable checked document's source-level binding occurrences.
pub const Index = struct {
    allocator: Allocator,
    env: *ModuleEnv,
    bindings: collections.DenseMapUnmanaged(CIR.Pattern.Idx, Binding) = .empty,
    occurrences: std.ArrayList(Occurrence) = .empty,
    unresolved_methods: std.ArrayList([]const u8) = .empty,

    pub fn deinit(self: *Index) void {
        self.bindings.deinit(self.allocator);
        self.occurrences.deinit(self.allocator);
        self.unresolved_methods.deinit(self.allocator);
    }

    pub fn init(allocator: Allocator, env: *ModuleEnv, resolve_method: ResolveMethod) Allocator.Error!Index {
        var result = Index{ .allocator = allocator, .env = env };
        errdefer result.deinit();

        var parse_env = try ModuleEnv.init(allocator, env.common.source);
        defer parse_env.deinit();
        const ast = try parse.file(allocator, &parse_env.common);
        defer ast.deinit();

        var walk = Walk{ .index = &result, .resolve_method = resolve_method };
        defer walk.read_names.deinit(allocator);
        defer walk.associated_names.deinit(allocator);

        // The parser records the final token separately from the qualifiers.
        // Index by the complete expression span, not by identifier text.
        for (ast.store.nodes.field(.tag), 0..) |tag, i| {
            if (tag != .ident) continue;
            const expr = ast.store.getExpr(@fromBackingInt(@intCast(i))).ident;
            const whole = ast.tokenizedRegionToRegion(expr.region);
            const token_region = ast.tokens.resolve(expr.token);
            const ident = ast.tokens.resolveIdentifier(expr.token) orelse continue;
            const name = parse_env.common.idents.getText(ident);
            const name_region = Region.from_raw_offsets(token_region.end.offset - @as(u32, @intCast(name.len)), token_region.end.offset);
            try walk.read_names.put(allocator, RegionKey.of(whole), name_region);
        }

        for (env.method_defs.entries.items()) |entry| {
            const def = env.store.getDef(entry.value.def_idx);
            try walk.associated_names.put(allocator, def.pattern, entry.key.methodIdent());
        }

        var visitor = CirVisitor(Walk).init(&walk, .{
            .visit_pattern_pre = Walk.pattern,
            .visit_stmt_pre = Walk.statement,
            .visit_expr_pre = Walk.expr,
        });
        for (env.store.sliceDefs(env.all_defs)) |def_idx| {
            const def = env.store.getDef(def_idx);
            visitor.walkPattern(&env.store, def.pattern);
            if (walk.oom) |err| return err;
            if (def.annotation) |anno| try walk.annotation(def.pattern, anno);
            visitor.walkExpr(&env.store, def.expr);
            if (walk.oom) |err| return err;
        }
        visitor.walkModule(&env.store, env.all_statements);
        if (walk.oom) |err| return err;
        for (env.store.write_occurrences.items()) |write| {
            try walk.append(write.pattern_idx, write.region(), false);
        }

        std.mem.sort(Occurrence, result.occurrences.items, {}, Occurrence.lessThan);
        var len: usize = 0;
        for (result.occurrences.items) |item| {
            if (len > 0 and std.meta.eql(result.occurrences.items[len - 1], item)) continue;
            result.occurrences.items[len] = item;
            len += 1;
        }
        result.occurrences.shrinkRetainingCapacity(len);
        return result;
    }

    pub fn at(self: *const Index, offset: u32) ?Occurrence {
        for (self.occurrences.items) |item| {
            // Editors also request rename with the caret just after the name.
            if (item.region.start.offset <= offset and offset <= item.region.end.offset) return item;
        }
        return null;
    }

    pub fn complete(self: *const Index, pattern: CIR.Pattern.Idx) bool {
        const binding = self.bindings.get(pattern) orelse return false;
        if (!binding.associated) return true;
        for (self.unresolved_methods.items) |name| {
            if (std.mem.eql(u8, name, binding.name)) return false;
        }
        return true;
    }

    pub fn ranges(self: *const Index, pattern: CIR.Pattern.Idx, include_declaration: bool) Allocator.Error![]queries.LspRange {
        var result: std.ArrayList(queries.LspRange) = .empty;
        errdefer result.deinit(self.allocator);
        const line_offsets = try position.LineOffsets.init(self.allocator, self.env.common.source);
        defer line_offsets.deinit();
        for (self.occurrences.items) |item| {
            if (item.pattern != pattern or (!include_declaration and item.declaration)) continue;
            if (queries.regionToRangeIn(&line_offsets, item.region)) |range| try result.append(self.allocator, range);
        }
        return result.toOwnedSlice(self.allocator);
    }

    pub fn edit(self: *const Index, pattern: CIR.Pattern.Idx, new_name: []const u8) Allocator.Error!Edit {
        var result = Edit{ .allocator = self.allocator };
        errdefer result.deinit();
        var previous: u32 = 0;
        for (self.occurrences.items) |item| {
            if (item.pattern != pattern) continue;
            try result.text.appendSlice(self.allocator, self.env.common.source[previous..item.region.start.offset]);
            try result.text.appendSlice(self.allocator, new_name);
            try result.regions.append(self.allocator, .{
                .before = item.region,
                .after_end = std.math.cast(u32, result.text.items.len) orelse return error.OutOfMemory,
            });
            previous = item.region.end.offset;
        }
        try result.text.appendSlice(self.allocator, self.env.common.source[previous..]);
        return result;
    }

    /// A successful check is not enough: a captured read can still type-check.
    /// Compare every recorded local occurrence, including unedited ones, by its
    /// declaration's source identity across the edit's exact offset mapping.
    pub fn preservesBindings(self: *const Index, candidate: *const Index, edit_: *const Edit) bool {
        if (self.occurrences.items.len != candidate.occurrences.items.len) return false;
        for (self.occurrences.items, candidate.occurrences.items) |before, after| {
            if (before.declaration != after.declaration or !std.meta.eql(edit_.mapRegion(before.region), after.region)) return false;
            const old_binding = self.env.store.getPatternRegion(before.pattern);
            const new_binding = candidate.env.store.getPatternRegion(after.pattern);
            if (!std.meta.eql(edit_.mapRegion(old_binding), new_binding)) return false;
        }
        // Canonicalization reports duplicate declarations and shadowing even
        // when no read was captured. Existing diagnostics remain permissible.
        const old_diags = self.env.store.sliceDiagnostics(self.env.diagnostics);
        const new_diags = candidate.env.store.sliceDiagnostics(candidate.env.diagnostics);
        if (old_diags.len != new_diags.len) return false;
        for (old_diags, new_diags) |before, after| {
            if (std.meta.activeTag(self.env.store.getDiagnostic(before)) != std.meta.activeTag(candidate.env.store.getDiagnostic(after))) return false;
            const before_region = self.env.store.getRegionAt(ModuleEnv.nodeIdxFrom(before));
            const after_region = candidate.env.store.getRegionAt(ModuleEnv.nodeIdxFrom(after));
            if (!std.meta.eql(edit_.mapRegion(before_region), after_region)) return false;
        }
        // Checking also records erroneous source nodes independently of
        // canonicalization diagnostics. An edit must not introduce a rejected
        // dispatch or a type error while leaving the lexical reads unchanged.
        const old_errors = self.env.erroneous_nodes.items();
        const new_errors = candidate.env.erroneous_nodes.items();
        if (old_errors.len != new_errors.len) return false;
        for (old_errors, new_errors) |before, after| {
            const before_region = self.env.store.getRegionAt(@fromBackingInt(before.node_idx));
            const after_region = candidate.env.store.getRegionAt(@fromBackingInt(after.node_idx));
            if (!std.meta.eql(edit_.mapRegion(before_region), after_region)) return false;
        }
        return true;
    }
};

/// Source edits and their monotone mapping from old to new byte boundaries.
pub const Edit = struct {
    allocator: Allocator,
    regions: std.ArrayList(struct { before: Region, after_end: u32 }) = .empty,
    text: std.ArrayList(u8) = .empty,

    pub fn deinit(self: *Edit) void {
        self.regions.deinit(self.allocator);
        self.text.deinit(self.allocator);
    }

    fn mapOffset(self: *const Edit, offset: u32) u32 {
        var low: usize = 0;
        var high = self.regions.items.len;
        while (low < high) {
            const mid = low + (high - low) / 2;
            if (self.regions.items[mid].before.end.offset <= offset) low = mid + 1 else high = mid;
        }
        if (low == 0) return offset;
        const change = self.regions.items[low - 1];
        return change.after_end + (offset - change.before.end.offset);
    }

    pub fn mapRegion(self: *const Edit, region: Region) Region {
        return Region.from_raw_offsets(self.mapOffset(region.start.offset), self.mapOffset(region.end.offset));
    }
};

const RegionKey = struct {
    start: u32,
    end: u32,

    fn of(region: Region) RegionKey {
        return .{ .start = region.start.offset, .end = region.end.offset };
    }
};

const Walk = struct {
    index: *Index,
    resolve_method: ResolveMethod,
    read_names: std.AutoHashMapUnmanaged(RegionKey, Region) = .{},
    associated_names: collections.DenseMapUnmanaged(CIR.Pattern.Idx, base.Ident.Idx) = .empty,
    oom: ?Allocator.Error = null,

    fn append(self: *Walk, pattern_idx: CIR.Pattern.Idx, region: Region, declaration: bool) Allocator.Error!void {
        try self.index.occurrences.append(self.index.allocator, .{ .pattern = pattern_idx, .region = region, .declaration = declaration });
    }

    fn bind(self: *Walk, pattern_idx: CIR.Pattern.Idx, region: Region) Allocator.Error!void {
        const env = self.index.env;
        const ident = self.associated_names.get(pattern_idx) orelse
            module_lookup.extractIdentFromPattern(&env.store, pattern_idx) orelse return;
        const name = env.common.idents.getText(ident);
        if (region.end.offset > env.common.source.len or region.start.offset > region.end.offset) return;
        if (!std.mem.eql(u8, env.getSource(region), name)) return;
        try self.index.bindings.put(self.index.allocator, pattern_idx, .{
            .name = name,
            .region = region,
            .associated = self.associated_names.contains(pattern_idx),
        });
        try self.append(pattern_idx, region, true);
    }

    fn annotation(self: *Walk, pattern_idx: CIR.Pattern.Idx, anno: CIR.Annotation.Idx) Allocator.Error!void {
        const region = self.index.env.store.getAnnotation(anno).name_region orelse return;
        if (!self.index.bindings.contains(pattern_idx)) try self.bind(pattern_idx, region);
        try self.append(pattern_idx, region, true);
    }

    fn pattern(self: *Walk, idx: CIR.Pattern.Idx, patt: CIR.Pattern) VisitAction {
        if (patt == .assign or patt == .var_assign) {
            self.bind(idx, self.index.env.store.getPatternRegion(idx)) catch |err| {
                self.oom = err;
                return .stop;
            };
        }
        return .continue_traversal;
    }

    fn statement(self: *Walk, _: CIR.Statement.Idx, stmt: CIR.Statement) VisitAction {
        const pattern_idx = module_lookup.getDeclarationPattern(stmt) orelse return .continue_traversal;
        const anno = switch (stmt) {
            .s_decl => |decl| decl.anno,
            .s_var => |decl| decl.anno,
            .s_var_uninitialized => |decl| decl.anno,
            .s_reassign,
            .s_crash,
            .s_dbg,
            .s_expr,
            .s_expect,
            .s_for,
            .s_while,
            .s_infinite_loop,
            .s_breakable_loop,
            .s_break,
            .s_return,
            .s_import,
            .s_alias_decl,
            .s_nominal_decl,
            .s_where_alias_decl,
            .s_type_anno,
            .s_type_var_alias,
            .s_runtime_error,
            => null,
        };
        if (anno) |idx| self.annotation(pattern_idx, idx) catch |err| {
            self.oom = err;
            return .stop;
        };
        return .continue_traversal;
    }

    fn expr(self: *Walk, idx: CIR.Expr.Idx, value: CIR.Expr) VisitAction {
        self.recordExpr(idx, value) catch |err| {
            self.oom = err;
            return .stop;
        };
        return .continue_traversal;
    }

    fn recordExpr(self: *Walk, idx: CIR.Expr.Idx, value: CIR.Expr) Allocator.Error!void {
        const env = self.index.env;
        switch (value) {
            .e_lookup_local => |lookup| {
                const region = self.read_names.get(RegionKey.of(env.store.getExprRegion(idx))) orelse return;
                try self.append(lookup.pattern_idx, region, false);
            },
            .e_lookup_associated_resolved => |lookup| {
                if (lookup.module_identity != env.selfModuleIdentity()) return;
                const region = self.read_names.get(RegionKey.of(env.store.getExprRegion(idx))) orelse return;
                try self.append(env.store.getDef(lookup.target_def_idx).pattern, region, false);
            },
            .e_method_call => |call| try self.methodCall(call.method_name, call.method_name_region, call.receiver),
            .e_dispatch_call => |call| try self.methodCall(call.method_name, call.method_name_region, call.receiver),
            .e_type_method_call => |call| try self.methodCall(call.method_name, call.method_name_region, null),
            .e_type_dispatch_call => |call| try self.methodCall(call.method_name, call.method_name_region, null),
            .e_method_eq, .e_structural_eq => try self.index.unresolved_methods.append(self.index.allocator, env.getIdent(env.idents.is_eq)),
            .e_structural_hash => try self.index.unresolved_methods.append(self.index.allocator, env.getIdent(env.idents.to_hash)),
            .e_dbg => try self.index.unresolved_methods.append(self.index.allocator, env.getIdent(env.idents.to_inspect)),
            .e_interpolation => try self.index.unresolved_methods.append(self.index.allocator, env.getIdent(env.idents.from_interpolation)),
            .e_num,
            .e_frac_f32,
            .e_frac_f64,
            .e_dec,
            .e_dec_small,
            .e_num_from_numeral,
            .e_typed_int,
            .e_typed_frac,
            .e_typed_num_from_numeral,
            .e_str_segment,
            .e_str,
            .e_bytes_literal,
            .e_lookup_external,
            .e_deferred_import_ref,
            .e_lookup_associated_local,
            .e_lookup_associated,
            .e_lookup_required,
            .e_list,
            .e_empty_list,
            .e_tuple,
            .e_match,
            .e_if,
            .e_call,
            .e_record,
            .e_empty_record,
            .e_block,
            .e_tag,
            .e_nominal,
            .e_nominal_external,
            .e_zero_argument_tag,
            .e_closure,
            .e_lambda,
            .e_binop,
            .e_unary_minus,
            .e_field_access,
            .e_tuple_access,
            .e_runtime_error,
            .e_crash,
            .e_expect_err,
            .e_expect,
            .e_ellipsis,
            .e_anno_only,
            .e_derived_method,
            .e_return,
            .e_break,
            .e_for,
            .e_hosted_lambda,
            .e_run_low_level,
            => {},
        }
    }

    fn methodCall(self: *Walk, method_name: base.Ident.Idx, method_name_region: Region, receiver: ?CIR.Expr.Idx) Allocator.Error!void {
        const env = self.index.env;
        const name = env.common.idents.getText(method_name);
        // Operators and other desugarings have no replaceable method
        // token. Changing their implementation's name cannot rewrite
        // the surface syntax as a name-for-name edit.
        if (!std.mem.eql(u8, env.getSource(method_name_region), name)) {
            try self.index.unresolved_methods.append(self.index.allocator, name);
            return;
        }
        const target: MethodTarget = if (receiver) |receiver_idx| self.resolve_method(env, receiver_idx, method_name) else .unresolved;
        switch (target) {
            .local => |patt| try self.append(patt, method_name_region, false),
            .external => {},
            .unresolved => try self.index.unresolved_methods.append(self.index.allocator, name),
        }
    }
};
