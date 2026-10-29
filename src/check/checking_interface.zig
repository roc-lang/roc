//! The producer's import-facing checking data. Types cross this boundary by
//! the same exact graph copy used by import checking, including descriptor
//! flags, nominal templates and dispatch metadata. Bodies contribute explicit
//! checking facts, never their implementation identity or evaluated values.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const types = @import("types");
const collections = @import("collections");
const dependencies = @import("checking_dependencies.zig");
const copy_import = @import("copy_import.zig");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Var = types.Var;
const CompactWriter = collections.CompactWriter;

/// The source checker owns this classification, shared with ordinary lookup.
pub const Definition = union(enum) {
    derived_method: @FieldType(CIR.Expr, "e_derived_method"),
    valueless,
    non_effectful_hosted,
    value,
};

/// The exact ordered environment scope supplied to checking.
pub fn makeScope(
    allocator: Allocator,
    env: *const ModuleEnv,
    builtin_env: ?*const ModuleEnv,
    imported: []const *const ModuleEnv,
    owners: []const *const ModuleEnv,
) Allocator.Error!dependencies.Scope {
    const own = env.contentIdentityHash() orelse
        base.invariant("checking interface publication follows module identity finalization", .{});
    var scope = try dependencies.Scope.init(allocator, own.*);
    errdefer scope.deinit();
    scope.imported = imported;
    if (builtin_env) |builtin_module| {
        if (!base.ModuleIdentity.eql(own, builtin_module.contentIdentityHash().?)) {
            try scope.appendDependency(builtin_module);
        }
    }
    inline for (.{ imported, owners }) |group| {
        dependencies.hashU32(&scope.hasher, std.math.cast(u32, group.len) orelse return error.OutOfMemory);
        for (group) |dependency| {
            if (base.ModuleIdentity.eql(own, dependency.contentIdentityHash().?)) {
                _ = try scope.appendOwner(own.*);
            } else {
                try scope.appendDependency(dependency);
            }
        }
    }
    return scope;
}

/// Publish one immutable checking dependency while source types are still live.
pub fn publish(
    env: *ModuleEnv,
    scope: *const dependencies.Scope,
    classify: *const fn (*const ModuleEnv, CIR.Def) Definition,
    binding_owner: *const fn (*const ModuleEnv, CIR.Node.Idx) Allocator.Error!?CIR.Def.Idx,
) Allocator.Error!void {
    var owners = try scope.copyOwners(env.gpa);
    errdefer owners.deinit(env.gpa);

    // Builtin is compiler-owned input, fixed for the compiler compatibility
    // domain. Its complete content identity is its checking contract.
    var references: collections.SafeList(u32) = .{};
    errdefer references.deinit(env.gpa);
    const digest = if (env.module_role == .builtin) env.contentIdentityHash().?.* else blk: {
        var publication = try Publication.init(env, scope);
        defer publication.deinit();
        try publication.writeInterface(classify, binding_owner);
        const result = try publication.finish();
        references = publication.references;
        publication.references = .{};
        break :blk result;
    };
    env.checking_dependency.deinit(env.gpa);
    env.checking_dependency = .{ .digest = digest, .owners = owners, .references = references, .published = true };
}

const Publication = struct {
    env: *const ModuleEnv,
    scope: *const dependencies.Scope,
    projected: ModuleEnv,
    copied: collections.DenseMap(Var, Var),
    seen_nodes: collections.DenseMap(CIR.Node.Idx, void),
    seen_annos: collections.DenseMap(CIR.TypeAnno.Idx, void),
    slots: collections.DenseMap(CIR.Node.Idx, u32),
    references: collections.SafeList(u32) = .{},
    annos: std.ArrayList(CIR.TypeAnno.Idx) = .empty,
    hasher: base.Sha256 = base.Sha256.init(.{}),

    fn init(env: *const ModuleEnv, scope: *const dependencies.Scope) Allocator.Error!Publication {
        var projected = try ModuleEnv.init(env.gpa, "");
        errdefer projected.deinit();
        try projected.initCIRFields("CheckingInterface");
        var result = Publication{
            .env = env,
            .scope = scope,
            .projected = projected,
            .copied = collections.DenseMap(Var, Var).init(env.gpa),
            .seen_nodes = collections.DenseMap(CIR.Node.Idx, void).init(env.gpa),
            .seen_annos = collections.DenseMap(CIR.TypeAnno.Idx, void).init(env.gpa),
            .slots = collections.DenseMap(CIR.Node.Idx, u32).init(env.gpa),
        };
        result.hasher.update("roc-checking-interface-v2");
        result.hasher.update(&scope.digest());
        return result;
    }

    fn deinit(self: *Publication) void {
        self.projected.deinit();
        self.copied.deinit();
        self.seen_nodes.deinit();
        self.seen_annos.deinit();
        self.slots.deinit();
        self.references.deinit(self.env.gpa);
        self.annos.deinit(self.env.gpa);
    }

    fn bytes(self: *Publication, data: []const u8) Allocator.Error!void {
        try dependencies.hashBytes(&self.hasher, data);
    }

    /// Field-by-field encoding, including every member of metadata records.
    /// Names and module ownership are canonical text and explicit parameters.
    fn value(self: *Publication, data: anytype) Allocator.Error!void {
        const T = @TypeOf(data);
        if (T == base.Ident.Idx) {
            try self.bytes(if (data.isNone()) "" else self.env.getIdent(data));
        } else if (T == base.ModuleIdentity.Idx) {
            dependencies.hashU32(&self.hasher, self.scope.ordinal(self.env.moduleIdentityHash(data).*));
        } else if (T == CIR.TypeAnno.LocalOrExternal) {
            try self.bytes(@tagName(data));
            switch (data) {
                .local => |local| try self.node(local.decl_idx),
                .external => |external| {
                    try self.value(external.module_idx);
                    dependencies.hashU32(&self.hasher, try self.importNode(external.module_idx, external.target_node_idx));
                },
                .external_identity => |external| {
                    try self.value(external.module_identity);
                    dependencies.hashU32(&self.hasher, try self.foreignNode(external.module_identity, external.target_node_idx));
                },
                .builtin => |builtin| try self.value(builtin),
                .pending => base.invariant("checking interface annotations have resolved imports", .{}),
            }
        } else if (T == CIR.Node.Idx or T == CIR.Statement.Idx or T == CIR.Def.Idx or T == CIR.Pattern.Idx or T == CIR.Expr.Idx or T == CIR.TypeAnno.Idx) {
            try self.node(data);
        } else if (T == Var) {
            try self.ty(data);
        } else switch (@typeInfo(T)) {
            .bool => self.hasher.update(&.{@intFromBool(data)}),
            .int => {
                const Encoded = @Int(@typeInfo(T).int.signedness, ((@bitSizeOf(T) + 7) / 8) * 8);
                var encoded: [@bitSizeOf(Encoded) / 8]u8 = undefined;
                std.mem.writeInt(Encoded, &encoded, data, .little);
                self.hasher.update(&encoded);
            },
            .@"enum" => try self.value(@backingInt(data)),
            .@"struct" => inline for (@typeInfo(T).@"struct".field_names) |field| try self.value(@field(data, field)),
            .@"union" => {
                try self.bytes(@tagName(data));
                inline for (@typeInfo(T).@"union".field_names) |field| {
                    if (data == @field(std.meta.Tag(T), field)) try self.value(@field(data, field));
                }
            },
            .optional => {
                self.hasher.update(&.{@intFromBool(data != null)});
                if (data) |present| try self.value(present);
            },
            .array => for (data) |item| try self.value(item),
            .void => {},
            .type,
            .noreturn,
            .float,
            .pointer,
            .error_union,
            .error_set,
            .@"fn",
            .@"opaque",
            .frame,
            .@"anyframe",
            .vector,
            .enum_literal,
            .comptime_int,
            .comptime_float,
            .undefined,
            .null,
            .spirv,
            => @compileError("checking interface metadata needs an explicit encoding for " ++ @typeName(T)),
        }
    }

    fn slot(self: *Publication, raw: u32) Allocator.Error!u32 {
        const entry = try self.slots.getOrPut(@fromBackingInt(raw));
        if (!entry.found_existing) {
            entry.value_ptr.* = std.math.cast(u32, self.references.items.items.len) orelse return error.OutOfMemory;
            _ = try self.references.append(self.env.gpa, raw);
        }
        return entry.value_ptr.*;
    }

    fn node(self: *Publication, id: anytype) Allocator.Error!void {
        dependencies.hashU32(&self.hasher, try self.slot(@backingInt(id)));
    }

    fn foreignNode(self: *Publication, owner: base.ModuleIdentity.Idx, raw: u32) Allocator.Error!u32 {
        if (owner == self.env.selfModuleIdentity()) return self.slot(raw);
        return self.foreignHash(self.env.moduleIdentityHash(owner).*, raw);
    }

    fn importNode(self: *Publication, import_idx: CIR.Import.Idx, raw: u32) Allocator.Error!u32 {
        const resolved = self.env.imports.getResolvedModule(import_idx) orelse
            base.invariant("published external annotation imports are resolved", .{});
        return self.foreignHash(self.scope.imported[resolved].contentIdentityHash().?.*, raw);
    }

    fn foreignHash(self: *Publication, hash: base.ModuleIdentity.Hash, raw: u32) Allocator.Error!u32 {
        const dependency = self.scope.environments.get(hash) orelse
            base.invariant("checking reference owners belong to the publication scope", .{});
        if (dependency.module_role == .builtin) return raw;
        for (dependency.checking_dependency.references.items.items, 0..) |node_id, ordinal| {
            if (node_id == raw) return @intCast(ordinal);
        }
        base.invariant("foreign checking references have producer-published slots", .{});
    }

    fn section(self: *Publication, name: []const u8, count: usize) Allocator.Error!void {
        try self.bytes(name);
        dependencies.hashU32(&self.hasher, std.math.cast(u32, count) orelse return error.OutOfMemory);
    }

    fn ty(self: *Publication, source: Var) Allocator.Error!void {
        const copied = try copy_import.copyVar(
            &self.env.types,
            &self.projected.types,
            source,
            &self.copied,
            null,
            self.env,
            &self.projected,
            self.env.gpa,
        );
        dependencies.hashU32(&self.hasher, @backingInt(copied));
    }

    fn scheme(self: *Publication, node_id: CIR.Node.Idx) Allocator.Error!void {
        try self.node(node_id);
        try self.ty(ModuleEnv.varFrom(node_id));
        try self.value(self.env.nodeIsBindingScheme(node_id));
        const requirements = self.env.bindingSchemeCodecRequirementsForNode(node_id);
        dependencies.hashU32(&self.hasher, std.math.cast(u32, requirements.len) orelse return error.OutOfMemory);
        for (requirements) |requirement| {
            try self.ty(@fromBackingInt(requirement.receiver_var));
            const constraint = self.env.types.getStaticDispatchConstraintAt(requirement.constraint_index);
            // These roots share the binding's one copy map, preserving evidence
            // identities that are not reachable from the ordinary callable root.
            try self.ty(constraint.fn_var);
            try self.value(constraint.fn_name);
            try self.value(constraint.origin);
            try self.value(requirement.requires_instantiation);
            try self.value(requirement.is_synthetic);
        }
    }

    fn definition(self: *Publication, id: CIR.Def.Idx, classify: *const fn (*const ModuleEnv, CIR.Def) Definition) Allocator.Error!void {
        try self.node(id);
        const entry = try self.seen_nodes.getOrPut(ModuleEnv.nodeIdxFrom(id));
        if (entry.found_existing) return;
        const def = self.env.store.getDef(id);
        try self.node(def.pattern);
        try self.node(def.expr);
        try self.value(classify(self.env, def));
        try self.value(def.annotation != null);
        if (def.annotation) |annotation_id| {
            try self.node(annotation_id);
            const annotation = self.env.store.getAnnotation(annotation_id);
            try self.value(annotation.erroneous);
            try self.value(annotation.mentions_type_var);
            try self.value(annotation.introduces_type_var);
            try self.value(annotation.contains_underscore);
            try self.anno(annotation.anno);
        }
        try self.scheme(ModuleEnv.nodeIdxFrom(def.pattern));
        try self.scheme(ModuleEnv.nodeIdxFrom(id));
        try self.ty(ModuleEnv.varFrom(def.expr));
        try self.value(self.env.defInvokes(id));
        try self.value(self.env.callableAllowsStoredConst(id));
        try self.value(self.env.diagnosticHostedReturn(id));
    }

    fn declaration(self: *Publication, id: CIR.Statement.Idx) Allocator.Error!void {
        try self.node(id);
        const entry = try self.seen_nodes.getOrPut(ModuleEnv.nodeIdxFrom(id));
        if (entry.found_existing) return;
        const statement = self.env.store.getStatement(id);
        try self.bytes(@tagName(statement));
        try self.scheme(ModuleEnv.nodeIdxFrom(id));
        switch (statement) {
            inline .s_alias_decl, .s_nominal_decl => |decl| {
                const header = self.env.store.getTypeHeader(decl.header);
                try self.value(header.name);
                try self.value(header.relative_name);
                try self.annoSpan(header.args);
                if (statement == .s_nominal_decl) try self.value(statement.s_nominal_decl.is_opaque);
                try self.anno(decl.anno);
            },
            .s_where_alias_decl => |decl| {
                const header = self.env.store.getTypeHeader(decl.header);
                try self.value(header.name);
                try self.value(header.relative_name);
                try self.annoSpan(header.args);
                try self.anno(decl.receiver);
                // The checked receiver carries the complete where constraints.
                try self.ty(ModuleEnv.varFrom(decl.receiver));
            },
            .s_runtime_error => {},
            .s_decl,
            .s_var,
            .s_var_uninitialized,
            .s_reassign,
            .s_expr,
            .s_dbg,
            .s_expect,
            .s_for,
            .s_while,
            .s_infinite_loop,
            .s_breakable_loop,
            .s_return,
            .s_import,
            .s_type_anno,
            .s_type_var_alias,
            .s_crash,
            .s_break,
            => base.invariant("checking interface type declarations have canonical declaration kinds", .{}),
        }
    }

    fn annoSpan(self: *Publication, span: CIR.TypeAnno.Span) Allocator.Error!void {
        const annotations = self.env.store.sliceTypeAnnos(span);
        dependencies.hashU32(&self.hasher, std.math.cast(u32, annotations.len) orelse return error.OutOfMemory);
        for (annotations) |annotation| {
            try self.node(annotation);
            try self.annos.append(self.env.gpa, annotation);
        }
    }

    fn anno(self: *Publication, annotation: CIR.TypeAnno.Idx) Allocator.Error!void {
        try self.node(annotation);
        if (annotation != .placeholder) try self.annos.append(self.env.gpa, annotation);
    }

    fn writeAnnotations(self: *Publication) Allocator.Error!void {
        while (self.annos.pop()) |id| {
            if (id == .placeholder) continue;
            const entry = try self.seen_annos.getOrPut(id);
            if (entry.found_existing) continue;
            try self.node(id);
            const annotation = self.env.store.getTypeAnno(id);
            try self.bytes(@tagName(annotation));
            switch (annotation) {
                .apply => |apply| {
                    try self.value(apply.name);
                    try self.value(apply.base);
                    try self.annoSpan(apply.args);
                },
                inline .lookup, .rigid_var, .rigid_var_lookup, .malformed => |data| try self.value(data),
                .underscore => {},
                .tag_union => |tag_union| {
                    try self.annoSpan(tag_union.tags);
                    try self.value(tag_union.ext != null);
                    if (tag_union.ext) |ext| try self.anno(ext);
                },
                .tag => |tag| {
                    try self.value(tag.name);
                    try self.annoSpan(tag.args);
                },
                .tuple => |tuple| try self.annoSpan(tuple.elems),
                .record => |record| {
                    const fields = self.env.store.sliceAnnoRecordFields(record.fields);
                    dependencies.hashU32(&self.hasher, std.math.cast(u32, fields.len) orelse return error.OutOfMemory);
                    for (fields) |field_id| {
                        const field = self.env.store.getAnnoRecordField(field_id);
                        try self.value(field);
                        try self.anno(field.ty);
                        if (field.default_value) |default| try self.ty(ModuleEnv.varFrom(default));
                    }
                    try self.value(record.ext != null);
                    if (record.ext) |ext| try self.anno(ext);
                },
                .@"fn" => |function| {
                    try self.value(function.effectful);
                    try self.annoSpan(function.args);
                    try self.anno(function.ret);
                },
                .parens => |parens| try self.anno(parens.anno),
            }
        }
    }

    fn writeInterface(
        self: *Publication,
        classify: *const fn (*const ModuleEnv, CIR.Def) Definition,
        binding_owner: *const fn (*const ModuleEnv, CIR.Node.Idx) Allocator.Error!?CIR.Def.Idx,
    ) Allocator.Error!void {
        const env = self.env;
        try self.bytes(env.module_name);
        // The type-module header payload is assigned by header validation,
        // not by inference (expression-checking clients do not validate a
        // module header). Declaration ownership comes from the tables below.
        try self.bytes(@tagName(env.module_kind));
        try self.value(env.module_role);

        const imports = env.imports.imports.items.items;
        try self.section("imports", imports.len);
        for (imports, 0..) |name, i| {
            try self.bytes(env.getString(name));
            const import_id: CIR.Import.Idx = @fromBackingInt(@as(u32, @intCast(i)));
            try self.value(env.imports.getResolvedModule(import_id));
        }

        const exposed = env.common.exposed_items.items.entries.items;
        const order = try env.gpa.alloc(usize, exposed.len);
        defer env.gpa.free(order);
        for (order, 0..) |*position, i| position.* = i;
        std.mem.sort(usize, order, env, struct {
            fn lessThan(module: *const ModuleEnv, a: usize, b: usize) bool {
                const entries = module.common.exposed_items.items.entries.items;
                return base.Ident.textLessThan(module.getIdent(@bitCast(entries[a].key)), module.getIdent(@bitCast(entries[b].key)));
            }
        }.lessThan);
        try self.section("exposures", order.len);
        for (order) |index| {
            const entry = exposed[index];
            try self.value(@as(base.Ident.Idx, @bitCast(entry.key)));
            try self.value(entry.value.tag());
            switch (entry.value.tag()) {
                .unresolved => {},
                .type_decl => try self.declaration(@fromBackingInt(entry.value.node_idx)),
                .value_def => {
                    const node_id: CIR.Node.Idx = @fromBackingInt(entry.value.node_idx);
                    if (env.store.nodes.get(node_id).tag == .def) {
                        try self.definition(@fromBackingInt(entry.value.node_idx), classify);
                    } else {
                        try self.scheme(node_id);
                        const owner = try binding_owner(env, node_id);
                        try self.value(owner != null);
                        if (owner) |def| try self.definition(def, classify);
                    }
                },
            }
        }
        // Declarations are also consumed through nominal ownership, independently
        // of whether their names appear in this module's exposure table.
        const declarations = env.store.sliceStatements(env.type_decls);
        try self.section("declarations", declarations.len);
        for (declarations) |declaration_id| try self.declaration(declaration_id);
        try self.section("method definitions", env.method_defs.entries.items.len);
        for (env.method_defs.entries.items) |entry| {
            const owner = entry.key.ownerIdent();
            dependencies.hashU32(&self.hasher, try self.foreignNode(env.selfModuleIdentity(), @backingInt(owner.owner)));
            try self.value(entry.key.methodIdent());
            try self.scheme(entry.value.type_node_idx);
            try self.definition(entry.value.def_idx, classify);
        }
        try self.section("method identifiers", env.method_idents.entries.items.len);
        for (env.method_idents.entries.items) |entry| {
            const owner = entry.key.ownerIdent();
            dependencies.hashU32(&self.hasher, try self.foreignNode(env.selfModuleIdentity(), @backingInt(owner.owner)));
            try self.value(entry.key.methodIdent());
            try self.value(entry.value);
        }
        try self.section("for aliases", env.for_clause_aliases.items.items.len);
        for (env.for_clause_aliases.items.items) |alias| try self.value(alias);
        try self.section("requirements", env.requires_types.items.items.len);
        for (env.requires_types.items.items) |required| {
            try self.value(required.ident);
            try self.anno(required.type_anno);
            try self.value(required.region);
            try self.ty(ModuleEnv.varFrom(required.type_anno));
        }
        try self.section("hosted entries", env.hosted_entries.items.items.len);
        for (env.hosted_entries.items.items) |entry| {
            try self.value(entry.module_ident);
            try self.value(entry.func_ident);
            try self.bytes(env.getString(entry.symbol));
            try self.value(entry.target_import);
            try self.value(entry.target_def != null);
            if (entry.target_def) |def| {
                if (entry.target_import) |import_idx| {
                    dependencies.hashU32(&self.hasher, try self.importNode(import_idx, @backingInt(def)));
                } else try self.node(def);
            }
            try self.value(entry.target_status);
        }
        try self.bytes("annotations");
        try self.writeAnnotations();
    }

    fn finish(self: *Publication) Allocator.Error![32]u8 {
        var arena = base.SingleThreadArena.init(self.env.gpa);
        defer arena.deinit();
        const allocator = arena.allocator();
        var mapper = ProjectionBinding{ .publication = self };
        try self.projected.types.rebindSourceReferences(&mapper);
        var writer = CompactWriter.init();
        const root = try writer.appendAlloc(allocator, types.Store.Serialized);
        try root.serialize(&self.projected.types, allocator, &writer);
        const names = try writer.appendAlloc(allocator, base.Ident.Store.Serialized);
        try names.serialize(self.projected.getIdentStoreConst(), allocator, &writer);
        const bytes_out = try allocator.alloc(u8, writer.total_bytes);
        _ = writer.writeToBuffer(bytes_out) catch unreachable; // bytes_out has exactly writer.total_bytes bytes
        try self.bytes(bytes_out);
        for (0..self.projected.module_identities.count()) |i| {
            const identity = self.projected.moduleIdentityHash(@fromBackingInt(@as(u32, @intCast(i))));
            dependencies.hashU32(&self.hasher, self.scope.ordinal(identity.*));
        }
        return self.hasher.finalResult();
    }
};

const ProjectionBinding = struct {
    pub const Error = Allocator.Error;
    publication: *Publication,

    pub fn node(self: *ProjectionBinding, owner: base.ModuleIdentity.Idx, raw: u32) Error!u32 {
        const publication = self.publication;
        const hash = publication.projected.moduleIdentityHash(owner);
        const original = publication.env.lookupModuleIdentity(hash) orelse
            base.invariant("projected type ownership was copied from the source environment", .{});
        return publication.foreignNode(original, raw);
    }
};
