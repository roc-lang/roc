//! What a module's Boxy object is a function of (design.md "Caching Under
//! `.boxy`"): the module's own source and stored values, and the interfaces
//! of the modules it imports, never their implementations. An edit that
//! leaves a module's interface alone therefore rebuilds only that module's
//! object.
//!
//! An interface has two halves. `Digests.interfaceOf` covers what a module's
//! checked data tells an importer's lowering; it is known once the module's
//! compile-time values are. `Digests.recordExports` covers the ownership
//! signatures the module's own object solved, known once that object exists.
//! Each folds in the same half of the module's imports, because lowering
//! reads through an import to the modules behind it.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const eval = @import("eval");
const object_cache = @import("boxy_object_cache.zig");

const Allocator = std.mem.Allocator;
const Sha256 = base.Sha256;
const CheckedArtifact = check.CheckedArtifact;
const Artifact = CheckedArtifact.CheckedModuleArtifact;
const ConstStore = check.ConstStore;
const canonical = check.CanonicalNames;
const ModuleNames = eval.CompileTimeFinalization.ModuleNames;

const Key = [32]u8;
const Digest = [32]u8;

/// The digests of the modules of one link, each computed once.
pub const Digests = struct {
    gpa: Allocator,
    names: ModuleNames,
    artifacts: std.AutoHashMapUnmanaged(Key, *const Artifact) = .empty,
    interfaces: std.AutoHashMapUnmanaged(Key, Digest) = .empty,
    exports: std.AutoHashMapUnmanaged(Key, Digest) = .empty,

    /// The digests of `artifacts`, every module of the link.
    pub fn init(gpa: Allocator, artifacts: []const *const Artifact) Allocator.Error!Digests {
        const views = try gpa.alloc(CheckedArtifact.ImportedModuleView, artifacts.len);
        defer gpa.free(views);
        for (artifacts, views) |artifact, *view| view.* = CheckedArtifact.importedView(artifact);
        var names = try ModuleNames.fromViews(gpa, views);
        errdefer names.deinit(gpa);
        var by_key: std.AutoHashMapUnmanaged(Key, *const Artifact) = .empty;
        errdefer by_key.deinit(gpa);
        for (artifacts) |artifact| try by_key.put(gpa, artifact.key.bytes, artifact);
        return .{ .gpa = gpa, .names = names, .artifacts = by_key };
    }

    pub fn deinit(self: *Digests) void {
        self.names.deinit(self.gpa);
        self.artifacts.deinit(self.gpa);
        self.interfaces.deinit(self.gpa);
        self.exports.deinit(self.gpa);
    }

    /// The digest of `artifact`'s interface and its imports'. The caller
    /// asks for a module only once its compile-time values are final.
    pub fn interfaceOf(self: *Digests, artifact: *const Artifact) Allocator.Error!Digest {
        if (self.interfaces.get(artifact.key.bytes)) |known| return known;
        var hasher = Sha256.init(.{});
        hasher.update("roc.boxy.interface.v1");
        hasher.update(&self.names.of(artifact.key.bytes));
        try self.hashPublicSurface(&hasher, artifact);
        for (artifact.direct_import_module_keys) |key| {
            const imported = self.artifacts.get(key.bytes) orelse base.invariant("a module of a link imports a module outside it", .{});
            hasher.update(&try self.interfaceOf(imported));
        }
        const digest = hasher.finalResult();
        try self.interfaces.put(self.gpa, artifact.key.bytes, digest);
        return digest;
    }

    /// Record `records`, the export records of `artifact`'s object. The
    /// caller records a module only after every import of it that has an
    /// object.
    pub fn recordExports(self: *Digests, artifact: *const Artifact, records: []const object_cache.ExportRecord) Allocator.Error!void {
        const sorted = try self.gpa.dupe(object_cache.ExportRecord, records);
        defer self.gpa.free(sorted);
        std.mem.sort(object_cache.ExportRecord, sorted, {}, struct {
            fn lessThan(_: void, a: object_cache.ExportRecord, b: object_cache.ExportRecord) bool {
                return std.mem.order(u8, &a.identity, &b.identity) == .lt;
            }
        }.lessThan);
        var hasher = Sha256.init(.{});
        hasher.update("roc.boxy.exports.v1");
        for (sorted) |record| {
            hasher.update(&record.identity);
            inline for (.{ record.rc_borrowed_params, record.rc_ret_lenders, record.rc_read_only_params, record.rc_ret_unique_fields }) |word| hasher.update(std.mem.asBytes(&word));
            hasher.update(&[_]u8{ @intFromBool(record.rc_ret_borrowed), @intFromBool(record.rc_ret_unique), @intFromBool(record.may_leave_pending) });
            hasher.update(std.mem.asBytes(&@as(u32, @intCast(record.rc_ret_conditions.len))));
            hasher.update(std.mem.sliceAsBytes(record.rc_ret_conditions));
        }
        self.hashImportedExports(&hasher, artifact);
        try self.exports.put(self.gpa, artifact.key.bytes, hasher.finalResult());
    }

    /// What `artifact`'s object is a function of beyond the compilation's
    /// target and policy: the key its cache entry is filed under folds this
    /// in. Every import with an object has recorded its exports.
    pub fn objectInput(self: *Digests, artifact: *const Artifact) Allocator.Error!Digest {
        var hasher = Sha256.init(.{});
        hasher.update("roc.boxy.object_input.v1");
        hasher.update(&artifact.source_hash);
        hasher.update(&self.names.of(artifact.key.bytes));
        hasher.update(&CheckedArtifact.checkingContextPolicyHash(artifact.checking_context_identity));
        try self.hashStoredValues(&hasher, artifact, .every_root);
        for (artifact.direct_import_module_keys) |key| {
            const imported = self.artifacts.get(key.bytes) orelse base.invariant("a module of a link imports a module outside it", .{});
            hasher.update(&try self.interfaceOf(imported));
        }
        self.hashImportedExports(&hasher, artifact);
        return hasher.finalResult();
    }

    fn hashImportedExports(self: *const Digests, hasher: *Sha256, artifact: *const Artifact) void {
        for (artifact.direct_import_module_keys) |key| {
            if (self.exports.get(key.bytes)) |digest| hasher.update(&digest);
        }
    }

    /// Everything of `artifact` lowering an importer reads: the checking
    /// interface (its names, schemes, nominal backings and method bindings),
    /// what checking's commit derives from bodies for callers, the workers
    /// they name, and the stored values they restore.
    fn hashPublicSurface(self: *Digests, hasher: *Sha256, artifact: *const Artifact) Allocator.Error!void {
        hasher.update(&artifact.moduleEnvConst().checking_dependency.digest);
        for (artifact.exported_procedure_templates.templates) |exported| {
            self.hashTemplateRef(hasher, artifact, exported.template);
        }
        for (artifact.exported_procedure_bindings.bindings) |binding| {
            hashTag(hasher, @tagName(std.meta.activeTag(binding.body)));
            switch (binding.body) {
                .direct_template => |direct| switch (direct.template) {
                    .checked => |ref| self.hashTemplateRef(hasher, artifact, ref),
                    .lifted, .synthetic => hasher.update(&artifact.key.bytes),
                },
                .callable_eval_template, .checked_error => {},
            }
        }
        for (artifact.method_registry.entries) |entry| {
            const target = entry.target orelse {
                hashTag(hasher, "rejected");
                continue;
            };
            hashTag(hasher, @tagName(std.meta.activeTag(target.kind)));
            switch (target.kind) {
                .procedure => |procedure| self.hashTemplateRef(hasher, artifact, procedure.template),
                .local_proc, .structural => {},
            }
        }
        hasher.update(std.mem.asBytes(&@as(u32, @intCast(artifact.checked_bodies.view().default_exprs.len))));
        const checked_types = artifact.checked_types.view();
        for (checked_types.nominal_declarations) |declaration| {
            hashBytes(hasher, artifact.canonical_names.typeNameText(checked_types.nominalDeclarationName(declaration)));
        }
        const exported_consts = artifact.exported_const_templates.templates;
        for (exported_consts) |exported| try self.hashConstTemplate(hasher, artifact, exported.template);
        for (artifact.const_templates.templates.items) |template| switch (template.owner) {
            .top_level_binding => try self.hashConstTemplate(hasher, artifact, template),
            .hoisted_expr => {},
        };
        try self.hashStoredValues(hasher, artifact, .callable_bindings);
    }

    /// The name of `ref`'s worker, and the hidden parameters a call to it
    /// passes when `artifact` declares it. An imported template's are part of
    /// its own module's interface.
    fn hashTemplateRef(self: *const Digests, hasher: *Sha256, artifact: *const Artifact, ref: canonical.ProcedureTemplateRef) void {
        hasher.update(&self.names.of(ref.artifact.bytes));
        hasher.update(std.mem.asBytes(&@as(u32, @backingInt(ref.template))));
        if (!std.mem.eql(u8, &ref.artifact.bytes, &artifact.key.bytes)) return;
        const templates = &artifact.checked_procedure_templates;
        const template = templates.templates.items[@backingInt(ref.template)];
        hasher.update(std.mem.asBytes(&template.scheme_vars.len));
        for (templates.evidence_params_pool[template.evidence_params.start..][0..template.evidence_params.len]) |param| {
            hashBytes(hasher, artifact.canonical_names.methodNameText(param.method));
            hasher.update(std.mem.asBytes(&(param.slot orelse @as(u32, std.math.maxInt(u32)))));
        }
        hasher.update(std.mem.asBytes(&@as(u32, @intCast(templates.templateLiteralRequirements(&template).len))));
    }

    /// How an importer reads a constant: the value stored for it, or the
    /// entry procedure of one compile-time evaluation did not store.
    fn hashConstTemplate(self: *Digests, hasher: *Sha256, artifact: *const Artifact, template: CheckedArtifact.ConstTemplate) Allocator.Error!void {
        hashTag(hasher, @tagName(std.meta.activeTag(template.state)));
        switch (template.state) {
            .eval_template => |eval_template| self.hashTemplateRef(hasher, artifact, eval_template.entry_template),
            .stored_const => {
                const root = switch (template.owner) {
                    .hoisted_expr => |hoisted| (artifact.hoisted_constants.lookupByExpr(hoisted.expr) orelse return).root,
                    .top_level_binding => |top_level| artifact.compile_time_roots.lookupIdByPattern(top_level.pattern) orelse return,
                };
                try self.hashRootValue(hasher, artifact, artifact.compile_time_roots.root(root));
            },
            .reserved, .unimplemented => {},
        }
    }

    const RootScope = enum { every_root, callable_bindings };

    /// The values of `artifact`'s compile-time roots in `scope`. A module's
    /// own object stores all of them.
    fn hashStoredValues(self: *Digests, hasher: *Sha256, artifact: *const Artifact, scope: RootScope) Allocator.Error!void {
        for (artifact.compile_time_roots.roots) |root| {
            switch (scope) {
                .every_root => {},
                .callable_bindings => if (root.kind != .callable_binding) continue,
            }
            try self.hashRootValue(hasher, artifact, root);
        }
    }

    fn hashRootValue(self: *Digests, hasher: *Sha256, artifact: *const Artifact, root: CheckedArtifact.CompileTimeRoot) Allocator.Error!void {
        const context: ValueContext = .{ .digests = self, .artifact = artifact };
        hashTag(hasher, @tagName(std.meta.activeTag(root.payload)));
        switch (root.payload) {
            .const_node => |node| try artifact.const_store.hashGraph(node, hasher, context),
            .fn_value => |id| hasher.update(&context.fnValueDigest(id)),
            .pending, .unevaluated, .discarded, .expect, .runtime => {},
        }
    }
};

/// How `ConstStore.hashGraph` names what a value refers to outside itself.
const ValueContext = struct {
    digests: *const Digests,
    artifact: *const Artifact,

    pub fn moduleDigest(self: ValueContext, module: check.CheckedArtifact.CheckedModuleDigest) Digest {
        return self.digests.names.of(module.bytes);
    }

    /// A stored function's evidence names types by canonical key, which
    /// names their modules by content, so a function value changes with its
    /// own module's key.
    pub fn fnValueDigest(self: ValueContext, _: ConstStore.ConstFnId) Digest {
        return self.artifact.key.bytes;
    }
};

fn hashTag(hasher: *Sha256, tag: []const u8) void {
    hasher.update(tag);
    hasher.update(&[_]u8{0});
}

fn hashBytes(hasher: *Sha256, bytes: []const u8) void {
    hasher.update(std.mem.asBytes(&@as(u32, @intCast(bytes.len))));
    hasher.update(bytes);
}
