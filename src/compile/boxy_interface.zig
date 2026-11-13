//! What a module's Boxy object is a function of (design.md "Caching Under
//! `.boxy`"): the module's own source and stored values, and the interfaces
//! of the modules it imports, never their implementations. An edit that
//! leaves a module's interface alone therefore rebuilds only that module's
//! object.
//!
//! An interface has two halves. `Digests.interfaceOf` covers what a module's
//! checked data tells an importer's lowering; it is known once the module's
//! compile-time values are. `Digests.recordExportsDigest` covers the ownership
//! signatures the module's own object solved, known once that object exists.
//! Each folds in the same half of the module's imports, because lowering
//! reads through an import to the modules behind it.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const eval = @import("eval");

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
    io: std.Io,
    names: ModuleNames,
    artifacts: std.AutoHashMapUnmanaged(Key, *const Artifact) = .empty,
    /// Each module of `artifacts`' name under `names`.
    module_names: std.AutoHashMapUnmanaged(Key, [32]u8) = .empty,
    interfaces: std.AutoHashMapUnmanaged(Key, Digest) = .empty,
    exports: std.AutoHashMapUnmanaged(Key, Digest) = .empty,
    /// The hashing of each module's own part of its digests, done ahead of
    /// `interfaceOf` and `objectInput` (`prepare`), which then add only
    /// the imports' digests.
    prepared: std.AutoHashMapUnmanaged(Key, Prepared) = .empty,
    /// Guards `interfaces`, `exports` and `prepared`, which the units of a
    /// link read and record from their lanes.
    mutex: Mutex = .init,
    /// Guards `artifacts` and `module_names`, which `add` grows on the
    /// thread owning the digests while units already running read them.
    /// Taken under `mutex` when both are held, never the other way around.
    link_mutex: Mutex = .init,

    /// A mutex on hosts with threads; a freestanding compiler runs every
    /// unit on its one thread.
    const Mutex = if (@import("builtin").os.tag == .freestanding) struct {
        pub const init: @This() = .{};
        pub fn lockUncancelable(_: *@This(), _: std.Io) void {}
        pub fn unlock(_: *@This(), _: std.Io) void {}
    } else std.Io.Mutex;

    /// The part of a module's digests that is a function of the module
    /// alone, as a hasher that has consumed it: what `interfaceOf` hashes
    /// before its imports' interfaces, and what `objectInput` hashes before
    /// its imports' interfaces and exports. Hashing a module's stored
    /// values and public surface is the bulk of either digest's cost, and it
    /// reads only the module, so `prepare` runs for every module of a link
    /// in parallel, off the thread that keys the units.
    pub const Prepared = struct {
        interface: Sha256,
        object_input: Sha256,
    };

    /// The digests of `artifacts`, every module of the link.
    pub fn init(gpa: Allocator, io: std.Io, artifacts: []const *const Artifact) Allocator.Error!Digests {
        const views = try gpa.alloc(CheckedArtifact.ImportedModuleView, artifacts.len);
        defer gpa.free(views);
        for (artifacts, views) |artifact, *view| view.* = CheckedArtifact.importedView(artifact);
        var digests: Digests = .{ .gpa = gpa, .io = io, .names = try ModuleNames.fromViews(gpa, views) };
        errdefer digests.deinit();
        for (artifacts) |artifact| _ = try digests.add(artifact);
        return digests;
    }

    /// Add `artifact` to the link; false when it is already there.
    pub fn add(self: *Digests, artifact: *const Artifact) Allocator.Error!bool {
        self.link_mutex.lockUncancelable(self.io);
        defer self.link_mutex.unlock(self.io);
        const entry = try self.artifacts.getOrPut(self.gpa, artifact.key.bytes);
        if (entry.found_existing) return false;
        entry.value_ptr.* = artifact;
        try self.module_names.put(self.gpa, artifact.key.bytes, self.names.of(CheckedArtifact.importedView(artifact)));
        return true;
    }

    pub fn nameOf(self: *Digests, key: Key) [32]u8 {
        self.link_mutex.lockUncancelable(self.io);
        defer self.link_mutex.unlock(self.io);
        return self.module_names.get(key) orelse base.invariant("a module of a link was never added to its digests", .{});
    }

    /// The module of the link imported under `key`.
    fn artifactOf(self: *Digests, key: Key) *const Artifact {
        self.link_mutex.lockUncancelable(self.io);
        defer self.link_mutex.unlock(self.io);
        return self.artifacts.get(key) orelse base.invariant("a module of a link imports a module outside it", .{});
    }

    pub fn deinit(self: *Digests) void {
        self.names.deinit(self.gpa);
        self.artifacts.deinit(self.gpa);
        self.module_names.deinit(self.gpa);
        self.interfaces.deinit(self.gpa);
        self.exports.deinit(self.gpa);
        self.prepared.deinit(self.gpa);
    }

    /// Hash the part of `artifact`'s digests that is a function of the
    /// module alone (`Prepared`), unless a unit did already. Reads only
    /// `artifact` and the link's names outside the mutex, so the units of
    /// a link hash their own parts in parallel.
    pub fn ensurePrepared(self: *Digests, artifact: *const Artifact) Allocator.Error!void {
        self.mutex.lockUncancelable(self.io);
        const known = self.prepared.contains(artifact.key.bytes);
        self.mutex.unlock(self.io);
        if (known) return;
        const prepared = try self.prepare(artifact);
        self.mutex.lockUncancelable(self.io);
        defer self.mutex.unlock(self.io);
        const entry = try self.prepared.getOrPut(self.gpa, artifact.key.bytes);
        if (!entry.found_existing) entry.value_ptr.* = prepared;
    }

    /// Hash the part of `artifact`'s digests that is a function of the
    /// module alone (`Prepared`). Reads only `artifact` and the link's
    /// names, so it runs on any thread while nothing adds to the link.
    fn prepare(self: *Digests, artifact: *const Artifact) Allocator.Error!Prepared {
        var interface = Sha256.init(.{});
        interface.update("roc.boxy.interface.v1");
        interface.update(&self.nameOf(artifact.key.bytes));
        try self.hashPublicSurface(&interface, artifact);
        var object_input = Sha256.init(.{});
        object_input.update("roc.boxy.object_input.v1");
        object_input.update(&artifact.source_hash);
        object_input.update(&self.nameOf(artifact.key.bytes));
        object_input.update(&CheckedArtifact.checkingContextPolicyHash(artifact.checking_context_identity));
        try self.hashStoredValues(&object_input, artifact, .every_root);
        return .{ .interface = interface, .object_input = object_input };
    }

    /// `artifact`'s part of its digests, hashed now when no unit did
    /// (a module with no unit of its own, such as a platform module
    /// lowered at its app's types). Called with the mutex held.
    fn preparedOf(self: *Digests, artifact: *const Artifact) Allocator.Error!Prepared {
        const entry = try self.prepared.getOrPut(self.gpa, artifact.key.bytes);
        if (!entry.found_existing) entry.value_ptr.* = try self.prepare(artifact);
        return entry.value_ptr.*;
    }

    /// The digest of `artifact`'s interface and its imports'. The caller
    /// asks for a module only once its compile-time values are final.
    pub fn interfaceOf(self: *Digests, artifact: *const Artifact) Allocator.Error!Digest {
        self.mutex.lockUncancelable(self.io);
        defer self.mutex.unlock(self.io);
        return try self.interfaceOfLocked(artifact);
    }

    /// `interfaceOf` with the mutex held.
    fn interfaceOfLocked(self: *Digests, artifact: *const Artifact) Allocator.Error!Digest {
        if (self.interfaces.get(artifact.key.bytes)) |known| return known;
        var hasher = (try self.preparedOf(artifact)).interface;
        for (artifact.direct_import_module_keys) |key| {
            const imported = self.artifactOf(key.bytes);
            hasher.update(&try self.interfaceOfLocked(imported));
        }
        const digest = hasher.finalResult();
        try self.interfaces.put(self.gpa, artifact.key.bytes, digest);
        if (base.dev_diagnostics.isSet(.trace_boxy_digests)) try self.traceInterface(artifact, digest);
        return digest;
    }

    /// Print the digest of each part of `artifact`'s interface, so a trace
    /// of two builds shows which part an edit changed.
    fn traceInterface(self: *Digests, artifact: *const Artifact, digest: Digest) Allocator.Error!void {
        std.debug.print("boxy-work: interface of {s}: {s}", .{ artifact.moduleEnvConst().module_name, &std.fmt.bytesToHex(digest[0..8], .lower) });
        for (std.enums.values(SurfacePart)) |part| {
            var hasher = Sha256.init(.{});
            try self.hashSurfacePart(&hasher, artifact, part);
            std.debug.print(" {s} {s}", .{ @tagName(part), &std.fmt.bytesToHex(hasher.finalResult()[0..8], .lower) });
        }
        std.debug.print("\n", .{});
    }

    const SurfacePart = enum { name, checking, templates, bindings, methods, defaults, nominals, consts, values, imports };

    fn hashSurfacePart(self: *Digests, hasher: *Sha256, artifact: *const Artifact, part: SurfacePart) Allocator.Error!void {
        switch (part) {
            .name => hasher.update(&self.nameOf(artifact.key.bytes)),
            .checking => hasher.update(&artifact.moduleEnvConst().checking_dependency.digest),
            .templates => for (artifact.exported_procedure_templates.templates) |exported| self.hashTemplateRef(hasher, artifact, exported.template),
            .bindings => self.hashBindings(hasher, artifact),
            .methods => self.hashMethods(hasher, artifact),
            .defaults => hasher.update(std.mem.asBytes(&@as(u32, @intCast(try artifact.bodies().defaultExprCount(self.gpa))))),
            .nominals => hashNominals(hasher, artifact),
            .consts => try self.hashConsts(hasher, artifact),
            .values => try self.hashStoredValues(hasher, artifact, .read_by_importers),
            .imports => for (artifact.direct_import_module_keys) |key| {
                const imported = self.artifactOf(key.bytes);
                hasher.update(&try self.interfaceOfLocked(imported));
            },
        }
    }

    /// Record the exports of `artifact`'s object by `records_digest`, the
    /// `object_cache.exportRecordsDigest` of its export records, which its
    /// cache entry files. The caller records a module only after every
    /// import of it that has an object.
    pub fn recordExportsDigest(self: *Digests, artifact: *const Artifact, records_digest: Digest) Allocator.Error!void {
        self.mutex.lockUncancelable(self.io);
        defer self.mutex.unlock(self.io);
        var hasher = Sha256.init(.{});
        hasher.update("roc.boxy.exports.v2");
        hasher.update(&records_digest);
        self.hashImportedExports(&hasher, artifact);
        try self.exports.put(self.gpa, artifact.key.bytes, hasher.finalResult());
    }

    /// Take `artifact`'s exports as `other`, digests no unit records to
    /// any more, recorded them.
    pub fn adoptExports(self: *Digests, other: *const Digests, artifact: *const Artifact) Allocator.Error!void {
        const digest = other.exports.get(artifact.key.bytes) orelse return;
        self.mutex.lockUncancelable(self.io);
        defer self.mutex.unlock(self.io);
        try self.exports.put(self.gpa, artifact.key.bytes, digest);
    }

    /// What `artifact`'s object is a function of beyond the compilation's
    /// target and policy: the key its cache entry is filed under folds this
    /// in. Every import with an object has recorded its exports.
    pub fn objectInput(self: *Digests, artifact: *const Artifact) Allocator.Error!Digest {
        self.mutex.lockUncancelable(self.io);
        defer self.mutex.unlock(self.io);
        var hasher = (try self.preparedOf(artifact)).object_input;
        for (artifact.direct_import_module_keys) |key| {
            const imported = self.artifactOf(key.bytes);
            hasher.update(&try self.interfaceOfLocked(imported));
        }
        self.hashImportedExports(&hasher, artifact);
        const digest = hasher.finalResult();
        if (base.dev_diagnostics.isSet(.trace_boxy_digests)) try self.traceObjectInput(artifact, digest);
        return digest;
    }

    /// Print the digest of each part of `artifact`'s object input, so a
    /// trace of two builds shows which part an edit changed. Called with
    /// the mutex held.
    fn traceObjectInput(self: *Digests, artifact: *const Artifact, digest: Digest) Allocator.Error!void {
        var values = Sha256.init(.{});
        try self.hashStoredValues(&values, artifact, .every_root);
        var imports = Sha256.init(.{});
        for (artifact.direct_import_module_keys) |key| {
            const imported = self.artifactOf(key.bytes);
            imports.update(&try self.interfaceOfLocked(imported));
        }
        var exports = Sha256.init(.{});
        self.hashImportedExports(&exports, artifact);
        std.debug.print("boxy-work: object input of {s}: {s} source {s} name {s} policy {s} values {s} imports {s} exports {s}\n", .{
            artifact.moduleEnvConst().module_name,
            &std.fmt.bytesToHex(digest[0..8], .lower),
            &std.fmt.bytesToHex(artifact.source_hash[0..8], .lower),
            &std.fmt.bytesToHex(self.nameOf(artifact.key.bytes)[0..8], .lower),
            &std.fmt.bytesToHex(CheckedArtifact.checkingContextPolicyHash(artifact.checking_context_identity)[0..8], .lower),
            &std.fmt.bytesToHex(values.finalResult()[0..8], .lower),
            &std.fmt.bytesToHex(imports.finalResult()[0..8], .lower),
            &std.fmt.bytesToHex(exports.finalResult()[0..8], .lower),
        });
    }

    /// Called with the mutex held.
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
        self.hashBindings(hasher, artifact);
        self.hashMethods(hasher, artifact);
        hasher.update(std.mem.asBytes(&@as(u32, @intCast(try artifact.bodies().defaultExprCount(self.gpa)))));
        hashNominals(hasher, artifact);
        try self.hashConsts(hasher, artifact);
        try self.hashStoredValues(hasher, artifact, .read_by_importers);
    }

    fn hashBindings(self: *Digests, hasher: *Sha256, artifact: *const Artifact) void {
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
    }

    fn hashMethods(self: *Digests, hasher: *Sha256, artifact: *const Artifact) void {
        for (artifact.method_registry.entries) |entry| {
            const target = entry.target orelse {
                hashTag(hasher, "rejected");
                continue;
            };
            hashTag(hasher, @tagName(std.meta.activeTag(target.kind)));
            switch (target.kind) {
                .procedure => |procedure| self.hashTemplateRef(hasher, artifact, procedure.template),
                .callable_binding => |binding| hashBytes(hasher, std.mem.asBytes(&@backingInt(binding.template))),
                .local_proc, .structural => {},
            }
        }
    }

    fn hashNominals(hasher: *Sha256, artifact: *const Artifact) void {
        const checked_types = artifact.typesView();
        for (checked_types.nominal_declarations) |declaration| {
            hashBytes(hasher, artifact.canonical_names.typeNameText(checked_types.nominalDeclarationName(declaration)));
        }
    }

    fn hashConsts(self: *Digests, hasher: *Sha256, artifact: *const Artifact) Allocator.Error!void {
        const exported_consts = artifact.exported_const_templates.templates;
        for (exported_consts) |exported| try self.hashConstTemplate(hasher, artifact, exported.template);
        for (artifact.const_templates.templates.items()) |template| switch (template.owner) {
            .top_level_binding => try self.hashConstTemplate(hasher, artifact, template),
            .hoisted_root => {},
        };
    }

    /// The name of `ref`'s worker, and the hidden inputs a call to it
    /// passes when `artifact` declares it. An imported template's are part of
    /// its own module's interface.
    fn hashTemplateRef(self: *Digests, hasher: *Sha256, artifact: *const Artifact, ref: canonical.ProcedureTemplateRef) void {
        hasher.update(&self.nameOf(ref.artifact.bytes));
        hasher.update(std.mem.asBytes(&@as(u32, @backingInt(ref.template))));
        if (!std.mem.eql(u8, &ref.artifact.bytes, &artifact.key.bytes)) return;
        const templates = &artifact.checked_procedure_templates;
        const template = templates.templates.items()[@backingInt(ref.template)];
        hasher.update(std.mem.asBytes(&template.scheme_vars.len));
        for (templates.evidence_params_pool[template.evidence_params.start..][0..template.evidence_params.len]) |param| {
            hashBytes(hasher, artifact.canonical_names.methodNameText(param.method));
            hasher.update(std.mem.asBytes(&(param.slot.get() orelse @as(u32, std.math.maxInt(u32)))));
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
                    .hoisted_root => |hoisted| (artifact.hoisted_constants.lookupByRoot(hoisted.root) orelse return).root,
                    .top_level_binding => |top_level| artifact.compile_time_roots.lookupIdByPattern(top_level.pattern) orelse return,
                };
                try self.hashRootValue(hasher, artifact, artifact.compile_time_roots.root(root));
            },
            .reserved, .unimplemented => {},
        }
    }

    /// `read_by_importers` names the roots an importer restores whatever
    /// their constant template says: callable bindings, and function values
    /// a `.boxy` program stored (design.md "Stored Function Values").
    const RootScope = enum { every_root, read_by_importers };

    /// The values of `artifact`'s compile-time roots in `scope`. A module's
    /// own object stores all of them.
    fn hashStoredValues(self: *Digests, hasher: *Sha256, artifact: *const Artifact, scope: RootScope) Allocator.Error!void {
        for (artifact.compile_time_roots.roots) |root| {
            switch (scope) {
                .every_root => {},
                .read_by_importers => if (root.kind != .callable_binding and root.boxy_value == null) continue,
            }
            try self.hashRootValue(hasher, artifact, root);
        }
    }

    fn hashRootValue(self: *Digests, hasher: *Sha256, artifact: *const Artifact, root: CheckedArtifact.CompileTimeRoot) Allocator.Error!void {
        const context: ValueContext = .{ .digests = self, .artifact = artifact };
        const payload = root.payloadFor(.boxy);
        hashTag(hasher, @tagName(std.meta.activeTag(payload)));
        switch (payload) {
            .const_node => |node| try artifact.const_store.hashGraph(self.gpa, node, hasher, context),
            .fn_value => |id| hasher.update(&context.fnValueDigest(id)),
            .pending, .unevaluated, .discarded, .expect, .runtime => {},
        }
    }
};

/// How `ConstStore.hashGraph` names what a value refers to outside itself.
const ValueContext = struct {
    digests: *Digests,
    artifact: *const Artifact,

    pub fn moduleDigest(self: ValueContext, module: check.CheckedArtifact.CheckedModuleDigest) Digest {
        return self.digests.nameOf(module.bytes);
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
