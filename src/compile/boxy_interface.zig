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
const Checked = check.Checked;
const CheckedModule = Checked.CheckedModule;
const ConstStore = check.ConstStore;
const canonical = check.CanonicalNames;
const ModuleNames = eval.CompileTimeFinalization.ModuleNames;

const Key = [32]u8;
const Digest = [32]u8;

/// The digests of the modules of one link, each computed once.
pub const Digests = struct {
    gpa: Allocator,
    names: ModuleNames,
    modules: std.AutoHashMapUnmanaged(Key, *const CheckedModule) = .empty,
    /// Each module of `modules`' name under `names`.
    module_names: std.AutoHashMapUnmanaged(Key, [32]u8) = .empty,
    interfaces: std.AutoHashMapUnmanaged(Key, Digest) = .empty,
    exports: std.AutoHashMapUnmanaged(Key, Digest) = .empty,

    /// The digests of `checked_modules`, every module of the link.
    pub fn init(gpa: Allocator, checked_modules: []const *const CheckedModule) Allocator.Error!Digests {
        const views = try gpa.alloc(Checked.ImportedModuleView, checked_modules.len);
        defer gpa.free(views);
        for (checked_modules, views) |checked, *view| view.* = Checked.importedView(checked);
        var digests: Digests = .{ .gpa = gpa, .names = try ModuleNames.fromViews(gpa, views) };
        errdefer digests.deinit();
        for (checked_modules) |checked| _ = try digests.add(checked);
        return digests;
    }

    /// Add `checked` to the link; false when it is already there.
    pub fn add(self: *Digests, checked: *const CheckedModule) Allocator.Error!bool {
        const entry = try self.modules.getOrPut(self.gpa, checked.key.bytes);
        if (entry.found_existing) return false;
        entry.value_ptr.* = checked;
        try self.module_names.put(self.gpa, checked.key.bytes, self.names.of(Checked.importedView(checked)));
        return true;
    }

    pub fn nameOf(self: *const Digests, key: Key) [32]u8 {
        return self.module_names.get(key) orelse base.invariant("a module of a link was never added to its digests", .{});
    }

    pub fn deinit(self: *Digests) void {
        self.names.deinit(self.gpa);
        self.modules.deinit(self.gpa);
        self.module_names.deinit(self.gpa);
        self.interfaces.deinit(self.gpa);
        self.exports.deinit(self.gpa);
    }

    /// The digest of `checked`'s interface and its imports'. The caller
    /// asks for a module only once its compile-time values are final.
    pub fn interfaceOf(self: *Digests, checked: *const CheckedModule) Allocator.Error!Digest {
        if (self.interfaces.get(checked.key.bytes)) |known| return known;
        var hasher = Sha256.init(.{});
        hasher.update("roc.boxy.interface.v1");
        hasher.update(&self.nameOf(checked.key.bytes));
        try self.hashPublicSurface(&hasher, checked);
        for (checked.direct_import_module_keys) |key| {
            const imported = self.modules.get(key.bytes) orelse base.invariant("a module of a link imports a module outside it", .{});
            hasher.update(&try self.interfaceOf(imported));
        }
        const digest = hasher.finalResult();
        try self.interfaces.put(self.gpa, checked.key.bytes, digest);
        if (base.dev_diagnostics.isSet(.trace_boxy_work)) try self.traceInterface(checked, digest);
        return digest;
    }

    /// Print the digest of each part of `checked`'s interface, so a trace
    /// of two builds shows which part an edit changed.
    fn traceInterface(self: *Digests, checked: *const CheckedModule, digest: Digest) Allocator.Error!void {
        std.debug.print("boxy-work: interface of {s}: {s}", .{ checked.moduleEnvConst().module_name, &std.fmt.bytesToHex(digest[0..8], .lower) });
        for (std.enums.values(SurfacePart)) |part| {
            var hasher = Sha256.init(.{});
            try self.hashSurfacePart(&hasher, checked, part);
            std.debug.print(" {s} {s}", .{ @tagName(part), &std.fmt.bytesToHex(hasher.finalResult()[0..8], .lower) });
        }
        std.debug.print("\n", .{});
    }

    const SurfacePart = enum { name, checking, templates, bindings, methods, defaults, nominals, consts, values, imports };

    fn hashSurfacePart(self: *Digests, hasher: *Sha256, checked: *const CheckedModule, part: SurfacePart) Allocator.Error!void {
        switch (part) {
            .name => hasher.update(&self.nameOf(checked.key.bytes)),
            .checking => hasher.update(&checked.moduleEnvConst().checking_dependency.digest),
            .templates => for ((try checked.exportRows()).procedure_templates) |exported| try self.hashTemplateRef(hasher, checked, exported.template),
            .bindings => try self.hashBindings(hasher, checked),
            .methods => try self.hashMethods(hasher, checked),
            .defaults => hasher.update(std.mem.asBytes(&@as(u32, @intCast((try checked.bodies()).defaultExprs().len)))),
            .nominals => hashNominals(hasher, checked),
            .consts => try self.hashConsts(hasher, checked),
            .values => try self.hashStoredValues(hasher, checked, .read_by_importers),
            .imports => for (checked.direct_import_module_keys) |key| {
                const imported = self.modules.get(key.bytes) orelse base.invariant("a module of a link imports a module outside it", .{});
                hasher.update(&try self.interfaceOf(imported));
            },
        }
    }

    /// Record `records`, the export records of `checked`'s object. The
    /// caller records a module only after every import of it that has an
    /// object.
    pub fn recordExports(self: *Digests, checked: *const CheckedModule, records: []const object_cache.ExportRecord) Allocator.Error!void {
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
        self.hashImportedExports(&hasher, checked);
        try self.exports.put(self.gpa, checked.key.bytes, hasher.finalResult());
    }

    /// Take `checked`'s exports as `other` recorded them.
    pub fn adoptExports(self: *Digests, other: *const Digests, checked: *const CheckedModule) Allocator.Error!void {
        if (other.exports.get(checked.key.bytes)) |digest| try self.exports.put(self.gpa, checked.key.bytes, digest);
    }

    /// What `checked`'s object is a function of beyond the compilation's
    /// target and policy: the key its cache entry is filed under folds this
    /// in. Every import with an object has recorded its exports.
    pub fn objectInput(self: *Digests, checked: *const CheckedModule) Allocator.Error!Digest {
        var hasher = Sha256.init(.{});
        hasher.update("roc.boxy.object_input.v1");
        hasher.update(&checked.source_hash);
        hasher.update(&self.nameOf(checked.key.bytes));
        hasher.update(&Checked.checkingContextPolicyHash(checked.checking_context_identity));
        try self.hashStoredValues(&hasher, checked, .every_root);
        for (checked.direct_import_module_keys) |key| {
            const imported = self.modules.get(key.bytes) orelse base.invariant("a module of a link imports a module outside it", .{});
            hasher.update(&try self.interfaceOf(imported));
        }
        self.hashImportedExports(&hasher, checked);
        const digest = hasher.finalResult();
        if (base.dev_diagnostics.isSet(.trace_boxy_work)) try self.traceObjectInput(checked, digest);
        return digest;
    }

    /// Print the digest of each part of `checked`'s object input, so a
    /// trace of two builds shows which part an edit changed.
    fn traceObjectInput(self: *Digests, checked: *const CheckedModule, digest: Digest) Allocator.Error!void {
        var values = Sha256.init(.{});
        try self.hashStoredValues(&values, checked, .every_root);
        var imports = Sha256.init(.{});
        for (checked.direct_import_module_keys) |key| {
            const imported = self.modules.get(key.bytes) orelse base.invariant("a module of a link imports a module outside it", .{});
            imports.update(&try self.interfaceOf(imported));
        }
        var exports = Sha256.init(.{});
        self.hashImportedExports(&exports, checked);
        std.debug.print("boxy-work: object input of {s}: {s} source {s} name {s} policy {s} values {s} imports {s} exports {s}\n", .{
            checked.moduleEnvConst().module_name,
            &std.fmt.bytesToHex(digest[0..8], .lower),
            &std.fmt.bytesToHex(checked.source_hash[0..8], .lower),
            &std.fmt.bytesToHex(self.nameOf(checked.key.bytes)[0..8], .lower),
            &std.fmt.bytesToHex(Checked.checkingContextPolicyHash(checked.checking_context_identity)[0..8], .lower),
            &std.fmt.bytesToHex(values.finalResult()[0..8], .lower),
            &std.fmt.bytesToHex(imports.finalResult()[0..8], .lower),
            &std.fmt.bytesToHex(exports.finalResult()[0..8], .lower),
        });
    }

    fn hashImportedExports(self: *const Digests, hasher: *Sha256, checked: *const CheckedModule) void {
        for (checked.direct_import_module_keys) |key| {
            if (self.exports.get(key.bytes)) |digest| hasher.update(&digest);
        }
    }

    /// Everything of `checked` lowering an importer reads: the checking
    /// interface (its names, schemes, nominal backings and method bindings),
    /// what checking's commit derives from bodies for callers, the workers
    /// they name, and the stored values they restore.
    fn hashPublicSurface(self: *Digests, hasher: *Sha256, checked: *const CheckedModule) Allocator.Error!void {
        hasher.update(&checked.moduleEnvConst().checking_dependency.digest);
        for ((try checked.exportRows()).procedure_templates) |exported| {
            try self.hashTemplateRef(hasher, checked, exported.template);
        }
        try self.hashBindings(hasher, checked);
        try self.hashMethods(hasher, checked);
        hasher.update(std.mem.asBytes(&@as(u32, @intCast((try checked.bodies()).defaultExprs().len))));
        hashNominals(hasher, checked);
        try self.hashConsts(hasher, checked);
        try self.hashStoredValues(hasher, checked, .read_by_importers);
    }

    fn hashBindings(self: *const Digests, hasher: *Sha256, checked: *const CheckedModule) Allocator.Error!void {
        for ((try checked.exportRows()).procedure_bindings) |binding| {
            hashTag(hasher, @tagName(std.meta.activeTag(binding.body)));
            switch (binding.body) {
                .direct_template => |direct| switch (direct.template) {
                    .checked => |ref| try self.hashTemplateRef(hasher, checked, ref),
                    .lifted, .synthetic => hasher.update(&checked.key.bytes),
                },
                .callable_eval_template, .checked_error => {},
            }
        }
    }

    fn hashMethods(self: *const Digests, hasher: *Sha256, checked: *const CheckedModule) Allocator.Error!void {
        for ((try checked.checkedTables()).method_registry.entries) |entry| {
            const target = entry.target orelse {
                hashTag(hasher, "rejected");
                continue;
            };
            hashTag(hasher, @tagName(std.meta.activeTag(target.kind)));
            switch (target.kind) {
                .procedure => |procedure| try self.hashTemplateRef(hasher, checked, procedure.template),
                .local_proc, .structural => {},
            }
        }
    }

    fn hashNominals(hasher: *Sha256, checked: *const CheckedModule) void {
        const checked_types = checked.typesView();
        for (checked_types.nominalDeclarationRows()) |declaration| {
            hashBytes(hasher, checked.nameStore().typeNameText(checked_types.nominalDeclarationName(declaration)));
        }
    }

    fn hashConsts(self: *Digests, hasher: *Sha256, checked: *const CheckedModule) Allocator.Error!void {
        const exported_consts = (try checked.exportRows()).const_templates;
        for (exported_consts) |exported| try self.hashConstTemplate(hasher, checked, exported.template);
        for ((try checked.checkedTables()).const_templates.templates.items) |template| switch (template.owner) {
            .top_level_binding => try self.hashConstTemplate(hasher, checked, template),
            .hoisted_root => {},
        };
    }

    /// The name of `ref`'s worker, and the hidden parameters a call to it
    /// passes when `checked` declares it. An imported template's are part of
    /// its own module's interface.
    fn hashTemplateRef(self: *const Digests, hasher: *Sha256, checked: *const CheckedModule, ref: canonical.ProcedureTemplateRef) Allocator.Error!void {
        hasher.update(&self.nameOf(ref.module.bytes));
        ref.template.digestInto(hasher);
        if (!std.mem.eql(u8, &ref.module.bytes, &checked.key.bytes)) return;
        const schema = try (try checked.dispatch()).templateSchema(ref.template);
        hasher.update(std.mem.asBytes(&schema.vars.len));
        for (schema.params) |param| {
            hashBytes(hasher, checked.nameStore().methodNameText(param.method));
            hasher.update(std.mem.asBytes(&(param.slot orelse @as(u32, std.math.maxInt(u32)))));
        }
        hasher.update(std.mem.asBytes(&@as(u32, @intCast((try (try checked.dispatch()).templateLiteralRequirements(ref.template)).len))));
    }

    /// How an importer reads a constant: the value stored for it, or the
    /// entry procedure of one compile-time evaluation did not store.
    fn hashConstTemplate(self: *Digests, hasher: *Sha256, checked: *const CheckedModule, template: Checked.ConstTemplate) Allocator.Error!void {
        const value = Checked.ConstTemplateValue.of(template, &checked.evaluation);
        hashTag(hasher, @tagName(std.meta.activeTag(value)));
        switch (value) {
            .eval_template => |eval_template| try self.hashTemplateRef(hasher, checked, eval_template.entry_template),
            .stored_const => {
                const root = switch (template.owner) {
                    .hoisted_root => |hoisted| ((try checked.checkedTables()).hoisted_constants.lookupByRoot(hoisted.root) orelse return).root,
                    .top_level_binding => |top_level| (try checked.checkedTables()).compile_time_roots.lookupIdByPattern(top_level.pattern) orelse return,
                };
                try self.hashRootValue(hasher, checked, (try checked.compileTimeRoot(root)));
            },
            .reserved, .unimplemented => {},
        }
    }

    /// `read_by_importers` names the roots an importer restores whatever
    /// their constant template says: callable bindings, and function values
    /// a `.boxy` program stored (design.md "Stored Function Values").
    const RootScope = enum { every_root, read_by_importers };

    /// The values of `checked`'s compile-time roots in `scope`. A module's
    /// own object stores all of them.
    fn hashStoredValues(self: *Digests, hasher: *Sha256, checked: *const CheckedModule, scope: RootScope) Allocator.Error!void {
        for ((try checked.checkedTables()).compile_time_roots.roots) |root| {
            switch (scope) {
                .every_root => {},
                .read_by_importers => if (root.kind != .callable_binding and checked.evaluation.rootBoxyValue(root.id) == null) continue,
            }
            try self.hashRootValue(hasher, checked, root);
        }
    }

    fn hashRootValue(self: *Digests, hasher: *Sha256, checked: *const CheckedModule, root: Checked.CompileTimeRoot) Allocator.Error!void {
        const context: ValueContext = .{ .digests = self, .module = checked };
        const payload = checked.evaluation.rootPayload(root.id);
        hashTag(hasher, @tagName(std.meta.activeTag(payload)));
        switch (payload) {
            .const_node => |node| try checked.evaluation.const_store.hashGraph(node, hasher, context),
            .fn_value => |id| hasher.update(&context.fnValueDigest(id)),
            .pending, .unevaluated, .discarded, .expect, .runtime => {},
        }
        if (checked.evaluation.rootBoxyValue(root.id)) |stored| {
            hashTag(hasher, "boxy_value");
            try checked.evaluation.const_store.hashGraph(stored.node, hasher, context);
        }
    }
};

/// How `ConstStore.hashGraph` names what a value refers to outside itself.
const ValueContext = struct {
    digests: *const Digests,
    module: *const CheckedModule,

    pub fn moduleDigest(self: ValueContext, module: check.Checked.CheckedModuleDigest) Digest {
        return self.digests.nameOf(module.bytes);
    }

    /// A stored function's evidence names types by canonical key, which
    /// names their modules by content, so a function value changes with its
    /// own module's key.
    pub fn fnValueDigest(self: ValueContext, _: ConstStore.ConstFnId) Digest {
        return self.module.key.bytes;
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
