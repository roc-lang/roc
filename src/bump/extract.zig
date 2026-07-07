//! Builds a `PackageApi` from checked compiler artifacts.
//!
//! Public items are enumerated from CIR structure (top-level defs and type
//! declaration statements carry dotted names like "Taxonomy.Domain.Kingdom"),
//! applying the type-module visibility rule: in a type module only the type
//! matching the module name and its associated items are public. Every item's
//! type comes from the checked artifact — the canonical scheme/root published
//! at check time — never from source annotations or rendered strings.
//!
//! Extraction has no fallbacks: missing checked data for a public item, an
//! unknown origin module, or a public type from an unstable (path or
//! versionless) dependency is a hard error with a structured failure.

const std = @import("std");
const can = @import("can");
const check = @import("check");

const PackageApi = @import("PackageApi.zig");

const ModuleEnv = can.ModuleEnv;
const CheckedArtifact = check.CheckedArtifact;
const canonical_type_keys = check.CanonicalTypeKeys;

const Allocator = std.mem.Allocator;
const CheckedTypeId = CheckedArtifact.CheckedTypeId;
const CheckedModuleArtifact = CheckedArtifact.CheckedModuleArtifact;
const CheckedTypeStoreView = CheckedArtifact.CheckedTypeStoreView;

/// One exposed module of the package being extracted.
pub const ModuleInput = struct {
    /// The module name as exposed by the package header, e.g. "Parser".
    exposed_name: []const u8,
    module_env: *const ModuleEnv,
    artifact: *const CheckedModuleArtifact,
};

/// Resolves an origin module identity (as recorded in checked types) to the
/// package that owns it. Built by the caller from resolver results.
pub const OriginMap = struct {
    map: std.StringHashMapUnmanaged(Origin) = .empty,
    identity_map: std.AutoHashMapUnmanaged([32]u8, Origin) = .empty,

    pub const Origin = struct {
        kind: Kind,
        /// The module's bare (unqualified) name. Paths are built from this so
        /// they stay stable when the root package renames a dependency alias.
        module_name: []const u8,

        pub const Kind = union(enum) {
            /// A module of the package being extracted.
            self,
            builtin,
            /// A versioned URL dependency.
            external: PackageApi.TypeOrigin.External,
            /// A path dependency or versionless URL dependency; the payload
            /// is the dependency spec, for error reporting.
            unstable: []const u8,
        };
    };

    pub fn put(self: *OriginMap, gpa: Allocator, key: []const u8, origin: Origin) Allocator.Error!void {
        try self.map.put(gpa, key, origin);
    }

    pub fn putIdentity(self: *OriginMap, gpa: Allocator, key: *const [32]u8, origin: Origin) Allocator.Error!void {
        try self.identity_map.put(gpa, key.*, origin);
    }

    pub fn deinit(self: *OriginMap, gpa: Allocator) void {
        self.identity_map.deinit(gpa);
        self.map.deinit(gpa);
    }
};

/// Extraction fails hard: `ExtractFailed` always comes with a `Failure`.
pub const ExtractError = error{ OutOfMemory, ExtractFailed };

/// Structured description of why extraction failed. The strings are owned
/// (duplicated with the allocator passed to `extractPackageApi`); free them
/// with `deinit`.
pub const Failure = struct {
    kind: Kind,
    module_name: []const u8,
    item_path: []const u8,
    detail: []const u8,

    pub const Kind = enum {
        unpublished_public_type,
        unknown_origin_module,
        unstable_dependency_in_public_api,
        private_type_in_public_api,
    };

    pub fn deinit(self: Failure, gpa: Allocator) void {
        gpa.free(self.module_name);
        gpa.free(self.item_path);
        gpa.free(self.detail);
    }
};

/// Extract the public API of a package from its exposed modules' checked
/// artifacts. On `error.ExtractFailed`, `failure` describes the problem.
pub fn extractPackageApi(
    gpa: Allocator,
    inputs: []const ModuleInput,
    origins: *const OriginMap,
    failure: *?Failure,
) ExtractError!PackageApi {
    var api = PackageApi.init(gpa);
    errdefer api.deinit();

    var extractor = Extractor{
        .gpa = gpa,
        .api = &api,
        .origins = origins,
        .failure = failure,
    };

    for (inputs) |input| {
        try extractor.extractModule(input);
    }

    try api.normalize();
    try extractor.checkSelfReferenceClosure();

    return api;
}

const Extractor = struct {
    gpa: Allocator,
    api: *PackageApi,
    origins: *const OriginMap,
    failure: *?Failure,
    /// Current module context for failure reporting.
    current_module: []const u8 = "",
    current_item: []const u8 = "",
    /// All `.self` named-reference paths seen in public signatures, for the
    /// closure check. Slices are arena-owned.
    self_refs: std.StringArrayHashMapUnmanaged(void) = .empty,

    fn fail(self: *Extractor, kind: Failure.Kind, detail: []const u8) ExtractError {
        // The failure outlives the PackageApi arena (which errdefer-frees on
        // this same error), so its strings must be owned copies.
        self.failure.* = .{
            .kind = kind,
            .module_name = try self.gpa.dupe(u8, self.current_module),
            .item_path = try self.gpa.dupe(u8, self.current_item),
            .detail = try self.gpa.dupe(u8, detail),
        };
        return error.ExtractFailed;
    }

    fn extractModule(self: *Extractor, input: ModuleInput) ExtractError!void {
        const module_env = input.module_env;
        self.current_module = input.exposed_name;
        self.current_item = "";

        const module_index = try self.api.addModule(try self.api.allocator().dupe(u8, input.exposed_name));
        const view = input.artifact.checked_types.view();
        const names = &input.artifact.canonical_names;

        // In a type module, only the type matching the module name and its
        // associated items (dotted names under it) are public; other
        // top-level defs are private helpers. Other module kinds expose all
        // their top-level items.
        const apply_name_filter = module_env.module_kind == .type_module;

        var seen_paths = std.StringHashMapUnmanaged(void).empty;
        defer seen_paths.deinit(self.gpa);

        // Type declaration statements (nominal and alias, including nested
        // associated types — their names are already dotted).
        const stmts_slice = module_env.store.sliceStatements(module_env.all_statements);
        for (stmts_slice) |stmt_idx| {
            const stmt = module_env.store.getStatement(stmt_idx);
            const header_idx = switch (stmt) {
                .s_alias_decl => |decl| decl.header,
                .s_nominal_decl => |decl| decl.header,
                else => continue,
            };
            const header = module_env.store.getTypeHeader(header_idx);
            const name = module_env.getIdentText(header.relative_name);
            if (apply_name_filter and !nameIsPublic(name, input.exposed_name)) continue;

            const path = try self.api.allocator().dupe(u8, name);
            self.current_item = path;
            const gop = try seen_paths.getOrPut(self.gpa, path);
            if (gop.found_existing) continue;

            const decl_var = ModuleEnv.varFrom(stmt_idx);
            const key = try canonical_type_keys.fromVar(
                self.gpa,
                &module_env.types,
                module_env,
                decl_var,
            );
            const root = view.rootForKey(key) orelse
                return self.fail(.unpublished_public_type, "exposed type declaration has no published checked root");

            const kind = try self.itemKindForTypeDecl(view, names, root);
            try self.api.addItem(module_index, .{ .path = path, .kind = kind });
        }

        // Top-level value defs (values, associated methods, and associated
        // constants — associated names are already dotted).
        const defs_slice = module_env.store.sliceDefs(module_env.all_defs);
        for (defs_slice) |def_idx| {
            const def = module_env.store.getDef(def_idx);
            const pattern = module_env.store.getPattern(def.pattern);
            const ident = switch (pattern) {
                .assign => |assign| assign.ident,
                // Type declarations reach defs through nominal patterns; the
                // statement walk above already covered them.
                else => continue,
            };
            const name = module_env.getIdentText(ident);
            if (apply_name_filter and !nameIsPublic(name, input.exposed_name)) continue;

            const path = try self.api.allocator().dupe(u8, name);
            self.current_item = path;
            const gop = try seen_paths.getOrPut(self.gpa, path);
            if (gop.found_existing) continue;

            const scheme_key = try canonical_type_keys.schemeFromVar(
                self.gpa,
                &module_env.types,
                module_env,
                ModuleEnv.varFrom(def_idx),
            );
            const scheme = view.schemeForKey(scheme_key) orelse
                return self.fail(.unpublished_public_type, "public value has no published checked type scheme");

            var memo = ConvertMemo.empty;
            defer memo.deinit(self.gpa);
            const ty = try self.convertType(view, names, scheme.root, &memo);
            try self.api.addItem(module_index, .{ .path = path, .kind = .{ .value = ty } });
        }
    }

    /// Build the item kind for an exposed type declaration from its published
    /// checked root, which must be an alias or nominal payload.
    fn itemKindForTypeDecl(
        self: *Extractor,
        view: CheckedTypeStoreView,
        names: *const check.CanonicalNames.CanonicalNameStore,
        root: CheckedTypeId,
    ) ExtractError!PackageApi.Item.Kind {
        var memo = ConvertMemo.empty;
        defer memo.deinit(self.gpa);

        switch (view.payload(root)) {
            .alias => |alias| {
                return .{ .alias = .{
                    .arity = @intCast(alias.args.len),
                    .target = try self.convertType(view, names, alias.backing, &memo),
                } };
            },
            .nominal => |nominal| {
                return .{ .nominal = .{
                    .arity = @intCast(nominal.args.len),
                    .is_opaque = nominal.is_opaque,
                    .backing = if (nominal.is_opaque)
                        null
                    else
                        try self.convertType(view, names, nominal.backing, &memo),
                } };
            },
            else => return self.fail(.unpublished_public_type, "exposed type declaration root is not an alias or nominal type"),
        }
    }

    const ConvertMemo = std.AutoHashMapUnmanaged(CheckedTypeId, PackageApi.TypeId);

    /// Convert a checked type into the PackageApi arena. The memo is
    /// per-item: variable nodes must not be shared across items, because
    /// normalization assigns per-item binding positions.
    fn convertType(
        self: *Extractor,
        view: CheckedTypeStoreView,
        names: *const check.CanonicalNames.CanonicalNameStore,
        id: CheckedTypeId,
        memo: *ConvertMemo,
    ) ExtractError!PackageApi.TypeId {
        if (memo.get(id)) |existing| return existing;
        const alloc = self.api.allocator();

        switch (view.payload(id)) {
            .pending => return self.fail(.unpublished_public_type, "public type contains an unresolved (pending) checked type"),
            .record_unbound => return self.fail(.unpublished_public_type, "public type contains an unbound record type"),
            .flex, .rigid => |variable| {
                // Insert the memo entry before converting constraints so a
                // constraint signature that mentions its own variable
                // terminates.
                const display_name = if (variable.name) |name| try alloc.dupe(u8, name) else null;
                const api_id = try self.api.addType(.{ .variable = .{ .display_name = display_name } });
                try memo.put(self.gpa, id, api_id);
                const constraints = try alloc.alloc(PackageApi.Constraint, variable.constraints.len);
                for (variable.constraints, 0..) |constraint, i| {
                    constraints[i] = .{
                        .method = try alloc.dupe(u8, names.methodNameText(constraint.fn_name)),
                        .fn_ty = try self.convertType(view, names, constraint.fn_ty, memo),
                    };
                }
                self.api.getType(api_id).variable.constraints = constraints;
                return api_id;
            },
            .alias => |alias| {
                const api_id = try self.addNamedRef(
                    view,
                    names,
                    alias.origin_module,
                    names.typeNameText(alias.name),
                    alias.builtin_origin,
                    alias.args,
                    memo,
                );
                try memo.put(self.gpa, id, api_id);
                return api_id;
            },
            .nominal => |nominal| {
                const api_id = try self.addNamedRef(
                    view,
                    names,
                    nominal.origin_module,
                    names.typeNameText(nominal.name),
                    nominal.builtin != null,
                    nominal.args,
                    memo,
                );
                try memo.put(self.gpa, id, api_id);
                return api_id;
            },
            .record => |record| {
                const fields = try alloc.alloc(PackageApi.Field, record.fields.len);
                for (record.fields, 0..) |field, i| {
                    fields[i] = .{
                        .name = try alloc.dupe(u8, names.recordFieldLabelText(field.name)),
                        .ty = try self.convertType(view, names, field.ty, memo),
                    };
                }
                const ext = try self.convertExt(view, names, record.ext, .empty_record, memo);
                const api_id = try self.api.addType(.{ .record = .{ .fields = fields, .ext = ext } });
                try memo.put(self.gpa, id, api_id);
                return api_id;
            },
            .tuple => |elems| {
                const api_elems = try alloc.alloc(PackageApi.TypeId, elems.len);
                for (elems, 0..) |elem, i| {
                    api_elems[i] = try self.convertType(view, names, elem, memo);
                }
                const api_id = try self.api.addType(.{ .tuple = api_elems });
                try memo.put(self.gpa, id, api_id);
                return api_id;
            },
            .function => |function| {
                const args = try alloc.alloc(PackageApi.TypeId, function.args.len);
                for (function.args, 0..) |arg, i| {
                    args[i] = try self.convertType(view, names, arg, memo);
                }
                const ret = try self.convertType(view, names, function.ret, memo);
                const api_id = try self.api.addType(.{ .function = .{
                    .effectful = CheckedArtifact.finalizedFunctionKind(function.kind) == .effectful,
                    .args = args,
                    .ret = ret,
                } });
                try memo.put(self.gpa, id, api_id);
                return api_id;
            },
            .tag_union => |tag_union| {
                const tags = try alloc.alloc(PackageApi.Tag, tag_union.tags.len);
                for (tag_union.tags, 0..) |tag, i| {
                    const tag_args = tag.argsSlice(view);
                    const api_args = try alloc.alloc(PackageApi.TypeId, tag_args.len);
                    for (tag_args, 0..) |arg, j| {
                        api_args[j] = try self.convertType(view, names, arg, memo);
                    }
                    tags[i] = .{
                        .name = try alloc.dupe(u8, names.tagLabelText(tag.name)),
                        .args = api_args,
                    };
                }
                const ext = try self.convertExt(view, names, tag_union.ext, .empty_tag_union, memo);
                const api_id = try self.api.addType(.{ .tag_union = .{ .tags = tags, .ext = ext } });
                try memo.put(self.gpa, id, api_id);
                return api_id;
            },
            .empty_record => {
                const api_id = try self.api.addType(.empty_record);
                try memo.put(self.gpa, id, api_id);
                return api_id;
            },
            .empty_tag_union => {
                const api_id = try self.api.addType(.empty_tag_union);
                try memo.put(self.gpa, id, api_id);
                return api_id;
            },
        }
    }

    /// A row extension that is the matching empty row means "closed" and is
    /// modeled as null; anything else (a variable, a chained row) is kept.
    fn convertExt(
        self: *Extractor,
        view: CheckedTypeStoreView,
        names: *const check.CanonicalNames.CanonicalNameStore,
        ext: CheckedTypeId,
        closed_tag: std.meta.Tag(CheckedArtifact.CheckedTypePayload),
        memo: *ConvertMemo,
    ) ExtractError!?PackageApi.TypeId {
        if (view.payload(ext) == closed_tag) return null;
        return try self.convertType(view, names, ext, memo);
    }

    fn addNamedRef(
        self: *Extractor,
        view: CheckedTypeStoreView,
        names: *const check.CanonicalNames.CanonicalNameStore,
        origin_module: check.CanonicalNames.ModuleIdentityId,
        type_name: []const u8,
        is_builtin: bool,
        args: []const CheckedTypeId,
        memo: *ConvertMemo,
    ) ExtractError!PackageApi.TypeId {
        const alloc = self.api.allocator();

        var path_module: []const u8 = "Builtin";
        const origin_hash = names.moduleIdentityBytes(origin_module);
        const origin: PackageApi.TypeOrigin = if (is_builtin)
            .builtin
        else if (self.origins.identity_map.get(origin_hash.*)) |resolved| origin_blk: {
            path_module = resolved.module_name;
            break :origin_blk switch (resolved.kind) {
                .self => .self,
                .builtin => .builtin,
                .external => |external| .{ .external = .{
                    .url_id = try alloc.dupe(u8, external.url_id),
                    .major = external.major,
                    .minor = external.minor,
                } },
                .unstable => |spec| return self.fail(.unstable_dependency_in_public_api, spec),
            };
        } else {
            const origin_hex = std.fmt.bytesToHex(origin_hash.*, .lower);
            const detail = try std.fmt.allocPrint(self.gpa, "0x{s}", .{origin_hex[0..]});
            defer self.gpa.free(detail);
            return self.fail(.unknown_origin_module, detail);
        };

        const path = try qualifiedPath(alloc, origin, path_module, type_name);
        if (origin == .self) {
            try self.self_refs.put(self.api.allocator(), path, {});
        }

        const api_args = try alloc.alloc(PackageApi.TypeId, args.len);
        for (args, 0..) |arg, i| {
            api_args[i] = try self.convertType(view, names, arg, memo);
        }

        return try self.api.addType(.{ .named = .{
            .origin = origin,
            .path = path,
            .args = api_args,
        } });
    }

    /// Every `.self` named reference in a public signature must resolve to an
    /// extracted public item; otherwise the API exposes a private type.
    fn checkSelfReferenceClosure(self: *Extractor) ExtractError!void {
        var item_paths = std.StringHashMapUnmanaged(void).empty;
        defer item_paths.deinit(self.gpa);
        for (self.api.modules.items) |module| {
            for (module.items.items) |item| {
                try item_paths.put(self.gpa, item.path, {});
            }
        }

        for (self.self_refs.keys()) |ref_path| {
            if (!item_paths.contains(ref_path)) {
                self.current_module = "";
                self.current_item = "";
                return self.fail(.private_type_in_public_api, ref_path);
            }
        }
    }
};

/// A name is public within a type module iff it is the module's type itself
/// or an associated item underneath it ("Color", "Color.hex", never "helper").
fn nameIsPublic(name: []const u8, module_name: []const u8) bool {
    if (std.mem.eql(u8, name, module_name)) return true;
    return std.mem.startsWith(u8, name, module_name) and
        name.len > module_name.len and
        name[module_name.len] == '.';
}

/// The stable qualified path for a named type reference. Type names in CIR
/// are dotted relative to their module's main type, which itself matches the
/// module name — so a name that already starts with the module name is used
/// as-is, and anything else is qualified with its module. Builtin types use
/// their bare name (there is no meaningful "Builtin." namespace for users).
fn qualifiedPath(
    alloc: Allocator,
    origin: PackageApi.TypeOrigin,
    origin_module: []const u8,
    type_name: []const u8,
) Allocator.Error![]const u8 {
    if (origin == .builtin) {
        // Builtin type names may arrive pre-qualified ("Builtin.Str"); users
        // never write that namespace, so drop it (same policy as docs).
        const bare = if (std.mem.startsWith(u8, type_name, "Builtin."))
            type_name["Builtin.".len..]
        else
            type_name;
        return try alloc.dupe(u8, bare);
    }
    if (nameIsPublic(type_name, origin_module)) return try alloc.dupe(u8, type_name);
    return try std.fmt.allocPrint(alloc, "{s}.{s}", .{ origin_module, type_name });
}

test "nameIsPublic follows the type module visibility rule" {
    try std.testing.expect(nameIsPublic("Color", "Color"));
    try std.testing.expect(nameIsPublic("Color.hex", "Color"));
    try std.testing.expect(nameIsPublic("Color.Nested.deep", "Color"));
    try std.testing.expect(!nameIsPublic("helper", "Color"));
    try std.testing.expect(!nameIsPublic("Colors", "Color"));
    try std.testing.expect(!nameIsPublic("Color2.hex", "Color"));
}
