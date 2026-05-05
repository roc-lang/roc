//! Deterministic checked-type keys for artifact and MIR boundaries.
//!
//! These keys are produced during checking finalization, while it is still valid
//! to inspect the checked type store and module-local identifiers. Post-check
//! stages consume the resulting keys; they must not recompute them from source
//! syntax or from environment lookup.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const types = @import("types");
const canonical = @import("canonical_names.zig");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const TypeStore = types.Store;
const Var = types.Var;

/// Public `TypeKeyInfo` declaration.
pub const TypeKeyInfo = struct {
    key: canonical.CanonicalTypeKey,
    contains_identity_variables: bool,
};

/// Public `fromVar` function.
pub fn fromVar(
    allocator: Allocator,
    store: *const TypeStore,
    idents: *const Ident.Store,
    var_: Var,
) Allocator.Error!canonical.CanonicalTypeKey {
    return (try fromVarInfo(allocator, store, idents, var_)).key;
}

/// Public `fromVarInfo` function.
pub fn fromVarInfo(
    allocator: Allocator,
    store: *const TypeStore,
    idents: *const Ident.Store,
    var_: Var,
) Allocator.Error!TypeKeyInfo {
    var builder = Builder.init(allocator, store, idents);
    defer builder.deinit();
    try builder.writeVar(var_);
    return .{
        .key = .{ .bytes = builder.hasher.finalResult() },
        .contains_identity_variables = builder.contains_identity_variables,
    };
}

/// Public `fromConcreteVar` function.
pub fn fromConcreteVar(
    allocator: Allocator,
    store: *const TypeStore,
    idents: *const Ident.Store,
    var_: Var,
) Allocator.Error!canonical.CanonicalTypeKey {
    var builder = Builder.init(allocator, store, idents);
    defer builder.deinit();
    builder.require_concrete = true;
    try builder.writeVar(var_);
    return .{ .bytes = builder.hasher.finalResult() };
}

/// Public `emptyTagUnion` function.
pub fn emptyTagUnion() canonical.CanonicalTypeKey {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    writeByteSlice(&hasher, "empty_tag_union");
    return .{ .bytes = hasher.finalResult() };
}

/// Public `defaultDec` function.
pub fn defaultDec(idents: *const Ident.Store) canonical.CanonicalTypeKey {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    writeByteSlice(&hasher, "nominal");
    writeIdentText(&hasher, idents, builtinDecTypeIdent(idents));
    writeIdentText(&hasher, idents, builtinModuleIdent(idents));
    writeBoolValue(&hasher, true);
    writeByteSlice(&hasher, "empty_tag_union");
    writeU32Value(&hasher, 0);
    return .{ .bytes = hasher.finalResult() };
}

/// Public `schemeFromVar` function.
pub fn schemeFromVar(
    allocator: Allocator,
    store: *const TypeStore,
    idents: *const Ident.Store,
    var_: Var,
) Allocator.Error!canonical.CanonicalTypeSchemeKey {
    var builder = Builder.init(allocator, store, idents);
    defer builder.deinit();
    builder.writeTag("canonical_type_scheme");
    try builder.writeVar(var_);
    return .{ .bytes = builder.hasher.finalResult() };
}

const Builder = struct {
    allocator: Allocator,
    store: *const TypeStore,
    idents: *const Ident.Store,
    hasher: std.crypto.hash.sha2.Sha256,
    active: std.AutoHashMap(Var, u32),
    identity_variables: std.AutoHashMap(Var, u32),
    require_concrete: bool = false,
    contains_identity_variables: bool = false,

    fn init(allocator: Allocator, store: *const TypeStore, idents: *const Ident.Store) Builder {
        return .{
            .allocator = allocator,
            .store = store,
            .idents = idents,
            .hasher = std.crypto.hash.sha2.Sha256.init(.{}),
            .active = std.AutoHashMap(Var, u32).init(allocator),
            .identity_variables = std.AutoHashMap(Var, u32).init(allocator),
        };
    }

    fn deinit(self: *Builder) void {
        self.identity_variables.deinit();
        self.active.deinit();
    }

    fn writeVar(self: *Builder, var_: Var) Allocator.Error!void {
        const resolved = self.store.resolveVar(var_);
        const root = resolved.var_;

        switch (resolved.desc.content) {
            .flex => |flex| {
                if (self.require_concrete and self.flexDefaultsToDec(flex)) {
                    self.writeDefaultDec();
                    return;
                }
                if (self.require_concrete) {
                    invariantViolation("concrete canonical type key requested for unsolved flex type variable");
                }
                try self.writeIdentityVariable(root, "flex", flex.name, flex.constraints);
                return;
            },
            .rigid => |rigid| {
                if (self.require_concrete) {
                    invariantViolation("concrete canonical type key requested for unsolved rigid type variable");
                }
                try self.writeIdentityVariable(root, "rigid", rigid.name, rigid.constraints);
                return;
            },
            else => {},
        }

        if (self.active.get(root)) |slot| {
            self.writeTag("cycle");
            self.writeU32(slot);
            return;
        }

        const slot: u32 = @intCast(self.active.count());
        try self.active.put(root, slot);
        try self.writeContent(resolved.desc.content);
        _ = self.active.remove(root);
    }

    fn writeIdentityVariable(
        self: *Builder,
        root: Var,
        comptime tag: []const u8,
        name: ?Ident.Idx,
        constraints: types.StaticDispatchConstraint.SafeList.Range,
    ) Allocator.Error!void {
        self.contains_identity_variables = true;
        if (self.identity_variables.get(root)) |slot| {
            self.writeTag("identity_var_ref");
            self.writeU32(slot);
            return;
        }

        const slot: u32 = @intCast(self.identity_variables.count());
        try self.identity_variables.put(root, slot);
        self.writeTag(tag);
        self.writeU32(slot);
        try self.writeOptionalIdent(name);
        try self.writeConstraints(constraints);
    }

    fn writeContent(self: *Builder, content: types.Content) Allocator.Error!void {
        switch (content) {
            .err => invariantViolation("canonical type key requested for erroneous checked type"),
            .flex => |flex| {
                if (self.require_concrete and self.flexDefaultsToDec(flex)) {
                    self.writeDefaultDec();
                    return;
                }
                if (self.require_concrete) {
                    invariantViolation("concrete canonical type key requested for unsolved flex type variable");
                }
                invariantViolation("canonical type key reached an unsolved flex without its root identity");
            },
            .rigid => |rigid| {
                if (self.require_concrete) {
                    invariantViolation("concrete canonical type key requested for unsolved rigid type variable");
                }
                _ = rigid;
                invariantViolation("canonical type key reached an unsolved rigid without its root identity");
            },
            .alias => |alias| {
                self.writeTag("alias");
                self.writeIdent(alias.ident.ident_idx);
                self.writeIdent(alias.origin_module);
                try self.writeVar(self.store.getAliasBackingVar(alias));
                const args = self.store.sliceAliasArgs(alias);
                self.writeU32(@intCast(args.len));
                for (args) |arg| {
                    try self.writeVar(arg);
                }
            },
            .structure => |flat| try self.writeFlat(flat),
        }
    }

    fn flexDefaultsToDec(self: *Builder, flex: types.Flex) bool {
        const constraints = self.store.sliceStaticDispatchConstraints(flex.constraints);
        for (constraints) |constraint| {
            if (constraint.origin == .from_numeral) return true;
        }
        return false;
    }

    fn writeDefaultDec(self: *Builder) void {
        self.writeTag("nominal");
        self.writeIdent(builtinDecTypeIdent(self.idents));
        self.writeIdent(builtinModuleIdent(self.idents));
        self.writeBool(true);
        self.writeTag("empty_tag_union");
        self.writeU32(0);
    }

    fn writeFlat(self: *Builder, flat: types.FlatType) Allocator.Error!void {
        switch (flat) {
            .empty_record => self.writeTag("empty_record"),
            .empty_tag_union => self.writeTag("empty_tag_union"),
            .record_unbound => |fields| {
                self.writeTag("record_unbound");
                try self.writeRecordFields(fields);
            },
            .record => |record| {
                self.writeTag("record");
                try self.writeRecordFields(record.fields);
                try self.writeVar(record.ext);
            },
            .tuple => |tuple| {
                self.writeTag("tuple");
                try self.writeVarRange(tuple.elems);
            },
            .nominal_type => |nominal| {
                self.writeTag("nominal");
                self.writeIdent(nominal.ident.ident_idx);
                self.writeIdent(nominal.origin_module);
                self.writeBool(nominal.is_opaque);
                try self.writeVar(self.store.getNominalBackingVar(nominal));
                const args = self.store.sliceNominalArgs(nominal);
                self.writeU32(@intCast(args.len));
                for (args) |arg| {
                    try self.writeVar(arg);
                }
            },
            .fn_pure => |func| {
                self.writeTag("fn_pure");
                try self.writeFunc(func);
            },
            .fn_effectful => |func| {
                self.writeTag("fn_effectful");
                try self.writeFunc(func);
            },
            .fn_unbound => |func| {
                self.writeTag("fn_unbound");
                try self.writeFunc(func);
            },
            .tag_union => |tag_union| {
                self.writeTag("tag_union");
                try self.writeTags(tag_union.tags);
                try self.writeVar(tag_union.ext);
            },
        }
    }

    fn writeFunc(self: *Builder, func: types.Func) Allocator.Error!void {
        self.writeBool(func.needs_instantiation);
        try self.writeVarRange(func.args);
        try self.writeVar(func.ret);
    }

    fn writeVarRange(self: *Builder, range: Var.SafeList.Range) Allocator.Error!void {
        const vars = self.store.sliceVars(range);
        self.writeU32(@intCast(vars.len));
        for (vars) |var_| {
            try self.writeVar(var_);
        }
    }

    fn writeRecordFields(self: *Builder, range: types.RecordField.SafeMultiList.Range) Allocator.Error!void {
        const fields = self.store.getRecordFieldsSlice(range);
        const names = fields.items(.name);
        const vars = fields.items(.var_);
        self.writeU32(@intCast(names.len));
        for (names, vars) |name, var_| {
            self.writeIdent(name);
            try self.writeVar(var_);
        }
    }

    fn writeTags(self: *Builder, range: types.Tag.SafeMultiList.Range) Allocator.Error!void {
        const tags = self.store.getTagsSlice(range);
        const names = tags.items(.name);
        const args = tags.items(.args);
        self.writeU32(@intCast(names.len));
        for (names, args) |name, arg_range| {
            self.writeIdent(name);
            try self.writeVarRange(arg_range);
        }
    }

    fn writeConstraints(self: *Builder, range: types.StaticDispatchConstraint.SafeList.Range) Allocator.Error!void {
        const constraints = self.store.sliceStaticDispatchConstraints(range);
        self.writeU32(@intCast(constraints.len));
        for (constraints) |constraint| {
            self.writeIdent(constraint.fn_name);
            try self.writeVar(constraint.fn_var);
            self.writeTag(@tagName(constraint.origin));
            self.writeBool(constraint.binop_negated);
            self.writeBool(constraint.num_literal != null);
            if (constraint.num_literal) |num_literal| {
                self.hasher.update(&num_literal.bytes);
                self.writeBool(num_literal.is_u128);
                self.writeBool(num_literal.is_negative);
                self.writeBool(num_literal.is_fractional);
            }
        }
    }

    fn writeOptionalIdent(self: *Builder, maybe_ident: ?Ident.Idx) Allocator.Error!void {
        self.writeBool(maybe_ident != null);
        if (maybe_ident) |ident| {
            self.writeIdent(ident);
        }
    }

    fn writeIdent(self: *Builder, ident: Ident.Idx) void {
        self.writeBytes(self.idents.getText(ident));
    }

    fn writeTag(self: *Builder, tag: []const u8) void {
        self.writeBytes(tag);
    }

    fn writeBytes(self: *Builder, bytes: []const u8) void {
        self.writeU32(@intCast(bytes.len));
        self.hasher.update(bytes);
    }

    fn writeBool(self: *Builder, value: bool) void {
        const byte: [1]u8 = if (value) .{1} else .{0};
        self.hasher.update(&byte);
    }

    fn writeU32(self: *Builder, value: u32) void {
        var bytes: [4]u8 = undefined;
        std.mem.writeInt(u32, &bytes, value, .little);
        self.hasher.update(&bytes);
    }
};

fn builtinDecTypeIdent(idents: *const Ident.Store) Ident.Idx {
    return idents.findByString("Builtin.Num.Dec") orelse invariantViolation("canonical type key requested Dec default but Builtin.Num.Dec was not interned");
}

fn builtinModuleIdent(idents: *const Ident.Store) Ident.Idx {
    return idents.findByString("Builtin") orelse invariantViolation("canonical type key requested Dec default but Builtin module name was not interned");
}

fn writeIdentText(hasher: *std.crypto.hash.sha2.Sha256, idents: *const Ident.Store, ident: Ident.Idx) void {
    writeByteSlice(hasher, idents.getText(ident));
}

fn writeByteSlice(hasher: *std.crypto.hash.sha2.Sha256, bytes: []const u8) void {
    writeU32Value(hasher, @intCast(bytes.len));
    hasher.update(bytes);
}

fn writeBoolValue(hasher: *std.crypto.hash.sha2.Sha256, value: bool) void {
    const byte: [1]u8 = if (value) .{1} else .{0};
    hasher.update(&byte);
}

fn writeU32Value(hasher: *std.crypto.hash.sha2.Sha256, value: u32) void {
    var bytes: [4]u8 = undefined;
    std.mem.writeInt(u32, &bytes, value, .little);
    hasher.update(&bytes);
}

fn invariantViolation(comptime message: []const u8) noreturn {
    if (builtin.mode == .Debug) {
        std.debug.panic(message, .{});
    }
    unreachable;
}

test "canonical type key declarations are referenced" {
    std.testing.refAllDecls(@This());
}
