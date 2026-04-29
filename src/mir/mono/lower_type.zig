//! Lower checked type-store variables into specialization-local mono MIR types.
//!
//! This is the only mono-stage code that may inspect the checked type store.
//! It immediately translates module-local identifiers into canonical checked
//! names or mono primitives so later MIR stages do not recover type identity from
//! source names.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const types = @import("types");

const Type = @import("type.zig");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const ModuleEnv = can.ModuleEnv;
const Var = types.Var;
const canonical = check.CanonicalNames;

pub const Lowerer = struct {
    allocator: Allocator,
    source: *const types.Store,
    idents: *const Ident.Store,
    common: ModuleEnv.CommonIdents,
    names: *const canonical.CanonicalNameStore,
    dest: *Type.Store,
    lowered: std.AutoHashMap(Var, Type.TypeId),

    pub fn init(
        allocator: Allocator,
        module_env: *const ModuleEnv,
        names: *const canonical.CanonicalNameStore,
        dest: *Type.Store,
    ) Lowerer {
        return .{
            .allocator = allocator,
            .source = &module_env.types,
            .idents = module_env.getIdentStoreConst(),
            .common = module_env.idents,
            .names = names,
            .dest = dest,
            .lowered = std.AutoHashMap(Var, Type.TypeId).init(allocator),
        };
    }

    pub fn deinit(self: *Lowerer) void {
        self.lowered.deinit();
    }

    pub fn lowerVar(self: *Lowerer, var_: Var) Allocator.Error!Type.TypeId {
        const resolved = self.source.resolveVar(var_);
        const root = resolved.var_;
        if (self.lowered.get(root)) |existing| return existing;

        const placeholder = try self.dest.addType(.placeholder);
        try self.lowered.put(root, placeholder);
        const lowered = try self.lowerContent(resolved.desc.content);
        self.dest.setType(placeholder, lowered);
        self.dest.debugValidateTypeGraph(placeholder);
        return try self.dest.internTypeId(placeholder);
    }

    fn lowerContent(self: *Lowerer, content: types.Content) Allocator.Error!Type.Content {
        return switch (content) {
            .err => invariantViolation("mono type lowering received an erroneous checked type"),
            .flex => invariantViolation("mono type lowering received an unsolved flex type variable"),
            .rigid => invariantViolation("mono type lowering received an unsolved rigid type variable"),
            .alias => |alias| blk: {
                const backing = self.source.getAliasBackingVar(alias);
                break :blk .{ .link = try self.lowerVar(backing) };
            },
            .structure => |flat| try self.lowerFlat(flat),
        };
    }

    fn lowerFlat(self: *Lowerer, flat: types.FlatType) Allocator.Error!Type.Content {
        return switch (flat) {
            .empty_record => .{ .record = .{ .fields = &.{} } },
            .empty_tag_union => .{ .tag_union = .{ .tags = &.{} } },
            .record_unbound => invariantViolation("mono type lowering received an unfinalized open record row"),
            .record => |record| try self.lowerRecord(record),
            .tuple => |tuple| .{ .tuple = try self.lowerVarRange(tuple.elems) },
            .nominal_type => |nominal| try self.lowerNominal(nominal),
            .fn_pure => |func| try self.lowerFunc(func),
            .fn_effectful => |func| try self.lowerFunc(func),
            .fn_unbound => |func| try self.lowerFunc(func),
            .tag_union => |tag_union| try self.lowerTagUnion(tag_union),
        };
    }

    fn lowerFunc(self: *Lowerer, func: types.Func) Allocator.Error!Type.Content {
        if (func.needs_instantiation) {
            invariantViolation("mono type lowering received a function type that still needs instantiation");
        }
        return .{ .func = .{
            .args = try self.lowerVarRange(func.args),
            .lambdas = &.{},
            .ret = try self.lowerVar(func.ret),
        } };
    }

    fn lowerRecord(self: *Lowerer, record: types.Record) Allocator.Error!Type.Content {
        var source_fields = std.ArrayList(SourceField).empty;
        defer source_fields.deinit(self.allocator);

        try self.collectRecordFields(record.fields, record.ext, &source_fields);
        std.mem.sort(SourceField, source_fields.items, self, SourceField.lessThan);

        const fields = try self.allocator.alloc(Type.Field, source_fields.items.len);
        errdefer self.allocator.free(fields);
        for (source_fields.items, 0..) |field, i| {
            fields[i] = .{
                .name = self.recordFieldLabel(field.name),
                .ty = try self.lowerVar(field.var_),
            };
        }

        return .{ .record = .{ .fields = fields } };
    }

    fn collectRecordFields(
        self: *Lowerer,
        fields: types.RecordField.SafeMultiList.Range,
        ext: Var,
        out: *std.ArrayList(SourceField),
    ) Allocator.Error!void {
        try self.appendRecordFields(fields, out);

        var current = ext;
        while (true) {
            const resolved = self.source.resolveVar(current);
            switch (resolved.desc.content) {
                .alias => |alias| current = self.source.getAliasBackingVar(alias),
                .structure => |flat| switch (flat) {
                    .empty_record => return,
                    .record_unbound => |ext_fields| {
                        try self.appendRecordFields(ext_fields, out);
                        return;
                    },
                    .record => |ext_record| {
                        try self.appendRecordFields(ext_record.fields, out);
                        current = ext_record.ext;
                    },
                    else => invariantViolation("record row extension resolved to a non-record type"),
                },
                .err => invariantViolation("record row extension resolved to an erroneous type"),
                .flex => invariantViolation("record row extension stayed as flex after checking"),
                .rigid => invariantViolation("record row extension stayed as rigid after checking"),
            }
        }
    }

    fn appendRecordFields(
        self: *Lowerer,
        range: types.RecordField.SafeMultiList.Range,
        out: *std.ArrayList(SourceField),
    ) Allocator.Error!void {
        const fields = self.source.getRecordFieldsSlice(range);
        const names = fields.items(.name);
        const vars = fields.items(.var_);
        try out.ensureUnusedCapacity(self.allocator, names.len);
        for (names, vars) |name, var_| {
            out.appendAssumeCapacity(.{ .name = name, .var_ = var_ });
        }
    }

    fn lowerTagUnion(self: *Lowerer, tag_union: types.TagUnion) Allocator.Error!Type.Content {
        var source_tags = std.ArrayList(SourceTag).empty;
        defer source_tags.deinit(self.allocator);

        try self.collectTags(tag_union.tags, tag_union.ext, &source_tags);
        std.mem.sort(SourceTag, source_tags.items, self, SourceTag.lessThan);

        const tags = try self.allocator.alloc(Type.Tag, source_tags.items.len);
        @memset(tags, .{ .name = @enumFromInt(0), .args = &.{} });
        errdefer {
            for (tags[0..source_tags.items.len]) |tag| {
                if (tag.args.len > 0) self.allocator.free(tag.args);
            }
            self.allocator.free(tags);
        }

        for (source_tags.items, 0..) |tag, i| {
            tags[i] = .{
                .name = self.tagLabel(tag.name),
                .args = try self.lowerVarRange(tag.args),
            };
        }

        return .{ .tag_union = .{ .tags = tags } };
    }

    fn collectTags(
        self: *Lowerer,
        tags: types.Tag.SafeMultiList.Range,
        ext: Var,
        out: *std.ArrayList(SourceTag),
    ) Allocator.Error!void {
        try self.appendTags(tags, out);

        var current = ext;
        while (true) {
            const resolved = self.source.resolveVar(current);
            switch (resolved.desc.content) {
                .alias => |alias| current = self.source.getAliasBackingVar(alias),
                .structure => |flat| switch (flat) {
                    .empty_tag_union => return,
                    .tag_union => |ext_tags| {
                        try self.appendTags(ext_tags.tags, out);
                        current = ext_tags.ext;
                    },
                    else => invariantViolation("tag-union extension resolved to a non-tag-union type"),
                },
                .err => invariantViolation("tag-union extension resolved to an erroneous type"),
                .flex => invariantViolation("tag-union extension stayed as flex after checking"),
                .rigid => invariantViolation("tag-union extension stayed as rigid after checking"),
            }
        }
    }

    fn appendTags(
        self: *Lowerer,
        range: types.Tag.SafeMultiList.Range,
        out: *std.ArrayList(SourceTag),
    ) Allocator.Error!void {
        const tags = self.source.getTagsSlice(range);
        const names = tags.items(.name);
        const args = tags.items(.args);
        try out.ensureUnusedCapacity(self.allocator, names.len);
        for (names, args) |name, arg_range| {
            out.appendAssumeCapacity(.{ .name = name, .args = arg_range });
        }
    }

    fn lowerNominal(self: *Lowerer, nominal: types.NominalType) Allocator.Error!Type.Content {
        if (self.primitiveForNominal(nominal)) |primitive| {
            return .{ .primitive = primitive };
        }

        const is_builtin = self.isBuiltinOrigin(nominal.origin_module);

        if (is_builtin and nominal.ident.ident_idx.eql(self.common.list)) {
            const args = self.source.sliceNominalArgs(nominal);
            if (args.len != 1) invariantViolation("List nominal type did not have exactly one argument");
            return .{ .list = try self.lowerVar(args[0]) };
        }

        if (is_builtin and nominal.ident.ident_idx.eql(self.common.box)) {
            const args = self.source.sliceNominalArgs(nominal);
            if (args.len != 1) invariantViolation("Box nominal type did not have exactly one argument");
            return .{ .box = try self.lowerVar(args[0]) };
        }

        const arg_vars = self.source.sliceNominalArgs(nominal);
        const args = try self.allocator.alloc(Type.TypeId, arg_vars.len);
        errdefer self.allocator.free(args);
        for (arg_vars, 0..) |arg, i| {
            args[i] = try self.lowerVar(arg);
        }

        return .{ .nominal = .{
            .nominal = .{
                .module_name = self.moduleName(nominal.origin_module),
                .type_name = self.typeName(nominal.ident.ident_idx),
            },
            .is_opaque = nominal.is_opaque,
            .args = args,
            .backing = try self.lowerVar(self.source.getNominalBackingVar(nominal)),
        } };
    }

    fn primitiveForNominal(self: *Lowerer, nominal: types.NominalType) ?Type.Prim {
        if (!self.isBuiltinOrigin(nominal.origin_module)) return null;

        const ident = nominal.ident.ident_idx;
        if (ident.eql(self.common.str) or ident.eql(self.common.builtin_str)) return .str;
        if (ident.eql(self.common.bool) or ident.eql(self.common.bool_type)) return .bool;
        if (ident.eql(self.common.u8) or ident.eql(self.common.u8_type)) return .u8;
        if (ident.eql(self.common.i8) or ident.eql(self.common.i8_type)) return .i8;
        if (ident.eql(self.common.u16) or ident.eql(self.common.u16_type)) return .u16;
        if (ident.eql(self.common.i16) or ident.eql(self.common.i16_type)) return .i16;
        if (ident.eql(self.common.u32) or ident.eql(self.common.u32_type)) return .u32;
        if (ident.eql(self.common.i32) or ident.eql(self.common.i32_type)) return .i32;
        if (ident.eql(self.common.u64) or ident.eql(self.common.u64_type)) return .u64;
        if (ident.eql(self.common.i64) or ident.eql(self.common.i64_type)) return .i64;
        if (ident.eql(self.common.u128) or ident.eql(self.common.u128_type)) return .u128;
        if (ident.eql(self.common.i128) or ident.eql(self.common.i128_type)) return .i128;
        if (ident.eql(self.common.f32) or ident.eql(self.common.f32_type)) return .f32;
        if (ident.eql(self.common.f64) or ident.eql(self.common.f64_type)) return .f64;
        if (ident.eql(self.common.dec) or ident.eql(self.common.dec_type)) return .dec;
        return null;
    }

    fn isBuiltinOrigin(self: *Lowerer, origin: Ident.Idx) bool {
        return origin.eql(self.common.builtin_module) or
            std.mem.eql(u8, self.idents.getText(origin), self.idents.getText(self.common.builtin_module));
    }

    fn lowerVarRange(self: *Lowerer, range: Var.SafeList.Range) Allocator.Error![]const Type.TypeId {
        const vars = self.source.sliceVars(range);
        if (vars.len == 0) return &.{};
        const out = try self.allocator.alloc(Type.TypeId, vars.len);
        errdefer self.allocator.free(out);
        for (vars, 0..) |var_, i| {
            out[i] = try self.lowerVar(var_);
        }
        return out;
    }

    fn moduleName(self: *Lowerer, ident: Ident.Idx) canonical.ModuleNameId {
        return self.names.lookupModuleIdent(self.idents, ident) orelse invariantViolation("nominal module name was not published in canonical names");
    }

    fn typeName(self: *Lowerer, ident: Ident.Idx) canonical.TypeNameId {
        return self.names.lookupTypeIdent(self.idents, ident) orelse invariantViolation("nominal type name was not published in canonical names");
    }

    fn recordFieldLabel(self: *Lowerer, ident: Ident.Idx) canonical.RecordFieldLabelId {
        return self.names.lookupRecordFieldIdent(self.idents, ident) orelse invariantViolation("record field label was not published in canonical names");
    }

    fn tagLabel(self: *Lowerer, ident: Ident.Idx) canonical.TagLabelId {
        return self.names.lookupTagIdent(self.idents, ident) orelse invariantViolation("tag label was not published in canonical names");
    }

    const SourceField = struct {
        name: Ident.Idx,
        var_: Var,

        fn lessThan(ctx: *Lowerer, a: SourceField, b: SourceField) bool {
            return std.mem.lessThan(u8, ctx.idents.getText(a.name), ctx.idents.getText(b.name));
        }
    };

    const SourceTag = struct {
        name: Ident.Idx,
        args: Var.SafeList.Range,

        fn lessThan(ctx: *Lowerer, a: SourceTag, b: SourceTag) bool {
            return std.mem.lessThan(u8, ctx.idents.getText(a.name), ctx.idents.getText(b.name));
        }
    };
};

fn invariantViolation(comptime message: []const u8) noreturn {
    if (builtin.mode == .Debug) {
        std.debug.panic(message, .{});
    }
    unreachable;
}

test "mono type lowerer declarations are referenced" {
    std.testing.refAllDecls(@This());
}
