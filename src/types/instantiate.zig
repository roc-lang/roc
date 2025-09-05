//! Type instantiation for Hindley-Milner type inference.
//!
//! This module provides functionality to instantiate polymorphic types with fresh
//! type variables while preserving type aliases and structure. This is a critical
//! component for proper handling of annotated functions in the type system.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");

const TypesStore = @import("store.zig").Store;
const Var = @import("types.zig").Var;
const Content = @import("types.zig").Content;
const FlatType = @import("types.zig").FlatType;
const Alias = @import("types.zig").Alias;
const Func = @import("types.zig").Func;
const Record = @import("types.zig").Record;
const TagUnion = @import("types.zig").TagUnion;
const RecordField = @import("types.zig").RecordField;
const Tag = @import("types.zig").Tag;
const Num = @import("types.zig").Num;
const NominalType = @import("types.zig").NominalType;
const Tuple = @import("types.zig").Tuple;
const Rank = @import("types.zig").Rank;
const Ident = base.Ident;

/// Type to manage instantiation.
///
/// Entry point is `instantiateVar`
///
/// This type does not own any of it's fields – it's a convenience wrapper to
/// making threading it's field through all the recursive functions easier
pub const Instantiate = struct {
    // not owned
    store: *TypesStore,
    idents: *const base.Ident.Store,
    seen_vars_subs: *SeenVars,

    const Self = @This();

    pub const IdentVar = struct { ident: []const u8, var_: Var };
    pub const RigidToFlexSubs = base.Scratch(IdentVar);

    pub const SeenVars = std.AutoHashMap(Var, Var);

    // general //

    pub fn init(
        store: *TypesStore,
        idents: *const base.Ident.Store,
        seen_vars_subs: *SeenVars,
    ) Self {
        return .{
            .store = store,
            .idents = idents,
            .seen_vars_subs = seen_vars_subs,
        };
    }
    // rigid vars //

    /// Check if, for the provided rigid var ident, we have a variable to substitute
    fn getRigidVarSub(rigid_vars_subs: *RigidToFlexSubs, ident: []const u8) ?Var {
        for (rigid_vars_subs.items.items) |elem| {
            if (std.mem.eql(u8, ident, elem.ident)) {
                return elem.var_;
            }
        }
        return null;
    }

    // instantiation //

    pub const Ctx = struct {
        rigid_var_subs: *RigidToFlexSubs,
        current_rank: Rank = Rank.top_level,
    };

    // instantiation //

    /// Instantiate a variable
    ///
    /// The caller provides a map that's used to substitute rigid variables,
    /// as depending on the context this map should contain vars from an
    /// annotation, or not
    pub fn instantiateVar(self: *Self, initial_var: Var, ctx: *Ctx) std.mem.Allocator.Error!Var {
        const resolved = self.store.resolveVar(initial_var);
        const resolved_var = resolved.var_;

        // Check if we've already instantiated this variable
        if (self.seen_vars_subs.get(resolved_var)) |fresh_var| {
            return fresh_var;
        }

        switch (resolved.desc.content) {
            .rigid_var => |ident| {
                // Get the ident of the rigid var
                const ident_bytes = self.getIdent(ident);

                if (Self.getRigidVarSub(ctx.rigid_var_subs, ident_bytes)) |existing_flex_var| {
                    try self.seen_vars_subs.put(resolved_var, existing_flex_var);
                    return existing_flex_var;
                } else {
                    // Create a new flex variable for this rigid variable name
                    const fresh_var = try self.store.freshFromContentWithRank(Content{ .flex_var = ident }, ctx.current_rank);
                    try ctx.rigid_var_subs.append(self.store.gpa, .{ .ident = ident_bytes, .var_ = fresh_var });

                    // Remember this substitution for recursive references
                    try self.seen_vars_subs.put(resolved_var, fresh_var);

                    return fresh_var;
                }
            },
            else => {
                const fresh_content = try self.instantiateContent(resolved.desc.content, ctx);

                // Create a fresh variable with the instantiated content
                const fresh_var = try self.store.freshFromContentWithRank(fresh_content, ctx.current_rank);

                // Remember this substitution for recursive references
                try self.seen_vars_subs.put(resolved_var, fresh_var);

                return fresh_var;
            },
        }
    }

    fn instantiateContent(self: *Self, content: Content, ctx: *Ctx) std.mem.Allocator.Error!Content {
        return switch (content) {
            .flex_var => |maybe_ident| Content{ .flex_var = maybe_ident },
            // .rigid_var => |maybe_ident| Content{ .rigid_var = maybe_ident },
            .rigid_var => unreachable,
            .alias => |alias| {
                // Instantiate the structure recursively
                const fresh_alias = try self.instantiateAlias(alias, ctx);
                return Content{ .alias = fresh_alias };
            },
            .structure => |flat_type| blk: {
                // Instantiate the structure recursively
                const fresh_flat_type = try self.instantiateFlatType(flat_type, ctx);
                break :blk Content{ .structure = fresh_flat_type };
            },
            .err => Content.err,
        };
    }

    fn instantiateAlias(self: *Self, alias: Alias, ctx: *Ctx) std.mem.Allocator.Error!Alias {
        var fresh_vars = std.ArrayList(Var).init(self.store.gpa);
        defer fresh_vars.deinit();

        const backing_var = self.store.getAliasBackingVar(alias);
        const fresh_backing_var = try self.instantiateVar(backing_var, ctx);
        try fresh_vars.append(fresh_backing_var);

        var iter = self.store.iterAliasArgs(alias);
        while (iter.next()) |arg_var| {
            const fresh_elem = try self.instantiateVar(arg_var, ctx);
            try fresh_vars.append(fresh_elem);
        }

        const fresh_vars_range = try self.store.appendVars(fresh_vars.items);
        return Alias{
            .ident = alias.ident,
            .vars = .{ .nonempty = fresh_vars_range },
        };
    }

    fn instantiateFlatType(self: *Self, flat_type: FlatType, ctx: *Ctx) std.mem.Allocator.Error!FlatType {
        return switch (flat_type) {
            .str => FlatType.str,
            .box => |box_var| FlatType{ .box = try self.instantiateVar(box_var, ctx) },
            .list => |list_var| FlatType{ .list = try self.instantiateVar(list_var, ctx) },
            .list_unbound => FlatType.list_unbound,
            .tuple => |tuple| FlatType{ .tuple = try self.instantiateTuple(tuple, ctx) },
            .num => |num| FlatType{ .num = try self.instantiateNum(num, ctx) },
            .nominal_type => |nominal| FlatType{ .nominal_type = try self.instantiateNominalType(nominal, ctx) },
            .fn_pure => |func| FlatType{ .fn_pure = try self.instantiateFunc(func, ctx) },
            .fn_effectful => |func| FlatType{ .fn_effectful = try self.instantiateFunc(func, ctx) },
            .fn_unbound => |func| FlatType{ .fn_unbound = try self.instantiateFunc(func, ctx) },
            .record => |record| FlatType{ .record = try self.instantiateRecord(record, ctx) },
            .record_unbound => |fields| FlatType{ .record_unbound = try self.instantiateRecordFields(fields, ctx) },
            .empty_record => FlatType.empty_record,
            .tag_union => |tag_union| FlatType{ .tag_union = try self.instantiateTagUnion(tag_union, ctx) },
            .empty_tag_union => FlatType.empty_tag_union,
        };
    }

    fn instantiateNominalType(self: *Self, nominal: NominalType, ctx: *Ctx) std.mem.Allocator.Error!NominalType {
        var fresh_vars = std.ArrayList(Var).init(self.store.gpa);
        defer fresh_vars.deinit();

        const backing_var = self.store.getNominalBackingVar(nominal);
        const fresh_backing_var = try self.instantiateVar(backing_var, ctx);
        try fresh_vars.append(fresh_backing_var);

        var iter = self.store.iterNominalArgs(nominal);
        while (iter.next()) |arg_var| {
            const fresh_elem = try self.instantiateVar(arg_var, ctx);
            try fresh_vars.append(fresh_elem);
        }

        const fresh_vars_range = try self.store.appendVars(fresh_vars.items);
        return NominalType{
            .ident = nominal.ident,
            .vars = .{ .nonempty = fresh_vars_range },
            .origin_module = nominal.origin_module,
        };
    }

    fn instantiateTuple(self: *Self, tuple: Tuple, ctx: *Ctx) std.mem.Allocator.Error!Tuple {
        const elems_slice = self.store.sliceVars(tuple.elems);
        var fresh_elems = std.ArrayList(Var).init(self.store.gpa);
        defer fresh_elems.deinit();

        for (elems_slice) |elem_var| {
            const fresh_elem = try self.instantiateVar(elem_var, ctx);
            try fresh_elems.append(fresh_elem);
        }

        const fresh_elems_range = try self.store.appendVars(fresh_elems.items);
        return Tuple{ .elems = fresh_elems_range };
    }

    fn instantiateNum(self: *Self, num: Num, ctx: *Ctx) std.mem.Allocator.Error!Num {
        return switch (num) {
            .num_poly => |poly| Num{ .num_poly = .{
                .var_ = try self.instantiateVar(poly.var_, ctx),
                .int_requirements = poly.int_requirements,
                .frac_requirements = poly.frac_requirements,
            } },
            .int_poly => |poly_var| Num{ .int_poly = try self.instantiateVar(poly_var, ctx) },
            .frac_poly => |poly_var| Num{ .frac_poly = try self.instantiateVar(poly_var, ctx) },
            // Concrete types remain unchanged
            .int_precision => |precision| Num{ .int_precision = precision },
            .frac_precision => |precision| Num{ .frac_precision = precision },
            .num_unbound => |unbound| Num{ .num_unbound = unbound },
            .int_unbound => |unbound| Num{ .int_unbound = unbound },
            .frac_unbound => |unbound| Num{ .frac_unbound = unbound },
            .num_compact => |compact| Num{ .num_compact = compact },
        };
    }

    fn instantiateFunc(self: *Self, func: Func, ctx: *Ctx) std.mem.Allocator.Error!Func {
        const args_slice = self.store.sliceVars(func.args);
        var fresh_args = std.ArrayList(Var).init(self.store.gpa);
        defer fresh_args.deinit();

        for (args_slice) |arg_var| {
            const fresh_arg = try self.instantiateVar(arg_var, ctx);
            try fresh_args.append(fresh_arg);
        }

        const fresh_ret = try self.instantiateVar(func.ret, ctx);
        const fresh_args_range = try self.store.appendVars(fresh_args.items);
        return Func{
            .args = fresh_args_range,
            .ret = fresh_ret,
            .needs_instantiation = true,
        };
    }

    fn instantiateRecordFields(self: *Self, fields: RecordField.SafeMultiList.Range, ctx: *Ctx) std.mem.Allocator.Error!RecordField.SafeMultiList.Range {
        const fields_slice = self.store.getRecordFieldsSlice(fields);

        var fresh_fields = std.ArrayList(RecordField).init(self.store.gpa);
        defer fresh_fields.deinit();

        for (fields_slice.items(.name), fields_slice.items(.var_)) |name, type_var| {
            const fresh_type = try self.instantiateVar(type_var, ctx);
            _ = try fresh_fields.append(RecordField{
                .name = name,
                .var_ = fresh_type,
            });
        }

        return try self.store.appendRecordFields(fresh_fields.items);
    }

    fn instantiateRecord(self: *Self, record: Record, ctx: *Ctx) std.mem.Allocator.Error!Record {
        const fields_slice = self.store.getRecordFieldsSlice(record.fields);

        var fresh_fields = std.ArrayList(RecordField).init(self.store.gpa);
        defer fresh_fields.deinit();

        for (fields_slice.items(.name), fields_slice.items(.var_)) |name, type_var| {
            const fresh_type = try self.instantiateVar(type_var, ctx);
            _ = try fresh_fields.append(RecordField{
                .name = name,
                .var_ = fresh_type,
            });
        }

        const fields_range = try self.store.appendRecordFields(fresh_fields.items);
        return Record{
            .fields = fields_range,
            .ext = try self.instantiateVar(record.ext, ctx),
        };
    }

    fn instantiateTagUnion(self: *Self, tag_union: TagUnion, ctx: *Ctx) std.mem.Allocator.Error!TagUnion {
        const tags_slice = self.store.getTagsSlice(tag_union.tags);

        var fresh_tags = std.ArrayList(Tag).init(self.store.gpa);
        defer fresh_tags.deinit();

        for (tags_slice.items(.name), tags_slice.items(.args)) |tag_name, tag_args| {
            var fresh_args = std.ArrayList(Var).init(self.store.gpa);
            defer fresh_args.deinit();

            const args_slice = self.store.sliceVars(tag_args);
            for (args_slice) |arg_var| {
                const fresh_arg = try self.instantiateVar(arg_var, ctx);
                try fresh_args.append(fresh_arg);
            }

            const fresh_args_range = try self.store.appendVars(fresh_args.items);

            _ = try fresh_tags.append(Tag{
                .name = tag_name,
                .args = fresh_args_range,
            });
        }

        const tags_range = try self.store.appendTags(fresh_tags.items);
        return TagUnion{
            .tags = tags_range,
            .ext = try self.instantiateVar(tag_union.ext, ctx),
        };
    }

    pub fn getIdent(self: *const Self, idx: Ident.Idx) []const u8 {
        return self.idents.getText(idx);
    }
};
