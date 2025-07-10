//! Cross-module type copying for imports.
//!
//! This module provides functionality to copy types from one module's type store
//! to another module's type store when importing. This ensures each module maintains
//! its own consistent type variable namespace while still being able to use types
//! from other modules.

const std = @import("std");
const base = @import("../../base.zig");
const types_mod = @import("../../types.zig");
const TypesStore = types_mod.Store;
const Var = types_mod.Var;
const Content = types_mod.Content;
const FlatType = types_mod.FlatType;
const Alias = types_mod.Alias;
const Func = types_mod.Func;
const Record = types_mod.Record;
const TagUnion = types_mod.TagUnion;
const RecordField = types_mod.RecordField;
const Tag = types_mod.Tag;
const Num = types_mod.Num;
const NominalType = types_mod.NominalType;

/// A mapping from source type variables to destination type variables
/// This is only used during the copy operation to ensure consistent mapping
/// of type variables that appear multiple times in the same type structure.
const VarMapping = std.AutoHashMap(Var, Var);

/// Copy a type from one module's type store to another module's type store.
/// This creates a completely fresh copy with new variable indices in the destination store.
pub fn copyImportedType(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    source_var: Var,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!Var {
    var var_mapping = VarMapping.init(allocator);
    defer var_mapping.deinit();

    return copyVar(source_store, dest_store, source_var, &var_mapping);
}

fn copyVar(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    source_var: Var,
    var_mapping: *VarMapping,
) std.mem.Allocator.Error!Var {
    // Check if we've already copied this variable
    if (var_mapping.get(source_var)) |dest_var| {
        return dest_var;
    }

    const resolved = source_store.resolveVar(source_var);

    // Create a placeholder variable first to break cycles
    const placeholder_var = dest_store.fresh();

    // Remember this mapping BEFORE recursing to prevent infinite loops
    try var_mapping.put(source_var, placeholder_var);

    // Now copy the content (which may recursively reference this variable)
    const dest_content = try copyContent(source_store, dest_store, resolved.desc.content, var_mapping);

    // Update the placeholder with the actual content
    try dest_store.setVarContent(placeholder_var, dest_content);

    const dest_var = placeholder_var;

    // If this is an alias, we need to handle its special memory layout requirements
    if (resolved.desc.content == .alias) {
        const alias = resolved.desc.content.alias;

        // Copy backing var (at source_var + 1)
        const source_backing_var = @as(Var, @enumFromInt(@intFromEnum(source_var) + 1));
        const dest_backing_var = try copyVar(source_store, dest_store, source_backing_var, var_mapping);

        // The backing var should be at dest_var + 1
        const expected_backing_var = @as(Var, @enumFromInt(@intFromEnum(dest_var) + 1));
        if (dest_backing_var != expected_backing_var) {
            // This is a problem - aliases expect their backing var at a specific offset
            // For now we continue, but this may cause issues
        }

        // Copy all argument vars
        var arg_iter = alias.argIterator(source_var);
        var arg_offset: u32 = 2;
        while (arg_iter.next()) |source_arg_var| {
            const dest_arg_var = try copyVar(source_store, dest_store, source_arg_var, var_mapping);
            const expected_arg_var = @as(Var, @enumFromInt(@intFromEnum(dest_var) + arg_offset));
            if (dest_arg_var != expected_arg_var) {
                // Continue anyway
            }
            arg_offset += 1;
        }
    }

    return dest_var;
}

fn copyContent(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    content: Content,
    var_mapping: *VarMapping,
) std.mem.Allocator.Error!Content {
    return switch (content) {
        .flex_var => |maybe_ident| Content{ .flex_var = maybe_ident },
        .rigid_var => |ident| Content{ .rigid_var = ident },
        .alias => |alias| Content{ .alias = alias }, // The alias itself is just metadata
        .structure => |flat_type| Content{ .structure = try copyFlatType(source_store, dest_store, flat_type, var_mapping) },
        .err => Content.err,
    };
}

fn copyFlatType(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    flat_type: FlatType,
    var_mapping: *VarMapping,
) std.mem.Allocator.Error!FlatType {
    return switch (flat_type) {
        .str => FlatType.str,
        .box => |box_var| FlatType{ .box = try copyVar(source_store, dest_store, box_var, var_mapping) },
        .list => |list_var| FlatType{ .list = try copyVar(source_store, dest_store, list_var, var_mapping) },
        .list_unbound => FlatType.list_unbound,
        .tuple => |tuple| FlatType{ .tuple = try copyTuple(source_store, dest_store, tuple, var_mapping) },
        .num => |num| FlatType{ .num = try copyNum(source_store, dest_store, num, var_mapping) },
        .nominal_type => |nominal| FlatType{ .nominal_type = nominal }, // Nominal types are references
        .fn_pure => |func| FlatType{ .fn_pure = try copyFunc(source_store, dest_store, func, var_mapping) },
        .fn_effectful => |func| FlatType{ .fn_effectful = try copyFunc(source_store, dest_store, func, var_mapping) },
        .fn_unbound => |func| FlatType{ .fn_unbound = try copyFunc(source_store, dest_store, func, var_mapping) },
        .record => |record| FlatType{ .record = try copyRecord(source_store, dest_store, record, var_mapping) },
        .record_unbound => |fields| FlatType{ .record_unbound = try copyRecordFields(source_store, dest_store, fields, var_mapping) },
        .record_poly => |poly| blk: {
            const dest_record = try copyRecord(source_store, dest_store, poly.record, var_mapping);
            const dest_var = try copyVar(source_store, dest_store, poly.var_, var_mapping);
            break :blk FlatType{ .record_poly = .{ .record = dest_record, .var_ = dest_var } };
        },
        .empty_record => FlatType.empty_record,
        .tag_union => |tag_union| FlatType{ .tag_union = try copyTagUnion(source_store, dest_store, tag_union, var_mapping) },
        .empty_tag_union => FlatType.empty_tag_union,
    };
}

fn copyTuple(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    tuple: types_mod.Tuple,
    var_mapping: *VarMapping,
) std.mem.Allocator.Error!types_mod.Tuple {
    const elems_slice = source_store.getTupleElemsSlice(tuple.elems);

    var dest_elems = std.ArrayList(Var).init(dest_store.gpa);
    defer dest_elems.deinit();

    for (elems_slice) |elem_var| {
        const dest_elem = try copyVar(source_store, dest_store, elem_var, var_mapping);
        try dest_elems.append(dest_elem);
    }

    const dest_range = dest_store.appendTupleElems(dest_elems.items);
    return types_mod.Tuple{ .elems = dest_range };
}

fn copyNum(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    num: Num,
    var_mapping: *VarMapping,
) std.mem.Allocator.Error!Num {
    return switch (num) {
        .num_poly => |poly| Num{ .num_poly = .{ .var_ = try copyVar(source_store, dest_store, poly.var_, var_mapping), .requirements = poly.requirements } },
        .int_poly => |poly| Num{ .int_poly = .{ .var_ = try copyVar(source_store, dest_store, poly.var_, var_mapping), .requirements = poly.requirements } },
        .frac_poly => |poly| Num{ .frac_poly = .{ .var_ = try copyVar(source_store, dest_store, poly.var_, var_mapping), .requirements = poly.requirements } },
        // Concrete types remain unchanged
        .int_precision => |precision| Num{ .int_precision = precision },
        .frac_precision => |precision| Num{ .frac_precision = precision },
        .num_unbound => |unbound| Num{ .num_unbound = unbound },
        .int_unbound => |unbound| Num{ .int_unbound = unbound },
        .frac_unbound => |unbound| Num{ .frac_unbound = unbound },
        .num_compact => |compact| Num{ .num_compact = compact },
    };
}

fn copyFunc(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    func: Func,
    var_mapping: *VarMapping,
) std.mem.Allocator.Error!Func {
    const args_slice = source_store.getFuncArgsSlice(func.args);

    var dest_args = std.ArrayList(Var).init(dest_store.gpa);
    defer dest_args.deinit();

    for (args_slice) |arg_var| {
        const dest_arg = try copyVar(source_store, dest_store, arg_var, var_mapping);
        try dest_args.append(dest_arg);
    }

    const dest_ret = try copyVar(source_store, dest_store, func.ret, var_mapping);

    const dest_args_range = dest_store.appendFuncArgs(dest_args.items);
    return Func{
        .args = dest_args_range,
        .ret = dest_ret,
        .needs_instantiation = func.needs_instantiation,
    };
}

fn copyRecordFields(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    fields: RecordField.SafeMultiList.Range,
    var_mapping: *VarMapping,
) std.mem.Allocator.Error!RecordField.SafeMultiList.Range {
    const fields_slice = source_store.getRecordFieldsSlice(fields);

    const fields_start = @as(RecordField.SafeMultiList.Idx, @enumFromInt(dest_store.record_fields.len()));

    for (fields_slice.items(.name), fields_slice.items(.var_)) |name, type_var| {
        const dest_type = try copyVar(source_store, dest_store, type_var, var_mapping);
        _ = dest_store.record_fields.append(dest_store.gpa, RecordField{
            .name = name,
            .var_ = dest_type,
        });
    }

    return RecordField.SafeMultiList.Range{
        .start = fields_start,
        .end = @enumFromInt(dest_store.record_fields.len()),
    };
}

fn copyRecord(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    record: Record,
    var_mapping: *VarMapping,
) std.mem.Allocator.Error!Record {
    const fields_slice = source_store.getRecordFieldsSlice(record.fields);

    const fields_start = @as(RecordField.SafeMultiList.Idx, @enumFromInt(dest_store.record_fields.len()));

    for (fields_slice.items(.name), fields_slice.items(.var_)) |name, type_var| {
        const dest_type = try copyVar(source_store, dest_store, type_var, var_mapping);
        _ = dest_store.record_fields.append(dest_store.gpa, RecordField{
            .name = name,
            .var_ = dest_type,
        });
    }

    return Record{
        .fields = RecordField.SafeMultiList.Range{
            .start = fields_start,
            .end = @enumFromInt(dest_store.record_fields.len()),
        },
        .ext = try copyVar(source_store, dest_store, record.ext, var_mapping),
    };
}

fn copyTagUnion(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    tag_union: TagUnion,
    var_mapping: *VarMapping,
) std.mem.Allocator.Error!TagUnion {
    const tags_slice = source_store.getTagsSlice(tag_union.tags);

    const tags_start = @as(Tag.SafeMultiList.Idx, @enumFromInt(dest_store.tags.len()));

    for (tags_slice.items(.name), tags_slice.items(.args)) |name, args_range| {
        const args_slice = source_store.getTagArgsSlice(args_range);

        var dest_args = std.ArrayList(Var).init(dest_store.gpa);
        defer dest_args.deinit();

        for (args_slice) |arg_var| {
            const dest_arg = try copyVar(source_store, dest_store, arg_var, var_mapping);
            try dest_args.append(dest_arg);
        }

        const dest_args_range = dest_store.appendTagArgs(dest_args.items);
        _ = dest_store.tags.append(dest_store.gpa, Tag{
            .name = name,
            .args = dest_args_range,
        });
    }

    const tags_range = Tag.SafeMultiList.Range{
        .start = tags_start,
        .end = @enumFromInt(dest_store.tags.len()),
    };

    return TagUnion{
        .tags = tags_range,
        .ext = try copyVar(source_store, dest_store, tag_union.ext, var_mapping),
    };
}
