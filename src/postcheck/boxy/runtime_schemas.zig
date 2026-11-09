//! Publish Boxy's committed field and tag positions for host marshalling.
//! Schemas follow boundary types, not which fields a generator happens to read.

const std = @import("std");
const types = @import("types.zig");
const schemas = @import("../runtime_schemas.zig");
const Program = @import("lower.zig").Program;

pub fn collect(program: *Program, roots: []const types.TypeId) std.mem.Allocator.Error!schemas.RuntimeSchemaStore {
    const allocator = program.allocator;
    var result = schemas.RuntimeSchemaStore.init(allocator);
    errdefer result.deinit();
    var seen: std.AutoHashMap(types.TypeId, void) = .init(allocator);
    defer seen.deinit();
    var named: std.AutoHashMap(types.NameId, void) = .init(allocator);
    defer named.deinit();
    var pending: std.ArrayList(types.TypeId) = .empty;
    defer pending.deinit(allocator);
    try pending.appendSlice(allocator, roots);
    while (pending.pop()) |ty| {
        if ((try seen.getOrPut(ty)).found_existing) continue;
        switch (program.store.get(ty)) {
            .nominal => |nominal| {
                try pending.appendSlice(allocator, program.store.span(nominal.args));
                if (!nominal.has_backing) continue;
                const backing = try program.converter().backing(ty);
                try pending.append(allocator, backing);
                if (nominal.is_opaque or (try named.getOrPut(nominal.name)).found_existing) continue;
                var shape = backing;
                while (program.store.get(shape) == .nominal) {
                    const inner = program.store.get(shape).nominal;
                    if (!inner.has_backing or inner.is_opaque) break;
                    shape = try program.converter().backing(shape);
                }
                switch (program.store.get(shape)) {
                    .record => |record| try appendRecord(program, &result, nominal.name, program.store.fieldSpan(record.fields)),
                    .empty_record => try appendRecord(program, &result, nominal.name, &.{}),
                    .tag_union => |tags| try appendTags(program, &result, nominal.name, program.store.tagSpan(tags.tags)),
                    .primitive, .tuple, .list, .box, .nominal, .function, .variable => {},
                }
            },
            .record => |record| {
                for (program.store.fieldSpan(record.fields)) |field| try pending.append(allocator, field.ty);
                if (record.ext) |ext| try pending.append(allocator, ext);
            },
            .tag_union => |tags| {
                for (program.store.tagSpan(tags.tags)) |tag| try pending.appendSlice(allocator, program.store.span(tag.payloads));
                if (tags.ext) |ext| try pending.append(allocator, ext);
            },
            .tuple => |items| try pending.appendSlice(allocator, program.store.span(items)),
            .list, .box => |child| try pending.append(allocator, child),
            .function => |function| {
                try pending.appendSlice(allocator, program.store.span(function.args));
                try pending.append(allocator, function.ret);
            },
            .primitive, .empty_record, .variable => {},
        }
    }
    return result;
}

fn appendRecord(program: *Program, out: *schemas.RuntimeSchemaStore, name: types.NameId, fields: []const types.Field) std.mem.Allocator.Error!void {
    const allocator = program.allocator;
    const type_name = try allocator.dupe(u8, program.store.nameText(name));
    errdefer allocator.free(type_name);
    const rows = try allocator.alloc(schemas.RuntimeRecordFieldSchema, fields.len);
    var filled: usize = 0;
    errdefer {
        for (rows[0..filled]) |row| allocator.free(row.name);
        allocator.free(rows);
    }
    for (fields, rows, 0..) |field, *row, index| {
        row.* = .{ .name = try allocator.dupe(u8, program.store.nameText(field.name)), .logical_index = @intCast(index) };
        filled += 1;
    }
    try out.records.append(allocator, .{ .type_name = type_name, .fields = rows });
}

fn appendTags(program: *Program, out: *schemas.RuntimeSchemaStore, name: types.NameId, tags: []const types.Tag) std.mem.Allocator.Error!void {
    const allocator = program.allocator;
    const type_name = try allocator.dupe(u8, program.store.nameText(name));
    errdefer allocator.free(type_name);
    const rows = try allocator.alloc(schemas.RuntimeTagSchema, tags.len);
    var filled: usize = 0;
    errdefer {
        for (rows[0..filled]) |row| allocator.free(row.name);
        allocator.free(rows);
    }
    for (tags, rows, 0..) |tag, *row, index| {
        row.* = .{ .name = try allocator.dupe(u8, program.store.nameText(tag.name)), .discriminant = @intCast(index) };
        filled += 1;
    }
    try out.tag_unions.append(allocator, .{ .type_name = type_name, .tags = rows });
}
