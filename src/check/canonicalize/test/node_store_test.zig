//! Unit tests to verify `CIR.Statement` are correctly stored in `NodeStore`

const std = @import("std");
const testing = std.testing;
const base = @import("../../../base.zig");
const types = @import("../../../types.zig");
const Node = @import("../Node.zig");
const NodeStore = @import("../NodeStore.zig");
const CIR = @import("../CIR.zig");

const from_raw_offsets = base.Region.from_raw_offsets;

const Ident = base.Ident;

test "NodeStore round trip - Statements" {
    const gpa = testing.allocator;
    var store = NodeStore.init(gpa);
    defer store.deinit();

    var statements = std.ArrayList(CIR.Statement).init(gpa);
    defer statements.deinit();

    try statements.append(CIR.Statement{
        .s_decl = .{
            .pattern = @enumFromInt(42),
            .expr = @enumFromInt(84),
            .region = from_raw_offsets(23, 56),
        },
    });

    try statements.append(CIR.Statement{
        .s_var = .{
            .pattern_idx = @enumFromInt(100),
            .expr = @enumFromInt(200),
            .region = from_raw_offsets(6, 23),
        },
    });

    try statements.append(CIR.Statement{
        .s_reassign = .{
            .expr = @enumFromInt(345),
            .pattern_idx = @enumFromInt(567),
            .region = from_raw_offsets(34, 156),
        },
    });

    try statements.append(CIR.Statement{
        .s_expr = .{
            .expr = @enumFromInt(3456),
            .region = from_raw_offsets(12, 34),
        },
    });

    try statements.append(CIR.Statement{
        .s_crash = .{
            .msg = @enumFromInt(1234),
            .region = from_raw_offsets(12, 34),
        },
    });

    try statements.append(CIR.Statement{
        .s_expect = .{
            .body = @enumFromInt(565),
            .region = from_raw_offsets(67, 234),
        },
    });

    try statements.append(CIR.Statement{
        .s_for = .{
            .body = @enumFromInt(565),
            .expr = @enumFromInt(687),
            .patt = @enumFromInt(5234),
            .region = from_raw_offsets(23, 547),
        },
    });

    try statements.append(CIR.Statement{
        .s_return = .{
            .expr = @enumFromInt(7567),
            .region = from_raw_offsets(3453, 1232),
        },
    });

    const alias: Ident.Idx = @bitCast(@as(u32, 2342));
    const module: Ident.Idx = @bitCast(@as(u32, 4565));
    const qualifier: Ident.Idx = @bitCast(@as(u32, 56756));
    try statements.append(CIR.Statement{
        .s_import = .{
            .alias_tok = alias,
            .exposes = CIR.ExposedItem.Span{
                .span = base.DataSpan.init(234, 345),
            },
            .module_name_tok = module,
            .qualifier_tok = qualifier,
            .region = from_raw_offsets(75646, 123),
        },
    });

    for (statements.items) |statement| {
        const idx = store.addStatement(statement);
        const retrieved = store.getStatement(idx);

        testing.expectEqualDeep(statement, retrieved) catch |err| {
            std.debug.print("\n\nOriginal:  {any}\n\n", .{statement});
            std.debug.print("Retrieved: {any}\n\n", .{retrieved});
            return err;
        };
    }
}

// /// Only valid at the top level of a module
// import: struct {
//     module_name_tok: Ident.Idx,
//     qualifier_tok: ?Ident.Idx,
//     alias_tok: ?Ident.Idx,
//     exposes: ExposedItem.Span,
//     region: Region,
// },
// /// A declaration of a new type - whether an alias or a new nominal custom type
// ///
// /// Only valid at the top level of a module
// type_decl: struct {
//     header: TypeHeader.Idx,
//     anno: CIR.TypeAnno.Idx,
//     where: ?WhereClause.Span,
//     region: Region,
// },
// /// A type annotation, declaring that the value referred to by an ident in the same scope should be a given type.
// type_anno: struct {
//     name: Ident.Idx,
//     anno: CIR.TypeAnno.Idx,
//     where: ?WhereClause.Span,
//     region: Region,

// },
