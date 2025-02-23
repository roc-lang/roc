const std = @import("std");
const testing = std.testing;
const base = @import("../../base.zig");
const IR = @import("./IR.zig");
const parse = @import("../parse.zig").parse;
const SExpr = base.SExpr;

const Ident = union(enum) {
    root,
    header: IR.NodeStore.Header,
    statement: IR.NodeStore.Statement,

    fn identToString(ident: @This()) []const u8 {
        switch (ident) {
            .root => {
                return "parse_ast";
            },
            .header => |header| {
                switch (header) {
                    .module => |module| {
                        _ = module;
                        // module.
                        return "header \"module\" (exposes ())";
                    },
                    else => @panic("not implemented yet"),
                }
            },
            .statement => {
                return "statement";
            },
        }
    }
};

const Value = struct {
    fn valueToString(value: @This()) []const u8 {
        _ = value;
        return "value";
    }
};

pub const Token = SExpr.Token(Ident, Value);

pub fn toStr(allocator: std.mem.Allocator, parse_ast: *IR) ![]u8 {
    var tokens = std.ArrayList(SExpr.Token(Ident, Value)).init(allocator);
    defer tokens.deinit();

    const file = parse_ast.store.getFile();

    const header = parse_ast.store.getHeader(file.header);

    try tokens.append(.lparen);
    try tokens.append(.{ .ident = .root });

    try tokens.append(.lparen);
    try tokens.append(.{ .ident = .{ .header = header } });
    // TODO push tokens for header "arguments/children"
    try tokens.append(.rparen);

    try tokens.append(.lparen);
    for (file.statements) |stmt_id| {
        const stmt = parse_ast.store.getStatement(stmt_id);
        try tokens.append(.{ .ident = .{ .statement = stmt } });
        // TODO push tokens for statement "arguments/children"
    }
    try tokens.append(.rparen);
    try tokens.append(.rparen);

    const generate_fns = SExpr.Generator(Ident, Value).GenerateFns{
        .identToString = Ident.identToString,
        .valueToString = Value.valueToString,
    };

    const str = try SExpr.Generator(Ident, Value).generate(testing.allocator, tokens.items, generate_fns);

    return str;
}

fn testHelper(allocator: std.mem.Allocator, source: []const u8, expected: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    var env = base.ModuleEnv.init(&arena);
    var parse_ast = parse(&env, testing.allocator, source);
    defer parse_ast.deinit();

    // shouldn't be required in future
    parse_ast.store.emptyScratch();

    var buf = std.ArrayList(u8).init(testing.allocator);
    defer buf.deinit();

    try parse_ast.toStr(&env, "{}", .{}, buf.writer().any());
    try testing.expectEqualStrings(expected, buf.items[0..]);
}

test "example s-expr" {
    const source =
        \\module []
        \\
        \\foo = "bar"
    ;
    const expected =
        \\(parse_ast (header) (statement))
    ;

    try testHelper(testing.allocator, source, expected);
}
