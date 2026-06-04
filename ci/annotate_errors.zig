//! TEMPORARY one-shot tool: rewrites bare inferred-error-union return types
//! (`fn foo() !T`) into `fn foo() Allocator.Error!T`. Locates the return-type
//! `!` token via the AST (operand-order robust, never touches `!=`/prefix-`!`)
//! and inserts `Allocator.Error` immediately before it. Re-runnable: after a run
//! the return type parses as an explicit `error_union`, so it won't double-apply.
//! Pass file paths as args. Delete this file when the migration is complete.

const std = @import("std");
const Ast = std.zig.Ast;

const INSERT = "Allocator.Error";
const max_file_size = 8 * 1024 * 1024;

pub fn main(init: std.process.Init) !void {
    var gpa_impl = std.heap.DebugAllocator(.{}){};
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();
    const io = init.io;

    var arg_it = try std.process.Args.Iterator.initAllocator(init.minimal.args, gpa);
    defer arg_it.deinit();
    var paths = std.ArrayList([]const u8).empty;
    defer {
        for (paths.items) |p| gpa.free(p);
        paths.deinit(gpa);
    }
    _ = arg_it.next(); // skip argv[0]
    while (arg_it.next()) |a| try paths.append(gpa, try gpa.dupe(u8, a));

    const buffer = try gpa.alloc(u8, max_file_size);
    defer gpa.free(buffer);

    var total_edits: usize = 0;
    var total_files: usize = 0;
    for (paths.items) |path| {
        const edits = try annotateFile(gpa, io, path, buffer);
        if (edits > 0) {
            total_files += 1;
            total_edits += edits;
            std.debug.print("{s}: {d}\n", .{ path, edits });
        }
    }
    std.debug.print("--- {d} edits across {d} files ---\n", .{ total_edits, total_files });
}

fn annotateFile(gpa: std.mem.Allocator, io: std.Io, path: []const u8, buffer: []u8) !usize {
    const bytes = try std.Io.Dir.cwd().readFile(io, path, buffer);
    const source = try gpa.dupeZ(u8, bytes);
    defer gpa.free(source);

    var tree = try Ast.parse(gpa, source, .zig);
    defer tree.deinit(gpa);

    const token_tags = tree.tokens.items(.tag);
    const token_starts = tree.tokens.items(.start);
    const node_tags = tree.nodes.items(.tag);

    var offsets = std.ArrayList(u32).empty;
    defer offsets.deinit(gpa);

    var buf: [1]Ast.Node.Index = undefined;
    for (node_tags, 0..) |tag, node_usize| {
        switch (tag) {
            .fn_proto, .fn_proto_multi, .fn_proto_one, .fn_proto_simple => {},
            else => continue,
        }
        const node: Ast.Node.Index = @enumFromInt(@as(u32, @intCast(node_usize)));
        const fn_proto = tree.fullFnProto(&buf, node) orelse continue;
        const return_type = fn_proto.ast.return_type.unwrap() orelse continue;
        const first_token = tree.firstToken(return_type);
        if (first_token == 0) continue;
        if (token_tags[first_token - 1] == .bang) {
            try offsets.append(gpa, token_starts[first_token - 1]);
        }
    }
    if (offsets.items.len == 0) return 0;

    std.mem.sort(u32, offsets.items, {}, std.sort.asc(u32));

    var out = std.ArrayList(u8).empty;
    defer out.deinit(gpa);
    var prev: usize = 0;
    for (offsets.items) |off| {
        try out.appendSlice(gpa, bytes[prev..off]);
        try out.appendSlice(gpa, INSERT);
        prev = off;
    }
    try out.appendSlice(gpa, bytes[prev..]);

    try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = path, .data = out.items });
    return offsets.items.len;
}
