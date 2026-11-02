//! Developer-only explanation of an imported CTFE image's exact dependencies.
const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const CodeExcerpt = @import("CodeExcerpt.zig");

pub fn trace(
    allocator: std.mem.Allocator,
    set: *const CodeExcerpt.Set,
    files: []const base.SourceFileEntry,
    roots: []const []const u8,
    host_visible: []const CodeExcerpt.DataItem,
    aliases: *const std.StringHashMap([]const u8),
) std.mem.Allocator.Error!void {
    var arena = collections.SingleThreadArena.init(allocator);
    defer arena.deinit();
    const a = arena.allocator();
    const names = try a.alloc([]const u8, set.excerpts.len);
    for (set.excerpts, names, 0..) |excerpt, *name, index| {
        name.* = switch (excerpt.kind) {
            .proc => |identity| try identity.symbolName(a),
            .rc_helper, .entrypoint => |symbol| symbol,
            else => try std.fmt.allocPrint(a, "excerpt-{d}", .{index}),
        };
    }
    std.debug.print("ctfe-graph\tbegin\n", .{});
    for (roots) |name| std.debug.print("ctfe-graph\troot\t{s}\n", .{name});
    var alias_iter = aliases.iterator();
    while (alias_iter.next()) |alias| edge(alias.key_ptr.*, alias.value_ptr.*, "alias");
    var carried = CodeExcerpt.DataCollector{ .allocator = a };
    defer carried.deinit();
    for (set.excerpts, names) |excerpt, name| {
        const file = if (excerpt.loc.hasLocation()) files[excerpt.loc.file].qualified_name else "-";
        std.debug.print("ctfe-graph\tcode\t{s}\t{d}\t{s}\t{d}\t{d}\t{s}\n", .{ name, excerpt.code.len, file, excerpt.loc.line, excerpt.loc.column, @tagName(excerpt.kind) });
        for (excerpt.refs) |ref| edge(name, names[ref.target], @tagName(ref.form));
        for (excerpt.symbolic_refs) |ref| {
            const target = switch (ref.binding) {
                .absent => continue,
                .local => |index| names[index],
                .unresolved => switch (ref.target) {
                    .proc => |identity| try identity.symbolName(a),
                    .rc_helper => |helper| helper,
                },
            };
            edge(name, target, @tagName(ref.form));
        }
        for (excerpt.relocations) |ref| edge(name, ref.name, @tagName(ref.kind));
        for (excerpt.data) |data| edge(name, data.name, "carried-data");
        if (excerpt.data_graph) |view| for (view.roots) |root| edge(name, view.graph.items[root].item.name, "data-root");
        try carried.add(excerpt);
    }
    try carried.items.appendSlice(a, host_visible);
    var seen = std.StringHashMap(void).init(a);
    defer seen.deinit();
    for (carried.items.items) |data| {
        if ((try seen.getOrPut(data.name)).found_existing) continue;
        std.debug.print("ctfe-graph\tdata\t{s}\t{d}\t{d}\n", .{ data.name, data.bytes.len, data.relocations.len });
        for (data.relocations) |ref| edge(data.name, ref.name, if (ref.function) "function-pointer" else "data-pointer");
    }
    std.debug.print("ctfe-graph\tend\n", .{});
}

fn edge(from: []const u8, to: []const u8, kind: []const u8) void {
    std.debug.print("ctfe-graph\tedge\t{s}\t{s}\t{s}\n", .{ from, to, kind });
}
