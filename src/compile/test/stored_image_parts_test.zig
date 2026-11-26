//! A stored value holding a function stores each of its parts that holds no
//! function as an image of its own (design.md "Stored Images").

const std = @import("std");
const base = @import("base");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

const source =
    \\module [pair]
    \\pair : { add : U64 -> U64, label : Str, table : List(U64) }
    \\pair = {
    \\    add: |x| x + 1,
    \\    label: "a label longer than any small string",
    \\    table: List.repeat(0, 3).map_with_index(|_, i| i * 2),
    \\}
    \\
;

test "a function-holding constant stores its function-free parts as images" {
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        const gpa = std.testing.allocator;
        const io = std.testing.io;
        var tmp = std.testing.tmpDir(.{});
        defer tmp.cleanup();
        try tmp.dir.writeFile(io, .{ .sub_path = "Pair.roc", .data = source });
        const cwd = try tmp.dir.realPathFileAlloc(io, ".", gpa);
        defer gpa.free(cwd);
        const path = try tmp.dir.realPathFileAlloc(io, "Pair.roc", gpa);
        defer gpa.free(path);
        var build = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io, strategy);
        defer build.deinit();
        try build.build(path);
        const reports = try build.drainReports();
        defer build.freeDrainedReports(reports);
        for (reports) |module_reports| for (module_reports.reports) |report| {
            try std.testing.expect(report.severity != .runtime_error and report.severity != .fatal);
        };

        const artifact = build.findModuleByPath(path).?.semanticData().?.checked_artifact.?;
        const store = &artifact.const_store;
        var found = false;
        for (artifact.compile_time_roots.roots.items()) |root| {
            if (root.kind != .constant) continue;
            const node = switch (root.payloadFor(strategy)) {
                .const_node => |node| node,
                .fn_value, .pending, .unevaluated, .discarded, .expect, .runtime => continue,
            };
            const fields = switch (store.get(node)) {
                .record => |fields| fields,
                .image, .pending, .list, .box, .tuple, .crash, .checked_error, .tag, .nominal, .fn_value, .boxy_fn => continue,
            };
            // Fields in name order: `add`, `label`, `table`.
            try std.testing.expectEqual(@as(usize, 3), fields.len);
            try std.testing.expect(store.imageOf(fields[0]) == null);
            const label = store.imageOf(fields[1]) orelse return error.TestUnexpectedResult;
            try std.testing.expectEqualStrings("a label longer than any small string", store.image(label).strAt(0));
            const table = store.imageOf(fields[2]) orelse return error.TestUnexpectedResult;
            const bytes = store.image(table).bytes;
            const items = std.mem.readInt(u64, bytes[0..8], .little);
            try std.testing.expectEqual(@as(u64, 3), std.mem.readInt(u64, bytes[8..16], .little));
            for (0..3) |index| {
                try std.testing.expectEqual(@as(u64, index * 2), std.mem.readInt(u64, bytes[@intCast(items + index * 8)..][0..8], .little));
            }
            found = true;
        }
        try std.testing.expect(found);
    }
}
