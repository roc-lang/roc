//! Regression test for issue #11131.

const std = @import("std");
const harness = @import("lower_to_lir_harness.zig");

const field_count = 50;
const update_count = 1000;

fn recordUpdateChainApp(gpa: std.mem.Allocator) (std.mem.Allocator.Error || std.Io.Writer.Error)![]u8 {
    var body: std.Io.Writer.Allocating = .init(gpa);
    defer body.deinit();
    const out = &body.writer;

    try out.writeAll("Big : {\n");
    for (0..field_count) |field| {
        try out.print("    f{d} : Try(Str, [Null]),\n", .{field});
    }
    try out.writeAll("}\n\nmk : Str -> Big\nmk = |s| {\n");
    for (0..field_count) |field| {
        try out.print("    f{d}: Ok(s),\n", .{field});
    }
    try out.writeAll("}\n\nbig : Str -> Str\nbig = |s| {\n    a0 = mk(s)\n");
    for (1..update_count + 1) |update| {
        try out.print(
            "    a{d} = {{ ..a{d}, f{d}: Ok(\"{d}\") }}\n",
            .{ update, update - 1, update % field_count, update },
        );
    }
    try out.print("    match a{d}.f0 {{ Ok(x) => x, Err(Null) => \"\" }}\n}}\n\n", .{update_count});
    try out.writeAll(
        \\main! = |args| {
        \\    echo!(big(args.len().to_str()))
        \\    Ok({})
        \\}
        \\
    );
    return body.toOwnedSlice();
}

test "issue 11131: a chain of record updates lowers without growing the native call stack" {
    // Use the dev compiler's optimization settings: the interpreter's .none
    // mode does not run the normalization that expanded these updates.
    const app_body = try recordUpdateChainApp(std.testing.allocator);
    defer std.testing.allocator.free(app_body);
    try harness.expectLowersToLirWithOptions(app_body, .{
        .inline_mode = .wrappers,
        .spec_constr_clone_inlining = .iterator_fusion,
    });
}
