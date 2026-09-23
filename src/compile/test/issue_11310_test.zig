//! Regression test for issue #11310.

const std = @import("std");
const harness = @import("lower_to_lir_harness.zig");

// The same-field fallback and nested record update demand an outcome variant
// of List.set under dev lowering. Success must transfer the replacement Str
// into the result rather than also promising its entry unit to the caller.
const app_source =
    \\app [main!] { pf: platform "./platform.roc" }
    \\
    \\import pf.FallibleHost
    \\
    \\Upload : { chunk_hashes : List(Str) }
    \\Page : { upload : Upload }
    \\
    \\replace_first : Upload, Str -> List(Str)
    \\replace_first = |upload, event| upload.chunk_hashes.set(0, event) ?? upload.chunk_hashes
    \\
    \\update : Page, Str -> Page
    \\update = |page, msg| { ..page, upload: { chunk_hashes: replace_first(page.upload, msg) } }
    \\
    \\main! : () => Str
    \\main! = || {
    \\    msg = FallibleHost.json_input!({})
    \\    upload = { chunk_hashes: [] }
    \\    page = update({ upload: upload }, msg)
    \\    "${page.upload.chunk_hashes.len().to_str()}"
    \\}
;

const platform_source =
    \\platform ""
    \\    requires {} { main! : () => Str }
    \\    exposes [FallibleHost]
    \\    packages {}
    \\    provides { "roc_main": main_for_host! }
    \\    hosted { "roc_json_input": FallibleHost.json_input! }
    \\
    \\import FallibleHost
    \\
    \\main_for_host! : () => Str
    \\main_for_host! = main!
;

const fallible_host_source =
    \\FallibleHost := [].{
    \\    json_input! : {} => Str
    \\}
;

test "issue 11310: a set fallback to its own field transfers the replacement string" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(io, .{ .sub_path = "app.roc", .data = app_source });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "platform.roc", .data = platform_source });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "FallibleHost.roc", .data = fallible_host_source });

    const app_path = try tmp_dir.dir.realPathFileAlloc(io, "app.roc", gpa);
    defer gpa.free(app_path);

    // Both dev options are required to preserve the failing call shape.
    try harness.expectAppPathLowersToLirWithOptions(app_path, .{
        .inline_mode = .wrappers,
        .spec_constr_clone_inlining = .iterator_fusion,
    });
}
