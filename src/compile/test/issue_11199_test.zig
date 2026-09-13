//! Regression test for issue #11199.

const std = @import("std");
const build_options = @import("build_options");
const collections = @import("collections");
const eval = @import("eval");
const roc_target = @import("roc_target");

const Coordinator = @import("../coordinator.zig").Coordinator;
const CoreCtx = @import("ctx").CoreCtx;

test "issue 11199: a call site supplying the field types of a generic record checks cleanly" {
    const gpa = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "main.roc",
        .data =
        \\app [main!] {}
        \\
        \\to_json = |a, b| Json.to_str({ a, b })
        \\
        \\result = to_json("hi", {})
        \\
        \\main! = |_args| Ok({})
        ,
    });
    const app_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, "main.roc", gpa);
    defer gpa.free(app_path);
    var arena_impl = collections.SingleThreadArena.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();
    var builtin_modules = try eval.BuiltinModules.init(gpa);
    defer builtin_modules.deinit();
    var coord = try Coordinator.init(
        gpa,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        &builtin_modules,
        build_options.compiler_version,
        null,
        CoreCtx.default(gpa, arena, std.testing.io),
    );
    defer coord.deinit();
    try coord.start();
    try coord.discoverAppFromPath(arena, .{ .entry_path = app_path });
    try coord.coordinatorLoop();
    if (coord.hasUserErrors()) {
        var reports = coord.iterReports();
        while (reports.next()) |entry| {
            std.debug.print("report: {s} in {s}\n", .{ entry.report.title, entry.module_name });
        }
    }
    try std.testing.expect(!coord.hasUserErrors());
}

const harness = @import("lower_to_lir_harness.zig");

test "issue 11199: generic record and tuple codecs forward through runtime calls" {
    const source =
        \\to_json = |a, b| Json.to_str({ a, b })
        \\forward = |a, b| to_json(a, b)
        \\tuple_json = |a, b| Json.to_str((a, b))
        \\main! = |args| {
        \\    echo!(forward(args, {}))
        \\    echo!(forward(True, args))
        \\    echo!(tuple_json(args, False))
        \\    Ok({})
        \\}
    ;
    try harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .lss });
    try harness.expectLowersToLirWithOptions(source, .{ .boxy_plan_inspect = expectForwardedSchemeDictionaries });
}

test "issue 11199: generic parser requirements forward through runtime calls" {
    const source =
        \\parse_or = |a, b, text| match Json.parse(text) {
        \\    Ok(value) => value
        \\    Err(_) => { a, b }
        \\}
        \\forward = |a, b, text| parse_or(a, b, text)
        \\main! = |args| {
        \\    text = match args.first() {
        \\        Ok(first) => first
        \\        Err(_) => "{}"
        \\    }
        \\    value = forward("", {}, text)
        \\    echo!(value.a)
        \\    Ok({})
        \\}
    ;
    try harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .lss });
}

test "issue 11199: unified codec callable relations retain their schema evidence" {
    const source =
        \\to_json = |a, b| Json.to_str({ a, b }).concat(Json.to_str({ a, b }))
        \\main! = |args| {
        \\    echo!(to_json(args, {}))
        \\    Ok({})
        \\}
    ;
    try harness.expectLowersToLirWithOptions(source, .{ .monotype_only = true });
    try harness.expectLowersToLirWithOptions(source, .{ .boxy_plan_inspect = expectForwardedSchemeDictionaries });
}

test "issue 11199: imported generic codecs retain their complete evidence schema" {
    var dir = std.testing.tmpDir(.{});
    defer dir.cleanup();
    try dir.dir.writeFile(std.testing.io, .{
        .sub_path = "platform.roc",
        .data =
        \\platform ""
        \\    requires {} { main! : Str => Str }
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_main": run! }
        \\run! = |input| main!(input)
        ,
    });
    try dir.dir.writeFile(std.testing.io, .{
        .sub_path = "Codecs.roc",
        .data =
        \\module [to_json]
        \\to_json = |a, b| Json.to_str({ a, b })
        ,
    });
    try dir.dir.writeFile(std.testing.io, .{
        .sub_path = "main.roc",
        .data =
        \\app [main!] { pf: platform "./platform.roc" }
        \\import Codecs
        \\forward = |a, b| Codecs.to_json(a, b)
        \\main! = |input| forward(input, {})
        ,
    });
    const path = try dir.dir.realPathFileAlloc(std.testing.io, "main.roc", std.testing.allocator);
    defer std.testing.allocator.free(path);
    try harness.expectAppPathLowersToLirWithOptions(path, .{ .specialization_strategy = .lss });
    try harness.expectAppPathLowersToLirWithOptions(path, .{ .boxy_plan_inspect = expectForwardedSchemeDictionaries });
}

const postcheck = @import("postcheck");

fn expectForwardedSchemeDictionaries(plan: *const postcheck.Boxy.Plan.ProgramPlan) harness.LowerToLirHarnessError!void {
    var forwarded: usize = 0;
    for (plan.direct_calls.items) |call| {
        for (plan.directCallHiddenDictionaryArgSlice(call.hidden_dict_args)) |arg| {
            if (arg.source != .bound_dictionaries) continue;
            const source = arg.source.bound_dictionaries;
            const caller = plan.workers.items[@intFromEnum(call.caller)];
            for (plan.hiddenDictionaryParamSlice(caller.hidden_dicts)) |param| {
                if (param.scheme_param != null and param.dictionaries.start == source.start) {
                    forwarded += 1;
                    break;
                }
            }
        }
    }
    try std.testing.expect(forwarded > 0);
}

test "issue 11199: a local codec closure retains its evidence schema" {
    const source =
        \\main! = |args| {
        \\    prefix = args.first() ?? ""
        \\    encode = |a, b| prefix.concat(Json.to_str({ a, b }))
        \\    echo!(encode(args, {}))
        \\    echo!(encode(True, args))
        \\    Ok({})
        \\}
    ;
    try harness.expectLowersToLirWithOptions(source, .{ .monotype_only = true });
}

test "issue 11199: repeated equivalent codec uses reuse specializations" {
    const prefix =
        \\to_json = |a, b| Json.to_str({ a, b })
        \\forward = |a, b| to_json(a, b)
        \\main! = |args| {
        \\    echo!(forward(args, {}))
        \\
    ;
    const suffix =
        \\    Ok({})
        \\}
    ;
    var once: postcheck.Monotype.Lower.Diagnostics = .{};
    var repeated: postcheck.Monotype.Lower.Diagnostics = .{};
    try harness.expectLowersToLirWithOptions(prefix ++ suffix, .{ .monotype_only = true, .monotype_diagnostics_out = &once });
    try harness.expectLowersToLirWithOptions(prefix ++ "    echo!(forward(args, {}))\n" ++ suffix, .{ .monotype_only = true, .monotype_diagnostics_out = &repeated });
    try std.testing.expectEqual(once.specialization.template_misses, repeated.specialization.template_misses);
    try std.testing.expectEqual(once.specialization.nested_misses, repeated.specialization.nested_misses);
    try std.testing.expectEqual(@as(u64, 0), repeated.specialization.evidence_missing);
}

test "issue 11199: a concrete unsupported field is rejected during checking" {
    const gpa = std.testing.allocator;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "main.roc",
        .data =
        \\app [main!] {}
        \\to_json = |a, b| Json.to_str({ a, b })
        \\result = to_json(|x| x, {})
        \\main! = |_args| Ok({})
        ,
    });
    const app_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, "main.roc", gpa);
    defer gpa.free(app_path);
    var arena_impl = collections.SingleThreadArena.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();
    var builtin_modules = try eval.BuiltinModules.init(gpa);
    defer builtin_modules.deinit();
    var coord = try Coordinator.init(
        gpa,
        .single_threaded,
        1,
        roc_target.RocTarget.detectNative(),
        &builtin_modules,
        build_options.compiler_version,
        null,
        CoreCtx.default(gpa, arena, std.testing.io),
    );
    defer coord.deinit();
    try coord.start();
    try coord.discoverAppFromPath(arena, .{ .entry_path = app_path });
    try coord.coordinatorLoop();
    try std.testing.expect(coord.hasUserErrors());
}

test "issue 11199: higher order uses supply composite codec evidence" {
    const source =
        \\to_json = |a, b| Json.to_str({ a, b })
        \\apply = |encode, a, b| encode(a, b)
        \\main! = |args| {
        \\    echo!(apply(to_json, args, {}))
        \\    Ok({})
        \\}
    ;
    try harness.expectLowersToLirWithOptions(source, .{ .monotype_only = true });
}
