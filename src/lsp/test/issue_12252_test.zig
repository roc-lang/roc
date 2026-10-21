//! Live-client regressions for compiler-recorded input refresh (ROC-12252).
const std = @import("std");
const lsp = @import("lsp");
const env = @import("integration_env.zig");
const spec = @import("integration_spec.zig");
const live = @import("live_harness.zig");

/// Regression specs for compiler-recorded live-input refresh and its LSP effects.
pub const specs = [_]spec.Spec{
    .{ .name = "ROC-12252 canonicalized cache hit rereads changed file import", .run = checked(canonicalizedReuse) },
    .{ .name = "ROC-12252 distinct closed dependency reports union in both orders", .run = checked(distinctUnion) },
    .{ .name = "ROC-12252 diagnostic ownership importer then dependency completion", .run = checked(ownershipImporterFirst) },
    .{ .name = "ROC-12252 buffer errors beat clean disk in both completion orders", .run = checked(bufferErrorOwnership) },
    .{ .name = "ROC-12252 pending independent result never substitutes disk errors", .run = checked(pendingPrecedence) },
    .{ .name = "ROC-12252 diagnostic ownership dependency then importer completion", .run = checked(ownershipDependencyFirst) },
    .{ .name = "ROC-12252 readability transitions refresh idle diagnostics", .run = checked(readability) },
    .{ .name = "ROC-12252 metadata and atomic identical replacement do not check", .run = checked(metadataReplacement) },
    .{ .name = "ROC-12252 failed check replaces dependency ownership", .run = checked(failedReplacement) },
    .{ .name = "ROC-12252 invalid package repairs and export edits refresh", .run = checked(packageExports) },
    .{ .name = "ROC-12252 fatal writer failure tears down worker", .run = checked(fatalWriter) },
    .{ .name = "ROC-12252 fatal reader failure tears down idle worker", .run = checked(fatalReader) },
    .{ .name = "ROC-12252 malformed framing tears down worker", .run = checked(malformedFraming) },
    .{ .name = "ROC-12252 harness complete frames and open idle input", .run = checked(harnessFrames) },
    .{ .name = "ROC-12252 harness EOF unblocks worker", .run = checked(harnessEof) },
    .{ .name = "ROC-12252 harness timeout cancellation unblocks worker", .run = checked(harnessTimeout) },
    .{ .name = "ROC-12252 idle Str input errors appear clear and framing survives", .run = checked(idleStr) },
    .{ .name = "ROC-12252 idle List U8 input errors appear clear", .run = checked(idleBytes) },
    .{ .name = "ROC-12252 idle imported Roc errors appear clear", .run = checked(idleRoc) },
    .{ .name = "ROC-12252 idle transitive file input errors appear clear", .run = checked(idleTransitive) },
    .{ .name = "ROC-12252 idle missing local package header repairs and rechecks", .run = checked(idlePackageHeader) },
    .{ .name = "ROC-12252 mutation after check converges without later events", .run = checked(afterCheck) },
    .{ .name = "ROC-12252 mutation during initial registration converges", .run = checked(duringRegistration) },
    .{ .name = "ROC-12252 injected startup failure reaches registration", .run = checked(startupFailure) },
    .{ .name = "ROC-12252 injected replacement failure reaches registration", .run = checked(replacementFailure) },
    .{ .name = "ROC-12252 injected idle runtime failure is visible", .run = checked(runtimeFailure) },
    .{ .name = "ROC-12252 unchanged bytes and unrelated writes do not check", .run = checked(unchangedInputs) },
    .{ .name = "ROC-12252 truncation before identical restore legitimately checks", .run = checked(truncatedInputs) },
    .{ .name = "ROC-12252 untrusted idle session never checks or registers", .run = checked(untrustedIdle) },
    .{ .name = "ROC-12252 replacement registration reconciles consumed state", .run = checked(replacementRace) },
    .{ .name = "ROC-12252 checked hover refreshes with compiler caches disabled", .run = checked(featuresUncached) },
    .{ .name = "ROC-12252 checked hover refreshes with warm compiler caches", .run = checked(featuresCached) },
    .{ .name = "ROC-12252 shared roots refresh without checking unrelated root", .run = checked(sharedRoots) },
    .{ .name = "ROC-12252 local platform invalid header repairs while idle", .run = checked(platformRepair) },
    .{ .name = "ROC-12252 missing parent chain repairs while idle", .run = checked(missingParents) },
    .{ .name = "ROC-12252 close reopen uses newly supplied buffer", .run = checked(closeReopen) },
    .{ .name = "ROC-12252 shutdown with pending check stops further work", .run = checked(pendingShutdown) },
    .{ .name = "ROC-12252 resolved header mutation converges at exact consumption boundary", .run = checked(resolutionRace) },
    .{ .name = "ROC-12252 queued editor version wins over pending dependency check", .run = checked(queuedEditorVersion) },
    .{ .name = "ROC-12252 missing local platform parent chain repairs", .run = checked(platformParents) },
    .{ .name = "ROC-12252 switching dependencies retains other root ownership", .run = checked(switchDependencies) },
    .{ .name = "ROC-12252 pending close cannot restore watch ownership", .run = checked(pendingClose) },
};

const LiveSpecError = spec.SpecError ||
    live.WaitError ||
    live.Server.RunError ||
    std.Thread.SpawnError ||
    std.Io.Event.WaitTimeoutError ||
    std.Io.Dir.DeleteFileError ||
    std.Io.Dir.RenameError ||
    std.Io.Dir.CreateDirError ||
    std.Io.Dir.ReadFileAllocError ||
    std.Io.File.SetPermissionsError ||
    std.Io.File.SetTimestampsError ||
    error{ TestExpectedError, TestUnexpectedError };

fn checked(comptime run: fn () LiveSpecError!void) spec.RunFn {
    return struct {
        fn call() spec.SpecError!void {
            run() catch |err| {
                std.debug.print("\nROC-12252 failure: {s}\n", .{@errorName(err)});
                return switch (err) {
                    error.SkipZigTest => error.SkipZigTest,
                    else => error.TestUnexpectedResult,
                };
            };
        }
    }.call;
}

fn canonicalizedReuse() LiveSpecError!void {
    var f: Fixture = undefined;
    try f.init();
    defer f.deinit();
    f.h.server.syntax_checker.cache_config.enabled = true;
    const source = try f.source("import \"input.txt\" as input : Str");
    defer env.allocator.free(source);
    try f.write("main.roc", source);
    try f.write("input.txt", "warm");
    try f.start();
    try f.open(f.uri, source, 1);
    try diagnostics(&f.h, f.uri, false);
    try f.reconcile();
    const checks = f.control.count(.after_check);
    f.control.hold(.after_check);
    try f.tmp.dir.deleteFile(env.io, "input.txt");
    try f.control.wait(.after_check, checks + 1, live.deadline(30000));
    const handle = f.h.server.syntax_checker.build_env orelse return error.TestUnexpectedResult;
    const build = handle.envPtr();
    const coord = build.coordinator orelse return error.TestUnexpectedResult;
    const root_name = build.discovered_pkg_name orelse return error.TestUnexpectedResult;
    const root_pkg = coord.getPackage(root_name) orelse return error.TestUnexpectedResult;
    const root_id = root_pkg.root_module_id orelse return error.TestUnexpectedResult;
    const root_module = root_pkg.getModule(root_id) orelse return error.TestUnexpectedResult;
    try std.testing.expect(root_module.canonicalized_cache_hit);
    f.control.release();
    try diagnostics(&f.h, f.uri, true);
    try f.write("input.txt", "restored");
    try diagnostics(&f.h, f.uri, false);
}

fn distinctUnion() LiveSpecError!void {
    var expected_union: ?[]u8 = null;
    defer if (expected_union) |p| env.allocator.free(p);
    for ([_]bool{ false, true }) |reverse| {
        var f: Fixture = undefined;
        try f.init();
        defer f.deinit();
        f.control.hooks.fail_startup.store(true, .seq_cst);
        const source = try f.source("import Dep\nvalue = Dep.value");
        defer env.allocator.free(source);
        const dep = try f.fileUri("Dep.roc");
        defer env.allocator.free(dep);
        const other = try f.fileUri("other.roc");
        defer env.allocator.free(other);
        try f.write("main.roc", source);
        try f.write("other.roc", source);
        const first_error = if (reverse) "Dep := Str.{\n\n    value : Str\n    value = 42\n}\n" else "Dep := Str.{\n    value : Str\n    value = 42\n}\n";
        const second_error = if (reverse) "Dep := Str.{\n    value : Str\n    value = 42\n}\n" else "Dep := Str.{\n\n    value : Str\n    value = 42\n}\n";
        try f.write("Dep.roc", first_error);
        try f.start();
        try f.open(f.uri, source, 1);
        const one = try expectPublication(&f, dep, 1, false);
        env.allocator.free(one);
        try f.write("Dep.roc", second_error);
        try f.open(other, source, 1);
        const combined = try expectPublication(&f, dep, 2, false);
        defer env.allocator.free(combined);
        // Payloads have no absolute paths: stable ordering must be independent
        // of the order in which roots completed their checks.
        if (expected_union) |p| try std.testing.expectEqualStrings(p, combined) else expected_union = try env.allocator.dupe(u8, combined);
        try f.close(f.uri);
        const remains = try expectPublication(&f, dep, 1, false);
        env.allocator.free(remains);
        try f.close(other);
        const empty = try expectPublication(&f, dep, 0, true);
        env.allocator.free(empty);
    }
}

fn bufferErrorOwnership() LiveSpecError!void {
    for ([_]bool{ false, true }) |dependency_first| {
        var f: Fixture = undefined;
        try f.init();
        defer f.deinit();
        const source = try f.source("import Dep\nvalue = Dep.value");
        defer env.allocator.free(source);
        const clean = "Dep := Str.{ value = \"disk\" }\n";
        const broken = "Dep := Str.{\n    value : Str\n    value = 42\n}\n";
        const dep = try f.fileUri("Dep.roc");
        defer env.allocator.free(dep);
        try f.write("main.roc", source);
        try f.write("Dep.roc", clean);
        try f.start();
        if (dependency_first) {
            try f.open(dep, broken, 1);
            const p = try expectPublication(&f, dep, 1, false);
            env.allocator.free(p);
        }
        try f.open(f.uri, source, 1);
        if (dependency_first) {
            const p = try expectPublication(&f, dep, 1, false);
            env.allocator.free(p);
        } else {
            try f.synchronize();
            try f.open(dep, broken, 1);
            const p = try expectPublication(&f, dep, 1, false);
            env.allocator.free(p);
        }
        // Removing the importer never clears an independently open buffer error.
        try f.close(f.uri);
        const p = try expectPublication(&f, dep, 1, false);
        env.allocator.free(p);
        try f.close(dep);
        const empty = try expectPublication(&f, dep, 0, true);
        env.allocator.free(empty);
    }
}

fn pendingPrecedence() LiveSpecError!void {
    var f: Fixture = undefined;
    try f.init();
    defer f.deinit();
    const source = try f.source("import Dep\nvalue = Dep.value");
    defer env.allocator.free(source);
    const dep = try f.fileUri("Dep.roc");
    defer env.allocator.free(dep);
    try f.write("main.roc", source);
    try f.write("Dep.roc", "Dep := Str.{\n    value : Str\n    value = 42\n}\n");
    try f.start();
    try f.open(f.uri, source, 1);
    const initial = try expectPublication(&f, dep, 1, false);
    env.allocator.free(initial);
    try f.reconcile();
    const count = f.control.count(.after_check);
    f.control.hold(.after_check);
    try f.open(dep, "Dep := Str.{ value = \"clean buffer\" }\n", 1);
    try f.control.wait(.after_check, count + 1, live.deadline(30000));
    // Only clearing is permitted while this independent result is pending.
    while (true) {
        var response = f.h.next(live.deadline(5)) catch |err| {
            switch (err) {
                error.Timeout => break,
                else => return err,
            }
        };
        defer response.deinit();
        const object = response.value.object;
        const method = object.get("method") orelse continue;
        if (method == .string and std.mem.eql(u8, method.string, "textDocument/publishDiagnostics")) {
            const params = object.get("params").?.object;
            if (std.mem.eql(u8, params.get("uri").?.string, dep)) try std.testing.expectEqual(@as(usize, 0), params.get("diagnostics").?.array.items.len);
        }
    }
    f.control.release();
    const clean = try expectPublication(&f, dep, 0, true);
    env.allocator.free(clean);
}

fn ownershipImporterFirst() LiveSpecError!void {
    try ownership(false);
}
fn ownershipDependencyFirst() LiveSpecError!void {
    try ownership(true);
}

const Publication = struct { payload: ?[]u8 = null, count: usize = 0, nonempty: usize = 0 };
fn publicationBoundary(f: *Fixture, uri: []const u8) live.WaitError!Publication {
    try f.h.sendBody("{\"jsonrpc\":\"2.0\",\"id\":92,\"method\":\"ownership-boundary\"}");
    var result: Publication = .{};
    errdefer if (result.payload) |p| env.allocator.free(p);
    const until = live.deadline(30000);
    while (true) {
        var response = try f.h.next(until);
        defer response.deinit();
        const object = response.value.object;
        if (object.get("id")) |id| {
            if (id == .integer and id.integer == 92) return result;
        }
        const method = object.get("method") orelse continue;
        if (method != .string or !std.mem.eql(u8, method.string, "textDocument/publishDiagnostics")) continue;
        const params = object.get("params").?.object;
        if (!std.mem.eql(u8, params.get("uri").?.string, uri)) continue;
        const items = params.get("diagnostics").?.array.items;
        if (items.len > 0) result.nonempty += 1;
        result.count = items.len;
        if (result.payload) |p| env.allocator.free(p);
        result.payload = try std.json.Stringify.valueAlloc(env.allocator, params.get("diagnostics").?, .{});
    }
}
fn expectPublication(f: *Fixture, uri: []const u8, expected: usize, forbid_nonempty: bool) (live.WaitError || error{ TestUnexpectedResult, TestExpectedEqual })![]u8 {
    const result = try publicationBoundary(f, uri);
    errdefer if (result.payload) |p| env.allocator.free(p);
    try std.testing.expect(result.payload != null);
    try std.testing.expectEqual(expected, result.count);
    if (forbid_nonempty) try std.testing.expectEqual(@as(usize, 0), result.nonempty);
    return result.payload.?;
}
fn ownership(dependency_first: bool) LiveSpecError!void {
    var f: Fixture = undefined;
    try f.init();
    defer f.deinit();
    const source = try f.source("import Dep\nvalue = Dep.value");
    defer env.allocator.free(source);
    const clean = "Dep := Str.{ value = \"buffer\" }\n";
    const broken = "Dep := Str.{\n    value : Str\n    value = 42\n}\n";
    const dep = try f.fileUri("Dep.roc");
    defer env.allocator.free(dep);
    const other = try f.fileUri("other.roc");
    defer env.allocator.free(other);
    const unrelated = try f.fileUri("unrelated.roc");
    defer env.allocator.free(unrelated);
    const unrelated_source = try f.source("");
    defer env.allocator.free(unrelated_source);
    try f.write("main.roc", source);
    try f.write("other.roc", source);
    try f.write("Dep.roc", broken);
    try f.write("unrelated.roc", unrelated_source);
    try f.start();
    if (dependency_first) {
        try f.open(dep, clean, 1);
        const p = try expectPublication(&f, dep, 0, true);
        env.allocator.free(p);
    }
    try f.open(f.uri, source, 1);
    if (!dependency_first) {
        const p = try expectPublication(&f, dep, 1, false);
        env.allocator.free(p);
    } else {
        const p = try expectPublication(&f, dep, 0, true);
        env.allocator.free(p);
    }
    if (!dependency_first) {
        try f.open(dep, clean, 1);
        const p = try expectPublication(&f, dep, 0, true);
        env.allocator.free(p);
    }
    try f.open(other, source, 1);
    const precedence = try expectPublication(&f, dep, 0, true);
    env.allocator.free(precedence);
    try f.close(dep);
    const union_payload = try expectPublication(&f, dep, 1, false);
    defer env.allocator.free(union_payload);
    // A clean unrelated check cannot erase another root's contribution.
    try f.open(unrelated, unrelated_source, 1);
    const after_clean = try expectPublication(&f, dep, 1, false);
    defer env.allocator.free(after_clean);
    try std.testing.expectEqualStrings(union_payload, after_clean);
    try f.change(unrelated_source, 2);
    const after_remove = try expectPublication(&f, dep, 1, false);
    defer env.allocator.free(after_remove);
    try std.testing.expectEqualStrings(union_payload, after_remove);
    try f.close(f.uri);
    const remaining = try expectPublication(&f, dep, 1, false);
    defer env.allocator.free(remaining);
    try std.testing.expectEqualStrings(union_payload, remaining);
    try f.close(other);
    const empty = try expectPublication(&f, dep, 0, true);
    env.allocator.free(empty);
}

const Termination = union(enum) {
    eof,
    read_failure,
    write_failure,
    malformed: struct { bytes: []const u8, expected: live.Server.RunError },
};

fn activeWatcherTermination(termination: Termination) LiveSpecError!void {
    var f: Fixture = undefined;
    try f.init();
    defer f.deinit();
    const source = try f.source("import \"input.txt\" as input : Str");
    defer env.allocator.free(source);
    try f.write("main.roc", source);
    try f.write("input.txt", "watched dependency");
    try f.start();
    try f.open(f.uri, source, 1);
    try f.control.wait(.after_registration, 1, live.deadline(30000));
    try diagnostics(&f.h, f.uri, false);
    // Finish an event-loop turn before inspecting worker-owned coverage.
    try f.synchronize();
    try std.testing.expect(f.h.server.watcher != null);
    try std.testing.expect(!f.h.finished.isSet());

    const expected: ?live.Server.RunError = switch (termination) {
        .eof => blk: {
            f.h.closeInput();
            break :blk null;
        },
        .read_failure => blk: {
            f.h.input.failRead();
            // Transport maps underlying read failures to EndOfStream.
            break :blk null;
        },
        .write_failure => blk: {
            f.h.output.close();
            try f.h.sendBody("{\"jsonrpc\":\"2.0\",\"id\":94,\"method\":\"termination-write\"}");
            break :blk error.WriteFailed;
        },
        .malformed => |frame| blk: {
            _ = try f.h.input.write(frame.bytes);
            break :blk frame.expected;
        },
    };
    if (expected) |err| {
        try std.testing.expectError(err, f.h.waitFinished(live.deadline(5000)));
    } else {
        try f.h.waitFinished(live.deadline(5000));
    }
    try std.testing.expect(f.h.finished.isSet());
    // run() must release coverage; Fixture.deinit must not hide a leak.
    try std.testing.expect(f.h.server.watcher == null);
}

fn fatalReader() LiveSpecError!void {
    try activeWatcherTermination(.read_failure);
}

fn fatalWriter() LiveSpecError!void {
    try activeWatcherTermination(.write_failure);
}

fn malformedFraming() LiveSpecError!void {
    const cases = [_]Termination{
        .{ .malformed = .{ .bytes = "Content-Length: nope\r\n\r\n", .expected = error.InvalidHeader } },
        .{ .malformed = .{ .bytes = "Wrong: 2\r\n\r\n{}", .expected = error.MissingContentLength } },
        .{ .malformed = .{ .bytes = "Content-Length: 999999999\r\n\r\n", .expected = error.PayloadTooLarge } },
    };
    for (cases) |case| try activeWatcherTermination(case);
    var q: live.Queue = .{};
    defer q.deinit();
    _ = try q.write("Invalid: 2\r\n\r\n{}");
    try std.testing.expectError(error.InvalidFrame, q.nextFrame(live.deadline(1000)));
}

fn readability() LiveSpecError!void {
    if (@import("builtin").os.tag == .windows) return error.SkipZigTest;
    var f: Fixture = undefined;
    try f.init();
    defer f.deinit();
    const source = try f.source("import \"input.txt\" as input : Str");
    defer env.allocator.free(source);
    try f.write("main.roc", source);
    try f.write("input.txt", "ok");
    const file = try f.tmp.dir.openFile(env.io, "input.txt", .{});
    defer file.close(env.io);
    try file.setPermissions(env.io, .fromMode(0));
    defer file.setPermissions(env.io, .fromMode(0o600)) catch {};
    if (f.tmp.dir.readFileAlloc(env.io, "input.txt", env.allocator, .limited(1024))) |bytes| {
        env.allocator.free(bytes);
        std.debug.print("ROC-12252 readability SKIP: environment can read mode-000 file\n", .{});
        return error.SkipZigTest;
    } else |err| {
        switch (err) {
            error.AccessDenied, error.PermissionDenied => {},
            else => return err,
        }
    }
    try file.setPermissions(env.io, .fromMode(0o600));
    try f.start();
    try f.open(f.uri, source, 1);
    try diagnostics(&f.h, f.uri, false);
    try file.setPermissions(env.io, .fromMode(0));
    try diagnostics(&f.h, f.uri, true);
    try file.setPermissions(env.io, .fromMode(0o600));
    try diagnostics(&f.h, f.uri, false);
}

fn metadataReplacement() LiveSpecError!void {
    var f: Fixture = undefined;
    try f.init();
    defer f.deinit();
    const source = try f.source("import \"input.txt\" as input : Str");
    defer env.allocator.free(source);
    try f.write("main.roc", source);
    try f.write("input.txt", "same");
    try f.start();
    try f.open(f.uri, source, 1);
    try diagnostics(&f.h, f.uri, false);
    try f.reconcile();
    const checks = f.control.hooks.checks.load(.seq_cst);
    const file = try f.tmp.dir.openFile(env.io, "input.txt", .{});
    defer file.close(env.io);
    try file.setTimestampsNow(env.io);
    try f.reconcile();
    try std.testing.expectEqual(checks, f.control.hooks.checks.load(.seq_cst));
    try f.write("replacement.txt", "same");
    try f.tmp.dir.rename("replacement.txt", f.tmp.dir, "input.txt", env.io);
    try f.reconcile();
    try std.testing.expectEqual(checks, f.control.hooks.checks.load(.seq_cst));
    // Coverage must survive the rename even though identical bytes did not check.
    try f.tmp.dir.deleteFile(env.io, "input.txt");
    try diagnostics(&f.h, f.uri, true);
}

fn failedReplacement() LiveSpecError!void {
    var f: Fixture = undefined;
    try f.init();
    defer f.deinit();
    const first = try f.source("import \"d1.txt\" as input : Str");
    defer env.allocator.free(first);
    const failed = try f.source("import \"d2.txt\" as input : Str");
    defer env.allocator.free(failed);
    try f.write("main.roc", first);
    try f.write("d1.txt", "ok");
    try f.start();
    try f.open(f.uri, first, 1);
    try diagnostics(&f.h, f.uri, false);
    try f.change(failed, 2);
    try diagnostics(&f.h, f.uri, true);
    try f.reconcile();
    const checks = f.control.hooks.checks.load(.seq_cst);
    try f.write("d1.txt", "no longer owned");
    try f.reconcile();
    try std.testing.expectEqual(checks, f.control.hooks.checks.load(.seq_cst));
    try f.write("d2.txt", "new ownership");
    try diagnostics(&f.h, f.uri, false);
}

fn packageExports() LiveSpecError!void {
    var f: Fixture = undefined;
    try f.init();
    defer f.deinit();
    const source = try std.fmt.allocPrint(env.allocator, "app [process_string] {{ dep: \"./pkg/main.roc\", pf: platform \"{s}\" }}\nimport dep.Thing\nvalue = Thing.value\nprocess_string = |text| text\n", .{env.tmp_dir_platform_path});
    defer env.allocator.free(source);
    try f.tmp.dir.createDir(env.io, "pkg", .default_dir);
    try f.write("main.roc", source);
    try f.write("pkg/main.roc", "not a package\n");
    try f.write("pkg/Thing.roc", "Thing := Str.{ value = \"ok\" }\n");
    try f.start();
    try f.open(f.uri, source, 1);
    try diagnostics(&f.h, f.uri, true);
    try f.write("pkg/main.roc", "package [Thing] {}\n");
    try diagnostics(&f.h, f.uri, false);
    try f.write("pkg/main.roc", "package [] {}\n");
    try diagnostics(&f.h, f.uri, true);
    try f.write("pkg/main.roc", "package [Thing] {}\n");
    try diagnostics(&f.h, f.uri, false);
}

fn harnessFrames() LiveSpecError!void {
    var q: live.Queue = .{};
    defer q.deinit();
    _ = try q.write("Content-Length: 2\r\n\r\n{");
    try std.testing.expectError(error.Timeout, q.nextFrame(live.deadline(10)));
    _ = try q.write("}Content-Length: 4\r\n\r\nnull");
    const first = try q.nextFrame(live.deadline(1000));
    defer env.allocator.free(first);
    const second = try q.nextFrame(live.deadline(1000));
    defer env.allocator.free(second);
    try std.testing.expectEqualStrings("{}", first);
    try std.testing.expectEqualStrings("null", second);
    try std.testing.expect(!q.closed);

    var h: live.Live = undefined;
    try h.init(".zig-cache");
    defer h.deinit();
    try h.start();
    try h.input.waiting.waitTimeout(env.io, live.deadline(5000));
    try std.testing.expect(!h.finished.isSet());
    try initialize(&h);
    try std.testing.expect(!h.finished.isSet());
    try h.sendBody("{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"shutdown\"}");
    var response = try responseById(&h, 2);
    defer response.deinit();
    try h.sendBody("{\"jsonrpc\":\"2.0\",\"method\":\"exit\"}");
    try h.waitFinished(live.deadline(5000));
}

fn harnessEof() LiveSpecError!void {
    try activeWatcherTermination(.eof);
}

fn harnessTimeout() LiveSpecError!void {
    var h: live.Live = undefined;
    try h.init(".zig-cache");
    defer h.deinit();
    try h.start();
    try h.input.waiting.waitTimeout(env.io, live.deadline(5000));
    try std.testing.expectError(error.Timeout, h.next(live.deadline(10)));
    try std.testing.expect(!h.finished.isSet());
    h.server.transport.reader.cancel();
    try h.waitFinished(live.deadline(5000));
}

fn untrustedIdle() LiveSpecError!void {
    var h: live.Live = undefined;
    try h.init(".zig-cache");
    defer h.deinit();
    var control: live.Control = .{};
    control.init(&h);
    try h.start();
    try h.sendBody(
        \\{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":1,"rootUri":null,"capabilities":{}}}
    );
    var initialized = try responseById(&h, 1);
    initialized.deinit();
    try h.sendBody("{\"jsonrpc\":\"2.0\",\"method\":\"initialized\",\"params\":{}}");
    try h.send(.{ .jsonrpc = "2.0", .method = "textDocument/didOpen", .params = .{ .textDocument = .{ .uri = "file:///tmp/roc-12252-untrusted.roc", .version = 1, .text = "invalid source" } } });
    // This response follows didOpen in the serialized loop and is the observed
    // boundary for checking that the notification did not enter the compiler.
    try h.send(.{ .jsonrpc = "2.0", .id = @as(i64, 3), .method = "textDocument/semanticTokens/full", .params = .{ .textDocument = .{ .uri = "file:///tmp/roc-12252-untrusted.roc" } } });
    var response = try responseById(&h, 3);
    defer response.deinit();
    try std.testing.expect(response.value.object.get("error") != null);
    try std.testing.expectEqual(@as(usize, 0), control.hooks.checks.load(.seq_cst));
    try std.testing.expectEqual(@as(usize, 0), control.count(.before_registration));
}

fn initialize(h: *live.Live) (live.WaitError || error{TestUnexpectedResult})!void {
    try h.sendBody(
        \\{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":1,"rootUri":null,"capabilities":{},"initializationOptions":{"trustedWorkspace":true}}}
    );
    var response = try responseById(h, 1);
    defer response.deinit();
    try std.testing.expect(response.value.object.get("result") != null);
    try h.sendBody("{\"jsonrpc\":\"2.0\",\"method\":\"initialized\",\"params\":{}}");
}

fn responseById(h: *live.Live, id: i64) live.WaitError!std.json.Parsed(std.json.Value) {
    const until = live.deadline(30000);
    while (true) {
        var response = try h.next(until);
        if (response.value.object.get("id")) |value| {
            if (value == .integer and value.integer == id) return response;
        }
        response.deinit();
    }
}

fn diagnostics(h: *live.Live, uri: []const u8, has_errors: bool) live.WaitError!void {
    const until = live.deadline(30000);
    while (true) {
        var response = try h.next(until);
        defer response.deinit();
        const object = response.value.object;
        const method = object.get("method") orelse continue;
        if (method != .string or !std.mem.eql(u8, method.string, "textDocument/publishDiagnostics")) continue;
        const params = object.get("params").?.object;
        if (!std.mem.eql(u8, params.get("uri").?.string, uri)) continue;
        const items = params.get("diagnostics").?.array.items;
        if ((items.len != 0) == has_errors) return;
        for (items) |item| std.debug.print("\n{s}\n", .{item.object.get("message").?.string});
    }
}

fn idlePackageHeader() LiveSpecError!void {
    var tmp = env.tmpDir(.{});
    defer tmp.cleanup();
    const directory = try tmp.dir.realPathFileAlloc(env.io, ".", env.allocator);
    defer env.allocator.free(directory);
    const path = try std.fs.path.join(env.allocator, &.{ directory, "main.roc" });
    defer env.allocator.free(path);
    const uri = try lsp.uri.pathToUri(env.allocator, path);
    defer env.allocator.free(uri);
    const source = try std.fmt.allocPrint(env.allocator, "app [process_string] {{ dep: \"./dep.roc\", pf: platform \"{s}\" }}\nprocess_string = |text| text\n", .{env.tmp_dir_platform_path});
    defer env.allocator.free(source);
    try tmp.dir.writeFile(env.io, .{ .sub_path = "main.roc", .data = source });
    var h: live.Live = undefined;
    try h.init(directory);
    defer h.deinit();
    try h.start();
    try initialize(&h);
    try h.send(.{ .jsonrpc = "2.0", .method = "textDocument/didOpen", .params = .{ .textDocument = .{ .uri = uri, .version = 1, .text = source } } });
    try diagnostics(&h, uri, true);
    try tmp.dir.writeFile(env.io, .{ .sub_path = "dep.roc", .data = "package [] {}\n" });
    try diagnostics(&h, uri, false);
    try tmp.dir.deleteFile(env.io, "dep.roc");
    try diagnostics(&h, uri, true);
    try tmp.dir.writeFile(env.io, .{ .sub_path = "dep.roc", .data = "package [] {}\n" });
    try diagnostics(&h, uri, false);
}

const Mode = enum { ordinary, after_check, registration, startup_failure, replacement_failure, runtime_failure, unchanged, truncated };
fn afterCheck() LiveSpecError!void {
    try idleCaseMode(.str, .after_check);
}
fn duringRegistration() LiveSpecError!void {
    try idleCaseMode(.str, .registration);
}
fn startupFailure() LiveSpecError!void {
    try idleCaseMode(.str, .startup_failure);
}
fn replacementFailure() LiveSpecError!void {
    try idleCaseMode(.str, .replacement_failure);
}
fn runtimeFailure() LiveSpecError!void {
    try idleCaseMode(.str, .runtime_failure);
}
fn truncatedInputs() LiveSpecError!void {
    try idleCaseMode(.str, .truncated);
}
fn unchangedInputs() LiveSpecError!void {
    try idleCaseMode(.str, .unchanged);
}

fn ordinaryChangeAfterFailure(h: *live.Live, control: *live.Control, uri: []const u8) (live.WaitError || error{TestUnexpectedResult})!void {
    const text = try std.fmt.allocPrint(env.allocator, "app [process_string] {{ pf: platform \"{s}\" }}\nprocess_string : Str -> Str\nprocess_string = |_text| 42\n", .{env.tmp_dir_platform_path});
    defer env.allocator.free(text);
    const checks = control.hooks.checks.load(.seq_cst);
    try h.send(.{ .jsonrpc = "2.0", .method = "textDocument/didChange", .params = .{ .textDocument = .{ .uri = uri, .version = @as(i64, 10) }, .contentChanges = .{.{ .text = text }} } });
    try diagnostics(h, uri, true);
    try std.testing.expect(control.hooks.checks.load(.seq_cst) > checks);
}

fn watchFailure(h: *live.Live) live.WaitError!void {
    const until = live.deadline(30000);
    while (true) {
        var response = try h.next(until);
        defer response.deinit();
        const method = response.value.object.get("method") orelse continue;
        if (method == .string and std.mem.eql(u8, method.string, "window/showMessage")) return;
    }
}

fn idleStr() LiveSpecError!void {
    try idleCase(.str);
}
fn idleBytes() LiveSpecError!void {
    try idleCase(.bytes);
}
fn idleRoc() LiveSpecError!void {
    try idleCase(.roc);
}
fn idleTransitive() LiveSpecError!void {
    try idleCase(.transitive);
}
const Kind = enum { str, bytes, roc, transitive };

const Fixture = struct {
    tmp: env.TmpDir = undefined,
    directory: [:0]u8 = undefined,
    uri: []u8 = undefined,
    h: live.Live = undefined,
    control: live.Control = .{},
    fn init(f: *Fixture) (std.Io.Dir.RealPathFileAllocError || std.mem.Allocator.Error)!void {
        f.* = .{};
        f.tmp = env.tmpDir(.{});
        f.directory = try f.tmp.dir.realPathFileAlloc(env.io, ".", env.allocator);
        f.uri = try f.fileUri("main.roc");
        try f.h.init(f.directory);
        f.control.init(&f.h);
    }
    fn start(f: *Fixture) (std.Thread.SpawnError || live.WaitError || error{TestUnexpectedResult})!void {
        try f.h.start();
        try initialize(&f.h);
    }
    fn deinit(f: *Fixture) void {
        f.control.release();
        f.h.deinit();
        env.allocator.free(f.uri);
        env.allocator.free(f.directory);
        f.tmp.cleanup();
    }
    fn write(f: *Fixture, name: []const u8, text: []const u8) std.Io.Dir.WriteFileError!void {
        try f.tmp.dir.writeFile(env.io, .{ .sub_path = name, .data = text });
    }
    fn fileUri(f: *Fixture, name: []const u8) std.mem.Allocator.Error![]u8 {
        const path = try std.fs.path.join(env.allocator, &.{ f.directory, name });
        defer env.allocator.free(path);
        return lsp.uri.pathToUri(env.allocator, path);
    }
    fn source(_: *Fixture, imports: []const u8) std.mem.Allocator.Error![]u8 {
        return std.fmt.allocPrint(env.allocator, "app [process_string] {{ pf: platform \"{s}\" }}\n{s}\nprocess_string = |text| text\n", .{ env.tmp_dir_platform_path, imports });
    }
    fn open(f: *Fixture, uri: []const u8, text: []const u8, version: i64) error{ OutOfMemory, Closed }!void {
        try f.h.send(.{ .jsonrpc = "2.0", .method = "textDocument/didOpen", .params = .{ .textDocument = .{ .uri = uri, .text = text, .version = version } } });
    }
    fn change(f: *Fixture, text: []const u8, version: i64) error{ OutOfMemory, Closed }!void {
        try f.h.send(.{ .jsonrpc = "2.0", .method = "textDocument/didChange", .params = .{ .textDocument = .{ .uri = f.uri, .version = version }, .contentChanges = .{.{ .text = text }} } });
    }
    fn close(f: *Fixture, uri: []const u8) error{ OutOfMemory, Closed }!void {
        try f.h.send(.{ .jsonrpc = "2.0", .method = "textDocument/didClose", .params = .{ .textDocument = .{ .uri = uri } } });
    }
    fn synchronize(f: *Fixture) live.WaitError!void {
        try f.h.sendBody("{\"jsonrpc\":\"2.0\",\"id\":91,\"method\":\"unknown-sync-request\"}");
        var response = try responseById(&f.h, 91);
        response.deinit();
    }
    fn consumeWatchReadiness(f: *Fixture) (live.WaitError || error{TestUnexpectedResult})!void {
        const until = live.deadline(5000).deadline;
        f.h.server.watch_event.set(env.io);
        while (f.h.server.watch_event.isSet()) {
            if (std.Io.Clock.Timestamp.now(env.io, .awake).compare(.gte, until)) return error.Timeout;
            std.atomic.spinLoopHint();
        }
        // Reset precedes refreshInputs; the response proves that call completed.
        try f.synchronize();
    }
    fn reconcile(f: *Fixture) std.Io.Condition.WaitTimeoutError!void {
        try f.control.reconcile(&f.h, live.deadline(30000));
    }
};

fn resolutionRace() LiveSpecError!void {
    var f: Fixture = undefined;
    try f.init();
    defer f.deinit();
    const source = try std.fmt.allocPrint(env.allocator, "app [process_string] {{ dep: \"./dep.roc\", pf: platform \"{s}\" }}\nprocess_string = |text| text\n", .{env.tmp_dir_platform_path});
    defer env.allocator.free(source);
    try f.write("main.roc", source);
    try f.write("dep.roc", "package [] {}\n");
    const Mutation = struct {
        fixture: *Fixture,
        calls: std.atomic.Value(usize) = .init(0),
        write_error: ?std.Io.Dir.WriteFileError = null,
        fn call(context: *anyopaque) error{OutOfMemory}!void {
            const m: *@This() = @ptrCast(@alignCast(context));
            if (m.calls.fetchAdd(1, .seq_cst) == 0) {
                m.fixture.write("dep.roc", "package [] { nested: \"./newly-discovered.roc\" }\n") catch |err| {
                    m.write_error = err;
                };
            }
        }
    };
    var mutation: Mutation = .{ .fixture = &f };
    f.control.hooks.after_resolution = .{ .ctx = &mutation, .call = Mutation.call };
    try f.start();
    try f.open(f.uri, source, 1);
    // The mutation occurs before watches exist. No subsequent event can rescue
    // an implementation that records a post-resolution sample as consumed.
    try f.control.wait(.after_check, 2, live.deadline(30000));
    try std.testing.expectEqual(@as(usize, 1), mutation.calls.load(.seq_cst));
    if (mutation.write_error) |err| return err;
    try diagnostics(&f.h, f.uri, true);
    // Repair only the newly discovered input, proving ownership converged too.
    try f.write("newly-discovered.roc", "package [] {}\n");
    try diagnostics(&f.h, f.uri, false);
}

fn replacementRace() LiveSpecError!void {
    var f: Fixture = undefined;
    try f.init();
    defer f.deinit();
    const source = try f.source("import \"input.txt\" as input : Str");
    defer env.allocator.free(source);
    try f.write("main.roc", source);
    try f.write("input.txt", "first");
    try f.start();
    try f.open(f.uri, source, 1);
    try diagnostics(&f.h, f.uri, false);
    try f.reconcile();
    const registration = f.control.count(.before_registration);
    f.control.hold(.before_registration);
    try f.write("input.txt", "second");
    try f.control.wait(.before_registration, registration + 1, live.deadline(30000));
    try f.tmp.dir.deleteFile(env.io, "input.txt");
    f.control.release();
    try diagnostics(&f.h, f.uri, true);
    try f.write("input.txt", "third");
    try diagnostics(&f.h, f.uri, false);
}

fn featuresUncached() LiveSpecError!void {
    try featureFreshness(false);
}
fn featuresCached() LiveSpecError!void {
    try featureFreshness(true);
}
fn hover(f: *Fixture, expected: []const u8) (live.WaitError || error{TestUnexpectedResult})!void {
    try f.h.send(.{ .jsonrpc = "2.0", .id = @as(i64, 44), .method = "textDocument/hover", .params = .{ .textDocument = .{ .uri = f.uri }, .position = .{ .line = @as(u32, 2), .character = @as(u32, 1) } } });
    var response = try responseById(&f.h, 44);
    defer response.deinit();
    const encoded = try std.json.Stringify.valueAlloc(env.allocator, response.value, .{});
    defer env.allocator.free(encoded);
    if (std.mem.find(u8, encoded, expected) == null) {
        std.debug.print("hover expected {s}: {s}\n", .{ expected, encoded });
        return error.TestUnexpectedResult;
    }
}
fn featureFreshness(cache: bool) LiveSpecError!void {
    var f: Fixture = undefined;
    try f.init();
    defer f.deinit();
    f.h.server.syntax_checker.cache_config.enabled = cache;
    const source = try f.source("import Dep\nvalue = Dep.value");
    defer env.allocator.free(source);
    // The target disk source intentionally differs from its editor buffer.
    try f.write("main.roc", "invalid disk target");
    try f.write("Dep.roc", "Dep := Str.{ value = \"saved\" }\n");
    try f.start();
    try f.open(f.uri, source, 1);
    try diagnostics(&f.h, f.uri, false);
    try hover(&f, "Str");
    try hover(&f, "Str");
    const dep_uri = try f.fileUri("Dep.roc");
    defer env.allocator.free(dep_uri);
    // An independently open dependency does not become an importer overlay.
    try f.open(dep_uri, "Dep := Str.{ value = True }\n", 1);
    try diagnostics(&f.h, dep_uri, false);
    try hover(&f, "Str");
    const checks = f.control.hooks.checks.load(.seq_cst);
    try f.write("Dep.roc", "Dep := Str.{ value = True }\n");
    try f.control.wait(.after_check, checks + 1, live.deadline(30000));
    try f.synchronize();
    try hover(&f, "[True]");
    // The same target text in a newer editor version must retain fresh data.
    try f.change(source, 2);
    try f.synchronize();
    try hover(&f, "[True]");
}

fn sharedRoots() LiveSpecError!void {
    var f: Fixture = undefined;
    try f.init();
    defer f.deinit();
    const source = try f.source("import \"input.txt\" as input : Str");
    defer env.allocator.free(source);
    const unrelated = try f.source("");
    defer env.allocator.free(unrelated);
    const second = try f.fileUri("second.roc");
    defer env.allocator.free(second);
    const third = try f.fileUri("third.roc");
    defer env.allocator.free(third);
    try f.write("main.roc", source);
    try f.write("second.roc", source);
    try f.write("third.roc", unrelated);
    try f.write("input.txt", "ok");
    try f.start();
    try f.open(f.uri, source, 1);
    try diagnostics(&f.h, f.uri, false);
    try f.open(second, source, 1);
    try diagnostics(&f.h, second, false);
    try f.open(third, unrelated, 1);
    try diagnostics(&f.h, third, false);
    try f.reconcile();
    const checks = f.control.hooks.checks.load(.seq_cst);
    try f.write("input.txt", "new bytes");
    try f.control.wait(.after_check, checks + 2, live.deadline(30000));
    try f.reconcile();
    try std.testing.expectEqual(checks + 2, f.control.hooks.checks.load(.seq_cst));
    try f.close(f.uri);
    try f.synchronize();
    const remaining = f.control.hooks.checks.load(.seq_cst);
    try f.write("input.txt", "another edit");
    try f.control.wait(.after_check, remaining + 1, live.deadline(30000));
    try f.reconcile();
    try std.testing.expectEqual(remaining + 1, f.control.hooks.checks.load(.seq_cst));
}

const local_platform = "platform \"\"\n    requires {} { process_string : Str -> Str }\n    exposes []\n    packages {}\n    provides { \"entry\": entry }\nentry = |text| process_string(text)\n";

fn queuedEditorVersion() LiveSpecError!void {
    var f: Fixture = undefined;
    try f.init();
    defer f.deinit();
    const original = try f.source("import \"input.txt\" as input : Str\nvalue = input");
    defer env.allocator.free(original);
    const latest = try f.source("import \"input.txt\" as input : Str\nvalue = True");
    defer env.allocator.free(latest);
    try f.write("main.roc", original);
    try f.write("input.txt", "ok");
    try f.start();
    try f.open(f.uri, original, 1);
    try diagnostics(&f.h, f.uri, false);
    try f.reconcile();
    const checks = f.control.hooks.checks.load(.seq_cst);
    f.control.hold(.after_check);
    try f.write("input.txt", "changed");
    try f.control.wait(.after_check, checks + 1, live.deadline(30000));
    try f.change(latest, 9);
    f.control.release();
    try f.synchronize();
    try hover(&f, "[True]");
    try f.reconcile();
    try hover(&f, "[True]");
}

fn platformParents() LiveSpecError!void {
    var f: Fixture = undefined;
    try f.init();
    defer f.deinit();
    const source = "app [process_string] { pf: platform \"./a/b/platform.roc\" }\nprocess_string = |text| text\n";
    try f.write("main.roc", source);
    try f.start();
    try f.open(f.uri, source, 1);
    try diagnostics(&f.h, f.uri, true);
    try f.reconcile();
    const checks = f.control.hooks.checks.load(.seq_cst);
    try f.tmp.dir.createDir(env.io, "a", .default_dir);
    try f.reconcile();
    try f.tmp.dir.createDir(env.io, "a/b", .default_dir);
    try f.reconcile();
    try std.testing.expectEqual(checks, f.control.hooks.checks.load(.seq_cst));
    try f.write("a/b/platform.roc", local_platform);
    try diagnostics(&f.h, f.uri, false);
}

fn platformRepair() LiveSpecError!void {
    var f: Fixture = undefined;
    try f.init();
    defer f.deinit();
    const source = "app [process_string] { pf: platform \"./platform.roc\" }\nprocess_string = |text| text\n";
    const platform = local_platform;
    try f.write("main.roc", source);
    try f.write("platform.roc", "not a platform\n");
    try f.start();
    try f.open(f.uri, source, 1);
    try diagnostics(&f.h, f.uri, true);
    try f.write("platform.roc", platform);
    try diagnostics(&f.h, f.uri, false);
    try f.write("platform.roc", "package [] {}\n");
    try diagnostics(&f.h, f.uri, true);
    try f.write("platform.roc", platform);
    try diagnostics(&f.h, f.uri, false);
}

fn missingParents() LiveSpecError!void {
    var f: Fixture = undefined;
    try f.init();
    defer f.deinit();
    const source = try f.source("import \"a/b/input.txt\" as input : Str");
    defer env.allocator.free(source);
    try f.write("main.roc", source);
    try f.start();
    try f.open(f.uri, source, 1);
    try diagnostics(&f.h, f.uri, true);
    try f.reconcile();
    const checks = f.control.hooks.checks.load(.seq_cst);
    try f.tmp.dir.createDir(env.io, "a", .default_dir);
    try f.reconcile();
    try std.testing.expectEqual(checks, f.control.hooks.checks.load(.seq_cst));
    try f.tmp.dir.createDir(env.io, "a/b", .default_dir);
    try f.reconcile();
    try std.testing.expectEqual(checks, f.control.hooks.checks.load(.seq_cst));
    try f.write("a/b/input.txt", "repaired");
    try diagnostics(&f.h, f.uri, false);
}

fn switchDependencies() LiveSpecError!void {
    var f: Fixture = undefined;
    try f.init();
    defer f.deinit();
    const first = try f.source("import \"d1.txt\" as input : Str");
    defer env.allocator.free(first);
    const second = try f.source("import \"d2.txt\" as input : Str");
    defer env.allocator.free(second);
    const other = try f.fileUri("other.roc");
    defer env.allocator.free(other);
    try f.write("main.roc", first);
    try f.write("other.roc", first);
    try f.write("d1.txt", "one");
    try f.write("d2.txt", "two");
    try f.start();
    try f.open(f.uri, first, 1);
    try diagnostics(&f.h, f.uri, false);
    try f.open(other, first, 1);
    try diagnostics(&f.h, other, false);
    try f.change(second, 2);
    try f.synchronize();
    try f.reconcile();
    const before = f.control.hooks.checks.load(.seq_cst);
    try f.write("d1.txt", "changed");
    try f.control.wait(.after_check, before + 1, live.deadline(30000));
    try f.reconcile();
    try std.testing.expectEqual(before + 1, f.control.hooks.checks.load(.seq_cst));
    try f.close(other);
    try f.synchronize();
    try f.reconcile();
    const closed = f.control.hooks.checks.load(.seq_cst);
    try f.write("d1.txt", "closed");
    try f.reconcile();
    try std.testing.expectEqual(closed, f.control.hooks.checks.load(.seq_cst));
    try f.write("d2.txt", "active");
    try f.control.wait(.after_check, closed + 1, live.deadline(30000));
}

fn pendingClose() LiveSpecError!void {
    var f: Fixture = undefined;
    try f.init();
    defer f.deinit();
    const source = try f.source("import \"input.txt\" as input : Str");
    defer env.allocator.free(source);
    try f.write("main.roc", source);
    try f.write("input.txt", "ok");
    try f.start();
    try f.open(f.uri, source, 1);
    try diagnostics(&f.h, f.uri, false);
    try f.reconcile();
    const before = f.control.hooks.checks.load(.seq_cst);
    f.control.hold(.after_check);
    try f.write("input.txt", "pending");
    try f.control.wait(.after_check, before + 1, live.deadline(30000));
    try f.close(f.uri);
    f.control.release();
    try f.synchronize();
    try f.reconcile();
    const closed = f.control.hooks.checks.load(.seq_cst);
    try f.write("input.txt", "after close");
    try f.reconcile();
    try std.testing.expectEqual(closed, f.control.hooks.checks.load(.seq_cst));
}

fn closeReopen() LiveSpecError!void {
    var f: Fixture = undefined;
    try f.init();
    defer f.deinit();
    const source = try f.source("import \"input.txt\" as input : Str");
    defer env.allocator.free(source);
    const reopened = try f.source("");
    defer env.allocator.free(reopened);
    try f.write("main.roc", source);
    try f.write("input.txt", "ok");
    try f.start();
    try f.open(f.uri, source, 1);
    try diagnostics(&f.h, f.uri, false);
    try f.close(f.uri);
    try f.synchronize();
    try f.open(f.uri, reopened, 7);
    try diagnostics(&f.h, f.uri, false);
    try f.reconcile();
    const checks = f.control.hooks.checks.load(.seq_cst);
    try f.tmp.dir.deleteFile(env.io, "input.txt");
    try f.reconcile();
    try std.testing.expectEqual(checks, f.control.hooks.checks.load(.seq_cst));
}

fn pendingShutdown() LiveSpecError!void {
    var f: Fixture = undefined;
    try f.init();
    defer f.deinit();
    const source = try f.source("import \"input.txt\" as input : Str");
    defer env.allocator.free(source);
    try f.write("main.roc", source);
    try f.write("input.txt", "ok");
    try f.start();
    try f.open(f.uri, source, 1);
    try diagnostics(&f.h, f.uri, false);
    try f.reconcile();
    const checks = f.control.hooks.checks.load(.seq_cst);
    f.control.hold(.after_check);
    try f.write("input.txt", "trigger");
    try f.control.wait(.after_check, checks + 1, live.deadline(30000));
    try f.h.sendBody("{\"jsonrpc\":\"2.0\",\"id\":81,\"method\":\"shutdown\"}");
    f.control.release();
    var response = try responseById(&f.h, 81);
    response.deinit();
    // The response can precede run()'s stopWatching; cross another turn first.
    try f.synchronize();
    try std.testing.expect(f.h.server.watcher == null);
    const final_checks = f.control.hooks.checks.load(.seq_cst);
    const publications = f.control.hooks.publications.load(.seq_cst);
    try f.write("input.txt", "after shutdown");
    try f.consumeWatchReadiness();
    try std.testing.expect(f.h.server.watcher == null);
    try std.testing.expectEqual(final_checks, f.control.hooks.checks.load(.seq_cst));
    try std.testing.expectEqual(publications, f.control.hooks.publications.load(.seq_cst));
    try f.h.sendBody("{\"jsonrpc\":\"2.0\",\"method\":\"exit\"}");
    try f.h.waitFinished(live.deadline(5000));
}

fn idleCase(kind: Kind) LiveSpecError!void {
    try idleCaseMode(kind, .ordinary);
}

fn idleCaseMode(kind: Kind, mode: Mode) LiveSpecError!void {
    var tmp = env.tmpDir(.{});
    defer tmp.cleanup();
    const directory = try tmp.dir.realPathFileAlloc(env.io, ".", env.allocator);
    defer env.allocator.free(directory);
    const path = try std.fs.path.join(env.allocator, &.{ directory, "main.roc" });
    defer env.allocator.free(path);
    const uri = try lsp.uri.pathToUri(env.allocator, path);
    defer env.allocator.free(uri);
    const import_source: []const u8 = switch (kind) {
        .str => "import \"input.txt\" as input : Str\n",
        .bytes => "import \"input.txt\" as input : List(U8)\n",
        .roc, .transitive => "import Dep\ninput = Dep.value\n",
    };
    const source = try std.fmt.allocPrint(env.allocator, "app [process_string] {{ pf: platform \"{s}\" }}\n{s}\nprocess_string = |_text| {{\n    _ = input\n    \"ok\"\n}}\n", .{ env.tmp_dir_platform_path, import_source });
    defer env.allocator.free(source);
    try tmp.dir.writeFile(env.io, .{ .sub_path = "main.roc", .data = source });
    const dep_source: []const u8 = if (kind == .transitive)
        "import \"input.txt\" as input : Str\nDep := Str.{\n    value = input\n}\n"
    else
        "Dep := Str.{\n    value = \"ok\"\n}\n";
    try tmp.dir.writeFile(env.io, .{ .sub_path = "Dep.roc", .data = dep_source });
    try tmp.dir.writeFile(env.io, .{ .sub_path = "input.txt", .data = "ok" });
    var h: live.Live = undefined;
    try h.init(directory);
    defer h.deinit();
    var control: live.Control = .{};
    control.init(&h);
    defer control.release();
    if (mode == .after_check) control.hold(.after_check);
    if (mode == .registration) control.hold(.before_registration);
    if (mode == .startup_failure) control.hooks.fail_startup.store(true, .seq_cst);
    try h.start();
    try initialize(&h);
    try h.send(.{ .jsonrpc = "2.0", .method = "textDocument/didOpen", .params = .{ .textDocument = .{ .uri = uri, .version = 1, .text = source } } });
    if (mode == .after_check or mode == .registration) {
        try control.wait(if (mode == .after_check) .after_check else .before_registration, 1, live.deadline(30000));
        try tmp.dir.deleteFile(env.io, "input.txt");
        control.release();
        try diagnostics(&h, uri, true);
        try tmp.dir.writeFile(env.io, .{ .sub_path = "input.txt", .data = "ok" });
        try diagnostics(&h, uri, false);
        try std.testing.expect(control.hooks.checks.load(.seq_cst) >= 3);
        return;
    }
    if (mode == .startup_failure) {
        try watchFailure(&h);
        try std.testing.expectEqual(@as(usize, 1), control.count(.before_registration));
        try std.testing.expectEqual(@as(usize, 0), control.count(.after_registration));
        try ordinaryChangeAfterFailure(&h, &control, uri);
        return;
    }
    try diagnostics(&h, uri, false);
    try control.wait(.drained, 1, live.deadline(30000));
    if (mode == .replacement_failure or mode == .runtime_failure) {
        if (mode == .replacement_failure) control.hooks.fail_replacement.store(true, .seq_cst) else control.hooks.fail_runtime.store(true, .seq_cst);
        try tmp.dir.writeFile(env.io, .{ .sub_path = "input.txt", .data = "changed" });
        try watchFailure(&h);
        if (mode == .replacement_failure) try std.testing.expect(control.count(.before_registration) >= 2);
        try ordinaryChangeAfterFailure(&h, &control, uri);
        try h.sendBody("{\"jsonrpc\":\"2.0\",\"id\":12,\"method\":\"shutdown\"}");
        var response = try responseById(&h, 12);
        defer response.deinit();
        return;
    }
    if (mode == .truncated) {
        const checks = control.hooks.checks.load(.seq_cst);
        // Split writeFile's truncate/write sequence to expose the intermediate
        // state deterministically. Native events must trigger both checks.
        const file = try tmp.dir.createFile(env.io, "input.txt", .{});
        defer file.close(env.io);
        try control.wait(.after_check, checks + 1, live.deadline(30000));
        try control.reconcile(&h, live.deadline(30000));
        try std.testing.expect(control.hooks.checks.load(.seq_cst) > checks);
        const empty_checks = control.hooks.checks.load(.seq_cst);
        try file.writeStreamingAll(env.io, "ok");
        try control.wait(.after_check, empty_checks + 1, live.deadline(30000));
        try control.reconcile(&h, live.deadline(30000));
        try std.testing.expect(control.hooks.checks.load(.seq_cst) > empty_checks);
        return;
    }
    if (mode == .unchanged) {
        const checks = control.hooks.checks.load(.seq_cst);
        try tmp.dir.writeFile(env.io, .{ .sub_path = "unrelated.txt", .data = "unrelated" });
        // Truncating before writing exposes empty bytes, which legitimately
        // require a check. Overwrite identical bytes without changing length.
        try tmp.dir.writeFile(env.io, .{ .sub_path = "input.txt", .data = "ok", .flags = .{ .truncate = false } });
        try control.reconcile(&h, live.deadline(30000));
        try std.testing.expectEqual(checks, control.hooks.checks.load(.seq_cst));
        return;
    }
    // No client writes between the initial result and both dependency results.
    const dependency = if (kind == .roc) "Dep.roc" else "input.txt";
    try tmp.dir.deleteFile(env.io, dependency);
    // The compiler attributes missing module reads to that module's path;
    // transitive file-import errors also belong to the containing module.
    const diagnostic_path = if (kind == .transitive or kind == .roc) try std.fs.path.join(env.allocator, &.{ directory, "Dep.roc" }) else null;
    defer if (diagnostic_path) |p| env.allocator.free(p);
    const diagnostic_uri = if (diagnostic_path) |p| try lsp.uri.pathToUri(env.allocator, p) else try env.allocator.dupe(u8, uri);
    defer env.allocator.free(diagnostic_uri);
    try diagnostics(&h, diagnostic_uri, true);
    try tmp.dir.writeFile(env.io, .{ .sub_path = dependency, .data = if (kind == .roc) dep_source else "ok" });
    try diagnostics(&h, diagnostic_uri, false);
    // A subsequent request proves the idle publications did not corrupt framing.
    try h.send(.{ .jsonrpc = "2.0", .id = @as(i64, 9), .method = "textDocument/semanticTokens/full", .params = .{ .textDocument = .{ .uri = uri } } });
    var response = try responseById(&h, 9);
    defer response.deinit();
    try std.testing.expect(response.value.object.get("result") != null);
}
