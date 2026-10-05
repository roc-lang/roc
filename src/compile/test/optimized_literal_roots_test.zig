//! An optimized runtime program converts custom literals at compile time, as
//! every other build does: `--opt` never moves work between compile time and
//! runtime.

const std = @import("std");
const base = @import("base");
const build_options = @import("build_options");
const eval = @import("eval");
const lir = @import("lir");
const backend = @import("backend");
const postcheck = @import("postcheck");
const roc_target = @import("roc_target");
const CoreCtx = @import("ctx").CoreCtx;
const Coordinator = @import("../coordinator.zig").Coordinator;
const is_freestanding = @import("../threading.zig").is_freestanding;

fn countProcsNaming(store: *const lir.LirStore, fragment: []const u8) usize {
    var count: usize = 0;
    for (0..store.procSpecCount()) |index| {
        const name = store.procDebugName(@enumFromInt(index)) orelse continue;
        if (std.mem.find(u8, name, fragment) != null) count += 1;
    }
    return count;
}

test "an optimized runtime program reads a generic function's custom literal conversions as completed values" {
    try testCompletedLiteralRuntime(false);
}

test "finalized literal producer preserves shared-root owners across coordinator and worker registration" {
    if (!base.CompilerFeatures.finalized_literal_cache) return error.SkipZigTest;
    try testCompletedLiteralRuntime(true);
}

fn testCompletedLiteralRuntime(comptime expect_certificate: bool) !void {
    if (is_freestanding) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try tmp_dir.dir.createDirPath(io, ".roc_echo_platform");
    const files = [_]struct { path: []const u8, source: []const u8 }{
        .{ .path = ".roc_echo_platform/main.roc", .source =
        \\platform ""
        \\    requires {} { main! : List(Str) => Try({}, [Exit(I8), ..]) }
        \\    exposes [Echo]
        \\    packages {}
        \\    provides { "roc_main": main_for_host! }
        \\    hosted { "roc_echo_line": Echo.line! }
        \\import Echo
        \\main_for_host! : List(Str) => I8
        \\main_for_host! = |args|
        \\    match main!(args) {
        \\        Ok({}) => 0
        \\        Err(Exit(code)) => code
        \\        Err(other) => {
        \\            Echo.line!("Program exited with error: ${Str.inspect(other)}")
        \\            1
        \\        }
        \\    }
        },
        .{ .path = ".roc_echo_platform/Echo.roc", .source =
        \\Echo := [].{
        \\    line! : Str => {}
        \\}
        },
        .{ .path = "main.roc", .source =
        \\app [main!] { pf: platform "./.roc_echo_platform/main.roc" }
        \\import pf.Echo
        \\Word(marker) := { text : Str }.{
        \\    is_eq : Word(marker), Word(marker) -> Bool
        \\    is_eq = |a, b| a.text == b.text
        \\    from_quote : Str -> Try(Word(marker), [BadQuotedBytes(Str)])
        \\    from_quote = |text| Ok({ text: Str.concat(text, "!") })
        \\}
        \\rank : a -> U64 where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]), a.is_eq : a, a -> Bool]
        \\rank = |value| match value {
        \\    "low" => 1
        \\    _ => 2
        \\}
        \\shared : a -> { marker : a, word : Word(b) }
        \\shared = |marker| {
        \\    word : Word(b)
        \\    word = "shared"
        \\    { marker, word }
        \\}
        \\make_get : Str -> ({} -> Str)
        \\make_get = |raw| |{}| raw
        \\CallableWord := { get : {} -> Str }.{
        \\    from_quote : Str -> Try(CallableWord, [BadQuotedBytes(Str)])
        \\    from_quote = |raw| Ok(CallableWord.{ get: make_get(raw) })
        \\}
        \\read_callable : U64 -> Str
        \\read_callable = |_n| {
        \\    literal : CallableWord
        \\    literal = "callable"
        \\    get = literal.get
        \\    get({})
        \\}
        \\main! = |args| {
        \\    word : Word(I32)
        \\    word = Word.{ text: if List.len(args) > 5 "a" else "b" }
        \\    small : I32
        \\    small = 1
        \\    first : { marker : U64, word : Word(I32) }
        \\    first = shared(List.len(args))
        \\    second : { marker : I32, word : Word(I32) }
        \\    second = shared(small)
        \\    duplicate : { marker : U64, word : Word(I32) }
        \\    duplicate = shared(List.len(args))
        \\    identity : I32 -> I32
        \\    identity = |value| value
        \\    third : { marker : I32 -> I32, word : Word(I32) }
        \\    third = shared(identity)
        \\    Echo.line!(first.word.text)
        \\    Echo.line!(second.word.text)
        \\    Echo.line!(duplicate.word.text)
        \\    Echo.line!(third.word.text)
        \\    callback = third.marker
        \\    Echo.line!(Str.inspect(callback(small)))
        \\    Echo.line!(read_callable(List.len(args)))
        \\    Echo.line!(Str.inspect(rank(word)))
        \\    Ok({})
        \\}
        },
    };
    for (files) |file| try tmp_dir.dir.writeFile(io, .{ .sub_path = file.path, .data = file.source });
    const app_path = try tmp_dir.dir.realPathFileAlloc(io, "main.roc", allocator);
    defer allocator.free(app_path);

    var builtin_modules = try eval.BuiltinModules.init(allocator);
    defer builtin_modules.deinit();
    const worker_counts: []const usize = if (expect_certificate) &.{ 1, 2 } else &.{1};
    var first: ?[]lir.LIR.FinalizedLiteralOutcomes.PortableSuccess = null;
    defer if (first) |facts| allocator.free(facts);
    var rejected = RejectedOffer{};
    for (worker_counts) |workers| {
        const facts = try completedLiteralFactsAtPath(allocator, io, app_path, &builtin_modules, expect_certificate, workers, null, if (expect_certificate and workers == 1) &rejected else null);
        defer allocator.free(facts);
        if (first) |expected| {
            try std.testing.expectEqualDeep(@as([]const lir.LIR.FinalizedLiteralOutcomes.PortableSuccess, expected), @as([]const lir.LIR.FinalizedLiteralOutcomes.PortableSuccess, facts));
        } else {
            first = try allocator.dupe(lir.LIR.FinalizedLiteralOutcomes.PortableSuccess, facts);
        }
    }
    if (expect_certificate and base.CompilerFeatures.early_ctfe_cache) {
        try std.testing.expect(rejected.configured);
        const facts = try completedLiteralFactsAtPath(allocator, io, app_path, &builtin_modules, true, 1, &rejected, null);
        defer allocator.free(facts);
        try std.testing.expect(rejected.offers != 0);
        try std.testing.expectEqualDeep(@as([]const lir.LIR.FinalizedLiteralOutcomes.PortableSuccess, first.?), @as([]const lir.LIR.FinalizedLiteralOutcomes.PortableSuccess, facts));
    }
}

const RejectedOffer = struct {
    configured: bool = false,
    offers: usize = 0,
    key: [32]u8 = undefined,
    identity: [32]u8 = undefined,
    borrowed: u64 = 0,
    ret_borrowed: bool = false,
    lenders: u64 = 0,
    roots: [1]lir.LIR.FinalizedLiteralOutcomes.PortableSuccess = undefined,
    certificate: lir.LIR.FinalizedLiteralOutcomes.Certificate = undefined,

    fn lookup(context: *anyopaque, key: [32]u8) ?postcheck.Common.SpecCacheHit {
        const self: *RejectedOffer = @ptrCast(@alignCast(context));
        if (!self.configured or !std.mem.eql(u8, &key, &self.key)) return null;
        self.offers += 1;
        var hit: postcheck.Common.SpecCacheHit = .{
            .identity = self.identity,
            .rc_borrowed_params = self.borrowed,
            .rc_ret_borrowed = self.ret_borrowed,
            .rc_ret_lenders = self.lenders,
        };
        if (base.CompilerFeatures.finalized_literal_cache) hit.finalized_literals = &self.certificate;
        return hit;
    }

    fn artifact(_: *anyopaque, _: lir.ProcIdentity) ?backend.dev.LocatedArtifact {
        // A declined certificate must never turn a missing statement body into
        // an external-code demand. No code is fabricated for this regression.
        return null;
    }
};

fn completedLiteralFactsAtPath(
    allocator: std.mem.Allocator,
    io: std.Io,
    app_path: []const u8,
    builtin_modules: *eval.BuiltinModules,
    comptime expect_certificate: bool,
    workers: usize,
    rejected: ?*RejectedOffer,
    capture: ?*RejectedOffer,
) ![]lir.LIR.FinalizedLiteralOutcomes.PortableSuccess {
    var coord = try Coordinator.init(
        allocator,
        if (workers > 1) .multi_threaded else .single_threaded,
        workers,
        roc_target.RocTarget.detectNative(),
        builtin_modules,
        build_options.compiler_version,
        null,
        CoreCtx.os(allocator, allocator, io),
    );
    defer coord.deinit();
    coord.enable_hosted_transform = true;
    if (rejected) |offer| coord.compile_time_object_cache = .{
        .spec_cache = .{ .context = offer, .find = RejectedOffer.lookup },
        .splice_source = .{ .context = offer, .find = RejectedOffer.artifact },
    };
    var arena = base.SingleThreadArena.init(allocator);
    defer arena.deinit();
    try coord.start();
    try coord.discoverAppFromPath(arena.allocator(), .{ .entry_path = app_path });
    try coord.coordinatorLoop();
    if (coord.hasUserErrors()) {
        var reports = coord.iterReports();
        while (reports.next()) |entry| std.debug.print("literal fixture report: {s} in {s}\n", .{ entry.report.title, entry.module_name });
    }
    try std.testing.expect(!coord.hasUserErrors());

    // `--opt=speed`'s Solved policy: compile-time evaluation runs inside
    // this build's own specialized program.
    const target: lir.CheckedPipeline.TargetConfig = .{
        .inline_mode = .wrappers,
        .spec_constr_clone_inlining = .all_calls,
        .inline_expects = .omit,
        .proc_debug_names = true,
        .finalized_literal_cache_context = expect_certificate,
        .keep_specialization_procs = false,
        .spec_cache = if (rejected) |offer| .{ .context = offer, .find = RejectedOffer.lookup } else null,
    };
    coord.runtime_lowering = .{ .target = target };
    try coord.finishCheckedProgram(.executable_artifacts);
    try std.testing.expect(!coord.hasUserErrors());
    const session = &coord.program_session.?;
    try std.testing.expect(session.runtime_prepared != null);
    var facts = std.ArrayList(lir.LIR.FinalizedLiteralOutcomes.PortableSuccess).empty;
    errdefer facts.deinit(allocator);
    if (expect_certificate) {
        const outcomes = if (session.literal_outcomes) |*retained| retained else return error.TestUnexpectedResult;
        try std.testing.expect(outcomes.records.items.len != 0);
        var null_owner_shared = false;
        var callable_owned = false;
        for (outcomes.records.items) |record| {
            // Write-only policy supplies no reader, fake hit, or forced key.
            // Association comes from ordinary specialization publication.
            if (record.specialization_key != null) {
                if (record.has_callable_result == true) {
                    // The owning procedure returns Str, but its converted
                    // literal contains a callable consumed into that result.
                    try std.testing.expect(record.portableSuccess() == null);
                    callable_owned = true;
                } else {
                    try std.testing.expect(record.portableSuccess() != null);
                    try facts.append(allocator, record.portableSuccess().?);
                }
            } else {
                try std.testing.expect(record.portableSuccess() == null);
                for (outcomes.records.items) |supported| {
                    if (supported.specialization_key != null and std.meta.eql(supported.root, record.root))
                        null_owner_shared = true;
                }
            }
        }
        try std.testing.expect(callable_owned);
        try std.testing.expect(null_owner_shared);
        var shared_root = false;
        for (facts.items, 0..) |left, index| {
            for (facts.items[index + 1 ..]) |right| {
                if (std.meta.eql(left.root, right.root) and
                    !std.mem.eql(u8, &left.owner_specialization_key, &right.owner_specialization_key))
                    shared_root = true;
            }
        }
        try std.testing.expect(shared_root);
    } else {
        try std.testing.expect(session.literal_outcomes == null);
        if (session.host) |host|
            try std.testing.expectEqual(@as(usize, 0), host.lir_result.literal_root_owners.items.len);
    }

    var runtime = try session.takeRuntime(allocator, session.runtime_roots, target);
    defer runtime.deinit();
    try std.testing.expectEqual(@as(usize, 0), countProcsNaming(&runtime.lir_result.store, "from_quote"));
    if (rejected != null) {
        for (runtime.lir_result.store.getProcSpecs()) |proc| try std.testing.expect(!proc.external);
    }
    if (capture) |offer| {
        for (runtime.lir_result.spec_procs.items) |spec| {
            for (facts.items) |fact| {
                if (offer.configured or !std.mem.eql(u8, &spec.key, &fact.owner_specialization_key)) continue;
                const proc = runtime.lir_result.store.getProcSpec(spec.proc);
                offer.key = spec.key;
                offer.identity = proc.identity.bytes;
                offer.borrowed = proc.rc_borrowed_params;
                offer.ret_borrowed = proc.rc_ret_borrowed;
                offer.lenders = proc.rc_ret_lenders;
                offer.roots[0] = fact;
                // Corrupt only source authority, never force an incorrect key
                // to simulate a hit. The exact real reservation stays intact.
                offer.roots[0].root.source.expr = @enumFromInt(std.math.maxInt(u32));
                offer.certificate = .{ .specialization_key = offer.key, .artifact_identity = offer.identity, .roots = &offer.roots };
                offer.configured = true;
            }
        }
    }
    if (expect_certificate) {
        const outcomes = if (session.literal_outcomes) |*retained| retained else return error.TestUnexpectedResult;
        var publication_body = false;
        for (runtime.lir_result.spec_procs.items) |spec| {
            if (spec.literal_publication_root) {
                const implementation = runtime.lir_result.store.getProcSpec(spec.proc);
                try std.testing.expect(implementation.body != null or implementation.external);
                publication_body = true;
            }
        }
        try std.testing.expect(publication_body);
        try std.testing.expect(runtime.lir_result.literal_root_uses.items.len != 0);
        var live_emitter = false;
        var supported_read = false;
        var unsupported_read = false;
        for (runtime.lir_result.literal_root_uses.items) |use| {
            var completed = false;
            try std.testing.expect(use.owner != null);
            for (outcomes.record_owners.items, 0..) |owner, ordinal| {
                if (owner == use.owner) {
                    try std.testing.expectEqual(use.root, outcomes.record_roots.items[ordinal].?);
                    completed = true;
                    if (outcomes.records.items[ordinal].specialization_key != null)
                        supported_read = true
                    else
                        unsupported_read = true;
                }
            }
            try std.testing.expect(completed);
            for (runtime.lir_result.store.getProcSpecs()) |proc| {
                if (std.meta.eql(proc.identity, use.emitter)) live_emitter = true;
            }
        }
        try std.testing.expect(live_emitter);
        try std.testing.expect(supported_read and unsupported_read);
    } else {
        try std.testing.expectEqual(@as(usize, 0), runtime.lir_result.literal_root_uses.items.len);
        if (base.CompilerFeatures.finalized_literal_cache) for (runtime.lir_result.spec_procs.items) |spec| {
            try std.testing.expect(!spec.literal_publication_root);
        };
    }
    std.mem.sort(lir.LIR.FinalizedLiteralOutcomes.PortableSuccess, facts.items, {}, struct {
        fn lessThan(_: void, left: lir.LIR.FinalizedLiteralOutcomes.PortableSuccess, right: lir.LIR.FinalizedLiteralOutcomes.PortableSuccess) bool {
            const owner = std.mem.order(u8, &left.owner_specialization_key, &right.owner_specialization_key);
            if (owner != .eq) return owner == .lt;
            const source = std.mem.order(u8, &left.root.source.module.bytes, &right.root.source.module.bytes);
            if (source != .eq) return source == .lt;
            if (left.root.source.expr != right.root.source.expr)
                return @intFromEnum(left.root.source.expr) < @intFromEnum(right.root.source.expr);
            return std.mem.order(u8, &left.root.procedure, &right.root.procedure) == .lt;
        }
    }.lessThan);
    return facts.toOwnedSlice(allocator);
}
