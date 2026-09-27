//! Closed Monotype specialization identity: which requests name one
//! specialization and which must stay apart.
//!
//! The checked source function type a call site instantiated a callable from
//! is not part of a closed specialization's identity (`design.md`), so call
//! sites that differ only in that provenance—a transparent alias annotation,
//! a second alias over the same backing type—reuse one record. What the
//! identity does contain still separates records, and these programs pin both
//! directions: the checked callable, the closed Monotype function type, and
//! (in `monotype/ast.zig`'s unit tests) the exact evidence, the nested site
//! and owner context, and a generated body's producer key.

const std = @import("std");
const lir = @import("lir");
const postcheck = @import("postcheck");
const harness = @import("lower_to_lir_harness.zig");

fn countProcsNamed(
    program: anytype,
    name: []const u8,
) usize {
    var count: usize = 0;
    for (program.defs) |def| {
        const debug_name = program.procDebugName(def.symbol) orelse continue;
        if (std.mem.eql(u8, program.names.exportNameText(debug_name), name)) count += 1;
    }
    return count;
}

fn expectRefreshSpecializations(
    comptime expected: usize,
) *const fn (*const lir.CheckedPipeline.PreparedMonotype) harness.LowerToLirHarnessError!void {
    return &struct {
        fn inspect(
            prepared: *const lir.CheckedPipeline.PreparedMonotype,
        ) harness.LowerToLirHarnessError!void {
            const program = prepared.program.view();
            try std.testing.expectEqual(expected, countProcsNamed(program, "refresh"));
        }
    }.inspect;
}

const U64IdentityCallable = enum { nested_site, proc_template };

/// Nested specializations whose closed request type is exactly `U64 -> U64`.
/// The lambda under test is the only nested callable at that type in these
/// programs, so this counts its specializations without naming a site id.
fn countNestedU64IdentitySpecs(program: anytype) usize {
    return countU64IdentitySpecs(program, .nested_site);
}

/// Specializations of the given callable kind whose closed request type is
/// exactly `U64 -> U64`.
fn countU64IdentitySpecs(program: anytype, callable: U64IdentityCallable) usize {
    var count: usize = 0;
    for (program.specs) |record| {
        switch (record.identity.callable) {
            .nested_site => if (callable != .nested_site) continue,
            .proc_template => if (callable != .proc_template) continue,
            .hosted, .generated => continue,
        }
        const fn_type = switch (program.types.get(record.request_fn_ty)) {
            .func => |func| func,
            .primitive, .named, .record, .tuple, .tag_union, .list, .box, .erased, .zst => continue,
        };
        const args = program.types.span(fn_type.args);
        if (args.len != 1) continue;
        if (!isU64(program, args[0])) continue;
        if (!isU64(program, fn_type.ret)) continue;
        count += 1;
    }
    return count;
}

fn isU64(program: anytype, ty: anytype) bool {
    return switch (program.types.get(ty)) {
        .primitive => |primitive| primitive == .u64,
        .func, .named, .record, .tuple, .tag_union, .list, .box, .erased, .zst => false,
    };
}

fn expectSingleNestedU64Specialization(
    prepared: *const lir.CheckedPipeline.PreparedMonotype,
) harness.LowerToLirHarnessError!void {
    const program = prepared.program.view();
    try std.testing.expectEqual(@as(usize, 1), countNestedU64IdentitySpecs(program));
}

fn expectSingleProcedureU64Specialization(
    prepared: *const lir.CheckedPipeline.PreparedMonotype,
) harness.LowerToLirHarnessError!void {
    const program = prepared.program.view();
    try std.testing.expectEqual(@as(usize, 1), countU64IdentitySpecs(program, .proc_template));
    try std.testing.expectEqual(@as(usize, 0), countNestedU64IdentitySpecs(program));
}

test "closed specialization identity: two aliases and the bare type share one specialization" {
    // `Count`, `Tally`, and `U64` are three checked source types and one
    // closed Monotype request, so `refresh` has one specialization.
    try harness.expectLowersToLirWithOptions(
        \\Count : U64
        \\Tally : U64
        \\refresh = |value| value + 1
        \\by_count : Count -> Count
        \\by_count = |value| refresh(value)
        \\by_tally : Tally -> Tally
        \\by_tally = |value| refresh(value)
        \\by_raw : U64 -> U64
        \\by_raw = |value| refresh(value)
        \\main! = |args| {
        \\    n = args.len()
        \\    echo!((by_count(n) + by_tally(n) + by_raw(n)).to_str())
        \\    Ok({})
        \\}
    , .{ .proc_debug_names = true, .prepared_inspect = expectRefreshSpecializations(1) });
}

test "closed specialization identity: a different closed type keeps its own specialization" {
    // The two `U64` requests merge across the alias; the `F64` request is a
    // different closed Monotype type and keeps its own body.
    try harness.expectLowersToLirWithOptions(
        \\Count : U64
        \\refresh = |value| value + 1
        \\first : Count -> Count
        \\first = |value| refresh(value)
        \\second : U64 -> U64
        \\second = |value| refresh(value)
        \\third : F64 -> F64
        \\third = |value| refresh(value)
        \\main! = |args| {
        \\    n = args.len()
        \\    echo!("${(first(n) + second(n)).to_str()} ${third(n.to_f64()).to_str()}")
        \\    Ok({})
        \\}
    , .{ .proc_debug_names = true, .prepared_inspect = expectRefreshSpecializations(2) });
}

test "closed specialization identity: alias-equal requests reuse one specialization under four workers" {
    // Reservation is coordinator-owned, so the reuse decision is the same
    // whether the requesting bodies lowered on one lane or four.
    try harness.expectLowersToLirWithOptions(
        \\Count : U64
        \\refresh = |value| value + 1
        \\first : Count -> Count
        \\first = |value| refresh(value)
        \\second : U64 -> U64
        \\second = |value| refresh(value)
        \\main! = |args| {
        \\    echo!((first(args.len()) + second(args.len())).to_str())
        \\    Ok({})
        \\}
    , .{
        .proc_debug_names = true,
        .specialization_workers = 4,
        .prepared_inspect = expectRefreshSpecializations(1),
    });
}

test "closed specialization identity: one lambda passed at alias-differing parameter types is one nested specialization" {
    // The nested site, its owner context, its captures, and the closed
    // request type are equal at both uses; only the checked parameter type of
    // the higher-order callee differs. The lambda captures the runtime `n`, so
    // it stays a nested function of `main!`.
    try harness.expectLowersToLirWithOptions(
        \\Count : U64
        \\apply_count : (Count -> Count), Count -> Count
        \\apply_count = |f, value| f(value)
        \\apply_raw : (U64 -> U64), U64 -> U64
        \\apply_raw = |f, value| f(value)
        \\main! = |args| {
        \\    n = args.len()
        \\    add_n = |value| value + n
        \\    echo!((apply_count(add_n, n) + apply_raw(add_n, n)).to_str())
        \\    Ok({})
        \\}
    , .{ .prepared_inspect = expectSingleNestedU64Specialization });
}

test "closed specialization identity: a promoted local function passed at alias-differing parameter types is one procedure specialization" {
    // A local function that uses nothing from `main!` is a procedure of its
    // own, so both uses request one closed procedure specialization.
    try harness.expectLowersToLirWithOptions(
        \\Count : U64
        \\apply_count : (Count -> Count), Count -> Count
        \\apply_count = |f, value| f(value)
        \\apply_raw : (U64 -> U64), U64 -> U64
        \\apply_raw = |f, value| f(value)
        \\main! = |args| {
        \\    n = args.len()
        \\    add_one = |value| value + 1
        \\    echo!((apply_count(add_one, n) + apply_raw(add_one, n)).to_str())
        \\    Ok({})
        \\}
    , .{ .prepared_inspect = expectSingleProcedureU64Specialization });
}

fn monotypeCounters(
    app_body: []const u8,
) harness.LowerToLirHarnessError!postcheck.Monotype.Lower.Diagnostics {
    var diagnostics: postcheck.Monotype.Lower.Diagnostics = .{};
    try harness.expectLowersToLirWithOptions(app_body, .{
        .monotype_only = true,
        .monotype_diagnostics_out = &diagnostics,
    });
    return diagnostics;
}

test "closed specialization identity: alias-differing calls in one body reserve one specialization before any body work" {
    // Both calls are in ONE body, so they are drafted by one graph. A draft
    // template request that misses reuse creates a draft function and counts
    // a `template_misses`; a request that reuses one counts a hit. Equality
    // with the twin program that has no alias therefore says the reuse
    // happened at the request, not by discarding a second lowered body.
    const aliased = try monotypeCounters(
        \\Count : U64
        \\refresh = |value| value + 1
        \\use_both : Count, U64 -> U64
        \\use_both = |a, b| refresh(a) + refresh(b)
        \\main! = |args| {
        \\    echo!(use_both(args.len(), args.len()).to_str())
        \\    Ok({})
        \\}
    );
    const plain = try monotypeCounters(
        \\refresh = |value| value + 1
        \\use_both : U64, U64 -> U64
        \\use_both = |a, b| refresh(a) + refresh(b)
        \\main! = |args| {
        \\    echo!(use_both(args.len(), args.len()).to_str())
        \\    Ok({})
        \\}
    );

    try std.testing.expectEqual(plain.specialization.template_misses, aliased.specialization.template_misses);
    try std.testing.expectEqual(plain.specialization.template_hits, aliased.specialization.template_hits);
    try std.testing.expectEqual(plain.specialization.nested_misses, aliased.specialization.nested_misses);
    try std.testing.expectEqual(
        plain.body.caller_owned_template_bodies_lowered,
        aliased.body.caller_owned_template_bodies_lowered,
    );
    try std.testing.expectEqual(
        plain.body.deferred_template_requests,
        aliased.body.deferred_template_requests,
    );
}

test "closed specialization identity: an alias annotation lowers no extra template body" {
    // The same program with and without the transparent alias must lower the
    // same number of template bodies. `template_misses` counts reservations
    // that created a body, so this pins the duplicate work itself rather
    // than a timing.
    const aliased =
        \\Count : U64
        \\refresh = |value| value + 1
        \\first : Count -> Count
        \\first = |value| refresh(value)
        \\second : U64 -> U64
        \\second = |value| refresh(value)
        \\main! = |args| {
        \\    echo!((first(args.len()) + second(args.len())).to_str())
        \\    Ok({})
        \\}
    ;
    const plain =
        \\refresh = |value| value + 1
        \\first : U64 -> U64
        \\first = |value| refresh(value)
        \\second : U64 -> U64
        \\second = |value| refresh(value)
        \\main! = |args| {
        \\    echo!((first(args.len()) + second(args.len())).to_str())
        \\    Ok({})
        \\}
    ;

    var aliased_diagnostics: postcheck.Monotype.Lower.Diagnostics = .{};
    var plain_diagnostics: postcheck.Monotype.Lower.Diagnostics = .{};
    try harness.expectLowersToLirWithOptions(aliased, .{
        .monotype_only = true,
        .monotype_diagnostics_out = &aliased_diagnostics,
    });
    try harness.expectLowersToLirWithOptions(plain, .{
        .monotype_only = true,
        .monotype_diagnostics_out = &plain_diagnostics,
    });

    try std.testing.expectEqual(
        plain_diagnostics.specialization.template_misses,
        aliased_diagnostics.specialization.template_misses,
    );
    try std.testing.expectEqual(
        plain_diagnostics.specialization.nested_misses,
        aliased_diagnostics.specialization.nested_misses,
    );
}
