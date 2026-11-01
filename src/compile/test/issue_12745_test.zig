//! Regression test for https://github.com/roc-lang/roc/issues/12745.
//!
//! At `--opt speed`, a sequence of `read!(...)?` steps over a callee that
//! rebuilds each variant of an opaque `Err` must make the LIR passes do work
//! that grows with the number of steps, not exponentially in it. `raw!` calls
//! itself so that it stays an opaque call, as a hosted function would. The test
//! counts the statements the passes append, because a later stage can remove
//! what they produced. Each size is small enough to lower quickly either way,
//! so the test measures the growth without exhausting the machine.
const std = @import("std");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

fn steps(comptime count: usize) []const u8 {
    @setEvalBranchQuota(100_000);
    var body: []const u8 = "";
    var result: []const u8 = "";
    for (1..count + 1) |index| {
        body = body ++ std.fmt.comptimePrint("    s{d} = read!(\"S{d}\")?\n", .{ index, index });
        result = result ++ std.fmt.comptimePrint("${{s{d}}}", .{index});
    }
    return body ++ "    Ok(\"" ++ result ++ "\")\n";
}

fn app(comptime count: usize) []const u8 {
    return
    \\raw! : Str, U64 => Try(Str, [VarNotFound(Str), EnvErr(Str)])
    \\raw! = |name, fuel|
    \\    if fuel > 0 {
    \\        echo!(name)
    \\        raw!(name, fuel - 1)
    \\    } else if name.count_utf8_bytes() > 8 {
    \\        Err(EnvErr(name))
    \\    } else if name.count_utf8_bytes() == 0 {
    \\        Err(VarNotFound(name))
    \\    } else {
    \\        Ok(name)
    \\    }
    \\
    \\from_raw : Str -> Str
    \\from_raw = |raw| raw.concat("!")
    \\
    \\var! : Str => Try(Str, [VarNotFound(Str), EnvErr(Str)])
    \\var! = |name|
    \\    match raw!(name, 2) {
    \\        Ok(raw) => Ok(from_raw(raw))
    \\        Err(VarNotFound(raw_name)) => Err(VarNotFound(from_raw(raw_name)))
    \\        Err(EnvErr(err)) => Err(EnvErr(err))
    \\    }
    \\
    \\env_var! : Str => Try(Str, _)
    \\env_var! = |name|
    \\    match var!(name) {
    \\        Ok(_) => Ok("v")
    \\        Err(e) => Err(EnvVarErr(Str.inspect(e)))
    \\    }
    \\
    \\read! : Str => Try(Str, _)
    \\read! = |name| env_var!(name).map_err(|_| Missing(name))
    \\
    \\setup! = || {
    \\
    ++ comptime steps(count) ++
        \\}
        \\
        \\main! = |_args| {
        \\    match setup!() {
        \\        Ok(s) => echo!(s)
        \\        Err(_) => echo!("failed")
        \\    }
        \\    Ok({})
        \\}
    ;
}

/// Statements the LIR passes appended while lowering the app. Parallel commits
/// are what record this count, so the lowering uses two workers.
fn appendedStatements(app_body: []const u8) harness.LowerToLirHarnessError!u64 {
    var metrics: lir.CheckedPipeline.LirPassParallelMetrics = .{};
    try harness.expectLowersToLirWithOptions(app_body, .{
        .inline_mode = .wrappers,
        .spec_constr_clone_inlining = .all_calls,
        .specialization_workers = 2,
        .lir_pass_parallel_metrics_out = &metrics,
    });
    return metrics.appended_statements;
}

test "issue 12745: LIR pass output grows with the number of `?` steps, not exponentially" {
    const short_count = try appendedStatements(app(4));
    const long_count = try appendedStatements(app(8));
    if (long_count > short_count * 3) {
        std.debug.print(
            "LIR passes appended {d} statements for 4 steps and {d} for 8 steps\n",
            .{ short_count, long_count },
        );
        return error.LirPassOutputGrewExponentiallyInStepCount;
    }
}
