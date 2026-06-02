//! Opt-in CLI compiler-bug repros found during bug hunting.
//!
//! These tests assert the desired behavior for known bugs and are intentionally
//! excluded from `zig build run-test-cli`. Run them explicitly with:
//!
//!   zig build run-test-cli-bughunt -- --filter bughunt --threads 1 --timeout 5000

const std = @import("std");
const builtin = @import("builtin");
const posix = std.posix;
const Allocator = std.mem.Allocator;

const harness = @import("test_harness");
const util = @import("util.zig");

const SourceFile = struct {
    path: []const u8,
    contents: []const u8,
};

const Command = enum {
    check,
    docs,
    glue,
    run_dev,
    run_interpreter,
    run_dev_allow_errors,
    test_dev,
    test_interpreter,
};

const Expect = union(enum) {
    success,
    success_stdout_exact: []const u8,
    warning_stdout_exact: []const u8,
    clean_failure,
    success_or_clean_failure,
};

const CliBugSpec = struct {
    id: usize,
    bug_id: []const u8,
    name: []const u8,
    files: []const SourceFile,
    main_file: []const u8 = "main.roc",
    command: Command,
    expect: Expect,
};

const fx_app_prefix =
    \\app [main!] { pf: platform "{FX_PLATFORM}" }
    \\
    \\import pf.Stdout
    \\
    \\
;

const fx_open_app_prefix =
    \\app [main!] { pf: platform "{FX_OPEN_PLATFORM}" }
    \\
    \\
;

fn runDevBodyCase(
    comptime id: usize,
    comptime bug_id: []const u8,
    comptime title: []const u8,
    comptime body: []const u8,
    comptime expected_stdout: []const u8,
) CliBugSpec {
    return .{
        .id = id,
        .bug_id = bug_id,
        .name = "bughunt " ++ bug_id ++ ": " ++ title,
        .files = &.{.{ .path = "main.roc", .contents = fx_app_prefix ++ body }},
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = expected_stdout },
    };
}

fn runDevCleanFailureBodyCase(
    comptime id: usize,
    comptime bug_id: []const u8,
    comptime title: []const u8,
    comptime body: []const u8,
) CliBugSpec {
    return .{
        .id = id,
        .bug_id = bug_id,
        .name = "bughunt " ++ bug_id ++ ": " ++ title,
        .files = &.{.{ .path = "main.roc", .contents = fx_app_prefix ++ body }},
        .command = .run_dev,
        .expect = .clean_failure,
    };
}

fn runDevTwoFileCase(
    comptime id: usize,
    comptime bug_id: []const u8,
    comptime title: []const u8,
    comptime main_body: []const u8,
    comptime a_source: []const u8,
    comptime expected_stdout: []const u8,
) CliBugSpec {
    return .{
        .id = id,
        .bug_id = bug_id,
        .name = "bughunt " ++ bug_id ++ ": " ++ title,
        .files = &.{
            .{ .path = "main.roc", .contents = fx_app_prefix ++ main_body },
            .{ .path = "A.roc", .contents = a_source },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = expected_stdout },
    };
}

fn runDevThreeFileCase(
    comptime id: usize,
    comptime bug_id: []const u8,
    comptime title: []const u8,
    comptime main_body: []const u8,
    comptime a_source: []const u8,
    comptime b_source: []const u8,
    comptime expected_stdout: []const u8,
) CliBugSpec {
    return .{
        .id = id,
        .bug_id = bug_id,
        .name = "bughunt " ++ bug_id ++ ": " ++ title,
        .files = &.{
            .{ .path = "main.roc", .contents = fx_app_prefix ++ main_body },
            .{ .path = "A.roc", .contents = a_source },
            .{ .path = "B.roc", .contents = b_source },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = expected_stdout },
    };
}

fn checkBodyCase(
    comptime id: usize,
    comptime bug_id: []const u8,
    comptime title: []const u8,
    comptime body: []const u8,
) CliBugSpec {
    return .{
        .id = id,
        .bug_id = bug_id,
        .name = "bughunt " ++ bug_id ++ ": " ++ title,
        .files = &.{.{ .path = "main.roc", .contents = fx_app_prefix ++ body }},
        .command = .check,
        .expect = .clean_failure,
    };
}

fn runDevOpenBodyCase(
    comptime id: usize,
    comptime bug_id: []const u8,
    comptime title: []const u8,
    comptime body: []const u8,
) CliBugSpec {
    return .{
        .id = id,
        .bug_id = bug_id,
        .name = "bughunt " ++ bug_id ++ ": " ++ title,
        .files = &.{.{ .path = "main.roc", .contents = fx_open_app_prefix ++ body }},
        .command = .run_dev,
        .expect = .clean_failure,
    };
}

fn testInterpBodyCase(
    comptime id: usize,
    comptime bug_id: []const u8,
    comptime title: []const u8,
    comptime body: []const u8,
    comptime expect: Expect,
) CliBugSpec {
    return .{
        .id = id,
        .bug_id = bug_id,
        .name = "bughunt " ++ bug_id ++ ": " ++ title,
        .files = &.{.{ .path = "main.roc", .contents = fx_app_prefix ++ body }},
        .command = .test_interpreter,
        .expect = expect,
    };
}

const Status = enum(u8) {
    pass = 0,
    fail = 1,
    crash = 2,
    timeout = 3,
};

const WireHeader = extern struct {
    status: u8,
    duration_ns: u64,
    setup_ns: u64,
    command_ns: u64,
    exit_code: u32,
    stderr_len: u32,
    stdout_len: u32,
    message_len: u32,
};

const TestResult = struct {
    status: Status = .crash,
    duration_ns: u64 = 0,
    setup_ns: u64 = 0,
    command_ns: u64 = 0,
    exit_code: u32 = 0,
    stderr_capture: ?[]const u8 = null,
    stdout_capture: ?[]const u8 = null,
    message: ?[]const u8 = null,
};

var roc_binary_path: []const u8 = "";

const tests = [_]CliBugSpec{
    .{
        .id = 1,
        .bug_id = "B001",
        .name = "bughunt B001: missing import reports a module-resolution diagnostic",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import Missing
            \\
            \\main! = || {}
            },
        },
        .command = .check,
        .expect = .clean_failure,
    },
    .{
        .id = 2,
        .bug_id = "B002",
        .name = "bughunt B002: explicit builtin import reports a user diagnostic",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import Str
            \\
            \\main! = || {}
            },
        },
        .command = .check,
        .expect = .clean_failure,
    },
    .{
        .id = 11,
        .bug_id = "B011",
        .name = "bughunt B011: 65-argument calls do not exceed ARC masks",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\f = |a0, _a1, _a2, _a3, _a4, _a5, _a6, _a7, _a8, _a9, _a10, _a11, _a12, _a13, _a14, _a15, _a16, _a17, _a18, _a19, _a20, _a21, _a22, _a23, _a24, _a25, _a26, _a27, _a28, _a29, _a30, _a31, _a32, _a33, _a34, _a35, _a36, _a37, _a38, _a39, _a40, _a41, _a42, _a43, _a44, _a45, _a46, _a47, _a48, _a49, _a50, _a51, _a52, _a53, _a54, _a55, _a56, _a57, _a58, _a59, _a60, _a61, _a62, _a63, _a64| a0
            \\
            \\main! = || {
            \\    _ = f(0.I64, 1.I64, 2.I64, 3.I64, 4.I64, 5.I64, 6.I64, 7.I64, 8.I64, 9.I64, 10.I64, 11.I64, 12.I64, 13.I64, 14.I64, 15.I64, 16.I64, 17.I64, 18.I64, 19.I64, 20.I64, 21.I64, 22.I64, 23.I64, 24.I64, 25.I64, 26.I64, 27.I64, 28.I64, 29.I64, 30.I64, 31.I64, 32.I64, 33.I64, 34.I64, 35.I64, 36.I64, 37.I64, 38.I64, 39.I64, 40.I64, 41.I64, 42.I64, 43.I64, 44.I64, 45.I64, 46.I64, 47.I64, 48.I64, 49.I64, 50.I64, 51.I64, 52.I64, 53.I64, 54.I64, 55.I64, 56.I64, 57.I64, 58.I64, 59.I64, 60.I64, 61.I64, 62.I64, 63.I64, 64.I64)
            \\    {}
            \\}
            },
        },
        .command = .run_interpreter,
        .expect = .success,
    },
    .{
        .id = 14,
        .bug_id = "B014",
        .name = "bughunt B014: roc docs reports parse errors without ICE text",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\main! =
            },
        },
        .command = .docs,
        .expect = .clean_failure,
    },
    .{
        .id = 15,
        .bug_id = "B015",
        .name = "bughunt B015: float div_trunc_by truncates toward zero",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\main! = || {
            \\    Stdout.line!(F32.div_trunc_by(7.5, 2.0).to_str())
            \\    Stdout.line!(F32.div_trunc_by(-7.5, 2.0).to_str())
            \\    Stdout.line!(F64.div_trunc_by(7.5, 2.0).to_str())
            \\    Stdout.line!(F64.div_trunc_by(-7.5, 2.0).to_str())
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "3\n-3\n3\n-3\n" },
    },
    .{
        .id = 17,
        .bug_id = "B017",
        .name = "bughunt B017: duplicate top-level definitions are blocking diagnostics",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\x = 1
            \\x = 2
            \\
            \\main! = || {}
            },
        },
        .command = .check,
        .expect = .clean_failure,
    },
    .{
        .id = 23,
        .bug_id = "B023",
        .name = "bughunt B023: roc test evaluates qualified Bool.True",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\expect Bool.True
            \\
            \\main! = || {}
            },
        },
        .command = .test_interpreter,
        .expect = .success,
    },
    .{
        .id = 32,
        .bug_id = "B032",
        .name = "bughunt B032: duplicate top-level type declarations are diagnostics",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\Foo := U8
            \\Foo := U16
            \\
            \\main! = || {}
            },
        },
        .command = .check,
        .expect = .clean_failure,
    },
    .{
        .id = 33,
        .bug_id = "B033",
        .name = "bughunt B033: top-level destructuring is supported or rejected cleanly",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\(a, b) = (1, 2)
            \\
            \\main! = || {}
            },
        },
        .command = .check,
        .expect = .success_or_clean_failure,
    },
    .{
        .id = 34,
        .bug_id = "B034",
        .name = "bughunt B034: missing platform-required value is diagnostic",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [] { pf: platform "{STR_PLATFORM}" }
            },
        },
        .command = .check,
        .expect = .clean_failure,
    },
    .{
        .id = 37,
        .bug_id = "B037",
        .name = "bughunt B037: too-many-exports diagnostic is renderable",
        .files = &.{
            .{ .path = "main.roc", .contents = "{GENERATE_TOO_MANY_EXPORTS_APP}" },
        },
        .command = .check,
        .expect = .clean_failure,
    },
    .{
        .id = 39,
        .bug_id = "B039",
        .name = "bughunt B039: List.join_with on list-valued items type-checks",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\main! = || {
            \\    _ = List.join_with([], [])
            \\    {}
            \\}
            },
        },
        .command = .check,
        .expect = .success,
    },
    .{
        .id = 40,
        .bug_id = "B040",
        .name = "bughunt B040: Str.from_utf8 does not leak runtime allocations",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\main! = || Stdout.line!(Str.inspect(Str.from_utf8([65.U8, 66])))
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "Ok(\"AB\")\n" },
    },
    .{
        .id = 41,
        .bug_id = "B041",
        .name = "bughunt B041: dotted local module imports resolve or diagnose cleanly",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import Dir.A
            \\
            \\main! = || {
            \\    _ = Dir.A.x
            \\    {}
            \\}
            },
            .{ .path = "Dir/A.roc", .contents =
            \\A :: [].{
            \\    x = 1
            \\}
            },
        },
        .command = .check,
        .expect = .success_or_clean_failure,
    },
    .{
        .id = 43,
        .bug_id = "B043",
        .name = "bughunt B043: Str.inspect on wide records does not overflow the compiler stack",
        .files = &.{
            .{ .path = "main.roc", .contents = "{GENERATE_WIDE_RECORD_INSPECT_APP}" },
        },
        .command = .run_dev,
        .expect = .success,
    },
    .{
        .id = 44,
        .bug_id = "B044",
        .name = "bughunt B044: platform relation rejects numeric return for unit effect",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\main! = || 1
            },
        },
        .command = .check,
        .expect = .clean_failure,
    },
    .{
        .id = 53,
        .bug_id = "B053",
        .name = "bughunt B053: unsuffixed numeric method receiver is defaulted",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\main! = || Stdout.line!(Str.inspect(1.to_str()))
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "\"1.0\"\n" },
    },
    .{
        .id = 54,
        .bug_id = "B054",
        .name = "bughunt B054: tail-recursive list concat does not leak",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\repeat_helper = |acc, list, n| match n {
            \\    0 => acc
            \\    _ => repeat_helper(acc.concat(list), list, n - 1)
            \\}
            \\
            \\main! = || Stdout.line!(Str.inspect(repeat_helper([], [1.I64], 2)))
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "[1, 1]\n" },
    },
    .{
        .id = 55,
        .bug_id = "B055",
        .name = "bughunt B055: --allow-errors missing methods do not panic lowering",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\main! = || Stdout.line!(Str.inspect(1.nope()))
            },
        },
        .command = .run_dev_allow_errors,
        .expect = .success_or_clean_failure,
    },
    .{
        .id = 61,
        .bug_id = "B061",
        .name = "bughunt B061: boxed builtin nominal constants can be planned",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\x : Box(Try(I64, [E]))
            \\x = Box.box(Ok(1))
            \\
            \\main! = || {
            \\    _ = x
            \\    {}
            \\}
            },
        },
        .command = .check,
        .expect = .success,
    },
    .{
        .id = 62,
        .bug_id = "B062",
        .name = "bughunt B062: zero-sized list concat does not leak",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\main! = || Stdout.line!(Str.inspect([{}].concat([{}])))
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "[{}, {}]\n" },
    },
    .{
        .id = 66,
        .bug_id = "B066",
        .name = "bughunt B066: transitive nominal method owners are available",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import A
            \\
            \\main! = || {
            \\    _ = Str.inspect(A.make)
            \\    {}
            \\}
            },
            .{ .path = "A.roc", .contents =
            \\import B
            \\
            \\A :: [].{
            \\    make = B.make
            \\}
            },
            .{ .path = "B.roc", .contents =
            \\B :: [].{
            \\    Color := [Red, Blue].{
            \\        to_inspect : Color -> Str
            \\        to_inspect = |c| match c {
            \\            Red => "R"
            \\            Blue => "B"
            \\        }
            \\    }
            \\
            \\    make : Color
            \\    make = Red
            \\}
            },
        },
        .command = .check,
        .expect = .success,
    },
    .{
        .id = 70,
        .bug_id = "B070",
        .name = "bughunt B070: inclusive U8 max range terminates",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\main! = || Stdout.line!(Str.inspect(Iter.fold(U8.to(255, 255), [], |acc, item| acc.append(item))))
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "[255]\n" },
    },
    .{
        .id = 71,
        .bug_id = "B071",
        .name = "bughunt B071: unannotated imported-nominal list results lower safely",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [make_glue] { pf: platform "{GLUE_PLATFORM}" }
            \\
            \\import pf.Types exposing [Types]
            \\import pf.File exposing [File]
            \\import pf.TypeRepr exposing [TypeRepr]
            \\
            \\make_glue : List(Types) -> Try(List(File), Str)
            \\make_glue = |_| {
            \\    type_table : List(TypeRepr)
            \\    type_table = []
            \\
            \\    found = lookup_record_in_type_table(type_table, 0)
            \\
            \\    Ok([{ name: "x", content: Str.inspect(found) }])
            \\}
            \\
            \\lookup_record_in_type_table = |type_table, type_id| {
            \\    match List.get(type_table, type_id) {
            \\        Ok(type_repr) =>
            \\            match type_repr {
            \\                RocRecord(rec) => { found: True, fields: rec.fields, size: rec.size, alignment: rec.alignment }
            \\                _ => { found: False, fields: [], size: 0, alignment: 0 }
            \\            }
            \\
            \\        Err(_) => { found: False, fields: [], size: 0, alignment: 0 }
            \\    }
            \\}
            },
        },
        .command = .test_interpreter,
        .expect = .success,
    },
    .{
        .id = 72,
        .bug_id = "B072",
        .name = "bughunt B072: roc glue lowers the synthetic app",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [make_glue] { pf: platform "{GLUE_PLATFORM}" }
            \\
            \\import pf.Types exposing [Types]
            \\import pf.File exposing [File]
            \\
            \\make_glue : List(Types) -> Try(List(File), Str)
            \\make_glue = |_| Ok([{ name: "x", content: "y" }])
            },
        },
        .command = .glue,
        .expect = .success,
    },
    .{
        .id = 73,
        .bug_id = "B073",
        .name = "bughunt B073: deep method chains do not overflow the compiler stack",
        .files = &.{
            .{ .path = "main.roc", .contents = "{GENERATE_DEEP_CONCAT_APP}" },
        },
        .command = .run_dev,
        .expect = .success,
    },
    .{
        .id = 74,
        .bug_id = "B074",
        .name = "bughunt B074: effectful top-level definitions are rejected",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\x = Stdout.line!("x")
            \\
            \\main! = || x
            },
        },
        .command = .check,
        .expect = .clean_failure,
    },
    .{
        .id = 75,
        .bug_id = "B075",
        .name = "bughunt B075: hosted effects inside expect do not panic the test runner",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\expect {
            \\    _ = Stdout.line!("x")
            \\    True
            \\}
            \\
            \\main! = || {}
            },
        },
        .command = .test_interpreter,
        .expect = .success_or_clean_failure,
    },
    .{
        .id = 81,
        .bug_id = "B081",
        .name = "bughunt B081: List.join_with list values terminate at runtime",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\main! = || {
            \\    _ = [[1.I64]].join_with([])
            \\    {}
            \\}
            },
        },
        .command = .run_dev,
        .expect = .success,
    },
    .{
        .id = 83,
        .bug_id = "B083",
        .name = "bughunt B083: Box equality uses value semantics or is rejected earlier",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\main! = || Stdout.line!(Str.inspect(Box.box(1.I64) == Box.box(1.I64)))
            },
        },
        .command = .run_dev,
        .expect = .clean_failure,
    },
    .{
        .id = 84,
        .bug_id = "B084",
        .name = "bughunt B084: higher-order effectful lambda calls lower safely",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\main! = || (|f| f())(|| Stdout.line!("x"))
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "x\n" },
    },
    .{
        .id = 86,
        .bug_id = "B086",
        .name = "bughunt B086: top-level user nominal constants have backing metadata",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\Color := [Red, Blue]
            \\
            \\make : Color
            \\make = Red
            \\
            \\main! = || Stdout.line!(Str.inspect(make))
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "Red\n" },
    },
    .{
        .id = 87,
        .bug_id = "B087",
        .name = "bughunt B087: warning-only runs exit successfully",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\main! = || {
            \\    x = 5
            \\    Stdout.line!("Hello")
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .warning_stdout_exact = "Hello\n" },
    },
    .{
        .id = 89,
        .bug_id = "B089",
        .name = "bughunt B089: adjacent invalid UTF-8 bytes collapse to one replacement",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\main! = || Stdout.line!(Str.inspect(Str.from_utf8_lossy([255.U8, 255])))
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "\"\xEF\xBF\xBD\"\n" },
    },
    .{
        .id = 93,
        .bug_id = "B093",
        .name = "bughunt B093: duplicate record-builder fields are diagnostics",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\B(a) := { value : a }.{
            \\    map2 : B(a), B(b), (a, b -> c) -> B(c)
            \\    map2 = |ba, bb, f| { value: f(ba.value, bb.value) }
            \\
            \\    pure : a -> B(a)
            \\    pure = |x| { value: x }
            \\}
            \\
            \\main! = || {
            \\    _ = { a: B.pure(1.I64), a: B.pure(2.I64) }.B
            \\    {}
            \\}
            },
        },
        .command = .check,
        .expect = .clean_failure,
    },
    .{
        .id = 94,
        .bug_id = "B094",
        .name = "bughunt B094: Str.repeat short-circuits empty strings",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\main! = || Stdout.line!(Str.inspect("".repeat(18446744073709551615.U64)))
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "\"\"\n" },
    },
    .{
        .id = 101,
        .bug_id = "B101",
        .name = "bughunt B101: imported boxed callables preserve callable payloads",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\import A
            \\
            \\main! = || Stdout.line!(I64.to_str(A.run(41.I64)))
            },
            .{ .path = "A.roc", .contents =
            \\A :: [].{
            \\    make : I64 -> Box((I64 -> I64))
            \\    make = |n| Box.box(|x| x + n)
            \\
            \\    run : I64 -> I64
            \\    run = |x| {
            \\        f = Box.unbox(make(1.I64))
            \\        f(x)
            \\    }
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 102,
        .bug_id = "B102",
        .name = "bughunt B102: imported boxed polymorphic identity is sealed per use",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\import A
            \\
            \\main! = || Stdout.line!(Str.inspect({ n: A.id_num(41.I64), s: A.id_str("ok") }))
            },
            .{ .path = "A.roc", .contents =
            \\A :: [].{
            \\    identity = |x| x
            \\    id_num = Box.unbox(Box.box(identity))
            \\    id_str = Box.unbox(Box.box(identity))
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "{ n: 41, s: \"ok\" }\n" },
    },
    .{
        .id = 103,
        .bug_id = "B103",
        .name = "bughunt B103: recursive functions can be boxed and unboxed",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\go = |n| {
            \\    f = Box.unbox(Box.box(go))
            \\    match n {
            \\        0 => 42.I64
            \\        _ => f(n - 1)
            \\    }
            \\}
            \\
            \\main! = || Stdout.line!(I64.to_str(go(1)))
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 104,
        .bug_id = "B104",
        .name = "bughunt B104: function-valued tag payloads lower safely",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\Value := [F((I64 -> I64)), N(I64)]
            \\
            \\main! = || {
            \\    v = F(|x| x + 1.I64)
            \\    out = match v {
            \\        F(f) => f(41.I64)
            \\        N(n) => n
            \\    }
            \\    Stdout.line!(I64.to_str(out))
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 105,
        .bug_id = "B105",
        .name = "bughunt B105: imported function-valued tag payloads lower safely",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\import A
            \\
            \\main! = || Stdout.line!(I64.to_str(A.run(A.make)))
            },
            .{ .path = "A.roc", .contents =
            \\A :: [].{
            \\    Value := [F((I64 -> I64)), N(I64)]
            \\
            \\    make : Value
            \\    make = F(|x| x + 1.I64)
            \\
            \\    run : Value -> I64
            \\    run = |v| match v {
            \\        F(f) => f(41.I64)
            \\        N(n) => n
            \\    }
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 106,
        .bug_id = "B106",
        .name = "bughunt B106: top-level record constants can contain function fields",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\r = { f: |x| x + 1.I64 }
            \\
            \\main! = || Stdout.line!(I64.to_str((r.f)(41.I64)))
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 107,
        .bug_id = "B107",
        .name = "bughunt B107: top-level tag constants can contain function payloads",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\Value := [F((I64 -> I64)), N(I64)]
            \\
            \\v : Value
            \\v = F(|x| x + 1.I64)
            \\
            \\main! = || {
            \\    out = match v {
            \\        F(f) => f(41.I64)
            \\        N(n) => n
            \\    }
            \\    Stdout.line!(I64.to_str(out))
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 108,
        .bug_id = "B108",
        .name = "bughunt B108: open-error custom tags are released on host exit",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_OPEN_PLATFORM}" }
            \\
            \\main! = |_| Err(CustomError)
            },
        },
        .command = .run_dev,
        .expect = .clean_failure,
    },
    .{
        .id = 109,
        .bug_id = "B109",
        .name = "bughunt B109: open-error payload tags are released with correct alignment",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_OPEN_PLATFORM}" }
            \\
            \\main! = |_| Err(CustomError("payload that allocates outside small string storage"))
            },
        },
        .command = .run_dev,
        .expect = .clean_failure,
    },
    .{
        .id = 110,
        .bug_id = "B110",
        .name = "bughunt B110: imported recursive boxed-tree equality terminates",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\import A
            \\
            \\main! = || Stdout.line!(Str.inspect(A.a == A.b))
            },
            .{ .path = "A.roc", .contents =
            \\A :: [].{
            \\    Tree := [Leaf(I64), Node(Box(Tree), Box(Tree))]
            \\
            \\    a : Tree
            \\    a = Node(Box.box(Leaf(1)), Box.box(Leaf(2)))
            \\
            \\    b : Tree
            \\    b = Node(Box.box(Leaf(1)), Box.box(Leaf(2)))
            \\}
            },
        },
        .command = .run_dev,
        .expect = .clean_failure,
    },
    .{
        .id = 111,
        .bug_id = "B111",
        .name = "bughunt B111: recursive boxed-tree custom equality terminates",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\Tree := [Leaf(I64), Node(Box(Tree), Box(Tree))].{
            \\    is_eq : Tree, Tree -> Bool
            \\    is_eq = |l, r| match (l, r) {
            \\        (Leaf(a), Leaf(b)) => a == b
            \\        (Node(al, ar), Node(bl, br)) => Box.unbox(al).is_eq(Box.unbox(bl)) and Box.unbox(ar).is_eq(Box.unbox(br))
            \\        _ => Bool.False
            \\    }
            \\}
            \\
            \\main! = || {
            \\    a : Tree
            \\    a = Node(Box.box(Leaf(1)), Box.box(Leaf(2)))
            \\
            \\    b : Tree
            \\    b = Node(Box.box(Leaf(1)), Box.box(Leaf(2)))
            \\
            \\    Stdout.line!(Str.inspect(a == b))
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "True\n" },
    },
    .{
        .id = 112,
        .bug_id = "B112",
        .name = "bughunt B112: top-level boxed function constants can be called",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\boxed = Box.box(|x| x + 1.I64)
            \\
            \\main! = || {
            \\    f = Box.unbox(boxed)
            \\    Stdout.line!(I64.to_str(f(41.I64)))
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 113,
        .bug_id = "B113",
        .name = "bughunt B113: imported top-level boxed function constants can be called",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\import A
            \\
            \\main! = || {
            \\    f = Box.unbox(A.boxed)
            \\    Stdout.line!(I64.to_str(f(41.I64)))
            \\}
            },
            .{ .path = "A.roc", .contents =
            \\A :: [].{
            \\    boxed : Box((I64 -> I64))
            \\    boxed = Box.box(|x| x + 1.I64)
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 114,
        .bug_id = "B114",
        .name = "bughunt B114: host can call a top-level boxed function constant",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\import pf.Host
            \\
            \\boxed = Box.box(|x| x + 1.I64)
            \\
            \\main! = || Stdout.line!(I64.to_str(Host.call_boxed!(boxed, 41.I64)))
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 115,
        .bug_id = "B115",
        .name = "bughunt B115: host can roundtrip a top-level boxed function constant",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\import pf.Host
            \\
            \\boxed = Box.box(|x| x + 1.I64)
            \\
            \\main! = || {
            \\    f = Box.unbox(Host.roundtrip_boxed!(boxed))
            \\    Stdout.line!(I64.to_str(f(41.I64)))
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 116,
        .bug_id = "B116",
        .name = "bughunt B116: host can store a top-level boxed function constant",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\import pf.Host
            \\
            \\boxed = Box.box(|x| x + 1.I64)
            \\
            \\main! = || {
            \\    Host.store_boxed!(boxed)
            \\    Stdout.line!(I64.to_str(Host.stored_boxed_call!(41.I64)))
            \\    Host.release_stored_boxed!()
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 117,
        .bug_id = "B117",
        .name = "bughunt B117: imported attached methods can return functions",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\import A
            \\
            \\main! = || {
            \\    f = A.h.make()
            \\    Stdout.line!(I64.to_str(f(41.I64)))
            \\}
            },
            .{ .path = "A.roc", .contents =
            \\A :: [].{
            \\    Holder := { n : I64 }.{
            \\        make : Holder -> (I64 -> I64)
            \\        make = |self| |x| x + self.n
            \\    }
            \\
            \\    h : Holder
            \\    h = { n: 1 }
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 118,
        .bug_id = "B118",
        .name = "bughunt B118: static-dispatch methods can return functions",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\Maker := { tag : I64 }.{
            \\    make : Maker -> (I64 -> I64)
            \\    make = |_| |x| x + 1.I64
            \\}
            \\
            \\use = |m| m.make()(41.I64)
            \\
            \\main! = || {
            \\    m : Maker
            \\    m = { tag: 0 }
            \\    Stdout.line!(I64.to_str(use(m)))
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 119,
        .bug_id = "B119",
        .name = "bughunt B119: imported static-dispatch methods can return functions",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\import A
            \\
            \\main! = || Stdout.line!(I64.to_str(A.use(A.m)))
            },
            .{ .path = "A.roc", .contents =
            \\A :: [].{
            \\    Maker := { tag : I64 }.{
            \\        make : Maker -> (I64 -> I64)
            \\        make = |_| |x| x + 1.I64
            \\    }
            \\
            \\    m : Maker
            \\    m = { tag: 0 }
            \\
            \\    use = |x| x.make()(41.I64)
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 120,
        .bug_id = "B120",
        .name = "bughunt B120: imported attached methods are available through qualified values",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\import A
            \\
            \\main! = || Stdout.line!(A.x.show())
            },
            .{ .path = "A.roc", .contents =
            \\A :: [].{
            \\    Id := [Id(I64)].{
            \\        show : Id -> Str
            \\        show = |v| match v {
            \\            Id(n) => I64.to_str(n)
            \\        }
            \\    }
            \\
            \\    x : Id
            \\    x = Id(42)
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 121,
        .bug_id = "B121",
        .name = "bughunt B121: imported nominal operators use attached operator methods",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\import A
            \\
            \\main! = || Stdout.line!(Str.inspect(A.a + A.b))
            },
            .{ .path = "A.roc", .contents =
            \\A :: [].{
            \\    Thing := [Thing(I64)].{
            \\        plus : Thing, Thing -> Thing
            \\        plus = |_, _| Thing(42)
            \\    }
            \\
            \\    a : Thing
            \\    a = Thing(1)
            \\
            \\    b : Thing
            \\    b = Thing(2)
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "Thing(42)\n" },
    },
    .{
        .id = 122,
        .bug_id = "B122",
        .name = "bughunt B122: boxed imported nominal equality uses payload equality",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\import A
            \\
            \\main! = || Stdout.line!(Str.inspect(Box.box(A.a) == Box.box(A.b)))
            },
            .{ .path = "A.roc", .contents =
            \\A :: [].{
            \\    Thing := [Thing(I64)].{
            \\        is_eq : Thing, Thing -> Bool
            \\        is_eq = |_, _| Bool.True
            \\    }
            \\
            \\    a : Thing
            \\    a = Thing(1)
            \\
            \\    b : Thing
            \\    b = Thing(2)
            \\}
            },
        },
        .command = .run_dev,
        .expect = .clean_failure,
    },
    .{
        .id = 123,
        .bug_id = "B123",
        .name = "bughunt B123: recursive data can carry function payloads",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\Tree := [Leaf((I64 -> I64)), Node(Box(Tree), Box(Tree))]
            \\
            \\main! = || {
            \\    tree = Leaf(|x| x + 1.I64)
            \\    out = match tree {
            \\        Leaf(f) => f(41.I64)
            \\        Node(_, _) => 0
            \\    }
            \\    Stdout.line!(I64.to_str(out))
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 124,
        .bug_id = "B124",
        .name = "bughunt B124: imported recursive data can carry function payloads",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\import A
            \\
            \\main! = || Stdout.line!(I64.to_str(A.run(A.tree)))
            },
            .{ .path = "A.roc", .contents =
            \\A :: [].{
            \\    Tree := [Leaf((I64 -> I64)), Node(Box(Tree), Box(Tree))]
            \\
            \\    tree : Tree
            \\    tree = Leaf(|x| x + 1.I64)
            \\
            \\    run : Tree -> I64
            \\    run = |value| match value {
            \\        Leaf(f) => f(41.I64)
            \\        Node(_, _) => 0
            \\    }
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 125,
        .bug_id = "B125",
        .name = "bughunt B125: record function fields can be called directly",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\main! = || {
            \\    r = { f: |x| x + 1.I64 }
            \\    Stdout.line!(I64.to_str((r.f)(41.I64)))
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 126,
        .bug_id = "B126",
        .name = "bughunt B126: record function fields can be returned and called",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\get = |r| r.f
            \\
            \\main! = || {
            \\    r = { f: |x| x + 1.I64 }
            \\    f = get(r)
            \\    Stdout.line!(I64.to_str(f(41.I64)))
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 127,
        .bug_id = "B127",
        .name = "bughunt B127: list function payloads can be selected and called",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\main! = || {
            \\    fs = [|x| x + 1.I64, |x| x + 10.I64]
            \\    f = match List.get(fs, 0) {
            \\        Ok(g) => g
            \\        Err(_) => |x| x
            \\    }
            \\    Stdout.line!(I64.to_str(f(41.I64)))
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 128,
        .bug_id = "B128",
        .name = "bughunt B128: top-level list constants can contain functions",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\fs = [|x| x + 1.I64, |x| x + 10.I64]
            \\
            \\main! = || {
            \\    f = match List.get(fs, 0) {
            \\        Ok(g) => g
            \\        Err(_) => |x| x
            \\    }
            \\    Stdout.line!(I64.to_str(f(41.I64)))
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 129,
        .bug_id = "B129",
        .name = "bughunt B129: imported list constants can contain functions",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\import A
            \\
            \\main! = || {
            \\    f = match List.get(A.fs, 0) {
            \\        Ok(g) => g
            \\        Err(_) => |x| x
            \\    }
            \\    Stdout.line!(I64.to_str(f(41.I64)))
            \\}
            },
            .{ .path = "A.roc", .contents =
            \\A :: [].{
            \\    fs = [|x| x + 1.I64, |x| x + 10.I64]
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 130,
        .bug_id = "B130",
        .name = "bughunt B130: tuple function payloads can be called",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\main! = || {
            \\    t = (|x| x + 1.I64, 41.I64)
            \\    Stdout.line!(I64.to_str((t.0)(t.1)))
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 131,
        .bug_id = "B131",
        .name = "bughunt B131: top-level tuple constants can contain functions",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\t = (|x| x + 1.I64, 41.I64)
            \\
            \\main! = || Stdout.line!(I64.to_str((t.0)(t.1)))
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 132,
        .bug_id = "B132",
        .name = "bughunt B132: imported tuple constants can contain functions",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\import A
            \\
            \\main! = || Stdout.line!(I64.to_str((A.t.0)(A.t.1)))
            },
            .{ .path = "A.roc", .contents =
            \\A :: [].{
            \\    t = (|x| x + 1.I64, 41.I64)
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 133,
        .bug_id = "B133",
        .name = "bughunt B133: Try payloads can contain functions",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\main! = || {
            \\    v = Ok(|x| x + 1.I64)
            \\    f = match v {
            \\        Ok(g) => g
            \\        Err(_) => |x| x
            \\    }
            \\    Stdout.line!(I64.to_str(f(41.I64)))
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 134,
        .bug_id = "B134",
        .name = "bughunt B134: imported Try payloads can contain functions",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\import A
            \\
            \\main! = || {
            \\    f = match A.v {
            \\        Ok(g) => g
            \\        Err(_) => |x| x
            \\    }
            \\    Stdout.line!(I64.to_str(f(41.I64)))
            \\}
            },
            .{ .path = "A.roc", .contents =
            \\A :: [].{
            \\    v = Ok(|x| x + 1.I64)
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 135,
        .bug_id = "B135",
        .name = "bughunt B135: record updates preserve function fields",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\main! = || {
            \\    r = { f: |x| x + 1.I64, n: 0.I64 }
            \\    updated = { ..r, n: 41.I64 }
            \\    Stdout.line!(I64.to_str((updated.f)(updated.n)))
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 136,
        .bug_id = "B136",
        .name = "bughunt B136: while loops can update function variables",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\main! = || {
            \\    var $n = 0.I64
            \\    var $f = |x| x
            \\    while $n < 1 {
            \\        $f = |x| x + 1.I64
            \\        $n = $n + 1
            \\    }
            \\    Stdout.line!(I64.to_str($f(41.I64)))
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 137,
        .bug_id = "B137",
        .name = "bughunt B137: for loops can destructure function fields",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\main! = || {
            \\    var $out = 0.I64
            \\    for { f, n } in [{ f: |x| x + 1.I64, n: 41.I64 }] {
            \\        $out = f(n)
            \\    }
            \\    Stdout.line!(I64.to_str($out))
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 138,
        .bug_id = "B138",
        .name = "bughunt B138: host can call imported boxed functions stored in tag payloads",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\import pf.Host
            \\import A
            \\
            \\main! = || {
            \\    out = match A.v {
            \\        A.Value.F(f) => Host.call_boxed!(f, 41.I64)
            \\    }
            \\    Stdout.line!(I64.to_str(out))
            \\}
            },
            .{ .path = "A.roc", .contents =
            \\A :: [].{
            \\    Value := [F(Box((I64 -> I64)))]
            \\
            \\    v : Value
            \\    v = F(Box.box(|x| x + 1.I64))
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .success_stdout_exact = "42\n" },
    },
    .{
        .id = 139,
        .bug_id = "B139",
        .name = "bughunt B139: function variable reassignment is not a warning",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\
            \\main! = || {
            \\    var $f = |x| x + 1.I64
            \\    $f = |x| x + 2.I64
            \\    Stdout.line!(I64.to_str($f(40.I64)))
            \\}
            },
        },
        .command = .run_dev,
        .expect = .{ .warning_stdout_exact = "42\n" },
    },
    runDevBodyCase(140, "B140", "nested record function fields can be called",
        \\main! = || {
        \\    r = { inner: { f: |x| x + 1.I64 } }
        \\    Stdout.line!(I64.to_str((r.inner.f)(41.I64)))
        \\}
    , "42\n"),
    runDevBodyCase(141, "B141", "tag payload records can contain callable fields",
        \\Value := [Wrap({ f : I64 -> I64 })]
        \\
        \\main! = || {
        \\    v = Wrap({ f: |x| x + 1.I64 })
        \\    out = match v {
        \\        Wrap(r) => (r.f)(41.I64)
        \\    }
        \\    Stdout.line!(I64.to_str(out))
        \\}
    , "42\n"),
    runDevBodyCase(142, "B142", "record function fields can be boxed and unboxed",
        \\main! = || {
        \\    r = { f: |x| x + 1.I64 }
        \\    g = Box.unbox(Box.box(r.f))
        \\    Stdout.line!(I64.to_str(g(41.I64)))
        \\}
    , "42\n"),
    checkBodyCase(143, "B143", "duplicate attached method definitions are diagnostics",
        \\Thing := [Thing(I64)].{
        \\    show = |_| "a"
        \\    show = |_| "b"
        \\}
        \\
        \\main! = || {}
    ),
    runDevBodyCase(144, "B144", "return works inside closures passed through higher-order calls",
        \\apply = |f| f(41.I64)
        \\
        \\main! = || {
        \\    out = apply(|x| {
        \\        return x + 1.I64
        \\        x
        \\    })
        \\    Stdout.line!(I64.to_str(out))
        \\}
    , "42\n"),
    runDevBodyCase(145, "B145", "return works inside closures stored in tag payloads",
        \\Value := [F((I64 -> I64))]
        \\
        \\main! = || {
        \\    v = F(|x| {
        \\        return x + 1.I64
        \\        x
        \\    })
        \\    out = match v {
        \\        F(f) => f(41.I64)
        \\    }
        \\    Stdout.line!(I64.to_str(out))
        \\}
    , "42\n"),
    runDevBodyCase(146, "B146", "return works inside boxed closures",
        \\main! = || {
        \\    f = Box.unbox(Box.box(|x| {
        \\        return x + 1.I64
        \\        x
        \\    }))
        \\    Stdout.line!(I64.to_str(f(41.I64)))
        \\}
    , "42\n"),
    runDevCleanFailureBodyCase(147, "B147", "break in record-stored closures inside loops is rejected cleanly",
        \\main! = || {
        \\    var $n = 0.I64
        \\    while $n < 1 {
        \\        r = { f: || { break } }
        \\        (r.f)()
        \\        $n = $n + 1
        \\    }
        \\    Stdout.line!(I64.to_str($n))
        \\}
    ),
    runDevCleanFailureBodyCase(148, "B148", "break in higher-order closures inside loops is rejected cleanly",
        \\call = |f| f()
        \\
        \\main! = || {
        \\    var $n = 0.I64
        \\    while $n < 1 {
        \\        call(|| { break })
        \\        $n = $n + 1
        \\    }
        \\    Stdout.line!(I64.to_str($n))
        \\}
    ),
    runDevBodyCase(149, "B149", "record destructuring preserves callable fields",
        \\main! = || {
        \\    { f } = { f: |x| x + 1.I64 }
        \\    Stdout.line!(I64.to_str(f(41.I64)))
        \\}
    , "42\n"),
    runDevBodyCase(150, "B150", "renamed record destructuring preserves callable fields",
        \\main! = || {
        \\    { f: g } = { f: |x| x + 1.I64 }
        \\    Stdout.line!(I64.to_str(g(41.I64)))
        \\}
    , "42\n"),
    runDevBodyCase(151, "B151", "tuple destructuring preserves callable elements",
        \\main! = || {
        \\    (f, n) = (|x| x + 1.I64, 41.I64)
        \\    Stdout.line!(I64.to_str(f(n)))
        \\}
    , "42\n"),
    runDevTwoFileCase(152, "B152", "imported record callable fields survive destructuring",
        \\import A
        \\
        \\main! = || {
        \\    { f } = A.r
        \\    Stdout.line!(I64.to_str(f(41.I64)))
        \\}
    ,
        \\A :: [].{
        \\    r = { f: |x| x + 1.I64 }
        \\}
    , "42\n"),
    runDevTwoFileCase(153, "B153", "imported callable tag payloads survive parameter destructuring",
        \\import A
        \\
        \\main! = || Stdout.line!(I64.to_str(A.run(A.v)))
    ,
        \\A :: [].{
        \\    Value := [F((I64 -> I64))]
        \\
        \\    v : Value
        \\    v = F(|x| x + 1.I64)
        \\
        \\    run = |F(f)| f(41.I64)
        \\}
    , "42\n"),
    runDevThreeFileCase(154, "B154", "transitive nominal equality has the original owner environment",
        \\import A
        \\
        \\main! = || Stdout.line!(Str.inspect(A.a == A.b))
    ,
        \\import B
        \\
        \\A :: [].{
        \\    a : B.Thing
        \\    a = B.a
        \\
        \\    b : B.Thing
        \\    b = B.b
        \\}
    ,
        \\B :: [].{
        \\    Thing := [Thing(I64)].{
        \\        is_eq : Thing, Thing -> Bool
        \\        is_eq = |_, _| Bool.True
        \\    }
        \\
        \\    a : Thing
        \\    a = Thing(1)
        \\
        \\    b : Thing
        \\    b = Thing(2)
        \\}
    , "True\n"),
    runDevThreeFileCase(155, "B155", "transitive nominal operators have the original owner environment",
        \\import A
        \\
        \\main! = || Stdout.line!(Str.inspect(A.a + A.b))
    ,
        \\import B
        \\
        \\A :: [].{
        \\    a : B.Thing
        \\    a = B.a
        \\
        \\    b : B.Thing
        \\    b = B.b
        \\}
    ,
        \\B :: [].{
        \\    Thing := [Thing(I64)].{
        \\        plus : Thing, Thing -> Thing
        \\        plus = |_, _| Thing(42)
        \\    }
        \\
        \\    a : Thing
        \\    a = Thing(1)
        \\
        \\    b : Thing
        \\    b = Thing(2)
        \\}
    , "Thing(42)\n"),
    runDevThreeFileCase(156, "B156", "transitive callable tag payloads keep exported templates",
        \\import A
        \\
        \\main! = || Stdout.line!(I64.to_str(A.run(A.v)))
    ,
        \\import B
        \\
        \\A :: [].{
        \\    v : B.Value
        \\    v = B.v
        \\
        \\    run = B.run
        \\}
    ,
        \\B :: [].{
        \\    Value := [F((I64 -> I64))]
        \\
        \\    v : Value
        \\    v = F(|x| x + 1.I64)
        \\
        \\    run : Value -> I64
        \\    run = |value| match value {
        \\        F(f) => f(41.I64)
        \\    }
        \\}
    , "42\n"),
    runDevThreeFileCase(157, "B157", "transitive boxed callable constants keep exported templates",
        \\import A
        \\
        \\main! = || {
        \\    f = Box.unbox(A.boxed)
        \\    Stdout.line!(I64.to_str(f(41.I64)))
        \\}
    ,
        \\import B
        \\
        \\A :: [].{
        \\    boxed : Box((I64 -> I64))
        \\    boxed = B.boxed
        \\}
    ,
        \\B :: [].{
        \\    boxed : Box((I64 -> I64))
        \\    boxed = Box.box(|x| x + 1.I64)
        \\}
    , "42\n"),
    runDevThreeFileCase(158, "B158", "transitive static-dispatch methods returning functions keep owner environments",
        \\import A
        \\
        \\main! = || Stdout.line!(I64.to_str(A.use(A.m)))
    ,
        \\import B
        \\
        \\A :: [].{
        \\    m : B.Maker
        \\    m = B.m
        \\
        \\    use = B.use
        \\}
    ,
        \\B :: [].{
        \\    Maker := { tag : I64 }.{
        \\        make : Maker -> (I64 -> I64)
        \\        make = |_| |x| x + 1.I64
        \\    }
        \\
        \\    m : Maker
        \\    m = { tag: 0 }
        \\
        \\    use = |x| x.make()(41.I64)
        \\}
    , "42\n"),
    runDevBodyCase(159, "B159", "top-level boxed polymorphic identity is sealed per concrete use",
        \\identity = |x| x
        \\id_num = Box.unbox(Box.box(identity))
        \\id_str = Box.unbox(Box.box(identity))
        \\
        \\main! = || Stdout.line!(Str.inspect({ n: id_num(41.I64), s: id_str("ok") }))
    , "{ n: 41, s: \"ok\" }\n"),
    runDevBodyCase(160, "B160", "top-level boxed generic record builders are sealed per concrete use",
        \\pair = |x| { left: x, right: x }
        \\mk_i = Box.unbox(Box.box(pair))
        \\mk_s = Box.unbox(Box.box(pair))
        \\
        \\main! = || Stdout.line!(Str.inspect({ i: mk_i(21.I64).left + mk_i(21.I64).right, s: mk_s("x").left }))
    , "{ i: 42, s: \"x\" }\n"),
    runDevBodyCase(161, "B161", "boxed top-level functions returning polymorphic functions are sealed",
        \\make = |_| |x| x
        \\f = Box.unbox(Box.box(make(0.I64)))
        \\
        \\main! = || Stdout.line!(I64.to_str(f(42.I64)))
    , "42\n"),
    runDevTwoFileCase(162, "B162", "imported boxed generic record builders are sealed per concrete use",
        \\import A
        \\
        \\main! = || Stdout.line!(Str.inspect({ i: A.mk_i(21.I64).left + A.mk_i(21.I64).right, s: A.mk_s("x").left }))
    ,
        \\A :: [].{
        \\    pair = |x| { left: x, right: x }
        \\    mk_i = Box.unbox(Box.box(pair))
        \\    mk_s = Box.unbox(Box.box(pair))
        \\}
    , "{ i: 42, s: \"x\" }\n"),
    runDevBodyCase(163, "B163", "top-level records can store boxed generic functions",
        \\identity = |x| x
        \\r = { f: Box.unbox(Box.box(identity)) }
        \\
        \\main! = || Stdout.line!(I64.to_str((r.f)(42.I64)))
    , "42\n"),
    .{
        .id = 164,
        .bug_id = "B164",
        .name = "bughunt B164: imported polymorphic recursive boxed-tree equality is rejected cleanly",
        .files = &.{
            .{ .path = "main.roc", .contents =
            \\app [main!] { pf: platform "{FX_PLATFORM}" }
            \\
            \\import pf.Stdout
            \\import A
            \\
            \\main! = || Stdout.line!(Str.inspect(A.a == A.b))
            },
            .{ .path = "A.roc", .contents =
            \\A :: [].{
            \\    Tree(a) := [Node(Box(Tree(a)), Box(Tree(a))), Leaf(a)]
            \\
            \\    a : Tree(I64)
            \\    a = Node(Box.box(Leaf(41.I64)), Box.box(Leaf(1)))
            \\
            \\    b : Tree(I64)
            \\    b = Node(Box.box(Leaf(41.I64)), Box.box(Leaf(1)))
            \\}
            },
        },
        .command = .run_dev,
        .expect = .clean_failure,
    },
    runDevBodyCase(165, "B165", "Str.inspect handles empty-error list payloads",
        \\x : Try(List(I64), [])
        \\x = Ok([42])
        \\
        \\main! = || Stdout.line!(Str.inspect(x))
    , "Ok([42])\n"),
    runDevBodyCase(166, "B166", "Str.inspect handles empty-error record payloads",
        \\x : Try({ a : I64 }, [])
        \\x = Ok({ a: 42 })
        \\
        \\main! = || Stdout.line!(Str.inspect(x))
    , "Ok({ a: 42 })\n"),
    runDevTwoFileCase(167, "B167", "Str.inspect handles imported empty-error list payloads",
        \\import A
        \\
        \\main! = || Stdout.line!(Str.inspect(A.x))
    ,
        \\A :: [].{
        \\    x : Try(List(I64), [])
        \\    x = Ok([42])
        \\}
    , "Ok([42])\n"),
    runDevTwoFileCase(168, "B168", "Str.inspect handles imported empty-error record payloads",
        \\import A
        \\
        \\main! = || Stdout.line!(Str.inspect(A.x))
    ,
        \\A :: [].{
        \\    x : Try({ a : I64 }, [])
        \\    x = Ok({ a: 42 })
        \\}
    , "Ok({ a: 42 })\n"),
    runDevBodyCase(169, "B169", "Str.inspect handles uninhabited error unions in record fields",
        \\Never : []
        \\
        \\main! = || {
        \\    x : { ok : I64, never : Try(I64, Never) }
        \\    x = { ok: 42, never: Ok(1) }
        \\    Stdout.line!(Str.inspect(x))
        \\}
    , "{ never: Ok(1), ok: 42 }\n"),
    runDevBodyCase(170, "B170", "Str.inspect handles uninhabited error unions in lists",
        \\Never : []
        \\
        \\main! = || {
        \\    xs : List(Try(I64, Never))
        \\    xs = [Ok(42)]
        \\    Stdout.line!(Str.inspect(xs))
        \\}
    , "[Ok(42)]\n"),
    runDevBodyCase(171, "B171", "Str.inspect handles boxed uninhabited error unions",
        \\Never : []
        \\
        \\main! = || {
        \\    x : Box(Try(I64, Never))
        \\    x = Box.box(Ok(42))
        \\    Stdout.line!(Str.inspect(Box.unbox(x)))
        \\}
    , "Ok(42)\n"),
    runDevOpenBodyCase(172, "B172", "open-error record payloads are released with the right alignment",
        \\main! = |_| Err(CustomError({ msg: "payload that allocates outside small string storage" }))
    ),
    runDevOpenBodyCase(173, "B173", "open-error list payloads are released with the right alignment",
        \\main! = |_| Err(CustomError(["payload that allocates outside small string storage"]))
    ),
    runDevOpenBodyCase(174, "B174", "open-error boxed payloads are released with the right alignment",
        \\main! = |_| Err(CustomError(Box.box("payload that allocates outside small string storage")))
    ),
    runDevOpenBodyCase(175, "B175", "open-error recursive payloads are released with the right alignment",
        \\Tree := [Leaf(Str), Node(Box(Tree), Box(Tree))]
        \\
        \\main! = |_| Err(CustomError(Node(Box.box(Leaf("left payload that allocates")), Box.box(Leaf("right payload that allocates")))))
    ),
    runDevOpenBodyCase(176, "B176", "open-error custom branches are released with the right alignment",
        \\main! = |args| match List.len(args) {
        \\    0 => Err(First("payload that allocates outside small string storage"))
        \\    _ => Err(Second("payload that allocates outside small string storage"))
        \\}
    ),
    testInterpBodyCase(177, "B177", "roc test handles hosted effects returning strings in expect blocks",
        \\import pf.Host
        \\
        \\expect {
        \\    host = Host.new("World")
        \\    _ = host.get_greeting!()
        \\    True
        \\}
        \\
        \\main! = || {}
    , .success_or_clean_failure),
    testInterpBodyCase(178, "B178", "roc test handles qualified Bool tags in patterns",
        \\expect match Bool.True {
        \\    Bool.True => Bool.True
        \\    Bool.False => Bool.False
        \\}
        \\
        \\main! = || {}
    , .success),
    testInterpBodyCase(179, "B179", "roc test treats block expects returning Bool.True as passing",
        \\expect {
        \\    Bool.True
        \\}
        \\
        \\main! = || {}
    , .success),
    checkBodyCase(180, "B180", "record extension aliases reject non-record extension arguments",
        \\R(x) : { a : I64, ..x }
        \\
        \\f : R(I64) -> I64
        \\f = |r| r.a
        \\
        \\main! = || {}
    ),
    checkBodyCase(181, "B181", "tag-union extension aliases reject non-union extension arguments",
        \\T(x) : [A, ..x]
        \\
        \\f : T(I64) -> I64
        \\f = |_| 1
        \\
        \\main! = || {}
    ),
    .{
        .id = 182,
        .bug_id = "B182",
        .name = "bughunt B182: recursive nominal definitions are accepted",
        .files = &.{.{ .path = "main.roc", .contents =
        \\app [main!] { pf: platform "{FX_PLATFORM}" }
        \\
        \\import pf.Stdout
        \\
        \\Tree := [Node(Tree)]
        \\
        \\main! = || Stdout.line!("ok")
        }},
        .command = .check,
        .expect = .success,
    },
    checkBodyCase(183, "B183", "or-pattern alternatives must bind the same names",
        \\main! = || {
        \\    _ = match Ok(1) {
        \\        Ok(x) | Err(_) => x
        \\    }
        \\    {}
        \\}
    ),
    checkBodyCase(184, "B184", "record extension aliases reject duplicate fields from extension arguments",
        \\Ext : { a : Str }
        \\R(x) : { a : I64, ..x }
        \\
        \\f : R(Ext) -> I64
        \\f = |r| r.a
        \\
        \\main! = || {}
    ),
    checkBodyCase(185, "B185", "tag-union extension aliases reject duplicate tags from extension arguments",
        \\Ext : [A]
        \\T(x) : [A, ..x]
        \\
        \\f : T(Ext) -> I64
        \\f = |_| 1
        \\
        \\main! = || {}
    ),
    runDevBodyCase(186, "B186", "boxed generic functions inside tag payloads keep Box provenance",
        \\Value(a) := [F((a -> a))]
        \\identity = |x| x
        \\v : Value(I64)
        \\v = F(Box.unbox(Box.box(identity)))
        \\
        \\main! = || {
        \\    out = match v {
        \\        F(f) => f(42.I64)
        \\    }
        \\    Stdout.line!(I64.to_str(out))
        \\}
    , "42\n"),
    runDevTwoFileCase(187, "B187", "imported boxed callable tag payloads keep Box provenance when unboxed",
        \\import A
        \\
        \\main! = || {
        \\    out = match A.v {
        \\        A.Value.F(boxed) => Box.unbox(boxed)(41)
        \\    }
        \\    Stdout.line!(I64.to_str(out))
        \\}
    ,
        \\A :: [].{
        \\    Value := [F(Box((I64 -> I64)))]
        \\    v : Value
        \\    v = F(Box.box(|x| x + 1.I64))
        \\}
    , "42\n"),
    runDevTwoFileCase(188, "B188", "qualified imported record field access works for primitive fields",
        \\import A
        \\
        \\main! = || Stdout.line!(I64.to_str(A.r.n))
    ,
        \\A :: [].{
        \\    r = { n: 42.I64 }
        \\}
    , "42\n"),
    runDevTwoFileCase(189, "B189", "qualified imported record field access works for nested fields",
        \\import A
        \\
        \\main! = || Stdout.line!(I64.to_str(A.r.inner.n))
    ,
        \\A :: [].{
        \\    r = { inner: { n: 42.I64 } }
        \\}
    , "42\n"),
    runDevTwoFileCase(190, "B190", "qualified imported record field access works for boxed callable fields",
        \\import A
        \\
        \\main! = || {
        \\    f = Box.unbox(A.r.boxed)
        \\    Stdout.line!(I64.to_str(f(41.I64)))
        \\}
    ,
        \\A :: [].{
        \\    r = { boxed: Box.box(|x| x + 1.I64) }
        \\}
    , "42\n"),
    runDevBodyCase(191, "B191", "nominal minus operators use attached methods",
        \\Thing := [Thing(I64)].{
        \\    minus : Thing, Thing -> Thing
        \\    minus = |_, _| Thing(42)
        \\}
        \\
        \\main! = || {
        \\    a : Thing
        \\    a = Thing(10)
        \\    b : Thing
        \\    b = Thing(3)
        \\    Stdout.line!(Str.inspect(a - b))
        \\}
    , "Thing(42)\n"),
    runDevTwoFileCase(192, "B192", "imported nominal minus operators use attached methods",
        \\import A
        \\
        \\main! = || Stdout.line!(Str.inspect(A.a - A.b))
    ,
        \\A :: [].{
        \\    Thing := [Thing(I64)].{
        \\        minus : Thing, Thing -> Thing
        \\        minus = |_, _| Thing(42)
        \\    }
        \\
        \\    a : Thing
        \\    a = Thing(10)
        \\
        \\    b : Thing
        \\    b = Thing(3)
        \\}
    , "Thing(42)\n"),
    runDevBodyCase(193, "B193", "nominal times operators use attached methods",
        \\Thing := [Thing(I64)].{
        \\    times : Thing, Thing -> Thing
        \\    times = |_, _| Thing(42)
        \\}
        \\
        \\main! = || {
        \\    a : Thing
        \\    a = Thing(10)
        \\    b : Thing
        \\    b = Thing(3)
        \\    Stdout.line!(Str.inspect(a * b))
        \\}
    , "Thing(42)\n"),
    runDevTwoFileCase(194, "B194", "imported nominal times operators use attached methods",
        \\import A
        \\
        \\main! = || Stdout.line!(Str.inspect(A.a * A.b))
    ,
        \\A :: [].{
        \\    Thing := [Thing(I64)].{
        \\        times : Thing, Thing -> Thing
        \\        times = |_, _| Thing(42)
        \\    }
        \\
        \\    a : Thing
        \\    a = Thing(10)
        \\
        \\    b : Thing
        \\    b = Thing(3)
        \\}
    , "Thing(42)\n"),
    runDevBodyCase(195, "B195", "nominal slash operators use attached div_by methods",
        \\Thing := [Thing(I64)].{
        \\    div_by : Thing, Thing -> Thing
        \\    div_by = |_, _| Thing(42)
        \\}
        \\
        \\main! = || {
        \\    a : Thing
        \\    a = Thing(10)
        \\    b : Thing
        \\    b = Thing(3)
        \\    Stdout.line!(Str.inspect(a / b))
        \\}
    , "Thing(42)\n"),
    runDevTwoFileCase(196, "B196", "imported nominal slash operators use attached div_by methods",
        \\import A
        \\
        \\main! = || Stdout.line!(Str.inspect(A.a / A.b))
    ,
        \\A :: [].{
        \\    Thing := [Thing(I64)].{
        \\        div_by : Thing, Thing -> Thing
        \\        div_by = |_, _| Thing(42)
        \\    }
        \\
        \\    a : Thing
        \\    a = Thing(10)
        \\
        \\    b : Thing
        \\    b = Thing(3)
        \\}
    , "Thing(42)\n"),
    runDevBodyCase(197, "B197", "nominal integer-division operators use attached div_trunc_by methods",
        \\Thing := [Thing(I64)].{
        \\    div_trunc_by : Thing, Thing -> Thing
        \\    div_trunc_by = |_, _| Thing(42)
        \\}
        \\
        \\main! = || {
        \\    a : Thing
        \\    a = Thing(10)
        \\    b : Thing
        \\    b = Thing(3)
        \\    Stdout.line!(Str.inspect(a // b))
        \\}
    , "Thing(42)\n"),
    runDevTwoFileCase(198, "B198", "imported nominal integer-division operators use attached div_trunc_by methods",
        \\import A
        \\
        \\main! = || Stdout.line!(Str.inspect(A.a // A.b))
    ,
        \\A :: [].{
        \\    Thing := [Thing(I64)].{
        \\        div_trunc_by : Thing, Thing -> Thing
        \\        div_trunc_by = |_, _| Thing(42)
        \\    }
        \\
        \\    a : Thing
        \\    a = Thing(10)
        \\
        \\    b : Thing
        \\    b = Thing(3)
        \\}
    , "Thing(42)\n"),
    runDevBodyCase(199, "B199", "nominal remainder operators use attached rem_by methods",
        \\Thing := [Thing(I64)].{
        \\    rem_by : Thing, Thing -> Thing
        \\    rem_by = |_, _| Thing(42)
        \\}
        \\
        \\main! = || {
        \\    a : Thing
        \\    a = Thing(10)
        \\    b : Thing
        \\    b = Thing(3)
        \\    Stdout.line!(Str.inspect(a % b))
        \\}
    , "Thing(42)\n"),
    runDevTwoFileCase(200, "B200", "imported nominal remainder operators use attached rem_by methods",
        \\import A
        \\
        \\main! = || Stdout.line!(Str.inspect(A.a % A.b))
    ,
        \\A :: [].{
        \\    Thing := [Thing(I64)].{
        \\        rem_by : Thing, Thing -> Thing
        \\        rem_by = |_, _| Thing(42)
        \\    }
        \\
        \\    a : Thing
        \\    a = Thing(10)
        \\
        \\    b : Thing
        \\    b = Thing(3)
        \\}
    , "Thing(42)\n"),
    runDevBodyCase(201, "B201", "nominal less-than operators use attached is_lt methods",
        \\Thing := [Thing(I64)].{
        \\    is_lt : Thing, Thing -> Bool
        \\    is_lt = |_, _| Bool.True
        \\}
        \\
        \\main! = || {
        \\    a : Thing
        \\    a = Thing(10)
        \\    b : Thing
        \\    b = Thing(3)
        \\    Stdout.line!(Str.inspect(a < b))
        \\}
    , "True\n"),
    runDevTwoFileCase(202, "B202", "imported nominal less-than operators use attached is_lt methods",
        \\import A
        \\
        \\main! = || Stdout.line!(Str.inspect(A.a < A.b))
    ,
        \\A :: [].{
        \\    Thing := [Thing(I64)].{
        \\        is_lt : Thing, Thing -> Bool
        \\        is_lt = |_, _| Bool.True
        \\    }
        \\
        \\    a : Thing
        \\    a = Thing(10)
        \\
        \\    b : Thing
        \\    b = Thing(3)
        \\}
    , "True\n"),
    runDevBodyCase(203, "B203", "nominal less-than-or-equal operators use attached is_lte methods",
        \\Thing := [Thing(I64)].{
        \\    is_lte : Thing, Thing -> Bool
        \\    is_lte = |_, _| Bool.True
        \\}
        \\
        \\main! = || {
        \\    a : Thing
        \\    a = Thing(10)
        \\    b : Thing
        \\    b = Thing(3)
        \\    Stdout.line!(Str.inspect(a <= b))
        \\}
    , "True\n"),
    runDevTwoFileCase(204, "B204", "imported nominal less-than-or-equal operators use attached is_lte methods",
        \\import A
        \\
        \\main! = || Stdout.line!(Str.inspect(A.a <= A.b))
    ,
        \\A :: [].{
        \\    Thing := [Thing(I64)].{
        \\        is_lte : Thing, Thing -> Bool
        \\        is_lte = |_, _| Bool.True
        \\    }
        \\
        \\    a : Thing
        \\    a = Thing(10)
        \\
        \\    b : Thing
        \\    b = Thing(3)
        \\}
    , "True\n"),
    runDevBodyCase(205, "B205", "nominal greater-than operators use attached is_gt methods",
        \\Thing := [Thing(I64)].{
        \\    is_gt : Thing, Thing -> Bool
        \\    is_gt = |_, _| Bool.False
        \\}
        \\
        \\main! = || {
        \\    a : Thing
        \\    a = Thing(10)
        \\    b : Thing
        \\    b = Thing(3)
        \\    Stdout.line!(Str.inspect(a > b))
        \\}
    , "False\n"),
    runDevTwoFileCase(206, "B206", "imported nominal greater-than operators use attached is_gt methods",
        \\import A
        \\
        \\main! = || Stdout.line!(Str.inspect(A.a > A.b))
    ,
        \\A :: [].{
        \\    Thing := [Thing(I64)].{
        \\        is_gt : Thing, Thing -> Bool
        \\        is_gt = |_, _| Bool.False
        \\    }
        \\
        \\    a : Thing
        \\    a = Thing(10)
        \\
        \\    b : Thing
        \\    b = Thing(3)
        \\}
    , "False\n"),
    runDevBodyCase(207, "B207", "nominal greater-than-or-equal operators use attached is_gte methods",
        \\Thing := [Thing(I64)].{
        \\    is_gte : Thing, Thing -> Bool
        \\    is_gte = |_, _| Bool.False
        \\}
        \\
        \\main! = || {
        \\    a : Thing
        \\    a = Thing(10)
        \\    b : Thing
        \\    b = Thing(3)
        \\    Stdout.line!(Str.inspect(a >= b))
        \\}
    , "False\n"),
    runDevTwoFileCase(208, "B208", "imported nominal greater-than-or-equal operators use attached is_gte methods",
        \\import A
        \\
        \\main! = || Stdout.line!(Str.inspect(A.a >= A.b))
    ,
        \\A :: [].{
        \\    Thing := [Thing(I64)].{
        \\        is_gte : Thing, Thing -> Bool
        \\        is_gte = |_, _| Bool.False
        \\    }
        \\
        \\    a : Thing
        \\    a = Thing(10)
        \\
        \\    b : Thing
        \\    b = Thing(3)
        \\}
    , "False\n"),
    runDevBodyCase(209, "B209", "nominal unary negation uses attached negate methods",
        \\Thing := [Thing(I64)].{
        \\    negate : Thing -> Thing
        \\    negate = |_| Thing(42)
        \\}
        \\
        \\main! = || {
        \\    a : Thing
        \\    a = Thing(1)
        \\    Stdout.line!(Str.inspect(-a))
        \\}
    , "Thing(42)\n"),
    runDevTwoFileCase(210, "B210", "imported nominal unary negation uses attached negate methods",
        \\import A
        \\
        \\main! = || Stdout.line!(Str.inspect(-A.a))
    ,
        \\A :: [].{
        \\    Thing := [Thing(I64)].{
        \\        negate : Thing -> Thing
        \\        negate = |_| Thing(42)
        \\    }
        \\
        \\    a : Thing
        \\    a = Thing(1)
        \\}
    , "Thing(42)\n"),
    runDevThreeFileCase(211, "B211", "transitive imported attached methods are available through qualified values",
        \\import A
        \\
        \\main! = || Stdout.line!(A.x.show())
    ,
        \\import B
        \\
        \\A :: [].{
        \\    x : B.Id
        \\    x = B.x
        \\}
    ,
        \\B :: [].{
        \\    Id := [Id(I64)].{
        \\        show : Id -> Str
        \\        show = |v| match v {
        \\            Id(n) => I64.to_str(n)
        \\        }
        \\    }
        \\
        \\    x : Id
        \\    x = Id(42)
        \\}
    , "42\n"),
};

fn currentProcessIdForFilename() u64 {
    if (comptime builtin.os.tag == .windows) {
        return std.os.windows.GetCurrentProcessId();
    }
    return @intCast(std.c.getpid());
}

fn pathJoin(gpa: Allocator, parts: []const []const u8) ![]u8 {
    return std.fs.path.join(gpa, parts);
}

fn appendReplacing(
    out: *std.ArrayListUnmanaged(u8),
    gpa: Allocator,
    input: []const u8,
    needle: []const u8,
    replacement: []const u8,
) !void {
    var rest = input;
    while (std.mem.find(u8, rest, needle)) |idx| {
        try out.appendSlice(gpa, rest[0..idx]);
        try out.appendSlice(gpa, replacement);
        rest = rest[idx + needle.len ..];
    }
    try out.appendSlice(gpa, rest);
}

fn generateTooManyExportsApp(gpa: Allocator, fx_platform: []const u8) ![]const u8 {
    var out: std.ArrayListUnmanaged(u8) = .empty;
    try out.appendSlice(gpa, "app [");
    for (0..65535) |i| {
        if (i > 0) try out.appendSlice(gpa, ", ");
        const name = try std.fmt.allocPrint(gpa, "x{d}", .{i});
        defer gpa.free(name);
        try out.appendSlice(gpa, name);
    }
    try out.appendSlice(gpa, "] { pf: platform \"");
    try out.appendSlice(gpa, fx_platform);
    try out.appendSlice(gpa, "\" }\n\nmain! = || {}\n");
    return try out.toOwnedSlice(gpa);
}

fn generateDeepConcatApp(gpa: Allocator, fx_platform: []const u8) ![]const u8 {
    var out: std.ArrayListUnmanaged(u8) = .empty;
    try out.appendSlice(gpa, "app [main!] { pf: platform \"");
    try out.appendSlice(gpa, fx_platform);
    try out.appendSlice(gpa, "\" }\n\nimport pf.Stdout\n\nmain! = || Stdout.line!(\"\"");
    for (0..480) |_| {
        try out.appendSlice(gpa, ".concat(\"x\")");
    }
    try out.appendSlice(gpa, ")\n");
    return try out.toOwnedSlice(gpa);
}

fn generateWideRecordInspectApp(gpa: Allocator, fx_platform: []const u8) ![]const u8 {
    var out: std.ArrayListUnmanaged(u8) = .empty;
    try out.appendSlice(gpa, "app [main!] { pf: platform \"");
    try out.appendSlice(gpa, fx_platform);
    try out.appendSlice(gpa, "\" }\n\nimport pf.Stdout\n\nmain! = || {\n    r = { ");
    for (0..112) |i| {
        if (i > 0) try out.appendSlice(gpa, ", ");
        const field = try std.fmt.allocPrint(gpa, "a{d}: {d}.I64", .{ i, i });
        defer gpa.free(field);
        try out.appendSlice(gpa, field);
    }
    try out.appendSlice(gpa, " }\n    Stdout.line!(Str.inspect(r))\n}\n");
    return try out.toOwnedSlice(gpa);
}

fn renderSource(
    gpa: Allocator,
    input: []const u8,
    fx_platform: []const u8,
    str_platform: []const u8,
    glue_platform: []const u8,
    fx_open_platform: []const u8,
) ![]const u8 {
    if (std.mem.eql(u8, input, "{GENERATE_TOO_MANY_EXPORTS_APP}")) {
        return generateTooManyExportsApp(gpa, fx_platform);
    }
    if (std.mem.eql(u8, input, "{GENERATE_DEEP_CONCAT_APP}")) {
        return generateDeepConcatApp(gpa, fx_platform);
    }
    if (std.mem.eql(u8, input, "{GENERATE_WIDE_RECORD_INSPECT_APP}")) {
        return generateWideRecordInspectApp(gpa, fx_platform);
    }

    var stage1: std.ArrayListUnmanaged(u8) = .empty;
    defer stage1.deinit(gpa);
    try appendReplacing(&stage1, gpa, input, "{FX_PLATFORM}", fx_platform);

    var stage2: std.ArrayListUnmanaged(u8) = .empty;
    defer stage2.deinit(gpa);
    try appendReplacing(&stage2, gpa, stage1.items, "{STR_PLATFORM}", str_platform);

    var stage3: std.ArrayListUnmanaged(u8) = .empty;
    defer stage3.deinit(gpa);
    try appendReplacing(&stage3, gpa, stage2.items, "{GLUE_PLATFORM}", glue_platform);

    var stage4: std.ArrayListUnmanaged(u8) = .empty;
    try appendReplacing(&stage4, gpa, stage3.items, "{FX_OPEN_PLATFORM}", fx_open_platform);
    return try stage4.toOwnedSlice(gpa);
}

fn writeFiles(io: std.Io, gpa: Allocator, spec: CliBugSpec, test_dir: []const u8) !void {
    const fx_platform = "../../../test/fx/platform/main.roc";
    const str_platform = "../../../test/str/platform/main.roc";
    const glue_platform = "../../../src/glue/platform/main.roc";
    const fx_open_platform = "../../../test/fx-open/platform/main.roc";

    for (spec.files) |file| {
        const rendered = try renderSource(gpa, file.contents, fx_platform, str_platform, glue_platform, fx_open_platform);
        const full_path = try pathJoin(gpa, &.{ test_dir, file.path });
        defer gpa.free(full_path);
        if (std.fs.path.dirname(full_path)) |dir| {
            try std.Io.Dir.cwd().createDirPath(io, dir);
        }
        try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = full_path, .data = rendered });
    }
}

fn buildArgv(gpa: Allocator, spec: CliBugSpec, main_path: []const u8, test_dir: []const u8, repo_root: []const u8) ![]const []const u8 {
    switch (spec.command) {
        .check => return gpa.dupe([]const u8, &.{ roc_binary_path, "check", "--no-cache", main_path }),
        .docs => return gpa.dupe([]const u8, &.{ roc_binary_path, "docs", main_path }),
        .run_dev => return gpa.dupe([]const u8, &.{ roc_binary_path, "--opt=dev", "--no-cache", main_path }),
        .run_interpreter => return gpa.dupe([]const u8, &.{ roc_binary_path, "--opt=interpreter", "--no-cache", main_path }),
        .run_dev_allow_errors => return gpa.dupe([]const u8, &.{ roc_binary_path, "--opt=dev", "--allow-errors", "--no-cache", main_path }),
        .test_dev => return gpa.dupe([]const u8, &.{ roc_binary_path, "test", "--opt=dev", "--no-cache", main_path }),
        .test_interpreter => return gpa.dupe([]const u8, &.{ roc_binary_path, "test", "--opt=interpreter", "--no-cache", main_path }),
        .glue => {
            const out_dir = try pathJoin(gpa, &.{ test_dir, "glue-out" });
            const glue_platform = try pathJoin(gpa, &.{ repo_root, "src", "glue", "platform", "main.roc" });
            return gpa.dupe([]const u8, &.{ roc_binary_path, "glue", main_path, out_dir, glue_platform });
        },
    }
}

fn isCleanUserFailure(result: std.process.RunResult) bool {
    const exited_nonzero = switch (result.term) {
        .exited => |code| code != 0,
        else => return false,
    };
    if (!exited_nonzero) return false;

    const combined_has_internal =
        std.mem.find(u8, result.stderr, "Internal compiler error") != null or
        std.mem.find(u8, result.stderr, "thread ") != null or
        std.mem.find(u8, result.stderr, "panic:") != null or
        std.mem.find(u8, result.stderr, "SIGABRT") != null or
        std.mem.find(u8, result.stderr, "Compiler bug") != null or
        std.mem.find(u8, result.stdout, "Internal compiler error") != null or
        std.mem.find(u8, result.stdout, "panic:") != null;
    return !combined_has_internal;
}

fn hasMemoryErrors(stderr: []const u8) ?[]const u8 {
    if (std.mem.find(u8, stderr, "error(gpa):") != null) return "memory error detected";
    if (std.mem.find(u8, stderr, "allocation(s) not freed") != null) return "memory leak detected";
    return null;
}

fn exitCode(term: std.process.Child.Term) u32 {
    return switch (term) {
        .exited => |code| @intCast(code),
        .signal => |sig| @as(u32, @intFromEnum(sig)) | 0x80000000,
        else => 0xFFFFFFFF,
    };
}

fn mismatchMessage(gpa: Allocator, prefix: []const u8, expected: []const u8, actual: []const u8) []const u8 {
    return std.fmt.allocPrint(gpa, "{s}: expected '{s}', got '{s}'", .{ prefix, expected, actual }) catch prefix;
}

fn evaluateResult(gpa: Allocator, expected: Expect, result: std.process.RunResult, timer: *harness.Timer, setup_ns: u64) TestResult {
    const duration_ns = timer.read();
    const command_ns = duration_ns -| setup_ns;

    if (hasMemoryErrors(result.stderr)) |msg| {
        return .{
            .status = .fail,
            .duration_ns = duration_ns,
            .setup_ns = setup_ns,
            .command_ns = command_ns,
            .exit_code = exitCode(result.term),
            .stderr_capture = result.stderr,
            .stdout_capture = result.stdout,
            .message = msg,
        };
    }

    switch (result.term) {
        .signal => {
            return .{
                .status = .crash,
                .duration_ns = duration_ns,
                .setup_ns = setup_ns,
                .command_ns = command_ns,
                .exit_code = exitCode(result.term),
                .stderr_capture = result.stderr,
                .stdout_capture = result.stdout,
                .message = "roc process crashed",
            };
        },
        else => {},
    }

    switch (expected) {
        .success => {
            const ok = switch (result.term) {
                .exited => |code| code == 0,
                else => false,
            };
            if (ok) return .{ .status = .pass, .duration_ns = duration_ns, .setup_ns = setup_ns, .command_ns = command_ns };
            return .{
                .status = .fail,
                .duration_ns = duration_ns,
                .setup_ns = setup_ns,
                .command_ns = command_ns,
                .exit_code = exitCode(result.term),
                .stderr_capture = result.stderr,
                .stdout_capture = result.stdout,
                .message = "expected successful command",
            };
        },
        .success_stdout_exact => |expected_stdout| {
            const ok = switch (result.term) {
                .exited => |code| code == 0 and std.mem.eql(u8, result.stdout, expected_stdout),
                else => false,
            };
            if (ok) return .{ .status = .pass, .duration_ns = duration_ns, .setup_ns = setup_ns, .command_ns = command_ns };

            const msg = if (switch (result.term) {
                .exited => |code| code == 0,
                else => false,
            })
                mismatchMessage(gpa, "stdout mismatch", expected_stdout, result.stdout)
            else
                "expected successful command";

            return .{
                .status = .fail,
                .duration_ns = duration_ns,
                .setup_ns = setup_ns,
                .command_ns = command_ns,
                .exit_code = exitCode(result.term),
                .stderr_capture = result.stderr,
                .stdout_capture = result.stdout,
                .message = msg,
            };
        },
        .warning_stdout_exact => |expected_stdout| {
            const ok = switch (result.term) {
                .exited => |code| code == 2 and std.mem.eql(u8, result.stdout, expected_stdout),
                else => false,
            };
            if (ok) return .{ .status = .pass, .duration_ns = duration_ns, .setup_ns = setup_ns, .command_ns = command_ns };

            const msg = if (switch (result.term) {
                .exited => |code| code == 2,
                else => false,
            })
                mismatchMessage(gpa, "stdout mismatch", expected_stdout, result.stdout)
            else
                "expected warning-only command";

            return .{
                .status = .fail,
                .duration_ns = duration_ns,
                .setup_ns = setup_ns,
                .command_ns = command_ns,
                .exit_code = exitCode(result.term),
                .stderr_capture = result.stderr,
                .stdout_capture = result.stdout,
                .message = msg,
            };
        },
        .clean_failure => {
            if (isCleanUserFailure(result)) return .{ .status = .pass, .duration_ns = duration_ns, .setup_ns = setup_ns, .command_ns = command_ns };
            return .{
                .status = .fail,
                .duration_ns = duration_ns,
                .setup_ns = setup_ns,
                .command_ns = command_ns,
                .exit_code = exitCode(result.term),
                .stderr_capture = result.stderr,
                .stdout_capture = result.stdout,
                .message = "expected a clean user-facing failure",
            };
        },
        .success_or_clean_failure => {
            const ok = switch (result.term) {
                .exited => |code| code == 0,
                else => false,
            };
            if (ok or isCleanUserFailure(result)) return .{ .status = .pass, .duration_ns = duration_ns, .setup_ns = setup_ns, .command_ns = command_ns };
            return .{
                .status = .fail,
                .duration_ns = duration_ns,
                .setup_ns = setup_ns,
                .command_ns = command_ns,
                .exit_code = exitCode(result.term),
                .stderr_capture = result.stderr,
                .stdout_capture = result.stdout,
                .message = "expected success or a clean user-facing failure",
            };
        },
    }
}

fn runSingleTest(io: std.Io, gpa: Allocator, spec: CliBugSpec, timeout_ms: u64) TestResult {
    var timer = harness.Timer.start() catch return .{ .status = .crash, .message = "no clock" };

    const repo_root = std.Io.Dir.cwd().realPathFileAlloc(io, ".", gpa) catch
        return .{ .status = .crash, .message = "failed to resolve repo root" };

    const cache_dirs = util.createIsolatedTestCacheDirs(io, gpa) catch
        return .{ .status = .crash, .message = "failed to create cache dirs" };
    defer cache_dirs.deinit(gpa);

    const test_dir = std.fmt.allocPrint(gpa, ".zig-cache/bughunt-cli/{d}_{d}", .{
        currentProcessIdForFilename(),
        spec.id,
    }) catch return .{ .status = .crash, .message = "OOM" };
    std.Io.Dir.cwd().deleteTree(io, test_dir) catch {};
    std.Io.Dir.cwd().createDirPath(io, test_dir) catch |err| {
        const msg = std.fmt.allocPrint(gpa, "failed to create temp dir: {}", .{err}) catch "failed to create temp dir";
        return .{ .status = .crash, .duration_ns = timer.read(), .message = msg };
    };

    writeFiles(io, gpa, spec, test_dir) catch |err| {
        const msg = std.fmt.allocPrint(gpa, "failed to write source files: {}", .{err}) catch "failed to write source files";
        return .{ .status = .crash, .duration_ns = timer.read(), .message = msg };
    };

    const main_path = pathJoin(gpa, &.{ test_dir, spec.main_file }) catch
        return .{ .status = .crash, .duration_ns = timer.read(), .message = "failed to build main path" };

    const argv = buildArgv(gpa, spec, main_path, test_dir, repo_root) catch
        return .{ .status = .crash, .duration_ns = timer.read(), .message = "failed to build argv" };

    var env_map = util.buildIsolatedTestEnvMap(io, gpa, null) catch
        return .{ .status = .crash, .duration_ns = timer.read(), .message = "failed to get env" };
    defer env_map.deinit();
    env_map.put("ROC_CACHE_DIR", cache_dirs.roc_cache_dir) catch
        return .{ .status = .crash, .duration_ns = timer.read(), .message = "failed to set roc cache env" };
    env_map.put("ZIG_LOCAL_CACHE_DIR", cache_dirs.zig_local_cache_dir) catch
        return .{ .status = .crash, .duration_ns = timer.read(), .message = "failed to set zig cache env" };

    const setup_ns = timer.read();
    const child_result = util.runChildWithTimeout(io, gpa, argv, .{
        .cwd = repo_root,
        .env_map = &env_map,
        .max_output_bytes = 2 * 1024 * 1024,
        .timeout_ms = timeout_ms,
    }) catch |err| {
        const msg = std.fmt.allocPrint(gpa, "spawn error: {}", .{err}) catch "spawn error";
        const duration_ns = timer.read();
        return .{ .status = .fail, .duration_ns = duration_ns, .setup_ns = setup_ns, .command_ns = duration_ns -| setup_ns, .message = msg };
    };

    return evaluateResult(gpa, spec.expect, child_result, &timer, setup_ns);
}

fn serializeResult(fd: posix.fd_t, result: TestResult) void {
    const stderr_data = result.stderr_capture orelse "";
    const stdout_data = result.stdout_capture orelse "";
    const message_data = result.message orelse "";

    const max_capture = 8192;
    const stderr_out = stderr_data[0..@min(stderr_data.len, max_capture)];
    const stdout_out = stdout_data[0..@min(stdout_data.len, max_capture)];
    const message_out = message_data[0..@min(message_data.len, max_capture)];

    const header = WireHeader{
        .status = @intFromEnum(result.status),
        .duration_ns = result.duration_ns,
        .setup_ns = result.setup_ns,
        .command_ns = result.command_ns,
        .exit_code = result.exit_code,
        .stderr_len = @intCast(stderr_out.len),
        .stdout_len = @intCast(stdout_out.len),
        .message_len = @intCast(message_out.len),
    };

    harness.writeAll(fd, std.mem.asBytes(&header));
    harness.writeAll(fd, stderr_out);
    harness.writeAll(fd, stdout_out);
    harness.writeAll(fd, message_out);
}

fn deserializeResult(buf: []const u8, gpa: Allocator) ?TestResult {
    if (buf.len < @sizeOf(WireHeader)) return null;

    const header: *const WireHeader = @ptrCast(@alignCast(buf.ptr));
    var offset: usize = @sizeOf(WireHeader);

    return .{
        .status = @enumFromInt(header.status),
        .duration_ns = header.duration_ns,
        .setup_ns = header.setup_ns,
        .command_ns = header.command_ns,
        .exit_code = header.exit_code,
        .stderr_capture = harness.readStr(buf, &offset, header.stderr_len, gpa),
        .stdout_capture = harness.readStr(buf, &offset, header.stdout_len, gpa),
        .message = harness.readStr(buf, &offset, header.message_len, gpa),
    };
}

fn dupeOptional(gpa: Allocator, value: ?[]const u8) ?[]const u8 {
    return if (value) |slice| gpa.dupe(u8, slice) catch null else null;
}

fn stabilizeResult(gpa: Allocator, result: TestResult) TestResult {
    return .{
        .status = result.status,
        .duration_ns = result.duration_ns,
        .setup_ns = result.setup_ns,
        .command_ns = result.command_ns,
        .exit_code = result.exit_code,
        .stderr_capture = dupeOptional(gpa, result.stderr_capture),
        .stdout_capture = dupeOptional(gpa, result.stdout_capture),
        .message = dupeOptional(gpa, result.message),
    };
}

fn getTestName(spec: CliBugSpec) []const u8 {
    return spec.name;
}

const Pool = harness.ProcessPool(CliBugSpec, TestResult, .{
    .runTest = &runSingleTest,
    .serialize = &serializeResult,
    .deserialize = &deserializeResult,
    .default_result = .{ .status = .crash },
    .timeout_result = .{ .status = .timeout },
    .stabilizeResult = &stabilizeResult,
    .getName = &getTestName,
    .use_process_groups = true,
});

fn matchesFilters(spec: CliBugSpec, filters: []const []const u8) bool {
    if (filters.len == 0) return true;
    for (filters) |filter| {
        if (std.mem.find(u8, spec.bug_id, filter) != null) return true;
        if (std.mem.find(u8, spec.name, filter) != null) return true;
    }
    return false;
}

fn filteredTests(gpa: Allocator, filters: []const []const u8) ![]const CliBugSpec {
    var result: std.ArrayListUnmanaged(CliBugSpec) = .empty;
    for (tests) |spec| {
        if (matchesFilters(spec, filters)) try result.append(gpa, spec);
    }
    return try result.toOwnedSlice(gpa);
}

fn printCapturedOutput(label: []const u8, capture: ?[]const u8) void {
    const data = capture orelse return;
    if (data.len == 0) return;
    var lines = std.mem.splitScalar(u8, data, '\n');
    var count: usize = 0;
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        if (count == 0) {
            std.debug.print("        {s}: {s}\n", .{ label, line });
        } else if (count < 5) {
            std.debug.print("        {s}\n", .{line});
        } else {
            std.debug.print("        ... ({s} truncated)\n", .{label});
            break;
        }
        count += 1;
    }
}

fn printResults(specs: []const CliBugSpec, results: []const TestResult, verbose: bool, wall_ns: u64, workers: usize) void {
    var passed: usize = 0;
    var failed: usize = 0;
    var crashed: usize = 0;
    var timed_out: usize = 0;

    for (specs, 0..) |spec, i| {
        const result = results[i];
        const ms = harness.nsToMs(result.duration_ns);
        switch (result.status) {
            .pass => {
                passed += 1;
                if (verbose) std.debug.print("  PASS  {s}  ({d:.1}ms)\n", .{ spec.name, ms });
            },
            .fail => {
                failed += 1;
                std.debug.print("  FAIL  {s}  ({d:.1}ms)\n", .{ spec.name, ms });
                if (result.message) |msg| std.debug.print("        {s}\n", .{msg});
                if (result.exit_code != 0) std.debug.print("        exit 0x{x}\n", .{result.exit_code});
                printCapturedOutput("stderr", result.stderr_capture);
                printCapturedOutput("stdout", result.stdout_capture);
                std.debug.print("        Repro: zig build run-test-cli-bughunt -- --filter {s}\n\n", .{spec.bug_id});
            },
            .crash => {
                crashed += 1;
                std.debug.print("  CRASH {s}  ({d:.1}ms)\n", .{ spec.name, ms });
                if (result.message) |msg| std.debug.print("        {s}\n", .{msg});
                printCapturedOutput("stderr", result.stderr_capture);
                printCapturedOutput("stdout", result.stdout_capture);
                std.debug.print("        Repro: zig build run-test-cli-bughunt -- --filter {s}\n\n", .{spec.bug_id});
            },
            .timeout => {
                timed_out += 1;
                std.debug.print("  HANG  {s}\n", .{spec.name});
                std.debug.print("        Repro: zig build run-test-cli-bughunt -- --filter {s}\n\n", .{spec.bug_id});
            },
        }
    }

    std.debug.print("\n{d} passed, {d} failed, {d} crashed, {d} hung ({d} total) in {d:.0}ms using {d} worker(s)\n", .{
        passed,
        failed,
        crashed,
        timed_out,
        specs.len,
        harness.nsToMs(wall_ns),
        workers,
    });
}

fn statsStatus(status: Status) []const u8 {
    return switch (status) {
        .pass => "pass",
        .fail => "fail",
        .crash => "crash",
        .timeout => "timeout",
    };
}

fn statsSummary(results: []const TestResult) harness.StatsSummary {
    var summary: harness.StatsSummary = .{ .total = results.len };
    for (results) |result| {
        switch (result.status) {
            .pass => summary.passed += 1,
            .fail => summary.failed += 1,
            .crash => summary.crashed += 1,
            .timeout => summary.timed_out += 1,
        }
    }
    return summary;
}

fn maybeStatsData(gpa: Allocator, result: TestResult) []const harness.StatsData {
    if (result.status == .pass) return &.{};

    var count: usize = 0;
    if (result.message != null) count += 1;
    if (result.stderr_capture != null) count += 1;
    if (result.stdout_capture != null) count += 1;
    if (result.exit_code != 0) count += 1;
    if (count == 0) return &.{};

    const data = gpa.alloc(harness.StatsData, count) catch return &.{};
    var next: usize = 0;
    if (result.message) |message| {
        data[next] = .{ .key = "message", .value = message };
        next += 1;
    }
    if (result.stderr_capture) |stderr| {
        data[next] = .{ .key = "stderr", .value = stderr };
        next += 1;
    }
    if (result.stdout_capture) |stdout| {
        data[next] = .{ .key = "stdout", .value = stdout };
        next += 1;
    }
    if (result.exit_code != 0) {
        const exit_text = std.fmt.allocPrint(gpa, "{d}", .{result.exit_code}) catch "unknown";
        data[next] = .{ .key = "exit_code", .value = exit_text };
    }
    return data;
}

fn appendStatsEvent(
    gpa: Allocator,
    events: *std.ArrayListUnmanaged(harness.StatsEvent),
    id: []const u8,
    parent_id: ?[]const u8,
    kind: []const u8,
    name: []const u8,
    status: []const u8,
    start_ns: u64,
    end_ns: u64,
    data: []const harness.StatsData,
) void {
    events.append(gpa, .{
        .id = id,
        .parent_id = parent_id,
        .kind = kind,
        .name = name,
        .status = status,
        .start_ns = start_ns,
        .end_ns = end_ns,
        .data = data,
    }) catch {};
}

fn writeStatsJson(
    gpa: Allocator,
    io: std.Io,
    path: []const u8,
    specs: []const CliBugSpec,
    results: []const TestResult,
) !void {
    var stats_arena = std.heap.ArenaAllocator.init(gpa);
    defer stats_arena.deinit();
    const stats_allocator = stats_arena.allocator();

    var events: std.ArrayListUnmanaged(harness.StatsEvent) = .empty;

    for (specs, results, 0..) |spec, result, i| {
        const case_id = try std.fmt.allocPrint(stats_allocator, "case-{d}", .{i});
        const status = statsStatus(result.status);
        appendStatsEvent(stats_allocator, &events, case_id, null, "case", spec.name, status, 0, result.duration_ns, maybeStatsData(stats_allocator, result));

        if (result.setup_ns > 0) {
            const setup_id = try std.fmt.allocPrint(stats_allocator, "case-{d}-setup", .{i});
            appendStatsEvent(stats_allocator, &events, setup_id, case_id, "setup", "write sources", "pass", 0, result.setup_ns, &.{});
        }
        if (result.command_ns > 0) {
            const command_id = try std.fmt.allocPrint(stats_allocator, "case-{d}-roc-command", .{i});
            appendStatsEvent(stats_allocator, &events, command_id, case_id, "roc command", @tagName(spec.command), status, result.setup_ns, result.setup_ns + result.command_ns, &.{});
        }
    }

    try harness.writeRunnerStatsJson(stats_allocator, io, path, .{
        .runner = "cli-bughunt",
        .summary = statsSummary(results),
        .events = events.items,
    });
}

fn printUsage() void {
    std.debug.print(
        \\Usage: bughunt_cli_repros <roc_binary> [options]
        \\
        \\Options:
        \\  --filter <pattern>   Run tests matching a bug id or name (repeatable)
        \\  --threads <N>        Max concurrent workers (default: CPU count)
        \\  --timeout <ms>       Per-test timeout in ms (default: 60000)
        \\  --verbose            Show PASS results with timing
        \\
    , .{});
}

/// Runs the CLI bughunt repro harness.
pub fn main(init: std.process.Init) !void {
    var gpa_impl: std.heap.DebugAllocator(.{}) = .init;
    defer _ = gpa_impl.deinit();
    const gpa = gpa_impl.allocator();

    var arena_impl = std.heap.ArenaAllocator.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    const args = try harness.parseStandardArgs(arena, init.minimal.args);
    if (args.help_requested) {
        printUsage();
        return;
    }
    if (args.positional.len < 1) {
        printUsage();
        std.process.exit(1);
    }

    roc_binary_path = args.positional[0];

    const specs = try filteredTests(arena, args.filters);
    if (specs.len == 0) return;

    const cpu_count = std.Thread.getCpuCount() catch 4;
    const workers = args.max_threads orelse @min(cpu_count, specs.len);
    const results = try gpa.alloc(TestResult, specs.len);
    defer gpa.free(results);
    @memset(results, .{ .status = .crash });

    std.debug.print("=== Bughunt CLI Repros ===\n", .{});
    std.debug.print("{d} tests, {d} workers, {d}s timeout\n\n", .{ specs.len, workers, args.timeout_ms / 1000 });

    var wall_timer = harness.Timer.start() catch @panic("no clock");
    Pool.run(init.io, specs, results, workers, args.timeout_ms, gpa, null);
    const wall_ns = wall_timer.read();

    printResults(specs, results, args.verbose, wall_ns, workers);

    if (args.stats_json_path) |path| {
        try writeStatsJson(gpa, init.io, path, specs, results);
    }

    for (results) |result| {
        if (result.stderr_capture) |stderr| gpa.free(stderr);
        if (result.stdout_capture) |stdout| gpa.free(stdout);
        if (result.message) |message| gpa.free(message);
    }

    for (results) |result| {
        switch (result.status) {
            .pass => {},
            .fail, .crash, .timeout => std.process.exit(1),
        }
    }
}
