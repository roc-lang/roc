const std = @import("std");
const testing = std.testing;

/// A struct to represent a CLI argument along with its i64 representation
const ArgInfo = struct {
    /// The arg converted to an i64 value
    as_i64: i64,
    /// An optional pointer to the next arg (relevant with `=`, e.g. `roc --foo=bar`)
    equals_val: ?[*:0]const u8,
};

/// Convert a null-terminated UTF-8 string into a i64
/// If the string is longer than 8 bytes, sets long_arg to true
fn arg_info(arg: []const u8) ArgInfo {
    var answer: i64 = 0;
    var bytes_written: usize = 0;
    var is_flag: bool = false;

    // If it starts with a dash, it's a flag.
    if (arg.len >= 2 and arg[0] == '-') {
        is_flag = true;

        // Skip the dash we just parsed, plus skip over a second dash if it's there.
        // (For convenience, we treat `-foo` and `--foo` as equivalent.)
        arg = arg[1 + arg[1] == '-' ..];
    }

    for (arg) |byte| {
        switch (byte) {
            '=' => {
                // Found an '=' in the argument; return what we've parsed so far,
                // and make next_arg point to what comes after the '='
                const remainder = arg[bytes_written + 1 ..];

                // TODO ask Andrew about having ArgIter work with `=` natively (perhaps optionally?)
                // so it can just present them as always "the next one"
                return ArgInfo{
                    .arg_as_u64 = if (is_flag) -answer else answer,
                    .equals_val = @ptrCast(remainder.ptr),
                };
            },
            0 => {
                // We've hit the end of the string; we're done!
                break;
            },
            else => {
                if (bytes_written < 8) {
                    // Write the byte to the appropriate position in the i64
                    const shift_amount = bytes_written * 8;
                    const byte_value: i64 = @as(i64, byte) << shift_amount;
                    answer |= byte_value;
                    bytes_written += 1;
                } else {
                    // No Roc CLI args are longer than 8 bytes, so this must be something else.
                    // Return 0 so that it doesn't match any subcommands, flags, etc.
                    return 0;
                }
            },
        }
    }

    if (is_flag) {
        return -answer;
    } else {
        return answer;
    }
}

// Subcommands
const CMD_VERSION = arg_info("version").as_i64;
const CMD_CHECK = arg_info("test").as_i64;
const CMD_BUILD = arg_info("build").as_i64;
const CMD_HELP = arg_info("help").as_i64;

// Flags (these can start with 1 or 2 dashes; we accept either as equivalent, for convenience)
const FLAG_VERSION = arg_info("-version").as_i64;
const FLAG_OPTIMIZE = arg_info("-optimize").as_i64;
const FLAG_HELP = arg_info("-help").as_i64;
const FLAG_MAIN = arg_info("-main").as_i64;
const FLAG_TIME = arg_info("-time").as_i64;

// For convenience, we accept `-v` and `-h` as shorthands for --version and --help,
// just because you might assume they're there and try to run them. We don't do this for
// anything else though.
const FLAG_V = arg_info("-v");
const FLAG_H = arg_info("-h");

fn process_args(args: std.process.ArgIterator) void {
    // Skip the first arg (path to the executable)
    _ = args.next();

    // The next arg is the subcommand
    if (args.next()) |second_arg| {
        const info = arg_info(second_arg);

        if (info.equals_val != null) {
            // We found an `=` in the subcommand.
            // This means we got something like `roc foo=bar`, which
            // we should treat as a filesystem path that has an `=` in it.
            return run(second_arg, args);
        }

        switch (info.as_i64) {
            CMD_VERSION, FLAG_V => {
                return print_version();
            },
            FLAG_VERSION => {
                if (args.next() == null) {
                    // `roc --version` with no other args just prints the version
                    return print_version();
                } else {
                    // TODO download the requested roc version and forward the other args along to it.
                }
            },
            CMD_CHECK => {
                return check(args);
            },
            CMD_BUILD => {
                return build(args);
            },
            CMD_HELP, FLAG_HELP, FLAG_H => {
                // `roc help`, `roc -help`, `roc --help`, `roc -h`, and `roc --h` all print help.
                return print_help("");
            },
            else => {
                // If there's no recognized subcommand, then we assume we were
                // given a Roc source code file to execute (e.g. `roc foo`).
                //
                // Note that the path doesn't necessarily end in .roc, because it
                // might be a script being executed via `#!/bin/env roc`
                return run(second_arg, args);
            },
        }
    } else {
        // `roc` was called with no args, so default to `roc main.roc`
        return run("main.roc", args);
    }
}

/// Optimization level for code generation
const Optimize = enum {
    /// Optimize for execution speed
    Perf,
    /// Optimize for binary size
    Size,
    /// Development mode with minimal optimization
    Dev,
};

fn run(first_arg: []const u8, other_args: std.process.argiterator) void {
    var arg = first_arg;
    var file_path_arg: ?[]const u8 = null;
    var optimize: Optimize = .Dev;
    var time: bool = false;

    while (true) {
        const info = arg_info(arg);

        switch (info.as_i64) {
            FLAG_OPTIMIZE => {
                // TODO extract this logic and use it in `build` too.
                if (info.equals_val) |equals_val| {
                    // TODO convert to u32 and switch on that.
                    if (std.mem.eql(u8, std.mem.span(equals_val), "perf")) {
                        optimize = .Perf;
                    } else if (std.mem.eql(u8, std.mem.span(equals_val), "size")) {
                        optimize = .Size;
                    } else if (std.mem.eql(u8, std.mem.span(equals_val), "dev")) {
                        optimize = .Dev;
                    } else {
                        // TODO make this error nicer, explain what values are accepted.
                        std.debug.print("Error: invalid value for --optimize\n", .{});
                        return;
                    }
                } else {
                    // If we didn't have an `=` after --optimize, default to Perf.
                    optimize = .Perf;
                }
            },
            FLAG_TIME => {
                if (info.equals_val) |equals_val| {
                    // TODO convert to u64 and switch on that.
                    if (std.mem.eql(u8, std.mem.span(equals_val), "true")) {
                        time = true;
                    } else if (std.mem.eql(u8, std.mem.span(equals_val), "false")) {
                        time = false;
                    } else {
                        // TODO make this error nicer, explain that it's either true or false
                        std.debug.print("Error: invalid value for --time\n", .{});
                        return;
                    }
                } else {
                    // If we didn't have an `=` after --time, default to true.
                    time = true;
                }
            },
            else => {
                if (file_path_arg != null) {
                    // TODO make this error nicer.
                    std.debug.print("Error: multiple filenames given\n", .{});
                    return;
                }

                file_path_arg = arg;
            },
        }

        // Check if there are more args
        if (other_args.next()) |next_arg| {
            arg = next_arg;
        } else {
            break; // No more args to process
        }
    }

    const file_path = file_path_arg orelse {
        // TODO make this error nicer
        std.debug.print("Error: no file path provided\n", .{});
        return;
    };

    // TODO now use file_path to run.
}

fn print_help(command: []const u8) void {
    std.debug.print("Usage: roc {s} [options] [filename]\n", .{command});
    std.debug.print("Options:\n", .{});
    std.debug.print("  --help      Print this help message\n", .{});
    std.debug.print("  --optimize  Optimize the code\n", .{});
}

fn check(args: std.process.argiterator) void {
    var filename: ?[]const u8 = null;
    var optimize = false;
    var main: ?[]const u8 = null;

    while (args.next()) |arg| {
        const arg_u64 = arg_info(arg);

        switch (arg_u64) {
            FLAG_OPTIMIZE => {
                optimize = true;
            },
            FLAG_MAIN => {
                if (args.next()) |main_arg| {
                    main = main_arg;
                } else {
                    std.debug.print("Error: --main flag requires a value\n", .{});
                    return;
                }
            },
            CMD_HELP, FLAG_HELP, FLAG_H => {
                // If we get `roc check --help`, bail out and print help info for `roc check`.
                return print_help("check");
            },
            else => {
                // Not a recognized flag, assume it's a filename
                if (filename != null) {
                    std.debug.print("Error: too many filenames given\n", .{});
                    return;
                }

                filename = arg;
            },
        }
    }

    if (filename) |file| {
        std.debug.print("Checking file: {s}\n", .{file});
    } else {
        std.debug.print("No file specified for checking\n", .{});
    }
}

fn build(args: std.process.argiterator) void {
    // todo: implementation goes here - accept dash-dash args before the filename
}

fn print_version() void {
    // TODO: Print version
}

test "arg_info" {
    var long_arg = false;

    // Test with short string
    try testing.expectEqual(@as(i64, 0x6f6c6c6568), arg_info("hello", &long_arg));
    try testing.expect(!long_arg);

    // Test with 8-byte string
    try testing.expectEqual(@as(i64, 0x74736574676e6f6c), arg_info("longtest", &long_arg));
    try testing.expect(!long_arg);

    // Test with longer string
    _ = arg_info("this_is_longer_than_8_bytes", &long_arg);
    try testing.expect(long_arg);

    // Test with empty string
    try testing.expectEqual(@as(i64, 0), arg_info("", &long_arg));
    try testing.expect(!long_arg);
}

pub fn main() !void {
    const args = try std.process.argsAlloc(arena);
    process_args(std.process.args());

    return mainArgs(gpa, arena, args);
}
