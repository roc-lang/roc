const std = @import("std");
const bytebox = @import("bytebox");
const config = bytebox.config;
const wasi = bytebox.wasi;

const Val = bytebox.Val;
const ValType = bytebox.ValType;
const TraceMode = bytebox.DebugTrace.Mode;

const log = bytebox.Logger.default();

const RunErrors = error{
    IoError,
    MissingFunction,
    FunctionParamMismatch,
    BadFunctionParam,
};

const CmdOpts = struct {
    print_help: bool = false,
    print_version: bool = false,
    print_dump: bool = false,
    trace: TraceMode = .None,

    filename: ?[]const u8 = null,
    invoke: ?InvokeArgs = null,
    invalid_arg: ?[]const u8 = null,
    missing_options: ?[]const u8 = null,

    wasm_argv: ?[]const []const u8 = null,
    wasm_env: ?[]const []const u8 = null,
    wasm_dirs: ?[]const []const u8 = null,
};

const InvokeArgs = struct {
    funcname: []const u8,
    args: []const []const u8,
};

fn isArgvOption(arg: []const u8) bool {
    return arg.len > 0 and arg[0] == '-';
}

fn getArgSafe(index: usize, args: []const []const u8) ?[]const u8 {
    return if (index < args.len) args[index] else null;
}

fn parseCmdOpts(args: []const [:0]const u8, env_buffer: *std.array_list.Managed([]const u8), dir_buffer: *std.array_list.Managed([]const u8)) CmdOpts {
    var opts = CmdOpts{};

    if (args.len < 2) {
        opts.print_help = true;
    }

    var arg_index: usize = 1;
    while (arg_index < args.len) {
        const arg: [:0]const u8 = args[arg_index];

        if (arg_index == 1 and !isArgvOption(arg)) {
            opts.filename = arg;
            opts.wasm_argv = args[1..2];
        } else if (arg_index == 2 and !isArgvOption(arg)) {
            const wasm_argv_begin: usize = arg_index - 1; // include wasm filename
            var wasm_argv_end: usize = arg_index;
            while (wasm_argv_end + 1 < args.len and !isArgvOption(args[wasm_argv_end + 1])) {
                wasm_argv_end += 1;
            }
            opts.wasm_argv = args[wasm_argv_begin .. wasm_argv_end + 1];
            arg_index = wasm_argv_end;
        } else if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
            opts.print_help = true;
        } else if (std.mem.eql(u8, arg, "-v") or std.mem.eql(u8, arg, "--version")) {
            opts.print_version = true;
        } else if (std.mem.eql(u8, arg, "--dump")) {
            if (opts.filename != null) {
                opts.print_dump = true;
            } else {
                opts.missing_options = arg;
            }
        } else if (std.mem.eql(u8, arg, "-i") or std.mem.eql(u8, arg, "--invoke")) {
            arg_index += 1;
            if (arg_index < args.len) {
                opts.invoke = InvokeArgs{
                    .funcname = args[arg_index],
                    .args = args[arg_index + 1 ..],
                };
            } else {
                opts.missing_options = arg;
            }
            arg_index = args.len;
        } else if (std.mem.eql(u8, arg, "-e") or std.mem.eql(u8, arg, "--env")) {
            arg_index += 1;
            if (getArgSafe(arg_index, args)) |env| {
                env_buffer.appendAssumeCapacity(env);
            } else {
                opts.missing_options = arg;
            }
        } else if (std.mem.eql(u8, arg, "-d") or std.mem.eql(u8, arg, "--dir")) {
            arg_index += 1;
            if (getArgSafe(arg_index, args)) |dir| {
                dir_buffer.appendAssumeCapacity(dir);
            } else {
                opts.missing_options = arg;
            }
        } else if (std.mem.eql(u8, arg, "-t") or std.mem.eql(u8, arg, "--trace")) {
            arg_index += 1;
            if (getArgSafe(arg_index, args)) |mode_str| {
                if (bytebox.DebugTrace.parseMode(mode_str)) |mode| {
                    if (config.enable_debug_trace == false) {
                        log.err("Bytebox was not compiled with -Ddebug_trace=true. Enable this compile time flag if you want to enable tracing at runtime.", .{});
                        opts.invalid_arg = mode_str;
                    } else {
                        opts.trace = mode;
                    }
                } else {
                    opts.invalid_arg = mode_str;
                }
            } else {
                opts.missing_options = arg;
            }
        } else {
            opts.invalid_arg = arg;
            break;
        }

        arg_index += 1;
    }

    if (env_buffer.items.len > 0) {
        opts.wasm_env = env_buffer.items;
    }

    if (dir_buffer.items.len > 0) {
        opts.wasm_dirs = dir_buffer.items;
    }

    return opts;
}

const version_string = "bytebox v0.0.1";

fn printHelp(args: []const []const u8) void {
    const usage_string: []const u8 =
        \\Usage: {s} <FILE> [WASM_ARGS]... [OPTION]...
        \\
        \\  Options:
        \\
        \\    -h, --help
        \\      Print help information.
        \\
        \\    -v, --version
        \\      Print version information.
        \\
        \\    --dump
        \\      Prints the given module definition's imports and exports. Imports are qualified
        \\      with the import module name.
        \\
        \\    -i, --invoke <FUNCTION> [ARGS]...
        \\      Call an exported, named function with arguments. The arguments are automatically
        \\      translated from string inputs to the function's native types. If the conversion
        \\      is not possible, an error is printed and execution aborts.
        \\
        \\    -e, --env <ENVAR>
        \\      Set an environment variable for the execution environment. Typically retrieved
        \\      via the WASI API environ_sizes_get() and environ_get(). Multiple instances of
        \\      this flag is needed to pass multiple variables.
        \\
        \\   -d, --dir <PATH>
        \\     Allow WASI programs to access this directory and paths within it. Can be relative
        \\     to the current working directory or absolute. Multiple instances of this flag can
        \\     be used to pass multiple dirs.
        \\
        \\  -t, --trace <MODE>
        \\     Print a trace of the wasm program as it executes. MODE can be:
        \\       * none (default)
        \\       * function
        \\       * instruction
        \\     Note that this requires bytebox to be compiled with the flag -Ddebug_trace=true,
        \\     which is off by default for performance reasons.
        \\
        \\
    ;

    log.info(usage_string, .{args[0]});
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator: std.mem.Allocator = gpa.allocator();

    const args: []const [:0]u8 = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var env_buffer = std.array_list.Managed([]const u8).init(allocator);
    defer env_buffer.deinit();
    try env_buffer.ensureTotalCapacity(4096); // 4096 vars should be enough for most insane script file scenarios.

    var dir_buffer = std.array_list.Managed([]const u8).init(allocator);
    defer dir_buffer.deinit();
    try dir_buffer.ensureTotalCapacity(4096);

    const opts: CmdOpts = parseCmdOpts(args, &env_buffer, &dir_buffer);

    if (opts.print_help) {
        printHelp(args);
        return;
    } else if (opts.print_version) {
        log.info("{s}", .{version_string});
        return;
    } else if (opts.invalid_arg) |invalid_arg| {
        log.err("Invalid argument '{s}'.\n", .{invalid_arg});
        printHelp(args);
        return;
    } else if (opts.missing_options) |missing_options| {
        log.err("Argument {s} is missing required options.\n", .{missing_options});
        printHelp(args);
        return;
    } else if (opts.invoke != null and opts.filename == null) {
        log.err("Cannot invoke {s} without a file to load.", .{opts.invoke.?.funcname});
        printHelp(args);
        return;
    }

    if (opts.trace != .None) {
        bytebox.DebugTrace.setMode(opts.trace);
    }

    std.debug.assert(opts.filename != null);

    var cwd = std.fs.cwd();
    const wasm_data: []u8 = cwd.readFileAlloc(allocator, opts.filename.?, 1024 * 1024 * 128) catch |e| {
        std.log.err("Failed to read file '{s}' into memory: {}", .{ opts.filename.?, e });
        return RunErrors.IoError;
    };
    defer allocator.free(wasm_data);

    const module_def_opts = bytebox.ModuleDefinitionOpts{
        .debug_name = std.fs.path.basename(opts.filename.?),
        .log = log,
    };
    var module_def = try bytebox.createModuleDefinition(allocator, module_def_opts);
    defer module_def.destroy();

    module_def.decode(wasm_data) catch |e| {
        std.log.err("Caught error decoding module: {}", .{e});
        return e;
    };

    if (opts.print_dump) {
        var strbuf = std.array_list.Managed(u8).init(allocator);
        try strbuf.ensureTotalCapacity(1024 * 16);
        try module_def.dump(strbuf.writer());
        log.info("{s}", .{strbuf.items});
        return;
    }

    var module_instance = try bytebox.createModuleInstance(.Stack, module_def, allocator);
    defer module_instance.destroy();

    var imports_wasi: bytebox.ModuleImportPackage = blk: {
        if (config.enable_wasi) {
            break :blk try wasi.initImports(.{
                .argv = opts.wasm_argv,
                .env = opts.wasm_env,
                .dirs = opts.wasm_dirs,
            }, allocator);
        } else {
            break :blk try bytebox.ModuleImportPackage.init("empty_stub", null, null, allocator);
        }
    };
    defer {
        if (config.enable_wasi) {
            wasi.deinitImports(&imports_wasi);
        } else {
            imports_wasi.deinit();
        }
    }

    const instantiate_opts = bytebox.ModuleInstantiateOpts{
        .imports = &[_]bytebox.ModuleImportPackage{imports_wasi},
        .log = log,
    };

    module_instance.instantiate(instantiate_opts) catch |e| {
        std.log.err("Caught error instantiating module {}.", .{e});
        return e;
    };

    const invoke_funcname: []const u8 = if (opts.invoke) |invoke| invoke.funcname else "_start";
    const invoke_args: []const []const u8 = if (opts.invoke) |invoke| invoke.args else &[_][]u8{};

    const func_handle: bytebox.FunctionHandle = module_instance.getFunctionHandle(invoke_funcname) catch {
        // don't log an error if the user didn't explicitly try to invoke a function
        if (opts.invoke != null) {
            std.log.err("Failed to find function '{s}' - either it doesn't exist or is not a public export.", .{invoke_funcname});
        }
        return RunErrors.MissingFunction;
    };

    const func_export: bytebox.FunctionExport = module_def.getFunctionExport(func_handle).?;

    const num_params: usize = invoke_args.len;
    if (func_export.params.len != num_params) {
        var strbuf = std.array_list.Managed(u8).init(allocator);
        defer strbuf.deinit();
        try writeSignature(&strbuf, &func_export);
        std.log.err("Specified {} params but expected {}. The signature of '{s}' is:\n{s}", .{
            num_params,
            func_export.params.len,
            invoke_funcname,
            strbuf.items,
        });
        return RunErrors.FunctionParamMismatch;
    }

    std.debug.assert(invoke_args.len == num_params);

    var params = std.array_list.Managed(bytebox.Val).init(allocator);
    defer params.deinit();
    try params.resize(invoke_args.len);
    for (func_export.params, 0..) |valtype, i| {
        const arg: []const u8 = invoke_args[i];
        switch (valtype) {
            .I32 => {
                const parsed: i32 = std.fmt.parseInt(i32, arg, 0) catch |e| {
                    std.log.err("Failed to parse arg at index {} ('{s}') as an i32: {}", .{ i, arg, e });
                    return RunErrors.BadFunctionParam;
                };
                params.items[i] = Val{ .I32 = parsed };
            },
            .I64 => {
                const parsed: i64 = std.fmt.parseInt(i64, arg, 0) catch |e| {
                    std.log.err("Failed to parse arg at index {} ('{s}') as an i64: {}", .{ i, arg, e });
                    return RunErrors.BadFunctionParam;
                };
                params.items[i] = Val{ .I64 = parsed };
            },
            .F32 => {
                const parsed: f32 = std.fmt.parseFloat(f32, arg) catch |e| {
                    std.log.err("Failed to parse arg at index {} ('{s}') as a f32: {}", .{ i, arg, e });
                    return RunErrors.BadFunctionParam;
                };
                params.items[i] = Val{ .F32 = parsed };
            },
            .F64 => {
                const parsed: f64 = std.fmt.parseFloat(f64, arg) catch |e| {
                    std.log.err("Failed to parse arg at index {} ('{s}') as a f64: {}", .{ i, arg, e });
                    return RunErrors.BadFunctionParam;
                };
                params.items[i] = Val{ .F64 = parsed };
            },
            .V128 => {
                std.log.err("Param at index {} is a v128, which is currently only invokeable from code.", .{i});
                return RunErrors.BadFunctionParam;
            },
            .FuncRef => {
                std.log.err("Param at index {} is a v128, making this function only invokeable from code.", .{i});
                return RunErrors.BadFunctionParam;
            },
            .ExternRef => {
                std.log.err("Param at index {} is an externref, making this function only invokeable from code.", .{i});
                return RunErrors.BadFunctionParam;
            },
        }
    }

    var returns = std.array_list.Managed(bytebox.Val).init(allocator);
    try returns.resize(func_export.returns.len);

    module_instance.invoke(func_handle, params.items.ptr, returns.items.ptr, .{}) catch |e| {
        var backtrace = module_instance.formatBacktrace(1, allocator) catch unreachable;
        std.log.err("Caught {} during function invoke. Backtrace:\n{s}\n", .{ e, backtrace.items });
        backtrace.deinit();
        return e;
    };

    {
        var strbuf = std.array_list.Managed(u8).init(allocator);
        defer strbuf.deinit();
        const writer = strbuf.writer();

        if (returns.items.len > 0) {
            const return_types = func_export.returns;
            try std.fmt.format(writer, "return:\n", .{});
            for (returns.items, 0..) |_, i| {
                switch (return_types[i]) {
                    .I32 => try std.fmt.format(writer, "  {} (i32)\n", .{returns.items[i].I32}),
                    .I64 => try std.fmt.format(writer, "  {} (i64)\n", .{returns.items[i].I64}),
                    .F32 => try std.fmt.format(writer, "  {} (f32)\n", .{returns.items[i].F32}),
                    .F64 => try std.fmt.format(writer, "  {} (f64)\n", .{returns.items[i].F64}),
                    .V128 => unreachable, // TODO support
                    .FuncRef => try std.fmt.format(writer, "  (funcref)\n", .{}),
                    .ExternRef => try std.fmt.format(writer, "  (externref)\n", .{}),
                }
            }
            try std.fmt.format(writer, "\n", .{});
        }
        if (strbuf.items.len > 0) {
            log.info("{s}\n", .{strbuf.items});
        }
    }
}

fn writeSignature(strbuf: *std.array_list.Managed(u8), info: *const bytebox.FunctionExport) !void {
    const writer = strbuf.writer();
    if (info.params.len == 0) {
        try std.fmt.format(writer, "  params: none\n", .{});
    } else {
        try std.fmt.format(writer, "  params:\n", .{});
        for (info.params) |valtype| {
            const name: []const u8 = valtypeToString(valtype);
            try std.fmt.format(writer, "    {s}\n", .{name});
        }
    }

    if (info.returns.len == 0) {
        try std.fmt.format(writer, "  returns: none\n", .{});
    } else {
        try std.fmt.format(writer, "  returns:\n", .{});
        for (info.returns) |valtype| {
            const name: []const u8 = valtypeToString(valtype);
            try std.fmt.format(writer, "    {s}\n", .{name});
        }
    }
}

fn valtypeToString(valtype: ValType) []const u8 {
    return switch (valtype) {
        inline else => |v| @typeInfo(ValType).@"enum".fields[@intFromEnum(v)].name,
    };
}
