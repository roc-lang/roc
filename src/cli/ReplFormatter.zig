//! Line-oriented REPL frontend plugins. The wire format belongs entirely to
//! Roc's `decode` and `encode` functions, never to the compiler.
const std = @import("std");
const builtins = @import("builtins");
const eval = @import("eval");
const layout = @import("layout");
const reporting = @import("reporting");
const CliCtx = @import("CliCtx.zig").CliCtx;
const ReplSession = @import("ReplSession.zig");
const RocStr = builtins.str.RocStr;

// This compiler-owned wrapper checks the plugin contract before lowering.
// Tuples are an internal host ABI; their offsets come from committed layouts.
const wrapper =
    \\import Formatter
    \\main : (Bool, Str, Str, Str, Str, Bool, Str, Bool, Str) -> (Bool, Str)
    \\main = |request| {
    \\    (decoding, input, result, stdout, diagnostics, is_tag, name, has_payload, payload) = request
    \\    if decoding {
    \\        decoded : Try(Str, Str)
    \\        decoded = Formatter.decode(input)
    \\        match decoded {
    \\            Ok(source) => (True, source)
    \\            Err(reply) => (False, reply)
    \\        }
    \\    } else {
    \\        value = if is_tag {
    \\            Tag({ name, payload: if has_payload Ok(payload) else Err({}) })
    \\        } else { Other }
    \\        (False, Formatter.encode({ result, stdout, diagnostics, value }))
    \\    }
    \\}
;

const Reply = struct {
    evaluate: bool,
    bytes: []u8,
};

/// Call an already checked and lowered formatter. Each call owns its runtime
/// allocations, including on a language crash; the compiled program is reused.
fn call(allocator: std.mem.Allocator, stderr: *std.Io.Writer, program: *const eval.Inspected.CompiledTargetProgram, decoding: bool, strings: [4][]const u8, value: ReplSession.ResultValue) !Reply {
    const lowered = &program.lowered;
    const layouts = &lowered.view.layouts;
    var runtime = eval.RuntimeHostEnv.init(allocator);
    defer runtime.deinit();
    var static_data = try eval.InterpreterStaticData.init(allocator, lowered.view.static_data, lowered.view.static_data_value_count);
    defer static_data.deinit();
    var static_strings = try eval.LirInterpreter.buildStaticStrings(allocator, &lowered.view.store);
    defer static_strings.deinit();
    var interpreter = try eval.LirInterpreter.initWithBoxyTables(
        allocator,
        &lowered.view.store,
        layouts,
        eval.boxy_runtime.BoxyTables.fromImageView(&lowered.view),
        static_strings.view(),
        runtime.get_ops(),
    );
    defer interpreter.deinit();
    static_data.install(&interpreter);
    static_data.ownByInterpreter(&interpreter);
    const arg_layouts = try eval.Inspected.mainProcArgLayouts(allocator, lowered);
    defer allocator.free(arg_layouts);
    std.debug.assert(arg_layouts.len == 1);
    const args = (try eval.Inspected.zeroedEntrypointArgBuffer(allocator, lowered, arg_layouts)).?;
    defer allocator.free(args);
    field(bool, args.ptr, layouts, arg_layouts[0], 0).* = decoding;
    for (strings, 0..) |bytes, i| {
        field(RocStr, args.ptr, layouts, arg_layouts[0], @intCast(i + 1)).* = RocStr.fromSlice(bytes, runtime.get_ops());
    }
    field(bool, args.ptr, layouts, arg_layouts[0], 5).* = value.tag_name != null;
    field(RocStr, args.ptr, layouts, arg_layouts[0], 6).* = RocStr.fromSlice(value.tag_name orelse "", runtime.get_ops());
    field(bool, args.ptr, layouts, arg_layouts[0], 7).* = value.string_payload != null;
    field(RocStr, args.ptr, layouts, arg_layouts[0], 8).* = RocStr.fromSlice(value.string_payload orelse "", runtime.get_ops());
    const returned = interpreter.eval(.{
        .proc_id = lowered.mainProc(),
        .arg_layouts = arg_layouts,
        .arg_ptr = args.ptr,
    }) catch |err| switch (err) {
        error.Crash => {
            try stderr.print("Formatter crashed: {s}\n", .{interpreter.getCrashMessage()});
            return error.InvalidFormatter;
        },
        error.RuntimeError => {
            if (interpreter.getRuntimeErrorMessage()) |message| try stderr.print("Formatter failed: {s}\n", .{message});
            return error.InvalidFormatter;
        },
        else => return err,
    };
    const ret_layout = lowered.view.store.getProcSpec(lowered.mainProc()).ret_layout;
    const should_evaluate = field(bool, returned.value.ptr, layouts, ret_layout, 0).*;
    const text = field(RocStr, returned.value.ptr, layouts, ret_layout, 1).*;
    defer text.decref(runtime.get_ops());
    return .{ .evaluate = should_evaluate, .bytes = try allocator.dupe(u8, text.asSlice()) };
}

fn field(comptime T: type, bytes: [*]u8, layouts: *const layout.Store, idx: layout.Idx, original_index: u32) *align(1) T {
    const aggregate = layouts.getLayout(layouts.runtimeRepresentationLayoutIdx(idx));
    std.debug.assert(aggregate.tag == .struct_);
    const offset = layouts.getStructFieldOffsetByOriginalIndex(aggregate.getStruct().idx, original_index);
    return @ptrCast(bytes + offset);
}

pub fn run(ctx: *CliCtx, session: *ReplSession, path: []const u8) !void {
    const allocator = ctx.gpa;
    session.capture_result_value = true;
    defer session.capture_result_value = false;
    const source = try ctx.coreCtx().readFile(path, allocator);
    defer allocator.free(source);
    var loader = session.sibling();
    defer loader.deinit();
    loader.module_root = std.fs.path.dirname(path) orelse ".";
    try loader.replaceVirtualModules(&.{.{ .name = "Formatter", .source = source }});
    try loader.addOrReplaceDefinition("import Formatter", "Formatter", .import);
    var config = ctx.reportConfig(.stderr);
    config.color_preference = .never;
    var program = switch (try loader.compileFrontend(wrapper)) {
        .compiled => |program| program,
        .import_error => |message| {
            defer allocator.free(message);
            try ctx.io.stderr().print("{s}\n", .{message});
            return error.InvalidFormatter;
        },
        .diagnostics => |value| {
            var resources = value;
            defer resources.deinit(allocator);
            const message = try eval.Inspected.renderParsedResourcesProblemsWithConfig(allocator, &resources, config);
            defer allocator.free(message);
            try ctx.io.stderr().writeAll(message);
            return error.InvalidFormatter;
        },
    };
    defer program.deinit(allocator);

    // A persistent reader retains bytes read ahead across requests. A dynamic
    // line writer permits notebook cells larger than the reader's buffer.
    var buffer: [4096]u8 = undefined;
    var input = std.Io.File.stdin().reader(ctx.io.std_io, &buffer);
    var line = std.Io.Writer.Allocating.init(allocator);
    defer line.deinit();
    while (true) {
        line.clearRetainingCapacity();
        _ = try input.interface.streamDelimiterEnding(&line.writer, '\n');
        const eof = input.interface.buffered().len == 0;
        if (eof and line.writer.buffered().len == 0) break;
        if (!eof) input.interface.toss(1);
        const decoded = try call(allocator, ctx.io.stderr(), &program, true, .{ std.mem.trimEnd(u8, line.writer.buffered(), "\r"), "", "", "" }, .{});
        defer allocator.free(decoded.bytes);
        if (!decoded.evaluate) {
            try writeReply(ctx, decoded.bytes);
            continue;
        }
        var result = std.Io.Writer.Allocating.init(allocator);
        defer result.deinit();
        var output = std.Io.Writer.Allocating.init(allocator);
        defer output.deinit();
        var diagnostics = std.Io.Writer.Allocating.init(allocator);
        defer diagnostics.deinit();
        const value = try evaluate(session, decoded.bytes, config, &result, &output, &diagnostics);
        defer value.deinit(allocator);
        const encoded = try call(allocator, ctx.io.stderr(), &program, false, .{ "", result.writer.buffered(), output.writer.buffered(), diagnostics.writer.buffered() }, value);
        defer allocator.free(encoded.bytes);
        try writeReply(ctx, encoded.bytes);
    }
}

fn writeReply(ctx: *CliCtx, bytes: []const u8) !void {
    // Framing is part of the formatter contract. Reject a broken plugin rather
    // than silently desynchronizing the caller's next response.
    if (std.mem.indexOfScalar(u8, bytes, '\n') != null) return error.MultilineFormatterReply;
    try ctx.io.stdout().print("{s}\n", .{bytes});
    ctx.io.flush();
}

fn evaluate(session: *ReplSession, source: []const u8, config: reporting.ReportingConfig, result: *std.Io.Writer.Allocating, output: *std.Io.Writer.Allocating, diagnostics: *std.Io.Writer.Allocating) !ReplSession.ResultValue {
    var value: ReplSession.ResultValue = .{};
    errdefer value.deinit(session.allocator);
    const statements = try session.splitInputIntoStatements(source);
    defer session.freeStatementSlices(statements);
    for (statements) |statement| {
        if (ReplSession.parseTypeQuery(statement)) |name| {
            const text = try session.printTypeOfVar(name, false);
            defer session.allocator.free(text);
            // A query is a text result, never the preceding expression's tag.
            value.deinit(session.allocator);
            value = .{};
            result.clearRetainingCapacity();
            try result.writer.writeAll(std.mem.trimEnd(u8, text, "\n"));
            continue;
        }
        const step = try session.stepLanguageWithConfig(statement, config);
        defer step.deinit(session.allocator);
        const events = session.takeEvents();
        defer {
            for (events) |*event| event.deinit(session.allocator);
            session.allocator.free(events);
        }
        for (events) |event| {
            switch (event) {
                .dbg => |message| try output.writer.print("{s}\n", .{message}),
                .expect_failed, .crashed => |message| try diagnostics.writer.print("{s}\n", .{message}),
                .effect => return error.UnsupportedReplEffect,
            }
        }
        switch (step) {
            .expression => |text| {
                value.deinit(session.allocator);
                value = session.takeResultValue();
                result.clearRetainingCapacity();
                try result.writer.writeAll(text);
            },
            .diagnostic => |diagnostic| {
                try diagnostics.writer.writeAll(diagnostic.message);
                break;
            },
            .runtime_crash => |message| {
                try diagnostics.writer.writeAll(message);
                break;
            },
            .definition, .statement, .none => {},
        }
    }
    return value;
}
