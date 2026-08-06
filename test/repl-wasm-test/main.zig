//! Bytebox integration tests for the dedicated REPL WebAssembly protocol.

const std = @import("std");
const build_options = @import("build_options");
const bytebox = @import("bytebox");

const Interface = struct {
    instance: *bytebox.ModuleInstance,
    memory: *bytebox.MemoryInstance,
    alloc: bytebox.FunctionHandle,
    free: bytebox.FunctionHandle,
    process: bytebox.FunctionHandle,
    free_response: bytebox.FunctionHandle,
};

fn readU32(bytes: []const u8) u32 {
    return @as(u32, bytes[0]) |
        (@as(u32, bytes[1]) << 8) |
        (@as(u32, bytes[2]) << 16) |
        (@as(u32, bytes[3]) << 24);
}

fn invoke(interface: Interface, allocator: std.mem.Allocator, request: []const u8) ![]u8 {
    var alloc_params = [_]bytebox.Val{.{ .I32 = @intCast(request.len) }};
    var alloc_returns = [_]bytebox.Val{.{ .I32 = 0 }};
    try interface.instance.invoke(interface.alloc, &alloc_params, &alloc_returns, .{});
    const request_ptr: u32 = @intCast(alloc_returns[0].I32);
    if (request_ptr == 0) return error.WasmAllocationFailed;

    var memory = interface.memory.buffer();
    if (@as(usize, request_ptr) + request.len > memory.len) return error.WasmBufferOutOfBounds;
    @memcpy(memory[request_ptr..][0..request.len], request);

    var process_params = [_]bytebox.Val{
        .{ .I32 = @intCast(request_ptr) },
        .{ .I32 = @intCast(request.len) },
    };
    var process_returns = [_]bytebox.Val{.{ .I32 = 0 }};
    try interface.instance.invoke(interface.process, &process_params, &process_returns, .{});

    var free_params = [_]bytebox.Val{
        .{ .I32 = @intCast(request_ptr) },
        .{ .I32 = @intCast(request.len) },
    };
    var no_returns = [_]bytebox.Val{};
    try interface.instance.invoke(interface.free, &free_params, &no_returns, .{});

    const response_ptr: u32 = @intCast(process_returns[0].I32);
    if (response_ptr == 0) return error.WasmProcessFailed;
    memory = interface.memory.buffer();
    if (@as(usize, response_ptr) + 4 > memory.len) return error.WasmBufferOutOfBounds;
    const response_len = readU32(memory[response_ptr..][0..4]);
    if (@as(usize, response_ptr) + 4 + response_len > memory.len) return error.WasmBufferOutOfBounds;
    const response = try allocator.dupe(u8, memory[response_ptr + 4 ..][0..response_len]);

    var response_free_params = [_]bytebox.Val{.{ .I32 = @intCast(response_ptr) }};
    try interface.instance.invoke(interface.free_response, &response_free_params, &no_returns, .{});
    return response;
}

fn requireContains(response: []const u8, expected: []const u8) !void {
    if (std.mem.find(u8, response, expected) != null) return;
    std.debug.print("Expected response to contain:\n{s}\nActual response:\n{s}\n", .{ expected, response });
    return error.UnexpectedResponse;
}

fn requireInOrder(response: []const u8, expected: []const []const u8) !void {
    var offset: usize = 0;
    for (expected) |fragment| {
        const relative = std.mem.find(u8, response[offset..], fragment) orelse {
            std.debug.print("Expected ordered fragment:\n{s}\nAfter offset {d} in response:\n{s}\n", .{ fragment, offset, response });
            return error.UnexpectedResponse;
        };
        offset += relative + fragment.len;
    }
}

fn sendAndRequire(interface: Interface, allocator: std.mem.Allocator, request: []const u8, expected: []const []const u8) !void {
    const response = try invoke(interface, allocator, request);
    defer allocator.free(response);
    for (expected) |fragment| try requireContains(response, fragment);
}

fn sendAndCheck(
    interface: Interface,
    allocator: std.mem.Allocator,
    request: []const u8,
    expected: []const []const u8,
    rejected: []const []const u8,
) !void {
    const response = try invoke(interface, allocator, request);
    defer allocator.free(response);
    for (expected) |fragment| try requireContains(response, fragment);
    for (rejected) |fragment| {
        if (std.mem.find(u8, response, fragment) == null) continue;
        std.debug.print("Expected response not to contain:\n{s}\nActual response:\n{s}\n", .{ fragment, response });
        return error.UnexpectedResponse;
    }
}

pub fn main(init: std.process.Init) !void {
    var gpa_impl: std.heap.DebugAllocator(.{ .stack_trace_frames = build_options.debug_gpa_stack_trace_frames }) = .init;
    defer _ = build_options.debugGpaOk(gpa_impl.deinit());
    const gpa = gpa_impl.allocator();

    var arena_impl = std.heap.ArenaAllocator.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    const args = try init.minimal.args.toSlice(arena);
    if (args.len != 2) return error.ExpectedWasmPath;
    const wasm_bytes = try std.Io.Dir.cwd().readFileAlloc(init.io, args[1], arena, .unlimited);

    var definition = try bytebox.createModuleDefinition(arena, .{ .debug_name = "repl_wasm" });
    defer definition.destroy();
    try definition.decode(wasm_bytes);
    var instance = try bytebox.createModuleInstance(.Stack, definition, gpa);
    defer instance.destroy();
    try instance.instantiate(.{ .stack_size = 1024 * 256 });

    const interface: Interface = .{
        .instance = instance,
        .memory = instance.store.getMemory(0),
        .alloc = try instance.getFunctionHandle("roc_repl_alloc"),
        .free = try instance.getFunctionHandle("roc_repl_free"),
        .process = try instance.getFunctionHandle("roc_repl_process"),
        .free_response = try instance.getFunctionHandle("roc_repl_free_response"),
    };

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":1,"op":"capabilities"}
    , &.{ "\"ok\":true", "one_session_per_wasm_instance", "\"revision_bits\":32", "\"presentation_strings\":false", "\"diagnostic_scope\":\"blocking_only\"", "\"completion_scope\":\"session_definitions\"", "\"filesystem\":false", "\"platform_effects\":\"one_way_events\"", "\"effect_module\":\"Repl\"", "\"effect_function\":\"emit!\"", "\"effect_payload_encoding\":\"caller_defined_utf8\"", "\"effect_responses\":false" });

    try sendAndCheck(interface, gpa,
        \\{"protocol":1,"id":2,"op":"eval","params":{"source":"x = 41"}}
    , &.{ "\"kind\":\"definition\"", "\"definition_kind\":\"value\"", "\"name\":\"x\"", "\"committed\":true", "\"revision\":1", "\"type\":\"" }, &.{ "assigned", "\"display\"" });

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":3,"op":"eval","params":{"source":"x + 1"}}
    , &.{ "\"kind\":\"expression\"", "\"committed\":false", "\"value\":\"42.0\"", "\"type\":\"" });

    try sendAndCheck(interface, gpa,
        \\{"protocol":1,"id":4,"op":"eval","params":{"source":"y = 1\ny + 2"}}
    , &.{ "\"name\":\"y\"", "\"committed\":true", "\"value\":\"3.0\"", "\"committed_count\":1" }, &.{"assigned"});

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":5,"op":"analyze","params":{"source":"[1,"}}
    , &.{"\"status\":\"incomplete\""});

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":6,"op":"inspect","params":{"source":"x + 1"}}
    , &.{ "\"ok\":true", "\"status\":\"ok\"", "\"type\":\"", "\"diagnostics\":[]" });

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":61,"op":"inspect","params":{"source":"not_committed = 1"}}
    , &.{ "\"status\":\"diagnostic\"", "\"code\":\"expected_expression\"" });

    try sendAndCheck(interface, gpa,
        \\{"protocol":1,"id":7,"op":"complete","params":{"source":"x","cursor":1}}
    , &.{ "\"label\":\"x\"", "\"insert_text\":\"x\"", "\"detail\":\"", "\"prefix\":\"x\"", "\"replacement\":{\"start\":0,\"end\":1}", "\"offset_unit\":\"utf8_bytes\"" }, &.{"\"label\":\"y\""});

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":8,"op":"get_state"}
    , &.{ "\"definitions\":[{\"name\":\"x\",\"source\":\"x = 41\",\"kind\":\"value\"}", "\"definition_source\":\"x = 41\\ny = 1\\n\"", "\"modules\":[]" });

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":9,"op":"eval","params":{"source":"a = 1\nbad = \"x\" + 1"}}
    , &.{ "\"name\":\"a\"", "\"status\":\"diagnostic\"", "\"code\":\"compile_error\"", "\"severity\":\"error\"", "\"region\":null", "\"completed\":false", "\"stop_reason\":\"diagnostic\"", "\"committed_count\":1" });

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":10,"op":"eval","params":{"source":"a + 1"}}
    , &.{"\"value\":\"2.0\""});

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":11,"op":"eval","params":{"source":"dbg \"hello\""}}
    , &.{ "\"kind\":\"dbg\"", "hello" });

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":111,"op":"eval","params":{"source":"{ expect 1 == 0\n42 }"}}
    , &.{ "\"kind\":\"expect_failed\"", "expect failed", "\"value\":\"42.0\"" });

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":12,"op":"eval","params":{"source":"crash \"boom\""}}
    , &.{ "\"status\":\"crashed\"", "\"crash\":{\"message\":\"boom\"}", "\"kind\":\"crashed\"" });

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":13,"op":"eval","params":{"source":"1 + 1"}}
    , &.{ "\"status\":\"ok\"", "\"value\":\"2.0\"" });

    const effect_response = try invoke(interface, gpa,
        \\{"protocol":1,"id":130,"op":"eval","params":{"source":"import Repl\n{\n    Repl.emit!({ name: \"log\", payload: \"héllo\" })\n    dbg \"middle\"\n    Repl.emit!({ name: \"toast\", payload: \"done\" })\n}"}}
    );
    defer gpa.free(effect_response);
    try requireInOrder(effect_response, &.{
        "\"definition_kind\":\"import\"",
        "\"kind\":\"effect\",\"name\":\"log\",\"payload\":\"héllo\"",
        "\"kind\":\"dbg\"",
        "\"kind\":\"effect\",\"name\":\"toast\",\"payload\":\"done\"",
    });
    try requireContains(effect_response, "\"type\":\"{}\"");

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":1300,"op":"inspect","params":{"source":"Repl.emit!({ name: \"log\", payload: \"inspection does not run\" })"}}
    , &.{ "\"status\":\"ok\"", "\"type\":\"{}\"" });

    const crashed_effect_response = try invoke(interface, gpa,
        \\{"protocol":1,"id":1301,"op":"eval","params":{"source":"{\n    Repl.emit!({ name: \"log\", payload: \"before crash\" })\n    crash \"effect boom\"\n}"}}
    );
    defer gpa.free(crashed_effect_response);
    try requireContains(crashed_effect_response, "\"crash\":{\"message\":\"effect boom\"}");
    try requireInOrder(crashed_effect_response, &.{
        "\"kind\":\"effect\",\"name\":\"log\",\"payload\":\"before crash\"",
        "\"kind\":\"crashed\"",
    });

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":131,"op":"eval","params":{"source":"pending : Str"}}
    , &.{ "\"definition_kind\":\"annotation\"", "\"name\":\"pending\"", "\"committed\":true" });

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":1311,"op":"eval","params":{"source":"pending = 42"}}
    , &.{ "\"status\":\"diagnostic\"", "\"code\":\"compile_error\"", "\"committed_count\":0" });

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":1312,"op":"get_state"}
    , &.{ "\"name\":\"pending\",\"source\":\"pending : Str\",\"kind\":\"annotation\"", "\"has_pending_annotation\":true" });

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":132,"op":"complete","params":{"source":"pend","cursor":4}}
    , &.{ "\"label\":\"pending\"", "\"kind\":\"annotation\"", "\"details_available\":false" });

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":133,"op":"eval","params":{"source":"pending = \"yes\""}}
    , &.{ "\"definition_kind\":\"value\"", "\"name\":\"pending\"", "\"type\":\"Str\"" });

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":134,"op":"complete","params":{"source":"é","cursor":1}}
    , &.{ "\"ok\":false", "\"code\":\"invalid_cursor\"" });

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":135,"op":"complete","params":{"source":"x","cursor":1,"typo":true}}
    , &.{ "\"ok\":false", "\"code\":\"invalid_json\"" });

    try sendAndCheck(interface, gpa,
        \\{"protocol":1,"id":136,"op":"eval","params":{"source":":help"}}
    , &.{ "\"status\":\"diagnostic\"", "\"code\":\"parse_error\"" }, &.{"Enter an expression"});

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":14,"op":"set_modules","params":{"modules":[{"name":"Util","source":"Util := [].{\n    answer = 42\n}\n"}]}}
    , &.{ "\"module_count\":1", "\"module_names\":[\"Util\"]", "\"cleared_definition_count\":" });

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":141,"op":"get_state"}
    , &.{"\"modules\":[{\"name\":\"Util\",\"source\":\"Util := [].{\\n    answer = 42\\n}\\n\"}]"});

    try sendAndCheck(interface, gpa,
        \\{"protocol":1,"id":15,"op":"eval","params":{"source":"import Util\nUtil.answer"}}
    , &.{ "\"definition_kind\":\"import\"", "\"name\":\"Util\"", "\"value\":\"42.0\"" }, &.{"imported `Util`"});

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":151,"op":"set_modules","params":{"modules":[{"name":"Duplicate","source":"Duplicate := [].{}"},{"name":"Duplicate","source":"Duplicate := [].{}"}]}}
    , &.{ "\"ok\":false", "\"code\":\"duplicate_module\"" });

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":1511,"op":"set_modules","params":{"modules":[{"name":"Repl","source":"Repl := [].{}"}]}}
    , &.{ "\"ok\":false", "\"code\":\"reserved_module\"" });

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":152,"op":"eval","params":{"source":"Util.answer"}}
    , &.{ "\"status\":\"ok\"", "\"value\":\"42.0\"" });

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":16,"op":"clear"}
    , &.{ "\"changed\":true", "\"removed_definition_count\":1" });

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":17,"op":"eval","params":{"source":"Util.answer"}}
    , &.{ "\"status\":\"diagnostic\"", "\"committed\":false" });

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":18,"op":"eval","params":{"source":"import \"secret.txt\" as secret : Str"}}
    , &.{ "\"status\":\"diagnostic\"", "\"definition_kind\":\"file_import\"", "\"code\":\"unsupported_file_import\"", "File imports are not available" });

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":19,"op":"set_modules","params":{"modules":[{"name":"Other","source":"Other := [].{\n    emit! : { name : Str, payload : Str } => {}\n}"}]}}
    , &.{ "\"ok\":true", "\"module_names\":[\"Other\"]" });

    try sendAndRequire(interface, gpa,
        \\{"protocol":1,"id":20,"op":"eval","params":{"source":"import Other\nOther.emit!({ name: \"log\", payload: \"not Repl\" })"}}
    , &.{ "\"status\":\"diagnostic\"", "This REPL only supports the hosted function Repl.emit!." });
}
