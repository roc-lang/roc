const std = @import("std");
const bytebox = @import("bytebox");
const build_options = @import("build_options");

var verbose_mode = false;

/// Verbose debug logging
fn logDebug(comptime format: []const u8, args: anytype) void {
    if (verbose_mode) {
        std.debug.print(format, args);
    }
}

/// Represents a message sent to the WASM playground module.
const WasmMessage = struct {
    /// The type of message, dictating the action to be performed (e.g., "INIT", "LOAD_SOURCE").
    type: []const u8,
    /// The Roc source code, used by "LOAD_SOURCE".
    source: ?[]const u8 = null,
    /// The REPL input, used by "REPL_STEP".
    input: ?[]const u8 = null,
    /// An identifier, used by "GET_HOVER_INFO" to specify what to query.
    identifier: ?[]const u8 = null,
    /// The line number (1-based), used by "GET_HOVER_INFO".
    line: ?u32 = null,
    /// The column number (1-based), used by "GET_HOVER_INFO".
    ch: ?u32 = null,
};

/// Represents a response received from the WASM playground module.
const WasmResponse = struct {
    /// The outcome of the operation (e.g., "SUCCESS", "ERROR").
    status: []const u8,
    /// A human-readable message describing the outcome.
    message: ?[]const u8 = null,
    /// Contains payload data for successful queries (e.g., AST, types).
    data: ?[]const u8 = null,
    /// Present after "LOAD_SOURCE" with compilation details.
    diagnostics: ?Diagnostics = null,
    /// Present after "GET_HOVER_INFO" with hover details for an identifier.
    hover_info: ?HoverInfo = null,
    /// Present after "INIT_REPL" with REPL initialization details.
    repl_info: ?ReplInfo = null,
    /// Present after "REPL_STEP" with REPL evaluation details.
    result: ?ReplResult = null,
    /// An optional error code, corresponding to the `WasmError` enum from the playground.
    code: ?u8 = null,

    /// Contains diagnostic information from the compiler.
    const Diagnostics = struct {
        /// A count of errors and warnings.
        summary: Summary,
        /// An HTML-formatted string of all diagnostic messages.
        html: ?[]const u8 = null,
        /// An array of individual `Diagnostic` objects for structured access.
        list: ?[]Diagnostic = null,
        /// A JSON object with detailed diagnostic counts per compiler stage.
        debug_counts: ?std.json.Value = null,
    };

    /// A summary of diagnostic counts.
    const Summary = struct {
        /// The total number of errors.
        errors: u32,
        /// The total number of warnings.
        warnings: u32,
    };

    /// Represents a single diagnostic message (error, warning, or info).
    const Diagnostic = struct {
        /// The severity level (e.g., "error", "warning").
        severity: []const u8,
        /// The diagnostic message text.
        message: []const u8,
        /// The source code region associated with the diagnostic.
        region: Region,
    };

    /// Defines a region in the source code, using 1-based line and column numbers.
    const Region = struct {
        /// The 1-based starting line number.
        start_line: u32,
        /// The 1-based starting column number.
        start_column: u32,
        /// The 1-based ending line number.
        end_line: u32,
        /// The 1-based ending column number.
        end_column: u32,
    };

    /// Contains hover information for a specific identifier, returned by "GET_HOVER_INFO".
    const HoverInfo = struct {
        name: []const u8,
        type_str: []const u8,
        definition_region: Region,
        docs: ?[]const u8,
    };

    const ReplInfo = struct {
        compiler_version: []const u8,
        state: []const u8,
    };

    const ReplResult = struct {
        output: []const u8,
        type: []const u8,
        compiler_available: ?bool = null,
        variable_name: ?[]const u8 = null,
        source: ?[]const u8 = null,
        evaluated_source: ?[]const u8 = null,
        error_stage: ?[]const u8 = null,
        error_details: ?[]const u8 = null,
    };
};

/// Holds the necessary components to interact with the WASM playground module.
const WasmInterface = struct {
    /// The WebAssembly module definition.
    module_def: *bytebox.ModuleDefinition,
    /// A live instance of the WebAssembly module.
    module_instance: *bytebox.ModuleInstance,
    /// Handle to the `init` exported function.
    init_handle: bytebox.FunctionHandle,
    /// Handle to the `processAndRespond` exported function.
    processAndRespond_handle: bytebox.FunctionHandle,
    /// Handle to the `freeWasmString` exported function.
    freeWasmString_handle: bytebox.FunctionHandle,
    /// Handle to the `allocateMessageBuffer` exported function.
    allocateMessageBuffer_handle: bytebox.FunctionHandle,
    /// Handle to the `freeMessageBuffer` exported function.
    freeMessageBuffer_handle: bytebox.FunctionHandle,
    /// Handle to the `getDebugLogBuffer` exported function.
    getDebugLogBuffer_handle: bytebox.FunctionHandle,
    /// Handle to the `getDebugLogLength` exported function.
    getDebugLogLength_handle: bytebox.FunctionHandle,
    /// Handle to the `clearDebugLog` exported function.
    clearDebugLog_handle: bytebox.FunctionHandle,
    /// A reference to the WebAssembly module's memory.
    memory: *bytebox.MemoryInstance,

    /// Cleans up the WASM module instance and its definition.
    pub fn deinit(self: *WasmInterface) void {
        self.module_instance.destroy();
        self.module_def.destroy();
    }
};

/// Defines a single step within a test case, representing an interaction with the WASM module.
const MessageStep = struct {
    /// The `WasmMessage` to send to the WASM module.
    message: WasmMessage,
    /// The expected `status` in the `WasmResponse`.
    expected_status: []const u8,
    /// An optional substring expected to be in the response `message`.
    expected_message_contains: ?[]const u8 = null,
    /// An optional substring expected to be in the response `data`.
    expected_data_contains: ?[]const u8 = null,
    /// An optional substring expected to be in the response `hover_info`.
    expected_hover_info_contains: ?[]const u8 = null,
    /// An optional substring expected to be in the REPL result `output`.
    expected_result_output_contains: ?[]const u8 = null,
    /// Expected REPL result type ("expression", "definition", "error").
    expected_result_type: ?[]const u8 = null,
    /// Expected error stage for REPL errors.
    expected_result_error_stage: ?[]const u8 = null,
    /// Optional expectations for diagnostic content.
    expected_diagnostics: ?DiagnosticExpectation = null,
    /// If true, the step is expected to result in a Wasm invocation error.
    should_fail: bool = false,
    /// If `should_fail` is true, this is the expected error name substring.
    expected_error: ?[]const u8 = null,
    /// If the `message` contains source, this holds the allocated string to be freed.
    owned_source: ?[]const u8 = null,

    /// Defines expectations for diagnostic information in a response.
    const DiagnosticExpectation = struct {
        /// Minimum number of errors expected.
        min_errors: u32 = 0,
        /// Minimum number of warnings expected.
        min_warnings: u32 = 0,
        /// A list of substrings, each expected to be found in an error message.
        error_messages: []const []const u8 = &.{},
        /// A list of substrings, each expected to be found in a warning message.
        warning_messages: []const []const u8 = &.{},
    };
};

/// Represents the outcome of executing a single `MessageStep`.
const StepExecutionResult = struct {
    /// The `TestResult` (passed, failed, or skipped).
    result: TestResult,
    /// A detailed message if the step failed; null otherwise.
    failure_message: ?[]const u8 = null,
};

/// Defines a test case, which is a sequence of `MessageStep`s to be executed.
const TestCase = struct {
    /// A descriptive name for the test case.
    name: []const u8,
    /// A slice of `MessageStep`s that make up the test logic.
    steps: []const MessageStep,
    /// An optional function to run before the test steps, for initialization.
    setup: ?*const fn (std.mem.Allocator, *WasmInterface) anyerror!void = null,
    /// An optional function to run after successful test steps, for cleanup.
    teardown: ?*const fn (std.mem.Allocator, *WasmInterface) anyerror!void = null,
};

/// Represents the possible outcomes of a test case or a single step.
const TestResult = enum {
    /// The test or step completed successfully.
    passed,
    /// The test or step failed due to an assertion or error.
    failed,
    /// The test or step was not executed.
    skipped,
};

/// Stores information about a test failure for reporting.
const TestFailure = struct {
    /// The name of the test case that failed.
    case_name: []const u8,
    /// The index of the step within the case that caused the failure.
    step_index: usize,
    /// The failure message detailing what went wrong.
    message: []const u8,
};

/// Aggregates statistics for the entire test run.
const TestStats = struct {
    /// Total number of test cases.
    total: usize = 0,
    /// Number of test cases that passed.
    passed: usize = 0,
    /// Number of test cases that failed.
    failed: usize = 0,
    /// Number of test cases that were skipped.
    skipped: usize = 0,
    /// Nanosecond timestamp for when the test run started.
    start_time: i128 = 0,
    /// Nanosecond timestamp for when the test run ended.
    end_time: i128 = 0,

    /// Calculates the total duration of the test run in milliseconds.
    pub fn durationMs(self: *const TestStats) u64 {
        return @intCast(@divTrunc(self.end_time - self.start_time, std.time.ns_per_ms));
    }

    /// Calculates the success rate as a percentage.
    pub fn successRate(self: *const TestStats) f64 {
        if (self.total == 0) return 0;
        return @as(f64, @floatFromInt(self.passed)) / @as(f64, @floatFromInt(self.total)) * 100;
    }
};

/// Helper to allocate source code for our test cases.
const TestData = struct {
    pub fn happyPathRocCode(allocator: std.mem.Allocator) ![]u8 {
        return allocator.dupe(u8,
            \\module [foo]
            \\
            \\foo = "bar"
        );
    }

    pub fn syntaxErrorRocCode(allocator: std.mem.Allocator) ![]u8 {
        return allocator.dupe(u8,
            \\module [main]
            \\main = [1, 2, 3
        );
    }

    pub fn typeErrorRocCode(allocator: std.mem.Allocator) ![]u8 {
        return allocator.dupe(u8,
            \\module [main]
            \\main = "hello" + 123
        );
    }
};

/// Helper to send a message to the WASM Playground and get a response.
fn sendMessageToWasm(wasm_interface: *const WasmInterface, allocator: std.mem.Allocator, message: WasmMessage) !WasmResponse {
    // Serialize message to JSON
    var message_json_buffer = std.array_list.Managed(u8).init(allocator);
    defer message_json_buffer.deinit();
    try std.json.stringify(message, .{}, message_json_buffer.writer());
    const message_json = message_json_buffer.items;

    // Allocate a buffer in WASM for the message.
    // The WASM module's allocateMessageBuffer export handles this.
    var params_alloc: [1]bytebox.Val = undefined;
    var returns_alloc: [1]bytebox.Val = undefined;
    params_alloc[0] = bytebox.Val{ .I32 = @intCast(message_json.len) };
    _ = wasm_interface.module_instance.invoke(wasm_interface.allocateMessageBuffer_handle, &params_alloc, &returns_alloc, .{}) catch |err| {
        logDebug("[ERROR] Error invoking WASM allocateMessageBuffer: {}\n", .{err});
        return error.WasmAllocationFailed;
    };

    const wasm_buffer_ptr_opt = returns_alloc[0].I32;
    if (wasm_buffer_ptr_opt == 0) {
        logDebug("[ERROR] WASM allocateMessageBuffer returned null (allocation failed).\n", .{});
        return error.WasmAllocationFailed;
    }
    const wasm_buffer_ptr: usize = @intCast(wasm_buffer_ptr_opt);

    // Copy the JSON message to the allocated WASM buffer.
    const wasm_memory = wasm_interface.memory.buffer();
    if (wasm_buffer_ptr + message_json.len > wasm_memory.len) {
        logDebug("[ERROR] WASM allocated buffer is out of bounds or too small. ptr: {}, len: {}, mem_size: {}\n", .{ wasm_buffer_ptr, message_json.len, wasm_memory.len });
        // Attempt to free the buffer if possible, though the contract might be broken.
        _ = wasm_interface.module_instance.invoke(wasm_interface.freeMessageBuffer_handle, &[_]bytebox.Val{}, &[_]bytebox.Val{}, .{}) catch {};
        return error.WasmAllocatedBufferInvalid;
    }
    @memcpy(wasm_memory[wasm_buffer_ptr .. wasm_buffer_ptr + message_json.len], message_json);

    // Call the processAndRespond function with the pointer and length of the allocated buffer.
    var params_process: [2]bytebox.Val = undefined;
    var returns_process: [1]bytebox.Val = undefined;
    params_process[0] = bytebox.Val{ .I32 = @intCast(wasm_buffer_ptr) };
    params_process[1] = bytebox.Val{ .I32 = @intCast(message_json.len) };

    _ = wasm_interface.module_instance.invoke(wasm_interface.processAndRespond_handle, &params_process, &returns_process, .{}) catch |err| {
        logDebug("[ERROR] Error invoking WASM processAndRespond: {}\n", .{err});
        logDebug("Printing WASM internal debug log after invocation error:\n", .{});
        printWasmDebugLog(wasm_interface);
        // Important: Try to free the message buffer even if processing failed.
        _ = wasm_interface.module_instance.invoke(wasm_interface.freeMessageBuffer_handle, &[_]bytebox.Val{}, &[_]bytebox.Val{}, .{}) catch {};
        return error.WasmProcessMessageFailed;
    };

    const response_ptr_opt = returns_process[0].I32; // Pointer to null-terminated response string, or 0.

    // Free the WASM-allocated message buffer. It's no longer needed.
    _ = wasm_interface.module_instance.invoke(wasm_interface.freeMessageBuffer_handle, &[_]bytebox.Val{}, &[_]bytebox.Val{}, .{}) catch |err| {
        logDebug("[WARNING] Error invoking WASM freeMessageBuffer: {}\n", .{err});
        // Non-critical for test continuation, but good to know.
    };

    if (response_ptr_opt == 0) {
        logDebug("[ERROR] WASM processAndRespond returned null, indicating an internal error.\n", .{});
        return error.WasmInternalError;
    }

    // Read the null-terminated response string from WASM memory.
    const response_ptr: usize = @intCast(response_ptr_opt);
    if (response_ptr >= wasm_memory.len) {
        logDebug("[ERROR] WASM returned response pointer out of bounds: {}\n", .{response_ptr});
        // Attempt to free the response string if possible.
        _ = wasm_interface.module_instance.invoke(wasm_interface.freeWasmString_handle, &[_]bytebox.Val{bytebox.Val{ .I32 = @intCast(response_ptr) }}, &[_]bytebox.Val{}, .{}) catch {};
        return error.WasmReturnedInvalidPointer;
    }

    const response_slice = wasm_memory[response_ptr..];
    const null_terminator_idx = std.mem.indexOfScalar(u8, response_slice, 0) orelse {
        logDebug("[ERROR] WASM returned response string without a null terminator.\n", .{});
        _ = wasm_interface.module_instance.invoke(wasm_interface.freeWasmString_handle, &[_]bytebox.Val{bytebox.Val{ .I32 = @intCast(response_ptr) }}, &[_]bytebox.Val{}, .{}) catch {};
        return error.WasmReturnedInvalidString;
    };
    const response_json_slice = response_slice[0..null_terminator_idx];

    // Parse JSON response
    const parsed_response = std.json.parseFromSlice(WasmResponse, allocator, response_json_slice, .{
        .allocate = .alloc_always,
    }) catch |err| {
        logDebug("[ERROR] Failed to parse JSON response: {}. JSON was: {s}\n", .{ err, response_json_slice });
        // Free the WASM string before returning error
        _ = wasm_interface.module_instance.invoke(wasm_interface.freeWasmString_handle, &[_]bytebox.Val{bytebox.Val{ .I32 = @intCast(response_ptr) }}, &[_]bytebox.Val{}, .{}) catch {};
        return err;
    };

    // Free the WASM-allocated response string
    _ = wasm_interface.module_instance.invoke(wasm_interface.freeWasmString_handle, &[_]bytebox.Val{bytebox.Val{ .I32 = @intCast(response_ptr) }}, &[_]bytebox.Val{}, .{}) catch |err| {
        logDebug("[ERROR] Error invoking WASM freeWasmString: {}\n", .{err});
    };

    return parsed_response.value;
}

/// Initialize WASM module and interface
///
/// - `gpa` allocator is the GeneralPurposeAllocator, intended for bytebox VM.
/// - `arena` allocator is the ArenaAllocator, used for other test harness allocations.
/// - `wasm_path` is the path to the WASM file to load.
fn setupWasm(gpa: std.mem.Allocator, arena: std.mem.Allocator, wasm_path: []const u8) !WasmInterface {
    const wasm_data: []const u8 = std.fs.cwd().readFileAlloc(arena, wasm_path, std.math.maxInt(usize)) catch |err| {
        logDebug("[ERROR] Failed to read WASM file '{s}': {}\n", .{ wasm_path, err });
        return err;
    };

    // Create and decode the module definition using the arena allocator
    var module_def = try bytebox.createModuleDefinition(arena, .{ .debug_name = "playground_wasm" });
    errdefer module_def.destroy();
    try module_def.decode(wasm_data);

    // Create and instantiate the module instance using the gpa allocator for the VM
    var module_instance = try bytebox.createModuleInstance(.Stack, module_def, gpa);
    errdefer module_instance.destroy();
    try module_instance.instantiate(.{});

    logDebug("[INFO] WASM module instantiated successfully.\n", .{});

    // Get the WASM interface (exports and memory)
    var wasm_interface = WasmInterface{
        .module_def = module_def,
        .module_instance = module_instance,
        .init_handle = try module_instance.getFunctionHandle("init"),
        .processAndRespond_handle = try module_instance.getFunctionHandle("processAndRespond"),
        .freeWasmString_handle = try module_instance.getFunctionHandle("freeWasmString"),
        .allocateMessageBuffer_handle = try module_instance.getFunctionHandle("allocateMessageBuffer"),
        .freeMessageBuffer_handle = try module_instance.getFunctionHandle("freeMessageBuffer"),
        .getDebugLogBuffer_handle = try module_instance.getFunctionHandle("getDebugLogBuffer"),
        .getDebugLogLength_handle = try module_instance.getFunctionHandle("getDebugLogLength"),
        .clearDebugLog_handle = try module_instance.getFunctionHandle("clearDebugLog"),
        .memory = module_instance.store.getMemory(0),
    };

    // Call the WASM init function
    {
        var params: [0]bytebox.Val = undefined;
        var returns: [0]bytebox.Val = undefined;
        _ = wasm_interface.module_instance.invoke(wasm_interface.init_handle, &params, &returns, .{}) catch |err| {
            logDebug("[ERROR] Error calling WASM Init handle {}\n", .{err});
            return error.WasmInitFailed;
        };
    }

    logDebug("[INFO] WASM module initialized.\n", .{});

    return wasm_interface;
}

/// Helper to read and print the debug log from WASM
fn printWasmDebugLog(wasm_interface: *const WasmInterface) void {
    var params_len: [0]bytebox.Val = undefined;
    var returns_len: [1]bytebox.Val = undefined;
    _ = wasm_interface.module_instance.invoke(wasm_interface.getDebugLogLength_handle, &params_len, &returns_len, .{}) catch {
        logDebug("Error invoking WASM getDebugLogLength.\n", .{});
        return;
    };
    const log_len = returns_len[0].I32;

    if (log_len == 0) {
        return;
    }

    var params_ptr: [0]bytebox.Val = undefined;
    var returns_ptr: [1]bytebox.Val = undefined;
    _ = wasm_interface.module_instance.invoke(wasm_interface.getDebugLogBuffer_handle, &params_ptr, &returns_ptr, .{}) catch {
        logDebug("Error invoking WASM getDebugLogBuffer.\n", .{});
        return;
    };
    const log_ptr = returns_ptr[0].I32;

    const wasm_memory = wasm_interface.memory.buffer();
    const log_ptr_usize: usize = @intCast(log_ptr);

    if (log_ptr_usize + @as(usize, @intCast(log_len)) > wasm_memory.len) {
        logDebug("WASM debug log pointer or length out of bounds. ptr: {}, len: {}, mem_size: {}\n", .{ log_ptr_usize, log_len, wasm_memory.len });
        return;
    }

    const log_slice = wasm_memory[log_ptr_usize .. log_ptr_usize + @as(usize, @intCast(log_len))];
    // Print the log slice directly. This avoids:
    // 1. A fixed-size buffer limitation.
    // 2. An unnecessary memory copy.
    // 3. Issues with null bytes within the log content, as {s} prints the exact slice length.
    logDebug("--- WASM Internal Debug Log ---\n{s}\n--- End WASM Internal Debug Log ---\n", .{log_slice});
}

/// An error set for test assertion failures.
const TestAssertionError = error{
    TestAssertionFailed,
};

/// Enhanced Assertion Helpers
fn expectStatus(actual: []const u8, expected: []const u8) TestAssertionError!void {
    std.testing.expectEqualStrings(expected, actual) catch {
        logDebug("[ERROR] Assertion Failed: Expected status '{s}', got '{s}'\n", .{ expected, actual });
        return error.TestAssertionFailed;
    };
}

/// Asserts that the `actual` optional message string contains the `contains` substring.
/// If `actual` is null or does not contain the substring, it logs an error and returns `TestAssertionFailed`.
fn expectMessageContains(actual: ?[]const u8, contains: []const u8) TestAssertionError!void {
    if (actual == null or !std.mem.containsAtLeast(u8, actual.?, 1, contains)) {
        logDebug("[ERROR] Assertion Failed: Expected message to contain '{s}', got '{?s}'\n", .{ contains, actual });
        return error.TestAssertionFailed;
    }
}

/// Asserts that the `actual` optional data string contains the `contains` substring.
/// If `actual` is null or does not contain the substring, it logs an error and returns `TestAssertionFailed`.
fn expectDataContains(actual: ?[]const u8, contains: []const u8) TestAssertionError!void {
    if (actual == null or !std.mem.containsAtLeast(u8, actual.?, 1, contains)) {
        logDebug("[ERROR] Assertion Failed: Expected data to contain '{s}', got '{?s}'\n", .{ contains, actual });
        return error.TestAssertionFailed;
    }
}

/// Asserts that the `actual` optional hover_info contains the `contains` substring.
/// If `actual` is null or does not contain the substring, it logs an error and returns `TestAssertionFailed`.
fn expectHoverInfoContains(actual: ?WasmResponse.HoverInfo, contains: []const u8) TestAssertionError!void {
    if (actual == null) {
        logDebug("[ERROR] Assertion Failed: Expected hover_info to contain '{s}', but hover_info is null\n", .{contains});
        return error.TestAssertionFailed;
    }

    const hover_info = actual.?;
    if (!std.mem.containsAtLeast(u8, hover_info.type_str, 1, contains)) {
        logDebug("[ERROR] Assertion Failed: Expected hover_info.type_str to contain '{s}', got '{s}'\n", .{ contains, hover_info.type_str });
        return error.TestAssertionFailed;
    }
}

/// Asserts that the result output contains a specific substring.
fn expectResultOutputContains(actual: ?WasmResponse.ReplResult, contains: []const u8) TestAssertionError!void {
    if (actual == null) {
        logDebug("[ERROR] Assertion Failed: Expected result.output to contain '{s}', but result is null\n", .{contains});
        return error.TestAssertionFailed;
    }

    const result = actual.?;
    if (!std.mem.containsAtLeast(u8, result.output, 1, contains)) {
        logDebug("[ERROR] Assertion Failed: Expected result.output to contain '{s}', got '{s}'\n", .{ contains, result.output });
        return error.TestAssertionFailed;
    }
}

/// Asserts that the diagnostics in a `WasmResponse` match the `DiagnosticExpectation`.
/// It checks for minimum error/warning counts and for specific substrings in error/warning messages.
fn expectDiagnostics(response: *const WasmResponse, expected: MessageStep.DiagnosticExpectation) TestAssertionError!void {
    const diagnostics = response.diagnostics orelse {
        if (expected.min_errors > 0 or expected.min_warnings > 0) {
            logDebug("[ERROR] Assertion Failed: Expected diagnostics but got none\n", .{});
            return error.TestAssertionFailed;
        }
        return;
    };

    const summary = diagnostics.summary;

    if (summary.errors < expected.min_errors) {
        logDebug("[ERROR] Assertion Failed: Expected at least {} errors, got {}\n", .{ expected.min_errors, summary.errors });
        return error.TestAssertionFailed;
    }

    if (summary.warnings < expected.min_warnings) {
        logDebug("[ERROR] Assertion Failed: Expected at least {} warnings, got {}\n", .{ expected.min_warnings, summary.warnings });
        return error.TestAssertionFailed;
    }

    // Check for specific error messages
    if (expected.error_messages.len > 0) {
        if (diagnostics.list) |list| {
            for (expected.error_messages) |expected_msg| {
                var found = false;
                for (list) |diag| {
                    if (std.mem.eql(u8, diag.severity, "error") and
                        std.mem.containsAtLeast(u8, diag.message, 1, expected_msg))
                    {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    logDebug("[ERROR] Assertion Failed: Expected error message containing '{s}' not found\n", .{expected_msg});
                    return error.TestAssertionFailed;
                }
            }
        } else if (expected.min_errors > 0) { // Only fail if we expected errors but list is null
            logDebug("[ERROR] Assertion Failed: Expected at least {} errors, but diagnostic list is null\n", .{expected.min_errors});
            return error.TestAssertionFailed;
        }
    }

    // Check for specific warning messages
    if (expected.warning_messages.len > 0) {
        if (diagnostics.list) |list| {
            for (expected.warning_messages) |expected_msg| {
                var found = false;
                for (list) |diag| {
                    if (std.mem.eql(u8, diag.severity, "warning") and
                        std.mem.containsAtLeast(u8, diag.message, 1, expected_msg))
                    {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    logDebug("[ERROR] Assertion Failed: Expected warning message containing '{s}' not found\n", .{expected_msg});
                    return error.TestAssertionFailed;
                }
            }
        } else if (expected.min_warnings > 0) { // Only fail if we expected warnings but list is null
            logDebug("[ERROR] Assertion Failed: Expected at least {} warnings, but diagnostic list is null\n", .{expected.min_warnings});
            return error.TestAssertionFailed;
        }
    }
}

// Test runner for a single test case
fn runTestCase(allocator: std.mem.Allocator, wasm_interface: *WasmInterface, test_case: TestCase) StepExecutionResult {
    logDebug("\n--- Running Test Case: {s} ---\n", .{test_case.name});

    const case_start_time = std.time.nanoTimestamp();

    // Setup phase
    if (test_case.setup) |setup_fn| {
        setup_fn(allocator, wasm_interface) catch |err| {
            const msg = std.fmt.allocPrint(allocator, "Setup failed: {}", .{err}) catch "Setup failed (OOM formatting message)";
            logDebug("[ERROR] {s} for test case '{s}'\n", .{ msg, test_case.name });
            return StepExecutionResult{ .result = .failed, .failure_message = msg };
        };
    }

    // Run test steps
    const step_result = runTestSteps(allocator, wasm_interface, test_case);

    // If steps failed, return that result immediately, skipping teardown.
    if (step_result.result != .passed) {
        const case_end_time = std.time.nanoTimestamp();
        const duration_ms: u64 = @intCast(@divTrunc((case_end_time - case_start_time), std.time.ns_per_ms));
        logDebug("--- Test Case {s}: {s} ({}ms) ---\n", .{ @tagName(step_result.result), test_case.name, duration_ms });
        return step_result;
    }

    // Teardown phase only runs if steps passed
    if (test_case.teardown) |teardown_fn| {
        teardown_fn(allocator, wasm_interface) catch |err| {
            const msg = std.fmt.allocPrint(allocator, "Teardown failed: {}", .{err}) catch "Teardown failed (OOM formatting message)";
            logDebug("[ERROR] {s} for test case '{s}'\n", .{ msg, test_case.name });
            const case_end_time = std.time.nanoTimestamp();
            const duration_ms: u64 = @intCast(@divTrunc((case_end_time - case_start_time), std.time.ns_per_ms));
            logDebug("--- Test Case {s}: {s} ({}ms) ---\n", .{ @tagName(.failed), test_case.name, duration_ms });
            return StepExecutionResult{ .result = .failed, .failure_message = msg };
        };
    }

    const case_end_time = std.time.nanoTimestamp();
    const duration_ms: u64 = @intCast(@divTrunc((case_end_time - case_start_time), std.time.ns_per_ms));

    logDebug("--- Test Case {s}: {s} ({}ms) ---\n", .{ @tagName(step_result.result), test_case.name, duration_ms });

    return step_result;
}

fn runTestSteps(allocator: std.mem.Allocator, wasm_interface: *WasmInterface, test_case: TestCase) StepExecutionResult {
    for (test_case.steps, 0..) |step, i| {
        logDebug("  Step {}: Sending {s} message...\n", .{ i + 1, step.message.type });

        if (step.should_fail) {
            // This step is expected to fail
            _ = sendMessageToWasm(wasm_interface, allocator, step.message) catch |err| {
                if (step.expected_error) |expected_err| {
                    if (std.mem.containsAtLeast(u8, @errorName(err), 1, expected_err)) {
                        logDebug("  Step {}: Expected failure occurred: {}\n", .{ i + 1, err });
                        continue;
                    }
                }
                const msg = std.fmt.allocPrint(allocator, "Unexpected error: {}", .{err}) catch "Unexpected error (OOM formatting message)";
                logDebug("  Step {}: {s}\n", .{ i + 1, msg });
                return StepExecutionResult{ .result = .failed, .failure_message = msg };
            };
            const msg = "Expected failure did not occur";
            logDebug("  Step {}: {s}\n", .{ i + 1, msg });
            return StepExecutionResult{ .result = .failed, .failure_message = msg };
        }

        var response = sendMessageToWasm(wasm_interface, allocator, step.message) catch |err| {
            const msg = std.fmt.allocPrint(allocator, "Failed to send message: {}", .{err}) catch "Failed to send message (OOM formatting message)";
            logDebug("  Step {}: {s}\n", .{ i + 1, msg });
            return StepExecutionResult{ .result = .failed, .failure_message = msg };
        };
        // No defer response.deinit() needed, arena handles it.

        // Perform assertions
        if (expectStatus(response.status, step.expected_status)) |_| {} else |_| {
            logDebug("  Step {}: Assertions failed. Printing WASM internal debug log:\n", .{i + 1});
            printWasmDebugLog(wasm_interface);
            return StepExecutionResult{ .result = .failed, .failure_message = "Assertion failed: Status mismatch" };
        }

        if (step.expected_message_contains) |contains| {
            if (expectMessageContains(response.message, contains)) |_| {} else |_| {
                logDebug("  Step {}: Assertions failed. Printing WASM internal debug log:\n", .{i + 1});
                printWasmDebugLog(wasm_interface);
                return StepExecutionResult{ .result = .failed, .failure_message = "Assertion failed: Message content mismatch" };
            }
        }

        if (step.expected_data_contains) |contains| {
            if (expectDataContains(response.data, contains)) |_| {} else |_| {
                logDebug("  Step {}: Assertions failed. Printing WASM internal debug log:\n", .{i + 1});
                printWasmDebugLog(wasm_interface);
                return StepExecutionResult{ .result = .failed, .failure_message = "Assertion failed: Data content mismatch" };
            }
        }

        if (step.expected_hover_info_contains) |contains| {
            if (expectHoverInfoContains(response.hover_info, contains)) |_| {} else |_| {
                logDebug("  Step {}: Assertions failed. Printing WASM internal debug log:\n", .{i + 1});
                printWasmDebugLog(wasm_interface);
                return StepExecutionResult{ .result = .failed, .failure_message = "Assertion failed: Hover info content mismatch" };
            }
        }

        if (step.expected_result_output_contains) |contains| {
            if (expectResultOutputContains(response.result, contains)) |_| {} else |_| {
                logDebug("  Step {}: Assertions failed. Printing WASM internal debug log:\n", .{i + 1});
                printWasmDebugLog(wasm_interface);
                return StepExecutionResult{ .result = .failed, .failure_message = "Assertion failed: REPL result output mismatch" };
            }
        }

        if (step.expected_result_type) |expected_type| {
            if (response.result) |result| {
                if (!std.mem.eql(u8, result.type, expected_type)) {
                    logDebug("  Step {}: Expected result type '{s}', got '{s}'\n", .{ i + 1, expected_type, result.type });
                    return StepExecutionResult{ .result = .failed, .failure_message = "Assertion failed: REPL result type mismatch" };
                }
            } else {
                logDebug("  Step {}: Expected result type '{s}', but result is null\n", .{ i + 1, expected_type });
                return StepExecutionResult{ .result = .failed, .failure_message = "Assertion failed: REPL result is null" };
            }
        }

        if (step.expected_result_error_stage) |expected_stage| {
            if (response.result) |result| {
                if (result.error_stage) |stage| {
                    if (!std.mem.eql(u8, stage, expected_stage)) {
                        logDebug("  Step {}: Expected error stage '{s}', got '{s}'\n", .{ i + 1, expected_stage, stage });
                        return StepExecutionResult{ .result = .failed, .failure_message = "Assertion failed: REPL error stage mismatch" };
                    }
                } else {
                    logDebug("  Step {}: Expected error stage '{s}', but error_stage is null\n", .{ i + 1, expected_stage });
                    return StepExecutionResult{ .result = .failed, .failure_message = "Assertion failed: REPL error_stage is null" };
                }
            } else {
                logDebug("  Step {}: Expected error stage '{s}', but result is null\n", .{ i + 1, expected_stage });
                return StepExecutionResult{ .result = .failed, .failure_message = "Assertion failed: REPL result is null" };
            }
        }

        if (step.expected_diagnostics) |expected_diag| {
            if (expectDiagnostics(&response, expected_diag)) |_| {} else |_| {
                logDebug("  Step {}: Assertions failed. Printing WASM internal debug log:\n", .{i + 1});
                printWasmDebugLog(wasm_interface);
                return StepExecutionResult{ .result = .failed, .failure_message = "Assertion failed: Diagnostics mismatch" };
            }
        }

        if (std.mem.eql(u8, step.message.type, "LOAD_SOURCE")) {
            if (response.diagnostics) |diag| {
                logDebug("  Step {}: LOAD_SOURCE successful. Status: {s}. Diagnostics: {} errors, {} warnings.\n", .{ i + 1, response.status, diag.summary.errors, diag.summary.warnings });
            } else {
                logDebug("  Step {}: LOAD_SOURCE successful. Status: {s}. No diagnostics.\n", .{ i + 1, response.status });
            }
        } else if (std.mem.eql(u8, step.message.type, "QUERY_TYPES")) {
            if (response.data) |data| {
                if (std.mem.containsAtLeast(u8, data, 1, "inferred-types")) {
                    logDebug("  Step {}: QUERY_TYPES successful. Status: {s}. Type information retrieved.\n", .{ i + 1, response.status });
                } else {
                    // This case might indicate an issue or an unexpected response format
                    logDebug("  Step {}: QUERY_TYPES successful. Status: {s}. Data: {s}\n", .{ i + 1, response.status, data });
                }
            } else {
                logDebug("  Step {}: QUERY_TYPES successful. Status: {s}. No type data returned.\n", .{ i + 1, response.status });
            }
        } else if (std.mem.eql(u8, step.message.type, "QUERY_FORMATTED")) {
            if (response.data) |data| {
                if (data.len > 0) {
                    logDebug("  Step {}: QUERY_FORMATTED successful. Status: {s}. Formatted code retrieved ({} chars).\n", .{ i + 1, response.status, data.len });
                } else {
                    logDebug("  Step {}: QUERY_FORMATTED successful. Status: {s}. Empty formatted code.\n", .{ i + 1, response.status });
                }
            } else {
                logDebug("  Step {}: QUERY_FORMATTED successful. Status: {s}. No formatted data returned.\n", .{ i + 1, response.status });
            }
        } else {
            logDebug("  Step {}: {s} successful. Status: {s}, Message: {?s}\n", .{ i + 1, step.message.type, response.status, response.message });
        }

        // Clean up owned_source if present
        if (step.owned_source) |owned| {
            allocator.free(owned);
        }
    }

    return StepExecutionResult{ .result = .passed };
}

// Main test runner with statistics and filtering
// - `arena` allocator is for test harness allocations.
// - `gpa` allocator is for the bytebox VM.
// - `wasm_path` is the path to the WASM file to load.
fn runTests(arena: std.mem.Allocator, gpa: std.mem.Allocator, test_cases: []const TestCase, wasm_path: []const u8) !TestStats {
    var stats = TestStats{
        .total = test_cases.len,
        .start_time = std.time.nanoTimestamp(),
        .passed = 0,
        .failed = 0,
        .skipped = 0,
    };

    var failures = std.array_list.Managed(TestFailure).init(arena);
    defer failures.deinit();

    for (test_cases) |case| {
        logDebug("\n[INFO] Setting up WASM interface for test case: {s}...\n", .{case.name});
        var wasm_interface = setupWasm(gpa, arena, wasm_path) catch |err| {
            logDebug("[ERROR] Failed to setup WASM for test case '{s}': {}\n", .{ case.name, err });
            stats.failed += 1;
            try failures.append(.{
                .case_name = case.name,
                .step_index = 0,
                .message = "WASM setup failed",
            });
            continue;
        };
        defer wasm_interface.deinit();

        const case_execution_result = runTestCase(arena, &wasm_interface, case);

        switch (case_execution_result.result) {
            .passed => stats.passed += 1,
            .failed => {
                stats.failed += 1;
                const failure_msg = case_execution_result.failure_message orelse "Test failed";
                try failures.append(.{
                    .case_name = case.name,
                    .step_index = 0, // Could be enhanced to track specific step
                    .message = failure_msg,
                });
            },
            .skipped => stats.skipped += 1,
        }
    }

    stats.end_time = std.time.nanoTimestamp();

    // Print summary
    logDebug("\n=== Test Summary ===\n", .{});
    logDebug("Total: {}, Passed: {}, Failed: {}, Skipped: {}\n", .{ stats.total, stats.passed, stats.failed, stats.skipped });
    logDebug("Success Rate: {d:.0}%\n", .{stats.successRate()});
    logDebug("Duration: {}ms\n", .{stats.durationMs()});

    if (stats.failed > 0) {
        logDebug("\n=== Failures ===\n", .{});
        for (failures.items) |failure| {
            logDebug("- {s}: {s}\n", .{ failure.case_name, failure.message });
        }
    }

    return stats;
}

fn teardownResetState(allocator: std.mem.Allocator, wasm_interface: *WasmInterface) !void {
    const message = WasmMessage{ .type = "RESET" };
    const response = try sendMessageToWasm(wasm_interface, allocator, message);
    // No defer response.deinit(); needed.

    try expectStatus(response.status, "SUCCESS");
}

// Helper function to create a simple test case with INIT, LOAD_SOURCE, and optional QUERY_TYPES
fn createSimpleTest(allocator: std.mem.Allocator, name: []const u8, code: []const u8, expected_diag: ?MessageStep.DiagnosticExpectation, query_types: bool) !TestCase {
    var steps = try allocator.alloc(MessageStep, if (query_types) 3 else 2);
    steps[0] = .{ .message = .{ .type = "INIT" }, .expected_status = "SUCCESS" };
    steps[1] = .{
        .message = .{ .type = "LOAD_SOURCE", .source = code },
        .expected_status = "SUCCESS",
        .expected_message_contains = "LOADED",
        .expected_diagnostics = expected_diag,
        .owned_source = code,
    };
    if (query_types) {
        steps[2] = .{ .message = .{ .type = "QUERY_TYPES" }, .expected_status = "SUCCESS" };
    }
    return TestCase{ .name = name, .steps = steps };
}

pub fn main() !void {
    // Setup gpa allocator used for bytebox WASM VM
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    // Setup arena allocator usrd for test harness
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    const allocator = arena.allocator();

    const stdout = std.io.getStdOut();

    // Handle CLI arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var wasm_path: ?[]const u8 = null;
    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--verbose")) {
            verbose_mode = true;
        } else if (std.mem.eql(u8, arg, "--help")) {
            try stdout.writer().print("Usage: playground-test [options] [wasm-path]\n", .{});
            try stdout.writer().print("Options:\n", .{});
            try stdout.writer().print("  --verbose           Enable verbose mode\n", .{});
            try stdout.writer().print("  --wasm-path PATH    Path to the playground WASM file\n", .{});
            try stdout.writer().print("  --help              Display this help message\n", .{});
            return;
        } else if (std.mem.eql(u8, arg, "--wasm-path")) {
            i += 1;
            if (i >= args.len) {
                try stdout.writer().print("Error: --wasm-path requires a path argument\n", .{});
                return;
            }
            wasm_path = args[i];
        } else if (!std.mem.startsWith(u8, arg, "--")) {
            // Positional argument - treat as WASM path
            wasm_path = arg;
        }
    }

    // Default WASM path if not provided
    const playground_wasm_path = wasm_path orelse "zig-out/bin/playground.wasm";

    // Setup our test cases
    var test_cases = std.array_list.Managed(TestCase).init(allocator);
    defer test_cases.deinit(); // This will free the TestCase structs and their `steps` slices.

    // Functional Test
    var happy_path_steps = try allocator.alloc(MessageStep, 8);
    // Check that INIT returns the compiler version (both test and WASM are built with same options)
    happy_path_steps[0] = .{ .message = .{ .type = "INIT" }, .expected_status = "SUCCESS", .expected_message_contains = build_options.compiler_version };
    const happy_path_code = try TestData.happyPathRocCode(allocator);
    happy_path_steps[1] = .{
        .message = .{ .type = "LOAD_SOURCE", .source = happy_path_code },
        .expected_status = "SUCCESS",
        .expected_message_contains = "LOADED",
        .expected_diagnostics = .{ .min_errors = 0, .min_warnings = 0 },
        .owned_source = happy_path_code,
    };
    happy_path_steps[2] = .{
        .message = .{ .type = "QUERY_TOKENS" },
        .expected_status = "SUCCESS",
        .expected_data_contains = "token-list",
    };
    happy_path_steps[3] = .{
        .message = .{ .type = "QUERY_AST" },
        .expected_status = "SUCCESS",
        .expected_data_contains = "<span class=\"token-punctuation\">",
    };
    happy_path_steps[4] = .{
        .message = .{ .type = "QUERY_CIR" },
        .expected_status = "SUCCESS",
        .expected_data_contains = "can-ir",
    };
    happy_path_steps[5] = .{
        .message = .{ .type = "QUERY_TYPES" },
        .expected_status = "SUCCESS",
        .expected_data_contains = "inferred-types",
    };
    happy_path_steps[6] = .{
        .message = .{ .type = "QUERY_FORMATTED" },
        .expected_status = "SUCCESS",
        .expected_data_contains = "foo",
    };
    happy_path_steps[7] = .{
        .message = .{ .type = "GET_HOVER_INFO", .identifier = "foo", .line = 3, .ch = 1 },
        .expected_status = "SUCCESS",
        .expected_hover_info_contains = "Str",
    };
    try test_cases.append(.{
        .name = "Happy Path - Simple Roc Program",
        .steps = happy_path_steps,
    });

    // Error Handling Test
    const syntax_error_code_val = try TestData.syntaxErrorRocCode(allocator);
    try test_cases.append(try createSimpleTest(allocator, "Syntax Error - Mismatched Braces", syntax_error_code_val, .{ .min_errors = 1, .error_messages = &.{"LIST NOT CLOSED"} }, true));

    const type_error_code_val = try TestData.typeErrorRocCode(allocator);
    try test_cases.append(try createSimpleTest(allocator, "Type Error - Adding String and Number", type_error_code_val, .{ .min_errors = 1, .error_messages = &.{"TYPE MISMATCH"} }, true));

    // Empty Source Test
    const empty_source_code = try allocator.dupe(u8, "");
    try test_cases.append(try createSimpleTest(allocator, "Empty Source Code", empty_source_code, null, false)); // Disable diagnostic expectations

    // Code Formatting Test
    var formatted_test_steps = try allocator.alloc(MessageStep, 3);
    formatted_test_steps[0] = .{ .message = .{ .type = "INIT" }, .expected_status = "SUCCESS" };
    const unformatted_code = try allocator.dupe(u8, "module [foo]\n\nfoo=42\nbar=\"hello world\"\n");
    formatted_test_steps[1] = .{
        .message = .{ .type = "LOAD_SOURCE", .source = unformatted_code },
        .expected_status = "SUCCESS",
        .expected_message_contains = "LOADED",
        .owned_source = unformatted_code,
    };
    formatted_test_steps[2] = .{
        .message = .{ .type = "QUERY_FORMATTED" },
        .expected_status = "SUCCESS",
        .expected_data_contains = "foo",
    };
    try test_cases.append(.{
        .name = "QUERY_FORMATTED - Code Formatting",
        .steps = formatted_test_steps,
    });

    // Invalid Message Type Test
    var invalid_msg_type_steps = try allocator.alloc(MessageStep, 2);
    invalid_msg_type_steps[0] = .{ .message = .{ .type = "INIT" }, .expected_status = "SUCCESS" };
    invalid_msg_type_steps[1] = .{
        .message = .{ .type = "NOT_A_REAL_MESSAGE_TYPE" },
        .expected_status = "INVALID_MESSAGE",
        .expected_message_contains = "Unknown message type",
    };
    try test_cases.append(.{
        .name = "Invalid Message Type",
        .steps = invalid_msg_type_steps,
    });

    // RESET Functionality Test
    var reset_test_steps = try allocator.alloc(MessageStep, 5);
    reset_test_steps[0] = .{ .message = .{ .type = "INIT" }, .expected_status = "SUCCESS" };
    const code_for_reset_test = try TestData.syntaxErrorRocCode(allocator);
    reset_test_steps[1] = .{
        .message = .{ .type = "LOAD_SOURCE", .source = code_for_reset_test },
        .expected_status = "SUCCESS",
        .expected_diagnostics = .{ .min_errors = 1, .error_messages = &.{"LIST NOT CLOSED"} },
        .owned_source = code_for_reset_test,
    };
    reset_test_steps[2] = .{ .message = .{ .type = "RESET" }, .expected_status = "SUCCESS" }; // Perform reset
    const happy_code_after_reset = try TestData.happyPathRocCode(allocator);
    reset_test_steps[3] = .{
        .message = .{ .type = "LOAD_SOURCE", .source = happy_code_after_reset },
        .expected_status = "SUCCESS",
        .expected_diagnostics = .{ .min_errors = 0, .min_warnings = 0 }, // Expect no errors from the new code
        .owned_source = happy_code_after_reset,
    };
    reset_test_steps[4] = .{ .message = .{ .type = "QUERY_TYPES" }, .expected_status = "SUCCESS", .expected_data_contains = "inferred-types" };
    try test_cases.append(.{
        .name = "RESET Message Functionality",
        .steps = reset_test_steps,
    });

    // Memory Corruption on Reset Test
    var memory_corruption_steps = try allocator.alloc(MessageStep, 4);
    memory_corruption_steps[0] = .{ .message = .{ .type = "INIT" }, .expected_status = "SUCCESS" };
    const code_for_mem_test_1 = try TestData.happyPathRocCode(allocator);
    memory_corruption_steps[1] = .{
        .message = .{ .type = "LOAD_SOURCE", .source = code_for_mem_test_1 },
        .expected_status = "SUCCESS",
        .owned_source = code_for_mem_test_1,
    };
    memory_corruption_steps[2] = .{ .message = .{ .type = "RESET" }, .expected_status = "SUCCESS" };
    const code_for_mem_test_2 = try TestData.syntaxErrorRocCode(allocator);
    memory_corruption_steps[3] = .{
        .message = .{ .type = "LOAD_SOURCE", .source = code_for_mem_test_2 },
        .expected_status = "SUCCESS",
        .expected_diagnostics = .{ .min_errors = 1, .error_messages = &.{"LIST NOT CLOSED"} },
        .owned_source = code_for_mem_test_2,
    };
    try test_cases.append(.{
        .name = "Memory Corruption on Reset - Load, Reset, Load",
        .steps = memory_corruption_steps,
    });

    // GET_HOVER_INFO Specific Test
    var get_hover_info_steps = try allocator.alloc(MessageStep, 3);
    get_hover_info_steps[0] = .{ .message = .{ .type = "INIT" }, .expected_status = "SUCCESS" };
    const get_hover_info_code = try allocator.dupe(u8,
        \\module [main]
        \\
        \\main : Str
        \\main = "hello"
        \\
        \\num : I32
        \\num = 123
    );
    get_hover_info_steps[1] = .{
        .message = .{ .type = "LOAD_SOURCE", .source = get_hover_info_code },
        .expected_status = "SUCCESS",
        .expected_diagnostics = .{ .min_errors = 0, .min_warnings = 0 },
        .owned_source = get_hover_info_code,
    };
    get_hover_info_steps[2] = .{
        .message = .{ .type = "GET_HOVER_INFO", .identifier = "num", .line = 7, .ch = 1 },
        .expected_status = "SUCCESS",
        .expected_hover_info_contains = "I32",
    };
    try test_cases.append(.{
        .name = "GET_HOVER_INFO - Specific Type Query",
        .steps = get_hover_info_steps,
    });

    // ====== REPL Test Cases ======

    // Test: REPL Lifecycle - Init, Step, Clear, Reset
    var repl_lifecycle_steps = try allocator.alloc(MessageStep, 5);
    repl_lifecycle_steps[0] = .{ .message = .{ .type = "INIT" }, .expected_status = "SUCCESS" };
    repl_lifecycle_steps[1] = .{ .message = .{ .type = "INIT_REPL" }, .expected_status = "SUCCESS", .expected_message_contains = "REPL initialized" };
    repl_lifecycle_steps[2] = .{ .message = .{ .type = "REPL_STEP", .input = "x = 42" }, .expected_status = "SUCCESS", .expected_result_output_contains = "assigned `x`" };
    repl_lifecycle_steps[3] = .{ .message = .{ .type = "CLEAR_REPL" }, .expected_status = "SUCCESS", .expected_message_contains = "REPL cleared" };
    repl_lifecycle_steps[4] = .{ .message = .{ .type = "RESET" }, .expected_status = "SUCCESS" };

    try test_cases.append(.{
        .name = "REPL Lifecycle - Init, Step, Clear, Reset",
        .steps = repl_lifecycle_steps,
    });

    // Test: REPL Core - Definitions and Expressions
    var repl_core_steps = try allocator.alloc(MessageStep, 6);
    repl_core_steps[0] = .{ .message = .{ .type = "INIT" }, .expected_status = "SUCCESS" };
    repl_core_steps[1] = .{ .message = .{ .type = "INIT_REPL" }, .expected_status = "SUCCESS" };
    repl_core_steps[2] = .{ .message = .{ .type = "REPL_STEP", .input = "x = 10" }, .expected_status = "SUCCESS", .expected_result_output_contains = "assigned `x`" };
    repl_core_steps[3] = .{ .message = .{ .type = "REPL_STEP", .input = "y = x + 5" }, .expected_status = "SUCCESS", .expected_result_output_contains = "assigned `y`" };
    repl_core_steps[4] = .{ .message = .{ .type = "REPL_STEP", .input = "y" }, .expected_status = "SUCCESS", .expected_result_output_contains = "15" };
    repl_core_steps[5] = .{ .message = .{ .type = "RESET" }, .expected_status = "SUCCESS" };

    try test_cases.append(.{
        .name = "REPL Core - Definitions and Expressions",
        .steps = repl_core_steps,
    });

    // Test: REPL Variable Redefinition - Dependency Updates
    var repl_redefinition_steps = try allocator.alloc(MessageStep, 8);
    repl_redefinition_steps[0] = .{ .message = .{ .type = "INIT" }, .expected_status = "SUCCESS" };
    repl_redefinition_steps[1] = .{ .message = .{ .type = "INIT_REPL" }, .expected_status = "SUCCESS" };
    repl_redefinition_steps[2] = .{ .message = .{ .type = "REPL_STEP", .input = "x = 10" }, .expected_status = "SUCCESS" };
    repl_redefinition_steps[3] = .{ .message = .{ .type = "REPL_STEP", .input = "y = x + 5" }, .expected_status = "SUCCESS" };
    repl_redefinition_steps[4] = .{ .message = .{ .type = "REPL_STEP", .input = "y" }, .expected_status = "SUCCESS", .expected_result_output_contains = "15" };
    repl_redefinition_steps[5] = .{ .message = .{ .type = "REPL_STEP", .input = "x = 20" }, .expected_status = "SUCCESS" };
    repl_redefinition_steps[6] = .{
        .message = .{ .type = "REPL_STEP", .input = "y" },
        .expected_status = "SUCCESS",
        .expected_result_output_contains = "25", // Should reflect new x value
    };
    repl_redefinition_steps[7] = .{ .message = .{ .type = "RESET" }, .expected_status = "SUCCESS" };

    try test_cases.append(.{
        .name = "REPL Variable Redefinition - Dependency Updates",
        .steps = repl_redefinition_steps,
    });

    // Test: REPL Error Handling - Invalid Syntax Recovery
    var repl_error_steps = try allocator.alloc(MessageStep, 6);
    repl_error_steps[0] = .{ .message = .{ .type = "INIT" }, .expected_status = "SUCCESS" };
    repl_error_steps[1] = .{ .message = .{ .type = "INIT_REPL" }, .expected_status = "SUCCESS" };
    repl_error_steps[2] = .{ .message = .{ .type = "REPL_STEP", .input = "x = 42" }, .expected_status = "SUCCESS" };
    repl_error_steps[3] = .{
        .message = .{ .type = "REPL_STEP", .input = "x +" }, // Invalid syntax - incomplete expression
        .expected_status = "SUCCESS",
        .expected_result_output_contains = "Evaluation error",
        .expected_result_type = "error",
        .expected_result_error_stage = "evaluation",
    };
    repl_error_steps[4] = .{
        .message = .{ .type = "REPL_STEP", .input = "x" }, // Should still work
        .expected_status = "SUCCESS",
        .expected_result_output_contains = "42", // Previous definition should still be valid
    };
    repl_error_steps[5] = .{ .message = .{ .type = "RESET" }, .expected_status = "SUCCESS" };

    try test_cases.append(.{
        .name = "REPL Error Handling - Invalid Syntax Recovery",
        .steps = repl_error_steps,
    });

    // Test: REPL Compiler Integration - Query After Evaluation
    var repl_compiler_steps = try allocator.alloc(MessageStep, 5);
    repl_compiler_steps[0] = .{ .message = .{ .type = "INIT" }, .expected_status = "SUCCESS" };
    repl_compiler_steps[1] = .{ .message = .{ .type = "INIT_REPL" }, .expected_status = "SUCCESS" };
    repl_compiler_steps[2] = .{ .message = .{ .type = "REPL_STEP", .input = "x = 42" }, .expected_status = "SUCCESS" };
    repl_compiler_steps[3] = .{ .message = .{ .type = "REPL_STEP", .input = "x + 10" }, .expected_status = "SUCCESS", .expected_result_output_contains = "52" };
    repl_compiler_steps[4] = .{
        .message = .{ .type = "QUERY_CIR" }, // Should work in REPL mode
        .expected_status = "SUCCESS",
        .expected_data_contains = "can-ir",
    };

    try test_cases.append(.{
        .name = "REPL Compiler Integration - Query After Evaluation",
        .steps = repl_compiler_steps,
    });

    // Test: REPL State Isolation - Mode Switching
    var repl_isolation_steps = try allocator.alloc(MessageStep, 6);
    repl_isolation_steps[0] = .{ .message = .{ .type = "INIT" }, .expected_status = "SUCCESS" };
    repl_isolation_steps[1] = .{ .message = .{ .type = "INIT_REPL" }, .expected_status = "SUCCESS" };
    repl_isolation_steps[2] = .{ .message = .{ .type = "REPL_STEP", .input = "x = 42" }, .expected_status = "SUCCESS" };
    repl_isolation_steps[3] = .{ .message = .{ .type = "RESET" }, .expected_status = "SUCCESS" };
    // After reset, should be back to single-file mode
    const simple_source = try TestData.happyPathRocCode(allocator);
    repl_isolation_steps[4] = .{
        .message = .{ .type = "LOAD_SOURCE", .source = simple_source },
        .expected_status = "SUCCESS",
        .expected_message_contains = "LOADED",
        .owned_source = simple_source,
    };
    repl_isolation_steps[5] = .{ .message = .{ .type = "QUERY_TYPES" }, .expected_status = "SUCCESS" };

    try test_cases.append(.{
        .name = "REPL State Isolation - Mode Switching",
        .steps = repl_isolation_steps,
    });

    logDebug("[INFO] Starting Playground Integration Tests...\n", .{});
    logDebug("[INFO] Running {} test cases\n", .{test_cases.items.len});

    const stats = try runTests(allocator, gpa.allocator(), test_cases.items, playground_wasm_path);

    logDebug("\nAll Playground Integration Tests Completed!\n", .{});
    logDebug("Final Results: {}/{} passed ({d:0.}%)\n", .{ stats.passed, stats.total, stats.successRate() });

    // Exit with error if any tests failed
    if (stats.failed > 0) {
        return error.TestsFailed;
    }
}
