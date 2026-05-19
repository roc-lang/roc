//! Testable REPL session core used by the CLI.
//!
//! The command-line entrypoint owns terminal I/O. This module owns statement
//! splitting, definition replacement, source construction, and evaluation
//! through the current checked-artifact eval helpers.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const eval = @import("eval");
const parse = @import("parse");

const Allocator = std.mem.Allocator;
const Allocators = base.Allocators;
const ModuleEnv = can.ModuleEnv;

const ReplSession = @This();

allocator: Allocator,
backend_kind: eval.EvalBackend,
definitions: DefinitionStore,

pub fn init(allocator: Allocator, backend_kind: eval.EvalBackend) ReplSession {
    return .{
        .allocator = allocator,
        .backend_kind = backend_kind,
        .definitions = DefinitionStore.init(),
    };
}

pub fn deinit(self: *ReplSession) void {
    self.definitions.deinit(self.allocator);
}

/// Process one complete REPL statement or command and return the user-visible output.
pub fn step(self: *ReplSession, input: []const u8) ![]u8 {
    const line = std.mem.trim(u8, input, " \t\r\n");
    if (line.len == 0) return self.allocator.dupe(u8, "");

    if (std.mem.eql(u8, line, ":help")) return self.helpText();
    if (std.mem.eql(u8, line, ":exit") or
        std.mem.eql(u8, line, ":quit") or
        std.mem.eql(u8, line, "exit"))
    {
        return self.allocator.dupe(u8, "Goodbye!");
    }

    const input_info = try self.classifyInput(line) orelse {
        return self.allocator.dupe(u8, "Parse error");
    };

    switch (input_info.kind) {
        .expression => return self.evaluateExpression(line),
        .definition => {
            const name = input_info.name orelse line;
            if (input_info.definition_kind == .annotation) {
                try self.addOrReplaceDefinition(line, name, .annotation);
                return self.allocator.dupe(u8, "");
            }

            var snapshot = try self.definitions.snapshot(self.allocator);
            errdefer snapshot.deinit(self.allocator);
            try self.addOrReplaceDefinition(line, name, input_info.definition_kind);
            const valid = self.validateDefinitions() catch false;
            if (!valid) {
                self.definitions.restore(self.allocator, &snapshot);
                return self.allocator.dupe(u8, "Definition failed to compile");
            }
            const message = try std.fmt.allocPrint(self.allocator, "assigned `{s}`", .{name});
            snapshot.deinit(self.allocator);
            return message;
        },
    }
}

/// Split pasted input into complete REPL statements using the parser as the boundary check.
pub fn splitInputIntoStatements(self: *ReplSession, input: []const u8) ![][]const u8 {
    const trimmed_input = std.mem.trim(u8, input, " \t\r\n");
    if (trimmed_input.len == 0) return self.allocator.alloc([]const u8, 0);
    if (isSpecialCommand(trimmed_input)) {
        const out = try self.allocator.alloc([]const u8, 1);
        out[0] = try self.allocator.dupe(u8, trimmed_input);
        return out;
    }

    var result = std.ArrayList([]const u8).empty;
    errdefer {
        for (result.items) |slice| self.allocator.free(slice);
        result.deinit(self.allocator);
    }

    var current = std.ArrayList(u8).empty;
    defer current.deinit(self.allocator);

    var lines = std.mem.splitScalar(u8, input, '\n');
    while (lines.next()) |raw_line| {
        const trimmed_line = std.mem.trimRight(u8, raw_line, " \t\r");
        if (std.mem.trim(u8, trimmed_line, " \t\r\n").len == 0 and current.items.len == 0) {
            continue;
        }

        if (current.items.len > 0) try current.append(self.allocator, '\n');
        try current.appendSlice(self.allocator, trimmed_line);

        const candidate = std.mem.trim(u8, current.items, " \t\r\n");
        if (candidate.len == 0) continue;
        if (try self.classifyInput(candidate) != null) {
            try result.append(self.allocator, try self.allocator.dupe(u8, candidate));
            current.clearRetainingCapacity();
        }
    }

    const remaining = std.mem.trim(u8, current.items, " \t\r\n");
    if (remaining.len > 0) {
        try result.append(self.allocator, try self.allocator.dupe(u8, remaining));
    }

    return result.toOwnedSlice(self.allocator);
}

/// Free slices returned by `splitInputIntoStatements`.
pub fn freeStatementSlices(self: *ReplSession, slices: []const []const u8) void {
    for (slices) |slice| self.allocator.free(slice);
    self.allocator.free(slices);
}

/// Add or replace one stored definition while preserving definition order.
pub fn addOrReplaceDefinition(self: *ReplSession, source: []const u8, name: []const u8, kind: DefinitionKind) !void {
    try self.definitions.addOrReplace(self.allocator, source, name, kind);
}

/// Build a block expression containing all current definitions followed by `expr`.
pub fn buildFullSource(self: *const ReplSession, expr: []const u8) ![]u8 {
    var out = std.ArrayList(u8).empty;
    errdefer out.deinit(self.allocator);

    try out.appendSlice(self.allocator, "{\n");
    for (self.definitions.items.items) |definition| {
        var lines = std.mem.splitScalar(u8, definition.source, '\n');
        while (lines.next()) |line| {
            try out.appendSlice(self.allocator, "    ");
            try out.appendSlice(self.allocator, line);
            try out.append(self.allocator, '\n');
        }
    }
    try out.appendSlice(self.allocator, "    ");
    try out.appendSlice(self.allocator, expr);
    try out.appendSlice(self.allocator, "\n}");

    return out.toOwnedSlice(self.allocator);
}

/// Build module-level source for the current stored definitions.
pub fn definitionsSource(self: *const ReplSession) ![]u8 {
    var out = std.ArrayList(u8).empty;
    errdefer out.deinit(self.allocator);

    for (self.definitions.items.items) |definition| {
        try out.appendSlice(self.allocator, definition.source);
        try out.append(self.allocator, '\n');
    }

    return out.toOwnedSlice(self.allocator);
}

fn helpText(self: *ReplSession) ![]u8 {
    return self.allocator.dupe(u8,
        \\Enter an expression or definition.
        \\
        \\Commands:
        \\  :help    Show this help
        \\  :exit    Exit the REPL
        \\  :quit    Exit the REPL
        \\
    );
}

fn validateDefinitions(self: *ReplSession) !bool {
    const definitions = try self.definitionsSource();
    defer self.allocator.free(definitions);

    const source = try std.fmt.allocPrint(self.allocator, "{s}\nmain = \"\"\n", .{definitions});
    defer self.allocator.free(source);

    const parsed = eval.test_helpers.parseAndCanonicalizeProgramPublishedRoots(self.allocator, .module, source, &.{}) catch {
        return false;
    };
    defer eval.test_helpers.cleanupParseAndCanonical(self.allocator, parsed);
    return true;
}

fn evaluateExpression(self: *ReplSession, expr: []const u8) ![]u8 {
    const definitions = try self.definitionsSource();
    defer self.allocator.free(definitions);

    const source = try std.fmt.allocPrint(self.allocator, "{s}\nmain = {s}\n", .{ definitions, expr });
    defer self.allocator.free(source);

    const target_usize: base.target.TargetUsize = switch (self.backend_kind) {
        .interpreter, .dev, .llvm => .native,
        .wasm => .u32,
    };
    var compiled = try eval.test_helpers.compileInspectedProgramForTarget(self.allocator, .module, source, &.{}, target_usize);
    defer compiled.deinit(self.allocator);

    return switch (self.backend_kind) {
        .interpreter => eval.test_helpers.lirInterpreterInspectedStr(self.allocator, &compiled.lowered),
        .dev, .llvm => eval.test_helpers.devEvaluatorInspectedStr(self.allocator, &compiled.lowered),
        .wasm => eval.test_helpers.wasmEvaluatorInspectedStr(self.allocator, &compiled.lowered),
    };
}

const InputKind = enum {
    definition,
    expression,
};

/// Distinguishes declarations that can share a name in the REPL definition store.
pub const DefinitionKind = enum {
    value,
    annotation,
    type_decl,
    import,
};

const InputInfo = struct {
    kind: InputKind,
    definition_kind: DefinitionKind = .value,
    name: ?[]const u8 = null,
};

fn classifyInput(self: *ReplSession, line: []const u8) !?InputInfo {
    var env = try ModuleEnv.init(self.allocator, line);
    defer env.deinit();
    env.common.source = line;
    try env.common.calcLineStarts(self.allocator);

    var allocators: Allocators = undefined;
    allocators.initInPlace(self.allocator);
    defer allocators.deinit();

    const ast = parse.parseStatement(&allocators, &env.common) catch return null;
    defer ast.deinit();
    if (ast.tokenize_diagnostics.items.len > 0 or ast.parse_diagnostics.items.len > 0) return null;

    const statement = ast.store.getStatement(@enumFromInt(ast.root_node_idx));
    return switch (statement) {
        .expr,
        .crash,
        .dbg,
        .expect,
        .@"for",
        .@"while",
        .@"return",
        .@"break",
        => .{ .kind = .expression },
        .decl => |decl| .{
            .kind = .definition,
            .definition_kind = .value,
            .name = declarationName(ast, decl.pattern),
        },
        .@"var" => |v| .{
            .kind = .definition,
            .definition_kind = .value,
            .name = ast.resolve(v.name),
        },
        .type_anno => |anno| .{
            .kind = .definition,
            .definition_kind = .annotation,
            .name = ast.resolve(anno.name),
        },
        .type_decl => |decl| blk: {
            const header = ast.store.getTypeHeader(decl.header) catch break :blk .{
                .kind = .definition,
                .definition_kind = .type_decl,
                .name = null,
            };
            break :blk .{
                .kind = .definition,
                .definition_kind = .type_decl,
                .name = ast.resolve(header.name),
            };
        },
        .import => |import| .{
            .kind = .definition,
            .definition_kind = .import,
            .name = if (import.alias_tok) |tok| ast.resolve(tok) else ast.resolve(import.module_name_tok),
        },
        .file_import => |file_import| .{
            .kind = .definition,
            .definition_kind = .import,
            .name = ast.resolve(file_import.name_tok),
        },
        .malformed => null,
    };
}

fn declarationName(ast: *const parse.AST, pattern_idx: parse.AST.Pattern.Idx) ?[]const u8 {
    const pattern = ast.store.getPattern(pattern_idx);
    return switch (pattern) {
        .ident => |ident| ast.resolve(ident.ident_tok),
        .var_ident => |ident| ast.resolve(ident.ident_tok),
        .as => |as_pattern| ast.resolve(as_pattern.name),
        else => null,
    };
}

fn isSpecialCommand(line: []const u8) bool {
    return std.mem.startsWith(u8, line, ":") or std.mem.eql(u8, line, "exit");
}

const Definition = struct {
    name: []u8,
    source: []u8,
    kind: DefinitionKind,

    fn deinit(self: *Definition, allocator: Allocator) void {
        allocator.free(self.name);
        allocator.free(self.source);
        self.* = undefined;
    }
};

/// Ordered REPL definition collection with same-name replacement by definition kind.
pub const DefinitionStore = struct {
    items: std.ArrayList(Definition),

    fn init() DefinitionStore {
        return .{ .items = .empty };
    }

    fn deinit(self: *DefinitionStore, allocator: Allocator) void {
        for (self.items.items) |*definition| definition.deinit(allocator);
        self.items.deinit(allocator);
        self.* = DefinitionStore.init();
    }

    pub fn count(self: *const DefinitionStore) usize {
        return self.items.items.len;
    }

    fn addOrReplace(self: *DefinitionStore, allocator: Allocator, source: []const u8, name: []const u8, kind: DefinitionKind) !void {
        for (self.items.items) |*definition| {
            if (definition.kind == kind and std.mem.eql(u8, definition.name, name)) {
                const new_source = try allocator.dupe(u8, source);
                const new_name = try allocator.dupe(u8, name);
                allocator.free(definition.source);
                allocator.free(definition.name);
                definition.* = .{ .name = new_name, .source = new_source, .kind = kind };
                return;
            }
        }

        try self.items.append(allocator, .{
            .name = try allocator.dupe(u8, name),
            .source = try allocator.dupe(u8, source),
            .kind = kind,
        });
    }

    fn snapshot(self: *const DefinitionStore, allocator: Allocator) !DefinitionStore {
        var result = DefinitionStore.init();
        errdefer result.deinit(allocator);
        try result.items.ensureTotalCapacity(allocator, self.items.items.len);
        for (self.items.items) |definition| {
            result.items.appendAssumeCapacity(.{
                .name = try allocator.dupe(u8, definition.name),
                .source = try allocator.dupe(u8, definition.source),
                .kind = definition.kind,
            });
        }
        return result;
    }

    fn restore(self: *DefinitionStore, allocator: Allocator, snapshot_store: *DefinitionStore) void {
        self.deinit(allocator);
        self.* = snapshot_store.*;
        snapshot_store.* = DefinitionStore.init();
    }
};

const testing = std.testing;

const TestBackend = enum { interpreter, dev, wasm };

fn toEvalBackend(backend: TestBackend) eval.EvalBackend {
    return switch (backend) {
        .interpreter => .interpreter,
        .dev => .dev,
        .wasm => .wasm,
    };
}

fn backendName(backend: TestBackend) []const u8 {
    return switch (backend) {
        .interpreter => "INTERPRETER",
        .dev => "DEV BACKEND",
        .wasm => "WASM BACKEND",
    };
}

fn expectBackend(backend: TestBackend, expr: []const u8, expected: []const u8) !void {
    const eval_backend = toEvalBackend(backend);
    if (!eval.backendAvailable(eval_backend)) return;

    var repl = ReplSession.init(testing.allocator, eval_backend);
    defer repl.deinit();

    const result = try repl.step(expr);
    defer testing.allocator.free(result);
    testing.expectEqualStrings(expected, result) catch |err| {
        std.debug.print("{s} FAILED for: {s}\n", .{ backendName(backend), expr });
        return err;
    };
}

fn expectInterpreter(expr: []const u8, expected: []const u8) !void {
    try expectBackend(.interpreter, expr, expected);
}

fn expectAllNative(expr: []const u8, expected: []const u8) !void {
    var repl = ReplSession.init(testing.allocator, .interpreter);
    defer repl.deinit();

    const line = std.mem.trim(u8, expr, " \t\r\n");
    const input_info = try repl.classifyInput(line) orelse return error.ParseError;
    try testing.expectEqual(InputKind.expression, input_info.kind);

    const definitions = try repl.definitionsSource();
    defer testing.allocator.free(definitions);

    const source = try std.fmt.allocPrint(testing.allocator, "{s}\nmain = {s}\n", .{ definitions, line });
    defer testing.allocator.free(source);

    var compiled = try eval.test_helpers.compileInspectedProgram(testing.allocator, .module, source, &.{});
    defer compiled.deinit(testing.allocator);

    try expectCompiledBackend(.interpreter, expr, expected, &compiled.lowered);
    try expectCompiledBackend(.dev, expr, expected, &compiled.lowered);
    try expectCompiledBackend(.wasm, expr, expected, &compiled.wasm_lowered);
}

fn expectCompiledBackend(
    backend: TestBackend,
    expr: []const u8,
    expected: []const u8,
    lowered: *eval.test_helpers.LoweredProgram,
) !void {
    const eval_backend = toEvalBackend(backend);
    if (!eval.backendAvailable(eval_backend)) return;

    const result = switch (backend) {
        .interpreter => try eval.test_helpers.lirInterpreterInspectedStr(testing.allocator, lowered),
        .dev => try eval.test_helpers.devEvaluatorInspectedStr(testing.allocator, lowered),
        .wasm => try eval.test_helpers.wasmEvaluatorInspectedStr(testing.allocator, lowered),
    };
    defer testing.allocator.free(result);

    testing.expectEqualStrings(expected, result) catch |err| {
        std.debug.print("{s} FAILED for: {s}\n", .{ backendName(backend), expr });
        return err;
    };
}

fn expectStateful(backend: TestBackend, steps: []const [2][]const u8) !void {
    const eval_backend = toEvalBackend(backend);
    if (!eval.backendAvailable(eval_backend)) return;

    var repl = ReplSession.init(testing.allocator, eval_backend);
    defer repl.deinit();

    for (steps) |step_pair| {
        const result = try repl.step(step_pair[0]);
        defer testing.allocator.free(result);
        testing.expectEqualStrings(step_pair[1], result) catch |err| {
            std.debug.print("{s} FAILED for: {s}\n", .{ backendName(backend), step_pair[0] });
            return err;
        };
    }
}

fn expectStepsFinal(backend: TestBackend, steps: []const []const u8, expected: []const u8) !void {
    const eval_backend = toEvalBackend(backend);
    if (!eval.backendAvailable(eval_backend)) return;

    var repl = ReplSession.init(testing.allocator, eval_backend);
    defer repl.deinit();

    for (steps, 0..) |step_input, i| {
        const result = try repl.step(step_input);
        defer testing.allocator.free(result);

        if (i + 1 == steps.len) {
            testing.expectEqualStrings(expected, result) catch |err| {
                std.debug.print("{s} FAILED for: {s}\n", .{ backendName(backend), step_input });
                return err;
            };
        }
    }
}

test "Repl - initialization and cleanup" {
    var repl = ReplSession.init(testing.allocator, .interpreter);
    defer repl.deinit();
    try testing.expect(repl.definitions.count() == 0);
}

test "Repl - special commands" {
    var repl = ReplSession.init(testing.allocator, .interpreter);
    defer repl.deinit();

    const help_result = try repl.step(":help");
    defer testing.allocator.free(help_result);
    try testing.expect(std.mem.indexOf(u8, help_result, "Enter an expression") != null);

    const exit_result = try repl.step(":exit");
    defer testing.allocator.free(exit_result);
    try testing.expectEqualStrings("Goodbye!", exit_result);

    const empty_result = try repl.step("");
    defer testing.allocator.free(empty_result);
    try testing.expectEqualStrings("", empty_result);
}

test "Repl - simple expressions" {
    try expectAllNative("42", "42.0");
}

test "Repl - string expressions" {
    try expectAllNative("\"Hello, World!\"", "\"Hello, World!\"");
}

test "Repl - Bool.True" {
    try expectAllNative("Bool.True", "True");
}

test "Repl - Bool.False" {
    try expectAllNative("Bool.False", "False");
}

test "Repl - Bool.not(False)" {
    try expectAllNative("Bool.not(False)", "True");
}

test "Repl - Bool.not(Bool.True)" {
    try expectAllNative("Bool.not(Bool.True)", "False");
}

test "Repl - Bool.not(Bool.False)" {
    try expectAllNative("Bool.not(Bool.False)", "True");
}

test "Repl - !Bool.True" {
    try expectAllNative("!Bool.True", "False");
}

test "Repl - !Bool.False" {
    try expectAllNative("!Bool.False", "True");
}

test "Repl - I8.mod_by negative positive" {
    try expectAllNative("I8.mod_by(-10, 3)", "2");
}

test "Repl - I8.mod_by positive negative" {
    try expectAllNative("I8.mod_by(10, -3)", "-2");
}

test "Repl - I8.mod_by negative negative" {
    try expectAllNative("I8.mod_by(-10, -3)", "-1");
}

test "Repl - Str.is_empty" {
    try expectAllNative("Str.is_empty(\"\")", "True");
    try expectAllNative("Str.is_empty(\"a\")", "False");
}

test "Repl - lambda renders as <function>" {
    try expectAllNative("|x| x + 1", "<function>");
    try expectAllNative("|x, y| x + y", "<function>");
}

test "Repl - Str.to_utf8" {
    try expectAllNative("Str.to_utf8(\"hello\")", "[104, 101, 108, 108, 111]");
    try expectAllNative("List.len(Str.to_utf8(\"\"))", "0");
    try expectAllNative("List.len(Str.to_utf8(\"hello\"))", "5");
    try expectAllNative("List.len(Str.to_utf8(\"é\"))", "2");
    try expectAllNative("List.len(Str.to_utf8(\"🎉\"))", "4");
    try expectAllNative("List.len(Str.to_utf8(\"Hello, World!\"))", "13");
    try expectAllNative("List.len(Str.to_utf8(\"日本語\"))", "9");
    try expectAllNative("List.len(Str.to_utf8(\"a é 🎉\"))", "9");
    try expectAllNative("List.is_empty(Str.to_utf8(\"\"))", "True");
    try expectAllNative("List.is_empty(Str.to_utf8(\"x\"))", "False");
}

test "Repl - Str.from_utf8_lossy" {
    try expectAllNative("Str.from_utf8_lossy(Str.to_utf8(\"hello\"))", "\"hello\"");
    try expectAllNative("Str.from_utf8_lossy(Str.to_utf8(\"\"))", "\"\"");
    try expectAllNative("Str.from_utf8_lossy(Str.to_utf8(\"🎉 party!\"))", "\"🎉 party!\"");
    try expectAllNative("Str.from_utf8_lossy(Str.to_utf8(\"abc123\"))", "\"abc123\"");
}

test "Repl - Str.from_utf8 Ok" {
    try expectAllNative("Str.from_utf8([72, 105])", "Ok(\"Hi\")");
}

test "Repl - Str.from_utf8 ok_or" {
    try expectAllNative("Str.from_utf8([72, 105]).ok_or(\"fallback\")", "\"Hi\"");
}

test "Repl - Str.from_utf8 snapshot sequence" {
    const steps = &[_][2][]const u8{
        .{ "Str.from_utf8([72, 105])", "Ok(\"Hi\")" },
        .{ "Str.from_utf8([])", "Ok(\"\")" },
        .{ "Str.from_utf8([82, 111, 99])", "Ok(\"Roc\")" },
        .{ "Str.from_utf8([240, 159, 144, 166])", "Ok(\"🐦\")" },
        .{ "Str.from_utf8([195, 169])", "Ok(\"é\")" },
        .{ "Str.from_utf8([255]).is_err()", "True" },
        .{ "Str.from_utf8([72, 105]).is_ok()", "True" },
        .{ "Str.from_utf8([72, 105]).ok_or(\"fallback\")", "\"Hi\"" },
        .{ "Str.from_utf8([255]).ok_or(\"fallback\")", "\"fallback\"" },
        .{ "Str.from_utf8([255])", "Err(BadUtf8({ index: 0, problem: InvalidStartByte }))" },
    };
    try expectStateful(.interpreter, steps);
    try expectStateful(.dev, steps);
}

test "Repl - U8.from_str result format" {
    try expectAllNative("U8.from_str(\"42\")", "Ok(42)");
}

test "Repl - F32.from_str result format" {
    try expectAllNative("F32.from_str(\"3.14\")", "Ok(3.14)");
}

test "Repl - list literals" {
    try expectAllNative("List.len([1, 2, 3])", "3");
    try expectAllNative("[1, 2, 3]", "[1.0, 2.0, 3.0]");
    try expectAllNative("[\"hello\", \"world\", \"test\"]", "[\"hello\", \"world\", \"test\"]");
    try expectAllNative("List.len([\"hello\", \"world\", \"test\"])", "3");
}

test "Repl - list operations" {
    try expectAllNative("List.len(List.concat([1, 2], [3, 4]))", "4");
    try expectAllNative("List.len(List.concat([], [1, 2, 3]))", "3");
    try expectAllNative("List.len(List.concat([1, 2, 3], []))", "3");
    try expectAllNative("List.contains([1, 2, 3, 4, 5], 3)", "True");
    try expectAllNative("List.drop_if([1, 2, 3, 4, 5], |x| x > 2)", "[1.0, 2.0]");
    try expectAllNative("List.keep_if([1, 2, 3, 4, 5], |x| x > 2)", "[3.0, 4.0, 5.0]");
    try expectAllNative("List.keep_if([1, 2, 3], |_| Bool.False)", "[]");
    try expectAllNative("List.fold_rev([1.I64, 2.I64, 3.I64], 0.I64, |x, acc| acc * 10 + x)", "321");
    try expectAllNative("List.fold_rev([1], 0, |x, acc| acc * 10 + x)", "1.0");
    try expectAllNative("List.fold_rev([1, 2, 3], 0, |x, acc| acc * 10 + x)", "321.0");
    try expectAllNative("List.fold_rev([], 42, |x, acc| x + acc)", "42.0");
}

test "Repl - List.with_capacity" {
    try expectAllNative("List.with_capacity(10)", "[]");
    try expectInterpreter("List.first(List.with_capacity(10))", "Err(ListWasEmpty)");
}

test "Repl - List.append" {
    try expectAllNative("List.append([1, 2], 3)", "[1.0, 2.0, 3.0]");
}

test "Repl - range_to" {
    try expectInterpreter("Iter.fold(1.to(3), [], |acc, item| acc.append(item))", "[1.0, 2.0, 3.0]");
}

test "Repl - list_sort_with" {
    try expectAllNative("List.len(List.sort_with([3, 1, 2], |a, b| if a < b LT else if a > b GT else EQ))", "3");
    try expectAllNative("List.len(List.sort_with([5, 2, 8, 1, 9], |a, b| if a < b LT else if a > b GT else EQ))", "5");
    try expectAllNative(
        \\{
        \\    xs : List(I64)
        \\    xs = []
        \\    List.len(List.sort_with(xs, |a, b| if a < b LT else if a > b GT else EQ))
        \\}
    , "0");
    try expectAllNative("List.len(List.sort_with([42], |a, b| if a < b LT else if a > b GT else EQ))", "1");
}

test "Repl - list fold with concat" {
    try expectAllNative("List.len(List.fold([1, 2, 3], [], |acc, x| List.concat(acc, [x])))", "3");
}

test "Repl - silent assignments" {
    const steps = &[_][2][]const u8{
        .{ "x = 5", "assigned `x`" },
        .{ "x", "5.0" },
    };
    try expectStateful(.interpreter, steps);
    try expectStateful(.dev, steps);
    try expectStateful(.wasm, steps);
}

test "Repl - issue 9258 opaque type param field access" {
    const steps = &[_][]const u8{
        "Wrapper(a) := { inner : a }",
        "unwrap : Wrapper(a) -> a",
        "unwrap = |w| w.inner",
        "unwrap({ inner: \"hello\" })",
    };

    try expectStepsFinal(.interpreter, steps, "\"hello\"");
    try expectStepsFinal(.dev, steps, "\"hello\"");
}

test "Repl - polymorphic numeric in comparison snapshot sequence" {
    const steps = &[_][2][]const u8{
        .{ "is_positive = |x| x > 0", "assigned `is_positive`" },
        .{ "List.any([-1, 0, 1], is_positive)", "True" },
        .{ "List.any([-1, 0, -2], is_positive)", "False" },
    };
    try expectStateful(.interpreter, steps);
    try expectStateful(.dev, steps);
}

test "Repl - variable redefinition" {
    const steps = &[_][2][]const u8{
        .{ "x = 5", "assigned `x`" },
        .{ "y = x + 1", "assigned `y`" },
        .{ "y", "6.0" },
        .{ "x = 3", "assigned `x`" },
        .{ "y", "4.0" },
    };
    try expectStateful(.interpreter, steps);
    try expectStateful(.dev, steps);
    try expectStateful(.wasm, steps);
}

test "Repl - for loop over list" {
    const steps = &[_][2][]const u8{
        .{ "[\"hello\", \"world\", \"test\"]", "[\"hello\", \"world\", \"test\"]" },
        .{
            \\count = {
            \\    var counter_ = 0
            \\    for _ in ["hello", "world", "test"] {
            \\        counter_ = counter_ + 1
            \\    }
            \\    counter_
            \\}
            ,
            "assigned `count`",
        },
    };
    try expectStateful(.interpreter, steps);
    try expectStateful(.dev, steps);
    try expectStateful(.wasm, steps);
}

test "Repl - for loop snapshots" {
    const steps = &[_][2][]const u8{
        .{
            \\unchanged = {
            \\    var value_ = 42
            \\    for n in [] {
            \\        value_ = n
            \\    }
            \\    value_
            \\}
            ,
            "assigned `unchanged`",
        },
        .{
            \\result = {
            \\    var all_true_ = Bool.True
            \\    for b in [Bool.True, Bool.True, Bool.False] {
            \\        if b == Bool.False {
            \\            all_true_ = Bool.False
            \\        } else {
            \\            {}
            \\        }
            \\    }
            \\    all_true_
            \\}
            ,
            "assigned `result`",
        },
        .{
            \\count = {
            \\    var counter_ = 0
            \\    for _ in ["hello", "world", "test"] {
            \\        counter_ = counter_ + 1
            \\    }
            \\    counter_
            \\}
            ,
            "assigned `count`",
        },
        .{
            \\sum = {
            \\    var total_ = 0
            \\    for n in [1, 2, 3, 4, 5] {
            \\        total_ = total_ + n
            \\    }
            \\    total_
            \\}
            ,
            "assigned `sum`",
        },
        .{
            \\product = {
            \\    var result_ = 0
            \\    for i in [1, 2, 3] {
            \\        for j in [10, 20] {
            \\            result_ = result_ + (i * j)
            \\        }
            \\    }
            \\    result_
            \\}
            ,
            "assigned `product`",
        },
    };
    try expectStateful(.interpreter, steps);
    try expectStateful(.dev, steps);
    try expectStateful(.wasm, steps);
}

test "Repl - build full source with block syntax" {
    var repl = ReplSession.init(testing.allocator, .interpreter);
    defer repl.deinit();

    try repl.addOrReplaceDefinition("x = 5", "x", .value);
    try repl.addOrReplaceDefinition("y = x + 1", "y", .value);

    const full_source = try repl.buildFullSource("y");
    defer testing.allocator.free(full_source);

    const expected =
        \\{
        \\    x = 5
        \\    y = x + 1
        \\    y
        \\}
    ;
    try testing.expectEqualStrings(expected, full_source);
}

test "Repl - definition replacement" {
    var repl = ReplSession.init(testing.allocator, .interpreter);
    defer repl.deinit();

    try repl.addOrReplaceDefinition("x = 1", "x", .value);
    try repl.addOrReplaceDefinition("x = 2", "x", .value);
    try repl.addOrReplaceDefinition("x = 3", "x", .value);

    try testing.expect(repl.definitions.count() == 1);

    const full_source = try repl.buildFullSource("x");
    defer testing.allocator.free(full_source);

    const expected =
        \\{
        \\    x = 3
        \\    x
        \\}
    ;
    try testing.expectEqualStrings(expected, full_source);
}

test "Repl - 4-arg lambda call (dev)" {
    const steps = &[_][2][]const u8{
        .{ "f = |a, b, c, d| a + b + c + d", "assigned `f`" },
        .{ "f(10, 20, 30, 40)", "100.0" },
    };
    try expectStateful(.interpreter, steps);
    try expectStateful(.dev, steps);
    try expectStateful(.wasm, steps);
}

fn expectSplit(input: []const u8, expected: []const []const u8) !void {
    var repl = ReplSession.init(testing.allocator, .interpreter);
    defer repl.deinit();

    const slices = try repl.splitInputIntoStatements(input);
    defer repl.freeStatementSlices(slices);

    try testing.expectEqual(expected.len, slices.len);
    for (expected, slices) |want, got| {
        try testing.expectEqualStrings(want, got);
    }
}

test "splitInputIntoStatements - single line passes through" {
    try expectSplit("x = 5", &.{"x = 5"});
}

test "splitInputIntoStatements - special command passes through" {
    try expectSplit(":help", &.{":help"});
}

test "splitInputIntoStatements - two assignments split into two slices" {
    try expectSplit("z = 5\ny = 6", &.{ "z = 5", "y = 6" });
}

test "splitInputIntoStatements - multi-line single statement stays whole" {
    const input =
        \\simple_match = |color| {
        \\    match color {
        \\        Red => "red"
        \\        Green => "green"
        \\        Blue => "blue"
        \\    }
        \\}
    ;
    try expectSplit(input, &.{input});
}

test "splitInputIntoStatements - definition followed by expression" {
    try expectSplit("f = |x| x + 1\nf(5)", &.{ "f = |x| x + 1", "f(5)" });
}

test "splitInputIntoStatements - blank lines between statements are dropped" {
    try expectSplit("a = 1\n\nb = 2", &.{ "a = 1", "b = 2" });
}

test "splitInputIntoStatements - annotation and decl stay separate" {
    try expectSplit("z : U64\nz = 5", &.{ "z : U64", "z = 5" });
}

test "Repl - paste of annotation + decl produces single assigned message" {
    var repl = ReplSession.init(testing.allocator, .interpreter);
    defer repl.deinit();

    const slices = try repl.splitInputIntoStatements("z : U64\nz = 5");
    defer repl.freeStatementSlices(slices);
    try testing.expectEqual(@as(usize, 2), slices.len);

    const r0 = try repl.step(slices[0]);
    defer testing.allocator.free(r0);
    try testing.expectEqualStrings("", r0);

    const r1 = try repl.step(slices[1]);
    defer testing.allocator.free(r1);
    try testing.expectEqualStrings("assigned `z`", r1);
}

test "Repl - paste of two assignments processes both" {
    var repl = ReplSession.init(testing.allocator, .interpreter);
    defer repl.deinit();

    const slices = try repl.splitInputIntoStatements("z = 5\ny = 6");
    defer repl.freeStatementSlices(slices);

    try testing.expectEqual(@as(usize, 2), slices.len);

    const r0 = try repl.step(slices[0]);
    defer testing.allocator.free(r0);
    try testing.expectEqualStrings("assigned `z`", r0);

    const r1 = try repl.step(slices[1]);
    defer testing.allocator.free(r1);
    try testing.expectEqualStrings("assigned `y`", r1);
}

test "issue 9364: F64.plus with integer literals" {
    try expectAllNative("F64.plus(1, 1)", "2");
}

test "issue 9364: F64.plus with float literals" {
    try expectAllNative("F64.plus(1.0, 1.0)", "2");
}

test "issue 9364: F64.to_str integer-valued float literal" {
    try expectAllNative("F64.to_str(2.0)", "\"2\"");
}

test "issue 9364: F64.to_str non-integer float literal" {
    try expectAllNative("F64.to_str(2.5)", "\"2.5\"");
}
