//! Compile-time inventory of public associated functions in Builtin.roc.
//!
//! The typecheck fuzzer still needs typed adapters to call these functions, but
//! this keeps those adapters grounded in the actual builtin source instead of a
//! hand-maintained copy of method names.

const std = @import("std");
const compiled_builtins = @import("compiled_builtins");

pub const source = compiled_builtins.builtin_source;

pub const Function = struct {
    owner: []const u8,
    name: []const u8,
    signature: []const u8,
    line: u32,
};

const Owner = struct {
    name: []const u8,
    indent: usize,
};

const max_functions = 1600;
const max_owner_depth = 64;

const Parsed = struct {
    items: [max_functions]Function,
    len: usize,
};

const parsed = parseFunctions();

pub const functions: []const Function = parsed.items[0..parsed.len];

pub fn count() usize {
    return functions.len;
}

pub fn has(comptime owner: []const u8, comptime name: []const u8) bool {
    @setEvalBranchQuota(5_000_000);

    inline for (functions) |function| {
        if (std.mem.eql(u8, function.owner, owner) and std.mem.eql(u8, function.name, name)) {
            return true;
        }
    }
    return false;
}

pub fn require(comptime owner: []const u8, comptime name: []const u8) void {
    if (!has(owner, name)) {
        @compileError("Builtin.roc no longer exposes " ++ owner ++ "." ++ name);
    }
}

fn parseFunctions() Parsed {
    @setEvalBranchQuota(5_000_000);

    var result: Parsed = .{ .items = undefined, .len = 0 };
    var owners: [max_owner_depth]Owner = undefined;
    var owner_count: usize = 0;
    var pending_owner: ?Owner = null;

    var lines = std.mem.splitScalar(u8, source, '\n');
    var line_number: u32 = 1;
    while (lines.next()) |line| : (line_number += 1) {
        const trimmed = trimLine(line);
        if (trimmed.len == 0 or std.mem.startsWith(u8, trimmed, "##")) continue;

        const indent = leadingIndent(line);

        while (owner_count > 0 and std.mem.eql(u8, trimmed, "}") and indent <= owners[owner_count - 1].indent) {
            owner_count -= 1;
        }
        if (pending_owner) |owner| {
            if (std.mem.startsWith(u8, trimmed, "].{") or std.mem.startsWith(u8, trimmed, "}.{")) {
                if (owner_count >= owners.len) @compileError("Builtin.roc associated block nesting exceeded parser capacity");
                owners[owner_count] = owner;
                owner_count += 1;
                pending_owner = null;
                continue;
            }
            if (std.mem.eql(u8, trimmed, "}") and indent <= owner.indent) {
                pending_owner = null;
                continue;
            }
            continue;
        }

        if (std.mem.eql(u8, trimmed, "}")) continue;

        if (associatedOwnerStart(trimmed)) |name| {
            const owner: Owner = .{ .name = name, .indent = indent };
            if (std.mem.find(u8, trimmed, ".{") != null) {
                if (owner_count >= owners.len) @compileError("Builtin.roc associated block nesting exceeded parser capacity");
                owners[owner_count] = owner;
                owner_count += 1;
            } else {
                pending_owner = owner;
            }
            continue;
        }

        if (owner_count == 0) continue;
        if (functionName(trimmed)) |name| {
            if (result.len >= result.items.len) @compileError("Builtin.roc associated function count exceeded parser capacity");
            result.items[result.len] = .{
                .owner = owners[owner_count - 1].name,
                .name = name,
                .signature = trimmed,
                .line = line_number,
            };
            result.len += 1;
        }
    }

    return result;
}

fn associatedOwnerStart(line: []const u8) ?[]const u8 {
    if (line.len == 0 or !isUpper(line[0])) return null;
    const name_end = identifierEnd(line, 0);
    const after_name = trimLeft(skipTypeParams(line[name_end..]));
    if (!(std.mem.startsWith(u8, after_name, "::") or std.mem.startsWith(u8, after_name, ":="))) return null;
    return line[0..name_end];
}

fn functionName(line: []const u8) ?[]const u8 {
    if (line.len == 0 or !isLower(line[0])) return null;
    if (std.mem.find(u8, line, "->") == null and std.mem.find(u8, line, "=>") == null) return null;

    const name_end = functionIdentifierEnd(line, 0);
    const after_name = trimLeft(line[name_end..]);
    if (!std.mem.startsWith(u8, after_name, ":")) return null;
    return line[0..name_end];
}

fn leadingIndent(line: []const u8) usize {
    var indent: usize = 0;
    while (indent < line.len and (line[indent] == ' ' or line[indent] == '\t')) : (indent += 1) {}
    return indent;
}

fn trimLeft(line: []const u8) []const u8 {
    var index: usize = 0;
    while (index < line.len and (line[index] == ' ' or line[index] == '\t')) : (index += 1) {}
    return line[index..];
}

fn skipTypeParams(line: []const u8) []const u8 {
    var rest = trimLeft(line);
    if (rest.len == 0 or rest[0] != '(') return rest;

    var depth: usize = 0;
    for (rest, 0..) |byte, index| {
        if (byte == '(') {
            depth += 1;
        } else if (byte == ')') {
            depth -= 1;
            if (depth == 0) {
                rest = rest[index + 1 ..];
                return rest;
            }
        }
    }
    return rest;
}

fn trimLine(line: []const u8) []const u8 {
    var start: usize = 0;
    while (start < line.len and isTrimByte(line[start])) : (start += 1) {}

    var end = line.len;
    while (end > start and isTrimByte(line[end - 1])) : (end -= 1) {}

    return line[start..end];
}

fn isTrimByte(byte: u8) bool {
    return byte == ' ' or byte == '\t' or byte == '\r';
}

fn identifierEnd(line: []const u8, start: usize) usize {
    var index = start;
    while (index < line.len and isIdentifierChar(line[index])) : (index += 1) {}
    return index;
}

fn functionIdentifierEnd(line: []const u8, start: usize) usize {
    var index = start;
    while (index < line.len and (isIdentifierChar(line[index]) or line[index] == '!')) : (index += 1) {}
    return index;
}

fn isIdentifierChar(byte: u8) bool {
    return isLower(byte) or isUpper(byte) or std.ascii.isDigit(byte) or byte == '_';
}

fn isLower(byte: u8) bool {
    return byte >= 'a' and byte <= 'z';
}

fn isUpper(byte: u8) bool {
    return byte >= 'A' and byte <= 'Z';
}
