//! Syntax-level Roc module generator for formatter fuzzers.
//!
//! Generated modules parse without diagnostics but need not type check, so
//! every header, statement, type annotation, pattern, and expression form can
//! nest inside any other. Userspace Roc identifiers come from `fresh`, using
//! the same naming scheme as `TypedCodeGenerator`.

const std = @import("std");
const FuzzReader = @import("FuzzReader.zig");
const TypedCodeGenerator = @import("TypedCodeGenerator.zig");

const Self = @This();

const Symbol = TypedCodeGenerator.Symbol;
const SymbolKind = TypedCodeGenerator.SymbolKind;

const max_depth = 4;

allocator: std.mem.Allocator,
reader: *FuzzReader,
output: std.ArrayList(u8),
next_id: u32,
indent: usize,

pub fn init(allocator: std.mem.Allocator, reader: *FuzzReader) Self {
    return .{
        .allocator = allocator,
        .reader = reader,
        .output = .empty,
        .next_id = 0,
        .indent = 0,
    };
}

pub fn deinit(self: *Self) void {
    self.output.deinit(self.allocator);
}

pub fn getOutput(self: *const Self) []const u8 {
    return self.output.items;
}

pub fn generateModule(self: *Self) std.mem.Allocator.Error!void {
    self.output.clearRetainingCapacity();
    self.next_id = 0;
    self.indent = 0;

    try self.writeHeader();
    const statement_count = self.reader.intRangeAtMost(u8, 1, 6);
    for (0..statement_count) |_| {
        try self.writeTopLevelStatement(self.chooseDepth());
        try self.write("\n");
    }
}

fn chooseDepth(self: *Self) u8 {
    return self.reader.intRangeAtMost(u8, 1, max_depth);
}

fn choose(self: *Self, count: u8) u8 {
    return self.reader.intRangeLessThan(u8, 0, count);
}

// Headers

fn writeHeader(self: *Self) std.mem.Allocator.Error!void {
    switch (self.choose(6)) {
        0 => {},
        1 => {
            try self.write("module ");
            try self.writeExposedList();
            try self.write("\n\n");
        },
        2 => {
            try self.write("app ");
            try self.writeExposedList();
            try self.write(" { ");
            try self.writeSymbol(self.fresh(.package));
            try self.write(": platform ");
            try self.writePlainString();
            if (self.reader.boolean()) {
                try self.write(", ");
                try self.writePackageEntry();
            }
            try self.write(" }\n\n");
        },
        3 => {
            try self.write("package ");
            try self.writeExposedList();
            try self.write(" {");
            try self.writePackageEntries();
            try self.write("}\n\n");
        },
        4 => {
            try self.write("hosted ");
            try self.writeExposedList();
            try self.write("\n\n");
        },
        5 => try self.writePlatformHeader(),
        else => unreachable,
    }
}

fn writeExposedList(self: *Self) std.mem.Allocator.Error!void {
    try self.write("[");
    const count = self.choose(4);
    for (0..count) |index| {
        if (index != 0) try self.write(", ");
        switch (self.choose(5)) {
            0 => try self.writeSymbol(self.fresh(.value)),
            1 => try self.writeEffectful(self.fresh(.function)),
            2 => try self.writeSymbol(self.fresh(.type)),
            3 => {
                try self.writeSymbol(self.fresh(.type));
                try self.write(" as ");
                try self.writeSymbol(self.fresh(.type));
            },
            4 => {
                try self.writeSymbol(self.fresh(.type));
                try self.write(".*");
            },
            else => unreachable,
        }
    }
    try self.writeTrailingComma(count);
    try self.write("]");
}

fn writePackageEntries(self: *Self) std.mem.Allocator.Error!void {
    const count = self.choose(3);
    for (0..count) |index| {
        try self.write(if (index == 0) " " else ", ");
        try self.writePackageEntry();
    }
    try self.writeTrailingComma(count);
    if (count != 0) try self.write(" ");
}

fn writePackageEntry(self: *Self) std.mem.Allocator.Error!void {
    try self.writeSymbol(self.fresh(.package));
    try self.write(": ");
    try self.writePlainString();
}

fn writePlatformHeader(self: *Self) std.mem.Allocator.Error!void {
    try self.write("platform ");
    try self.writePlainString();
    try self.write("\n    requires {");
    // A requires entry's function type can take comma-separated arguments,
    // so a second entry would parse as another argument.
    const requires_count = self.choose(2);
    for (0..requires_count) |_| {
        try self.write(" ");
        if (self.reader.boolean()) {
            try self.write("[");
            const rigid_count = self.reader.intRangeAtMost(u8, 1, 2);
            for (0..rigid_count) |rigid_index| {
                if (rigid_index != 0) try self.write(", ");
                try self.writeSymbol(self.fresh(.type));
                try self.write(" : ");
                try self.writeSymbol(self.fresh(.value));
            }
            try self.write("] for ");
        }
        try self.writeSymbol(self.fresh(.value));
        try self.write(" : ");
        try self.writeTypeAnno(2, .top);
    }
    if (requires_count != 0) try self.write(" ");
    try self.write("}\n    exposes ");
    try self.writeExposedList();
    try self.write("\n    packages {");
    try self.writePackageEntries();
    try self.write("}\n    provides ");
    try self.writeSymbolMap();
    if (self.reader.boolean()) {
        try self.write("\n    hosted ");
        try self.writeSymbolMap();
    }
    if (self.reader.boolean()) {
        try self.write("\n    targets: {");
        const target_count = self.choose(3);
        for (0..target_count) |index| {
            try self.write(if (index == 0) " " else ", ");
            try self.writeTargetEntry(index);
        }
        try self.writeTrailingComma(target_count);
        if (target_count != 0) try self.write(" ");
        try self.write("}");
    }
    try self.write("\n\n");
}

fn writeSymbolMap(self: *Self) std.mem.Allocator.Error!void {
    try self.write("{");
    const count = self.choose(3);
    for (0..count) |index| {
        try self.write(if (index == 0) " " else ", ");
        try self.writePlainString();
        try self.write(": ");
        if (self.reader.boolean()) {
            try self.writeSymbol(self.fresh(.type));
            try self.write(".");
        }
        try self.writeSymbol(self.fresh(.value));
    }
    try self.writeTrailingComma(count);
    if (count != 0) try self.write(" ");
    try self.write("}");
}

fn writeTargetEntry(self: *Self, index: usize) std.mem.Allocator.Error!void {
    if (index == 0 and self.reader.boolean()) {
        try self.write("inputs_dir: ");
        try self.writePlainString();
        return;
    }
    const target_names = [_][]const u8{ "x64musl", "arm64mac", "wasm32" };
    try self.write(target_names[self.choose(target_names.len)]);
    try self.write(": {");
    const entry_count = self.choose(3);
    for (0..entry_count) |entry_index| {
        try self.write(if (entry_index == 0) " " else ", ");
        if (entry_index == 0) {
            try self.write("inputs: [");
            const input_count = self.choose(3);
            for (0..input_count) |input_index| {
                if (input_index != 0) try self.write(", ");
                if (self.reader.boolean()) {
                    try self.writePlainString();
                } else {
                    try self.write("app");
                }
            }
            try self.write("]");
        } else {
            try self.writeSymbol(self.fresh(.field));
            try self.write(": ");
            try self.writeTargetConfigValue(2);
        }
    }
    try self.writeTrailingComma(entry_count);
    if (entry_count != 0) try self.write(" ");
    try self.write("}");
}

fn writeTargetConfigValue(self: *Self, depth: u8) std.mem.Allocator.Error!void {
    switch (self.choose(if (depth == 0) 4 else 5)) {
        0 => try self.writeIntLiteral(),
        1 => try self.writePlainString(),
        2 => try self.writeSymbol(self.fresh(.tag)),
        3 => try self.writeSymbol(self.fresh(.value)),
        4 => {
            try self.write("[");
            const count = self.choose(3);
            for (0..count) |index| {
                if (index != 0) try self.write(", ");
                try self.writeTargetConfigValue(depth - 1);
            }
            try self.write("]");
        },
        else => unreachable,
    }
}

// Statements

fn writeTopLevelStatement(self: *Self, depth: u8) std.mem.Allocator.Error!void {
    switch (self.choose(8)) {
        0 => try self.writeImport(),
        1 => try self.writeTypeDecl(depth),
        2 => try self.writeWhereAlias(depth),
        3 => try self.writeTypeAnnoStatement(depth),
        4 => {
            try self.write("expect ");
            try self.writeExpr(depth);
        },
        else => try self.writeDecl(depth),
    }
}

fn writeImport(self: *Self) std.mem.Allocator.Error!void {
    try self.write("import ");
    if (self.choose(4) == 0) {
        try self.writePlainString();
        try self.write(" as ");
        try self.writeSymbol(self.fresh(.value));
        try self.write(if (self.reader.boolean()) " : Str" else " : List(U8)");
        return;
    }
    if (self.reader.boolean()) {
        try self.writeSymbol(self.fresh(.package));
        try self.write(".");
    }
    try self.writeSymbol(self.fresh(.type));
    if (self.reader.boolean()) {
        try self.write(".");
        try self.writeSymbol(self.fresh(.type));
    }
    if (self.reader.boolean()) {
        try self.write(" as ");
        try self.writeSymbol(self.fresh(.type));
    }
    if (self.reader.boolean()) {
        try self.write(" exposing [");
        const count = self.reader.intRangeAtMost(u8, 1, 3);
        for (0..count) |index| {
            if (index != 0) try self.write(", ");
            switch (self.choose(3)) {
                0 => try self.writeSymbol(self.fresh(.value)),
                1 => try self.writeSymbol(self.fresh(.type)),
                2 => {
                    try self.writeSymbol(self.fresh(.type));
                    try self.write(" as ");
                    try self.writeSymbol(self.fresh(.type));
                },
                else => unreachable,
            }
        }
        try self.writeTrailingComma(count);
        try self.write("]");
    }
}

fn writeTypeDecl(self: *Self, depth: u8) std.mem.Allocator.Error!void {
    try self.writeSymbol(self.fresh(.type));
    if (self.reader.boolean()) {
        try self.write("(");
        const count = self.reader.intRangeAtMost(u8, 1, 2);
        for (0..count) |index| {
            if (index != 0) try self.write(", ");
            try self.writeSymbol(self.fresh(.value));
        }
        try self.write(")");
    }
    const kind = self.choose(3);
    try self.write(switch (kind) {
        0 => " : ",
        1 => " := ",
        2 => " :: ",
        else => unreachable,
    });
    if (kind != 0 and self.reader.boolean()) {
        if (self.reader.boolean()) try self.writeTagUnionAnno(depth) else try self.writeRecordAnno(depth);
        try self.write(".{");
        try self.writeBlockStatements(depth, .associated);
        try self.write("}");
        return;
    }
    try self.writeTypeAnno(depth, .top);
}

fn writeWhereAlias(self: *Self, depth: u8) std.mem.Allocator.Error!void {
    const receiver = self.fresh(.value);
    try self.writeSymbol(receiver);
    try self.write(".");
    try self.writeSymbol(self.fresh(.type));
    try self.write(" : ");
    try self.writeWhereClause(receiver, depth);
}

fn writeWhereClause(self: *Self, receiver: Symbol, depth: u8) std.mem.Allocator.Error!void {
    try self.write("where [");
    const count = self.reader.intRangeAtMost(u8, 1, 3);
    for (0..count) |index| {
        if (index != 0) try self.write(", ");
        try self.writeSymbol(receiver);
        try self.write(".");
        try self.writeSymbol(self.fresh(.function));
        try self.write(" : ");
        try self.writeTypeAnno(depth -| 1, .top);
    }
    try self.writeTrailingComma(count);
    try self.write("]");
}

fn writeTypeAnnoStatement(self: *Self, depth: u8) std.mem.Allocator.Error!void {
    if (self.reader.boolean()) {
        try self.writeEffectful(self.fresh(.function));
    } else {
        try self.writeSymbol(self.fresh(.value));
    }
    try self.write(" : ");
    try self.writeTypeAnno(depth, .top);
    if (self.choose(3) == 0) {
        try self.write(" ");
        try self.writeWhereClause(self.fresh(.value), depth);
    }
}

fn writeDecl(self: *Self, depth: u8) std.mem.Allocator.Error!void {
    try self.writeDeclPattern(depth -| 2);
    try self.write(" = ");
    if (self.choose(12) == 0) {
        try self.write("...");
    } else {
        try self.writeExpr(depth);
    }
}

const BlockKind = enum { body, loop_body, associated };

/// Writes statements on their own lines, ending at the block's indentation
/// level so the caller can write the closing delimiter.
fn writeBlockStatements(self: *Self, depth: u8, kind: BlockKind) std.mem.Allocator.Error!void {
    self.indent += 1;
    const count: u8 = switch (kind) {
        .associated => self.choose(3),
        .body, .loop_body => self.choose(3),
    };
    for (0..count) |index| {
        try self.writeNewline();
        switch (kind) {
            .associated => switch (self.choose(3)) {
                0 => try self.writeTypeAnnoStatement(depth -| 1),
                1 => try self.writeTypeDecl(depth -| 1),
                else => try self.writeDecl(depth -| 1),
            },
            .body, .loop_body => try self.writeBodyStatement(depth -| 1, kind, index == 0),
        }
    }
    if (kind != .associated and count == 0) {
        // `{ v }` and `{ v.f() }` start a record, so a block that is only an
        // expression must not begin with a lowercase name.
        try self.writeNewline();
        try self.writeDecl(depth -| 1);
    }
    if (kind != .associated) {
        try self.writeNewline();
        if (kind == .loop_body and self.choose(4) == 0) {
            try self.write("break");
        } else {
            try self.writeExpr(depth -| 1);
        }
    }
    self.indent -= 1;
    try self.writeNewline();
}

/// The first statement of a block is never a bare expression or a type
/// annotation, both of which would make the block parse as a record.
fn writeBodyStatement(self: *Self, depth: u8, kind: BlockKind, first: bool) std.mem.Allocator.Error!void {
    switch (self.choose(if (first) 8 else 12)) {
        0 => {
            try self.write("var $");
            try self.writeSymbol(self.fresh(.value));
            try self.write(" = ");
            try self.writeExpr(depth);
        },
        1 => {
            try self.write("$");
            try self.writeSymbol(self.fresh(.value));
            try self.write(" = ");
            try self.writeExpr(depth);
        },
        2 => {
            try self.write("dbg ");
            try self.writeExpr(depth);
        },
        3 => {
            try self.write("expect ");
            try self.writeExpr(depth);
        },
        4 => {
            try self.write("return ");
            try self.writeExpr(depth);
        },
        5 => {
            try self.write("crash ");
            try self.writePlainString();
        },
        6 => try self.writeForStatement(depth),
        7 => try self.writeDecl(depth),
        8 => {
            try self.write("while ");
            try self.writeExpr(depth);
            try self.write(" {");
            try self.writeBlockStatements(depth, .loop_body);
            try self.write("}");
        },
        9 => try self.writeTypeAnnoStatement(depth),
        10 => {
            try self.write("if ");
            try self.writeExpr(depth);
            try self.write(" {");
            try self.writeBlockStatements(depth, kind);
            try self.write("}");
        },
        11 => if (kind == .loop_body) try self.write("break") else try self.writeExpr(depth),
        else => unreachable,
    }
}

fn writeForStatement(self: *Self, depth: u8) std.mem.Allocator.Error!void {
    try self.write(if (self.reader.boolean()) "for " else "for! ");
    try self.writeDeclPattern(depth -| 2);
    try self.write(" in ");
    try self.writeExpr(depth);
    try self.write(" {");
    try self.writeBlockStatements(depth, .loop_body);
    try self.write("}");
}

// Type annotations

const AnnoPosition = enum {
    /// A position where an unparenthesized function type is allowed.
    top,
    /// A function argument, tuple element, or type application argument.
    nested,
};

fn writeTypeAnno(self: *Self, depth: u8, position: AnnoPosition) std.mem.Allocator.Error!void {
    if (depth == 0) {
        try self.writeLeafTypeAnno();
        return;
    }
    switch (self.choose(8)) {
        0 => try self.writeLeafTypeAnno(),
        1 => {
            try self.writeTypeName();
            try self.write("(");
            const count = self.reader.intRangeAtMost(u8, 1, 3);
            for (0..count) |index| {
                if (index != 0) try self.write(", ");
                try self.writeTypeAnno(depth - 1, .nested);
            }
            try self.writeTrailingComma(count);
            try self.write(")");
        },
        2 => try self.writeTagUnionAnno(depth),
        3 => try self.writeRecordAnno(depth),
        4 => {
            try self.write("(");
            const count = self.reader.intRangeAtMost(u8, 2, 3);
            for (0..count) |index| {
                if (index != 0) try self.write(", ");
                try self.writeTypeAnno(depth - 1, .nested);
            }
            try self.writeTrailingComma(count);
            try self.write(")");
        },
        5 => {
            try self.write("(");
            try self.writeTypeAnno(depth - 1, .top);
            try self.write(")");
        },
        else => {
            if (position == .nested) try self.write("(");
            const count = self.choose(3);
            for (0..count) |index| {
                if (index != 0) try self.write(", ");
                try self.writeTypeAnno(depth - 1, .nested);
            }
            if (count == 0) try self.write("()");
            try self.write(if (self.reader.boolean()) " -> " else " => ");
            try self.writeTypeAnno(depth - 1, .nested);
            if (position == .nested) try self.write(")");
        },
    }
}

fn writeLeafTypeAnno(self: *Self) std.mem.Allocator.Error!void {
    switch (self.choose(5)) {
        0 => try self.writeTypeName(),
        1 => try self.writeSymbol(self.fresh(.value)),
        2 => try self.write("_"),
        3 => {
            try self.write("_");
            try self.writeSymbol(self.fresh(.value));
        },
        4 => try self.write(builtin_types[self.choose(builtin_types.len)]),
        else => unreachable,
    }
}

const builtin_types = [_][]const u8{ "Str", "U8", "I64", "Bool", "Dec" };

fn writeTypeName(self: *Self) std.mem.Allocator.Error!void {
    if (self.choose(3) == 0) {
        try self.writeSymbol(self.fresh(.type));
        try self.write(".");
    }
    try self.writeSymbol(self.fresh(.type));
}

fn writeTagUnionAnno(self: *Self, depth: u8) std.mem.Allocator.Error!void {
    try self.write("[");
    const count = self.choose(4);
    for (0..count) |index| {
        if (index != 0) try self.write(", ");
        try self.writeSymbol(self.fresh(.tag));
        if (depth > 0 and self.reader.boolean()) {
            try self.write("(");
            const arg_count = self.reader.intRangeAtMost(u8, 1, 2);
            for (0..arg_count) |arg_index| {
                if (arg_index != 0) try self.write(", ");
                try self.writeTypeAnno(depth -| 1, .nested);
            }
            try self.write(")");
        }
    }
    try self.writeAnnoExtension(count);
    try self.write("]");
}

fn writeRecordAnno(self: *Self, depth: u8) std.mem.Allocator.Error!void {
    try self.write("{");
    const count = self.choose(4);
    for (0..count) |index| {
        try self.write(if (index == 0) " " else ", ");
        try self.writeSymbol(self.fresh(.field));
        try self.write(" : ");
        try self.writeTypeAnno(depth -| 1, .top);
    }
    try self.writeAnnoExtension(count);
    if (count != 0) try self.write(" ");
    try self.write("}");
}

fn writeAnnoExtension(self: *Self, count: usize) std.mem.Allocator.Error!void {
    switch (self.choose(4)) {
        0 => {
            try self.write(if (count == 0) ".." else ", ..");
        },
        1 => {
            try self.write(if (count == 0) ".." else ", ..");
            try self.writeSymbol(self.fresh(.value));
        },
        else => try self.writeTrailingComma(count),
    }
}

// Patterns

/// A pattern on the left of `=` or after `for`. A statement starting with a
/// tag, literal, or list parses as something other than a declaration.
fn writeDeclPattern(self: *Self, depth: u8) std.mem.Allocator.Error!void {
    switch (self.choose(if (depth == 0) 3 else 5)) {
        0 => try self.writeSymbol(self.fresh(.value)),
        1 => try self.writeEffectful(self.fresh(.function)),
        2 => {
            try self.write("_");
            try self.writeSymbol(self.fresh(.value));
        },
        3 => try self.writeRecordPattern(depth),
        4 => try self.writeTuplePattern(depth),
        else => unreachable,
    }
}

const PatternPosition = enum {
    /// A match branch pattern.
    branch,
    /// A lambda argument.
    arg,
};

fn writePattern(self: *Self, depth: u8, position: PatternPosition) std.mem.Allocator.Error!void {
    if (position == .branch and depth > 0 and self.choose(4) == 0) {
        const count = self.reader.intRangeAtMost(u8, 2, 3);
        for (0..count) |index| {
            if (index != 0) try self.write(" | ");
            try self.writePatternNoAlternatives(depth - 1);
        }
        return;
    }
    try self.writePatternNoAlternatives(depth);
    if (depth > 0 and self.choose(6) == 0) {
        try self.write(" as ");
        try self.writeSymbol(self.fresh(.value));
    }
}

fn writePatternNoAlternatives(self: *Self, depth: u8) std.mem.Allocator.Error!void {
    if (depth == 0) {
        try self.writeLeafPattern();
        return;
    }
    switch (self.choose(7)) {
        0 => try self.writeLeafPattern(),
        1 => {
            try self.writeTagName();
            try self.write("(");
            const count = self.reader.intRangeAtMost(u8, 1, 3);
            for (0..count) |index| {
                if (index != 0) try self.write(", ");
                try self.writePattern(depth - 1, .arg);
            }
            try self.writeTrailingComma(count);
            try self.write(")");
        },
        2 => try self.writeRecordPattern(depth),
        3 => {
            try self.write("[");
            const count = self.choose(4);
            const rest_index = if (count != 0 and self.reader.boolean()) self.choose(@intCast(count)) else count;
            for (0..count) |index| {
                if (index != 0) try self.write(", ");
                if (index == rest_index) {
                    try self.write("..");
                    if (self.reader.boolean()) {
                        try self.write(" as ");
                        try self.writeSymbol(self.fresh(.value));
                    }
                } else {
                    try self.writePattern(depth - 1, .arg);
                }
            }
            try self.writeTrailingComma(count);
            try self.write("]");
        },
        4 => try self.writeTuplePattern(depth),
        else => try self.writeSymbol(self.fresh(.value)),
    }
}

fn writeRecordPattern(self: *Self, depth: u8) std.mem.Allocator.Error!void {
    try self.write("{");
    const count = self.choose(4);
    for (0..count) |index| {
        try self.write(if (index == 0) " " else ", ");
        if (index + 1 == count and self.choose(3) == 0) {
            try self.write("..");
            if (self.reader.boolean()) try self.writeSymbol(self.fresh(.value));
            break;
        }
        try self.writeSymbol(self.fresh(.field));
        if (depth > 0 and self.reader.boolean()) {
            try self.write(": ");
            try self.writePattern(depth - 1, .arg);
        }
    }
    if (count != 0) try self.write(" ");
    try self.write("}");
}

fn writeTuplePattern(self: *Self, depth: u8) std.mem.Allocator.Error!void {
    try self.write("(");
    const count = self.reader.intRangeAtMost(u8, 2, 3);
    for (0..count) |index| {
        if (index != 0) try self.write(", ");
        try self.writePattern(depth -| 1, .arg);
    }
    try self.writeTrailingComma(count);
    try self.write(")");
}

fn writeLeafPattern(self: *Self) std.mem.Allocator.Error!void {
    switch (self.choose(9)) {
        0 => try self.write("_"),
        1 => {
            try self.write("_");
            try self.writeSymbol(self.fresh(.value));
        },
        2 => try self.writeTagName(),
        3 => try self.writeIntLiteral(),
        4 => try self.writePlainString(),
        5 => try self.writeSingleQuote(),
        6 => try self.writeTypedInt(),
        else => try self.writeSymbol(self.fresh(.value)),
    }
}

fn writeTagName(self: *Self) std.mem.Allocator.Error!void {
    if (self.choose(3) == 0) {
        try self.writeSymbol(self.fresh(.type));
        try self.write(".");
    }
    try self.writeSymbol(self.fresh(.tag));
}

// Expressions

fn writeExpr(self: *Self, depth: u8) std.mem.Allocator.Error!void {
    if (depth == 0) {
        try self.writeLeafExpr();
        return;
    }
    switch (self.choose(20)) {
        0 => try self.writeLeafExpr(),
        1 => {
            try self.write("|");
            const count = self.choose(3);
            for (0..count) |index| {
                if (index != 0) try self.write(", ");
                try self.writePattern(depth - 1, .arg);
            }
            try self.write("| ");
            try self.writeExpr(depth - 1);
        },
        2 => try self.writeBinOp(depth),
        3 => {
            try self.write(if (self.reader.boolean()) "-" else "!");
            switch (self.choose(3)) {
                0 => try self.writeSymbol(self.fresh(.value)),
                1 => {
                    try self.writeFunctionName();
                    try self.writeCallArgs(depth);
                },
                2 => {
                    try self.write("(");
                    try self.writeExpr(depth - 1);
                    try self.write(")");
                },
                else => unreachable,
            }
        },
        4 => {
            try self.write("if ");
            try self.writeExpr(depth - 1);
            try self.write(" ");
            try self.writeExprOrBlock(depth - 1);
            try self.write(" else ");
            try self.writeExprOrBlock(depth - 1);
        },
        5 => try self.writeMatch(depth),
        6 => {
            try self.write("{");
            try self.writeBlockStatements(depth, .body);
            try self.write("}");
        },
        7 => {
            try self.write("dbg ");
            try self.writeReceiver(depth - 1);
        },
        8 => {
            try self.write("crash ");
            try self.writePlainString();
        },
        10 => {
            try self.writeMultilineString();
        },
        else => try self.writePostfixExpr(depth),
    }
}

fn writeExprOrBlock(self: *Self, depth: u8) std.mem.Allocator.Error!void {
    if (self.reader.boolean()) {
        try self.write("{");
        try self.writeBlockStatements(depth, .body);
        try self.write("}");
    } else {
        try self.writePostfixExpr(depth);
    }
}

const bin_ops = [_][]const u8{ "+", "-", "*", "/", "//", "%", "==", "!=", "<", "<=", ">", ">=", "and", "or", "??", "..<", "..=" };

fn writeBinOp(self: *Self, depth: u8) std.mem.Allocator.Error!void {
    const count = self.reader.intRangeAtMost(u8, 2, 3);
    for (0..count) |index| {
        if (index != 0) {
            try self.write(" ");
            try self.write(bin_ops[self.choose(bin_ops.len)]);
            try self.write(" ");
        }
        try self.writePostfixExpr(depth - 1);
    }
}

fn writeMatch(self: *Self, depth: u8) std.mem.Allocator.Error!void {
    try self.write("match ");
    try self.writeReceiver(depth - 1);
    try self.write(" {");
    self.indent += 1;
    const count = self.reader.intRangeAtMost(u8, 1, 3);
    for (0..count) |_| {
        try self.writeNewline();
        try self.writePattern(depth - 1, .branch);
        if (self.choose(4) == 0) {
            try self.write(" if ");
            try self.writeReceiver(depth - 1);
        }
        try self.write(" => ");
        try self.writeExpr(depth - 1);
        if (self.reader.boolean()) try self.write(",");
    }
    self.indent -= 1;
    try self.writeNewline();
    try self.write("}");
}

/// An expression that can be the receiver of a postfix operation or the
/// operand of an operator without parentheses.
fn writePostfixExpr(self: *Self, depth: u8) std.mem.Allocator.Error!void {
    if (depth == 0) {
        try self.writeLeafExpr();
        return;
    }
    switch (self.choose(19)) {
        0 => try self.writeLeafExpr(),
        1 => {
            try self.write("(");
            try self.writeExpr(depth - 1);
            try self.write(")");
        },
        2 => try self.writeListExpr(depth),
        16 => {
            try self.writeReceiver(depth - 1);
            try self.write(".?");
            try self.writeSymbol(self.fresh(.field));
        },
        17 => {
            try self.writeTagName();
            try self.write(".");
            try self.writeCallArgs(depth);
        },
        18 => {
            try self.writeTagName();
            try self.write(".");
            try self.writeRecordExpr(depth, false);
        },
        3 => {
            try self.write("(");
            const count = self.reader.intRangeAtMost(u8, 2, 3);
            for (0..count) |index| {
                if (index != 0) try self.write(", ");
                try self.writeExpr(depth - 1);
            }
            try self.writeTrailingComma(count);
            try self.write(")");
        },
        4 => try self.writeRecordExpr(depth, false),
        5 => {
            try self.writeRecordExpr(depth, true);
        },
        6 => {
            try self.write("{");
            const count = self.choose(3);
            for (0..count) |index| {
                try self.write(if (index == 0) " " else ", ");
                try self.writeSymbol(self.fresh(.field));
                try self.write(": ");
                try self.writeExpr(depth - 1);
            }
            try self.writeTrailingComma(count);
            if (count != 0) try self.write(" ");
            try self.write("}.");
            try self.writeSymbol(self.fresh(.type));
        },
        7 => {
            try self.writeTagName();
            try self.writeCallArgs(depth);
        },
        8 => {
            if (self.reader.boolean()) {
                try self.writeSymbol(self.fresh(.type));
                try self.write(".");
            }
            try self.writeFunctionName();
            try self.writeCallArgs(depth);
        },
        9 => {
            try self.writeReceiver(depth - 1);
            try self.write(".");
            try self.writeFunctionName();
            try self.writeCallArgs(depth);
        },
        10 => {
            try self.writeReceiver(depth - 1);
            try self.write(".");
            try self.writeSymbol(self.fresh(.field));
        },
        11 => {
            try self.writeReceiver(depth - 1);
            try self.write(".");
            try self.writeTupleIndex();
        },
        12 => {
            try self.writeReceiver(depth - 1);
            try self.write(" |> ");
            try self.writeFunctionName();
            try self.writeCallArgs(depth);
        },
        13 => {
            try self.writeReceiver(depth - 1);
            try self.write("?");
        },
        14 => try self.writeInterpolatedString(depth),
        15 => {
            try self.writeSymbol(self.fresh(.type));
            try self.write(".");
            try self.writeSymbol(self.fresh(.value));
        },
        else => unreachable,
    }
}

/// The receiver of a field access, method call, or other suffix, which
/// cannot be a literal.
fn writeReceiver(self: *Self, depth: u8) std.mem.Allocator.Error!void {
    switch (self.choose(if (depth == 0) 2 else 6)) {
        0 => try self.writeSymbol(self.fresh(.value)),
        1 => {
            try self.write("$");
            try self.writeSymbol(self.fresh(.value));
        },
        2 => {
            try self.writeFunctionName();
            try self.writeCallArgs(depth);
        },
        3 => {
            try self.write("(");
            try self.writeExpr(depth - 1);
            try self.write(")");
        },
        4 => try self.writeRecordExpr(depth, false),
        5 => try self.writeListExpr(depth),
        else => unreachable,
    }
}

fn writeCallArgs(self: *Self, depth: u8) std.mem.Allocator.Error!void {
    try self.write("(");
    const count = self.choose(4);
    for (0..count) |index| {
        if (index != 0) try self.write(", ");
        try self.writeExpr(depth - 1);
    }
    try self.writeTrailingComma(count);
    try self.write(")");
}

fn writeListExpr(self: *Self, depth: u8) std.mem.Allocator.Error!void {
    try self.write("[");
    const count = self.choose(4);
    for (0..count) |index| {
        if (index != 0) try self.write(", ");
        try self.writeExpr(depth - 1);
    }
    try self.writeTrailingComma(count);
    try self.write("]");
}

fn writeRecordExpr(self: *Self, depth: u8, update: bool) std.mem.Allocator.Error!void {
    try self.write("{");
    var count: usize = 0;
    if (update) {
        try self.write(" ..");
        try self.writePostfixExpr(depth - 1);
        count += 1;
    }
    const field_count = if (update) self.reader.intRangeAtMost(u8, 1, 3) else self.choose(4);
    for (0..field_count) |_| {
        try self.write(if (count == 0) " " else ", ");
        try self.writeSymbol(self.fresh(.field));
        if (self.choose(3) != 0) {
            try self.write(": ");
            try self.writeExpr(depth - 1);
        }
        count += 1;
    }
    try self.writeTrailingComma(count);
    if (count != 0) try self.write(" ");
    try self.write("}");
}

fn writeLeafExpr(self: *Self) std.mem.Allocator.Error!void {
    switch (self.choose(12)) {
        0 => try self.writeIntLiteral(),
        1 => try self.writeFracLiteral(),
        2 => try self.writeTypedInt(),
        3 => try self.writeSingleQuote(),
        4 => try self.writePlainString(),
        5 => try self.writeTagName(),
        6 => try self.write("[]"),
        7 => try self.write("{}"),
        8 => try self.writeFunctionName(),
        9 => {
            try self.write("$");
            try self.writeSymbol(self.fresh(.value));
        },
        else => try self.writeSymbol(self.fresh(.value)),
    }
}

fn writeFunctionName(self: *Self) std.mem.Allocator.Error!void {
    if (self.reader.boolean()) {
        try self.writeEffectful(self.fresh(.function));
    } else {
        try self.writeSymbol(self.fresh(.function));
    }
}

fn writeInterpolatedString(self: *Self, depth: u8) std.mem.Allocator.Error!void {
    try self.write("\"");
    try self.writeStringChars();
    const count = self.reader.intRangeAtMost(u8, 1, 2);
    for (0..count) |_| {
        try self.write("${");
        try self.writePostfixExpr(depth - 1);
        try self.write("}");
        try self.writeStringChars();
    }
    try self.write("\"");
}

fn writeMultilineString(self: *Self) std.mem.Allocator.Error!void {
    const line_count = self.reader.intRangeAtMost(u8, 1, 2);
    for (0..line_count) |index| {
        if (index != 0) try self.writeNewline();
        try self.write("\\\\");
        try self.writeStringChars();
    }
    try self.writeNewline();
}

// Literals and names

fn writePlainString(self: *Self) std.mem.Allocator.Error!void {
    try self.write("\"");
    try self.writeStringChars();
    try self.write("\"");
}

/// String contents never contain `#`, so every comment in a generated module
/// is one the fuzzer inserted.
fn writeStringChars(self: *Self) std.mem.Allocator.Error!void {
    const chars = "abcxyz ";
    const len = self.choose(5);
    for (0..len) |_| {
        try self.output.append(self.allocator, chars[self.choose(chars.len)]);
    }
}

fn writeIntLiteral(self: *Self) std.mem.Allocator.Error!void {
    try self.output.print(self.allocator, "{d}", .{self.reader.intRangeAtMost(u16, 0, 999)});
}

fn writeFracLiteral(self: *Self) std.mem.Allocator.Error!void {
    try self.output.print(self.allocator, "{d}.{d}", .{ self.choose(100), self.choose(100) });
}

fn writeTypedInt(self: *Self) std.mem.Allocator.Error!void {
    try self.writeIntLiteral();
    try self.write(".U8");
}

fn writeSingleQuote(self: *Self) std.mem.Allocator.Error!void {
    const chars = "abcxyz";
    try self.output.print(self.allocator, "'{c}'", .{chars[self.choose(chars.len)]});
}

fn writeTupleIndex(self: *Self) std.mem.Allocator.Error!void {
    try self.output.print(self.allocator, "{d}", .{self.choose(3)});
}

fn writeTrailingComma(self: *Self, count: usize) std.mem.Allocator.Error!void {
    if (count != 0 and self.choose(3) == 0) try self.write(",");
}

fn fresh(self: *Self, kind: SymbolKind) Symbol {
    const symbol: Symbol = .{ .kind = kind, .id = self.next_id };
    self.next_id += 1;
    return symbol;
}

fn writeSymbol(self: *Self, symbol: Symbol) std.mem.Allocator.Error!void {
    try TypedCodeGenerator.writeSymbolTo(&self.output, self.allocator, symbol);
}

fn writeEffectful(self: *Self, symbol: Symbol) std.mem.Allocator.Error!void {
    try self.writeSymbol(symbol);
    try self.write("!");
}

fn writeNewline(self: *Self) std.mem.Allocator.Error!void {
    try self.write("\n");
    for (0..self.indent) |_| try self.write("    ");
}

fn write(self: *Self, text: []const u8) std.mem.Allocator.Error!void {
    try self.output.appendSlice(self.allocator, text);
}
