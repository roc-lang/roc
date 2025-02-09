const std = @import("std");
const tokenize = @import("parse/tokenize.zig");
const Region = @import("../base/Region.zig");

pub const IR = @import("parse/IR.zig");

pub const Node = struct {
    tag: Tag,
    data: Data,
    region: Region,

    pub const Tag = enum {
        Unary,
        Binary,
        // TODO
    };

    pub const Data = union {
        Unary: UnaryOpData,
        Binary: BinaryOpData,
        // Add more node data as needed
    };

    pub const UnaryOpData = struct {
        // TODO
    };

    pub const BinaryOpData = struct {
        // TODO
    };
};

pub const Diagnostic = struct {
    tag: Tag,
    region: Region,

    pub const Tag = enum {
        // TODO
    };
};

pub const Parser = struct {
    pos: usize,
    tokens: tokenize.TokenizedBuffer,
    nodes: std.MultiArrayList(Node),
    diagnostics: std.ArrayList(tokenize.Diagnostic),
    allocator: std.mem.Allocator,

    pub fn init(tokens: tokenize.TokenizedBuffer, allocator: std.mem.Allocator) Parser {
        return Parser{
            .pos = 0,
            .tokens = tokens,
            .nodes = std.MultiArrayList(Node){},
            .diagnostics = std.ArrayList(tokenize.Diagnostic).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn advance(self: *Parser) void {
        if (self.pos >= self.tokens.tokens.len) {
            return;
        }
        std.debug.print("advance {s}\n", .{@tagName(self.tokens.tokens.items(.tag)[self.pos])});
        self.pos += 1;
    }

    pub fn peek(self: *Parser) tokenize.Token.Tag {
        if (self.pos >= self.tokens.tokens.len) {
            return .EndOfFile;
        }
        return self.tokens.tokens.items(.tag)[self.pos];
    }

    // If the next token is a newline, consume it
    // Returns the indent level of the next line if it is a newline, otherwise null
    pub fn consumeNewline(self: *Parser) ?u16 {
        if (self.peek() != .Newline) {
            return null;
        }
        const indent = self.tokens.tokens.items(.offset)[self.pos];
        self.advance();
        return @intCast(indent);
    }

    // Returns the indent level of the next line if the next token is a newline, otherwise null
    pub fn peekNewline(self: *Parser) ?u16 {
        if (self.peek() != .Newline) {
            return null;
        }
        const indent = self.tokens.tokens.items(.offset)[self.pos];
        return @intCast(indent);
    }

    pub fn parseFile(self: *Parser) !void {
        while (self.peek() != .EndOfFile) {
            if (self.consumeNewline()) |indent| {
                std.debug.print("parseFile indent {d}\n", .{indent});
                std.debug.assert(indent == 0); // TODO: report an error
            }
            if (self.peek() == .EndOfFile) {
                break;
            }
            self.parseStmt(0);
        }
    }

    pub fn parseStmt(self: *Parser, base_indent: u16) void {
        switch (self.peek()) {
            .LowerIdent => {
                self.advance();
                if (self.peek() == .OpEquals) {
                    self.finishParseAssign(base_indent);
                    std.debug.print("parseStmt assign\n", .{});
                } else {
                    std.debug.print("parseStmt expr\n", .{});
                }
            },
            else => {
                std.debug.panic("todo: emit error, unexpected token {s}", .{@tagName(self.peek())});
            },
        }
    }

    pub fn parseExpr(self: *Parser) void {
        switch (self.peek()) {
            .LowerIdent => {
                self.advance();
                std.debug.print("parseExpr {s}\n", .{@tagName(self.peek())});
                // TODO: add node
            },
            .Int => {
                self.advance();
                std.debug.print("parseExpr {s}\n", .{@tagName(self.peek())});
                // TODO: add node
            },
            else => {
                std.debug.panic("todo: emit error", .{});
            },
        }
    }

    pub fn finishParseAssign(self: *Parser, base_indent: u16) void {
        std.debug.assert(self.peek() == .OpEquals);
        self.advance();
        if (self.consumeNewline()) |indent| {
            std.debug.print("startParseAssign indent {d}\n", .{indent});
            if (indent <= base_indent) {
                std.debug.panic("todo: emit error", .{});
            }

            self.parseStmt(indent);

            while (true) {
                if (self.peekNewline()) |i| {
                    if (i <= base_indent) {
                        break;
                    }
                    self.advance();
                } else {
                    break;
                }
                self.parseStmt(indent);
            }
        } else {
            self.parseExpr();
        }

        std.debug.print("finishParseAssign\n", .{});
    }
};
test "Parser advance and peek" {
    const allocator = std.heap.page_allocator;
    var tokens = try tokenize.TokenizedBuffer.init(allocator);
    // x =
    //     y = 1
    //     y
    try tokens.pushToken(.LowerIdent, 0, 1);
    try tokens.pushToken(.OpEquals, 0, 0);
    try tokens.pushNewline(4);
    try tokens.pushToken(.LowerIdent, 0, 0);
    try tokens.pushToken(.OpEquals, 0, 0);
    try tokens.pushToken(.Int, 0, 0);
    try tokens.pushNewline(4);
    try tokens.pushToken(.LowerIdent, 0, 0);
    try tokens.pushNewline(0);
    try tokens.pushToken(.EndOfFile, 0, 0);

    var parser = Parser.init(tokens, allocator);

    try parser.parseFile();

    // std.debug.assert(parser.nodes)
}
