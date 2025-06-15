//! The buffer that accumulates tokens.

const std = @import("std");
const base = @import("../../../base.zig");
const collections = @import("../../../collections.zig");
const Token = @import("Token.zig");
const TokenizedBuffer = @import("Buffer.zig");

const Comment = Token.Comment;
const exitOnOom = collections.utils.exitOnOom;

tokens: Token.List,
env: *base.ModuleEnv,

/// Initialize a tokenized buffer with the specified initial capacity.
pub fn initCapacity(env: *base.ModuleEnv, capacity: usize) TokenizedBuffer {
    var tokens = Token.List{};
    tokens.ensureTotalCapacity(env.gpa, capacity) catch |err| exitOnOom(err);
    return TokenizedBuffer{
        .tokens = tokens,
        .env = env,
    };
}

/// Free the memory used by this tokenized buffer.
pub fn deinit(self: *TokenizedBuffer) void {
    self.tokens.deinit(self.env.gpa);
}

/// Resolve a token index to its corresponding region in the source code.
pub fn resolve(self: *const TokenizedBuffer, token: Token.Idx) base.Region {
    const tag = self.tokens.items(.tag)[@intCast(token)];
    const start = self.tokens.items(.offset)[@intCast(token)];
    const extra = self.tokens.items(.extra)[@intCast(token)];
    switch (tag) {
        .LowerIdent,
        .DotLowerIdent,
        .NoSpaceDotLowerIdent,
        .MalformedUnicodeIdent,
        .MalformedDotUnicodeIdent,
        .DotUpperIdent,
        .NoSpaceDotUpperIdent,
        .UpperIdent,
        => {
            return self.env.idents.getRegion(extra.interned);
        },
        else => {
            const end = start + extra.length;
            return .{ .start = base.Region.Position{ .offset = start }, .end = base.Region.Position{ .offset = end } };
        },
    }
}

/// Loads the current token if it is an identifier.
/// Otherwise returns null.
pub fn resolveIdentifier(self: *TokenizedBuffer, token: Token.Idx) ?base.Ident.Idx {
    const tag = self.tokens.items(.tag)[@intCast(token)];
    const extra = self.tokens.items(.extra)[@intCast(token)];
    switch (tag) {
        .LowerIdent,
        .DotLowerIdent,
        .NoSpaceDotLowerIdent,
        .MalformedUnicodeIdent,
        .MalformedDotUnicodeIdent,
        .DotUpperIdent,
        .NoSpaceDotUpperIdent,
        .UpperIdent,
        => {
            return extra.interned;
        },
        else => {
            return null;
        },
    }
}

/// Pushes a token with the given tag, token offset, and extra.
/// Add a normal token with the specified tag, offset, and length to the buffer.
pub fn pushTokenNormal(self: *TokenizedBuffer, tag: Token.Tag, tok_offset: u32, length: u32) void {
    self.tokens.append(self.env.gpa, .{
        .tag = tag,
        .offset = tok_offset,
        .extra = .{ .length = length },
    }) catch |err| exitOnOom(err);
}

/// Add a newline token to the buffer, optionally with an associated comment.
pub fn pushNewline(self: *TokenizedBuffer, comment: ?Comment) void {
    var token = Token{
        .tag = .Newline,
        .offset = 0, // store the Comment start - if it is exists here
        .extra = .{ .length = 0 },
    };
    if (comment) |c| {
        token.offset = c.begin;
        token.extra = .{ .length = if (c.end > c.begin) c.end - c.begin else 0 };
    }
    self.tokens.append(self.env.gpa, token) catch |err| exitOnOom(err);
}

/// Returns the offset of the token at index `idx`.
pub fn offset(self: *TokenizedBuffer, idx: u32) u32 {
    // newline tokens don't have offsets - that field is used to store the indent.
    std.debug.assert(self.tokens.items(.tag)[idx] != .Newline);
    return self.tokens.items(.offset)[@intCast(idx)];
}
