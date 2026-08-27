//! Name rules for `textDocument/rename`.
//!
//! A rename may only change what a binding is called. In Roc part of what a
//! name *means* is spelled in the name itself: a trailing `!` marks a value
//! effectful, a leading `_` marks it deliberately unused, and a leading `$`
//! marks it reassignable, while upper and lower case separate types and tags
//! from values. Rewriting `foo` to `foo!` is therefore not a rename—it is an
//! edit the type checker would reject, or worse, silently accept with
//! different meaning.
//!
//! The rules are read off the real tokenizer and `Ident.Attributes` rather
//! than restated here, so a change to what Roc considers an identifier cannot
//! drift away from what rename accepts.

const std = @import("std");
const Allocator = std.mem.Allocator;
const base = @import("base");
const can = @import("can");
const parse = @import("parse");

const Ident = base.Ident;
const Token = parse.tokenize.Token;

/// Why a requested new name cannot be used.
pub const Rejection = enum {
    /// The text is not exactly one Roc identifier.
    not_an_identifier,
    /// The text is an identifier, but not the same kind of identifier as the
    /// name being replaced.
    changes_ident_kind,

    /// A message suitable for reporting back to the editor.
    pub fn message(self: Rejection) []const u8 {
        return switch (self) {
            .not_an_identifier => "the new name must be a single Roc identifier",
            .changes_ident_kind => "the new name must be the same kind of identifier as the old one: " ++
                "upper- and lowercase names are not interchangeable, and the `!`, `_` and `$` markers carry meaning",
        };
    }
};

/// What a name spells, as the compiler reads it.
const NameShape = struct {
    tag: Token.Tag,
    attributes: Ident.Attributes,

    fn eql(self: NameShape, other: NameShape) bool {
        return self.tag == other.tag and
            self.attributes.effectful == other.attributes.effectful and
            self.attributes.ignored == other.attributes.ignored and
            self.attributes.reassignable == other.attributes.reassignable;
    }
};

/// Check whether `new_name` may replace `old_name`.
///
/// Returns null when the rename is allowed. Both names are run through the
/// tokenizer, so "is this an identifier" is answered by the compiler rather
/// than by a rule restated here.
pub fn checkNewName(
    allocator: Allocator,
    old_name: []const u8,
    new_name: []const u8,
) Allocator.Error!?Rejection {
    const new_shape = try nameShape(allocator, new_name) orelse return .not_an_identifier;

    // The old name came out of the CIR, so it tokenizes by construction; if it
    // somehow does not, refuse rather than guess what kind of name it was.
    const old_shape = try nameShape(allocator, old_name) orelse return .changes_ident_kind;

    if (!new_shape.eql(old_shape)) return .changes_ident_kind;
    return null;
}

/// Tokenize `text` on its own and report what single identifier it spells.
///
/// Returns null when the text is anything else: empty, several tokens, a
/// keyword, punctuation, or an identifier the tokenizer complained about.
fn nameShape(allocator: Allocator, text: []const u8) Allocator.Error!?NameShape {
    if (text.len == 0) return null;

    var module_env = try can.ModuleEnv.init(allocator, text);
    defer module_env.deinit();

    var messages: [4]parse.tokenize.Diagnostic = undefined;
    var tokenizer = try parse.tokenize.Tokenizer.init(&module_env.common, allocator, text, &messages);
    try tokenizer.tokenize(allocator);
    var output = tokenizer.finishAndDeinit();
    defer output.tokens.deinit(allocator);

    if (output.messages.len != 0 or output.extra_messages_dropped != 0) return null;

    const tags = output.tokens.tokens.items(.tag);
    const regions = output.tokens.tokens.items(.region);

    // One identifier and the end of input, and the identifier must span the
    // whole text—otherwise the editor sent something like `foo bar` or `foo=`.
    if (tags.len != 2 or tags[1] != .EndOfFile) return null;
    if (!isIdentifierTag(tags[0])) return null;
    if (regions[0].start.offset != 0 or regions[0].end.offset != text.len) return null;

    return NameShape{
        .tag = tags[0],
        .attributes = Ident.Attributes.fromString(text),
    };
}

/// Whether a token tag names something a binding can be called.
fn isIdentifierTag(tag: Token.Tag) bool {
    return tag == .LowerIdent or tag == .UpperIdent or tag == .NamedUnderscore;
}
