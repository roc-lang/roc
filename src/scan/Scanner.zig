const std = @import("std");
const base = @import("base");
const Bitmasks = @import("Bitmasks.zig");
const TokenModule = @import("Token.zig");

const Span = base.Span;
const Region = base.Region;
const SrcBytes = base.SrcBytes;
const Self = @This();
const Blake3 = std.crypto.hash.Blake3;

/// The full raw source bytes of the file (or expression), guaranteed to end in newline
src_bytes: [:'\n']align(16) u8,

/// How many bytes within the raw source we've already processed.
src_bytes_chomped: usize = 0,

/// The current 64B page of masks we're working on.
bitmasks: Bitmasks,
pages_processed: usize = 0,

/// Tracks how deep we are in string interpolation nesting
str_interpolation_level: usize = 0,

/// UTF-8 validation state that persists across page boundaries
utf8_state: Bitmasks.Utf8State = @import("simd_utf8_faithful.zig").initState(),

/// Bitmap of invalid UTF-8 locations in current page
invalid_utf8_locs: u64 = 0,

pub fn parse(self: *Self) void {
    const token = self.nextToken(false) orelse return;
    const current_byte = self.currentSrcByte();
    const is_interpolating = self.str_interpolation_level > 0;

    // This should only mispredict in the highly unlikely event that we hit a (disallowed) newline
    // inside a string interpolation.
    if (current_byte == '\n' and is_interpolating) {
        @panic("TODO: report an error; newlines are disallowed inside string interpolations!");
    }

    switch (token) {
        .str_start => {
            if (current_byte == '\n' and token.typ != .multiline_str) {
                self.advance(1); // Skip past the newline
                @panic("TODO: The single-line string ended prematurely on a newline, so report an error and early return");
            }

            if (current_byte == '\\') {
                self.advance(1); // Skip past the backslash
                @panic("TODO: handle the backslash fully, then have scanner advance to next str seg end and GOTO .str_start =>");
            }

            // We only ever stop segments on "${", not "$" alone, so if we stopped on "$" that means it's interpolation.
            const is_interpolation_start = current_byte == '$';

            // Skip past the segment end, whatever it was (newline, quote, backslash, or interpolation start).
            // Advance an extra byte if this was a string interpolation (so, consume the '{' in "${")
            self.advance(1 + @intFromBool(is_interpolation_start));

            // Increment interpolation level if appropriate.
            self.str_interpolation_level += @intFromBool(is_interpolation_start);

            @panic("TODO: copy over the segment bytes, add segment to AST, proceed as normal");
        },
    }
}

/// Advance to the next token, skipping over whitespace. Returns null if we reached the end of the
/// source without encountering another token.
pub fn nextToken(self: *Self, _: bool) ?Token {
    // Should only mispredict at 64B "page" boundaries.
    _ = self.chompUntilNonzero(self.bitmask(.non_whitespace)) orelse return null;

    // Our initial index into src is equal to however many bytes we've chomped so far.
    const start = self.src_bytes_chomped;
    const current_byte = self.src_bytes[start];
    // It's safe to access idx + 1 bc we guarantee there's a newline at the end of src, and
    // we know current is not a newline. So if nothing else, idx + 1 will point to a newline.
    const next_byte = self.src_bytes[start + 1];
    // The first byte determines which bitmask we'll use to find the end
    const starts_with = TokenModule.Token.StartsWith.fromUtf8Byte(current_byte);
    // Branchlessly determine full token type, including 2-byte tokens (e.g. `:=` or `""`).
    // TODO need to inline this and do it with seamless advancing.
    const tokenAndLen = Token.fromBytePair(current_byte, next_byte);
    // Chomp either either 1 or 2 bytes depending on how many we matched.
    self.advance(tokenAndLen.len);
    // Branchlessly determine the bitmask that tells us where the end of this token is
    // TODO: properly implement this mapping based on actual token scanning needs
    const bitmasks_by_starts_with = comptime blk: {
        const len = @typeInfo(TokenModule.Token.StartsWith).Enum.fields.len;
        var arr: [len]Bitmasks.Bitmask = undefined;

        // Default all to non_whitespace for now - TODO: set proper masks
        for (&arr) |*item| {
            item.* = .non_whitespace;
        }

        // Override specific cases
        arr[@intFromEnum(TokenModule.Token.StartsWith.comment)] = .newlines;
        arr[@intFromEnum(TokenModule.Token.StartsWith.double_quote)] = .single_str_seg_ends;
        arr[@intFromEnum(TokenModule.Token.StartsWith.digit)] = .num_or_ident;
        arr[@intFromEnum(TokenModule.Token.StartsWith.uc)] = .num_or_ident;
        arr[@intFromEnum(TokenModule.Token.StartsWith.lc)] = .num_or_ident;
        arr[@intFromEnum(TokenModule.Token.StartsWith.underscore_or_dollar)] = .num_or_ident;

        break :blk arr;
    };
    const mask = self.bitmask(bitmasks_by_starts_with[@intFromEnum(starts_with)]);

    // TODO: incorporate stop_on_newline into the mask, so if we're in string interpolation we fail on newlines.

    // Should only mispredict at 64B "page" boundaries.
    _ = self.chompUntilNonzero(mask) orelse return null;

    // TODO: Track span information when needed
    return tokenAndLen.token;
}

/// These are all 1 or 2 source bytes. Since the parser gets a lookahead of 1 token,
/// this is sufficient for our 3-byte syntax (triple quotes and triple dots), because
/// for those the parser can do things like "I got a double dot token followed by a
/// single dot token, and their regions are back-to-back; handle this as triple dots."
///
/// We branchlessly turn source bytes into these. The way we do this is that we:
/// - Branchlessly classify the UTF-8 byte as one of 5 regions:
///   - Start of multibyte UTF-8 sequence (sign bit is 1)
///   - ASCII <= 32 (whitespace, and only valid ones are "\n", "\r", and "\t")
///   - ASCII 33-47 (operator)
///   - ASCII 48-57 (digit)
///   - ASCII 58-64 (operator)
///   - ASCII 65-90 (uppercase)
///   - ASCII 91-96 (operator)
///   - ASCII 97-122 (lowercase)
///   - ASCII 123-126 (operator)
///   - ASCII 127 (DEL - invalid)
pub const Token = enum {
    // ---------------------
    num_or_ident, // '0'..'9', 'A'..'Z', 'a'..'z', '_', and UTF-8 multibyte sequences
    whitespace, // ' ', '\t', '\r', '\n'
    comment, // '#'
    // ---------------------

    invalid_utf8, // Invalid UTF-8 sequence in this Region. Parser should report an error!
    ascii_lc, // [a-z]
    ascii_uc, // [A-Z]
    digit, // [0-9]
    double_quote_x2, // ""
    double_quote, // "
    single_quote_x2, // '' (will be an error)
    single_quote, // '
    // binops start here
    dot, // .
    dot_x2, // ..
    equals, // =
    equals_x2, // ==
    gt, // >
    lt, // <
    gte, // >=
    lte, // <=
    back_arrow, // <- (no thin vs thick distinction bc `<=` is already `lte`)
    thin_arrow, // ->
    thick_arrow, // =>
    open_round, // (
    close_round, // )
    open_curly, // {
    close_curly, // }
    open_square, // [
    close_square, // ]
    star, // *
    plus, // +
    minus, // -
    slash, // /
    slash_x2, // //
    colon, // :
    colon_equals, // :=
    // unsupported binops - other langs have these, so classify and give a helpful error
    colon_x2, // ::
    star_x2, // **
    minus_x2, // --
    plus_x2, // ++
    caret, // ^
    ampersand, // &
    ampersand_x2, // && (formatter will rewrite to `and`)
    pipe, // |
    pipe_x2, // || (formatter will rewrite to `and`)
    semicolon, // ; (formatter will drop these)
    semicolon_x2, // ;; (formatter will drop these)
};

pub const TokenWithRegion = struct {
    token: Token,
    region: Region,
};

/// Skip over the next n bytes.
pub fn skip(_: *Self, _: usize) void {
    // TODO
}

/// Used inside string interpolation, since interpolation may not contain newlines.
/// Also used by the formatter, which detects and preserves blank lines sometimes.
pub fn nextNonWhitespaceOrNewline(self: *Self) TokenWithRegion {
    const non_whitespace = self.bitmask(.non_whitespace);
    const newlines = self.bitmask(.newlines);
    const non_ws_or_newline = non_whitespace | newlines;
    _ = non_ws_or_newline;
    // TODO: implement
    return undefined;
}

/// Either "${" or a newline, backslash, or double quote.
/// All of these mean the parser has to stop, copy the bytes it's seen so far
/// into the BytesSlice, and then do some special processing (e.g. end the string,
/// possibly after reporting an "unclosed string" error, etc.)
pub fn nextSingleStrSegmentEnd(_: *Self) void {
    // TODO
}

/// Relevant for line comments, which go until the end of the line.
pub fn nextNewline(_: *Self) void {
    // TODO: implement
}

pub fn nextNonWhitespace(self: *Self) void {
    _ = self.bitmask(.non_whitespace);

    // TODO:
    // - bit shift by index_in_page
    // - count leading zeros
    // - if there's nothing, we need a new page! (misprediction ok - also, right here we can check whether we're at eof)
}

/// This is for when we're inside a string literal. Returns the next one of the following:
/// - double quote (indicating end of single-line string)
/// - "${" (indicating start of string interpolation)
/// - newline (indicating valid end of multiline str line, or unclosed single-line str)
/// - backslash (indicating an escape sequence that can't be copied directly into
///   backing memory and needs special processing)
pub fn nextMultiStrSegmentEnd(_: *Self) void {
    // TODO we need to have one (1) special-case dollar-curly per page. Just look at the
    // end, if the last bytes is $, lookahead to the next page (if we're not on the last
    // page), if it starts with "{" then set the last bit of dollar_curlies. Do this
    // *after* all the normal scanning stuff. Also set next_page_offset = 1 if we do.
    //
    // TODO Definitely also need to special-case consecutive backslashes on the boundary;
    // need to think more about that. Maybe the easiest: whenever we are doing
    // backslashes, we see if we're at the end, and if so, just special-case it by
    // scanning as long as it takes to get to the end. This can cause next_page_offset
    // to be more than 64B, which is fine!
}

fn nextPage(self: *Self) void {
    // Grab a slice of the next 64B (or less, if we run into the end of the src bytes.)
    const completed = self.pages_processed + 1;
    self.pages_processed = completed; // Increment this for the future.
    const page_bytes = self.src_bytes[completed * Bitmasks.page_size ..];

    // Load the bitmasks with UTF-8 validation
    self.bitmasks.load(@alignCast(page_bytes), &self.utf8_state, &self.invalid_utf8_locs);
    // TODO: if we are on the last page, need to fill the end of the 64B page with spaces.
    // We just discard them; they're harmless.

}

pub fn bitmask(self: *const Self, which_mask: Bitmasks.Bitmask) u64 {
    return self.bitmasks.masks[@intFromEnum(which_mask)];
}

/// Get the current byte we're looking at in the source.
pub fn currentSrcByte(self: *const Self) u8 {
    return self.src_bytes[self.src_bytes_chomped];
}

/// Advance past n bytes in the source.
pub fn advance(self: *Self, n: usize) void {
    self.src_bytes_chomped += n;
}

/// Chomp bytes until we hit a non-zero bit in the given mask.
/// Returns the number of bytes chomped, or null if we hit the end.
pub fn chompUntilNonzero(self: *Self, mask: u64) ?usize {
    // TODO: implement this properly
    _ = self;
    _ = mask;
    return 0;
}
