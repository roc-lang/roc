const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const Token = @import("Token.zig");

const Region = base.Region;
const Allocator = std.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const SafeList = collections.SafeList;
const ByteSlices = collections.ByteSlices;

pub const Self = @This();

gpa: Allocator,
src: [:0]const u8,
problems: *SafeList(Diagnostic),
byte_slices: *collections.ByteSlices,
pos: usize = 0, // Initial byte position within src
pending_string_interpolation: bool = false, // Were we about to begin a string interpolation?
just_ended_interpolation: bool = false, // Did we just finish a string interpolation?
unclosed_curlies: u32 = 0, // Used to tell when string interpolation ends
inside_string: enum { None, SingleQuote, TripleQuote } = .None, // What type of string we're working on tokenizing

/// Get the next token from the iterator, pushing diagnostics if there are errors.
/// Returns Token.EndOfFile when it has reached the end of the file (or string).
pub fn next(self: *Self) Allocator.Error!Token {
    debugVerifySrc(self.src);

    if (self.pending_string_interpolation) {
        self.pending_string_interpolation = false;
        return Token.no_payload(.OpenStringInterpolation, @intCast(self.pos), @intCast(self.pos));
    }

    if (self.just_ended_interpolation) {
        self.just_ended_interpolation = false;
        // We just ended an interpolation, so pick up where we left off with our string parsing.
        switch (self.inside_string) {
            .None => unreachable,
            .SingleQuote => return self.tokenizeSingleQuoteString(),
            .TripleQuote => return self.tokenizeTripleQuoteString(),
        }
    }

    while (true) { // We keep checking the byte until we hit the null terminator
        std.debug.assert(self.pos < self.src.len); // We should have exited via null terminator before this ended

        const start: u32 = @intCast(self.pos);
        const byte = self.buf[self.pos];

        switch (byte) {
            // Comments and doc comments
            '#' => {
                self.pos += 1; // Chomp the '#'

                const is_doc = self.src[self.pos + 1] == '#'; // Detect if it's a doc comment (##)
                self.pos += is_doc; // Branchlessly chomp the second '#'

                // Find the newline that ends the comment. (src ends in newline followed by zeros, so this is safe.)
                // TODO: Could try this with SIMD if we terminate src with 15 zeros after the newline.
                while (self.buf[self.pos] != '\n') self.pos += 1;

                return Token.no_payload(if (is_doc) .DocComment else .LineComment, @intCast(start), @intCast(self.pos));
            },
            // Spaces, tabs, and carriage returns all get discarded. (We don't verify anything about carriage returns.)
            ' ', '\t', '\r' => self.pos += 1, // TODO: Could try this with SIMD if we end src with 15 zeros after the newline.
            '\n' => {
                self.pos += 1; // Advance past the '\n'

                // Emit a BlankLine token if this \n has another \n after it with only whitespace, if anything, in between.
                while (true) {
                    switch (self.buf[self.pos]) {
                        ' ', '\t', '\r' => self.pos += 1, // Discard spaces, tabs, and carriage returns.
                        '\n' => {
                            // 2+ newlines separated only by whitespace, if anything? That's a blank line!
                            const end: u32 = @intCast(self.pos);
                            while (true) {
                                // Discard all whitespace following the blank line,
                                // so that we never emit consecutive BlankLine tokens.
                                switch (self.buf[self.pos]) {
                                    ' ', '\t', '\r', '\n' => self.pos += 1,
                                    else => return Token.no_payload(.BlankLine, start, end),
                                }
                            }
                        },
                        else => break, // This wasn't a blank line; continue as normal.
                    }
                }
            },
            '"' => {
                // Advance past after the opening quote
                self.pos += 1;

                // If it's a triple-quote string, go down that path.
                if (self.src[self.pos] == '"' and self.src[self.pos + 1] == '"') {
                    self.pos += 2; // Advance past the second and third quote
                    return self.tokenizeTripleQuoteString();
                }

                return self.tokenizeSingleQuoteString();
            },
            'a'...'z' => return try self.tokenizeLowerIdentOrKeyword(gpa, start),
            'A'...'Z' => return try self.tokenizeUpperIdent(gpa, start),
            '_' => {
                self.pos += 1; // Advance past the underscore

                if (isValidIdentStart(self.buf[self.pos])) return try self.tokenizeLowerIdentOrKeyword(gpa, start);

                return Token.no_payload(.Underscore, start, @intCast(*pos));
            },
            '0'...'9' => return try self.tokenizeNumber(gpa, start),
            '/' => return chompOneOfTwo(&self.pos, self.src, '/', .OpSlash, .OpDoubleSlash),
            '-' => return chompOneOfTwo(&self.pos, self.src, '>', .OpMinus, .OpArrow), // TODO handle unary minus in Parser
            '=' => return chompOneOfThree(&self.pos, '=', '>', .OpAssign, .OpEquals, .OpThinArrow),
            '(' => return chompOne(&self.pos, .OpenRound),
            ')' => return chompOne(&self.pos, .CloseRound),
            '[' => return chompOne(&self.pos, .OpenSquare),
            ']' => return chompOne(&self.pos, .CloseSquare),
            '{' => {
                self.unclosed_curlies += 1;
                return chompOne(&self.pos, .OpenCurly);
            },
            '}' => {
                self.unclosed_curlies -= 1;
                // If we just closed the last curly inside a string interpolation, emit CloseStringInterpolation.
                self.just_ended_interpolation = self.unclosed_curlies == 0 and self.inside_string != .None;
                const tag = if (self.just_ended_interpolation) .CloseStringInterpolation else .CloseCurly;
                return chompOne(&self.pos, tag);
            },
            ',' => return chompOne(&self.pos, .Comma),
            ':' => return chompOneOfTwo(&self.pos, self.src, '=', .OpColon, .OpColonEqual),
            '+' => return chompOne(&self.pos, .OpPlus),
            '*' => return chompOne(&self.pos, .OpStar),
            '.' => {
                // ".", "..", and "..."
                self.pos += 1;

                if (self.pos >= self.buf.len) return Token.no_payload(.Dot, start, @intCast(*pos));

                if (self.buf[self.pos] == '.') {
                    self.pos += 1;

                    if (self.pos >= self.buf.len) return Token.no_payload(.DoubleDot, start, @intCast(*pos));

                    if (self.buf[self.pos] == '.') {
                        self.pos += 1;
                        return Token.no_payload(.TripleDot, start, @intCast(self.pos));
                    }

                    return Token.no_payload(.DoubleDot, start, @intCast(self.pos));
                }

                return Token.no_payload(.Dot, start, @intCast(self.pos));
            },
            '<' => return chompOneOfThree(self.buf, &self.pos, '=', '-', .OpLessThan, .OpLessThanOrEq, .OpBackArrow),
            '>' => return chompOneOfTwo(&self.pos, self.src, '=', .OpGreaterThan, .OpGreaterThanOrEq),
            '!' => return chompOneOfTwo(&self.pos, self.src, '=', .OpBang, .OpNotEquals),
            '&' => {
                // TODO should treat both of these as malformed, report diagnostic, etc.
                return chompOneOfTwo(&self.pos, self.src, '&', .OpAmpersand, .OpAnd);
            },
            '|' => {
                // TODO should treat both of these as malformed, report diagnostic, etc.
                return chompOneOfTwo(&self.pos, self.src, '&', .OpBar, .OpOr);
            },
            '?' => return chompOneOfTwo(&self.pos, self.src, '?', .OpDouble, .OpDoubleQuestion),
            '\\' => return chompOne(&self.pos, .OpBackslash),
            '\'' => return try self.tokenizeSingleQuote(gpa, start),
            '#' => {
                self.pos += 1;

                // Check if it's a doc comment (##)
                if (self.pos >= self.buf.len) return Token.no_payload(.Comment, start, @intCast(*pos));
                const is_doc = self.buf[self.pos] == '#';
                self.pos += is_doc;

                // Find the end of the comment (newline or EOF)
                while (self.pos < self.buf.len and self.buf[self.pos] != '\n') {
                    self.pos += 1;
                }

                // Don't include the newline in the comment.
                // Don't advance past the newline either, so it can participate in blank line detection.
                return Token.no_payload(if (is_doc) .DocComment else .LineComment, start, @intCast(self.pos));
            },
            else => { // Anything else is an unsupported  token
                self.pos += 1;
                // TODO Keep chomping until we hit a valid token, then report *one* diagnostic spanning the chomped region
                return Token{
                    .tag = .MalformedUnknownToken,
                    .region = Region{
                        .start = Region.Position{ .offset = @intCast(start) },
                        .end = Region.Position{ .offset = @intCast(self.pos) },
                    },
                    .payload = .none,
                };
            },
        }
    }

    return Token{
        .tag = .EndOfFile,
        .region = Region.from_raw_offsets(@intCast(self.pos), @intCast(self.pos)),
        .payload = .none,
    };
}

fn tokenizeLowerIdentOrKeyword(self: *Self, gpa: std.mem.Allocator, start_pos: usize) std.mem.Allocator.Error!Token {
    // Consume identifier characters
    while (self.pos < self.buf.len) {
        const c = self.buf[self.pos];
        if ((c >= 'a' and c <= 'z') or
            (c >= 'A' and c <= 'Z') or
            (c >= '0' and c <= '9') or
            c == '_')
        {
            self.pos += 1;
        } else {
            break;
        }
    }

    // Get the identifier text
    const ident_text = self.buf[start_pos..self.pos];

    // Check if it's a keyword or a named underscore
    const tag: Token.Tag = if (ident_text[0] == '_')
        .NamedUnderscore
    else if (std.mem.eql(u8, ident_text, "and")) .OpAnd else if (std.mem.eql(u8, ident_text, "app")) .KwApp else if (std.mem.eql(u8, ident_text, "as")) .KwAs else if (std.mem.eql(u8, ident_text, "crash")) .KwCrash else if (std.mem.eql(u8, ident_text, "dbg")) .KwDbg else if (std.mem.eql(u8, ident_text, "debug")) .KwDebug else if (std.mem.eql(u8, ident_text, "else")) .KwElse else if (std.mem.eql(u8, ident_text, "expect")) .KwExpect else if (std.mem.eql(u8, ident_text, "exposes")) .KwExposes else if (std.mem.eql(u8, ident_text, "exposing")) .KwExposing else if (std.mem.eql(u8, ident_text, "for")) .KwFor else if (std.mem.eql(u8, ident_text, "generates")) .KwGenerates else if (std.mem.eql(u8, ident_text, "has")) .KwHas else if (std.mem.eql(u8, ident_text, "hosted")) .KwHosted else if (std.mem.eql(u8, ident_text, "if")) .KwIf else if (std.mem.eql(u8, ident_text, "implements")) .KwImplements else if (std.mem.eql(u8, ident_text, "import")) .KwImport else if (std.mem.eql(u8, ident_text, "imports")) .KwImports else if (std.mem.eql(u8, ident_text, "in")) .KwIn else if (std.mem.eql(u8, ident_text, "interface")) .KwInterface else if (std.mem.eql(u8, ident_text, "match")) .KwMatch else if (std.mem.eql(u8, ident_text, "module")) .KwModule else if (std.mem.eql(u8, ident_text, "or")) .OpOr else if (std.mem.eql(u8, ident_text, "package")) .KwPackage else if (std.mem.eql(u8, ident_text, "packages")) .KwPackages else if (std.mem.eql(u8, ident_text, "platform")) .KwPlatform else if (std.mem.eql(u8, ident_text, "provides")) .KwProvides else if (std.mem.eql(u8, ident_text, "requires")) .KwRequires else if (std.mem.eql(u8, ident_text, "return")) .KwReturn else if (std.mem.eql(u8, ident_text, "var")) .KwVar else if (std.mem.eql(u8, ident_text, "where")) .KwWhere else if (std.mem.eql(u8, ident_text, "while")) .KwWhile else if (std.mem.eql(u8, ident_text, "with")) .KwWith else .LowerIdent;

    // For identifiers, we need to intern them
    if (tag == .LowerIdent or tag == .NamedUnderscore) {
        const ident = base.Ident.for_text(ident_text);
        const interned = try self.env.idents.insert(gpa, ident);
        return Token{
            .tag = tag,
            .region = Region{
                .start = Region.Position{ .offset = @intCast(start_pos) },
                .end = Region.Position{ .offset = @intCast(self.pos) },
            },
            .payload = .{ .interned = interned },
        };
    } else {
        // Keywords don't need interning
        return Token{
            .tag = tag,
            .region = Region{
                .start = Region.Position{ .offset = @intCast(start_pos) },
                .end = Region.Position{ .offset = @intCast(self.pos) },
            },
            .payload = .none,
        };
    }
}

fn tokenizeUpperIdent(self: *Self, gpa: std.mem.Allocator, start_pos: usize) std.mem.Allocator.Error!Token {
    // Consume identifier characters
    while (self.pos < self.buf.len) {
        const c = self.buf[self.pos];
        if ((c >= 'a' and c <= 'z') or
            (c >= 'A' and c <= 'Z') or
            (c >= '0' and c <= '9') or
            c == '_')
        {
            self.pos += 1;
        } else {
            break;
        }
    }

    // Get the identifier text and intern it
    const ident_text = self.buf[start_pos..self.pos];
    const ident = base.Ident.for_text(ident_text);
    const interned = try self.env.idents.insert(gpa, ident);

    return Token{
        .tag = .UpperIdent,
        .region = Region{
            .start = Region.Position{ .offset = @intCast(start_pos) },
            .end = Region.Position{ .offset = @intCast(self.pos) },
        },
        .payload = .{ .interned = interned },
    };
}

fn tokenizeNumber(self: *Self, gpa: std.mem.Allocator, start_pos: usize) std.mem.Allocator.Error!Token {
    // Start position is at the first digit, move past it
    self.pos = @intCast(start_pos + 1);

    // Check for hex, octal, binary prefixes
    if (self.buf[start_pos] == '0' and self.pos < self.buf.len) {
        const next_char = self.buf[self.pos];
        if (next_char == 'x' or next_char == 'X') {
            // Hex number
            self.pos += 1;
            // Consume hex digits
            var has_digits = false;
            while (self.pos < self.buf.len) {
                const c = self.buf[self.pos];
                if ((c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F') or c == '_') {
                    if (c != '_') has_digits = true;
                    self.pos += 1;
                } else {
                    break;
                }
            }
            if (!has_digits) {
                // No digits after 0x - malformed
                return Token{
                    .tag = .MalformedNumberNoDigits,
                    .region = Region{
                        .start = Region.Position{ .offset = @intCast(start_pos) },
                        .end = Region.Position{ .offset = @intCast(self.pos) },
                    },
                    .payload = .none,
                };
            }
            // Parse the hex number and convert to decimal string
            const hex_text = self.buf[start_pos + 2 .. self.pos]; // Skip "0x"

            // Parse hex to integer
            var value: u128 = 0;
            for (hex_text) |c| {
                if (c == '_') continue; // Skip underscores
                const digit: u8 = if (c >= '0' and c <= '9')
                    c - '0'
                else if (c >= 'a' and c <= 'f')
                    c - 'a' + 10
                else if (c >= 'A' and c <= 'F')
                    c - 'A' + 10
                else
                    unreachable; // Already validated above
                value = value * 16 + digit;
            }

            // Convert to decimal string
            var buf: [40]u8 = undefined; // u128 max is 39 digits
            const decimal_str = std.fmt.bufPrint(&buf, "{}", .{value}) catch unreachable;
            const bytes_idx = try self.byte_slices.append(gpa, decimal_str);

            return Token{
                .tag = .IntBase,
                .region = Region{
                    .start = Region.Position{ .offset = @intCast(start_pos) },
                    .end = Region.Position{ .offset = @intCast(self.pos) },
                },
                .payload = .{ .bytes_idx = bytes_idx },
            };
        } else if (next_char == 'b' or next_char == 'B') {
            // Binary number
            self.pos += 1;
            var has_digits = false;
            while (self.pos < self.buf.len) {
                const c = self.buf[self.pos];
                if (c == '0' or c == '1' or c == '_') {
                    if (c != '_') has_digits = true;
                    self.pos += 1;
                } else {
                    break;
                }
            }
            if (!has_digits) {
                return Token{
                    .tag = .MalformedNumberNoDigits,
                    .region = Region{
                        .start = Region.Position{ .offset = @intCast(start_pos) },
                        .end = Region.Position{ .offset = @intCast(self.pos) },
                    },
                    .payload = .none,
                };
            }
            // Parse the binary number and convert to decimal string
            const bin_text = self.buf[start_pos + 2 .. self.pos]; // Skip "0b"

            // Parse binary to integer
            var value: u128 = 0;
            for (bin_text) |c| {
                if (c == '_') continue; // Skip underscores
                const digit: u8 = c - '0'; // '0' or '1'
                value = value * 2 + digit;
            }

            // Convert to decimal string
            var buf: [40]u8 = undefined; // u128 max is 39 digits
            const decimal_str = std.fmt.bufPrint(&buf, "{}", .{value}) catch unreachable;
            const bytes_idx = try self.byte_slices.append(gpa, decimal_str);

            return Token{
                .tag = .IntBase,
                .region = Region{
                    .start = Region.Position{ .offset = @intCast(start_pos) },
                    .end = Region.Position{ .offset = @intCast(self.pos) },
                },
                .payload = .{ .bytes_idx = bytes_idx },
            };
        } else if (next_char == 'o' or next_char == 'O') {
            // Octal number
            self.pos += 1;
            var has_digits = false;
            while (self.pos < self.buf.len) {
                const c = self.buf[self.pos];
                if ((c >= '0' and c <= '7') or c == '_') {
                    if (c != '_') has_digits = true;
                    self.pos += 1;
                } else {
                    break;
                }
            }
            if (!has_digits) {
                return Token{
                    .tag = .MalformedNumberNoDigits,
                    .region = Region{
                        .start = Region.Position{ .offset = @intCast(start_pos) },
                        .end = Region.Position{ .offset = @intCast(self.pos) },
                    },
                    .payload = .none,
                };
            }
            // Parse the octal number and convert to decimal string
            const oct_text = self.buf[start_pos + 2 .. self.pos]; // Skip "0o"

            // Parse octal to integer
            var value: u128 = 0;
            for (oct_text) |c| {
                if (c == '_') continue; // Skip underscores
                const digit: u8 = c - '0'; // '0' through '7'
                value = value * 8 + digit;
            }

            // Convert to decimal string
            var buf: [40]u8 = undefined; // u128 max is 39 digits
            const decimal_str = std.fmt.bufPrint(&buf, "{}", .{value}) catch unreachable;
            const bytes_idx = try self.byte_slices.append(gpa, decimal_str);

            return Token{
                .tag = .IntBase,
                .region = Region{
                    .start = Region.Position{ .offset = @intCast(start_pos) },
                    .end = Region.Position{ .offset = @intCast(self.pos) },
                },
                .payload = .{ .bytes_idx = bytes_idx },
            };
        }
    }

    // Regular decimal number parsing
    var has_decimal = false;
    var has_exponent = false;

    while (self.pos < self.buf.len) {
        const c = self.buf[self.pos];
        if (c >= '0' and c <= '9') {
            self.pos += 1;
        } else if (c == '.' and !has_decimal and !has_exponent) {
            // Check if it's a decimal point
            if (self.pos + 1 < self.buf.len) {
                const next_char = self.buf[self.pos + 1];
                if (next_char >= '0' and next_char <= '9') {
                    // It's a decimal number
                    has_decimal = true;
                    self.pos += 1;
                    // Continue consuming digits after decimal
                    while (self.pos < self.buf.len) {
                        const dc = self.buf[self.pos];
                        if (dc >= '0' and dc <= '9') {
                            self.pos += 1;
                        } else {
                            break;
                        }
                    }
                } else {
                    break; // Dot is not part of the number
                }
            } else {
                break;
            }
        } else if ((c == 'e' or c == 'E') and !has_exponent) {
            // Check for scientific notation
            if (self.pos + 1 < self.buf.len) {
                var exp_pos = self.pos + 1;
                const next_char = self.buf[exp_pos];

                // Check for optional + or - after e
                if (next_char == '+' or next_char == '-') {
                    exp_pos += 1;
                }

                // Check if there's at least one digit after e/E
                if (exp_pos < self.buf.len and self.buf[exp_pos] >= '0' and self.buf[exp_pos] <= '9') {
                    has_exponent = true;
                    self.pos = exp_pos;
                    // Consume exponent digits
                    while (self.pos < self.buf.len) {
                        const ec = self.buf[self.pos];
                        if (ec >= '0' and ec <= '9') {
                            self.pos += 1;
                        } else {
                            break;
                        }
                    }
                } else {
                    break; // 'e' is not part of the number
                }
            } else {
                break;
            }
        } else {
            break;
        }
    }

    const num_text = self.buf[start_pos..self.pos];

    // Check if it's a float
    if (has_decimal or has_exponent) {
        // Try to parse as SmallDec first
        if (parseSmallDec(num_text)) |small_dec| {
            return Token{
                .tag = .Float,
                .region = Region{
                    .start = Region.Position{ .offset = @intCast(start_pos) },
                    .end = Region.Position{ .offset = @intCast(self.pos) },
                },
                .payload = .{ .frac_literal_small = small_dec },
            };
        } else {
            // Fallback to ByteSlices for large numbers or scientific notation
            const bytes_idx = try self.byte_slices.append(gpa, num_text);
            return Token{
                .tag = .Float,
                .region = Region{
                    .start = Region.Position{ .offset = @intCast(start_pos) },
                    .end = Region.Position{ .offset = @intCast(self.pos) },
                },
                .payload = .{ .bytes_idx = bytes_idx },
            };
        }
    }

    // Parse as integer
    const value = std.fmt.parseInt(i32, num_text, 10) catch {
        // Number too big or invalid, store in ByteSlices
        const bytes_idx = try self.byte_slices.append(gpa, num_text);
        return Token{
            .tag = .Int,
            .region = Region{
                .start = Region.Position{ .offset = @intCast(start_pos) },
                .end = Region.Position{ .offset = @intCast(self.pos) },
            },
            .payload = .{ .bytes_idx = bytes_idx },
        };
    };

    return Token{
        .tag = .Int,
        .region = Region{
            .start = Region.Position{ .offset = @intCast(start_pos) },
            .end = Region.Position{ .offset = @intCast(self.pos) },
        },
        .payload = .{ .num_literal_i32 = value },
    };
}

inline fn chompEscapedChar(self: *Self, escaped: u8, bytes_idx: ByteSlices.Idx) Allocator.Error!void {
    self.pos += 1;
    switch (escaped) {
        'n' => try self.byte_slices.extendLast(self.gpa, bytes_idx, &[_]u8{'\n'}),
        'r' => try self.byte_slices.extendLast(self.gpa, bytes_idx, &[_]u8{'\r'}),
        't' => try self.byte_slices.extendLast(self.gpa, bytes_idx, &[_]u8{'\t'}),
        '\\' => try self.byte_slices.extendLast(self.gpa, bytes_idx, &[_]u8{'\\'}),
        '\"' => try self.byte_slices.extendLast(self.gpa, bytes_idx, &[_]u8{'\"'}),
        'u' => {
            @panic("TODO: implement unicode escape sequences");
        },
        else => {
            // Report the invalid escape sequence
            const start: u32 = @intCast(self.pos - 1); // Include the backslash in the region
            try self.problems.append(Diagnostic{
                .region = Region.from_raw_offsets(start, @intCast(self.pos)),
                .kind = .InvalidEscapeSequence,
            });
            // Write the unescaped string into the buffer.
            try self.byte_slices.extendLast(self.gpa, bytes_idx, &[_]u8{ '\\', escaped });
        },
    }
}

/// We're starting on a single quote that opens a string.
fn tokenizeSingleQuote(self: *Self, start_pos: usize) std.mem.Allocator.Error!Token {
    // Start after the opening single quote
    self.pos += 1;

    if (self.pos >= self.buf.len) {
        return Token{
            .tag = .MalformedSingleQuoteUnclosed,
            .region = Region{
                .start = Region.Position{ .offset = @intCast(start_pos) },
                .end = Region.Position{ .offset = @intCast(self.pos) },
            },
            .payload = .none,
        };
    }

    var char_value: u8 = 0;
    const c = self.buf[self.pos];

    if (c == '\\') {
        // Handle escape sequence
        self.pos += 1;
        if (self.pos >= self.buf.len) {
            return Token{
                .tag = .MalformedSingleQuoteUnclosed,
                .region = Region{
                    .start = Region.Position{ .offset = @intCast(start_pos) },
                    .end = Region.Position{ .offset = @intCast(self.pos) },
                },
                .payload = .none,
            };
        }
        const escaped = self.buf[self.pos];
        switch (escaped) {
            'n' => char_value = '\n',
            'r' => char_value = '\r',
            't' => char_value = '\t',
            '\\' => char_value = '\\',
            '\'' => char_value = '\'',
            else => {
                self.pos += 1;
                // Check for closing quote
                if (self.pos < self.buf.len and self.buf[self.pos] == '\'') {
                    self.pos += 1;
                }
                return Token{
                    .tag = .MalformedSingleQuoteInvalidEscapeSequence,
                    .region = Region{
                        .start = Region.Position{ .offset = @intCast(start_pos) },
                        .end = Region.Position{ .offset = @intCast(self.pos) },
                    },
                    .payload = .none,
                };
            },
        }
        self.pos += 1;
    } else if (c == '\'') {
        // Empty single quote
        self.pos += 1;
        return Token{
            .tag = .MalformedSingleQuoteEmpty,
            .region = Region{
                .start = Region.Position{ .offset = @intCast(start_pos) },
                .end = Region.Position{ .offset = @intCast(self.pos) },
            },
            .payload = .none,
        };
    } else {
        // Regular character
        char_value = c;
        self.pos += 1;
    }

    // Check for closing quote
    if (self.pos >= self.buf.len or self.buf[self.pos] != '\'') {
        return Token{
            .tag = .MalformedSingleQuoteUnclosed,
            .region = Region{
                .start = Region.Position{ .offset = @intCast(start_pos) },
                .end = Region.Position{ .offset = @intCast(self.pos) },
            },
            .payload = .none,
        };
    }
    self.pos += 1;

    // Store the character value in the extra field
    return Token{
        .tag = .SingleQuote,
        .region = Region{
            .start = Region.Position{ .offset = @intCast(start_pos) },
            .end = Region.Position{ .offset = @intCast(self.pos) },
        },
        .payload = .{ .none = char_value },
    };
}

fn tokenizeTripleQuoteString(self: *Self) std.mem.Allocator.Error!Token {
    const start: u32 = @intCast(self.pos);

    while (true) {
        var segment_start = self.pos;

        // Process escapes until we hit a newline or an interpolation start
        while (true) {
            switch (self.src[self.pos]) {
                '\\' => {
                    // We hit an escape, so copy over the segment we've seen so far
                    try str_buf.appendSlice(self.src[segment_start..self.pos]);
                    self.pos += 1; // Advance past the backslash
                    try self.chompEscapedChar(gpa, str_buf, problems);
                    segment_start = self.pos; // The next segment begins right after the escaped char
                },
                '$' => {
                    self.pos += 1; // Advance past the '$'

                    // "${" begins string interpolation.
                    if (self.buf[self.pos] == '{') {
                        self.pos += 1;
                        self.pending_string_interpolation = true;
                        self.unclosed_curlies += 1;
                        self.inside_string = .TripleQuote;
                        return Token.no_payload(.StringPart, start, @intCast(self.pos));
                    }
                },
                '\n' => break,
                else => self.pos += 1, // Advance past whatever ordinary char it was
            }
        }

        const end = self.pos; // If it turns out this was the end of the string, mark it before the newline.

        // Copy the remainder of the current segment, including the newline.
        try str_buf.appendSlice(self.src[segment_start..self.pos]);
        self.pos += 1; // Advance past the newline.

        // Chomp all the spaces and tabs at the beginning of the next line,
        // then see if we have another triple quote to continue the multiline string.
        while (self.buf[self.pos] == ' ' or self.buf[self.pos] == '\t') self.pos += 1;

        // We don't need a bounds check here, because the source always ends in newline followed by three zeros,
        // so even if we hit the "\n" at the last possible point in the source, this will still just read the zeros.
        if (self.buf[self.pos] == '"' and self.buf[self.pos + 1] == '"' and self.buf[self.pos + 2] == '"') {
            // We hit another triple-quoted string, so chomp the three quotes and continue.
            // The next segment will naturally begin right after these triple quotes, after we loop back around.
            self.pos += 3;
        } else {
            // This was the end of the triple-quoted string, so return.
            return Token.no_payload(.MultilineString, start, @intCast(end));
        }
    }
}

fn tokenizeSingleQuoteString(self: *Self) std.mem.Allocator.Error!Token {
    // It's a single-quote string, so proceed accordingly.
    var segment_start = self.pos;
    var tag = undefined;
    const bytes_idx = self.byte_slices.appendEmpty();

    // Process escapes until we hit a closing quote, newline or an interpolation start
    while (true) {
        switch (self.src[self.pos]) {
            '\\' => {
                // We hit an escape, so copy over the segment we've seen so far
                try self.byte_slices.extendLast(self.src[segment_start..self.pos]);
                self.pos += 1; // Advance past the backslash
                try self.chompEscapedChar(self.src[self.pos], bytes_idx);
                segment_start = self.pos; // The next segment begins right after the escaped char
            },
            '$' => {
                self.pos += 1; // Advance past the '$'

                // "${" begins string interpolation.
                if (self.src[self.pos] == '{') {
                    self.pos += 1;
                    self.pending_string_interpolation = true;
                    self.unclosed_curlies += 1;
                    self.inside_string = .SingleQuote;
                    return Token.no_payload(.StringPart, start, @intCast(self.pos));
                }
            },
            '\n' => {
                tag = .MalformedString; // Line ended without this single-line string having been closed
                problems.append(problem);
            },
            '"' => {
                tag = .String;
                break;
            },
            else => self.pos += 1, // Advance past whatever ordinary char it was
        }
    }

    const end = self.pos; // Mark the end of the string as being before the closing delimiter

    // Copy the remainder of the current segment, including the newline.
    try str_buf.appendSlice(self.src[segment_start..self.pos]);
    self.pos += 1; // Advance past the delimiter.

    return Token{
        .tag = tag,
        .region = Region.from_raw_offset(start, @intCast(end)),
        .payload = .{ .bytes_idx = bytes_idx },
    };
}

fn testTokenization(gpa: std.mem.Allocator, input: []const u8, expected: []const Token.Tag) !void {
    var messages: [10]Diagnostic = undefined;

    var env = try CommonEnv.init(gpa, try gpa.dupe(u8, ""));
    defer env.deinit(gpa);

    var byte_slices = collections.ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(gpa);

    var tokenizer = try TokenIterator.init(&env, gpa, input, &messages, &byte_slices);
    defer tokenizer.deinit(gpa);

    // Collect all tokens
    var token_list = std.ArrayList(Token.Tag).init(gpa);
    defer token_list.deinit();

    while (true) {
        const token = try tokenizer.next(gpa);
        switch (token.tag) {
            .EndOfFile => break,
            else => try token_list.append(token.tag),
        }
    }

    const tokens = token_list.items;

    try std.testing.expectEqualSlices(Token.Tag, expected[0..expected.len], tokens);
}

test "tokenizer" {
    const gpa = std.testing.allocator;
    try testTokenization(gpa, "42", &[_]Token.Tag{.Int});
    try testTokenization(gpa, "3.14", &[_]Token.Tag{.Float});
    try testTokenization(gpa, ".", &[_]Token.Tag{.Dot});
    try testTokenization(gpa, "..", &[_]Token.Tag{.DoubleDot});
    try testTokenization(gpa, "1..2", &[_]Token.Tag{ .Int, .DoubleDot, .Int });
    try testTokenization(gpa, "...", &[_]Token.Tag{.TripleDot});
    try testTokenization(gpa, "-", &[_]Token.Tag{.OpUnaryMinus});
    try testTokenization(gpa, "-42", &[_]Token.Tag{ .OpUnaryMinus, .Int });
    try testTokenization(gpa, "1e10", &[_]Token.Tag{.Float});
    try testTokenization(gpa, "_ident", &[_]Token.Tag{.NamedUnderscore});
    try testTokenization(gpa, "1..2", &[_]Token.Tag{ .Int, .DoubleDot, .Int });
    try testTokenization(gpa, "3...4", &[_]Token.Tag{ .Int, .TripleDot, .Int });
    try testTokenization(gpa, "1. .2", &[_]Token.Tag{ .Int, .Dot, .DotInt });
    try testTokenization(gpa, "1.2.3", &[_]Token.Tag{ .Float, .NoSpaceDotInt });
    try testTokenization(gpa, "match", &[_]Token.Tag{.KwMatch});
    try testTokenization(gpa, "var", &[_]Token.Tag{.KwVar});
    try testTokenization(gpa, "{a, b}", &[_]Token.Tag{ .OpenCurly, .LowerIdent, .Comma, .LowerIdent, .CloseCurly });
    // Test hex/binary/octal numbers
    try testTokenization(gpa, "0x42", &[_]Token.Tag{.Int});
    try testTokenization(gpa, "0X42", &[_]Token.Tag{.Int});
    try testTokenization(gpa, "0b101", &[_]Token.Tag{.Int});
    try testTokenization(gpa, "0o77", &[_]Token.Tag{.Int});
    try testTokenization(gpa, "\"abc\"", &[_]Token.Tag{ .StringStart, .StringPart, .StringEnd });
    try testTokenization(gpa, "\"a${b}c\"", &[_]Token.Tag{
        .StringStart,
        .StringPart,
        .OpenStringInterpolation,
        .LowerIdent,
        .CloseStringInterpolation,
        .StringPart,
        .StringEnd,
    });
    try testTokenization(
        gpa,
        \\"""abc
    ,
        &[_]Token.Tag{ .MultilineStringStart, .StringPart },
    );
    try testTokenization(
        gpa,
        \\"""abc
        \\"""def
    ,
        &[_]Token.Tag{ .MultilineStringStart, .StringPart, .MultilineStringStart, .StringPart },
    );
    try testTokenization(
        gpa,
        \\"""abc"""
    ,
        &[_]Token.Tag{ .MultilineStringStart, .StringPart },
    );
    try testTokenization(
        gpa,
        \\"""a${b}c
        \\"""def
    ,
        &[_]Token.Tag{
            .MultilineStringStart,
            .StringPart,
            .OpenStringInterpolation,
            .LowerIdent,
            .CloseStringInterpolation,
            .StringPart,
            .MultilineStringStart,
            .StringPart,
        },
    );
}

test "tokenizer with invalid UTF-8" {
    const gpa = std.testing.allocator;

    // Invalid UTF-8 start byte
    {
        const invalid_utf8 = [_]u8{ '"', 'H', 'e', 'l', 'l', 'o', ' ', 0xFF, ' ', 'w', 'o', 'r', 'l', 'd', '"' };
        var diagnostics: [10]Diagnostic = undefined;

        var env = try CommonEnv.init(gpa, try gpa.dupe(u8, ""));
        defer env.deinit(gpa);

        var byte_slices = collections.ByteSlices{ .entries = .{} };
        defer byte_slices.entries.deinit(gpa);

        var tokenizer = try TokenIterator.init(&env, gpa, &invalid_utf8, &diagnostics, &byte_slices);
        defer tokenizer.deinit(gpa);

        // Tokenize all tokens
        while ((try tokenizer.next(gpa)).tag != .EndOfFile) {}

        // Should have reported InvalidUtf8InSource
        const messages = tokenizer.cursor.messages[0..tokenizer.cursor.message_count];
        try std.testing.expect(messages.len > 0);
        try std.testing.expectEqual(Diagnostic.Tag.InvalidUtf8InSource, messages[0].tag);
    }

    // Incomplete UTF-8 sequence at end
    {
        const incomplete_utf8 = [_]u8{ '"', 'H', 'e', 'l', 'l', 'o', ' ', 0xC3, '"' }; // 0xC3 expects another byte
        var diagnostics: [10]Diagnostic = undefined;

        var env = try CommonEnv.init(gpa, try gpa.dupe(u8, ""));
        defer env.deinit(gpa);

        var byte_slices = collections.ByteSlices{ .entries = .{} };
        defer byte_slices.entries.deinit(gpa);

        var tokenizer = try TokenIterator.init(&env, gpa, &incomplete_utf8, &diagnostics, &byte_slices);
        defer tokenizer.deinit(gpa);

        // Tokenize all tokens
        while ((try tokenizer.next(gpa)).tag != .EndOfFile) {}

        // Should have reported InvalidUtf8InSource
        const messages = tokenizer.cursor.messages[0..tokenizer.cursor.message_count];
        try std.testing.expect(messages.len > 0);
        try std.testing.expectEqual(Diagnostic.Tag.InvalidUtf8InSource, messages[0].tag);
    }
}

test "non-printable characters in string literal" {
    const gpa = std.testing.allocator;

    // Non-printable ASCII
    {
        const non_printable = [_]u8{ '"', 'H', 'e', 'l', 'l', 'o', '\x01', 'w', 'o', 'r', 'l', 'd', '"' }; // 0x01 is SOH (non-printable)
        var diagnostics: [10]Diagnostic = undefined;

        var env = try CommonEnv.init(gpa, try gpa.dupe(u8, ""));
        defer env.deinit(gpa);

        var byte_slices = collections.ByteSlices{ .entries = .{} };
        defer byte_slices.entries.deinit(gpa);

        var tokenizer = try TokenIterator.init(&env, gpa, &non_printable, &diagnostics, &byte_slices);
        defer tokenizer.deinit(gpa);

        // Tokenize all tokens
        while ((try tokenizer.next(gpa)).tag != .EndOfFile) {}

        // Should have reported NonPrintableUnicodeInStrLiteral
        const messages = tokenizer.cursor.messages[0..tokenizer.cursor.message_count];
        try std.testing.expect(messages.len > 0);
        try std.testing.expectEqual(Diagnostic.Tag.NonPrintableUnicodeInStrLiteral, messages[0].tag);
    }

    // Non-printable (but valid) non-ASCII Unicode characters
    {
        const control_char = [_]u8{ '"', 'H', 'e', 'l', 'l', 'o', ' ', 0xC2, 0x80, ' ', 'w', 'o', 'r', 'l', 'd', '"' }; // U+0080 (C1 control)
        var diagnostics: [10]Diagnostic = undefined;

        var env = try CommonEnv.init(gpa, try gpa.dupe(u8, ""));
        defer env.deinit(gpa);

        var byte_slices = collections.ByteSlices{ .entries = .{} };
        defer byte_slices.entries.deinit(gpa);

        var tokenizer = try TokenIterator.init(&env, gpa, &control_char, &diagnostics, &byte_slices);
        defer tokenizer.deinit(gpa);

        // Tokenize all tokens
        while ((try tokenizer.next(gpa)).tag != .EndOfFile) {}

        const messages = tokenizer.cursor.messages[0..tokenizer.cursor.message_count];
        try std.testing.expect(messages.len > 0);
        try std.testing.expectEqual(Diagnostic.Tag.NonPrintableUnicodeInStrLiteral, messages[0].tag);
    }

    // No errors should be reported for these
    {
        const valid_chars = [_]u8{ '"', 'H', 'e', 'l', 'l', 'o', '\t', ' ', 'w', 'o', 'r', 'l', 'd', '"' };
        var diagnostics: [10]Diagnostic = undefined;

        var env = try CommonEnv.init(gpa, try gpa.dupe(u8, ""));
        defer env.deinit(gpa);

        var byte_slices = collections.ByteSlices{ .entries = .{} };
        defer byte_slices.entries.deinit(gpa);

        var tokenizer = try TokenIterator.init(&env, gpa, &valid_chars, &diagnostics, &byte_slices);
        defer tokenizer.deinit(gpa);

        // Tokenize all tokens
        while ((try tokenizer.next(gpa)).tag != .EndOfFile) {}

        const messages = tokenizer.cursor.messages[0..tokenizer.cursor.message_count];
        try std.testing.expect(messages.len == 0);
    }
}

/// Represents a diagnostic message including its position in the source.
pub const Diagnostic = struct {
    tag: Tag,
    region: Region,

    /// Represents the type of diagnostic message.
    pub const Tag = enum {
        MisplacedCarriageReturn,
        AsciiControl,
        LeadingZero,
        UppercaseBase,
        InvalidUnicodeEscapeSequence,
        InvalidEscapeSequence,
        UnclosedString,
        NonPrintableUnicodeInStrLiteral,
        InvalidUtf8InSource,
    };
};

pub const keywords = std.StaticStringMap(Token.Tag).initComptime(.{
    .{ "and", .OpAnd },
    .{ "app", .KwApp },
    .{ "as", .KwAs },
    .{ "crash", .KwCrash },
    .{ "dbg", .KwDbg },
    .{ "else", .KwElse },
    .{ "expect", .KwExpect },
    .{ "exposes", .KwExposes },
    .{ "exposing", .KwExposing },
    .{ "for", .KwFor },
    .{ "generates", .KwGenerates },
    .{ "has", .KwHas },
    .{ "hosted", .KwHosted },
    .{ "if", .KwIf },
    .{ "implements", .KwImplements },
    .{ "import", .KwImport },
    .{ "imports", .KwImports },
    .{ "in", .KwIn },
    .{ "interface", .KwInterface },
    .{ "match", .KwMatch },
    .{ "module", .KwModule },
    .{ "or", .OpOr },
    .{ "package", .KwPackage },
    .{ "packages", .KwPackages },
    .{ "platform", .KwPlatform },
    .{ "provides", .KwProvides },
    .{ "requires", .KwRequires },
    .{ "return", .KwReturn },
    .{ "var", .KwVar },
    .{ "where", .KwWhere },
    .{ "with", .KwWith },
});

inline fn chompOne(pos: *usize, tag: Token.Tag) Token {
    const start: u32 = @intCast(*pos);
    const end: u32 = start + 1;

    pos.? = @intCast(end); // Chomp the byte

    return Token{
        .tag = tag,
        .region = Region.from_raw_offsets(start, end),
        .payload = .none,
    };
}

inline fn chompOneOfTwo(
    pos: *usize,
    src: [:0]const u8,
    snd: u8,
    fst_tag: Token.Tag,
    snd_tag: Token.Tag,
) Token {
    debugVerifySrc(src);

    const start: u32 = @intCast(*pos);
    pos.? = *pos + 1; // Chomp the first byte

    // Branchlessly check for the second tag
    const has_snd = src[*pos] == snd; // Safe bc we early returned if we were on the last byte
    pos.? = *pos + has_snd; // Branchlessly chomp the second char

    return Token.no_payload(if (has_snd) snd_tag else fst_tag, start, @intCast(*pos));
}

inline fn chompOneOfThree(
    pos: *usize,
    src: [:0]const u8,
    snd: u8,
    thd: u8,
    fst_tag: Token.Tag,
    snd_tag: Token.Tag,
    thd_tag: Token.Tag,
) Token {
    debugVerifySrc(src);

    const start: u32 = @intCast(*pos);

    pos.? = *pos + 1; // Chomp the first byte

    // Check for the second and third tags. This won't be out-of-bounds bc src is guaranteed to end in "\n\0"
    const has_snd = src[*pos] == snd;
    const has_thd = src[*pos] == thd;

    pos.? = *pos + (has_snd or has_thd); // Branchlessly chomp the second char

    return Token{
        .tag = if (has_snd) snd_tag else (if (has_thd) thd_tag else fst_tag),
        .region = Region.from_raw_offsets(start, @intCast(*pos)),
        .payload = .none,
    };
}

fn isValidIdentStart(byte: u8) bool {
    return (byte >= 'a' and byte <= 'z') or (byte >= 'A' and byte <= 'Z');
}

// Verify that src ends in a newline followed by 3 zeros. We'll infinite loop (or worse) if it isn't!
// The newline lets us check for end-of-comment and end-of-triple-quote-string without needing to check for EOF too,
// and also means we can always peek ahead 3 characters safely (useful for 3-length tokens like triple quotes).
fn debugVerifySrc(src: [:0]const u8) void {
    std.debug.assert(std.mem.endsWith(u8, src, &[_]u8{ '\n', 0, 0, 0 }));
}
