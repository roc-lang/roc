const std = @import("std");
const roc_src = @import("roc_src");

const num_one_byte_ops = 20;
const lowest_binop_char = '!';

const ascii_byte_type: [127 - 32]Token.StartsWith = .{
    // DO NOT REORDER THESE! They are carefully ordered to line up with UTF-8 bytes

    // We skip the first 32 because they're all either invalid or whitespace.
    .unary, // 33 ! - the ident handling logic chomps postfix `!` so we only care about prefix here
    .double_quote, // 34 "
    .comment, // 35 #
    .underscore_or_dollar, // 36 $
    .infix, // 37 %
    .infix, // 38 &
    .single_quote, // 39 '
    .delimiter, // 40 (
    .delimiter, // 41 )
    .infix, // 42 *
    .infix, // 43 +
    .comma, // 44 ,
    .digit, // 45 - we treat minus as a digit and special-case it not being followed by digits
    .infix, // 46 . (note: we assume infix, and in error cases may reinterpret as prefix, e.g. `x * .5` or `(.foo)`)
    .infix, // 47 /
    .digit, // 48 0
    .digit, // 49 1
    .digit, // 50 2
    .digit, // 51 3
    .digit, // 52 4
    .digit, // 53 5
    .digit, // 54 6
    .digit, // 55 7
    .digit, // 56 8
    .digit, // 57 9
    .infix, // 58 :
    .infix, // 59 ; (note: we assume infix, and in error cases may reinterpret as postfix, e.g. `x = y();`
    .infix, // 60 <
    .infix, // 61 =
    .infix, // 62 >
    .invalid, // 63 ? - question mark is special bc it's either a postfix unary op, or if it's followed by '?', infix binary.
    .invalid, // 64 @
    .uc, // 65 A
    .uc, // 66 B
    .uc, // 67 C
    .uc, // 68 D
    .uc, // 69 E
    .uc, // 70 F
    .uc, // 71 G
    .uc, // 72 H
    .uc, // 73 I
    .uc, // 74 J
    .uc, // 75 K
    .uc, // 76 L
    .uc, // 77 M
    .uc, // 78 N
    .uc, // 79 O
    .uc, // 80 P
    .uc, // 81 Q
    .uc, // 82 R
    .uc, // 83 S
    .uc, // 84 T
    .uc, // 85 U
    .uc, // 86 V
    .uc, // 87 W
    .uc, // 88 X
    .uc, // 89 Y
    .uc, // 90 Z
    .delimiter, // 91 [
    .invalid, // 92 \ - backslash
    .delimiter, // 93 ]
    .invalid, // 94 ^
    .underscore_or_dollar, // 95 _
    .invalid, // 96 `
    .lc, // 97 a
    .lc, // 98 b
    .lc, // 99 c
    .lc, // 100 d
    .lc, // 101 e
    .lc, // 102 f
    .lc, // 103 g
    .lc, // 104 h
    .lc, // 105 i
    .lc, // 106 j
    .lc, // 107 k
    .lc, // 108 l
    .lc, // 109 m
    .lc, // 110 n
    .lc, // 111 o
    .lc, // 112 p
    .lc, // 113 q
    .lc, // 114 r
    .lc, // 115 s
    .lc, // 116 t
    .lc, // 117 u
    .lc, // 118 v
    .lc, // 119 w
    .lc, // 120 x
    .lc, // 121 y
    .lc, // 122 z
    .delimiter, // 123 {
    .delimiter, // 124 |
    .delimiter, // 125 }
    .invalid, // 126 ~
    .invalid, // 127 Delete
};

const unsupported = 0xC0; // This is an invalid UTF-8 byte, so a validted UTF-8 byte will never be equal to it.

/// Once we have the first byte of a multi-byte binop, we check this table to see if
/// its second byte is supported. We also check supportsEqualsSuffix, because there
/// happens to be a contiguous range of these where they support equals as a suffix,
/// and in some cases also support a different suffix from this table.
/// (Example: '=' can optionally be followed by either '>' or by another '='.)
const op_second_byte: [num_one_byte_ops]u8 = .{
    // DO NOT REORDER THESE! They are carefully ordered to match the corresponding Operator entries.
    '=', // '!' becomes "!="
    '"', // '"' becomes `""` (empty string or start of multiline string)
    '#', // '#' becomes "##" (doc comment)
    unsupported, // '$'
    unsupported, // '%'
    unsupported, // '&'
    '\\', // `\` becomes `\\` (multiline string)
    unsupported, // '('
    unsupported, // ')'
    unsupported, // '*'
    unsupported, // '+'
    unsupported, // ','
    '>', // '-' becomes "->"
    '.', // '.' becomes ".."
    '/', // '/' becomes "//"
    unsupported, // ':'
    unsupported, // ';'
    '-', // '<' becomes "<-"
    '>', // '=' becomes "=>"
    unsupported, // '>'
    ':', // '?' becomes "?:"
};

const mask_type = .{
    .non_, // .unary, // unary op, e.g. `!` or `-` or `$` (parser converts prefix $ and postfix ! into modifiers on adjacent idents.)
    .ident_or_ws, // .infix, // infix binary operator, e.g. `+`, `or`, `and`, etc.
    // .single_quote, // Includes both `'` and `''`
    // .double_quote, // Includes both `"` and `""`
    // .delimiter, // Delimiters have a payload of either +1 (open) or -1 (close)
    // .comment, // Includes both doc comments and regular comments
    // .comma, // Includes both single commas and double commas (which are just warnings)
    // .minus, // Special becasue it can be part of a number, or unary prefix op, or infix binary op, depending on surroundings
    // .digit, // 0-9
    // .uc, // A-Z
    // .lc, // a-z
    // .underscore_or_dollar, // _ (this will almost always be followed by either lc or a non-ident char)
    // .multibyte_utf8, // Non-ASCII
    // .invalid, // an invalid byte, e.g. ASCII 127 ("Delete") or just an operator that Roc doesn't support.
};

const masks_by_starts_with = .{
    .non_ws, // .comma
    .non_ws, // .operator
    .single_quote_end, // .single_quote
    .double_quote_seg_end, // .double_quote
    .non_ws, // .delimiter
    .newline, // .comment
    .non_ident, // .digit
    .non_ident, // .uc
    .non_ident, // .lc
    .non_ident, // .underscore_or_dollar
    .non_ident, // .multibyte_utf8
    .non_ws, // .invalid
};

pub const Token = enum {
    dollar, // '$' outside a string literal - parser will apply this to an ident token.
    interpolation, // "${" inside a string literal
    minus,

    /// So we know this byte isn't whitespace. But what is it?
    /// payload should be a pointer to 0
    pub fn fromNonWhitespaceByte(src: roc_src.Bytes, src_idx: usize, in_str_literal: bool, payload: *u32) struct {
        token: Token,
        len: u32,
    } {
        const current = src[src_idx];
        starts_with: switch (StartsWith.fromUtf8Byte(current)) {
            .dollar => {
                std.debug.assert(current == '$'); // The following calculation assumes this.

                // Begin interpolation iff we're in a string literal and the next byte is '{'
                const interpolate = '{' == src[src_idx + @intFromBool(in_str_literal)];
                const len = 1 + @intFromBool(interpolate); // 2 bytes iff we chomped "${"
                const tok = if (interpolate) .interpolation else .dollar;

                return .{ .len = len, .token = tok };
            },
            .digit => {
                // Chomp leading minus, if it's there.
                var is_negated = current == '-';
                var digit_idx = src_idx + @intFromBool(is_negated);
                var digit = src[digit_idx];
                const len = @panic("TODO get length from ident mask");

                // If we're negated and have an ident-or-num length of 0,
                // then this is a standalone minus - not part of a literal!
                if (len < @intFromBool(is_negated)) {
                    return .{ .len = 1, .token = .minus };
                }

                const has_decimal_pt = src[len] == '.';
                const len_after_decimal = @panic("TODO get length from ident mask");
                var underscores_before_decimal = 0;

                // Custom bases are rare, so use a branch on purpose here
                // even though this could be branchless. The bet is that they
                // come up rarely enough that it's better to occasionally
                // mispredict than to do all this every time.
                var base = if (digit == 0) blk: {
                    // Once we're in here, we can certainly
                    // branchlessly determine the base.
                    var has_custom_base = digit == '0';
                    const next = src[digit_idx + 1];
                    const hex = 4 << @intFromBool(next == 'x');
                    const oct = 3 << @intFromBool(next == 'o');
                    const bin = @intFromBool(next == 'b');
                    const custom_base = hex | oct | bin;

                    if (custom_base == 0) {
                        // This wasn't one of the custom bases we support, so report a problem
                        // and gracefully recover.
                        if (next >= '0' and next <= '9') {
                            // TODO emit a WARNING to delete leading zeros bc they do nothing.
                            digit_idx += 1;
                        } else {
                            // TODO push an ERROR - this isn't one of the custom bases we support.
                            // TODO special-case capitalized versions, where we make it a warning.
                            digit_idx += 2;

                            // TODO loop through and chomp remaining digits, then return malformed.
                            return null;
                        }
                    } else {
                        digit_idx += 2;
                    }

                    // Advance past e.g. "0x"
                    digit_idx += 2;

                    break :blk custom_base;
                } else 10;

                if (digit == '_') {
                    // This should only happen if you start your number with "-_"
                    std.debug.assert(is_negated);
                    underscores_before_decimal += 1;

                    // TODO warn that starting numbers with underscores is disallowed
                    // TODO chomp any remaining underscores
                    // TODO if it turned out to be all underscores, instead err "you can't negate underscore"
                }

                // Ensure capacity sufficient for the length plus enough
                // for the highest amount of storage bytes we could need.
                // (This is probably more than we need, but it's not worth
                // paying for the calculation when we could just reserve a
                // couple of extra bytes here and there.)
                nodes.ensureCapacity(len + len_after_decimal + (2 * @sizeOf(u32)));

                // Now that we've ensured we have enough capacity, we can just
                // write directly there as needed.
                const original_literals_len = nodes.len;
                var dest = nodes.ptr + original_literals_len;
                const end = digit_idx + len + @intFromBool(has_decimal_pt) + len_after_decimal;

                for (digit_idx..end) |idx| {
                    const ch = src[idx];
                    const after_decimal_pt = idx > len;

                    if (after_decimal_pt and (ch == 'e' or ch == 'E')) {
                        // TODO chomp the parts after `e` and then go back and retroactively
                        // modify all the digits we've seen so far.
                        // Basically we want to precompute this so there's no chance of overflow
                        // or precision loss that could result from doing it after the fact.
                        // Once we're done, we should break out of the loop early.
                        @panic("TODO Not yet supported: scientific notation in frac literals!");
                    }

                    const is_underscore = ch == '_';
                    const is_not_underscore = !is_underscore;

                    base = if (after_decimal_pt) 10 else base;

                    // Our previous bitmasking should have ensured that we're only
                    // iterating through valid identifier chars here, so ASCII alphanumeric
                    // or UTF-8 multibyte (over 127).
                    std.debug.assert((ch == '_' or ch == '.' or ch >= '0' or ch > 127) and
                        ((ch <= '9') or (ch >= 'A' and ch <= 'Z') or (ch >= 'a' and ch <= 'z')));

                    // Normalize ASCII '0' as 0 so all the invalid numbers are above 'z'.
                    // This way, all we have to do to check if it was invalid is to check
                    // (ch > base) and that will catch not only invalid letters (e.g.
                    // 'a' if we're base-10 or 'z' for all supported bases) but also things
                    // like UTF-8 multibyte sequences, which our identifier bitmask accepts
                    // but which aren't valid in number literals.
                    var num = ch -% '0';
                    num -= if (num >= 'A' - '0') 'A' - '0' else 0;
                    num -= if (num >= 'a' - 'A' - '0') 'a' - 'A' - '0' else 0;

                    // If this is outside our range (e.g. 'a' when we're base-10), error.
                    if (num > base and is_not_underscore) {
                        // TODO report an error and early return.
                    }

                    const is_decimal_pt_next = idx == len - 1;
                    const advance_amount =
                        // Don't advance the destination or length if this was an underscore,
                        // because we want to overwrite that garbage value with the next digit.
                        @intFromBool(is_not_underscore)
                        // Also, skip over the decimal point.
                        + @intFromBool(is_decimal_pt_next);

                    underscores_before_decimal += @intFromBool(is_underscore) & @intFromBool(after_decimal_pt);
                    dest += advance_amount;
                }

                const len_before_decimal_pt =
                    len - underscores_before_decimal - @intFromBool(is_negated);
                const len_after_decimal_pt =
                    // dest tells us how much we wrote in total; from that, we can infer
                    // how many actual bytes we wrote after the decimal point.
                    @intFromPtr(dest) - nodes.ptr - original_literals_len - len_before_decimal_pt - @intFromBool(has_decimal_pt);

                if (len_before_decimal_pt + len_after_decimal_pt <= 4) {
                    std.debug.assert(*payload == 0); // Payload should have been initialized to 0

                    // TODO total len ended up being 4 or less, so copy the bytes
                    // from the end of the literals into the payload (0-padded).
                } else {
                    // TODO make sure appendLenAssumeCapacity is branchless and can handle
                    // being passed a len of 0!
                    const idx = nodes.appendLenAssumeCapacity(len_before_decimal_pt);
                    nodes.appendLenAssumeCapacity(len_after_decimal_pt);
                }

                return .{ .len = digit_idx - src_idx, .token = tok };
            },
            .bang => {
                return .{ .len = 1, .token = .bang };
            },
            .infix => {
                return chompSymbol(); // TODO chomp 1 or 2 using lookup tables.
            },
            .single_quote => {
                const next_idx = src_idx + 1;
                const first_ch = src[next_idx];
                const is_escaped = first_ch == '\\';
                const last_idx = next_idx + @intFromBool(is_escaped);
                const last_ch = src[last_idx];

                if (last == '\'') {
                    // TODO error - early return too short, either '' or '\'
                }

                if (src[last_idx + 1] != '\'') {
                    // TODO report unclosed single-quote literal
                }

                const is_n = @intFromBool(last_ch == 'n');
                const is_t = @intFromBool(last_ch == 't');
                const is_r = @intFromBool(last_ch == 'r');

                if (is_escaped and !(is_n | is_t | is_r)) {
                    // TODO error unsupported escape
                }

                const escaped_payload =
                    // Get the last 3 bits correct
                    (is_n << 1) // 010 if 'n'
                    | (is_r << 2) // 100 if 'r'
                    | @intFromBool(last_ch <= 't') // 101 if 'r', 001 if 't'
                    | 0b0000_1000; // They all have a 1-bit here.

                payload.* = @intCast(if (is_escaped) escaped_payload else first_ch);
            },
            .double_quote => {
                // TODO find next segment end
                // TODO ...
            },
            .comment => {
                const next_idx = src_idx + 1;
                var is_doc_comment = src[next_id] == '#';

                // Doc comments have to be "##" followed by a space.
                is_doc_comment = src[next_idx + @intFromBool(is_doc_comment)] == ' ';

                // TODO find next newline to end the comment
                // TODO only write down region, not the contents of the comment
                // TODO make token be .comment vs .doc_comment based on is_doc_comment
            },
            .comma => {
                return .{ .len = 1, .token = .comma };
            },
            .ident => {
                // TODO determine uc vs lc after dealing with leading underscore.
                // Note: parser deals with leading '$' (modifies adjacent ident to get $ modifier)
                // TODO handle leading UTF-8 multibyte
            },
            .delimiter => {
                // TODO handle [, {, (
            },
            .invalid => {
                // TODO handle an invalid byte, e.g. ASCII 127 ("Delete")
                // or just an operator that Roc doesn't support.
            },
        }
    }

    pub const StartsWith = enum {
        unary, // unary op, e.g. `!` or `-` or `$` (parser converts prefix $ and postfix ! into modifiers on adjacent idents.)
        infix, // infix binary operator, e.g. `+`, `or`, `and`, etc.
        single_quote, // Includes both `'` and `''`
        double_quote, // Includes both `"` and `""`
        delimiter, // Delimiters have a payload of either +1 (open) or -1 (close)
        comment, // Includes both doc comments and regular comments
        comma, // Includes both single commas and double commas (which are just warnings)
        minus, // Special becasue it can be part of a number, or unary prefix op, or infix binary op, depending on surroundings
        digit, // 0-9
        uc, // A-Z
        lc, // a-z
        underscore_or_dollar, // _ or $ (this will almost always be followed by either lc or a non-ident char)
        multibyte_utf8, // Non-ASCII
        invalid, // an invalid byte, e.g. ASCII 127 ("Delete") or just an operator that Roc doesn't support.

        pub fn fromUtf8Byte(utf8_byte: u8) StartsWith {
            if (utf8_byte > 127) {
                return .multibyte_utf8;
            }

            // Only ASCII values greater than 32 are valid here.
            const result = @subWithOverflow(utf8_byte, 32);
            if (result[1] != 0) {
                return .invalid;
            } else {
                return ascii_byte_type[result[0]];
            }
        }
    };
};

// Precedence table for operators (indexed by operator enum value)
// TODO: Set proper precedence values based on language rules
const precedence_table = [_]u8{0} ** 52; // 52 operators from kw_as to kw_where

const Operator = enum(u8) {
    // DO NOT CHANGE THESE NUMBERS UNLESS YOU ARE CONFIDENT YOU KNOW WHAT YOU ARE DOING!
    // Conversion functions do arithmetic to convert bytes to these numbers, so
    // changing them can very easily break things.

    // 1-byte operators
    bang = 0, // '!'
    double_qt = 1, // `"`
    pound = 2, // '#'
    dollar = 3, // '$'
    percent = 4, // '%' - unsupported, but we parse it anyway to keep this table contiguous (and report an error)
    ampersand = 5, // `&` - unsupported, but we parse it anyway to keep this table contiguous (and report an error)
    single_qt = 6, // `'`
    open_round = 7, // '('
    close_round = 8, // ')'
    star = 9, // '*'
    plus = 10, // '+'
    comma = 11, // ','
    minus = 12, // '-'
    dot = 13, // '.'
    slash = 14, // '/'
    colon = 15, // ':'
    semicolon = 16, // ';' - unsupported, but we parse it anyway to keep this table contiguous (and report a warning)
    lt = 17, // '<'
    eq = 18, // '='
    gt = 19, // '>'
    question = 20, // '?'
    // 2-byte operators
    bang_equals = 21, // "!="
    double_quote_x2 = 22, // `""`
    pound_x2 = 23, // "##" (doc comment)
    // (gap in numbers for some unsupported ones)
    backslash_quote = 27, // "\#" (multiline string)
    // (gap in numbers for some unsupported ones)
    thin_arrow = 33, // "->"
    dot_x2 = 34, // ".."
    slash_x2 = 35, // "//"
    // (gap in numbers for some unsupported ones)
    back_arrow = 38, // "<-"
    thick_arrow = 39, // "=>"
    // (gap in numbers for an unsupported one)
    question_x2 = 41, // "??"
    // 2-byte operators ending in '=' (aside from "!=" which isn't contiguous with these, and which was handled earlier)
    colon_equals = 42, // ":="
    semicolon_equals = 43, // ";=" - unsupported, but we parse it anyway to save an instruction (and report an error)
    lte = 44, // "<="
    eq_x2 = 45, // "=="
    gte = 46, // ">="
    question_equals = 47, // "?=" - unsupported, but we parse it anyway to save an instruction (and report an error)
    // keyword operators
    kw_as = 48,
    kw_or = 49,
    kw_and = 50,
    kw_where = 51,

    //////////////////////////////////////
    // TODO: Parser should be able to use a compact jump table here to figure out what to do!
    //////////////////////////////////////

    pub const Type = enum {
        unary, // unary op, e.g. `!` or `-` or `$` (parser converts $ into an ident modifier and adjusts region.)
        infix, // infix operator, e.g. `+`, `or`, `and`, etc.
        single_quote, // Includes both `"` and `""`
        double_quote, // Includes both `"` and `""`
        delimiter, // Delimiters have a payload of either +1 (open) or -1 (close)
        comment, // Includes both doc comments and regular comments
        comma, // Includes both single commas and double commas (which are just warnings)
    };

    pub fn precedence(self: Operator) u8 {
        return precedence_table[@intFromEnum(self)];
    }

    /// Must always be given a slice of length exactly 1 or exactly 2.
    /// Additionally, the first byte must be in the ASCII range, and must not be
    /// '\n', '\r', '\t', or ' '. It also must not be a valid identifier char.
    /// Some other parsing branch should have already handled those scenarios!
    /// This debug-asserts that the above holds.
    pub fn fromBytes(bytes: []u8) error{InvalidOp}!Operator {
        std.debug.assert(bytes.len == 1 or bytes.len == 2);

        const has_second_byte = bytes.len > 1;
        const first = try Operator.singleByte(bytes[0]);
        const op = first.trySecondByte(bytes[@intFromBool(has_second_byte)]);

        if (op.isOneByte() and has_second_byte) {
            return .InvalidOp;
        } else {
            return op;
        }
    }

    /// NOTE: the byte must be in the ASCII range, and must not be
    /// '\n', '\r', '\t', or ' '. It also must not be a valid identifier char.
    /// Some other parsing branch should have already handled those scenarios!
    /// This debug-asserts that the above holds.
    ///
    /// This naturally implies that keyword operators (`as`, `or`, `and`, `where`)
    /// can't possibly be handled by this function, and must be handled elsewhere!
    inline fn singleByte(byte: u8) error{InvalidOp}!Operator {
        // We should always be given an ASCII character which is
        // not '\n', '\r', '\t', or ' ', and which is also not a valid ident char.
        std.debug.assert(byte <= 127);
        std.debug.assert(byte != '_');
        std.debug.assert(byte != '\n');
        std.debug.assert(byte != '\r');
        std.debug.assert(byte != '\t');
        std.debug.assert(byte < '0' or byte > '9');
        std.debug.assert(byte < 'a' or byte > 'z');
        std.debug.assert(byte < 'A' or byte > 'Z');

        // Branchlessly convert the byte to an Operator.
        const num_offset = if (byte > '9') ('9' - '0') else 0;
        const uc_offset = if (byte > 'Z') ('Z' - 'A') else 0;
        const lc_offset = if (byte > 'z') ('z' - 'a') else 0;
        const converted_byte = (byte -% 32) // ASCII 0-32 contains no ident or operator chars; wrap them away!
            - num_offset // skip ASCII digits
            - uc_offset // skip ASCII uppercase letters
            - lc_offset; // skip ASCII lowercase letters

        // If it's over this amount (after the wrapping subtraction), this couldn't be
        // a valid Roc operator. (This should basically only mispredict on error cases.)
        if (converted_byte >= num_one_byte_ops) {
            return .InvalidOp;
        }

        return @enumFromInt(converted_byte);
    }

    /// If the given byte works as the second byte of a 2-byte operator
    /// when appearing after the given 1-byte Operator, returns that 2-byte Operator.
    /// Otherwise, returns the given operator unmodified.
    ///
    /// NOTE: the given Operator must be one of the single-byte ones.
    /// This debug asserts that the above holds.
    inline fn trySecondByte(fst: Operator, snd: u8) Operator {
        std.debug.assert(@intFromEnum(fst) <= num_one_byte_ops);

        // Branchlessly resolve 2-byte operators.
        const has_valid_snd_byte = snd == op_second_byte[@intFromEnum(fst)];
        const snd_is_valid_equals = (snd == '=') and fst.supportsEqualsSuffix();
        const offset_non_equals = if (has_valid_snd_byte) num_one_byte_ops else 0;
        const offset_from_equals = if (snd_is_valid_equals) num_one_byte_ops * 2 else 0;

        // These should never be true at the same time. If they are, we have a bug!
        std.debug.assert(!has_valid_snd_byte or !snd_is_valid_equals);

        return @enumFromInt(@intFromEnum(fst) + offset_non_equals + offset_from_equals);
    }

    fn isOneByte(self: Operator) bool {
        @intFromEnum(self) < num_one_byte_ops;
    }

    /// Many operators support having an '=' after them.
    /// These are essentially all of them, except that we're also supporting ";=" and "?="
    /// just so we don't need extra instructions to rule them out. We'll report errors for those later.
    fn supportsEqualsSuffix(binop: Operator) bool {
        std.debug.assert(@intFromEnum(binop) < num_one_byte_ops);
        return @intFromEnum(binop) >= (':' - lowest_binop_char);
    }
};

// TODO this preserves comments, but how do we detect blank lines here?
// I think the answer is: when we just finished formatting a stmt, scan for ws and preserve blank lines, the end.

// // So then in the parser we have...
// switch (starts_with) {
//     .comma => {
//         // TODO handle commas (can these just be binops that are allowed to be trailing? idk kinda cool to support like (+ 1) and (- 1))
//         // Includes both single commas and double commas (which are just warnings)
//     },
//     .operator => {
//         // infix binary operator, e.g. `+`, `or`, `and`, etc., OR prefix unary operator `!`, OR postfix unary operator `?`
//         // TODO get the second byte, then proceed doing binop Pratt parsing etc
//     },
//     .single_quote => {
//         // TODO ask the scanner for the next end of single quote segment (single quote, backslash, or newline), proceed accordingly
//     },
//     .double_quote => {
//         // TODO figure out if this is a triple-quoted string; if so, ask scanner for end of triple quote segment
//         // TODO otherwise ask the scanner for the next end of double quote segment (double quote, backslash, newline, interpolation), proceed accordingly
//     },
//     .delimiter => {
//         // TODO handle delimiter I guess? Maybe there's a bit pattern thing we can use to figure out if it's open or closed.
//         // Maybe pipes need special handling bc open and closed are the same? I dunno.
//         // Delimiters have a payload of either +1 (open) or -1 (close)
//     },
//     .comment => {
//         // TODO detect if doc comment
//         // Includes both doc comments and regular comments
//     },
//     .digit => {
//         // TODO handle minus, which is considered a digit by default - including multiple prefix minuses (e.g. `--x`) - special error etc.
//         // TODO if we end up with a "number" that's just minus, that means we need to reclassify as unary negation.
//         // TODO parse number
//         // 0-9
//     },
//     .uc => {
//         // TODO parse uc ident
//         // A-Z
//     },
//     .lc => {
//         // TODO parse lc ident, ALSO TODO: at the very end, see if we have 1+ postfix `!`s - if so, chomp them!
//         // TODO see if it's exactly "as", "or", "and", "where", and if so, reclassify.
//         // also warn if we chomped more than one.
//     },
//     .underscore_or_dollar => {
//         // TODO special handling bc it's an ident but we don't yet know if it's a uc or lc ident.
//         // _ (this will almost always be followed by either lc or a non-ident char)
//         // warn for multiple prefix underscores - there's no point! - and multiple dollar signs, which aren't allowed.
//     },
//     .multibyte_utf8 => {
//         // TODO parse multibyte utf8 identifier
//     },
//     .invalid => {
//         // TODO handle invalid
//         // an invalid byte, e.g. ASCII 127 ("Delete") or just an operator that Roc doesn't support.
//     },
// }

// /// Sometimes we detect a multi-byte sequence at the very end of this page,
// /// and need to start the next page offset a bit. For example, the last byte
// /// might be "$" and then we have to lookahead to the next page and see that
// /// it starts as "{" and if so, we treat that as a "${" token on this page,
// /// and set a next_page_offset of 1 so we know to skip the "{" on the next page.
// ///
// /// In the most extreme case, we can end up with a ton of consecutive backslashes
// /// and have to offset all the way across one 64B page and into another!
// next_page_offset: usize,
