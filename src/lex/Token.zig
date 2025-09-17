const std = @import("std");
const roc_src = @import("roc_src");

pub const Self = @This();

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
                const len_before_decimal = bitmasks.lenFrom(digit_idx, .num_or_ident);

                // If we're negated and have an ident-or-num length of 0,
                // then this is a standalone minus - not part of a literal!
                if (len_before_decimal < @intFromBool(is_negated)) {
                    return .{ .len = 1, .token = .minus };
                }

                const decimal_pt_idx = digit_idx + len_before_decimal;
                const has_decimal_pt = src[decimal_pt_idx] == '.';
                const len_after_decimal = bitmasks.lenFrom(decimal_pt_idx, .num_or_ident);
                var underscores_before_decimal = 0;

                // Custom bases are rare, so use a branch on purpose here
                // even though this could be done branchlessly. The bet is
                // that they come up rarely enough that it's better to
                // occasionally mispredict than to do all this every time.
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
                nodes.ensureCapacity(len_before_decimal + len_after_decimal + (2 * @sizeOf(u32)));

                // Now that we've ensured we have enough capacity, we can just
                // write directly there as needed.
                const original_literals_len = nodes.len;
                var dest = nodes.ptr + original_literals_len;
                const end = digit_idx + len_before_decimal + @intFromBool(has_decimal_pt) + len_after_decimal;

                for (digit_idx..end) |idx| {
                    const ch = src[idx];
                    const after_decimal_pt = idx > len_before_decimal;

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

                    const is_decimal_pt_next = idx == len_before_decimal - 1;
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
                    len_before_decimal - underscores_before_decimal - @intFromBool(is_negated);
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
                return Keyword.chomp(); // TODO chomp 1 or 2 using lookup tables.
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
