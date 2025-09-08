const std = @import("std");

/// Small decimal representation - TODO move this into its own small_dec.zig module
numerator: i16,
denominator_power_of_ten: u8,

pub const Self = @This();

/// Try to parse a fractional literal as a small dec (numerator/10^power)
pub fn parse(token_text: []const u8) Self {
    // Return null if input is too long to fit in our 32-byte buffer
    if (token_text.len > 32) return null;

    // For negative zero, we'll return null to force f64 path
    if (token_text.len > 0 and token_text[0] == '-') {
        const rest = token_text[1..];
        // Check if it's -0, -0.0, -0.00, etc.
        var all_zeros = true;
        for (rest) |c| {
            if (c != '0' and c != '.') {
                all_zeros = false;
                break;
            }
        }
        if (all_zeros) return null;
    }

    // Parse as a whole number by removing the decimal point
    const dot_pos = std.mem.indexOf(u8, token_text, ".") orelse {
        // No decimal point, parse as integer
        const val = std.fmt.parseInt(i32, token_text, 10) catch return null;
        if (val < -32768 or val > 32767) return null;
        return Self{ .numerator = @as(i16, @intCast(val)), .denominator_power_of_ten = 0 };
    };

    // Count digits after decimal point
    const after_decimal_len = token_text.len - dot_pos - 1;
    if (after_decimal_len > 255) return null; // Too many decimal places

    // Build the string without the decimal point
    var buf: [32]u8 = undefined;
    var len: usize = 0;

    // Copy part before decimal
    @memcpy(buf[0..dot_pos], token_text[0..dot_pos]);
    len = dot_pos;

    // Copy part after decimal
    if (after_decimal_len > 0) {
        @memcpy(buf[len..][0..after_decimal_len], token_text[dot_pos + 1 ..]);
        len += after_decimal_len;
    }

    // Parse the combined number
    const val = std.fmt.parseInt(i32, buf[0..len], 10) catch return null;
    if (val < -32768 or val > 32767) return null;

    return Self{ .numerator = @as(i16, @intCast(val)), .denominator_power_of_ten = @as(u8, @intCast(after_decimal_len)) };
}
