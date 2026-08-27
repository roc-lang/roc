//! Tests for byte offset and UTF-16 column conversion.

const std = @import("std");
const position = @import("lsp").position;

test "utf16 columns count code units, not bytes" {
    // "aé😀": a is one byte, é two bytes and one unit, the emoji four bytes
    // and two units.
    const line = "aé😀";
    try std.testing.expectEqual(@as(u32, 0), position.byteOffsetToUtf16Column(line, 0));
    try std.testing.expectEqual(@as(u32, 1), position.byteOffsetToUtf16Column(line, 1));
    try std.testing.expectEqual(@as(u32, 2), position.byteOffsetToUtf16Column(line, 3));
    try std.testing.expectEqual(@as(u32, 4), position.byteOffsetToUtf16Column(line, 7));
}

test "an offset inside a character does not crash the conversion" {
    // Byte 3 sits between the two bytes of é. Pointing Utf8Iterator at the
    // truncated slice used to slice past its end and panic.
    const line = "aeé";
    try std.testing.expectEqual(@as(u32, 2), position.byteOffsetToUtf16Column(line, 2));

    // The half-read é counts as the one byte that is there, so the column
    // still only ever moves forward.
    try std.testing.expectEqual(@as(u32, 3), position.byteOffsetToUtf16Column(line, 3));
    try std.testing.expectEqual(@as(u32, 3), position.byteOffsetToUtf16Column(line, 4));
}

test "a byte that starts no sequence does not crash the conversion" {
    // 0xFF starts no UTF-8 sequence. The tokenizer reports InvalidUtf8InSource
    // and keeps going, so such a line reaches this conversion.
    const line = "a\xffb";
    try std.testing.expectEqual(@as(u32, 1), position.byteOffsetToUtf16Column(line, 1));
    try std.testing.expectEqual(@as(u32, 2), position.byteOffsetToUtf16Column(line, 2));
    try std.testing.expectEqual(@as(u32, 3), position.byteOffsetToUtf16Column(line, 3));
}

test "a sequence cut short by the end of the line does not crash the conversion" {
    // 0xC3 announces two bytes but the line ends after one.
    const line = "ab\xc3";
    try std.testing.expectEqual(@as(u32, 3), position.byteOffsetToUtf16Column(line, 3));
}

test "columns resolve to byte offsets across invalid bytes" {
    const line = "a\xffb";

    // Strict landing refuses to place an edit against bytes it cannot read.
    try std.testing.expectEqual(@as(?usize, null), position.utf16ColumnToByteOffset(line, 2, .exact));

    // A query keeps its footing: the invalid byte counts as one unit, so
    // column 2 is the `b` after it.
    try std.testing.expectEqual(@as(?usize, 2), position.utf16ColumnToByteOffset(line, 2, .nearest));
    try std.testing.expectEqual(@as(?usize, null), position.utf16ColumnToByteOffset(line, 9, .nearest));
}

test "columns resolve to byte offsets in valid text" {
    const line = "aé😀";
    try std.testing.expectEqual(@as(?usize, 1), position.utf16ColumnToByteOffset(line, 1, .exact));
    try std.testing.expectEqual(@as(?usize, 3), position.utf16ColumnToByteOffset(line, 2, .exact));
    try std.testing.expectEqual(@as(?usize, 7), position.utf16ColumnToByteOffset(line, 4, .exact));

    // Column 3 lands between the emoji's two surrogates.
    try std.testing.expectEqual(@as(?usize, null), position.utf16ColumnToByteOffset(line, 3, .exact));
    try std.testing.expectEqual(@as(?usize, 7), position.utf16ColumnToByteOffset(line, 3, .nearest));
}
