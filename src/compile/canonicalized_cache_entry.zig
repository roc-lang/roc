//! The parse-stage half of a canonicalized-module cache entry.
//!
//! A canonicalized-module cache entry holds two things: the serialized
//! `ModuleEnv` canonicalization produced, and the parse-stage facts the parse
//! task produces and the coordinator consumes before canonicalization is ever
//! reached. Those facts are the module's tokenizer and parser diagnostics and
//! the import inventory the parser recorded. They are a pure function of the
//! module's source, exactly like the env, but they live in the AST rather than
//! in the env, and a cache hit has no AST.
//!
//! So they are encoded explicitly here, field by field in little-endian order,
//! and decoded the same way. Nothing is recovered, re-derived, or re-parsed on
//! a hit: what the parse task hands the coordinator on a hit is read straight
//! out of these bytes.
//!
//! Location-dependent import normalization is deliberately *not* stored. An
//! import's package-root-relative module name depends on the importing module's
//! logical path, and its filesystem path depends on the package root; neither
//! is a canonicalization input, so neither may key or live in an entry. The
//! record holds the parser's own spelling (`import_name`, `base`,
//! `parent_count`), and the parse task normalizes it against the task's own
//! package root on a hit exactly as it does on a miss.

const std = @import("std");
const parse = @import("parse");

const AST = parse.AST;
const DeclIndex = AST.DeclIndex;

/// Version of the encoding below. Folded into the cache entry's version hash,
/// so a format change invalidates every stored entry.
pub const format_version: u32 = 1;

/// A local import exactly as the parser recorded it, before any
/// importer-relative or package-root normalization.
pub const LocalImport = struct {
    import_name: []const u8,
    base: DeclIndex.Import.LocalBase,
    parent_count: u16,
};

/// The parse-stage facts one cache entry carries.
pub const ParseStageRecord = struct {
    diagnostics: []const AST.ResolvedDiagnostic,
    local_imports: []const LocalImport,
    external_imports: []const []const u8,
};

/// A decoded entry is only as trustworthy as its bytes; a corrupt body is a
/// cache miss, never a compilation failure.
pub const DecodeError = error{CorruptParseStageRecord};

/// Number of bytes `encode` writes for `record`.
pub fn encodedLen(record: ParseStageRecord) usize {
    var len: usize = 4 + record.diagnostics.len * diagnostic_encoded_len;
    len += 4;
    for (record.local_imports) |local_import| {
        len += local_import_header_len + local_import.import_name.len;
    }
    len += 4;
    for (record.external_imports) |name| {
        len += 4 + name.len;
    }
    return len;
}

/// Encode `record` into `dest`, which must be exactly `encodedLen` bytes.
pub fn encode(record: ParseStageRecord, dest: []u8) void {
    std.debug.assert(dest.len == encodedLen(record));

    var offset: usize = 0;
    offset = writeU32(dest, offset, @intCast(record.diagnostics.len));
    for (record.diagnostics) |diagnostic| {
        offset = writeU16(dest, offset, @intFromEnum(diagnostic.stage));
        offset = writeU16(dest, offset, diagnostic.tag);
        offset = writeU16(dest, offset, diagnostic.token_tag);
        offset = writeU16(dest, offset, 0);
        offset = writeU32(dest, offset, diagnostic.region.start.offset);
        offset = writeU32(dest, offset, diagnostic.region.end.offset);
    }

    offset = writeU32(dest, offset, @intCast(record.local_imports.len));
    for (record.local_imports) |local_import| {
        offset = writeU16(dest, offset, @intFromEnum(local_import.base));
        offset = writeU16(dest, offset, local_import.parent_count);
        offset = writeU32(dest, offset, @intCast(local_import.import_name.len));
        @memcpy(dest[offset..][0..local_import.import_name.len], local_import.import_name);
        offset += local_import.import_name.len;
    }

    offset = writeU32(dest, offset, @intCast(record.external_imports.len));
    for (record.external_imports) |name| {
        offset = writeU32(dest, offset, @intCast(name.len));
        @memcpy(dest[offset..][0..name.len], name);
        offset += name.len;
    }

    std.debug.assert(offset == dest.len);
}

/// Sequential reader over an encoded record. Every read is bounds-checked
/// against the buffer, and the byte slices it returns borrow from that buffer.
pub const Reader = struct {
    bytes: []const u8,
    offset: usize = 0,

    pub fn init(bytes: []const u8) Reader {
        return .{ .bytes = bytes };
    }

    /// Read one count field.
    pub fn readCount(self: *Reader) DecodeError!u32 {
        return self.readU32();
    }

    /// Read one resolved parse-stage diagnostic.
    pub fn readDiagnostic(self: *Reader) DecodeError!AST.ResolvedDiagnostic {
        const stage_raw = try self.readU16();
        const tag = try self.readU16();
        const token_tag = try self.readU16();
        _ = try self.readU16();
        const start = try self.readU32();
        const end = try self.readU32();

        const stage = enumFromIntChecked(AST.ResolvedDiagnostic.Stage, stage_raw) orelse
            return DecodeError.CorruptParseStageRecord;
        try validateDiagnosticTags(stage, tag, token_tag);

        return .{
            .stage = stage,
            .tag = tag,
            .token_tag = token_tag,
            .region = .{ .start = .{ .offset = start }, .end = .{ .offset = end } },
        };
    }

    /// Read one parser-recorded local import. Its name borrows from the buffer.
    pub fn readLocalImport(self: *Reader) DecodeError!LocalImport {
        const base_raw = try self.readU16();
        const parent_count = try self.readU16();
        const name = try self.readSlice(try self.readU32());
        const base = enumFromIntChecked(DeclIndex.Import.LocalBase, base_raw) orelse
            return DecodeError.CorruptParseStageRecord;
        return .{
            .import_name = name,
            .base = base,
            .parent_count = parent_count,
        };
    }

    /// Read one package-qualified import name. It borrows from the buffer.
    pub fn readExternalImport(self: *Reader) DecodeError![]const u8 {
        return self.readSlice(try self.readU32());
    }

    /// Whether every encoded byte has been consumed. A record with trailing
    /// bytes is rejected the same way a truncated one is.
    pub fn atEnd(self: *const Reader) bool {
        return self.offset == self.bytes.len;
    }

    fn readU16(self: *Reader) DecodeError!u16 {
        const slice = try self.readSlice(2);
        return std.mem.readInt(u16, slice[0..2], .little);
    }

    fn readU32(self: *Reader) DecodeError!u32 {
        const slice = try self.readSlice(4);
        return std.mem.readInt(u32, slice[0..4], .little);
    }

    fn readSlice(self: *Reader, len: u32) DecodeError![]const u8 {
        const end = std.math.add(usize, self.offset, len) catch
            return DecodeError.CorruptParseStageRecord;
        if (end > self.bytes.len) return DecodeError.CorruptParseStageRecord;
        const slice = self.bytes[self.offset..end];
        self.offset = end;
        return slice;
    }
};

const diagnostic_encoded_len: usize = 2 + 2 + 2 + 2 + 4 + 4;
const local_import_header_len: usize = 2 + 2 + 4;

fn validateDiagnosticTags(
    stage: AST.ResolvedDiagnostic.Stage,
    tag: u16,
    token_tag: u16,
) DecodeError!void {
    switch (stage) {
        .tokenize => _ = enumFromIntChecked(parse.tokenize.Diagnostic.Tag, tag) orelse
            return DecodeError.CorruptParseStageRecord,
        .parse => _ = enumFromIntChecked(AST.Diagnostic.Tag, tag) orelse
            return DecodeError.CorruptParseStageRecord,
    }
    _ = enumFromIntChecked(AST.Token.Tag, token_tag) orelse
        return DecodeError.CorruptParseStageRecord;
}

/// Convert a stored integer into `E` only when it names one of `E`'s values, so
/// a corrupt entry is rejected instead of producing an out-of-range enum.
fn enumFromIntChecked(comptime E: type, value: u16) ?E {
    inline for (@typeInfo(E).@"enum".fields) |field| {
        if (value == field.value) return @field(E, field.name);
    }
    return null;
}

fn writeU16(dest: []u8, offset: usize, value: u16) usize {
    std.mem.writeInt(u16, dest[offset..][0..2], value, .little);
    return offset + 2;
}

fn writeU32(dest: []u8, offset: usize, value: u32) usize {
    std.mem.writeInt(u32, dest[offset..][0..4], value, .little);
    return offset + 4;
}

test "parse-stage record round-trips every field" {
    const gpa = std.testing.allocator;

    const diagnostics = [_]AST.ResolvedDiagnostic{
        .{
            .stage = .tokenize,
            .tag = @intFromEnum(parse.tokenize.Diagnostic.Tag.UnclosedString),
            .token_tag = @intFromEnum(AST.Token.Tag.EndOfFile),
            .region = .{ .start = .{ .offset = 3 }, .end = .{ .offset = 9 } },
        },
        .{
            .stage = .parse,
            .tag = @intFromEnum(AST.Diagnostic.Tag.missing_arrow),
            .token_tag = @intFromEnum(AST.Token.Tag.LowerIdent),
            .region = .{ .start = .{ .offset = 11 }, .end = .{ .offset = 17 } },
        },
    };
    const local_imports = [_]LocalImport{
        .{ .import_name = "Shared/Foo", .base = .importer, .parent_count = 0 },
        .{ .import_name = "../Up", .base = .parent, .parent_count = 2 },
    };
    const external_imports = [_][]const u8{ "pf.Stdout", "json.Json" };

    const record = ParseStageRecord{
        .diagnostics = &diagnostics,
        .local_imports = &local_imports,
        .external_imports = &external_imports,
    };

    const bytes = try gpa.alloc(u8, encodedLen(record));
    defer gpa.free(bytes);
    encode(record, bytes);

    var reader = Reader.init(bytes);
    try std.testing.expectEqual(@as(u32, diagnostics.len), try reader.readCount());
    for (diagnostics) |expected| {
        const actual = try reader.readDiagnostic();
        try std.testing.expectEqual(expected.stage, actual.stage);
        try std.testing.expectEqual(expected.tag, actual.tag);
        try std.testing.expectEqual(expected.token_tag, actual.token_tag);
        try std.testing.expectEqual(expected.region.start.offset, actual.region.start.offset);
        try std.testing.expectEqual(expected.region.end.offset, actual.region.end.offset);
    }

    try std.testing.expectEqual(@as(u32, local_imports.len), try reader.readCount());
    for (local_imports) |expected| {
        const actual = try reader.readLocalImport();
        try std.testing.expectEqualStrings(expected.import_name, actual.import_name);
        try std.testing.expectEqual(expected.base, actual.base);
        try std.testing.expectEqual(expected.parent_count, actual.parent_count);
    }

    try std.testing.expectEqual(@as(u32, external_imports.len), try reader.readCount());
    for (external_imports) |expected| {
        try std.testing.expectEqualStrings(expected, try reader.readExternalImport());
    }

    try std.testing.expect(reader.atEnd());
}

test "parse-stage record rejects truncated and corrupt bytes" {
    const gpa = std.testing.allocator;

    const diagnostics = [_]AST.ResolvedDiagnostic{.{
        .stage = .parse,
        .tag = @intFromEnum(AST.Diagnostic.Tag.missing_arrow),
        .token_tag = @intFromEnum(AST.Token.Tag.LowerIdent),
        .region = .{ .start = .{ .offset = 1 }, .end = .{ .offset = 2 } },
    }};
    const local_imports = [_]LocalImport{
        .{ .import_name = "Shared/Foo", .base = .importer, .parent_count = 0 },
    };
    const record = ParseStageRecord{
        .diagnostics = &diagnostics,
        .local_imports = &local_imports,
        .external_imports = &.{},
    };

    const bytes = try gpa.alloc(u8, encodedLen(record));
    defer gpa.free(bytes);
    encode(record, bytes);

    // A truncated body cannot be read to the end.
    {
        var reader = Reader.init(bytes[0 .. bytes.len - 1]);
        try std.testing.expectEqual(@as(u32, 1), try reader.readCount());
        _ = try reader.readDiagnostic();
        try std.testing.expectEqual(@as(u32, 1), try reader.readCount());
        _ = try reader.readLocalImport();
        try std.testing.expectError(DecodeError.CorruptParseStageRecord, reader.readCount());
    }

    // A length that reaches past the buffer is rejected, not trusted.
    {
        const corrupted = try gpa.dupe(u8, bytes);
        defer gpa.free(corrupted);
        const name_len_offset = 4 + diagnostic_encoded_len + 4 + 2 + 2;
        std.mem.writeInt(u32, corrupted[name_len_offset..][0..4], std.math.maxInt(u32), .little);

        var reader = Reader.init(corrupted);
        _ = try reader.readCount();
        _ = try reader.readDiagnostic();
        _ = try reader.readCount();
        try std.testing.expectError(DecodeError.CorruptParseStageRecord, reader.readLocalImport());
    }

    // An out-of-range enum value is rejected rather than cast blindly.
    {
        const corrupted = try gpa.dupe(u8, bytes);
        defer gpa.free(corrupted);
        std.mem.writeInt(u16, corrupted[4..][0..2], 99, .little);

        var reader = Reader.init(corrupted);
        _ = try reader.readCount();
        try std.testing.expectError(DecodeError.CorruptParseStageRecord, reader.readDiagnostic());
    }
}
