//! The parse-stage half of a canonicalized-module cache entry.
//!
//! A canonicalized-module cache entry holds two things: the serialized
//! `ModuleEnv` canonicalization produced, and the parse-stage facts the parse
//! task produces and the coordinator consumes before canonicalization is ever
//! reached. Those facts are the module's tokenizer and parser diagnostics and
//! the import inventory the parser recorded. They are a pure function of the
//! module's source, exactly like the env, but they live in the AST rather than
//! in the env, and a cache hit has no AST.
//! A declared source version pin also carries the normalized human version
//! used to validate it; a hit checks that input before accepting the env.
//!
//! The record is laid out to be used where it sits: a `Serialized` header of
//! `SafeList.Serialized` lists of fixed-size extern records (diagnostics,
//! local imports, external imports) plus one byte list every stored name
//! points into, written with a `CompactWriter`. Reading it is a pointer cast
//! (`View.init`) followed by bounds checks; nothing is recovered, re-derived, or
//! re-parsed on a hit. Every stored offset, length and enum value is checked
//! against the record before it is used, because a corrupt record is a cache
//! miss, never a compilation failure.
//!
//! Location-dependent import normalization is deliberately *not* stored. An
//! import's package-root-relative module name depends on the importing module's
//! logical path, and its filesystem path depends on the package root; neither
//! is a canonicalization input, so neither may key or live in an entry. The
//! record holds the parser's own spelling (`import_name`, `base`,
//! `parent_count`), and the parse task normalizes it against the task's own
//! package root on a hit exactly as it does on a miss.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const parse = @import("parse");

const Allocator = std.mem.Allocator;
const CompactWriter = collections.CompactWriter;
const SafeList = collections.SafeList;
const Sha256 = base.Sha256;
const AST = parse.AST;
const DeclIndex = AST.DeclIndex;

/// Version of the layout below. Folded into the cache entry's version hash,
/// so a format change invalidates every stored entry.
pub const format_version: u32 = 3;

/// A readable source pin was checked against this version. Null means the
/// caller did not report a recognized release/nightly version, which skips
/// mismatch warnings. Unpinned source carries no stamp at all.
pub const PinValidation = struct {
    compiler_version: ?[]const u8,
};

/// Returns the supplied compiler version only when source-pin parsing recognizes it.
pub fn recognizedPinVersion(version: ?[]const u8) ?[]const u8 {
    const text = version orelse return null;
    return if (@import("base").roc_version.parse(text) != null) text else null;
}

/// Records the compiler version used to validate a recognized source header pin.
pub fn pinValidationForAst(ast: *const AST, version: ?[]const u8) ?PinValidation {
    const field = switch (ast.store.getHeader(ast.store.getFile().header)) {
        .app => |header| header.roc_version,
        .package => |header| header.roc_version,
        .platform => |header| header.roc_version,
        .module, .hosted, .type_module, .default_app, .malformed => null,
    } orelse return null;
    const pinned = ast.rocVersionText(field) orelse return null;
    if (@import("base").roc_version.parse(pinned) == null) return null;
    return .{ .compiler_version = recognizedPinVersion(version) };
}

/// Checks whether a cached source pin used the same recognized compiler version.
/// Entries without a source pin match any compiler version.
pub fn pinValidationMatches(stamp: ?PinValidation, version: ?[]const u8) bool {
    const pinned = stamp orelse return true;
    const current = recognizedPinVersion(version);
    const previous = pinned.compiler_version orelse return current == null;
    const actual = current orelse return false;
    return std.mem.eql(u8, previous, actual);
}

/// Cache metadata for the source-pin validation input, independent of the
/// checked artifact's semantic key and its target-independent identity.
pub fn pinValidationHash(has_pin: bool, version: ?[]const u8) [32]u8 {
    if (!has_pin) return @splat(0);
    var hash = Sha256.init(.{});
    hash.update("roc-source-pin-validation-v1");
    if (recognizedPinVersion(version)) |text| hash.update(text);
    return hash.finalResult();
}

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
    pin_validation: ?PinValidation = null,
};

/// A stored record is only as trustworthy as its bytes; a corrupt body is a
/// cache miss, never a compilation failure.
pub const DecodeError = error{CorruptParseStageRecord};

/// A byte range in a record's `names` list.
pub const StoredName = extern struct {
    start: u32,
    len: u32,
};

/// One resolved parse-stage diagnostic.
pub const StoredDiagnostic = extern struct {
    stage: u16,
    tag: u16,
    token_tag: u16,
    _padding: u16 = 0,
    region_start: u32,
    region_end: u32,
};

/// One parser-recorded local import.
pub const StoredLocalImport = extern struct {
    name: StoredName,
    base: u16,
    parent_count: u16,
};

/// Whether the source declared a version pin, and whether the pin was checked
/// against a recognized compiler version.
pub const PinState = enum(u32) { unpinned = 0, pinned_unrecognized = 1, pinned = 2 };

/// The record's header. Its lists are relative to the start of the record.
pub const Serialized = extern struct {
    diagnostics: SafeList(StoredDiagnostic).Serialized,
    local_imports: SafeList(StoredLocalImport).Serialized,
    external_imports: SafeList(StoredName).Serialized,
    names: SafeList(u8).Serialized,
    pin_version: StoredName,
    pin_state: u32,
    _padding: u32 = 0,
};

/// Builds a record's byte list of names.
pub const NameTable = struct {
    bytes: SafeList(u8) = .{},

    /// Append `text` and return its range.
    pub fn add(self: *NameTable, gpa: Allocator, text: []const u8) Allocator.Error!StoredName {
        const start = std.math.cast(u32, self.bytes.len()) orelse return error.OutOfMemory;
        const len = std.math.cast(u32, text.len) orelse return error.OutOfMemory;
        _ = try self.bytes.appendSlice(gpa, text);
        return .{ .start = start, .len = len };
    }
};

/// Append `record` to `writer` as a `Serialized` header followed by its lists.
/// `gpa` must outlive the writer: the lists stay referenced until it is
/// written out.
pub fn write(record: ParseStageRecord, gpa: Allocator, writer: *CompactWriter) Allocator.Error!void {
    const header = try writer.appendAlloc(gpa, Serialized);

    var names = NameTable{};
    var diagnostics = try SafeList(StoredDiagnostic).initCapacity(gpa, record.diagnostics.len);
    for (record.diagnostics) |diagnostic| _ = diagnostics.appendAssumeCapacity(.{
        .stage = @backingInt(diagnostic.stage),
        .tag = diagnostic.tag,
        .token_tag = diagnostic.token_tag,
        .region_start = diagnostic.region.start.offset,
        .region_end = diagnostic.region.end.offset,
    });
    var local_imports = try SafeList(StoredLocalImport).initCapacity(gpa, record.local_imports.len);
    for (record.local_imports) |local_import| _ = local_imports.appendAssumeCapacity(.{
        .name = try names.add(gpa, local_import.import_name),
        .base = @backingInt(local_import.base),
        .parent_count = local_import.parent_count,
    });
    var external_imports = try SafeList(StoredName).initCapacity(gpa, record.external_imports.len);
    for (record.external_imports) |name| _ = external_imports.appendAssumeCapacity(try names.add(gpa, name));

    var pin_state: PinState = .unpinned;
    var pin_version: StoredName = .{ .start = 0, .len = 0 };
    if (record.pin_validation) |stamp| {
        if (stamp.compiler_version) |version| {
            pin_state = .pinned;
            pin_version = try names.add(gpa, version);
        } else {
            pin_state = .pinned_unrecognized;
        }
    }

    try header.diagnostics.serialize(&diagnostics, gpa, writer);
    try header.local_imports.serialize(&local_imports, gpa, writer);
    try header.external_imports.serialize(&external_imports, gpa, writer);
    try header.names.serialize(&names.bytes, gpa, writer);
    header.pin_version = pin_version;
    header.pin_state = @backingInt(pin_state);
    header._padding = 0;
}

/// A validated stored record, read where it sits.
pub const View = struct {
    diagnostics: []const StoredDiagnostic,
    local_imports: []const StoredLocalImport,
    external_imports: []const StoredName,
    names: []const u8,
    pin_validation: ?PinValidation,

    /// View the record at the start of `bytes`, which must be aligned to
    /// `CompactWriter.SERIALIZATION_ALIGNMENT`. The lists are checked to lie
    /// inside `bytes`; each element is checked as it is read.
    pub fn init(bytes: []const u8) DecodeError!View {
        if (bytes.len < @sizeOf(Serialized)) return error.CorruptParseStageRecord;
        if (@intFromPtr(bytes.ptr) % CompactWriter.SERIALIZATION_ALIGNMENT.toByteUnits() != 0) return error.CorruptParseStageRecord;
        const header: *const Serialized = @ptrCast(@alignCast(bytes.ptr));
        header.diagnostics.validateRelocations(bytes.len) catch return error.CorruptParseStageRecord;
        header.local_imports.validateRelocations(bytes.len) catch return error.CorruptParseStageRecord;
        header.external_imports.validateRelocations(bytes.len) catch return error.CorruptParseStageRecord;
        header.names.validateRelocations(bytes.len) catch return error.CorruptParseStageRecord;
        const base_addr = @intFromPtr(bytes.ptr);
        var view = View{
            .diagnostics = header.diagnostics.deserializeInto(base_addr).items(),
            .local_imports = header.local_imports.deserializeInto(base_addr).items(),
            .external_imports = header.external_imports.deserializeInto(base_addr).items(),
            .names = header.names.deserializeInto(base_addr).items(),
            .pin_validation = null,
        };
        const pin_state = enumFromIntChecked(PinState, header.pin_state) orelse return error.CorruptParseStageRecord;
        view.pin_validation = switch (pin_state) {
            .unpinned => null,
            .pinned_unrecognized => .{ .compiler_version = null },
            .pinned => .{ .compiler_version = try view.name(header.pin_version) },
        };
        return view;
    }

    /// The text of a stored name.
    pub fn name(self: View, stored: StoredName) DecodeError![]const u8 {
        const end = @as(u64, stored.start) + stored.len;
        if (end > self.names.len) return error.CorruptParseStageRecord;
        return self.names[stored.start..][0..stored.len];
    }

    /// The `index`th diagnostic, with its stage and tags checked.
    pub fn diagnostic(self: View, index: usize) DecodeError!AST.ResolvedDiagnostic {
        const stored = self.diagnostics[index];
        const stage = enumFromIntChecked(AST.ResolvedDiagnostic.Stage, stored.stage) orelse
            return error.CorruptParseStageRecord;
        try validateDiagnosticTags(stage, stored.tag, stored.token_tag);
        return .{
            .stage = stage,
            .tag = stored.tag,
            .token_tag = stored.token_tag,
            .region = .{ .start = .{ .offset = stored.region_start }, .end = .{ .offset = stored.region_end } },
        };
    }

    /// The `index`th local import. Its name borrows from the record.
    pub fn localImport(self: View, index: usize) DecodeError!LocalImport {
        const stored = self.local_imports[index];
        return .{
            .import_name = try self.name(stored.name),
            .base = enumFromIntChecked(DeclIndex.Import.LocalBase, stored.base) orelse
                return error.CorruptParseStageRecord,
            .parent_count = stored.parent_count,
        };
    }

    /// The `index`th package-qualified import name. It borrows from the record.
    pub fn externalImport(self: View, index: usize) DecodeError![]const u8 {
        return self.name(self.external_imports[index]);
    }
};

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
pub fn enumFromIntChecked(comptime E: type, value: anytype) ?E {
    inline for (@typeInfo(E).@"enum".field_names, @typeInfo(E).@"enum".field_values) |field_name, field_value| {
        if (value == field_value) return @field(E, field_name);
    }
    return null;
}

/// Write `record` into a fresh aligned buffer, for tests.
fn writeToBuffer(gpa: Allocator, record: ParseStageRecord) (Allocator.Error || error{BufferTooSmall})![]align(16) u8 {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    var writer = CompactWriter.init();
    try write(record, arena.allocator(), &writer);
    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, writer.total_bytes);
    _ = try writer.writeToBuffer(buffer);
    return buffer;
}

test "parse-stage record round-trips every field" {
    const gpa = std.testing.allocator;

    const diagnostics = [_]AST.ResolvedDiagnostic{
        .{
            .stage = .tokenize,
            .tag = @backingInt(parse.tokenize.Diagnostic.Tag.UnclosedString),
            .token_tag = @backingInt(AST.Token.Tag.EndOfFile),
            .region = .{ .start = .{ .offset = 3 }, .end = .{ .offset = 9 } },
        },
        .{
            .stage = .parse,
            .tag = @backingInt(AST.Diagnostic.Tag.missing_arrow),
            .token_tag = @backingInt(AST.Token.Tag.LowerIdent),
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
        .pin_validation = .{ .compiler_version = "nightly-2026-10-05-abcdef0" },
    };

    const bytes = try writeToBuffer(gpa, record);
    defer gpa.free(bytes);
    const view = try View.init(bytes);

    try std.testing.expectEqual(diagnostics.len, view.diagnostics.len);
    for (diagnostics, 0..) |expected, i| {
        const actual = try view.diagnostic(i);
        try std.testing.expectEqual(expected.stage, actual.stage);
        try std.testing.expectEqual(expected.tag, actual.tag);
        try std.testing.expectEqual(expected.token_tag, actual.token_tag);
        try std.testing.expectEqual(expected.region.start.offset, actual.region.start.offset);
        try std.testing.expectEqual(expected.region.end.offset, actual.region.end.offset);
    }

    try std.testing.expectEqual(local_imports.len, view.local_imports.len);
    for (local_imports, 0..) |expected, i| {
        const actual = try view.localImport(i);
        try std.testing.expectEqualStrings(expected.import_name, actual.import_name);
        try std.testing.expectEqual(expected.base, actual.base);
        try std.testing.expectEqual(expected.parent_count, actual.parent_count);
    }

    try std.testing.expectEqual(external_imports.len, view.external_imports.len);
    for (external_imports, 0..) |expected, i| {
        try std.testing.expectEqualStrings(expected, try view.externalImport(i));
    }

    try std.testing.expectEqualStrings(record.pin_validation.?.compiler_version.?, view.pin_validation.?.compiler_version.?);
}

test "parse-stage record round-trips an unpinned and an unrecognized pin" {
    const gpa = std.testing.allocator;
    for ([_]?PinValidation{ null, .{ .compiler_version = null } }) |pin| {
        const bytes = try writeToBuffer(gpa, .{ .diagnostics = &.{}, .local_imports = &.{}, .external_imports = &.{}, .pin_validation = pin });
        defer gpa.free(bytes);
        const view = try View.init(bytes);
        try std.testing.expectEqual(pin == null, view.pin_validation == null);
        if (view.pin_validation) |stamp| try std.testing.expectEqual(@as(?[]const u8, null), stamp.compiler_version);
    }
}

test "parse-stage record rejects truncated and corrupt bytes" {
    const gpa = std.testing.allocator;

    const diagnostics = [_]AST.ResolvedDiagnostic{.{
        .stage = .parse,
        .tag = @backingInt(AST.Diagnostic.Tag.missing_arrow),
        .token_tag = @backingInt(AST.Token.Tag.LowerIdent),
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

    const bytes = try writeToBuffer(gpa, record);
    defer gpa.free(bytes);

    // A truncated body's lists reach past its end.
    try std.testing.expectError(error.CorruptParseStageRecord, View.init(bytes[0 .. bytes.len - 1]));
    try std.testing.expectError(error.CorruptParseStageRecord, View.init(bytes[0 .. @sizeOf(Serialized) - 1]));

    const header: *Serialized = @ptrCast(bytes.ptr);

    // A name that reaches past the names list is rejected, not trusted.
    {
        const view = try View.init(bytes);
        const stored: *StoredLocalImport = @constCast(&view.local_imports[0]);
        const saved = stored.name;
        stored.name.len = std.math.maxInt(u32);
        try std.testing.expectError(error.CorruptParseStageRecord, view.localImport(0));
        stored.name = saved;
    }

    // An out-of-range enum value is rejected rather than cast blindly.
    {
        const view = try View.init(bytes);
        const stored: *StoredDiagnostic = @constCast(&view.diagnostics[0]);
        stored.stage = 99;
        try std.testing.expectError(error.CorruptParseStageRecord, view.diagnostic(0));
    }

    // An unknown pin state is rejected.
    header.pin_state = 7;
    try std.testing.expectError(error.CorruptParseStageRecord, View.init(bytes));
}
