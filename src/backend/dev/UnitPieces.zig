//! One unit's machine code in pieces (design.md "Unit Pieces And
//! Whole-Program Dead Code"): one excerpt per function with its frame, source
//! lines, failure sites and outgoing references, each reference naming its
//! target by identity (a procedure), by name (a refcount helper, a host or
//! runtime function) or by content (a constant), never by a position in the
//! unit. The unit's constants travel with the excerpts that name them, under
//! their content names, so a piece is the same bytes wherever it lands.
//!
//! The unit's constants are written once, as a table, and each excerpt
//! names the constants it points into by index; decoding rebuilds the
//! constant graph, so every excerpt carries the closure it reaches without
//! a copy of it.
//!
//! The encoding is a pure function of its inputs: fixed-width little-endian
//! integers, length-prefixed bytes, excerpts in set order. Two builds of the
//! same unit write identical bytes.
const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const lir = @import("lir");
const CodeExcerpt = @import("CodeExcerpt.zig");
const RelocationMod = @import("Relocation.zig");
const Allocator = std.mem.Allocator;

const magic = "RUPC";
/// Format version; bump whenever the encoding or its contents change.
pub const format_version: u32 = 3;

/// One source file the pieces' lines name, in the order the set indexes them.
pub const SourceFile = struct {
    name: []const u8,
    qualified_name: []const u8,
    identity: [32]u8,
};

/// A refcount helper excerpt that static data reaches under a second symbol.
pub const HelperAlias = struct {
    helper: []const u8,
    alias: []const u8,
};

/// A unit's pieces and what the program's composition needs beside them.
pub const Pieces = struct {
    /// Owns every slice below except the set's own.
    arena: collections.SingleThreadArena,
    set: CodeExcerpt.Set,
    /// The constant graph decoded excerpts point into; null for pieces the
    /// backend produced, whose excerpts retain their producer's graph.
    graph: ?CodeExcerpt.PreparedData = null,
    /// The erased-callable capture prefix the unit's code was generated
    /// for; every piece of one program shares it.
    erased_capture_prefix: u32,
    /// Parallel to `set.source_files`.
    source_files: []const SourceFile,
    /// The constants the host binds by name, with every constant they name,
    /// under the content names the unit's code names them by: the roots of
    /// the unit's static data, which no excerpt need reference.
    exported_data: []const CodeExcerpt.DataItem,
    helper_aliases: []const HelperAlias,

    pub fn deinit(self: *Pieces) void {
        self.set.deinit();
        if (self.graph) |*graph| graph.deinit();
        self.arena.deinit();
        self.* = undefined;
    }
};

/// Why bytes could not be read as pieces.
pub const ReadError = Allocator.Error || error{ MalformedPieces, UnsupportedPiecesVersion };

/// Encode `pieces`. Every excerpt's constants are written flat, with the
/// constant graph it reaches materialized, as the set's consumers read them.
pub fn encode(allocator: Allocator, pieces: *const Pieces) Allocator.Error![]u8 {
    var bytes = std.ArrayList(u8).empty;
    errdefer bytes.deinit(allocator);
    var writer = Writer{ .allocator = allocator, .bytes = &bytes };
    try writer.raw(magic);
    try writer.word(format_version);
    try writer.word(pieces.erased_capture_prefix);
    const set = &pieces.set;
    std.debug.assert(pieces.source_files.len == set.source_files.len);
    try writer.word(@intCast(pieces.source_files.len));
    for (pieces.source_files, set.source_files) |file, identity| {
        std.debug.assert(std.mem.eql(u8, &file.identity, &identity));
        try writer.str(file.name);
        try writer.str(file.qualified_name);
        try writer.raw(&file.identity);
    }
    // Every constant any excerpt reaches, once, in first-use order.
    var table: std.StringArrayHashMapUnmanaged(CodeExcerpt.DataItem) = .empty;
    defer table.deinit(allocator);
    var roots = std.ArrayList([]const u32).empty;
    defer {
        for (roots.items) |list| allocator.free(list);
        roots.deinit(allocator);
    }
    var carried = CodeExcerpt.DataCollector{ .allocator = allocator };
    defer carried.deinit();
    for (set.excerpts) |excerpt| {
        carried.restart();
        try carried.add(excerpt);
        var indices = std.ArrayList(u32).empty;
        errdefer indices.deinit(allocator);
        for (carried.items.items) |item| {
            const gop = try table.getOrPut(allocator, item.name);
            if (!gop.found_existing) gop.value_ptr.* = item;
            try indices.append(allocator, @intCast(gop.index));
        }
        try roots.append(allocator, try indices.toOwnedSlice(allocator));
    }
    try writeData(&writer, table.values());
    try writer.word(@intCast(set.excerpts.len));
    for (set.excerpts, roots.items) |excerpt, excerpt_roots| {
        switch (excerpt.kind) {
            .proc => |identity| {
                try writer.byte(0);
                try writer.raw(&identity.bytes);
            },
            .rc_helper => |name| {
                try writer.byte(1);
                try writer.str(name);
            },
            .entrypoint => |name| {
                try writer.byte(2);
                try writer.str(name);
            },
            .message_pool_run => try writer.byte(3),
            .branch_island => try writer.byte(4),
        }
        try writer.word(excerpt.entry);
        if (excerpt.frame) |frame| {
            try writer.byte(1);
            try writer.word(frame.prologue_size);
            try writer.word(frame.stack_alloc);
            try writer.word(frame.frame_size);
            try writer.word(frame.callee_saved_mask);
            try writer.word(frame.epilogue_offset);
            try writer.byte(@intFromBool(frame.uses_frame_pointer));
        } else {
            try writer.byte(0);
        }
        try writer.str(excerpt.code);
        try writer.word(@intCast(excerpt.refs.len));
        for (excerpt.refs) |ref| {
            try writer.word(ref.site);
            try writer.byte(formByte(ref.form));
            try writer.word(ref.target);
            try writer.word(ref.delta);
            try writer.word(ref.veneer orelse std.math.maxInt(u32));
        }
        try writer.word(@intCast(excerpt.symbolic_refs.len));
        for (excerpt.symbolic_refs) |ref| {
            try writer.word(ref.site);
            try writer.byte(formByte(ref.form));
            try writer.word(ref.veneer orelse std.math.maxInt(u32));
            switch (ref.binding) {
                .unresolved => try writer.byte(0),
                .absent => try writer.byte(1),
                .local => |index| {
                    try writer.byte(2);
                    try writer.word(index);
                },
            }
            switch (ref.target) {
                .proc => |identity| {
                    try writer.byte(0);
                    try writer.raw(&identity.bytes);
                },
                .rc_helper => |name| {
                    try writer.byte(1);
                    try writer.str(name);
                },
            }
        }
        try writeLoc(&writer, excerpt.loc);
        try writer.word(@intCast(excerpt.lines.len));
        for (excerpt.lines) |line| {
            try writer.word(line.offset);
            try writeLoc(&writer, line.loc);
            try writer.word(line.region.start.offset);
            try writer.word(line.region.end.offset);
        }
        try writer.word(@intCast(excerpt.failure_sites.len));
        for (excerpt.failure_sites) |site| {
            try writer.word(site.offset);
            try writeFailureMeaning(&writer, site.meaning);
        }
        try writer.word(@intCast(excerpt.relocations.len));
        for (excerpt.relocations) |relocation| {
            try writer.word(relocation.offset);
            switch (relocation.kind) {
                .function => |function_kind| {
                    try writer.byte(0);
                    try writer.byte(@backingInt(function_kind));
                },
                .data => |data_kind| {
                    try writer.byte(1);
                    try writer.byte(@backingInt(data_kind));
                },
            }
            try writer.str(relocation.name);
            try writer.byte(@backingInt(relocation.scope));
        }
        try writer.word(@intCast(excerpt_roots.len));
        for (excerpt_roots) |index| try writer.word(index);
    }
    try writeData(&writer, pieces.exported_data);
    try writer.word(@intCast(pieces.helper_aliases.len));
    for (pieces.helper_aliases) |alias| {
        try writer.str(alias.helper);
        try writer.str(alias.alias);
    }
    return try bytes.toOwnedSlice(allocator);
}

/// Decode `bytes`. The result owns copies of everything it holds.
pub fn decode(allocator: Allocator, bytes: []const u8) ReadError!Pieces {
    var arena = collections.SingleThreadArena.init(allocator);
    errdefer arena.deinit();
    const a = arena.allocator();
    var reader = Reader{ .bytes = bytes };
    const header = try reader.raw(magic.len);
    if (!std.mem.eql(u8, header, magic)) return error.MalformedPieces;
    if (try reader.word() != format_version) return error.UnsupportedPiecesVersion;
    const erased_capture_prefix = try reader.word();
    const source_files = try a.alloc(SourceFile, try reader.word());
    for (source_files) |*file| {
        file.* = .{
            .name = try a.dupe(u8, try reader.str()),
            .qualified_name = try a.dupe(u8, try reader.str()),
            .identity = (try reader.raw(32))[0..32].*,
        };
    }
    var set_arena = collections.SingleThreadArena.init(allocator);
    errdefer set_arena.deinit();
    const sa = set_arena.allocator();
    const set_files = try sa.alloc([32]u8, source_files.len);
    for (source_files, set_files) |file, *identity| identity.* = file.identity;
    const table = try readData(a, &reader);
    var graph = try CodeExcerpt.PreparedData.init(allocator, &.{}, &.{}, table);
    errdefer graph.deinit();
    const excerpt_count = try reader.word();
    const excerpts = try sa.alloc(CodeExcerpt.Excerpt, excerpt_count);
    for (excerpts) |*excerpt| {
        const kind: CodeExcerpt.Kind = switch (try reader.byte()) {
            0 => .{ .proc = .{ .bytes = (try reader.raw(32))[0..32].* } },
            1 => .{ .rc_helper = try sa.dupe(u8, try reader.str()) },
            2 => .{ .entrypoint = try sa.dupe(u8, try reader.str()) },
            3 => .message_pool_run,
            4 => .branch_island,
            else => return error.MalformedPieces,
        };
        const entry = try reader.word();
        const frame: ?CodeExcerpt.Frame = switch (try reader.byte()) {
            0 => null,
            1 => .{
                .prologue_size = try reader.word(),
                .stack_alloc = try reader.word(),
                .frame_size = try reader.word(),
                .callee_saved_mask = try reader.word(),
                .epilogue_offset = try reader.word(),
                .uses_frame_pointer = try reader.flag(),
            },
            else => return error.MalformedPieces,
        };
        const code = try sa.dupe(u8, try reader.str());
        const refs = try sa.alloc(CodeExcerpt.Reference, try reader.word());
        for (refs) |*ref| {
            ref.* = .{
                .site = try reader.word(),
                .form = try readForm(&reader),
                .target = try reader.word(),
                .delta = try reader.word(),
                .veneer = optionalWord(try reader.word()),
            };
            if (ref.target >= excerpt_count) return error.MalformedPieces;
        }
        const symbolic_refs = try sa.alloc(CodeExcerpt.SymbolicReference, try reader.word());
        for (symbolic_refs) |*ref| {
            const site = try reader.word();
            const form = try readForm(&reader);
            const veneer = optionalWord(try reader.word());
            const binding: @FieldType(CodeExcerpt.SymbolicReference, "binding") = switch (try reader.byte()) {
                0 => .unresolved,
                1 => .absent,
                2 => binding: {
                    const index = try reader.word();
                    if (index >= excerpt_count) return error.MalformedPieces;
                    break :binding .{ .local = index };
                },
                else => return error.MalformedPieces,
            };
            const target: @FieldType(CodeExcerpt.SymbolicReference, "target") = switch (try reader.byte()) {
                0 => .{ .proc = .{ .bytes = (try reader.raw(32))[0..32].* } },
                1 => .{ .rc_helper = try sa.dupe(u8, try reader.str()) },
                else => return error.MalformedPieces,
            };
            ref.* = .{ .site = site, .form = form, .veneer = veneer, .binding = binding, .target = target };
        }
        const loc = try readLoc(&reader, source_files.len);
        const lines = try sa.alloc(CodeExcerpt.LineEntry, try reader.word());
        for (lines) |*line| {
            line.* = .{
                .offset = try reader.word(),
                .loc = try readLoc(&reader, source_files.len),
                .region = base.Region.from_raw_offsets(try reader.word(), try reader.word()),
            };
        }
        const failure_sites = try sa.alloc(CodeExcerpt.FailureSite, try reader.word());
        for (failure_sites) |*site| {
            site.* = .{ .offset = try reader.word(), .meaning = try readFailureMeaning(&reader) };
        }
        const relocations = try sa.alloc(CodeExcerpt.NamedRelocation, try reader.word());
        for (relocations) |*relocation| {
            const offset = try reader.word();
            const kind_tag = try reader.byte();
            const encoding_raw = try reader.byte();
            const name = try sa.dupe(u8, try reader.str());
            const scope = std.enums.fromInt(CodeExcerpt.SymbolScope, try reader.byte()) orelse return error.MalformedPieces;
            relocation.* = .{
                .offset = offset,
                .name = name,
                .scope = scope,
                .kind = switch (kind_tag) {
                    0 => .{ .function = std.enums.fromInt(RelocationMod.FunctionRelocationKind, encoding_raw) orelse return error.MalformedPieces },
                    1 => .{ .data = std.enums.fromInt(RelocationMod.DataRelocationKind, encoding_raw) orelse return error.MalformedPieces },
                    else => return error.MalformedPieces,
                },
            };
        }
        const roots = try sa.alloc(u32, try reader.word());
        for (roots) |*index| {
            index.* = try reader.word();
            if (index.* >= table.len) return error.MalformedPieces;
        }
        excerpt.* = .{
            .kind = kind,
            .code = code,
            .entry = entry,
            .frame = frame,
            .refs = refs,
            .symbolic_refs = symbolic_refs,
            .lines = lines,
            .failure_sites = failure_sites,
            .relocations = relocations,
            .data = &.{},
            .data_graph = if (roots.len == 0) null else .{ .graph = graph.graph, .roots = roots },
            .loc = loc,
        };
    }
    const exported_data = try readData(a, &reader);
    const helper_aliases = try a.alloc(HelperAlias, try reader.word());
    for (helper_aliases) |*alias| {
        alias.* = .{ .helper = try a.dupe(u8, try reader.str()), .alias = try a.dupe(u8, try reader.str()) };
    }
    if (reader.at != bytes.len) return error.MalformedPieces;
    const owners = try CodeExcerpt.Set.retainOwners(sa, excerpts);
    return .{
        .arena = arena,
        .set = .{ .arena = set_arena, .excerpts = excerpts, .source_files = set_files, .owners = owners },
        .graph = graph,
        .erased_capture_prefix = erased_capture_prefix,
        .source_files = source_files,
        .exported_data = exported_data,
        .helper_aliases = helper_aliases,
    };
}

fn formByte(form: CodeExcerpt.Form) u8 {
    return switch (form) {
        .call => 0,
        .addr => 1,
        .inline_call => 2,
    };
}

fn readForm(reader: *Reader) ReadError!CodeExcerpt.Form {
    return switch (try reader.byte()) {
        0 => .call,
        1 => .addr,
        2 => .inline_call,
        else => error.MalformedPieces,
    };
}

fn optionalWord(value: u32) ?u32 {
    return if (value == std.math.maxInt(u32)) null else value;
}

fn writeLoc(writer: *Writer, loc: base.SourceLoc) Allocator.Error!void {
    try writer.word(loc.file);
    try writer.word(loc.line);
    try writer.word(loc.column);
}

fn readLoc(reader: *Reader, file_count: usize) ReadError!base.SourceLoc {
    const loc: base.SourceLoc = .{ .file = try reader.word(), .line = try reader.word(), .column = try reader.word() };
    if (loc.hasLocation() and loc.file >= file_count) return error.MalformedPieces;
    return loc;
}

fn writeFailureMeaning(writer: *Writer, meaning: lir.LIR.FailureMeaning) Allocator.Error!void {
    switch (meaning) {
        .checked_error => try writer.byte(0),
        .literal_rejection => |literal| {
            try writer.byte(1);
            try writer.raw(&literal.module);
            try writer.word(literal.checked_expr);
            try writer.byte(@backingInt(literal.kind));
        },
        .exhaustiveness => |exhaustiveness| {
            try writer.byte(2);
            try writer.raw(&exhaustiveness.module);
            if (exhaustiveness.checked_site) |checked_site| {
                try writer.byte(1);
                try writer.word(@backingInt(checked_site));
            } else {
                try writer.byte(0);
            }
        },
        .failed_value => |value| {
            try writer.byte(3);
            try writer.raw(&value.module);
            switch (value.producer) {
                .checked => |root| {
                    try writer.byte(0);
                    try writer.word(@backingInt(root));
                },
                .literal => |root| {
                    try writer.byte(1);
                    try writer.word(@backingInt(root));
                },
            }
        },
    }
}

fn readFailureMeaning(reader: *Reader) ReadError!lir.LIR.FailureMeaning {
    return switch (try reader.byte()) {
        0 => .checked_error,
        1 => .{ .literal_rejection = .{
            .module = (try reader.raw(32))[0..32].*,
            .checked_expr = try reader.word(),
            .kind = std.enums.fromInt(lir.LIR.LiteralRejectionKind, try reader.byte()) orelse return error.MalformedPieces,
        } },
        2 => .{ .exhaustiveness = .{
            .module = (try reader.raw(32))[0..32].*,
            .checked_site = switch (try reader.byte()) {
                0 => null,
                1 => @fromBackingInt(try reader.word()),
                else => return error.MalformedPieces,
            },
        } },
        3 => .{ .failed_value = .{
            .module = (try reader.raw(32))[0..32].*,
            .producer = switch (try reader.byte()) {
                0 => .{ .checked = @fromBackingInt(try reader.word()) },
                1 => .{ .literal = @fromBackingInt(try reader.word()) },
                else => return error.MalformedPieces,
            },
        } },
        else => error.MalformedPieces,
    };
}

fn writeData(writer: *Writer, data: []const CodeExcerpt.DataItem) Allocator.Error!void {
    try writer.word(@intCast(data.len));
    for (data) |item| {
        try writer.str(item.name);
        try writer.str(item.bytes);
        try writer.word(item.alignment);
        try writer.word(item.symbol_offset);
        try writer.byte(@intFromBool(item.is_mutable));
        try writer.byte(@intFromBool(item.program_local_name));
        try writer.word(@intCast(item.relocations.len));
        for (item.relocations) |relocation| {
            try writer.word(relocation.offset);
            try writer.wide(@bitCast(relocation.addend));
            try writer.byte(@intFromBool(relocation.function));
            try writer.byte(@intFromBool(relocation.external));
            try writer.str(relocation.name);
        }
    }
}

fn readData(a: Allocator, reader: *Reader) ReadError![]CodeExcerpt.DataItem {
    const data = try a.alloc(CodeExcerpt.DataItem, try reader.word());
    for (data) |*item| {
        const name = try a.dupe(u8, try reader.str());
        const item_bytes = try a.dupe(u8, try reader.str());
        const alignment = try reader.word();
        const symbol_offset = try reader.word();
        const is_mutable = try reader.flag();
        const program_local_name = try reader.flag();
        const relocations = try a.alloc(CodeExcerpt.DataRelocation, try reader.word());
        for (relocations) |*relocation| {
            const offset = try reader.word();
            const addend: i64 = @bitCast(try reader.wide());
            const function = try reader.flag();
            const external = try reader.flag();
            relocation.* = .{
                .offset = offset,
                .name = try a.dupe(u8, try reader.str()),
                .addend = addend,
                .function = function,
                .external = external,
            };
        }
        if (is_mutable and (relocations.len != 0 or !std.mem.allEqual(u8, item_bytes, 0))) return error.MalformedPieces;
        item.* = .{
            .name = name,
            .bytes = item_bytes,
            .alignment = alignment,
            .symbol_offset = symbol_offset,
            .is_mutable = is_mutable,
            .program_local_name = program_local_name,
            .relocations = relocations,
        };
    }
    return data;
}

const Writer = struct {
    allocator: Allocator,
    bytes: *std.ArrayList(u8),

    fn raw(self: *Writer, data: []const u8) Allocator.Error!void {
        try self.bytes.appendSlice(self.allocator, data);
    }
    fn byte(self: *Writer, value: u8) Allocator.Error!void {
        try self.bytes.append(self.allocator, value);
    }
    fn word(self: *Writer, value: u32) Allocator.Error!void {
        var buffer: [4]u8 = undefined;
        std.mem.writeInt(u32, &buffer, value, .little);
        try self.raw(&buffer);
    }
    fn wide(self: *Writer, value: u64) Allocator.Error!void {
        var buffer: [8]u8 = undefined;
        std.mem.writeInt(u64, &buffer, value, .little);
        try self.raw(&buffer);
    }
    fn str(self: *Writer, data: []const u8) Allocator.Error!void {
        try self.word(@intCast(data.len));
        try self.raw(data);
    }
};

const Reader = struct {
    bytes: []const u8,
    at: usize = 0,

    fn raw(self: *Reader, len: usize) ReadError![]const u8 {
        if (self.bytes.len - self.at < len) return error.MalformedPieces;
        const slice = self.bytes[self.at..][0..len];
        self.at += len;
        return slice;
    }
    fn byte(self: *Reader) ReadError!u8 {
        return (try self.raw(1))[0];
    }
    fn flag(self: *Reader) ReadError!bool {
        return switch (try self.byte()) {
            0 => false,
            1 => true,
            else => error.MalformedPieces,
        };
    }
    fn word(self: *Reader) ReadError!u32 {
        return std.mem.readInt(u32, (try self.raw(4))[0..4], .little);
    }
    fn wide(self: *Reader) ReadError!u64 {
        return std.mem.readInt(u64, (try self.raw(8))[0..8], .little);
    }
    fn str(self: *Reader) ReadError![]const u8 {
        return self.raw(try self.word());
    }
};

test "pieces round-trip through their encoding" {
    const allocator = std.testing.allocator;
    var arena = collections.SingleThreadArena.init(allocator);
    const a = arena.allocator();
    var set_arena = collections.SingleThreadArena.init(allocator);
    const sa = set_arena.allocator();
    const identity = lir.ProcIdentity.forTest(41);
    const excerpts = try sa.alloc(CodeExcerpt.Excerpt, 2);
    excerpts[0] = .{
        .kind = .{ .proc = identity },
        .code = try sa.dupe(u8, "\x01\x02\x03\x04"),
        .entry = 0,
        .frame = .{ .prologue_size = 4, .stack_alloc = 16, .frame_size = 32, .callee_saved_mask = 3, .epilogue_offset = 2, .uses_frame_pointer = true },
        .refs = try sa.dupe(CodeExcerpt.Reference, &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0, .veneer = null }}),
        .symbolic_refs = try sa.dupe(CodeExcerpt.SymbolicReference, &.{.{ .site = 2, .form = .addr, .target = .{ .rc_helper = try sa.dupe(u8, "roc__helper") } }}),
        .lines = try sa.dupe(CodeExcerpt.LineEntry, &.{.{ .offset = 0, .loc = .{ .file = 0, .line = 3, .column = 1 }, .region = base.Region.from_raw_offsets(5, 9) }}),
        .failure_sites = try sa.dupe(CodeExcerpt.FailureSite, &.{.{ .offset = 4, .meaning = .checked_error }}),
        .relocations = try sa.dupe(CodeExcerpt.NamedRelocation, &.{.{ .offset = 0, .name = try sa.dupe(u8, "roc_alloc"), .scope = .shared, .kind = .{ .function = .aarch64_call26 } }}),
        .data = try sa.dupe(CodeExcerpt.DataItem, &.{.{ .name = try sa.dupe(u8, "roc__hcafe"), .bytes = try sa.dupe(u8, "constant"), .alignment = 8, .symbol_offset = 0 }}),
        .loc = .{ .file = 0, .line = 2, .column = 0 },
    };
    excerpts[1] = .{
        .kind = .{ .entrypoint = try sa.dupe(u8, "roc__main") },
        .code = try sa.dupe(u8, "\xc0\x03\x5f\xd6"),
        .entry = 0,
        .frame = null,
        .refs = &.{},
        .relocations = &.{},
        .data = &.{},
    };
    const set_files = try sa.dupe([32]u8, &.{@splat(7)});
    const source_files = try a.dupe(SourceFile, &.{.{ .name = "Main.roc", .qualified_name = "app.Main", .identity = @splat(7) }});
    const exported_data = try a.dupe(CodeExcerpt.DataItem, &.{.{ .name = try a.dupe(u8, "roc_answer"), .bytes = try a.dupe(u8, "\x2a"), .alignment = 1, .symbol_offset = 0 }});
    const helper_aliases = try a.dupe(HelperAlias, &.{.{ .helper = "roc__helper", .alias = "roc__helper_static" }});
    // The arenas are copied into the pieces after every allocation from them.
    var pieces = Pieces{
        .arena = arena,
        .set = .{ .arena = set_arena, .excerpts = excerpts, .source_files = set_files },
        .erased_capture_prefix = 16,
        .source_files = source_files,
        .exported_data = exported_data,
        .helper_aliases = helper_aliases,
    };
    defer pieces.deinit();
    const bytes = try encode(allocator, &pieces);
    defer allocator.free(bytes);
    var decoded = try decode(allocator, bytes);
    defer decoded.deinit();
    const again = try encode(allocator, &decoded);
    defer allocator.free(again);
    try std.testing.expectEqualSlices(u8, bytes, again);
    try std.testing.expectEqual(@as(usize, 2), decoded.set.excerpts.len);
    try std.testing.expectEqualStrings("roc__main", decoded.set.excerpts[1].kind.entrypoint);
    try std.testing.expectEqual(@as(u32, 16), decoded.erased_capture_prefix);
    try std.testing.expectEqualStrings("Main.roc", decoded.source_files[0].name);
    try std.testing.expectEqualStrings("roc_answer", decoded.exported_data[0].name);
    const carried = try CodeExcerpt.materializeData(allocator, decoded.set.excerpts[0]);
    defer allocator.free(carried);
    try std.testing.expectEqual(@as(usize, 1), carried.len);
    try std.testing.expectEqualStrings("roc__hcafe", carried[0].name);
    try std.testing.expectEqualStrings("constant", carried[0].bytes);
    try std.testing.expectEqualStrings("roc__helper_static", decoded.helper_aliases[0].alias);
    try std.testing.expectError(error.MalformedPieces, decode(allocator, bytes[0 .. bytes.len - 1]));
}
