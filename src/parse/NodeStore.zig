//! Typesafe access to an underlying SoA of Nodes.
//! This - along with the types used in its API - should
//! be the only way that other modules interact with
//! the AST.

const std = @import("std");
const Allocator = std.mem.Allocator;
const base = @import("base");

const AST = @import("AST.zig");
const Node = @import("Node.zig");
const NumericLiteral = @import("NumericLiteral.zig");
const Token = @import("tokenize.zig").Token;
const Region = AST.TokenizedRegion;
const Diagnostic = AST.Diagnostic;

const TargetConfigValueNodeTag = enum(u32) {
    int_literal,
    string_literal,
    tag_literal,
    ident,
    list,
    files,
    malformed,
};

const HeaderNodeTag = enum {
    app_header,
    module_header,
    hosted_header,
    package_header,
    platform_header,
    type_module_header,
    default_app_header,
    malformed,
};

const ExposedItemNodeTag = enum {
    exposed_item_lower,
    exposed_item_upper,
    exposed_item_upper_star,
    malformed,
};

const StatementNodeTag = enum {
    decl,
    @"var",
    expr,
    import,
    expect,
    @"for",
    for_bang,
    @"while",
    crash,
    dbg,
    @"return",
    @"break",
    type_decl,
    type_decl_nominal,
    type_decl_opaque,
    type_decl_where_alias,
    type_anno,
    file_import,
    malformed,
};

const PatternNodeTag = enum {
    ident_patt,
    var_ident_patt,
    tag_patt,
    string_patt,
    single_quote_patt,
    int_patt,
    frac_patt,
    typed_int_patt,
    typed_frac_patt,
    record_patt,
    list_patt,
    list_rest_patt,
    tuple_patt,
    alternatives_patt,
    underscore_patt,
    as_patt,
    malformed,
};

const ExprNodeTag = enum {
    int,
    frac,
    typed_int,
    typed_frac,
    single_quote,
    ident,
    tag,
    string_part,
    string,
    typed_string,
    typed_multiline_string,
    multiline_string,
    list,
    tuple,
    record,
    record_update,
    field_access,
    method_call,
    tuple_access,
    arrow_call,
    arrow_method_call,
    lambda,
    apply,
    suffix_single_question,
    if_then_else,
    if_without_else,
    match,
    dbg,
    crash,
    bin_op,
    block,
    ellipsis,
    for_expr,
    for_bang_expr,
    break_expr,
    return_expr,
    malformed,
    record_builder,
    nominal_record,
    nominal_apply,
    unary_op,
};

const WhereClauseNodeTag = enum {
    where_mod_method,
    where_mod_alias,
    malformed,
};

const TypeAnnoNodeTag = enum {
    ty_apply,
    ty_var,
    ty_underscore_var,
    ty_underscore,
    ty_ty,
    ty_union,
    ty_tuple,
    ty_record,
    ty_fn,
    ty_parens,
    malformed,
};

const TargetFileNodeTag = enum {
    target_file_string,
    target_file_ident,
    malformed,
};

fn narrowNodeTag(comptime T: type, tag: Node.Tag) ?T {
    const table = comptime blk: {
        @setEvalBranchQuota(100_000);
        var narrowed = std.EnumArray(Node.Tag, ?T).initFill(null);
        for (std.enums.values(Node.Tag)) |t| {
            if (@hasField(T, @tagName(t))) narrowed.set(t, @field(T, @tagName(t)));
        }
        break :blk narrowed;
    };
    return table.get(tag);
}

/// Where one field of a slot-encoded AST variant is stored in its node.
const Slot = enum {
    main_token,
    lhs,
    rhs,
    /// A span: its start in `lhs` and its length in `rhs`.
    span,
    /// A span appended to extra_data as start then length, with its index stored in `rhs`.
    rhs_extra_span,
};

/// Builds the node for a slot-encoded AST variant. `shape` is the node tag
/// followed by a struct mapping each field of `value` other than `region` to
/// the `Slot` that stores it.
fn slotNode(store: *NodeStore, comptime shape: anytype, value: anytype) std.mem.Allocator.Error!Node {
    const slots = shape[1];
    var node = Node{ .tag = shape[0], .main_token = 0, .data = .{ .lhs = 0, .rhs = 0 }, .region = value.region };
    inline for (@typeInfo(@TypeOf(value)).@"struct".field_names) |name| {
        if (comptime std.mem.eql(u8, name, "region")) continue;
        const field = @field(value, name);
        switch (@as(Slot, @field(slots, name))) {
            .main_token => node.main_token = try store.encodeSlotWord(field),
            .lhs => node.data.lhs = try store.encodeSlotWord(field),
            .rhs => node.data.rhs = try store.encodeSlotWord(field),
            .span => node.data = .{ .lhs = field.span.start, .rhs = field.span.len },
            .rhs_extra_span => {
                node.data.rhs = @intCast(store.extra_data.items.len);
                try store.extra_data.appendSlice(store.gpa, &.{ field.span.start, field.span.len });
            },
        }
    }
    return node;
}

/// Reads the `Union` variant stored by `slotNode` in a node tagged `tag`, whose
/// shape is the entry in `shapes` with that tag.
fn slotValue(store: *const NodeStore, comptime Union: type, comptime shapes: anytype, comptime tag: anytype, node: Node) Union {
    const variant = comptime blk: {
        for (@typeInfo(@TypeOf(shapes)).@"struct".field_names) |name| {
            if (@field(shapes, name)[0] == @field(Node.Tag, @tagName(tag))) break :blk name;
        }
        @compileError("no slot shape stores a ." ++ @tagName(tag) ++ " node");
    };
    return @unionInit(Union, variant, store.slotStruct(@FieldType(Union, variant), @field(shapes, variant)[1], node));
}

/// Reads the `V` stored by `slotNode` under `slots` back out of `node`.
fn slotStruct(store: *const NodeStore, comptime V: type, comptime slots: anytype, node: Node) V {
    var value: V = undefined;
    inline for (@typeInfo(V).@"struct".field_names) |name| {
        const T = @FieldType(V, name);
        if (comptime std.mem.eql(u8, name, "region")) {
            value.region = node.region;
        } else {
            @field(value, name) = switch (@as(Slot, @field(slots, name))) {
                .main_token => store.decodeSlotWord(T, node.main_token),
                .lhs => store.decodeSlotWord(T, node.data.lhs),
                .rhs => store.decodeSlotWord(T, node.data.rhs),
                .span => .{ .span = .{ .start = node.data.lhs, .len = node.data.rhs } },
                .rhs_extra_span => .{ .span = .{
                    .start = store.extra_data.items[node.data.rhs],
                    .len = store.extra_data.items[node.data.rhs + 1],
                } },
            };
        }
    }
    return value;
}

/// Appends the node `slotNode` builds for the AST struct `value` and returns its index.
fn addSlotted(store: *NodeStore, comptime Idx: type, comptime shape: anytype, value: anytype) std.mem.Allocator.Error!Idx {
    const nid = try store.nodes.append(store.gpa, try store.slotNode(shape, value));
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

/// Reads the AST struct `V` stored by `addSlotted` under `shape` at `idx`.
fn getSlotted(store: *const NodeStore, comptime V: type, comptime shape: anytype, idx: anytype) V {
    const node = store.nodes.get(@fromBackingInt(@intCast(@backingInt(idx))));
    std.debug.assert(node.tag == shape[0]);
    return store.slotStruct(V, shape[1], node);
}

/// Converts a slot field to the word that stores it: an index to its backing
/// integer, an optional to zero or its packed non-null word, and a literal
/// type suffix to its extra-data index.
fn encodeSlotWord(store: *NodeStore, value: anytype) std.mem.Allocator.Error!u32 {
    const T = @TypeOf(value);
    if (T == AST.LiteralTypeSuffix) return store.addLiteralTypeSuffix(value);
    if (@typeInfo(T) == .optional) return if (value) |inner| packNonNullOptionalU32(try store.encodeSlotWord(inner)) else 0;
    if (@typeInfo(T) == .@"enum") return @backingInt(value);
    return value;
}

/// Converts a slot word back to the field it stores, inverting `encodeSlotWord`.
fn decodeSlotWord(store: *const NodeStore, comptime T: type, word: u32) T {
    if (T == AST.LiteralTypeSuffix) return store.getLiteralTypeSuffix(word);
    if (@typeInfo(T) == .optional) return if (word == 0) null else store.decodeSlotWord(@typeInfo(T).optional.child, unpackNonNullOptionalU32(word));
    if (@typeInfo(T) == .@"enum") return @fromBackingInt(@intCast(word));
    return word;
}

/// Packed optional indices store null as 0 and non-null values as value + 1.
/// All producers must use the fallible helpers below so maxInt(u32) overflows
/// become OutOfMemory in every build mode.
const OPTIONAL_VALUE_OFFSET: u32 = 1;

/// The root node is always stored at index 0 in the node list.
pub const root_node_idx: Node.List.Idx = .first;

const NodeStore = @This();

/// The kinds whose indices are collected in scratch buffers before being
/// moved into `extra_data` spans, one buffer per kind.
const scratch_kinds = [_]type{
    AST.Statement,
    Token,
    AST.Expr,
    AST.Pattern,
    AST.PatternStringPart,
    AST.RecordField,
    AST.PatternRecordField,
    AST.MatchBranch,
    AST.TypeAnno,
    AST.AnnoRecordField,
    AST.ExposedItem,
    AST.WhereClause,
    AST.TargetEntry,
    AST.TargetFile,
    AST.TargetConfigEntry,
    AST.TargetConfigValue,
    AST.SymbolMapEntry,
    AST.ForClauseTypeAlias,
    AST.RequiresEntry,
};

const Scratches = @Tuple(&blk: {
    var types: [scratch_kinds.len]type = undefined;
    for (scratch_kinds, &types) |Kind, *T| T.* = base.Scratch(Kind.Idx);
    break :blk types;
});

fn scratchIndex(comptime Kind: type) usize {
    for (scratch_kinds, 0..) |K, i| {
        if (K == Kind) return i;
    }
    @compileError("no scratch buffer for " ++ @typeName(Kind));
}

gpa: std.mem.Allocator,
nodes: Node.List,
extra_data: std.ArrayList(u32),
scratch: Scratches,
numeric_literals: std.ArrayList(NumericLiteral.Stored),
numeric_literal_bytes: std.ArrayList(u8),
pattern_string_parts: std.ArrayList(AST.PatternStringPart),
field_access_segments: std.ArrayList(AST.FieldAccessSegment),

fn reserveExtraDataStart(store: *NodeStore, count: usize) std.mem.Allocator.Error!u32 {
    const start = std.math.cast(u32, store.extra_data.items.len) orelse return error.OutOfMemory;
    if (count != 0) {
        const last_offset = std.math.cast(u32, count - 1) orelse return error.OutOfMemory;
        if (start > std.math.maxInt(u32) - last_offset) return error.OutOfMemory;
    }
    try store.extra_data.ensureUnusedCapacity(store.gpa, count);
    return start;
}

fn reserveExtraDataToken(store: *NodeStore, count: usize) std.mem.Allocator.Error!u32 {
    const start = try store.reserveExtraDataStart(count);
    return packNonNullOptionalU32(start);
}

fn extraDataStart(token: u32) u32 {
    std.debug.assert(token != 0);
    return unpackNonNullOptionalU32(token);
}

fn packNonNullOptionalU32(value: u32) std.mem.Allocator.Error!u32 {
    if (value == std.math.maxInt(u32)) return error.OutOfMemory;
    return value + OPTIONAL_VALUE_OFFSET;
}

fn packOptionalIndex(value: anytype) std.mem.Allocator.Error!u32 {
    if (value) |idx| {
        return packNonNullOptionalU32(@backingInt(idx));
    }
    return 0;
}

fn packOptionalToken(value: ?Token.Idx) std.mem.Allocator.Error!u32 {
    if (value) |idx| {
        return packNonNullOptionalU32(idx);
    }
    return 0;
}

fn unpackNonNullOptionalU32(value: u32) u32 {
    std.debug.assert(value != 0);
    return value - OPTIONAL_VALUE_OFFSET;
}

fn unpackOptionalIndex(comptime Idx: type, value: u32) ?Idx {
    if (value == 0) return null;
    return @fromBackingInt(@intCast(unpackNonNullOptionalU32(value)));
}

fn unpackOptionalToken(value: u32) ?Token.Idx {
    if (value == 0) return null;
    return unpackNonNullOptionalU32(value);
}

const TypeDeclExtra = struct {
    where: ?AST.Collection.Idx,
    associated: ?AST.Associated,
};

fn decodeTypeDeclExtra(store: *const NodeStore, token: u32) TypeDeclExtra {
    if (token == 0) {
        return .{
            .where = null,
            .associated = null,
        };
    }

    const extra_start = extraDataStart(token);
    const has_where = store.extra_data.items[extra_start] != 0;
    const where_clause: ?AST.Collection.Idx = if (has_where)
        @fromBackingInt(@intCast(store.extra_data.items[extra_start + 1]))
    else
        null;

    const has_associated = store.extra_data.items[extra_start + 2] != 0;
    const associated: ?AST.Associated = if (has_associated) blk: {
        const stmt_start = store.extra_data.items[extra_start + 3];
        const stmt_len = store.extra_data.items[extra_start + 4];
        const scope_idx = store.extra_data.items[extra_start + 5];
        const reg_start = store.extra_data.items[extra_start + 6];
        const reg_end = store.extra_data.items[extra_start + 7];
        break :blk AST.Associated{
            .statements = AST.Statement.Span{ .span = .{ .start = stmt_start, .len = stmt_len } },
            .scope = @fromBackingInt(@intCast(scope_idx)),
            .region = .{ .start = reg_start, .end = reg_end },
        };
    } else null;

    return .{
        .where = where_clause,
        .associated = associated,
    };
}

/// Union variant counts, which the node store tests use as coverage floors so
/// a new AST variant cannot go untested.
///
/// Count of the header nodes in the AST
pub const AST_HEADER_NODE_COUNT = @typeInfo(AST.Header).@"union".field_names.len;
/// Count of the statement nodes in the AST
pub const AST_STATEMENT_NODE_COUNT = @typeInfo(AST.Statement).@"union".field_names.len;
/// Count of the pattern nodes in the AST
pub const AST_PATTERN_NODE_COUNT = @typeInfo(AST.Pattern).@"union".field_names.len;
/// Count of the type annotation nodes in the AST
pub const AST_TYPE_ANNO_NODE_COUNT = @typeInfo(AST.TypeAnno).@"union".field_names.len;
/// Count of the expression nodes in the AST
pub const AST_EXPR_NODE_COUNT = @typeInfo(AST.Expr).@"union".field_names.len;

/// Initialize the store with an assumed capacity to
/// ensure resizing of underlying data structures happens
/// very rarely.
pub fn initCapacity(gpa: std.mem.Allocator, capacity: usize) std.mem.Allocator.Error!NodeStore {
    var nodes = try Node.List.initCapacity(gpa, capacity);
    errdefer nodes.deinit(gpa);
    var extra_data = try std.ArrayList(u32).initCapacity(gpa, capacity / 2);
    errdefer extra_data.deinit(gpa);
    var scratch: Scratches = undefined;
    inline for (0..scratch_kinds.len) |i| {
        scratch[i] = @TypeOf(scratch[i]).init(gpa) catch |err| {
            inline for (0..i) |j| scratch[j].deinit();
            return err;
        };
    }
    errdefer inline for (0..scratch_kinds.len) |i| scratch[i].deinit();
    var numeric_literals = try std.ArrayList(NumericLiteral.Stored).initCapacity(gpa, capacity / 8);
    errdefer numeric_literals.deinit(gpa);
    var numeric_literal_bytes = try std.ArrayList(u8).initCapacity(gpa, capacity);
    errdefer numeric_literal_bytes.deinit(gpa);
    var pattern_string_parts = try std.ArrayList(AST.PatternStringPart).initCapacity(gpa, capacity / 8);
    errdefer pattern_string_parts.deinit(gpa);
    var field_access_segments = try std.ArrayList(AST.FieldAccessSegment).initCapacity(gpa, capacity / 4);
    errdefer field_access_segments.deinit(gpa);

    var store: NodeStore = .{
        .gpa = gpa,
        .nodes = nodes,
        .extra_data = extra_data,
        .scratch = scratch,
        .numeric_literals = numeric_literals,
        .numeric_literal_bytes = numeric_literal_bytes,
        .pattern_string_parts = pattern_string_parts,
        .field_access_segments = field_access_segments,
    };

    const expected_idx = store.nodes.items.len;
    const idx = try store.nodes.append(gpa, .{
        .tag = .root,
        .main_token = 0,
        .data = .{ .lhs = 0, .rhs = 0 },
        .region = .{ .start = 0, .end = 0 },
    });
    std.debug.assert(@backingInt(idx) == expected_idx);
    return store;
}

/// Deinitializes all data owned by the store.
/// A caller should ensure that they have taken
/// ownership of all Node data before calling this
/// method.
pub fn deinit(store: *NodeStore) void {
    store.nodes.deinit(store.gpa);
    store.extra_data.deinit(store.gpa);
    inline for (0..scratch_kinds.len) |i| store.scratch[i].deinit();
    store.numeric_literals.deinit(store.gpa);
    store.numeric_literal_bytes.deinit(store.gpa);
    store.pattern_string_parts.deinit(store.gpa);
    store.field_access_segments.deinit(store.gpa);
}

/// Ensures that all scratch buffers in the store
/// are clear for use.
pub fn emptyScratch(store: *NodeStore) void {
    inline for (0..scratch_kinds.len) |i| store.scratch[i].clearFrom(0);
}

/// Prints debug information about all nodes and scratch buffers in the store.
pub fn debug(store: *NodeStore, writer: *std.Io.Writer) void {
    store.debugTo(writer) catch {};
}

/// Writes debug information about all nodes and scratch buffers to the given writer.
pub fn debugTo(store: *NodeStore, writer: *std.Io.Writer) error{WriteFailed}!void {
    try writer.print("\n==> IR.NodeStore DEBUG <==\n", .{});
    try writer.print("Nodes:\n", .{});
    var nodes_iter = store.nodes.iterIndices();
    while (nodes_iter.next()) |idx| {
        try writer.print("{d}: {any}\n", .{ @backingInt(idx), store.nodes.get(idx) });
    }
    try writer.print("Extra Data: {any}\n", .{store.extra_data.items});
    inline for (scratch_kinds, 0..) |Kind, i| {
        try writer.print("Scratch " ++ @typeName(Kind) ++ ": {any}\n", .{store.scratch[i].items});
    }
    try writer.print("==> IR.NodeStore DEBUG <==\n\n", .{});
}

/// Any node type can be malformed, but must come with a diagnostic reason
pub fn addMalformed(store: *NodeStore, comptime T: type, reason: Diagnostic.Tag, region: Region) std.mem.Allocator.Error!T {
    const nid = try store.nodes.append(store.gpa, .{
        .tag = .malformed,
        .main_token = 0,
        .data = .{ .lhs = @backingInt(reason), .rhs = 0 },
        .region = region,
    });
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

/// Records parser-owned numeric literal facts and returns the id stored on the AST node.
pub fn addNumericLiteral(
    store: *NodeStore,
    text: []const u8,
    kind: NumericLiteral.Kind,
) std.mem.Allocator.Error!NumericLiteral.Idx {
    // The parse's digit buffers die here once copied into the store, so the
    // usual literal parses entirely on this frame.
    var parse_buffer: [1024]u8 align(@alignOf(usize)) = undefined;
    var parse_state = std.heap.BufferFirstAllocator.init(&parse_buffer, store.gpa);
    const parse_allocator = parse_state.allocator();
    const parsed = try NumericLiteral.parse(parse_allocator, text, kind);
    defer parsed.deinit(parse_allocator);

    const digits_start: u32 = @intCast(store.numeric_literal_bytes.items.len);
    try store.numeric_literal_bytes.appendSlice(store.gpa, parsed.before);
    try store.numeric_literal_bytes.appendSlice(store.gpa, parsed.after);

    const idx: NumericLiteral.Idx = @fromBackingInt(@intCast(store.numeric_literals.items.len));
    try store.numeric_literals.append(store.gpa, .{
        .kind = parsed.kind,
        .compact = parsed.compact,
        .digits_start = digits_start,
        .before_len = @intCast(parsed.before.len),
        .after_len = @intCast(parsed.after.len),
        .after_decimal_digit_count = parsed.after_decimal_digit_count,
        .flags = .{
            .is_negative = parsed.is_negative,
            .had_decimal_point = parsed.had_decimal_point,
            .is_materialized = parsed.is_materialized,
        },
    });
    return idx;
}

/// Return parsed numeric literal facts by index.
pub fn getNumericLiteral(store: *const NodeStore, idx: NumericLiteral.Idx) NumericLiteral.Stored {
    return store.numeric_literals.items[@backingInt(idx)];
}

/// Return base-256 digits before the decimal point for a numeric literal.
pub fn numericDigitsBefore(store: *const NodeStore, literal: NumericLiteral.Stored) []const u8 {
    return store.numeric_literal_bytes.items[literal.digits_start..][0..literal.before_len];
}

/// Return base-256 digits after the decimal point for a numeric literal.
pub fn numericDigitsAfter(store: *const NodeStore, literal: NumericLiteral.Stored) []const u8 {
    const start = literal.digits_start + literal.before_len;
    return store.numeric_literal_bytes.items[start..][0..literal.after_len];
}

/// Adds a file node to the store.
pub fn addFile(store: *NodeStore, file: AST.File) std.mem.Allocator.Error!void {
    try store.extra_data.append(store.gpa, @backingInt(file.header));
    store.nodes.set(root_node_idx, .{
        .tag = .root,
        .main_token = @backingInt(file.scope),
        .data = .{ .lhs = file.statements.span.start, .rhs = file.statements.span.len },
        .region = file.region,
    });
}

/// Adds a AST.Collection node with the specified tag and returns its index.
pub fn addCollection(store: *NodeStore, tag: Node.Tag, collection: AST.Collection) std.mem.Allocator.Error!AST.Collection.Idx {
    const nid = try store.nodes.append(store.gpa, Node{
        .tag = tag,
        .main_token = 0,
        .data = .{
            .lhs = collection.span.start,
            .rhs = collection.span.len,
        },
        .region = collection.region,
        .collection_layout = collection.layout,
    });
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

/// Node shapes of the `AST.Header` variants stored by `slotNode`, by variant name.
const header_shapes = .{
    .module = .{ .module_header, .{ .exposes = .lhs } },
    .hosted = .{ .hosted_header, .{ .exposes = .lhs } },
    .type_module = .{ .type_module_header, .{} },
    .default_app = .{ .default_app_header, .{ .main_fn_idx = .main_token } },
    .malformed = .{ .malformed, .{ .reason = .lhs } },
};

/// Adds a header node of any type (app, module, hosted, package, platform) and returns its index.
pub fn addHeader(store: *NodeStore, header: AST.Header) std.mem.Allocator.Error!AST.Header.Idx {
    var node = Node{
        .tag = .statement,
        .main_token = 0,
        .data = .{
            .lhs = 0,
            .rhs = 0,
        },
        .region = AST.TokenizedRegion.empty(),
    };
    switch (header) {
        inline .app, .package => |h, variant| {
            node.tag = if (variant == .app) .app_header else .package_header;
            // The provided or exposed collection is stored in the node.
            // `packages`, the optional platform entry, and the optional `roc`
            // version pin do not all fit in the node, so they share an
            // extra_data record.
            node.data.lhs = @backingInt(if (variant == .app) h.provides else h.exposes);
            const ed_start = try store.reserveExtraDataStart(3);
            store.extra_data.appendAssumeCapacity(@backingInt(h.packages));
            store.extra_data.appendAssumeCapacity(try packOptionalIndex(h.roc_version));
            store.extra_data.appendAssumeCapacity(try packOptionalIndex(h.platform_idx));
            node.data.rhs = ed_start;
            node.region = h.region;
        },
        inline .module, .hosted, .type_module, .default_app => |v, variant| node = try store.slotNode(@field(header_shapes, @tagName(variant)), v),
        .platform => |platform| {
            node.tag = .platform_header;
            node.main_token = platform.name;

            const ed_start = try store.reserveExtraDataStart(17);
            // Store requires_entries span (start and len)
            store.extra_data.appendAssumeCapacity(platform.requires_entries.span.start);
            store.extra_data.appendAssumeCapacity(platform.requires_entries.span.len);
            store.extra_data.appendAssumeCapacity(@backingInt(platform.exposes));
            store.extra_data.appendAssumeCapacity(@backingInt(platform.packages));
            store.extra_data.appendAssumeCapacity(platform.provides.span.start);
            store.extra_data.appendAssumeCapacity(platform.provides.span.len);
            store.extra_data.appendAssumeCapacity(platform.provides.region.start);
            store.extra_data.appendAssumeCapacity(platform.provides.region.end);
            store.extra_data.appendAssumeCapacity(platform.hosted.span.start);
            store.extra_data.appendAssumeCapacity(platform.hosted.span.len);
            store.extra_data.appendAssumeCapacity(platform.hosted.region.start);
            store.extra_data.appendAssumeCapacity(platform.hosted.region.end);
            const symbol_map_layouts: u32 = @backingInt(platform.provides.layout) |
                (@as(u32, @backingInt(platform.hosted.layout)) << 8);
            store.extra_data.appendAssumeCapacity(symbol_map_layouts);
            store.extra_data.appendAssumeCapacity(try packOptionalIndex(platform.targets));
            store.extra_data.appendAssumeCapacity(try packOptionalIndex(platform.roc_version));
            store.extra_data.appendAssumeCapacity(platform.requires_entries.region.start);
            store.extra_data.appendAssumeCapacity(platform.requires_entries.region.end);

            node.data.lhs = ed_start;
            node.data.rhs = 17;

            node.region = platform.region;
        },
        .malformed => {
            base.invariant("{s}", .{"Use addMalformed instead"});
        },
    }

    const node_idx = try store.nodes.append(store.gpa, node);
    return @fromBackingInt(@intCast(@backingInt(node_idx)));
}

/// Adds an exposed item node (function, value, type, etc.) and returns its index.
pub fn addExposedItem(store: *NodeStore, item: AST.ExposedItem) std.mem.Allocator.Error!AST.ExposedItem.Idx {
    var node = Node{
        .tag = .malformed,
        .main_token = 0,
        .data = .{
            .lhs = 0,
            .rhs = 0,
        },
        .region = AST.TokenizedRegion.empty(),
    };

    switch (item) {
        inline .lower_ident, .upper_ident => |i, variant| {
            node.tag = if (variant == .lower_ident) .exposed_item_lower else .exposed_item_upper;
            node.main_token = i.ident;
            if (i.qualifiers.span.len > 0) {
                const extra_start = @as(u32, @intCast(store.extra_data.items.len));
                try store.extra_data.append(store.gpa, i.qualifiers.span.start);
                try store.extra_data.append(store.gpa, i.qualifiers.span.len);
                try store.extra_data.append(store.gpa, if (i.as) |a| a else 0);
                node.data.lhs = extra_start;
                node.data.rhs = 2;
            } else if (i.as) |a| {
                std.debug.assert(a > 0);
                node.data.lhs = a;
                node.data.rhs = 1;
            }
            node.region = i.region;
        },
        .upper_ident_star => |i| {
            node.tag = .exposed_item_upper_star;
            node.main_token = i.ident;
            node.data.lhs = i.qualifiers.span.start;
            node.data.rhs = i.qualifiers.span.len;
            node.region = i.region;
        },
        .malformed => |m| {
            node.tag = .malformed;
            node.data.lhs = @backingInt(m.reason);
            node.data.rhs = 0;
            node.region = m.region;
        },
    }

    const nid = try store.nodes.append(store.gpa, node);
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

/// Node shapes of the `AST.Statement` variants stored by `slotNode`, by variant name.
const statement_shapes = .{
    .decl = .{ .decl, .{ .pattern = .lhs, .body = .rhs } },
    .@"var" = .{ .@"var", .{ .name = .main_token, .body = .lhs } },
    .expr = .{ .expr, .{ .expr = .lhs } },
    .crash = .{ .crash, .{ .expr = .lhs } },
    .dbg = .{ .dbg, .{ .expr = .lhs } },
    .expect = .{ .expect, .{ .body = .lhs } },
    .@"return" = .{ .@"return", .{ .expr = .lhs } },
    .@"break" = .{ .@"break", .{} },
    .malformed = .{ .malformed, .{ .reason = .lhs } },
};

/// Adds a statement node (value def, type def, import, etc.) and returns its index.
pub fn addStatement(store: *NodeStore, statement: AST.Statement) std.mem.Allocator.Error!AST.Statement.Idx {
    var node = Node{
        .tag = .statement,
        .main_token = 0,
        .data = .{
            .lhs = 0,
            .rhs = 0,
        },
        .region = AST.TokenizedRegion.empty(),
    };
    switch (statement) {
        inline .decl, .@"var", .expr, .crash, .dbg, .expect, .@"break", .@"return" => |v, variant| node = try store.slotNode(@field(statement_shapes, @tagName(variant)), v),
        .@"for" => |f| {
            node.tag = switch (f.kind) {
                .iter => .@"for",
                .stream => .for_bang,
            };
            node.main_token = @backingInt(f.patt);
            node.data.lhs = @backingInt(f.expr);
            node.data.rhs = @backingInt(f.body);
            node.region = f.region;
        },
        .@"while" => |w| {
            node.tag = .@"while";
            node.main_token = @backingInt(w.cond);
            node.data.lhs = @backingInt(w.cond);
            node.data.rhs = @backingInt(w.body);
            node.region = w.region;
        },
        .import => |i| {
            node.tag = .import;
            node.region = i.region;
            node.main_token = i.target.module_name_tok;
            var rhs = AST.ImportRhs{
                .aliased = 0,
                .qualified = 0,
                .has_nested = @intFromBool(i.target.hasNestedTypes()),
                .origin = @backingInt(i.target.origin),
                .base = @backingInt(i.target.base),
                .reserved = 0,
            };

            // [exposes.start, exposes.len, target.start, target.path_start,
            //  parent_count, nested_start?, nested_len, qualifier?, alias?]
            const data_start = @as(u32, @intCast(store.extra_data.items.len));
            try store.extra_data.append(store.gpa, i.exposes.span.start);
            try store.extra_data.append(store.gpa, i.exposes.span.len);
            try store.extra_data.append(store.gpa, i.target.start_tok);
            try store.extra_data.append(store.gpa, i.target.path_start_tok);
            try store.extra_data.append(store.gpa, i.target.parent_count);
            if (i.target.nested_start_tok) |tok| {
                try store.extra_data.append(store.gpa, tok);
            }
            try store.extra_data.append(store.gpa, i.target.nested_len);

            if (i.target.qualifier_tok) |tok| {
                rhs.qualified = 1;
                try store.extra_data.append(store.gpa, tok);
            }
            if (i.alias_tok) |tok| {
                rhs.aliased = 1;
                try store.extra_data.append(store.gpa, tok);
            }

            node.data.rhs = @as(u32, @bitCast(rhs));
            node.data.lhs = data_start;
        },
        .type_decl => |d| {
            node.tag = switch (d.kind) {
                .alias => .type_decl,
                .nominal => .type_decl_nominal,
                .@"opaque" => .type_decl_opaque,
                .where_alias => .type_decl_where_alias,
            };
            node.region = d.region;
            node.data.lhs = @backingInt(d.header);
            node.data.rhs = @backingInt(d.anno);

            // Store optional where and associated in extra_data if either is present.
            // Collection.Idx 0 is valid, so presence is stored explicitly.
            if (d.where != null or d.associated != null) {
                // Format: [has_where, where_idx, has_associated, associated_data...]
                // where_idx is meaningful only when has_where is 1.
                // has_associated is 0 or 1
                // associated_data is [statements_start, statements_len, scope_idx, region_start, region_end] if has_associated == 1
                const extra_count: usize = if (d.associated != null) 8 else 3;
                node.main_token = try store.reserveExtraDataToken(extra_count);

                const has_where: u32 = @intFromBool(d.where != null);
                const where_idx: u32 = if (d.where) |w| @backingInt(w) else 0;
                store.extra_data.appendAssumeCapacity(has_where);
                store.extra_data.appendAssumeCapacity(where_idx);

                // Store associated data if present
                if (d.associated) |assoc| {
                    store.extra_data.appendAssumeCapacity(1); // has_associated = 1
                    store.extra_data.appendAssumeCapacity(assoc.statements.span.start);
                    store.extra_data.appendAssumeCapacity(assoc.statements.span.len);
                    store.extra_data.appendAssumeCapacity(@backingInt(assoc.scope));
                    store.extra_data.appendAssumeCapacity(assoc.region.start);
                    store.extra_data.appendAssumeCapacity(assoc.region.end);
                } else {
                    store.extra_data.appendAssumeCapacity(0); // has_associated = 0
                }
            } else {
                node.main_token = 0;
            }
        },
        .type_anno => |a| {
            node.tag = .type_anno;
            node.region = a.region;
            node.data.lhs = a.name;
            node.data.rhs = @backingInt(a.anno);
            if (a.where != null or a.is_var) {
                node.main_token = try store.reserveExtraDataToken(3);
                store.extra_data.appendAssumeCapacity(@intFromBool(a.where != null));
                store.extra_data.appendAssumeCapacity(if (a.where) |w| @backingInt(w) else 0);
                store.extra_data.appendAssumeCapacity(@intFromBool(a.is_var));
            } else {
                node.main_token = 0;
            }
        },
        .file_import => |fi| {
            node.tag = .file_import;
            node.region = fi.region;
            node.main_token = fi.path_tok;
            node.data.lhs = fi.name_tok;
            node.data.rhs = fi.type_tok | (if (fi.is_bytes) @as(u32, 1) << 31 else 0);
        },
        .malformed => {
            base.invariant("{s}", .{"Use addMalformed instead"});
        },
    }
    const nid = try store.nodes.append(store.gpa, node);
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

/// Node shapes of the `AST.Pattern` variants stored by `slotNode`, by variant name.
const pattern_shapes = .{
    .ident = .{ .ident_patt, .{ .ident_tok = .main_token } },
    .var_ident = .{ .var_ident_patt, .{ .ident_tok = .main_token } },
    .int = .{ .int_patt, .{ .number_tok = .main_token, .literal = .lhs } },
    .frac = .{ .frac_patt, .{ .number_tok = .main_token, .literal = .lhs } },
    .typed_int = .{ .typed_int_patt, .{ .number_tok = .main_token, .type_suffix = .lhs, .literal = .rhs } },
    .typed_frac = .{ .typed_frac_patt, .{ .number_tok = .main_token, .type_suffix = .lhs, .literal = .rhs } },
    .string = .{ .string_patt, .{ .string_tok = .main_token, .parts = .span } },
    .record = .{ .record_patt, .{ .fields = .span } },
    .list = .{ .list_patt, .{ .patterns = .span } },
    .tuple = .{ .tuple_patt, .{ .patterns = .span } },
    .underscore = .{ .underscore_patt, .{} },
    .alternatives = .{ .alternatives_patt, .{ .patterns = .span } },
    .as = .{ .as_patt, .{ .pattern = .lhs, .name = .main_token } },
    .malformed = .{ .malformed, .{ .reason = .lhs } },
};

/// Adds a pattern node (identifier, literal, record destructure, etc.) and returns its index.
pub fn addPattern(store: *NodeStore, pattern: AST.Pattern) std.mem.Allocator.Error!AST.Pattern.Idx {
    var node = Node{
        .tag = .statement,
        .main_token = 0,
        .data = .{
            .lhs = 0,
            .rhs = 0,
        },
        .region = AST.TokenizedRegion.empty(),
    };
    switch (pattern) {
        inline .ident, .var_ident, .int, .frac, .typed_int, .typed_frac, .string, .record, .list, .tuple, .underscore, .as => |v, variant| node = try store.slotNode(@field(pattern_shapes, @tagName(variant)), v),
        .tag => |t| {
            std.debug.assert(!t.record_shorthand or (t.backing_value and t.args.span.len == 1));
            const data_start = @as(u32, @intCast(store.extra_data.items.len));
            try store.extra_data.append(store.gpa, t.args.span.len);
            try store.extra_data.append(store.gpa, t.qualifiers.span.start);
            try store.extra_data.append(store.gpa, t.qualifiers.span.len);
            // One word of flags: backing value, has args, record shorthand.
            try store.extra_data.append(store.gpa, @as(u32, @intFromBool(t.backing_value)) |
                (@as(u32, @intFromBool(t.has_args)) << 1) |
                (@as(u32, @intFromBool(t.record_shorthand)) << 2));

            node.tag = .tag_patt;
            node.region = t.region;
            node.main_token = t.tag_tok;
            node.data.lhs = t.args.span.start;
            node.data.rhs = data_start;
        },
        .single_quote => |sq| {
            node.tag = .single_quote_patt;
            node.region = sq.region;
            node.main_token = sq.token;
            if (sq.type_suffix) |type_suffix| {
                node.data.lhs = try store.addLiteralTypeSuffix(type_suffix);
                node.data.rhs = @intFromBool(true);
            }
        },
        .list_rest => |r| {
            node.tag = .list_rest_patt;
            node.region = r.region;
            if (r.name) |n| {
                node.data.lhs = n;
            }
        },
        .alternatives => |a| {
            std.debug.assert(a.patterns.span.len > 1);
            node.region = a.region;
            node.tag = .alternatives_patt;
            node.data.lhs = a.patterns.span.start;
            node.data.rhs = a.patterns.span.len;
        },
        .malformed => {
            base.invariant("{s}", .{"Use addMalformed instead"});
        },
    }
    const nid = try store.nodes.append(store.gpa, node);
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

/// Node shapes of the `AST.Expr` variants stored by `slotNode`, by variant name.
const expr_shapes = .{
    .int = .{ .int, .{ .token = .main_token, .literal = .lhs } },
    .frac = .{ .frac, .{ .token = .main_token, .literal = .lhs } },
    .typed_int = .{ .typed_int, .{ .token = .main_token, .type_suffix = .lhs, .literal = .rhs } },
    .typed_frac = .{ .typed_frac, .{ .token = .main_token, .type_suffix = .lhs, .literal = .rhs } },
    .string_part = .{ .string_part, .{ .token = .main_token } },
    .string = .{ .string, .{ .token = .main_token, .parts = .span } },
    .multiline_string = .{ .multiline_string, .{ .token = .main_token, .parts = .span } },
    .typed_string = .{ .typed_string, .{ .token = .main_token, .type_suffix = .lhs, .parts = .rhs_extra_span } },
    .typed_multiline_string = .{ .typed_multiline_string, .{ .token = .main_token, .type_suffix = .lhs, .parts = .rhs_extra_span } },
    .list = .{ .list, .{ .items = .span } },
    .tuple = .{ .tuple, .{ .items = .span } },
    .lambda = .{ .lambda, .{ .args = .span, .body = .main_token } },
    .apply = .{ .apply, .{ .args = .span, .@"fn" = .main_token } },
    .record_updater = .{ .record_update, .{ .token = .main_token } },
    .method_call = .{ .method_call, .{ .receiver = .lhs, .method_token = .main_token, .args = .rhs_extra_span } },
    .tuple_access = .{ .tuple_access, .{ .expr = .lhs, .elem_token = .main_token } },
    .bin_op = .{ .bin_op, .{ .left = .lhs, .right = .rhs, .operator = .main_token } },
    .suffix_single_question = .{ .suffix_single_question, .{ .operator = .main_token, .expr = .lhs } },
    .unary_op = .{ .unary_op, .{ .operator = .main_token, .expr = .lhs } },
    .if_without_else = .{ .if_without_else, .{ .condition = .lhs, .then = .rhs } },
    .match = .{ .match, .{ .expr = .main_token, .branches = .span } },
    .dbg = .{ .dbg, .{ .expr = .lhs } },
    .crash = .{ .crash, .{ .expr = .lhs } },
    .nominal_record = .{ .nominal_record, .{ .mapper = .lhs, .backing = .rhs } },
    .nominal_apply = .{ .nominal_apply, .{ .mapper = .lhs, .args = .rhs_extra_span } },
    .ellipsis = .{ .ellipsis, .{} },
    .@"break" = .{ .break_expr, .{} },
    .@"return" = .{ .return_expr, .{ .expr = .lhs } },
    .block = .{ .block, .{ .statements = .span, .scope = .main_token } },
    .malformed = .{ .malformed, .{ .reason = .lhs } },
};

/// Adds an expression node (literal, variable, call, record, etc.) and returns its index.
pub fn addExpr(store: *NodeStore, expr: AST.Expr) std.mem.Allocator.Error!AST.Expr.Idx {
    var node = Node{
        .tag = .statement,
        .main_token = 0,
        .data = .{ .lhs = 0, .rhs = 0 },
        .region = AST.TokenizedRegion.empty(),
    };
    switch (expr) {
        inline .int, .frac, .typed_int, .typed_frac, .string_part, .string, .multiline_string, .typed_string, .typed_multiline_string, .tuple, .lambda, .apply, .record_updater, .method_call, .tuple_access, .bin_op, .suffix_single_question, .unary_op, .if_without_else, .match, .dbg, .crash, .nominal_record, .nominal_apply, .ellipsis, .@"break", .@"return", .block => |v, variant| node = try store.slotNode(@field(expr_shapes, @tagName(variant)), v),
        .tag => |e| {
            node.tag = .tag;
            node.region = e.region;
            node.main_token = e.token;
            if (e.qualifiers.span.len > 0) {
                node.data.lhs = e.qualifiers.span.start;
                node.data.rhs = e.qualifiers.span.len;
            }
        },
        .single_quote => |e| {
            node.tag = .single_quote;
            node.region = e.region;
            node.main_token = e.token;
            if (e.type_suffix) |type_suffix| {
                node.data.lhs = try store.addLiteralTypeSuffix(type_suffix);
                node.data.rhs = @intFromBool(true);
            }
        },
        .list => |l| {
            node.tag = .list;
            node.region = l.region;
            node.main_token = l.region.start;
            node.data.lhs = l.items.span.start;
            node.data.rhs = l.items.span.len;
        },
        .record => |r| {
            node.tag = .record;
            node.region = r.region;

            // Store all record data in flat format:
            // [fields.span.start, fields.span.len, ext_or_zero]
            const data_start = @as(u32, @intCast(store.extra_data.items.len));
            try store.extra_data.append(store.gpa, r.fields.span.start);
            try store.extra_data.append(store.gpa, r.fields.span.len);

            // Store ext value or 0 for null
            const ext_value = if (r.ext) |ext| @backingInt(ext) else 0;
            try store.extra_data.append(store.gpa, ext_value);

            node.data.lhs = data_start;
            node.data.rhs = 0; // Not used
        },
        .field_access => |fa| {
            node.tag = .field_access;
            node.region = fa.region;
            node.data.lhs = @backingInt(fa.receiver);
            node.data.rhs = fa.segments.span.start;
            node.main_token = fa.segments.span.len;
        },
        .arrow_call => |ld| {
            node.tag = switch (ld.target_kind) {
                .ordinary => .arrow_call,
                .method_call => .arrow_method_call,
            };
            node.region = ld.region;
            node.main_token = ld.operator;
            node.data.lhs = @backingInt(ld.left);
            node.data.rhs = @backingInt(ld.right);
        },
        .if_then_else => |i| {
            node.tag = .if_then_else;
            node.region = i.region;
            node.data.lhs = @backingInt(i.condition);
            node.data.rhs = @as(u32, @intCast(store.extra_data.items.len));
            try store.extra_data.append(store.gpa, @backingInt(i.then));
            try store.extra_data.append(store.gpa, @backingInt(i.@"else"));
        },
        .ident => |id| {
            node.tag = .ident;
            node.region = id.region;
            node.main_token = id.token;
            if (id.qualifiers.span.len > 0) {
                node.data.lhs = id.qualifiers.span.start;
                node.data.rhs = id.qualifiers.span.len;
            }
        },
        .record_builder => |rb| {
            node.tag = .record_builder;
            node.region = rb.region;
            // Store [fields.span.start, fields.span.len, mapper] in extra_data
            const data_start = @as(u32, @intCast(store.extra_data.items.len));
            try store.extra_data.append(store.gpa, rb.fields.span.start);
            try store.extra_data.append(store.gpa, rb.fields.span.len);
            try store.extra_data.append(store.gpa, @backingInt(rb.mapper));
            node.data.lhs = data_start;
        },
        .for_expr => |f| {
            node.tag = switch (f.kind) {
                .iter => .for_expr,
                .stream => .for_bang_expr,
            };
            node.region = f.region;
            node.main_token = @backingInt(f.patt);
            node.data.lhs = @backingInt(f.expr);
            node.data.rhs = @backingInt(f.body);
        },
        .malformed => {
            base.invariant("{s}", .{"Use addMalformed instead"});
        },
    }
    const nid = try store.nodes.append(store.gpa, node);
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

/// TODO
pub fn addPatternRecordField(store: *NodeStore, field: AST.PatternRecordField) std.mem.Allocator.Error!AST.PatternRecordField.Idx {
    var node = Node{
        .tag = .record_field_patt,
        .main_token = field.name orelse 0,
        .data = .{
            .lhs = @intFromBool(field.rest),
            .rhs = 0,
        },
        .region = field.region,
    };
    if (field.value) |value| {
        node.data.rhs = @backingInt(value);
    }
    const nid = try store.nodes.append(store.gpa, node);
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

/// TODO
pub fn getPatternRecordField(store: *const NodeStore, field: AST.PatternRecordField.Idx) AST.PatternRecordField {
    const node = store.nodes.get(@fromBackingInt(@intCast(@backingInt(field))));
    return .{
        .name = if (node.main_token == 0) null else node.main_token,
        .value = if (node.data.rhs == 0) null else @fromBackingInt(@intCast(node.data.rhs)),
        .rest = node.data.lhs == 1,
        .region = node.region,
    };
}

/// TODO
pub fn addRecordField(store: *NodeStore, field: AST.RecordField) std.mem.Allocator.Error!AST.RecordField.Idx {
    var node = Node{
        .tag = .statement,
        .main_token = 0,
        .data = .{
            .lhs = 0,
            .rhs = 0,
        },
        .region = field.region,
    };
    node.tag = .record_field;
    node.main_token = field.name;
    // rhs discriminates the value state: 0 = punned, 1 = unset, 2 = supplied
    // (with lhs holding the expression index).
    switch (field.value) {
        .supplied => |v| {
            node.data.lhs = @backingInt(v);
            node.data.rhs = 2;
        },
        .punned => node.data.rhs = 0,
        .unset => node.data.rhs = 1,
    }

    const nid = try store.nodes.append(store.gpa, node);
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

/// Node shape of an `AST.MatchBranch`, stored by `slotNode`.
const match_branch_shape = .{ .branch, .{ .pattern = .lhs, .body = .rhs, .guard = .main_token } };

/// TODO
pub fn addMatchBranch(store: *NodeStore, branch: AST.MatchBranch) std.mem.Allocator.Error!AST.MatchBranch.Idx {
    return store.addSlotted(AST.MatchBranch.Idx, match_branch_shape, branch);
}

/// Node shape of an `AST.TypeHeader`, stored by `slotNode`.
const type_header_shape = .{ .ty_header, .{ .name = .main_token, .args = .span } };

/// TODO
pub fn addTypeHeader(store: *NodeStore, header: AST.TypeHeader) std.mem.Allocator.Error!AST.TypeHeader.Idx {
    return store.addSlotted(AST.TypeHeader.Idx, type_header_shape, header);
}

/// TODO
pub fn addAnnoRecordField(store: *NodeStore, field: AST.AnnoRecordField) std.mem.Allocator.Error!AST.AnnoRecordField.Idx {
    const node = if (field.default_value) |default_idx| blk: {
        const ed_start = store.extra_data.items.len;
        try store.extra_data.append(store.gpa, @backingInt(field.ty));
        try store.extra_data.append(store.gpa, @backingInt(default_idx));
        break :blk Node{
            .tag = .ty_record_field_defaulted,
            .main_token = try packOptionalToken(field.optional_mark),
            .data = .{
                .lhs = field.name,
                .rhs = @intCast(ed_start),
            },
            .region = field.region,
        };
    } else Node{
        .tag = .ty_record_field,
        .main_token = try packOptionalToken(field.optional_mark),
        .data = .{
            .lhs = field.name,
            .rhs = @backingInt(field.ty),
        },
        .region = field.region,
    };

    const nid = try store.nodes.append(store.gpa, node);
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

/// Node shapes of the `AST.WhereClause` variants stored by `slotNode`, by variant name.
const where_clause_shapes = .{
    .mod_method = .{ .where_mod_method, .{ .var_tok = .main_token, .name_tok = .lhs, .anno = .rhs } },
    .mod_alias = .{ .where_mod_alias, .{ .var_tok = .main_token, .alias = .lhs } },
    .malformed = .{ .malformed, .{ .reason = .lhs } },
};

/// Adds a WhereClause node to the store, returning a type-safe index to the node.
pub fn addWhereClause(store: *NodeStore, clause: AST.WhereClause) std.mem.Allocator.Error!AST.WhereClause.Idx {
    var node = Node{
        .tag = .where_mod_method,
        .main_token = 0,
        .data = .{
            .lhs = 0,
            .rhs = 0,
        },
        .region = AST.TokenizedRegion.empty(),
    };

    switch (clause) {
        inline .mod_method, .mod_alias => |v, variant| node = try store.slotNode(@field(where_clause_shapes, @tagName(variant)), v),
        .malformed => {
            base.invariant("{s}", .{"Use addMalformed instead"});
        },
    }

    const nid = try store.nodes.append(store.gpa, node);
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

/// Appends a span and a type annotation extension to extra_data as
/// [span.start, span.len, ext_data...], returning node data whose `lhs` is
/// that extra-data index and whose `rhs` records the extension kind.
fn addSpanWithExt(store: *NodeStore, span: base.DataSpan, ext: AST.TypeAnno.Ext) std.mem.Allocator.Error!Node.Data {
    const data_start = @as(u32, @intCast(store.extra_data.items.len));
    try store.extra_data.append(store.gpa, span.start);
    try store.extra_data.append(store.gpa, span.len);

    // ext_kind: 0 = closed, 1 = anonymous open, 2 = named open
    const ext_kind: u2 = switch (ext) {
        .closed => 0,
        .open => 1,
        .named => 2,
    };
    switch (ext) {
        .named => |named| {
            try store.extra_data.append(store.gpa, @backingInt(named.anno));
            try store.extra_data.append(store.gpa, named.region.start);
            try store.extra_data.append(store.gpa, named.region.end);
        },
        .open => |double_dot_tok| {
            try store.extra_data.append(store.gpa, double_dot_tok);
        },
        .closed => {},
    }
    return .{ .lhs = data_start, .rhs = @bitCast(AST.TypeAnno.ExtRhs{ .ext_kind = ext_kind }) };
}

/// Reads the span and extension stored by `addSpanWithExt`.
fn spanWithExt(store: *const NodeStore, data: Node.Data) struct { span: base.DataSpan, ext: AST.TypeAnno.Ext } {
    const rhs = @as(AST.TypeAnno.ExtRhs, @bitCast(data.rhs));
    const pos = data.lhs;
    return .{
        .span = .{ .start = store.extra_data.items[pos], .len = store.extra_data.items[pos + 1] },
        .ext = switch (rhs.ext_kind) {
            0 => .closed,
            1 => .{ .open = store.extra_data.items[pos + 2] },
            2 => .{ .named = .{
                .anno = @fromBackingInt(@intCast(store.extra_data.items[pos + 2])),
                .region = .{
                    .start = store.extra_data.items[pos + 3],
                    .end = store.extra_data.items[pos + 4],
                },
            } },
            3 => unreachable, // addSpanWithExt encodes only kinds 0 (closed), 1 (open), and 2 (named)
        },
    };
}

/// Node shapes of the `AST.TypeAnno` variants stored by `slotNode`, by variant name.
const type_anno_shapes = .{
    .apply = .{ .ty_apply, .{ .args = .span } },
    .ty_var = .{ .ty_var, .{ .tok = .main_token } },
    .underscore_type_var = .{ .ty_underscore_var, .{ .tok = .main_token } },
    .underscore = .{ .ty_underscore, .{} },
    .ty = .{ .ty_ty, .{ .token = .main_token, .qualifiers = .span } },
    .tuple = .{ .ty_tuple, .{ .annos = .span } },
    .parens = .{ .ty_parens, .{ .anno = .lhs } },
    .malformed = .{ .malformed, .{ .reason = .lhs } },
};

/// TODO
pub fn addTypeAnno(store: *NodeStore, anno: AST.TypeAnno) std.mem.Allocator.Error!AST.TypeAnno.Idx {
    var node = Node{
        .tag = .branch,
        .main_token = 0,
        .data = .{
            .lhs = 0,
            .rhs = 0,
        },
        .region = AST.TokenizedRegion.empty(),
    };

    switch (anno) {
        inline .apply, .ty_var, .underscore_type_var, .underscore, .ty, .tuple, .parens => |v, variant| node = try store.slotNode(@field(type_anno_shapes, @tagName(variant)), v),
        .tag_union => |tu| {
            node.tag = .ty_union;
            node.region = tu.region;
            node.data = try store.addSpanWithExt(tu.tags.span, tu.ext);
        },
        .record => |r| {
            node.tag = .ty_record;
            node.region = r.region;
            node.data = try store.addSpanWithExt(r.fields.span, r.ext);
        },
        .@"fn" => |f| {
            node.tag = .ty_fn;
            node.region = f.region;
            node.data.lhs = f.args.span.start;
            node.data.rhs = @bitCast(AST.TypeAnno.TypeAnnoFnRhs{
                .effectful = @intFromBool(f.effectful),
                .args_len = @intCast(f.args.span.len), // We hope a function has less than 2.147b args
            });
            const ret_idx = store.extra_data.items.len;
            try store.extra_data.append(store.gpa, @backingInt(f.ret));
            node.main_token = @intCast(ret_idx);
        },
        .malformed => {
            base.invariant("{s}", .{"Use addMalformed instead"});
        },
    }

    const nid = try store.nodes.append(store.gpa, node);
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

/// TODO
pub fn getFile(store: *const NodeStore) AST.File {
    const node = store.nodes.get(root_node_idx);
    const header_ed_idx = @as(usize, @intCast(node.data.lhs + node.data.rhs));
    const header = store.extra_data.items[header_ed_idx];
    return .{
        .header = @fromBackingInt(@intCast(header)),
        .statements = .{ .span = .{ .start = node.data.lhs, .len = node.data.rhs } },
        .scope = @fromBackingInt(@intCast(node.main_token)),
        .region = node.region,
    };
}

/// Retrieves collection data from a stored collection node.
pub fn getCollection(store: *const NodeStore, collection_idx: AST.Collection.Idx) AST.Collection {
    const node = store.nodes.get(@fromBackingInt(@intCast(@backingInt(collection_idx))));
    return .{
        .span = .{
            .start = node.data.lhs,
            .len = node.data.rhs,
        },
        .region = node.region,
        .layout = node.collection_layout,
    };
}

/// Sets the parser-produced layout fact for a collection-owning node.
pub fn setCollectionLayout(store: *NodeStore, idx: anytype, layout: AST.CollectionLayout) void {
    store.nodes.items.items(.collection_layout)[@backingInt(idx)] = layout;
}

/// Returns the parser-produced layout fact for a collection-owning node.
pub fn getCollectionLayout(store: *const NodeStore, idx: anytype) AST.CollectionLayout {
    return store.nodes.items.items(.collection_layout)[@backingInt(idx)];
}

/// Returns the number of nodes in the store.
pub fn nodeCount(store: *const NodeStore) usize {
    return store.nodes.len();
}

/// Returns the source-ordered segments of a record-field access path.
pub fn fieldAccessSegmentSlice(
    store: *const NodeStore,
    span: AST.FieldAccessSegment.Span,
) []const AST.FieldAccessSegment {
    const start: usize = span.span.start;
    const len: usize = span.span.len;
    return store.field_access_segments.items[start..][0..len];
}

/// Adds one segment to the dedicated record-field access segment store.
///
/// This low-level operation is primarily useful to producers that already own
/// the enclosing path span. Parser code should use `addOrExtendFieldAccess` so
/// a failed node allocation rolls the segment append back atomically.
pub fn addFieldAccessSegment(
    store: *NodeStore,
    segment: AST.FieldAccessSegment,
) std.mem.Allocator.Error!u32 {
    const start = std.math.cast(u32, store.field_access_segments.items.len) orelse return error.OutOfMemory;
    // Keep `start + len` representable for every non-empty DataSpan consumer.
    if (start == std.math.maxInt(u32)) return error.OutOfMemory;
    try store.field_access_segments.append(store.gpa, segment);
    return start;
}

/// Appends a source segment to the current contiguous field path, or creates a
/// new path whose receiver is `receiver` when the expression is a path
/// boundary.
///
/// The parser produces field paths at the tail of the segment store and passes
/// only its current, not-yet-owned expression to this function. Extending such
/// a path in place is therefore O(1) amortized and does not copy earlier
/// segments. A non-tail field-access node is already closed and is treated as
/// the receiver of a new path instead of being mutated.
/// Segment storage is rolled back if allocating the enclosing expression node
/// fails.
pub fn addOrExtendFieldAccess(
    store: *NodeStore,
    receiver: AST.Expr.Idx,
    segment: AST.FieldAccessSegment,
    region: AST.TokenizedRegion,
) std.mem.Allocator.Error!AST.Expr.Idx {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(receiver)));
    const node_index = @backingInt(node_idx);

    if (store.nodes.fieldItem(.tag, node_idx) == .field_access) {
        // Extending the open tail updates only the segment count and the end.
        const main_tokens = store.nodes.field(.main_token);
        const regions = store.nodes.field(.region);
        const segment_start: usize = store.nodes.fieldItem(.data, node_idx).rhs;
        const segment_len: usize = main_tokens[node_index];
        const path_is_open_tail = segment_len <= store.field_access_segments.items.len and
            segment_start == store.field_access_segments.items.len - segment_len and
            regions[node_index].start == region.start;

        if (path_is_open_tail) {
            if (main_tokens[node_index] == std.math.maxInt(u32)) return error.OutOfMemory;
            try store.field_access_segments.append(store.gpa, segment);
            main_tokens[node_index] += 1;
            regions[node_index].end = region.end;
            return receiver;
        }
    }

    const previous_segment_len = store.field_access_segments.items.len;
    const segment_start = try store.addFieldAccessSegment(segment);
    errdefer store.field_access_segments.items.len = previous_segment_len;

    return store.addExpr(.{ .field_access = .{
        .receiver = receiver,
        .segments = .{ .span = .{ .start = segment_start, .len = 1 } },
        .region = region,
    } });
}

/// Returns whether a field-access path explicitly contains an optional
/// segment. Every non-field expression is a path boundary and returns false.
pub fn fieldAccessContainsOptional(store: *const NodeStore, expr_idx: AST.Expr.Idx) bool {
    const node = store.nodes.get(@fromBackingInt(@intCast(@backingInt(expr_idx))));
    if (node.tag != .field_access) return false;

    const access = AST.FieldAccessSegment.Span{ .span = .{
        .start = node.data.rhs,
        .len = node.main_token,
    } };
    for (store.fieldAccessSegmentSlice(access)) |segment| {
        if (segment.mode == .optional) return true;
    }
    return false;
}

/// Retrieves header data from a stored header node, reconstructing the appropriate header type.
pub fn getHeader(store: *const NodeStore, header_idx: AST.Header.Idx) AST.Header {
    const node = store.nodes.get(@fromBackingInt(@intCast(@backingInt(header_idx))));
    const tag = narrowNodeTag(HeaderNodeTag, node.tag) orelse
        base.invariant("Expected a valid header tag, got {s}", .{@tagName(node.tag)});
    switch (tag) {
        inline .app_header, .package_header => |t| {
            const ed_start = node.data.rhs;
            const collection: AST.Collection.Idx = @fromBackingInt(@intCast(node.data.lhs));
            const packages: AST.Collection.Idx = @fromBackingInt(@intCast(store.extra_data.items[ed_start]));
            const roc_version = unpackOptionalIndex(AST.RecordField.Idx, store.extra_data.items[ed_start + 1]);
            const platform_idx = unpackOptionalIndex(AST.RecordField.Idx, store.extra_data.items[ed_start + 2]);
            return if (t == .app_header)
                .{ .app = .{ .provides = collection, .platform_idx = platform_idx, .packages = packages, .roc_version = roc_version, .region = node.region } }
            else
                .{ .package = .{ .exposes = collection, .platform_idx = platform_idx, .packages = packages, .roc_version = roc_version, .region = node.region } };
        },
        inline .module_header, .hosted_header, .type_module_header, .default_app_header, .malformed => |t| return store.slotValue(AST.Header, header_shapes, t, node),
        .platform_header => {
            const ed_start = node.data.lhs;
            std.debug.assert(node.data.rhs == 17);

            const symbol_map_layouts = store.extra_data.items[ed_start + 12];
            const targets_val = store.extra_data.items[ed_start + 13];
            const targets = unpackOptionalIndex(AST.TargetsSection.Idx, targets_val);
            const roc_version = unpackOptionalIndex(AST.RecordField.Idx, store.extra_data.items[ed_start + 14]);

            return .{ .platform = .{
                .name = node.main_token,
                .requires_entries = .{ .span = .{
                    .start = store.extra_data.items[ed_start],
                    .len = store.extra_data.items[ed_start + 1],
                }, .region = .{
                    .start = store.extra_data.items[ed_start + 15],
                    .end = store.extra_data.items[ed_start + 16],
                } },
                .exposes = @fromBackingInt(@intCast(store.extra_data.items[ed_start + 2])),
                .packages = @fromBackingInt(@intCast(store.extra_data.items[ed_start + 3])),
                .roc_version = roc_version,
                .provides = .{ .span = .{
                    .start = store.extra_data.items[ed_start + 4],
                    .len = store.extra_data.items[ed_start + 5],
                }, .region = .{
                    .start = store.extra_data.items[ed_start + 6],
                    .end = store.extra_data.items[ed_start + 7],
                }, .layout = @fromBackingInt(@intCast(symbol_map_layouts & 0xff)) },
                .hosted = .{ .span = .{
                    .start = store.extra_data.items[ed_start + 8],
                    .len = store.extra_data.items[ed_start + 9],
                }, .region = .{
                    .start = store.extra_data.items[ed_start + 10],
                    .end = store.extra_data.items[ed_start + 11],
                }, .layout = @fromBackingInt(@intCast((symbol_map_layouts >> 8) & 0xff)) },
                .targets = targets,
                .region = node.region,
            } };
        },
    }
}

/// Retrieves exposed item data from a stored exposed item node.
pub fn getExposedItem(store: *const NodeStore, exposed_item_idx: AST.ExposedItem.Idx) AST.ExposedItem {
    const node = store.nodes.get(@fromBackingInt(@intCast(@backingInt(exposed_item_idx))));
    const tag = narrowNodeTag(ExposedItemNodeTag, node.tag) orelse
        base.invariant("Expected a valid exposed item tag, got {s}", .{@tagName(node.tag)});
    switch (tag) {
        inline .exposed_item_lower, .exposed_item_upper => |t| {
            var qualifiers = base.DataSpan.empty();
            var as: ?Token.Idx = null;
            if (node.data.rhs == 2) {
                const extra_start = node.data.lhs;
                qualifiers = .{ .start = store.extra_data.items[extra_start], .len = store.extra_data.items[extra_start + 1] };
                const as_tok = store.extra_data.items[extra_start + 2];
                as = if (as_tok != 0) as_tok else null;
            } else if (node.data.rhs == 1) {
                as = node.data.lhs;
            }
            return @unionInit(AST.ExposedItem, if (t == .exposed_item_lower) "lower_ident" else "upper_ident", .{
                .region = node.region,
                .ident = node.main_token,
                .qualifiers = .{ .span = qualifiers },
                .as = as,
            });
        },
        .exposed_item_upper_star => {
            return .{ .upper_ident_star = .{
                .region = node.region,
                .ident = node.main_token,
                .qualifiers = .{ .span = .{ .start = node.data.lhs, .len = node.data.rhs } },
            } };
        },
        .malformed => {
            return .{ .malformed = .{
                .reason = @fromBackingInt(@intCast(node.data.lhs)),
                .region = node.region,
            } };
        },
    }
}

/// Retrieves statement data from a stored statement node, reconstructing the appropriate statement type.
pub fn getStatement(store: *const NodeStore, statement_idx: AST.Statement.Idx) AST.Statement {
    const node = store.nodes.get(@fromBackingInt(@intCast(@backingInt(statement_idx))));
    const tag = narrowNodeTag(StatementNodeTag, node.tag) orelse
        base.invariant("Expected a valid statement tag, got {s}", .{@tagName(node.tag)});
    switch (tag) {
        inline .decl, .@"var", .expr, .expect, .crash, .dbg, .@"return", .@"break", .malformed => |t| return store.slotValue(AST.Statement, statement_shapes, t, node),
        .import => {
            const rhs = @as(AST.ImportRhs, @bitCast(node.data.rhs));

            var extra_data_pos = node.data.lhs;
            const exposes_start = store.extra_data.items[extra_data_pos];
            extra_data_pos += 1;
            const exposes_len = store.extra_data.items[extra_data_pos];
            extra_data_pos += 1;
            const target_start_tok = store.extra_data.items[extra_data_pos];
            extra_data_pos += 1;
            const path_start_tok = store.extra_data.items[extra_data_pos];
            extra_data_pos += 1;
            const parent_count: u16 = @intCast(store.extra_data.items[extra_data_pos]);
            extra_data_pos += 1;

            var nested_start_tok: ?Token.Idx = null;
            if (rhs.has_nested == 1) {
                nested_start_tok = store.extra_data.items[extra_data_pos];
                extra_data_pos += 1;
            }
            const nested_len: u16 = @intCast(store.extra_data.items[extra_data_pos]);
            extra_data_pos += 1;

            var qualifier_tok: ?Token.Idx = null;
            var alias_tok: ?Token.Idx = null;
            if (rhs.qualified == 1) {
                qualifier_tok = store.extra_data.items[extra_data_pos];
                extra_data_pos += 1;
            }
            if (rhs.aliased == 1) {
                alias_tok = store.extra_data.items[extra_data_pos];
            }

            return AST.Statement{ .import = .{
                .target = .{
                    .origin = @fromBackingInt(@intCast(rhs.origin)),
                    .base = @fromBackingInt(@intCast(rhs.base)),
                    .parent_count = parent_count,
                    .start_tok = target_start_tok,
                    .path_start_tok = path_start_tok,
                    .module_name_tok = node.main_token,
                    .qualifier_tok = qualifier_tok,
                    .nested_start_tok = nested_start_tok,
                    .nested_len = nested_len,
                },
                .alias_tok = alias_tok,
                .exposes = .{ .span = .{
                    .start = exposes_start,
                    .len = exposes_len,
                } },
                .region = node.region,
            } };
        },
        .@"for", .for_bang => {
            return .{ .@"for" = .{
                .kind = if (tag == .for_bang) .stream else .iter,
                .patt = @fromBackingInt(@intCast(node.main_token)),
                .expr = @fromBackingInt(@intCast(node.data.lhs)),
                .body = @fromBackingInt(@intCast(node.data.rhs)),
                .region = node.region,
            } };
        },
        .@"while" => {
            return .{ .@"while" = .{
                .cond = @fromBackingInt(@intCast(node.data.lhs)),
                .body = @fromBackingInt(@intCast(node.data.rhs)),
                .region = node.region,
            } };
        },
        .type_decl => {
            const extra = store.decodeTypeDeclExtra(node.main_token);

            return .{ .type_decl = .{
                .region = node.region,
                .header = @fromBackingInt(@intCast(node.data.lhs)),
                .anno = @fromBackingInt(@intCast(node.data.rhs)),
                .kind = .alias,
                .where = extra.where,
                .associated = extra.associated,
            } };
        },
        .type_decl_nominal => {
            const extra = store.decodeTypeDeclExtra(node.main_token);

            return .{ .type_decl = .{
                .region = node.region,
                .header = @fromBackingInt(@intCast(node.data.lhs)),
                .anno = @fromBackingInt(@intCast(node.data.rhs)),
                .kind = .nominal,
                .where = extra.where,
                .associated = extra.associated,
            } };
        },
        .type_decl_opaque => {
            const extra = store.decodeTypeDeclExtra(node.main_token);

            return .{ .type_decl = .{
                .region = node.region,
                .header = @fromBackingInt(@intCast(node.data.lhs)),
                .anno = @fromBackingInt(@intCast(node.data.rhs)),
                .kind = .@"opaque",
                .where = extra.where,
                .associated = extra.associated,
            } };
        },
        .type_decl_where_alias => {
            const extra = store.decodeTypeDeclExtra(node.main_token);

            return .{ .type_decl = .{
                .region = node.region,
                .header = @fromBackingInt(@intCast(node.data.lhs)),
                .anno = @fromBackingInt(@intCast(node.data.rhs)),
                .kind = .where_alias,
                .where = extra.where,
                .associated = extra.associated,
            } };
        },
        .type_anno => {
            var where_clause: ?AST.Collection.Idx = null;
            var is_var = false;
            if (node.main_token != 0) {
                const extra_start = extraDataStart(node.main_token);
                if (store.extra_data.items[extra_start] != 0) {
                    where_clause = @fromBackingInt(@intCast(store.extra_data.items[extra_start + 1]));
                }
                is_var = store.extra_data.items[extra_start + 2] != 0;
            }
            return .{ .type_anno = .{
                .region = node.region,
                .name = node.data.lhs,
                .anno = @fromBackingInt(@intCast(node.data.rhs)),
                .where = where_clause,
                .is_var = is_var,
            } };
        },
        .file_import => {
            const is_bytes = (node.data.rhs & (@as(u32, 1) << 31)) != 0;
            const type_tok = node.data.rhs & ~(@as(u32, 1) << 31);
            return .{ .file_import = .{
                .path_tok = node.main_token,
                .name_tok = node.data.lhs,
                .type_tok = type_tok,
                .is_bytes = is_bytes,
                .region = node.region,
            } };
        },
    }
}

/// The identifier token of an identifier pattern, or null for any other
/// pattern, read without rebuilding the pattern.
pub fn patternIdentToken(store: *const NodeStore, pattern_idx: AST.Pattern.Idx) ?Token.Idx {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(pattern_idx)));
    if (store.nodes.fieldItem(.tag, node_idx) != .ident_patt) return null;
    return store.nodes.fieldItem(.main_token, node_idx);
}

/// The argument count of a lambda expression, or null for any other
/// expression, read without rebuilding the expression.
pub fn exprLambdaArity(store: *const NodeStore, expr_idx: AST.Expr.Idx) ?u32 {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(expr_idx)));
    if (store.nodes.fieldItem(.tag, node_idx) != .lambda) return null;
    return store.nodes.fieldItem(.data, node_idx).rhs;
}

/// Retrieves pattern data from a stored pattern node, reconstructing the appropriate pattern type.
pub fn getPattern(store: *const NodeStore, pattern_idx: AST.Pattern.Idx) AST.Pattern {
    const node = store.nodes.get(@fromBackingInt(@intCast(@backingInt(pattern_idx))));
    const tag = narrowNodeTag(PatternNodeTag, node.tag) orelse
        base.invariant("Expected a valid pattern tag, got {s}", .{@tagName(node.tag)});
    switch (tag) {
        inline .ident_patt, .var_ident_patt, .int_patt, .frac_patt, .typed_int_patt, .typed_frac_patt, .string_patt, .record_patt, .list_patt, .tuple_patt, .underscore_patt, .alternatives_patt, .as_patt, .malformed => |t| return store.slotValue(AST.Pattern, pattern_shapes, t, node),
        .tag_patt => {
            const args_start = node.data.lhs;

            const ed_start = @as(usize, @intCast(node.data.rhs));
            const args_len = store.extra_data.items[ed_start];
            const qualifiers_start = store.extra_data.items[ed_start + 1];
            const qualifiers_len = store.extra_data.items[ed_start + 2];
            const flags = store.extra_data.items[ed_start + 3];
            const backing_value = flags & 1 != 0;
            const has_args = flags & 2 != 0;
            const record_shorthand = flags & 4 != 0;

            return .{ .tag = .{
                .tag_tok = node.main_token,
                .args = .{ .span = .{
                    .start = args_start,
                    .len = args_len,
                } },
                .qualifiers = .{ .span = .{
                    .start = qualifiers_start,
                    .len = qualifiers_len,
                } },
                .backing_value = backing_value,
                .has_args = has_args,
                .record_shorthand = record_shorthand,
                .region = node.region,
            } };
        },
        .single_quote_patt => {
            return .{ .single_quote = .{
                .token = node.main_token,
                .type_suffix = if (node.data.rhs != 0) store.getLiteralTypeSuffix(node.data.lhs) else null,
                .region = node.region,
            } };
        },
        .list_rest_patt => {
            return .{ .list_rest = .{
                .region = node.region,
                .name = if (node.data.lhs == 0) null else node.data.lhs,
            } };
        },
    }
}

/// Retrieves expression data from a stored expression node, reconstructing the appropriate expression type.
pub fn getExpr(store: *const NodeStore, expr_idx: AST.Expr.Idx) AST.Expr {
    const node = store.nodes.get(@fromBackingInt(@intCast(@backingInt(expr_idx))));
    const tag = narrowNodeTag(ExprNodeTag, node.tag) orelse
        base.invariant("Expected a valid expr tag, got {s}", .{@tagName(node.tag)});
    switch (tag) {
        inline .int, .frac, .typed_int, .typed_frac, .string_part, .string, .multiline_string, .typed_string, .typed_multiline_string, .list, .tuple, .lambda, .apply, .record_update, .method_call, .tuple_access, .bin_op, .suffix_single_question, .unary_op, .if_without_else, .match, .dbg, .crash, .nominal_record, .nominal_apply, .ellipsis, .break_expr, .return_expr, .block, .malformed => |t| return store.slotValue(AST.Expr, expr_shapes, t, node),
        .single_quote => {
            return .{ .single_quote = .{
                .token = node.main_token,
                .type_suffix = if (node.data.rhs != 0) store.getLiteralTypeSuffix(node.data.lhs) else null,
                .region = node.region,
            } };
        },
        .ident => {
            // Retrieve qualifier span from stored data
            var qualifiers_span = Token.Span{ .span = .{ .start = 0, .len = 0 } };
            if (node.data.rhs > 0) {
                qualifiers_span = .{ .span = .{ .start = node.data.lhs, .len = node.data.rhs } };
            }

            return .{ .ident = .{
                .token = node.main_token,
                .qualifiers = qualifiers_span,
                .region = node.region,
            } };
        },
        .tag => {
            // Retrieve qualifier span from stored data
            var qualifiers_span = Token.Span{ .span = .{ .start = 0, .len = 0 } };
            if (node.data.rhs > 0) {
                qualifiers_span = .{ .span = .{ .start = node.data.lhs, .len = node.data.rhs } };
            }

            return .{ .tag = .{
                .token = node.main_token,
                .qualifiers = qualifiers_span,
                .region = node.region,
            } };
        },
        .record => {
            const extra_data_pos = node.data.lhs;
            const fields_start = store.extra_data.items[extra_data_pos];
            const fields_len = store.extra_data.items[extra_data_pos + 1];
            const ext_value = store.extra_data.items[extra_data_pos + 2];

            // Convert 0 back to null, otherwise create the Idx
            const ext = if (ext_value == 0) null else @as(AST.Expr.Idx, @fromBackingInt(@intCast(ext_value)));

            return .{ .record = .{
                .fields = .{ .span = .{
                    .start = fields_start,
                    .len = fields_len,
                } },
                .ext = ext,
                .region = node.region,
            } };
        },
        .field_access => {
            return .{ .field_access = .{
                .receiver = @fromBackingInt(@intCast(node.data.lhs)),
                .segments = .{ .span = .{
                    .start = node.data.rhs,
                    .len = node.main_token,
                } },
                .region = node.region,
            } };
        },
        .arrow_call => {
            return .{ .arrow_call = .{
                .left = @fromBackingInt(@intCast(node.data.lhs)),
                .right = @fromBackingInt(@intCast(node.data.rhs)),
                .operator = node.main_token,
                .region = node.region,
                .target_kind = .ordinary,
            } };
        },
        .arrow_method_call => {
            return .{ .arrow_call = .{
                .left = @fromBackingInt(@intCast(node.data.lhs)),
                .right = @fromBackingInt(@intCast(node.data.rhs)),
                .operator = node.main_token,
                .region = node.region,
                .target_kind = .method_call,
            } };
        },
        .if_then_else => {
            const then_idx = @as(usize, @intCast(node.data.rhs));
            const else_idx = then_idx + 1;
            const then_ed = store.extra_data.items[then_idx];
            const else_ed = store.extra_data.items[else_idx];
            return .{ .if_then_else = .{
                .region = node.region,
                .condition = @fromBackingInt(@intCast(node.data.lhs)),
                .then = @fromBackingInt(@intCast(then_ed)),
                .@"else" = @fromBackingInt(@intCast(else_ed)),
            } };
        },
        .for_expr, .for_bang_expr => {
            return .{ .for_expr = .{
                .kind = if (tag == .for_bang_expr) .stream else .iter,
                .patt = @fromBackingInt(@intCast(node.main_token)),
                .expr = @fromBackingInt(@intCast(node.data.lhs)),
                .body = @fromBackingInt(@intCast(node.data.rhs)),
                .region = node.region,
            } };
        },
        .record_builder => {
            const extra_data_pos = node.data.lhs;
            const fields_start = store.extra_data.items[extra_data_pos];
            const fields_len = store.extra_data.items[extra_data_pos + 1];
            const mapper_value = store.extra_data.items[extra_data_pos + 2];
            return .{ .record_builder = .{
                .mapper = @fromBackingInt(@intCast(mapper_value)),
                .fields = .{ .span = .{
                    .start = fields_start,
                    .len = fields_len,
                } },
                .region = node.region,
            } };
        },
    }
}

/// Retrieves record field data from a stored record field node.
pub fn getRecordField(store: *const NodeStore, field_idx: AST.RecordField.Idx) AST.RecordField {
    const node = store.nodes.get(@fromBackingInt(@intCast(@backingInt(field_idx))));
    const name = node.main_token;
    const value: AST.RecordField.Value = if (node.tag == .malformed)
        .punned
    else switch (node.data.rhs) {
        0 => .punned,
        1 => .unset,
        else => .{ .supplied = @fromBackingInt(@intCast(node.data.lhs)) },
    };

    return .{
        .name = name,
        .value = value,
        .region = node.region,
    };
}

/// The token holding the whole text of a string literal that is made of
/// exactly one literal part, i.e. a plain string with no interpolation.
///
/// Returns null for any other expression, and for strings whose text is split
/// across several parts (interpolation, escapes, line continuations)—callers
/// use this to read short literals like a pinned compiler version straight out
/// of the source, which is only sound when the source says it verbatim.
pub fn singleStringPartToken(store: *const NodeStore, expr_idx: AST.Expr.Idx) ?Token.Idx {
    const expr = store.getExpr(expr_idx);
    if (expr != .string) return null;
    const string = expr.string;
    const parts = store.exprSlice(string.parts);
    if (parts.len != 1) return null;
    const part = store.getExpr(parts[0]);
    if (part != .string_part) return null;
    return part.string_part.token;
}

/// Retrieves match branch data from a stored match branch node.
pub fn getBranch(store: *const NodeStore, branch_idx: AST.MatchBranch.Idx) AST.MatchBranch {
    return store.getSlotted(AST.MatchBranch, match_branch_shape, branch_idx);
}

/// Retrieves type header data from a stored type header node.
pub fn getTypeHeader(store: *const NodeStore, header_idx: AST.TypeHeader.Idx) error{MalformedNode}!AST.TypeHeader {
    const node = store.nodes.get(@fromBackingInt(@intCast(@backingInt(header_idx))));
    if (node.tag != .ty_header) return error.MalformedNode;
    return store.slotStruct(AST.TypeHeader, type_header_shape[1], node);
}

/// Retrieves annotation record field data from a stored annotation record field node.
pub fn getAnnoRecordField(store: *const NodeStore, anno_record_field_idx: AST.AnnoRecordField.Idx) error{MalformedNode}!AST.AnnoRecordField {
    const node = store.nodes.get(@fromBackingInt(@intCast(@backingInt(anno_record_field_idx))));

    if (node.tag == .ty_record_field) {
        return .{
            .region = node.region,
            .name = node.data.lhs,
            .optional_mark = unpackOptionalToken(node.main_token),
            .ty = @fromBackingInt(@intCast(node.data.rhs)),
        };
    }
    if (node.tag == .ty_record_field_defaulted) {
        const ed_start = @as(usize, @intCast(node.data.rhs));
        return .{
            .region = node.region,
            .name = node.data.lhs,
            .optional_mark = unpackOptionalToken(node.main_token),
            .ty = @fromBackingInt(@intCast(store.extra_data.items[ed_start])),
            .default_value = @fromBackingInt(@intCast(store.extra_data.items[ed_start + 1])),
        };
    }
    return error.MalformedNode;
}

/// Returns the source region for a stored type annotation without reconstructing the full AST union.
pub fn typeAnnoRegion(store: *const NodeStore, ty_anno_idx: AST.TypeAnno.Idx) AST.TokenizedRegion {
    const node = store.nodes.get(@fromBackingInt(@intCast(@backingInt(ty_anno_idx))));
    return node.region;
}

/// Returns whether a stored type annotation is malformed without reconstructing the full AST union.
pub fn typeAnnoIsMalformed(store: *const NodeStore, ty_anno_idx: AST.TypeAnno.Idx) bool {
    const node = store.nodes.get(@fromBackingInt(@intCast(@backingInt(ty_anno_idx))));
    return node.tag == .malformed;
}

/// Get a WhereClause node from the store, using a type-safe index to the node.
pub fn getWhereClause(store: *const NodeStore, where_clause_idx: AST.WhereClause.Idx) AST.WhereClause {
    const node = store.nodes.get(@fromBackingInt(@intCast(@backingInt(where_clause_idx))));
    const tag = narrowNodeTag(WhereClauseNodeTag, node.tag) orelse
        base.invariant("Expected a valid where clause node, found {s}", .{@tagName(node.tag)});
    switch (tag) {
        inline .where_mod_method, .where_mod_alias, .malformed => |t| return store.slotValue(AST.WhereClause, where_clause_shapes, t, node),
    }
}

/// Retrieves type annotation data from a stored type annotation node, reconstructing the appropriate annotation type.
pub fn getTypeAnno(store: *const NodeStore, ty_anno_idx: AST.TypeAnno.Idx) AST.TypeAnno {
    const node = store.nodes.get(@fromBackingInt(@intCast(@backingInt(ty_anno_idx))));
    const tag = narrowNodeTag(TypeAnnoNodeTag, node.tag) orelse return .{ .malformed = .{
        .reason = .ty_anno_unexpected_token,
        .region = node.region,
    } };

    switch (tag) {
        inline .ty_apply, .ty_var, .ty_underscore_var, .ty_underscore, .ty_ty, .ty_tuple, .ty_parens, .malformed => |t| return store.slotValue(AST.TypeAnno, type_anno_shapes, t, node),
        .ty_union => {
            const span_with_ext = store.spanWithExt(node.data);
            return .{ .tag_union = .{
                .region = node.region,
                .ext = span_with_ext.ext,
                .tags = .{ .span = span_with_ext.span },
            } };
        },
        .ty_record => {
            const span_with_ext = store.spanWithExt(node.data);
            return .{ .record = .{
                .region = node.region,
                .fields = .{ .span = span_with_ext.span },
                .ext = span_with_ext.ext,
            } };
        },
        .ty_fn => {
            const rhs = @as(AST.TypeAnno.TypeAnnoFnRhs, @bitCast(node.data.rhs));
            return .{ .@"fn" = .{
                .region = node.region,
                .ret = @fromBackingInt(@intCast(store.extra_data.items[@as(usize, @intCast(node.main_token))])),
                .args = .{ .span = .{
                    .start = node.data.lhs,
                    .len = @intCast(rhs.args_len),
                } },
                .effectful = rhs.effectful == 1,
            } };
        },
    }
}

/// Returns a new ExprIter so that the caller can iterate through
/// all items in the span.
pub fn exprSlice(store: *const NodeStore, span: AST.Expr.Span) []AST.Expr.Idx {
    return @ptrCast(store.extra_data.items[span.span.start..(span.span.start + span.span.len)]);
}

/// Returns a new Statement slice so that the caller can iterate through
/// all items in the span.
pub fn statementSlice(store: *const NodeStore, span: AST.Statement.Span) []AST.Statement.Idx {
    return store.sliceFromSpan(AST.Statement.Idx, span.span);
}

/// Appends a pattern string part and returns its id.
pub fn addPatternStringPart(store: *NodeStore, part: AST.PatternStringPart) std.mem.Allocator.Error!AST.PatternStringPart.Idx {
    const id: AST.PatternStringPart.Idx = @fromBackingInt(@intCast(@as(u32, @intCast(store.pattern_string_parts.items.len))));
    try store.pattern_string_parts.append(store.gpa, part);
    return id;
}

/// Returns a pattern string part by id.
pub fn getPatternStringPart(store: *const NodeStore, idx: AST.PatternStringPart.Idx) AST.PatternStringPart {
    return store.pattern_string_parts.items[@backingInt(idx)];
}

/// Returns pattern string part ids for a span.
pub fn patternStringPartSlice(store: *const NodeStore, span: AST.PatternStringPart.Span) []AST.PatternStringPart.Idx {
    return store.sliceFromSpan(AST.PatternStringPart.Idx, span.span);
}

/// Returns the scratch buffer that collects `Kind.Idx` values.
pub fn scratchFor(store: *NodeStore, comptime Kind: type) *base.Scratch(Kind.Idx) {
    return &store.scratch[comptime scratchIndex(Kind)];
}

/// Returns the start position for a new span of `Kind.Idx` values in scratch.
pub fn scratchTop(store: *NodeStore, comptime Kind: type) u32 {
    return store.scratchFor(Kind).top();
}

/// Places a new `Kind.Idx` in scratch.
pub fn addScratch(store: *NodeStore, comptime Kind: type, idx: Kind.Idx) std.mem.Allocator.Error!void {
    try store.scratchFor(Kind).append(idx);
}

/// Clears any `Kind.Idx` values added to scratch from `start` until the end.
/// Should be used wherever the scratch items will not be used,
/// as in when parsing fails.
pub fn clearScratchFrom(store: *NodeStore, comptime Kind: type, start: u32) void {
    store.scratchFor(Kind).clearFrom(start);
}

/// Creates a new span of the `Kind.Idx` values added to scratch since
/// `start`, moving them from scratch to extra_data.
pub fn spanFrom(store: *NodeStore, comptime Kind: type, start: u32) std.mem.Allocator.Error!Kind.Span {
    return .{ .span = try store.dataSpanFrom(Kind, start) };
}

/// Moves the `Kind` scratch items from `start` to the top of scratch into
/// extra_data and returns the span they occupy there.
fn dataSpanFrom(store: *NodeStore, comptime Kind: type, start: u32) std.mem.Allocator.Error!base.DataSpan {
    const scratch = store.scratchFor(Kind);
    const end = scratch.top();
    defer scratch.clearFrom(start);
    std.debug.assert(end >= start);
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    const items = scratch.items.items[start..end];
    try store.extra_data.ensureUnusedCapacity(store.gpa, items.len);
    for (items) |item| {
        store.extra_data.appendAssumeCapacity(if (Kind.Idx == u32) item else @backingInt(item));
    }
    return .{ .start = ed_start, .len = end - start };
}

/// Creates a slice corresponding to a span.
pub fn sliceFromSpan(store: *const NodeStore, comptime T: type, span: base.DataSpan) []T {
    if (span.len == 0) return &.{};
    return @ptrCast(store.extra_data.items[span.start..][0..span.len]);
}

/// Returns a new Pattern slice so that the caller can iterate through
/// all items in the span.
pub fn patternSlice(store: *const NodeStore, span: AST.Pattern.Span) []AST.Pattern.Idx {
    return store.sliceFromSpan(AST.Pattern.Idx, span.span);
}

/// Returns a new AST.PatternRecordFieldIter so that the caller can iterate through
/// all items in the span.
pub fn patternRecordFieldSlice(store: *const NodeStore, span: AST.PatternRecordField.Span) []AST.PatternRecordField.Idx {
    return store.sliceFromSpan(AST.PatternRecordField.Idx, span.span);
}
/// Returns a new RecordField slice so that the caller can iterate through
/// all items in the span.
pub fn recordFieldSlice(store: *const NodeStore, span: AST.RecordField.Span) []AST.RecordField.Idx {
    return sliceFromSpan(store, AST.RecordField.Idx, span.span);
}
/// Returns a new WhenBranch slice so that the caller can iterate through
/// all items in the span.
pub fn matchBranchSlice(store: *const NodeStore, span: AST.MatchBranch.Span) []AST.MatchBranch.Idx {
    return store.sliceFromSpan(AST.MatchBranch.Idx, span.span);
}

/// Returns a new TypeAnno slice so that the caller can iterate through
/// all items in the span.
pub fn typeAnnoSlice(store: *const NodeStore, span: AST.TypeAnno.Span) []AST.TypeAnno.Idx {
    return store.sliceFromSpan(AST.TypeAnno.Idx, span.span);
}

/// Returns a new AnnoRecordField slice so that the caller can iterate through
/// all items in the span.
pub fn annoRecordFieldSlice(store: *const NodeStore, span: AST.AnnoRecordField.Span) []AST.AnnoRecordField.Idx {
    return store.sliceFromSpan(AST.AnnoRecordField.Idx, span.span);
}

const LiteralTypeSuffixKind = enum(u32) { path, deprecated_builtin };

/// Stores a literal type suffix in extra data, returning its extra-data index.
fn addLiteralTypeSuffix(store: *NodeStore, suffix: AST.LiteralTypeSuffix) std.mem.Allocator.Error!u32 {
    const extra_idx: u32 = @intCast(store.extra_data.items.len);
    switch (suffix) {
        .path => |path| try store.extra_data.appendSlice(store.gpa, &.{
            @backingInt(LiteralTypeSuffixKind.path),
            path.final_token,
            path.qualifiers.span.start,
            path.qualifiers.span.len,
        }),
        .deprecated_builtin => |type_name| try store.extra_data.appendSlice(store.gpa, &.{
            @backingInt(LiteralTypeSuffixKind.deprecated_builtin),
            @bitCast(type_name),
        }),
    }
    return extra_idx;
}

/// Retrieves a literal type suffix stored by `addLiteralTypeSuffix`.
pub fn getLiteralTypeSuffix(store: *const NodeStore, extra_idx: u32) AST.LiteralTypeSuffix {
    const data = store.extra_data.items[extra_idx..];
    return switch (@as(LiteralTypeSuffixKind, @fromBackingInt(@intCast(data[0])))) {
        .path => .{ .path = .{
            .final_token = data[1],
            .qualifiers = .{ .span = .{ .start = data[2], .len = data[3] } },
        } },
        .deprecated_builtin => .{ .deprecated_builtin = @bitCast(data[1]) },
    };
}

/// Returns a new Token slice so that the caller can iterate through
/// all items in the span.
pub fn tokenSlice(store: *const NodeStore, span: Token.Span) []Token.Idx {
    return store.sliceFromSpan(Token.Idx, span.span);
}

/// Returns a new ExposedItem slice so that the caller can iterate through
/// all items in the span.
pub fn exposedItemSlice(store: *const NodeStore, span: AST.ExposedItem.Span) []AST.ExposedItem.Idx {
    return store.sliceFromSpan(AST.ExposedItem.Idx, span.span);
}

/// Returns a new WhereClause slice so that the caller can iterate through
/// all items in the span.
pub fn whereClauseSlice(store: *const NodeStore, span: AST.WhereClause.Span) []AST.WhereClause.Idx {
    return store.sliceFromSpan(AST.WhereClause.Idx, span.span);
}

/// Adds a TargetsSection node and returns its index.
pub fn addTargetsSection(store: *NodeStore, section: AST.TargetsSection) std.mem.Allocator.Error!AST.TargetsSection.Idx {
    const node = Node{
        .tag = .targets_section,
        .main_token = section.inputs_dir orelse 0,
        .data = .{
            .lhs = section.entries.span.start,
            .rhs = section.entries.span.len,
        },
        .region = section.region,
    };
    const nid = try store.nodes.append(store.gpa, node);
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

/// Node shape of an `AST.SymbolMapEntry`, stored by `slotNode`.
const symbol_map_entry_shape = .{ .symbol_map_entry, .{ .symbol = .main_token, .module = .lhs, .func = .rhs } };

/// Adds a SymbolMapEntry node and returns its index.
pub fn addSymbolMapEntry(store: *NodeStore, entry: AST.SymbolMapEntry) std.mem.Allocator.Error!AST.SymbolMapEntry.Idx {
    return store.addSlotted(AST.SymbolMapEntry.Idx, symbol_map_entry_shape, entry);
}

/// Node shape of an `AST.TargetEntry`, stored by `slotNode`.
const target_entry_shape = .{ .target_entry, .{ .target = .main_token, .config = .lhs } };

/// Adds a TargetEntry node and returns its index.
pub fn addTargetEntry(store: *NodeStore, entry: AST.TargetEntry) std.mem.Allocator.Error!AST.TargetEntry.Idx {
    return store.addSlotted(AST.TargetEntry.Idx, target_entry_shape, entry);
}

/// Adds a TargetFile node and returns its index.
pub fn addTargetFile(store: *NodeStore, file: AST.TargetFile, region: AST.TokenizedRegion) std.mem.Allocator.Error!AST.TargetFile.Idx {
    var node = Node{
        .tag = .malformed,
        .main_token = 0,
        .data = .{ .lhs = 0, .rhs = 0 },
        .region = region,
    };

    switch (file) {
        .string_literal => |maybe_tok| {
            node.tag = .target_file_string;
            node.data.lhs = @intFromBool(maybe_tok != null);
            if (maybe_tok) |tok| node.main_token = tok;
        },
        .special_ident => |tok| {
            node.tag = .target_file_ident;
            node.main_token = tok;
        },
        .malformed => |m| {
            node.tag = .malformed;
            node.data.lhs = @backingInt(m.reason);
            node.region = m.region;
        },
    }

    const nid = try store.nodes.append(store.gpa, node);
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

/// Node shape of an `AST.TargetConfig`, stored by `slotNode`.
const target_config_shape = .{ .target_config, .{ .entries = .span } };

/// Adds a TargetConfig node and returns its index.
pub fn addTargetConfig(store: *NodeStore, config: AST.TargetConfig) std.mem.Allocator.Error!AST.TargetConfig.Idx {
    return store.addSlotted(AST.TargetConfig.Idx, target_config_shape, config);
}

/// Node shape of an `AST.TargetConfigEntry`, stored by `slotNode`.
const target_config_entry_shape = .{ .target_config_entry, .{ .name = .main_token, .value = .lhs } };

/// Adds a TargetConfigEntry node and returns its index.
pub fn addTargetConfigEntry(store: *NodeStore, entry: AST.TargetConfigEntry) std.mem.Allocator.Error!AST.TargetConfigEntry.Idx {
    return store.addSlotted(AST.TargetConfigEntry.Idx, target_config_entry_shape, entry);
}

/// Adds a TargetConfigValue node and returns its index.
pub fn addTargetConfigValue(store: *NodeStore, value: AST.TargetConfigValue, region: AST.TokenizedRegion) std.mem.Allocator.Error!AST.TargetConfigValue.Idx {
    var node = Node{
        .tag = .target_config_value,
        .main_token = 0,
        .data = .{ .lhs = 0, .rhs = 0 },
        .region = region,
    };

    switch (value) {
        .int_literal => |tok| {
            node.main_token = tok;
            node.data.lhs = @backingInt(TargetConfigValueNodeTag.int_literal);
        },
        .string_literal => |maybe_tok| {
            node.data.rhs = @intFromBool(maybe_tok != null);
            if (maybe_tok) |tok| node.main_token = tok;
            node.data.lhs = @backingInt(TargetConfigValueNodeTag.string_literal);
        },
        .tag_literal => |tok| {
            node.main_token = tok;
            node.data.lhs = @backingInt(TargetConfigValueNodeTag.tag_literal);
        },
        .ident => |tok| {
            node.main_token = tok;
            node.data.lhs = @backingInt(TargetConfigValueNodeTag.ident);
        },
        .list => |span| {
            const extra_token = try store.reserveExtraDataToken(2);
            store.extra_data.appendAssumeCapacity(span.span.start);
            store.extra_data.appendAssumeCapacity(span.span.len);
            node.data.lhs = @backingInt(TargetConfigValueNodeTag.list);
            node.data.rhs = extra_token;
        },
        .files => |span| {
            const extra_token = try store.reserveExtraDataToken(2);
            store.extra_data.appendAssumeCapacity(span.span.start);
            store.extra_data.appendAssumeCapacity(span.span.len);
            node.data.lhs = @backingInt(TargetConfigValueNodeTag.files);
            node.data.rhs = extra_token;
        },
        .malformed => |m| {
            node.data.lhs = @backingInt(TargetConfigValueNodeTag.malformed);
            node.data.rhs = @backingInt(m.reason);
            node.region = m.region;
        },
    }

    const nid = try store.nodes.append(store.gpa, node);
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

/// Returns a new TargetEntry slice for iteration.
pub fn targetEntrySlice(store: *const NodeStore, span: AST.TargetEntry.Span) []AST.TargetEntry.Idx {
    return store.sliceFromSpan(AST.TargetEntry.Idx, span.span);
}

/// Returns a new TargetFile slice for iteration.
pub fn targetFileSlice(store: *const NodeStore, span: AST.TargetFile.Span) []AST.TargetFile.Idx {
    return store.sliceFromSpan(AST.TargetFile.Idx, span.span);
}

/// Returns a new TargetConfigEntry slice for iteration.
pub fn targetConfigEntrySlice(store: *const NodeStore, span: AST.TargetConfigEntry.Span) []AST.TargetConfigEntry.Idx {
    return store.sliceFromSpan(AST.TargetConfigEntry.Idx, span.span);
}

/// Creates a SymbolMapEntry span from scratch entries added since start.
pub fn symbolMapEntrySpanFrom(store: *NodeStore, start: u32, region: AST.TokenizedRegion, layout: AST.CollectionLayout) std.mem.Allocator.Error!AST.SymbolMapEntry.Span {
    return .{ .span = try store.dataSpanFrom(AST.SymbolMapEntry, start), .region = region, .layout = layout };
}

/// Returns a SymbolMapEntry slice for iteration over a span.
pub fn symbolMapEntrySlice(store: *const NodeStore, span: AST.SymbolMapEntry.Span) []AST.SymbolMapEntry.Idx {
    return store.sliceFromSpan(AST.SymbolMapEntry.Idx, span.span);
}

/// Returns a new TargetConfigValue slice for iteration.
pub fn targetConfigValueSlice(store: *const NodeStore, span: AST.TargetConfigValue.Span) []AST.TargetConfigValue.Idx {
    return store.sliceFromSpan(AST.TargetConfigValue.Idx, span.span);
}

/// Retrieves a TargetsSection from a stored node.
pub fn getTargetsSection(store: *const NodeStore, idx: AST.TargetsSection.Idx) AST.TargetsSection {
    const node = store.nodes.get(@fromBackingInt(@intCast(@backingInt(idx))));
    std.debug.assert(node.tag == .targets_section);

    const inputs_dir: ?Token.Idx = if (node.main_token == 0) null else node.main_token;

    return .{
        .inputs_dir = inputs_dir,
        .entries = .{ .span = .{ .start = node.data.lhs, .len = node.data.rhs } },
        .region = node.region,
    };
}

/// Retrieves a SymbolMapEntry from a stored node.
pub fn getSymbolMapEntry(store: *const NodeStore, idx: AST.SymbolMapEntry.Idx) AST.SymbolMapEntry {
    return store.getSlotted(AST.SymbolMapEntry, symbol_map_entry_shape, idx);
}

/// Retrieves a TargetEntry from a stored node.
pub fn getTargetEntry(store: *const NodeStore, idx: AST.TargetEntry.Idx) AST.TargetEntry {
    return store.getSlotted(AST.TargetEntry, target_entry_shape, idx);
}

/// Retrieves a TargetFile from a stored node.
pub fn getTargetFile(store: *const NodeStore, idx: AST.TargetFile.Idx) AST.TargetFile {
    const node = store.nodes.get(@fromBackingInt(@intCast(@backingInt(idx))));
    const tag = narrowNodeTag(TargetFileNodeTag, node.tag) orelse
        base.invariant("Expected a valid target_file tag, got {s}", .{@tagName(node.tag)});

    switch (tag) {
        .target_file_string => {
            return .{ .string_literal = if (node.data.lhs != 0) node.main_token else null };
        },
        .target_file_ident => {
            return .{ .special_ident = node.main_token };
        },
        .malformed => {
            return .{ .malformed = .{
                .reason = @fromBackingInt(@intCast(node.data.lhs)),
                .region = node.region,
            } };
        },
    }
}

/// Retrieves a TargetConfig from a stored node.
pub fn getTargetConfig(store: *const NodeStore, idx: AST.TargetConfig.Idx) AST.TargetConfig {
    return store.getSlotted(AST.TargetConfig, target_config_shape, idx);
}

/// Retrieves a TargetConfigEntry from a stored node.
pub fn getTargetConfigEntry(store: *const NodeStore, idx: AST.TargetConfigEntry.Idx) AST.TargetConfigEntry {
    return store.getSlotted(AST.TargetConfigEntry, target_config_entry_shape, idx);
}

/// Retrieves a TargetConfigValue from a stored node.
pub fn getTargetConfigValue(store: *const NodeStore, idx: AST.TargetConfigValue.Idx) AST.TargetConfigValue {
    const node = store.nodes.get(@fromBackingInt(@intCast(@backingInt(idx))));
    std.debug.assert(node.tag == .target_config_value);

    const tag: TargetConfigValueNodeTag = @fromBackingInt(@intCast(node.data.lhs));
    return switch (tag) {
        .int_literal => .{ .int_literal = node.main_token },
        .string_literal => .{ .string_literal = if (node.data.rhs != 0) node.main_token else null },
        .tag_literal => .{ .tag_literal = node.main_token },
        .ident => .{ .ident = node.main_token },
        .list => blk: {
            const extra_start = extraDataStart(node.data.rhs);
            break :blk .{ .list = .{ .span = .{
                .start = store.extra_data.items[extra_start],
                .len = store.extra_data.items[extra_start + 1],
            } } };
        },
        .files => blk: {
            const extra_start = extraDataStart(node.data.rhs);
            break :blk .{ .files = .{ .span = .{
                .start = store.extra_data.items[extra_start],
                .len = store.extra_data.items[extra_start + 1],
            } } };
        },
        .malformed => .{ .malformed = .{
            .reason = @fromBackingInt(@intCast(node.data.rhs)),
            .region = node.region,
        } },
    };
}

/// Node shape of an `AST.ForClauseTypeAlias`, stored by `slotNode`.
const for_clause_type_alias_shape = .{ .for_clause_type_alias, .{ .alias_name = .main_token, .rigid_name = .lhs } };

/// Adds a ForClauseTypeAlias node and returns its index.
pub fn addForClauseTypeAlias(store: *NodeStore, alias: AST.ForClauseTypeAlias) std.mem.Allocator.Error!AST.ForClauseTypeAlias.Idx {
    return store.addSlotted(AST.ForClauseTypeAlias.Idx, for_clause_type_alias_shape, alias);
}

/// Returns a new ForClauseTypeAlias slice for iteration.
pub fn forClauseTypeAliasSlice(store: *const NodeStore, span: AST.ForClauseTypeAlias.Span) []AST.ForClauseTypeAlias.Idx {
    return store.sliceFromSpan(AST.ForClauseTypeAlias.Idx, span.span);
}

/// Retrieves a ForClauseTypeAlias from a stored node.
pub fn getForClauseTypeAlias(store: *const NodeStore, idx: AST.ForClauseTypeAlias.Idx) AST.ForClauseTypeAlias {
    return store.getSlotted(AST.ForClauseTypeAlias, for_clause_type_alias_shape, idx);
}

/// Adds a RequiresEntry node and returns its index.
pub fn addRequiresEntry(store: *NodeStore, entry: AST.RequiresEntry) std.mem.Allocator.Error!AST.RequiresEntry.Idx {
    // Pack type_aliases len and type_anno idx into rhs
    const rhs_packed: u32 = (@as(u32, entry.type_aliases.span.len) << 16) | @as(u32, @backingInt(entry.type_anno));
    const node = Node{
        .tag = .requires_entry,
        .main_token = entry.entrypoint_name,
        .data = .{
            .lhs = entry.type_aliases.span.start,
            .rhs = rhs_packed,
        },
        .region = entry.region,
    };
    const nid = try store.nodes.append(store.gpa, node);
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

/// Returns a new RequiresEntry slice for iteration.
pub fn requiresEntrySlice(store: *const NodeStore, span: AST.RequiresEntry.Span) []AST.RequiresEntry.Idx {
    return store.sliceFromSpan(AST.RequiresEntry.Idx, span.span);
}

/// Retrieves a RequiresEntry from a stored node.
pub fn getRequiresEntry(store: *const NodeStore, idx: AST.RequiresEntry.Idx) AST.RequiresEntry {
    const node = store.nodes.get(@fromBackingInt(@intCast(@backingInt(idx))));
    std.debug.assert(node.tag == .requires_entry);

    // Unpack type_aliases len and type_anno idx from rhs
    const type_aliases_len: u32 = node.data.rhs >> 16;
    const type_anno_idx: u16 = @truncate(node.data.rhs);

    return .{
        .type_aliases = .{ .span = .{ .start = node.data.lhs, .len = type_aliases_len } },
        .entrypoint_name = node.main_token,
        .type_anno = @fromBackingInt(@intCast(type_anno_idx)),
        .region = node.region,
    };
}

/// Record returns whose value flows directly to the enclosing function result.
/// Only function bodies and their final blocks/conditional branches propagate
/// this context. Operands, bindings, conditions and loop bodies do not.
pub fn collectRedundantReturns(store: *const NodeStore) Allocator.Error!std.DynamicBitSetUnmanaged {
    var returns = try std.DynamicBitSetUnmanaged.initEmpty(store.gpa, store.nodes.len());
    errdefer returns.deinit(store.gpa);
    var pending: std.ArrayList(AST.Expr.Idx) = .empty;
    defer pending.deinit(store.gpa);

    // A lambda node keeps its body's index in `main_token`.
    for (store.nodes.items.items(.tag), store.nodes.items.items(.main_token)) |tag, body| {
        if (tag == .lambda) try pending.append(store.gpa, @fromBackingInt(body));
    }
    while (pending.pop()) |idx| {
        switch (store.getExpr(idx)) {
            .block => |block| {
                const statements = store.statementSlice(block.statements);
                if (statements.len == 0) continue;
                const last = statements[statements.len - 1];
                switch (store.getStatement(last)) {
                    .expr => |expr| try pending.append(store.gpa, expr.expr),
                    .@"return" => |ret| {
                        returns.set(@backingInt(last));
                        try pending.append(store.gpa, ret.expr);
                    },
                    .decl,
                    .@"var",
                    .crash,
                    .dbg,
                    .expect,
                    .@"for",
                    .@"while",
                    .@"break",
                    .import,
                    .file_import,
                    .type_decl,
                    .type_anno,
                    .malformed,
                    => {},
                }
            },
            .if_then_else => |cond| {
                try pending.append(store.gpa, cond.then);
                try pending.append(store.gpa, cond.@"else");
            },
            .tuple => |tuple| {
                const items = store.exprSlice(tuple.items);
                if (items.len == 1 and store.getCollectionLayout(idx) == .compact) {
                    try pending.append(store.gpa, items[0]);
                }
            },
            .match => |match| {
                for (store.matchBranchSlice(match.branches)) |branch| {
                    try pending.append(store.gpa, store.getBranch(branch).body);
                }
            },
            .@"return" => |ret| {
                returns.set(@backingInt(idx));
                try pending.append(store.gpa, ret.expr);
            },
            .int,
            .frac,
            .typed_int,
            .typed_frac,
            .single_quote,
            .string_part,
            .string,
            .multiline_string,
            .typed_string,
            .typed_multiline_string,
            .list,
            .record,
            .tag,
            .lambda,
            .apply,
            .record_updater,
            .field_access,
            .method_call,
            .tuple_access,
            .arrow_call,
            .bin_op,
            .suffix_single_question,
            .unary_op,
            .if_without_else,
            .ident,
            .dbg,
            .crash,
            .record_builder,
            .nominal_record,
            .nominal_apply,
            .ellipsis,
            .@"break",
            .for_expr,
            .malformed,
            => {},
        }
    }
    return returns;
}
