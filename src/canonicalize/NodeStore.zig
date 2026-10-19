//! Stores AST nodes and provides scratch arrays for working with nodes.

const std = @import("std");
const base = @import("base");
const types = @import("types");
const builtins = @import("builtins");
const collections = @import("collections");

const Diagnostic = @import("Diagnostic.zig");
const Node = @import("Node.zig");
const CIR = @import("CIR.zig");

const Allocator = std.mem.Allocator;
const CompactWriter = collections.CompactWriter;
const SafeList = collections.SafeList;
const RocDec = builtins.dec.RocDec;
const DataSpan = base.DataSpan;
const Region = base.Region;
const Ident = base.Ident;
const NodeStore = @This();

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

/// A CIR variant whose node payload is its fields copied word for word by
/// `encodePayload` and `decodePayload`: the variant's name, the tag its node
/// is stored under, and the `Node.Payload` field laid over the node's bytes
/// (null for a variant with no fields, whose payload stays zeroed).
const CopiedShape = struct {
    cir: []const u8,
    tag: Node.Tag,
    payload: ?[]const u8,
};

fn copiedShapeForCir(comptime shapes: []const CopiedShape, comptime cir: []const u8) CopiedShape {
    for (shapes) |shape| {
        if (std.mem.eql(u8, shape.cir, cir)) return shape;
    }
    @compileError("no node shape copies the fields of ." ++ cir);
}

fn copiedShapeForTag(comptime shapes: []const CopiedShape, comptime tag: Node.Tag) CopiedShape {
    for (shapes) |shape| {
        if (shape.tag == tag) return shape;
    }
    @compileError("no CIR variant is copied out of a ." ++ @tagName(tag) ++ " node");
}

/// Stores the `cir_tag` variant `value` as the node its shape names.
fn copiedNode(comptime shapes: []const CopiedShape, comptime cir_tag: anytype, value: anytype) Node {
    const shape = comptime copiedShapeForCir(shapes, @tagName(cir_tag));
    var node = Node.init(shape.tag);
    if (comptime shape.payload) |payload_name| {
        node.setPayload(@unionInit(Node.Payload, payload_name, encodePayload(@FieldType(Node.Payload, payload_name), value)));
    }
    return node;
}

/// Reads the `Union` variant copied into a node tagged `narrowed_tag` back
/// out of the node's payload.
fn copiedValue(comptime Union: type, comptime shapes: []const CopiedShape, comptime narrowed_tag: anytype, payload: Node.Payload) Union {
    const shape = comptime copiedShapeForTag(shapes, @field(Node.Tag, @tagName(narrowed_tag)));
    const V = @FieldType(Union, shape.cir);
    if (comptime shape.payload) |payload_name| {
        return @unionInit(Union, shape.cir, decodePayload(V, @field(payload, payload_name)));
    }
    return @unionInit(Union, shape.cir, if (V == void) {} else .{});
}

/// Fills a payload struct from the CIR struct it stores. Each payload field
/// is matched to a CIR field by name: `x` to the field `x`, `x_start` and
/// `x_len` to the span `x`, and `x_plus_one` to the optional index `x`
/// (stored as zero when null). A payload field with no counterpart keeps its
/// default: the explicit padding, and the data other store operations write
/// once the node exists (a literal's dispatch plan, a replaced source node).
fn encodePayload(comptime P: type, value: anytype) P {
    const info = @typeInfo(P).@"struct";
    var payload: P = undefined;
    inline for (info.field_names, info.field_types, info.field_attrs) |name, W, attrs| {
        @field(payload, name) = encodePayloadWord(name, W, attrs.defaultValue(W), value);
    }
    return payload;
}

fn encodePayloadWord(comptime name: []const u8, comptime W: type, comptime default: ?W, value: anytype) W {
    const V = @TypeOf(value);
    if (comptime !std.mem.startsWith(u8, name, "_")) {
        if (comptime @hasField(V, name)) return encodeWord(W, @field(value, name));
        if (comptime spanFieldName(V, name, "_start")) |span_name| return @field(value, span_name).span.start;
        if (comptime spanFieldName(V, name, "_len")) |span_name| return @field(value, span_name).span.len;
        if (comptime optionalFieldName(V, name)) |optional_name| {
            return if (@field(value, optional_name)) |index| encodeWord(W, index) + 1 else 0;
        }
    }
    return default orelse @compileError("payload field " ++ name ++ " has no counterpart in " ++ @typeName(V) ++ " and no default");
}

/// Reads the CIR struct a payload stores, inverting `encodePayload` field by
/// field; a CIR field the payload has no words for is a compile error.
fn decodePayload(comptime V: type, payload: anytype) V {
    const info = @typeInfo(V).@"struct";
    if (info.field_names.len == 0) return .{};
    var value: V = undefined;
    inline for (info.field_names, info.field_types) |name, T| {
        @field(value, name) = decodePayloadWord(name, T, payload);
    }
    return value;
}

fn decodePayloadWord(comptime name: []const u8, comptime T: type, payload: anytype) T {
    const P = @TypeOf(payload);
    if (comptime @hasField(P, name)) return decodeWord(T, @field(payload, name));
    if (comptime isSpan(T) and @hasField(P, name ++ "_start") and @hasField(P, name ++ "_len")) {
        return .{ .span = .{ .start = @field(payload, name ++ "_start"), .len = @field(payload, name ++ "_len") } };
    }
    if (comptime @typeInfo(T) == .optional and @hasField(P, name ++ "_plus_one")) {
        const word = @field(payload, name ++ "_plus_one");
        return if (word == 0) null else decodeWord(@typeInfo(T).optional.child, word - 1);
    }
    @compileError(@typeName(P) ++ " has no field storing " ++ name);
}

/// The CIR span field a `<x>_start` or `<x>_len` payload word belongs to.
fn spanFieldName(comptime V: type, comptime name: []const u8, comptime suffix: []const u8) ?[]const u8 {
    if (!std.mem.endsWith(u8, name, suffix)) return null;
    const stem = name[0 .. name.len - suffix.len];
    if (!@hasField(V, stem) or !isSpan(@FieldType(V, stem))) return null;
    return stem;
}

/// The CIR optional field a `<x>_plus_one` payload word belongs to.
fn optionalFieldName(comptime V: type, comptime name: []const u8) ?[]const u8 {
    const suffix = "_plus_one";
    if (!std.mem.endsWith(u8, name, suffix)) return null;
    const stem = name[0 .. name.len - suffix.len];
    if (!@hasField(V, stem) or @typeInfo(@FieldType(V, stem)) != .optional) return null;
    return stem;
}

fn isSpan(comptime T: type) bool {
    return @typeInfo(T) == .@"struct" and @hasField(T, "span") and @FieldType(T, "span") == DataSpan;
}

/// Converts a CIR scalar to the payload word that stores it: an index or
/// enum to its backing integer, a bool to 0 or 1, a float or an `Ident.Idx`
/// to its bits.
fn encodeWord(comptime W: type, value: anytype) W {
    const V = @TypeOf(value);
    if (V == W) return value;
    return switch (@typeInfo(V)) {
        .@"enum" => @intCast(@backingInt(value)),
        .int => @intCast(value),
        .bool => @intFromBool(value),
        .float => @as(W, @bitCast(value)),
        .@"struct" => |info| if (info.layout == .@"packed") @as(W, @bitCast(value)) else @compileError("no payload word stores a " ++ @typeName(V)),
        .type,
        .void,
        .noreturn,
        .pointer,
        .array,
        .comptime_float,
        .comptime_int,
        .undefined,
        .null,
        .optional,
        .error_union,
        .error_set,
        .@"union",
        .@"fn",
        .@"opaque",
        .frame,
        .@"anyframe",
        .vector,
        .enum_literal,
        .spirv,
        => @compileError("no payload word stores a " ++ @typeName(V)),
    };
}

/// Converts a payload word back to the CIR scalar it stores.
fn decodeWord(comptime V: type, word: anytype) V {
    if (@TypeOf(word) == V) return word;
    return switch (@typeInfo(V)) {
        .@"enum" => @fromBackingInt(@intCast(word)),
        .int => @intCast(word),
        .bool => word != 0,
        .float => @as(V, @bitCast(word)),
        .@"struct" => |info| if (info.layout == .@"packed") @as(V, @bitCast(word)) else @compileError("no payload word stores a " ++ @typeName(V)),
        .type,
        .void,
        .noreturn,
        .pointer,
        .array,
        .comptime_float,
        .comptime_int,
        .undefined,
        .null,
        .optional,
        .error_union,
        .error_set,
        .@"union",
        .@"fn",
        .@"opaque",
        .frame,
        .@"anyframe",
        .vector,
        .enum_literal,
        .spirv,
        => @compileError("no payload word stores a " ++ @typeName(V)),
    };
}

/// A diagnostic whose node payload is its data copied word for word by
/// `encodeDiagnosticPayload` and `decodeDiagnosticPayload`: the variant's
/// name, the tag its node is stored under, the `Node.Payload` field laid over
/// the node's bytes (null when the diagnostic carries nothing but its region),
/// and the variant's fields in the order of the payload's data words, where a
/// Region fills a `<x>_start`/`<x>_end` pair. The variant's one field not
/// listed is the Region stored as the node's region, except in a diagnostic
/// about its file as a whole (`file_level`), which has no region; its node's
/// region is empty.
const DiagnosticShape = struct {
    cir: []const u8,
    tag: Node.Tag,
    payload: ?[]const u8,
    names: []const []const u8,
    file_level: bool = false,

    /// The field stored as the node's region, or null for a file-level
    /// diagnostic.
    fn regionField(comptime shape: DiagnosticShape) ?[]const u8 {
        const info = @typeInfo(@FieldType(CIR.Diagnostic, shape.cir)).@"struct";
        var region_field: ?[]const u8 = null;
        for (info.field_names, info.field_types) |name, T| {
            if (nameIndex(shape.names, name) != null) continue;
            if (T != Region or region_field != null or shape.file_level) {
                @compileError("the node shape of ." ++ shape.cir ++ " does not store its field " ++ name);
            }
            region_field = name;
        }
        if (region_field == null and !shape.file_level) {
            @compileError("." ++ shape.cir ++ " has no region to store as its node's region");
        }
        return region_field;
    }
};

fn diagnosticShapeForCir(comptime cir: []const u8) DiagnosticShape {
    for (&copied_diagnostic_shapes) |shape| {
        if (std.mem.eql(u8, shape.cir, cir)) return shape;
    }
    @compileError("no node shape copies the fields of ." ++ cir);
}

fn diagnosticShapeForTag(comptime tag: Node.Tag) DiagnosticShape {
    for (&copied_diagnostic_shapes) |shape| {
        if (shape.tag == tag) return shape;
    }
    @compileError("no diagnostic is copied out of a ." ++ @tagName(tag) ++ " node");
}

fn nameIndex(comptime names: []const []const u8, comptime name: []const u8) ?usize {
    for (names, 0..) |candidate, index| {
        if (std.mem.eql(u8, candidate, name)) return index;
    }
    return null;
}

/// The names of a diagnostic payload's data words in declaration order; a
/// Region's `<x>_start`/`<x>_end` pair is named by its `_start` word.
fn diagnosticWords(comptime P: type) []const []const u8 {
    var words: []const []const u8 = &.{};
    for (@typeInfo(P).@"struct".field_names) |name| {
        if (std.mem.startsWith(u8, name, "_") or std.mem.endsWith(u8, name, "_end")) continue;
        words = words ++ &[_][]const u8{name};
    }
    return words;
}

/// Stores the `cir_tag` diagnostic `value` as the node its shape names and
/// returns the region to store as the node's region.
fn copiedDiagnosticNode(comptime cir_tag: anytype, value: anytype, node: *Node) Region {
    const shape = comptime diagnosticShapeForCir(@tagName(cir_tag));
    node.tag = shape.tag;
    if (comptime shape.payload) |payload_name| {
        node.setPayload(@unionInit(Node.Payload, payload_name, encodeDiagnosticPayload(@FieldType(Node.Payload, payload_name), shape.names, value)));
    } else if (shape.names.len != 0) {
        @compileError("." ++ shape.cir ++ " names fields its node has no payload for");
    }
    return if (comptime shape.regionField()) |region_field| @field(value, region_field) else Region.zero();
}

fn encodeDiagnosticPayload(comptime P: type, comptime names: []const []const u8, value: anytype) P {
    const words = comptime diagnosticWords(P);
    if (words.len != names.len) @compileError(@typeName(P) ++ " has " ++ std.fmt.comptimePrint("{d}", .{words.len}) ++ " data words for " ++ std.fmt.comptimePrint("{d}", .{names.len}) ++ " fields");
    const info = @typeInfo(P).@"struct";
    var payload: P = undefined;
    inline for (info.field_names, info.field_types, info.field_attrs) |name, W, attrs| {
        if (comptime std.mem.startsWith(u8, name, "_")) @field(payload, name) = attrs.defaultValue(W).?;
    }
    inline for (words, names) |word, name| {
        if (comptime std.mem.endsWith(u8, word, "_start")) {
            const region: Region = @field(value, name);
            @field(payload, word) = region.start.offset;
            @field(payload, word[0 .. word.len - "_start".len] ++ "_end") = region.end.offset;
        } else {
            @field(payload, word) = encodeWord(@FieldType(P, word), @field(value, name));
        }
    }
    return payload;
}

/// Reads the diagnostic copied into a node tagged `narrowed_tag` back out of
/// the node's payload and region.
fn copiedDiagnostic(store: *const NodeStore, node_idx: Node.Idx, comptime narrowed_tag: anytype, payload: Node.Payload) CIR.Diagnostic {
    const shape = comptime diagnosticShapeForTag(@field(Node.Tag, @tagName(narrowed_tag)));
    const V = @FieldType(CIR.Diagnostic, shape.cir);
    const info = @typeInfo(V).@"struct";
    var value: V = undefined;
    inline for (info.field_names, info.field_types) |name, T| {
        if (comptime nameIndex(shape.names, name)) |index| {
            @field(value, name) = decodeDiagnosticWord(T, @field(payload, shape.payload.?), index);
        } else {
            comptime std.debug.assert(std.mem.eql(u8, name, shape.regionField().?));
            @field(value, name) = store.getRegionAt(node_idx);
        }
    }
    return @unionInit(CIR.Diagnostic, shape.cir, value);
}

fn decodeDiagnosticWord(comptime V: type, payload: anytype, comptime index: usize) V {
    const word = comptime diagnosticWords(@TypeOf(payload))[index];
    if (comptime std.mem.endsWith(u8, word, "_start")) {
        return .{
            .start = .{ .offset = @field(payload, word) },
            .end = .{ .offset = @field(payload, word[0 .. word.len - "_start".len] ++ "_end") },
        };
    }
    return decodeWord(V, @field(payload, word));
}

const copied_statement_shapes = [_]CopiedShape{
    .{ .cir = "s_reassign", .tag = .statement_reassign, .payload = "statement_reassign" },
    .{ .cir = "s_crash", .tag = .statement_crash, .payload = "statement_crash" },
    .{ .cir = "s_dbg", .tag = .statement_dbg, .payload = "statement_single_expr" },
    .{ .cir = "s_expr", .tag = .statement_expr, .payload = "statement_single_expr" },
    .{ .cir = "s_for", .tag = .statement_for, .payload = "statement_for" },
    .{ .cir = "s_while", .tag = .statement_while, .payload = "statement_while" },
    .{ .cir = "s_infinite_loop", .tag = .statement_infinite_loop, .payload = "statement_while" },
    .{ .cir = "s_breakable_loop", .tag = .statement_breakable_loop, .payload = "statement_while" },
    .{ .cir = "s_break", .tag = .statement_break, .payload = null },
    .{ .cir = "s_return", .tag = .statement_return, .payload = "statement_return" },
    .{ .cir = "s_alias_decl", .tag = .statement_alias_decl, .payload = "statement_alias_decl" },
    .{ .cir = "s_nominal_decl", .tag = .statement_nominal_decl, .payload = "statement_nominal_decl" },
    .{ .cir = "s_type_var_alias", .tag = .statement_type_var_alias, .payload = "statement_type_var_alias" },
    .{ .cir = "s_runtime_error", .tag = .malformed, .payload = "malformed" },
};

const copied_expr_shapes = [_]CopiedShape{
    .{ .cir = "e_lookup_local", .tag = .expr_var, .payload = "expr_var" },
    .{ .cir = "e_lookup_associated_local", .tag = .expr_associated_lookup_local, .payload = "expr_associated_lookup_local" },
    .{ .cir = "e_lookup_associated", .tag = .expr_associated_lookup, .payload = "expr_associated_lookup" },
    .{ .cir = "e_lookup_associated_resolved", .tag = .expr_associated_lookup_resolved, .payload = "expr_associated_lookup_resolved" },
    .{ .cir = "e_lookup_required", .tag = .expr_required_lookup, .payload = "expr_required_lookup" },
    .{ .cir = "e_list", .tag = .expr_list, .payload = "expr_list" },
    .{ .cir = "e_empty_list", .tag = .expr_empty_list, .payload = null },
    .{ .cir = "e_tuple", .tag = .expr_tuple, .payload = "expr_tuple" },
    .{ .cir = "e_tuple_access", .tag = .expr_tuple_access, .payload = "expr_tuple_access" },
    .{ .cir = "e_frac_f32", .tag = .expr_frac_f32, .payload = "expr_frac_f32" },
    .{ .cir = "e_num_from_numeral", .tag = .expr_num_from_numeral, .payload = "expr_num_from_numeral" },
    .{ .cir = "e_typed_num_from_numeral", .tag = .expr_typed_num_from_numeral, .payload = "expr_typed_num_from_numeral" },
    .{ .cir = "e_tag", .tag = .expr_tag, .payload = "expr_tag" },
    .{ .cir = "e_nominal", .tag = .expr_nominal, .payload = "expr_nominal" },
    .{ .cir = "e_structural_eq", .tag = .expr_structural_eq, .payload = "expr_structural_eq" },
    .{ .cir = "e_structural_hash", .tag = .expr_structural_hash, .payload = "expr_structural_hash" },
    .{ .cir = "e_method_eq", .tag = .expr_method_eq, .payload = "expr_method_eq" },
    .{ .cir = "e_runtime_error", .tag = .malformed, .payload = "malformed" },
    .{ .cir = "e_crash", .tag = .expr_crash, .payload = "expr_crash" },
    .{ .cir = "e_dbg", .tag = .expr_dbg, .payload = "expr_dbg" },
    .{ .cir = "e_expect_err", .tag = .expr_expect_err, .payload = "expr_expect_err" },
    .{ .cir = "e_ellipsis", .tag = .expr_ellipsis, .payload = null },
    .{ .cir = "e_anno_only", .tag = .expr_anno_only, .payload = "expr_anno_only" },
    .{ .cir = "e_derived_method", .tag = .expr_derived_method, .payload = "expr_derived_method" },
    .{ .cir = "e_return", .tag = .expr_return, .payload = "expr_return" },
    .{ .cir = "e_break", .tag = .expr_break, .payload = null },
    .{ .cir = "e_expect", .tag = .expr_expect, .payload = "expr_expect" },
    .{ .cir = "e_for", .tag = .expr_for, .payload = "expr_for" },
    .{ .cir = "e_empty_record", .tag = .expr_empty_record, .payload = null },
    .{ .cir = "e_lambda", .tag = .expr_lambda, .payload = "expr_lambda" },
    .{ .cir = "e_binop", .tag = .expr_bin_op, .payload = "expr_bin_op" },
    .{ .cir = "e_unary_minus", .tag = .expr_unary_minus, .payload = "expr_unary" },
    .{ .cir = "e_block", .tag = .expr_block, .payload = "expr_block" },
};

const copied_pattern_shapes = [_]CopiedShape{
    .{ .cir = "assign", .tag = .pattern_identifier, .payload = "pattern_identifier" },
    .{ .cir = "var_assign", .tag = .pattern_var_identifier, .payload = "pattern_var_identifier" },
    .{ .cir = "as", .tag = .pattern_as, .payload = "pattern_as" },
    .{ .cir = "applied_tag", .tag = .pattern_applied_tag, .payload = "pattern_applied_tag" },
    .{ .cir = "nominal", .tag = .pattern_nominal, .payload = "pattern_nominal" },
    .{ .cir = "record_destructure", .tag = .pattern_record_destructure, .payload = "pattern_record_destructure" },
    .{ .cir = "tuple", .tag = .pattern_tuple, .payload = "pattern_tuple" },
    .{ .cir = "num_from_numeral_literal", .tag = .pattern_num_from_numeral_literal, .payload = "pattern_num_from_numeral_literal" },
    .{ .cir = "str_literal", .tag = .pattern_str_literal, .payload = "pattern_str_literal" },
    .{ .cir = "frac_f32_literal", .tag = .pattern_f32_literal, .payload = "pattern_frac_f32" },
    .{ .cir = "underscore", .tag = .pattern_underscore, .payload = null },
    .{ .cir = "runtime_error", .tag = .malformed, .payload = "malformed" },
};

const copied_type_anno_shapes = [_]CopiedShape{
    .{ .cir = "rigid_var", .tag = .ty_rigid_var, .payload = "ty_rigid_var" },
    .{ .cir = "rigid_var_lookup", .tag = .ty_rigid_var_lookup, .payload = "ty_rigid_var_lookup" },
    .{ .cir = "underscore", .tag = .ty_underscore, .payload = null },
    .{ .cir = "tag_union", .tag = .ty_tag_union, .payload = "ty_tag_union" },
    .{ .cir = "tag", .tag = .ty_tag, .payload = "ty_tag" },
    .{ .cir = "tuple", .tag = .ty_tuple, .payload = "ty_tuple" },
    .{ .cir = "record", .tag = .ty_record, .payload = "ty_record" },
    .{ .cir = "parens", .tag = .ty_parens, .payload = "ty_parens" },
    .{ .cir = "malformed", .tag = .ty_malformed, .payload = "ty_malformed" },
};

const copied_diagnostic_shapes = [_]DiagnosticShape{
    .{ .cir = "not_implemented", .tag = .diag_not_implemented, .payload = "diag_single_value", .names = &.{"feature"} },
    .{ .cir = "invalid_num_literal", .tag = .diag_invalid_num_literal, .payload = null, .names = &.{} },
    .{ .cir = "empty_tuple", .tag = .diag_empty_tuple, .payload = null, .names = &.{} },
    .{ .cir = "erroneous_value_expr", .tag = .diag_erroneous_value_expr, .payload = null, .names = &.{} },
    .{ .cir = "expr_not_canonicalized", .tag = .diag_expr_not_canonicalized, .payload = null, .names = &.{} },
    .{ .cir = "expr_syntax_error", .tag = .diag_expr_syntax_error, .payload = null, .names = &.{} },
    .{ .cir = "unreachable_string_pattern_capture", .tag = .diag_unreachable_string_pattern_capture, .payload = null, .names = &.{} },
    .{ .cir = "pattern_arg_invalid", .tag = .diag_pattern_arg_invalid, .payload = null, .names = &.{} },
    .{ .cir = "pattern_not_canonicalized", .tag = .diag_pattern_not_canonicalized, .payload = null, .names = &.{} },
    .{ .cir = "if_expr_without_else", .tag = .diag_if_expr_without_else, .payload = null, .names = &.{} },
    .{ .cir = "malformed_type_annotation", .tag = .diag_malformed_type_annotation, .payload = null, .names = &.{} },
    .{ .cir = "malformed_where_clause", .tag = .diag_malformed_where_clause, .payload = null, .names = &.{} },
    .{ .cir = "where_clause_not_allowed_in_type_decl", .tag = .diag_where_clause_not_allowed_in_type_decl, .payload = null, .names = &.{} },
    .{ .cir = "open_ext_not_allowed_in_type_decl", .tag = .diag_open_ext_not_allowed_in_type_decl, .payload = null, .names = &.{} },
    .{ .cir = "unnamed_field_not_allowed_in_structural_record", .tag = .diag_unnamed_field_not_allowed_in_structural_record, .payload = null, .names = &.{} },
    .{ .cir = "optional_field_cannot_have_default", .tag = .diag_optional_field_cannot_have_default, .payload = null, .names = &.{} },
    .{ .cir = "unnamed_field_cannot_have_default", .tag = .diag_unnamed_field_cannot_have_default, .payload = null, .names = &.{} },
    .{ .cir = "default_not_allowed_in_structural_record", .tag = .diag_default_not_allowed_in_structural_record, .payload = null, .names = &.{} },
    .{ .cir = "default_not_allowed_on_local_type_decl", .tag = .diag_default_not_allowed_on_local_type_decl, .payload = null, .names = &.{} },
    .{ .cir = "module_header_deprecated", .tag = .diag_module_header_deprecated, .payload = null, .names = &.{} },
    .{ .cir = "var_across_function_boundary", .tag = .diag_var_across_function_boundary, .payload = null, .names = &.{} },
    .{ .cir = "crash_expects_string", .tag = .diag_crash_expects_string, .payload = null, .names = &.{} },
    .{ .cir = "f64_pattern_literal", .tag = .diag_f64_pattern_literal, .payload = null, .names = &.{} },
    .{ .cir = "break_outside_loop", .tag = .diag_break_outside_loop, .payload = null, .names = &.{} },
    .{ .cir = "infinite_loop_never_exits", .tag = .diag_infinite_loop_never_exits, .payload = null, .names = &.{} },
    .{ .cir = "trailing_try_suffix", .tag = .diag_trailing_try_suffix, .payload = null, .names = &.{} },
    .{ .cir = "redundant_return", .tag = .diag_redundant_return, .payload = null, .names = &.{} },
    .{ .cir = "unreachable_code", .tag = .diag_unreachable_code, .payload = null, .names = &.{} },
    .{ .cir = "range_op_chained", .tag = .diag_range_op_chained, .payload = null, .names = &.{} },
    .{ .cir = "ident_already_in_scope", .tag = .diag_ident_already_in_scope, .payload = "diag_single_ident", .names = &.{"ident"} },
    .{ .cir = "provided_value_is_required", .tag = .diag_provided_value_is_required, .payload = "diag_single_ident", .names = &.{"ident"} },
    .{ .cir = "ident_not_in_scope", .tag = .diag_ident_not_in_scope, .payload = "diag_single_ident", .names = &.{"ident"} },
    .{ .cir = "read_uninitialized_var", .tag = .diag_read_uninitialized_var, .payload = "diag_single_ident", .names = &.{"ident"} },
    .{ .cir = "self_referential_definition", .tag = .diag_self_referential_definition, .payload = "diag_single_ident", .names = &.{"ident"} },
    .{ .cir = "circular_value_definition", .tag = .diag_circular_value_definition, .payload = "diag_single_ident", .names = &.{"ident"} },
    .{ .cir = "local_reference_before_definition", .tag = .diag_local_reference_before_definition, .payload = "diag_single_ident", .names = &.{"ident"} },
    .{ .cir = "erroneous_value_use", .tag = .diag_erroneous_value_use, .payload = "diag_single_ident", .names = &.{"ident"} },
    .{ .cir = "qualified_ident_does_not_exist", .tag = .diag_qualified_ident_does_not_exist, .payload = "diag_single_ident", .names = &.{"ident"} },
    .{ .cir = "unused_variable", .tag = .diag_unused_variable, .payload = "diag_single_ident", .names = &.{"ident"} },
    .{ .cir = "used_underscore_variable", .tag = .diag_used_underscore_variable, .payload = "diag_single_ident", .names = &.{"ident"} },
    .{ .cir = "exposed_but_not_implemented", .tag = .diagnostic_exposed_but_not_implemented, .payload = "diag_single_ident", .names = &.{"ident"} },
    .{ .cir = "mutually_recursive_local_definitions", .tag = .diag_mutually_recursive_local_definitions, .payload = "diag_two_idents", .names = &.{ "ident1", "ident2" } },
    .{ .cir = "invalid_top_level_statement", .tag = .diag_invalid_top_level_statement, .payload = "diag_single_value", .names = &.{"stmt"} },
    .{ .cir = "invalid_associated_statement", .tag = .diag_invalid_associated_statement, .payload = "diag_single_value", .names = &.{"stmt"} },
    .{ .cir = "where_alias_constraint_not_on_receiver", .tag = .diag_where_alias_constraint_not_on_receiver, .payload = "diag_single_ident", .names = &.{"receiver_name"} },
    .{ .cir = "record_default_reference_cycle", .tag = .diag_record_default_reference_cycle, .payload = "diag_single_ident", .names = &.{"field_name"} },
    .{ .cir = "type_module_missing_matching_type", .tag = .diag_type_module_missing_matching_type, .payload = "diag_single_ident", .names = &.{"module_name"}, .file_level = true },
    .{ .cir = "type_module_has_alias_not_nominal", .tag = .diag_type_module_has_alias_not_nominal, .payload = "diag_single_ident", .names = &.{"module_name"} },
    .{ .cir = "default_app_missing_main", .tag = .diag_default_app_missing_main, .payload = "diag_single_ident", .names = &.{"module_name"}, .file_level = true },
    .{ .cir = "default_app_wrong_arity", .tag = .diag_default_app_wrong_arity, .payload = "diag_single_value", .names = &.{"arity"} },
    .{ .cir = "cannot_import_default_app", .tag = .diag_cannot_import_default_app, .payload = "diag_single_ident", .names = &.{"module_name"} },
    .{ .cir = "type_name_case_mismatch", .tag = .diag_type_name_case_mismatch, .payload = "diag_two_idents", .names = &.{ "module_name", "type_name" } },
    .{ .cir = "roc_version_mismatch", .tag = .diag_roc_version_mismatch, .payload = "diag_two_idents", .names = &.{ "pinned", "running" } },
    .{ .cir = "redundant_expose_main_type", .tag = .diag_redundant_expose_main_type, .payload = "diag_two_idents", .names = &.{ "type_name", "module_name" } },
    .{ .cir = "invalid_main_type_rename_in_exposing", .tag = .diag_invalid_main_type_rename_in_exposing, .payload = "diag_two_idents", .names = &.{ "type_name", "alias" } },
    .{ .cir = "shadowing_warning", .tag = .diag_shadowing_warning, .payload = "diag_ident_with_region", .names = &.{ "ident", "original_region" } },
    .{ .cir = "binding_name_does_not_match_mutability", .tag = .diag_binding_name_does_not_match_mutability, .payload = "diag_two_idents", .names = &.{ "ident", "mutability" } },
    .{ .cir = "var_in_binder_pattern", .tag = .diag_var_in_binder_pattern, .payload = "diag_two_idents", .names = &.{ "ident", "position" } },
    .{ .cir = "type_redeclared", .tag = .diag_type_redeclared, .payload = "diag_ident_with_region", .names = &.{ "name", "original_region" } },
    .{ .cir = "undeclared_type", .tag = .diag_undeclared_type, .payload = "diag_single_ident", .names = &.{"name"} },
    .{ .cir = "type_alias_but_needed_nominal", .tag = .diag_type_alias_but_needed_nominal, .payload = "diag_single_ident", .names = &.{"name"} },
    .{ .cir = "undeclared_type_var", .tag = .diag_undeclared_type_var, .payload = "diag_single_ident", .names = &.{"name"} },
    .{ .cir = "type_alias_redeclared", .tag = .diag_type_alias_redeclared, .payload = "diag_ident_with_region", .names = &.{ "name", "original_region" } },
    .{ .cir = "file_import_not_found", .tag = .diag_file_import_not_found, .payload = "diag_single_value", .names = &.{"path"} },
    .{ .cir = "file_import_io_error", .tag = .diag_file_import_io_error, .payload = "diag_single_value", .names = &.{"path"} },
    .{ .cir = "file_import_absolute_path", .tag = .diag_file_import_absolute_path, .payload = "diag_single_value", .names = &.{"path"} },
    .{ .cir = "file_import_not_utf8", .tag = .diag_file_import_not_utf8, .payload = "diag_single_value", .names = &.{"path"} },
    .{ .cir = "module_not_found", .tag = .diag_module_not_found, .payload = "diag_single_ident", .names = &.{"module_name"} },
    .{ .cir = "value_not_exposed", .tag = .diag_value_not_exposed, .payload = "diag_two_idents", .names = &.{ "module_name", "value_name" } },
    .{ .cir = "type_not_exposed", .tag = .diag_type_not_exposed, .payload = "diag_two_idents", .names = &.{ "module_name", "type_name" } },
    .{ .cir = "private_type_in_exposed_type", .tag = .diag_private_type_in_exposed_type, .payload = "diag_two_idents", .names = &.{ "exposed_type", "private_type" } },
    .{ .cir = "private_type_in_exposed_field", .tag = .diag_private_type_in_exposed_field, .payload = "diag_three_idents", .names = &.{ "exposed_type", "field_name", "private_type" } },
    .{ .cir = "type_from_missing_module", .tag = .diag_type_from_missing_module, .payload = "diag_two_idents", .names = &.{ "module_name", "type_name" } },
    .{ .cir = "module_not_imported", .tag = .diag_module_not_imported, .payload = "diag_single_ident", .names = &.{"module_name"} },
    .{ .cir = "public_type_not_defined", .tag = .diag_public_type_not_defined, .payload = "diag_single_ident", .names = &.{"import_name"} },
    .{ .cir = "nested_type_not_found", .tag = .diag_nested_type_not_found, .payload = "diag_two_idents", .names = &.{ "parent_name", "nested_name" } },
    .{ .cir = "internal_builtin_type", .tag = .diag_internal_builtin_type, .payload = "diag_internal_builtin_type", .names = &.{ "parent_name", "nested_name", "kind" } },
    .{ .cir = "nested_value_not_found", .tag = .diag_nested_value_not_found, .payload = "diag_two_idents", .names = &.{ "parent_name", "nested_name" } },
    .{ .cir = "record_builder_map2_not_found", .tag = .diag_record_builder_map2_not_found, .payload = "diag_single_ident", .names = &.{"type_name"} },
    .{ .cir = "too_many_exports", .tag = .diag_too_many_exports, .payload = "diag_single_value", .names = &.{"count"} },
    .{ .cir = "nominal_type_redeclared", .tag = .diag_nominal_type_redeclared, .payload = "diag_ident_with_region", .names = &.{ "name", "original_region" } },
    .{ .cir = "type_shadowed_warning", .tag = .diag_type_shadowed_warning, .payload = "diag_ident_with_region", .names = &.{ "name", "original_region" } },
    .{ .cir = "builtin_type_shadowed_warning", .tag = .diag_builtin_type_shadowed_warning, .payload = "diag_single_ident", .names = &.{"name"} },
    .{ .cir = "duplicate_record_field", .tag = .diag_duplicate_record_field, .payload = "diag_ident_with_region", .names = &.{ "field_name", "original_region" } },
    .{ .cir = "duplicate_pattern_binder", .tag = .diag_duplicate_pattern_binder, .payload = "diag_ident_with_region", .names = &.{ "ident", "original_region" } },
    .{ .cir = "duplicate_tag", .tag = .diag_duplicate_tag, .payload = "diag_ident_with_region", .names = &.{ "tag_name", "original_region" } },
    .{ .cir = "type_var_starting_with_dollar", .tag = .diag_type_var_starting_with_dollar, .payload = "diag_two_idents", .names = &.{ "name", "suggested_name" } },
    .{ .cir = "underscore_in_type_declaration", .tag = .diag_underscore_in_type_declaration, .payload = "diag_single_value", .names = &.{"declared"} },
    .{ .cir = "return_outside_fn", .tag = .diag_return_outside_fn, .payload = "diag_single_value", .names = &.{"context"} },
    .{ .cir = "control_flow_in_expect", .tag = .diag_control_flow_in_expect, .payload = "diag_single_value", .names = &.{"kind"} },
    .{ .cir = "var_reassigned_in_expect", .tag = .diag_var_reassigned_in_expect, .payload = "diag_ident_with_region", .names = &.{ "ident", "declaration_region" } },
    .{ .cir = "deprecated_number_suffix", .tag = .diag_deprecated_number_suffix, .payload = "diag_two_enums", .names = &.{ "suffix", "suggested" } },
};

const LiteralNodeTag = enum {
    expr_num,
    expr_frac_f32,
    expr_frac_f64,
    expr_dec,
    expr_dec_small,
    expr_num_from_numeral,
    expr_typed_int,
    expr_typed_frac,
    expr_typed_num_from_numeral,
    expr_string,
    pattern_num_literal,
    pattern_small_dec_literal,
    pattern_dec_literal,
    pattern_num_from_numeral_literal,
    pattern_str_literal,
};

const StatementNodeTag = enum {
    statement_decl,
    statement_var,
    statement_var_uninitialized,
    statement_reassign,
    statement_crash,
    statement_dbg,
    statement_expr,
    statement_expect,
    statement_for,
    statement_while,
    statement_infinite_loop,
    statement_breakable_loop,
    statement_break,
    statement_return,
    statement_import,
    statement_alias_decl,
    statement_nominal_decl,
    statement_where_alias_decl,
    statement_type_anno,
    statement_type_var_alias,
    malformed,
};

const ExprNodeTag = enum {
    expr_var,
    expr_external_lookup,
    expr_deferred_import_ref,
    expr_deferred_nominal_external,
    expr_associated_lookup_local,
    expr_associated_lookup,
    expr_associated_lookup_resolved,
    expr_required_lookup,
    expr_num,
    expr_list,
    expr_tuple,
    expr_tuple_access,
    expr_call,
    expr_frac_f32,
    expr_frac_f64,
    expr_dec,
    expr_dec_small,
    expr_num_from_numeral,
    expr_typed_int,
    expr_typed_frac,
    expr_typed_num_from_numeral,
    expr_string_segment,
    expr_bytes_literal,
    expr_string,
    expr_tag,
    expr_nominal,
    expr_nominal_external,
    expr_bin_op,
    expr_closure,
    expr_lambda,
    expr_block,
    expr_empty_record,
    expr_empty_list,
    expr_record,
    expr_match,
    expr_zero_argument_tag,
    expr_crash,
    expr_dbg,
    expr_expect_err,
    expr_unary_minus,
    expr_ellipsis,
    expr_anno_only,
    expr_derived_method,
    expr_return,
    expr_break,
    expr_hosted_lambda,
    expr_run_low_level,
    expr_expect,
    expr_for,
    expr_if_then_else,
    expr_field_access,
    expr_method_call,
    expr_dispatch_call,
    expr_interpolation,
    expr_structural_eq,
    expr_structural_hash,
    expr_method_eq,
    expr_type_method_call,
    expr_type_dispatch_call,
    expr_type_dispatch_call_dispatcher,
    malformed,
};

const WhereNodeTag = enum { where_method, where_alias, where_malformed };

const PatternNodeTag = enum {
    pattern_identifier,
    pattern_var_identifier,
    pattern_as,
    pattern_applied_tag,
    pattern_nominal,
    pattern_nominal_external,
    pattern_deferred_import_ref,
    pattern_record_destructure,
    pattern_list,
    pattern_tuple,
    pattern_num_literal,
    pattern_f32_literal,
    pattern_f64_literal,
    pattern_num_from_numeral_literal,
    pattern_dec_literal,
    pattern_small_dec_literal,
    pattern_str_literal,
    pattern_str_interpolation,
    pattern_underscore,
    malformed,
};

const TypeAnnoNodeTag = enum {
    ty_apply,
    ty_rigid_var,
    ty_rigid_var_lookup,
    ty_underscore,
    ty_lookup,
    ty_tag_union,
    ty_tag,
    ty_tuple,
    ty_record,
    ty_fn,
    ty_parens,
    ty_malformed,
    malformed,
};

const ExposedItemNodeTag = enum { exposed_item };

const DiagnosticNodeTag = enum {
    diag_not_implemented,
    diag_invalid_num_literal,
    diag_empty_tuple,
    diag_ident_already_in_scope,
    diagnostic_exposed_but_not_implemented,
    diag_provided_value_is_required,
    diag_redundant_exposed,
    diag_ident_not_in_scope,
    diag_read_uninitialized_var,
    diag_self_referential_definition,
    diag_circular_value_definition,
    diag_local_reference_before_definition,
    diag_mutually_recursive_local_definitions,
    diag_erroneous_value_use,
    diag_erroneous_value_expr,
    diag_qualified_ident_does_not_exist,
    diag_invalid_top_level_statement,
    diag_invalid_associated_statement,
    diag_expr_not_canonicalized,
    diag_expr_syntax_error,
    diag_unreachable_string_pattern_capture,
    diag_pattern_arg_invalid,
    diag_pattern_not_canonicalized,
    diag_if_expr_without_else,
    diag_var_across_function_boundary,
    diag_shadowing_warning,
    diag_binding_name_does_not_match_mutability,
    diag_var_in_binder_pattern,
    diag_type_redeclared,
    diag_undeclared_type,
    diag_type_alias_but_needed_nominal,
    diag_file_import_not_found,
    diag_file_import_io_error,
    diag_file_import_absolute_path,
    diag_file_import_not_utf8,
    diag_module_not_found,
    diag_value_not_exposed,
    diag_type_not_exposed,
    diag_private_type_in_exposed_type,
    diag_private_type_in_exposed_field,
    diag_type_from_missing_module,
    diag_module_not_imported,
    diag_public_type_not_defined,
    diag_nested_type_not_found,
    diag_internal_builtin_type,
    diag_nested_value_not_found,
    diag_record_builder_map2_not_found,
    diag_too_many_exports,
    diag_undeclared_type_var,
    diag_malformed_type_annotation,
    diag_malformed_where_clause,
    diag_where_clause_not_allowed_in_type_decl,
    diag_where_alias_constraint_not_on_receiver,
    diag_open_ext_not_allowed_in_type_decl,
    diag_unnamed_field_not_allowed_in_structural_record,
    diag_optional_field_cannot_have_default,
    diag_record_default_reference_cycle,
    diag_type_module_missing_matching_type,
    diag_type_module_has_alias_not_nominal,
    diag_default_app_missing_main,
    diag_default_app_wrong_arity,
    diag_cannot_import_default_app,
    diag_type_name_case_mismatch,
    diag_module_header_deprecated,
    diag_roc_version_mismatch,
    diag_redundant_expose_main_type,
    diag_invalid_main_type_rename_in_exposing,
    diag_type_alias_redeclared,
    diag_nominal_type_redeclared,
    diag_type_shadowed_warning,
    diag_builtin_type_shadowed_warning,
    diag_type_parameter_conflict,
    diag_unused_variable,
    diag_used_underscore_variable,
    diag_duplicate_record_field,
    diag_duplicate_pattern_binder,
    diag_duplicate_tag,
    diag_crash_expects_string,
    diag_f64_pattern_literal,
    diag_type_var_starting_with_dollar,
    diag_underscore_in_type_declaration,
    diag_break_outside_loop,
    diag_infinite_loop_never_exits,
    diag_trailing_try_suffix,
    diag_return_outside_fn,
    diag_redundant_return,
    diag_unreachable_code,
    diag_control_flow_in_expect,
    diag_var_reassigned_in_expect,
    diag_mutually_recursive_type_aliases,
    diag_deprecated_number_suffix,
    diag_range_op_chained,
    diag_unnamed_field_cannot_have_default,
    diag_default_not_allowed_in_structural_record,
    diag_default_not_allowed_on_local_type_decl,
};

gpa: Allocator,
nodes: Node.List,
/// Source nodes replaced in place by `.malformed` runtime errors, each named
/// by its replacement's `source_node_plus_one`. Compilation reads the
/// replacement; source-level tools read through it to the code as written.
replaced_source_nodes: Node.List,
regions: Region.List,
write_occurrences: collections.SafeList(WriteOccurrence),
int128_values: collections.SafeList(i128), // Typed storage for large numeric literals
literal_dispatch_plans: collections.SafeList(LiteralDispatchPlan), // Checked literal dispatch metadata owned by literal nodes
literal_pattern_contexts: collections.SafeList(LiteralPatternContext),
interpolation_data: collections.SafeList(InterpolationData), // Canonical and checked data owned by interpolation expressions
span2_data: collections.SafeList(Span2), // Typed storage for (start, len) span pairs
span_with_node_data: collections.SafeList(SpanWithNode), // Typed storage for (start, len, node) triples
method_call_data: collections.SafeList(MethodCallData), // Typed storage for method args plus method-token source region
match_data: collections.SafeList(MatchData), // Typed storage for match expression data
if_data: collections.SafeList(IfData), // Typed storage for if expression data
match_branch_data: collections.SafeList(MatchBranchData), // Typed storage for match branch data
closure_data: collections.SafeList(ClosureData), // Typed storage for closure expressions
zero_arg_tag_data: collections.SafeList(ZeroArgTagData), // Typed storage for zero-argument tags
def_data: collections.SafeList(DefData), // Typed storage for definitions
import_data: collections.SafeList(ImportData), // Typed storage for import statements
type_apply_data: collections.SafeList(TypeApplyData), // Typed storage for type annotation apply
pattern_list_data: collections.SafeList(PatternListData), // Typed storage for pattern lists
pattern_str_interpolation_data: collections.SafeList(PatternStrInterpolationData), // Typed storage for string interpolation patterns
pattern_str_interpolation_steps: collections.SafeList(PatternStrInterpolationStepData), // Typed storage for string interpolation pattern steps
where_clause_owners: collections.SafeList(WhereClauseOwnerData), // Canonical receiver ownership for each where-clause scope
index_data: collections.SafeList(u32), // Storage for variable-length index arrays (tuple elems, tag args, scratch spans)
scratch: ?*Scratch, // Nullable because when we deserialize a NodeStore, we don't bother to reinitialize scratch.

/// A source name that writes an existing mutable binding. Binding patterns keep
/// their declaration regions; each write retains its own exact token region.
/// This table contains only writes, not reads or fresh declarations.
pub const WriteOccurrence = extern struct {
    pattern_idx: CIR.Pattern.Idx,
    start: u32,
    end: u32,

    pub fn region(self: WriteOccurrence) Region {
        return Region.from_raw_offsets(self.start, self.end);
    }
};

/// Record a write when canonicalization resolves its target binding.
pub fn recordWriteOccurrence(store: *NodeStore, pattern_idx: CIR.Pattern.Idx, region: Region) Allocator.Error!void {
    std.debug.assert(store.getPattern(pattern_idx) == .var_assign);
    _ = try store.write_occurrences.append(store.gpa, .{
        .pattern_idx = pattern_idx,
        .start = region.start.offset,
        .end = region.end.offset,
    });
}

/// A pair of u32 values representing a span (start index and length).
/// Used for storing argument lists, field lists, branch lists, etc.
pub const Span2 = extern struct {
    start: u32,
    len: u32,
};

/// A span with an associated node index.
/// Used for lambda args + body, where method args + return type, etc.
pub const SpanWithNode = extern struct {
    start: u32,
    len: u32,
    node: u32,
};

/// Checked dispatch metadata owned by one literal expression or pattern node.
/// The owning node stores this record's dense index, so replacing the node
/// atomically retires the plan instead of leaving raw-node side metadata live.
pub const LiteralDispatchPlan = extern struct {
    node_idx: u32,
    target_var: u32,
    /// Zero only for a proved concrete builtin numeral, which needs no
    /// conversion signature. All deferred/custom/pattern conversions name one.
    fn_var_plus_one: u32,
    kind_and_resolution: u32,
    pattern_context_plus_one: u32,

    pub const Kind = enum(u32) {
        numeral,
        quote,
    };

    pub const Resolution = enum(u32) {
        unresolved,
        builtin_direct,
        custom_dispatch,
        specialization_dispatch,
        checked_error,
    };

    const resolution_shift = 1;

    fn packKindAndResolution(kind: Kind, resolution: Resolution) u32 {
        return @backingInt(kind) | (@backingInt(resolution) << resolution_shift);
    }

    /// The conversion obligation, absent for a validated concrete builtin numeral.
    pub fn fnVar(self: LiteralDispatchPlan) ?types.Var {
        return if (self.fn_var_plus_one == 0) null else @fromBackingInt(@intCast(self.fn_var_plus_one - 1));
    }

    pub fn dispatchKind(self: LiteralDispatchPlan) Kind {
        return @fromBackingInt(@intCast(self.kind_and_resolution & 1));
    }

    pub fn dispatchResolution(self: LiteralDispatchPlan) Resolution {
        return @fromBackingInt(@intCast(self.kind_and_resolution >> resolution_shift));
    }

    pub fn patternContext(self: LiteralDispatchPlan, store: *const NodeStore) ?*const LiteralPatternContext {
        if (self.pattern_context_plus_one == 0) return null;
        return store.literal_pattern_contexts.get(@fromBackingInt(@intCast(self.pattern_context_plus_one - 1)));
    }

    pub fn patternFailureOwner(self: LiteralDispatchPlan, store: *const NodeStore) ?u32 {
        return if (self.patternContext(store)) |context| context.failure_owner else null;
    }

    fn setResolution(self: *LiteralDispatchPlan, resolution: Resolution) void {
        self.kind_and_resolution = packKindAndResolution(self.dispatchKind(), resolution);
    }
};

/// Pattern-only checked relations. Expression literals allocate no context.
/// The literal plan owns this row; retired plans retain it for error recovery.
pub const LiteralPatternContext = extern struct {
    failure_owner: u32,
    equality_fn_var_plus_one: u32 = 0,
};

comptime {
    if (@sizeOf(LiteralDispatchPlan) != 5 * @sizeOf(u32)) {
        @compileError("LiteralDispatchPlan must remain five words");
    }
}

/// Canonical and checked data owned by a compiler-created interpolation expression.
/// Optional type variables use zero for null and otherwise store `@backingInt(var) + 1`.
pub const InterpolationData = extern struct {
    parts_start: u32,
    parts_len: u32,
    method_region_start: u32,
    method_region_end: u32,
    constraint_fn_var_plus_one: u32,
    assembler_fn_var_plus_one: u32,
    dispatcher_var_plus_one: u32,
};

/// Method-call side data.
/// Stores argument span, the exact method-token source region, and the
/// surface origin (encoded via `encodeSurfaceOrigin`/`decodeSurfaceOrigin`).
pub const MethodCallData = extern struct {
    args_start: u32,
    args_len: u32,
    method_region_start: u32,
    method_region_end: u32,
    surface_origin: u32,
};

/// Match expression data.
/// Stores cond, branches span, exhaustive flag, try-suffix flag, and exhaustiveness-reporting flag.
pub const MatchData = extern struct {
    cond: u32,
    branches_start: u32,
    branches_len: u32,
    exhaustive: u32,
    is_try_suffix: u32,
    skip_exhaustiveness: u32,
};

/// If expression data.
/// Stores branches span, final else, and the construct the `if` canonicalizes.
pub const IfData = extern struct {
    branches_start: u32,
    branches_len: u32,
    final_else: u32,
    origin: u32,
};

/// Match branch data.
/// Stores patterns span, value, guard, and redundant flag.
pub const MatchBranchData = extern struct {
    patterns_start: u32,
    patterns_len: u32,
    value: u32,
    guard: u32, // 0 if no guard, otherwise node index
    redundant: u32,
};

/// Closure expression data.
/// Stores lambda index, captures span, and tag name.
pub const ClosureData = extern struct {
    lambda_idx: u32,
    captures_start: u32,
    captures_len: u32,
    tag_name: u32,
};

/// Zero-argument tag expression data.
/// Stores closure name, variant var, ext var, and tag name.
pub const ZeroArgTagData = extern struct {
    closure_name: u32,
    variant_var: u32,
    ext_var: u32,
    name: u32,
};

/// Definition data.
/// Stores pattern, expr, kind (2 u32s), and annotation index.
pub const DefData = extern struct {
    pattern: u32,
    expr: u32,
    kind_0: u32,
    kind_1: u32,
    anno_idx: u32, // 0 if no annotation
};

/// Import statement data.
/// Stores alias_tok, qualifier_tok, flags, and exposes span.
pub const ImportData = extern struct {
    alias_tok: u32, // @bitCast(Ident.Idx) or 0
    qualifier_tok: u32, // @bitCast(Ident.Idx) or 0
    flags: u32, // bit 0 = has_alias, bit 1 = has_qualifier
    exposes_start: u32,
    exposes_len: u32,
};

/// Type annotation apply data.
/// Stores args_len, base_tag, and base value(s).
pub const TypeApplyData = extern struct {
    args_len: u32,
    base_tag: u32, // LocalOrExternal.Tag
    value1: u32, // builtin_type, decl_idx, or module_idx
    value2: u32, // target_node_idx for external, 0 otherwise
};

/// Pattern list data.
/// Stores patterns span and optional rest pattern info.
pub const PatternListData = extern struct {
    patterns_start: u32,
    patterns_len: u32,
    has_rest: u32, // 0 or 1
    rest_index: u32, // only valid if has_rest=1
    has_pattern: u32, // only valid if has_rest=1, 0 or 1
    pattern_idx: u32, // only valid if has_rest=1 and has_pattern=1
};

/// Pattern string interpolation data.
/// Stores the literal prefix, span into pattern_str_interpolation_steps, and end mode.
pub const PatternStrInterpolationData = extern struct {
    prefix: u32,
    steps_start: u32,
    steps_len: u32,
    end: u32,
};

/// One step in a string interpolation pattern.
/// capture_plus_one is 0 for `${_}`, otherwise pattern index + 1.
pub const PatternStrInterpolationStepData = extern struct {
    capture_plus_one: u32,
    delimiter: u32,
};

/// Canonical ownership of a group of method where clauses by one rigid-var
/// declaration. The clause indices are stored in `index_data`.
pub const WhereClauseOwnerData = extern struct {
    rigid_var: u32,
    clauses_start: u32,
    clauses_len: u32,
    owned_by_annotation: bool,
    _padding: [3]u8 = .{ 0, 0, 0 },
};

const Scratch = struct {
    statements: base.Scratch(CIR.Statement.Idx),
    exprs: base.Scratch(CIR.Expr.Idx),
    record_fields: base.Scratch(CIR.RecordField.Idx),
    record_unset_fields: base.Scratch(CIR.UnsetField.Idx),
    match_branches: base.Scratch(CIR.Expr.Match.Branch.Idx),
    match_branch_patterns: base.Scratch(CIR.Expr.Match.BranchPattern.Idx),
    if_branches: base.Scratch(CIR.Expr.IfBranch.Idx),
    where_clauses: base.Scratch(CIR.WhereClause.Idx),
    patterns: base.Scratch(CIR.Pattern.Idx),
    record_destructs: base.Scratch(CIR.Pattern.RecordDestruct.Idx),
    type_annos: base.Scratch(CIR.TypeAnno.Idx),
    anno_record_fields: base.Scratch(CIR.TypeAnno.RecordField.Idx),
    exposed_items: base.Scratch(CIR.ExposedItem.Idx),
    defs: base.Scratch(CIR.Def.Idx),
    diagnostics: base.Scratch(CIR.Diagnostic.Idx),
    captures: base.Scratch(CIR.Expr.Capture.Idx),

    fn init(gpa: Allocator) Allocator.Error!*@This() {
        const ptr = try gpa.create(Scratch);
        errdefer gpa.destroy(ptr);

        var statements = try base.Scratch(CIR.Statement.Idx).init(gpa);
        errdefer statements.deinit();
        var exprs = try base.Scratch(CIR.Expr.Idx).init(gpa);
        errdefer exprs.deinit();
        var record_fields = try base.Scratch(CIR.RecordField.Idx).init(gpa);
        errdefer record_fields.deinit();
        var record_unset_fields = try base.Scratch(CIR.UnsetField.Idx).init(gpa);
        errdefer record_unset_fields.deinit();
        var match_branches = try base.Scratch(CIR.Expr.Match.Branch.Idx).init(gpa);
        errdefer match_branches.deinit();
        var match_branch_patterns = try base.Scratch(CIR.Expr.Match.BranchPattern.Idx).init(gpa);
        errdefer match_branch_patterns.deinit();
        var if_branches = try base.Scratch(CIR.Expr.IfBranch.Idx).init(gpa);
        errdefer if_branches.deinit();
        var where_clauses = try base.Scratch(CIR.WhereClause.Idx).init(gpa);
        errdefer where_clauses.deinit();
        var patterns = try base.Scratch(CIR.Pattern.Idx).init(gpa);
        errdefer patterns.deinit();
        var record_destructs = try base.Scratch(CIR.Pattern.RecordDestruct.Idx).init(gpa);
        errdefer record_destructs.deinit();
        var type_annos = try base.Scratch(CIR.TypeAnno.Idx).init(gpa);
        errdefer type_annos.deinit();
        var anno_record_fields = try base.Scratch(CIR.TypeAnno.RecordField.Idx).init(gpa);
        errdefer anno_record_fields.deinit();
        var exposed_items = try base.Scratch(CIR.ExposedItem.Idx).init(gpa);
        errdefer exposed_items.deinit();
        var defs = try base.Scratch(CIR.Def.Idx).init(gpa);
        errdefer defs.deinit();
        var diagnostics = try base.Scratch(CIR.Diagnostic.Idx).init(gpa);
        errdefer diagnostics.deinit();
        var captures = try base.Scratch(CIR.Expr.Capture.Idx).init(gpa);
        errdefer captures.deinit();

        ptr.* = .{
            .statements = statements,
            .exprs = exprs,
            .record_fields = record_fields,
            .record_unset_fields = record_unset_fields,
            .match_branches = match_branches,
            .match_branch_patterns = match_branch_patterns,
            .if_branches = if_branches,
            .where_clauses = where_clauses,
            .patterns = patterns,
            .record_destructs = record_destructs,
            .type_annos = type_annos,
            .anno_record_fields = anno_record_fields,
            .exposed_items = exposed_items,
            .defs = defs,
            .diagnostics = diagnostics,
            .captures = captures,
        };

        return ptr;
    }

    fn deinit(self: *@This(), gpa: Allocator) void {
        self.statements.deinit();
        self.exprs.deinit();
        self.record_fields.deinit();
        self.record_unset_fields.deinit();
        self.match_branches.deinit();
        self.match_branch_patterns.deinit();
        self.if_branches.deinit();
        self.where_clauses.deinit();
        self.patterns.deinit();
        self.record_destructs.deinit();
        self.type_annos.deinit();
        self.anno_record_fields.deinit();
        self.exposed_items.deinit();
        self.defs.deinit();
        self.diagnostics.deinit();
        self.captures.deinit();
        gpa.destroy(self);
    }
};

/// Initializes the NodeStore
pub fn init(gpa: Allocator) Allocator.Error!NodeStore {
    // TODO determine what capacity to use
    // maybe these should be moved to build/compile flags?
    return try NodeStore.initCapacity(gpa, 128);
}

/// Names of `T`'s backing-list fields in declaration order, for `T` either
/// `NodeStore` or `NodeStore.Serialized`: every field except the allocator and
/// the scratch buffers. Construction, `clone`, `deinit`, and the `Serialized`
/// conversions visit exactly these, so declaring a list in both structs is all
/// it takes to enroll it.
fn backingLists(comptime T: type) []const []const u8 {
    comptime {
        const fields = std.meta.fieldNames(T);
        var names: [fields.len - 2][]const u8 = undefined;
        var count: usize = 0;
        for (fields) |name| {
            if (std.mem.eql(u8, name, "gpa") or std.mem.eql(u8, name, "scratch")) continue;
            names[count] = name;
            count += 1;
        }
        const result = names;
        return &result;
    }
}

/// How many nodes a store holds per entry of each backing list: `initCapacity`
/// reserves `capacity / divisor` entries. Lists not named here start empty.
const nodes_per_list_entry = .{
    .nodes = 1,
    .regions = 1,
    .int128_values = 8,
    .literal_dispatch_plans = 8,
    .interpolation_data = 16,
    .span2_data = 4,
    .span_with_node_data = 4,
    .method_call_data = 8,
    .match_data = 8,
    .if_data = 8,
    .match_branch_data = 8,
    .closure_data = 16,
    .zero_arg_tag_data = 16,
    .def_data = 8,
    .import_data = 16,
    .type_apply_data = 16,
    .pattern_list_data = 16,
    .pattern_str_interpolation_data = 32,
    .pattern_str_interpolation_steps = 16,
    .where_clause_owners = 16,
    .index_data = 4,
};

/// A store whose backing lists are all empty and which has no scratch buffers.
fn initEmpty(gpa: Allocator) NodeStore {
    var store: NodeStore = undefined;
    store.gpa = gpa;
    store.scratch = null;
    inline for (comptime backingLists(NodeStore)) |name| @field(store, name) = .{};
    return store;
}

/// Initializes the NodeStore with a specified capacity.
pub fn initCapacity(gpa: Allocator, capacity: usize) Allocator.Error!NodeStore {
    var store = initEmpty(gpa);
    errdefer store.deinit();
    inline for (comptime std.meta.fieldNames(@TypeOf(nodes_per_list_entry))) |name| {
        @field(store, name) = try @FieldType(NodeStore, name).initCapacity(gpa, capacity / @field(nodes_per_list_entry, name));
    }
    store.scratch = try Scratch.init(gpa);
    return store;
}

/// Public function `clone`.
pub fn clone(self: *const NodeStore, gpa: Allocator) Allocator.Error!NodeStore {
    var cloned = initEmpty(gpa);
    errdefer cloned.deinit();
    inline for (comptime backingLists(NodeStore)) |name| {
        @field(cloned, name) = try @field(self, name).clone(gpa);
    }
    return cloned;
}

/// Deinitializes the NodeStore, freeing any allocated resources.
pub fn deinit(store: *NodeStore) void {
    inline for (comptime backingLists(NodeStore)) |name| @field(store, name).deinit(store.gpa);
    if (store.scratch) |scratch| {
        scratch.deinit(store.gpa);
    }
}

/// Compile-time constants for union variant counts to ensure we don't miss cases
/// when adding/removing variants from ModuleEnv unions. Update these when modifying the unions.
///
/// Count of the diagnostic nodes in the ModuleEnv
pub const MODULEENV_DIAGNOSTIC_NODE_COUNT = 95;
/// Count of the expression nodes in the ModuleEnv
pub const MODULEENV_EXPR_NODE_COUNT = 59;
/// Count of the statement nodes in the ModuleEnv
pub const MODULEENV_STATEMENT_NODE_COUNT = 21;
/// Count of the type annotation nodes in the ModuleEnv
pub const MODULEENV_TYPE_ANNO_NODE_COUNT = 12;
/// Count of the pattern nodes in the ModuleEnv
pub const MODULEENV_PATTERN_NODE_COUNT = 20;

comptime {
    // Check the number of CIR.Diagnostic nodes
    const diagnostic_fields = @typeInfo(CIR.Diagnostic).@"union".field_names;
    std.debug.assert(diagnostic_fields.len == MODULEENV_DIAGNOSTIC_NODE_COUNT);
}

comptime {
    // Check the number of CIR.Expr nodes
    const expr_fields = @typeInfo(CIR.Expr).@"union".field_names;
    std.debug.assert(expr_fields.len == MODULEENV_EXPR_NODE_COUNT);
}

comptime {
    // Check the number of CIR.Statement nodes
    const statement_fields = @typeInfo(CIR.Statement).@"union".field_names;
    std.debug.assert(statement_fields.len == MODULEENV_STATEMENT_NODE_COUNT);
}

comptime {
    // Check the number of CIR.TypeAnno nodes
    const type_anno_fields = @typeInfo(CIR.TypeAnno).@"union".field_names;
    std.debug.assert(type_anno_fields.len == MODULEENV_TYPE_ANNO_NODE_COUNT);
}

comptime {
    // Check the number of CIR.Pattern nodes
    const pattern_fields = @typeInfo(CIR.Pattern).@"union".field_names;
    std.debug.assert(pattern_fields.len == MODULEENV_PATTERN_NODE_COUNT);
}

/// Helper function to get a region by node index, handling the type conversion
pub fn getRegionAt(store: *const NodeStore, node_idx: Node.Idx) Region {
    const idx: Region.Idx = @fromBackingInt(@intCast(@backingInt(node_idx)));
    return store.regions.get(idx).*;
}

/// Move a node to another source region. A forward-reference placeholder is
/// created at the first use of a name and becomes that name's binder once the
/// declaration is reached, at which point it belongs at the declaration.
pub fn setRegionAt(store: *NodeStore, node_idx: Node.Idx, region: Region) void {
    const idx: Region.Idx = @fromBackingInt(@intCast(@backingInt(node_idx)));
    store.regions.set(idx, region);
}

fn literalDispatchKindForTag(tag: Node.Tag) ?LiteralDispatchPlan.Kind {
    const literal_tag = narrowNodeTag(LiteralNodeTag, tag) orelse return null;
    return switch (literal_tag) {
        .expr_num,
        .expr_frac_f32,
        .expr_frac_f64,
        .expr_dec,
        .expr_dec_small,
        .expr_num_from_numeral,
        .expr_typed_int,
        .expr_typed_frac,
        .expr_typed_num_from_numeral,
        .pattern_num_literal,
        .pattern_small_dec_literal,
        .pattern_dec_literal,
        .pattern_num_from_numeral_literal,
        => .numeral,
        .expr_string,
        .pattern_str_literal,
        => .quote,
    };
}

fn literalDispatchPlanPlusOne(node: Node) u32 {
    const payload = node.getPayload();
    const tag = narrowNodeTag(LiteralNodeTag, node.tag) orelse return 0;
    return switch (tag) {
        .expr_num => payload.expr_num.literal_dispatch_plan_plus_one,
        .expr_frac_f32 => payload.expr_frac_f32.literal_dispatch_plan_plus_one,
        .expr_frac_f64 => payload.expr_frac_f64.literal_dispatch_plan_plus_one,
        .expr_dec => payload.expr_dec.literal_dispatch_plan_plus_one,
        .expr_dec_small => payload.expr_dec_small.literal_dispatch_plan_plus_one,
        .expr_num_from_numeral => payload.expr_num_from_numeral.literal_dispatch_plan_plus_one,
        .expr_typed_int => payload.expr_typed_int.literal_dispatch_plan_plus_one,
        .expr_typed_frac => payload.expr_typed_frac.literal_dispatch_plan_plus_one,
        .expr_typed_num_from_numeral => payload.expr_typed_num_from_numeral.literal_dispatch_plan_plus_one,
        .expr_string => payload.expr_string.literal_dispatch_plan_plus_one,
        .pattern_num_literal => payload.pattern_num_literal.literal_dispatch_plan_plus_one,
        .pattern_small_dec_literal => payload.pattern_small_dec_literal.literal_dispatch_plan_plus_one,
        .pattern_dec_literal => payload.pattern_dec_literal.literal_dispatch_plan_plus_one,
        .pattern_num_from_numeral_literal => payload.pattern_num_from_numeral_literal.literal_dispatch_plan_plus_one,
        .pattern_str_literal => payload.pattern_str_literal.literal_dispatch_plan_plus_one,
    };
}

fn setLiteralDispatchPlanPlusOne(store: *NodeStore, node_idx: Node.Idx, plan_plus_one: u32) void {
    var node = store.nodes.get(node_idx);
    const payload = node.getPayload();
    const tag = narrowNodeTag(LiteralNodeTag, node.tag) orelse
        base.invariant("literal dispatch plan attached to non-literal node {s}", .{@tagName(node.tag)});
    switch (tag) {
        .expr_num => {
            var data = payload.expr_num;
            data.literal_dispatch_plan_plus_one = plan_plus_one;
            node.setPayload(.{ .expr_num = data });
        },
        .expr_frac_f32 => {
            var data = payload.expr_frac_f32;
            data.literal_dispatch_plan_plus_one = plan_plus_one;
            node.setPayload(.{ .expr_frac_f32 = data });
        },
        .expr_frac_f64 => {
            var data = payload.expr_frac_f64;
            data.literal_dispatch_plan_plus_one = plan_plus_one;
            node.setPayload(.{ .expr_frac_f64 = data });
        },
        .expr_dec => {
            var data = payload.expr_dec;
            data.literal_dispatch_plan_plus_one = plan_plus_one;
            node.setPayload(.{ .expr_dec = data });
        },
        .expr_dec_small => {
            var data = payload.expr_dec_small;
            data.literal_dispatch_plan_plus_one = plan_plus_one;
            node.setPayload(.{ .expr_dec_small = data });
        },
        .expr_num_from_numeral => {
            var data = payload.expr_num_from_numeral;
            data.literal_dispatch_plan_plus_one = plan_plus_one;
            node.setPayload(.{ .expr_num_from_numeral = data });
        },
        .expr_typed_int => {
            var data = payload.expr_typed_int;
            data.literal_dispatch_plan_plus_one = plan_plus_one;
            node.setPayload(.{ .expr_typed_int = data });
        },
        .expr_typed_frac => {
            var data = payload.expr_typed_frac;
            data.literal_dispatch_plan_plus_one = plan_plus_one;
            node.setPayload(.{ .expr_typed_frac = data });
        },
        .expr_typed_num_from_numeral => {
            var data = payload.expr_typed_num_from_numeral;
            data.literal_dispatch_plan_plus_one = plan_plus_one;
            node.setPayload(.{ .expr_typed_num_from_numeral = data });
        },
        .expr_string => {
            var data = payload.expr_string;
            data.literal_dispatch_plan_plus_one = plan_plus_one;
            node.setPayload(.{ .expr_string = data });
        },
        .pattern_num_literal => {
            var data = payload.pattern_num_literal;
            data.literal_dispatch_plan_plus_one = plan_plus_one;
            node.setPayload(.{ .pattern_num_literal = data });
        },
        .pattern_small_dec_literal => {
            var data = payload.pattern_small_dec_literal;
            data.literal_dispatch_plan_plus_one = plan_plus_one;
            node.setPayload(.{ .pattern_small_dec_literal = data });
        },
        .pattern_dec_literal => {
            var data = payload.pattern_dec_literal;
            data.literal_dispatch_plan_plus_one = plan_plus_one;
            node.setPayload(.{ .pattern_dec_literal = data });
        },
        .pattern_num_from_numeral_literal => {
            var data = payload.pattern_num_from_numeral_literal;
            data.literal_dispatch_plan_plus_one = plan_plus_one;
            node.setPayload(.{ .pattern_num_from_numeral_literal = data });
        },
        .pattern_str_literal => {
            var data = payload.pattern_str_literal;
            data.literal_dispatch_plan_plus_one = plan_plus_one;
            node.setPayload(.{ .pattern_str_literal = data });
        },
    }
    store.nodes.set(node_idx, node);
}

/// Attach or update the checked dispatch evidence owned by a literal node.
pub fn recordLiteralDispatchPlan(
    store: *NodeStore,
    node_idx: Node.Idx,
    kind: LiteralDispatchPlan.Kind,
    target_var: types.Var,
    fn_var: ?types.Var,
    pattern_failure_owner: ?u32,
) Allocator.Error!void {
    std.debug.assert(fn_var != null or (kind == .numeral and pattern_failure_owner == null));
    const node = store.nodes.get(node_idx);
    std.debug.assert(literalDispatchKindForTag(node.tag) == kind);
    std.debug.assert(narrowNodeTag(PatternNodeTag, node.tag) == null or pattern_failure_owner != null);

    const plan_plus_one = literalDispatchPlanPlusOne(node);
    const context_plus_one = if (plan_plus_one != 0)
        store.literal_dispatch_plans.get(@fromBackingInt(@intCast(plan_plus_one - 1))).pattern_context_plus_one
    else if (pattern_failure_owner) |owner|
        @backingInt(try store.literal_pattern_contexts.append(store.gpa, .{ .failure_owner = owner })) + 1
    else
        0;
    if (pattern_failure_owner) |owner| {
        std.debug.assert(context_plus_one != 0);
        store.literal_pattern_contexts.get(@fromBackingInt(@intCast(context_plus_one - 1))).failure_owner = owner;
    } else std.debug.assert(context_plus_one == 0);
    var plan = LiteralDispatchPlan{
        .node_idx = @backingInt(node_idx),
        .target_var = @backingInt(target_var),
        .fn_var_plus_one = if (fn_var) |var_| @backingInt(var_) + 1 else 0,
        .kind_and_resolution = LiteralDispatchPlan.packKindAndResolution(kind, .unresolved),
        .pattern_context_plus_one = context_plus_one,
    };
    if (plan_plus_one != 0) {
        plan.setResolution(store.literal_dispatch_plans.get(@fromBackingInt(@intCast(plan_plus_one - 1))).dispatchResolution());
        store.literal_dispatch_plans.set(@fromBackingInt(@intCast(plan_plus_one - 1)), plan);
        return;
    }

    const plan_idx = try store.literal_dispatch_plans.append(store.gpa, plan);
    setLiteralDispatchPlanPlusOne(store, node_idx, @backingInt(plan_idx) + 1);
}

/// Retain the exact equality constraint created for this literal pattern.
pub fn recordLiteralPatternEquality(store: *NodeStore, node_idx: Node.Idx, fn_var: types.Var) void {
    const plan = store.literalDispatchPlanForNode(node_idx) orelse base.invariant("literal pattern node {d} has no literal dispatch plan", .{@backingInt(node_idx)});
    std.debug.assert(plan.pattern_context_plus_one != 0);
    const context = store.literal_pattern_contexts.get(@fromBackingInt(@intCast(plan.pattern_context_plus_one - 1)));
    context.equality_fn_var_plus_one = @backingInt(fn_var) + 1;
}

/// Finalize the checker-owned resolution for a live literal plan. An
/// unresolved record is construction state; a second, different resolution
/// would mean checking produced contradictory evidence for one literal.
pub fn finalizeLiteralDispatchResolution(
    store: *NodeStore,
    node_idx: Node.Idx,
    resolution: LiteralDispatchPlan.Resolution,
) void {
    std.debug.assert(resolution != .unresolved);
    const node = store.nodes.get(node_idx);
    const plan_plus_one = literalDispatchPlanPlusOne(node);
    if (plan_plus_one == 0) return;

    var plan = store.literal_dispatch_plans.get(@fromBackingInt(@intCast(plan_plus_one - 1))).*;
    const previous = plan.dispatchResolution();
    if (previous != .unresolved and previous != resolution) {
        base.invariant(
            "literal dispatch plan for node {d} finalized twice ({s}, then {s})",
            .{ @backingInt(node_idx), @tagName(previous), @tagName(resolution) },
        );
    }
    plan.setResolution(resolution);
    store.literal_dispatch_plans.set(@fromBackingInt(@intCast(plan_plus_one - 1)), plan);
}

/// Return the checked dispatch evidence owned by a literal node, if any.
pub fn literalDispatchPlanForNode(store: *const NodeStore, node_idx: Node.Idx) ?LiteralDispatchPlan {
    const node = store.nodes.get(node_idx);
    const plan_plus_one = literalDispatchPlanPlusOne(node);
    if (plan_plus_one == 0) return null;
    const plan = store.literal_dispatch_plans.get(@fromBackingInt(@intCast(plan_plus_one - 1))).*;
    std.debug.assert(plan.node_idx == @backingInt(node_idx));
    std.debug.assert(plan.dispatchKind() == literalDispatchKindForTag(node.tag).?);
    return plan;
}

/// The dense set of live literal dispatch plans. Every record is owned by the
/// node named in `node_idx`; node replacement removes its record immediately.
pub fn literalDispatchPlans(store: *const NodeStore) []const LiteralDispatchPlan {
    if (@import("builtin").mode == .debug) {
        for (store.literal_dispatch_plans.items.items, 0..) |plan, plan_index| {
            const owner = store.nodes.get(@fromBackingInt(@intCast(plan.node_idx)));
            std.debug.assert(literalDispatchPlanPlusOne(owner) == @as(u32, @intCast(plan_index + 1)));
            std.debug.assert(literalDispatchKindForTag(owner.tag).? == plan.dispatchKind());
        }
    }
    return store.literal_dispatch_plans.items.items;
}

/// Retire and return the literal plan owned by `node_idx`, if any. Error
/// recovery calls this for every expression discarded with a replaced subtree,
/// so no live plan can outlive the source node that would execute it.
pub fn retireLiteralDispatchPlan(store: *NodeStore, node_idx: Node.Idx) ?LiteralDispatchPlan {
    const node = store.nodes.get(node_idx);
    const plan_plus_one = literalDispatchPlanPlusOne(node);
    if (plan_plus_one == 0) return null;

    const plan_index: usize = plan_plus_one - 1;
    const plans = &store.literal_dispatch_plans.items;
    std.debug.assert(plans.items[plan_index].node_idx == @backingInt(node_idx));
    const retired = plans.items[plan_index];
    const last_index = plans.items.len - 1;
    setLiteralDispatchPlanPlusOne(store, node_idx, 0);
    if (plan_index != last_index) {
        const moved = plans.items[last_index];
        plans.items[plan_index] = moved;
        setLiteralDispatchPlanPlusOne(store, @fromBackingInt(@intCast(moved.node_idx)), @intCast(plan_index + 1));
    }
    _ = plans.pop();
    return retired;
}

/// Helper function to get a region by pattern index
pub fn getPatternRegion(store: *const NodeStore, pattern_idx: CIR.Pattern.Idx) Region {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(pattern_idx)));
    return store.getRegionAt(node_idx);
}

/// Helper function to get a region by expression index
pub fn getExprRegion(store: *const NodeStore, expr_idx: CIR.Expr.Idx) Region {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(expr_idx)));
    return store.getRegionAt(node_idx);
}

/// Returns the exact source region of one field-access path segment.
pub fn getFieldAccessSegmentRegion(store: *const NodeStore, segment_idx: CIR.Expr.FieldAccessSegment.Idx) Region {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(segment_idx)));
    std.debug.assert(store.nodes.get(node_idx).tag == .field_access_segment);
    return store.getRegionAt(node_idx);
}

/// Helper function to get a region by statement index
pub fn getStatementRegion(store: *const NodeStore, stmt_idx: CIR.Statement.Idx) Region {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(stmt_idx)));
    return store.getRegionAt(node_idx);
}

/// Helper function to get a region by type annotation index
pub fn getTypeAnnoRegion(store: *const NodeStore, type_anno_idx: CIR.TypeAnno.Idx) Region {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(type_anno_idx)));
    return store.getRegionAt(node_idx);
}

/// Helper function to get a region by  annotation index
pub fn getAnnotationRegion(store: *const NodeStore, anno_idx: CIR.Annotation.Idx) Region {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(anno_idx)));
    return store.getRegionAt(node_idx);
}

/// Retrieves a region from node from the store.
pub fn getNodeRegion(store: *const NodeStore, node_idx: Node.Idx) Region {
    return store.getRegionAt(node_idx);
}

fn addMethodCallData(store: *NodeStore, args: CIR.Expr.Span, method_name_region: Region, surface_origin: CIR.Expr.SurfaceOrigin) Allocator.Error!u32 {
    const data_idx: u32 = @intCast(store.method_call_data.len());
    _ = try store.method_call_data.append(store.gpa, .{
        .args_start = args.span.start,
        .args_len = args.span.len,
        .method_region_start = method_name_region.start.offset,
        .method_region_end = method_name_region.end.offset,
        .surface_origin = encodeSurfaceOrigin(surface_origin),
    });
    return data_idx;
}

// Bidirectional u32 encoding for `CIR.Expr.SurfaceOrigin` in `MethodCallData`.
// Binop ops are offset past the two unit tags so every `Binop.Op` value has
// a distinct slot.
const surface_origin_binop_offset: u32 = 2;
comptime {
    // The binop range starts after the unit (payload-less) tags; keep the offset
    // in sync so a new unit variant can't collide with a `Binop.Op` slot.
    var unit_count: u32 = 0;
    for (@typeInfo(CIR.Expr.SurfaceOrigin).@"union".field_types) |field_type| {
        if (field_type == void) unit_count += 1;
    }
    std.debug.assert(surface_origin_binop_offset == unit_count);
}

/// Encode surface origin
pub fn encodeSurfaceOrigin(origin: CIR.Expr.SurfaceOrigin) u32 {
    return switch (origin) {
        .method_call => 0,
        .unary_minus => 1,
        .binop => |op| surface_origin_binop_offset + @as(u32, @backingInt(op)),
    };
}

/// Decode surface origin
pub fn decodeSurfaceOrigin(encoded: u32) CIR.Expr.SurfaceOrigin {
    return switch (encoded) {
        0 => .method_call,
        1 => .unary_minus,
        else => .{ .binop = @fromBackingInt(@intCast(encoded - surface_origin_binop_offset)) },
    };
}

fn getMethodCallSurfaceOrigin(store: *const NodeStore, data_idx: u32) CIR.Expr.SurfaceOrigin {
    return decodeSurfaceOrigin(store.method_call_data.items.items[data_idx].surface_origin);
}

fn getMethodCallArgs(store: *const NodeStore, data_idx: u32) CIR.Expr.Span {
    const data = store.method_call_data.items.items[data_idx];
    return .{ .span = .{
        .start = data.args_start,
        .len = data.args_len,
    } };
}

fn getMethodNameRegion(store: *const NodeStore, data_idx: u32) Region {
    const data = store.method_call_data.items.items[data_idx];
    return .{
        .start = .{ .offset = data.method_region_start },
        .end = .{ .offset = data.method_region_end },
    };
}

/// Retrieves a statement node from the store.
pub fn getStatement(store: *const NodeStore, statement: CIR.Statement.Idx) CIR.Statement {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(statement)));
    return @call(.always_inline, statementFromNode, .{ store, store.nodes.get(node_idx) });
}

/// Retrieves a statement as written in source, reading through a runtime
/// error put in its place.
pub fn getSourceStatement(store: *const NodeStore, statement: CIR.Statement.Idx) CIR.Statement {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(statement)));
    return store.statementFromNode(store.sourceNode(node_idx));
}

fn statementFromNode(store: *const NodeStore, node: Node) CIR.Statement {
    const payload = node.getPayload();

    const tag = narrowNodeTag(StatementNodeTag, node.tag) orelse
        base.invariant("unreachable, node is not a statement tag: {}", .{node.tag});
    switch (tag) {
        .statement_decl => {
            const p = payload.statement_decl;
            return CIR.Statement{ .s_decl = .{
                .pattern = @fromBackingInt(@intCast(p.pattern)),
                .expr = @fromBackingInt(@intCast(p.expr)),
                .anno = blk: {
                    const anno_data = store.span2_data.items.items[p.anno_span2_idx];
                    if (anno_data.start != 0) {
                        break :blk @as(CIR.Annotation.Idx, @fromBackingInt(@intCast(anno_data.len)));
                    } else {
                        break :blk null;
                    }
                },
            } };
        },
        .statement_var => {
            const p = payload.statement_var;
            return CIR.Statement{ .s_var = .{
                .pattern_idx = @fromBackingInt(@intCast(p.pattern_idx)),
                .expr = @fromBackingInt(@intCast(p.expr)),
                .anno = blk: {
                    const anno_data = store.span2_data.items.items[p.anno_span2_idx];
                    if (anno_data.start != 0) {
                        break :blk @fromBackingInt(@intCast(anno_data.len));
                    } else {
                        break :blk null;
                    }
                },
            } };
        },
        .statement_var_uninitialized => {
            const p = payload.statement_var_uninitialized;
            return CIR.Statement{ .s_var_uninitialized = .{
                .pattern_idx = @fromBackingInt(@intCast(p.pattern_idx)),
                .anno = blk: {
                    const anno_data = store.span2_data.items.items[p.anno_span2_idx];
                    if (anno_data.start != 0) {
                        break :blk @fromBackingInt(@intCast(anno_data.len));
                    } else {
                        break :blk null;
                    }
                },
            } };
        },
        .statement_expect => {
            const p = payload.statement_single_expr;
            return CIR.Statement{ .s_expect = .{
                .body = @fromBackingInt(@intCast(p.expr)),
            } };
        },
        .statement_import => {
            const p = payload.statement_import;
            const import = store.import_data.items.items[p.import_data_idx];

            const alias_tok = if (import.flags & 1 != 0) @as(?Ident.Idx, @bitCast(import.alias_tok)) else null;
            const qualifier_tok = if (import.flags & 2 != 0) @as(?Ident.Idx, @bitCast(import.qualifier_tok)) else null;

            return CIR.Statement{
                .s_import = .{
                    .module_name_tok = @bitCast(p.module_name_tok),
                    .qualifier_tok = qualifier_tok,
                    .alias_tok = alias_tok,
                    .exposes = DataSpan.init(import.exposes_start, import.exposes_len).as(CIR.ExposedItem.Span),
                },
            };
        },
        .statement_where_alias_decl => {
            const p = payload.statement_where_alias_decl;
            return CIR.Statement{
                .s_where_alias_decl = .{
                    .header = @fromBackingInt(@intCast(p.header)),
                    .receiver = @fromBackingInt(@intCast(p.receiver)),
                    .where = store.loadWhereClauseSpan(p.where_span_idx),
                },
            };
        },
        .statement_type_anno => {
            const p = payload.statement_type_anno;

            const where_clause = if (p.where_span2_idx_plus_one != 0)
                store.loadWhereClauseSpan(p.where_span2_idx_plus_one - 1)
            else
                null;

            return CIR.Statement{
                .s_type_anno = .{
                    .name = @bitCast(p.name),
                    .anno = @fromBackingInt(@intCast(p.anno)),
                    .where = where_clause,
                },
            };
        },
        inline .statement_reassign,
        .statement_crash,
        .statement_dbg,
        .statement_expr,
        .statement_for,
        .statement_while,
        .statement_infinite_loop,
        .statement_breakable_loop,
        .statement_break,
        .statement_return,
        .statement_alias_decl,
        .statement_nominal_decl,
        .statement_type_var_alias,
        .malformed,
        => |node_tag| return copiedValue(CIR.Statement, &copied_statement_shapes, node_tag, payload),
    }
}

/// Retrieves an expression node from the store.
pub fn getExpr(store: *const NodeStore, expr: CIR.Expr.Idx) CIR.Expr {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(expr)));
    return @call(.always_inline, exprFromNode, .{ store, node_idx, store.nodes.get(node_idx) });
}

/// Retrieves an expression as written in source, reading through a runtime
/// error put in its place.
pub fn getSourceExpr(store: *const NodeStore, expr: CIR.Expr.Idx) CIR.Expr {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(expr)));
    return store.exprFromNode(node_idx, store.sourceNode(node_idx));
}

fn exprFromNode(store: *const NodeStore, node_idx: Node.Idx, node: Node) CIR.Expr {
    const payload = node.getPayload();

    const tag = narrowNodeTag(ExprNodeTag, node.tag) orelse
        base.invariant("unreachable, node is not an expression tag: {}", .{node.tag});
    switch (tag) {
        .expr_external_lookup => {
            const p = payload.expr_external_lookup;
            // Handle external lookups
            return CIR.Expr{ .e_lookup_external = .{
                .module_idx = @fromBackingInt(@intCast(p.module_idx)),
                .target_node_idx = @intCast(p.target_node_idx),
                .ident_idx = @bitCast(p.ident_idx),
                .region = store.getRegionAt(node_idx),
            } };
        },
        .expr_deferred_import_ref => {
            const p = payload.expr_deferred_import_ref;
            return CIR.Expr{ .e_deferred_import_ref = .{
                .ref = @fromBackingInt(@intCast(p.ref)),
                .backing = null,
            } };
        },
        .expr_deferred_nominal_external => {
            const p = payload.expr_deferred_nominal_external;
            const backing = store.span2_data.items.items[p.backing_span2_idx];
            return CIR.Expr{ .e_deferred_import_ref = .{
                .ref = @fromBackingInt(@intCast(p.ref)),
                .backing = .{
                    .expr = @fromBackingInt(@intCast(backing.start)),
                    .ty = @fromBackingInt(@intCast(backing.len)),
                },
            } };
        },
        .expr_num => {
            const p = payload.expr_num;
            const kind: CIR.NumKind = @fromBackingInt(@intCast(p.kind));
            const val_kind: CIR.IntValue.IntKind = @fromBackingInt(@intCast(p.val_kind));
            const value = store.int128_values.items.items[p.int128_idx];

            return CIR.Expr{
                .e_num = .{
                    .value = .{ .bytes = @bitCast(value), .kind = val_kind },
                    .kind = kind,
                },
            };
        },
        .expr_call => {
            const p = payload.expr_call;
            // Retrieve args span from span2_data
            const args_span = store.span2_data.items.items[p.args_span2_idx];

            return CIR.Expr{
                .e_call = .{
                    .func = @fromBackingInt(@intCast(p.func)),
                    .args = .{ .span = .{ .start = args_span.start, .len = args_span.len } },
                    .called_via = @fromBackingInt(@intCast(p.called_via)),
                    .constraint_fn_var = if (p.constraint_fn_var_plus_one == 0)
                        null
                    else
                        @fromBackingInt(@intCast(p.constraint_fn_var_plus_one - 1)),
                },
            };
        },
        .expr_frac_f64 => {
            const p = payload.expr_frac_f64;
            const raw: [2]u32 = .{ p.value_lo, p.value_hi };

            return CIR.Expr{ .e_frac_f64 = .{
                .value = @bitCast(raw),
                .has_suffix = p.has_suffix,
            } };
        },
        .expr_dec => {
            const p = payload.expr_dec;
            const value = store.int128_values.items.items[p.int128_idx];

            return CIR.Expr{
                .e_dec = .{
                    .value = RocDec{ .num = value },
                    .has_suffix = p.has_suffix,
                },
            };
        },
        .expr_dec_small => {
            const p = payload.expr_dec_small;
            // Unpack small dec data
            const numerator = @as(i16, @intCast(@as(i32, @bitCast(p.numerator))));
            const denominator_power_of_ten = @as(u8, @truncate(p.denom_power));

            return CIR.Expr{
                .e_dec_small = .{
                    .value = .{
                        .numerator = numerator,
                        .denominator_power_of_ten = denominator_power_of_ten,
                    },
                    .has_suffix = p.has_suffix,
                },
            };
        },
        .expr_typed_int => {
            const p = payload.expr_typed_int;
            const value = store.int128_values.items.items[p.int128_idx];
            return CIR.Expr{
                .e_typed_int = .{
                    .value = .{
                        .bytes = @bitCast(value),
                        .kind = @fromBackingInt(@intCast(p.val_kind)),
                    },
                    .type_name = @bitCast(p.type_name),
                },
            };
        },
        .expr_typed_frac => {
            const p = payload.expr_typed_frac;
            const value = store.int128_values.items.items[p.int128_idx];
            return CIR.Expr{
                .e_typed_frac = .{
                    .value = .{
                        .bytes = @bitCast(value),
                        .kind = @fromBackingInt(@intCast(p.val_kind)),
                    },
                    .type_name = @bitCast(p.type_name),
                },
            };
        },
        .expr_string_segment => {
            const p = payload.expr_string_segment;
            return CIR.Expr.initStrSegment(@fromBackingInt(@intCast(p.segment_idx)));
        },
        .expr_bytes_literal => {
            const p = payload.expr_string_segment;
            return .{ .e_bytes_literal = .{ .literal = @fromBackingInt(@intCast(p.segment_idx)) } };
        },
        .expr_string => {
            const p = payload.expr_string;
            return CIR.Expr.initStr(DataSpan.init(p.segments_start, p.segments_len).as(CIR.Expr.Span));
        },
        .expr_nominal_external => {
            const p = payload.expr_nominal_external;
            // Retrieve backing data from span2_data
            const backing = store.span2_data.items.items[p.backing_span2_idx];

            return CIR.Expr{
                .e_nominal_external = .{
                    .module_idx = @fromBackingInt(@intCast(p.module_idx)),
                    .target_node_idx = @intCast(p.target_node_idx),
                    .backing_expr = @fromBackingInt(@intCast(backing.start)),
                    .backing_type = @fromBackingInt(@intCast(backing.len)),
                },
            };
        },
        .expr_closure => {
            const p = payload.expr_closure;
            // Retrieve closure data from closure_data list
            const cd = store.closure_data.items.items[p.closure_data_idx];

            return CIR.Expr{
                .e_closure = .{
                    .lambda_idx = @fromBackingInt(@intCast(cd.lambda_idx)),
                    .captures = .{ .span = .{ .start = cd.captures_start, .len = cd.captures_len } },
                    .tag_name = @bitCast(cd.tag_name),
                },
            };
        },
        .expr_record => {
            const p = payload.expr_record;
            // Retrieve fields span and ext from span_with_node_data
            const fields_ext = store.span_with_node_data.items.items[p.fields_ext_idx];
            const ext = if (fields_ext.node == 0) null else @as(CIR.Expr.Idx, @fromBackingInt(@intCast(fields_ext.node)));
            const unsets = store.span2_data.items.items[p.unsets_span2_idx];

            return CIR.Expr{
                .e_record = .{
                    .fields = .{ .span = .{ .start = fields_ext.start, .len = fields_ext.len } },
                    .unsets = .{ .span = .{ .start = unsets.start, .len = unsets.len } },
                    .ext = ext,
                },
            };
        },
        .expr_match => {
            const p = payload.expr_match;
            // Retrieve match data from match_data list
            const md = store.match_data.items.items[p.match_data_idx];

            return CIR.Expr{
                .e_match = CIR.Expr.Match{
                    .cond = @fromBackingInt(@intCast(md.cond)),
                    .branches = .{ .span = .{ .start = md.branches_start, .len = md.branches_len } },
                    .exhaustive = @fromBackingInt(@intCast(md.exhaustive)),
                    .is_try_suffix = md.is_try_suffix != 0,
                    .skip_exhaustiveness = md.skip_exhaustiveness != 0,
                },
            };
        },
        .expr_zero_argument_tag => {
            const p = payload.expr_zero_argument_tag;
            // Retrieve zero-arg tag data from zero_arg_tag_data list
            const zat = store.zero_arg_tag_data.items.items[p.zero_arg_tag_idx];

            return CIR.Expr{
                .e_zero_argument_tag = .{
                    .closure_name = @bitCast(zat.closure_name),
                    .variant_var = @fromBackingInt(@intCast(zat.variant_var)),
                    .ext_var = @fromBackingInt(@intCast(zat.ext_var)),
                    .name = @bitCast(zat.name),
                },
            };
        },
        .expr_hosted_lambda => {
            const p = payload.expr_hosted_lambda;
            const args_span = store.span2_data.items.items[p.args_span2_idx];

            return CIR.Expr{ .e_hosted_lambda = .{
                .symbol_name = @bitCast(p.symbol_name),
                .args = .{ .span = .{ .start = args_span.start, .len = args_span.len } },
            } };
        },
        .expr_run_low_level => {
            const p = payload.expr_run_low_level;
            const args_span = store.span2_data.items.items[p.args_span2_idx];

            return CIR.Expr{ .e_run_low_level = .{
                .op = @fromBackingInt(@intCast(p.op)),
                .args = .{ .span = .{ .start = args_span.start, .len = args_span.len } },
            } };
        },
        .expr_if_then_else => {
            const p = payload.expr_if_then_else;
            const if_data = store.if_data.items.items[p.branches_else_idx];

            return CIR.Expr{ .e_if = .{
                .branches = .{ .span = .{ .start = if_data.branches_start, .len = if_data.branches_len } },
                .final_else = @fromBackingInt(if_data.final_else),
                .origin = @fromBackingInt(if_data.origin),
            } };
        },
        .expr_field_access => {
            const p = payload.expr_field_access;
            return CIR.Expr{ .e_field_access = .{
                .receiver = @fromBackingInt(@intCast(p.receiver)),
                .segments = .{
                    .start = @fromBackingInt(@intCast(p.segments_start)),
                    .len = p.segments_len,
                },
            } };
        },
        .expr_method_call => {
            const p = payload.expr_method_call;
            return CIR.Expr{ .e_method_call = .{
                .receiver = @fromBackingInt(@intCast(p.receiver)),
                .method_name = @bitCast(p.method_name),
                .method_name_region = store.getMethodNameRegion(p.method_call_data_idx),
                .args = store.getMethodCallArgs(p.method_call_data_idx),
            } };
        },
        .expr_dispatch_call => {
            const p = payload.expr_dispatch_call;
            return CIR.Expr{ .e_dispatch_call = .{
                .receiver = @fromBackingInt(@intCast(p.receiver)),
                .method_name = @bitCast(p.method_name),
                .method_name_region = store.getMethodNameRegion(p.method_call_data_idx),
                .args = store.getMethodCallArgs(p.method_call_data_idx),
                .constraint_fn_var = @fromBackingInt(@intCast(p.constraint_fn_var)),
                .surface_origin = store.getMethodCallSurfaceOrigin(p.method_call_data_idx),
            } };
        },
        .expr_interpolation => {
            const p = payload.expr_interpolation;
            const data = store.interpolation_data.items.items[p.interpolation_data_idx];
            return CIR.Expr{ .e_interpolation = .{
                .first = @fromBackingInt(@intCast(p.first)),
                .parts = .{ .span = .{
                    .start = data.parts_start,
                    .len = data.parts_len,
                } },
                .method_name_region = base.Region{
                    .start = .{ .offset = data.method_region_start },
                    .end = .{ .offset = data.method_region_end },
                },
                .constraint_fn_var = if (data.constraint_fn_var_plus_one == 0)
                    null
                else
                    @fromBackingInt(data.constraint_fn_var_plus_one - 1),
                .assembler_fn_var = if (data.assembler_fn_var_plus_one == 0)
                    null
                else
                    @fromBackingInt(data.assembler_fn_var_plus_one - 1),
                .dispatcher_var = if (data.dispatcher_var_plus_one == 0)
                    null
                else
                    @fromBackingInt(@intCast(data.dispatcher_var_plus_one - 1)),
            } };
        },
        .expr_type_method_call => {
            const p = payload.expr_type_method_call;
            return CIR.Expr{ .e_type_method_call = .{
                .type_dispatch_stmt = @fromBackingInt(@intCast(p.type_dispatch_stmt)),
                .method_name = @bitCast(p.method_name),
                .method_name_region = store.getMethodNameRegion(p.method_call_data_idx),
                .args = store.getMethodCallArgs(p.method_call_data_idx),
            } };
        },
        .expr_type_dispatch_call, .expr_type_dispatch_call_dispatcher => {
            const p = payload.expr_type_dispatch_call;
            return CIR.Expr{ .e_type_dispatch_call = .{
                .owner = if (tag == .expr_type_dispatch_call)
                    .{ .statement = @fromBackingInt(@intCast(p.owner)) }
                else
                    .{ .dispatcher = @fromBackingInt(@intCast(p.owner)) },
                .method_name = @bitCast(p.method_name),
                .method_name_region = store.getMethodNameRegion(p.method_call_data_idx),
                .args = store.getMethodCallArgs(p.method_call_data_idx),
                .constraint_fn_var = @fromBackingInt(@intCast(p.constraint_fn_var)),
            } };
        },
        inline .expr_var,
        .expr_associated_lookup_local,
        .expr_associated_lookup,
        .expr_associated_lookup_resolved,
        .expr_required_lookup,
        .expr_list,
        .expr_tuple,
        .expr_tuple_access,
        .expr_frac_f32,
        .expr_num_from_numeral,
        .expr_typed_num_from_numeral,
        .expr_tag,
        .expr_nominal,
        .expr_bin_op,
        .expr_lambda,
        .expr_block,
        .expr_empty_record,
        .expr_empty_list,
        .expr_crash,
        .expr_dbg,
        .expr_expect_err,
        .expr_unary_minus,
        .expr_ellipsis,
        .expr_anno_only,
        .expr_derived_method,
        .expr_return,
        .expr_break,
        .expr_expect,
        .expr_for,
        .expr_structural_eq,
        .expr_structural_hash,
        .expr_method_eq,
        .malformed,
        => |node_tag| return copiedValue(CIR.Expr, &copied_expr_shapes, node_tag, payload),
    }
}

/// Replaces an associated lookup whose checked target is a method declared in
/// a function body with a lookup of that method's local binding.
pub fn replaceExprWithLocalLookup(
    store: *NodeStore,
    expr_idx: CIR.Expr.Idx,
    pattern_idx: CIR.Pattern.Idx,
) void {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(expr_idx)));
    var node = Node.init(.expr_var);
    node.setPayload(.{ .expr_var = .{
        .pattern_idx = @backingInt(pattern_idx),
    } });
    store.nodes.set(node_idx, node);
}

/// Replaces an imported-type associated lookup with its exact checked target.
pub fn replaceExprWithResolvedAssociatedLookup(
    store: *NodeStore,
    expr_idx: CIR.Expr.Idx,
    module_identity: base.ModuleIdentity.Idx,
    target_node_idx: CIR.Node.Idx,
    target_def_idx: CIR.Def.Idx,
    source_ident: Ident.Idx,
) void {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(expr_idx)));
    var node = Node.init(.expr_associated_lookup_resolved);
    node.setPayload(.{ .expr_associated_lookup_resolved = .{
        .module_identity = @backingInt(module_identity),
        .target_node_idx = @backingInt(target_node_idx),
        .target_def_idx = @backingInt(target_def_idx),
        .source_ident = @bitCast(source_ident),
    } });
    store.nodes.set(node_idx, node);
}

/// Rewrites a deferred import reference expression into an external lookup.
pub fn resolveDeferredExprToExternalLookup(
    store: *NodeStore,
    expr_idx: CIR.Expr.Idx,
    module_idx: CIR.Import.Idx,
    target_node_idx: u32,
    ident_idx: Ident.Idx,
) void {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(expr_idx)));
    var node = Node.init(.expr_external_lookup);
    node.setPayload(.{ .expr_external_lookup = .{
        .module_idx = @backingInt(module_idx),
        .target_node_idx = target_node_idx,
        .ident_idx = @bitCast(ident_idx),
    } });
    store.nodes.set(node_idx, node);
}

/// Rewrites a deferred file import into the text the file holds.
pub fn resolveDeferredExprToStringSegment(
    store: *NodeStore,
    expr_idx: CIR.Expr.Idx,
    literal: base.StringLiteral.Idx,
) void {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(expr_idx)));
    var node = Node.init(.expr_string_segment);
    node.setPayload(.{ .expr_string_segment = .{ .segment_idx = @backingInt(literal) } });
    store.nodes.set(node_idx, node);
}

/// Rewrites a deferred file import into the raw bytes the file holds.
pub fn resolveDeferredExprToBytesLiteral(
    store: *NodeStore,
    expr_idx: CIR.Expr.Idx,
    literal: base.StringLiteral.Idx,
) void {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(expr_idx)));
    var node = Node.init(.expr_bytes_literal);
    node.setPayload(.{ .expr_string_segment = .{ .segment_idx = @backingInt(literal) } });
    store.nodes.set(node_idx, node);
}

/// Rewrites a deferred import reference expression into an associated lookup
/// on an imported type declaration.
pub fn resolveDeferredExprToAssociatedLookup(
    store: *NodeStore,
    expr_idx: CIR.Expr.Idx,
    module_idx: CIR.Import.Idx,
    type_node_idx: u32,
    type_ident: Ident.Idx,
    item_ident: Ident.Idx,
) void {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(expr_idx)));
    var node = Node.init(.expr_associated_lookup);
    node.setPayload(.{ .expr_associated_lookup = .{
        .module_idx = @backingInt(module_idx),
        .type_node_idx = type_node_idx,
        .type_ident = @bitCast(type_ident),
        .item_ident = @bitCast(item_ident),
    } });
    store.nodes.set(node_idx, node);
}

/// Rewrites a deferred import reference expression into an imported nominal
/// type applied to the backing expression the source wrote.
pub fn resolveDeferredExprToNominalExternal(
    store: *NodeStore,
    expr_idx: CIR.Expr.Idx,
    module_idx: CIR.Import.Idx,
    target_node_idx: u32,
    backing_expr: CIR.Expr.Idx,
    backing_type: CIR.Expr.NominalBackingType,
) Allocator.Error!void {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(expr_idx)));
    const backing_span2_idx: u32 = @intCast(store.span2_data.len());
    _ = try store.span2_data.append(store.gpa, .{
        .start = @backingInt(backing_expr),
        .len = @backingInt(backing_type),
    });
    var node = Node.init(.expr_nominal_external);
    node.setPayload(.{ .expr_nominal_external = .{
        .module_idx = @backingInt(module_idx),
        .target_node_idx = target_node_idx,
        .backing_span2_idx = backing_span2_idx,
    } });
    store.nodes.set(node_idx, node);
}

/// Rewrites a deferred import reference pattern into an imported nominal type
/// matched against the backing pattern the source wrote.
pub fn resolveDeferredPatternToNominalExternal(
    store: *NodeStore,
    pattern_idx: CIR.Pattern.Idx,
    module_idx: CIR.Import.Idx,
    target_node_idx: u32,
    backing_pattern: CIR.Pattern.Idx,
    backing_type: CIR.Expr.NominalBackingType,
) Allocator.Error!void {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(pattern_idx)));
    const backing_span2_idx: u32 = @intCast(store.span2_data.len());
    _ = try store.span2_data.append(store.gpa, .{
        .start = @backingInt(backing_pattern),
        .len = @backingInt(backing_type),
    });
    var node = Node.init(.pattern_nominal_external);
    node.setPayload(.{ .pattern_nominal_external = .{
        .module_idx = @backingInt(module_idx),
        .target_node_idx = target_node_idx,
        .backing_span2_idx = backing_span2_idx,
    } });
    store.nodes.set(node_idx, node);
}

/// Rewrites the pending base of a type-annotation lookup (`ty_lookup`) into
/// the external declaration the drain resolved it to.
pub fn resolveDeferredTypeAnnoLookupBase(
    store: *NodeStore,
    anno_idx: CIR.TypeAnno.Idx,
    module_idx: CIR.Import.Idx,
    target_node_idx: u32,
) void {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(anno_idx)));
    var node = store.nodes.get(node_idx);
    var p = node.getPayload().ty_lookup;
    store.span2_data.items.items[p.base_span2_idx] = .{
        .start = @backingInt(module_idx),
        .len = target_node_idx,
    };
    p.base = @backingInt(CIR.TypeAnno.LocalOrExternal.Tag.external);
    node.setPayload(.{ .ty_lookup = p });
    store.nodes.set(node_idx, node);
}

/// Rewrites the pending base of a type-annotation application (`ty_apply`)
/// into the external declaration the drain resolved it to.
pub fn resolveDeferredTypeAnnoApplyBase(
    store: *NodeStore,
    anno_idx: CIR.TypeAnno.Idx,
    module_idx: CIR.Import.Idx,
    target_node_idx: u32,
) void {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(anno_idx)));
    const node = store.nodes.get(node_idx);
    const p = node.getPayload().ty_apply;
    const apply_data = &store.type_apply_data.items.items[p.type_apply_data_idx];
    apply_data.base_tag = @backingInt(CIR.TypeAnno.LocalOrExternal.Tag.external);
    apply_data.value1 = @backingInt(module_idx);
    apply_data.value2 = target_node_idx;
}

/// Rewrites the pending base of a type-annotation lookup (`ty_lookup`) into a
/// declaration in the module the drain reached by following an exposed alias.
pub fn resolveDeferredTypeAnnoLookupBaseIdentity(
    store: *NodeStore,
    anno_idx: CIR.TypeAnno.Idx,
    module_identity: base.ModuleIdentity.Idx,
    target_node_idx: u32,
) void {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(anno_idx)));
    var node = store.nodes.get(node_idx);
    var p = node.getPayload().ty_lookup;
    store.span2_data.items.items[p.base_span2_idx] = .{
        .start = @backingInt(module_identity),
        .len = target_node_idx,
    };
    p.base = @backingInt(CIR.TypeAnno.LocalOrExternal.Tag.external_identity);
    node.setPayload(.{ .ty_lookup = p });
    store.nodes.set(node_idx, node);
}

/// Rewrites the pending base of a type-annotation application (`ty_apply`)
/// into a declaration in the module the drain reached by following an exposed
/// alias.
pub fn resolveDeferredTypeAnnoApplyBaseIdentity(
    store: *NodeStore,
    anno_idx: CIR.TypeAnno.Idx,
    module_identity: base.ModuleIdentity.Idx,
    target_node_idx: u32,
) void {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(anno_idx)));
    const node = store.nodes.get(node_idx);
    const p = node.getPayload().ty_apply;
    const apply_data = &store.type_apply_data.items.items[p.type_apply_data_idx];
    apply_data.base_tag = @backingInt(CIR.TypeAnno.LocalOrExternal.Tag.external_identity);
    apply_data.value1 = @backingInt(module_identity);
    apply_data.value2 = target_node_idx;
}

/// Replaces a type annotation with a runtime error node in place.
pub fn replaceTypeAnnoWithRuntimeError(
    store: *NodeStore,
    anno_idx: CIR.TypeAnno.Idx,
    diagnostic_idx: CIR.Diagnostic.Idx,
) void {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(anno_idx)));
    var node = Node.init(.ty_malformed);
    node.setPayload(.{ .ty_malformed = .{
        .diagnostic = @backingInt(diagnostic_idx),
    } });
    store.nodes.set(node_idx, node);
}

/// Replaces an existing expression with an explicit structural equality node.
/// This is used when the checker has already decided that equality is structural
/// rather than an attached method dispatch.
pub fn replaceExprWithStructuralEq(
    store: *NodeStore,
    expr_idx: CIR.Expr.Idx,
    lhs: CIR.Expr.Idx,
    rhs: CIR.Expr.Idx,
    negated: bool,
) void {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(expr_idx)));
    var node = Node.init(.expr_structural_eq);
    node.setPayload(.{ .expr_structural_eq = .{
        .lhs = @backingInt(lhs),
        .rhs = @backingInt(rhs),
        .negated = @intFromBool(negated),
    } });
    store.nodes.set(node_idx, node);
}

/// Replaces an existing expression with an explicit structural hash node.
/// This is used when the checker has decided that `to_hash` is satisfied
/// structurally rather than via an attached method dispatch.
pub fn replaceExprWithStructuralHash(
    store: *NodeStore,
    expr_idx: CIR.Expr.Idx,
    value: CIR.Expr.Idx,
    hasher: CIR.Expr.Idx,
) void {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(expr_idx)));
    var node = Node.init(.expr_structural_hash);
    node.setPayload(.{ .expr_structural_hash = .{
        .value = @backingInt(value),
        .hasher = @backingInt(hasher),
    } });
    store.nodes.set(node_idx, node);
}

/// Replaces an existing expression with a call node carrying its checker relation.
pub fn replaceExprWithCallConstraint(
    store: *NodeStore,
    expr_idx: CIR.Expr.Idx,
    func: CIR.Expr.Idx,
    args: CIR.Expr.Span,
    called_via: base.CalledVia,
    constraint_fn_var: types.Var,
) Allocator.Error!void {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(expr_idx)));
    const args_span2_idx: u32 = @intCast(store.span2_data.len());
    _ = try store.span2_data.append(store.gpa, .{
        .start = args.span.start,
        .len = args.span.len,
    });
    var node = Node.init(.expr_call);
    node.setPayload(.{ .expr_call = .{
        .func = @backingInt(func),
        .args_span2_idx = args_span2_idx,
        .called_via = @backingInt(called_via),
        .constraint_fn_var_plus_one = @backingInt(constraint_fn_var) + 1,
    } });
    store.nodes.set(node_idx, node);
}

/// Replaces an existing expression with explicit method-equality metadata.
pub fn replaceExprWithMethodEq(
    store: *NodeStore,
    expr_idx: CIR.Expr.Idx,
    lhs: CIR.Expr.Idx,
    rhs: CIR.Expr.Idx,
    negated: bool,
    constraint_fn_var: types.Var,
) void {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(expr_idx)));
    var node = Node.init(.expr_method_eq);
    node.setPayload(.{ .expr_method_eq = .{
        .lhs = @backingInt(lhs),
        .rhs = @backingInt(rhs),
        .negated = @intFromBool(negated),
        .constraint_fn_var = @backingInt(constraint_fn_var),
    } });
    store.nodes.set(node_idx, node);
}

/// Rewrites a call into a method call on `receiver`, keeping the call's
/// arguments. Used when the deferred import drain finds that the callee of
/// `Alias.Path.U.v(...)` is the member `v` accessed on the qualified tag
/// `Alias.Path.U`.
pub fn replaceCallWithMethodCall(
    store: *NodeStore,
    call_idx: CIR.Expr.Idx,
    receiver: CIR.Expr.Idx,
    method_name: base.Ident.Idx,
    method_name_region: Region,
) Allocator.Error!void {
    const args = store.getExpr(call_idx).e_call.args;
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(call_idx)));
    const method_call_data_idx = try store.addMethodCallData(args, method_name_region, .method_call);
    var node = Node.init(.expr_method_call);
    node.setPayload(.{ .expr_method_call = .{
        .receiver = @backingInt(receiver),
        .method_name = @bitCast(method_name),
        .method_call_data_idx = method_call_data_idx,
    } });
    store.nodes.set(node_idx, node);
}

/// Rewrites a deferred import reference into a field access on `receiver`.
pub fn resolveDeferredExprToFieldAccess(
    store: *NodeStore,
    expr_idx: CIR.Expr.Idx,
    receiver: CIR.Expr.Idx,
    segments: CIR.Expr.FieldAccessSegment.Span,
) void {
    std.debug.assert(segments.len > 0);
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(expr_idx)));
    var node = Node.init(.expr_field_access);
    node.setPayload(.{ .expr_field_access = .{
        .receiver = @backingInt(receiver),
        .segments_start = @backingInt(segments.start),
        .segments_len = segments.len,
    } });
    store.nodes.set(node_idx, node);
}

/// Replaces an existing expression with unresolved receiver dispatch metadata.
pub fn replaceExprWithDispatchCall(
    store: *NodeStore,
    expr_idx: CIR.Expr.Idx,
    receiver: CIR.Expr.Idx,
    method_name: base.Ident.Idx,
    method_name_region: Region,
    args: CIR.Expr.Span,
    constraint_fn_var: types.Var,
    surface_origin: CIR.Expr.SurfaceOrigin,
) Allocator.Error!void {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(expr_idx)));
    const method_call_data_idx = try store.addMethodCallData(args, method_name_region, surface_origin);
    var node = Node.init(.expr_dispatch_call);
    node.setPayload(.{ .expr_dispatch_call = .{
        .receiver = @backingInt(receiver),
        .method_name = @bitCast(method_name),
        .method_call_data_idx = method_call_data_idx,
        .constraint_fn_var = @backingInt(constraint_fn_var),
    } });
    store.nodes.set(node_idx, node);
}

/// Replaces an existing expression with checked interpolation dispatch metadata.
pub fn replaceExprWithInterpolationConstraint(
    store: *NodeStore,
    expr_idx: CIR.Expr.Idx,
    first: CIR.Expr.Idx,
    parts: CIR.Expr.Span,
    method_name_region: Region,
    constraint_fn_var: types.Var,
    assembler_fn_var: types.Var,
    dispatcher_var: types.Var,
) Allocator.Error!void {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(expr_idx)));
    const interpolation_data_idx: u32 = @intCast(store.interpolation_data.len());
    _ = try store.interpolation_data.append(store.gpa, .{
        .parts_start = parts.span.start,
        .parts_len = parts.span.len,
        .method_region_start = method_name_region.start.offset,
        .method_region_end = method_name_region.end.offset,
        .constraint_fn_var_plus_one = @backingInt(constraint_fn_var) + 1,
        .assembler_fn_var_plus_one = @backingInt(assembler_fn_var) + 1,
        .dispatcher_var_plus_one = @backingInt(dispatcher_var) + 1,
    });
    var node = Node.init(.expr_interpolation);
    node.setPayload(.{ .expr_interpolation = .{
        .first = @backingInt(first),
        .interpolation_data_idx = interpolation_data_idx,
    } });
    store.nodes.set(node_idx, node);
}

fn typeDispatchCallNodeTag(owner: CIR.TypeDispatchOwner) Node.Tag {
    return switch (owner) {
        .statement => .expr_type_dispatch_call,
        .dispatcher => .expr_type_dispatch_call_dispatcher,
    };
}

fn typeDispatchOwnerBits(owner: CIR.TypeDispatchOwner) u32 {
    return switch (owner) {
        .statement => |stmt| @backingInt(stmt),
        .dispatcher => |dispatcher| @backingInt(dispatcher),
    };
}

/// Replaces an existing expression with unresolved type dispatch metadata.
pub fn replaceExprWithTypeDispatchCall(
    store: *NodeStore,
    expr_idx: CIR.Expr.Idx,
    owner: CIR.TypeDispatchOwner,
    method_name: base.Ident.Idx,
    method_name_region: Region,
    args: CIR.Expr.Span,
    constraint_fn_var: types.Var,
) Allocator.Error!void {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(expr_idx)));
    const method_call_data_idx = try store.addMethodCallData(args, method_name_region, .method_call);
    var node = Node.init(typeDispatchCallNodeTag(owner));
    node.setPayload(.{ .expr_type_dispatch_call = .{
        .owner = typeDispatchOwnerBits(owner),
        .method_name = @bitCast(method_name),
        .method_call_data_idx = method_call_data_idx,
        .constraint_fn_var = @backingInt(constraint_fn_var),
    } });
    store.nodes.set(node_idx, node);
}

/// Replaces an existing expression with an in-place runtime error node.
/// Used when an earlier compilation stage has already determined that the
/// expression is erroneous and later stages must observe an explicit crash.
/// The expression as written stays readable through `getSourceExpr`.
pub fn replaceExprWithRuntimeError(
    store: *NodeStore,
    expr_idx: CIR.Expr.Idx,
    diagnostic_idx: CIR.Diagnostic.Idx,
) Allocator.Error!void {
    try store.replaceExprWithRuntimeErrorAfter(expr_idx, diagnostic_idx, .{ .span = .{ .start = 0, .len = 0 } });
}

/// Replaces an existing expression with an in-place runtime error node that
/// evaluates `evaluated` (`CIR.Expr.e_runtime_error.evaluated`) before it
/// crashes.
pub fn replaceExprWithRuntimeErrorAfter(
    store: *NodeStore,
    expr_idx: CIR.Expr.Idx,
    diagnostic_idx: CIR.Diagnostic.Idx,
    evaluated: CIR.Expr.Span,
) Allocator.Error!void {
    const node_idx: Node.Idx = @fromBackingInt(@backingInt(expr_idx));
    _ = store.retireLiteralDispatchPlan(node_idx);
    try store.replaceSourceNodeWithRuntimeError(node_idx, diagnostic_idx, evaluated);
}

/// Replaces an existing statement with an in-place runtime error node after
/// checking has rejected the statement and recorded its diagnostic.
/// The statement as written stays readable through `getSourceStatement`.
pub fn replaceStatementWithRuntimeError(
    store: *NodeStore,
    stmt_idx: CIR.Statement.Idx,
    diagnostic_idx: CIR.Diagnostic.Idx,
    evaluated: CIR.Expr.Span,
) Allocator.Error!void {
    try store.replaceSourceNodeWithRuntimeError(@fromBackingInt(@backingInt(stmt_idx)), diagnostic_idx, evaluated);
}

/// Replace a rejected literal leaf while retaining the surrounding definition
/// pattern and its binder identities. Retire the leaf's evidence atomically.
/// The pattern as written stays readable through `getSourcePattern`.
pub fn replacePatternWithRuntimeError(
    store: *NodeStore,
    pattern_idx: CIR.Pattern.Idx,
    diagnostic_idx: CIR.Diagnostic.Idx,
) Allocator.Error!void {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(pattern_idx)));
    _ = store.retireLiteralDispatchPlan(node_idx);
    try store.replaceSourceNodeWithRuntimeError(node_idx, diagnostic_idx, .{ .span = .{ .start = 0, .len = 0 } });
}

/// Keep the node being replaced in `replaced_source_nodes` and put a runtime
/// error in its place. Callers retire the node's checked metadata first, so
/// the kept node carries only source structure. Replacing a node that is
/// already a runtime error keeps the source node it was replaced from.
fn replaceSourceNodeWithRuntimeError(
    store: *NodeStore,
    node_idx: Node.Idx,
    diagnostic_idx: CIR.Diagnostic.Idx,
    evaluated: CIR.Expr.Span,
) Allocator.Error!void {
    const replaced = store.nodes.get(node_idx);
    const source_node_plus_one: u32 = if (replaced.tag == .malformed)
        replaced.getPayload().malformed.source_node_plus_one
    else
        @backingInt(try store.replaced_source_nodes.append(store.gpa, replaced)) + 1;
    var node = Node.init(.malformed);
    node.setPayload(.{ .malformed = .{
        .diagnostic = @backingInt(diagnostic_idx),
        .source_node_plus_one = source_node_plus_one,
        .evaluated_start = evaluated.span.start,
        .evaluated_len = evaluated.span.len,
    } });
    store.nodes.set(node_idx, node);
}

/// Settle a deferred import reference expression that resolves to nothing as
/// a runtime error. The deferred node is a placeholder for the resolved form,
/// not source structure, so nothing is kept for it.
pub fn settleDeferredExprAsRuntimeError(
    store: *NodeStore,
    expr_idx: CIR.Expr.Idx,
    diagnostic_idx: CIR.Diagnostic.Idx,
) void {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(expr_idx)));
    const tag = store.nodes.get(node_idx).tag;
    std.debug.assert(tag == .expr_deferred_import_ref or tag == .expr_deferred_nominal_external);
    var node = Node.init(.malformed);
    node.setPayload(.{ .malformed = .{ .diagnostic = @backingInt(diagnostic_idx) } });
    store.nodes.set(node_idx, node);
}

/// Settle a deferred import reference pattern that resolves to nothing as a
/// runtime error. The deferred node is a placeholder for the nominal pattern,
/// but its backing pattern is source structure whose binders the program
/// wrote, so the runtime error keeps the backing pattern as its source.
pub fn settleDeferredPatternAsRuntimeError(
    store: *NodeStore,
    pattern_idx: CIR.Pattern.Idx,
    diagnostic_idx: CIR.Diagnostic.Idx,
) Allocator.Error!void {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(pattern_idx)));
    std.debug.assert(store.nodes.get(node_idx).tag == .pattern_deferred_import_ref);
    const backing_pattern = store.getPattern(pattern_idx).deferred_import_ref.backing_pattern;
    const backing_node = store.nodes.get(@fromBackingInt(@intCast(@backingInt(backing_pattern))));
    const source_node_plus_one = @backingInt(try store.replaced_source_nodes.append(store.gpa, backing_node)) + 1;
    var node = Node.init(.malformed);
    node.setPayload(.{ .malformed = .{
        .diagnostic = @backingInt(diagnostic_idx),
        .source_node_plus_one = source_node_plus_one,
    } });
    store.nodes.set(node_idx, node);
}

/// The node at `node_idx` as written in source: the node a runtime error
/// replaced, if one did, and otherwise the node itself.
fn sourceNode(store: *const NodeStore, node_idx: Node.Idx) Node {
    const node = store.nodes.get(node_idx);
    if (node.tag != .malformed) return node;
    const source_node_plus_one = node.getPayload().malformed.source_node_plus_one;
    if (source_node_plus_one == 0) return node;
    return store.replaced_source_nodes.get(@fromBackingInt(@intCast(source_node_plus_one - 1)));
}

/// Updates the body of an e_lambda expression.
/// Used when the lambda was created with a placeholder body and needs to be updated
/// after the actual body is canonicalized.
pub fn updateLambdaBody(store: *NodeStore, lambda_idx: CIR.Expr.Idx, body_idx: CIR.Expr.Idx) void {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(lambda_idx)));
    var node = store.nodes.get(node_idx);

    std.debug.assert(node.tag == .expr_lambda);
    const p = node.getPayload().expr_lambda;

    node.setPayload(.{ .expr_lambda = .{
        .args_start = p.args_start,
        .args_len = p.args_len,
        .body = @backingInt(body_idx),
    } });
    store.nodes.set(node_idx, node);
}

/// Retrieves a 'when' branch from the store.
pub fn getMatchBranch(store: *const NodeStore, branch: CIR.Expr.Match.Branch.Idx) CIR.Expr.Match.Branch {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(branch)));
    const node = store.nodes.get(node_idx);
    const payload = node.getPayload();

    std.debug.assert(node.tag == .match_branch);

    // Retrieve match branch data from match_branch_data list
    const p = payload.match_branch;
    const mbd = store.match_branch_data.items.items[p.match_branch_idx];

    return CIR.Expr.Match.Branch{
        .patterns = .{ .span = .{ .start = mbd.patterns_start, .len = mbd.patterns_len } },
        .value = @fromBackingInt(@intCast(mbd.value)),
        .guard = if (mbd.guard == 0) null else @fromBackingInt(@intCast(mbd.guard)),
        .redundant = @fromBackingInt(@intCast(mbd.redundant)),
    };
}

/// Retrieves a pattern of a 'match' branch from the store.
pub fn getMatchBranchPattern(store: *const NodeStore, branch_pat: CIR.Expr.Match.BranchPattern.Idx) CIR.Expr.Match.BranchPattern {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(branch_pat)));
    const node = store.nodes.get(node_idx);
    const payload = node.getPayload();

    std.debug.assert(node.tag == .match_branch_pattern);

    const p = payload.match_branch_pattern;
    return CIR.Expr.Match.BranchPattern{
        .pattern = @fromBackingInt(@intCast(p.pattern)),
        .degenerate = p.degenerate != 0,
    };
}

/// Returns a slice of match branches from the given span.
pub fn matchBranchSlice(store: *const NodeStore, span: CIR.Expr.Match.Branch.Span) []CIR.Expr.Match.Branch.Idx {
    const slice = store.index_data.items.items[span.span.start..(span.span.start + span.span.len)];
    const result: []CIR.Expr.Match.Branch.Idx = @ptrCast(@alignCast(slice));
    return result;
}

/// Retrieves a 'where' clause from the store.
pub fn getWhereClause(store: *const NodeStore, whereClause: CIR.WhereClause.Idx) CIR.WhereClause {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(whereClause)));
    const node = store.nodes.get(node_idx);
    const payload = node.getPayload();

    const tag = narrowNodeTag(WhereNodeTag, node.tag) orelse
        base.invariant("unreachable, node is not a where tag: {}", .{node.tag});
    switch (tag) {
        .where_method => {
            const p = payload.where_clause;
            return CIR.WhereClause{ .w_method = .{
                .var_ = @fromBackingInt(@intCast(p.var_idx)),
                .method_name = @bitCast(p.name),
                .anno = @fromBackingInt(@intCast(p.anno)),
            } };
        },
        .where_alias => {
            const p = payload.where_alias;

            return CIR.WhereClause{ .w_alias = .{
                .var_ = @fromBackingInt(@intCast(p.var_idx)),
                .alias = @fromBackingInt(@intCast(p.alias_idx)),
            } };
        },
        .where_malformed => {
            const p = payload.diag_single_value;
            const diagnostic = @as(CIR.Diagnostic.Idx, @fromBackingInt(@intCast(p.value)));

            return CIR.WhereClause{ .w_malformed = .{
                .diagnostic = diagnostic,
            } };
        },
    }
}

/// Returns true if the given node tag represents a pattern node.
fn isPatternTag(tag: Node.Tag) bool {
    return narrowNodeTag(PatternNodeTag, tag) != null;
}

/// Retrieves a pattern from the store.
pub fn getPattern(store: *const NodeStore, pattern_idx: CIR.Pattern.Idx) CIR.Pattern {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(pattern_idx)));
    return @call(.always_inline, patternFromNode, .{ store, store.nodes.get(node_idx) });
}

/// Retrieves a pattern as written in source, reading through a runtime error
/// put in its place.
pub fn getSourcePattern(store: *const NodeStore, pattern_idx: CIR.Pattern.Idx) CIR.Pattern {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(pattern_idx)));
    return store.patternFromNode(store.sourceNode(node_idx));
}

fn patternFromNode(store: *const NodeStore, node: Node) CIR.Pattern {
    // Safety check: Handle cross-module node index issues where a pattern index
    // might point to a non-pattern node (e.g., type_header from another module)
    if (!isPatternTag(node.tag)) {
        // Return a placeholder pattern to avoid crash
        // This indicates a bug in cross-module canonicalization
        return CIR.Pattern{ .underscore = {} };
    }

    const payload = node.getPayload();

    const tag = narrowNodeTag(PatternNodeTag, node.tag) orelse
        base.invariant("unreachable, node is not a pattern tag: {}", .{node.tag});
    switch (tag) {
        .pattern_nominal_external => {
            const p = payload.pattern_nominal_external;
            // Retrieve backing data from span2_data
            const backing = store.span2_data.items.items[p.backing_span2_idx];

            return CIR.Pattern{
                .nominal_external = .{
                    .module_idx = @fromBackingInt(@intCast(p.module_idx)),
                    .target_node_idx = @intCast(p.target_node_idx),
                    .backing_pattern = @fromBackingInt(@intCast(backing.start)),
                    .backing_type = @fromBackingInt(@intCast(backing.len)),
                },
            };
        },
        .pattern_deferred_import_ref => {
            const p = payload.pattern_deferred_import_ref;
            const backing = store.span2_data.items.items[p.backing_span2_idx];
            return CIR.Pattern{ .deferred_import_ref = .{
                .ref = @fromBackingInt(@intCast(p.ref)),
                .backing_pattern = @fromBackingInt(@intCast(backing.start)),
                .backing_type = @fromBackingInt(@intCast(backing.len)),
            } };
        },
        .pattern_list => {
            const p = payload.pattern_list;
            const list_data = store.pattern_list_data.items.items[p.pattern_list_data_idx];

            const rest_info = if (list_data.has_rest != 0) blk: {
                const rest_pattern = if (list_data.has_pattern != 0)
                    @as(CIR.Pattern.Idx, @fromBackingInt(@intCast(list_data.pattern_idx)))
                else
                    null;
                break :blk @as(@FieldType(@FieldType(CIR.Pattern, "list"), "rest_info"), .{
                    .index = list_data.rest_index,
                    .pattern = rest_pattern,
                });
            } else null;

            return CIR.Pattern{
                .list = .{
                    .patterns = DataSpan.init(list_data.patterns_start, list_data.patterns_len).as(CIR.Pattern.Span),
                    .rest_info = rest_info,
                },
            };
        },
        .pattern_num_literal => {
            const p = payload.pattern_num_literal;
            const value = store.int128_values.items.items[p.int128_idx];

            return CIR.Pattern{
                .num_literal = .{
                    .value = .{ .bytes = @bitCast(value), .kind = @fromBackingInt(@intCast(p.value_kind)) },
                    .kind = @fromBackingInt(@intCast(p.kind)),
                },
            };
        },
        .pattern_f64_literal => {
            const p = payload.pattern_frac_f64;
            const raw: u64 = (@as(u64, p.value_hi) << 32) | @as(u64, p.value_lo);

            return CIR.Pattern{
                .frac_f64_literal = .{ .value = @bitCast(raw) },
            };
        },
        .pattern_dec_literal => {
            const p = payload.pattern_dec_literal;
            const value = store.int128_values.items.items[p.int128_idx];

            return CIR.Pattern{
                .dec_literal = .{
                    .value = RocDec{ .num = value },
                    .has_suffix = p.has_suffix,
                },
            };
        },
        .pattern_small_dec_literal => {
            const p = payload.pattern_small_dec_literal;
            // Unpack small dec data
            const numerator: i16 = @intCast(@as(i32, @bitCast(p.numerator)));
            const denominator_power_of_ten: u8 = @intCast(p.denominator_power & 0xFF);

            return CIR.Pattern{
                .small_dec_literal = .{
                    .value = .{
                        .numerator = numerator,
                        .denominator_power_of_ten = denominator_power_of_ten,
                    },
                    .has_suffix = p.has_suffix,
                },
            };
        },
        .pattern_str_interpolation => {
            const p = payload.pattern_str_interpolation;
            const data = store.pattern_str_interpolation_data.items.items[p.data_idx];
            return CIR.Pattern{ .str_interpolation = .{
                .prefix = @fromBackingInt(@intCast(data.prefix)),
                .steps = .{ .span = .{ .start = data.steps_start, .len = data.steps_len } },
                .end = @fromBackingInt(@intCast(data.end)),
            } };
        },
        inline .pattern_identifier,
        .pattern_var_identifier,
        .pattern_as,
        .pattern_applied_tag,
        .pattern_nominal,
        .pattern_record_destructure,
        .pattern_tuple,
        .pattern_f32_literal,
        .pattern_num_from_numeral_literal,
        .pattern_str_literal,
        .pattern_underscore,
        .malformed,
        => |node_tag| return copiedValue(CIR.Pattern, &copied_pattern_shapes, node_tag, payload),
    }
}

/// Retrieves a type annotation from the store.
pub fn getTypeAnno(store: *const NodeStore, typeAnno: CIR.TypeAnno.Idx) CIR.TypeAnno {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(typeAnno)));
    const node = store.nodes.get(node_idx);
    const payload = node.getPayload();

    const tag = narrowNodeTag(TypeAnnoNodeTag, node.tag) orelse
        base.invariant("unreachable, node is not a type annotation tag: {}", .{node.tag});
    switch (tag) {
        .ty_apply => {
            const p = payload.ty_apply;
            const apply_data = store.type_apply_data.items.items[p.type_apply_data_idx];
            const base_enum: CIR.TypeAnno.LocalOrExternal.Tag = @fromBackingInt(@intCast(apply_data.base_tag));
            const type_base: CIR.TypeAnno.LocalOrExternal = switch (base_enum) {
                .builtin => .{ .builtin = @fromBackingInt(@intCast(apply_data.value1)) },
                .local => .{ .local = .{ .decl_idx = @fromBackingInt(@intCast(apply_data.value1)) } },
                .external => .{ .external = .{
                    .module_idx = @fromBackingInt(@intCast(apply_data.value1)),
                    .target_node_idx = @intCast(apply_data.value2),
                } },
                .external_identity => .{ .external_identity = .{
                    .module_identity = @fromBackingInt(@intCast(apply_data.value1)),
                    .target_node_idx = apply_data.value2,
                } },
                .pending => .{ .pending = .{
                    .module_idx = @fromBackingInt(@intCast(apply_data.value1)),
                    .ref = @fromBackingInt(@intCast(apply_data.value2)),
                } },
            };

            return CIR.TypeAnno{ .apply = .{
                .name = @bitCast(p.name),
                .base = type_base,
                .args = .{ .span = .{ .start = p.args_start, .len = apply_data.args_len } },
            } };
        },
        .ty_lookup => {
            const p = payload.ty_lookup;
            const base_data = store.span2_data.items.items[p.base_span2_idx];
            const base_enum: CIR.TypeAnno.LocalOrExternal.Tag = @fromBackingInt(@intCast(p.base));
            const type_base: CIR.TypeAnno.LocalOrExternal = switch (base_enum) {
                .builtin => .{ .builtin = @fromBackingInt(@intCast(base_data.start)) },
                .local => .{ .local = .{ .decl_idx = @fromBackingInt(@intCast(base_data.start)) } },
                .external => .{ .external = .{
                    .module_idx = @fromBackingInt(@intCast(base_data.start)),
                    .target_node_idx = @intCast(base_data.len),
                } },
                .external_identity => .{ .external_identity = .{
                    .module_identity = @fromBackingInt(@intCast(base_data.start)),
                    .target_node_idx = base_data.len,
                } },
                .pending => .{ .pending = .{
                    .module_idx = @fromBackingInt(@intCast(base_data.start)),
                    .ref = @fromBackingInt(@intCast(base_data.len)),
                } },
            };

            return CIR.TypeAnno{ .lookup = .{
                .name = @bitCast(p.name),
                .base = type_base,
            } };
        },
        .ty_fn => {
            const p = payload.ty_fn;
            const fn_info = store.span2_data.items.items[p.fn_info_span2_idx];
            return CIR.TypeAnno{ .@"fn" = .{
                .args = .{ .span = .{ .start = p.args_start, .len = p.args_len } },
                .ret = @fromBackingInt(@intCast(fn_info.len)),
                .effectful = fn_info.start != 0,
            } };
        },
        .malformed => {
            const p = payload.malformed;
            return CIR.TypeAnno{ .malformed = .{
                .diagnostic = @fromBackingInt(@intCast(p.diagnostic)),
            } };
        },
        inline .ty_rigid_var,
        .ty_rigid_var_lookup,
        .ty_underscore,
        .ty_tag_union,
        .ty_tag,
        .ty_tuple,
        .ty_record,
        .ty_parens,
        .ty_malformed,
        => |node_tag| return copiedValue(CIR.TypeAnno, &copied_type_anno_shapes, node_tag, payload),
    }
}

/// Retrieves a type header from the store.
pub fn getTypeHeader(store: *const NodeStore, typeHeader: CIR.TypeHeader.Idx) CIR.TypeHeader {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(typeHeader)));
    const node = store.nodes.get(node_idx);
    const payload = node.getPayload();

    std.debug.assert(node.tag == .type_header);

    const p = payload.type_header;
    // Unpack args from packed format (start in upper 16 bits, len in lower 16 bits)
    const packed_args = p.packed_args;
    const args_start: u32 = packed_args >> 16;
    const args_len: u32 = packed_args & 0xFFFF;

    return CIR.TypeHeader{
        .name = @bitCast(p.name),
        .relative_name = @bitCast(p.relative_name),
        .args = .{ .span = .{ .start = args_start, .len = args_len } },
    };
}

/// Retrieves an annotation record field from the store.
pub fn getAnnoRecordField(store: *const NodeStore, annoRecordField: CIR.TypeAnno.RecordField.Idx) CIR.TypeAnno.RecordField {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(annoRecordField)));
    const node = store.nodes.get(node_idx);
    if (node.tag == .ty_record_field) {
        const p = node.getPayload().ty_record_field;
        return .{
            .name = @bitCast(p.name),
            .ty = @fromBackingInt(@intCast(p.ty)),
            .is_optional = p.is_optional,
            .is_unnamed = p.is_unnamed,
        };
    }
    std.debug.assert(node.tag == .ty_record_field_defaulted);
    const p = node.getPayload().ty_record_field_defaulted;
    return .{
        .name = @bitCast(p.name),
        .ty = @fromBackingInt(@intCast(p.ty)),
        .is_optional = false,
        .is_unnamed = false,
        .default_value = @fromBackingInt(@intCast(p.default_value)),
    };
}

/// Retrieves an annotation from the store.
pub fn getAnnotation(store: *const NodeStore, annotation: CIR.Annotation.Idx) CIR.Annotation {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(annotation)));
    const node = store.nodes.get(node_idx);
    const payload = node.getPayload();

    std.debug.assert(node.tag == .annotation);

    const p = payload.annotation;
    const anno: CIR.TypeAnno.Idx = @fromBackingInt(@intCast(p.anno));

    const where_clause = if (p.flags.has_where)
        store.loadWhereClauseSpan(p.where_span2_idx)
    else
        null;

    const name_region: ?base.Region = if (p.flags.has_name_region) blk: {
        const span = store.span2_data.items.items[p.name_region_span2_idx];
        break :blk base.Region.from_raw_offsets(span.start, span.start + span.len);
    } else null;

    return CIR.Annotation{
        .anno = anno,
        .where = where_clause,
        .name_region = name_region,
        .mentions_type_var = p.flags.mentions_type_var,
        .introduces_type_var = p.flags.introduces_type_var,
        .contains_underscore = p.flags.contains_underscore,
        .erroneous = p.flags.erroneous,
    };
}

/// Records that type checking generated an erroneous type for `annotation`.
pub fn markAnnotationErroneous(store: *NodeStore, annotation: CIR.Annotation.Idx) void {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(annotation)));
    var node = store.nodes.get(node_idx);
    std.debug.assert(node.tag == .annotation);
    var payload = node.getPayload();
    payload.annotation.flags.erroneous = true;
    node.setPayload(payload);
    store.nodes.set(node_idx, node);
}

/// Retrieves an exposed item from the store.
pub fn getExposedItem(store: *const NodeStore, exposedItem: CIR.ExposedItem.Idx) CIR.ExposedItem {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(exposedItem)));
    const node = store.nodes.get(node_idx);
    const payload = node.getPayload();

    const tag = narrowNodeTag(ExposedItemNodeTag, node.tag) orelse
        base.invariant("Expected exposed_item node, got {s}\n", .{@tagName(node.tag)});
    switch (tag) {
        .exposed_item => {
            const p = payload.exposed_item;
            return CIR.ExposedItem{
                .name = @bitCast(p.name),
                .alias = if (p.alias == 0) null else @bitCast(p.alias),
                .is_wildcard = p.flags & 1 != 0,
                .kind = @fromBackingInt(@intCast((p.flags >> 1) & 1)),
                .target = if (p.flags & exposed_item_has_target != 0) p.target else null,
            };
        },
    }
}

/// Adds a statement to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addStatement(store: *NodeStore, statement: CIR.Statement, region: base.Region) Allocator.Error!CIR.Statement.Idx {
    const node = try store.makeStatementNode(statement);
    const node_idx = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @fromBackingInt(@intCast(@backingInt(node_idx)));
}

/// Set a statement idx to the provided statement
///
/// This is used when defininig recursive type declarations:
/// 1. Make the placeholder node
/// 2. Introduce to scope
/// 3. Canonicalize the annotation
/// 4. Update the placeholder node with the actual annotation
pub fn setStatementNode(store: *NodeStore, stmt_idx: CIR.Statement.Idx, statement: CIR.Statement) Allocator.Error!void {
    const node = try store.makeStatementNode(statement);
    store.nodes.set(@fromBackingInt(@intCast(@backingInt(stmt_idx))), node);
}

/// Creates a statement node, but does not append to the store.
/// IMPORTANT: It *does* append to typed data lists (span2_data, import_data, etc.)
///
/// See `setStatementNode` to see why this exists
fn makeStatementNode(store: *NodeStore, statement: CIR.Statement) Allocator.Error!Node {
    var node = Node.init(undefined);

    switch (statement) {
        .s_decl => |s| {
            const anno_span2_idx: u32 = @intCast(store.span2_data.len());
            const anno_data: Span2 = if (s.anno) |anno| .{
                .start = 1,
                .len = @backingInt(anno),
            } else .{ .start = 0, .len = 0 };
            _ = try store.span2_data.append(store.gpa, anno_data);

            node.tag = .statement_decl;
            node.setPayload(.{ .statement_decl = .{
                .pattern = @backingInt(s.pattern),
                .expr = @backingInt(s.expr),
                .anno_span2_idx = anno_span2_idx,
            } });
        },
        .s_var => |s| {
            const anno_span2_idx: u32 = @intCast(store.span2_data.len());
            const anno_data: Span2 = if (s.anno) |anno| .{
                .start = 1,
                .len = @backingInt(anno),
            } else .{ .start = 0, .len = 0 };
            _ = try store.span2_data.append(store.gpa, anno_data);

            node.tag = .statement_var;
            node.setPayload(.{ .statement_var = .{
                .pattern_idx = @backingInt(s.pattern_idx),
                .expr = @backingInt(s.expr),
                .anno_span2_idx = anno_span2_idx,
            } });
        },
        .s_var_uninitialized => |s| {
            const anno_span2_idx: u32 = @intCast(store.span2_data.len());
            const anno_data: Span2 = if (s.anno) |anno| .{
                .start = 1,
                .len = @backingInt(anno),
            } else .{ .start = 0, .len = 0 };
            _ = try store.span2_data.append(store.gpa, anno_data);

            node.tag = .statement_var_uninitialized;
            node.setPayload(.{ .statement_var_uninitialized = .{
                .pattern_idx = @backingInt(s.pattern_idx),
                .anno_span2_idx = anno_span2_idx,
            } });
        },
        .s_expect => |s| {
            node.tag = .statement_expect;
            node.setPayload(.{ .statement_single_expr = .{
                .expr = @backingInt(s.body),
            } });
        },
        .s_import => |s| {
            node.tag = .statement_import;

            // Build flags indicating which optional fields are present
            var flags: u32 = 0;
            if (s.alias_tok != null) flags |= 1;
            if (s.qualifier_tok != null) flags |= 2;

            // Store import data in typed list
            const import_data_idx: u32 = @intCast(store.import_data.len());
            _ = try store.import_data.append(store.gpa, .{
                .alias_tok = if (s.alias_tok) |alias| @as(u32, @bitCast(alias)) else 0,
                .qualifier_tok = if (s.qualifier_tok) |qualifier| @as(u32, @bitCast(qualifier)) else 0,
                .flags = flags,
                .exposes_start = s.exposes.span.start,
                .exposes_len = s.exposes.span.len,
            });

            node.setPayload(.{ .statement_import = .{
                .module_name_tok = @bitCast(s.module_name_tok),
                .import_data_idx = import_data_idx,
            } });
        },
        .s_where_alias_decl => |s| {
            node.tag = .statement_where_alias_decl;
            node.setPayload(.{ .statement_where_alias_decl = .{
                .header = @backingInt(s.header),
                .receiver = @backingInt(s.receiver),
                .where_span_idx = try store.storeWhereClauseSpan(s.where),
            } });
        },
        .s_type_anno => |s| {
            node.tag = .statement_type_anno;

            const where_span2_idx_plus_one: u32 = if (s.where) |where_clause|
                (try store.storeWhereClauseSpan(where_clause)) + 1
            else
                0;

            node.setPayload(.{ .statement_type_anno = .{
                .anno = @backingInt(s.anno),
                .name = @bitCast(s.name),
                .where_span2_idx_plus_one = where_span2_idx_plus_one,
            } });
        },
        inline .s_reassign,
        .s_crash,
        .s_dbg,
        .s_expr,
        .s_for,
        .s_while,
        .s_infinite_loop,
        .s_breakable_loop,
        .s_break,
        .s_return,
        .s_alias_decl,
        .s_nominal_decl,
        .s_type_var_alias,
        .s_runtime_error,
        => |s, cir_tag| node = copiedNode(&copied_statement_shapes, cir_tag, s),
    }

    return node;
}

/// Adds an expression node to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addExpr(store: *NodeStore, expr: CIR.Expr, region: base.Region) Allocator.Error!CIR.Expr.Idx {
    var node = Node.init(undefined); // tag set below in switch

    switch (expr) {
        .e_lookup_external => |e| {
            // For external lookups, store the module index, target node index, and ident
            node.tag = .expr_external_lookup;
            node.setPayload(.{ .expr_external_lookup = .{
                .module_idx = @backingInt(e.module_idx),
                .target_node_idx = e.target_node_idx,
                .ident_idx = @bitCast(e.ident_idx),
            } });
        },
        .e_deferred_import_ref => |e| {
            if (e.backing) |backing| {
                node.tag = .expr_deferred_nominal_external;
                const backing_span2_idx: u32 = @intCast(store.span2_data.len());
                _ = try store.span2_data.append(store.gpa, .{
                    .start = @backingInt(backing.expr),
                    .len = @backingInt(backing.ty),
                });
                node.setPayload(.{ .expr_deferred_nominal_external = .{
                    .ref = @backingInt(e.ref),
                    .backing_span2_idx = backing_span2_idx,
                } });
            } else {
                node.tag = .expr_deferred_import_ref;
                node.setPayload(.{ .expr_deferred_import_ref = .{
                    .ref = @backingInt(e.ref),
                } });
            }
        },
        .e_num => |e| {
            node.tag = .expr_num;
            const int128_idx: u32 = @intCast(store.int128_values.len());
            _ = try store.int128_values.append(store.gpa, @bitCast(e.value.bytes));
            node.setPayload(.{ .expr_num = .{
                .kind = @backingInt(e.kind),
                .val_kind = @backingInt(e.value.kind),
                .int128_idx = int128_idx,
            } });
        },
        .e_frac_f64 => |e| {
            node.tag = .expr_frac_f64;
            const raw: [2]u32 = @bitCast(e.value);
            node.setPayload(.{ .expr_frac_f64 = .{
                .value_lo = raw[0],
                .value_hi = raw[1],
                .has_suffix = e.has_suffix,
            } });
        },
        .e_dec => |e| {
            node.tag = .expr_dec;
            const int128_idx: u32 = @intCast(store.int128_values.len());
            _ = try store.int128_values.append(store.gpa, e.value.num);
            node.setPayload(.{ .expr_dec = .{
                .int128_idx = int128_idx,
                .has_suffix = e.has_suffix,
            } });
        },
        .e_dec_small => |e| {
            node.tag = .expr_dec_small;

            // Pack small dec data
            node.setPayload(.{ .expr_dec_small = .{
                .numerator = @as(u32, @bitCast(@as(i32, e.value.numerator))),
                .denom_power = @as(u32, e.value.denominator_power_of_ten),
                .has_suffix = e.has_suffix,
            } });
        },
        .e_typed_int => |e| {
            node.tag = .expr_typed_int;
            const int128_idx: u32 = @intCast(store.int128_values.len());
            _ = try store.int128_values.append(store.gpa, @bitCast(e.value.bytes));
            node.setPayload(.{ .expr_typed_int = .{
                .type_name = @bitCast(e.type_name),
                .val_kind = @backingInt(e.value.kind),
                .int128_idx = int128_idx,
            } });
        },
        .e_typed_frac => |e| {
            node.tag = .expr_typed_frac;
            const int128_idx: u32 = @intCast(store.int128_values.len());
            _ = try store.int128_values.append(store.gpa, @bitCast(e.value.bytes));
            node.setPayload(.{ .expr_typed_frac = .{
                .type_name = @bitCast(e.type_name),
                .val_kind = @backingInt(e.value.kind),
                .int128_idx = int128_idx,
            } });
        },
        .e_str_segment => |e| {
            node.tag = .expr_string_segment;
            node.setPayload(.{ .expr_string_segment = .{
                .segment_idx = @backingInt(e.literal),
            } });
        },
        .e_bytes_literal => |e| {
            node.tag = .expr_bytes_literal;
            node.setPayload(.{ .expr_string_segment = .{
                .segment_idx = @backingInt(e.literal),
            } });
        },
        .e_str => |e| {
            node.tag = .expr_string;
            node.setPayload(.{ .expr_string = .{
                .segments_start = e.span.span.start,
                .segments_len = e.span.span.len,
            } });
        },
        .e_nominal_external => |e| {
            node.tag = .expr_nominal_external;
            const backing_span2_idx: u32 = @intCast(store.span2_data.len());
            _ = try store.span2_data.append(store.gpa, .{
                .start = @backingInt(e.backing_expr),
                .len = @backingInt(e.backing_type),
            });
            node.setPayload(.{ .expr_nominal_external = .{
                .module_idx = @backingInt(e.module_idx),
                .target_node_idx = @intCast(e.target_node_idx),
                .backing_span2_idx = backing_span2_idx,
            } });
        },
        .e_field_access => |e| {
            std.debug.assert(e.segments.len > 0);
            node.tag = .expr_field_access;
            node.setPayload(.{ .expr_field_access = .{
                .receiver = @backingInt(e.receiver),
                .segments_start = @backingInt(e.segments.start),
                .segments_len = e.segments.len,
            } });
        },
        .e_method_call => |e| {
            node.tag = .expr_method_call;
            const method_call_data_idx = try store.addMethodCallData(e.args, e.method_name_region, .method_call);
            node.setPayload(.{ .expr_method_call = .{
                .receiver = @backingInt(e.receiver),
                .method_name = @bitCast(e.method_name),
                .method_call_data_idx = method_call_data_idx,
            } });
        },
        .e_dispatch_call => |e| {
            node.tag = .expr_dispatch_call;
            const method_call_data_idx = try store.addMethodCallData(e.args, e.method_name_region, e.surface_origin);
            node.setPayload(.{ .expr_dispatch_call = .{
                .receiver = @backingInt(e.receiver),
                .method_name = @bitCast(e.method_name),
                .method_call_data_idx = method_call_data_idx,
                .constraint_fn_var = @backingInt(e.constraint_fn_var),
            } });
        },
        .e_interpolation => |e| {
            node.tag = .expr_interpolation;
            const interpolation_data_idx: u32 = @intCast(store.interpolation_data.len());
            _ = try store.interpolation_data.append(store.gpa, .{
                .parts_start = e.parts.span.start,
                .parts_len = e.parts.span.len,
                .method_region_start = e.method_name_region.start.offset,
                .method_region_end = e.method_name_region.end.offset,
                .constraint_fn_var_plus_one = if (e.constraint_fn_var) |var_| @backingInt(var_) + 1 else 0,
                .assembler_fn_var_plus_one = if (e.assembler_fn_var) |var_| @backingInt(var_) + 1 else 0,
                .dispatcher_var_plus_one = if (e.dispatcher_var) |var_| @backingInt(var_) + 1 else 0,
            });
            node.setPayload(.{ .expr_interpolation = .{
                .first = @backingInt(e.first),
                .interpolation_data_idx = interpolation_data_idx,
            } });
        },
        .e_type_method_call => |e| {
            node.tag = .expr_type_method_call;
            const method_call_data_idx = try store.addMethodCallData(e.args, e.method_name_region, .method_call);
            node.setPayload(.{ .expr_type_method_call = .{
                .type_dispatch_stmt = @backingInt(e.type_dispatch_stmt),
                .method_name = @bitCast(e.method_name),
                .method_call_data_idx = method_call_data_idx,
            } });
        },
        .e_type_dispatch_call => |e| {
            node.tag = typeDispatchCallNodeTag(e.owner);
            const method_call_data_idx = try store.addMethodCallData(e.args, e.method_name_region, .method_call);
            node.setPayload(.{ .expr_type_dispatch_call = .{
                .owner = typeDispatchOwnerBits(e.owner),
                .method_name = @bitCast(e.method_name),
                .method_call_data_idx = method_call_data_idx,
                .constraint_fn_var = @backingInt(e.constraint_fn_var),
            } });
        },
        .e_hosted_lambda => |hosted| {
            node.tag = .expr_hosted_lambda;
            const args_span2_idx: u32 = @intCast(store.span2_data.len());
            _ = try store.span2_data.append(store.gpa, .{
                .start = hosted.args.span.start,
                .len = hosted.args.span.len,
            });

            node.setPayload(.{ .expr_hosted_lambda = .{
                .symbol_name = @bitCast(hosted.symbol_name),
                .args_span2_idx = args_span2_idx,
            } });
        },
        .e_run_low_level => |run_ll| {
            node.tag = .expr_run_low_level;
            const span2_idx: u32 = @intCast(store.span2_data.len());
            _ = try store.span2_data.append(store.gpa, .{ .start = run_ll.args.span.start, .len = run_ll.args.span.len });

            node.setPayload(.{ .expr_run_low_level = .{
                .op = @backingInt(run_ll.op),
                .args_span2_idx = span2_idx,
            } });
        },
        .e_match => |e| {
            node.tag = .expr_match;
            const match_data_idx: u32 = @intCast(store.match_data.len());
            _ = try store.match_data.append(store.gpa, .{
                .cond = @backingInt(e.cond),
                .branches_start = e.branches.span.start,
                .branches_len = e.branches.span.len,
                .exhaustive = @backingInt(e.exhaustive),
                .is_try_suffix = @intFromBool(e.is_try_suffix),
                .skip_exhaustiveness = @intFromBool(e.skip_exhaustiveness),
            });

            node.setPayload(.{ .expr_match = .{
                .match_data_idx = match_data_idx,
            } });
        },
        .e_if => |e| {
            node.tag = .expr_if_then_else;
            const if_data_idx: u32 = @intCast(store.if_data.len());
            _ = try store.if_data.append(store.gpa, .{
                .branches_start = e.branches.span.start,
                .branches_len = e.branches.span.len,
                .final_else = @backingInt(e.final_else),
                .origin = @backingInt(e.origin),
            });

            node.setPayload(.{ .expr_if_then_else = .{
                .branches_else_idx = if_data_idx,
            } });
        },
        .e_call => |e| {
            node.tag = .expr_call;
            const span2_idx: u32 = @intCast(store.span2_data.len());
            _ = try store.span2_data.append(store.gpa, .{ .start = e.args.span.start, .len = e.args.span.len });

            node.setPayload(.{ .expr_call = .{
                .func = @backingInt(e.func),
                .args_span2_idx = span2_idx,
                .called_via = @backingInt(e.called_via),
                .constraint_fn_var_plus_one = if (e.constraint_fn_var) |var_|
                    @backingInt(var_) + 1
                else
                    0,
            } });
        },
        .e_record => |e| {
            node.tag = .expr_record;
            const fields_ext_idx: u32 = @intCast(store.span_with_node_data.len());
            const ext_value = if (e.ext) |ext| @backingInt(ext) else 0;
            _ = try store.span_with_node_data.append(store.gpa, .{
                .start = e.fields.span.start,
                .len = e.fields.span.len,
                .node = ext_value,
            });
            const unsets_span2_idx: u32 = @intCast(store.span2_data.len());
            _ = try store.span2_data.append(store.gpa, .{ .start = e.unsets.span.start, .len = e.unsets.span.len });

            node.setPayload(.{ .expr_record = .{
                .fields_ext_idx = fields_ext_idx,
                .unsets_span2_idx = unsets_span2_idx,
            } });
        },
        .e_zero_argument_tag => |e| {
            node.tag = .expr_zero_argument_tag;
            const zero_arg_tag_idx: u32 = @intCast(store.zero_arg_tag_data.len());
            _ = try store.zero_arg_tag_data.append(store.gpa, .{
                .closure_name = @bitCast(e.closure_name),
                .variant_var = @backingInt(e.variant_var),
                .ext_var = @backingInt(e.ext_var),
                .name = @bitCast(e.name),
            });

            node.setPayload(.{ .expr_zero_argument_tag = .{
                .zero_arg_tag_idx = zero_arg_tag_idx,
            } });
        },
        .e_closure => |e| {
            node.tag = .expr_closure;
            const closure_data_idx: u32 = @intCast(store.closure_data.len());
            _ = try store.closure_data.append(store.gpa, .{
                .lambda_idx = @backingInt(e.lambda_idx),
                .captures_start = e.captures.span.start,
                .captures_len = e.captures.span.len,
                .tag_name = @bitCast(e.tag_name),
            });

            node.setPayload(.{ .expr_closure = .{
                .closure_data_idx = closure_data_idx,
            } });
        },
        inline .e_frac_f32,
        .e_num_from_numeral,
        .e_typed_num_from_numeral,
        .e_lookup_local,
        .e_lookup_associated_local,
        .e_lookup_associated,
        .e_lookup_associated_resolved,
        .e_lookup_required,
        .e_list,
        .e_empty_list,
        .e_tuple,
        .e_empty_record,
        .e_block,
        .e_tag,
        .e_nominal,
        .e_lambda,
        .e_binop,
        .e_unary_minus,
        .e_structural_eq,
        .e_structural_hash,
        .e_method_eq,
        .e_tuple_access,
        .e_runtime_error,
        .e_crash,
        .e_dbg,
        .e_expect_err,
        .e_expect,
        .e_ellipsis,
        .e_anno_only,
        .e_derived_method,
        .e_return,
        .e_break,
        .e_for,
        => |e, cir_tag| node = copiedNode(&copied_expr_shapes, cir_tag, e),
    }

    const node_idx = try store.nodes.append(store.gpa, node);
    // External lookups carry their source region explicitly.
    const actual_region = if (expr == .e_lookup_external) expr.e_lookup_external.region else region;
    _ = try store.regions.append(store.gpa, actual_region);
    return @fromBackingInt(@intCast(@backingInt(node_idx)));
}

/// Adds a record field to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addRecordField(store: *NodeStore, recordField: CIR.RecordField, region: base.Region) Allocator.Error!CIR.RecordField.Idx {
    var node = Node.init(.record_field);
    node.setPayload(.{ .record_field = .{
        .name = @bitCast(recordField.name),
        .expr = @backingInt(recordField.value),
    } });

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

/// Adds an unset record field (`name: _`) to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addUnsetField(store: *NodeStore, unset_field: CIR.UnsetField, region: base.Region) Allocator.Error!CIR.UnsetField.Idx {
    var node = Node.init(.record_unset_field);
    node.setPayload(.{ .record_unset_field = .{
        .name = @bitCast(unset_field.name),
    } });

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

/// Adds a record destructuring to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addRecordDestruct(store: *NodeStore, record_destruct: CIR.Pattern.RecordDestruct, region: base.Region) Allocator.Error!CIR.Pattern.RecordDestruct.Idx {
    const kind_span2_idx: u32 = @intCast(store.span2_data.len());

    // Store kind in span2_data: (kind_tag, pattern_idx)
    const kind_data: Span2 = switch (record_destruct.kind) {
        .Required => |pattern_idx| .{ .start = 0, .len = @backingInt(pattern_idx) },
        .SubPattern => |pattern_idx| .{ .start = 1, .len = @backingInt(pattern_idx) },
        .Rest => |pattern_idx| .{ .start = 2, .len = @backingInt(pattern_idx) },
    };
    _ = try store.span2_data.append(store.gpa, kind_data);

    var node = Node.init(.record_destruct);
    node.setPayload(.{ .record_destruct = .{
        .label = @bitCast(record_destruct.label),
        .ident = @bitCast(record_destruct.ident),
        .kind_span2_idx = kind_span2_idx,
    } });

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

/// Adds a capture to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addCapture(store: *NodeStore, capture: CIR.Expr.Capture, region: base.Region) Allocator.Error!CIR.Expr.Capture.Idx {
    var node = Node.init(.lambda_capture);
    node.setPayload(.{ .lambda_capture = .{
        .name = @bitCast(capture.name),
        .scope_depth = capture.scope_depth,
        .pattern_idx = @backingInt(capture.pattern_idx),
    } });

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

/// Adds a 'match' branch to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addMatchBranch(store: *NodeStore, branch: CIR.Expr.Match.Branch, region: base.Region) Allocator.Error!CIR.Expr.Match.Branch.Idx {
    const match_branch_idx: u32 = @intCast(store.match_branch_data.len());
    const guard_idx = if (branch.guard) |g| @backingInt(g) else 0;
    _ = try store.match_branch_data.append(store.gpa, .{
        .patterns_start = branch.patterns.span.start,
        .patterns_len = branch.patterns.span.len,
        .value = @backingInt(branch.value),
        .guard = guard_idx,
        .redundant = @backingInt(branch.redundant),
    });

    var node = Node.init(.match_branch);
    node.setPayload(.{ .match_branch = .{
        .match_branch_idx = match_branch_idx,
    } });

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

/// Adds a 'match' branch to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addMatchBranchPattern(store: *NodeStore, branchPattern: CIR.Expr.Match.BranchPattern, region: base.Region) Allocator.Error!CIR.Expr.Match.BranchPattern.Idx {
    var node = Node.init(.match_branch_pattern);
    node.setPayload(.{ .match_branch_pattern = .{
        .pattern = @backingInt(branchPattern.pattern),
        .degenerate = @intFromBool(branchPattern.degenerate),
    } });
    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

/// Adds a 'where' clause to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addWhereClause(store: *NodeStore, whereClause: CIR.WhereClause, region: base.Region) Allocator.Error!CIR.WhereClause.Idx {
    var node = Node.init(undefined);

    switch (whereClause) {
        .w_method => |where_method| {
            node.tag = .where_method;
            node.setPayload(.{ .where_clause = .{
                .var_idx = @backingInt(where_method.var_),
                .name = @bitCast(where_method.method_name),
                .anno = @backingInt(where_method.anno),
            } });
        },
        .w_alias => |where_alias| {
            node.tag = .where_alias;
            node.setPayload(.{ .where_alias = .{
                .var_idx = @backingInt(where_alias.var_),
                .alias_idx = @backingInt(where_alias.alias),
            } });
        },
        .w_malformed => |malformed| {
            node.tag = .where_malformed;
            node.setPayload(.{ .where_malformed = .{
                .diagnostic = @backingInt(malformed.diagnostic),
            } });
        },
    }

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

/// Adds a pattern to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addPattern(store: *NodeStore, pattern: CIR.Pattern, region: base.Region) Allocator.Error!CIR.Pattern.Idx {
    var node = Node.init(undefined);

    switch (pattern) {
        .nominal_external => |n| {
            node.tag = .pattern_nominal_external;
            const backing_span2_idx: u32 = @intCast(store.span2_data.len());
            _ = try store.span2_data.append(store.gpa, .{
                .start = @backingInt(n.backing_pattern),
                .len = @backingInt(n.backing_type),
            });
            node.setPayload(.{ .pattern_nominal_external = .{
                .module_idx = @backingInt(n.module_idx),
                .target_node_idx = @intCast(n.target_node_idx),
                .backing_span2_idx = backing_span2_idx,
            } });
        },
        .deferred_import_ref => |p| {
            node.tag = .pattern_deferred_import_ref;
            const backing_span2_idx: u32 = @intCast(store.span2_data.len());
            _ = try store.span2_data.append(store.gpa, .{
                .start = @backingInt(p.backing_pattern),
                .len = @backingInt(p.backing_type),
            });
            node.setPayload(.{ .pattern_deferred_import_ref = .{
                .ref = @backingInt(p.ref),
                .backing_span2_idx = backing_span2_idx,
            } });
        },
        .list => |p| {
            node.tag = .pattern_list;
            const pattern_list_data_idx: u32 = @intCast(store.pattern_list_data.len());
            const list_data: PatternListData = if (p.rest_info) |rest| .{
                .patterns_start = p.patterns.span.start,
                .patterns_len = p.patterns.span.len,
                .has_rest = 1,
                .rest_index = rest.index,
                .has_pattern = if (rest.pattern != null) 1 else 0,
                .pattern_idx = if (rest.pattern) |pattern_idx| @backingInt(pattern_idx) else 0,
            } else .{
                .patterns_start = p.patterns.span.start,
                .patterns_len = p.patterns.span.len,
                .has_rest = 0,
                .rest_index = 0,
                .has_pattern = 0,
                .pattern_idx = 0,
            };
            _ = try store.pattern_list_data.append(store.gpa, list_data);

            node.setPayload(.{ .pattern_list = .{
                .pattern_list_data_idx = pattern_list_data_idx,
            } });
        },
        .num_literal => |p| {
            node.tag = .pattern_num_literal;
            const int128_idx: u32 = @intCast(store.int128_values.len());
            _ = try store.int128_values.append(store.gpa, @bitCast(p.value.bytes));
            node.setPayload(.{ .pattern_num_literal = .{
                .kind = @backingInt(p.kind),
                .value_kind = @backingInt(p.value.kind),
                .int128_idx = int128_idx,
            } });
        },
        .small_dec_literal => |p| {
            node.tag = .pattern_small_dec_literal;
            node.setPayload(.{ .pattern_small_dec_literal = .{
                .numerator = @bitCast(@as(i32, p.value.numerator)),
                .denominator_power = @as(u32, p.value.denominator_power_of_ten),
                .has_suffix = p.has_suffix,
            } });
        },
        .dec_literal => |p| {
            node.tag = .pattern_dec_literal;
            const int128_idx: u32 = @intCast(store.int128_values.len());
            _ = try store.int128_values.append(store.gpa, p.value.num);
            node.setPayload(.{ .pattern_dec_literal = .{
                .int128_idx = int128_idx,
                .has_suffix = p.has_suffix,
            } });
        },
        .str_interpolation => |p| {
            node.tag = .pattern_str_interpolation;
            const data_idx: u32 = @intCast(store.pattern_str_interpolation_data.len());
            _ = try store.pattern_str_interpolation_data.append(store.gpa, .{
                .prefix = @backingInt(p.prefix),
                .steps_start = p.steps.span.start,
                .steps_len = p.steps.span.len,
                .end = @backingInt(p.end),
            });
            node.setPayload(.{ .pattern_str_interpolation = .{
                .data_idx = data_idx,
            } });
        },
        .frac_f64_literal => |p| {
            node.tag = Node.Tag.pattern_f64_literal;
            const raw: u64 = @bitCast(p.value);
            node.setPayload(.{ .pattern_frac_f64 = .{
                .value_lo = @intCast(raw & 0xFFFFFFFF),
                .value_hi = @intCast(raw >> 32),
            } });
        },
        inline .assign,
        .var_assign,
        .as,
        .applied_tag,
        .nominal,
        .record_destructure,
        .tuple,
        .frac_f32_literal,
        .num_from_numeral_literal,
        .str_literal,
        .underscore,
        .runtime_error,
        => |p, cir_tag| node = copiedNode(&copied_pattern_shapes, cir_tag, p),
    }

    const node_idx = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @fromBackingInt(@intCast(@backingInt(node_idx)));
}

/// Adds a type annotation to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addTypeAnno(store: *NodeStore, typeAnno: CIR.TypeAnno, region: base.Region) Allocator.Error!CIR.TypeAnno.Idx {
    var node = Node.init(undefined);

    switch (typeAnno) {
        .apply => |a| {
            node.tag = .ty_apply;
            const type_apply_data_idx: u32 = @intCast(store.type_apply_data.len());
            const apply_data: TypeApplyData = switch (a.base) {
                .builtin => |builtin_type| .{
                    .args_len = a.args.span.len,
                    .base_tag = @backingInt(CIR.TypeAnno.LocalOrExternal.Tag.builtin),
                    .value1 = @backingInt(builtin_type),
                    .value2 = 0,
                },
                .local => |local| .{
                    .args_len = a.args.span.len,
                    .base_tag = @backingInt(CIR.TypeAnno.LocalOrExternal.Tag.local),
                    .value1 = @backingInt(local.decl_idx),
                    .value2 = 0,
                },
                .external => |ext| .{
                    .args_len = a.args.span.len,
                    .base_tag = @backingInt(CIR.TypeAnno.LocalOrExternal.Tag.external),
                    .value1 = @backingInt(ext.module_idx),
                    .value2 = @intCast(ext.target_node_idx),
                },
                .external_identity => |ext| .{
                    .args_len = a.args.span.len,
                    .base_tag = @backingInt(CIR.TypeAnno.LocalOrExternal.Tag.external_identity),
                    .value1 = @backingInt(ext.module_identity),
                    .value2 = ext.target_node_idx,
                },
                .pending => |pend| .{
                    .args_len = a.args.span.len,
                    .base_tag = @backingInt(CIR.TypeAnno.LocalOrExternal.Tag.pending),
                    .value1 = @backingInt(pend.module_idx),
                    .value2 = @backingInt(pend.ref),
                },
            };
            _ = try store.type_apply_data.append(store.gpa, apply_data);
            node.setPayload(.{ .ty_apply = .{
                .name = @bitCast(a.name),
                .args_start = a.args.span.start,
                .type_apply_data_idx = type_apply_data_idx,
            } });
        },
        .lookup => |t| {
            node.tag = .ty_lookup;
            const base_span2_idx: u32 = @intCast(store.span2_data.len());
            const base_data: Span2 = switch (t.base) {
                .builtin => |builtin_type| .{
                    .start = @backingInt(builtin_type),
                    .len = 0,
                },
                .local => |local| .{
                    .start = @backingInt(local.decl_idx),
                    .len = 0,
                },
                .external => |ext| .{
                    .start = @backingInt(ext.module_idx),
                    .len = @intCast(ext.target_node_idx),
                },
                .external_identity => |ext| .{
                    .start = @backingInt(ext.module_identity),
                    .len = ext.target_node_idx,
                },
                .pending => |pend| .{
                    .start = @backingInt(pend.module_idx),
                    .len = @backingInt(pend.ref),
                },
            };
            _ = try store.span2_data.append(store.gpa, base_data);
            node.setPayload(.{ .ty_lookup = .{
                .name = @bitCast(t.name),
                .base = @backingInt(@as(CIR.TypeAnno.LocalOrExternal.Tag, std.meta.activeTag(t.base))),
                .base_span2_idx = base_span2_idx,
            } });
        },
        .@"fn" => |f| {
            node.tag = .ty_fn;
            const fn_info_span2_idx: u32 = @intCast(store.span2_data.len());
            _ = try store.span2_data.append(store.gpa, .{
                .start = if (f.effectful) 1 else 0,
                .len = @backingInt(f.ret),
            });
            node.setPayload(.{ .ty_fn = .{
                .args_start = f.args.span.start,
                .args_len = f.args.span.len,
                .fn_info_span2_idx = fn_info_span2_idx,
            } });
        },
        inline .rigid_var,
        .rigid_var_lookup,
        .underscore,
        .tag_union,
        .tag,
        .tuple,
        .record,
        .parens,
        .malformed,
        => |t, cir_tag| node = copiedNode(&copied_type_anno_shapes, cir_tag, t),
    }

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

/// Adds a type header to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addTypeHeader(store: *NodeStore, typeHeader: CIR.TypeHeader, region: base.Region) Allocator.Error!CIR.TypeHeader.Idx {
    std.debug.assert(typeHeader.args.span.len <= std.math.maxInt(u16));
    std.debug.assert(typeHeader.args.span.len == 0 or typeHeader.args.span.start <= std.math.maxInt(u16));
    const packed_args: u32 = if (typeHeader.args.span.len == 0)
        0
    else
        (@as(u32, @intCast(typeHeader.args.span.start)) << 16) | @as(u32, @intCast(typeHeader.args.span.len));

    var node = Node.init(.type_header);
    node.setPayload(.{ .type_header = .{
        .name = @bitCast(typeHeader.name),
        .relative_name = @bitCast(typeHeader.relative_name),
        .packed_args = packed_args,
    } });

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

/// Adds an annotation record field to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addAnnoRecordField(store: *NodeStore, annoRecordField: CIR.TypeAnno.RecordField, region: base.Region) Allocator.Error!CIR.TypeAnno.RecordField.Idx {
    const node = if (annoRecordField.default_value) |default_idx| blk: {
        // A defaulted field is never optional or unnamed (rejected at
        // canonicalization), so the payload carries the default instead.
        std.debug.assert(!annoRecordField.is_optional and !annoRecordField.is_unnamed);
        var defaulted_node = Node.init(.ty_record_field_defaulted);
        defaulted_node.setPayload(.{ .ty_record_field_defaulted = .{
            .name = @bitCast(annoRecordField.name),
            .ty = @backingInt(annoRecordField.ty),
            .default_value = @backingInt(default_idx),
        } });
        break :blk defaulted_node;
    } else blk: {
        var plain_node = Node.init(.ty_record_field);
        plain_node.setPayload(.{ .ty_record_field = .{
            .name = @bitCast(annoRecordField.name),
            .ty = @backingInt(annoRecordField.ty),
            .is_optional = annoRecordField.is_optional,
            .is_unnamed = annoRecordField.is_unnamed,
        } });
        break :blk plain_node;
    };

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

/// Drop the `??` default from a defaulted annotation record field, degrading
/// it to a plain required field in place. The default-cycle pass uses this as
/// its recovery so check and lowering never see a cyclic default (design.md
/// "Defaulted Fields"); the default expression node stays in the store (it
/// was canonicalized and diagnosed) but nothing references it.
pub fn dropAnnoRecordFieldDefault(store: *NodeStore, field_idx: CIR.TypeAnno.RecordField.Idx) void {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(field_idx)));
    var node = store.nodes.get(node_idx);
    std.debug.assert(node.tag == .ty_record_field_defaulted);
    const payload = node.getPayload().ty_record_field_defaulted;
    node = Node.init(.ty_record_field);
    node.setPayload(.{ .ty_record_field = .{
        .name = payload.name,
        .ty = payload.ty,
        .is_optional = false,
        .is_unnamed = false,
    } });
    store.nodes.set(node_idx, node);
}

/// Adds an annotation to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addAnnotation(store: *NodeStore, annotation: CIR.Annotation, region: base.Region) Allocator.Error!CIR.Annotation.Idx {
    var node = Node.init(.annotation);

    // Derive the type-variable flags once, here, so the check phase can read them
    // off the annotation rather than re-walking the type tree (see getAnnotation).
    const mentions_type_var = try store.typeAnnoTreeHas(annotation.anno, mentionsTypeVar);
    const introduces_type_var = try store.typeAnnoTreeHas(annotation.anno, introducesTypeVar);
    const contains_underscore = try store.annotationContainsUnderscore(annotation.anno, annotation.where);

    const where_span2_idx: u32 = if (annotation.where) |where_clause|
        try store.storeWhereClauseSpan(where_clause)
    else
        0;

    const name_region_span2_idx: u32 = if (annotation.name_region) |name_region| blk: {
        const idx: u32 = @intCast(store.span2_data.len());
        _ = try store.span2_data.append(store.gpa, .{
            .start = name_region.start.offset,
            .len = name_region.end.offset - name_region.start.offset,
        });
        break :blk idx;
    } else 0;

    node.setPayload(.{ .annotation = .{
        .anno = @backingInt(annotation.anno),
        .where_span2_idx = where_span2_idx,
        .name_region_span2_idx = name_region_span2_idx,
        .flags = .{
            .has_where = annotation.where != null,
            .mentions_type_var = mentions_type_var,
            .introduces_type_var = introduces_type_var,
            .contains_underscore = contains_underscore,
            .has_name_region = annotation.name_region != null,
        },
    } });

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

fn storeWhereClauseSpan(store: *NodeStore, where: CIR.WhereClause.Span) Allocator.Error!u32 {
    const owners_span2_idx: u32 = @intCast(store.span2_data.len());
    _ = try store.span2_data.append(store.gpa, .{
        .start = where.owners.span.start,
        .len = where.owners.span.len,
    });

    const where_span_idx: u32 = @intCast(store.span_with_node_data.len());
    _ = try store.span_with_node_data.append(store.gpa, .{
        .start = where.span.start,
        .len = where.span.len,
        .node = owners_span2_idx,
    });
    return where_span_idx;
}

fn loadWhereClauseSpan(store: *const NodeStore, idx: u32) CIR.WhereClause.Span {
    const where = store.span_with_node_data.items.items[idx];
    const owners = store.span2_data.items.items[where.node];
    return .{
        .span = DataSpan.init(where.start, where.len),
        .owners = .{ .span = DataSpan.init(owners.start, owners.len) },
    };
}

/// Whether a type annotation mentions a type variable (a user-written var like
/// `a`, or an anonymous open-extension var from `..`), introduced by the
/// annotation or looked up from an enclosing one.
fn mentionsTypeVar(anno: CIR.TypeAnno) ?bool {
    return switch (anno) {
        .rigid_var, .rigid_var_lookup => true,
        .underscore, .lookup, .malformed => false,
        .apply, .tag_union, .tag, .tuple, .record, .@"fn", .parens => null,
    };
}

/// Whether a type annotation *introduces* a type variable (`.rigid_var`), not
/// one it references from an enclosing scope. Detects a variable the
/// annotation introduces but its binding cannot quantify (rejected on a
/// mutable `var` and on a value binding that does not generalize).
fn introducesTypeVar(anno: CIR.TypeAnno) ?bool {
    return switch (anno) {
        .rigid_var => true,
        .rigid_var_lookup, .underscore, .lookup, .malformed => false,
        .apply, .tag_union, .tag, .tuple, .record, .@"fn", .parens => null,
    };
}

/// Whether a type annotation contains an `_` inference hole.
fn containsUnderscore(anno: CIR.TypeAnno) ?bool {
    return switch (anno) {
        .underscore => true,
        .rigid_var, .rigid_var_lookup, .lookup, .malformed => false,
        .apply, .tag_union, .tag, .tuple, .record, .@"fn", .parens => null,
    };
}

/// Whether some node of the annotation tree at `root` satisfies `leaf`, which
/// decides leaf nodes and returns null for nodes decided by their children.
fn typeAnnoTreeHas(store: *const NodeStore, root: CIR.TypeAnno.Idx, comptime leaf: fn (CIR.TypeAnno) ?bool) Allocator.Error!bool {
    var pending: std.ArrayList(CIR.TypeAnno.Idx) = .empty;
    defer pending.deinit(store.gpa);
    try pending.append(store.gpa, root);
    while (pending.pop()) |anno_idx| {
        const anno = store.getTypeAnno(anno_idx);
        if (leaf(anno)) |decided| {
            if (decided) return true;
            continue;
        }
        switch (anno) {
            .apply => |a| try pending.appendSlice(store.gpa, store.sliceTypeAnnos(a.args)),
            .tag_union => |tu| {
                try pending.appendSlice(store.gpa, store.sliceTypeAnnos(tu.tags));
                if (tu.ext) |ext| try pending.append(store.gpa, ext);
            },
            .tag => |t| try pending.appendSlice(store.gpa, store.sliceTypeAnnos(t.args)),
            .tuple => |t| try pending.appendSlice(store.gpa, store.sliceTypeAnnos(t.elems)),
            .record => |r| {
                for (store.sliceAnnoRecordFields(r.fields)) |field_idx| {
                    try pending.append(store.gpa, store.getAnnoRecordField(field_idx).ty);
                }
                if (r.ext) |ext| try pending.append(store.gpa, ext);
            },
            .@"fn" => |f| {
                try pending.appendSlice(store.gpa, store.sliceTypeAnnos(f.args));
                try pending.append(store.gpa, f.ret);
            },
            .parens => |p| try pending.append(store.gpa, p.anno),
            .rigid_var, .rigid_var_lookup, .underscore, .lookup, .malformed => {},
        }
    }
    return false;
}

/// Returns true if the annotation contains an `_` inference hole—in its type
/// tree (`anno`) or in any where-clause method signature. Derived once by
/// `addAnnotation` so the check phase can read `Annotation.contains_underscore`
/// instead of re-walking the tree.
fn annotationContainsUnderscore(store: *const NodeStore, anno_idx: CIR.TypeAnno.Idx, where: ?CIR.WhereClause.Span) Allocator.Error!bool {
    if (try store.typeAnnoTreeHas(anno_idx, containsUnderscore)) return true;
    if (where) |where_span| {
        for (store.sliceWhereClauses(where_span)) |where_idx| {
            switch (store.getWhereClause(where_idx)) {
                .w_method => |method| {
                    if (try store.typeAnnoTreeHas(method.var_, containsUnderscore)) return true;
                    if (try store.typeAnnoTreeHas(method.anno, containsUnderscore)) return true;
                },
                .w_alias, .w_malformed => {},
            }
        }
    }
    return false;
}

/// Adds an exposed item to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addExposedItem(store: *NodeStore, exposedItem: CIR.ExposedItem, region: base.Region) Allocator.Error!CIR.ExposedItem.Idx {
    var node = Node.init(.exposed_item);
    node.setPayload(.{ .exposed_item = .{
        .name = @bitCast(exposedItem.name),
        .alias = if (exposedItem.alias) |alias| @bitCast(alias) else 0,
        .flags = @as(u32, @intFromBool(exposedItem.is_wildcard)) |
            (@as(u32, @backingInt(exposedItem.kind)) << 1) |
            (if (exposedItem.target != null) exposed_item_has_target else 0),
        .target = exposedItem.target orelse 0,
    } });

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

/// `ExposedItem` payload flag: `target` holds the declaration the item
/// resolved to.
const exposed_item_has_target: u32 = 1 << 2;

/// Record the declaration node in the imported module that an exposed item
/// of an import resolved to.
pub fn setExposedItemTarget(store: *NodeStore, exposed_item: CIR.ExposedItem.Idx, target: u32) void {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(exposed_item)));
    var node = store.nodes.get(node_idx);
    var payload = node.getPayload().exposed_item;
    payload.flags |= exposed_item_has_target;
    payload.target = target;
    node.setPayload(.{ .exposed_item = payload });
    store.nodes.set(node_idx, node);
}

/// Adds a definition to the store.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addDef(store: *NodeStore, def: CIR.Def, region: base.Region) Allocator.Error!CIR.Def.Idx {
    var node = Node.init(.def);

    const def_data_idx: u32 = @intCast(store.def_data.len());
    const kind_encoded = def.kind.encode();
    const anno_idx = if (def.annotation) |anno| @backingInt(anno) else 0;
    _ = try store.def_data.append(store.gpa, .{
        .pattern = @backingInt(def.pattern),
        .expr = @backingInt(def.expr),
        .kind_0 = kind_encoded[0],
        .kind_1 = kind_encoded[1],
        .anno_idx = anno_idx,
    });

    node.setPayload(.{ .def = .{
        .def_data_idx = def_data_idx,
    } });

    const nid = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @fromBackingInt(@intCast(@backingInt(nid)));
}

/// Retrieves a definition from the store.
pub fn getDef(store: *const NodeStore, def_idx: CIR.Def.Idx) CIR.Def {
    const nid: Node.Idx = @fromBackingInt(@intCast(@backingInt(def_idx)));
    const node = store.nodes.get(nid);

    std.debug.assert(node.tag == .def);

    const payload = node.getPayload();
    // Retrieve def data from def_data list
    const dd = store.def_data.items.items[payload.def.def_data_idx];

    const kind = CIR.Def.Kind.decode(.{ dd.kind_0, dd.kind_1 });
    const annotation = if (dd.anno_idx == 0) null else @as(CIR.Annotation.Idx, @fromBackingInt(@intCast(dd.anno_idx)));

    return CIR.Def{
        .pattern = @fromBackingInt(@intCast(dd.pattern)),
        .expr = @fromBackingInt(@intCast(dd.expr)),
        .annotation = annotation,
        .kind = kind,
    };
}

/// Updates the expression field of an existing definition.
/// This is used during constant folding to replace expressions with their folded equivalents.
pub fn setDefExpr(store: *NodeStore, def_idx: CIR.Def.Idx, new_expr: CIR.Expr.Idx) void {
    const nid: Node.Idx = @fromBackingInt(@intCast(@backingInt(def_idx)));
    const node = store.nodes.get(nid);

    std.debug.assert(node.tag == .def);

    const payload = node.getPayload();
    // Update expr field in def_data list
    store.def_data.items.items[payload.def.def_data_idx].expr = @backingInt(new_expr);
}

/// Retrieves one segment of a flattened field-access path.
pub fn getFieldAccessSegment(store: *const NodeStore, segment_idx: CIR.Expr.FieldAccessSegment.Idx) CIR.Expr.FieldAccessSegment {
    const nid: Node.Idx = @fromBackingInt(@intCast(@backingInt(segment_idx)));
    const node = store.nodes.get(nid);

    std.debug.assert(node.tag == .field_access_segment);
    const payload = node.getPayload().field_access_segment;

    return .{
        .name = @bitCast(payload.name),
        .mode = @fromBackingInt(@intCast(payload.mode)),
    };
}

/// Retrieves a capture from the store.
pub fn getCapture(store: *const NodeStore, capture_idx: CIR.Expr.Capture.Idx) CIR.Expr.Capture {
    const nid: Node.Idx = @fromBackingInt(@intCast(@backingInt(capture_idx)));
    const node = store.nodes.get(nid);

    std.debug.assert(node.tag == .lambda_capture);

    const payload = node.getPayload();
    return CIR.Expr.Capture{
        .name = @bitCast(payload.lambda_capture.name),
        .scope_depth = payload.lambda_capture.scope_depth,
        .pattern_idx = @fromBackingInt(@intCast(payload.lambda_capture.pattern_idx)),
    };
}

/// Retrieves a record field from the store.
pub fn getRecordField(store: *const NodeStore, idx: CIR.RecordField.Idx) CIR.RecordField {
    const node = store.nodes.get(@fromBackingInt(@intCast(@backingInt(idx))));
    const payload = node.getPayload();
    return CIR.RecordField{
        .name = @bitCast(payload.record_field.name),
        .value = @fromBackingInt(@intCast(payload.record_field.expr)),
    };
}

/// Retrieves an unset record field from the store.
pub fn getUnsetField(store: *const NodeStore, idx: CIR.UnsetField.Idx) CIR.UnsetField {
    const node = store.nodes.get(@fromBackingInt(@intCast(@backingInt(idx))));
    const payload = node.getPayload();
    return CIR.UnsetField{
        .name = @bitCast(payload.record_unset_field.name),
    };
}

/// Retrieves a record destructure from the store.
pub fn getRecordDestruct(store: *const NodeStore, idx: CIR.Pattern.RecordDestruct.Idx) CIR.Pattern.RecordDestruct {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(idx)));
    const node = store.nodes.get(node_idx);

    std.debug.assert(node.tag == .record_destruct);

    const payload = node.getPayload();
    // Retrieve kind from span2_data
    const kind_data = store.span2_data.items.items[payload.record_destruct.kind_span2_idx];

    const kind = switch (kind_data.start) {
        0 => CIR.Pattern.RecordDestruct.Kind{ .Required = @fromBackingInt(@intCast(kind_data.len)) },
        1 => CIR.Pattern.RecordDestruct.Kind{ .SubPattern = @fromBackingInt(@intCast(kind_data.len)) },
        2 => CIR.Pattern.RecordDestruct.Kind{ .Rest = @fromBackingInt(@intCast(kind_data.len)) },
        else => unreachable, // addRecordDestruct encodes only kind tags 0, 1, and 2
    };

    return CIR.Pattern.RecordDestruct{
        .label = @bitCast(payload.record_destruct.label),
        .ident = @bitCast(payload.record_destruct.ident),
        .kind = kind,
    };
}

/// Retrieves an if branch from the store.
pub fn getIfBranch(store: *const NodeStore, if_branch_idx: CIR.Expr.IfBranch.Idx) CIR.Expr.IfBranch {
    const nid: Node.Idx = @fromBackingInt(@intCast(@backingInt(if_branch_idx)));
    const node = store.nodes.get(nid);

    std.debug.assert(node.tag == .if_branch);

    const payload = node.getPayload();
    return CIR.Expr.IfBranch{
        .cond = @fromBackingInt(@intCast(payload.if_branch.cond)),
        .body = @fromBackingInt(@intCast(payload.if_branch.body)),
    };
}

/// Check if a raw node index refers to a definition node.
/// This is useful when exposed items might be either definitions or type declarations.
pub fn isDefNode(store: *const NodeStore, node_idx: u32) bool {
    const nid: Node.Idx = @fromBackingInt(@intCast(node_idx));
    const node = store.nodes.get(nid);
    return node.tag == .def;
}

/// Initialize scratch buffers if they are null (e.g. after deserialization).
pub fn ensureScratch(store: *NodeStore) Allocator.Error!void {
    if (store.scratch == null) {
        store.scratch = try Scratch.init(store.gpa);
    }
}

/// Generic function to get the top of any scratch buffer
pub fn scratchTop(store: *NodeStore, comptime field_name: []const u8) u32 {
    return @field(store.scratch.?, field_name).top();
}

/// Generic function to add an item to any scratch buffer
pub fn addScratch(store: *NodeStore, comptime field_name: []const u8, idx: anytype) Allocator.Error!void {
    try @field(store.scratch.?, field_name).append(idx);
}

/// Generic function to clear any scratch buffer from a given position
pub fn clearScratchFrom(store: *NodeStore, comptime field_name: []const u8, start: u32) void {
    @field(store.scratch.?, field_name).clearFrom(start);
}

/// Generic function to create a span from any scratch buffer
pub fn spanFrom(store: *NodeStore, comptime field_name: []const u8, comptime SpanType: type, start: u32) Allocator.Error!SpanType {
    const scratch_field = &@field(store.scratch.?, field_name);
    const end = scratch_field.top();
    defer scratch_field.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const index_start = store.index_data.len();
    std.debug.assert(end >= i);
    while (i < end) {
        _ = try store.index_data.append(store.gpa, @backingInt(scratch_field.items.items[i]));
        i += 1;
    }
    return .{ .span = .{ .start = @intCast(index_start), .len = @as(u32, @intCast(end)) - start } };
}

/// Returns the top of the scratch expressions buffer.
pub fn scratchExprTop(store: *NodeStore) u32 {
    return store.scratchTop("exprs");
}

/// Adds a scratch expression to temporary storage.
pub fn addScratchExpr(store: *NodeStore, idx: CIR.Expr.Idx) Allocator.Error!void {
    try store.addScratch("exprs", idx);
}

/// In-progress, scratch-free assembly of a contiguous field-access path.
///
/// Segment nodes are appended directly to the node and region stores. The
/// builder makes that contiguity explicit and provides one rollback boundary
/// for allocation failure while constructing the path.
pub const FieldAccessPathBuilder = struct {
    start: CIR.Expr.FieldAccessSegment.Idx,
    segment_count: u32,
};

/// Reserves all storage needed to build one field-access path and its enclosing
/// expression.
///
/// Reserving the auxiliary nodes and their regions in a batch makes every
/// append in the path-construction loop allocation-free. The extra node slot
/// ensures adding the enclosing `e_field_access` cannot strand a finished path
/// after an allocation failure.
pub fn startFieldAccessPath(store: *NodeStore, segment_count: u32) Allocator.Error!FieldAccessPathBuilder {
    std.debug.assert(segment_count > 0);
    std.debug.assert(store.nodes.len() == store.regions.len());

    const start = store.nodes.len();
    const after_segments = std.math.add(u32, start, segment_count) catch return error.OutOfMemory;
    const total = std.math.add(u32, after_segments, 1) catch return error.OutOfMemory;

    try store.nodes.ensureTotalCapacity(store.gpa, total);
    try store.regions.items.ensureUnusedCapacity(store.gpa, @as(usize, segment_count) + 1);

    return .{
        .start = @fromBackingInt(@intCast(start)),
        .segment_count = segment_count,
    };
}

/// Appends one source-ordered segment without allocating.
pub fn appendFieldAccessPathSegmentAssumeCapacity(
    store: *NodeStore,
    builder: FieldAccessPathBuilder,
    segment: CIR.Expr.FieldAccessSegment,
    region: Region,
) CIR.Expr.FieldAccessSegment.Idx {
    const start = @backingInt(builder.start);
    std.debug.assert(store.nodes.len() == store.regions.len());
    std.debug.assert(store.nodes.len() >= start);
    const appended = store.nodes.len() - start;
    std.debug.assert(appended < builder.segment_count);

    var node = Node.init(.field_access_segment);
    node.setPayload(.{ .field_access_segment = .{
        .name = @bitCast(segment.name),
        .mode = @backingInt(segment.mode),
    } });

    const node_idx = store.nodes.appendAssumeCapacity(node);
    const region_idx = store.regions.appendAssumeCapacity(region);
    std.debug.assert(@backingInt(node_idx) == start + appended);
    std.debug.assert(@backingInt(region_idx) == start + appended);
    return @fromBackingInt(@intCast(@backingInt(node_idx)));
}

/// Finishes a source-ordered path and returns its direct node-index span.
pub fn finishFieldAccessPath(store: *NodeStore, builder: FieldAccessPathBuilder) CIR.Expr.FieldAccessSegment.Span {
    const start = @backingInt(builder.start);
    std.debug.assert(store.nodes.len() == store.regions.len());
    std.debug.assert(store.nodes.len() == start + builder.segment_count);
    return .{ .start = builder.start, .len = builder.segment_count };
}

/// Rolls back a path whose construction failed before it was finalized.
pub fn rollbackFieldAccessPath(store: *NodeStore, builder: FieldAccessPathBuilder) void {
    const start: usize = @backingInt(builder.start);
    const end = start + builder.segment_count;
    std.debug.assert(store.nodes.items.len >= start);
    std.debug.assert(store.nodes.items.len <= end);
    std.debug.assert(store.regions.items.items.len == store.nodes.items.len);
    store.nodes.items.shrinkRetainingCapacity(start);
    store.regions.items.shrinkRetainingCapacity(start);
}

/// Appends a persistent expression span directly to index storage.
pub fn appendExprSpan(store: *NodeStore, exprs: []const CIR.Expr.Idx) Allocator.Error!CIR.Expr.Span {
    const index_start = store.index_data.len();
    for (exprs) |expr| {
        _ = try store.index_data.append(store.gpa, @backingInt(expr));
    }
    return .{ .span = .{ .start = @intCast(index_start), .len = @intCast(exprs.len) } };
}

/// Adds a capture index to the scratch captures list for building spans.
pub fn addScratchCapture(store: *NodeStore, idx: CIR.Expr.Capture.Idx) Allocator.Error!void {
    try store.addScratch("captures", idx);
}

/// Adds a statement index to the scratch statements list for building spans.
pub fn addScratchStatement(store: *NodeStore, idx: CIR.Statement.Idx) Allocator.Error!void {
    try store.addScratch("statements", idx);
}

/// Computes the span of an expression starting from a given index.
pub fn exprSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.Expr.Span {
    return try store.spanFrom("exprs", CIR.Expr.Span, start);
}

/// Computes the span of captures starting from a given index.
pub fn capturesSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.Expr.Capture.Span {
    return try store.spanFrom("captures", CIR.Expr.Capture.Span, start);
}

/// Creates a statement span from the given start position to the current top of scratch statements.
pub fn statementSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.Statement.Span {
    return try store.spanFrom("statements", CIR.Statement.Span, start);
}

/// Clears scratch expressions starting from a specified index.
pub fn clearScratchExprsFrom(store: *NodeStore, start: u32) void {
    store.clearScratchFrom("exprs", start);
}

/// Returns a slice of expressions from the scratch space.
pub fn exprSlice(store: *const NodeStore, span: CIR.Expr.Span) []CIR.Expr.Idx {
    return store.sliceFromSpan(CIR.Expr.Idx, span.span);
}

/// Returns the auxiliary node identity at one source-order path position.
pub fn fieldAccessSegmentAt(
    store: *const NodeStore,
    span: CIR.Expr.FieldAccessSegment.Span,
    position: u32,
) CIR.Expr.FieldAccessSegment.Idx {
    std.debug.assert(position < span.len);
    const raw = std.math.add(u32, @backingInt(span.start), position) catch unreachable; // position < span.len, and every node of the span has a u32 index in the store
    const segment_idx: CIR.Expr.FieldAccessSegment.Idx = @fromBackingInt(@intCast(raw));
    const node_idx: Node.Idx = @fromBackingInt(@intCast(raw));
    std.debug.assert(raw < store.nodes.len());
    std.debug.assert(store.nodes.get(node_idx).tag == .field_access_segment);
    return segment_idx;
}

/// Returns the top index for scratch definitions.
pub fn scratchDefTop(store: *NodeStore) u32 {
    return store.scratchTop("defs");
}

/// Adds a scratch definition to temporary storage.
pub fn addScratchDef(store: *NodeStore, idx: CIR.Def.Idx) Allocator.Error!void {
    try store.addScratch("defs", idx);
}

/// Adds a type annotation to the scratch buffer.
pub fn addScratchTypeAnno(store: *NodeStore, idx: CIR.TypeAnno.Idx) Allocator.Error!void {
    try store.addScratch("type_annos", idx);
}

/// Adds a where clause to the scratch buffer.
pub fn addScratchWhereClause(store: *NodeStore, idx: CIR.WhereClause.Idx) Allocator.Error!void {
    try store.addScratch("where_clauses", idx);
}

/// Returns the current top of the scratch type annotations buffer.
pub fn scratchTypeAnnoTop(store: *NodeStore) u32 {
    return store.scratchTop("type_annos");
}

/// Returns the current top of the scratch where clauses buffer.
pub fn scratchWhereClauseTop(store: *NodeStore) u32 {
    return store.scratchTop("where_clauses");
}

/// Clears scratch type annotations from the given index.
pub fn clearScratchTypeAnnosFrom(store: *NodeStore, from: u32) void {
    store.clearScratchFrom("type_annos", from);
}

/// Clears scratch where clauses from the given index.
pub fn clearScratchWhereClausesFrom(store: *NodeStore, from: u32) void {
    store.clearScratchFrom("where_clauses", from);
}

/// Creates a span from the scratch type annotations starting at the given index.
pub fn typeAnnoSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.TypeAnno.Span {
    return try store.spanFrom("type_annos", CIR.TypeAnno.Span, start);
}

/// Returns a span from the scratch anno record fields starting at the given index.
pub fn annoRecordFieldSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.TypeAnno.RecordField.Span {
    return try store.spanFrom("anno_record_fields", CIR.TypeAnno.RecordField.Span, start);
}

/// Returns a span from the scratch record fields starting at the given index.
pub fn recordFieldSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.RecordField.Span {
    return try store.spanFrom("record_fields", CIR.RecordField.Span, start);
}

/// Returns a span from the scratch unset record fields starting at the given index.
pub fn unsetFieldSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.UnsetField.Span {
    return try store.spanFrom("record_unset_fields", CIR.UnsetField.Span, start);
}

/// Returns a span from the scratch where clauses starting at the given index,
/// together with the canonical rigid declaration that owns each method group.
pub fn whereClauseSpanFrom(store: *NodeStore, start: u32, root_annos: []const CIR.TypeAnno.Idx) Allocator.Error!CIR.WhereClause.Span {
    const clauses = try store.spanFrom("where_clauses", CIR.WhereClause.IdxSpan, start);

    const ClauseList = std.ArrayListUnmanaged(CIR.WhereClause.Idx);
    var grouped = std.AutoArrayHashMapUnmanaged(CIR.TypeAnno.Idx, ClauseList){};
    defer {
        for (grouped.values()) |*list| list.deinit(store.gpa);
        grouped.deinit(store.gpa);
    }

    for (store.sliceWhereClauseIndices(clauses)) |where_idx| {
        const constrained_var = switch (store.getWhereClause(where_idx)) {
            .w_method => |method| method.var_,
            .w_alias => |alias| alias.var_,
            .w_malformed => continue,
        };
        const anno = store.getTypeAnno(constrained_var);
        const owner = if (anno == .rigid_var)
            constrained_var
        else if (anno == .rigid_var_lookup)
            anno.rigid_var_lookup.ref
        else
            continue;
        const gop = try grouped.getOrPut(store.gpa, owner);
        if (!gop.found_existing) gop.value_ptr.* = .empty;
        try gop.value_ptr.append(store.gpa, where_idx);
    }

    var locally_declared = std.AutoHashMapUnmanaged(CIR.TypeAnno.Idx, void){};
    defer locally_declared.deinit(store.gpa);
    for (root_annos) |root_anno| {
        try store.collectIntroducedRigidVars(root_anno, &locally_declared);
    }
    for (store.sliceWhereClauseIndices(clauses)) |where_idx| {
        switch (store.getWhereClause(where_idx)) {
            .w_method => |method| {
                try store.collectIntroducedRigidVars(method.var_, &locally_declared);
                try store.collectIntroducedRigidVars(method.anno, &locally_declared);
            },
            .w_alias => |alias| {
                try store.collectIntroducedRigidVars(alias.var_, &locally_declared);
                try store.collectIntroducedRigidVars(alias.alias, &locally_declared);
            },
            .w_malformed => {},
        }
    }

    // A constraint-only rigid is owned by this annotation only when a chain of
    // method signatures connects it to a rigid in the ordinary annotation.
    var reachable = std.AutoHashMapUnmanaged(CIR.TypeAnno.Idx, void){};
    defer reachable.deinit(store.gpa);
    for (root_annos) |root_anno| {
        try store.collectIntroducedRigidVars(root_anno, &reachable);
    }

    var owner_queue = std.ArrayListUnmanaged(CIR.TypeAnno.Idx).empty;
    defer owner_queue.deinit(store.gpa);
    for (grouped.keys()) |owner| {
        if (reachable.contains(owner)) {
            try owner_queue.append(store.gpa, owner);
        }
    }

    var dependencies = std.AutoHashMapUnmanaged(CIR.TypeAnno.Idx, void){};
    defer dependencies.deinit(store.gpa);
    var queue_index: usize = 0;
    while (queue_index < owner_queue.items.len) : (queue_index += 1) {
        const owner = owner_queue.items[queue_index];
        dependencies.clearRetainingCapacity();

        for (grouped.get(owner).?.items) |where_idx| {
            switch (store.getWhereClause(where_idx)) {
                .w_method => |method| {
                    try store.collectReferencedRigidVars(method.anno, &dependencies);
                },
                // A where alias reaches the type variables its arguments name.
                .w_alias => |alias| try store.collectReferencedRigidVars(alias.alias, &dependencies),
                .w_malformed => {},
            }
        }

        var dependency_it = dependencies.keyIterator();
        while (dependency_it.next()) |dependency_ptr| {
            const dependency = dependency_ptr.*;
            if (!locally_declared.contains(dependency) or reachable.contains(dependency)) continue;

            try reachable.put(store.gpa, dependency, {});
            if (grouped.contains(dependency)) {
                try owner_queue.append(store.gpa, dependency);
            }
        }
    }

    const owners_start: u32 = @intCast(store.where_clause_owners.len());
    for (grouped.keys(), grouped.values()) |owner, owned_clauses| {
        const clauses_start: u32 = @intCast(store.index_data.len());
        for (owned_clauses.items) |where_idx| {
            _ = try store.index_data.append(store.gpa, @backingInt(where_idx));
        }
        _ = try store.where_clause_owners.append(store.gpa, .{
            .rigid_var = @backingInt(owner),
            .clauses_start = clauses_start,
            .clauses_len = @intCast(owned_clauses.items.len),
            .owned_by_annotation = reachable.contains(owner),
        });
    }

    return .{
        .span = clauses.span,
        .owners = .{ .span = .{
            .start = owners_start,
            .len = @intCast(grouped.count()),
        } },
    };
}

fn collectIntroducedRigidVars(
    store: *const NodeStore,
    anno_idx: CIR.TypeAnno.Idx,
    introduced: *std.AutoHashMapUnmanaged(CIR.TypeAnno.Idx, void),
) Allocator.Error!void {
    switch (store.getTypeAnno(anno_idx)) {
        .rigid_var => try introduced.put(store.gpa, anno_idx, {}),
        .rigid_var_lookup, .underscore, .lookup, .malformed => {},
        .apply => |apply| for (store.sliceTypeAnnos(apply.args)) |arg| {
            try store.collectIntroducedRigidVars(arg, introduced);
        },
        .tag_union => |tag_union| {
            for (store.sliceTypeAnnos(tag_union.tags)) |tag| {
                try store.collectIntroducedRigidVars(tag, introduced);
            }
            if (tag_union.ext) |ext| try store.collectIntroducedRigidVars(ext, introduced);
        },
        .tag => |tag| for (store.sliceTypeAnnos(tag.args)) |arg| {
            try store.collectIntroducedRigidVars(arg, introduced);
        },
        .tuple => |tuple| for (store.sliceTypeAnnos(tuple.elems)) |elem| {
            try store.collectIntroducedRigidVars(elem, introduced);
        },
        .record => |record| {
            for (store.sliceAnnoRecordFields(record.fields)) |field_idx| {
                try store.collectIntroducedRigidVars(store.getAnnoRecordField(field_idx).ty, introduced);
            }
            if (record.ext) |ext| try store.collectIntroducedRigidVars(ext, introduced);
        },
        .@"fn" => |func| {
            for (store.sliceTypeAnnos(func.args)) |arg| {
                try store.collectIntroducedRigidVars(arg, introduced);
            }
            try store.collectIntroducedRigidVars(func.ret, introduced);
        },
        .parens => |parens| try store.collectIntroducedRigidVars(parens.anno, introduced),
    }
}

fn collectReferencedRigidVars(
    store: *const NodeStore,
    anno_idx: CIR.TypeAnno.Idx,
    referenced: *std.AutoHashMapUnmanaged(CIR.TypeAnno.Idx, void),
) Allocator.Error!void {
    switch (store.getTypeAnno(anno_idx)) {
        .rigid_var => try referenced.put(store.gpa, anno_idx, {}),
        .rigid_var_lookup => |lookup| try referenced.put(store.gpa, lookup.ref, {}),
        .underscore, .lookup, .malformed => {},
        .apply => |apply| for (store.sliceTypeAnnos(apply.args)) |arg| {
            try store.collectReferencedRigidVars(arg, referenced);
        },
        .tag_union => |tag_union| {
            for (store.sliceTypeAnnos(tag_union.tags)) |tag| {
                try store.collectReferencedRigidVars(tag, referenced);
            }
            if (tag_union.ext) |ext| try store.collectReferencedRigidVars(ext, referenced);
        },
        .tag => |tag| for (store.sliceTypeAnnos(tag.args)) |arg| {
            try store.collectReferencedRigidVars(arg, referenced);
        },
        .tuple => |tuple| for (store.sliceTypeAnnos(tuple.elems)) |elem| {
            try store.collectReferencedRigidVars(elem, referenced);
        },
        .record => |record| {
            for (store.sliceAnnoRecordFields(record.fields)) |field_idx| {
                try store.collectReferencedRigidVars(store.getAnnoRecordField(field_idx).ty, referenced);
            }
            if (record.ext) |ext| try store.collectReferencedRigidVars(ext, referenced);
        },
        .@"fn" => |func| {
            for (store.sliceTypeAnnos(func.args)) |arg| {
                try store.collectReferencedRigidVars(arg, referenced);
            }
            try store.collectReferencedRigidVars(func.ret, referenced);
        },
        .parens => |parens| try store.collectReferencedRigidVars(parens.anno, referenced),
    }
}

/// Returns the current top of the scratch exposed items buffer.
pub fn scratchExposedItemTop(store: *NodeStore) u32 {
    return store.scratchTop("exposed_items");
}

/// Adds an exposed item to the scratch buffer.
pub fn addScratchExposedItem(store: *NodeStore, idx: CIR.ExposedItem.Idx) Allocator.Error!void {
    try store.addScratch("exposed_items", idx);
}

/// Creates a span from the scratch exposed items starting at the given index.
pub fn exposedItemSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.ExposedItem.Span {
    return try store.spanFrom("exposed_items", CIR.ExposedItem.Span, start);
}

/// Clears scratch exposed items from the given index.
pub fn clearScratchExposedItemsFrom(store: *NodeStore, start: u32) void {
    store.clearScratchFrom("exposed_items", start);
}

/// Returns the start position for a new Span of annoRecordFieldIdxs in scratch
pub fn scratchAnnoRecordFieldTop(store: *NodeStore) u32 {
    return store.scratchTop("anno_record_fields");
}

/// Places a new CIR.TypeAnno.RecordField.Idx in the scratch. Will panic on OOM.
pub fn addScratchAnnoRecordField(store: *NodeStore, idx: CIR.TypeAnno.RecordField.Idx) Allocator.Error!void {
    try store.addScratch("anno_record_fields", idx);
}

/// Clears any AnnoRecordFieldIds added to scratch from start until the end.
pub fn clearScratchAnnoRecordFieldsFrom(store: *NodeStore, start: u32) void {
    store.clearScratchFrom("anno_record_fields", start);
}

/// Returns a new AnnoRecordField slice so that the caller can iterate through
/// all items in the span.
pub fn annoRecordFieldSlice(store: *NodeStore, span: CIR.TypeAnno.RecordField.Span) []CIR.TypeAnno.RecordField.Idx {
    return store.sliceFromSpan(CIR.TypeAnno.RecordField.Idx, span.span);
}

/// Computes the span of a definition starting from a given index.
pub fn defSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.Def.Span {
    return try store.spanFrom("defs", CIR.Def.Span, start);
}

/// Retrieves a slice of record destructures from the store.
pub fn recordDestructSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.Pattern.RecordDestruct.Span {
    return try store.spanFrom("record_destructs", CIR.Pattern.RecordDestruct.Span, start);
}

/// Returns the current top of the scratch patterns buffer.
pub fn scratchPatternTop(store: *NodeStore) u32 {
    return store.scratchTop("patterns");
}

/// Adds a pattern to the scratch patterns list for building spans.
pub fn addScratchPattern(store: *NodeStore, idx: CIR.Pattern.Idx) Allocator.Error!void {
    try store.addScratch("patterns", idx);
}

/// Returns the current top of the scratch record destructures buffer.
pub fn scratchRecordDestructTop(store: *NodeStore) u32 {
    return store.scratchTop("record_destructs");
}

/// Adds a record destructure to the scratch record destructures list for building spans.
pub fn addScratchRecordDestruct(store: *NodeStore, idx: CIR.Pattern.RecordDestruct.Idx) Allocator.Error!void {
    try store.addScratch("record_destructs", idx);
}

/// Creates a pattern span from the given start position to the current top of scratch patterns.
pub fn patternSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.Pattern.Span {
    return try store.spanFrom("patterns", CIR.Pattern.Span, start);
}

/// Stores string interpolation pattern steps and returns their span.
pub fn strPatternStepSpanFromSlice(store: *NodeStore, steps: []const CIR.Pattern.StrPatternStep) Allocator.Error!CIR.Pattern.StrPatternStep.Span {
    const start: u32 = @intCast(store.pattern_str_interpolation_steps.len());
    for (steps) |step| {
        const capture_plus_one: u32 = if (step.capture) |capture|
            @backingInt(capture) + 1
        else
            0;
        _ = try store.pattern_str_interpolation_steps.append(store.gpa, .{
            .capture_plus_one = capture_plus_one,
            .delimiter = @backingInt(step.delimiter),
        });
    }
    return .{ .span = .{ .start = start, .len = @intCast(steps.len) } };
}

/// Creates a slice corresponding to a span.
pub fn sliceFromSpan(store: *const NodeStore, comptime T: type, span: base.DataSpan) []T {
    if (span.len == 0) return &.{};
    return @ptrCast(store.index_data.items.items[span.start..][0..span.len]);
}

/// Retrieve one item from a span without borrowing the backing index storage.
pub fn getFromSpan(store: *const NodeStore, comptime T: type, span: base.DataSpan, offset: usize) T {
    std.debug.assert(offset < span.len);
    return @as(T, @fromBackingInt(@intCast(store.index_data.items.items[span.start + offset])));
}

/// Returns a slice of definitions from the store.
pub fn sliceDefs(store: *const NodeStore, span: CIR.Def.Span) []CIR.Def.Idx {
    return store.sliceFromSpan(CIR.Def.Idx, span.span);
}

/// Returns a single definition index from a span.
pub fn defAt(store: *const NodeStore, span: CIR.Def.Span, offset: usize) CIR.Def.Idx {
    return store.getFromSpan(CIR.Def.Idx, span.span, offset);
}

/// Returns a slice of expressions from the store.
pub fn sliceExpr(store: *const NodeStore, span: CIR.Expr.Span) []CIR.Expr.Idx {
    return store.sliceFromSpan(CIR.Expr.Idx, span.span);
}

/// Returns a slice of `CanIR.Pattern.Idx`
pub fn slicePatterns(store: *const NodeStore, span: CIR.Pattern.Span) []CIR.Pattern.Idx {
    return store.sliceFromSpan(CIR.Pattern.Idx, span.span);
}

/// Returns a single pattern index from a span.
pub fn patternAt(store: *const NodeStore, span: CIR.Pattern.Span, offset: usize) CIR.Pattern.Idx {
    return store.getFromSpan(CIR.Pattern.Idx, span.span, offset);
}

/// Returns a slice of `CIR.Expr.Capture.Idx`
pub fn sliceCaptures(store: *const NodeStore, span: CIR.Expr.Capture.Span) []CIR.Expr.Capture.Idx {
    return store.sliceFromSpan(CIR.Expr.Capture.Idx, span.span);
}

/// Returns a slice of statements from the store.
pub fn sliceStatements(store: *const NodeStore, span: CIR.Statement.Span) []CIR.Statement.Idx {
    return store.sliceFromSpan(CIR.Statement.Idx, span.span);
}

/// Returns a single statement index from a span.
pub fn statementAt(store: *const NodeStore, span: CIR.Statement.Span, offset: usize) CIR.Statement.Idx {
    return store.getFromSpan(CIR.Statement.Idx, span.span, offset);
}

/// Returns a slice of record fields from the store.
pub fn sliceRecordFields(store: *const NodeStore, span: CIR.RecordField.Span) []CIR.RecordField.Idx {
    return store.sliceFromSpan(CIR.RecordField.Idx, span.span);
}

/// Returns a slice of unset record fields from the store.
pub fn sliceUnsetFields(store: *const NodeStore, span: CIR.UnsetField.Span) []CIR.UnsetField.Idx {
    return store.sliceFromSpan(CIR.UnsetField.Idx, span.span);
}

/// Retrieve a slice of IfBranch Idx's from a span
pub fn sliceIfBranches(store: *const NodeStore, span: CIR.Expr.IfBranch.Span) []CIR.Expr.IfBranch.Idx {
    return store.sliceFromSpan(CIR.Expr.IfBranch.Idx, span.span);
}

/// Retrieve a slice of Match.Branch Idx's from a span
pub fn sliceMatchBranches(store: *const NodeStore, span: CIR.Expr.Match.Branch.Span) []CIR.Expr.Match.Branch.Idx {
    return store.sliceFromSpan(CIR.Expr.Match.Branch.Idx, span.span);
}

/// Retrieve a slice of Match.BranchPattern Idx's from a span
pub fn sliceMatchBranchPatterns(store: *const NodeStore, span: CIR.Expr.Match.BranchPattern.Span) []CIR.Expr.Match.BranchPattern.Idx {
    return store.sliceFromSpan(CIR.Expr.Match.BranchPattern.Idx, span.span);
}

/// Returns a slice of if branches from the store.
pub fn scratchIfBranchTop(store: *NodeStore) u32 {
    return store.scratchTop("if_branches");
}

/// Adds an if branch to the scratch if branches list for building spans.
pub fn addScratchIfBranch(store: *NodeStore, if_branch_idx: CIR.Expr.IfBranch.Idx) Allocator.Error!void {
    try store.addScratch("if_branches", if_branch_idx);
}

/// Creates an if branch span from the given start position to the current top of scratch if branches.
pub fn ifBranchSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.Expr.IfBranch.Span {
    return try store.spanFrom("if_branches", CIR.Expr.IfBranch.Span, start);
}

/// Adds an if branch to the store and returns its index.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addIfBranch(store: *NodeStore, if_branch: CIR.Expr.IfBranch, region: base.Region) Allocator.Error!CIR.Expr.IfBranch.Idx {
    var node = Node.init(.if_branch);
    node.setPayload(.{ .if_branch = .{
        .cond = @backingInt(if_branch.cond),
        .body = @backingInt(if_branch.body),
    } });
    const node_idx = try store.nodes.append(store.gpa, node);
    _ = try store.regions.append(store.gpa, region);
    return @fromBackingInt(@intCast(@backingInt(node_idx)));
}

/// Returns a slice of diagnostics from the store.
pub fn sliceDiagnostics(store: *const NodeStore, span: CIR.Diagnostic.Span) []CIR.Diagnostic.Idx {
    return store.sliceFromSpan(CIR.Diagnostic.Idx, span.span);
}

/// Returns a slice of type annotations from the store.
pub fn sliceTypeAnnos(store: *const NodeStore, span: CIR.TypeAnno.Span) []CIR.TypeAnno.Idx {
    return store.sliceFromSpan(CIR.TypeAnno.Idx, span.span);
}

/// Returns a slice of exposed items from the store.
pub fn sliceExposedItems(store: *const NodeStore, span: CIR.ExposedItem.Span) []CIR.ExposedItem.Idx {
    return store.sliceFromSpan(CIR.ExposedItem.Idx, span.span);
}

/// Returns a slice of where clauses from the store.
pub fn sliceWhereClauses(store: *const NodeStore, span: CIR.WhereClause.Span) []CIR.WhereClause.Idx {
    return store.sliceFromSpan(CIR.WhereClause.Idx, span.span);
}

fn sliceWhereClauseIndices(store: *const NodeStore, span: CIR.WhereClause.IdxSpan) []CIR.WhereClause.Idx {
    return store.sliceFromSpan(CIR.WhereClause.Idx, span.span);
}

/// Returns the canonical rigid-owner groups for one where-clause scope.
pub fn sliceWhereClauseOwners(store: *const NodeStore, span: CIR.WhereClause.Span) []const WhereClauseOwnerData {
    const start: usize = span.owners.span.start;
    const end = start + span.owners.span.len;
    return store.where_clause_owners.items.items[start..end];
}

/// Returns the method clauses canonically assigned to one rigid owner.
pub fn sliceWhereClausesForOwner(store: *const NodeStore, owner: WhereClauseOwnerData) []CIR.WhereClause.Idx {
    return store.sliceFromSpan(CIR.WhereClause.Idx, .{
        .start = owner.clauses_start,
        .len = owner.clauses_len,
    });
}

/// Returns a slice of annotation record fields from the store.
pub fn sliceAnnoRecordFields(store: *const NodeStore, span: CIR.TypeAnno.RecordField.Span) []CIR.TypeAnno.RecordField.Idx {
    return store.sliceFromSpan(CIR.TypeAnno.RecordField.Idx, span.span);
}

/// Returns a slice of record destruct fields from the store.
pub fn sliceRecordDestructs(store: *const NodeStore, span: CIR.Pattern.RecordDestruct.Span) []CIR.Pattern.RecordDestruct.Idx {
    return store.sliceFromSpan(CIR.Pattern.RecordDestruct.Idx, span.span);
}

/// Retrieves one string interpolation pattern step.
pub fn getStrPatternStep(store: *const NodeStore, span: CIR.Pattern.StrPatternStep.Span, offset: u32) CIR.Pattern.StrPatternStep {
    std.debug.assert(offset < span.span.len);
    const item = store.pattern_str_interpolation_steps.items.items[span.span.start + offset];
    return .{
        .capture = if (item.capture_plus_one == 0)
            null
        else
            @as(CIR.Pattern.Idx, @fromBackingInt(@intCast(item.capture_plus_one - 1))),
        .delimiter = @fromBackingInt(@intCast(item.delimiter)),
    };
}

/// Creates a diagnostic node that stores error information.
///
/// Diagnostics are informational nodes that contain details about compilation errors.
/// They are stored separately from the main IR and are referenced by malformed nodes.
///
/// Note: This function creates diagnostic nodes for storage only.
/// To create a malformed node that represents a runtime error, use `addMalformed()` instead.
///
/// Returns: Index to the created diagnostic node
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addDiagnostic(store: *NodeStore, reason: CIR.Diagnostic) Allocator.Error!CIR.Diagnostic.Idx {
    const diag_idx = try store.addDiagnosticUnregistered(reason);
    // append to our scratch so we can get a span later of all our diagnostics
    try store.addScratch("diagnostics", diag_idx);
    return diag_idx;
}

/// Create a diagnostic node WITHOUT registering it in the reported-diagnostics
/// scratch list. Used when a malformed/runtime-error node needs a diagnostic to
/// reference, but the diagnostic that should actually be reported is decided and
/// pushed later (e.g. deferred forward-ref vs mutual-recursion classification).
pub fn addDiagnosticUnregistered(store: *NodeStore, reason: CIR.Diagnostic) Allocator.Error!CIR.Diagnostic.Idx {
    var node = Node.init(undefined);
    var region = base.Region.zero();

    switch (reason) {
        .redundant_exposed => |r| {
            node.tag = .diag_redundant_exposed;
            region = r.region;
            const region_span2_idx: u32 = @intCast(store.span2_data.len());
            _ = try store.span2_data.append(store.gpa, .{
                .start = r.original_region.start.offset,
                .len = r.original_region.end.offset,
            });
            node.setPayload(.{ .diag_single_ident_extra = .{ .ident = @bitCast(r.ident), .region_span2_idx = region_span2_idx } });
        },
        .type_parameter_conflict => |r| {
            node.tag = .diag_type_parameter_conflict;
            region = r.region;
            const region_span2_idx: u32 = @intCast(store.span2_data.len());
            _ = try store.span2_data.append(store.gpa, .{
                .start = r.original_region.start.offset,
                .len = r.original_region.end.offset,
            });
            node.setPayload(.{ .diag_two_idents_extra = .{ .ident1 = @bitCast(r.name), .ident2 = @bitCast(r.parameter_name), .region_span2_idx = region_span2_idx } });
        },
        .mutually_recursive_type_aliases => |r| {
            node.tag = .diag_mutually_recursive_type_aliases;
            region = r.region;
            const region_span2_idx: u32 = @intCast(store.span2_data.len());
            _ = try store.span2_data.append(store.gpa, .{
                .start = r.other_region.start.offset,
                .len = r.other_region.end.offset,
            });
            node.setPayload(.{ .diag_two_idents_extra = .{ .ident1 = @bitCast(r.name), .ident2 = @bitCast(r.other_name), .region_span2_idx = region_span2_idx } });
        },
        inline .not_implemented,
        .exposed_but_not_implemented,
        .provided_value_is_required,
        .invalid_num_literal,
        .empty_tuple,
        .ident_already_in_scope,
        .ident_not_in_scope,
        .read_uninitialized_var,
        .self_referential_definition,
        .circular_value_definition,
        .local_reference_before_definition,
        .mutually_recursive_local_definitions,
        .erroneous_value_use,
        .erroneous_value_expr,
        .qualified_ident_does_not_exist,
        .invalid_top_level_statement,
        .invalid_associated_statement,
        .expr_not_canonicalized,
        .range_op_chained,
        .expr_syntax_error,
        .unreachable_string_pattern_capture,
        .pattern_arg_invalid,
        .pattern_not_canonicalized,
        .if_expr_without_else,
        .malformed_type_annotation,
        .malformed_where_clause,
        .where_clause_not_allowed_in_type_decl,
        .where_alias_constraint_not_on_receiver,
        .open_ext_not_allowed_in_type_decl,
        .unnamed_field_not_allowed_in_structural_record,
        .optional_field_cannot_have_default,
        .unnamed_field_cannot_have_default,
        .default_not_allowed_in_structural_record,
        .default_not_allowed_on_local_type_decl,
        .record_default_reference_cycle,
        .var_across_function_boundary,
        .shadowing_warning,
        .binding_name_does_not_match_mutability,
        .var_in_binder_pattern,
        .type_redeclared,
        .file_import_not_found,
        .file_import_io_error,
        .file_import_absolute_path,
        .file_import_not_utf8,
        .module_not_found,
        .value_not_exposed,
        .type_not_exposed,
        .private_type_in_exposed_type,
        .private_type_in_exposed_field,
        .type_from_missing_module,
        .module_not_imported,
        .public_type_not_defined,
        .nested_type_not_found,
        .internal_builtin_type,
        .nested_value_not_found,
        .record_builder_map2_not_found,
        .too_many_exports,
        .undeclared_type,
        .undeclared_type_var,
        .type_alias_but_needed_nominal,
        .crash_expects_string,
        .type_module_missing_matching_type,
        .type_module_has_alias_not_nominal,
        .default_app_missing_main,
        .default_app_wrong_arity,
        .cannot_import_default_app,
        .type_name_case_mismatch,
        .module_header_deprecated,
        .roc_version_mismatch,
        .redundant_expose_main_type,
        .invalid_main_type_rename_in_exposing,
        .type_alias_redeclared,
        .nominal_type_redeclared,
        .type_shadowed_warning,
        .builtin_type_shadowed_warning,
        .unused_variable,
        .used_underscore_variable,
        .duplicate_record_field,
        .duplicate_pattern_binder,
        .duplicate_tag,
        .f64_pattern_literal,
        .underscore_in_type_declaration,
        .type_var_starting_with_dollar,
        .break_outside_loop,
        .infinite_loop_never_exits,
        .trailing_try_suffix,
        .redundant_return,
        .unreachable_code,
        .return_outside_fn,
        .control_flow_in_expect,
        .var_reassigned_in_expect,
        .deprecated_number_suffix,
        => |r, cir_tag| region = copiedDiagnosticNode(cir_tag, r, &node),
    }

    const nid = @backingInt(try store.nodes.append(store.gpa, node));
    _ = try store.regions.append(store.gpa, region);

    return @fromBackingInt(@intCast(nid));
}

/// Creates a malformed node that represents a runtime error in the IR.
///
/// Malformed nodes follow the "Inform Don't Block" principle: they allow compilation
/// to continue while preserving error information. When encountered during execution,
/// they will crash with the associated diagnostic.
///
/// This function:
/// 1. Creates a diagnostic node to store the error details
/// 2. Creates a malformed node (.malformed tag) that references the diagnostic
/// 3. Returns an index of the requested type that points to the malformed node
///
/// The malformed node will generate a runtime_error in the ModuleEnv that properly
/// references the diagnostic index.
///
/// IMPORTANT: You should not use this function directly! Instead, use it's
/// corresponding function in `ModuleEnv`.
pub fn addMalformed(store: *NodeStore, diagnostic_idx: CIR.Diagnostic.Idx, region: Region) Allocator.Error!Node.Idx {
    var malformed_node = Node.init(.malformed);
    malformed_node.setPayload(.{ .malformed = .{
        .diagnostic = @backingInt(diagnostic_idx),
    } });
    const malformed_nid = try store.nodes.append(store.gpa, malformed_node);
    _ = try store.regions.append(store.gpa, region);
    return malformed_nid;
}

/// Retrieves diagnostic information from a diagnostic node.
///
/// This function extracts the stored diagnostic data from nodes with .diag_* tags.
/// It reconstructs the original CIR.Diagnostic from the node's stored data.
pub fn getDiagnostic(store: *const NodeStore, diagnostic: CIR.Diagnostic.Idx) CIR.Diagnostic {
    const node_idx: Node.Idx = @fromBackingInt(@intCast(@backingInt(diagnostic)));
    const node = store.nodes.get(node_idx);
    const payload = node.getPayload();

    const tag = narrowNodeTag(DiagnosticNodeTag, node.tag) orelse
        base.invariant("{s}", .{"getDiagnostic called with non-diagnostic node - this indicates a compiler bug"});
    switch (tag) {
        .diag_redundant_exposed => {
            const p = payload.diag_single_ident_extra;
            const region_data = store.span2_data.items.items[p.region_span2_idx];
            return CIR.Diagnostic{ .redundant_exposed = .{
                .ident = @bitCast(p.ident),
                .region = store.getRegionAt(node_idx),
                .original_region = Region{
                    .start = .{ .offset = region_data.start },
                    .end = .{ .offset = region_data.len },
                },
            } };
        },
        .diag_type_parameter_conflict => {
            const p = payload.diag_two_idents_extra;
            const region_data = store.span2_data.items.items[p.region_span2_idx];
            return CIR.Diagnostic{ .type_parameter_conflict = .{
                .name = @bitCast(p.ident1),
                .parameter_name = @bitCast(p.ident2),
                .region = store.getRegionAt(node_idx),
                .original_region = .{
                    .start = .{ .offset = region_data.start },
                    .end = .{ .offset = region_data.len },
                },
            } };
        },
        .diag_mutually_recursive_type_aliases => {
            const p = payload.diag_two_idents_extra;
            const region_data = store.span2_data.items.items[p.region_span2_idx];
            return CIR.Diagnostic{ .mutually_recursive_type_aliases = .{
                .name = @bitCast(p.ident1),
                .other_name = @bitCast(p.ident2),
                .region = store.getRegionAt(node_idx),
                .other_region = .{
                    .start = .{ .offset = region_data.start },
                    .end = .{ .offset = region_data.len },
                },
            } };
        },
        inline .diag_not_implemented,
        .diag_invalid_num_literal,
        .diag_empty_tuple,
        .diag_ident_already_in_scope,
        .diagnostic_exposed_but_not_implemented,
        .diag_provided_value_is_required,
        .diag_ident_not_in_scope,
        .diag_read_uninitialized_var,
        .diag_self_referential_definition,
        .diag_circular_value_definition,
        .diag_local_reference_before_definition,
        .diag_mutually_recursive_local_definitions,
        .diag_erroneous_value_use,
        .diag_erroneous_value_expr,
        .diag_qualified_ident_does_not_exist,
        .diag_invalid_top_level_statement,
        .diag_invalid_associated_statement,
        .diag_expr_not_canonicalized,
        .diag_expr_syntax_error,
        .diag_unreachable_string_pattern_capture,
        .diag_pattern_arg_invalid,
        .diag_pattern_not_canonicalized,
        .diag_if_expr_without_else,
        .diag_var_across_function_boundary,
        .diag_shadowing_warning,
        .diag_binding_name_does_not_match_mutability,
        .diag_var_in_binder_pattern,
        .diag_type_redeclared,
        .diag_undeclared_type,
        .diag_type_alias_but_needed_nominal,
        .diag_file_import_not_found,
        .diag_file_import_io_error,
        .diag_file_import_absolute_path,
        .diag_file_import_not_utf8,
        .diag_module_not_found,
        .diag_value_not_exposed,
        .diag_type_not_exposed,
        .diag_private_type_in_exposed_type,
        .diag_private_type_in_exposed_field,
        .diag_type_from_missing_module,
        .diag_module_not_imported,
        .diag_public_type_not_defined,
        .diag_nested_type_not_found,
        .diag_internal_builtin_type,
        .diag_nested_value_not_found,
        .diag_record_builder_map2_not_found,
        .diag_too_many_exports,
        .diag_undeclared_type_var,
        .diag_malformed_type_annotation,
        .diag_malformed_where_clause,
        .diag_where_clause_not_allowed_in_type_decl,
        .diag_where_alias_constraint_not_on_receiver,
        .diag_open_ext_not_allowed_in_type_decl,
        .diag_unnamed_field_not_allowed_in_structural_record,
        .diag_optional_field_cannot_have_default,
        .diag_record_default_reference_cycle,
        .diag_type_module_missing_matching_type,
        .diag_type_module_has_alias_not_nominal,
        .diag_default_app_missing_main,
        .diag_default_app_wrong_arity,
        .diag_cannot_import_default_app,
        .diag_type_name_case_mismatch,
        .diag_module_header_deprecated,
        .diag_roc_version_mismatch,
        .diag_redundant_expose_main_type,
        .diag_invalid_main_type_rename_in_exposing,
        .diag_type_alias_redeclared,
        .diag_nominal_type_redeclared,
        .diag_type_shadowed_warning,
        .diag_builtin_type_shadowed_warning,
        .diag_unused_variable,
        .diag_used_underscore_variable,
        .diag_duplicate_record_field,
        .diag_duplicate_pattern_binder,
        .diag_duplicate_tag,
        .diag_crash_expects_string,
        .diag_f64_pattern_literal,
        .diag_type_var_starting_with_dollar,
        .diag_underscore_in_type_declaration,
        .diag_break_outside_loop,
        .diag_infinite_loop_never_exits,
        .diag_trailing_try_suffix,
        .diag_return_outside_fn,
        .diag_redundant_return,
        .diag_unreachable_code,
        .diag_control_flow_in_expect,
        .diag_var_reassigned_in_expect,
        .diag_deprecated_number_suffix,
        .diag_range_op_chained,
        .diag_unnamed_field_cannot_have_default,
        .diag_default_not_allowed_in_structural_record,
        .diag_default_not_allowed_on_local_type_decl,
        => |node_tag| return copiedDiagnostic(store, node_idx, node_tag, payload),
    }
}

/// Computes the span of a diagnostic starting from a given index.
pub fn diagnosticSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.Diagnostic.Span {
    return try store.spanFrom("diagnostics", CIR.Diagnostic.Span, start);
}

/// Return the current top index for scratch match branches.
pub fn scratchMatchBranchTop(store: *NodeStore) u32 {
    return store.scratchTop("match_branches");
}

/// Add a match branch index to the scratch buffer.
pub fn addScratchMatchBranch(store: *NodeStore, branch_idx: CIR.Expr.Match.Branch.Idx) Allocator.Error!void {
    try store.addScratch("match_branches", branch_idx);
}

/// Create a span from the scratch match branches starting at the given index.
pub fn matchBranchSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.Expr.Match.Branch.Span {
    return try store.spanFrom("match_branches", CIR.Expr.Match.Branch.Span, start);
}

/// Return the current top index for scratch match branch patterns.
pub fn scratchMatchBranchPatternTop(store: *NodeStore) u32 {
    return store.scratchTop("match_branch_patterns");
}

/// Add a match branch pattern index to the scratch buffer.
pub fn addScratchMatchBranchPattern(store: *NodeStore, pattern_idx: CIR.Expr.Match.BranchPattern.Idx) Allocator.Error!void {
    try store.addScratch("match_branch_patterns", pattern_idx);
}

/// Create a span from the scratch match branch patterns starting at the given index.
pub fn matchBranchPatternSpanFrom(store: *NodeStore, start: u32) Allocator.Error!CIR.Expr.Match.BranchPattern.Span {
    return try store.spanFrom("match_branch_patterns", CIR.Expr.Match.BranchPattern.Span, start);
}

/// Serialized representation of NodeStore
/// Uses extern struct to guarantee consistent field layout across optimization levels.
/// IMPORTANT: int128_values is placed first (after gpa) to ensure 16-byte alignment
/// since i128 requires 16-byte alignment and the buffer start is well-aligned.
pub const Serialized = extern struct {
    gpa: [2]u64, // Reserve enough space for 2 64-bit pointers (16 bytes total)
    int128_values: collections.SafeList(i128).Serialized, // Must be first data field for 16-byte alignment
    literal_dispatch_plans: collections.SafeList(LiteralDispatchPlan).Serialized,
    literal_pattern_contexts: collections.SafeList(LiteralPatternContext).Serialized,
    interpolation_data: collections.SafeList(InterpolationData).Serialized,
    nodes: Node.List.Serialized,
    replaced_source_nodes: Node.List.Serialized,
    regions: Region.List.Serialized,
    write_occurrences: collections.SafeList(WriteOccurrence).Serialized,
    span2_data: collections.SafeList(Span2).Serialized,
    span_with_node_data: collections.SafeList(SpanWithNode).Serialized,
    method_call_data: collections.SafeList(MethodCallData).Serialized,
    match_data: collections.SafeList(MatchData).Serialized,
    if_data: collections.SafeList(IfData).Serialized,
    match_branch_data: collections.SafeList(MatchBranchData).Serialized,
    closure_data: collections.SafeList(ClosureData).Serialized,
    zero_arg_tag_data: collections.SafeList(ZeroArgTagData).Serialized,
    def_data: collections.SafeList(DefData).Serialized,
    import_data: collections.SafeList(ImportData).Serialized,
    type_apply_data: collections.SafeList(TypeApplyData).Serialized,
    pattern_list_data: collections.SafeList(PatternListData).Serialized,
    pattern_str_interpolation_data: collections.SafeList(PatternStrInterpolationData).Serialized,
    pattern_str_interpolation_steps: collections.SafeList(PatternStrInterpolationStepData).Serialized,
    where_clause_owners: collections.SafeList(WhereClauseOwnerData).Serialized,
    index_data: collections.SafeList(u32).Serialized,
    scratch: u64, // Reserve enough space for a 64-bit pointer

    comptime {
        collections.serde_validation.assertBidirectionalFieldSet(NodeStore, Serialized, &.{}, &.{}, &.{});
    }

    /// Serialize a NodeStore into this Serialized struct, appending data to the writer
    pub fn serialize(
        self: *Serialized,
        store: *const NodeStore,
        allocator: Allocator,
        writer: *CompactWriter,
    ) Allocator.Error!void {
        // Lists are written in this struct's field order, so int128_values
        // goes first to ensure 16-byte alignment (i128 requires it).
        inline for (comptime backingLists(Serialized)) |name| {
            try @field(self, name).serialize(&@field(store, name), allocator, writer);
        }
    }

    /// Deserialize into a NodeStore value (no in-place modification of cache buffer).
    /// The base_addr parameter is the base address of the serialized buffer in memory.
    /// WARNING: The returned NodeStore points into the cache buffer and is read-only,
    /// so it has no scratch memory.
    /// Use deserializeWithCopy() if the store needs to be mutable.
    pub fn deserializeInto(self: *const Serialized, base_addr: usize, gpa: Allocator) NodeStore {
        var store = initEmpty(gpa);
        inline for (comptime backingLists(NodeStore)) |name| {
            @field(store, name) = @field(self, name).deserializeInto(base_addr);
        }
        return store;
    }

    /// Deserialize into a NodeStore value with fresh memory allocation for fields that may need to grow.
    /// Use this for cache modules where regions may need to be extended during type checking.
    pub fn deserializeWithCopy(self: *const Serialized, base_addr: usize, gpa: Allocator) Allocator.Error!NodeStore {
        var store = self.deserializeInto(base_addr, gpa);
        // Regions needs to be mutable (grown during type checking)
        store.regions = try self.regions.deserializeWithCopy(base_addr, gpa);
        return store;
    }

    /// Deserialize into a NodeStore that owns every list it holds and carries
    /// its own scratch buffers, so later compilation stages may append nodes,
    /// regions, and extra data to it. `deinit` releases it exactly like a
    /// freshly constructed `NodeStore`.
    pub fn deserializeOwned(self: *const Serialized, base_addr: usize, gpa: Allocator) Allocator.Error!NodeStore {
        var store = initEmpty(gpa);
        errdefer store.deinit();
        inline for (comptime backingLists(NodeStore)) |name| {
            @field(store, name) = try @field(self, name).deserializeWithCopy(base_addr, gpa);
        }
        try store.ensureScratch();
        return store;
    }
};

test "NodeStore round-trips every backing list" {
    const testing = std.testing;
    const gpa = testing.allocator;
    const lists = comptime backingLists(NodeStore);

    // Every list gets a distinct, nonzero number of entries, so a conversion
    // that skips a list or crosses two of them changes a length.
    var original = initEmpty(gpa);
    defer original.deinit();
    inline for (lists, 1..) |name, entry_count| {
        const Entry = @FieldType(Serialized, name).SerializedElement;
        for (0..entry_count) |_| _ = try @field(original, name).append(gpa, std.mem.zeroes(Entry));
    }

    var cloned = try original.clone(gpa);
    defer cloned.deinit();

    var writer = CompactWriter.init();
    defer writer.deinit(gpa);
    const serialized = try writer.appendAlloc(gpa, Serialized);
    try serialized.serialize(&original, gpa, &writer);
    const buffer = try gpa.alignedAlloc(u8, .@"16", @intCast(writer.total_bytes));
    defer gpa.free(buffer);
    _ = try writer.writeToBuffer(buffer);
    const loaded: *const Serialized = @ptrCast(@alignCast(buffer.ptr));
    const base_addr = @intFromPtr(buffer.ptr);

    const borrowed = loaded.deserializeInto(base_addr, gpa);
    var regions_owned = try loaded.deserializeWithCopy(base_addr, gpa);
    defer regions_owned.regions.deinit(gpa);
    var owned = try loaded.deserializeOwned(base_addr, gpa);
    defer owned.deinit();

    inline for (lists, 1..) |name, entry_count| {
        inline for (.{ &cloned, &borrowed, &regions_owned, &owned }) |store| {
            try testing.expectEqual(@as(u64, entry_count), @field(store, name).len());
        }
    }

    // Serializing the reloaded store reproduces the original bytes.
    var rewriter = CompactWriter.init();
    defer rewriter.deinit(gpa);
    const reserialized = try rewriter.appendAlloc(gpa, Serialized);
    try reserialized.serialize(&owned, gpa, &rewriter);
    const rebuffer = try gpa.alignedAlloc(u8, .@"16", @intCast(rewriter.total_bytes));
    defer gpa.free(rebuffer);
    _ = try rewriter.writeToBuffer(rebuffer);
    try testing.expectEqualSlices(u8, buffer, rebuffer);
}

test "NodeStore empty CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create an empty NodeStore
    var original = try NodeStore.init(gpa);
    defer original.deinit();

    // Create a temp file
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile(std.testing.io, "test_empty_nodestore.dat", .{ .read = true });
    defer file.close(std.testing.io);

    // Serialize using CompactWriter
    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    const serialized = try writer.appendAlloc(gpa, NodeStore.Serialized);
    try serialized.serialize(&original, gpa, &writer);

    // Write to file
    try writer.writeGather(file, std.testing.io);

    // Read back
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @intCast(writer.total_bytes));
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(std.testing.io, buffer, 0);

    // Cast and deserialize
    const serialized_ptr: *NodeStore.Serialized = @ptrCast(@alignCast(buffer.ptr));
    const deserialized = serialized_ptr.deserializeInto(@intFromPtr(buffer.ptr), gpa);

    // Verify empty
    try testing.expectEqual(@as(usize, 0), deserialized.nodes.len());
    try testing.expectEqual(@as(usize, 0), deserialized.regions.len());
    try testing.expectEqual(@as(usize, 0), deserialized.int128_values.len());
}

test "NodeStore basic CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create NodeStore and add some nodes
    var original = try NodeStore.init(gpa);
    defer original.deinit();

    // Add integer value to int128_values
    const value: i128 = 42;
    const int128_idx = try original.int128_values.append(gpa, value);

    // Add a numeric expression node using the int128 value
    var node1 = Node.init(.expr_num);
    node1.setPayload(.{
        .expr_num = .{
            .kind = 0, // Integer kind
            .val_kind = 0,
            .int128_idx = @backingInt(int128_idx),
        },
    });
    const node1_idx = try original.nodes.append(gpa, node1);
    try original.recordLiteralDispatchPlan(node1_idx, .numeral, @fromBackingInt(@intCast(7)), @fromBackingInt(@intCast(9)), 11);
    original.recordLiteralPatternEquality(node1_idx, @fromBackingInt(@intCast(13)));
    original.finalizeLiteralDispatchResolution(node1_idx, .builtin_direct);

    // Add a region
    const region = Region{
        .start = .{ .offset = 0 },
        .end = .{ .offset = 5 },
    };
    const region1_idx = try original.regions.append(gpa, region);

    // Create a temp file
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile(std.testing.io, "test_basic_nodestore.dat", .{ .read = true });
    defer file.close(std.testing.io);

    // Serialize
    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    const serialized = try writer.appendAlloc(gpa, NodeStore.Serialized);
    try serialized.serialize(&original, gpa, &writer);

    // Write to file
    try writer.writeGather(file, std.testing.io);

    // Read back
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @intCast(writer.total_bytes));
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(std.testing.io, buffer, 0);

    // Cast and deserialize
    const serialized_ptr: *NodeStore.Serialized = @ptrCast(@alignCast(buffer.ptr));
    const deserialized = serialized_ptr.deserializeInto(@intFromPtr(buffer.ptr), gpa);

    // Verify nodes
    try testing.expectEqual(@as(usize, 1), deserialized.nodes.len());
    const retrieved_node = deserialized.nodes.get(node1_idx);
    try testing.expectEqual(Node.Tag.expr_num, retrieved_node.tag);
    try testing.expectEqual(@as(u32, 0), retrieved_node.getPayload().expr_num.int128_idx);

    // Verify int128_values
    try testing.expectEqual(@as(usize, 1), deserialized.int128_values.len());
    const retrieved_value = deserialized.int128_values.items.items[0];
    try testing.expectEqual(@as(i128, 42), retrieved_value);
    const literal_plan = deserialized.literalDispatchPlanForNode(node1_idx).?;
    try testing.expectEqual(LiteralDispatchPlan.Kind.numeral, literal_plan.dispatchKind());
    try testing.expectEqual(@as(u32, 7), literal_plan.target_var);
    try testing.expectEqual(@as(u32, 9), @backingInt(literal_plan.fnVar().?));
    try testing.expectEqual(LiteralDispatchPlan.Resolution.builtin_direct, literal_plan.dispatchResolution());
    try testing.expectEqual(@as(?u32, 11), literal_plan.patternFailureOwner(&deserialized));
    try testing.expectEqual(@as(u32, 14), literal_plan.patternContext(&deserialized).?.equality_fn_var_plus_one);

    // Verify regions
    try testing.expectEqual(@as(usize, 1), deserialized.regions.len());
    const retrieved_region = deserialized.regions.get(region1_idx);
    try testing.expectEqual(region.start.offset, retrieved_region.start.offset);
    try testing.expectEqual(region.end.offset, retrieved_region.end.offset);
}

test "literal dispatch plans are retired with their owning nodes" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var store = try NodeStore.init(gpa);
    defer store.deinit();

    const quote_expr = try store.addExpr(
        CIR.Expr.initStr(.{ .span = .{ .start = 0, .len = 0 } }),
        Region.zero(),
    );
    const numeral_expr = try store.addExpr(.{ .e_num = .{
        .value = .{ .bytes = @bitCast(@as(i128, 0)), .kind = .i128 },
        .kind = .num_unbound,
    } }, Region.zero());

    try store.recordLiteralDispatchPlan(
        @fromBackingInt(@intCast(@backingInt(quote_expr))),
        .quote,
        @fromBackingInt(@intCast(1)),
        @fromBackingInt(@intCast(2)),
        null,
    );
    try store.recordLiteralDispatchPlan(
        @fromBackingInt(@intCast(@backingInt(numeral_expr))),
        .numeral,
        @fromBackingInt(@intCast(3)),
        @fromBackingInt(@intCast(4)),
        17,
    );
    try testing.expectEqual(@as(usize, 2), store.literalDispatchPlans().len);

    const runtime_error_diagnostic = try store.addDiagnosticUnregistered(.{ .erroneous_value_expr = .{
        .region = Region.zero(),
    } });

    try store.replaceExprWithRuntimeError(quote_expr, runtime_error_diagnostic);
    try testing.expect(store.literalDispatchPlanForNode(@fromBackingInt(@intCast(@backingInt(quote_expr)))) == null);
    try testing.expectEqual(@as(usize, 1), store.literalDispatchPlans().len);

    const numeral_plan = store.literalDispatchPlanForNode(@fromBackingInt(@intCast(@backingInt(numeral_expr)))).?;
    try testing.expectEqual(LiteralDispatchPlan.Kind.numeral, numeral_plan.dispatchKind());
    try testing.expectEqual(@as(?u32, 17), numeral_plan.patternFailureOwner(&store));
    try testing.expectEqual(@as(u32, 3), numeral_plan.target_var);
    try testing.expectEqual(@as(u32, 4), @backingInt(numeral_plan.fnVar().?));

    try store.replaceExprWithRuntimeError(numeral_expr, runtime_error_diagnostic);
    try testing.expectEqual(@as(usize, 0), store.literalDispatchPlans().len);
}

test "NodeStore multiple nodes CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create NodeStore with various node types
    var original = try NodeStore.init(gpa);
    defer original.deinit();

    // Add expression variable node
    var var_node = Node.init(.expr_var);
    var_node.setPayload(.{ .expr_var = .{
        .pattern_idx = 5,
    } });
    const var_node_idx = try original.nodes.append(gpa, var_node);

    // Add expression list node
    var list_node = Node.init(.expr_list);
    list_node.setPayload(.{ .expr_list = .{
        .elems_start = 10,
        .elems_len = 3,
    } });
    const list_node_idx = try original.nodes.append(gpa, list_node);

    // Add float node with inline value (f64 stored as two u32s in payload)
    const float_value: f64 = 3.14159;
    const float_as_u64: u64 = @bitCast(float_value);
    const float_lo: u32 = @truncate(float_as_u64);
    const float_hi: u32 = @truncate(float_as_u64 >> 32);
    var float_node = Node.init(.expr_frac_f64);
    float_node.setPayload(.{ .expr_frac_f64 = .{
        .value_lo = float_lo,
        .value_hi = float_hi,
        .has_suffix = false,
    } });
    const float_node_idx = try original.nodes.append(gpa, float_node);

    // Add both field-access modes explicitly. These bytes previously belonged
    // to payload padding, so this roundtrip pins that serialization preserves
    // the new semantic discriminant rather than zeroing it.
    var required_segment_node = Node.init(.field_access_segment);
    required_segment_node.setPayload(.{ .field_access_segment = .{
        .name = 41,
        .mode = @backingInt(CIR.Expr.FieldAccessMode.required),
    } });
    const required_segment_node_idx = try original.nodes.append(gpa, required_segment_node);

    var optional_segment_node = Node.init(.field_access_segment);
    optional_segment_node.setPayload(.{ .field_access_segment = .{
        .name = 42,
        .mode = @backingInt(CIR.Expr.FieldAccessMode.optional),
    } });
    const optional_segment_node_idx = try original.nodes.append(gpa, optional_segment_node);

    // Add regions for each node
    const region1 = Region{ .start = .{ .offset = 0 }, .end = .{ .offset = 5 } };
    const region2 = Region{ .start = .{ .offset = 10 }, .end = .{ .offset = 20 } };
    const region3 = Region{ .start = .{ .offset = 25 }, .end = .{ .offset = 32 } };
    const region4 = Region{ .start = .{ .offset = 40 }, .end = .{ .offset = 49 } };
    const region5 = Region{ .start = .{ .offset = 50 }, .end = .{ .offset = 59 } };
    const region1_idx = try original.regions.append(gpa, region1);
    const region2_idx = try original.regions.append(gpa, region2);
    const region3_idx = try original.regions.append(gpa, region3);
    const region4_idx = try original.regions.append(gpa, region4);
    const region5_idx = try original.regions.append(gpa, region5);

    // Create a temp file
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile(std.testing.io, "test_multiple_nodestore.dat", .{ .read = true });
    defer file.close(std.testing.io);

    // Serialize
    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    const serialized = try writer.appendAlloc(gpa, NodeStore.Serialized);
    try serialized.serialize(&original, gpa, &writer);

    // Write to file
    try writer.writeGather(file, std.testing.io);

    // Read back
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @intCast(writer.total_bytes));
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(std.testing.io, buffer, 0);

    // Cast and deserialize
    const serialized_ptr: *NodeStore.Serialized = @ptrCast(@alignCast(buffer.ptr));
    const deserialized = serialized_ptr.deserializeInto(@intFromPtr(buffer.ptr), gpa);

    // Verify nodes
    try testing.expectEqual(@as(usize, 5), deserialized.nodes.len());

    // Verify var node using captured index
    const retrieved_var = deserialized.nodes.get(var_node_idx);
    try testing.expectEqual(Node.Tag.expr_var, retrieved_var.tag);
    try testing.expectEqual(@as(u32, 5), retrieved_var.getPayload().expr_var.pattern_idx);

    // Verify list node using captured index
    const retrieved_list = deserialized.nodes.get(list_node_idx);
    try testing.expectEqual(Node.Tag.expr_list, retrieved_list.tag);
    try testing.expectEqual(@as(u32, 10), retrieved_list.getPayload().expr_list.elems_start);
    try testing.expectEqual(@as(u32, 3), retrieved_list.getPayload().expr_list.elems_len);

    // Verify float node using captured index
    const retrieved_float = deserialized.nodes.get(float_node_idx);
    try testing.expectEqual(Node.Tag.expr_frac_f64, retrieved_float.tag);
    // Verify float value stored inline in payload
    const payload = retrieved_float.getPayload().expr_frac_f64;
    const retrieved_u64: u64 = @as(u64, payload.value_hi) << 32 | payload.value_lo;
    const retrieved_float_value: f64 = @bitCast(retrieved_u64);
    try testing.expectApproxEqAbs(float_value, retrieved_float_value, 0.0001);

    const required_segment = deserialized.getFieldAccessSegment(@fromBackingInt(@intCast(@backingInt(required_segment_node_idx))));
    try testing.expectEqual(@as(u32, 41), @as(u32, @bitCast(required_segment.name)));
    try testing.expectEqual(CIR.Expr.FieldAccessMode.required, required_segment.mode);
    const optional_segment = deserialized.getFieldAccessSegment(@fromBackingInt(@intCast(@backingInt(optional_segment_node_idx))));
    try testing.expectEqual(@as(u32, 42), @as(u32, @bitCast(optional_segment.name)));
    try testing.expectEqual(CIR.Expr.FieldAccessMode.optional, optional_segment.mode);

    // Verify regions using captured indices
    try testing.expectEqual(@as(usize, 5), deserialized.regions.len());
    const retrieved_region1 = deserialized.regions.get(region1_idx);
    try testing.expectEqual(region1.start.offset, retrieved_region1.start.offset);
    try testing.expectEqual(region1.end.offset, retrieved_region1.end.offset);
    const retrieved_region2 = deserialized.regions.get(region2_idx);
    try testing.expectEqual(region2.start.offset, retrieved_region2.start.offset);
    try testing.expectEqual(region2.end.offset, retrieved_region2.end.offset);
    const retrieved_region3 = deserialized.regions.get(region3_idx);
    try testing.expectEqual(region3.start.offset, retrieved_region3.start.offset);
    try testing.expectEqual(region3.end.offset, retrieved_region3.end.offset);
    const retrieved_region4 = deserialized.regions.get(region4_idx);
    try testing.expectEqual(region4.start.offset, retrieved_region4.start.offset);
    try testing.expectEqual(region4.end.offset, retrieved_region4.end.offset);
    const retrieved_region5 = deserialized.regions.get(region5_idx);
    try testing.expectEqual(region5.start.offset, retrieved_region5.start.offset);
    try testing.expectEqual(region5.end.offset, retrieved_region5.end.offset);

    // Verify scratch is null (deserialized NodeStores don't allocate scratch)
    try testing.expect(deserialized.scratch == null);
}
