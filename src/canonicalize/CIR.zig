//! Common IR types and utilities
//! This module contains type definitions and utilities used across the canonicalization IR.

const std = @import("std");
const Allocator = std.mem.Allocator;
const types_mod = @import("types");
const collections = @import("collections");
const base = @import("base");
const reporting = @import("reporting");
const builtins = @import("builtins");

const CompactWriter = collections.CompactWriter;
const Ident = base.Ident;
const StringLiteral = base.StringLiteral;
const Region = base.Region;
const SExprTree = base.SExprTree;
const TypeVar = types_mod.Var;

// Re-export these from other modules for convenience
pub const NodeStore = @import("NodeStore.zig");
pub const Node = @import("Node.zig");
pub const Expr = @import("Expression.zig").Expr;
pub const Pattern = @import("Pattern.zig").Pattern;
pub const Statement = @import("Statement.zig").Statement;
pub const TypeAnno = @import("TypeAnnotation.zig").TypeAnno;
pub const Diagnostic = @import("Diagnostic.zig").Diagnostic;

/// Indices of builtin type declarations within the Builtin module.
/// Generated at build time into typed static data embedded in the compiler.
/// Contains both statement indices (positions within Builtin.bin) and ident indices
/// (interned identifiers for comparison without string lookups).
pub const BuiltinIndices = struct {
    // Statement indices - positions within the Builtin module
    bool_type: Statement.Idx,
    try_type: Statement.Idx,
    dict_type: Statement.Idx,
    set_type: Statement.Idx,
    str_type: Statement.Idx,
    hasher_type: Statement.Idx,
    iter_type: Statement.Idx,
    stream_type: Statement.Idx,
    list_type: Statement.Idx,
    box_type: Statement.Idx,
    parse_tag_union_spec_type: Statement.Idx,
    fields_type: Statement.Idx,
    field_type: Statement.Idx,
    json_state_type: Statement.Idx,
    json_encode_state_type: Statement.Idx,
    json_encoding_type: Statement.Idx,
    json_type: Statement.Idx,
    http_header_state_type: Statement.Idx,
    http_header_encoding_type: Statement.Idx,
    http_header_type: Statement.Idx,
    utf8_problem_type: Statement.Idx,
    u8_type: Statement.Idx,
    i8_type: Statement.Idx,
    u16_type: Statement.Idx,
    i16_type: Statement.Idx,
    u32_type: Statement.Idx,
    i32_type: Statement.Idx,
    u64_type: Statement.Idx,
    i64_type: Statement.Idx,
    u128_type: Statement.Idx,
    i128_type: Statement.Idx,
    dec_type: Statement.Idx,
    f32_type: Statement.Idx,
    f64_type: Statement.Idx,
    numeral_type: Statement.Idx,
    crypto_type: Statement.Idx,
    crypto_digest_bytes_err_type: Statement.Idx,
    crypto_digest_hex_err_type: Statement.Idx,
    crypto_sha256_digest_type: Statement.Idx,
    crypto_sha256_hasher_type: Statement.Idx,
    crypto_blake3_digest_type: Statement.Idx,
    crypto_blake3_hasher_type: Statement.Idx,

    // Ident indices - simple unqualified names (e.g., "Bool", "U8")
    bool_ident: Ident.Idx,
    try_ident: Ident.Idx,
    dict_ident: Ident.Idx,
    set_ident: Ident.Idx,
    str_ident: Ident.Idx,
    hasher_ident: Ident.Idx,
    iter_ident: Ident.Idx,
    stream_ident: Ident.Idx,
    list_ident: Ident.Idx,
    box_ident: Ident.Idx,
    parse_tag_union_spec_ident: Ident.Idx,
    fields_ident: Ident.Idx,
    field_ident: Ident.Idx,
    json_state_ident: Ident.Idx,
    json_encode_state_ident: Ident.Idx,
    json_encoding_ident: Ident.Idx,
    json_ident: Ident.Idx,
    http_header_state_ident: Ident.Idx,
    http_header_encoding_ident: Ident.Idx,
    http_header_ident: Ident.Idx,
    utf8_problem_ident: Ident.Idx,
    u8_ident: Ident.Idx,
    i8_ident: Ident.Idx,
    u16_ident: Ident.Idx,
    i16_ident: Ident.Idx,
    u32_ident: Ident.Idx,
    i32_ident: Ident.Idx,
    u64_ident: Ident.Idx,
    i64_ident: Ident.Idx,
    u128_ident: Ident.Idx,
    i128_ident: Ident.Idx,
    dec_ident: Ident.Idx,
    f32_ident: Ident.Idx,
    f64_ident: Ident.Idx,
    numeral_ident: Ident.Idx,
    crypto_ident: Ident.Idx,
    crypto_digest_bytes_err_ident: Ident.Idx,
    crypto_digest_hex_err_ident: Ident.Idx,
    crypto_sha256_digest_ident: Ident.Idx,
    crypto_sha256_hasher_ident: Ident.Idx,
    crypto_blake3_digest_ident: Ident.Idx,
    crypto_blake3_hasher_ident: Ident.Idx,
    // Tag idents for Try type
    ok_ident: Ident.Idx,
    err_ident: Ident.Idx,

    /// Convert a nominal type's ident to a NumKind, if it's a builtin numeric type.
    /// This allows direct ident comparison instead of string comparison for type identification.
    pub fn numKindFromIdent(self: BuiltinIndices, ident: Ident.Idx) ?NumKind {
        inline for (builtin_type_specs) |spec| {
            if (spec.num_kind) |num_kind| {
                if (ident.eql(@field(self, spec.ident_field))) return num_kind;
            }
        }
        return null;
    }
};

/// How the builtin compiler should locate a type declaration in Builtin.roc.
pub const BuiltinTypeLookup = union(enum) {
    top_level: []const u8,
    nested: struct {
        parent: []const u8,
        name: []const u8,
    },
    qualified: []const u8,
};

/// Static registry entry describing one builtin type that the compiler needs by index.
pub const BuiltinTypeSpec = struct {
    display_name: []const u8,
    qualified_name: []const u8,
    type_field: []const u8,
    ident_field: []const u8,
    lookup: BuiltinTypeLookup,
    num_kind: ?NumKind = null,
    auto_import: bool = true,
};

/// Ordered builtin type registry shared by builtin generation and runtime validation.
pub const builtin_type_specs = [_]BuiltinTypeSpec{
    .{ .display_name = "Bool", .qualified_name = "Builtin.Bool", .type_field = "bool_type", .ident_field = "bool_ident", .lookup = .{ .top_level = "Bool" } },
    .{ .display_name = "Try", .qualified_name = "Builtin.Try", .type_field = "try_type", .ident_field = "try_ident", .lookup = .{ .top_level = "Try" } },
    .{ .display_name = "Dict", .qualified_name = "Builtin.Dict", .type_field = "dict_type", .ident_field = "dict_ident", .lookup = .{ .top_level = "Dict" } },
    .{ .display_name = "Set", .qualified_name = "Builtin.Set", .type_field = "set_type", .ident_field = "set_ident", .lookup = .{ .top_level = "Set" } },
    .{ .display_name = "Str", .qualified_name = "Builtin.Str", .type_field = "str_type", .ident_field = "str_ident", .lookup = .{ .top_level = "Str" } },
    .{ .display_name = "Hasher", .qualified_name = "Builtin.Hasher", .type_field = "hasher_type", .ident_field = "hasher_ident", .lookup = .{ .top_level = "Hasher" } },
    .{ .display_name = "Iter", .qualified_name = "Builtin.Iter", .type_field = "iter_type", .ident_field = "iter_ident", .lookup = .{ .top_level = "Iter" } },
    .{ .display_name = "Stream", .qualified_name = "Builtin.Stream", .type_field = "stream_type", .ident_field = "stream_ident", .lookup = .{ .top_level = "Stream" } },
    .{ .display_name = "List", .qualified_name = "Builtin.List", .type_field = "list_type", .ident_field = "list_ident", .lookup = .{ .top_level = "List" } },
    .{ .display_name = "Box", .qualified_name = "Builtin.Box", .type_field = "box_type", .ident_field = "box_ident", .lookup = .{ .top_level = "Box" } },
    .{ .display_name = "ParseTagUnionSpec", .qualified_name = "Builtin.Encoding.ParseTagUnionSpec", .type_field = "parse_tag_union_spec_type", .ident_field = "parse_tag_union_spec_ident", .lookup = .{ .nested = .{ .parent = "Encoding", .name = "ParseTagUnionSpec" } }, .auto_import = false },
    .{ .display_name = "FieldNames", .qualified_name = "Builtin.Encoding.FieldName.FieldNames", .type_field = "fields_type", .ident_field = "fields_ident", .lookup = .{ .qualified = "Builtin.Encoding.FieldName.FieldNames" }, .auto_import = false },
    .{ .display_name = "FieldName", .qualified_name = "Builtin.Encoding.FieldName", .type_field = "field_type", .ident_field = "field_ident", .lookup = .{ .qualified = "Builtin.Encoding.FieldName" }, .auto_import = false },
    .{ .display_name = "JsonState", .qualified_name = "Builtin.Encoding.JsonState", .type_field = "json_state_type", .ident_field = "json_state_ident", .lookup = .{ .qualified = "Builtin.Encoding.JsonState" }, .auto_import = false },
    .{ .display_name = "JsonEncodeState", .qualified_name = "Builtin.Encoding.JsonEncodeState", .type_field = "json_encode_state_type", .ident_field = "json_encode_state_ident", .lookup = .{ .qualified = "Builtin.Encoding.JsonEncodeState" }, .auto_import = false },
    .{ .display_name = "JsonEncoding", .qualified_name = "Builtin.Encoding.JsonEncoding", .type_field = "json_encoding_type", .ident_field = "json_encoding_ident", .lookup = .{ .qualified = "Builtin.Encoding.JsonEncoding" }, .auto_import = false },
    .{ .display_name = "Json", .qualified_name = "Builtin.Encoding.Json", .type_field = "json_type", .ident_field = "json_ident", .lookup = .{ .nested = .{ .parent = "Encoding", .name = "Json" } } },
    .{ .display_name = "HttpHeaderState", .qualified_name = "Builtin.Encoding.HttpHeaderState", .type_field = "http_header_state_type", .ident_field = "http_header_state_ident", .lookup = .{ .qualified = "Builtin.Encoding.HttpHeaderState" }, .auto_import = false },
    .{ .display_name = "HttpHeaderEncoding", .qualified_name = "Builtin.Encoding.HttpHeaderEncoding", .type_field = "http_header_encoding_type", .ident_field = "http_header_encoding_ident", .lookup = .{ .qualified = "Builtin.Encoding.HttpHeaderEncoding" }, .auto_import = false },
    .{ .display_name = "HttpHeader", .qualified_name = "Builtin.Encoding.HttpHeader", .type_field = "http_header_type", .ident_field = "http_header_ident", .lookup = .{ .nested = .{ .parent = "Encoding", .name = "HttpHeader" } }, .auto_import = false },
    .{ .display_name = "Utf8Problem", .qualified_name = "Builtin.Str.Utf8Problem", .type_field = "utf8_problem_type", .ident_field = "utf8_problem_ident", .lookup = .{ .nested = .{ .parent = "Str", .name = "Utf8Problem" } } },
    .{ .display_name = "U8", .qualified_name = "Builtin.Num.U8", .type_field = "u8_type", .ident_field = "u8_ident", .lookup = .{ .nested = .{ .parent = "Num", .name = "U8" } }, .num_kind = .u8 },
    .{ .display_name = "I8", .qualified_name = "Builtin.Num.I8", .type_field = "i8_type", .ident_field = "i8_ident", .lookup = .{ .nested = .{ .parent = "Num", .name = "I8" } }, .num_kind = .i8 },
    .{ .display_name = "U16", .qualified_name = "Builtin.Num.U16", .type_field = "u16_type", .ident_field = "u16_ident", .lookup = .{ .nested = .{ .parent = "Num", .name = "U16" } }, .num_kind = .u16 },
    .{ .display_name = "I16", .qualified_name = "Builtin.Num.I16", .type_field = "i16_type", .ident_field = "i16_ident", .lookup = .{ .nested = .{ .parent = "Num", .name = "I16" } }, .num_kind = .i16 },
    .{ .display_name = "U32", .qualified_name = "Builtin.Num.U32", .type_field = "u32_type", .ident_field = "u32_ident", .lookup = .{ .nested = .{ .parent = "Num", .name = "U32" } }, .num_kind = .u32 },
    .{ .display_name = "I32", .qualified_name = "Builtin.Num.I32", .type_field = "i32_type", .ident_field = "i32_ident", .lookup = .{ .nested = .{ .parent = "Num", .name = "I32" } }, .num_kind = .i32 },
    .{ .display_name = "U64", .qualified_name = "Builtin.Num.U64", .type_field = "u64_type", .ident_field = "u64_ident", .lookup = .{ .nested = .{ .parent = "Num", .name = "U64" } }, .num_kind = .u64 },
    .{ .display_name = "I64", .qualified_name = "Builtin.Num.I64", .type_field = "i64_type", .ident_field = "i64_ident", .lookup = .{ .nested = .{ .parent = "Num", .name = "I64" } }, .num_kind = .i64 },
    .{ .display_name = "U128", .qualified_name = "Builtin.Num.U128", .type_field = "u128_type", .ident_field = "u128_ident", .lookup = .{ .nested = .{ .parent = "Num", .name = "U128" } }, .num_kind = .u128 },
    .{ .display_name = "I128", .qualified_name = "Builtin.Num.I128", .type_field = "i128_type", .ident_field = "i128_ident", .lookup = .{ .nested = .{ .parent = "Num", .name = "I128" } }, .num_kind = .i128 },
    .{ .display_name = "Dec", .qualified_name = "Builtin.Num.Dec", .type_field = "dec_type", .ident_field = "dec_ident", .lookup = .{ .nested = .{ .parent = "Num", .name = "Dec" } }, .num_kind = .dec },
    .{ .display_name = "F32", .qualified_name = "Builtin.Num.F32", .type_field = "f32_type", .ident_field = "f32_ident", .lookup = .{ .nested = .{ .parent = "Num", .name = "F32" } }, .num_kind = .f32 },
    .{ .display_name = "F64", .qualified_name = "Builtin.Num.F64", .type_field = "f64_type", .ident_field = "f64_ident", .lookup = .{ .nested = .{ .parent = "Num", .name = "F64" } }, .num_kind = .f64 },
    .{ .display_name = "Numeral", .qualified_name = "Builtin.Num.Numeral", .type_field = "numeral_type", .ident_field = "numeral_ident", .lookup = .{ .nested = .{ .parent = "Num", .name = "Numeral" } } },
    .{ .display_name = "Crypto", .qualified_name = "Builtin.Crypto", .type_field = "crypto_type", .ident_field = "crypto_ident", .lookup = .{ .top_level = "Crypto" } },
    .{ .display_name = "DigestBytesErr", .qualified_name = "Builtin.Crypto.DigestBytesErr", .type_field = "crypto_digest_bytes_err_type", .ident_field = "crypto_digest_bytes_err_ident", .lookup = .{ .qualified = "Builtin.Crypto.DigestBytesErr" }, .auto_import = false },
    .{ .display_name = "DigestHexErr", .qualified_name = "Builtin.Crypto.DigestHexErr", .type_field = "crypto_digest_hex_err_type", .ident_field = "crypto_digest_hex_err_ident", .lookup = .{ .qualified = "Builtin.Crypto.DigestHexErr" }, .auto_import = false },
    .{ .display_name = "Digest", .qualified_name = "Builtin.Crypto.SHA256.Digest", .type_field = "crypto_sha256_digest_type", .ident_field = "crypto_sha256_digest_ident", .lookup = .{ .qualified = "Builtin.Crypto.SHA256.Digest" }, .auto_import = false },
    .{ .display_name = "Hasher", .qualified_name = "Builtin.Crypto.SHA256.Hasher", .type_field = "crypto_sha256_hasher_type", .ident_field = "crypto_sha256_hasher_ident", .lookup = .{ .qualified = "Builtin.Crypto.SHA256.Hasher" }, .auto_import = false },
    .{ .display_name = "Digest", .qualified_name = "Builtin.Crypto.BLAKE3.Digest", .type_field = "crypto_blake3_digest_type", .ident_field = "crypto_blake3_digest_ident", .lookup = .{ .qualified = "Builtin.Crypto.BLAKE3.Digest" }, .auto_import = false },
    .{ .display_name = "Hasher", .qualified_name = "Builtin.Crypto.BLAKE3.Hasher", .type_field = "crypto_blake3_hasher_type", .ident_field = "crypto_blake3_hasher_ident", .lookup = .{ .qualified = "Builtin.Crypto.BLAKE3.Hasher" }, .auto_import = false },
};

/// Nominal declarations that only group nested builtin types rather than representing builtin types.
pub const builtin_type_container_names = [_][]const u8{
    "Builtin",
    "Builtin.Num",
    "Builtin.Encoding",
    "Builtin.Crypto",
    "Builtin.Crypto.SHA256",
    "Builtin.Crypto.BLAKE3",
};

const hash_offset: u64 = 0xcbf29ce484222325;
const hash_prime: u64 = 0x100000001b3;

fn hashByte(hash: u64, byte: u8) u64 {
    return (hash ^ byte) *% hash_prime;
}

fn hashBytes(hash: u64, bytes: []const u8) u64 {
    var result = hash;
    for (bytes) |byte| result = hashByte(result, byte);
    return result;
}

fn hashInt(hash: u64, value: u64) u64 {
    var result = hash;
    inline for (0..8) |shift| {
        result = hashByte(result, @intCast((value >> (shift * 8)) & 0xff));
    }
    return result;
}

/// Hash of the builtin type registry used to reject stale generated builtin index metadata.
pub const BUILTIN_TYPE_REGISTRY_HASH: u64 = blk: {
    @setEvalBranchQuota(10_000);
    var hash = hashBytes(hash_offset, "roc-builtin-type-registry-v1");
    for (builtin_type_specs) |spec| {
        hash = hashBytes(hash, spec.display_name);
        hash = hashBytes(hash, spec.qualified_name);
        hash = hashBytes(hash, spec.type_field);
        hash = hashBytes(hash, spec.ident_field);
        hash = hashBytes(hash, @tagName(spec.lookup));
        switch (spec.lookup) {
            .top_level => |name| hash = hashBytes(hash, name),
            .nested => |nested| {
                hash = hashBytes(hash, nested.parent);
                hash = hashBytes(hash, nested.name);
            },
            .qualified => |name| hash = hashBytes(hash, name),
        }
        hash = if (spec.num_kind) |num_kind| hashBytes(hash, @tagName(num_kind)) else hashBytes(hash, "-");
        hash = hashBytes(hash, if (spec.auto_import) "auto" else "internal");
    }
    break :blk hash;
};

/// Hash of the BuiltinIndices field layout used to reject stale generated index values.
pub const BUILTIN_INDICES_LAYOUT_HASH: u64 = blk: {
    @setEvalBranchQuota(10_000);
    var hash = hashBytes(hash_offset, "roc-builtin-indices-layout-v1");
    hash = hashInt(hash, @sizeOf(BuiltinIndices));
    hash = hashInt(hash, @alignOf(BuiltinIndices));
    for (@typeInfo(BuiltinIndices).@"struct".fields) |field| {
        hash = hashBytes(hash, field.name);
        hash = hashBytes(hash, @typeName(field.type));
    }
    break :blk hash;
};

// Type definitions for module compilation

/// Represents a definition (binding of a pattern to an expression) in the CIR
pub const Def = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = extern struct { span: base.DataSpan };

    pattern: Pattern.Idx,
    expr: Expr.Idx,
    annotation: ?Annotation.Idx,
    kind: Kind,

    pub const Kind = union(enum) {
        /// A def that introduces identifiers
        let: void,
        /// A standalone statement with an fx variable
        stmt: TypeVar,
        /// Ignored result, must be effectful
        ignored: TypeVar,

        pub fn decode(encoded: [2]u32) Kind {
            if (encoded[0] == 0) {
                return .let;
            } else if (encoded[0] == 1) {
                return .{ .stmt = @as(TypeVar, @enumFromInt(encoded[1])) };
            } else {
                return .{ .ignored = @as(TypeVar, @enumFromInt(encoded[1])) };
            }
        }

        pub fn encode(self: Kind) [2]u32 {
            switch (self) {
                .let => return .{ 0, 0 },
                .stmt => |ty_var| return .{ 1, @intFromEnum(ty_var) },
                .ignored => |ty_var| return .{ 2, @intFromEnum(ty_var) },
            }
        }
    };

    pub fn pushToSExprTree(self: *const Def, cir: anytype, tree: anytype) Allocator.Error!void {
        const begin = tree.beginNode();
        const name: []const u8 = switch (self.kind) {
            .let => "d-let",
            .stmt => "d-stmt",
            .ignored => "d-ignored",
        };
        try tree.pushStaticAtom(name);

        const attrs = tree.beginNode();

        // Safety check: verify pattern index points to actual pattern node
        // This prevents crashes from cross-module node index issues
        const pattern_node_idx: @TypeOf(cir.store.nodes).Idx = @enumFromInt(@intFromEnum(self.pattern));
        const pattern_node = cir.store.nodes.get(pattern_node_idx);
        const is_valid_pattern = switch (pattern_node.tag) {
            .pattern_identifier,
            .pattern_as,
            .pattern_applied_tag,
            .pattern_nominal,
            .pattern_nominal_external,
            .pattern_record_destructure,
            .pattern_list,
            .pattern_tuple,
            .pattern_num_literal,
            .pattern_dec_literal,
            .pattern_f32_literal,
            .pattern_f64_literal,
            .pattern_small_dec_literal,
            .pattern_str_literal,
            .pattern_str_interpolation,
            .pattern_underscore,
            => true,
            else => false,
        };

        if (is_valid_pattern) {
            try cir.store.getPattern(self.pattern).pushToSExprTree(cir, tree, self.pattern);
        } else {
            // Pattern index is invalid - output placeholder to avoid crash
            try tree.pushStaticAtom("invalid-pattern");
        }

        try cir.store.getExpr(self.expr).pushToSExprTree(cir, tree, self.expr);

        if (self.annotation) |annotation_idx| {
            try cir.store.getAnnotation(annotation_idx).pushToSExprTree(cir, tree, annotation_idx);
        }

        try tree.endNode(begin, attrs);
    }
};

/// Represents a type header (e.g., 'Maybe a' or 'Try err ok') in type annotations
pub const TypeHeader = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = extern struct { start: u32, len: u32 };

    /// The fully qualified name for lookups (e.g., "Builtin.Bool" or "MyModule.Foo.Bar")
    name: base.Ident.Idx,
    /// The name relative to the module, without the module prefix (e.g., "Bool" or "Foo.Bar").
    /// This is what should be used for NominalType.ident to avoid redundancy with origin_module.
    relative_name: base.Ident.Idx,
    args: TypeAnno.Span,

    pub fn pushToSExprTree(self: *const TypeHeader, cir: anytype, tree: anytype, idx: TypeHeader.Idx) Allocator.Error!void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("ty-header");

        // Get the region for this TypeHeader
        const node_idx: Node.Idx = @enumFromInt(@intFromEnum(idx));
        const region = cir.store.getRegionAt(node_idx);
        try cir.appendRegionInfoToSExprTreeFromRegion(tree, region);

        const name_str = cir.getIdent(self.name);
        try tree.pushStringPair("name", name_str);

        const attrs = tree.beginNode();

        if (self.args.span.len > 0) {
            const args_begin = tree.beginNode();
            try tree.pushStaticAtom("ty-args");
            const args_attrs = tree.beginNode();
            for (cir.store.sliceTypeAnnos(self.args)) |anno_idx| {
                try cir.store.getTypeAnno(anno_idx).pushToSExprTree(cir, tree, anno_idx);
            }
            try tree.endNode(args_begin, args_attrs);
        }

        try tree.endNode(begin, attrs);
    }
};

/// Represents a where clause constraint in type definitions
pub const WhereClause = union(enum) {
    pub const Idx = enum(u32) { _ };
    pub const Span = extern struct { span: base.DataSpan };

    w_method: struct {
        var_: TypeAnno.Idx,
        method_name: base.Ident.Idx,
        args: TypeAnno.Span,
        ret: TypeAnno.Idx,
    },
    w_alias: struct {
        var_: TypeAnno.Idx,
        alias_name: base.Ident.Idx,
    },
    w_malformed: struct {
        diagnostic: Diagnostic.Idx,
    },

    pub fn pushToSExprTree(self: *const WhereClause, cir: anytype, tree: anytype, idx: WhereClause.Idx) Allocator.Error!void {
        switch (self.*) {
            .w_method => |method| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("method");

                // Get the region for this WhereClause
                const node_idx: Node.Idx = @enumFromInt(@intFromEnum(idx));
                const region = cir.store.getRegionAt(node_idx);
                try cir.appendRegionInfoToSExprTreeFromRegion(tree, region);

                // Add module-of and ident information
                try cir.store.getTypeAnno(method.var_).pushToSExprTree(cir, tree, method.var_);

                const method_name_str = cir.getIdent(method.method_name);
                try tree.pushStringPair("name", method_name_str);

                const attrs = tree.beginNode();

                // Add actual argument types
                const args_begin = tree.beginNode();
                try tree.pushStaticAtom("args");
                const args_attrs = tree.beginNode();
                for (cir.store.sliceTypeAnnos(method.args)) |arg_idx| {
                    try cir.store.getTypeAnno(arg_idx).pushToSExprTree(cir, tree, arg_idx);
                }
                try tree.endNode(args_begin, args_attrs);

                // Add actual return type
                try cir.store.getTypeAnno(method.ret).pushToSExprTree(cir, tree, method.ret);
                try tree.endNode(begin, attrs);
            },
            .w_alias => |alias| {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("alias");

                // Get the region for this WhereClause
                const node_idx: Node.Idx = @enumFromInt(@intFromEnum(idx));
                const region = cir.store.getRegionAt(node_idx);
                try cir.appendRegionInfoToSExprTreeFromRegion(tree, region);

                try cir.store.getTypeAnno(alias.var_).pushToSExprTree(cir, tree, alias.var_);

                const alias_name_str = cir.getIdent(alias.alias_name);
                try tree.pushStringPair("name", alias_name_str);

                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
            .w_malformed => {
                const begin = tree.beginNode();
                try tree.pushStaticAtom("malformed");

                // Get the region for this WhereClause
                const node_idx: Node.Idx = @enumFromInt(@intFromEnum(idx));
                const region = cir.store.getRegionAt(node_idx);
                try cir.appendRegionInfoToSExprTreeFromRegion(tree, region);

                const attrs = tree.beginNode();
                try tree.endNode(begin, attrs);
            },
        }
    }
};

/// Represents a type annotation associated with definitions
pub const Annotation = struct {
    pub const Idx = enum(u32) { _ };

    anno: TypeAnno.Idx,
    where: ?WhereClause.Span,
    /// Whether `anno` mentions any type variable (a fresh `.rigid_var` or a
    /// `.rigid_var_lookup` to an enclosing one). Derived from `anno` by
    /// `addAnnotation` and populated on read by `getAnnotation`; the value passed
    /// at construction is ignored.
    mentions_type_var: bool = false,
    /// Whether `anno` *introduces* a type variable (`.rigid_var`). Derived and
    /// populated like `mentions_type_var`.
    introduces_type_var: bool = false,

    pub fn pushToSExprTree(self: *const @This(), env: anytype, tree: *SExprTree, idx: Annotation.Idx) Allocator.Error!void {
        const annotation = self.*;

        const begin = tree.beginNode();
        try tree.pushStaticAtom("annotation");
        const attrs = tree.beginNode();

        // Get the region for this Annotation
        const region = env.store.getAnnotationRegion(idx);
        try env.appendRegionInfoToSExprTreeFromRegion(tree, region);

        // Append annotation
        try env.store.getTypeAnno(annotation.anno).pushToSExprTree(env, tree, self.anno);

        // Append where clause
        if (annotation.where) |where_span| {
            const where_begin = tree.beginNode();
            try tree.pushStaticAtom("where");
            const where_attrs = tree.beginNode();
            const where_clauses = env.store.sliceWhereClauses(where_span);
            for (where_clauses) |clause_idx| {
                const clause = env.store.getWhereClause(clause_idx);
                try clause.pushToSExprTree(env, tree, clause_idx);
            }
            try tree.endNode(where_begin, where_attrs);
        }

        try tree.endNode(begin, attrs);
    }
};

/// Represents an item exposed by a module's interface
pub const ExposedItem = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = extern struct { span: base.DataSpan };

    name: base.Ident.Idx,
    alias: ?base.Ident.Idx,
    is_wildcard: bool,

    pub fn pushToSExprTree(self: *const ExposedItem, _: anytype, cir: anytype, tree: anytype) Allocator.Error!void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("exposed");

        const name_str = cir.getIdent(self.name);
        try tree.pushStringPair("name", name_str);

        if (self.alias) |alias_idx| {
            const alias_str = cir.getIdent(alias_idx);
            try tree.pushStringPair("alias", alias_str);
        }

        try tree.pushBoolPair("wildcard", self.is_wildcard);

        const attrs = tree.beginNode();
        try tree.endNode(begin, attrs);
    }
};

/// Represents an arbitrary precision smallish decimal value
pub const SmallDecValue = struct {
    numerator: i16,
    denominator_power_of_ten: u8,

    /// Convert a small dec to f64 (use for size comparisons)
    pub fn toF64(self: @This()) f64 {
        const numerator_f64 = @as(f64, @floatFromInt(self.numerator));
        const divisor = std.math.pow(f64, 10, @as(f64, @floatFromInt(self.denominator_power_of_ten)));
        return numerator_f64 / divisor;
    }

    /// Calculate the int requirements of a SmallDecValue
    pub fn toFracRequirements(self: SmallDecValue) types_mod.FracRequirements {
        const f64_val = self.toF64();
        return types_mod.FracRequirements{
            .fits_in_f32 = fitsInF32(f64_val),
            .fits_in_dec = fitsInDec(f64_val),
        };
    }

    /// Convert to RocDec representation (i128 scaled by 10^18)
    pub fn toRocDec(self: SmallDecValue) RocDec {
        return RocDec.fromFraction(self.numerator, self.denominator_power_of_ten);
    }

    test "SmallDecValue.toF64 - basic cases" {
        // Test integer values
        {
            const val = SmallDecValue{ .numerator = 42, .denominator_power_of_ten = 0 };
            try std.testing.expectEqual(@as(f64, 42.0), val.toF64());
        }

        // Test decimal values
        {
            const val = SmallDecValue{ .numerator = 314, .denominator_power_of_ten = 2 };
            try std.testing.expectApproxEqAbs(@as(f64, 3.14), val.toF64(), 0.0001);
        }

        // Test negative values
        {
            const val = SmallDecValue{ .numerator = -500, .denominator_power_of_ten = 3 };
            try std.testing.expectApproxEqAbs(@as(f64, -0.5), val.toF64(), 0.0001);
        }

        // Test very small values
        {
            const val = SmallDecValue{ .numerator = 1, .denominator_power_of_ten = 10 };
            try std.testing.expectApproxEqAbs(@as(f64, 1e-10), val.toF64(), 1e-15);
        }

        // Test maximum numerator
        {
            const val = SmallDecValue{ .numerator = 32767, .denominator_power_of_ten = 0 };
            try std.testing.expectEqual(@as(f64, 32767.0), val.toF64());
        }

        // Test minimum numerator
        {
            const val = SmallDecValue{ .numerator = -32768, .denominator_power_of_ten = 0 };
            try std.testing.expectEqual(@as(f64, -32768.0), val.toF64());
        }
    }

    test "SmallDecValue.toFracRequirements - fits in all types" {
        // Small integer - fits in everything
        {
            const val = SmallDecValue{ .numerator = 100, .denominator_power_of_ten = 0 };
            const req = val.toFracRequirements();
            try std.testing.expect(req.fits_in_f32);
            try std.testing.expect(req.fits_in_dec);
        }

        // Pi approximation - fits in everything
        {
            const val = SmallDecValue{ .numerator = 31416, .denominator_power_of_ten = 4 };
            const req = val.toFracRequirements();
            try std.testing.expect(req.fits_in_f32);
            try std.testing.expect(req.fits_in_dec);
        }
    }
};

/// Check if the given f64 fits in f32 range (ignoring precision loss)
pub fn fitsInF32(f64_val: f64) bool {
    // Check if it's within the range that f32 can represent.
    // This includes normal, subnormal, and zero values.
    // (This is a magnitude check, so take the abs value to check
    // positive and negative at the same time.)
    const abs_val = @abs(f64_val);
    return abs_val == 0.0 or (abs_val >= std.math.floatTrueMin(f32) and abs_val <= std.math.floatMax(f32));
}

/// Check if a float value can be represented accurately in RocDec
pub fn fitsInDec(value: f64) bool {
    // RocDec uses i128 with 18 decimal places
    const max_dec_value = 170141183460469231731.0;
    const min_dec_value = -170141183460469231731.0;

    return value >= min_dec_value and value <= max_dec_value;
}

/// Represents an arbitrary precision integer value
pub const IntValue = struct {
    bytes: [16]u8,
    kind: IntKind,

    pub const IntKind = enum(u8) {
        i128,
        u128,
    };

    pub fn toI128(self: IntValue) i128 {
        return @bitCast(self.bytes);
    }

    pub fn bufPrint(self: IntValue, buf: []u8) Allocator.Error![]u8 {
        const i128h = builtins.compiler_rt_128;
        switch (self.kind) {
            .i128 => {
                const val: i128 = @bitCast(self.bytes);
                const result = i128h.i128_to_str(buf, val);
                return buf[result.start..buf.len];
            },
            .u128 => {
                const val: u128 = @bitCast(self.bytes);
                const result = i128h.u128_to_str(buf, val);
                return buf[result.start..buf.len];
            },
        }
    }

    pub fn pushStringPair(self: IntValue, tree: *SExprTree, key: []const u8) std.mem.Allocator.Error!void {
        const begin = try tree.reserveStringBuffer(40);
        errdefer tree.discardReservedStringBuffer(begin);
        const value_str = self.bufPrint(tree.reservedStringBuffer(begin)[0..40]) catch unreachable;
        try tree.pushReservedStringPair(key, begin, value_str);
    }

    /// Calculate the int requirements of an IntValue
    pub fn toIntRequirements(self: IntValue) types_mod.IntRequirements {
        var is_negated = false;
        var u128_val: u128 = undefined;

        switch (self.kind) {
            .i128 => {
                const val: i128 = @bitCast(self.bytes);
                is_negated = val < 0;
                u128_val = if (val < 0) @abs(val) else @intCast(val);
            },
            .u128 => {
                const val: u128 = @bitCast(self.bytes);
                is_negated = false;
                u128_val = val;
            },
        }

        // Special handling for minimum signed values
        // These are the exact minimum values for each signed integer type.
        // They need special handling because their absolute value is one more
        // than the maximum positive value of the same signed type.
        // For example: i8 range is -128 to 127, so abs(-128) = 128 doesn't fit in i8's positive range
        const is_minimum_signed = is_negated and switch (u128_val) {
            @as(u128, @intCast(std.math.maxInt(i8))) + 1 => true,
            @as(u128, @intCast(std.math.maxInt(i16))) + 1 => true,
            @as(u128, @intCast(std.math.maxInt(i32))) + 1 => true,
            @as(u128, @intCast(std.math.maxInt(i64))) + 1 => true,
            @as(u128, @intCast(std.math.maxInt(i128))) + 1 => true,
            else => false,
        };

        // For minimum signed values, subtract 1 from the magnitude
        // This makes the bit calculation work correctly with the "n-1 bits for magnitude" rule
        const adjusted_val = if (is_minimum_signed) u128_val - 1 else u128_val;
        const bits_needed = types_mod.Int.BitsNeeded.fromValue(adjusted_val);
        return types_mod.IntRequirements{
            .sign_needed = is_negated and u128_val != 0, // -0 doesn't need a sign
            .bits_needed = bits_needed.toBits(),
            .is_minimum_signed = is_minimum_signed,
        };
    }

    /// Calculate the frac requirements of an IntValue
    pub fn toFracRequirements(self: IntValue) types_mod.FracRequirements {
        // Convert to f64 for checking
        const f64_val: f64 = switch (self.kind) {
            .i128 => builtins.compiler_rt_128.i128_to_f64(@as(i128, @bitCast(self.bytes))),
            .u128 => blk: {
                const val = @as(u128, @bitCast(self.bytes));
                if (val > @as(u128, 1) << 64) {
                    break :blk std.math.inf(f64);
                }
                break :blk builtins.compiler_rt_128.u128_to_f64(val);
            },
        };

        // For integers, check both range AND exact representability in f32
        const fits_in_f32 = blk: {
            // Check range
            if (!fitsInF32(f64_val)) {
                break :blk false;
            }

            // Additionally check exact representability for integers
            // F32 can exactly represent integers only up to 2^24
            const f32_max_exact_int = 16777216.0; // 2^24
            break :blk @abs(f64_val) <= f32_max_exact_int;
        };

        const fits_in_dec = fitsInDec(f64_val);

        return types_mod.FracRequirements{
            .fits_in_f32 = fits_in_f32,
            .fits_in_dec = fits_in_dec,
        };
    }
};

/// Canonical information about a number
pub const NumKind = enum {
    // If this number has no restrictions
    num_unbound,

    // If this number is an int with no restrictions
    int_unbound,

    // If the number has an explicit suffix
    u8,
    i8,
    u16,
    i16,
    u32,
    i32,
    u64,
    i64,
    u128,
    i128,
    f32,
    f64,
    dec,
};

/// Base-256 digit storage for Numeral values.
/// Used to construct Roc Numeral values during compile-time evaluation.
///
/// Numeral in Roc stores:
/// - is_negative: Bool (whether there was a minus sign)
/// - digits_before_pt: List(U8) (base-256 digits before decimal point)
/// - digits_after_pt: List(U8) (base-256 digits after decimal point)
/// - digits_after_pt_count: U64 (how many decimal digits appeared after the point)
///
/// Example: "356.5170" becomes:
/// - is_negative = false
/// - digits_before_pt = [1, 100] (because 356 = 1*256 + 100)
/// - digits_after_pt = [20, 50] (because 5170 = 20*256 + 50)
/// - digits_after_pt_count = 4
pub const NumeralDigits = struct {
    /// Index into the shared digit byte array in ModuleEnv
    digits_start: u32,
    /// Number of bytes for digits_before_pt
    before_pt_len: u16,
    /// Number of bytes for digits_after_pt
    after_pt_len: u16,
    /// Number of decimal digits after the point before base-256 encoding
    after_pt_digit_count: u32,
    /// Whether the literal had a minus sign
    is_negative: bool,

    /// Get the total length of stored digits
    pub fn totalLen(self: NumeralDigits) u32 {
        return @as(u32, self.before_pt_len) + @as(u32, self.after_pt_len);
    }

    /// Extract digits_before_pt from the shared byte array
    pub fn getDigitsBeforePt(self: NumeralDigits, digit_bytes: []const u8) []const u8 {
        return digit_bytes[self.digits_start..][0..self.before_pt_len];
    }

    /// Extract digits_after_pt from the shared byte array
    pub fn getDigitsAfterPt(self: NumeralDigits, digit_bytes: []const u8) []const u8 {
        const after_start = self.digits_start + self.before_pt_len;
        return digit_bytes[after_start..][0..self.after_pt_len];
    }
};

// RocDec type definition (for missing export)
// Must match the structure of builtins.RocDec
pub const RocDec = builtins.dec.RocDec;

/// Converts a RocDec to an i128 integer
pub fn toI128(self: RocDec) i128 {
    return self.num;
}

/// Creates a RocDec from an f64 value, returns null if conversion fails
pub fn fromF64(f: f64) ?RocDec {
    // Simple conversion - the real implementation is in builtins/dec.zig
    const scaled = builtins.compiler_rt_128.f64_to_i128(f * 1_000_000_000_000_000_000.0);
    return RocDec{ .num = scaled };
}

/// Represents an import statement in a module
pub const Import = struct {
    pub const compiler_builtin_import_name = "\x00compiler.Builtin";

    pub fn isCompilerBuiltinImportName(module_name: []const u8) bool {
        return std.mem.eql(u8, module_name, compiler_builtin_import_name);
    }

    pub const Idx = enum(u32) {
        first = 0,
        _,
    };

    pub const ResolvedModuleIdx = enum(u32) {
        failed_before_checking = std.math.maxInt(u32) - 1,
        none = std.math.maxInt(u32),
        _,

        pub fn isNone(self: ResolvedModuleIdx) bool {
            return self == .none;
        }

        pub fn isFailedBeforeChecking(self: ResolvedModuleIdx) bool {
            return self == .failed_before_checking;
        }

        pub fn isResolved(self: ResolvedModuleIdx) bool {
            return !self.isNone() and !self.isFailedBeforeChecking();
        }
    };

    pub const Store = struct {
        /// Map from interned string idx to Import.Idx for deduplication
        map: std.AutoHashMapUnmanaged(base.StringLiteral.Idx, Import.Idx) = .{},
        /// List of interned string IDs indexed by Import.Idx
        imports: collections.SafeList(base.StringLiteral.Idx) = .{},
        /// List of interned ident IDs indexed by Import.Idx (parallel to imports)
        /// Used for efficient index-based lookups instead of string comparison
        import_idents: collections.SafeList(base.Ident.Idx) = .{},
        /// Resolved module indices, parallel to imports list
        /// Each entry is either a valid module index or unresolved
        resolved_modules: collections.SafeList(ResolvedModuleIdx) = .{},

        pub fn init() Store {
            return .{};
        }

        pub fn deinit(self: *Store, allocator: std.mem.Allocator) void {
            self.map.deinit(allocator);
            self.imports.deinit(allocator);
            self.import_idents.deinit(allocator);
            self.resolved_modules.deinit(allocator);
        }

        pub fn clone(self: *const Store, allocator: std.mem.Allocator) std.mem.Allocator.Error!Store {
            var result = Store{
                .map = .{},
                .imports = try self.imports.clone(allocator),
                .import_idents = try self.import_idents.clone(allocator),
                .resolved_modules = try self.resolved_modules.clone(allocator),
            };
            errdefer result.deinit(allocator);

            for (result.imports.items.items, 0..) |string_idx, i| {
                const import_idx = @as(Import.Idx, @enumFromInt(i));
                try result.map.put(allocator, string_idx, import_idx);
            }

            return result;
        }

        /// Deinit only the hash map, not the SafeLists.
        /// Used for cached modules where the SafeLists point into the cache buffer
        /// but the map was heap-allocated during deserialization.
        pub fn deinitMapOnly(self: *Store, allocator: std.mem.Allocator) void {
            self.map.deinit(allocator);
        }

        /// Get or create an Import.Idx for the given module name.
        /// The module name is first checked against existing imports by comparing strings.
        /// New imports are initially unresolved (unresolved).
        /// If ident_idx is provided, it will be stored for index-based lookups.
        pub fn getOrPut(self: *Store, allocator: std.mem.Allocator, common: *base.CommonEnv, module_name: []const u8) Allocator.Error!Import.Idx {
            return self.getOrPutWithIdent(allocator, common, module_name, null);
        }

        /// Get or create an Import.Idx for the given module name, with an associated ident.
        /// The module name is first checked against existing imports by comparing strings.
        /// New imports are initially unresolved (unresolved).
        /// If ident_idx is provided, it will be stored for index-based lookups.
        pub fn getOrPutWithIdent(self: *Store, allocator: std.mem.Allocator, common: *base.CommonEnv, module_name: []const u8, ident_idx: ?base.Ident.Idx) Allocator.Error!Import.Idx {
            // First check if we already have this module name by comparing strings
            for (self.imports.items.items, 0..) |existing_string_idx, i| {
                const existing_name = common.getString(existing_string_idx);
                if (std.mem.eql(u8, existing_name, module_name)) {
                    // Found existing import with same name
                    // Update ident if provided and not already set
                    if (ident_idx) |ident| {
                        if (i < self.import_idents.len()) {
                            const current = self.import_idents.items.items[i];
                            if (current.isNone()) {
                                self.import_idents.items.items[i] = ident;
                            }
                        }
                    }
                    return @as(Import.Idx, @enumFromInt(i));
                }
            }

            // Not found - create new import
            const string_idx = try common.insertString(allocator, module_name);
            const idx = @as(Import.Idx, @enumFromInt(self.imports.len()));

            // Add to both the list and the map, with unresolved module initially
            const imports_idx = try self.imports.append(allocator, string_idx);
            std.debug.assert(@intFromEnum(imports_idx) == @intFromEnum(idx));
            const ident_idx_added = try self.import_idents.append(allocator, ident_idx orelse base.Ident.Idx.NONE);
            std.debug.assert(@intFromEnum(ident_idx_added) == @intFromEnum(idx));
            const resolved_idx = try self.resolved_modules.append(allocator, ResolvedModuleIdx.none);
            std.debug.assert(@intFromEnum(resolved_idx) == @intFromEnum(idx));
            try self.map.put(allocator, string_idx, idx);

            return idx;
        }

        /// Get the ident index for an import, or null if not set
        pub fn getIdentIdx(self: *const Store, import_idx: Import.Idx) ?base.Ident.Idx {
            const idx = @intFromEnum(import_idx);
            if (idx >= self.import_idents.len()) return null;
            const ident = self.import_idents.items.items[idx];
            if (ident.isNone()) return null;
            return ident;
        }

        /// Get the resolved module index for an import, or null if unresolved
        pub fn getResolvedModule(self: *const Store, import_idx: Import.Idx) ?u32 {
            const idx = @intFromEnum(import_idx);
            if (idx >= self.resolved_modules.len()) return null;
            const resolved = self.resolved_modules.items.items[idx];
            if (!resolved.isResolved()) return null;
            return @intFromEnum(resolved);
        }

        /// Return true when import resolution has already reported a user-facing
        /// diagnostic before type checking. Type checking may continue for source
        /// tooling, but post-check lowering must never consume this import.
        pub fn importFailedBeforeChecking(self: *const Store, import_idx: Import.Idx) bool {
            const idx = @intFromEnum(import_idx);
            if (idx >= self.resolved_modules.len()) return false;
            return self.resolved_modules.items.items[idx].isFailedBeforeChecking();
        }

        /// Set the resolved module index for an import
        pub fn setResolvedModule(self: *Store, import_idx: Import.Idx, module_idx: u32) void {
            const idx = @intFromEnum(import_idx);
            if (idx < self.resolved_modules.len()) {
                self.resolved_modules.items.items[idx] = @enumFromInt(module_idx);
            }
        }

        /// Mark one import as intentionally unavailable because an earlier stage
        /// already owns the user-facing diagnostic.
        pub fn setImportFailedBeforeChecking(self: *Store, import_idx: Import.Idx) void {
            const idx = @intFromEnum(import_idx);
            if (idx < self.resolved_modules.len()) {
                self.resolved_modules.items.items[idx] = .failed_before_checking;
            }
        }

        /// Diagnostics-only tooling can continue type inspection after unresolved
        /// imports have been reported. This makes that state explicit instead of
        /// leaving imports in the pre-resolution state.
        pub fn markUnresolvedImportsFailedBeforeChecking(self: *Store) void {
            for (self.resolved_modules.items.items) |*resolved| {
                if (resolved.isNone()) resolved.* = .failed_before_checking;
            }
        }

        /// Clear all resolved module indices.
        pub fn clearResolvedModules(self: *Store) void {
            for (self.resolved_modules.items.items) |*resolved| {
                resolved.* = .none;
            }
        }

        /// Resolve any still-unresolved imports by exact module-name match against
        /// the provided array.
        /// Existing `resolved_modules` entries are preserved.
        ///
        /// Parameters:
        /// - env: The module environment containing the string store for import names
        /// - available_modules: Array of module environments to match against
        ///
        /// For each unresolved import, this finds the module in available_modules whose
        /// module_name exactly matches the import name and sets the resolved index
        /// accordingly.
        pub fn resolveImportsByExactModuleName(
            self: *Store,
            env: anytype,
            available_modules: []const *const @import("ModuleEnv.zig"),
        ) std.mem.Allocator.Error!void {
            const import_count: usize = @intCast(self.imports.len());
            if (import_count == 0) return;

            // Index modules by name once. First occurrence wins, matching the
            // "first match in iteration order" semantics of the previous linear scan.
            var name_to_idx = std.StringHashMap(u32).init(env.gpa);
            defer name_to_idx.deinit();
            try name_to_idx.ensureTotalCapacity(@intCast(available_modules.len));
            var compiler_builtin_module_idx: ?u32 = null;
            for (available_modules, 0..) |module_env, module_idx| {
                if (module_env.module_role == .builtin and compiler_builtin_module_idx == null) {
                    compiler_builtin_module_idx = @intCast(module_idx);
                    continue;
                }
                const gop = name_to_idx.getOrPutAssumeCapacity(module_env.module_name);
                if (!gop.found_existing) gop.value_ptr.* = @intCast(module_idx);
            }

            for (0..import_count) |i| {
                const import_idx: Import.Idx = @enumFromInt(i);
                const current = self.resolved_modules.items.items[i];
                if (!current.isNone()) continue;
                const str_idx = self.imports.items.items[i];
                const import_name = env.common.getString(str_idx);

                if (Import.isCompilerBuiltinImportName(import_name)) {
                    if (compiler_builtin_module_idx) |module_idx| {
                        self.setResolvedModule(import_idx, module_idx);
                    }
                    continue;
                }

                if (name_to_idx.get(import_name)) |module_idx| {
                    self.setResolvedModule(import_idx, module_idx);
                }
            }
        }

        /// Serialize this Store to the given CompactWriter. The resulting Store
        /// in the writer's buffer will have offsets instead of pointers. Calling any
        /// methods on it or dereferencing its internal "pointers" (which are now
        /// offsets) is illegal behavior!
        pub fn serialize(
            self: *const Store,
            allocator: std.mem.Allocator,
            writer: *CompactWriter,
        ) std.mem.Allocator.Error!*const Store {
            // First, write the Store struct itself
            const offset_self = try writer.appendAlloc(allocator, Store);

            // Then serialize the sub-structures and update the struct
            offset_self.* = .{
                .map = .{}, // Map will be empty after deserialization (only used for deduplication during insertion)
                .imports = (try self.imports.serialize(allocator, writer)).*,
                .import_idents = (try self.import_idents.serialize(allocator, writer)).*,
                .resolved_modules = (try self.resolved_modules.serialize(allocator, writer)).*,
            };

            return @constCast(offset_self);
        }

        /// Add the given offset to the memory addresses of all pointers in `self`.
        pub fn relocate(self: *Store, offset: isize) void {
            self.imports.relocate(offset);
            self.import_idents.relocate(offset);
            self.resolved_modules.relocate(offset);
        }

        /// Uses extern struct to guarantee consistent field layout across optimization levels.
        pub const Serialized = extern struct {
            // Placeholder to match Store size - not serialized
            // Reserve space for hashmap (3 pointers for unmanaged hashmap internals)
            map: [3]u64,
            imports: collections.SafeList(base.StringLiteral.Idx).Serialized,
            import_idents: collections.SafeList(base.Ident.Idx).Serialized,
            resolved_modules: collections.SafeList(Import.ResolvedModuleIdx).Serialized,

            /// Serialize a Store into this Serialized struct, appending data to the writer
            pub fn serialize(
                self: *Serialized,
                store: *const Store,
                allocator: std.mem.Allocator,
                writer: *CompactWriter,
            ) std.mem.Allocator.Error!void {
                // Serialize the imports SafeList
                try self.imports.serialize(&store.imports, allocator, writer);
                // Serialize the import_idents SafeList
                try self.import_idents.serialize(&store.import_idents, allocator, writer);
                // Serialize the resolved_modules SafeList
                try self.resolved_modules.serialize(&store.resolved_modules, allocator, writer);

                // Set map to all zeros; the space needs to be here,
                // but the map will be rebuilt during deserialization.
                self.map = .{ 0, 0, 0 };
                // Note: The map is not serialized as it's only used for deduplication during insertion
            }

            /// Deserialize into a Store value (no in-place modification of cache buffer).
            /// The base parameter is the base address of the serialized buffer in memory.
            /// Note: The map is freshly allocated and populated from the deserialized imports.
            pub fn deserializeInto(self: *const Serialized, base_addr: usize, allocator: std.mem.Allocator) std.mem.Allocator.Error!Store {
                var store = Store{
                    .map = .{}, // Will be repopulated below
                    .imports = self.imports.deserializeInto(base_addr),
                    .import_idents = self.import_idents.deserializeInto(base_addr),
                    .resolved_modules = self.resolved_modules.deserializeInto(base_addr),
                };

                // Pre-allocate the exact capacity needed for the map
                const import_count = store.imports.items.items.len;
                try store.map.ensureTotalCapacity(allocator, @intCast(import_count));

                // Repopulate the map - we know there's enough capacity since we
                // are deserializing from a Serialized struct
                for (store.imports.items.items, 0..) |string_idx, i| {
                    const import_idx = @as(Import.Idx, @enumFromInt(i));
                    store.map.putAssumeCapacityNoClobber(string_idx, import_idx);
                }

                return store;
            }
        };
    };
};

/// Represents a field in a record expression
pub const RecordField = struct {
    pub const Idx = enum(u32) { _ };
    pub const Span = extern struct { span: base.DataSpan };

    name: base.Ident.Idx,
    value: Expr.Idx,

    pub fn pushToSExprTree(self: *const RecordField, cir: anytype, tree: anytype) Allocator.Error!void {
        const begin = tree.beginNode();
        try tree.pushStaticAtom("field");
        try tree.pushStringPair("name", cir.getIdent(self.name));
        const attrs = tree.beginNode();
        try cir.store.getExpr(self.value).pushToSExprTree(cir, tree, self.value);
        try tree.endNode(begin, attrs);
    }
};

/// Represents an external declaration from another module
pub const ExternalDecl = struct {
    /// Fully qualified name (e.g., "json.Json.utf8")
    qualified_name: base.Ident.Idx,
    /// Module this decl comes from (e.g., "json.Json")
    module_name: base.Ident.Idx,
    /// Local name within that module (e.g., "utf8")
    local_name: base.Ident.Idx,
    /// Type variable for this declaration
    type_var: TypeVar,
    /// Kind of external declaration
    kind: enum { value, type },
    /// Region where this was referenced
    region: Region,

    pub const Idx = enum(u32) { _ };
    pub const Span = extern struct { span: base.DataSpan };
    /// A safe list of external declarations
    pub const SafeList = collections.SafeList(ExternalDecl);

    pub fn pushToSExprTree(self: *const ExternalDecl, cir: anytype, tree: anytype) Allocator.Error!void {
        const node = tree.beginNode();
        try tree.pushStaticAtom("ext-decl");
        try cir.appendRegionInfoToSExprTreeFromRegion(tree, self.region);

        // Add fully qualified name
        try tree.pushStringPair("ident", cir.getIdent(self.qualified_name));

        // Add kind
        switch (self.kind) {
            .value => try tree.pushStringPair("kind", "value"),
            .type => try tree.pushStringPair("kind", "type"),
        }

        const attrs = tree.beginNode();
        try tree.endNode(node, attrs);
    }

    pub fn pushToSExprTreeWithRegion(self: *const ExternalDecl, cir: anytype, tree: anytype, region: Region) Allocator.Error!void {
        const node = tree.beginNode();
        try tree.pushStaticAtom("ext-decl");
        try cir.appendRegionInfoToSExprTreeFromRegion(tree, region);

        // Add fully qualified name
        try tree.pushStringPair("ident", cir.getIdent(self.qualified_name));

        // Add kind
        switch (self.kind) {
            .value => try tree.pushStringPair("kind", "value"),
            .type => try tree.pushStringPair("kind", "type"),
        }

        const attrs = tree.beginNode();
        try tree.endNode(node, attrs);
    }
};

// Real Report type from the reporting module
pub const Report = reporting.Report;

/// Checks if a type is castable for index type conversions
pub fn isCastable(comptime T: type) bool {
    return switch (T) {
        Expr.Idx,
        Pattern.Idx,
        Statement.Idx,
        TypeAnno.Idx,
        Def.Idx,
        TypeHeader.Idx,
        RecordField.Idx,
        Pattern.RecordDestruct.Idx,
        Expr.IfBranch.Idx,
        Expr.Match.Branch.Idx,
        WhereClause.Idx,
        Annotation.Idx,
        TypeAnno.RecordField.Idx,
        ExposedItem.Idx,
        Expr.Match.BranchPattern.Idx,
        Node.Idx,
        TypeVar,
        => true,
        else => false,
    };
}

/// Safely casts between compatible index types
pub fn castIdx(comptime From: type, comptime To: type, idx: From) To {
    return @as(To, @enumFromInt(@intFromEnum(idx)));
}
