//! The common state for a module: any data useful over the full lifetime of its compilation that lives beyond individual IR's.
//!
//! Stores all interned data like idents, strings, and problems.
//!
//! This reduces the size of this module's IRs as they can store references to this
//! interned (and deduplicated) data instead of storing the values themselves.

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const types_mod = @import("types");
const collections = @import("collections");
const base = @import("base");

const Node = @import("Node.zig");
const NodeStore = @import("NodeStore.zig");
const CIR = @import("CIR.zig");
const DependencyGraph = @import("DependencyGraph.zig");

const TypeWriter = types_mod.TypeWriter;
const CompactWriter = collections.CompactWriter;
const SortedArrayBuilder = collections.SortedArrayBuilder;
const CommonEnv = base.CommonEnv;
const Ident = base.Ident;
const StringLiteral = base.StringLiteral;
const RegionInfo = base.RegionInfo;
const Region = base.Region;
const SExprTree = base.SExprTree;
const TypeVar = types_mod.Var;
const TypeStore = types_mod.Store;

const Self = @This();

/// The kind of module being canonicalized, set during header processing
pub const ModuleKind = union(enum) {
    type_module: Ident.Idx, // Holds the main type identifier for type modules
    default_app,
    app,
    package,
    platform,
    hosted,
    module,
    malformed,

    /// Extern-compatible tag for serialization
    pub const Tag = enum(u32) {
        type_module,
        default_app,
        app,
        package,
        platform,
        hosted,
        module,
        malformed,
    };

    /// Extern-compatible payload union for serialization
    pub const Payload = extern union {
        type_module_ident: Ident.Idx,
        none: u32,
    };

    /// Extern-compatible serialized form
    pub const Serialized = extern struct {
        tag: Tag,
        payload: Payload,

        pub fn encode(kind: ModuleKind) @This() {
            return switch (kind) {
                .type_module => |idx| .{ .tag = .type_module, .payload = .{ .type_module_ident = idx } },
                .default_app => .{ .tag = .default_app, .payload = .{ .none = 0 } },
                .app => .{ .tag = .app, .payload = .{ .none = 0 } },
                .package => .{ .tag = .package, .payload = .{ .none = 0 } },
                .platform => .{ .tag = .platform, .payload = .{ .none = 0 } },
                .hosted => .{ .tag = .hosted, .payload = .{ .none = 0 } },
                .module => .{ .tag = .module, .payload = .{ .none = 0 } },
                .malformed => .{ .tag = .malformed, .payload = .{ .none = 0 } },
            };
        }

        pub fn decode(self: @This()) ModuleKind {
            return switch (self.tag) {
                .type_module => .{ .type_module = self.payload.type_module_ident },
                .default_app => .default_app,
                .app => .app,
                .package => .package,
                .platform => .platform,
                .hosted => .hosted,
                .module => .module,
                .malformed => .malformed,
            };
        }
    };
};

/// Module role known before header canonicalization.
pub const ModuleRole = enum(u8) {
    user,
    builtin,
};

/// Well-known identifiers that are interned once and reused throughout compilation.
/// These are needed for type checking, operator desugaring, and layout generation.
/// This is an extern struct so it can be embedded in serialized ModuleEnv.
pub const CommonIdents = extern struct {
    // Method names for operator desugaring
    plus: Ident.Idx,
    minus: Ident.Idx,
    times: Ident.Idx,
    div_by: Ident.Idx,
    div_trunc_by: Ident.Idx,
    rem_by: Ident.Idx,
    negate: Ident.Idx,
    abs: Ident.Idx,
    abs_diff: Ident.Idx,
    not: Ident.Idx,
    is_lt: Ident.Idx,
    is_lte: Ident.Idx,
    is_gt: Ident.Idx,
    is_gte: Ident.Idx,
    is_eq: Ident.Idx,
    to_hash: Ident.Idx,
    parser_for: Ident.Idx,
    encode_to: Ident.Idx,

    // Type/module names
    @"try": Ident.Idx,
    out_of_range: Ident.Idx,
    builtin_module: Ident.Idx,
    main_bang: Ident.Idx,
    str: Ident.Idx,
    list: Ident.Idx,
    iter: Ident.Idx,
    box: Ident.Idx,
    dict: Ident.Idx,
    set: Ident.Idx,

    // Unqualified builtin type names (for checking if a type name shadows a builtin)
    num: Ident.Idx,
    bool: Ident.Idx,
    u8: Ident.Idx,
    u16: Ident.Idx,
    u32: Ident.Idx,
    u64: Ident.Idx,
    u128: Ident.Idx,
    i8: Ident.Idx,
    i16: Ident.Idx,
    i32: Ident.Idx,
    i64: Ident.Idx,
    i128: Ident.Idx,
    f32: Ident.Idx,
    f64: Ident.Idx,
    dec: Ident.Idx,

    // Fully-qualified type identifiers for type checking and layout generation
    builtin_iter: Ident.Idx,
    builtin_try: Ident.Idx,
    builtin_numeral: Ident.Idx,
    builtin_str: Ident.Idx,
    builtin_list: Ident.Idx,
    builtin_box: Ident.Idx,
    builtin_dict: Ident.Idx,
    builtin_set: Ident.Idx,
    builtin_encoding_parse_tag_union_spec: Ident.Idx,
    builtin_encoding_field_names: Ident.Idx,
    builtin_encoding_field_name: Ident.Idx,
    builtin_str_inspect: Ident.Idx,
    builtin_crypto_sha256_digest: Ident.Idx,
    builtin_crypto_sha256_hasher: Ident.Idx,
    builtin_crypto_blake3_digest: Ident.Idx,
    builtin_crypto_blake3_hasher: Ident.Idx,
    u8_type: Ident.Idx,
    i8_type: Ident.Idx,
    u16_type: Ident.Idx,
    i16_type: Ident.Idx,
    u32_type: Ident.Idx,
    i32_type: Ident.Idx,
    u64_type: Ident.Idx,
    i64_type: Ident.Idx,
    u128_type: Ident.Idx,
    i128_type: Ident.Idx,
    f32_type: Ident.Idx,
    f64_type: Ident.Idx,
    dec_type: Ident.Idx,
    bool_type: Ident.Idx,

    // Field/tag names used during type checking and evaluation
    before_dot: Ident.Idx,
    after_dot: Ident.Idx,
    provided_by_compiler: Ident.Idx,
    tag: Ident.Idx,
    payload: Ident.Idx,
    is_negative: Ident.Idx,
    digits_before_pt: Ident.Idx,
    digits_after_pt: Ident.Idx,
    digits_after_pt_count: Ident.Idx,
    box_method: Ident.Idx,
    unbox_method: Ident.Idx,
    // Fully qualified Box intrinsic method names
    builtin_box_box: Ident.Idx,
    builtin_box_unbox: Ident.Idx,
    to_inspect: Ident.Idx,
    ok: Ident.Idx,
    err: Ident.Idx,
    from_numeral: Ident.Idx,
    from_quote: Ident.Idx,
    from_interpolation: Ident.Idx,
    true_tag: Ident.Idx,
    false_tag: Ident.Idx,
    // from_utf8 result fields
    byte_index: Ident.Idx,
    string: Ident.Idx,
    is_ok: Ident.Idx,
    problem_code: Ident.Idx,
    // from_utf8 error payload fields (BadUtf8 record)
    problem: Ident.Idx,
    index: Ident.Idx,
    // Synthetic identifiers for ? operator desugaring
    question_ok: Ident.Idx,
    question_err: Ident.Idx,
    // Synthetic identifier for .. implicit rigids in open tag unions or records
    open_ext: Ident.Idx,

    /// Insert all well-known identifiers into a CommonEnv.
    /// Use this when creating a fresh ModuleEnv from scratch.
    pub fn insert(gpa: std.mem.Allocator, common: *CommonEnv) std.mem.Allocator.Error!CommonIdents {
        return .{
            .plus = try common.insertIdent(gpa, Ident.for_text(Ident.PLUS_METHOD_NAME)),
            .minus = try common.insertIdent(gpa, Ident.for_text("minus")),
            .times = try common.insertIdent(gpa, Ident.for_text("times")),
            .div_by = try common.insertIdent(gpa, Ident.for_text("div_by")),
            .div_trunc_by = try common.insertIdent(gpa, Ident.for_text("div_trunc_by")),
            .rem_by = try common.insertIdent(gpa, Ident.for_text("rem_by")),
            .negate = try common.insertIdent(gpa, Ident.for_text(Ident.NEGATE_METHOD_NAME)),
            .abs = try common.insertIdent(gpa, Ident.for_text("abs")),
            .abs_diff = try common.insertIdent(gpa, Ident.for_text("abs_diff")),
            .not = try common.insertIdent(gpa, Ident.for_text("not")),
            .is_lt = try common.insertIdent(gpa, Ident.for_text("is_lt")),
            .is_lte = try common.insertIdent(gpa, Ident.for_text("is_lte")),
            .is_gt = try common.insertIdent(gpa, Ident.for_text("is_gt")),
            .is_gte = try common.insertIdent(gpa, Ident.for_text("is_gte")),
            .is_eq = try common.insertIdent(gpa, Ident.for_text("is_eq")),
            .to_hash = try common.insertIdent(gpa, Ident.for_text("to_hash")),
            .parser_for = try common.insertIdent(gpa, Ident.for_text("parser_for")),
            .encode_to = try common.insertIdent(gpa, Ident.for_text("encode_to")),
            .@"try" = try common.insertIdent(gpa, Ident.for_text("Try")),
            .out_of_range = try common.insertIdent(gpa, Ident.for_text("OutOfRange")),
            .builtin_module = try common.insertIdent(gpa, Ident.for_text("Builtin")),
            .main_bang = try common.insertIdent(gpa, Ident.for_text("main!")),
            .str = try common.insertIdent(gpa, Ident.for_text("Str")),
            .list = try common.insertIdent(gpa, Ident.for_text("List")),
            .iter = try common.insertIdent(gpa, Ident.for_text("Iter")),
            .box = try common.insertIdent(gpa, Ident.for_text("Box")),
            .dict = try common.insertIdent(gpa, Ident.for_text("Dict")),
            .set = try common.insertIdent(gpa, Ident.for_text("Set")),
            // Unqualified builtin type names
            .num = try common.insertIdent(gpa, Ident.for_text("Num")),
            .bool = try common.insertIdent(gpa, Ident.for_text("Bool")),
            .u8 = try common.insertIdent(gpa, Ident.for_text("U8")),
            .u16 = try common.insertIdent(gpa, Ident.for_text("U16")),
            .u32 = try common.insertIdent(gpa, Ident.for_text("U32")),
            .u64 = try common.insertIdent(gpa, Ident.for_text("U64")),
            .u128 = try common.insertIdent(gpa, Ident.for_text("U128")),
            .i8 = try common.insertIdent(gpa, Ident.for_text("I8")),
            .i16 = try common.insertIdent(gpa, Ident.for_text("I16")),
            .i32 = try common.insertIdent(gpa, Ident.for_text("I32")),
            .i64 = try common.insertIdent(gpa, Ident.for_text("I64")),
            .i128 = try common.insertIdent(gpa, Ident.for_text("I128")),
            .f32 = try common.insertIdent(gpa, Ident.for_text("F32")),
            .f64 = try common.insertIdent(gpa, Ident.for_text("F64")),
            .dec = try common.insertIdent(gpa, Ident.for_text("Dec")),
            .builtin_iter = try common.insertIdent(gpa, Ident.for_text("Builtin.Iter")),
            .builtin_try = try common.insertIdent(gpa, Ident.for_text("Builtin.Try")),
            .builtin_numeral = try common.insertIdent(gpa, Ident.for_text("Builtin.Num.Numeral")),
            .builtin_str = try common.insertIdent(gpa, Ident.for_text("Builtin.Str")),
            .builtin_list = try common.insertIdent(gpa, Ident.for_text("Builtin.List")),
            .builtin_box = try common.insertIdent(gpa, Ident.for_text("Builtin.Box")),
            .builtin_dict = try common.insertIdent(gpa, Ident.for_text("Builtin.Dict")),
            .builtin_set = try common.insertIdent(gpa, Ident.for_text("Builtin.Set")),
            .builtin_encoding_parse_tag_union_spec = try common.insertIdent(gpa, Ident.for_text("Builtin.Encoding.ParseTagUnionSpec")),
            .builtin_encoding_field_names = try common.insertIdent(gpa, Ident.for_text("Builtin.Encoding.FieldName.FieldNames")),
            .builtin_encoding_field_name = try common.insertIdent(gpa, Ident.for_text("Builtin.Encoding.FieldName")),
            .builtin_str_inspect = try common.insertIdent(gpa, Ident.for_text("Builtin.Str.inspect")),
            .builtin_crypto_sha256_digest = try common.insertIdent(gpa, Ident.for_text("Builtin.Crypto.SHA256.Digest")),
            .builtin_crypto_sha256_hasher = try common.insertIdent(gpa, Ident.for_text("Builtin.Crypto.SHA256.Hasher")),
            .builtin_crypto_blake3_digest = try common.insertIdent(gpa, Ident.for_text("Builtin.Crypto.BLAKE3.Digest")),
            .builtin_crypto_blake3_hasher = try common.insertIdent(gpa, Ident.for_text("Builtin.Crypto.BLAKE3.Hasher")),
            .u8_type = try common.insertIdent(gpa, Ident.for_text("Builtin.Num.U8")),
            .i8_type = try common.insertIdent(gpa, Ident.for_text("Builtin.Num.I8")),
            .u16_type = try common.insertIdent(gpa, Ident.for_text("Builtin.Num.U16")),
            .i16_type = try common.insertIdent(gpa, Ident.for_text("Builtin.Num.I16")),
            .u32_type = try common.insertIdent(gpa, Ident.for_text("Builtin.Num.U32")),
            .i32_type = try common.insertIdent(gpa, Ident.for_text("Builtin.Num.I32")),
            .u64_type = try common.insertIdent(gpa, Ident.for_text("Builtin.Num.U64")),
            .i64_type = try common.insertIdent(gpa, Ident.for_text("Builtin.Num.I64")),
            .u128_type = try common.insertIdent(gpa, Ident.for_text("Builtin.Num.U128")),
            .i128_type = try common.insertIdent(gpa, Ident.for_text("Builtin.Num.I128")),
            .f32_type = try common.insertIdent(gpa, Ident.for_text("Builtin.Num.F32")),
            .f64_type = try common.insertIdent(gpa, Ident.for_text("Builtin.Num.F64")),
            .dec_type = try common.insertIdent(gpa, Ident.for_text("Builtin.Num.Dec")),
            .bool_type = try common.insertIdent(gpa, Ident.for_text("Builtin.Bool")),
            .before_dot = try common.insertIdent(gpa, Ident.for_text("before_dot")),
            .after_dot = try common.insertIdent(gpa, Ident.for_text("after_dot")),
            .provided_by_compiler = try common.insertIdent(gpa, Ident.for_text("ProvidedByCompiler")),
            .tag = try common.insertIdent(gpa, Ident.for_text("tag")),
            .payload = try common.insertIdent(gpa, Ident.for_text("payload")),
            .is_negative = try common.insertIdent(gpa, Ident.for_text("is_negative")),
            .digits_before_pt = try common.insertIdent(gpa, Ident.for_text("digits_before_pt")),
            .digits_after_pt = try common.insertIdent(gpa, Ident.for_text("digits_after_pt")),
            .digits_after_pt_count = try common.insertIdent(gpa, Ident.for_text("digits_after_pt_count")),
            .box_method = try common.insertIdent(gpa, Ident.for_text("box")),
            .unbox_method = try common.insertIdent(gpa, Ident.for_text("unbox")),
            // Fully qualified Box intrinsic method names
            .builtin_box_box = try common.insertIdent(gpa, Ident.for_text("Builtin.Box.box")),
            .builtin_box_unbox = try common.insertIdent(gpa, Ident.for_text("Builtin.Box.unbox")),
            .to_inspect = try common.insertIdent(gpa, Ident.for_text("to_inspect")),
            .ok = try common.insertIdent(gpa, Ident.for_text("Ok")),
            .err = try common.insertIdent(gpa, Ident.for_text("Err")),
            .from_numeral = try common.insertIdent(gpa, Ident.for_text("from_numeral")),
            .from_quote = try common.insertIdent(gpa, Ident.for_text("from_quote")),
            .from_interpolation = try common.insertIdent(gpa, Ident.for_text("from_interpolation")),
            .true_tag = try common.insertIdent(gpa, Ident.for_text("True")),
            .false_tag = try common.insertIdent(gpa, Ident.for_text("False")),
            // from_utf8 result fields
            .byte_index = try common.insertIdent(gpa, Ident.for_text("byte_index")),
            .string = try common.insertIdent(gpa, Ident.for_text("string")),
            .is_ok = try common.insertIdent(gpa, Ident.for_text("is_ok")),
            .problem_code = try common.insertIdent(gpa, Ident.for_text("problem_code")),
            // from_utf8 error payload fields (BadUtf8 record)
            .problem = try common.insertIdent(gpa, Ident.for_text("problem")),
            .index = try common.insertIdent(gpa, Ident.for_text("index")),
            // Synthetic identifiers for ? operator desugaring
            .question_ok = try common.insertIdent(gpa, Ident.for_text("#ok")),
            .question_err = try common.insertIdent(gpa, Ident.for_text("#err")),
            // Synthetic identifier for .. implicit rigids in open tag unions or records
            .open_ext = try common.insertIdent(gpa, Ident.for_text("#others")),
        };
    }

    /// Find all well-known identifiers in a CommonEnv that has already interned them.
    /// Use this when loading a pre-compiled module where identifiers are already present.
    /// Panics if any identifier is not found (indicates corrupted/incompatible pre-compiled data).
    pub fn find(common: *const CommonEnv) CommonIdents {
        return .{
            .plus = common.findIdent(Ident.PLUS_METHOD_NAME) orelse unreachable,
            .minus = common.findIdent("minus") orelse unreachable,
            .times = common.findIdent("times") orelse unreachable,
            .div_by = common.findIdent("div_by") orelse unreachable,
            .div_trunc_by = common.findIdent("div_trunc_by") orelse unreachable,
            .rem_by = common.findIdent("rem_by") orelse unreachable,
            .negate = common.findIdent(Ident.NEGATE_METHOD_NAME) orelse unreachable,
            .abs = common.findIdent("abs") orelse unreachable,
            .abs_diff = common.findIdent("abs_diff") orelse unreachable,
            .not = common.findIdent("not") orelse unreachable,
            .is_lt = common.findIdent("is_lt") orelse unreachable,
            .is_lte = common.findIdent("is_lte") orelse unreachable,
            .is_gt = common.findIdent("is_gt") orelse unreachable,
            .is_gte = common.findIdent("is_gte") orelse unreachable,
            .is_eq = common.findIdent("is_eq") orelse unreachable,
            .to_hash = common.findIdent("to_hash") orelse unreachable,
            .parser_for = common.findIdent("parser_for") orelse unreachable,
            .encode_to = common.findIdent("encode_to") orelse unreachable,
            .@"try" = common.findIdent("Try") orelse unreachable,
            .out_of_range = common.findIdent("OutOfRange") orelse unreachable,
            .builtin_module = common.findIdent("Builtin") orelse unreachable,
            .main_bang = common.findIdent("main!") orelse unreachable,
            .str = common.findIdent("Str") orelse unreachable,
            .list = common.findIdent("List") orelse unreachable,
            .iter = common.findIdent("Iter") orelse unreachable,
            .box = common.findIdent("Box") orelse unreachable,
            .dict = common.findIdent("Dict") orelse unreachable,
            .set = common.findIdent("Set") orelse unreachable,
            // Unqualified builtin type names
            .num = common.findIdent("Num") orelse unreachable,
            .bool = common.findIdent("Bool") orelse unreachable,
            .u8 = common.findIdent("U8") orelse unreachable,
            .u16 = common.findIdent("U16") orelse unreachable,
            .u32 = common.findIdent("U32") orelse unreachable,
            .u64 = common.findIdent("U64") orelse unreachable,
            .u128 = common.findIdent("U128") orelse unreachable,
            .i8 = common.findIdent("I8") orelse unreachable,
            .i16 = common.findIdent("I16") orelse unreachable,
            .i32 = common.findIdent("I32") orelse unreachable,
            .i64 = common.findIdent("I64") orelse unreachable,
            .i128 = common.findIdent("I128") orelse unreachable,
            .f32 = common.findIdent("F32") orelse unreachable,
            .f64 = common.findIdent("F64") orelse unreachable,
            .dec = common.findIdent("Dec") orelse unreachable,
            .builtin_iter = common.findIdent("Builtin.Iter") orelse unreachable,
            .builtin_try = common.findIdent("Builtin.Try") orelse unreachable,
            .builtin_numeral = common.findIdent("Builtin.Num.Numeral") orelse unreachable,
            .builtin_str = common.findIdent("Builtin.Str") orelse unreachable,
            .builtin_list = common.findIdent("Builtin.List") orelse unreachable,
            .builtin_box = common.findIdent("Builtin.Box") orelse unreachable,
            .builtin_dict = common.findIdent("Builtin.Dict") orelse unreachable,
            .builtin_set = common.findIdent("Builtin.Set") orelse unreachable,
            .builtin_encoding_parse_tag_union_spec = common.findIdent("Builtin.Encoding.ParseTagUnionSpec") orelse unreachable,
            .builtin_encoding_field_names = common.findIdent("Builtin.Encoding.FieldName.FieldNames") orelse unreachable,
            .builtin_encoding_field_name = common.findIdent("Builtin.Encoding.FieldName") orelse unreachable,
            .builtin_str_inspect = common.findIdent("Builtin.Str.inspect") orelse unreachable,
            .builtin_crypto_sha256_digest = common.findIdent("Builtin.Crypto.SHA256.Digest") orelse unreachable,
            .builtin_crypto_sha256_hasher = common.findIdent("Builtin.Crypto.SHA256.Hasher") orelse unreachable,
            .builtin_crypto_blake3_digest = common.findIdent("Builtin.Crypto.BLAKE3.Digest") orelse unreachable,
            .builtin_crypto_blake3_hasher = common.findIdent("Builtin.Crypto.BLAKE3.Hasher") orelse unreachable,
            .u8_type = common.findIdent("Builtin.Num.U8") orelse unreachable,
            .i8_type = common.findIdent("Builtin.Num.I8") orelse unreachable,
            .u16_type = common.findIdent("Builtin.Num.U16") orelse unreachable,
            .i16_type = common.findIdent("Builtin.Num.I16") orelse unreachable,
            .u32_type = common.findIdent("Builtin.Num.U32") orelse unreachable,
            .i32_type = common.findIdent("Builtin.Num.I32") orelse unreachable,
            .u64_type = common.findIdent("Builtin.Num.U64") orelse unreachable,
            .i64_type = common.findIdent("Builtin.Num.I64") orelse unreachable,
            .u128_type = common.findIdent("Builtin.Num.U128") orelse unreachable,
            .i128_type = common.findIdent("Builtin.Num.I128") orelse unreachable,
            .f32_type = common.findIdent("Builtin.Num.F32") orelse unreachable,
            .f64_type = common.findIdent("Builtin.Num.F64") orelse unreachable,
            .dec_type = common.findIdent("Builtin.Num.Dec") orelse unreachable,
            .bool_type = common.findIdent("Builtin.Bool") orelse unreachable,
            .before_dot = common.findIdent("before_dot") orelse unreachable,
            .after_dot = common.findIdent("after_dot") orelse unreachable,
            .provided_by_compiler = common.findIdent("ProvidedByCompiler") orelse unreachable,
            .tag = common.findIdent("tag") orelse unreachable,
            .payload = common.findIdent("payload") orelse unreachable,
            .is_negative = common.findIdent("is_negative") orelse unreachable,
            .digits_before_pt = common.findIdent("digits_before_pt") orelse unreachable,
            .digits_after_pt = common.findIdent("digits_after_pt") orelse unreachable,
            .digits_after_pt_count = common.findIdent("digits_after_pt_count") orelse unreachable,
            .box_method = common.findIdent("box") orelse unreachable,
            .unbox_method = common.findIdent("unbox") orelse unreachable,
            // Fully qualified Box intrinsic method names
            .builtin_box_box = common.findIdent("Builtin.Box.box") orelse unreachable,
            .builtin_box_unbox = common.findIdent("Builtin.Box.unbox") orelse unreachable,
            .to_inspect = common.findIdent("to_inspect") orelse unreachable,
            .ok = common.findIdent("Ok") orelse unreachable,
            .err = common.findIdent("Err") orelse unreachable,
            .from_numeral = common.findIdent("from_numeral") orelse unreachable,
            .from_quote = common.findIdent("from_quote") orelse unreachable,
            .from_interpolation = common.findIdent("from_interpolation") orelse unreachable,
            .true_tag = common.findIdent("True") orelse unreachable,
            .false_tag = common.findIdent("False") orelse unreachable,
            // from_utf8 result fields
            .byte_index = common.findIdent("byte_index") orelse unreachable,
            .string = common.findIdent("string") orelse unreachable,
            .is_ok = common.findIdent("is_ok") orelse unreachable,
            .problem_code = common.findIdent("problem_code") orelse unreachable,
            // from_utf8 error payload fields (BadUtf8 record)
            .problem = common.findIdent("problem") orelse unreachable,
            .index = common.findIdent("index") orelse unreachable,
            // Synthetic identifiers for ? operator desugaring
            .question_ok = common.findIdent("#ok") orelse unreachable,
            .question_err = common.findIdent("#err") orelse unreachable,
            // Synthetic identifier for .. implicit rigids in open tag unions or records
            .open_ext = common.findIdent("#others") orelse unreachable,
        };
    }
};

/// Key for method lookup: (owner type declaration, method_ident) pair.
pub const MethodKey = extern struct {
    owner: CIR.Statement.Idx,
    method_ident_bits: u32,

    pub fn init(owner: CIR.Statement.Idx, method_ident: Ident.Idx) MethodKey {
        return .{
            .owner = owner,
            .method_ident_bits = @bitCast(method_ident),
        };
    }

    pub fn methodIdent(self: MethodKey) Ident.Idx {
        return @bitCast(self.method_ident_bits);
    }

    pub fn order(a: MethodKey, b: MethodKey) std.math.Order {
        const a_owner = @intFromEnum(a.owner);
        const b_owner = @intFromEnum(b.owner);
        if (a_owner != b_owner) {
            return if (a_owner < b_owner) .lt else .gt;
        }

        const a_method = a.method_ident_bits;
        const b_method = b.method_ident_bits;
        if (a_method == b_method) return .eq;
        return if (a_method < b_method) .lt else .gt;
    }
};

/// Mapping from (owner declaration, method_ident) pairs to their qualified
/// method ident.
///
/// This is populated during canonicalization when methods are defined in associated blocks.
pub const MethodIdents = SortedArrayBuilder(MethodKey, Ident.Idx);
/// Type/checking and implementation metadata for a method.
pub const MethodBinding = extern struct {
    /// Node whose type variable contains the checked method type.
    type_node_idx: Node.Idx,
    /// Def that owns the method implementation identity.
    def_idx: CIR.Def.Idx,
};

/// Mapping from (owner declaration, method_ident) pairs to the method binding.
/// This keeps method implementation lookup explicit without requiring local
/// associated methods to be published through the module exposure table.
pub const MethodDefs = SortedArrayBuilder(MethodKey, MethodBinding);

/// Checked dispatch metadata for one source `for` loop.
///
/// Checking writes this when it creates the loop's required `iter` and `next`
/// static-dispatch constraints. Checked artifact publication consumes it to
/// publish an explicit iterator-for plan for mono lowering.
pub const ForLoopDispatchPlan = extern struct {
    node_idx: u32,
    pattern_idx: u32,
    iterable_idx: u32,
    iter_fn_var: u32,
    next_fn_var: u32,

    pub const SafeList = collections.SafeList(@This());
};

/// Exact digit data for one numeric source node.
///
/// The parser converts numeric text to base-256 byte lists. Canonicalization
/// copies those bytes here so later stages can construct `Num.Numeral` values
/// for custom `from_numeral` calls without parsing source text.
pub const NumeralLiteral = extern struct {
    node_idx: u32,
    digits_start: u32,
    before_len: u32,
    after_len: u32,
    after_decimal_digit_count: u32,
    flags: u32,

    pub const negative_flag: u32 = 1;
    pub const fractional_flag: u32 = 2;
    pub const decimal_point_flag: u32 = 4;
    pub const SafeList = collections.SafeList(@This());

    pub fn isNegative(self: NumeralLiteral) bool {
        return (self.flags & negative_flag) != 0;
    }

    pub fn isFractional(self: NumeralLiteral) bool {
        return (self.flags & fractional_flag) != 0;
    }

    pub fn hadDecimalPoint(self: NumeralLiteral) bool {
        return (self.flags & decimal_point_flag) != 0;
    }
};

/// Checked dispatch metadata for a numeric literal that must call
/// `from_numeral` at runtime.
pub const NumeralDispatchPlan = extern struct {
    node_idx: u32,
    target_var: u32,
    fn_var: u32,

    pub const SafeList = collections.SafeList(@This());
};

/// Resolved type target for an explicit numeric suffix such as `123.U64` or
/// `123.Custom`. Canonicalization records this once from scope resolution;
/// checking consumes it directly instead of looking up the suffix text again.
pub const NumericSuffixTarget = extern struct {
    node_idx: u32,
    kind: u32,
    data1: u32,
    data2: u32,

    pub const SafeList = collections.SafeList(@This());

    pub const Kind = enum(u32) {
        builtin,
        local,
        external,
        invalid,
    };

    pub const Target = union(enum) {
        builtin: CIR.NumKind,
        local: CIR.Statement.Idx,
        external: struct {
            import_idx: CIR.Import.Idx,
            target_node_idx: u32,
        },
        invalid,
    };

    pub fn target(self: NumericSuffixTarget) Target {
        return switch (@as(Kind, @enumFromInt(self.kind))) {
            .builtin => .{ .builtin = @enumFromInt(self.data1) },
            .local => .{ .local = @enumFromInt(self.data1) },
            .external => .{ .external = .{
                .import_idx = @enumFromInt(self.data1),
                .target_node_idx = self.data2,
            } },
            .invalid => .invalid,
        };
    }
};

gpa: std.mem.Allocator,

common: CommonEnv,
types: TypeStore,

// Module compilation fields
// NOTE: These fields are populated during canonicalization and preserved for later use

/// The kind of module (type_module, app, etc.) - set during canonicalization
module_kind: ModuleKind,
/// The compiler role of this module, known before header canonicalization.
module_role: ModuleRole,
/// All the definitions in the module (populated by canonicalization)
all_defs: CIR.Def.Span,
/// Module-global value definitions: top-level values, associated items, and
/// compiler-created hosted globals. Local block definitions are not included.
global_value_defs: CIR.Def.Span,
/// All the top-level statements in the module (populated by canonicalization)
all_statements: CIR.Statement.Span,
/// All canonical type-declaration statements in the module.
type_decls: CIR.Statement.Span,
/// Type declarations prepared by forward references before their source declaration.
forward_type_decls: CIR.Statement.Span,
/// Definitions that are exported by this module (populated by canonicalization)
exports: CIR.Def.Span,
/// Required type signatures for platform modules (from `requires { main! : () => {} }`)
/// Maps identifier names to their expected type annotations.
/// Empty for non-platform modules.
requires_types: RequiredType.SafeList,
/// Type alias mappings from for-clauses in requires declarations.
/// Stores (alias_name, rigid_name) pairs like (Model, model).
for_clause_aliases: ForClauseAlias.SafeList,
/// Platform provides entries mapping Roc identifiers to FFI symbols.
/// Populated during canonicalization for platform modules. Empty for non-platform modules.
provides_entries: ProvidesEntry.SafeList,
/// Platform hosted entries in header declaration order (defines dispatch order)
hosted_entries: HostedEntry.SafeList,
/// All builtin stmts (temporary until module imports are working)
builtin_statements: CIR.Statement.Span,
/// All external declarations referenced in this module
external_decls: CIR.ExternalDecl.SafeList,
/// Store for interned module imports
imports: CIR.Import.Store,
/// Source-relative file imports read while canonicalizing this module.
file_dependencies: FileDependency.SafeList,
/// The module's name as a string
/// This is needed for import resolution to match import names to modules
module_name: []const u8,
/// The module's bare name as an interned identifier (e.g., "Color").
/// Used for display, type module validation, and method name construction.
display_module_name_idx: Ident.Idx,
/// Package-qualified module identity (e.g., "pf.Color"). Used as origin_module on types
/// for identity comparisons across packages. Set by the coordinator after parse or cache hit.
qualified_module_ident: Ident.Idx,
/// Diagnostics collected during canonicalization (optional)
diagnostics: CIR.Diagnostic.Span,
/// Stores the raw nodes which represent the intermediate representation
/// Uses an efficient data structure, and provides helpers for storing and retrieving nodes.
store: NodeStore,

/// Dependency analysis results (evaluation order for defs)
/// Set after canonicalization completes. Must not be accessed before then.
evaluation_order: ?*DependencyGraph.EvaluationOrder,

/// True only after `check.TypedCIR.prepareRuntimeEnv` has prepared this env for
/// checked-artifact consumption. Serialized user modules intentionally do not
/// preserve this flag; the baked builtin module does, because its static env is
/// prepared before embedding and must not allocate/copy on compiler startup.
runtime_prepared: bool,

/// Well-known identifiers for type checking, operator desugaring, and layout generation.
/// Interned once during init to avoid repeated string comparisons.
idents: CommonIdents,

/// Import mapping for type display names in error messages.
/// Maps fully-qualified type identifiers to their shortest display names based on imports.
/// Built during canonicalization when processing import statements.
/// Example: "MyModule.Foo" -> "F" if user has `import MyModule exposing [Foo as F]`
import_mapping: types_mod.import_mapping.ImportMapping,

/// Mapping from (owner declaration, method_ident) pairs to qualified method idents.
/// Populated during canonicalization when methods are defined in associated blocks.
method_idents: MethodIdents,
/// Mapping from (owner declaration, method_ident) pairs to defining def indices.
method_defs: MethodDefs,

/// Dispatch plans attached by checking to source `for` loop nodes.
for_loop_dispatch_plans: ForLoopDispatchPlan.SafeList,
/// Base-256 bytes referenced by `numeral_literals`.
numeral_digit_bytes: collections.SafeList(u8),
/// Exact numeric literals attached to source expression and pattern nodes.
numeral_literals: NumeralLiteral.SafeList,
/// `from_numeral` dispatch plans attached by checking to source expression nodes.
numeral_dispatch_plans: NumeralDispatchPlan.SafeList,
/// `from_quote` dispatch plans attached by checking to source string literal
/// expression and pattern nodes. Shares `NumeralDispatchPlan`'s shape: a source
/// node plus the constraint's target and function type vars.
quote_dispatch_plans: NumeralDispatchPlan.SafeList,
/// Scope-resolved explicit numeric suffix targets attached by canonicalization.
numeric_suffix_targets: NumericSuffixTarget.SafeList,

/// A type alias mapping from a for-clause: [Model : model]
/// Maps an alias name (Model) to a rigid variable name (model)
pub const ForClauseAlias = struct {
    /// The alias name (e.g., "Model") - to be looked up in the app
    alias_name: Ident.Idx,
    /// The rigid variable name (e.g., "model") - the rigid in the required type
    rigid_name: Ident.Idx,
    /// The type annotation of this alias stmt
    alias_stmt_idx: CIR.Statement.Idx,

    pub const SafeList = collections.SafeList(@This());
};

/// Platform provides entry mapping a Roc identifier to its FFI symbol.
/// Populated during canonicalization for platform modules from the provides clause.
/// For example, `{ main_for_host!: "main" }` creates an entry with ident="main_for_host!"
/// and ffi_symbol pointing to the interned string "main".
pub const ProvidesEntry = struct {
    /// The Roc identifier (e.g., "main_for_host!")
    ident: Ident.Idx,
    /// The FFI symbol string (e.g., "main")
    ffi_symbol: StringLiteral.Idx,

    pub const SafeList = collections.SafeList(@This());
};

/// Platform hosted entry mapping a linker symbol to a hosted function in an
/// exposed type module. Populated during canonicalization for platform modules
/// from the hosted clause, in declaration order (which defines hosted dispatch
/// order). For example, `hosted { "roc_stdout_line": Stdout.line! }` creates an
/// entry with module_ident="Stdout", func_ident="line!", and symbol pointing to
/// the interned string "roc_stdout_line".
pub const HostedEntry = struct {
    /// The type module name (e.g., "Stdout"); null for unqualified functions
    module_ident: ?Ident.Idx,
    /// The hosted function name (e.g., "line!")
    func_ident: Ident.Idx,
    /// The literal linker symbol (e.g., "roc_stdout_line")
    symbol: StringLiteral.Idx,

    pub const SafeList = collections.SafeList(@This());
};

/// Required type for platform modules - maps an identifier to its expected type annotation.
/// Used to enforce that apps provide values matching the platform's required types.
pub const RequiredType = struct {
    /// The identifier name (e.g., "main!")
    ident: Ident.Idx,
    /// The canonicalized type annotation for this required value
    type_anno: CIR.TypeAnno.Idx,
    /// Region of the requirement for error reporting
    region: Region,
    /// Type alias mappings from the for-clause (e.g., [Model : model])
    /// These specify which app type aliases should be substituted for which rigids
    type_aliases: ForClauseAlias.SafeList.Range,

    pub const SafeList = collections.SafeList(@This());
};

/// File import dependency state for watch mode and checked-cache identity.
/// The content hash is meaningful only when the state is `present`.
pub const FileDependencyState = enum(u8) {
    pending,
    missing,
    unreadable,
    present,
};

/// Source-relative file import dependency for watch mode and checked-cache
/// identity. `relative_path` is interpreted relative to the module source
/// directory by higher-level build code; it is never an absolute or realpathed
/// host path.
pub const FileDependency = extern struct {
    relative_path: StringLiteral.Idx,
    state: FileDependencyState,
    _padding: [3]u8,
    content_hash: [32]u8,

    pub const SafeList = collections.SafeList(@This());
};

/// Relocate all pointers in the ModuleEnv by the given offset.
/// This is used by serialized compiler artifacts whose internal pointers are
/// stored relative to the artifact buffer.
pub fn relocate(self: *Self, offset: isize) void {
    // Relocate all sub-structures that contain pointers
    self.common.relocate(offset);
    self.types.relocate(offset);
    self.external_decls.relocate(offset);
    self.requires_types.relocate(offset);
    self.for_clause_aliases.relocate(offset);
    self.provides_entries.relocate(offset);
    self.hosted_entries.relocate(offset);
    self.imports.relocate(offset);
    self.file_dependencies.relocate(offset);
    self.store.relocate(offset);
    self.method_idents.relocate(offset);
    self.method_defs.relocate(offset);
    self.for_loop_dispatch_plans.relocate(offset);

    // Relocate the module_name pointer if it's not empty
    if (self.module_name.len > 0) {
        const old_ptr = @intFromPtr(self.module_name.ptr);
        const new_ptr = @as(isize, @intCast(old_ptr)) + offset;
        self.module_name.ptr = @ptrFromInt(@as(usize, @intCast(new_ptr)));
    }
}

/// Initialize the compilation fields in an existing ModuleEnv
pub fn initCIRFields(self: *Self, module_name: []const u8) Allocator.Error!void {
    self.module_kind = .module; // Placeholder - set to actual kind during header canonicalization
    self.module_role = .user;
    self.all_defs = .{ .span = .{ .start = 0, .len = 0 } };
    self.global_value_defs = .{ .span = .{ .start = 0, .len = 0 } };
    self.all_statements = .{ .span = .{ .start = 0, .len = 0 } };
    self.type_decls = .{ .span = .{ .start = 0, .len = 0 } };
    self.forward_type_decls = .{ .span = .{ .start = 0, .len = 0 } };
    self.exports = .{ .span = .{ .start = 0, .len = 0 } };
    self.builtin_statements = .{ .span = .{ .start = 0, .len = 0 } };
    // Note: external_decls already exists from ModuleEnv.init(), so we don't create a new one
    self.imports = CIR.Import.Store.init();
    self.module_name = module_name;
    self.display_module_name_idx = try self.insertIdent(Ident.for_text(module_name));
    self.qualified_module_ident = self.display_module_name_idx; // Default to bare name; coordinator later records the package-qualified name
    self.diagnostics = CIR.Diagnostic.Span{ .span = base.DataSpan{ .start = 0, .len = 0 } };
    // Note: self.store already exists from ModuleEnv.init(), so we don't create a new one
    self.evaluation_order = null; // Will be set after canonicalization completes
    self.runtime_prepared = false;
}

/// Alias for initCIRFields for backwards compatibility with tests
pub fn initModuleEnvFields(self: *Self, module_name: []const u8) Allocator.Error!void {
    return self.initCIRFields(module_name);
}

/// Initialize the module environment with capacity heuristics based on source size.
pub fn init(gpa: std.mem.Allocator, source: []const u8) std.mem.Allocator.Error!Self {
    var common = try CommonEnv.init(gpa, source);
    const idents = try CommonIdents.insert(gpa, &common);

    // Use source-based heuristics for initial capacities
    // Typical Roc code generates ~1 node per 20 bytes
    // Use generous minimums to avoid too many reallocations for small files
    const source_len = source.len;
    const node_capacity = @max(1024, @min(100_000, source_len / 20));

    return Self{
        .gpa = gpa,
        .common = common,
        .types = try TypeStore.initFromSourceLen(gpa, source_len),
        .module_kind = .module, // Placeholder - set to actual kind during header canonicalization
        .module_role = .user,
        .all_defs = .{ .span = .{ .start = 0, .len = 0 } },
        .global_value_defs = .{ .span = .{ .start = 0, .len = 0 } },
        .all_statements = .{ .span = .{ .start = 0, .len = 0 } },
        .type_decls = .{ .span = .{ .start = 0, .len = 0 } },
        .forward_type_decls = .{ .span = .{ .start = 0, .len = 0 } },
        .exports = .{ .span = .{ .start = 0, .len = 0 } },
        .requires_types = try RequiredType.SafeList.initCapacity(gpa, 4),
        .for_clause_aliases = try ForClauseAlias.SafeList.initCapacity(gpa, 4),
        .provides_entries = try ProvidesEntry.SafeList.initCapacity(gpa, 4),
        .hosted_entries = try HostedEntry.SafeList.initCapacity(gpa, 4),
        .builtin_statements = .{ .span = .{ .start = 0, .len = 0 } },
        .external_decls = try CIR.ExternalDecl.SafeList.initCapacity(gpa, 16),
        .imports = CIR.Import.Store.init(),
        .file_dependencies = .{},
        .module_name = "", // May be set later during canonicalization
        .display_module_name_idx = Ident.Idx.NONE, // Will be set later during canonicalization
        .qualified_module_ident = Ident.Idx.NONE, // Will be set by coordinator
        .diagnostics = CIR.Diagnostic.Span{ .span = base.DataSpan{ .start = 0, .len = 0 } },
        .store = try NodeStore.initCapacity(gpa, node_capacity),
        .evaluation_order = null, // Will be set after canonicalization completes
        .runtime_prepared = false,
        .idents = idents,
        .import_mapping = types_mod.import_mapping.ImportMapping.init(gpa),
        .method_idents = MethodIdents.init(),
        .method_defs = MethodDefs.init(),
        .for_loop_dispatch_plans = try ForLoopDispatchPlan.SafeList.initCapacity(gpa, 4),
        .numeral_digit_bytes = try collections.SafeList(u8).initCapacity(gpa, 32),
        .numeral_literals = try NumeralLiteral.SafeList.initCapacity(gpa, 8),
        .numeral_dispatch_plans = try NumeralDispatchPlan.SafeList.initCapacity(gpa, 8),
        .quote_dispatch_plans = try NumeralDispatchPlan.SafeList.initCapacity(gpa, 8),
        .numeric_suffix_targets = try NumericSuffixTarget.SafeList.initCapacity(gpa, 8),
    };
}

/// Deinitialize the module environment.
pub fn deinit(self: *Self) void {
    self.common.deinit(self.gpa);
    self.types.deinit();
    self.external_decls.deinit(self.gpa);
    self.requires_types.deinit(self.gpa);
    self.for_clause_aliases.deinit(self.gpa);
    self.provides_entries.deinit(self.gpa);
    self.hosted_entries.deinit(self.gpa);
    self.imports.deinit(self.gpa);
    self.file_dependencies.deinit(self.gpa);
    self.import_mapping.deinit();
    self.method_idents.deinit(self.gpa);
    self.method_defs.deinit(self.gpa);
    self.for_loop_dispatch_plans.deinit(self.gpa);
    self.numeral_digit_bytes.deinit(self.gpa);
    self.numeral_literals.deinit(self.gpa);
    self.numeral_dispatch_plans.deinit(self.gpa);
    self.quote_dispatch_plans.deinit(self.gpa);
    self.numeric_suffix_targets.deinit(self.gpa);
    // diagnostics are stored in the NodeStore, no need to free separately
    self.store.deinit();

    if (self.evaluation_order) |eval_order| {
        eval_order.deinit();
        self.gpa.destroy(eval_order);
    }
}

/// Deinitialize a cached module environment.
/// This frees heap-allocated data from deserialization:
/// - Hash maps (imports, import_mapping)
/// - Type store arrays (when using deserializeWithMutableTypes)
/// - NodeStore regions (when using deserializeWithMutableTypes)
///
/// After deserialization with deserializeWithMutableTypes, the type store
/// arrays and NodeStore regions are heap-allocated and can be mutated.
/// Other data (common env, nodes, etc.) still points into the cache buffer
/// and must NOT be freed.
///
/// Call this instead of deinit() for modules loaded from cache.
pub fn deinitCachedModule(self: *Self) void {
    // Free the type store arrays (allocated by deserializeWithMutableTypes)
    self.types.deinit();

    // Free the NodeStore regions (allocated by deserializeWithMutableTypes)
    self.store.regions.deinit(self.gpa);

    // Only free the hash map that was allocated during deserialization
    // (see CIR.Import.Store.Serialized.deserialize which calls ensureTotalCapacity)
    self.imports.deinitMapOnly(self.gpa);

    // import_mapping is initialized empty during deserialization and may have
    // items added later, so we need to free it
    self.import_mapping.deinit();
    self.for_loop_dispatch_plans.deinit(self.gpa);
    self.numeral_digit_bytes.deinit(self.gpa);
    self.numeral_literals.deinit(self.gpa);
    self.numeral_dispatch_plans.deinit(self.gpa);
    self.quote_dispatch_plans.deinit(self.gpa);
    self.numeric_suffix_targets.deinit(self.gpa);

    // If enableRuntimeInserts was called on the interner, it allocated new memory
    // that needs to be freed. The interner.deinit checks supports_inserts internally
    // and will only free if memory was actually allocated (not for pure cached data).
    self.common.idents.interner.deinit(self.gpa);
}

/// Record a relative file dependency before its final read state is known.
pub fn recordFileDependency(self: *Self, relative_path: []const u8) Allocator.Error!FileDependency.SafeList.Idx {
    const path_idx = try self.insertString(relative_path);
    return try self.file_dependencies.append(self.gpa, .{
        .relative_path = path_idx,
        .state = .pending,
        ._padding = .{ 0, 0, 0 },
        .content_hash = [_]u8{0} ** 32,
    });
}

/// Mark a previously recorded file dependency as missing.
pub fn setFileDependencyMissing(self: *Self, idx: FileDependency.SafeList.Idx) void {
    const dep = &self.file_dependencies.items.items[@intFromEnum(idx)];
    dep.state = .missing;
    dep.content_hash = [_]u8{0} ** 32;
}

/// Mark a previously recorded file dependency as unreadable.
pub fn setFileDependencyUnreadable(self: *Self, idx: FileDependency.SafeList.Idx) void {
    const dep = &self.file_dependencies.items.items[@intFromEnum(idx)];
    dep.state = .unreadable;
    dep.content_hash = [_]u8{0} ** 32;
}

/// Set the content hash for a previously recorded file dependency.
pub fn setFileDependencyContentHash(self: *Self, idx: FileDependency.SafeList.Idx, content_hash: [32]u8) void {
    const dep = &self.file_dependencies.items.items[@intFromEnum(idx)];
    dep.state = .present;
    dep.content_hash = content_hash;
}

/// Return the relative path string stored for a file dependency.
pub fn fileDependencyRelativePath(self: *const Self, dep: FileDependency) []const u8 {
    return self.getString(dep.relative_path);
}

// Module compilation functionality

/// Records a diagnostic error during canonicalization without blocking compilation.
pub fn pushDiagnostic(self: *Self, reason: CIR.Diagnostic) std.mem.Allocator.Error!void {
    _ = try self.addDiagnostic(reason);
}

/// Creates a malformed node that represents a runtime error in the IR.
pub fn pushMalformed(self: *Self, comptime RetIdx: type, reason: CIR.Diagnostic) std.mem.Allocator.Error!RetIdx {
    comptime if (!isCastable(RetIdx)) @compileError("Idx type " ++ @typeName(RetIdx) ++ " is not castable");
    const diag_idx = try self.addDiagnostic(reason);
    const region = getDiagnosticRegion(reason);
    const malformed_idx = try self.addMalformed(diag_idx, region);
    return castIdx(Node.Idx, RetIdx, malformed_idx);
}

/// Like `pushMalformed`, but does NOT register `reason` in the reported
/// diagnostics list. The malformed node still references the diagnostic (for
/// runtime crash text), but the diagnostic that is actually reported for this
/// site is pushed separately and later — used when forward-reference vs
/// mutual-recursion classification of a local definition is deferred to the end
/// of the enclosing block.
pub fn pushRuntimeErrorExpr(self: *Self, comptime RetIdx: type, reason: CIR.Diagnostic) std.mem.Allocator.Error!RetIdx {
    comptime if (!isCastable(RetIdx)) @compileError("Idx type " ++ @typeName(RetIdx) ++ " is not castable");
    const diag_idx = try self.store.addDiagnosticUnregistered(reason);
    const region = getDiagnosticRegion(reason);
    const malformed_idx = try self.addMalformed(diag_idx, region);
    return castIdx(Node.Idx, RetIdx, malformed_idx);
}

/// Replaces an existing expression with a runtime error and records the diagnostic.
pub fn replaceExprWithRuntimeError(self: *Self, expr_idx: CIR.Expr.Idx, reason: CIR.Diagnostic) std.mem.Allocator.Error!void {
    const diag_idx = try self.addDiagnostic(reason);
    self.store.setExprRuntimeError(expr_idx, diag_idx);
    self.debugAssertArraysInSync();
}

/// Extract the region from any diagnostic variant
fn getDiagnosticRegion(diagnostic: CIR.Diagnostic) Region {
    return switch (diagnostic) {
        .type_redeclared => |data| data.redeclared_region,
        .type_alias_redeclared => |data| data.redeclared_region,
        .nominal_type_redeclared => |data| data.redeclared_region,
        .duplicate_record_field => |data| data.duplicate_region,
        .duplicate_tag => |data| data.duplicate_region,
        inline else => |data| data.region,
    };
}

/// Import helper functions from CIR
const isCastable = CIR.isCastable;
/// Cast function for safely converting between compatible index types
pub const castIdx = CIR.castIdx;

// Module compilation functions

/// Retrieve all diagnostics collected during canonicalization.
pub fn getDiagnostics(self: *Self) std.mem.Allocator.Error![]CIR.Diagnostic {
    const diagnostic_indices = self.store.sliceDiagnostics(self.diagnostics);
    const diagnostics = try self.gpa.alloc(CIR.Diagnostic, diagnostic_indices.len);
    for (diagnostic_indices, 0..) |diagnostic_idx, i| {
        diagnostics[i] = self.store.getDiagnostic(diagnostic_idx);
    }
    return diagnostics;
}

/// Publish diagnostics that have been recorded since the current diagnostic
/// span was last finalized.
pub fn publishScratchDiagnostics(self: *Self) std.mem.Allocator.Error!void {
    const scratch = self.store.scratch orelse return;
    const new_top = scratch.diagnostics.top();
    if (new_top == 0) return;

    const existing = self.store.sliceDiagnostics(self.diagnostics);
    const index_start = self.store.index_data.len();

    for (existing) |diagnostic_idx| {
        _ = try self.store.index_data.append(self.gpa, @intFromEnum(diagnostic_idx));
    }

    var i: u32 = 0;
    while (i < new_top) : (i += 1) {
        const diagnostic_idx = scratch.diagnostics.items.items[@intCast(i)];
        _ = try self.store.index_data.append(self.gpa, @intFromEnum(diagnostic_idx));
    }

    scratch.diagnostics.clearFrom(0);
    self.diagnostics = .{
        .span = .{
            .start = @intCast(index_start),
            .len = @intCast(existing.len + new_top),
        },
    };
}

/// Compilation error report type for user-friendly error messages
pub const Report = CIR.Report;

/// Convert a canonicalization diagnostic to a Report for rendering.
pub fn diagnosticToReport(self: *Self, diagnostic: CIR.Diagnostic, allocator: std.mem.Allocator, filename: []const u8) Allocator.Error!Report {
    return switch (diagnostic) {
        .invalid_num_literal => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            // Extract the literal text from the source
            const literal_text = self.getSource(data.region);

            var report = try Report.init(allocator, "Invalid Number", "", .runtime_error);
            const owned_literal = try report.addOwnedString(literal_text);
            try report.headline.addReflowingText("This number literal is not valid: ");
            try report.headline.addInlineCode(owned_literal);
            try report.headline.addReflowingText(".");

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            try report.document.addLineBreak();
            try report.document.addReflowingText("Check that the number is correctly formatted. Valid examples include: ");
            try report.document.addInlineCode("42");
            try report.document.addReflowingText(", ");
            try report.document.addInlineCode("3.14");
            try report.document.addReflowingText(", ");
            try report.document.addInlineCode("0x1A");
            try report.document.addReflowingText(", or ");
            try report.document.addInlineCode("1_000_000");
            try report.document.addReflowingText(".");

            break :blk report;
        },
        .ident_not_in_scope => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            const ident_name = self.getIdent(data.ident);

            var report = try Report.init(allocator, "Name Not In Scope", "", .runtime_error);
            const owned_ident = try report.addOwnedString(ident_name);
            try report.headline.addReflowingText("Nothing is named ");
            try report.headline.addUnqualifiedSymbol(owned_ident);
            try report.headline.addReflowingText(" in this scope.");
            try report.document.addReflowingText("Is it misspelled, or is there an import missing?");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .read_uninitialized_var => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            const ident_name = self.getIdent(data.ident);

            var report = try Report.init(allocator, "Reading Uninitialized Var", "", .runtime_error);
            const owned_ident = try report.addOwnedString(ident_name);
            try report.headline.addReflowingText("This reads ");
            try report.headline.addUnqualifiedSymbol(owned_ident);
            try report.headline.addReflowingText(" before every path has assigned it a value.");
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .self_referential_definition => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            const ident_name = self.getIdent(data.ident);

            var report = try Report.init(allocator, "Invalid Assignment To Itself", "", .runtime_error);
            const owned_ident = try report.addOwnedString(ident_name);
            try report.headline.addReflowingText("The value ");
            try report.headline.addUnqualifiedSymbol(owned_ident);
            try report.headline.addReflowingText(" is assigned to itself, which would cause an infinite loop at runtime.");
            try report.document.addReflowingText("Only functions can reference themselves (for recursion). For non-function values, the right-hand side must be fully computable without referring to the value being assigned.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .circular_value_definition => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            const ident_name = self.getIdent(data.ident);

            var report = try Report.init(allocator, "Circular Value Definition", "", .runtime_error);
            const owned_ident = try report.addOwnedString(ident_name);
            try report.headline.addReflowingText("The value ");
            try report.headline.addUnqualifiedSymbol(owned_ident);
            try report.headline.addReflowingText(" is part of a recursive non-function definition cycle.");
            try report.document.addReflowingText("Only functions can be recursive. Non-function top-level values must be fully computable without depending on themselves through other values.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .local_reference_before_definition => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            const ident_name = self.getIdent(data.ident);

            var report = try Report.init(allocator, "Used Before Definition", "", .runtime_error);
            const owned_ident = try report.addOwnedString(ident_name);
            try report.headline.addReflowingText("The name ");
            try report.headline.addUnqualifiedSymbol(owned_ident);
            try report.headline.addReflowingText(" is used before it is defined.");
            try report.document.addReflowingText("Local definitions are evaluated in order: a definition can refer to itself or to definitions written before it, but not to definitions written later in the same block. Move ");
            try report.document.addUnqualifiedSymbol(owned_ident);
            try report.document.addReflowingText(" above this use, or move both to the top level.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .mutually_recursive_local_definitions => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            const ident1_name = self.getIdent(data.ident1);
            const ident2_name = self.getIdent(data.ident2);

            var report = try Report.init(allocator, "Mutually Recursive Local Definitions", "", .runtime_error);
            const owned_ident1 = try report.addOwnedString(ident1_name);
            const owned_ident2 = try report.addOwnedString(ident2_name);
            try report.headline.addReflowingText("The local definitions ");
            try report.headline.addUnqualifiedSymbol(owned_ident1);
            try report.headline.addReflowingText(" and ");
            try report.headline.addUnqualifiedSymbol(owned_ident2);
            try report.headline.addReflowingText(" are mutually recursive, which isn't supported for local definitions.");
            try report.document.addReflowingText("Local definitions are evaluated in order and can only refer to themselves or to earlier definitions. Move these mutually recursive definitions to the top level, where mutual recursion is supported.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .erroneous_value_use => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            const ident_name = self.getIdent(data.ident);

            var report = try Report.init(allocator, "Erroneous Value Use", "", .runtime_error);
            const owned_ident = try report.addOwnedString(ident_name);
            try report.headline.addReflowingText("This use of ");
            try report.headline.addUnqualifiedSymbol(owned_ident);
            try report.headline.addReflowingText(" was rewritten to crash because the referenced top-level value failed type checking earlier.");
            try report.document.addReflowingText("Fix the earlier type error instead of trying to execute this value.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .erroneous_value_expr => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Erroneous Value", "This expression was rewritten to crash because it failed type checking.", .runtime_error);
            try report.document.addReflowingText("Fix the earlier type error instead of trying to execute this expression.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .qualified_ident_does_not_exist => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            const ident_name = self.getIdent(data.ident);

            var report = try Report.init(allocator, "Does Not Exist", "", .runtime_error);
            const owned_ident = try report.addOwnedString(ident_name);
            try report.headline.addUnqualifiedSymbol(owned_ident);
            try report.headline.addReflowingText(" does not exist.");
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .exposed_but_not_implemented => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            const ident_name = self.getIdent(data.ident);

            var report = try Report.init(allocator, "Exposed But Not Defined", "", .runtime_error);

            const owned_ident = try report.addOwnedString(ident_name);
            try report.headline.addReflowingText("The module header says that ");
            try report.headline.addUnqualifiedSymbol(owned_ident);
            try report.headline.addReflowingText(" is exposed, but it is not defined anywhere in this module.");

            // Add source context with location
            const owned_filename = try report.addOwnedString(filename);
            try report.addSourceContext(region_info, owned_filename, self.getSourceAll(), self.getLineStartsAll());

            try report.document.addReflowingText("You can fix this by either defining ");
            try report.document.addUnqualifiedSymbol(owned_ident);
            try report.document.addReflowingText(" in this module, or by removing it from the list of exposed values.");

            break :blk report;
        },
        .unused_variable => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            const ident_name = self.getIdent(data.ident);

            var report = try Report.init(allocator, "Unused Variable", "", .warning);
            const owned_ident = try report.addOwnedString(ident_name);
            try report.headline.addReflowingText("Variable ");
            try report.headline.addUnqualifiedSymbol(owned_ident);
            try report.headline.addReflowingText(" is defined here and then never used.");

            try report.document.addReflowingText("If you don't need this variable, prefix it with an underscore like ");
            const ident_with_underscore = try std.fmt.allocPrint(allocator, "_{s}", .{owned_ident});
            defer allocator.free(ident_with_underscore);
            try report.document.addUnqualifiedSymbol(ident_with_underscore);
            try report.document.addReflowingText(" to suppress this warning.");

            try report.document.addLineBreak();

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .underscore_in_type_declaration => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            const kind = if (data.is_alias) "alias" else "opaque type";
            const headline = try std.fmt.allocPrint(allocator, "Underscores are not allowed in type {s} declarations.", .{kind});
            defer allocator.free(headline);
            var report = try Report.init(allocator, "Underscore In Type Alias", headline, .runtime_error);

            // Add source context with location
            const owned_filename = try report.addOwnedString(filename);
            try report.addSourceContext(region_info, owned_filename, self.getSourceAll(), self.getLineStartsAll());

            try report.document.addLineBreak();
            const explanation = try std.fmt.allocPrint(allocator, "Underscores in type annotations mean \"I don't care about this type\", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.", .{});
            defer allocator.free(explanation);
            const owned_explanation = try report.addOwnedString(explanation);
            try report.document.addReflowingText(owned_explanation);

            break :blk report;
        },
        .undeclared_type => |data| blk: {
            const type_name = self.getIdent(data.name);
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Undeclared Type", "", .runtime_error);
            const owned_type_name = try report.addOwnedString(type_name);
            try report.headline.addReflowingText("The type ");
            try report.headline.addInlineCode(owned_type_name);
            try report.headline.addReflowingText(" is not declared in this scope.");
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .type_alias_but_needed_nominal => |data| blk: {
            const type_name = self.getIdent(data.name);
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Expected Nominal Type", "", .runtime_error);
            const owned_type_name = try report.addOwnedString(type_name);
            try report.headline.addReflowingText("You are using the type ");
            try report.headline.addInlineCode(owned_type_name);
            try report.headline.addReflowingText(" like a nominal type, but it is an alias.");
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            try report.document.addLineBreak();
            try report.document.addLineBreak();
            try report.document.addAnnotated("Hint:", .emphasized);
            try report.document.addReflowingText(" You can declare this type with ");
            try report.document.addInlineCode(":=");
            try report.document.addReflowingText(" to make it nominal.");

            break :blk report;
        },
        .type_redeclared => |data| blk: {
            const type_name = self.getIdent(data.name);
            const original_region_info = self.calcRegionInfo(data.original_region);
            const redeclared_region_info = self.calcRegionInfo(data.redeclared_region);

            var report = try Report.init(allocator, "Type Redeclared", "", .runtime_error);
            const owned_type_name = try report.addOwnedString(type_name);
            try report.headline.addReflowingText("The type ");
            try report.headline.addInlineCode(owned_type_name);
            try report.headline.addReflowingText(" is being redeclared.");

            // Show where the redeclaration is
            try report.document.addReflowingText("The redeclaration is here:");
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                redeclared_region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            try report.document.addLineBreak();
            try report.document.addReflowingText("But ");
            try report.document.addType(owned_type_name);
            try report.document.addReflowingText(" was already declared here:");
            try report.document.addLineBreak();
            try report.document.addSourceRegion(
                original_region_info,
                .dimmed,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .type_alias_redeclared => |data| blk: {
            const type_name = self.getIdent(data.name);
            const original_region_info = self.calcRegionInfo(data.original_region);
            const redeclared_region_info = self.calcRegionInfo(data.redeclared_region);

            var report = try Report.init(allocator, "Type Alias Redeclared", "", .runtime_error);
            const owned_type_name = try report.addOwnedString(type_name);
            try report.headline.addReflowingText("The type alias ");
            try report.headline.addInlineCode(owned_type_name);
            try report.headline.addReflowingText(" is being redeclared.");

            try report.document.addReflowingText("The redeclaration is here:");
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                redeclared_region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            try report.document.addLineBreak();
            try report.document.addReflowingText("But ");
            try report.document.addType(owned_type_name);
            try report.document.addReflowingText(" was already declared here:");
            try report.document.addLineBreak();
            try report.document.addSourceRegion(
                original_region_info,
                .dimmed,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .nominal_type_redeclared => |data| blk: {
            const type_name = self.getIdent(data.name);
            const original_region_info = self.calcRegionInfo(data.original_region);
            const redeclared_region_info = self.calcRegionInfo(data.redeclared_region);

            var report = try Report.init(allocator, "Nominal Type Redeclared", "", .runtime_error);
            const owned_type_name = try report.addOwnedString(type_name);
            try report.headline.addReflowingText("The nominal type ");
            try report.headline.addInlineCode(owned_type_name);
            try report.headline.addReflowingText(" is being redeclared.");

            try report.document.addReflowingText("The redeclaration is here:");
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                redeclared_region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            try report.document.addLineBreak();
            try report.document.addReflowingText("But ");
            try report.document.addType(owned_type_name);
            try report.document.addReflowingText(" was already declared here:");
            try report.document.addLineBreak();
            try report.document.addSourceRegion(
                original_region_info,
                .dimmed,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .invalid_top_level_statement => |data| blk: {
            const stmt_name = self.getString(data.stmt);
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Invalid Statement", "", .runtime_error);
            const owned_stmt = try report.addOwnedString(stmt_name);
            try report.headline.addReflowingText("The statement ");
            try report.headline.addInlineCode(owned_stmt);
            try report.headline.addReflowingText(" is not allowed at the top level.");
            try report.document.addReflowingText("Only definitions, type annotations, and imports are allowed at the top level.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .used_underscore_variable => |data| blk: {
            const ident_name = self.getIdent(data.ident);
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Underscore Variable Used", "", .warning);
            const owned_ident = try report.addOwnedString(ident_name);
            try report.headline.addReflowingText("Variable ");
            try report.headline.addUnqualifiedSymbol(owned_ident);
            try report.headline.addReflowingText(" is prefixed with an underscore but is actually used.");

            try report.document.addReflowingText("Variables prefixed with ");
            try report.document.addUnqualifiedSymbol("_");
            try report.document.addReflowingText(" are intended to be unused. Remove the underscore prefix: ");

            // Create the suggested name without underscore
            const suggested_name = ident_name[1..]; // Remove first character (_)
            const owned_suggested = try report.addOwnedString(suggested_name);
            try report.document.addUnqualifiedSymbol(owned_suggested);
            try report.document.addReflowingText(".");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .warning_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .expr_not_canonicalized => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Unrecognized Syntax", "I don't recognize this syntax.", .runtime_error);

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            try report.document.addLineBreak();
            try report.document.addReflowingText("This might be a syntax error, an unsupported language feature, or a typo.");

            break :blk report;
        },
        .crash_expects_string => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Crash Expects String", "", .runtime_error);
            try report.headline.addReflowingText("The ");
            try report.headline.addAnnotated("crash", .inline_code);
            try report.headline.addReflowingText(" keyword expects a string literal as its argument.");
            try report.document.addReflowingText("For example: ");
            try report.document.addAnnotated("crash \"Something went wrong\"", .inline_code);
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .duplicate_record_field => |data| blk: {
            const field_name = self.getIdent(data.field_name);
            const duplicate_region_info = self.calcRegionInfo(data.duplicate_region);
            const original_region_info = self.calcRegionInfo(data.original_region);

            var report = try Report.init(allocator, "Duplicate Record Field", "", .runtime_error);
            const owned_field_name = try report.addOwnedString(field_name);
            try report.headline.addReflowingText("The record field ");
            try report.headline.addRecordField(owned_field_name);
            try report.headline.addReflowingText(" appears more than once in this record.");

            // Show where the duplicate field is
            try report.document.addReflowingText("This field is duplicated here:");
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                duplicate_region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            try report.document.addLineBreak();
            try report.document.addReflowingText("The field ");
            try report.document.addRecordField(owned_field_name);
            try report.document.addReflowingText(" was first defined here:");
            try report.document.addLineBreak();
            try report.document.addSourceRegion(
                original_region_info,
                .dimmed,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            try report.document.addLineBreak();
            try report.document.addReflowingText("Record fields must have unique names. Consider renaming one of these fields or removing the duplicate.");

            break :blk report;
        },
        .duplicate_tag => |data| blk: {
            const tag_name = self.getIdent(data.tag_name);
            const duplicate_region_info = self.calcRegionInfo(data.duplicate_region);
            const original_region_info = self.calcRegionInfo(data.original_region);

            break :blk try CIR.Diagnostic.buildDuplicateTagReport(
                allocator,
                tag_name,
                duplicate_region_info,
                original_region_info,
                filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );
        },
        .redundant_exposed => |data| blk: {
            const ident_name = self.getIdent(data.ident);
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Redundant Exposed", "", .warning);
            const owned_ident = try report.addOwnedString(ident_name);
            try report.headline.addReflowingText("The identifier ");
            try report.headline.addUnqualifiedSymbol(owned_ident);
            try report.headline.addReflowingText(" is exposed multiple times in the module header.");
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            try report.document.addReflowingText("You can remove the duplicate entry to fix this warning.");

            break :blk report;
        },
        .undeclared_type_var => |data| blk: {
            const type_var_name = self.getIdent(data.name);
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Undeclared Type Variable", "", .runtime_error);
            const owned_type_var_name = try report.addOwnedString(type_var_name);
            try report.headline.addReflowingText("The type variable ");
            try report.headline.addInlineCode(owned_type_var_name);
            try report.headline.addReflowingText(" is not declared in this scope.");
            try report.document.addReflowingText("Type variables must be introduced in a type annotation before they can be used.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .not_implemented => |data| blk: {
            const feature = self.getString(data.feature);
            var report = try Report.init(allocator, "Not Implemented", "", .fatal);
            const owned_feature = try report.addOwnedString(feature);
            try report.headline.addReflowingText("This feature is not yet implemented: ");
            try report.headline.addAnnotatedText(owned_feature, .emphasized);
            try report.headline.addReflowingText(".");
            const owned_filename = try report.addOwnedString(filename);
            const region_info = self.calcRegionInfo(data.region);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );
            try report.document.addLineBreak();
            try report.document.addReflowingText("This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!");
            try report.document.addLineBreak();
            break :blk report;
        },
        .malformed_type_annotation => |data| blk: {
            var report = try Report.init(allocator, "Malformed Type", "This type annotation is malformed or contains invalid syntax.", .runtime_error);

            const owned_filename = try report.addOwnedString(filename);
            const region_info = self.calcRegionInfo(data.region);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .if_condition_not_canonicalized => blk: {
            var report = try Report.init(allocator, "Invalid If Condition", "", .runtime_error);
            try report.headline.addReflowingText("The condition in this ");
            try report.headline.addKeyword("if");
            try report.headline.addReflowingText(" expression could not be processed.");
            try report.document.addReflowingText("The condition must be a valid expression that evaluates to a ");
            try report.document.addKeyword("Bool");
            try report.document.addReflowingText(" value (");
            try report.document.addKeyword("Bool.true");
            try report.document.addReflowingText(" or ");
            try report.document.addKeyword("Bool.false");
            try report.document.addReflowingText(").");
            break :blk report;
        },
        .if_then_not_canonicalized => blk: {
            var report = try Report.init(allocator, "Invalid If Branch", "", .runtime_error);
            try report.headline.addReflowingText("The branch in this ");
            try report.headline.addKeyword("if");
            try report.headline.addReflowingText(" expression could not be processed.");
            try report.document.addReflowingText("The branch must contain a valid expression. Check for syntax errors or missing values.");
            break :blk report;
        },
        .if_else_not_canonicalized => blk: {
            var report = try Report.init(allocator, "Invalid If Branch", "", .runtime_error);
            try report.headline.addReflowingText("The ");
            try report.headline.addKeyword("else");
            try report.headline.addReflowingText(" branch of this ");
            try report.headline.addKeyword("if");
            try report.headline.addReflowingText(" expression could not be processed.");
            try report.document.addReflowingText("The ");
            try report.document.addKeyword("else");
            try report.document.addReflowingText(" branch must contain a valid expression. Check for syntax errors or missing values.");
            try report.document.addLineBreak();
            break :blk report;
        },
        .if_expr_without_else => blk: {
            var report = try Report.init(allocator, "If Expression Without Else", "", .runtime_error);
            try report.headline.addReflowingText("This ");
            try report.headline.addKeyword("if");
            try report.headline.addReflowingText(" has no ");
            try report.headline.addKeyword("else");
            try report.headline.addReflowingText(" branch, but it's being used as an expression (assigned to a variable, passed to a function, etc.).");
            try report.document.addReflowingText("You can only use ");
            try report.document.addKeyword("if");
            try report.document.addReflowingText(" without ");
            try report.document.addKeyword("else");
            try report.document.addReflowingText(" when it's a statement. When ");
            try report.document.addKeyword("if");
            try report.document.addReflowingText(" is used as an expression that evaluates to a value, ");
            try report.document.addKeyword("else");
            try report.document.addReflowingText(" is required because otherwise there wouldn't always be a value available.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            try report.document.addReflowingText("Either add an ");
            try report.document.addKeyword("else");
            try report.document.addReflowingText(" branch, or use this ");
            try report.document.addKeyword("if");
            try report.document.addReflowingText(" as a standalone statement.");
            break :blk report;
        },
        .pattern_not_canonicalized => blk: {
            const report = try Report.init(allocator, "Invalid Pattern", "This pattern contains invalid syntax or uses unsupported features.", .runtime_error);
            break :blk report;
        },
        .pattern_arg_invalid => blk: {
            const report = try Report.init(allocator, "Invalid Pattern Argument", "Pattern arguments must be valid patterns like identifiers, literals, or destructuring patterns.", .runtime_error);
            break :blk report;
        },
        .unreachable_string_pattern_capture => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Unreachable Pattern Capture", "This string pattern capture is directly after another capture, so it is unreachable.", .warning);
            try report.document.addReflowingText("String pattern captures need literal text between them. Add a delimiter between the captures, or remove this capture.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .warning_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .shadowing_warning => |data| blk: {
            const ident_name = self.getIdent(data.ident);
            const new_region_info = self.calcRegionInfo(data.region);
            const original_region_info = self.calcRegionInfo(data.original_region);

            var report = try Report.init(allocator, "Duplicate Definition", "", .warning);
            const owned_ident = try report.addOwnedString(ident_name);
            try report.headline.addReflowingText("The name ");
            try report.headline.addUnqualifiedSymbol(owned_ident);
            try report.headline.addReflowingText(" is being redeclared here.");

            // The main box shows the new declaration; point below it at the original.
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                new_region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            try report.document.addLineBreak();
            try report.document.addReflowingText("In this scope, ");
            try report.document.addUnqualifiedSymbol(owned_ident);
            try report.document.addReflowingText(" was already defined here:");
            try report.document.addLineBreak();
            try report.document.addSourceRegion(
                original_region_info,
                .dimmed,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .empty_tuple => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Empty Tuple Not Allowed", "I am part way through parsing this tuple, but it is empty.", .runtime_error);
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );
            try report.document.addLineBreak();
            try report.document.addReflowingText("If you want to represent nothing, try using an empty record: ");
            try report.document.addAnnotated("{}", .inline_code);
            try report.document.addReflowingText(".");

            break :blk report;
        },
        .lambda_body_not_canonicalized => blk: {
            const report = try Report.init(allocator, "Invalid Lambda", "The body of this lambda expression is not valid.", .runtime_error);

            break :blk report;
        },
        .malformed_where_clause => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Malformed Where Clause", "This where clause could not be parsed correctly.", .runtime_error);
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );
            try report.document.addLineBreak();
            try report.document.addReflowingText("Check the syntax of your where clause.");

            break :blk report;
        },
        .var_across_function_boundary => blk: {
            var report = try Report.init(allocator, "Var Reassignment Error", "", .runtime_error);
            try report.headline.addReflowingText("Cannot reassign a ");
            try report.headline.addKeyword("var");
            try report.headline.addReflowingText(" from outside the function where it was declared.");
            try report.document.addReflowingText("Variables declared with ");
            try report.document.addKeyword("var");
            try report.document.addReflowingText(" can only be reassigned within the same function scope.");

            break :blk report;
        },
        .tuple_elem_not_canonicalized => blk: {
            const report = try Report.init(allocator, "Invalid Tuple Element", "This tuple element is malformed or contains invalid syntax.", .runtime_error);

            break :blk report;
        },
        .f64_pattern_literal => |data| blk: {
            // Extract the literal text from the source
            const literal_text = self.getSource(data.region);

            var report = try Report.init(allocator, "F64 Not Allowed In Pattern", "", .runtime_error);
            const owned_literal = try report.addOwnedString(literal_text);
            try report.headline.addText("This floating-point literal cannot be used in a pattern match: ");
            try report.headline.addInlineCode(owned_literal);
            try report.headline.addReflowingText(".");

            try report.document.addReflowingText("This number exceeds the precision range of Roc's ");
            try report.document.addInlineCode("Dec");
            try report.document.addReflowingText(" type and would require F64 representation. ");
            try report.document.addReflowingText("Floating-point numbers (F64) cannot be used in patterns because they don't have reliable equality comparison.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addText("Consider one of these alternatives:");
            try report.document.addLineBreak();
            try report.document.addText("• Use a guard condition with a range check");
            try report.document.addLineBreak();
            try report.document.addText("• Use a smaller number that fits in Dec's precision");
            try report.document.addLineBreak();
            try report.document.addText("• Restructure your code to avoid pattern matching on this value");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addText("For example, instead of:");
            try report.document.addLineBreak();
            try report.document.addInlineCode("1e100 => ...");
            try report.document.addLineBreak();
            try report.document.addText("Use a guard:");
            try report.document.addLineBreak();
            try report.document.addInlineCode("n if n > 1e99 => ...");

            break :blk report;
        },
        .type_not_exposed => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            const type_name_bytes = self.getIdent(data.type_name);
            const module_name_bytes = self.getIdent(data.module_name);

            var report = try Report.init(allocator, "Type Not Exposed", "", .runtime_error);
            const type_name = try report.addOwnedString(type_name_bytes);
            const module_name = try report.addOwnedString(module_name_bytes);
            try report.headline.addText("The type ");
            try report.headline.addInlineCode(type_name);
            try report.headline.addReflowingText(" is not exposed by the module ");
            try report.headline.addInlineCode(module_name);
            try report.headline.addReflowingText(".");

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .private_type_in_exposed_type => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            const exposed_type_bytes = self.getIdent(data.exposed_type);
            const private_type_bytes = self.getIdent(data.private_type);

            var report = try Report.init(allocator, "Private Type In Exposed Type", "", .warning);
            const exposed_type = try report.addOwnedString(exposed_type_bytes);
            const private_type = try report.addOwnedString(private_type_bytes);
            try report.headline.addReflowingText("The exposed type ");
            try report.headline.addInlineCode(exposed_type);
            try report.headline.addReflowingText(" refers to ");
            try report.headline.addInlineCode(private_type);
            try report.headline.addReflowingText(", but ");
            try report.headline.addInlineCode(private_type);
            try report.headline.addReflowingText(" is private to this module.");

            try report.document.addReflowingText("Other modules can see ");
            try report.document.addType(exposed_type);
            try report.document.addReflowingText("'s public shape, but they cannot name this private type.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .warning_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            try report.document.addLineBreak();
            try report.document.addLineBreak();
            try report.document.addAnnotated("Hint:", .emphasized);
            try report.document.addReflowingText(" Expose the referenced type, make ");
            try report.document.addType(exposed_type);
            try report.document.addReflowingText(" opaque with ");
            try report.document.addInlineCode("::");
            try report.document.addReflowingText(", or move the type into ");
            try report.document.addType(exposed_type);
            try report.document.addReflowingText("'s associated block.");

            break :blk report;
        },
        .private_type_in_exposed_field => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            const exposed_type_bytes = self.getIdent(data.exposed_type);
            const field_name_bytes = self.getIdent(data.field_name);
            const private_type_bytes = self.getIdent(data.private_type);

            var report = try Report.init(allocator, "Private Type In Exposed Field", "", .warning);
            const exposed_type = try report.addOwnedString(exposed_type_bytes);
            const field_name = try report.addOwnedString(field_name_bytes);
            const private_type = try report.addOwnedString(private_type_bytes);
            try report.headline.addReflowingText("The ");
            try report.headline.addUnqualifiedSymbol(field_name);
            try report.headline.addReflowingText(" field of ");
            try report.headline.addInlineCode(exposed_type);
            try report.headline.addReflowingText(" refers to ");
            try report.headline.addInlineCode(private_type);
            try report.headline.addReflowingText(", but ");
            try report.headline.addInlineCode(private_type);
            try report.headline.addReflowingText(" is private to this module.");

            try report.document.addReflowingText("Other modules can see this field because ");
            try report.document.addType(exposed_type);
            try report.document.addReflowingText(" is exposed and not opaque, but they cannot name this private type.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .warning_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            try report.document.addLineBreak();
            try report.document.addLineBreak();
            try report.document.addAnnotated("Hint:", .emphasized);
            try report.document.addReflowingText(" Expose the referenced type, make ");
            try report.document.addType(exposed_type);
            try report.document.addReflowingText(" opaque with ");
            try report.document.addInlineCode("::");
            try report.document.addReflowingText(", or move the type into ");
            try report.document.addType(exposed_type);
            try report.document.addReflowingText("'s associated block.");

            break :blk report;
        },
        .type_from_missing_module => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            const type_name_bytes = self.getIdent(data.type_name);
            const module_name_bytes = self.getIdent(data.module_name);

            var report = try Report.init(allocator, "Module Not Found", "", .runtime_error);
            const type_name = try report.addOwnedString(type_name_bytes);
            const module_name = try report.addOwnedString(module_name_bytes);
            try report.headline.addText("This ");
            try report.headline.addInlineCode(type_name);
            try report.headline.addReflowingText(" type is declared to be in ");
            try report.headline.addInlineCode(module_name);
            try report.headline.addReflowingText(", which does not exist.");

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .value_not_exposed => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Value Not Exposed", "", .runtime_error);
            try report.headline.addText("The value ");
            try report.headline.addInlineCode(self.getIdent(data.value_name));
            try report.headline.addReflowingText(" is not exposed by the module ");
            try report.headline.addInlineCode(self.getIdent(data.module_name));
            try report.headline.addReflowingText(".");

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .file_import_not_found => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            const path_text = self.common.getString(data.path);
            break :blk try CIR.Diagnostic.buildFileImportNotFoundReport(
                allocator,
                path_text,
                region_info,
                filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );
        },
        .file_import_io_error => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            const path_text = self.common.getString(data.path);
            break :blk try CIR.Diagnostic.buildFileImportIOErrorReport(
                allocator,
                path_text,
                region_info,
                filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );
        },
        .file_import_absolute_path => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            const path_text = self.common.getString(data.path);
            break :blk try CIR.Diagnostic.buildFileImportAbsolutePathReport(
                allocator,
                path_text,
                region_info,
                filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );
        },
        .file_import_not_utf8 => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            const path_text = self.common.getString(data.path);
            break :blk try CIR.Diagnostic.buildFileImportNotUtf8Report(
                allocator,
                path_text,
                region_info,
                filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );
        },
        .module_not_found => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            const module_name_bytes = self.getIdent(data.module_name);

            var report = try Report.init(allocator, "Module Not Found", "", .runtime_error);
            const module_name = try report.addOwnedString(module_name_bytes);
            try report.headline.addText("The module ");
            try report.headline.addInlineCode(module_name);
            try report.headline.addReflowingText(" was not found in this Roc project.");

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .module_not_imported => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            const module_name_bytes = self.getIdent(data.module_name);

            var report = try Report.init(allocator, "Module Not Imported", "", .runtime_error);
            const module_name = try report.addOwnedString(module_name_bytes);
            try report.headline.addText("There is no module with the name ");
            try report.headline.addInlineCode(module_name);
            try report.headline.addReflowingText(" imported into this Roc file.");

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .nested_type_not_found => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            const parent_bytes = self.getIdent(data.parent_name);
            const nested_bytes = self.getIdent(data.nested_name);

            var report = try Report.init(allocator, "Missing Nested Type", "", .runtime_error);
            const parent_name = try report.addOwnedString(parent_bytes);
            const nested_name = try report.addOwnedString(nested_bytes);

            try report.headline.addInlineCode(parent_name);
            try report.headline.addReflowingText(" is in scope, but it doesn't have a nested type ");

            if (std.mem.eql(u8, parent_bytes, nested_bytes)) {
                // Say "also named" if the parent and nested types are equal, e.g. `Foo.Foo` - when
                // this happens it can be kind of a confusing message if the message just says
                // "Foo is in scope, but it doesn't have a nested type named Foo" compared to
                // "Foo is in scope, but it doesn't have a nested type that's also named Foo"
                try report.headline.addReflowingText("that's also ");
            }

            try report.headline.addReflowingText("named ");
            try report.headline.addInlineCode(nested_name);
            try report.headline.addReflowingText(".");

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .nested_value_not_found => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            const parent_bytes = self.getIdent(data.parent_name);
            const nested_bytes = self.getIdent(data.nested_name);

            var report = try Report.init(allocator, "Does Not Exist", "", .runtime_error);

            const parent_name = try report.addOwnedString(parent_bytes);
            const nested_name = try report.addOwnedString(nested_bytes);

            // First line: "Foo.bar does not exist."
            const full_name = try std.fmt.allocPrint(allocator, "{s}.{s}", .{ parent_bytes, nested_bytes });
            defer allocator.free(full_name);
            const owned_full_name = try report.addOwnedString(full_name);
            try report.headline.addInlineCode(owned_full_name);
            try report.headline.addReflowingText(" does not exist.");

            // Second line: "Foo is in scope, but it has no associated bar."
            try report.document.addInlineCode(parent_name);
            try report.document.addReflowingText(" is in scope, but it has no associated ");
            try report.document.addInlineCode(nested_name);
            try report.document.addReflowingText(".");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .record_builder_map2_not_found => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            const type_bytes = self.getIdent(data.type_name);

            var report = try Report.init(allocator, "Record Builder Not Supported", "", .runtime_error);
            const type_name = try report.addOwnedString(type_bytes);

            // "The type `Foo` is used in a record builder expression, but does not implement `map2`:"
            try report.headline.addReflowingText("The type ");
            try report.headline.addInlineCode(type_name);
            try report.headline.addReflowingText(" is used in a record builder expression, but does not implement ");
            try report.headline.addInlineCode("map2");
            try report.headline.addReflowingText(".");

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );
            try report.document.addLineBreak();

            // Hint
            try report.document.addReflowingText("Hint: To use ");
            try report.document.addInlineCode(type_name);
            try report.document.addReflowingText(" as a record builder, add a ");
            try report.document.addInlineCode("map2");
            try report.document.addReflowingText(" method to its type module.");

            break :blk report;
        },
        .too_many_exports => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            const count_text = try std.fmt.allocPrint(allocator, "{d}", .{data.count});
            defer allocator.free(count_text);

            var report = try Report.init(allocator, "Too Many Exports", "", .runtime_error);
            const owned_count = try report.addOwnedString(count_text);

            try report.headline.addReflowingText("This module exposes ");
            try report.headline.addInlineCode(owned_count);
            try report.headline.addReflowingText(" values, which exceeds the compiler limit.");

            try report.document.addReflowingText("The export list starts here:");
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .where_clause_not_allowed_in_type_decl => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Where Clause Not Allowed In Type Declaration", "", .runtime_error);
            try report.headline.addText("You cannot define a ");
            try report.headline.addInlineCode("where");
            try report.headline.addReflowingText(" clause inside a type declaration.");

            try report.document.addReflowingText("You're attempting do this here:");
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .open_ext_not_allowed_in_type_decl => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Open Ext Not Allowed In Type Declaration", "", .runtime_error);
            try report.headline.addText("You cannot use a ");
            try report.headline.addInlineCode("..");
            try report.headline.addReflowingText(" inside a type declaration.");

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            try report.document.addLineBreak();
            try report.document.addLineBreak();
            try report.document.addAnnotated("Hint:", .emphasized);
            try report.document.addReflowingText(" You need a named variable, like ");
            try report.document.addInlineCode("..others");
            try report.document.addReflowingText(", to use this here.");

            break :blk report;
        },
        .unnamed_field_not_allowed_in_structural_record => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Unnamed Field Not Allowed In Structural Record", "", .runtime_error);
            try report.headline.addReflowingText("Unnamed fields (written ");
            try report.headline.addInlineCode("_");
            try report.headline.addReflowingText(" or ");
            try report.headline.addInlineCode("_name");
            try report.headline.addReflowingText(") are only allowed in nominal record type declarations, not in structural record types.");

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            try report.document.addLineBreak();
            try report.document.addLineBreak();
            try report.document.addAnnotated("Hint:", .emphasized);
            try report.document.addReflowingText(" Unnamed fields reserve layout padding for a nominal type (declared with ");
            try report.document.addInlineCode(":=");
            try report.document.addReflowingText("). Give the field a name, or move it into a nominal type declaration.");

            break :blk report;
        },
        .type_module_missing_matching_type => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Type Module Missing Matching Type", "Type modules must have a nominal type declaration matching the module name.", .runtime_error);

            const module_name_bytes = self.getIdent(data.module_name);
            const module_name = try report.addOwnedString(module_name_bytes);

            try report.document.addText("This file is named ");
            try report.document.addInlineCode(module_name);
            try report.document.addReflowingText(".roc, but no top-level nominal type named ");
            try report.document.addInlineCode(module_name);
            try report.document.addReflowingText(" was found.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addReflowingText("Add a nominal type like:");
            try report.document.addLineBreak();
            const nominal_msg = try std.fmt.allocPrint(allocator, "{s} := ...", .{module_name_bytes});
            defer allocator.free(nominal_msg);
            const owned_nominal = try report.addOwnedString(nominal_msg);
            try report.document.addInlineCode(owned_nominal);
            try report.document.addLineBreak();
            try report.document.addReflowingText("or:");
            try report.document.addLineBreak();
            const opaque_msg = try std.fmt.allocPrint(allocator, "{s} :: ...", .{module_name_bytes});
            defer allocator.free(opaque_msg);
            const owned_opaque = try report.addOwnedString(opaque_msg);
            try report.document.addInlineCode(owned_opaque);
            try report.document.addReflowingText(" (opaque nominal type)");
            try report.document.addLineBreak();

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .type_module_has_alias_not_nominal => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            const module_name_bytes = self.getIdent(data.module_name);

            var report = try Report.init(allocator, "Type Module Requires Nominal Type", "", .runtime_error);
            const module_name = try report.addOwnedString(module_name_bytes);
            try report.headline.addText("This file is named ");
            try report.headline.addInlineCode(module_name);
            try report.headline.addText(".roc, and contains a type alias ");
            try report.headline.addInlineCode(module_name);
            try report.headline.addReflowingText(".");

            try report.document.addReflowingText("Type modules must use nominal types (");
            try report.document.addInlineCode(":=");
            try report.document.addReflowingText(" or ");
            try report.document.addInlineCode("::");
            try report.document.addReflowingText("), not type aliases (");
            try report.document.addInlineCode(":");
            try report.document.addReflowingText(").");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addReflowingText("Nominal types must be records or tag unions:");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addReflowingText("# Record example:");
            try report.document.addLineBreak();
            const record_example = try std.fmt.allocPrint(allocator, "{s} := {{ data: List(U8) }}.{{}}", .{module_name_bytes});
            defer allocator.free(record_example);
            const owned_record = try report.addOwnedString(record_example);
            try report.document.addInlineCode(owned_record);
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addReflowingText("# Tag union example:");
            try report.document.addLineBreak();
            const tag_example = try std.fmt.allocPrint(allocator, "{s} := [ State(List(U8)) ].{{}}", .{module_name_bytes});
            defer allocator.free(tag_example);
            const owned_tag = try report.addOwnedString(tag_example);
            try report.document.addInlineCode(owned_tag);
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addReflowingText("Tip: Nominal types have their own identity and can have associated functions. Type aliases (");
            try report.document.addInlineCode(":");
            try report.document.addReflowingText(") are just shorthand for another type and cannot define modules.");
            try report.document.addLineBreak();

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .default_app_missing_main => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Missing `main!` Function", "", .runtime_error);
            try report.headline.addReflowingText("Default app modules must have a ");
            try report.headline.addInlineCode("main!");
            try report.headline.addReflowingText(" function.");

            try report.document.addText("No ");
            try report.document.addInlineCode("main!");
            try report.document.addReflowingText(" function was found.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addReflowingText("Add a main! function like:");
            try report.document.addLineBreak();
            try report.document.addInlineCode("main! = |arg| { ... }");
            try report.document.addLineBreak();

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .default_app_wrong_arity => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "`main!` Should Take 1 Argument", "", .runtime_error);
            try report.headline.addInlineCode("main!");
            try report.headline.addReflowingText(" is defined but has the wrong number of arguments. ");
            try report.headline.addInlineCode("main!");
            try report.headline.addReflowingText(" should take 1 argument.");

            const arity_msg = try std.fmt.allocPrint(allocator, "{d}", .{data.arity});
            defer allocator.free(arity_msg);
            const owned_arity = try report.addOwnedString(arity_msg);
            try report.document.addText("Found ");
            try report.document.addInlineCode(owned_arity);
            try report.document.addReflowingText(" arguments.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addReflowingText("Change it to:");
            try report.document.addLineBreak();
            try report.document.addInlineCode("main! = |arg| { ... }");
            try report.document.addLineBreak();

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .cannot_import_default_app => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Cannot Import Default App", "You cannot import a default app module.", .runtime_error);

            const module_name_bytes = self.getIdent(data.module_name);
            const module_name = try report.addOwnedString(module_name_bytes);

            try report.document.addText("The module ");
            try report.document.addInlineCode(module_name);
            try report.document.addReflowingText(" is a default app module and cannot be imported.");
            try report.document.addLineBreak();

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .execution_requires_app_or_default_app => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Execution Requires App Or Default App", "This file cannot be executed because it is not an app or default-app module.", .runtime_error);

            try report.document.addReflowingText("Add either:");
            try report.document.addLineBreak();
            try report.document.addInlineCode("app");
            try report.document.addReflowingText(" header at the top of the file");
            try report.document.addLineBreak();
            try report.document.addReflowingText("or:");
            try report.document.addLineBreak();
            try report.document.addReflowingText("a ");
            try report.document.addInlineCode("main!");
            try report.document.addReflowingText(" function with 1 argument (for default-app)");
            try report.document.addLineBreak();

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .type_name_case_mismatch => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Type Name Case Mismatch", "Type module name must match the type declaration.", .runtime_error);

            const module_name_bytes = self.getIdent(data.module_name);
            const module_name = try report.addOwnedString(module_name_bytes);
            const type_name_bytes = self.getIdent(data.type_name);
            const type_name = try report.addOwnedString(type_name_bytes);

            try report.document.addText("This file is named ");
            try report.document.addInlineCode(module_name);
            try report.document.addReflowingText(".roc, but the type is named ");
            try report.document.addInlineCode(type_name);
            try report.document.addReflowingText(".");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addReflowingText("Make sure the type name matches the filename exactly (case-sensitive).");
            try report.document.addLineBreak();

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .module_header_deprecated => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Module Header Deprecated", "", .warning);
            try report.headline.addReflowingText("The ");
            try report.headline.addInlineCode("module");
            try report.headline.addReflowingText(" header is deprecated.");

            try report.document.addReflowingText("Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addReflowingText("Remove the ");
            try report.document.addInlineCode("module");
            try report.document.addReflowingText(" header and ensure your file defines a type that matches the filename.");
            try report.document.addLineBreak();

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .warning_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .redundant_expose_main_type => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            const type_name_bytes = self.getIdent(data.type_name);
            const module_name_bytes = self.getIdent(data.module_name);

            var report = try Report.init(allocator, "Redundant Expose", "", .warning);
            const type_name = try report.addOwnedString(type_name_bytes);
            const module_name = try report.addOwnedString(module_name_bytes);
            try report.headline.addReflowingText("Redundantly exposing ");
            try report.headline.addInlineCode(type_name);
            try report.headline.addReflowingText(" when importing ");
            try report.headline.addInlineCode(module_name);
            try report.headline.addReflowingText(".");

            try report.document.addReflowingText("The type ");
            try report.document.addInlineCode(type_name);
            try report.document.addReflowingText(" is automatically exposed when importing a type module.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addReflowingText("Remove ");
            try report.document.addInlineCode(type_name);
            try report.document.addReflowingText(" from the exposing clause.");
            try report.document.addLineBreak();

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .warning_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .invalid_main_type_rename_in_exposing => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            const type_name_bytes = self.getIdent(data.type_name);
            const alias_bytes = self.getIdent(data.alias);

            var report = try Report.init(allocator, "Invalid Type Rename", "", .runtime_error);
            const type_name = try report.addOwnedString(type_name_bytes);
            const alias = try report.addOwnedString(alias_bytes);
            try report.headline.addReflowingText("Cannot rename ");
            try report.headline.addInlineCode(type_name);
            try report.headline.addReflowingText(" to ");
            try report.headline.addInlineCode(alias);
            try report.headline.addReflowingText(" in the exposing clause.");

            try report.document.addReflowingText("To rename both the module and its main type, use ");
            try report.document.addInlineCode("as");
            try report.document.addReflowingText(" at the module level:");
            try report.document.addLineBreak();

            const example_msg = try std.fmt.allocPrint(allocator, "import ModuleName as {s}", .{alias_bytes});
            defer allocator.free(example_msg);
            const owned_example = try report.addOwnedString(example_msg);
            try report.document.addInlineCode(owned_example);
            try report.document.addLineBreak();

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .ident_already_in_scope => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            const ident_name = self.getIdent(data.ident);

            var report = try Report.init(allocator, "Shadowing", "", .runtime_error);
            const owned_ident = try report.addOwnedString(ident_name);
            try report.headline.addReflowingText("The name ");
            try report.headline.addUnqualifiedSymbol(owned_ident);
            try report.headline.addReflowingText(" is already defined in this scope.");
            try report.document.addReflowingText("Choose a different name for this identifier.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .break_outside_loop => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Break Outside Loop", "", .runtime_error);
            try report.headline.addReflowingText("The ");
            try report.headline.addAnnotated("break", .inline_code);
            try report.headline.addReflowingText(" statement can only be used inside loops like ");
            try report.headline.addAnnotated("while", .inline_code);
            try report.headline.addReflowingText(" or ");
            try report.headline.addAnnotated("for", .inline_code);
            try report.headline.addReflowingText(" to exit the loop early.");

            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .infinite_loop_never_exits => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Infinite Loop Never Exits", "", .warning);
            try report.headline.addReflowingText("This infinite loop has no ");
            try report.headline.addAnnotated("return", .inline_code);
            try report.headline.addReflowingText(", ");
            try report.headline.addAnnotated("?", .inline_code);
            try report.headline.addReflowingText(", ");
            try report.headline.addAnnotated("crash", .inline_code);
            try report.headline.addReflowingText(", or ");
            try report.headline.addAnnotated("break", .inline_code);
            try report.headline.addReflowingText(" that exits this loop, so it will run forever and hang the program.");

            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .return_outside_fn => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = switch (data.context) {
                .try_suffix => r: {
                    var r = try Report.init(allocator, "Try Operator Outside Function", "", .runtime_error);
                    try r.headline.addReflowingText("The ");
                    try r.headline.addAnnotated("?", .inline_code);
                    try r.headline.addReflowingText(" operator can only be used inside function bodies because it can cause an early return.");
                    break :r r;
                },
                .return_statement, .return_expr => r: {
                    var r = try Report.init(allocator, "Return Outside Function", "", .runtime_error);
                    try r.headline.addReflowingText("The ");
                    try r.headline.addAnnotated("return", .inline_code);
                    try r.headline.addReflowingText(" keyword can only be used inside function bodies.");
                    break :r r;
                },
            };

            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .mutually_recursive_type_aliases => |data| blk: {
            const type_name = self.getIdent(data.name);
            const other_type_name = self.getIdent(data.other_name);
            const region_info = self.calcRegionInfo(data.region);
            const other_region_info = self.calcRegionInfo(data.other_region);

            var report = try Report.init(allocator, "Mutually Recursive Type Aliases", "", .runtime_error);
            const owned_type_name = try report.addOwnedString(type_name);
            const owned_other_name = try report.addOwnedString(other_type_name);
            try report.headline.addReflowingText("The type alias ");
            try report.headline.addInlineCode(owned_type_name);
            try report.headline.addReflowingText(" and ");
            try report.headline.addInlineCode(owned_other_name);
            try report.headline.addReflowingText(" form a recursive cycle.");

            try report.document.addReflowingText("Type aliases are transparent synonyms and cannot be mutually recursive. ");
            try report.document.addReflowingText("If you need recursive types, use nominal types (");
            try report.document.addAnnotated(":=", .inline_code);
            try report.document.addReflowingText(") instead.");
            try report.document.addLineBreak();
            try report.document.addLineBreak();

            try report.document.addReflowingText("This type is declared here:");
            try report.document.addLineBreak();
            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            try report.document.addLineBreak();
            try report.document.addReflowingText("And it references ");
            try report.document.addType(owned_other_name);
            try report.document.addReflowingText(" declared here:");
            try report.document.addLineBreak();
            try report.document.addSourceRegion(
                other_region_info,
                .dimmed,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .deprecated_number_suffix => |data| blk: {
            const suffix = self.getString(data.suffix);
            const suggested = self.getString(data.suggested);
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Deprecated Number Suffix", "This number literal uses a deprecated suffix syntax.", .runtime_error);
            const owned_suffix = try report.addOwnedString(suffix);
            const owned_suggested = try report.addOwnedString(suggested);

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            try report.document.addLineBreak();
            try report.document.addReflowingText("The ");
            try report.document.addInlineCode(owned_suffix);
            try report.document.addReflowingText(" suffix is no longer supported. Use ");
            try report.document.addInlineCode(owned_suggested);
            try report.document.addReflowingText(" instead.");

            break :blk report;
        },
        .range_op_chained => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Chained Range", "", .runtime_error);
            try report.headline.addReflowingText("Range operators can't be chained. Write a single range instead, like ");
            try report.headline.addInlineCode("a..<b");
            try report.headline.addReflowingText(" or ");
            try report.headline.addInlineCode("a..=b");
            try report.headline.addReflowingText(".");

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            break :blk report;
        },
        .type_shadowed_warning => |data| blk: {
            const new_region_info = self.calcRegionInfo(data.region);
            const original_region_info = self.calcRegionInfo(data.original_region);
            break :blk try CIR.Diagnostic.buildTypeShadowedWarningReport(
                allocator,
                self.getIdent(data.name),
                new_region_info,
                original_region_info,
                data.cross_scope,
                filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );
        },
        else => std.debug.panic("Unhandled canonicalize diagnostic in diagnosticToReport: {s}", .{@tagName(diagnostic)}),
    };
}

/// Get region info for a given region
pub fn getRegionInfo(self: *const Self, region: Region) error{ BeginTooLarge, EndTooLarge, InvalidPosition, NoLineStarts, OutOfOrder }!RegionInfo {
    return self.common.getRegionInfo(region);
}

/// Returns diagnostic position information for the given region.
/// This is a standalone utility function that takes the source text as a parameter
/// to avoid storing it in the cacheable IR structure.
pub fn calcRegionInfo(self: *const Self, region: Region) RegionInfo {
    return self.common.calcRegionInfo(region);
}

/// Extract a literal from source code between given byte offsets
pub fn literal_from_source(self: *const Self, start_offset: u32, end_offset: u32) []const u8 {
    return self.common.source[start_offset..end_offset];
}

/// Get the source line for a given region
pub fn getSourceLine(self: *const Self, region: Region) error{ BeginTooLarge, EndTooLarge, InvalidPosition, NoLineStarts, OutOfOrder }![]const u8 {
    return self.common.getSourceLine(region);
}

/// Serialized representation of ModuleEnv.
/// Uses extern struct to guarantee consistent field layout across optimization levels.
pub const Serialized = extern struct {
    // Field order must match the runtime ModuleEnv struct exactly for in-place deserialization
    gpa: [2]u64, // Reserve space for allocator (vtable ptr + context ptr), provided during deserialization
    common: CommonEnv.Serialized,
    types: TypeStore.Serialized,
    module_kind: ModuleKind.Serialized,
    module_role: ModuleRole,
    all_defs: CIR.Def.Span,
    global_value_defs: CIR.Def.Span,
    all_statements: CIR.Statement.Span,
    type_decls: CIR.Statement.Span,
    forward_type_decls: CIR.Statement.Span,
    exports: CIR.Def.Span,
    requires_types: RequiredType.SafeList.Serialized,
    for_clause_aliases: ForClauseAlias.SafeList.Serialized,
    provides_entries: ProvidesEntry.SafeList.Serialized,
    hosted_entries: HostedEntry.SafeList.Serialized,
    builtin_statements: CIR.Statement.Span,
    external_decls: CIR.ExternalDecl.SafeList.Serialized,
    imports: CIR.Import.Store.Serialized,
    file_dependencies: FileDependency.SafeList.Serialized,
    module_name: [2]u64, // Reserve space for slice (ptr + len), provided during deserialization
    display_module_name_idx_reserved: u32, // Reserved space for display_module_name_idx field (interned during deserialization)
    qualified_module_ident_reserved: u32, // Reserved space for qualified_module_ident field
    diagnostics: CIR.Diagnostic.Span,
    store: NodeStore.Serialized,
    evaluation_order_reserved: u64, // Reserved space for evaluation_order field (required for in-place deserialization cast)
    runtime_prepared: bool,
    runtime_prepared_padding: [7]u8,
    // Well-known identifier indices (serialized directly, no lookup needed during deserialization)
    idents: CommonIdents,
    import_mapping_reserved: [6]u64, // Reserved space for import_mapping (AutoHashMap is ~40 bytes), initialized at runtime
    method_idents: MethodIdents.Serialized,
    method_defs: MethodDefs.Serialized,
    for_loop_dispatch_plans: ForLoopDispatchPlan.SafeList.Serialized,
    numeral_digit_bytes: collections.SafeList(u8).Serialized,
    numeral_literals: NumeralLiteral.SafeList.Serialized,
    numeral_dispatch_plans: NumeralDispatchPlan.SafeList.Serialized,
    quote_dispatch_plans: NumeralDispatchPlan.SafeList.Serialized,
    numeric_suffix_targets: NumericSuffixTarget.SafeList.Serialized,
    // Reserved space (was is_lambda_lifted and is_defunctionalized, now unused)
    _reserved_flags: [2]u8 = .{ 0, 0 },
    _padding: [6]u8 = .{ 0, 0, 0, 0, 0, 0 },

    /// Serialize a ModuleEnv into this Serialized struct, appending data to the writer
    pub fn serialize(
        self: *Serialized,
        env: *const Self,
        allocator: std.mem.Allocator,
        writer: *CompactWriter,
    ) Allocator.Error!void {
        try self.common.serialize(&env.common, allocator, writer);
        try self.types.serialize(&env.types, allocator, writer);

        // Copy simple values directly
        self.module_kind = ModuleKind.Serialized.encode(env.module_kind);
        self.module_role = env.module_role;
        self.all_defs = env.all_defs;
        self.global_value_defs = env.global_value_defs;
        self.all_statements = env.all_statements;
        self.type_decls = env.type_decls;
        self.forward_type_decls = env.forward_type_decls;
        self.exports = env.exports;
        self.builtin_statements = env.builtin_statements;

        try self.requires_types.serialize(&env.requires_types, allocator, writer);
        try self.for_clause_aliases.serialize(&env.for_clause_aliases, allocator, writer);
        try self.provides_entries.serialize(&env.provides_entries, allocator, writer);
        try self.hosted_entries.serialize(&env.hosted_entries, allocator, writer);
        try self.external_decls.serialize(&env.external_decls, allocator, writer);
        try self.imports.serialize(&env.imports, allocator, writer);
        try self.file_dependencies.serialize(&env.file_dependencies, allocator, writer);

        self.diagnostics = env.diagnostics;

        // Serialize NodeStore
        try self.store.serialize(&env.store, allocator, writer);

        // Set gpa, module_name, evaluation_order_reserved to zeros;
        // these are runtime-only and will be set during deserialization.
        // Preserve display_module_name_idx since the ident store is also serialized and indices remain valid.
        self.gpa = .{ 0, 0 };
        self.module_name = .{ 0, 0 };
        self.display_module_name_idx_reserved = @bitCast(env.display_module_name_idx);
        self.qualified_module_ident_reserved = @bitCast(env.qualified_module_ident);
        self.evaluation_order_reserved = 0;
        self.runtime_prepared = env.module_role == .builtin and env.runtime_prepared;
        self.runtime_prepared_padding = .{ 0, 0, 0, 0, 0, 0, 0 };
        // Serialize well-known identifier indices directly (no lookup needed during deserialization)
        self.idents = env.idents;
        // import_mapping is runtime-only and initialized fresh during deserialization
        self.import_mapping_reserved = .{ 0, 0, 0, 0, 0, 0 };
        if (builtin.mode == .Debug) {
            std.debug.assert(env.method_idents.sorted);
            std.debug.assert(env.method_idents.deduplicated);
            std.debug.assert(env.method_defs.sorted);
            std.debug.assert(env.method_defs.deduplicated);
        }
        try self.method_idents.serialize(&env.method_idents, allocator, writer);
        try self.method_defs.serialize(&env.method_defs, allocator, writer);
        try self.for_loop_dispatch_plans.serialize(&env.for_loop_dispatch_plans, allocator, writer);
        try self.numeral_digit_bytes.serialize(&env.numeral_digit_bytes, allocator, writer);
        try self.numeral_literals.serialize(&env.numeral_literals, allocator, writer);
        try self.numeral_dispatch_plans.serialize(&env.numeral_dispatch_plans, allocator, writer);
        try self.quote_dispatch_plans.serialize(&env.quote_dispatch_plans, allocator, writer);
        try self.numeric_suffix_targets.serialize(&env.numeric_suffix_targets, allocator, writer);

        self._reserved_flags = .{ 0, 0 };
    }

    /// Deserialize into a freshly allocated ModuleEnv (no in-place modification of cache buffer).
    /// The base_addr parameter is the base address of the serialized buffer in memory.
    /// WARNING: The returned ModuleEnv has data pointing into the cache buffer (read-only).
    /// Use deserializeWithMutableTypes() if types/store need to be mutable.
    pub fn deserializeInto(
        self: *const Serialized,
        base_addr: usize,
        gpa: std.mem.Allocator,
        source: []const u8,
        module_name: []const u8,
    ) std.mem.Allocator.Error!*Self {
        // Allocate a fresh ModuleEnv on the heap
        const env = try gpa.create(Self);
        errdefer gpa.destroy(env);

        env.* = Self{
            .gpa = gpa,
            .common = self.common.deserializeInto(base_addr, source),
            .types = self.types.deserializeInto(base_addr, gpa),
            .module_kind = self.module_kind.decode(),
            .module_role = self.module_role,
            .all_defs = self.all_defs,
            .global_value_defs = self.global_value_defs,
            .all_statements = self.all_statements,
            .type_decls = self.type_decls,
            .forward_type_decls = self.forward_type_decls,
            .exports = self.exports,
            .requires_types = self.requires_types.deserializeInto(base_addr),
            .for_clause_aliases = self.for_clause_aliases.deserializeInto(base_addr),
            .provides_entries = self.provides_entries.deserializeInto(base_addr),
            .hosted_entries = self.hosted_entries.deserializeInto(base_addr),
            .builtin_statements = self.builtin_statements,
            .external_decls = self.external_decls.deserializeInto(base_addr),
            .imports = try self.imports.deserializeInto(base_addr, gpa),
            .file_dependencies = self.file_dependencies.deserializeInto(base_addr),
            .module_name = module_name,
            .display_module_name_idx = @bitCast(self.display_module_name_idx_reserved),
            .qualified_module_ident = @bitCast(self.qualified_module_ident_reserved),
            .diagnostics = self.diagnostics,
            .store = self.store.deserializeInto(base_addr, gpa),
            .evaluation_order = null, // Not serialized, will be recomputed if needed
            .runtime_prepared = self.runtime_prepared,
            .idents = self.idents,
            .import_mapping = types_mod.import_mapping.ImportMapping.init(gpa),
            .method_idents = self.method_idents.deserializeInto(base_addr),
            .method_defs = self.method_defs.deserializeInto(base_addr),
            .for_loop_dispatch_plans = self.for_loop_dispatch_plans.deserializeInto(base_addr),
            .numeral_digit_bytes = self.numeral_digit_bytes.deserializeInto(base_addr),
            .numeral_literals = self.numeral_literals.deserializeInto(base_addr),
            .numeral_dispatch_plans = self.numeral_dispatch_plans.deserializeInto(base_addr),
            .quote_dispatch_plans = self.quote_dispatch_plans.deserializeInto(base_addr),
            .numeric_suffix_targets = self.numeric_suffix_targets.deserializeInto(base_addr),
        };

        return env;
    }

    /// Materialize a non-owning view over statically embedded serialized bytes.
    /// This is for the compiler's baked Builtin module: every list/slice points
    /// into the executable's aligned static data and no backing bytes are copied.
    pub fn viewStatic(
        self: *const Serialized,
        base_addr: usize,
        gpa: std.mem.Allocator,
        source: []const u8,
        module_name: []const u8,
    ) error{CorruptSerializedModuleEnv}!Self {
        if (self.imports.imports.len != 0) return error.CorruptSerializedModuleEnv;
        if (self.imports.import_idents.len != 0) return error.CorruptSerializedModuleEnv;
        if (self.imports.resolved_modules.len != 0) return error.CorruptSerializedModuleEnv;

        return Self{
            .gpa = gpa,
            .common = self.common.deserializeInto(base_addr, source),
            .types = self.types.deserializeInto(base_addr, gpa),
            .module_kind = self.module_kind.decode(),
            .module_role = self.module_role,
            .all_defs = self.all_defs,
            .global_value_defs = self.global_value_defs,
            .all_statements = self.all_statements,
            .type_decls = self.type_decls,
            .forward_type_decls = self.forward_type_decls,
            .exports = self.exports,
            .requires_types = self.requires_types.deserializeInto(base_addr),
            .for_clause_aliases = self.for_clause_aliases.deserializeInto(base_addr),
            .provides_entries = self.provides_entries.deserializeInto(base_addr),
            .hosted_entries = self.hosted_entries.deserializeInto(base_addr),
            .builtin_statements = self.builtin_statements,
            .external_decls = self.external_decls.deserializeInto(base_addr),
            .imports = CIR.Import.Store.init(),
            .file_dependencies = self.file_dependencies.deserializeInto(base_addr),
            .module_name = module_name,
            .display_module_name_idx = @bitCast(self.display_module_name_idx_reserved),
            .qualified_module_ident = @bitCast(self.qualified_module_ident_reserved),
            .diagnostics = self.diagnostics,
            .store = self.store.deserializeInto(base_addr, gpa),
            .evaluation_order = null,
            .runtime_prepared = self.runtime_prepared,
            .idents = self.idents,
            .import_mapping = types_mod.import_mapping.ImportMapping.init(gpa),
            .method_idents = self.method_idents.deserializeInto(base_addr),
            .method_defs = self.method_defs.deserializeInto(base_addr),
            .for_loop_dispatch_plans = self.for_loop_dispatch_plans.deserializeInto(base_addr),
            .numeral_digit_bytes = self.numeral_digit_bytes.deserializeInto(base_addr),
            .numeral_literals = self.numeral_literals.deserializeInto(base_addr),
            .numeral_dispatch_plans = self.numeral_dispatch_plans.deserializeInto(base_addr),
            .quote_dispatch_plans = self.quote_dispatch_plans.deserializeInto(base_addr),
            .numeric_suffix_targets = self.numeric_suffix_targets.deserializeInto(base_addr),
        };
    }

    /// Deserialize with mutable type store and node store for cache modules.
    /// Allocates fresh memory for the type store and node store arrays,
    /// allowing them to be mutated (e.g., during type checking).
    /// Use this for disk cache modules that may need to add new types.
    pub fn deserializeWithMutableTypes(
        self: *const Serialized,
        base_addr: usize,
        gpa: std.mem.Allocator,
        source: []const u8,
        module_name: []const u8,
    ) std.mem.Allocator.Error!*Self {
        // Allocate a fresh ModuleEnv on the heap
        const env = try gpa.create(Self);
        errdefer gpa.destroy(env);

        env.* = Self{
            .gpa = gpa,
            .common = self.common.deserializeInto(base_addr, source),
            // Use deserializeWithCopy to get mutable type store
            .types = try self.types.deserializeWithCopy(base_addr, gpa),
            .module_kind = self.module_kind.decode(),
            .module_role = self.module_role,
            .all_defs = self.all_defs,
            .global_value_defs = self.global_value_defs,
            .all_statements = self.all_statements,
            .type_decls = self.type_decls,
            .forward_type_decls = self.forward_type_decls,
            .exports = self.exports,
            .requires_types = self.requires_types.deserializeInto(base_addr),
            .for_clause_aliases = self.for_clause_aliases.deserializeInto(base_addr),
            .provides_entries = self.provides_entries.deserializeInto(base_addr),
            .hosted_entries = self.hosted_entries.deserializeInto(base_addr),
            .builtin_statements = self.builtin_statements,
            .external_decls = self.external_decls.deserializeInto(base_addr),
            .imports = try self.imports.deserializeInto(base_addr, gpa),
            .file_dependencies = self.file_dependencies.deserializeInto(base_addr),
            .module_name = module_name,
            .display_module_name_idx = @bitCast(self.display_module_name_idx_reserved),
            .qualified_module_ident = @bitCast(self.qualified_module_ident_reserved),
            .diagnostics = self.diagnostics,
            // Use deserializeWithCopy for NodeStore so regions can be extended
            .store = try self.store.deserializeWithCopy(base_addr, gpa),
            .evaluation_order = null,
            .runtime_prepared = self.runtime_prepared,
            .idents = self.idents,
            .import_mapping = types_mod.import_mapping.ImportMapping.init(gpa),
            .method_idents = self.method_idents.deserializeInto(base_addr),
            .method_defs = self.method_defs.deserializeInto(base_addr),
            .for_loop_dispatch_plans = try self.for_loop_dispatch_plans.deserializeWithCopy(base_addr, gpa),
            .numeral_digit_bytes = try self.numeral_digit_bytes.deserializeWithCopy(base_addr, gpa),
            .numeral_literals = try self.numeral_literals.deserializeWithCopy(base_addr, gpa),
            .numeral_dispatch_plans = try self.numeral_dispatch_plans.deserializeWithCopy(base_addr, gpa),
            .quote_dispatch_plans = try self.quote_dispatch_plans.deserializeWithCopy(base_addr, gpa),
            .numeric_suffix_targets = try self.numeric_suffix_targets.deserializeWithCopy(base_addr, gpa),
        };

        return env;
    }
};

/// Convert a type into a node index
pub fn nodeIdxFrom(idx: anytype) Node.Idx {
    return @enumFromInt(@intFromEnum(idx));
}

/// Convert a type into a type var
pub fn varFrom(idx: anytype) TypeVar {
    return @enumFromInt(@intFromEnum(idx));
}

/// Record the checked iterator dispatch functions for a semantic `for` loop.
pub fn recordForLoopDispatchPlan(
    self: *Self,
    node_idx: Node.Idx,
    pattern_idx: Node.Idx,
    iterable_idx: Node.Idx,
    iter_fn_var: TypeVar,
    next_fn_var: TypeVar,
) std.mem.Allocator.Error!void {
    const raw_node: u32 = @intFromEnum(node_idx);
    const raw_pattern: u32 = @intFromEnum(pattern_idx);
    const raw_iterable: u32 = @intFromEnum(iterable_idx);
    for (self.for_loop_dispatch_plans.items.items) |*plan| {
        if (plan.node_idx != raw_node) continue;
        plan.* = .{
            .node_idx = raw_node,
            .pattern_idx = raw_pattern,
            .iterable_idx = raw_iterable,
            .iter_fn_var = @intFromEnum(iter_fn_var),
            .next_fn_var = @intFromEnum(next_fn_var),
        };
        return;
    }
    _ = try self.for_loop_dispatch_plans.append(self.gpa, .{
        .node_idx = raw_node,
        .pattern_idx = raw_pattern,
        .iterable_idx = raw_iterable,
        .iter_fn_var = @intFromEnum(iter_fn_var),
        .next_fn_var = @intFromEnum(next_fn_var),
    });
}

/// Return the checked iterator dispatch functions for a semantic `for` loop node.
pub fn forLoopDispatchPlanForNode(self: *const Self, node_idx: Node.Idx) ?ForLoopDispatchPlan {
    const raw_node: u32 = @intFromEnum(node_idx);
    for (self.for_loop_dispatch_plans.items.items) |plan| {
        if (plan.node_idx == raw_node) return plan;
    }
    return null;
}

/// Record exact base-256 digits for a numeric source node.
pub fn recordNumeralLiteral(
    self: *Self,
    node_idx: Node.Idx,
    before: []const u8,
    after: []const u8,
    after_decimal_digit_count: u32,
    is_negative: bool,
    is_fractional: bool,
    had_decimal_point: bool,
) std.mem.Allocator.Error!void {
    const raw_node: u32 = @intFromEnum(node_idx);
    const digits_start: u32 = @intCast(self.numeral_digit_bytes.len());
    _ = try self.numeral_digit_bytes.appendSlice(self.gpa, before);
    _ = try self.numeral_digit_bytes.appendSlice(self.gpa, after);

    const literal = NumeralLiteral{
        .node_idx = raw_node,
        .digits_start = digits_start,
        .before_len = @intCast(before.len),
        .after_len = @intCast(after.len),
        .after_decimal_digit_count = after_decimal_digit_count,
        .flags = (if (is_negative) NumeralLiteral.negative_flag else 0) |
            (if (is_fractional) NumeralLiteral.fractional_flag else 0) |
            (if (had_decimal_point) NumeralLiteral.decimal_point_flag else 0),
    };
    for (self.numeral_literals.items.items) |*existing| {
        if (existing.node_idx == raw_node) {
            existing.* = literal;
            return;
        }
    }
    _ = try self.numeral_literals.append(self.gpa, literal);
}

/// Return exact base-256 digits for a numeric source node.
pub fn numeralLiteralForNode(self: *const Self, node_idx: Node.Idx) ?NumeralLiteral {
    const raw_node: u32 = @intFromEnum(node_idx);
    for (self.numeral_literals.items.items) |literal| {
        if (literal.node_idx == raw_node) return literal;
    }
    return null;
}

/// Return the digits before the decimal point for a recorded numeral.
pub fn numeralDigitsBefore(self: *const Self, literal: NumeralLiteral) []const u8 {
    return self.numeral_digit_bytes.items.items[literal.digits_start..][0..literal.before_len];
}

/// Return the digits after the decimal point for a recorded numeral.
pub fn numeralDigitsAfter(self: *const Self, literal: NumeralLiteral) []const u8 {
    const start = literal.digits_start + literal.before_len;
    return self.numeral_digit_bytes.items.items[start..][0..literal.after_len];
}

/// Record the checked `from_numeral` function for a numeric expression.
pub fn recordNumeralDispatchPlan(
    self: *Self,
    node_idx: Node.Idx,
    target_var: TypeVar,
    fn_var: TypeVar,
) std.mem.Allocator.Error!void {
    const raw_node: u32 = @intFromEnum(node_idx);
    for (self.numeral_dispatch_plans.items.items) |*plan| {
        if (plan.node_idx != raw_node) continue;
        plan.* = .{
            .node_idx = raw_node,
            .target_var = @intFromEnum(target_var),
            .fn_var = @intFromEnum(fn_var),
        };
        return;
    }
    _ = try self.numeral_dispatch_plans.append(self.gpa, .{
        .node_idx = raw_node,
        .target_var = @intFromEnum(target_var),
        .fn_var = @intFromEnum(fn_var),
    });
}

/// Return the checked `from_numeral` function for a numeric expression.
pub fn numeralDispatchPlanForNode(self: *const Self, node_idx: Node.Idx) ?NumeralDispatchPlan {
    const raw_node: u32 = @intFromEnum(node_idx);
    for (self.numeral_dispatch_plans.items.items) |plan| {
        if (plan.node_idx == raw_node) return plan;
    }
    return null;
}

/// Record the checked `from_quote` function for a string literal node.
pub fn recordQuoteDispatchPlan(
    self: *Self,
    node_idx: Node.Idx,
    target_var: TypeVar,
    fn_var: TypeVar,
) std.mem.Allocator.Error!void {
    const raw_node: u32 = @intFromEnum(node_idx);
    for (self.quote_dispatch_plans.items.items) |*plan| {
        if (plan.node_idx != raw_node) continue;
        plan.* = .{
            .node_idx = raw_node,
            .target_var = @intFromEnum(target_var),
            .fn_var = @intFromEnum(fn_var),
        };
        return;
    }
    _ = try self.quote_dispatch_plans.append(self.gpa, .{
        .node_idx = raw_node,
        .target_var = @intFromEnum(target_var),
        .fn_var = @intFromEnum(fn_var),
    });
}

/// Return the checked `from_quote` function for a string literal node.
pub fn quoteDispatchPlanForNode(self: *const Self, node_idx: Node.Idx) ?NumeralDispatchPlan {
    const raw_node: u32 = @intFromEnum(node_idx);
    for (self.quote_dispatch_plans.items.items) |plan| {
        if (plan.node_idx == raw_node) return plan;
    }
    return null;
}

/// Record the scope-resolved type target for an explicit numeric suffix.
pub fn recordNumericSuffixTarget(
    self: *Self,
    node_idx: Node.Idx,
    target: NumericSuffixTarget.Target,
) std.mem.Allocator.Error!void {
    const raw_node: u32 = @intFromEnum(node_idx);
    const suffix_target = switch (target) {
        .builtin => |num_kind| NumericSuffixTarget{
            .node_idx = raw_node,
            .kind = @intFromEnum(NumericSuffixTarget.Kind.builtin),
            .data1 = @intFromEnum(num_kind),
            .data2 = 0,
        },
        .local => |stmt_idx| NumericSuffixTarget{
            .node_idx = raw_node,
            .kind = @intFromEnum(NumericSuffixTarget.Kind.local),
            .data1 = @intFromEnum(stmt_idx),
            .data2 = 0,
        },
        .external => |external| NumericSuffixTarget{
            .node_idx = raw_node,
            .kind = @intFromEnum(NumericSuffixTarget.Kind.external),
            .data1 = @intFromEnum(external.import_idx),
            .data2 = external.target_node_idx,
        },
        .invalid => NumericSuffixTarget{
            .node_idx = raw_node,
            .kind = @intFromEnum(NumericSuffixTarget.Kind.invalid),
            .data1 = 0,
            .data2 = 0,
        },
    };

    for (self.numeric_suffix_targets.items.items) |*existing| {
        if (existing.node_idx != raw_node) continue;
        existing.* = suffix_target;
        return;
    }
    _ = try self.numeric_suffix_targets.append(self.gpa, suffix_target);
}

/// Return the scope-resolved type target for an explicit numeric suffix.
pub fn numericSuffixTargetForNode(self: *const Self, node_idx: Node.Idx) ?NumericSuffixTarget {
    const raw_node: u32 = @intFromEnum(node_idx);
    for (self.numeric_suffix_targets.items.items) |suffix_target| {
        if (suffix_target.node_idx == raw_node) return suffix_target;
    }
    return null;
}

/// Adds an identifier to the list of exposed items by its identifier index.
pub fn addExposedById(self: *Self, ident_idx: Ident.Idx) Allocator.Error!void {
    return try self.common.exposed_items.addExposedById(self.gpa, @bitCast(ident_idx));
}

/// Associates a value definition node index with an exposed identifier.
pub fn setExposedValueNodeIndexById(self: *Self, ident_idx: Ident.Idx, node_idx: u32) Allocator.Error!void {
    return try self.common.setValueNodeIndexById(self.gpa, ident_idx, node_idx);
}

/// Associates a type declaration node index with an exposed identifier.
pub fn setExposedTypeNodeIndexById(self: *Self, ident_idx: Ident.Idx, node_idx: u32) Allocator.Error!void {
    return try self.common.setTypeNodeIndexById(self.gpa, ident_idx, node_idx);
}

/// Retrieves the value definition node index associated with an exposed identifier, if any.
pub fn getExposedValueNodeIndexById(self: *const Self, ident_idx: Ident.Idx) ?u32 {
    return self.common.getValueNodeIndexById(self.gpa, ident_idx);
}

/// Retrieves the type declaration node index associated with an exposed identifier, if any.
pub fn getExposedTypeNodeIndexById(self: *const Self, ident_idx: Ident.Idx) ?u32 {
    return self.common.getTypeNodeIndexById(self.gpa, ident_idx);
}

/// Retrieves the explicit exposure target associated with an exposed identifier, if any.
pub fn getExposedTargetById(self: *const Self, ident_idx: Ident.Idx) ?collections.ExposedItemTarget {
    return self.common.getExposedTargetById(self.gpa, ident_idx);
}

/// Get the exposed node index for a type given its statement index.
/// This is used for auto-imported builtin types where we have the statement index pre-computed.
/// For auto-imported types, the statement index IS the node/var index directly.
pub fn getExposedNodeIndexByStatementIdx(_: *const Self, stmt_idx: CIR.Statement.Idx) ?u32 {

    // For auto-imported builtin types (Bool, Try, etc.), the statement index
    // IS the node/var index. This is because type declarations get type variables
    // indexed by their statement index, not by their position in arrays.
    return @intFromEnum(stmt_idx);
}

/// Ensures that the exposed items are sorted by identifier index.
pub fn ensureExposedSorted(self: *Self, allocator: std.mem.Allocator) void {
    self.common.exposed_items.ensureSorted(allocator);
}

/// Checks whether the given identifier is exposed by this module.
pub fn containsExposedById(self: *const Self, ident_idx: Ident.Idx) bool {
    return self.common.exposed_items.containsById(self.gpa, @bitCast(ident_idx));
}

/// Assert that nodes and regions are in sync
pub inline fn debugAssertArraysInSync(self: *const Self) void {
    if (builtin.mode == .Debug) {
        const cir_nodes = self.store.nodes.items.len;
        const region_nodes = self.store.regions.len();

        if (!(cir_nodes == region_nodes)) {
            std.debug.panic(
                "Arrays out of sync:\n  cir_nodes={}\n  region_nodes={}\n",
                .{ cir_nodes, region_nodes },
            );
        }
    }
}

/// Add a new expression to the node store.
/// This function asserts that the nodes and regions are in sync.
pub fn addDef(self: *Self, expr: CIR.Def, region: Region) std.mem.Allocator.Error!CIR.Def.Idx {
    const expr_idx = try self.store.addDef(expr, region);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new type header to the node store.
/// This function asserts that the nodes and regions are in sync.
pub fn addTypeHeader(self: *Self, expr: CIR.TypeHeader, region: Region) std.mem.Allocator.Error!CIR.TypeHeader.Idx {
    const expr_idx = try self.store.addTypeHeader(expr, region);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new statement to the node store.
/// This function asserts that the nodes and regions are in sync.
pub fn addStatement(self: *Self, expr: CIR.Statement, region: Region) std.mem.Allocator.Error!CIR.Statement.Idx {
    const expr_idx = try self.store.addStatement(expr, region);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new pattern to the node store.
/// This function asserts that the nodes and regions are in sync.
pub fn addPattern(self: *Self, expr: CIR.Pattern, region: Region) std.mem.Allocator.Error!CIR.Pattern.Idx {
    const expr_idx = try self.store.addPattern(expr, region);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression to the node store.
/// This function asserts that the nodes and regions are in sync.
pub fn addExpr(self: *Self, expr: CIR.Expr, region: Region) std.mem.Allocator.Error!CIR.Expr.Idx {
    const expr_idx = try self.store.addExpr(expr, region);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new capture to the node store.
/// This function asserts that the nodes and regions are in sync.
pub fn addCapture(self: *Self, capture: CIR.Expr.Capture, region: Region) std.mem.Allocator.Error!CIR.Expr.Capture.Idx {
    const capture_idx = try self.store.addCapture(capture, region);
    self.debugAssertArraysInSync();
    return capture_idx;
}

/// Add a new record field to the node store.
/// This function asserts that the nodes and regions are in sync.
pub fn addRecordField(self: *Self, expr: CIR.RecordField, region: Region) std.mem.Allocator.Error!CIR.RecordField.Idx {
    const expr_idx = try self.store.addRecordField(expr, region);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new record destructuring to the node store.
/// This function asserts that the nodes and regions are in sync.
pub fn addRecordDestruct(self: *Self, expr: CIR.Pattern.RecordDestruct, region: Region) std.mem.Allocator.Error!CIR.Pattern.RecordDestruct.Idx {
    const expr_idx = try self.store.addRecordDestruct(expr, region);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Adds a new if branch to the store.
/// This function asserts that the nodes and regions are in sync.
pub fn addIfBranch(self: *Self, expr: CIR.Expr.IfBranch, region: Region) std.mem.Allocator.Error!CIR.Expr.IfBranch.Idx {
    const expr_idx = try self.store.addIfBranch(expr, region);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new match branch to the node store.
/// This function asserts that the nodes and regions are in sync.
pub fn addMatchBranch(self: *Self, expr: CIR.Expr.Match.Branch, region: Region) std.mem.Allocator.Error!CIR.Expr.Match.Branch.Idx {
    const expr_idx = try self.store.addMatchBranch(expr, region);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new where clause to the node store.
/// This function asserts that the nodes and regions are in sync.
pub fn addWhereClause(self: *Self, expr: CIR.WhereClause, region: Region) std.mem.Allocator.Error!CIR.WhereClause.Idx {
    const expr_idx = try self.store.addWhereClause(expr, region);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new type annotation to the node store.
/// This function asserts that the nodes and regions are in sync.
pub fn addTypeAnno(self: *Self, expr: CIR.TypeAnno, region: Region) std.mem.Allocator.Error!CIR.TypeAnno.Idx {
    const expr_idx = try self.store.addTypeAnno(expr, region);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new annotation to the node store.
/// This function asserts that the nodes and regions are in sync.
pub fn addAnnotation(self: *Self, expr: CIR.Annotation, region: Region) std.mem.Allocator.Error!CIR.Annotation.Idx {
    const expr_idx = try self.store.addAnnotation(expr, region);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new record field to the node store.
/// This function asserts that the nodes and regions are in sync.
pub fn addAnnoRecordField(self: *Self, expr: CIR.TypeAnno.RecordField, region: Region) std.mem.Allocator.Error!CIR.TypeAnno.RecordField.Idx {
    const expr_idx = try self.store.addAnnoRecordField(expr, region);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new exposed item to the node store.
/// This function asserts that the nodes and regions are in sync.
pub fn addExposedItem(self: *Self, expr: CIR.ExposedItem, region: Region) std.mem.Allocator.Error!CIR.ExposedItem.Idx {
    const expr_idx = try self.store.addExposedItem(expr, region);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a diagnostic.
/// This function asserts that the nodes and regions are in sync.
pub fn addDiagnostic(self: *Self, reason: CIR.Diagnostic) std.mem.Allocator.Error!CIR.Diagnostic.Idx {
    const expr_idx = try self.store.addDiagnostic(reason);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new malformed node to the node store.
/// This function asserts that the nodes and regions are in sync.
pub fn addMalformed(self: *Self, diagnostic_idx: CIR.Diagnostic.Idx, region: Region) std.mem.Allocator.Error!CIR.Node.Idx {
    const malformed_idx = try self.store.addMalformed(diagnostic_idx, region);
    self.debugAssertArraysInSync();
    return malformed_idx;
}

/// Add a new match branch pattern to the node store.
/// This function asserts that the nodes and regions are in sync.
pub fn addMatchBranchPattern(self: *Self, expr: CIR.Expr.Match.BranchPattern, region: Region) std.mem.Allocator.Error!CIR.Expr.Match.BranchPattern.Idx {
    const expr_idx = try self.store.addMatchBranchPattern(expr, region);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new type variable to the node store.
/// This function asserts that the nodes and regions are in sync.
pub fn addTypeSlot(
    self: *Self,
    parent_node: CIR.Node.Idx,
    region: Region,
    comptime RetIdx: type,
) std.mem.Allocator.Error!RetIdx {
    comptime if (!isCastable(RetIdx)) @compileError("Idx type " ++ @typeName(RetIdx) ++ " is not castable");
    const node_idx = try self.store.addTypeVarSlot(parent_node, region);
    self.debugAssertArraysInSync();
    return @enumFromInt(@intFromEnum(node_idx));
}

/// Adds an external declaration and returns its index
pub fn pushExternalDecl(self: *Self, decl: CIR.ExternalDecl) std.mem.Allocator.Error!CIR.ExternalDecl.Idx {
    const idx = @as(u32, @intCast(self.external_decls.len()));
    _ = try self.external_decls.append(self.gpa, decl);
    return @enumFromInt(idx);
}

/// Retrieves an external declaration by its index
pub fn getExternalDecl(self: *const Self, idx: CIR.ExternalDecl.Idx) *const CIR.ExternalDecl {
    return self.external_decls.get(@as(CIR.ExternalDecl.SafeList.Idx, @enumFromInt(@intFromEnum(idx))));
}

/// Adds multiple external declarations and returns a span
pub fn pushExternalDecls(self: *Self, decls: []const CIR.ExternalDecl) std.mem.Allocator.Error!CIR.ExternalDecl.Span {
    const start = @as(u32, @intCast(self.external_decls.len()));
    for (decls) |decl| {
        _ = try self.external_decls.append(self.gpa, decl);
    }
    return CIR.ExternalDecl.Span{ .span = .{ .start = start, .len = @as(u32, @intCast(decls.len)) } };
}

/// Gets a slice of external declarations from a span
pub fn sliceExternalDecls(self: *const Self, span: CIR.ExternalDecl.Span) []const CIR.ExternalDecl {
    const range = CIR.ExternalDecl.SafeList.Range{ .start = @enumFromInt(span.span.start), .count = span.span.len };
    return self.external_decls.sliceRange(range);
}

/// Retrieves the text of an identifier by its index
pub fn getIdentText(self: *const Self, idx: Ident.Idx) []const u8 {
    return self.getIdent(idx);
}

/// Builds a mapping from platform for-clause alias ident indices to the
/// equivalent ident indices in the app module's store.
///
/// This encapsulates all cross-module string-based ident resolution so that
/// downstream code (e.g. in src/eval/) only needs to do index lookups via `map.get()`.
pub fn buildPlatformToAppIdentMap(
    self: *const Self,
    gpa: std.mem.Allocator,
    app_env: *const Self,
) std.mem.Allocator.Error!std.AutoHashMap(Ident.Idx, Ident.Idx) {
    var map = std.AutoHashMap(Ident.Idx, Ident.Idx).init(gpa);
    errdefer map.deinit();
    const all_aliases = self.for_clause_aliases.items.items;
    for (self.requires_types.items.items) |required_type| {
        const type_aliases_slice = all_aliases[@intFromEnum(required_type.type_aliases.start)..][0..required_type.type_aliases.count];
        for (type_aliases_slice) |alias| {
            if (app_env.common.findIdentFrom(&self.common, alias.alias_name)) |app_ident| {
                try map.put(alias.alias_name, app_ident);
            }
        }
    }
    return map;
}

/// Helper function to generate the S-expression node for the entire module.
/// If a single expression is provided, only that expression is returned.
pub fn pushToSExprTree(self: *Self, maybe_expr_idx: ?CIR.Expr.Idx, tree: *SExprTree) std.mem.Allocator.Error!void {
    if (maybe_expr_idx) |expr_idx| {
        // Only output the given expression
        try self.store.getExpr(expr_idx).pushToSExprTree(self, tree, expr_idx);
    } else {
        const root_begin = tree.beginNode();
        try tree.pushStaticAtom("can-ir");

        // Iterate over all the definitions in the file and convert each to an S-expression tree
        const defs_slice = self.store.sliceDefs(self.all_defs);
        const statements_slice = self.store.sliceStatements(self.all_statements);

        if (defs_slice.len == 0 and statements_slice.len == 0 and self.external_decls.len() == 0) {
            try tree.pushBoolPair("empty", true);
        }
        const attrs = tree.beginNode();

        for (defs_slice) |def_idx| {
            try self.store.getDef(def_idx).pushToSExprTree(self, tree);
        }

        for (statements_slice) |stmt_idx| {
            try self.store.getStatement(stmt_idx).pushToSExprTree(self, tree, stmt_idx);
        }

        for (0..@intCast(self.external_decls.len())) |i| {
            const external_decl = self.external_decls.get(@enumFromInt(i));
            try external_decl.pushToSExprTree(self, tree);
        }

        try tree.endNode(root_begin, attrs);
    }
}

/// Append region information to an S-expression node for a given index.
pub fn appendRegionInfoToSExprTree(self: *const Self, tree: *SExprTree, idx: anytype) std.mem.Allocator.Error!void {
    const region = self.store.getNodeRegion(@enumFromInt(@intFromEnum(idx)));
    try self.appendRegionInfoToSExprTreeFromRegion(tree, region);
}

/// Append region information to an S-expression node from a specific region.
pub fn appendRegionInfoToSExprTreeFromRegion(self: *const Self, tree: *SExprTree, region: Region) std.mem.Allocator.Error!void {
    const info = self.getRegionInfo(region) catch RegionInfo{
        .start_line_idx = 0,
        .start_col_idx = 0,
        .end_line_idx = 0,
        .end_col_idx = 0,
    };
    try tree.pushBytesRange(
        region.start.offset,
        region.end.offset,
        info,
    );
}

/// Get region information for a node.
pub fn getNodeRegionInfo(self: *const Self, idx: anytype) RegionInfo {
    const region = self.store.getNodeRegion(@enumFromInt(@intFromEnum(idx)));
    return self.getRegionInfo(region);
}

/// Helper function to convert type information to an SExpr node
/// in S-expression format for snapshot testing. Implements the definition-focused
/// format showing final types for defs, expressions, and builtins.
pub fn pushTypesToSExprTree(self: *Self, maybe_expr_idx: ?CIR.Expr.Idx, tree: *SExprTree) (std.mem.Allocator.Error || error{WriteFailed})!void {
    if (maybe_expr_idx) |expr_idx| {
        try self.pushExprTypesToSExprTree(expr_idx, tree);
    } else {
        // Create a TypeWriter to format the type
        var type_writer = try self.initTypeWriter();
        defer type_writer.deinit();

        // Generate full type information for all definitions and expressions
        const root_begin = tree.beginNode();
        try tree.pushStaticAtom("inferred-types");

        const root_attrs = tree.beginNode();

        // Create defs section
        const defs_begin = tree.beginNode();
        try tree.pushStaticAtom("defs");
        const defs_attrs = tree.beginNode();

        // Iterate through all definitions to extract pattern types
        const defs_slice = self.store.sliceDefs(self.all_defs);
        for (defs_slice) |def_idx| {
            const def = self.store.getDef(def_idx);

            // Only process assign patterns - skip destructuring patterns
            const pattern = self.store.getPattern(def.pattern);
            switch (pattern) {
                .assign => {},
                else => continue, // Skip non-assign patterns (like destructuring)
            }

            // Use def_idx for type lookup, not def.pattern. During type checking,
            // def_var and pattern_var are unified, but the type store may not have
            // slots for all pattern indices. Def indices are always within bounds.
            const def_var = varFrom(def_idx);

            // Get the region for this definition
            const pattern_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(def.pattern));
            const pattern_region = self.store.getRegionAt(pattern_node_idx);

            // Write the type to the buffer
            try type_writer.write(def_var, .one_line);

            // Add the pattern type entry
            const patt_begin = tree.beginNode();
            try tree.pushStaticAtom("patt");
            try self.appendRegionInfoToSExprTreeFromRegion(tree, pattern_region);

            const type_str = type_writer.get();
            try tree.pushStringPair("type", type_str);

            try tree.endNode(patt_begin, tree.beginNode());
        }

        try tree.endNode(defs_begin, defs_attrs);

        // Check if we have any type declarations to output
        const all_stmts = self.store.sliceStatements(self.all_statements);
        var has_type_decl = false;
        for (all_stmts) |stmt_idx| {
            const stmt = self.store.getStatement(stmt_idx);
            switch (stmt) {
                .s_alias_decl, .s_nominal_decl => {
                    has_type_decl = true;
                    break;
                },
                else => continue,
            }
        }

        // Create type_decls section if we have any type declarations
        if (has_type_decl) {
            const type_decls_begin = tree.beginNode();
            try tree.pushStaticAtom("type_decls");
            const type_decls_attrs = tree.beginNode();

            for (all_stmts) |stmt_idx| {
                const stmt = self.store.getStatement(stmt_idx);
                switch (stmt) {
                    .s_alias_decl => |alias| {
                        const stmt_begin = tree.beginNode();
                        try tree.pushStaticAtom("alias");

                        // Add region info for the statement
                        const stmt_region = self.store.getStatementRegion(stmt_idx);
                        try self.appendRegionInfoToSExprTreeFromRegion(tree, stmt_region);

                        // Get the type variable for this statement
                        const stmt_var = varFrom(stmt_idx);

                        // Write the type to the buffer
                        try type_writer.write(stmt_var, .one_line);

                        const type_str = type_writer.get();
                        try tree.pushStringPair("type", type_str);

                        const stmt_attrs = tree.beginNode();

                        // Add the type header
                        const header = self.store.getTypeHeader(alias.header);
                        try header.pushToSExprTree(self, tree, alias.header);

                        try tree.endNode(stmt_begin, stmt_attrs);
                    },
                    .s_nominal_decl => |nominal| {
                        const stmt_begin = tree.beginNode();
                        try tree.pushStaticAtom("nominal");

                        // Add region info for the statement
                        const stmt_region = self.store.getStatementRegion(stmt_idx);
                        try self.appendRegionInfoToSExprTreeFromRegion(tree, stmt_region);

                        // Get the type variable for this statement
                        const stmt_var = varFrom(stmt_idx);

                        // Write the type to the buffer
                        try type_writer.write(stmt_var, .one_line);

                        const type_str = type_writer.get();
                        try tree.pushStringPair("type", type_str);

                        const stmt_attrs = tree.beginNode();

                        // Add the type header
                        const header = self.store.getTypeHeader(nominal.header);
                        try header.pushToSExprTree(self, tree, nominal.header);

                        try tree.endNode(stmt_begin, stmt_attrs);
                    },
                    else => continue,
                }
            }

            try tree.endNode(type_decls_begin, type_decls_attrs);
        }

        // Create expressions section
        const exprs_begin = tree.beginNode();
        try tree.pushStaticAtom("expressions");
        const exprs_attrs = tree.beginNode();

        // Iterate through all definitions to extract expression types
        for (defs_slice) |def_idx| {
            const def = self.store.getDef(def_idx);
            const expr_var = varFrom(def.expr);

            // Get the region for this expression
            const expr_node_idx: CIR.Node.Idx = @enumFromInt(@intFromEnum(def.expr));
            const expr_region = self.store.getRegionAt(expr_node_idx);

            // Create a TypeWriter to format the type
            // Write the type to the buffer
            try type_writer.write(expr_var, .one_line);

            // Add the expression type entry
            const expr_begin = tree.beginNode();
            try tree.pushStaticAtom("expr");
            try self.appendRegionInfoToSExprTreeFromRegion(tree, expr_region);

            const type_str = type_writer.get();
            try tree.pushStringPair("type", type_str);

            try tree.endNode(expr_begin, tree.beginNode());
        }

        try tree.endNode(exprs_begin, exprs_attrs);
        try tree.endNode(root_begin, root_attrs);
    }
}

fn pushExprTypesToSExprTree(self: *Self, expr_idx: CIR.Expr.Idx, tree: *SExprTree) (std.mem.Allocator.Error || error{WriteFailed})!void {
    const expr_begin = tree.beginNode();
    try tree.pushStaticAtom("expr");

    // Add region info for the expression
    try self.appendRegionInfoToSExprTree(tree, expr_idx);

    // Get the type variable for this expression
    const expr_var = varFrom(expr_idx);

    // Create a TypeWriter to format the type
    var type_writer = try self.initTypeWriter();
    defer type_writer.deinit();

    // Write the type to the buffer
    try type_writer.write(expr_var, .one_line);

    // Add the formatted type to the S-expression tree
    const type_str = type_writer.get();
    try tree.pushStringPair("type", type_str);

    try tree.endNode(expr_begin, tree.beginNode());
}

/// Retrieves a string literal by its index from the common environment.
pub fn getString(self: *const Self, idx: StringLiteral.Idx) []const u8 {
    return self.common.getString(idx);
}

/// Inserts a string literal into the common environment and returns its index.
pub fn insertString(self: *Self, string: []const u8) std.mem.Allocator.Error!StringLiteral.Idx {
    return try self.common.insertString(self.gpa, string);
}

/// Returns a mutable reference to the identifier store.
pub fn getIdentStore(self: *Self) *Ident.Store {
    return &self.common.idents;
}

/// Returns an immutable reference to the identifier store.
pub fn getIdentStoreConst(self: *const Self) *const Ident.Store {
    return &self.common.idents;
}

/// Retrieves the text of an identifier by its index.
pub fn getIdent(self: *const Self, idx: Ident.Idx) []const u8 {
    return self.common.getIdent(idx);
}

/// Get the source text for a given region
pub fn getSource(self: *const Self, region: Region) []const u8 {
    return self.common.getSource(region);
}

/// Get the entire source text. This is primarily needed for diagnostic output
/// where `addSourceRegion` requires access to the full source and line starts
/// to render error messages with context lines.
///
/// For extracting source text for a specific region, prefer `getSource(region)` instead.
pub fn getSourceAll(self: *const Self) []const u8 {
    return self.common.getSourceAll();
}

/// Get all line start offsets. This is primarily needed for diagnostic output
/// where `addSourceRegion` requires access to the full source and line starts
/// to render error messages with context lines.
pub fn getLineStartsAll(self: *const Self) []const u32 {
    return self.common.getLineStartsAll();
}

pub fn initTypeWriter(self: *Self) std.mem.Allocator.Error!TypeWriter {
    return TypeWriter.initFromParts(self.gpa, &self.types, self.getIdentStore(), null);
}

/// Inserts an identifier into the common environment and returns its index.
pub fn insertIdent(self: *Self, ident: Ident) std.mem.Allocator.Error!Ident.Idx {
    return try self.common.insertIdent(self.gpa, ident);
}

/// Creates and inserts a qualified identifier (e.g., "Foo.bar") into the common environment.
/// This handles the full lifecycle: building the qualified name, creating the Ident,
/// inserting it into the store, and cleaning up any temporary allocations.
/// All memory management is handled internally with no caller obligations.
pub fn insertQualifiedIdent(
    self: *Self,
    parent: []const u8,
    child: []const u8,
) std.mem.Allocator.Error!Ident.Idx {
    const qualified = try std.fmt.allocPrint(self.gpa, "{s}.{s}", .{ parent, child });
    defer self.gpa.free(qualified);
    return try self.insertIdent(Ident.for_text(qualified));
}

/// Registers a method identifier mapping for an explicit owner declaration.
pub fn registerMethodIdentForOwner(self: *Self, owner: CIR.Statement.Idx, method_ident: Ident.Idx, qualified_ident: Ident.Idx) Allocator.Error!void {
    const key = MethodKey.init(owner, method_ident);
    try self.method_idents.put(self.gpa, key, qualified_ident);
}

/// Registers a method definition mapping for an explicit owner declaration.
pub fn registerMethodDefForOwner(self: *Self, owner: CIR.Statement.Idx, method_ident: Ident.Idx, binding: MethodBinding) Allocator.Error!void {
    const key = MethodKey.init(owner, method_ident);
    try self.method_defs.put(self.gpa, key, binding);
}

/// Looks up a qualified method ident for an explicit owner declaration.
pub fn lookupMethodIdentForOwner(self: *Self, owner: CIR.Statement.Idx, method_ident: Ident.Idx) ?Ident.Idx {
    const key = MethodKey.init(owner, method_ident);
    return self.method_idents.get(self.gpa, key);
}

/// Looks up a qualified method ident in finalized tables for an explicit owner declaration.
pub fn lookupMethodIdentForOwnerConst(self: *const Self, owner: CIR.Statement.Idx, method_ident: Ident.Idx) ?Ident.Idx {
    const key = MethodKey.init(owner, method_ident);
    return self.method_idents.getFinalized(key);
}

/// Looks up method type/check metadata in finalized tables for an explicit owner declaration.
pub fn lookupMethodBindingForOwnerConst(self: *const Self, owner: CIR.Statement.Idx, method_ident: Ident.Idx) ?MethodBinding {
    const key = MethodKey.init(owner, method_ident);
    return self.method_defs.getFinalized(key);
}

/// Finalizes method owner, ident, and definition tables.
pub fn finalizeMethodTables(self: *Self) void {
    self.method_idents.ensureSortedUnique();
    self.method_defs.ensureSortedUnique();
}

/// Looks up method metadata using a type declaration owner from one environment
/// and a method ident from the same source environment.
pub fn lookupMethodBindingFromEnvAndDeclConst(self: *const Self, source_env: *const Self, source_decl: ?u32, method_ident: Ident.Idx) ?MethodBinding {
    const method_name = source_env.getIdent(method_ident);

    const local_method_ident = self.common.findIdent(method_name) orelse return null;
    const owner: CIR.Statement.Idx = @enumFromInt(source_decl orelse return null);

    return self.lookupMethodBindingForOwnerConst(owner, local_method_ident);
}

/// Looks up method metadata using a type declaration owner and a method ident
/// that come from different source environments.
pub fn lookupMethodBindingFromTwoEnvsAndDeclConst(
    self: *const Self,
    source_decl: ?u32,
    method_source_env: *const Self,
    method_ident: Ident.Idx,
) ?MethodBinding {
    const method_name = method_source_env.getIdent(method_ident);

    const local_method_ident = self.common.findIdent(method_name) orelse return null;
    const owner: CIR.Statement.Idx = @enumFromInt(source_decl orelse return null);

    return self.lookupMethodBindingForOwnerConst(owner, local_method_ident);
}

/// Returns the line start positions for source code position mapping.
/// Each element represents the byte offset where a new line begins.
pub fn getLineStarts(self: *const Self) []const u32 {
    return self.common.getLineStartsAll();
}
