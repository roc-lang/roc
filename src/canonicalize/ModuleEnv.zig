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

    /// Extern-compatible serialized form
    pub const Serialized = extern struct {
        tag: Tag,
        payload: u32,

        pub fn encode(kind: ModuleKind) @This() {
            return switch (kind) {
                .type_module => |idx| .{ .tag = .type_module, .payload = @as(u32, @bitCast(idx)) },
                .default_app => .{ .tag = .default_app, .payload = 0 },
                .app => .{ .tag = .app, .payload = 0 },
                .package => .{ .tag = .package, .payload = 0 },
                .platform => .{ .tag = .platform, .payload = 0 },
                .hosted => .{ .tag = .hosted, .payload = 0 },
                .module => .{ .tag = .module, .payload = 0 },
                .malformed => .{ .tag = .malformed, .payload = 0 },
            };
        }

        pub fn decode(self: @This()) ModuleKind {
            return switch (self.tag) {
                .type_module => .{ .type_module = @as(Ident.Idx, @bitCast(self.payload)) },
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
    range_exclusive_to: Ident.Idx,
    range_inclusive_to: Ident.Idx,
    to_hash: Ident.Idx,
    parser_for: Ident.Idx,
    encoder_for: Ident.Idx,
    map: Ident.Idx,
    map_bang: Ident.Idx,

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
    builtin_range: Ident.Idx,
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
    u8x16_type: Ident.Idx,
    i8x16_type: Ident.Idx,
    u16x8_type: Ident.Idx,
    i16x8_type: Ident.Idx,
    u32x4_type: Ident.Idx,
    i32x4_type: Ident.Idx,
    u64x2_type: Ident.Idx,
    i64x2_type: Ident.Idx,
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
    // Synthetic identifier naming the rigid presence variable minted when
    // checking a definition's body against its own `?:` optional-field signature.
    optional_presence: Ident.Idx,
    // Error tag produced by optional field access (`r.?x`) when the field is
    // absent: the Err side of `Try(field_type, [MissingField])`.
    missing_field: Ident.Idx,

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
            .range_exclusive_to = try common.insertIdent(gpa, Ident.for_text("range_exclusive_to")),
            .range_inclusive_to = try common.insertIdent(gpa, Ident.for_text("range_inclusive_to")),
            .to_hash = try common.insertIdent(gpa, Ident.for_text("to_hash")),
            .parser_for = try common.insertIdent(gpa, Ident.for_text("parser_for")),
            .encoder_for = try common.insertIdent(gpa, Ident.for_text("encoder_for")),
            .map = try common.insertIdent(gpa, Ident.for_text("map")),
            .map_bang = try common.insertIdent(gpa, Ident.for_text("map!")),
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
            .builtin_range = try common.insertIdent(gpa, Ident.for_text("Builtin.Num.Range")),
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
            .u8x16_type = try common.insertIdent(gpa, Ident.for_text("Builtin.Num.U8x16")),
            .i8x16_type = try common.insertIdent(gpa, Ident.for_text("Builtin.Num.I8x16")),
            .u16x8_type = try common.insertIdent(gpa, Ident.for_text("Builtin.Num.U16x8")),
            .i16x8_type = try common.insertIdent(gpa, Ident.for_text("Builtin.Num.I16x8")),
            .u32x4_type = try common.insertIdent(gpa, Ident.for_text("Builtin.Num.U32x4")),
            .i32x4_type = try common.insertIdent(gpa, Ident.for_text("Builtin.Num.I32x4")),
            .u64x2_type = try common.insertIdent(gpa, Ident.for_text("Builtin.Num.U64x2")),
            .i64x2_type = try common.insertIdent(gpa, Ident.for_text("Builtin.Num.I64x2")),
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
            // Synthetic identifier naming rigid presence vars for `?:` fields
            .optional_presence = try common.insertIdent(gpa, Ident.for_text("#optional")),
            // Error tag for optional field access on an absent field
            .missing_field = try common.insertIdent(gpa, Ident.for_text("MissingField")),
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
            .range_exclusive_to = common.findIdent("range_exclusive_to") orelse unreachable,
            .range_inclusive_to = common.findIdent("range_inclusive_to") orelse unreachable,
            .to_hash = common.findIdent("to_hash") orelse unreachable,
            .parser_for = common.findIdent("parser_for") orelse unreachable,
            .encoder_for = common.findIdent("encoder_for") orelse unreachable,
            .map = common.findIdent("map") orelse unreachable,
            .map_bang = common.findIdent("map!") orelse unreachable,
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
            .builtin_range = common.findIdent("Builtin.Num.Range") orelse unreachable,
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
            .u8x16_type = common.findIdent("Builtin.Num.U8x16") orelse unreachable,
            .i8x16_type = common.findIdent("Builtin.Num.I8x16") orelse unreachable,
            .u16x8_type = common.findIdent("Builtin.Num.U16x8") orelse unreachable,
            .i16x8_type = common.findIdent("Builtin.Num.I16x8") orelse unreachable,
            .u32x4_type = common.findIdent("Builtin.Num.U32x4") orelse unreachable,
            .i32x4_type = common.findIdent("Builtin.Num.I32x4") orelse unreachable,
            .u64x2_type = common.findIdent("Builtin.Num.U64x2") orelse unreachable,
            .i64x2_type = common.findIdent("Builtin.Num.I64x2") orelse unreachable,
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
            // Synthetic identifier naming rigid presence vars for `?:` fields
            .optional_presence = common.findIdent("#optional") orelse unreachable,
            // Error tag for optional field access on an absent field
            .missing_field = common.findIdent("MissingField") orelse unreachable,
        };
    }
};

/// Owner identity for static-dispatch method lookup.
pub const MethodOwner = extern struct {
    owner_module_ident_bits: u32,
    owner: CIR.Statement.Idx,

    pub fn init(owner_module_ident: Ident.Idx, owner: CIR.Statement.Idx) MethodOwner {
        return .{
            .owner_module_ident_bits = @bitCast(owner_module_ident),
            .owner = owner,
        };
    }

    pub fn moduleIdent(self: MethodOwner) Ident.Idx {
        return @bitCast(self.owner_module_ident_bits);
    }

    pub fn eql(a: MethodOwner, b: MethodOwner) bool {
        return a.owner_module_ident_bits == b.owner_module_ident_bits and a.owner == b.owner;
    }
};

/// Key for method lookup: (receiver owner declaration, method_ident) pair.
pub const MethodKey = extern struct {
    owner_module_ident_bits: u32,
    owner: CIR.Statement.Idx,
    method_ident_bits: u32,

    pub fn init(owner: MethodOwner, method_ident: Ident.Idx) MethodKey {
        return .{
            .owner_module_ident_bits = owner.owner_module_ident_bits,
            .owner = owner.owner,
            .method_ident_bits = @bitCast(method_ident),
        };
    }

    pub fn ownerIdent(self: MethodKey) MethodOwner {
        return .{
            .owner_module_ident_bits = self.owner_module_ident_bits,
            .owner = self.owner,
        };
    }

    pub fn ownerModuleIdent(self: MethodKey) Ident.Idx {
        return @bitCast(self.owner_module_ident_bits);
    }

    pub fn methodIdent(self: MethodKey) Ident.Idx {
        return @bitCast(self.method_ident_bits);
    }

    pub fn order(a: MethodKey, b: MethodKey) std.math.Order {
        const a_module = a.owner_module_ident_bits;
        const b_module = b.owner_module_ident_bits;
        if (a_module != b_module) {
            return if (a_module < b_module) .lt else .gt;
        }

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

/// Mapping from (receiver owner declaration, method_ident) pairs to their qualified
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

/// Mapping from (receiver owner declaration, method_ident) pairs to the method binding.
/// This keeps method implementation lookup explicit without requiring local
/// associated methods to be published through the module exposure table.
pub const MethodDefs = SortedArrayBuilder(MethodKey, MethodBinding);

/// Construction-time position shared by the parallel method identity and
/// definition tables.
pub const MethodTableIndex = enum(u32) { _ };

/// A definition whose implementation was authored by the compiler as one
/// exact low-level operation. Canonicalization publishes this alongside CIR so
/// every later stage can consume the producer-owned runtime identity without
/// inspecting the generated lambda body.
pub const ProvidedLowLevelDef = extern struct {
    def_idx: u32,
    op: base.LowLevel,
    _padding: u16 = 0,

    pub const SafeList = collections.SafeList(@This());
};

/// Exact checker-owned shape of an iterator step result.
pub const IteratorStepTopology = extern struct {
    done_tag_ident: u32,
    one_tag_ident: u32,
    skip_tag_ident: u32,
    item_field_ident: u32,
    rest_field_ident: u32,
    one_payload_var: u32,
    skip_payload_var: u32,
};

/// Checked dispatch and topology metadata for one source `for` loop.
/// Later stages consume these exact identities instead of inferring the
/// iterator protocol from names or row shapes.
pub const ForLoopDispatchPlan = extern struct {
    node_idx: u32,
    pattern_idx: u32,
    iterable_idx: u32,
    iterator_var: u32,
    step_var: u32,
    iter_fn_var: u32,
    next_fn_var: u32,
    step_topology: IteratorStepTopology,

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
    after_decimal_digit_count: u64,
    flags: u32,

    pub const negative_flag: u32 = 1;
    pub const fractional_flag: u32 = 2;
    pub const decimal_point_flag: u32 = 4;
    pub const materialized_flag: u32 = 8;
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

    pub fn isMaterialized(self: NumeralLiteral) bool {
        return (self.flags & materialized_flag) != 0;
    }
};

/// One constrained-scheme use recorded by checking for static-dispatch
/// evidence. It names the source node, the scheme root used at that edge, and—
/// for an instantiation—the fresh var each constrained scheme var was
/// copied to. Shared monomorphic edges have no copy pairs. Publication resolves
/// the recorded vars after checking settles to decide how each of the callee's
/// dispatch constraints was satisfied at this site.
pub const SchemeUseRecord = extern struct {
    node_idx: u32,
    /// `Slot`—distinguishes several schemes instantiated at one node (a value
    /// use, an expression-position function stored as a value, or the target
    /// of a dispatch constraint).
    slot_kind: u32,
    /// For `dispatch_target` slots, the raw fn `Var` of the constraint whose
    /// discharge instantiated this scheme—unique per constraint
    /// instantiation, so nested evidence chains resolve without ambiguity.
    /// 0 for value and nested-function use slots (keyed by `node_idx`
    /// instead).
    slot_data: u32,
    /// The scheme root `Var` used at this edge. For imported schemes this is
    /// the pristine local copy; for shared uses it is the in-flight local root.
    scheme_root: u32,
    /// Range into `scheme_use_pairs`.
    pairs_start: u32,
    pairs_len: u32,

    pub const SafeList = collections.SafeList(@This());

    pub const Slot = enum(u32) {
        /// The scheme of a value that was referenced (e.g. an `e_lookup` of a
        /// generalized definition).
        value_use,
        /// A generalized expression-position function instantiated when a
        /// containing value (record, tuple, list, tag, or nominal) stores it.
        /// The nested function specialization consumes this edge's evidence.
        nested_function_use,
        /// The scheme of the method target chosen while discharging a static
        /// dispatch constraint originating at this node.
        dispatch_target,
        /// A monomorphic reference to an in-flight unannotated definition.
        /// The edge shares the definition's vars, so its record has no copy
        /// pairs but still names the exact scheme root used by checking.
        shared_value_use,
    };
};

/// One (constrained scheme var → fresh instantiated var) pair of a
/// `SchemeUseRecord`.
pub const SchemeUsePair = extern struct {
    /// Constrained var in the pristine scheme (`Var`).
    old_var: u32,
    /// The fresh copy created for this instantiation (`Var`).
    fresh_var: u32,

    pub const SafeList = collections.SafeList(@This());
};

/// One compiler-generated parser or encoder derivation validated by checking.
/// The referenced vars remain checker-owned here; checked publication converts
/// them to stable checked type ids before post-check compilation.
pub const GeneratedCodecDerivation = extern struct {
    kind: u32,
    source_constraint_fn_var: u32,
    source_runtime_fn_var: u32,
    source_shape_var: u32,
    source_encoding_var: u32,
    source_state_var: u32,
    source_error_var: u32,
    constraint_fn_var: u32,
    runtime_fn_var: u32,
    shape_var: u32,
    encoding_var: u32,
    state_var: u32,
    error_var: u32,
    calls_start: u32,
    calls_len: u32,

    pub const SafeList = collections.SafeList(@This());

    pub const Kind = enum(u32) {
        parser,
        encoder,
    };
};

/// One exact method callable used inside a checked generated codec.
pub const GeneratedCodecCall = extern struct {
    method_ident: u32,
    dispatcher_var: u32,
    callable_var: u32,
    /// Exact generated callable relation whose dispatch-target record owns the
    /// selected method scheme's nested evidence.
    evidence_var: u32,
    /// The value shape this call handles, or `no_subject_var` when the method
    /// has no shape-specific call contract.
    subject_var: u32,

    pub const no_subject_var = std.math.maxInt(u32);
    pub const SafeList = collections.SafeList(@This());
};

/// One static-dispatch obligation checking rejected. The raw constraint
/// function variable is the obligation identity used by dispatch expressions,
/// instantiated scheme evidence, and checked-artifact publication.
pub const RejectedStaticDispatch = extern struct {
    constraint_fn_var: u32,

    pub const SafeList = collections.SafeList(@This());

    pub fn fnVar(self: RejectedStaticDispatch) TypeVar {
        return @enumFromInt(self.constraint_fn_var);
    }
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

/// Checker-produced construction evidence for one field omitted by a record
/// literal through defaulted-field width absorption. The field's default is
/// construction-site data; it must survive even when later value unification
/// normalizes the shared runtime row to `required`.
pub const RecordOmittedDefault = extern struct {
    expr: CIR.Expr.Idx,
    field_name: Ident.Idx,
    origin_module: base.ModuleIdentity.Idx,
    default_expr_node: u32,

    pub const SafeList = collections.SafeList(@This());
};

/// A source node whose checked value is a rank-1 polymorphic type scheme.
///
/// Generalization records this explicitly because a partially generalized
/// scheme can have a monomorphic structural root with quantified descendants.
/// Consumers must therefore not infer scheme-ness from the root variable's
/// rank. The table is kept sorted by `node_idx` for allocation-free imported
/// lookup.
pub const BindingScheme = extern struct {
    node_idx: u32,

    pub const SafeList = collections.SafeList(@This());
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
/// Exact module-global value definitions selected by canonicalization's
/// source-name collision policy. Contains one definition per source-visible
/// name plus every definition whose pattern has no single source name.
top_level_value_defs: CIR.Def.Span,
/// Module-global definitions that introduce checked value bindings. Concrete
/// shadowed definitions retain their exact identities; annotation-only
/// declarations superseded by an implementation are excluded.
value_binding_defs: CIR.Def.Span,
/// Exact definitions rewritten from annotation-only declarations to hosted lambdas.
hosted_defs: CIR.Def.Span,
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
/// Package-qualified module display name (e.g., "pf.Color"). Display-only; identity
/// comparisons use content-based module identities (see `module_identities`).
/// Set by the coordinator after parse or cache hit.
qualified_module_ident: Ident.Idx,
/// Env-local module identity table: dense `base.ModuleIdentity.Idx` -> 32-byte
/// deep content hash (see `base.module_identity`). Entry ids are the
/// `origin_module` values stored on nominal/alias types in this env's type
/// store. Populated by `setContentIdentity` (self) and by cross-store type
/// copies rebasing imported origins into this table.
module_identities: base.SerialStringInterner,
/// Display ident (into this env's ident store) for each `module_identities`
/// entry, parallel by index. Display-only by itself; identity decisions must
/// read the paired content hash from `module_identities`.
module_identity_displays: collections.SafeList(Ident.Idx),
/// This module's own entry in `module_identities`; `NONE` until the deep
/// content identity has been computed (after import resolution, before
/// type-checking).
self_module_identity: base.ModuleIdentity.Idx,
/// Diagnostics collected during canonicalization (optional)
diagnostics: CIR.Diagnostic.Span,
/// Stores the raw nodes which represent the intermediate representation
/// Uses an efficient data structure, and provides helpers for storing and retrieving nodes.
store: NodeStore,

/// Dependency analysis results (evaluation order for defs)
/// Set after canonicalization completes. Must not be accessed before then.
evaluation_order: ?*DependencyGraph.EvaluationOrder,

/// Exact strict-demand edges between top-level definitions. Canonicalization
/// produces the initial relation and checking replaces it after resolving
/// literal dispatch; serialization preserves the finalized relation for
/// checked-artifact publication. Unlike `evaluation_order`, it is not a
/// transient traversal aid.
top_level_demand_dependencies: DependencyGraph.Dependency.SafeList,
top_level_demand_dependencies_ready: bool,

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
/// Compiler-authored low-level implementations, ordered by definition index.
provided_low_level_defs: ProvidedLowLevelDef.SafeList,

/// Dispatch plans attached by checking to source `for` loop nodes.
for_loop_dispatch_plans: ForLoopDispatchPlan.SafeList,
/// Base-256 bytes referenced by `numeral_literals`.
numeral_digit_bytes: collections.SafeList(u8),
/// Exact numeric literals attached to source expression and pattern nodes.
numeral_literals: NumeralLiteral.SafeList,
/// Scope-resolved explicit numeric suffix targets attached by canonicalization.
numeric_suffix_targets: NumericSuffixTarget.SafeList,
/// Constrained-scheme uses recorded by checking for static-dispatch evidence;
/// consumed at checked-module publication.
scheme_uses: SchemeUseRecord.SafeList,
/// Flat pool of (scheme var → fresh var) pairs backing `scheme_uses`.
scheme_use_pairs: SchemeUsePair.SafeList,
/// Exact source bindings that checking generalized into rank-1 type schemes.
/// Sorted by source node for allocation-free cross-module lookup.
binding_schemes: BindingScheme.SafeList,
/// Generated codec derivations validated by checking and consumed by checked
/// artifact publication.
generated_codec_derivations: GeneratedCodecDerivation.SafeList,
/// Flat pool backing `generated_codec_derivations.calls_start/calls_len`.
generated_codec_calls: GeneratedCodecCall.SafeList,
/// Static-dispatch obligations explicitly rejected by checking. Publication
/// consumes these records instead of inferring rejection from erroneous types.
rejected_static_dispatches: RejectedStaticDispatch.SafeList,
/// Exact default identities selected at record-literal omission sites.
record_omitted_defaults: RecordOmittedDefault.SafeList,

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
    /// The platform-local definition selected by this declaration, or null
    /// when canonicalization diagnosed an invalid target.
    local_def: ?CIR.Def.Idx,

    pub const SafeList = collections.SafeList(@This());
};

/// Platform hosted entry mapping a linker symbol to a hosted function in an
/// exposed type module. Populated during canonicalization for platform modules
/// from the hosted clause, in declaration order (which defines hosted dispatch
/// order). For example, `hosted { "roc_stdout_line": Stdout.line! }` creates an
/// entry with module_ident="Stdout", func_ident="line!", and symbol pointing to
/// the interned string "roc_stdout_line".
pub const HostedEntry = struct {
    pub const TargetStatus = enum(u8) {
        unresolved,
        resolved,
        missing_module,
        missing_value,
    };

    /// The type module name (e.g., "Stdout"); null for unqualified functions
    module_ident: ?Ident.Idx,
    /// The hosted function name (e.g., "line!")
    func_ident: Ident.Idx,
    /// The literal linker symbol (e.g., "roc_stdout_line")
    symbol: StringLiteral.Idx,
    /// Exact imported definition selected by this entry after canonicalization.
    target_import: ?CIR.Import.Idx,
    target_def: ?CIR.Def.Idx,
    target_status: TargetStatus,

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
    _padding: [3]u8 = .{ 0, 0, 0 },
    content_hash: [32]u8,
    start_offset: u32,
    end_offset: u32,

    pub const SafeList = collections.SafeList(@This());

    pub fn region(self: @This()) Region {
        return .{
            .start = .{ .offset = self.start_offset },
            .end = .{ .offset = self.end_offset },
        };
    }
};

/// Relocate all pointers in the ModuleEnv by the given offset.
/// This is used by serialized compiler artifacts whose internal pointers are
/// stored relative to the artifact buffer.
pub fn relocate(self: *Self, offset: isize) void {
    // Relocate all sub-structures that contain pointers
    self.common.relocate(offset);
    self.types.relocate(offset);
    self.module_identities.relocate(offset);
    self.module_identity_displays.relocate(offset);
    self.external_decls.relocate(offset);
    self.requires_types.relocate(offset);
    self.for_clause_aliases.relocate(offset);
    self.provides_entries.relocate(offset);
    self.hosted_entries.relocate(offset);
    self.imports.relocate(offset);
    self.file_dependencies.relocate(offset);
    self.store.relocate(offset);
    self.top_level_demand_dependencies.relocate(offset);
    self.method_idents.relocate(offset);
    self.method_defs.relocate(offset);
    self.provided_low_level_defs.relocate(offset);
    self.for_loop_dispatch_plans.relocate(offset);
    self.binding_schemes.relocate(offset);
    self.rejected_static_dispatches.relocate(offset);
    self.record_omitted_defaults.relocate(offset);

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
    self.top_level_value_defs = .{ .span = .{ .start = 0, .len = 0 } };
    self.value_binding_defs = .{ .span = .{ .start = 0, .len = 0 } };
    self.hosted_defs = .{ .span = .{ .start = 0, .len = 0 } };
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
    self.top_level_demand_dependencies = .{};
    self.top_level_demand_dependencies_ready = false;
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
        .top_level_value_defs = .{ .span = .{ .start = 0, .len = 0 } },
        .value_binding_defs = .{ .span = .{ .start = 0, .len = 0 } },
        .hosted_defs = .{ .span = .{ .start = 0, .len = 0 } },
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
        .module_identities = .{},
        .module_identity_displays = .{},
        .self_module_identity = base.ModuleIdentity.Idx.NONE,
        .diagnostics = CIR.Diagnostic.Span{ .span = base.DataSpan{ .start = 0, .len = 0 } },
        .store = try NodeStore.initCapacity(gpa, node_capacity),
        .evaluation_order = null, // Will be set after canonicalization completes
        .top_level_demand_dependencies = .{},
        .top_level_demand_dependencies_ready = false,
        .runtime_prepared = false,
        .idents = idents,
        .import_mapping = types_mod.import_mapping.ImportMapping.init(gpa),
        .method_idents = MethodIdents.init(),
        .method_defs = MethodDefs.init(),
        .provided_low_level_defs = try ProvidedLowLevelDef.SafeList.initCapacity(gpa, 4),
        .for_loop_dispatch_plans = try ForLoopDispatchPlan.SafeList.initCapacity(gpa, 4),
        .numeral_digit_bytes = try collections.SafeList(u8).initCapacity(gpa, 32),
        .numeral_literals = try NumeralLiteral.SafeList.initCapacity(gpa, 8),
        .numeric_suffix_targets = try NumericSuffixTarget.SafeList.initCapacity(gpa, 8),
        .scheme_uses = try SchemeUseRecord.SafeList.initCapacity(gpa, 8),
        .scheme_use_pairs = try SchemeUsePair.SafeList.initCapacity(gpa, 8),
        .binding_schemes = try BindingScheme.SafeList.initCapacity(gpa, 8),
        .generated_codec_derivations = try GeneratedCodecDerivation.SafeList.initCapacity(gpa, 4),
        .generated_codec_calls = try GeneratedCodecCall.SafeList.initCapacity(gpa, 16),
        .rejected_static_dispatches = try RejectedStaticDispatch.SafeList.initCapacity(gpa, 4),
        .record_omitted_defaults = try RecordOmittedDefault.SafeList.initCapacity(gpa, 4),
    };
}

/// Deinitialize the module environment.
pub fn deinit(self: *Self) void {
    self.common.deinit(self.gpa);
    self.types.deinit();
    self.module_identities.deinit(self.gpa);
    self.module_identity_displays.deinit(self.gpa);
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
    self.provided_low_level_defs.deinit(self.gpa);
    self.for_loop_dispatch_plans.deinit(self.gpa);
    self.numeral_digit_bytes.deinit(self.gpa);
    self.numeral_literals.deinit(self.gpa);
    self.numeric_suffix_targets.deinit(self.gpa);
    self.scheme_uses.deinit(self.gpa);
    self.scheme_use_pairs.deinit(self.gpa);
    self.binding_schemes.deinit(self.gpa);
    self.generated_codec_derivations.deinit(self.gpa);
    self.generated_codec_calls.deinit(self.gpa);
    self.rejected_static_dispatches.deinit(self.gpa);
    self.record_omitted_defaults.deinit(self.gpa);
    self.top_level_demand_dependencies.deinit(self.gpa);
    // diagnostics are stored in the NodeStore, no need to free separately
    self.store.deinit();

    if (self.evaluation_order) |eval_order| {
        eval_order.deinit();
        self.gpa.destroy(eval_order);
    }
}

/// Replace the module's exact strict-demand relation with freshly produced
/// canonical dependency data. Ownership of `dependencies` transfers here.
pub fn setTopLevelDemandDependencies(
    self: *Self,
    dependencies: DependencyGraph.Dependency.SafeList,
) void {
    self.top_level_demand_dependencies.deinit(self.gpa);
    self.top_level_demand_dependencies = dependencies;
    self.top_level_demand_dependencies_ready = true;
}

/// Return the producer-authored low-level implementation for `def_idx`.
pub fn providedLowLevelForDef(self: *const Self, def_idx: CIR.Def.Idx) ?base.LowLevel {
    const entries = self.provided_low_level_defs.items.items;
    const wanted: u32 = @intFromEnum(def_idx);
    var low: usize = 0;
    var high: usize = entries.len;
    while (low < high) {
        const mid = low + (high - low) / 2;
        const candidate = entries[mid];
        if (candidate.def_idx < wanted) {
            low = mid + 1;
        } else if (candidate.def_idx > wanted) {
            high = mid;
        } else {
            return candidate.op;
        }
    }
    return null;
}

/// Return the current producer-authored exact strict-demand relation.
pub fn topLevelDemandDependencies(self: *const Self) []const DependencyGraph.Dependency {
    std.debug.assert(self.top_level_demand_dependencies_ready);
    return self.top_level_demand_dependencies.items.items;
}

/// Whether a compiler stage has produced the exact strict-demand relation.
pub fn topLevelDemandDependenciesReady(self: *const Self) bool {
    return self.top_level_demand_dependencies_ready;
}

/// Whether the current exact strict-demand relation contains one edge.
pub fn hasTopLevelDemandDependency(
    self: *const Self,
    dependent: CIR.Def.Idx,
    dependency: CIR.Def.Idx,
) bool {
    return DependencyGraph.hasDependency(
        self.topLevelDemandDependencies(),
        dependent,
        dependency,
    );
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
    self.provided_low_level_defs.deinit(self.gpa);
    self.for_loop_dispatch_plans.deinit(self.gpa);
    self.numeral_digit_bytes.deinit(self.gpa);
    self.numeral_literals.deinit(self.gpa);
    self.numeric_suffix_targets.deinit(self.gpa);
    self.scheme_uses.deinit(self.gpa);
    self.scheme_use_pairs.deinit(self.gpa);
    self.binding_schemes.deinit(self.gpa);
    self.generated_codec_derivations.deinit(self.gpa);
    self.generated_codec_calls.deinit(self.gpa);
    self.rejected_static_dispatches.deinit(self.gpa);
    self.record_omitted_defaults.deinit(self.gpa);

    // If enableRuntimeInserts was called on the interner, it allocated new memory
    // that needs to be freed. The interner.deinit checks supports_inserts internally
    // and will only free if memory was actually allocated (not for pure cached data).
    self.common.idents.interner.deinit(self.gpa);

    // Same pattern for the module identity table: frozen (buffer-aliased) data is
    // a no-op to deinit; runtime-grown data is freed.
    self.module_identities.deinit(self.gpa);
    self.module_identity_displays.deinit(self.gpa);
}

/// Record a relative file dependency before its final read state is known.
pub fn recordFileDependency(self: *Self, relative_path: []const u8, start_offset: u32, end_offset: u32) Allocator.Error!FileDependency.SafeList.Idx {
    const path_idx = try self.insertString(relative_path);
    return try self.file_dependencies.append(self.gpa, .{
        .relative_path = path_idx,
        .state = .pending,
        ._padding = .{ 0, 0, 0 },
        .content_hash = [_]u8{0} ** 32,
        .start_offset = start_offset,
        .end_offset = end_offset,
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
/// site is pushed separately and later—used when forward-reference vs
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
    return diagnostic.toRegion();
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

    const existing_span = self.diagnostics.span;
    const index_len = self.store.index_data.len();
    const existing_at_tail = @as(u64, existing_span.start) + @as(u64, existing_span.len) == index_len;
    const copy_count: u32 = if (existing_at_tail) 0 else existing_span.len;
    const additional_capacity: usize = @intCast(@as(u64, copy_count) + @as(u64, new_top));
    const index_start = if (existing_at_tail) existing_span.start else @as(u32, @intCast(index_len));

    // Reserve before borrowing existing diagnostics. The diagnostic span is a
    // view into index_data, so growing index_data while iterating that view
    // would invalidate it if the backing allocation moved.
    try self.store.index_data.items.ensureUnusedCapacity(self.gpa, additional_capacity);

    if (!existing_at_tail) {
        const existing = self.store.sliceDiagnostics(self.diagnostics);
        for (existing) |diagnostic_idx| {
            _ = self.store.index_data.appendAssumeCapacity(@intFromEnum(diagnostic_idx));
        }
    }

    var i: u32 = 0;
    while (i < new_top) : (i += 1) {
        const diagnostic_idx = scratch.diagnostics.items.items[@intCast(i)];
        _ = self.store.index_data.appendAssumeCapacity(@intFromEnum(diagnostic_idx));
    }

    scratch.diagnostics.clearFrom(0);
    self.diagnostics = .{
        .span = .{
            .start = index_start,
            .len = @intCast(@as(u64, existing_span.len) + @as(u64, new_top)),
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
        .provided_value_is_required => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            const ident_name = self.getIdent(data.ident);
            const is_effectful = std.mem.endsWith(u8, ident_name, "!");
            const stem = if (is_effectful) ident_name[0 .. ident_name.len - 1] else ident_name;
            const example = try std.fmt.allocPrint(
                allocator,
                "{s}_for_host{s} = {s}",
                .{ stem, if (is_effectful) "!" else "", ident_name },
            );
            defer allocator.free(example);

            var report = try Report.init(allocator, "Required Value in Provides", "", .runtime_error);
            const owned_ident = try report.addOwnedString(ident_name);
            try report.headline.addUnqualifiedSymbol(owned_ident);
            try report.headline.addReflowingText(" is supplied by the app through the platform's ");
            try report.headline.addInlineCode("requires");
            try report.headline.addReflowingText(" section, so ");
            try report.headline.addInlineCode("provides");
            try report.headline.addReflowingText(" cannot expose it to the host directly.");

            const owned_filename = try report.addOwnedString(filename);
            try report.addSourceContext(region_info, owned_filename, self.getSourceAll(), self.getLineStartsAll());

            try report.document.addReflowingText("Define a platform-local entrypoint which forwards to ");
            try report.document.addUnqualifiedSymbol(owned_ident);
            try report.document.addReflowingText(", then reference that entrypoint from ");
            try report.document.addInlineCode("provides");
            try report.document.addReflowingText(". For example:");
            try report.document.addLineBreak();
            try report.document.addLineBreak();
            const owned_example = try report.addOwnedString(example);
            try report.document.addInlineCode(owned_example);

            break :blk report;
        },
        .unused_variable => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            const ident_name = self.getIdent(data.ident);

            var report = try Report.init(allocator, "Unused Variable", "", .warning);
            const owned_ident = try report.addOwnedString(ident_name);
            try report.headline.addReflowingText("Variable ");
            try report.headline.addUnqualifiedSymbol(owned_ident);
            try report.headline.addReflowingText(" is defined here and then never used:");

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

            const headline = try std.fmt.allocPrint(allocator, "Underscores are not allowed in type {s} declarations.", .{data.declared.label()});
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
            try report.document.addReflowingText(" was already declared in ");
            try report.document.addSourceLocation(original_region_info, owned_filename);
            try report.document.addReflowingText(":");
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
            try report.document.addReflowingText(" was already declared in ");
            try report.document.addSourceLocation(original_region_info, owned_filename);
            try report.document.addReflowingText(":");
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
            try report.document.addReflowingText(" was already declared in ");
            try report.document.addSourceLocation(original_region_info, owned_filename);
            try report.document.addReflowingText(":");
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
        .invalid_associated_statement => |data| blk: {
            const stmt_name = self.getString(data.stmt);
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Invalid Statement", "", .runtime_error);
            const owned_stmt = try report.addOwnedString(stmt_name);
            try report.headline.addReflowingText("The statement ");
            try report.headline.addInlineCode(owned_stmt);
            try report.headline.addReflowingText(" is not allowed in an associated block.");
            try report.document.addReflowingText("Only associated values, type declarations, and type annotations are allowed in an associated block.");
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
            try report.document.addReflowingText(" was first defined in ");
            try report.document.addSourceLocation(original_region_info, owned_filename);
            try report.document.addReflowingText(":");
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
            try report.headline.addReflowingText(" is being redeclared here:");

            // The primary region shows the new declaration; point below it at the original.
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
            try report.document.addReflowingText(" was already defined in ");
            try report.document.addSourceLocation(original_region_info, owned_filename);
            try report.document.addReflowingText(":");
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
        .internal_builtin_type => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            const parent_bytes = self.getIdent(data.parent_name);
            const nested_bytes = self.getIdent(data.nested_name);

            var report = try Report.init(allocator, "Internal Builtin Type", "", .runtime_error);
            const parent_name = try report.addOwnedString(parent_bytes);
            const nested_name = try report.addOwnedString(nested_bytes);

            try report.headline.addInlineCode(nested_name);
            try report.headline.addReflowingText(" is internal to ");
            try report.headline.addInlineCode(parent_name);
            try report.headline.addReflowingText(", so it can't be named here.");

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            try report.document.addReflowingText("It describes how a builtin format tracks its own state while encoding or parsing, which is why it has no spelling in Roc code. To require that a type can be encoded or parsed, name the constraint instead, as in ");
            try report.document.addInlineCode(switch (data.kind) {
                .json => "where [a.Json.Encodable([])]",
                .http_header => "where [a.Encoding.HttpHeader.Parseable([])]",
            });
            try report.document.addReflowingText(".");

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

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );
            try report.document.addLineBreak();
            try report.document.addAnnotated("Hint:", .emphasized);
            try report.document.addReflowingText(" ");
            try report.document.addInlineCode("where");
            try report.document.addReflowingText(" clauses can only go on function type annotations.");

            break :blk report;
        },
        .where_alias_constraint_not_on_receiver => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Where Alias Constrains Another Type", "", .runtime_error);
            try report.headline.addReflowingText("A where alias constrains only its receiver, but this constraint is on a different type variable.");

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );
            try report.document.addLineBreak();
            try report.document.addReflowingText("Write this constraint against ");
            try report.document.addInlineCode(self.getIdent(data.receiver_name));
            try report.document.addReflowingText(", or declare a separate where alias for the other type variable and apply it alongside this one.");

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
        .record_default_not_literal => |data| blk: {
            const field_name = self.getIdent(data.field_name);
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Default Value Must Be A Literal", "", .runtime_error);
            const owned_field_name = try report.addOwnedString(field_name);
            try report.headline.addReflowingText("The default value for the ");
            try report.headline.addRecordField(owned_field_name);
            try report.headline.addReflowingText(" field is not a literal.");

            const owned_filename = try report.addOwnedString(filename);
            try report.document.addSourceRegion(
                region_info,
                .error_highlight,
                owned_filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );

            try report.document.addLineBreak();
            try report.document.addReflowingText("A field default (");
            try report.document.addInlineCode("??");
            try report.document.addReflowingText(") is materialized by the compiler at every construction site that omits the field, so it must be a literal: a number, an interpolation-free string, a tag, or a list, record, or tuple built only from literals. Anything that refers to another value could form an evaluation cycle the compiler will not chase.");

            break :blk report;
        },
        .optional_field_cannot_have_default => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Optional Field Cannot Have A Default", "", .runtime_error);
            try report.headline.addReflowingText("A field cannot be both optional (");
            try report.headline.addInlineCode("?:");
            try report.headline.addReflowingText(") and defaulted (");
            try report.headline.addInlineCode("??");
            try report.headline.addReflowingText("): a default fills the field whenever construction omits it, so the field can never be missing. Use ");
            try report.headline.addInlineCode(":");
            try report.headline.addReflowingText(" with ");
            try report.headline.addInlineCode("??");
            try report.headline.addReflowingText(" instead.");

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
        .unnamed_field_cannot_have_default => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Unnamed Field Cannot Have A Default", "", .runtime_error);
            try report.headline.addReflowingText("Unnamed fields (");
            try report.headline.addInlineCode("_");
            try report.headline.addReflowingText(" or ");
            try report.headline.addInlineCode("_name");
            try report.headline.addReflowingText(") reserve padding in a nominal record layout, so they cannot have a ");
            try report.headline.addInlineCode("??");
            try report.headline.addReflowingText(" default. Remove the default, or give the field a regular name if it should be filled when omitted.");

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
        .roc_version_mismatch => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            const pinned_bytes = self.getIdent(data.pinned);
            const running_bytes = self.getIdent(data.running);

            var report = try Report.init(allocator, "Roc Version Mismatch", "", .warning);
            const pinned = try report.addOwnedString(pinned_bytes);
            const running = try report.addOwnedString(running_bytes);
            try report.headline.addReflowingText("This header pins Roc version ");
            try report.headline.addInlineCode(pinned);
            try report.headline.addReflowingText(", but you are running ");
            try report.headline.addInlineCode(running);
            try report.headline.addReflowingText(".");

            try report.document.addReflowingText("Run ");
            try report.document.addInlineCode("roc fmt");
            try report.document.addReflowingText(" to update the pin, or switch to the pinned version of the compiler.");
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
            try report.document.addReflowingText(" declared in ");
            try report.document.addSourceLocation(other_region_info, owned_filename);
            try report.document.addReflowingText(":");
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
        .type_parameter_conflict => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);
            const original_region_info = self.calcRegionInfo(data.original_region);
            break :blk try CIR.Diagnostic.buildTypeParameterConflictReport(
                allocator,
                self.getIdent(data.name),
                self.getIdent(data.parameter_name),
                region_info,
                original_region_info,
                filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );
        },
        .type_shadowed_warning => |data| blk: {
            const new_region_info = self.calcRegionInfo(data.region);
            const original_region_info = self.calcRegionInfo(data.original_region);
            break :blk try CIR.Diagnostic.buildTypeShadowedWarningReport(
                allocator,
                self.getIdent(data.name),
                new_region_info,
                original_region_info,
                filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );
        },
        .builtin_type_shadowed_warning => |data| blk: {
            const new_region_info = self.calcRegionInfo(data.region);
            break :blk try CIR.Diagnostic.buildBuiltinTypeShadowedWarningReport(
                allocator,
                self.getIdent(data.name),
                new_region_info,
                filename,
                self.getSourceAll(),
                self.getLineStartsAll(),
            );
        },
        .invalid_string_interpolation,
        .can_lambda_not_implemented,
        .unused_type_var_name,
        .type_var_marked_unused,
        .type_var_starting_with_dollar,
        => std.debug.panic("Unhandled canonicalize diagnostic in diagnosticToReport: {s}", .{@tagName(diagnostic)}),
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
    top_level_value_defs: CIR.Def.Span,
    value_binding_defs: CIR.Def.Span,
    hosted_defs: CIR.Def.Span,
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
    module_identities: base.SerialStringInterner.Serialized,
    module_identity_displays: collections.SafeList(Ident.Idx).Serialized,
    self_module_identity_reserved: u32,
    self_module_identity_padding: u32 = 0,
    diagnostics: CIR.Diagnostic.Span,
    store: NodeStore.Serialized,
    evaluation_order_reserved: u64, // Reserved space for evaluation_order field (required for in-place deserialization cast)
    top_level_demand_dependencies: DependencyGraph.Dependency.SafeList.Serialized,
    top_level_demand_dependencies_ready: bool,
    runtime_prepared: bool,
    runtime_prepared_padding: [6]u8,
    // Well-known identifier indices (serialized directly, no lookup needed during deserialization)
    idents: CommonIdents,
    import_mapping_reserved: [6]u64, // Reserved space for import_mapping (AutoHashMap is ~40 bytes), initialized at runtime
    method_idents: MethodIdents.Serialized,
    method_defs: MethodDefs.Serialized,
    provided_low_level_defs: ProvidedLowLevelDef.SafeList.Serialized,
    for_loop_dispatch_plans: ForLoopDispatchPlan.SafeList.Serialized,
    numeral_digit_bytes: collections.SafeList(u8).Serialized,
    numeral_literals: NumeralLiteral.SafeList.Serialized,
    numeric_suffix_targets: NumericSuffixTarget.SafeList.Serialized,
    scheme_uses: SchemeUseRecord.SafeList.Serialized,
    scheme_use_pairs: SchemeUsePair.SafeList.Serialized,
    binding_schemes: BindingScheme.SafeList.Serialized,
    generated_codec_derivations: GeneratedCodecDerivation.SafeList.Serialized,
    generated_codec_calls: GeneratedCodecCall.SafeList.Serialized,
    rejected_static_dispatches: RejectedStaticDispatch.SafeList.Serialized,
    record_omitted_defaults: RecordOmittedDefault.SafeList.Serialized,
    // Reserved space (was is_lambda_lifted and is_defunctionalized, now unused)
    _reserved_flags: [2]u8 = .{ 0, 0 },
    _padding: [6]u8 = .{ 0, 0, 0, 0, 0, 0 },

    comptime {
        const renamed_fields = [_]collections.serde_validation.FieldRename{
            .{ .owner = "display_module_name_idx", .serialized = "display_module_name_idx_reserved" },
            .{ .owner = "qualified_module_ident", .serialized = "qualified_module_ident_reserved" },
            .{ .owner = "self_module_identity", .serialized = "self_module_identity_reserved" },
            .{ .owner = "evaluation_order", .serialized = "evaluation_order_reserved" },
            .{ .owner = "import_mapping", .serialized = "import_mapping_reserved" },
        };
        const serialized_only_fields = [_][]const u8{
            "self_module_identity_padding", // Fixed-width padding for the reserved identity slot.
            "runtime_prepared_padding", // Fixed-width padding for the serialized bool.
            "_reserved_flags", // Format-reserved bytes for fields removed from ModuleEnv.
            "_padding", // Tail padding kept explicit and zeroed for deterministic bytes.
        };
        collections.serde_validation.assertBidirectionalFieldSet(
            Self,
            Serialized,
            &.{},
            &serialized_only_fields,
            &renamed_fields,
        );
        collections.serde_validation.assertSerializedRelocatable(Serialized);
    }

    pub fn validate(self: *const Serialized, backing_len: usize) error{CorruptArtifact}!void {
        if (backing_len < @sizeOf(Serialized)) return error.CorruptArtifact;
        try collections.validateSerializedRelocations(Serialized, self, backing_len);
    }

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
        self.top_level_value_defs = env.top_level_value_defs;
        self.value_binding_defs = env.value_binding_defs;
        self.hosted_defs = env.hosted_defs;
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

        try self.top_level_demand_dependencies.serialize(
            &env.top_level_demand_dependencies,
            allocator,
            writer,
        );

        // Set gpa, module_name, evaluation_order_reserved to zeros;
        // these are runtime-only and will be set during deserialization.
        // Preserve display_module_name_idx since the ident store is also serialized and indices remain valid.
        self.gpa = .{ 0, 0 };
        self.module_name = .{ 0, 0 };
        self.display_module_name_idx_reserved = @bitCast(env.display_module_name_idx);
        self.qualified_module_ident_reserved = @bitCast(env.qualified_module_ident);
        try self.module_identities.serialize(&env.module_identities, allocator, writer);
        try self.module_identity_displays.serialize(&env.module_identity_displays, allocator, writer);
        self.self_module_identity_reserved = @intFromEnum(env.self_module_identity);
        self.self_module_identity_padding = 0;
        self.evaluation_order_reserved = 0;
        self.top_level_demand_dependencies_ready = env.top_level_demand_dependencies_ready;
        self.runtime_prepared = env.module_role == .builtin and env.runtime_prepared;
        self.runtime_prepared_padding = .{ 0, 0, 0, 0, 0, 0 };
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
        try self.provided_low_level_defs.serialize(&env.provided_low_level_defs, allocator, writer);
        try self.for_loop_dispatch_plans.serialize(&env.for_loop_dispatch_plans, allocator, writer);
        try self.numeral_digit_bytes.serialize(&env.numeral_digit_bytes, allocator, writer);
        try self.numeral_literals.serialize(&env.numeral_literals, allocator, writer);
        try self.numeric_suffix_targets.serialize(&env.numeric_suffix_targets, allocator, writer);
        try self.scheme_uses.serialize(&env.scheme_uses, allocator, writer);
        try self.scheme_use_pairs.serialize(&env.scheme_use_pairs, allocator, writer);
        try self.binding_schemes.serialize(&env.binding_schemes, allocator, writer);
        try self.generated_codec_derivations.serialize(&env.generated_codec_derivations, allocator, writer);
        try self.generated_codec_calls.serialize(&env.generated_codec_calls, allocator, writer);
        try self.rejected_static_dispatches.serialize(&env.rejected_static_dispatches, allocator, writer);
        try self.record_omitted_defaults.serialize(&env.record_omitted_defaults, allocator, writer);

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
            .top_level_value_defs = self.top_level_value_defs,
            .value_binding_defs = self.value_binding_defs,
            .hosted_defs = self.hosted_defs,
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
            .module_identities = self.module_identities.deserialize(base_addr),
            .module_identity_displays = self.module_identity_displays.deserializeInto(base_addr),
            .self_module_identity = @enumFromInt(self.self_module_identity_reserved),
            .diagnostics = self.diagnostics,
            .store = self.store.deserializeInto(base_addr, gpa),
            .evaluation_order = null, // Not serialized, will be recomputed if needed
            .top_level_demand_dependencies = self.top_level_demand_dependencies.deserializeInto(base_addr),
            .top_level_demand_dependencies_ready = self.top_level_demand_dependencies_ready,
            .runtime_prepared = self.runtime_prepared,
            .idents = self.idents,
            .import_mapping = types_mod.import_mapping.ImportMapping.init(gpa),
            .method_idents = self.method_idents.deserializeInto(base_addr),
            .method_defs = self.method_defs.deserializeInto(base_addr),
            .provided_low_level_defs = self.provided_low_level_defs.deserializeInto(base_addr),
            .for_loop_dispatch_plans = self.for_loop_dispatch_plans.deserializeInto(base_addr),
            .numeral_digit_bytes = self.numeral_digit_bytes.deserializeInto(base_addr),
            .numeral_literals = self.numeral_literals.deserializeInto(base_addr),
            .numeric_suffix_targets = self.numeric_suffix_targets.deserializeInto(base_addr),
            .scheme_uses = self.scheme_uses.deserializeInto(base_addr),
            .scheme_use_pairs = self.scheme_use_pairs.deserializeInto(base_addr),
            .binding_schemes = self.binding_schemes.deserializeInto(base_addr),
            .generated_codec_derivations = self.generated_codec_derivations.deserializeInto(base_addr),
            .generated_codec_calls = self.generated_codec_calls.deserializeInto(base_addr),
            .rejected_static_dispatches = self.rejected_static_dispatches.deserializeInto(base_addr),
            .record_omitted_defaults = self.record_omitted_defaults.deserializeInto(base_addr),
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
            .top_level_value_defs = self.top_level_value_defs,
            .value_binding_defs = self.value_binding_defs,
            .hosted_defs = self.hosted_defs,
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
            .module_identities = self.module_identities.deserialize(base_addr),
            .module_identity_displays = self.module_identity_displays.deserializeInto(base_addr),
            .self_module_identity = @enumFromInt(self.self_module_identity_reserved),
            .diagnostics = self.diagnostics,
            .store = self.store.deserializeInto(base_addr, gpa),
            .evaluation_order = null,
            .top_level_demand_dependencies = self.top_level_demand_dependencies.deserializeInto(base_addr),
            .top_level_demand_dependencies_ready = self.top_level_demand_dependencies_ready,
            .runtime_prepared = self.runtime_prepared,
            .idents = self.idents,
            .import_mapping = types_mod.import_mapping.ImportMapping.init(gpa),
            .method_idents = self.method_idents.deserializeInto(base_addr),
            .method_defs = self.method_defs.deserializeInto(base_addr),
            .provided_low_level_defs = self.provided_low_level_defs.deserializeInto(base_addr),
            .for_loop_dispatch_plans = self.for_loop_dispatch_plans.deserializeInto(base_addr),
            .numeral_digit_bytes = self.numeral_digit_bytes.deserializeInto(base_addr),
            .numeral_literals = self.numeral_literals.deserializeInto(base_addr),
            .numeric_suffix_targets = self.numeric_suffix_targets.deserializeInto(base_addr),
            .scheme_uses = self.scheme_uses.deserializeInto(base_addr),
            .scheme_use_pairs = self.scheme_use_pairs.deserializeInto(base_addr),
            .binding_schemes = self.binding_schemes.deserializeInto(base_addr),
            .generated_codec_derivations = self.generated_codec_derivations.deserializeInto(base_addr),
            .generated_codec_calls = self.generated_codec_calls.deserializeInto(base_addr),
            .rejected_static_dispatches = self.rejected_static_dispatches.deserializeInto(base_addr),
            .record_omitted_defaults = self.record_omitted_defaults.deserializeInto(base_addr),
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
            .top_level_value_defs = self.top_level_value_defs,
            .value_binding_defs = self.value_binding_defs,
            .hosted_defs = self.hosted_defs,
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
            .module_identities = self.module_identities.deserialize(base_addr),
            // Copy so the display list can grow if runtime type copies add identities.
            .module_identity_displays = try self.module_identity_displays.deserializeWithCopy(base_addr, gpa),
            .self_module_identity = @enumFromInt(self.self_module_identity_reserved),
            .diagnostics = self.diagnostics,
            // Use deserializeWithCopy for NodeStore so regions can be extended
            .store = try self.store.deserializeWithCopy(base_addr, gpa),
            .evaluation_order = null,
            .top_level_demand_dependencies = self.top_level_demand_dependencies.deserializeInto(base_addr),
            .top_level_demand_dependencies_ready = self.top_level_demand_dependencies_ready,
            .runtime_prepared = self.runtime_prepared,
            .idents = self.idents,
            .import_mapping = types_mod.import_mapping.ImportMapping.init(gpa),
            .method_idents = self.method_idents.deserializeInto(base_addr),
            .method_defs = self.method_defs.deserializeInto(base_addr),
            .provided_low_level_defs = try self.provided_low_level_defs.deserializeWithCopy(base_addr, gpa),
            .for_loop_dispatch_plans = try self.for_loop_dispatch_plans.deserializeWithCopy(base_addr, gpa),
            .numeral_digit_bytes = try self.numeral_digit_bytes.deserializeWithCopy(base_addr, gpa),
            .numeral_literals = try self.numeral_literals.deserializeWithCopy(base_addr, gpa),
            .numeric_suffix_targets = try self.numeric_suffix_targets.deserializeWithCopy(base_addr, gpa),
            .scheme_uses = try self.scheme_uses.deserializeWithCopy(base_addr, gpa),
            .scheme_use_pairs = try self.scheme_use_pairs.deserializeWithCopy(base_addr, gpa),
            .binding_schemes = try self.binding_schemes.deserializeWithCopy(base_addr, gpa),
            .generated_codec_derivations = try self.generated_codec_derivations.deserializeWithCopy(base_addr, gpa),
            .generated_codec_calls = try self.generated_codec_calls.deserializeWithCopy(base_addr, gpa),
            .rejected_static_dispatches = try self.rejected_static_dispatches.deserializeWithCopy(base_addr, gpa),
            .record_omitted_defaults = try self.record_omitted_defaults.deserializeWithCopy(base_addr, gpa),
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
    iterator_var: TypeVar,
    step_var: TypeVar,
    iter_fn_var: TypeVar,
    next_fn_var: TypeVar,
    step_topology: IteratorStepTopology,
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
            .iterator_var = @intFromEnum(iterator_var),
            .step_var = @intFromEnum(step_var),
            .iter_fn_var = @intFromEnum(iter_fn_var),
            .next_fn_var = @intFromEnum(next_fn_var),
            .step_topology = step_topology,
        };
        return;
    }
    _ = try self.for_loop_dispatch_plans.append(self.gpa, .{
        .node_idx = raw_node,
        .pattern_idx = raw_pattern,
        .iterable_idx = raw_iterable,
        .iterator_var = @intFromEnum(iterator_var),
        .step_var = @intFromEnum(step_var),
        .iter_fn_var = @intFromEnum(iter_fn_var),
        .next_fn_var = @intFromEnum(next_fn_var),
        .step_topology = step_topology,
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
///
/// The table is kept sorted by `node_idx` so lookups are O(log n).
/// Canonicalization records each literal right after allocating its node, so
/// appends arrive in increasing node order and the sort costs nothing; an
/// out-of-order record shifts the tail to keep the order invariant.
pub fn recordNumeralLiteral(
    self: *Self,
    node_idx: Node.Idx,
    before: []const u8,
    after: []const u8,
    after_decimal_digit_count: u64,
    is_negative: bool,
    is_fractional: bool,
    had_decimal_point: bool,
    is_materialized: bool,
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
            (if (had_decimal_point) NumeralLiteral.decimal_point_flag else 0) |
            (if (is_materialized) NumeralLiteral.materialized_flag else 0),
    };
    try upsertSortedByNode(NumeralLiteral, &self.numeral_literals, self.gpa, literal);
}

/// Return exact base-256 digits for a numeric source node.
pub fn numeralLiteralForNode(self: *const Self, node_idx: Node.Idx) ?NumeralLiteral {
    return findSortedByNode(NumeralLiteral, self.numeral_literals.items.items, @intFromEnum(node_idx));
}

/// First index whose `node_idx` is >= `raw_node` in a node-sorted table.
fn sortedNodeSlot(comptime T: type, entries: []const T, raw_node: u32) usize {
    var low: usize = 0;
    var high: usize = entries.len;
    while (low < high) {
        const mid = low + (high - low) / 2;
        if (entries[mid].node_idx < raw_node) {
            low = mid + 1;
        } else {
            high = mid;
        }
    }
    return low;
}

/// Insert or replace `entry` in a node-sorted SafeList. Appends are O(1) when
/// entries arrive in increasing node order (the common case—recording
/// follows node allocation); out-of-order inserts shift the tail.
fn upsertSortedByNode(comptime T: type, list: *collections.SafeList(T), gpa: std.mem.Allocator, entry: T) std.mem.Allocator.Error!void {
    const entries = list.items.items;
    if (entries.len == 0 or entries[entries.len - 1].node_idx < entry.node_idx) {
        _ = try list.append(gpa, entry);
        return;
    }
    const slot = sortedNodeSlot(T, entries, entry.node_idx);
    if (slot < entries.len and entries[slot].node_idx == entry.node_idx) {
        entries[slot] = entry;
        return;
    }
    _ = try list.append(gpa, entry);
    const grown = list.items.items;
    std.mem.copyBackwards(T, grown[slot + 1 ..], grown[slot .. grown.len - 1]);
    grown[slot] = entry;
}

/// Binary-search a node-sorted table for `raw_node`.
fn findSortedByNode(comptime T: type, entries: []const T, raw_node: u32) ?T {
    const slot = sortedNodeSlot(T, entries, raw_node);
    if (slot < entries.len and entries[slot].node_idx == raw_node) return entries[slot];
    return null;
}

/// Record that `node_idx` names a rank-1 polymorphic value scheme. This is
/// checker-produced binding metadata, not a property reconstructed from the
/// solved type graph.
pub fn recordBindingScheme(self: *Self, node_idx: Node.Idx) std.mem.Allocator.Error!void {
    try upsertSortedByNode(
        BindingScheme,
        &self.binding_schemes,
        self.gpa,
        .{ .node_idx = @intFromEnum(node_idx) },
    );
}

/// Whether checking classified `node_idx` as a rank-1 polymorphic value
/// scheme. Imported value resolution uses this exact producer-authored bit to
/// preserve the classification on its local type-graph copy.
pub fn nodeIsBindingScheme(self: *const Self, node_idx: Node.Idx) bool {
    return findSortedByNode(
        BindingScheme,
        self.binding_schemes.items.items,
        @intFromEnum(node_idx),
    ) != null;
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

/// The exact-digit view of a recorded numeral—the input every literal fit
/// and bit computation consumes (src/types/numeral.zig). Borrowed from this
/// env's digit pool.
pub fn exactNumeral(self: *const Self, literal: NumeralLiteral) types_mod.numeral.Exact {
    return .{
        .before = self.numeralDigitsBefore(literal),
        .after = self.numeralDigitsAfter(literal),
        // Saturating: a materialized literal's scale is bounded by the digit
        // recording limit (~158k), far below u32. Only unmaterialized
        // literals (whose digit buffers are empty and whose fit set is
        // forced empty) can carry a u64-sized count.
        .scale = std.math.lossyCast(u32, literal.after_decimal_digit_count),
        .is_negative = literal.isNegative(),
        .is_fractional = literal.after_decimal_digit_count != 0 or literal.hadDecimalPoint(),
    };
}

/// Record the checked `from_numeral` function for a numeric expression.
pub fn recordNumeralDispatchPlan(
    self: *Self,
    node_idx: Node.Idx,
    target_var: TypeVar,
    fn_var: TypeVar,
) std.mem.Allocator.Error!void {
    try self.store.recordLiteralDispatchPlan(node_idx, .numeral, target_var, fn_var);
}

/// Return the checked `from_numeral` function for a numeric expression.
pub fn numeralDispatchPlanForNode(self: *const Self, node_idx: Node.Idx) ?NodeStore.LiteralDispatchPlan {
    const plan = self.store.literalDispatchPlanForNode(node_idx) orelse return null;
    return if (plan.dispatchKind() == .numeral) plan else null;
}

/// Commit checking's exact resolution for a live numeral or quote literal.
pub fn finalizeLiteralDispatchResolution(
    self: *Self,
    node_idx: Node.Idx,
    resolution: NodeStore.LiteralDispatchPlan.Resolution,
) void {
    self.store.finalizeLiteralDispatchResolution(node_idx, resolution);
}

/// Record the checked `from_quote` function for a string literal node.
pub fn recordQuoteDispatchPlan(
    self: *Self,
    node_idx: Node.Idx,
    target_var: TypeVar,
    fn_var: TypeVar,
) std.mem.Allocator.Error!void {
    try self.store.recordLiteralDispatchPlan(node_idx, .quote, target_var, fn_var);
}

/// Record a constrained-scheme use for static-dispatch evidence.
/// `slot_data` is the raw fn `Var` of the discharged constraint for
/// `dispatch_target` slots and 0 for value and nested-function use slots.
pub fn recordSchemeUse(
    self: *Self,
    node_idx: u32,
    slot: SchemeUseRecord.Slot,
    slot_data: u32,
    scheme_root: TypeVar,
    pairs: []const SchemeUsePair,
) std.mem.Allocator.Error!void {
    const pairs_start: u32 = @intCast(self.scheme_use_pairs.items.items.len);
    for (pairs) |pair| {
        _ = try self.scheme_use_pairs.append(self.gpa, pair);
    }
    _ = try self.scheme_uses.append(self.gpa, .{
        .node_idx = node_idx,
        .slot_kind = @intFromEnum(slot),
        .slot_data = slot_data,
        .scheme_root = @intFromEnum(scheme_root),
        .pairs_start = pairs_start,
        .pairs_len = @intCast(pairs.len),
    });
}

/// Record one successfully checked generated codec derivation and its exact
/// internal method callables.
pub fn recordGeneratedCodecDerivation(
    self: *Self,
    kind: GeneratedCodecDerivation.Kind,
    source_constraint_fn_var: TypeVar,
    source_runtime_fn_var: TypeVar,
    source_shape_var: TypeVar,
    source_encoding_var: TypeVar,
    source_state_var: TypeVar,
    source_error_var: TypeVar,
    constraint_fn_var: TypeVar,
    runtime_fn_var: TypeVar,
    shape_var: TypeVar,
    encoding_var: TypeVar,
    state_var: TypeVar,
    error_var: TypeVar,
    calls: []const GeneratedCodecCall,
) std.mem.Allocator.Error!void {
    var existing_index: ?usize = null;
    for (self.generated_codec_derivations.items.items, 0..) |existing, index| {
        if (existing.kind == @intFromEnum(kind) and
            existing.source_constraint_fn_var == @intFromEnum(source_constraint_fn_var))
        {
            existing_index = index;
            break;
        }
    }
    if (existing_index) |index| {
        const existing = self.generated_codec_derivations.items.items[index];
        if (existing.calls_start + existing.calls_len == self.generated_codec_calls.items.items.len) {
            self.generated_codec_calls.items.shrinkRetainingCapacity(existing.calls_start);
        }
    }

    const calls_start: u32 = @intCast(self.generated_codec_calls.items.items.len);
    _ = try self.generated_codec_calls.appendSlice(self.gpa, calls);
    const derivation = GeneratedCodecDerivation{
        .kind = @intFromEnum(kind),
        .source_constraint_fn_var = @intFromEnum(source_constraint_fn_var),
        .source_runtime_fn_var = @intFromEnum(source_runtime_fn_var),
        .source_shape_var = @intFromEnum(source_shape_var),
        .source_encoding_var = @intFromEnum(source_encoding_var),
        .source_state_var = @intFromEnum(source_state_var),
        .source_error_var = @intFromEnum(source_error_var),
        .constraint_fn_var = @intFromEnum(constraint_fn_var),
        .runtime_fn_var = @intFromEnum(runtime_fn_var),
        .shape_var = @intFromEnum(shape_var),
        .encoding_var = @intFromEnum(encoding_var),
        .state_var = @intFromEnum(state_var),
        .error_var = @intFromEnum(error_var),
        .calls_start = calls_start,
        .calls_len = @intCast(calls.len),
    };
    if (existing_index) |index| {
        self.generated_codec_derivations.items.items[index] = derivation;
        return;
    }
    _ = try self.generated_codec_derivations.append(self.gpa, derivation);
}

/// Persist one checker-rejected static-dispatch obligation.
pub fn recordRejectedStaticDispatch(self: *Self, constraint_fn_var: TypeVar) std.mem.Allocator.Error!void {
    _ = try self.rejected_static_dispatches.append(self.gpa, .{
        .constraint_fn_var = @intFromEnum(constraint_fn_var),
    });
}

/// Checker-rejected static-dispatch obligations in production order.
pub fn rejectedStaticDispatches(self: *const Self) []const RejectedStaticDispatch {
    return self.rejected_static_dispatches.items.items;
}

/// Return the checked `from_quote` function for a string literal node.
pub fn quoteDispatchPlanForNode(self: *const Self, node_idx: Node.Idx) ?NodeStore.LiteralDispatchPlan {
    const plan = self.store.literalDispatchPlanForNode(node_idx) orelse return null;
    return if (plan.dispatchKind() == .quote) plan else null;
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

    try upsertSortedByNode(NumericSuffixTarget, &self.numeric_suffix_targets, self.gpa, suffix_target);
}

/// Return the scope-resolved type target for an explicit numeric suffix.
pub fn numericSuffixTargetForNode(self: *const Self, node_idx: Node.Idx) ?NumericSuffixTarget {
    return findSortedByNode(NumericSuffixTarget, self.numeric_suffix_targets.items.items, @intFromEnum(node_idx));
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

/// Reserve one contiguous field-access path plus its enclosing expression.
pub fn startFieldAccessPath(self: *Self, segment_count: u32) std.mem.Allocator.Error!NodeStore.FieldAccessPathBuilder {
    return self.store.startFieldAccessPath(segment_count);
}

/// Append one source-ordered field-access segment to a reserved path.
pub fn appendFieldAccessPathSegmentAssumeCapacity(
    self: *Self,
    builder: NodeStore.FieldAccessPathBuilder,
    segment: CIR.Expr.FieldAccessSegment,
    region: Region,
) CIR.Expr.FieldAccessSegment.Idx {
    const segment_idx = self.store.appendFieldAccessPathSegmentAssumeCapacity(builder, segment, region);
    self.debugAssertArraysInSync();
    return segment_idx;
}

/// Finish a fully populated field-access path.
pub fn finishFieldAccessPath(self: *Self, builder: NodeStore.FieldAccessPathBuilder) CIR.Expr.FieldAccessSegment.Span {
    const span = self.store.finishFieldAccessPath(builder);
    self.debugAssertArraysInSync();
    return span;
}

/// Roll back a field-access path whose construction did not finish.
pub fn rollbackFieldAccessPath(self: *Self, builder: NodeStore.FieldAccessPathBuilder) void {
    self.store.rollbackFieldAccessPath(builder);
    self.debugAssertArraysInSync();
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
            if (std.meta.activeTag(pattern) != .assign) continue; // Skip non-assign patterns (like destructuring)

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
            const tag = std.meta.activeTag(self.store.getStatement(stmt_idx));
            if (tag == .s_alias_decl or tag == .s_nominal_decl or tag == .s_where_alias_decl) {
                has_type_decl = true;
                break;
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
                    .s_where_alias_decl => |where_alias| {
                        const stmt_begin = tree.beginNode();
                        try tree.pushStaticAtom("where-alias");

                        const stmt_region = self.store.getStatementRegion(stmt_idx);
                        try self.appendRegionInfoToSExprTreeFromRegion(tree, stmt_region);

                        try type_writer.write(varFrom(stmt_idx), .one_line);
                        try tree.pushStringPair("type", type_writer.get());

                        const stmt_attrs = tree.beginNode();
                        const header = self.store.getTypeHeader(where_alias.header);
                        try header.pushToSExprTree(self, tree, where_alias.header);
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
                    .s_decl,
                    .s_var,
                    .s_var_uninitialized,
                    .s_reassign,
                    .s_crash,
                    .s_dbg,
                    .s_expr,
                    .s_expect,
                    .s_for,
                    .s_while,
                    .s_infinite_loop,
                    .s_breakable_loop,
                    .s_break,
                    .s_return,
                    .s_import,
                    .s_type_anno,
                    .s_type_var_alias,
                    .s_runtime_error,
                    => continue,
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
    var type_writer = try TypeWriter.initFromParts(self.gpa, &self.types, self.getIdentStore(), null);
    type_writer.setDefaultSourceResolver(self, typeWriterDefaultSource);
    return type_writer;
}

/// Resolve a defaulted field's identity to its default's source snippet for
/// type rendering (design.md "Defaulted Fields"): renderable exactly when
/// the default was declared in THIS module and its source text is a short
/// single line; a foreign or unwieldy default renders as `?? …`.
pub fn typeWriterDefaultSource(ctx: *const anyopaque, id: types_mod.DefaultId) ?[]const u8 {
    const env: *const Self = @ptrCast(@alignCast(ctx));
    if (id.origin_module != env.selfModuleIdentity()) return null;
    const region = env.store.getExprRegion(@as(CIR.Expr.Idx, @enumFromInt(id.expr_node)));
    const source = env.getSourceAll();
    if (region.start.offset > region.end.offset or region.end.offset > source.len) return null;
    const snippet = source[region.start.offset..region.end.offset];
    // Keep type strings readable: long or multi-line defaults render `…`.
    if (snippet.len == 0 or snippet.len > 40) return null;
    if (std.mem.findScalar(u8, snippet, '\n') != null) return null;
    return snippet;
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

// Module identity table --------------------------------------------------
//
// See `base.module_identity` for the identity model. The table maps dense
// env-local ids to 32-byte deep content hashes; `origin_module` fields on
// nominal/alias types in this env's type store are indices into this table.

/// Intern a 32-byte module content identity into this env's identity table,
/// recording `display` (an ident in this env's ident store, used only for
/// diagnostics) when the hash is new. Returns the dense env-local index.
pub fn internModuleIdentity(
    self: *Self,
    hash: *const base.ModuleIdentity.Hash,
    display: Ident.Idx,
) std.mem.Allocator.Error!base.ModuleIdentity.Idx {
    const before = self.module_identities.count();
    const id = try self.module_identities.insert(self.gpa, hash);
    if (id == before) {
        _ = try self.module_identity_displays.append(self.gpa, display);
    }
    std.debug.assert(self.module_identity_displays.len() == self.module_identities.count());
    return @enumFromInt(id);
}

/// Look up a module content identity in this env's table without inserting.
pub fn lookupModuleIdentity(self: *const Self, hash: *const base.ModuleIdentity.Hash) ?base.ModuleIdentity.Idx {
    const id = self.module_identities.lookup(hash) orelse return null;
    return @enumFromInt(id);
}

/// The 32-byte content identity hash for an env-local identity index.
pub fn moduleIdentityHash(self: *const Self, idx: base.ModuleIdentity.Idx) *const base.ModuleIdentity.Hash {
    std.debug.assert(!idx.isNone());
    const bytes = self.module_identities.getText(@intFromEnum(idx));
    std.debug.assert(bytes.len == 32);
    return @ptrCast(bytes.ptr);
}

/// Display ident for an env-local identity index. Diagnostics only—never
/// use for identity decisions.
pub fn moduleIdentityDisplayIdent(self: *const Self, idx: base.ModuleIdentity.Idx) Ident.Idx {
    std.debug.assert(!idx.isNone());
    return self.module_identity_displays.items.items[@intFromEnum(idx)];
}

/// Look up an env-local module identity entry by its env-local display ident.
/// Callers must use the returned identity's content hash for identity decisions.
pub fn moduleIdentityForDisplayIdent(self: *const Self, display: Ident.Idx) ?base.ModuleIdentity.Idx {
    for (self.module_identity_displays.items.items, 0..) |candidate, i| {
        if (candidate.eql(display)) return @enumFromInt(i);
    }
    return null;
}

/// Display text for an env-local identity index. Diagnostics only.
pub fn moduleIdentityDisplayText(self: *const Self, idx: base.ModuleIdentity.Idx) []const u8 {
    const display = self.moduleIdentityDisplayIdent(idx);
    if (display.isNone()) return "";
    return self.getIdent(display);
}

/// This module's own deep content identity hash; null until finalized.
pub fn contentIdentityHash(self: *const Self) ?*const base.ModuleIdentity.Hash {
    if (self.self_module_identity.isNone()) return null;
    return self.moduleIdentityHash(self.self_module_identity);
}

/// This module's own identity table entry. Panics if not yet finalized:
/// callers run after import resolution, where the identity must exist.
pub fn selfModuleIdentity(self: *const Self) base.ModuleIdentity.Idx {
    if (self.self_module_identity.isNone()) {
        std.debug.panic("module content identity not finalized for module '{s}'", .{self.module_name});
    }
    return self.self_module_identity;
}

/// Record this module's deep content identity. Idempotent for an equal hash;
/// panics if a different identity was already recorded.
pub fn setContentIdentity(self: *Self, hash: base.ModuleIdentity.Hash) std.mem.Allocator.Error!void {
    if (self.contentIdentityHash()) |existing| {
        if (!std.mem.eql(u8, existing, &hash)) {
            std.debug.panic("conflicting module content identity for module '{s}'", .{self.module_name});
        }
        return;
    }
    self.self_module_identity = try self.internModuleIdentity(&hash, self.display_module_name_idx);
}

/// Compute and record this module's deep content identity from its resolved
/// direct imports: H(module name, source bytes, import identity hashes).
/// Idempotent. Every imported env must already be finalized—imports are
/// checked (or at least identity-finalized) before their dependents.
pub fn ensureContentIdentity(
    self: *Self,
    imported_envs: []const *const Self,
) std.mem.Allocator.Error!void {
    if (!self.self_module_identity.isNone()) return;

    var import_hashes = try std.ArrayList(base.ModuleIdentity.Hash).initCapacity(self.gpa, imported_envs.len);
    defer import_hashes.deinit(self.gpa);
    for (imported_envs) |imported_env| {
        if (imported_env == @as(*const Self, self)) continue;
        // An import that is this module's own content (same name, same source
        // bytes—e.g. the baked Builtin env while `roc check Builtin.roc`
        // checks the identical source) contributes nothing to the transitive
        // closure; folding it in would make byte-identical modules disagree
        // on identity depending on which copy was loaded first.
        if (std.mem.eql(u8, imported_env.module_name, self.module_name) and
            std.mem.eql(u8, imported_env.common.source, self.common.source))
        {
            continue;
        }
        const import_hash = imported_env.contentIdentityHash() orelse {
            std.debug.panic(
                "module content identity missing for import '{s}' of module '{s}'",
                .{ imported_env.module_name, self.module_name },
            );
        };
        import_hashes.appendAssumeCapacity(import_hash.*);
    }

    const hash = try base.ModuleIdentity.computeDeep(
        self.gpa,
        self.module_name,
        self.common.source,
        import_hashes.items,
    );
    try self.setContentIdentity(hash);
}

/// Registers a method identifier mapping for an explicit owner declaration.
pub fn registerMethodIdentForOwner(self: *Self, owner: CIR.Statement.Idx, method_ident: Ident.Idx, qualified_ident: Ident.Idx) Allocator.Error!void {
    try self.registerMethodIdentForMethodOwner(MethodOwner.init(self.qualified_module_ident, owner), method_ident, qualified_ident);
}

/// Registers a method identifier mapping for an explicit receiver owner declaration.
pub fn registerMethodIdentForMethodOwner(self: *Self, owner: MethodOwner, method_ident: Ident.Idx, qualified_ident: Ident.Idx) Allocator.Error!void {
    const key = MethodKey.init(owner, method_ident);
    try self.method_idents.put(self.gpa, key, qualified_ident);
}

/// Registers a method definition mapping for an explicit owner declaration.
pub fn registerMethodDefForOwner(self: *Self, owner: CIR.Statement.Idx, method_ident: Ident.Idx, binding: MethodBinding) Allocator.Error!void {
    try self.registerMethodDefForMethodOwner(MethodOwner.init(self.qualified_module_ident, owner), method_ident, binding);
}

/// Registers a method definition mapping for an explicit receiver owner declaration.
pub fn registerMethodDefForMethodOwner(self: *Self, owner: MethodOwner, method_ident: Ident.Idx, binding: MethodBinding) Allocator.Error!void {
    const key = MethodKey.init(owner, method_ident);
    try self.method_defs.put(self.gpa, key, binding);
}

/// Appends one complete method entry to the parallel construction tables.
pub fn appendMethodForMethodOwner(
    self: *Self,
    owner: MethodOwner,
    method_ident: Ident.Idx,
    qualified_ident: Ident.Idx,
    binding: MethodBinding,
) Allocator.Error!MethodTableIndex {
    std.debug.assert(self.method_idents.entries.items.len == self.method_defs.entries.items.len);
    const index: MethodTableIndex = @enumFromInt(self.method_idents.entries.items.len);

    try self.method_idents.entries.ensureUnusedCapacity(self.gpa, 1);
    try self.method_defs.entries.ensureUnusedCapacity(self.gpa, 1);
    try self.registerMethodIdentForMethodOwner(owner, method_ident, qualified_ident);
    try self.registerMethodDefForMethodOwner(owner, method_ident, binding);
    return index;
}

/// Replaces the values at one construction-time method table position while
/// preserving its explicit owner-and-name key.
pub fn replaceMethodAt(
    self: *Self,
    index: MethodTableIndex,
    owner: MethodOwner,
    method_ident: Ident.Idx,
    qualified_ident: Ident.Idx,
    binding: MethodBinding,
) void {
    const table_index: usize = @intFromEnum(index);
    const key = MethodKey.init(owner, method_ident);
    std.debug.assert(MethodKey.order(self.method_idents.entries.items[table_index].key, key) == .eq);
    std.debug.assert(MethodKey.order(self.method_defs.entries.items[table_index].key, key) == .eq);
    self.method_idents.entries.items[table_index].value = qualified_ident;
    self.method_defs.entries.items[table_index].value = binding;
}

/// Looks up a qualified method ident for an explicit owner declaration.
pub fn lookupMethodIdentForOwner(self: *Self, owner: CIR.Statement.Idx, method_ident: Ident.Idx) ?Ident.Idx {
    const key = MethodKey.init(MethodOwner.init(self.qualified_module_ident, owner), method_ident);
    return self.method_idents.get(self.gpa, key);
}

/// Looks up a qualified method ident in finalized tables for an explicit owner declaration.
pub fn lookupMethodIdentForOwnerConst(self: *const Self, owner: CIR.Statement.Idx, method_ident: Ident.Idx) ?Ident.Idx {
    return self.lookupMethodIdentForMethodOwnerConst(MethodOwner.init(self.qualified_module_ident, owner), method_ident);
}

/// Looks up a qualified method ident in finalized tables for an explicit receiver owner declaration.
pub fn lookupMethodIdentForMethodOwnerConst(self: *const Self, owner: MethodOwner, method_ident: Ident.Idx) ?Ident.Idx {
    const key = MethodKey.init(owner, method_ident);
    return self.method_idents.getFinalized(key);
}

/// Looks up method type/check metadata in finalized tables for an explicit owner declaration.
pub fn lookupMethodBindingForOwnerConst(self: *const Self, owner: CIR.Statement.Idx, method_ident: Ident.Idx) ?MethodBinding {
    return self.lookupMethodBindingForMethodOwnerConst(MethodOwner.init(self.qualified_module_ident, owner), method_ident);
}

/// Looks up method type/check metadata in finalized tables for an explicit receiver owner declaration.
pub fn lookupMethodBindingForMethodOwnerConst(self: *const Self, owner: MethodOwner, method_ident: Ident.Idx) ?MethodBinding {
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
    return self.lookupMethodBindingFromOwnerAndMethodEnvsConst(source_env, source_decl, source_env, method_ident);
}

/// Looks up method metadata using a type declaration owner and a method ident
/// that come from different source environments.
pub fn lookupMethodBindingFromTwoEnvsAndDeclConst(
    self: *const Self,
    source_decl: ?u32,
    method_source_env: *const Self,
    method_ident: Ident.Idx,
) ?MethodBinding {
    return self.lookupMethodBindingFromOwnerAndMethodEnvsConst(self, source_decl, method_source_env, method_ident);
}

/// Looks up method metadata using an owner declaration and method ident that may
/// both come from different source environments.
pub fn lookupMethodBindingFromOwnerAndMethodEnvsConst(
    self: *const Self,
    owner_source_env: *const Self,
    source_decl: ?u32,
    method_source_env: *const Self,
    method_ident: Ident.Idx,
) ?MethodBinding {
    const method_name = method_source_env.getIdent(method_ident);
    const owner_module_name = owner_source_env.getIdent(owner_source_env.qualified_module_ident);

    const local_method_ident = self.common.findIdent(method_name) orelse return null;
    const local_owner_module_ident = self.common.findIdent(owner_module_name) orelse return null;
    const owner: CIR.Statement.Idx = @enumFromInt(source_decl orelse return null);

    return self.lookupMethodBindingForMethodOwnerConst(MethodOwner.init(local_owner_module_ident, owner), local_method_ident);
}

/// Returns the line start positions for source code position mapping.
/// Each element represents the byte offset where a new line begins.
pub fn getLineStarts(self: *const Self) []const u32 {
    return self.common.getLineStartsAll();
}
