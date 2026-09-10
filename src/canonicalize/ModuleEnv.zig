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

/// Durable lifecycle of the canonical/typechecked ModuleEnv. A source env is
/// consumed by exactly one checker run; checked cache hits are validated and
/// consumed as immutable inputs rather than repaired or checked again.
pub const TypecheckState = enum(u8) {
    canonical_unchecked,
    checking,
    checked_file,
    checked_repl,
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
    // Synthetic identifier for polarity-deferred tag union extensions in alias
    // declaration bodies (see types.polarity_var_text)
    polarity_var: Ident.Idx,

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
            // Synthetic identifier for polarity-deferred tag union extensions
            .polarity_var = try common.insertIdent(gpa, Ident.for_text(types_mod.polarity_var_text)),
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
            // Synthetic identifier for polarity-deferred tag union extensions
            .polarity_var = common.findIdent(types_mod.polarity_var_text) orelse unreachable,
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

/// One exact successful lookup in the finalized method-definition table.
/// `entry_index` is provider-local and remains stable for the lifetime and
/// serialized form of the canonical table.
pub const MethodBindingEntry = struct {
    entry_index: u32,
    binding: MethodBinding,
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
pub const ForLoopDispatchOutcome = extern struct {
    kind: u32,
    first: u32,
    second: u32,

    pub const none = std.math.maxInt(u32);
    pub const Kind = enum(u32) {
        live_constraint,
        rejected_before_constraint,
    };

    pub fn liveConstraint(constraint_index: u32, anchor_index: u32) @This() {
        return .{
            .kind = @intFromEnum(Kind.live_constraint),
            .first = constraint_index,
            .second = anchor_index,
        };
    }

    pub fn rejectedBeforeConstraint(rejected_static_dispatch_index: u32) @This() {
        return .{
            .kind = @intFromEnum(Kind.rejected_before_constraint),
            .first = rejected_static_dispatch_index,
            .second = 0,
        };
    }

    pub fn decodedKind(self: @This()) ?Kind {
        return std.enums.fromInt(Kind, self.kind);
    }

    pub fn constraintIndex(self: @This()) ?u32 {
        return if (self.decodedKind() == .live_constraint) self.first else null;
    }

    pub fn anchorIndex(self: @This()) ?u32 {
        return if (self.decodedKind() == .live_constraint) self.second else null;
    }

    pub fn rejectionIndex(self: @This()) ?u32 {
        return if (self.decodedKind() == .rejected_before_constraint) self.first else null;
    }

    pub fn hasCanonicalTags(self: @This()) bool {
        return switch (self.decodedKind() orelse return false) {
            .live_constraint => self.first != none and self.second != none,
            .rejected_before_constraint => self.first != none and self.second == 0,
        };
    }
};

pub const ForLoopDispatchPlan = extern struct {
    node_idx: u32,
    pattern_idx: u32,
    iterable_idx: u32,
    iterator_var: u32,
    step_var: u32,
    iter_fn_var: u32,
    next_fn_var: u32,
    step_topology: IteratorStepTopology,
    iter_outcome: ForLoopDispatchOutcome,
    next_outcome: ForLoopDispatchOutcome,

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
/// for an instantiation—the source-to-fresh var relation needed by its slot.
/// Ordinary evidence edges retain constrained vars; a where-method use retains
/// the instantiator's complete structural map. Shared monomorphic edges have no
/// copy pairs. Checked-artifact construction resolves the recorded vars after
/// checking settles to decide how each callee dispatch requirement was
/// satisfied at this site.
pub const SchemeUseRecord = extern struct {
    node_idx: u32,
    /// `Slot`—distinguishes several schemes instantiated at one node (a value
    /// use, an expression-position function stored as a value, the target of
    /// a dispatch constraint, or a per-use where-method signature copy).
    slot_kind: u32,
    /// For `dispatch_target` slots, the raw fn `Var` of the constraint whose
    /// discharge instantiated this scheme—unique per constraint
    /// instantiation, so nested evidence chains resolve without ambiguity.
    /// For `where_method_use`, the raw fn `Var` of the body dispatch whose
    /// callable instantiated the where-method signature. 0 for value and
    /// nested-function use slots (keyed by `node_idx` instead).
    slot_data: u32,
    /// The scheme root `Var` used at this edge. For imported schemes this is
    /// the pristine local copy; for shared uses it is the in-flight local root.
    scheme_root: u32,
    /// Range into `scheme_use_pairs`.
    pairs_start: u32,
    pairs_len: u32,
    /// Range into `where_method_marker_uses`. Non-where slots are empty.
    marker_uses_start: u32,
    marker_uses_len: u32,

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
        /// One body dispatch's per-use instantiation of its where-method
        /// signature. `slot_data` is the body's constraint callable and
        /// `scheme_root` is the pristine where-method signature callable.
        /// The use has no child dispatch requirements; this record deliberately
        /// relates the two callable identities for checked-plan construction.
        where_method_use,
        /// A monomorphic reference to an in-flight unannotated definition.
        /// The edge shares the definition's vars, so its record has no copy
        /// pairs but still names the exact scheme root used by checking.
        shared_value_use,
        /// The exact method scheme inspected while validating one
        /// `Str.inspect` call argument. `node_idx` is the call and
        /// `slot_data` is its sole argument expression.
        inspect_method,
    };
};

/// Result positions for which checking authorized a closed implementation
/// row to serve the wider callable copied for one where-method body use.
pub const ResultRowWidening = packed struct(u8) {
    direct: bool = false,
    try_ok: bool = false,
    try_err: bool = false,
    _reserved: u5 = 0,

    pub fn isEmpty(self: @This()) bool {
        return @as(u8, @bitCast(self)) == 0;
    }

    pub fn hasValidShape(self: @This()) bool {
        return self._reserved == 0 and (!self.direct or (!self.try_ok and !self.try_err));
    }

    pub fn bits(self: @This()) u32 {
        return @as(u8, @bitCast(self));
    }

    pub fn fromBits(raw: u32) ?@This() {
        if (raw > std.math.maxInt(u8) or (raw & 0xf8) != 0) return null;
        const decoded: @This() = @bitCast(@as(u8, @intCast(raw)));
        return if (decoded.hasValidShape()) decoded else null;
    }

    pub fn merge(self: *@This(), other: @This()) void {
        if (!self.hasValidShape() or !other.hasValidShape()) {
            std.debug.panic("invalid result-row widening shape", .{});
        }
        self.direct = self.direct or other.direct;
        self.try_ok = self.try_ok or other.try_ok;
        self.try_err = self.try_err or other.try_err;
        if (!self.hasValidShape()) {
            std.debug.panic("incompatible result-row widening positions", .{});
        }
    }
};

/// One polarity marker opened while a where-method signature is instantiated
/// for a body use. Source and fresh identities come directly from that copy;
/// the path was authored during the same walk and names the logical tag row.
pub const WhereMethodMarkerUse = extern struct {
    /// Exact same-ModuleEnv source occurrence of this marker. A reusable
    /// where-alias declaration is rebound to the referencing annotation and
    /// expanded method name, so independent references never share widening
    /// facts. This key is a durable source witness and is never cross-copied.
    producer_owner_node: u32,
    producer_where_node: u32,
    producer_method_name: u32,
    source_row_var: u32,
    source_tail_var: u32,
    fresh_row_var: u32,
    fresh_tail_var: u32,
    written_tag_count: u32,
    position: u32,
    path_start: u32,
    path_len: u32,

    pub const SafeList = collections.SafeList(@This());
};

/// Exact source occurrence that produced one where-method requirement. The
/// marker-contract range may be canonically empty. The key is owned by the
/// referencing annotation, including when the where clause names a reusable
/// alias, and never crosses a module boundary. `source_ordinal` is the dense
/// position in that owner's complete flattened direct-and-alias-expanded source
/// sequence. `retained_constraint_index` identifies the canonical method-key
/// constraint after duplicate written signatures were related; the source
/// range preserves this occurrence's distinct producer-authored paths.
pub const WhereMethodSource = extern struct {
    owner_node: u32,
    where_node: u32,
    method_ident: u32,
    source_ordinal: u32,
    retained_constraint_index: u32,
    source_contracts_start: u32,
    source_contracts_len: u32,

    pub const SafeList = collections.SafeList(@This());
};

/// One exact source-to-destination variable pair in a where-marker copy proof.
/// A copy step owns a complete relation, sorted and deduplicated strictly by
/// the full `(source_var, destination_var)` tuple. Later unification may make
/// distinct raw source occurrences share a resolved source (or destinations
/// share a resolved destination), so neither coordinate is independently
/// unique. The immutable occurrence pool separately enforces the copy-time
/// function: one raw source id cannot map to two raw destination ids.
/// `discovery_depth` is the canonical shortest distance from the step's root
/// under the exact replay relation. The checked-boundary transaction computes
/// it after pruning; draft checker rows use `maxInt(u32)` except for the root.
/// Each non-root row also names the lexicographically first shortest incoming
/// edge by pair offset and that source descriptor's finite edge ordinal. The
/// root uses `maxInt(u32)` for both predecessor fields. Admission replays and
/// locates exact tuples in the relation, proving reachability and canonical shortest
/// discovery without allocating.
pub const WhereMarkerCopyPair = extern struct {
    source_var: u32,
    destination_var: u32,
    discovery_depth: u32,
    predecessor_pair_offset: u32,
    predecessor_edge_ordinal: u32,

    pub const SafeList = collections.SafeList(@This());
};

/// One immutable copy-time occurrence in a where-marker copy proof. Multiple
/// raw source/destination occurrences may resolve to the same canonical pair;
/// each row preserves the exact producer request and names that pair by an
/// offset relative to the owning copy step.
pub const WhereMarkerCopyOccurrence = extern struct {
    raw_source_var: u32,
    raw_destination_var: u32,
    canonical_pair_offset: u32,

    pub const SafeList = collections.SafeList(@This());

    /// Canonical owner-local order groups every raw occurrence that projects
    /// to one pair, then orders the immutable raw request ids.
    pub fn canonicalLessThan(a: @This(), b: @This()) bool {
        if (a.canonical_pair_offset != b.canonical_pair_offset) {
            return a.canonical_pair_offset < b.canonical_pair_offset;
        }
        if (a.raw_source_var != b.raw_source_var) {
            return a.raw_source_var < b.raw_source_var;
        }
        return a.raw_destination_var < b.raw_destination_var;
    }
};

/// Exact movement of one immutable static-dispatch constraint occurrence into
/// a newly appended merged occurrence. The owned offset slice is total over
/// the source constraint's marker range: offset `i` stores the destination-
/// local offset of source-local marker `i`. This evidence authenticates carried
/// destination metadata without changing the identity of the source occurrence.
pub const WhereMarkerConstraintMove = extern struct {
    source_constraint_index: u32,
    destination_constraint_index: u32,
    offsets_start: u32,
    offsets_len: u32,

    pub const SafeList = collections.SafeList(@This());
};

/// One exact for-clause substitution at a platform-requirement copy boundary.
/// These are locators rather than duplicated type claims. Admission derives
/// both platform roots and the alias-source identity from the admitted
/// platform statement, derives the app declaration from the fresh app CIR,
/// and obtains the destination occurrence from the referenced local
/// instantiation step.
pub const WhereMarkerPlatformSubstitution = extern struct {
    platform_alias_statement: u32,
    app_declaration_node: u32,
    app_instantiation_step: u32,

    pub const SafeList = collections.SafeList(@This());
};

/// One default decision made by the checker for an exact receiver constraint
/// range. `contributors_start/len` owns the nonempty, strictly canonical range
/// of every exact producer token which contributed that receiver/range. A
/// default-method copy cites this row and a relative constraint offset instead
/// of reconstructing defaulting from settled types or choosing one arbitrary
/// contributor.
pub const DefaultDecision = extern struct {
    receiver_var: u32,
    constraints_start: u32,
    constraints_len: u32,
    target: u32,
    contributors_start: u32,
    contributors_len: u32,

    pub const SafeList = collections.SafeList(@This());

    pub const Target = enum(u32) {
        dec,
        str,
    };

    pub const DriverKind = DefaultDecisionContributor.DriverKind;
    pub const LiteralKind = DefaultDecisionContributor.LiteralKind;
    pub const LiteralOccurrenceKind = DefaultDecisionContributor.LiteralOccurrenceKind;

    pub fn decodedTarget(self: @This()) ?Target {
        return std.enums.fromInt(Target, self.target);
    }
};

/// One exact producer token contributing to a default decision. The closed
/// union is represented as canonical inactive zero fields. Canonical order is
/// the stable semantic key `(driver_kind, copy_step, pair_offset, source_node,
/// occurrence_kind, literal_kind)`; a decision range is strictly increasing,
/// so duplicates and order-dependent first-driver choices are impossible.
pub const DefaultDecisionContributor = extern struct {
    driver_kind: u32,
    driver_copy_step: u32,
    driver_pair_offset: u32,
    literal_source_node: u32,
    literal_occurrence_kind: u32,
    literal_kind: u32,

    pub const SafeList = collections.SafeList(@This());

    pub const DriverKind = enum(u32) {
        instantiation_copy,
        literal_creation,
    };

    pub const LiteralKind = enum(u32) {
        numeral,
        quote,
        interpolation,
    };

    pub const LiteralOccurrenceKind = enum(u32) {
        expr,
        pattern,
    };

    pub fn decodedDriverKind(self: @This()) ?DriverKind {
        return std.enums.fromInt(DriverKind, self.driver_kind);
    }

    pub fn decodedLiteralKind(self: @This()) ?LiteralKind {
        return std.enums.fromInt(LiteralKind, self.literal_kind);
    }

    pub fn decodedLiteralOccurrenceKind(self: @This()) ?LiteralOccurrenceKind {
        return std.enums.fromInt(LiteralOccurrenceKind, self.literal_occurrence_kind);
    }

    pub fn canonicalLessThan(a: @This(), b: @This()) bool {
        inline for (.{
            "driver_kind",
            "driver_copy_step",
            "driver_pair_offset",
            "literal_source_node",
            "literal_occurrence_kind",
            "literal_kind",
        }) |field| {
            if (@field(a, field) != @field(b, field)) {
                return @field(a, field) < @field(b, field);
            }
        }
        return false;
    }
};

/// One completed selected-dispatch event whose imported method scheme carried
/// where-marker authority. Rows are appended in the producer's dispatch-target
/// SchemeUse order. Every row names the mandatory cached cross-module support
/// root in `root_copy_step` and its own exact local `use_copy_step`. The root's
/// finite origin records the cache's actual first consumer; it names this
/// decision only when selected dispatch itself seeded that cache entry.
///
/// `constraint_evidence_start/len` is the exact canonical tagged handle set
/// attached to the selected constraint occurrence. The selected-receiver
/// subset must be nonempty. `constraint_moves_start/len` owns the complete
/// producer transition subgraph from that subset's creation occurrences to
/// the constraint. The receiver-owner fields preserve the exact nominal or
/// alias declaration selected before target checking can retire or poison the
/// live receiver descriptor. `provider_method_entry_index` is the exact
/// finalized row returned by that owner-key lookup, so another owner key which
/// publishes the same binding cannot substitute. A reserved row is never a
/// valid checked-module artifact.
pub const SelectedMethodDecision = extern struct {
    state: u32,
    scheme_use_index: u32,
    constraint_index: u32,
    receiver_var: u32,
    provider_dependency_index: u32,
    provider_type_node: u32,
    provider_def: u32,
    constraint_evidence_start: u32,
    constraint_evidence_len: u32,
    constraint_moves_start: u32,
    constraint_moves_len: u32,
    root_copy_step: u32,
    use_copy_step: u32,
    receiver_owner_origin_module: u32,
    receiver_owner_source_decl: u32,
    provider_method_entry_index: u32,

    pub const SafeList = collections.SafeList(@This());
    pub const none = std.math.maxInt(u32);

    pub const State = enum(u32) {
        reserved,
        complete,
    };

    pub const ReceiverAnchorKind = enum(u32) {
        dispatch_expr,
        literal_conversion,
        interpolation,
        pattern_literal_equality,
        for_loop_dispatch,
        negated_equality_not,
        copied_constraint,
    };

    pub const LiteralConversionSlot = enum(u32) {
        numeral,
        quote,
    };

    pub const ForLoopSlot = enum(u32) {
        iter,
        next,
    };

    pub fn decodedState(self: @This()) ?State {
        return std.enums.fromInt(State, self.state);
    }
};

/// One finite producer-authored origin for a selectable receiver constraint.
/// Direct creation kinds name an exact CIR node/slot; copied constraints name
/// the exact local copy step, receiver occurrence, and constraint-copy pair.
/// Unused coordinates are `SelectedMethodDecision.none`.
pub const SelectedReceiverAnchor = extern struct {
    constraint_index: u32,
    receiver_var: u32,
    kind: u32,
    node: u32,
    slot: u32,
    copy_step: u32,
    receiver_occurrence_offset: u32,
    constraint_pair_offset: u32,

    pub const SafeList = collections.SafeList(@This());

    pub fn decodedKind(self: @This()) ?SelectedMethodDecision.ReceiverAnchorKind {
        return std.enums.fromInt(SelectedMethodDecision.ReceiverAnchorKind, self.kind);
    }
};

/// Exact component of a copied static-dispatch constraint. The containing
/// `DispatchSettlementSource.copied_constraint` arm supplies the component's
/// receiver/function role: receiver references canonically zero the
/// role-inactive constraint-pair word, while function references name the
/// exact owner-relative constraint-copy pair. Origin-specific inactive words
/// are likewise always zero.
pub const CopiedConstraintComponentRef = extern struct {
    kind: u32,
    copy_step_index: u32,
    occurrence_offset: u32,
    constraint_pair_offset: u32,
    requirement_ordinal: u32,
    binding_root_step: u32,

    pub const none = std.math.maxInt(u32);

    pub const Kind = enum(u32) {
        root_graph,
        scheme_requirement,
        binding_codec_receiver,
        binding_codec_function,
    };

    pub const Role = enum {
        receiver,
        function,
    };

    pub fn rootGraphReceiver(copy_step_index: u32, occurrence_offset: u32) @This() {
        return .{
            .kind = @intFromEnum(Kind.root_graph),
            .copy_step_index = copy_step_index,
            .occurrence_offset = occurrence_offset,
            .constraint_pair_offset = 0,
            .requirement_ordinal = 0,
            .binding_root_step = 0,
        };
    }

    pub fn rootGraphFunction(
        copy_step_index: u32,
        occurrence_offset: u32,
        constraint_pair_offset: u32,
    ) @This() {
        return .{
            .kind = @intFromEnum(Kind.root_graph),
            .copy_step_index = copy_step_index,
            .occurrence_offset = occurrence_offset,
            .constraint_pair_offset = constraint_pair_offset,
            .requirement_ordinal = 0,
            .binding_root_step = 0,
        };
    }

    pub fn schemeRequirementReceiver(
        copy_step_index: u32,
        occurrence_offset: u32,
        requirement_ordinal: u32,
    ) @This() {
        return .{
            .kind = @intFromEnum(Kind.scheme_requirement),
            .copy_step_index = copy_step_index,
            .occurrence_offset = occurrence_offset,
            .constraint_pair_offset = 0,
            .requirement_ordinal = requirement_ordinal,
            .binding_root_step = 0,
        };
    }

    pub fn schemeRequirementFunction(
        copy_step_index: u32,
        occurrence_offset: u32,
        constraint_pair_offset: u32,
        requirement_ordinal: u32,
    ) @This() {
        return .{
            .kind = @intFromEnum(Kind.scheme_requirement),
            .copy_step_index = copy_step_index,
            .occurrence_offset = occurrence_offset,
            .constraint_pair_offset = constraint_pair_offset,
            .requirement_ordinal = requirement_ordinal,
            .binding_root_step = 0,
        };
    }

    pub fn bindingCodecReceiver(
        copy_step_index: u32,
        occurrence_offset: u32,
        requirement_ordinal: u32,
        binding_root_step: u32,
    ) @This() {
        return .{
            .kind = @intFromEnum(Kind.binding_codec_receiver),
            .copy_step_index = copy_step_index,
            .occurrence_offset = occurrence_offset,
            .constraint_pair_offset = 0,
            .requirement_ordinal = requirement_ordinal,
            .binding_root_step = binding_root_step,
        };
    }

    pub fn bindingCodecFunction(
        copy_step_index: u32,
        occurrence_offset: u32,
        constraint_pair_offset: u32,
        requirement_ordinal: u32,
        binding_root_step: u32,
    ) @This() {
        return .{
            .kind = @intFromEnum(Kind.binding_codec_function),
            .copy_step_index = copy_step_index,
            .occurrence_offset = occurrence_offset,
            .constraint_pair_offset = constraint_pair_offset,
            .requirement_ordinal = requirement_ordinal,
            .binding_root_step = binding_root_step,
        };
    }

    pub fn decodedKind(self: @This()) ?Kind {
        return std.enums.fromInt(Kind, self.kind);
    }

    /// Validate the closed origin arm and every role/origin-inactive word.
    /// Referenced-row bounds and paired receiver/function identity are checked
    /// by terminal settlement admission.
    pub fn hasCanonicalTags(self: @This(), role: Role) bool {
        const kind = self.decodedKind() orelse return false;
        if (self.copy_step_index == none or self.occurrence_offset == none) return false;
        switch (role) {
            .receiver => if (self.constraint_pair_offset != 0) return false,
            .function => if (self.constraint_pair_offset == none) return false,
        }

        return switch (kind) {
            .root_graph => self.requirement_ordinal == 0 and self.binding_root_step == 0,
            .scheme_requirement => self.requirement_ordinal != none and
                self.binding_root_step == 0,
            .binding_codec_receiver => role == .receiver and
                self.requirement_ordinal != none and self.binding_root_step != none,
            .binding_codec_function => role == .function and
                self.requirement_ordinal != none and self.binding_root_step != none,
        };
    }

    pub fn canonicalLessThan(left: @This(), right: @This()) bool {
        inline for (.{
            "kind",
            "copy_step_index",
            "occurrence_offset",
            "constraint_pair_offset",
            "requirement_ordinal",
            "binding_root_step",
        }) |field| {
            if (@field(left, field) != @field(right, field)) {
                return @field(left, field) < @field(right, field);
            }
        }
        return false;
    }
};

/// Immutable creation plan for one initial static-dispatch obligation. The
/// tag selects one fixed thirteen-word payload; every word outside the active
/// arm's exact semantic coordinate tuple is zero. These rows are the source
/// pool owned by terminal `DispatchSettlementEvent`s, so they remain valid
/// even after checking retires or rewrites their original CIR nodes.
pub const DispatchSettlementSource = extern struct {
    kind: u32,
    payload: Payload,

    pub const none = std.math.maxInt(u32);
    pub const SafeList = collections.SafeList(@This());

    pub const Kind = enum(u32) {
        dispatch_expr,
        literal_conversion,
        interpolation,
        pattern_literal_equality,
        negated_equality_not,
        for_loop_dispatch,
        where_requirement,
        copied_constraint,
    };

    pub const OriginalNodeKind = enum(u32) {
        dispatch_call,
        type_dispatch_call,
        method_eq_is_eq,
    };

    /// Closed source snapshot of `NodeStore.LiteralDispatchPlan.Kind`.
    pub const LiteralKind = enum(u32) {
        numeral = @intFromEnum(NodeStore.LiteralDispatchPlan.Kind.numeral),
        quote = @intFromEnum(NodeStore.LiteralDispatchPlan.Kind.quote),
    };

    /// Closed source snapshot of `SelectedMethodDecision.ForLoopSlot`.
    pub const ForLoopSlot = enum(u32) {
        iter = @intFromEnum(SelectedMethodDecision.ForLoopSlot.iter),
        next = @intFromEnum(SelectedMethodDecision.ForLoopSlot.next),
    };

    pub const DispatchExpr = extern struct {
        anchor_index: u32,
        node_index: u32,
        original_node_kind: u32,
        receiver_var: u32,
        constraint_fn_var: u32,
        method_ident: u32,
        reserved: [7]u32 = [_]u32{0} ** 7,
    };

    pub const LiteralConversion = extern struct {
        anchor_index: u32,
        node_index: u32,
        target_var: u32,
        constraint_fn_var: u32,
        literal_kind: u32,
        reserved: [8]u32 = [_]u32{0} ** 8,
    };

    pub const Interpolation = extern struct {
        anchor_index: u32,
        node_index: u32,
        dispatcher_var: u32,
        constraint_fn_var: u32,
        reserved: [9]u32 = [_]u32{0} ** 9,
    };

    pub const PatternLiteralEquality = extern struct {
        anchor_index: u32,
        pattern_index: u32,
        literal_kind: u32,
        receiver_var: u32,
        is_eq_fn_var: u32,
        reserved: [8]u32 = [_]u32{0} ** 8,
    };

    pub const NegatedEqualityNot = extern struct {
        anchor_index: u32,
        node_index: u32,
        equality_constraint_index: u32,
        receiver_var: u32,
        not_fn_var: u32,
        reserved: [8]u32 = [_]u32{0} ** 8,
    };

    pub const ForLoopDispatch = extern struct {
        anchor_index: u32,
        plan_index: u32,
        slot: u32,
        reserved: [10]u32 = [_]u32{0} ** 10,
    };

    pub const WhereRequirement = extern struct {
        source_index: u32,
        reserved: [12]u32 = [_]u32{0} ** 12,
    };

    pub const CopiedConstraint = extern struct {
        anchor_index: u32,
        receiver_component_ref: CopiedConstraintComponentRef,
        function_component_ref: CopiedConstraintComponentRef,
    };

    pub const Payload = extern union {
        pub const serialized_portable_extern_union = true;

        dispatch_expr: DispatchExpr,
        literal_conversion: LiteralConversion,
        interpolation: Interpolation,
        pattern_literal_equality: PatternLiteralEquality,
        negated_equality_not: NegatedEqualityNot,
        for_loop_dispatch: ForLoopDispatch,
        where_requirement: WhereRequirement,
        copied_constraint: CopiedConstraint,
    };

    pub fn dispatchExpr(
        anchor_index: u32,
        node_index: u32,
        original_node_kind: OriginalNodeKind,
        receiver_var: u32,
        constraint_fn_var: u32,
        method_ident: u32,
    ) @This() {
        return .{
            .kind = @intFromEnum(Kind.dispatch_expr),
            .payload = .{ .dispatch_expr = .{
                .anchor_index = anchor_index,
                .node_index = node_index,
                .original_node_kind = @intFromEnum(original_node_kind),
                .receiver_var = receiver_var,
                .constraint_fn_var = constraint_fn_var,
                .method_ident = method_ident,
            } },
        };
    }

    pub fn literalConversion(
        anchor_index: u32,
        node_index: u32,
        target_var: u32,
        constraint_fn_var: u32,
        literal_kind: LiteralKind,
    ) @This() {
        return .{
            .kind = @intFromEnum(Kind.literal_conversion),
            .payload = .{ .literal_conversion = .{
                .anchor_index = anchor_index,
                .node_index = node_index,
                .target_var = target_var,
                .constraint_fn_var = constraint_fn_var,
                .literal_kind = @intFromEnum(literal_kind),
            } },
        };
    }

    pub fn interpolation(
        anchor_index: u32,
        node_index: u32,
        dispatcher_var: u32,
        constraint_fn_var: u32,
    ) @This() {
        return .{
            .kind = @intFromEnum(Kind.interpolation),
            .payload = .{ .interpolation = .{
                .anchor_index = anchor_index,
                .node_index = node_index,
                .dispatcher_var = dispatcher_var,
                .constraint_fn_var = constraint_fn_var,
            } },
        };
    }

    pub fn patternLiteralEquality(
        anchor_index: u32,
        pattern_index: u32,
        literal_kind: LiteralKind,
        receiver_var: u32,
        is_eq_fn_var: u32,
    ) @This() {
        return .{
            .kind = @intFromEnum(Kind.pattern_literal_equality),
            .payload = .{ .pattern_literal_equality = .{
                .anchor_index = anchor_index,
                .pattern_index = pattern_index,
                .literal_kind = @intFromEnum(literal_kind),
                .receiver_var = receiver_var,
                .is_eq_fn_var = is_eq_fn_var,
            } },
        };
    }

    pub fn negatedEqualityNot(
        anchor_index: u32,
        node_index: u32,
        equality_constraint_index: u32,
        receiver_var: u32,
        not_fn_var: u32,
    ) @This() {
        return .{
            .kind = @intFromEnum(Kind.negated_equality_not),
            .payload = .{ .negated_equality_not = .{
                .anchor_index = anchor_index,
                .node_index = node_index,
                .equality_constraint_index = equality_constraint_index,
                .receiver_var = receiver_var,
                .not_fn_var = not_fn_var,
            } },
        };
    }

    pub fn forLoopDispatch(
        anchor_index: u32,
        plan_index: u32,
        slot: ForLoopSlot,
    ) @This() {
        return .{
            .kind = @intFromEnum(Kind.for_loop_dispatch),
            .payload = .{ .for_loop_dispatch = .{
                .anchor_index = anchor_index,
                .plan_index = plan_index,
                .slot = @intFromEnum(slot),
            } },
        };
    }

    pub fn whereRequirement(source_index: u32) @This() {
        return .{
            .kind = @intFromEnum(Kind.where_requirement),
            .payload = .{ .where_requirement = .{ .source_index = source_index } },
        };
    }

    pub fn copiedConstraint(
        anchor_index: u32,
        receiver_component_ref: CopiedConstraintComponentRef,
        function_component_ref: CopiedConstraintComponentRef,
    ) @This() {
        return .{
            .kind = @intFromEnum(Kind.copied_constraint),
            .payload = .{ .copied_constraint = .{
                .anchor_index = anchor_index,
                .receiver_component_ref = receiver_component_ref,
                .function_component_ref = function_component_ref,
            } },
        };
    }

    pub fn decodedKind(self: @This()) ?Kind {
        return std.enums.fromInt(Kind, self.kind);
    }

    pub fn decodedDispatchExpr(self: @This()) ?DispatchExpr {
        return if (self.decodedKind() == .dispatch_expr) self.payload.dispatch_expr else null;
    }

    pub fn decodedLiteralConversion(self: @This()) ?LiteralConversion {
        return if (self.decodedKind() == .literal_conversion)
            self.payload.literal_conversion
        else
            null;
    }

    pub fn decodedInterpolation(self: @This()) ?Interpolation {
        return if (self.decodedKind() == .interpolation) self.payload.interpolation else null;
    }

    pub fn decodedPatternLiteralEquality(self: @This()) ?PatternLiteralEquality {
        return if (self.decodedKind() == .pattern_literal_equality)
            self.payload.pattern_literal_equality
        else
            null;
    }

    pub fn decodedNegatedEqualityNot(self: @This()) ?NegatedEqualityNot {
        return if (self.decodedKind() == .negated_equality_not)
            self.payload.negated_equality_not
        else
            null;
    }

    pub fn decodedForLoopDispatch(self: @This()) ?ForLoopDispatch {
        return if (self.decodedKind() == .for_loop_dispatch)
            self.payload.for_loop_dispatch
        else
            null;
    }

    pub fn decodedWhereRequirement(self: @This()) ?WhereRequirement {
        return if (self.decodedKind() == .where_requirement)
            self.payload.where_requirement
        else
            null;
    }

    pub fn decodedCopiedConstraint(self: @This()) ?CopiedConstraint {
        return if (self.decodedKind() == .copied_constraint)
            self.payload.copied_constraint
        else
            null;
    }

    pub fn decodedOriginalNodeKind(self: @This()) ?OriginalNodeKind {
        const source = self.decodedDispatchExpr() orelse return null;
        return std.enums.fromInt(OriginalNodeKind, source.original_node_kind);
    }

    pub fn decodedLiteralKind(self: @This()) ?LiteralKind {
        const source = self.decodedLiteralConversion() orelse return null;
        return std.enums.fromInt(LiteralKind, source.literal_kind);
    }

    pub fn decodedPatternLiteralKind(self: @This()) ?LiteralKind {
        const source = self.decodedPatternLiteralEquality() orelse return null;
        return std.enums.fromInt(LiteralKind, source.literal_kind);
    }

    pub fn decodedForLoopSlot(self: @This()) ?ForLoopSlot {
        const source = self.decodedForLoopDispatch() orelse return null;
        return std.enums.fromInt(ForLoopSlot, source.slot);
    }

    /// Return the source's selected-receiver anchor. A where requirement uses
    /// its own evidence-handle namespace and therefore has no anchor.
    pub fn primaryAnchorIndex(self: @This()) ?u32 {
        const kind = self.decodedKind() orelse return null;
        return switch (kind) {
            .dispatch_expr => self.payload.dispatch_expr.anchor_index,
            .literal_conversion => self.payload.literal_conversion.anchor_index,
            .interpolation => self.payload.interpolation.anchor_index,
            .pattern_literal_equality => self.payload.pattern_literal_equality.anchor_index,
            .negated_equality_not => self.payload.negated_equality_not.anchor_index,
            .for_loop_dispatch => self.payload.for_loop_dispatch.anchor_index,
            .where_requirement => null,
            .copied_constraint => self.payload.copied_constraint.anchor_index,
        };
    }

    pub fn whereSourceIndex(self: @This()) ?u32 {
        return if (self.decodedKind() == .where_requirement)
            self.payload.where_requirement.source_index
        else
            null;
    }

    /// Validate the closed arm, every nested closed enum, and every inactive
    /// payload word. Bounds and cross-ledger snapshot equality belong to
    /// terminal settlement admission.
    pub fn hasCanonicalTags(self: @This()) bool {
        const kind = self.decodedKind() orelse return false;
        return switch (kind) {
            .dispatch_expr => blk: {
                const source = self.payload.dispatch_expr;
                break :blk source.anchor_index != none and source.node_index != none and
                    std.enums.fromInt(OriginalNodeKind, source.original_node_kind) != null and
                    source.receiver_var != none and source.constraint_fn_var != none and
                    source.method_ident != none and allZero(&source.reserved);
            },
            .literal_conversion => blk: {
                const source = self.payload.literal_conversion;
                break :blk source.anchor_index != none and source.node_index != none and
                    source.target_var != none and source.constraint_fn_var != none and
                    std.enums.fromInt(LiteralKind, source.literal_kind) != null and
                    allZero(&source.reserved);
            },
            .interpolation => blk: {
                const source = self.payload.interpolation;
                break :blk source.anchor_index != none and source.node_index != none and
                    source.dispatcher_var != none and source.constraint_fn_var != none and
                    allZero(&source.reserved);
            },
            .pattern_literal_equality => blk: {
                const source = self.payload.pattern_literal_equality;
                break :blk source.anchor_index != none and source.pattern_index != none and
                    std.enums.fromInt(LiteralKind, source.literal_kind) != null and
                    source.receiver_var != none and source.is_eq_fn_var != none and
                    allZero(&source.reserved);
            },
            .negated_equality_not => blk: {
                const source = self.payload.negated_equality_not;
                break :blk source.anchor_index != none and source.node_index != none and
                    source.equality_constraint_index != none and source.receiver_var != none and
                    source.not_fn_var != none and allZero(&source.reserved);
            },
            .for_loop_dispatch => blk: {
                const source = self.payload.for_loop_dispatch;
                break :blk source.anchor_index != none and source.plan_index != none and
                    std.enums.fromInt(ForLoopSlot, source.slot) != null and
                    allZero(&source.reserved);
            },
            .where_requirement => blk: {
                const source = self.payload.where_requirement;
                break :blk source.source_index != none and allZero(&source.reserved);
            },
            .copied_constraint => blk: {
                const source = self.payload.copied_constraint;
                break :blk source.anchor_index != none and
                    source.receiver_component_ref.hasCanonicalTags(.receiver) and
                    source.function_component_ref.hasCanonicalTags(.function) and
                    hasCanonicalCopiedPair(
                        source.receiver_component_ref,
                        source.function_component_ref,
                    );
            },
        };
    }

    /// Stable order by closed arm followed by the arm's complete semantic
    /// coordinate tuple. Callers validate both rows before sorting.
    pub fn canonicalLessThan(left: @This(), right: @This()) bool {
        if (left.kind != right.kind) return left.kind < right.kind;
        const kind = left.decodedKind() orelse unreachable;
        return switch (kind) {
            .dispatch_expr => blk: {
                const a = left.payload.dispatch_expr;
                const b = right.payload.dispatch_expr;
                break :blk wordsLessThan(
                    &.{ a.anchor_index, a.node_index, a.original_node_kind, a.receiver_var, a.constraint_fn_var, a.method_ident },
                    &.{ b.anchor_index, b.node_index, b.original_node_kind, b.receiver_var, b.constraint_fn_var, b.method_ident },
                );
            },
            .literal_conversion => blk: {
                const a = left.payload.literal_conversion;
                const b = right.payload.literal_conversion;
                break :blk wordsLessThan(
                    &.{ a.anchor_index, a.node_index, a.target_var, a.constraint_fn_var, a.literal_kind },
                    &.{ b.anchor_index, b.node_index, b.target_var, b.constraint_fn_var, b.literal_kind },
                );
            },
            .interpolation => blk: {
                const a = left.payload.interpolation;
                const b = right.payload.interpolation;
                break :blk wordsLessThan(
                    &.{ a.anchor_index, a.node_index, a.dispatcher_var, a.constraint_fn_var },
                    &.{ b.anchor_index, b.node_index, b.dispatcher_var, b.constraint_fn_var },
                );
            },
            .pattern_literal_equality => blk: {
                const a = left.payload.pattern_literal_equality;
                const b = right.payload.pattern_literal_equality;
                break :blk wordsLessThan(
                    &.{ a.anchor_index, a.pattern_index, a.literal_kind, a.receiver_var, a.is_eq_fn_var },
                    &.{ b.anchor_index, b.pattern_index, b.literal_kind, b.receiver_var, b.is_eq_fn_var },
                );
            },
            .negated_equality_not => blk: {
                const a = left.payload.negated_equality_not;
                const b = right.payload.negated_equality_not;
                break :blk wordsLessThan(
                    &.{ a.anchor_index, a.node_index, a.equality_constraint_index, a.receiver_var, a.not_fn_var },
                    &.{ b.anchor_index, b.node_index, b.equality_constraint_index, b.receiver_var, b.not_fn_var },
                );
            },
            .for_loop_dispatch => blk: {
                const a = left.payload.for_loop_dispatch;
                const b = right.payload.for_loop_dispatch;
                break :blk wordsLessThan(
                    &.{ a.anchor_index, a.plan_index, a.slot },
                    &.{ b.anchor_index, b.plan_index, b.slot },
                );
            },
            .where_requirement => left.payload.where_requirement.source_index <
                right.payload.where_requirement.source_index,
            .copied_constraint => blk: {
                const a = left.payload.copied_constraint;
                const b = right.payload.copied_constraint;
                if (a.anchor_index != b.anchor_index) break :blk a.anchor_index < b.anchor_index;
                if (CopiedConstraintComponentRef.canonicalLessThan(
                    a.receiver_component_ref,
                    b.receiver_component_ref,
                )) break :blk true;
                if (CopiedConstraintComponentRef.canonicalLessThan(
                    b.receiver_component_ref,
                    a.receiver_component_ref,
                )) break :blk false;
                break :blk CopiedConstraintComponentRef.canonicalLessThan(
                    a.function_component_ref,
                    b.function_component_ref,
                );
            },
        };
    }

    fn allZero(words: []const u32) bool {
        for (words) |word| if (word != 0) return false;
        return true;
    }

    fn wordsLessThan(left: []const u32, right: []const u32) bool {
        std.debug.assert(left.len == right.len);
        for (left, right) |a, b| {
            if (a != b) return a < b;
        }
        return false;
    }

    fn hasCanonicalCopiedPair(
        receiver: CopiedConstraintComponentRef,
        function: CopiedConstraintComponentRef,
    ) bool {
        const receiver_kind = receiver.decodedKind() orelse return false;
        const function_kind = function.decodedKind() orelse return false;
        return switch (receiver_kind) {
            .root_graph => function_kind == .root_graph and
                receiver.copy_step_index == function.copy_step_index,
            .scheme_requirement => function_kind == .scheme_requirement and
                receiver.copy_step_index == function.copy_step_index and
                receiver.requirement_ordinal == function.requirement_ordinal,
            .binding_codec_receiver => function_kind == .binding_codec_function and
                receiver.requirement_ordinal == function.requirement_ordinal and
                receiver.binding_root_step == function.binding_root_step,
            .binding_codec_function => false,
        };
    }

    comptime {
        const payload_size = 13 * @sizeOf(u32);
        for (.{
            DispatchExpr,
            LiteralConversion,
            Interpolation,
            PatternLiteralEquality,
            NegatedEqualityNot,
            ForLoopDispatch,
            WhereRequirement,
            CopiedConstraint,
        }) |PayloadType| {
            if (@sizeOf(PayloadType) != payload_size) {
                @compileError("DispatchSettlementSource payload arms must remain fixed-width");
            }
        }
        if (@sizeOf(Payload) != payload_size) {
            @compileError("DispatchSettlementSource payload must remain thirteen u32 words");
        }
    }
};

/// Exact movement of one tagged constraint-evidence handle when a new
/// constraint occurrence is appended by unification or checker-side dedup.
/// Every handle carried by the destination has a producer transition from
/// each contributing source occurrence; marker-free constraints participate.
pub const ConstraintEvidenceMove = extern struct {
    handle: types_mod.ConstraintEvidenceHandle,
    source_constraint_index: u32,
    destination_constraint_index: u32,

    pub const SafeList = collections.SafeList(@This());
};

/// One exact constraint-movement row in a selected decision's complete
/// receiver-provenance subgraph. A decision's gapless range is grouped by its
/// terminal selected-anchor handles' canonical order and then by ascending
/// movement-row index.
/// For each handle it contains every reverse-reachable transition from the
/// selected constraint occurrence to that handle's unique direct/copy anchor;
/// this representation therefore preserves branches and reconverging diamonds
/// instead of pretending a merged provenance graph is one arbitrary chain.
pub const SelectedMethodDecisionMove = extern struct {
    receiver_anchor_index: u32,
    constraint_move_index: u32,

    pub const SafeList = collections.SafeList(@This());
};

/// The resolved source identity used by the external-template cache. During
/// canonicalization `resolved_module_idx` is temporarily `unresolved`; the
/// import table supplies the resolved module before checking seals this key.
pub const ExternalLookupKey = extern struct {
    resolved_module_idx: u32,
    target_node: u32,

    pub const unresolved = std.math.maxInt(u32);

    pub fn canonicalLessThan(_: void, left: @This(), right: @This()) bool {
        if (left.resolved_module_idx != right.resolved_module_idx) {
            return left.resolved_module_idx < right.resolved_module_idx;
        }
        return left.target_node < right.target_node;
    }

    pub fn eql(left: @This(), right: @This()) bool {
        return left.resolved_module_idx == right.resolved_module_idx and
            left.target_node == right.target_node;
    }
};

/// Closed producer site for one external-template cache lookup. The node is
/// the exact canonical CIR origin used to reconstruct the copy origin. The
/// parameter ordinal is active only for `external_where_alias_parameter`.
pub const ExternalLookupSiteKind = enum(u32) {
    external_type_annotation_lookup,
    external_type_annotation_apply,
    external_numeric_suffix,
    external_where_alias_receiver,
    external_where_alias_parameter,
    external_nominal_pattern,
    external_nominal_expr,
    external_lookup_expr,
    external_associated_lookup,
};

/// Complete canonical external-lookup token stream. Tokens are keyed by the
/// resolved `(module,target)` pair plus their exact origin node; the raw import
/// index remains so canonicalization can publish before import resolution.
pub const ExternalLookupToken = extern struct {
    key: ExternalLookupKey,
    import_idx: u32,
    origin_node: u32,
    site_kind: u32,
    parameter_ordinal: u32,
    reserved_0: u32 = 0,

    pub const SafeList = collections.SafeList(@This());
    pub const none = std.math.maxInt(u32);

    pub fn decodedSiteKind(self: @This()) ?ExternalLookupSiteKind {
        return std.enums.fromInt(ExternalLookupSiteKind, self.site_kind);
    }

    pub fn hasCanonicalTags(self: @This()) bool {
        const site_kind = self.decodedSiteKind() orelse return false;
        return self.key.target_node != ExternalLookupToken.none and
            self.import_idx != ExternalLookupToken.none and
            self.origin_node != ExternalLookupToken.none and
            self.reserved_0 == 0 and
            switch (site_kind) {
                .external_type_annotation_lookup,
                .external_type_annotation_apply,
                .external_numeric_suffix,
                .external_where_alias_receiver,
                .external_nominal_pattern,
                .external_nominal_expr,
                .external_lookup_expr,
                .external_associated_lookup,
                => self.parameter_ordinal == ExternalLookupToken.none,
                .external_where_alias_parameter => self.parameter_ordinal != ExternalLookupToken.none,
            };
    }

    pub fn canonicalLessThan(_: void, left: @This(), right: @This()) bool {
        if (ExternalLookupKey.canonicalLessThan({}, left.key, right.key)) return true;
        if (ExternalLookupKey.canonicalLessThan({}, right.key, left.key)) return false;
        if (left.site_kind != right.site_kind) return left.site_kind < right.site_kind;
        if (left.origin_node != right.origin_node) return left.origin_node < right.origin_node;
        if (left.parameter_ordinal != right.parameter_ordinal) {
            return left.parameter_ordinal < right.parameter_ordinal;
        }
        return left.import_idx < right.import_idx;
    }
};

/// The unique minimum token for an external cache key owns the durable eager
/// cache seed. Later lookup tokens may observe that seed but cannot claim it.
pub const ExternalCacheSeed = extern struct {
    key: ExternalLookupKey,
    seed_token: u32,
    seed_node: u32,
    support_step: u32,
    state: u32 = @intFromEnum(State.complete),
    reserved_0: u32 = 0,

    pub const SafeList = collections.SafeList(@This());
    pub const none = std.math.maxInt(u32);
    pub const State = enum(u32) {
        reserved,
        complete,
    };

    pub fn decodedState(self: @This()) ?State {
        return std.enums.fromInt(State, self.state);
    }

    pub fn hasCanonicalTags(self: @This()) bool {
        return self.key.resolved_module_idx != ExternalLookupKey.unresolved and
            self.key.target_node != ExternalLookupKey.unresolved and
            self.seed_token != ExternalCacheSeed.none and
            self.seed_node != ExternalCacheSeed.none and
            self.support_step != ExternalCacheSeed.none and
            self.decodedState() == .complete and
            self.reserved_0 == 0;
    }

    pub fn hasReservedTags(self: @This()) bool {
        return self.key.resolved_module_idx != ExternalLookupKey.unresolved and
            self.key.target_node != ExternalLookupKey.unresolved and
            self.seed_token != ExternalCacheSeed.none and
            self.seed_node != ExternalCacheSeed.none and
            self.support_step == ExternalCacheSeed.none and
            self.decodedState() == .reserved and
            self.reserved_0 == 0;
    }

    pub fn canonicalLessThan(_: void, left: @This(), right: @This()) bool {
        if (ExternalLookupKey.canonicalLessThan({}, left.key, right.key)) return true;
        if (ExternalLookupKey.canonicalLessThan({}, right.key, left.key)) return false;
        return left.seed_token < right.seed_token;
    }
};

pub const WhereMarkerNodeOrigin = extern struct {
    node: u32,
    reserved_0: u32 = 0,
    reserved_1: u32 = 0,
    reserved_2: u32 = 0,
    reserved_3: u32 = 0,
    reserved_4: u32 = 0,
};

pub const WhereMarkerWhereAliasParameterOrigin = extern struct {
    alias_reference_anno: u32,
    parameter_ordinal: u32,
    reserved_0: u32 = 0,
    reserved_1: u32 = 0,
    reserved_2: u32 = 0,
    reserved_3: u32 = 0,
};

/// Explicit terminal authority for an eager external-template cache seed.
/// The row is resolved through `external_cache_seeds`; no source node or
/// sibling lookup is inferred from this payload.
pub const WhereMarkerExternalCacheSeedOrigin = extern struct {
    seed_row: u32,
    reserved_0: u32 = 0,
    reserved_1: u32 = 0,
    reserved_2: u32 = 0,
    reserved_3: u32 = 0,
    reserved_4: u32 = 0,
};

pub const WhereMarkerBindingCodecOrigin = extern struct {
    binding_root_step: u32,
    requirement_ordinal: u32,
    reserved_0: u32 = 0,
    reserved_1: u32 = 0,
    reserved_2: u32 = 0,
    reserved_3: u32 = 0,
};

/// Cross-module selected-method roots name the durable decision that owns the
/// import-cache creation event. The decision ledger, rather than this fixed-
/// width origin payload, carries the exact consumer and copy-lineage anchors.
pub const WhereMarkerSelectedMethodOrigin = extern struct {
    decision_index: u32,
    reserved_0: u32 = 0,
    reserved_1: u32 = 0,
    reserved_2: u32 = 0,
    reserved_3: u32 = 0,
    reserved_4: u32 = 0,
};

pub const WhereMarkerGeneratedCodecMethodOrigin = extern struct {
    scheme_use_index: u32,
    derivation_index: u32,
    call_ordinal: u32,
    provider_dependency_index: u32,
    provider_type_node: u32,
    provider_def: u32,
};

pub const WhereMarkerInspectMethodOrigin = extern struct {
    scheme_use_index: u32,
    call_expr: u32,
    argument_expr: u32,
    provider_dependency_index: u32,
    provider_type_node: u32,
    provider_def: u32,
};

pub const WhereMarkerAssociatedMethodOrigin = extern struct {
    scheme_use_index: u32,
    expr_node: u32,
    provider_dependency_index: u32,
    provider_type_node: u32,
    provider_def: u32,
    reserved_0: u32 = 0,
};

/// Immutable authority for the first selected default-method target attempt
/// which seeded a cached provider root. A rejected attempt can own this root,
/// so it never names a speculative SchemeUse.
pub const WhereMarkerDefaultMethodOrigin = extern struct {
    decision_index: u32,
    constraint_offset: u32,
    /// The cached root is authorized by its first target attempt, which may be
    /// rejected before any use exists. Committed uses are owned exclusively by
    /// `WhereMarkerDefaultMethodUseOrigin` child steps.
    reserved_0: u32 = 0,
    builtin_decl_index: u32,
    provider_type_node: u32,
    provider_def: u32,
};

/// One exact local instantiation of a cached default-method template. The
/// cached cross-module root keeps the immutable first-cache-seed authority and
/// may have been seeded by another imported-method consumer; this child instead
/// binds every committed default use (including a successful first attempt) to
/// its decision, relative constraint, and freshly minted `SchemeUseRecord`.
pub const WhereMarkerDefaultMethodUseOrigin = extern struct {
    decision_index: u32,
    constraint_offset: u32,
    scheme_use_index: u32,
    root_copy_step: u32,
    reserved_1: u32 = 0,
    reserved_2: u32 = 0,
};

/// Explicit origin of a platform requirement copied under the canonical
/// platform-for-application substitution relation. The substitution range
/// addresses `ModuleEnv.where_marker_platform_substitutions`, never a presumed
/// contiguous subset of the source-var-sorted complete pair relation.
pub const WhereMarkerPlatformRequirementOrigin = extern struct {
    requires_index: u32,
    solution_def: u32,
    substitutions_start: u32,
    substitutions_len: u32,
    reserved_0: u32 = 0,
    reserved_1: u32 = 0,
};

/// One committed literal/defaulting compatibility probe. This is not a
/// selected-dispatch use: the exact retained constraint and candidate nominal
/// anchor the probe, while the provider coordinates authenticate the method
/// template copied to test that candidate.
pub const WhereMarkerCandidateProbeMethodOrigin = extern struct {
    constraint_index: u32,
    candidate_origin_module: u32,
    candidate_source_decl: u32,
    provider_dependency_index: u32,
    provider_type_node: u32,
    provider_def: u32,
};

pub const WhereMarkerSchemeUseOrigin = extern struct {
    scheme_use_index: u32,
    reserved_0: u32 = 0,
    reserved_1: u32 = 0,
    reserved_2: u32 = 0,
    reserved_3: u32 = 0,
    reserved_4: u32 = 0,
};

pub const WhereMarkerPlatformInstanceOrigin = extern struct {
    requires_index: u32,
    solution_def: u32,
    reserved_0: u32 = 0,
    reserved_1: u32 = 0,
    reserved_2: u32 = 0,
    reserved_3: u32 = 0,
};

pub const WhereMarkerPlatformAliasAppDeclOrigin = extern struct {
    platform_alias_statement: u32,
    app_declaration_node: u32,
    reserved_0: u32 = 0,
    reserved_1: u32 = 0,
    reserved_2: u32 = 0,
    reserved_3: u32 = 0,
};

pub const WhereMarkerDefaultFieldTypeOrigin = extern struct {
    field_type_anno: u32,
    default_expr: u32,
    reserved_0: u32 = 0,
    reserved_1: u32 = 0,
    reserved_2: u32 = 0,
    reserved_3: u32 = 0,
};

pub const WhereMarkerRecordUpdateBaseOrigin = extern struct {
    record_expr: u32,
    base_expr: u32,
    root_binding: u32 = @intFromEnum(RootBinding.direct_request),
    reserved_1: u32 = 0,
    reserved_2: u32 = 0,
    reserved_3: u32 = 0,

    pub const RootBinding = enum(u32) {
        direct_request,
        redirected_identity_share,
    };

    pub fn decodedRootBinding(self: @This()) ?RootBinding {
        return std.enums.fromInt(RootBinding, self.root_binding);
    }
};

pub const WhereMarkerCopyOccurrenceSide = enum(u32) {
    source,
    destination,
};

/// The exact earlier authority from which an Expected value is consumed.
/// Copy occurrences preserve an immutable raw source/destination request;
/// producer-root plans preserve the exact raw variable published by a finite
/// producer which has no earlier copy occurrence; relation plans preserve the
/// exact raw accumulator carried by an earlier successful branch contribution;
/// evidence-free plans preserve the exact earlier plan which proved that raw
/// graph carries no attached, default, or where evidence. A raw `none` kind is
/// allowed only when the containing tagged outcome does not consume parent
/// authority.
pub const ExpectedMarkerAuthority = extern struct {
    kind: u32,
    payload: Payload,

    pub const none = std.math.maxInt(u32);

    pub const Kind = enum(u32) {
        copy_occurrence,
        producer_root_plan,
        relation_plan,
        evidence_free_plan,
    };

    pub const CopyOccurrence = extern struct {
        copy_step: u32,
        occurrence_offset: u32,
        side: u32,
    };

    pub const ProducerRootPlan = extern struct {
        plan_index: u32,
        raw_var: u32,
        reserved_0: u32 = 0,
    };

    pub const Payload = extern union {
        pub const serialized_portable_extern_union = true;

        copy_occurrence: CopyOccurrence,
        producer_root_plan: ProducerRootPlan,
        relation_plan: ProducerRootPlan,
        evidence_free_plan: ProducerRootPlan,
    };

    pub fn inactive() @This() {
        return .{
            .kind = none,
            .payload = .{ .copy_occurrence = .{
                .copy_step = none,
                .occurrence_offset = none,
                .side = none,
            } },
        };
    }

    pub fn decodedKind(self: @This()) ?Kind {
        if (self.kind == none) return null;
        return std.enums.fromInt(Kind, self.kind);
    }

    /// Validate the tag and inactive payload bytes without interpreting index
    /// bounds. The caller decides whether absence is legal for its outcome.
    pub fn hasCanonicalTags(self: @This(), required: bool) bool {
        if (self.kind == none) {
            const inactive_payload = self.payload.copy_occurrence;
            return !required and inactive_payload.copy_step == none and
                inactive_payload.occurrence_offset == none and inactive_payload.side == none;
        }

        const kind = self.decodedKind() orelse return false;
        return switch (kind) {
            .copy_occurrence => blk: {
                const occurrence = self.payload.copy_occurrence;
                break :blk occurrence.copy_step != none and
                    occurrence.occurrence_offset != none and
                    std.enums.fromInt(WhereMarkerCopyOccurrenceSide, occurrence.side) != null;
            },
            .producer_root_plan => blk: {
                const producer_root = self.payload.producer_root_plan;
                break :blk producer_root.plan_index != none and
                    producer_root.raw_var != none and producer_root.reserved_0 == 0;
            },
            .relation_plan => blk: {
                const relation = self.payload.relation_plan;
                break :blk relation.plan_index != none and
                    relation.raw_var != none and relation.reserved_0 == 0;
            },
            .evidence_free_plan => blk: {
                const evidence_free = self.payload.evidence_free_plan;
                break :blk evidence_free.plan_index != none and
                    evidence_free.raw_var != none and evidence_free.reserved_0 == 0;
            },
        };
    }
};

/// Exact authority for the raw subject of one `ExpectedFailure`. Unlike
/// `ExpectedMarkerAuthority`, this is failure-specific: it admits direct
/// producer phases and provider failures in addition to ordinary Expected
/// endpoints, including relation-plan endpoints. The raw `none` tag exists
/// only so builders can reserve a canonical inactive value; published failure
/// rows require an active arm.
pub const SubjectAuthority = extern struct {
    kind: u32,
    payload: Payload,

    pub const none = std.math.maxInt(u32);

    pub const Kind = enum(u32) {
        direct,
        expected_copy_occurrence,
        expected_producer_root_plan,
        expected_relation_plan,
        expected_evidence_free_plan,
        provider_where_alias_checked_error,
    };

    /// Every direct producer branch has its own phase. This makes a new branch
    /// a schema change rather than allowing it to hide behind a generic phase.
    pub const DirectPhase = enum(u32) {
        annotation_malformed_type,
        annotation_malformed_where,
        annotation_invalid_tag_child,
        annotation_where_receiver_not_introduced,
        annotation_where_alias_not_alias,
        annotation_recursive_where_alias,
        annotation_local_where_alias_checked_error,
        annotation_where_alias_unresolved,
        annotation_where_alias_arity,
        annotation_where_alias_in_type_position,
        annotation_builtin_not_type,
        annotation_recursive_type_decl,
        annotation_type_decl_poisoned,
        annotation_type_formal_poisoned,
        annotation_type_apply_arity,
        annotation_alias_row_rejected,
        annotation_external_type_unresolved,
        annotation_child_before_copy,
        annotation_duplicate_where_relation,
        direct_binder_lookup_checked_error,
        annotated_binding_lookup_checked_error,
        nominal_pattern_external_unresolved,
        nominal_pattern_decl_poisoned,
        nominal_pattern_opaque_inaccessible,
        nominal_pattern_backing_unavailable,
    };

    pub const Direct = extern struct {
        producer_node: u32,
        phase: u32,
        /// Optional exact record in a local durable producer ledger. It is
        /// active only for phases whose legality row requires it.
        local_record_index: u32,
    };

    pub const ExpectedCopyOccurrence = extern struct {
        copy_step: u32,
        occurrence_offset: u32,
        side: u32,
    };

    pub const ExpectedPlan = extern struct {
        plan_index: u32,
        raw_var: u32,
        reserved_0: u32 = 0,
    };

    pub const ProviderWhereAliasCheckedError = extern struct {
        dependency_index: u32,
        publication_index: u32,
        reserved_0: u32 = 0,
    };

    pub const Payload = extern union {
        pub const serialized_portable_extern_union = true;

        direct: Direct,
        expected_copy_occurrence: ExpectedCopyOccurrence,
        expected_producer_root_plan: ExpectedPlan,
        expected_relation_plan: ExpectedPlan,
        expected_evidence_free_plan: ExpectedPlan,
        provider_where_alias_checked_error: ProviderWhereAliasCheckedError,
    };

    pub fn inactive() @This() {
        return .{
            .kind = none,
            .payload = .{ .direct = .{
                .producer_node = none,
                .phase = none,
                .local_record_index = none,
            } },
        };
    }

    pub fn direct(
        producer_node: u32,
        phase: DirectPhase,
        local_record_index: ?u32,
    ) @This() {
        return .{
            .kind = @intFromEnum(Kind.direct),
            .payload = .{ .direct = .{
                .producer_node = producer_node,
                .phase = @intFromEnum(phase),
                .local_record_index = local_record_index orelse none,
            } },
        };
    }

    pub fn expectedCopyOccurrence(
        copy_step: u32,
        occurrence_offset: u32,
        side: WhereMarkerCopyOccurrenceSide,
    ) @This() {
        return .{
            .kind = @intFromEnum(Kind.expected_copy_occurrence),
            .payload = .{ .expected_copy_occurrence = .{
                .copy_step = copy_step,
                .occurrence_offset = occurrence_offset,
                .side = @intFromEnum(side),
            } },
        };
    }

    pub fn expectedProducerRootPlan(plan_index: u32, raw_var: u32) @This() {
        return .{
            .kind = @intFromEnum(Kind.expected_producer_root_plan),
            .payload = .{ .expected_producer_root_plan = .{
                .plan_index = plan_index,
                .raw_var = raw_var,
            } },
        };
    }

    pub fn expectedRelationPlan(plan_index: u32, raw_var: u32) @This() {
        return .{
            .kind = @intFromEnum(Kind.expected_relation_plan),
            .payload = .{ .expected_relation_plan = .{
                .plan_index = plan_index,
                .raw_var = raw_var,
            } },
        };
    }

    pub fn expectedEvidenceFreePlan(plan_index: u32, raw_var: u32) @This() {
        return .{
            .kind = @intFromEnum(Kind.expected_evidence_free_plan),
            .payload = .{ .expected_evidence_free_plan = .{
                .plan_index = plan_index,
                .raw_var = raw_var,
            } },
        };
    }

    pub fn providerWhereAliasCheckedError(
        dependency_index: u32,
        publication_index: u32,
    ) @This() {
        return .{
            .kind = @intFromEnum(Kind.provider_where_alias_checked_error),
            .payload = .{ .provider_where_alias_checked_error = .{
                .dependency_index = dependency_index,
                .publication_index = publication_index,
            } },
        };
    }

    pub fn decodedKind(self: @This()) ?Kind {
        if (self.kind == none) return null;
        return std.enums.fromInt(Kind, self.kind);
    }

    pub fn decodedDirect(self: @This()) ?Direct {
        return if (self.decodedKind() == .direct) self.payload.direct else null;
    }

    pub fn decodedDirectPhase(self: @This()) ?DirectPhase {
        const value = self.decodedDirect() orelse return null;
        return std.enums.fromInt(DirectPhase, value.phase);
    }

    pub fn decodedExpectedCopyOccurrence(self: @This()) ?ExpectedCopyOccurrence {
        return if (self.decodedKind() == .expected_copy_occurrence)
            self.payload.expected_copy_occurrence
        else
            null;
    }

    pub fn decodedExpectedCopySide(self: @This()) ?WhereMarkerCopyOccurrenceSide {
        const value = self.decodedExpectedCopyOccurrence() orelse return null;
        return std.enums.fromInt(WhereMarkerCopyOccurrenceSide, value.side);
    }

    pub fn decodedExpectedProducerRootPlan(self: @This()) ?ExpectedPlan {
        return if (self.decodedKind() == .expected_producer_root_plan)
            self.payload.expected_producer_root_plan
        else
            null;
    }

    pub fn decodedExpectedRelationPlan(self: @This()) ?ExpectedPlan {
        return if (self.decodedKind() == .expected_relation_plan)
            self.payload.expected_relation_plan
        else
            null;
    }

    pub fn decodedExpectedEvidenceFreePlan(self: @This()) ?ExpectedPlan {
        return if (self.decodedKind() == .expected_evidence_free_plan)
            self.payload.expected_evidence_free_plan
        else
            null;
    }

    pub fn decodedProviderWhereAliasCheckedError(self: @This()) ?ProviderWhereAliasCheckedError {
        return if (self.decodedKind() == .provider_where_alias_checked_error)
            self.payload.provider_where_alias_checked_error
        else
            null;
    }

    /// Validate the discriminant and all inactive/reserved payload words. Index
    /// bounds and cross-ledger identity are checked by artifact admission.
    pub fn hasCanonicalTags(self: @This(), required: bool) bool {
        if (self.kind == none) {
            const inactive_payload = self.payload.direct;
            return !required and inactive_payload.producer_node == none and
                inactive_payload.phase == none and
                inactive_payload.local_record_index == none;
        }

        const kind = self.decodedKind() orelse return false;
        return switch (kind) {
            .direct => blk: {
                const value = self.payload.direct;
                break :blk value.producer_node != none and
                    std.enums.fromInt(DirectPhase, value.phase) != null;
            },
            .expected_copy_occurrence => blk: {
                const value = self.payload.expected_copy_occurrence;
                break :blk value.copy_step != none and value.occurrence_offset != none and
                    std.enums.fromInt(WhereMarkerCopyOccurrenceSide, value.side) != null;
            },
            .expected_producer_root_plan => blk: {
                const value = self.payload.expected_producer_root_plan;
                break :blk value.plan_index != none and value.raw_var != none and
                    value.reserved_0 == 0;
            },
            .expected_relation_plan => blk: {
                const value = self.payload.expected_relation_plan;
                break :blk value.plan_index != none and value.raw_var != none and
                    value.reserved_0 == 0;
            },
            .expected_evidence_free_plan => blk: {
                const value = self.payload.expected_evidence_free_plan;
                break :blk value.plan_index != none and value.raw_var != none and
                    value.reserved_0 == 0;
            },
            .provider_where_alias_checked_error => blk: {
                const value = self.payload.provider_where_alias_checked_error;
                break :blk value.dependency_index != none and value.publication_index != none and
                    value.reserved_0 == 0;
            },
        };
    }
};

/// Exact durable owner of one Expected checked-error or suppression. Absence is
/// encoded by the raw `none` tag plus three `none` payload words, never by an
/// enum member. Active local arms reserve their remaining payload words as zero.
pub const CauseOwner = extern struct {
    kind: u32,
    payload: Payload,

    pub const none = std.math.maxInt(u32);

    pub const Kind = enum(u32) {
        expected_failure,
        expected_consumer_retirement,
        cir_diagnostic,
        provider_where_alias_checked_error,
    };

    pub const LocalIndex = extern struct {
        index: u32,
        reserved_0: u32 = 0,
        reserved_1: u32 = 0,
    };

    pub const ProviderWhereAliasCheckedError = extern struct {
        dependency_index: u32,
        publication_index: u32,
        reserved_0: u32 = 0,
    };

    pub const Payload = extern union {
        pub const serialized_portable_extern_union = true;

        expected_failure: LocalIndex,
        expected_consumer_retirement: LocalIndex,
        cir_diagnostic: LocalIndex,
        provider_where_alias_checked_error: ProviderWhereAliasCheckedError,
    };

    pub fn inactive() @This() {
        return .{
            .kind = none,
            .payload = .{ .expected_failure = .{
                .index = none,
                .reserved_0 = none,
                .reserved_1 = none,
            } },
        };
    }

    pub fn expectedFailure(index: u32) @This() {
        return .{
            .kind = @intFromEnum(Kind.expected_failure),
            .payload = .{ .expected_failure = .{ .index = index } },
        };
    }

    pub fn expectedConsumerRetirement(index: u32) @This() {
        return .{
            .kind = @intFromEnum(Kind.expected_consumer_retirement),
            .payload = .{ .expected_consumer_retirement = .{ .index = index } },
        };
    }

    pub fn cirDiagnostic(index: u32) @This() {
        return .{
            .kind = @intFromEnum(Kind.cir_diagnostic),
            .payload = .{ .cir_diagnostic = .{ .index = index } },
        };
    }

    pub fn providerWhereAliasCheckedError(
        dependency_index: u32,
        publication_index: u32,
    ) @This() {
        return .{
            .kind = @intFromEnum(Kind.provider_where_alias_checked_error),
            .payload = .{ .provider_where_alias_checked_error = .{
                .dependency_index = dependency_index,
                .publication_index = publication_index,
            } },
        };
    }

    pub fn decodedKind(self: @This()) ?Kind {
        if (self.kind == none) return null;
        return std.enums.fromInt(Kind, self.kind);
    }

    pub fn decodedExpectedFailure(self: @This()) ?LocalIndex {
        return if (self.decodedKind() == .expected_failure)
            self.payload.expected_failure
        else
            null;
    }

    pub fn decodedExpectedConsumerRetirement(self: @This()) ?LocalIndex {
        return if (self.decodedKind() == .expected_consumer_retirement)
            self.payload.expected_consumer_retirement
        else
            null;
    }

    pub fn decodedCirDiagnostic(self: @This()) ?LocalIndex {
        return if (self.decodedKind() == .cir_diagnostic)
            self.payload.cir_diagnostic
        else
            null;
    }

    pub fn decodedProviderWhereAliasCheckedError(self: @This()) ?ProviderWhereAliasCheckedError {
        return if (self.decodedKind() == .provider_where_alias_checked_error)
            self.payload.provider_where_alias_checked_error
        else
            null;
    }

    pub fn hasCanonicalTags(self: @This(), required: bool) bool {
        if (self.kind == none) {
            const inactive_payload = self.payload.expected_failure;
            return !required and inactive_payload.index == none and
                inactive_payload.reserved_0 == none and
                inactive_payload.reserved_1 == none;
        }

        const kind = self.decodedKind() orelse return false;
        return switch (kind) {
            .expected_failure => canonicalLocalIndex(self.payload.expected_failure),
            .expected_consumer_retirement => canonicalLocalIndex(
                self.payload.expected_consumer_retirement,
            ),
            .cir_diagnostic => canonicalLocalIndex(self.payload.cir_diagnostic),
            .provider_where_alias_checked_error => blk: {
                const value = self.payload.provider_where_alias_checked_error;
                break :blk value.dependency_index != none and value.publication_index != none and
                    value.reserved_0 == 0;
            },
        };
    }

    fn canonicalLocalIndex(value: LocalIndex) bool {
        return value.index != none and value.reserved_0 == 0 and value.reserved_1 == 0;
    }
};

pub const WhereMarkerAggregateFreshShapeChildOrigin = extern struct {
    expected_plans_start: u32,
    expected_plans_len: u32,
    parent_authority: ExpectedMarkerAuthority,
};

pub const WhereMarkerExpectedProjectionOrigin = extern struct {
    consumer_node: u32,
    parent_authority: ExpectedMarkerAuthority,
    /// Exact Expected-consumption plan which owns this projection copy.
    /// Plan index zero is valid; `ExpectedConsumptionPlan.none` is the only
    /// absence sentinel and is never legal on a published step.
    expected_plan_index: u32,
};

/// Call-specific local instantiation origin. The canonical root token is
/// authored before checking and is independently replayed against the fresh
/// canonical request.
pub const WhereMarkerExpectedCallInstantiationOrigin = extern struct {
    call_root_token_index: u32,
    source_kind: u32,
    source_owner_node: u32,
    source_support_step: u32,
    source_seed_index: u32,
    reserved_1: u32 = 0,

    pub const none = std.math.maxInt(u32);

    pub const SourceKind = enum(u32) {
        local_callee_expression,
        local_binding_pattern,
        local_predeclared_annotation,
        external_import_root,
    };

    pub fn decodedSourceKind(self: @This()) ?SourceKind {
        return std.enums.fromInt(SourceKind, self.source_kind);
    }
};

/// Immutable producer-time identity for one successful call formal. The row
/// authenticates the raw formal named by a `.call_argument/.producer_root`
/// Expected plan without reopening a callable descriptor after later
/// unification has changed its topology.
pub const ExpectedCallKind = enum(u32) {
    apply,
    record_builder,
};

/// Canonicalization-authored call cardinality token. One root row followed by
/// one source-ordered argument row is emitted atomically with every call node.
pub const ExpectedCallSlotToken = extern struct {
    owner_node: u32,
    site_node: u32,
    slot: u32,
    cardinality: u32,
    called_via: u32,
    role: u32,
    reserved_0: u32 = 0,
    reserved_1: u32 = 0,

    pub const SafeList = collections.SafeList(@This());
    pub const none = std.math.maxInt(u32);

    pub const Role = enum(u32) {
        root,
        argument,
    };

    pub fn decodedCalledVia(self: @This()) ?ExpectedCallKind {
        return std.enums.fromInt(ExpectedCallKind, self.called_via);
    }

    pub fn decodedRole(self: @This()) ?Role {
        return std.enums.fromInt(Role, self.role);
    }

    pub fn hasCanonicalTags(self: @This()) bool {
        if (self.owner_node == none or self.site_node == none or
            self.reserved_0 != 0 or self.reserved_1 != 0 or
            self.decodedCalledVia() == null)
        {
            return false;
        }
        return switch (self.decodedRole() orelse return false) {
            .root => self.slot == none,
            .argument => self.slot < self.cardinality,
        };
    }
};

pub const ExpectedCallFormal = extern struct {
    owner_node: u32,
    call_root_plan_index: u32,
    argument_plan_index: u32,
    call_root_token_index: u32,
    slot_token_index: u32,
    raw_callee_var: u32,
    instantiation_source_var: u32,
    raw_callable_var: u32,
    fresh_shape_var: u32,
    slot: u32,
    raw_formal_var: u32,
    immutable_exposure_var: u32,
    instantiation_copy_step: u32,
    instantiation_root_occurrence: u32,
    instantiation_formal_occurrence: u32,
    fresh_args_start: u32,
    fresh_args_len: u32,
    fresh_ret_var: u32,
    fresh_effect_deps_start: u32,
    fresh_effect_deps_len: u32,
    called_via: u32,
    shape_kind: u32,
    formal_origin: u32,
    reserved_0: u32 = 0,

    pub const SafeList = collections.SafeList(@This());
    pub const none = std.math.maxInt(u32);

    pub const ShapeKind = enum(u32) {
        existing_callable,
        fresh_arity_shape,
    };

    pub const FormalOrigin = enum(u32) {
        direct_monomorphic_or_alias,
        explicitly_instantiated,
        fresh_arity_shape,
    };

    pub fn decodedCalledVia(self: @This()) ?ExpectedCallKind {
        return std.enums.fromInt(ExpectedCallKind, self.called_via);
    }

    pub fn decodedShapeKind(self: @This()) ?ShapeKind {
        return std.enums.fromInt(ShapeKind, self.shape_kind);
    }

    pub fn decodedFormalOrigin(self: @This()) ?FormalOrigin {
        return std.enums.fromInt(FormalOrigin, self.formal_origin);
    }

    pub fn hasCanonicalTags(self: @This()) bool {
        if (self.reserved_0 != 0 or
            self.owner_node == none or
            self.call_root_plan_index == none or
            self.argument_plan_index == none or
            self.call_root_token_index == none or
            self.slot_token_index == none or
            self.raw_callee_var == none or
            self.raw_callable_var == none or
            self.raw_formal_var == none)
        {
            return false;
        }
        _ = self.decodedCalledVia() orelse return false;
        const shape = self.decodedShapeKind() orelse return false;
        const origin = self.decodedFormalOrigin() orelse return false;
        return switch (shape) {
            .existing_callable => self.fresh_shape_var == none and
                self.fresh_args_start == none and self.fresh_args_len == 0 and
                self.fresh_ret_var == none and self.fresh_effect_deps_start == none and
                self.fresh_effect_deps_len == 0 and switch (origin) {
                .direct_monomorphic_or_alias => self.raw_callee_var == self.raw_callable_var and
                    self.instantiation_source_var == none and
                    self.immutable_exposure_var != none and
                    self.instantiation_copy_step == none and
                    self.instantiation_root_occurrence == none and
                    self.instantiation_formal_occurrence == none,
                .explicitly_instantiated => self.raw_callee_var != self.raw_callable_var and
                    self.instantiation_source_var != none and
                    self.instantiation_source_var != self.raw_callable_var and
                    self.immutable_exposure_var == none and
                    self.instantiation_copy_step != none and
                    self.instantiation_root_occurrence != none and
                    self.instantiation_formal_occurrence != none,
                .fresh_arity_shape => false,
            },
            .fresh_arity_shape => origin == .fresh_arity_shape and
                self.fresh_shape_var != none and
                self.fresh_shape_var != self.raw_callable_var and
                self.fresh_shape_var != self.raw_callee_var and
                self.fresh_shape_var != self.raw_formal_var and
                self.immutable_exposure_var == none and
                ((self.raw_callee_var == self.raw_callable_var and
                    self.instantiation_source_var == none and
                    self.instantiation_copy_step == none and
                    self.instantiation_root_occurrence == none) or
                    (self.raw_callee_var != self.raw_callable_var and
                        self.instantiation_source_var != none and
                        self.instantiation_source_var != self.raw_callable_var and
                        self.instantiation_copy_step != none and
                        self.instantiation_root_occurrence != none)) and
                self.instantiation_formal_occurrence == none and
                self.fresh_args_start != none and self.fresh_args_len != 0 and
                self.fresh_ret_var != none and
                self.fresh_effect_deps_start == none and
                self.fresh_effect_deps_len == 0,
        };
    }

    pub fn canonicalLessThan(_: void, lhs: @This(), rhs: @This()) bool {
        inline for (.{
            "owner_node",
            "call_root_plan_index",
            "slot",
            "argument_plan_index",
            "call_root_token_index",
            "slot_token_index",
            "called_via",
            "shape_kind",
            "formal_origin",
            "raw_callee_var",
            "instantiation_source_var",
            "raw_callable_var",
            "fresh_shape_var",
            "raw_formal_var",
            "immutable_exposure_var",
            "instantiation_copy_step",
            "instantiation_root_occurrence",
            "instantiation_formal_occurrence",
            "fresh_args_start",
            "fresh_args_len",
            "fresh_ret_var",
            "fresh_effect_deps_start",
            "fresh_effect_deps_len",
        }) |field| {
            if (@field(lhs, field) != @field(rhs, field)) {
                return @field(lhs, field) < @field(rhs, field);
            }
        }
        return false;
    }
};

/// One producer-authored outcome for an exact syntactically eligible expected-
/// type consumer and slot. All coordinates are raw module-local integers; the
/// checked-artifact validator interprets their active meaning from the closed
/// role and outcome tags rather than reconstructing authority from solved
/// types. Optional coordinates and reasons use `none`, never an enum fallback.
pub const ExpectedConsumptionPlan = extern struct {
    owner_node: u32,
    site_node: u32,
    role: u32,
    slot: u32,
    outcome: u32,
    reason: u32,
    raw_consumer_var: u32,
    parent_authority: ExpectedMarkerAuthority,
    produced_copy_step: u32,
    produced_occurrence_offset: u32,
    produced_side: u32,
    /// Exact call-root cardinality plan which owns a call argument producer
    /// root. Active only for `.producer_root`.
    call_root_plan_index: u32,
    failure_owner: CauseOwner,
    /// Exact earlier plan which supplies every upstream cause. The typed owner
    /// identifies the durable cause; this index independently authenticates
    /// the exact prior-plan causal path.
    failure_cause_plan_index: u32,
    /// Exact retirement authorizing destruction of a checked-error
    /// record-update base. Zero is inactive; an active value is the retirement
    /// index plus one so retirement index zero remains representable.
    source_retirement_index_plus_one: u32 = 0,

    pub const SafeList = collections.SafeList(@This());
    pub const none = std.math.maxInt(u32);

    pub const Role = enum(u32) {
        aggregate_owner,
        list_element,
        tuple_element,
        record_field,
        tag_payload,
        record_update_base,
        record_update_field,
        branch_seed,
        branch_contribution,
        branch_final,
        nominal_decl,
        nominal_backing,
        call_root,
        call_argument,
        default_field,
        lambda_return,
    };

    pub const Outcome = enum(u32) {
        anchored,
        /// A record-update base copy rooted directly at its exact CIR source.
        /// Unlike an anchored Expected projection, it has no earlier parent
        /// authority; the produced destination-root occurrence is the proof.
        source_root_copy,
        /// The same real base copy when checking the source expression already
        /// returned an exact typed failure owner.
        source_root_copy_checked_error,
        /// A producer-authored root which is not itself copied from an earlier
        /// Expected endpoint. The plan itself, together with its exact
        /// `raw_consumer_var`, is the authority endpoint.
        producer_root,
        evidence_free,
        /// This consumer proved that its source already denotes the carried
        /// Expected endpoint. It retains `parent_authority` unchanged and
        /// publishes no replacement copy endpoint.
        retained,
        /// A branch contribution or final row committed its relation to the
        /// live accumulator without producing a detached support copy.
        related,
        not_projected,
        checked_error,
        /// Checker-only reservation; terminal publication and admission reject it.
        reserved,
    };

    pub const Reason = enum(u32) {
        parent_evidence_free,
        aggregate_no_expected,
        aggregate_expected_contains_error,
        aggregate_expected_direct_error,
        aggregate_shape_mismatch,
        aggregate_child_relation_rejected,
        aggregate_child_relation_suppressed,
        aggregate_retired_after_child_relation,
        aggregate_retired_by_parent_branch_failure,
        record_update_base_checked_error,
        record_update_field_base_checked_error,
        record_update_projection_mismatch,
        record_update_field_checked_error,
        record_update_field_relation_rejected,
        branch_no_expected_result,
        branch_expected_error_short_circuit,
        branch_expected_direct_error,
        branch_body_error_short_circuit,
        branch_body_already_expected,
        branch_expected_compatibility_rejected,
        branch_accumulator_fold_rejected,
        branch_final_relation_rejected,
        branch_retired_after_failure,
        nominal_external_unresolved,
        nominal_decl_poisoned,
        nominal_decl_not_nominal,
        nominal_opaque_inaccessible,
        nominal_backing_unavailable,
        nominal_backing_checked_error,
        nominal_backing_relation_rejected,
        call_callee_checked_error,
        call_shape_ready,
        call_shape_relation_rejected,
        call_arity_rejected,
        call_operand_checked_error,
        call_formal_actual_relation_rejected,
        call_record_builder_return_rejected,
        call_retired,
        call_retired_after_operand_checked_error,
        default_expr_checked_error,
        default_effectful_rejected,
        default_type_relation_rejected,
        default_parameter_constraint_rejected,
        default_recursive_rejected,
        branch_retired_after_ambiguity_verdict,
    };

    /// How a failure owner relates to this plan. A same-node retirement owns
    /// this plan in its exhaustive retired-consumer range. A direct-source
    /// failure owns the exact parent Expected endpoint consumed by this
    /// still-present plan but does not own the plan itself. An upstream
    /// failure is linked through the exact earlier plan named by
    /// `failure_cause_plan_index`, independently of the typed cause arm.
    pub const OwnerRelation = enum {
        none,
        same_node_retirement,
        direct_source_retirement,
        upstream_retirement,
    };

    pub fn decodedRole(self: @This()) ?Role {
        return std.enums.fromInt(Role, self.role);
    }

    pub fn decodedOutcome(self: @This()) ?Outcome {
        return std.enums.fromInt(Outcome, self.outcome);
    }

    pub fn decodedReason(self: @This()) ?Reason {
        if (self.reason == none) return null;
        return std.enums.fromInt(Reason, self.reason);
    }

    pub fn decodedProducedSide(self: @This()) ?WhereMarkerCopyOccurrenceSide {
        if (self.produced_side == none) return null;
        return std.enums.fromInt(WhereMarkerCopyOccurrenceSide, self.produced_side);
    }

    pub fn decodedSourceRetirementIndex(self: @This()) ?u32 {
        if (self.source_retirement_index_plus_one == 0) return null;
        return self.source_retirement_index_plus_one - 1;
    }

    /// The exact copy-root side which carries the result of an anchored
    /// Expected consumer. The branch seed is the only branch role which
    /// produces an anchored copy; contributions and the final relation use
    /// `.related` and publish no copy endpoint.
    pub fn anchoredProducedSideForRole(role: Role) ?WhereMarkerCopyOccurrenceSide {
        return switch (role) {
            .branch_contribution, .branch_final, .call_root => null,
            .aggregate_owner,
            .list_element,
            .tuple_element,
            .record_field,
            .tag_payload,
            .record_update_base,
            .record_update_field,
            .branch_seed,
            .nominal_decl,
            .nominal_backing,
            .call_argument,
            .default_field,
            .lambda_return,
            => .destination,
        };
    }

    /// Validate the closed role/outcome/reason/owner-kind surface without
    /// interpreting any referenced indexes. Checked-artifact admission owns
    /// the corresponding bounds and semantic replay.
    pub fn hasLegalTags(self: @This()) bool {
        const role = self.decodedRole() orelse return false;
        const outcome = self.decodedOutcome() orelse return false;
        if ((self.decodedSourceRetirementIndex() != null) !=
            (outcome == .source_root_copy_checked_error)) return false;
        const reason: ?Reason = if (self.reason == none)
            null
        else
            self.decodedReason() orelse return false;

        const owner_present = self.failure_owner.kind != CauseOwner.none;
        if (!self.failure_owner.hasCanonicalTags(owner_present)) return false;
        const owner_kind = self.failure_owner.decodedKind();

        if ((outcome == .producer_root) != (self.call_root_plan_index != none)) {
            return false;
        }

        if (!legalCombination(
            role,
            outcome,
            reason,
            owner_kind,
            self.failure_cause_plan_index != none,
        )) return false;

        const parent_present = self.parent_authority.kind != ExpectedMarkerAuthority.none;
        if (!self.parent_authority.hasCanonicalTags(parent_present)) return false;

        const produced_any = self.produced_copy_step != none or
            self.produced_occurrence_offset != none or self.produced_side != none;
        const produced_all = self.produced_copy_step != none and
            self.produced_occurrence_offset != none and self.produced_side != none;
        if (produced_any != produced_all or
            (produced_all and self.decodedProducedSide() == null)) return false;

        const owner_relation = if (reason) |present_reason|
            ownerRelationForReason(present_reason)
        else
            OwnerRelation.none;
        return switch (outcome) {
            .anchored => anchoredProducedSideForRole(role) != null and
                parent_present and produced_all and
                self.decodedProducedSide().? == anchoredProducedSideForRole(role).? and
                switch (self.parent_authority.decodedKind().?) {
                    .copy_occurrence => self.parent_authority.payload.copy_occurrence.copy_step <
                        self.produced_copy_step,
                    .producer_root_plan => true,
                    .relation_plan => true,
                    .evidence_free_plan => true,
                },
            .source_root_copy => self.source_retirement_index_plus_one == 0 and
                role == .record_update_base and
                reason == null and !owner_present and !parent_present and
                produced_all and self.decodedProducedSide().? == .destination,
            .source_root_copy_checked_error => self.decodedSourceRetirementIndex() != null and
                role == .record_update_base and
                reason == .record_update_base_checked_error and owner_present and
                owner_relation == .direct_source_retirement and !parent_present and
                produced_all and self.decodedProducedSide().? == .destination,
            .producer_root => self.source_retirement_index_plus_one == 0 and
                !parent_present and !produced_any,
            .evidence_free => self.source_retirement_index_plus_one == 0 and parent_present and
                self.parent_authority.decodedKind().? == .evidence_free_plan and
                !produced_any,
            .retained => self.source_retirement_index_plus_one == 0 and
                parent_present and !produced_any,
            .related => self.source_retirement_index_plus_one == 0 and
                parent_present and !produced_any,
            .not_projected => self.source_retirement_index_plus_one == 0 and !produced_any and
                (owner_relation != .none or !parent_present),
            .checked_error => self.source_retirement_index_plus_one == 0 and
                !produced_any and switch (owner_relation) {
                .direct_source_retirement, .upstream_retirement => parent_present,
                .none, .same_node_retirement => true,
            },
            .reserved => false,
        };
    }

    /// Closed legality table for all expected-consumption roles. A reason is
    /// part of the outcome, not free-form context; every non-benign reason is
    /// owned through exactly one typed durable cause route.
    pub fn legalCombination(
        role: Role,
        outcome: Outcome,
        reason: ?Reason,
        owner_kind: ?CauseOwner.Kind,
        has_failure_cause_plan: bool,
    ) bool {
        if (reason) |present_reason| {
            if (!reasonAllowsRole(present_reason, role)) return false;
            if (outcomeForReason(present_reason) != outcome) return false;
            const owner_relation = ownerRelationForReason(present_reason);
            if (!failureOwnerKindAllowed(present_reason, owner_kind)) return false;
            return has_failure_cause_plan == (owner_relation == .upstream_retirement);
        }

        if (!planCauseAllowed(.none, owner_kind) or has_failure_cause_plan) return false;
        return switch (outcome) {
            .anchored => true,
            .source_root_copy => role == .record_update_base,
            .producer_root => role == .call_argument,
            .related => role == .branch_contribution or role == .branch_final,
            .source_root_copy_checked_error,
            .evidence_free,
            .retained,
            .not_projected,
            .checked_error,
            .reserved,
            => false,
        };
    }

    fn outcomeForReason(reason: Reason) Outcome {
        return switch (reason) {
            .parent_evidence_free => .evidence_free,
            .branch_body_already_expected => .retained,
            .aggregate_no_expected,
            .branch_no_expected_result,
            .call_shape_ready,
            .call_shape_relation_rejected,
            .call_arity_rejected,
            => .not_projected,
            .aggregate_expected_contains_error,
            .aggregate_expected_direct_error,
            .aggregate_shape_mismatch,
            .aggregate_child_relation_rejected,
            .aggregate_child_relation_suppressed,
            .aggregate_retired_after_child_relation,
            .aggregate_retired_by_parent_branch_failure,
            .record_update_field_base_checked_error,
            .record_update_projection_mismatch,
            .record_update_field_checked_error,
            .record_update_field_relation_rejected,
            .branch_expected_error_short_circuit,
            .branch_expected_direct_error,
            .branch_body_error_short_circuit,
            .branch_expected_compatibility_rejected,
            .branch_accumulator_fold_rejected,
            .branch_final_relation_rejected,
            .branch_retired_after_failure,
            .branch_retired_after_ambiguity_verdict,
            .nominal_external_unresolved,
            .nominal_decl_poisoned,
            .nominal_decl_not_nominal,
            .nominal_opaque_inaccessible,
            .nominal_backing_unavailable,
            .nominal_backing_checked_error,
            .nominal_backing_relation_rejected,
            .call_callee_checked_error,
            .call_operand_checked_error,
            .call_formal_actual_relation_rejected,
            .call_record_builder_return_rejected,
            .call_retired,
            .call_retired_after_operand_checked_error,
            .default_expr_checked_error,
            .default_effectful_rejected,
            .default_type_relation_rejected,
            .default_parameter_constraint_rejected,
            .default_recursive_rejected,
            => .checked_error,
            .record_update_base_checked_error => .source_root_copy_checked_error,
        };
    }

    pub fn ownerRelationForReason(reason: Reason) OwnerRelation {
        return switch (reason) {
            .parent_evidence_free,
            .aggregate_no_expected,
            .branch_no_expected_result,
            .branch_body_already_expected,
            .call_shape_ready,
            => .none,
            .aggregate_expected_contains_error,
            .branch_expected_error_short_circuit,
            => .upstream_retirement,
            .aggregate_expected_direct_error,
            .aggregate_child_relation_rejected,
            .aggregate_child_relation_suppressed,
            .record_update_base_checked_error,
            .record_update_field_checked_error,
            .branch_expected_direct_error,
            .branch_body_error_short_circuit,
            .branch_expected_compatibility_rejected,
            .branch_accumulator_fold_rejected,
            .nominal_backing_checked_error,
            .call_callee_checked_error,
            .call_operand_checked_error,
            .default_expr_checked_error,
            => .direct_source_retirement,
            .aggregate_shape_mismatch,
            .aggregate_retired_after_child_relation,
            .aggregate_retired_by_parent_branch_failure,
            .branch_retired_after_failure,
            .branch_retired_after_ambiguity_verdict,
            .record_update_projection_mismatch,
            .record_update_field_relation_rejected,
            .branch_final_relation_rejected,
            .nominal_external_unresolved,
            .nominal_decl_poisoned,
            .nominal_decl_not_nominal,
            .nominal_opaque_inaccessible,
            .nominal_backing_unavailable,
            .nominal_backing_relation_rejected,
            .call_shape_relation_rejected,
            .call_arity_rejected,
            .call_formal_actual_relation_rejected,
            .call_record_builder_return_rejected,
            .call_retired,
            .call_retired_after_operand_checked_error,
            .default_effectful_rejected,
            .default_type_relation_rejected,
            .default_parameter_constraint_rejected,
            .default_recursive_rejected,
            => .same_node_retirement,
            .record_update_field_base_checked_error => .upstream_retirement,
        };
    }

    /// Structural cause partition. Admission additionally proves that the
    /// selected cause owns the exact source named by the plan.
    fn failureOwnerKindAllowed(reason: Reason, owner_kind: ?CauseOwner.Kind) bool {
        return switch (ownerRelationForReason(reason)) {
            .none => planCauseAllowed(.none, owner_kind),
            .same_node_retirement => planCauseAllowed(.retirement, owner_kind),
            .upstream_retirement => planCauseAllowed(.any, owner_kind),
            .direct_source_retirement => switch (reason) {
                .aggregate_child_relation_rejected,
                .nominal_backing_checked_error,
                => planCauseAllowed(.expected_failure, owner_kind),
                .aggregate_expected_direct_error,
                .aggregate_child_relation_suppressed,
                .branch_expected_direct_error,
                .branch_body_error_short_circuit,
                => planCauseAllowed(.any, owner_kind),
                .record_update_field_checked_error,
                .branch_expected_compatibility_rejected,
                .branch_accumulator_fold_rejected,
                .call_callee_checked_error,
                .default_expr_checked_error,
                => planCauseAllowed(.retirement_or_diagnostic, owner_kind),
                .record_update_base_checked_error => planCauseAllowed(.any, owner_kind),
                .call_operand_checked_error => planCauseAllowed(.retirement, owner_kind),
                .parent_evidence_free,
                .aggregate_no_expected,
                .aggregate_expected_contains_error,
                .aggregate_shape_mismatch,
                .aggregate_retired_after_child_relation,
                .aggregate_retired_by_parent_branch_failure,
                .branch_retired_after_failure,
                .branch_retired_after_ambiguity_verdict,
                .record_update_field_base_checked_error,
                .record_update_projection_mismatch,
                .record_update_field_relation_rejected,
                .branch_no_expected_result,
                .branch_expected_error_short_circuit,
                .branch_body_already_expected,
                .branch_final_relation_rejected,
                .nominal_external_unresolved,
                .nominal_decl_poisoned,
                .nominal_decl_not_nominal,
                .nominal_opaque_inaccessible,
                .nominal_backing_unavailable,
                .nominal_backing_relation_rejected,
                .call_shape_ready,
                .call_shape_relation_rejected,
                .call_arity_rejected,
                .call_formal_actual_relation_rejected,
                .call_record_builder_return_rejected,
                .call_retired,
                .call_retired_after_operand_checked_error,
                .default_effectful_rejected,
                .default_type_relation_rejected,
                .default_parameter_constraint_rejected,
                .default_recursive_rejected,
                => unreachable,
            },
        };
    }

    const PlanCauseRequirement = enum {
        none,
        expected_failure,
        retirement,
        retirement_or_diagnostic,
        any,
    };

    fn planCauseAllowed(requirement: PlanCauseRequirement, owner_kind: ?CauseOwner.Kind) bool {
        const kind = owner_kind orelse return requirement == .none;
        return switch (kind) {
            .expected_failure => requirement == .expected_failure or requirement == .any,
            .expected_consumer_retirement => requirement == .retirement or
                requirement == .retirement_or_diagnostic or requirement == .any,
            .cir_diagnostic => requirement == .retirement_or_diagnostic or requirement == .any,
            .provider_where_alias_checked_error => requirement == .any,
        };
    }

    fn reasonAllowsRole(reason: Reason, role: Role) bool {
        return switch (reason) {
            .parent_evidence_free => switch (role) {
                .aggregate_owner,
                .list_element,
                .tuple_element,
                .record_field,
                .tag_payload,
                .branch_seed,
                .lambda_return,
                => true,
                .record_update_base,
                .record_update_field,
                .branch_contribution,
                .branch_final,
                .nominal_decl,
                .nominal_backing,
                .call_root,
                .call_argument,
                .default_field,
                => false,
            },
            .aggregate_no_expected,
            .aggregate_expected_contains_error,
            .aggregate_expected_direct_error,
            .aggregate_shape_mismatch,
            => switch (role) {
                .aggregate_owner,
                .list_element,
                .tuple_element,
                .record_field,
                .tag_payload,
                => true,
                .record_update_base,
                .record_update_field,
                .branch_seed,
                .branch_contribution,
                .branch_final,
                .nominal_decl,
                .nominal_backing,
                .call_root,
                .call_argument,
                .default_field,
                .lambda_return,
                => false,
            },
            .aggregate_child_relation_rejected,
            .aggregate_child_relation_suppressed,
            => switch (role) {
                .list_element,
                .tuple_element,
                .record_field,
                .tag_payload,
                => true,
                .aggregate_owner,
                .record_update_base,
                .record_update_field,
                .branch_seed,
                .branch_contribution,
                .branch_final,
                .nominal_decl,
                .nominal_backing,
                .call_root,
                .call_argument,
                .default_field,
                .lambda_return,
                => false,
            },
            .aggregate_retired_after_child_relation,
            .aggregate_retired_by_parent_branch_failure,
            => switch (role) {
                .aggregate_owner,
                .list_element,
                .tuple_element,
                .record_field,
                .tag_payload,
                => true,
                .record_update_base,
                .record_update_field,
                .branch_seed,
                .branch_contribution,
                .branch_final,
                .nominal_decl,
                .nominal_backing,
                .call_root,
                .call_argument,
                .default_field,
                .lambda_return,
                => false,
            },
            .record_update_base_checked_error,
            => role == .record_update_base,
            .record_update_field_base_checked_error,
            .record_update_projection_mismatch,
            .record_update_field_checked_error,
            .record_update_field_relation_rejected,
            => role == .record_update_field,
            .branch_no_expected_result => switch (role) {
                .branch_seed, .branch_contribution, .branch_final => true,
                .aggregate_owner,
                .list_element,
                .tuple_element,
                .record_field,
                .tag_payload,
                .record_update_base,
                .record_update_field,
                .nominal_decl,
                .nominal_backing,
                .call_root,
                .call_argument,
                .default_field,
                .lambda_return,
                => false,
            },
            .branch_expected_error_short_circuit,
            .branch_expected_direct_error,
            => switch (role) {
                .branch_seed, .branch_contribution, .branch_final => true,
                .aggregate_owner,
                .list_element,
                .tuple_element,
                .record_field,
                .tag_payload,
                .record_update_base,
                .record_update_field,
                .nominal_decl,
                .nominal_backing,
                .call_root,
                .call_argument,
                .default_field,
                .lambda_return,
                => false,
            },
            .branch_body_error_short_circuit,
            .branch_body_already_expected,
            .branch_expected_compatibility_rejected,
            .branch_accumulator_fold_rejected,
            => role == .branch_contribution,
            .branch_final_relation_rejected => role == .branch_final,
            .branch_retired_after_failure,
            .branch_retired_after_ambiguity_verdict,
            => switch (role) {
                .branch_seed, .branch_contribution, .branch_final => true,
                .aggregate_owner,
                .list_element,
                .tuple_element,
                .record_field,
                .tag_payload,
                .record_update_base,
                .record_update_field,
                .nominal_decl,
                .nominal_backing,
                .call_root,
                .call_argument,
                .default_field,
                .lambda_return,
                => false,
            },
            .nominal_external_unresolved,
            .nominal_decl_poisoned,
            .nominal_decl_not_nominal,
            .nominal_opaque_inaccessible,
            => role == .nominal_decl,
            .nominal_backing_unavailable,
            .nominal_backing_checked_error,
            .nominal_backing_relation_rejected,
            => role == .nominal_backing,
            .call_callee_checked_error,
            .call_shape_ready,
            .call_shape_relation_rejected,
            .call_arity_rejected,
            => if (reason == .call_shape_ready)
                role == .call_root
            else
                role == .call_root or role == .call_argument,
            .call_operand_checked_error,
            .call_formal_actual_relation_rejected,
            => role == .call_argument,
            .call_record_builder_return_rejected => role == .call_root,
            .call_retired,
            .call_retired_after_operand_checked_error,
            => role == .call_root or role == .call_argument,
            .default_expr_checked_error,
            .default_effectful_rejected,
            .default_type_relation_rejected,
            .default_parameter_constraint_rejected,
            .default_recursive_rejected,
            => role == .default_field,
        };
    }
};

/// One immutable checker publication for an exact failed Expected producer.
/// The closed kind selects which raw coordinates and external authority are
/// active. Every inactive optional field has the sole canonical `none` value;
/// semantic admission additionally replays the named CIR node, copy endpoint,
/// constraint/relation, and external authority.
pub const ExpectedFailure = extern struct {
    owner_node: u32,
    site_node: u32,
    owner_kind: u32,
    kind: u32,
    raw_owner_var: u32,
    raw_subject_var: u32,
    raw_peer_var: u32,
    constraint_index: u32,
    slot: u32,
    plan_index: u32,
    subject_authority: SubjectAuthority,
    cause_owner: CauseOwner,
    reserved_0: u32 = 0,

    pub const SafeList = collections.SafeList(@This());
    pub const none = std.math.maxInt(u32);

    pub const OwnerKind = enum(u32) {
        expression,
        pattern,
    };

    pub const Kind = enum(u32) {
        annotation_malformed_type,
        annotation_malformed_where,
        annotation_invalid_tag_child,
        annotation_where_receiver_not_introduced,
        annotation_where_alias_not_alias,
        annotation_recursive_where_alias,
        annotation_where_alias_publication_error,
        annotation_where_alias_unresolved,
        annotation_where_alias_arity,
        annotation_where_alias_in_type_position,
        annotation_builtin_not_type,
        annotation_recursive_type_decl,
        annotation_type_decl_poisoned,
        annotation_type_formal_poisoned,
        annotation_type_apply_arity,
        annotation_alias_row_rejected,
        annotation_external_type_unresolved,
        annotation_child_failure,
        annotation_duplicate_where_signature_rejected,
        direct_binder_lookup_checked_error,
        annotated_binding_lookup_checked_error,
        call_operand_checked_error,
        nominal_pattern_external_unresolved,
        nominal_pattern_decl_poisoned,
        nominal_pattern_opaque_inaccessible,
        nominal_pattern_backing_unavailable,
        nominal_pattern_backing_checked_error,
        nominal_pattern_backing_relation_rejected,
        aggregate_child_relation_rejected,
    };

    pub fn decodedOwnerKind(self: @This()) ?OwnerKind {
        return std.enums.fromInt(OwnerKind, self.owner_kind);
    }

    pub fn decodedKind(self: @This()) ?Kind {
        return std.enums.fromInt(Kind, self.kind);
    }

    fn isAnnotationKind(kind: Kind) bool {
        return switch (kind) {
            .annotation_malformed_type,
            .annotation_malformed_where,
            .annotation_invalid_tag_child,
            .annotation_where_receiver_not_introduced,
            .annotation_where_alias_not_alias,
            .annotation_recursive_where_alias,
            .annotation_where_alias_publication_error,
            .annotation_where_alias_unresolved,
            .annotation_where_alias_arity,
            .annotation_where_alias_in_type_position,
            .annotation_builtin_not_type,
            .annotation_recursive_type_decl,
            .annotation_type_decl_poisoned,
            .annotation_type_formal_poisoned,
            .annotation_type_apply_arity,
            .annotation_alias_row_rejected,
            .annotation_external_type_unresolved,
            .annotation_child_failure,
            .annotation_duplicate_where_signature_rejected,
            => true,
            .direct_binder_lookup_checked_error,
            .annotated_binding_lookup_checked_error,
            .call_operand_checked_error,
            .nominal_pattern_external_unresolved,
            .nominal_pattern_decl_poisoned,
            .nominal_pattern_opaque_inaccessible,
            .nominal_pattern_backing_unavailable,
            .nominal_pattern_backing_checked_error,
            .nominal_pattern_backing_relation_rejected,
            .aggregate_child_relation_rejected,
            => false,
        };
    }

    fn isNominalPatternKind(kind: Kind) bool {
        return switch (kind) {
            .nominal_pattern_external_unresolved,
            .nominal_pattern_decl_poisoned,
            .nominal_pattern_opaque_inaccessible,
            .nominal_pattern_backing_unavailable,
            .nominal_pattern_backing_checked_error,
            .nominal_pattern_backing_relation_rejected,
            => true,
            .annotation_malformed_type,
            .annotation_malformed_where,
            .annotation_invalid_tag_child,
            .annotation_where_receiver_not_introduced,
            .annotation_where_alias_not_alias,
            .annotation_recursive_where_alias,
            .annotation_where_alias_publication_error,
            .annotation_where_alias_unresolved,
            .annotation_where_alias_arity,
            .annotation_where_alias_in_type_position,
            .annotation_builtin_not_type,
            .annotation_recursive_type_decl,
            .annotation_type_decl_poisoned,
            .annotation_type_formal_poisoned,
            .annotation_type_apply_arity,
            .annotation_alias_row_rejected,
            .annotation_external_type_unresolved,
            .annotation_child_failure,
            .annotation_duplicate_where_signature_rejected,
            .direct_binder_lookup_checked_error,
            .annotated_binding_lookup_checked_error,
            .call_operand_checked_error,
            .aggregate_child_relation_rejected,
            => false,
        };
    }

    fn requiresPeer(kind: Kind) bool {
        return switch (kind) {
            .annotation_type_formal_poisoned,
            .annotation_duplicate_where_signature_rejected,
            .direct_binder_lookup_checked_error,
            .call_operand_checked_error,
            .nominal_pattern_backing_checked_error,
            .nominal_pattern_backing_relation_rejected,
            .aggregate_child_relation_rejected,
            => true,
            else => false,
        };
    }

    /// Failure kinds whose producer owns one scalar site use the sole slot
    /// value zero. The remaining kinds carry a producer-authored ordinal whose
    /// CIR bounds are replayed by checked-artifact admission.
    fn slotMustBeZero(kind: Kind) bool {
        return switch (kind) {
            .annotation_malformed_type,
            .annotation_where_alias_not_alias,
            .annotation_recursive_where_alias,
            .annotation_where_alias_publication_error,
            .annotation_where_alias_unresolved,
            .annotation_where_alias_arity,
            .annotation_where_alias_in_type_position,
            .annotation_builtin_not_type,
            .annotation_recursive_type_decl,
            .annotation_type_decl_poisoned,
            .annotation_type_apply_arity,
            .annotation_alias_row_rejected,
            .annotation_external_type_unresolved,
            .annotated_binding_lookup_checked_error,
            .nominal_pattern_external_unresolved,
            .nominal_pattern_decl_poisoned,
            .nominal_pattern_opaque_inaccessible,
            .nominal_pattern_backing_unavailable,
            .nominal_pattern_backing_checked_error,
            .nominal_pattern_backing_relation_rejected,
            => true,
            .annotation_malformed_where,
            .annotation_invalid_tag_child,
            .annotation_where_receiver_not_introduced,
            .annotation_type_formal_poisoned,
            .annotation_child_failure,
            .annotation_duplicate_where_signature_rejected,
            .aggregate_child_relation_rejected,
            .direct_binder_lookup_checked_error,
            .call_operand_checked_error,
            => false,
        };
    }

    const CauseRequirement = enum {
        none,
        cir_diagnostic,
        expected_consumer_retirement,
        provider_where_alias_checked_error,
        any,
    };

    /// One exhaustive cause-arm gate shared by the kind table. Adding a cause
    /// namespace cannot silently become legal for composite failures.
    fn causeAllowed(requirement: CauseRequirement, cause_kind: ?CauseOwner.Kind) bool {
        const kind = cause_kind orelse return requirement == .none;
        return switch (kind) {
            .expected_failure,
            => requirement == .any,
            .expected_consumer_retirement => requirement == .expected_consumer_retirement or
                requirement == .any,
            .cir_diagnostic => requirement == .cir_diagnostic or requirement == .any,
            .provider_where_alias_checked_error => requirement == .provider_where_alias_checked_error or requirement == .any,
        };
    }

    /// Validate the closed tag/active-field surface. Index bounds and the
    /// strictly-earlier local-failure cause rule require the row's pool
    /// position and are checked by `hasLegalTagsAt` and semantic admission.
    pub fn hasLegalTags(self: @This()) bool {
        const owner_kind = self.decodedOwnerKind() orelse return false;
        const kind = self.decodedKind() orelse return false;
        if (self.reserved_0 != 0 or self.owner_node == none or self.site_node == none or
            self.raw_owner_var == none or self.raw_subject_var == none or self.slot == none)
        {
            return false;
        }
        if ((isAnnotationKind(kind) or kind == .aggregate_child_relation_rejected or
            kind == .direct_binder_lookup_checked_error or
            kind == .annotated_binding_lookup_checked_error or
            kind == .call_operand_checked_error) !=
            (owner_kind == .expression)) return false;
        if (isNominalPatternKind(kind) != (owner_kind == .pattern)) return false;
        if (slotMustBeZero(kind) and self.slot != 0) return false;

        if (requiresPeer(kind) != (self.raw_peer_var != none)) return false;
        if ((kind == .annotation_duplicate_where_signature_rejected) !=
            (self.constraint_index != none)) return false;
        if ((kind == .aggregate_child_relation_rejected or kind == .call_operand_checked_error) !=
            (self.plan_index != none)) return false;

        if (!self.subject_authority.hasCanonicalTags(true)) return false;
        const cause_present = self.cause_owner.kind != CauseOwner.none;
        if (!self.cause_owner.hasCanonicalTags(cause_present)) return false;

        const subject_kind = self.subject_authority.decodedKind().?;
        const cause_kind = self.cause_owner.decodedKind();
        switch (subject_kind) {
            .direct => if (self.subject_authority.payload.direct.producer_node != self.site_node)
                return false,
            .expected_producer_root_plan => if (self.subject_authority.payload.expected_producer_root_plan.raw_var !=
                self.raw_subject_var) return false,
            .expected_relation_plan => if (self.subject_authority.payload.expected_relation_plan.raw_var !=
                self.raw_subject_var) return false,
            .expected_evidence_free_plan => if (self.subject_authority.payload.expected_evidence_free_plan.raw_var !=
                self.raw_subject_var) return false,
            .expected_copy_occurrence,
            .provider_where_alias_checked_error,
            => {},
        }

        return subjectAndCauseAllowed(
            kind,
            self.subject_authority,
            subject_kind,
            self.cause_owner,
            cause_kind,
        );
    }

    /// Structural validation which also proves the append-order certificate for
    /// a local failure cause. Semantic admission still checks exact reachability
    /// for every cause arm.
    pub fn hasLegalTagsAt(self: @This(), failure_index: u32) bool {
        if (!self.hasLegalTags()) return false;
        if (self.decodedKind() == .direct_binder_lookup_checked_error) {
            const direct = self.subject_authority.decodedDirect() orelse return false;
            if (direct.local_record_index >= failure_index) return false;
        }
        const cause = self.cause_owner.decodedExpectedFailure() orelse return true;
        return cause.index < failure_index;
    }

    fn subjectAndCauseAllowed(
        kind: Kind,
        subject: SubjectAuthority,
        subject_kind: SubjectAuthority.Kind,
        cause: CauseOwner,
        cause_kind: ?CauseOwner.Kind,
    ) bool {
        return switch (kind) {
            .annotation_malformed_type => directSubjectAllowed(subject, .annotation_malformed_type, true) and
                causeAllowed(.cir_diagnostic, cause_kind),
            .annotation_malformed_where => directSubjectAllowed(subject, .annotation_malformed_where, true) and
                causeAllowed(.cir_diagnostic, cause_kind),
            .annotation_invalid_tag_child => directSubjectAllowed(subject, .annotation_invalid_tag_child, true) and
                causeAllowed(.cir_diagnostic, cause_kind),
            .annotation_where_receiver_not_introduced => directSubjectAllowed(subject, .annotation_where_receiver_not_introduced, false) and
                causeAllowed(.none, cause_kind),
            .annotation_where_alias_not_alias => directSubjectAllowed(subject, .annotation_where_alias_not_alias, false) and
                causeAllowed(.none, cause_kind),
            .annotation_recursive_where_alias => directSubjectAllowed(subject, .annotation_recursive_where_alias, false) and
                causeAllowed(.none, cause_kind),
            .annotation_where_alias_publication_error => switch (subject_kind) {
                .direct => directSubjectAllowed(
                    subject,
                    .annotation_local_where_alias_checked_error,
                    true,
                ) and causeAllowed(.none, cause_kind),
                .provider_where_alias_checked_error => causeAllowed(
                    .provider_where_alias_checked_error,
                    cause_kind,
                ) and
                    providerSubjectMatchesCause(subject, cause),
                .expected_copy_occurrence,
                .expected_producer_root_plan,
                .expected_relation_plan,
                .expected_evidence_free_plan,
                => false,
            },
            .annotation_where_alias_unresolved => directSubjectAllowed(subject, .annotation_where_alias_unresolved, false) and
                causeAllowed(.none, cause_kind),
            .annotation_where_alias_arity => directSubjectAllowed(subject, .annotation_where_alias_arity, false) and
                causeAllowed(.none, cause_kind),
            .annotation_where_alias_in_type_position => directSubjectAllowed(subject, .annotation_where_alias_in_type_position, false) and
                causeAllowed(.none, cause_kind),
            .annotation_builtin_not_type => directSubjectAllowed(subject, .annotation_builtin_not_type, true) and
                causeAllowed(.none, cause_kind),
            .annotation_recursive_type_decl => directSubjectAllowed(subject, .annotation_recursive_type_decl, false) and
                causeAllowed(.none, cause_kind),
            .annotation_type_decl_poisoned => directSubjectAllowed(subject, .annotation_type_decl_poisoned, false) and
                causeAllowed(.none, cause_kind),
            .annotation_type_formal_poisoned => directSubjectAllowed(subject, .annotation_type_formal_poisoned, false) and
                causeAllowed(.none, cause_kind),
            .annotation_type_apply_arity => directSubjectAllowed(subject, .annotation_type_apply_arity, false) and
                causeAllowed(.none, cause_kind),
            .annotation_alias_row_rejected => directSubjectAllowed(subject, .annotation_alias_row_rejected, false) and
                causeAllowed(.none, cause_kind),
            .annotation_external_type_unresolved => directSubjectAllowed(subject, .annotation_external_type_unresolved, false) and
                causeAllowed(.none, cause_kind),
            .annotation_child_failure => switch (subject_kind) {
                .direct => directSubjectAllowed(subject, .annotation_child_before_copy, false) and
                    causeAllowed(.any, cause_kind),
                .expected_copy_occurrence => subject.decodedExpectedCopySide() == .source and
                    causeAllowed(.any, cause_kind),
                .expected_producer_root_plan,
                .expected_relation_plan,
                .expected_evidence_free_plan,
                .provider_where_alias_checked_error,
                => false,
            },
            .annotation_duplicate_where_signature_rejected => directSubjectAllowed(subject, .annotation_duplicate_where_relation, false) and
                causeAllowed(.none, cause_kind),
            .direct_binder_lookup_checked_error => directSubjectAllowed(
                subject,
                .direct_binder_lookup_checked_error,
                true,
            ) and causeAllowed(.expected_consumer_retirement, cause_kind),
            .annotated_binding_lookup_checked_error => directSubjectAllowed(
                subject,
                .annotated_binding_lookup_checked_error,
                true,
            ) and causeAllowed(.expected_consumer_retirement, cause_kind),
            .call_operand_checked_error => subject_kind == .expected_producer_root_plan and
                causeAllowed(.expected_consumer_retirement, cause_kind),
            .nominal_pattern_external_unresolved => directSubjectAllowed(subject, .nominal_pattern_external_unresolved, false) and
                causeAllowed(.none, cause_kind),
            .nominal_pattern_decl_poisoned => directSubjectAllowed(subject, .nominal_pattern_decl_poisoned, false) and
                causeAllowed(.none, cause_kind),
            .nominal_pattern_opaque_inaccessible => directSubjectAllowed(subject, .nominal_pattern_opaque_inaccessible, false) and
                causeAllowed(.none, cause_kind),
            .nominal_pattern_backing_unavailable => directSubjectAllowed(subject, .nominal_pattern_backing_unavailable, false) and
                causeAllowed(.none, cause_kind),
            .nominal_pattern_backing_checked_error => subject_kind == .expected_copy_occurrence and
                subject.decodedExpectedCopySide() == .destination and
                causeAllowed(.any, cause_kind),
            .nominal_pattern_backing_relation_rejected => subject_kind == .expected_copy_occurrence and
                subject.decodedExpectedCopySide() == .destination and
                causeAllowed(.none, cause_kind),
            .aggregate_child_relation_rejected => switch (subject_kind) {
                .expected_copy_occurrence,
                .expected_producer_root_plan,
                .expected_relation_plan,
                .expected_evidence_free_plan,
                => causeAllowed(.none, cause_kind),
                .direct,
                .provider_where_alias_checked_error,
                => false,
            },
        };
    }

    fn directSubjectAllowed(
        subject: SubjectAuthority,
        phase: SubjectAuthority.DirectPhase,
        requires_local_record: bool,
    ) bool {
        const direct = subject.decodedDirect() orelse return false;
        return subject.decodedDirectPhase() == phase and
            (direct.local_record_index != none) == requires_local_record;
    }

    fn providerSubjectMatchesCause(subject: SubjectAuthority, cause: CauseOwner) bool {
        const subject_ref = subject.decodedProviderWhereAliasCheckedError() orelse return false;
        const cause_ref = cause.decodedProviderWhereAliasCheckedError() orelse return false;
        return subject_ref.dependency_index == cause_ref.dependency_index and
            subject_ref.publication_index == cause_ref.publication_index;
    }
};

/// One exact late ambiguity verdict which retires an already-settled Expected
/// consumer group. The closed kind selects either a body-forced instantiation
/// branch (with an exact copy-time driver) or a direct creation call (whose
/// copy-driver coordinates are canonically inactive). Semantic admission
/// authenticates the selected constraint, diagnostic, and complete group.
pub const ExpectedAmbiguityRetirement = extern struct {
    raw_receiver_var: u32,
    retired_expr: u32,
    selected_constraint_index: u32,
    copy_step: u32,
    occurrence_offset: u32,
    pair_offset: u32,
    diagnostic_index: u32,
    consumer_root_plan: u32,
    consumer_plan_count: u32,
    kind: u32,
    source: u32,
    consumer_group_kind: u32,
    selection_flags: u32,
    reserved_0: u32 = 0,

    pub const SafeList = collections.SafeList(@This());
    pub const none = std.math.maxInt(u32);

    pub const Kind = enum(u32) {
        body_forced_instantiation_branch,
        creation_dispatch_call,
    };

    pub const Source = enum(u32) {
        instantiation,
        creation,
    };

    pub const ConsumerGroupKind = enum(u32) {
        branch,
        call,
    };

    pub const instantiated_where_clause_flag: u32 = 1 << 0;
    pub const only_where_clause_contracts_flag: u32 = 1 << 1;
    pub const body_forced_flag: u32 = 1 << 2;
    pub const has_where_dispatch_use_flag: u32 = 1 << 3;
    pub const known_selection_flags: u32 = instantiated_where_clause_flag |
        only_where_clause_contracts_flag |
        body_forced_flag |
        has_where_dispatch_use_flag;

    pub fn encodeSelectionFlags(
        is_instantiated_where_clause: bool,
        only_where_clause_contracts: bool,
        body_forced: bool,
        has_where_dispatch_use: bool,
    ) u32 {
        return (if (is_instantiated_where_clause) instantiated_where_clause_flag else 0) |
            (if (only_where_clause_contracts) only_where_clause_contracts_flag else 0) |
            (if (body_forced) body_forced_flag else 0) |
            (if (has_where_dispatch_use) has_where_dispatch_use_flag else 0);
    }

    pub fn decodedKind(self: @This()) ?Kind {
        return std.enums.fromInt(Kind, self.kind);
    }

    pub fn decodedSource(self: @This()) ?Source {
        return std.enums.fromInt(Source, self.source);
    }

    pub fn decodedConsumerGroupKind(self: @This()) ?ConsumerGroupKind {
        return std.enums.fromInt(ConsumerGroupKind, self.consumer_group_kind);
    }

    pub fn isInstantiatedWhereClause(self: @This()) bool {
        return (self.selection_flags & instantiated_where_clause_flag) != 0;
    }

    pub fn onlyWhereClauseContracts(self: @This()) bool {
        return (self.selection_flags & only_where_clause_contracts_flag) != 0;
    }

    pub fn isBodyForced(self: @This()) bool {
        return (self.selection_flags & body_forced_flag) != 0;
    }

    pub fn hasWhereDispatchUse(self: @This()) bool {
        return (self.selection_flags & has_where_dispatch_use_flag) != 0;
    }

    /// Stable semantic order for terminal publication. Original pool ordinals
    /// never break ties.
    pub fn canonicalLessThan(left: @This(), right: @This()) bool {
        inline for (.{
            "retired_expr",
            "kind",
            "source",
            "consumer_group_kind",
            "consumer_root_plan",
            "consumer_plan_count",
            "raw_receiver_var",
            "selected_constraint_index",
            "copy_step",
            "occurrence_offset",
            "pair_offset",
            "diagnostic_index",
            "selection_flags",
        }) |field| {
            if (@field(left, field) != @field(right, field)) {
                return @field(left, field) < @field(right, field);
            }
        }
        return false;
    }

    /// Validate the closed arms without interpreting referenced indexes.
    /// Bounds and exact semantic identity are checked by checked-artifact
    /// admission.
    pub fn hasCanonicalTags(self: @This()) bool {
        const kind = self.decodedKind() orelse return false;
        const source = self.decodedSource() orelse return false;
        const group_kind = self.decodedConsumerGroupKind() orelse return false;
        if ((self.selection_flags & ~known_selection_flags) != 0) return false;

        const copy_any = self.copy_step != none or
            self.occurrence_offset != none or self.pair_offset != none;
        const copy_all = self.copy_step != none and
            self.occurrence_offset != none and self.pair_offset != none;
        if (copy_any != copy_all) return false;

        return self.raw_receiver_var != none and
            self.retired_expr != none and
            self.selected_constraint_index != none and
            self.diagnostic_index != none and
            self.consumer_root_plan != none and
            self.consumer_plan_count != 0 and
            self.reserved_0 == 0 and
            switch (kind) {
                .body_forced_instantiation_branch => source == .instantiation and
                    group_kind == .branch and copy_all and
                    self.isInstantiatedWhereClause() and
                    self.isBodyForced() and
                    !self.hasWhereDispatchUse(),
                .creation_dispatch_call => source == .creation and
                    group_kind == .call and !copy_any and
                    self.selection_flags == encodeSelectionFlags(false, false, false, false),
            };
    }
};

/// One immutable pre-rewrite snapshot for an expression or pattern whose
/// Expected-eligible topology is no longer recoverable from its final checked
/// state. The exact `Node.Tag` and all sixteen payload bytes preserve enough
/// syntax to replay expected-consumer eligibility after an expression rewrite.
/// A poisoned pattern retains its node but loses the successful raw type
/// endpoint needed by that replay, so it uses its own closed kind. Eligible
/// retirements own a complete gapless range in `expected_retired_consumers`
/// and a complete gapless cause range in `expected_retirement_failures`;
/// preexisting and ineligible expression errors may own no failure causes.
pub const ExpectedConsumerRetirement = extern struct {
    retired_node: u32,
    owner_kind: u32,
    original_node_tag: u32,
    original_payload: [4]u32,
    kind: u32,
    retired_consumers_start: u32,
    retired_consumers_len: u32,
    diagnostic_index: u32,
    /// Complete range of checker-authored failures which caused this owner to
    /// be retired. An empty range has the sole canonical start value zero.
    expected_failures_start: u32,
    expected_failures_len: u32,
    rejection_owner_kind: u32,
    rejection_owner_index: u32,
    rejection_subject_var: u32,
    reserved_0: u32 = 0,
    reserved_1: u32 = 0,

    pub const SafeList = collections.SafeList(@This());
    pub const none = std.math.maxInt(u32);

    pub const OwnerKind = enum(u32) {
        expression,
        pattern,
    };

    pub const Kind = enum(u32) {
        preexisting_runtime_error,
        checker_rewrite_expected,
        checker_rewrite_ineligible,
        checker_poison_expected_pattern,
    };

    pub const RejectionOwnerKind = enum(u32) {
        rejected_static_dispatch,
        expected_ambiguity_retirement,
    };

    pub fn decodedKind(self: @This()) ?Kind {
        return std.enums.fromInt(Kind, self.kind);
    }

    pub fn decodedOwnerKind(self: @This()) ?OwnerKind {
        return std.enums.fromInt(OwnerKind, self.owner_kind);
    }

    pub fn decodedOriginalNodeTag(self: @This()) ?Node.Tag {
        return std.enums.fromInt(Node.Tag, self.original_node_tag);
    }

    pub fn decodedRejectionOwnerKind(self: @This()) ?RejectionOwnerKind {
        if (self.rejection_owner_kind == none) return null;
        return std.enums.fromInt(RejectionOwnerKind, self.rejection_owner_kind);
    }

    /// Validate the closed owner namespace and retirement-kind pairing. Bounds,
    /// payload/tag compatibility, diagnostics, consumer ranges, and exact final
    /// node state are checked by semantic admission.
    pub fn hasLegalTags(self: @This()) bool {
        const owner_kind = self.decodedOwnerKind() orelse return false;
        const kind = self.decodedKind() orelse return false;
        const original_tag = self.decodedOriginalNodeTag() orelse return false;
        const legal_owner_and_range = switch (kind) {
            .preexisting_runtime_error => owner_kind == .expression and
                original_tag == .malformed and self.retired_consumers_len == 0,
            .checker_rewrite_expected => owner_kind == .expression and
                original_tag != .malformed and self.retired_consumers_len != 0,
            .checker_rewrite_ineligible => owner_kind == .expression and
                original_tag != .malformed and self.retired_consumers_len == 0,
            .checker_poison_expected_pattern => owner_kind == .pattern and
                (original_tag == .pattern_nominal or original_tag == .pattern_nominal_external) and
                self.retired_consumers_len != 0,
        };
        const has_terminal_authority = switch (kind) {
            .preexisting_runtime_error => self.diagnostic_index != none and
                self.expected_failures_len == 0,
            .checker_rewrite_expected, .checker_rewrite_ineligible => self.diagnostic_index != none,
            .checker_poison_expected_pattern => self.diagnostic_index == none and
                self.expected_failures_len != 0,
        };
        return self.reserved_0 == 0 and self.reserved_1 == 0 and
            legal_owner_and_range and has_terminal_authority and
            self.hasCanonicalExpectedFailureRange() and
            self.hasCanonicalRejectionOwner();
    }

    /// Validate the owned cause range's unique empty encoding. Nonempty range
    /// bounds, ordering, uniqueness, and owner identity require the failure
    /// pool and are checked by semantic admission.
    pub fn hasCanonicalExpectedFailureRange(self: @This()) bool {
        return self.expected_failures_len != 0 or self.expected_failures_start == 0;
    }

    /// Validate the optional rejection-owner union's canonical inactive form.
    /// Bounds and subject identity are checked by semantic admission.
    pub fn hasCanonicalRejectionOwner(self: @This()) bool {
        if (self.rejection_owner_kind == none) {
            return self.rejection_owner_index == none and self.rejection_subject_var == none;
        }
        return self.decodedRejectionOwnerKind() != null and
            self.rejection_owner_index != none and
            self.rejection_subject_var != none;
    }

    comptime {
        if (@sizeOf(Node.Payload) != @sizeOf([4]u32)) {
            @compileError("ExpectedConsumerRetirement must snapshot the complete Node.Payload");
        }
    }
};

/// One exact checker-authored failure owned by an Expected-consumer
/// retirement. The retirement's gapless range is the complete cause set.
pub const ExpectedRetirementFailure = extern struct {
    failure_index: u32,

    pub const SafeList = collections.SafeList(@This());
    pub const none = std.math.maxInt(u32);

    /// Bounds, stable-key order, uniqueness, and retirement owner identity are
    /// checked by semantic admission.
    pub fn hasLegalTags(self: @This()) bool {
        return self.failure_index != none;
    }
};

/// One exact expected-consumer site owned by an eligible checker retirement.
/// `plan_index` makes the plan/retirement relation explicit and bijective;
/// duplicated role/slot claims cannot substitute for that identity.
pub const ExpectedRetiredConsumer = extern struct {
    plan_index: u32,
    owner_node: u32,
    site_node: u32,
    role: u32,
    slot: u32,
    raw_owner_var: u32,
    raw_consumer_var: u32,
    reason: u32,
    reserved_0: u32 = 0,

    pub const SafeList = collections.SafeList(@This());

    /// Finite reasons which belong only to a retirement's durable consumer
    /// range. They deliberately do not extend `ExpectedConsumptionPlan.Reason`:
    /// the referenced plan keeps its producer-authored outcome and cause.
    pub const RetirementOnlyReason = enum(u32) {
        record_update_retired_after_base_checked_error = 0x8000_0000,

        comptime {
            for (std.meta.fields(ExpectedConsumptionPlan.Reason)) |plan_reason| {
                for (std.meta.fields(@This())) |retirement_reason| {
                    if (plan_reason.value == retirement_reason.value) {
                        @compileError("retirement-only reasons must remain disjoint from Expected plan reasons");
                    }
                }
            }
        }
    };

    pub fn decodedRole(self: @This()) ?ExpectedConsumptionPlan.Role {
        return std.enums.fromInt(ExpectedConsumptionPlan.Role, self.role);
    }

    pub fn decodedReason(self: @This()) ?ExpectedConsumptionPlan.Reason {
        return std.enums.fromInt(ExpectedConsumptionPlan.Reason, self.reason);
    }

    pub fn decodedRetirementOnlyReason(self: @This()) ?RetirementOnlyReason {
        return std.enums.fromInt(RetirementOnlyReason, self.reason);
    }
};

/// Canonicalization-authored proof that one shared-tag `.malformed` node was
/// created in the `Expr.Idx` namespace. Checked-cache admission replays this
/// complete stream against the freshly canonicalized module before accepting
/// a preexisting runtime-error retirement.
pub const MalformedExpressionPublication = extern struct {
    expr_node: u32,
    diagnostic_index: u32,

    pub const SafeList = collections.SafeList(@This());
    pub const none = std.math.maxInt(u32);

    pub fn hasLegalTags(self: @This()) bool {
        return self.expr_node != none and self.diagnostic_index != none;
    }
};

/// Canonicalization-authored proof that one shared-tag `.malformed` node was
/// created in the `TypeAnno.Idx` namespace. This is a distinct complete stream
/// from malformed expressions so neither namespace can authorize the other.
pub const MalformedTypeAnnotationPublication = extern struct {
    annotation_node: u32,
    diagnostic_index: u32,

    pub const SafeList = collections.SafeList(@This());
    pub const none = std.math.maxInt(u32);

    pub fn hasLegalTags(self: @This()) bool {
        return self.annotation_node != none and self.diagnostic_index != none;
    }
};

/// Canonicalization-authored proof of one exact annotation attachment whose
/// body is checked. The closed kind establishes whether `attachment_node` is
/// a Def, local declaration statement, or initialized local variable
/// statement; staged associated-value Defs never publish this row.
pub const BodyAnnotationAttachment = extern struct {
    attachment_kind: u32,
    attachment_node: u32,
    annotation_root: u32,
    body_expr: u32,

    pub const SafeList = collections.SafeList(@This());
    pub const none = std.math.maxInt(u32);

    pub const AttachmentKind = enum(u32) {
        top_level_def,
        local_decl,
        local_var,
    };

    pub fn decodedAttachmentKind(self: @This()) ?AttachmentKind {
        return std.enums.fromInt(AttachmentKind, self.attachment_kind);
    }

    pub fn hasLegalTags(self: @This()) bool {
        return self.decodedAttachmentKind() != null and
            self.attachment_node != none and
            self.annotation_root != none and
            self.body_expr != none;
    }
};

/// Canonicalization-authored join between one body annotation and one exact
/// malformed type-annotation leaf owned by it. The leaf publication proves the
/// shared-tag `TypeAnno.Idx` namespace; this row separately proves the
/// containing `Annotation.Idx` and the exact expression which consumes it.
pub const BodyAnnotationMalformedTypePublication = extern struct {
    attachment_kind: u32,
    attachment_node: u32,
    annotation_root: u32,
    body_expr: u32,
    malformed_type_publication_index: u32,

    pub const SafeList = collections.SafeList(@This());
    pub const none = std.math.maxInt(u32);

    pub const AttachmentKind = BodyAnnotationAttachment.AttachmentKind;

    pub fn decodedAttachmentKind(self: @This()) ?AttachmentKind {
        return std.enums.fromInt(AttachmentKind, self.attachment_kind);
    }

    pub fn hasLegalTags(self: @This()) bool {
        return self.decodedAttachmentKind() != null and
            self.attachment_node != none and
            self.annotation_root != none and
            self.body_expr != none and
            self.malformed_type_publication_index != none;
    }
};

/// A stable append-only slot reserved before a local Instantiator traversal.
/// The checker must replace this payload in place with the traversal's exact
/// completed origin before the step can reach checked-module publication.
/// Admission rejects this state unconditionally.
pub const WhereMarkerReservedOrigin = extern struct {
    reserved_0: u32 = 0,
    reserved_1: u32 = 0,
    reserved_2: u32 = 0,
    reserved_3: u32 = 0,
    reserved_4: u32 = 0,
    reserved_5: u32 = 0,
};

/// Checker-only tombstone for a completed publish-if-marker traversal which
/// produced no retained support and was not referenced by a nested producer.
/// Terminal rebuilding proves the slot globally unreferenced and prunes it;
/// checked-module admission rejects it unconditionally.
pub const WhereMarkerDiscardedOrigin = extern struct {
    reserved_0: u32 = 0,
    reserved_1: u32 = 0,
    reserved_2: u32 = 0,
    reserved_3: u32 = 0,
    reserved_4: u32 = 0,
    reserved_5: u32 = 0,
};

/// Fixed-width tagged payload for `WhereMarkerCopyStep`. The active member is
/// selected exclusively by `WhereMarkerCopyStep.kind`; every inactive byte is
/// required to be zero by producer and admission validation so serialization
/// is deterministic.
pub const WhereMarkerCopyOrigin = extern union {
    pub const serialized_portable_extern_union = true;

    external_cir_node: WhereMarkerNodeOrigin,
    external_numeric_suffix: WhereMarkerNodeOrigin,
    external_where_alias_receiver: WhereMarkerNodeOrigin,
    external_where_alias_parameter: WhereMarkerWhereAliasParameterOrigin,
    external_cache_seed: WhereMarkerExternalCacheSeedOrigin,
    binding_codec_receiver: WhereMarkerBindingCodecOrigin,
    binding_codec_function: WhereMarkerBindingCodecOrigin,
    selected_dispatch_method: WhereMarkerSelectedMethodOrigin,
    generated_codec_method: WhereMarkerGeneratedCodecMethodOrigin,
    inspect_method: WhereMarkerInspectMethodOrigin,
    associated_method: WhereMarkerAssociatedMethodOrigin,
    default_method: WhereMarkerDefaultMethodOrigin,
    default_method_use: WhereMarkerDefaultMethodUseOrigin,
    candidate_probe_method_root: WhereMarkerCandidateProbeMethodOrigin,
    candidate_probe_method_use: WhereMarkerCandidateProbeMethodOrigin,
    platform_requirement: WhereMarkerPlatformRequirementOrigin,
    scheme_use: WhereMarkerSchemeUseOrigin,
    type_annotation: WhereMarkerNodeOrigin,
    numeric_suffix_use: WhereMarkerNodeOrigin,
    nominal_expr_decl: WhereMarkerNodeOrigin,
    nominal_expr_backing: WhereMarkerNodeOrigin,
    nominal_pattern_decl: WhereMarkerNodeOrigin,
    nominal_pattern_backing: WhereMarkerNodeOrigin,
    required_lookup: WhereMarkerNodeOrigin,
    platform_requirement_instance: WhereMarkerPlatformInstanceOrigin,
    platform_alias_app_decl: WhereMarkerPlatformAliasAppDeclOrigin,
    default_field_type: WhereMarkerDefaultFieldTypeOrigin,
    record_update_base: WhereMarkerRecordUpdateBaseOrigin,
    aggregate_fresh_shape_child: WhereMarkerAggregateFreshShapeChildOrigin,
    predeclared_annotation: WhereMarkerNodeOrigin,
    generated_codec_snapshot: WhereMarkerNodeOrigin,
    expected_call_instantiation: WhereMarkerExpectedCallInstantiationOrigin,
    aggregate_expected_projection: WhereMarkerExpectedProjectionOrigin,
    branch_expected_copy: WhereMarkerExpectedProjectionOrigin,
    reserved: WhereMarkerReservedOrigin,
    discarded: WhereMarkerDiscardedOrigin,
};

/// One producer-authored edge or authenticated cut in a where-marker copy
/// relation. Occurrence offsets are relative to the owning copy step and bind
/// the edge to immutable copy-time source/destination requests; each occurrence
/// separately names its canonical resolved pair. The locator names the
/// source-side graph edge without depending on TypeStore child-pool offsets;
/// fields unused by its `edge_kind`, `action`, or auxiliary origin are always
/// zero.
pub const WhereMarkerCopyWitness = extern struct {
    parent_occurrence_offset: u32,
    child_occurrence_offset: u32,
    edge_kind: u32,
    edge_index: u32,
    edge_name: u32,
    edge_origin_module: u32,
    edge_source_decl: u32,
    /// Owner-relative exact constraint-copy row for constraint/interpolation
    /// edges, or `none`. This is independent of the action's auxiliary cut:
    /// one edge can require both its constraint correspondence and (for
    /// example) a binding-copy or platform-preseed authority.
    constraint_pair_offset: u32 = std.math.maxInt(u32),
    action: u32,
    auxiliary_origin_kind: u32,
    /// Exact prior copy step which owns `auxiliary_origin_index` for a
    /// `binding_copy_pair` cut. Every other auxiliary kind canonically uses
    /// zero. Keeping the step explicit lets later binding components reuse a
    /// mapping authored by an earlier component without scanning solved
    /// graphs or guessing which equal pair produced it.
    auxiliary_origin_step: u32 = 0,
    auxiliary_origin_index: u32,
    /// Exact copy-time occurrences for actions whose authority is destroyed by
    /// later unification. Ordinary traversal and external cuts use `none` in
    /// both fields; admission rejects inactive nonzero data.
    raw_source_var: u32 = std.math.maxInt(u32),
    raw_destination_var: u32 = std.math.maxInt(u32),

    pub const SafeList = collections.SafeList(@This());

    /// Finite source-descriptor edges, ordered by this tag and then the
    /// edge-specific locator fields. `edge_index` is an argument, element,
    /// payload, part, dependency, or requirement ordinal. Named rows also use
    /// `edge_name`; alias and nominal argument rows additionally use the raw
    /// module-local origin and packed `types.SourceDecl` bits.
    pub const EdgeKind = enum(u32) {
        /// A typed root action for every root-level non-traverse cut or
        /// creation, or for an ordinary traverse with no structural outgoing
        /// edge. Both occurrence offsets name the root occurrence. A
        /// non-traverse action may coexist with structural or detached virtual
        /// witnesses; this row is never a graph self-edge for BFS purposes.
        root_copy_action,
        static_dispatch_function,
        interpolation_part,
        interpolation_item,
        alias_backing,
        alias_argument,
        tuple_element,
        nominal_argument,
        function_argument,
        function_return,
        function_effect_dependency,
        record_field_type,
        record_field_presence,
        record_extension,
        record_unbound_field_type,
        record_unbound_field_presence,
        tag_payload,
        tag_extension,
        scheme_requirement_receiver,
        scheme_requirement_function,
    };

    /// Whether replay follows the ordinary source/destination child edge,
    /// authenticates one of the finite copy-policy cuts, or records the first
    /// fresh-flex allocation. A cycle/share revisit is an ordinary `traverse`
    /// edge to an already discovered pair.
    pub const Action = enum(u32) {
        traverse,
        local_raw_identity_share_cut,
        rigid_fresh_flex_cut,
        rigid_fresh_rigid_cut,
        exact_annotation_substitution_cut,
        polarity_open_cut,
        polarity_close_cut,
        polarity_preserve_cut,
        binding_codec_reuse_cut,
        platform_preseed_cut,
        requirement_component_ingress,
        flex_fresh_flex_copy,
        requirement_component_fresh_flex_copy,
    };

    /// The table whose exact row or owner-relative offset is named by
    /// `auxiliary_origin_index`. `binding_copy_pair` additionally uses
    /// `auxiliary_origin_step`; all other kinds require that step word to be
    /// zero. The action determines which kinds are legal; `.none` requires an
    /// all-zero auxiliary coordinate.
    pub const AuxiliaryOriginKind = enum(u32) {
        none,
        scheme_use,
        scheme_use_pair,
        binding_scheme_codec_requirement,
        platform_substitution,
        binding_copy_pair,
        predeclared_annotation_event,
        annotation_substitution,
    };

    pub fn decodedEdgeKind(self: @This()) ?EdgeKind {
        return std.enums.fromInt(EdgeKind, self.edge_kind);
    }

    pub fn decodedAction(self: @This()) ?Action {
        return std.enums.fromInt(Action, self.action);
    }

    pub fn decodedAuxiliaryOriginKind(self: @This()) ?AuxiliaryOriginKind {
        return std.enums.fromInt(AuxiliaryOriginKind, self.auxiliary_origin_kind);
    }

    /// One canonical byte order shared by cross-module copying, local
    /// instantiation, checked-boundary rebuilding, and cache admission.
    pub fn canonicalLessThan(a: @This(), b: @This()) bool {
        inline for (.{
            "parent_occurrence_offset",
            "edge_kind",
            "edge_index",
            "edge_name",
            "edge_origin_module",
            "edge_source_decl",
            "constraint_pair_offset",
            "action",
            "auxiliary_origin_kind",
            "auxiliary_origin_step",
            "auxiliary_origin_index",
            "child_occurrence_offset",
            "raw_source_var",
            "raw_destination_var",
        }) |field| {
            if (@field(a, field) != @field(b, field)) {
                return @field(a, field) < @field(b, field);
            }
        }
        return false;
    }
};

/// Exact source-to-destination static-dispatch occurrence relation for one
/// graph copy. Rows are sorted strictly by source constraint index and are a
/// complete function over constraints actually appended by the copy. A
/// terminating shared-var cut emits no row for constraints on the shared
/// descriptor. A copy witness that traverses a constraint receiver/function
/// edge names the owner-relative row offset instead of recovering a
/// correspondence from the final graph.
pub const WhereMarkerConstraintCopyPair = extern struct {
    source_constraint_index: u32,
    destination_constraint_index: u32,

    pub const SafeList = collections.SafeList(@This());

    pub fn canonicalLessThan(a: @This(), b: @This()) bool {
        if (a.source_constraint_index != b.source_constraint_index) {
            return a.source_constraint_index < b.source_constraint_index;
        }
        return a.destination_constraint_index < b.destination_constraint_index;
    }
};

/// Proof-carrying lineage for one marker-bearing graph copy. Root steps bind a
/// cross-module copy to one finite canonical source occurrence; instantiate
/// steps bind a fresh local occurrence to an earlier step. `pairs` is always a
/// complete, gaplessly owned canonical source-to-destination graph relation;
/// `occurrences` preserves every immutable raw request and binds it to a pair;
/// and `witnesses` is the complete gaplessly owned edge/cut replay certificate
/// over those occurrences.
pub const WhereMarkerCopyStep = extern struct {
    kind: u32,
    copy_policy: u32,
    source_root_var: u32,
    destination_root_var: u32,
    pairs_start: u32,
    pairs_len: u32,
    occurrences_start: u32 = 0,
    occurrences_len: u32 = 0,
    root_occurrence_offset: u32 = 0,
    constraint_pairs_start: u32 = 0,
    constraint_pairs_len: u32 = 0,
    witnesses_start: u32 = 0,
    witnesses_len: u32 = 0,
    copied_groups_start: u32 = 0,
    copied_groups_len: u32 = 0,
    origin: WhereMarkerCopyOrigin,

    pub const SafeList = collections.SafeList(@This());

    /// Closed producer operation. The validator switches exhaustively over
    /// this enum and then admits only the finite subset legal for the step's
    /// typed origin. Polarity-sensitive policies preserve the root polarity;
    /// the replay flips it only across witnessed function-argument edges.
    pub const CopyPolicy = enum(u32) {
        cross_module_import,
        ranked_fresh_flex_close,
        ranked_fresh_flex_close_scheme,
        ranked_fresh_flex_preserve,
        ranked_fresh_flex_resolve_positive,
        ranked_fresh_flex_resolve_negative,
        ranked_fresh_flex_defer_positive,
        ranked_fresh_flex_defer_negative,
        ranked_substitute_rigids_close,
        ranked_substitute_rigids_preserve,
        ranked_substitute_rigids_resolve_positive,
        ranked_substitute_rigids_resolve_negative,
        ranked_substitute_rigids_defer_positive,
        ranked_substitute_rigids_defer_negative,
        all_fresh_rigid_preserve,
        all_fresh_flex_preserve,
        all_share_leaves_resolve_positive,
    };

    pub const Kind = enum(u32) {
        external_cir_node,
        external_numeric_suffix,
        external_where_alias_receiver,
        external_where_alias_parameter,
        external_cache_seed,
        binding_codec_receiver,
        binding_codec_function,
        selected_dispatch_method,
        generated_codec_method,
        inspect_method,
        associated_method,
        default_method,
        default_method_use,
        candidate_probe_method_root,
        candidate_probe_method_use,
        platform_requirement,
        scheme_use,
        type_annotation,
        numeric_suffix_use,
        nominal_expr_decl,
        nominal_expr_backing,
        nominal_pattern_decl,
        nominal_pattern_backing,
        required_lookup,
        platform_requirement_instance,
        platform_alias_app_decl,
        default_field_type,
        record_update_base,
        aggregate_fresh_shape_child,
        predeclared_annotation,
        generated_codec_snapshot,
        expected_call_instantiation,
        aggregate_expected_projection,
        branch_expected_copy,
        /// Transient checker-only placeholder. Checked-module admission and
        /// terminal publication reject every surviving reserved step.
        reserved,
        /// Completed checker-only tombstone; terminal publication may only
        /// prune it after proving no durable or transient owner references it.
        discarded,

        /// Store which owns the source side of this copy relation. Every
        /// durable step has either a checker-local source or an admitted
        /// cross-module source; reservation/tombstone states have neither.
        pub const SourceNamespace = enum {
            local,
            cross_module,
            transient,
        };

        pub fn sourceNamespace(self: @This()) SourceNamespace {
            return switch (self) {
                .scheme_use,
                .default_method_use,
                .type_annotation,
                .numeric_suffix_use,
                .nominal_expr_decl,
                .nominal_expr_backing,
                .nominal_pattern_decl,
                .nominal_pattern_backing,
                .required_lookup,
                .platform_requirement_instance,
                .platform_alias_app_decl,
                .default_field_type,
                .record_update_base,
                .aggregate_fresh_shape_child,
                .predeclared_annotation,
                .generated_codec_snapshot,
                .expected_call_instantiation,
                .aggregate_expected_projection,
                .branch_expected_copy,
                .candidate_probe_method_use,
                => .local,
                .external_cir_node,
                .external_numeric_suffix,
                .external_where_alias_receiver,
                .external_where_alias_parameter,
                .external_cache_seed,
                .binding_codec_receiver,
                .binding_codec_function,
                .selected_dispatch_method,
                .generated_codec_method,
                .inspect_method,
                .associated_method,
                .default_method,
                .candidate_probe_method_root,
                .platform_requirement,
                => .cross_module,
                .reserved,
                .discarded,
                => .transient,
            };
        }
    };

    pub fn decodedKind(self: @This()) ?Kind {
        return std.enums.fromInt(Kind, self.kind);
    }

    pub fn decodedCopyPolicy(self: @This()) ?CopyPolicy {
        return std.enums.fromInt(CopyPolicy, self.copy_policy);
    }
};

/// Closed identity of the copied graph component which owns one fresh
/// open-literal receiver. Every payload word inactive for `kind` is zero, so
/// the fixed-width representation has one canonical serialized encoding.
pub const CopiedOpenLiteralComponent = extern struct {
    kind: u32,
    binding_root_step: u32,
    requirement_ordinal: u32,

    pub const none = std.math.maxInt(u32);

    pub const Kind = enum(u32) {
        root_graph,
        scheme_requirement,
        binding_codec_receiver,
        binding_codec_function,
    };

    pub const SchemeRequirement = extern struct {
        requirement_ordinal: u32,
    };

    pub const BindingCodec = extern struct {
        binding_root_step: u32,
        requirement_ordinal: u32,
    };

    pub fn rootGraph() @This() {
        return .{
            .kind = @intFromEnum(Kind.root_graph),
            .binding_root_step = 0,
            .requirement_ordinal = 0,
        };
    }

    pub fn schemeRequirement(requirement_ordinal: u32) @This() {
        return .{
            .kind = @intFromEnum(Kind.scheme_requirement),
            .binding_root_step = 0,
            .requirement_ordinal = requirement_ordinal,
        };
    }

    pub fn bindingCodecReceiver(
        binding_root_step: u32,
        requirement_ordinal: u32,
    ) @This() {
        return .{
            .kind = @intFromEnum(Kind.binding_codec_receiver),
            .binding_root_step = binding_root_step,
            .requirement_ordinal = requirement_ordinal,
        };
    }

    pub fn bindingCodecFunction(
        binding_root_step: u32,
        requirement_ordinal: u32,
    ) @This() {
        return .{
            .kind = @intFromEnum(Kind.binding_codec_function),
            .binding_root_step = binding_root_step,
            .requirement_ordinal = requirement_ordinal,
        };
    }

    pub fn decodedKind(self: @This()) ?Kind {
        return std.enums.fromInt(Kind, self.kind);
    }

    pub fn decodedSchemeRequirement(self: @This()) ?SchemeRequirement {
        if (self.decodedKind() != .scheme_requirement) return null;
        return .{ .requirement_ordinal = self.requirement_ordinal };
    }

    pub fn decodedBindingCodecReceiver(self: @This()) ?BindingCodec {
        if (self.decodedKind() != .binding_codec_receiver) return null;
        return .{
            .binding_root_step = self.binding_root_step,
            .requirement_ordinal = self.requirement_ordinal,
        };
    }

    pub fn decodedBindingCodecFunction(self: @This()) ?BindingCodec {
        if (self.decodedKind() != .binding_codec_function) return null;
        return .{
            .binding_root_step = self.binding_root_step,
            .requirement_ordinal = self.requirement_ordinal,
        };
    }

    /// Validate the closed tag and the sole canonical encoding of every
    /// inactive payload word. Referenced-row bounds are validated by checked
    /// admission against the owning copy step.
    pub fn hasCanonicalTags(self: @This()) bool {
        return switch (self.decodedKind() orelse return false) {
            .root_graph => self.binding_root_step == 0 and
                self.requirement_ordinal == 0,
            .scheme_requirement => self.binding_root_step == 0 and
                self.requirement_ordinal != none,
            .binding_codec_receiver, .binding_codec_function => self.binding_root_step != none and self.requirement_ordinal != none,
        };
    }

    /// Canonical component-key order: tag followed by the complete active
    /// payload in its declared arm order.
    pub fn canonicalLessThan(left: @This(), right: @This()) bool {
        inline for (.{ "kind", "binding_root_step", "requirement_ordinal" }) |field| {
            if (@field(left, field) != @field(right, field)) {
                return @field(left, field) < @field(right, field);
            }
        }
        return false;
    }
};

/// Receiver-level authority for one freshly copied open-literal component.
/// Copy-step and group event ranges form gapless partitions validated at
/// checked-module admission.
pub const CopiedOpenLiteralGroup = extern struct {
    copy_step_index: u32,
    receiver_occurrence_offset: u32,
    source_constraints_start: u32,
    source_constraints_len: u32,
    destination_constraints_start: u32,
    destination_constraints_len: u32,
    component: CopiedOpenLiteralComponent,
    events_start: u32,
    events_len: u32,

    pub const none = std.math.maxInt(u32);
    pub const SafeList = collections.SafeList(@This());

    pub fn hasCanonicalTags(self: @This()) bool {
        return self.copy_step_index != none and
            self.receiver_occurrence_offset != none and
            self.source_constraints_start != none and
            self.source_constraints_len != 0 and
            self.source_constraints_len == self.destination_constraints_len and
            self.destination_constraints_start != none and
            self.events_start != none and
            self.events_len != 0 and
            self.component.hasCanonicalTags();
    }

    /// Pool order is copy-step order followed by the exact within-step key
    /// required by the copied-open-literal inventory.
    pub fn canonicalLessThan(left: @This(), right: @This()) bool {
        if (left.copy_step_index != right.copy_step_index) {
            return left.copy_step_index < right.copy_step_index;
        }
        if (CopiedOpenLiteralComponent.canonicalLessThan(left.component, right.component)) {
            return true;
        }
        if (CopiedOpenLiteralComponent.canonicalLessThan(right.component, left.component)) {
            return false;
        }
        inline for (.{
            "receiver_occurrence_offset",
            "source_constraints_start",
            "source_constraints_len",
            "destination_constraints_start",
            "destination_constraints_len",
        }) |field| {
            if (@field(left, field) != @field(right, field)) {
                return @field(left, field) < @field(right, field);
            }
        }
        return false;
    }
};

/// One copied literal-conversion constraint. A group's event slice is strictly
/// ordered by `constraint_offset`; `literal_kind` is copied from the shared
/// literal-defaulting classification at the copy boundary.
pub const CopiedOpenLiteralEvent = extern struct {
    group_index: u32,
    constraint_offset: u32,
    literal_kind: u32,

    pub const none = std.math.maxInt(u32);
    pub const SafeList = collections.SafeList(@This());

    pub const LiteralKind = enum(u32) {
        numeral,
        quote,
        interpolation,
    };

    pub fn decodedLiteralKind(self: @This()) ?LiteralKind {
        return std.enums.fromInt(LiteralKind, self.literal_kind);
    }

    pub fn hasCanonicalTags(self: @This()) bool {
        return self.group_index != none and
            self.constraint_offset != none and
            self.decodedLiteralKind() != null;
    }

    pub fn canonicalLessThan(left: @This(), right: @This()) bool {
        if (left.group_index != right.group_index) {
            return left.group_index < right.group_index;
        }
        return left.constraint_offset < right.constraint_offset;
    }
};

/// One source-clause contribution to a reusable where-alias declaration.
/// Rows are ordered by declaration, exported method, then source clause.
/// `retained_constraint_index` names the declaration's method-key
/// representative after duplicate written clauses were related. An alias
/// source clause contributes one row for every distinct method it expands.
pub const WhereAliasExpansion = extern struct {
    alias_decl_node: u32,
    source_where_node: u32,
    method_ident: u32,
    retained_constraint_index: u32,

    pub const SafeList = collections.SafeList(@This());
};

/// Settlement certificate for one reusable where-alias declaration. Ready
/// declarations own one exact slice of `where_alias_expansions`; an empty
/// slice is a valid ready export. Checked-error declarations own no rows.
pub const WhereAliasDeclarationPublication = extern struct {
    decl_node: u32,
    dependency_rank: u32,
    expansions_start: u32,
    expansions_len: u32,
    outcome: u32,

    pub const SafeList = collections.SafeList(@This());

    pub const Outcome = enum(u32) {
        ready,
        checked_error,
    };

    pub fn decodedOutcome(self: @This()) ?Outcome {
        return std.enums.fromInt(Outcome, self.outcome);
    }
};

/// Durable form of `types.Instantiator.MarkerPathStep`. Every discriminant is
/// decoded explicitly before checked consumers walk it.
pub const WhereMethodMarkerPathStep = extern struct {
    kind: u32,
    index: u32,
    arity: u32,
    name: u32,
    origin_module: u32,
    source_decl: u32,

    pub const SafeList = collections.SafeList(@This());

    pub const Kind = enum(u32) {
        fn_arg,
        fn_ret,
        nominal_arg,
        tuple_elem,
        record_field,
        tag_payload,
    };

    pub fn kindOrNull(self: @This()) ?Kind {
        return std.enums.fromInt(Kind, self.kind);
    }
};

/// One positive tag row in a settled method scheme. Checking enumerates these
/// from the method's own checked type at its body/group boundary. Target
/// instantiation maps `row_var` and `tail_var` through its explicit copy maps;
/// obligation discharge joins only equal producer-authored paths.
pub const MethodOutputRow = extern struct {
    row_var: u32,
    tail_var: u32,
    position: u32,
    path_start: u32,
    path_len: u32,

    pub const SafeList = collections.SafeList(@This());
};

/// Explicit settled output-row publication for one method binding. A zero
/// `rows_len` is meaningful: the method was checked and has no positive tag
/// rows. Consumers distinguish that state from an unpublished method whose
/// body has not reached its checking boundary.
pub const MethodOutputPublication = extern struct {
    type_node_idx: u32,
    rows_start: u32,
    rows_len: u32,

    pub const SafeList = collections.SafeList(@This());
};

/// Exact selected dispatch occurrence authorized to request a wider result
/// row. `source_where_fn_var` names the per-body-use record that proved the
/// authority; consumers never infer it from a solved callable class.
pub const ResultRowWideningUse = extern struct {
    constraint_fn_var: u32,
    source_where_fn_var: u32,
    widening: u32,

    pub const SafeList = collections.SafeList(@This());
};

/// One (source scheme var → fresh instantiated var) pair of a
/// `SchemeUseRecord`. Ordinary evidence records retain constrained vars;
/// `where_method_use` retains the instantiator's complete structural map.
pub const SchemeUsePair = extern struct {
    /// Source var in the pristine scheme (`Var`).
    old_var: u32,
    /// The fresh copy created for this instantiation (`Var`).
    fresh_var: u32,

    pub const SafeList = collections.SafeList(@This());
};

/// One checker-authored relation between callable targets merged while a type
/// scheme is generalized. All variables are raw `Var` witnesses;
/// checked-artifact construction resolves them only after checking has settled
/// the type store.
pub const GeneralizedDispatchTargetShare = extern struct {
    receiver_var: u32,
    method_ident: u32,
    omitted_fn_var: u32,
    retained_fn_var: u32,
    /// For `where_method_use`, the exact raw callable key of the validating
    /// `SchemeUseRecord`. For `shape_only`, this equals `omitted_fn_var` and is
    /// not interpreted as a scheme-use key.
    proof_fn_var: u32,
    proof_kind: u32,

    pub const SafeList = collections.SafeList(@This());

    pub const ProofKind = enum(u32) {
        /// The two callables had the same generalized callable shape.
        shape_only,
        /// A raw where-method use copied the retained signature into the
        /// omitted callable's settled class.
        where_method_use,
    };
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

/// One generated-codec dispatch relation that remains part of a binding's
/// scheme across checked-module boundaries. `scheme_root` preserves alias
/// identity while an importer copies the checked scheme, and
/// `constraint_index` names the exact `StaticDispatchConstraint` in this
/// environment's serialized `TypeStore`; import copying therefore preserves
/// the complete callable graph and metadata without reconstructing either from
/// the receiver's final shape.
pub const BindingSchemeCodecRequirement = extern struct {
    node_idx: u32,
    scheme_root: u32,
    receiver_var: u32,
    constraint_index: u32,

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
/// Explicit one-shot typechecking state. Unlike checker-local traversal state,
/// this is serialized so a checked cache env cannot be mistaken for a fresh
/// canonical input.
typecheck_state: TypecheckState,
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

/// Runtime-only proof that every persisted W6b table passed semantic
/// validation after the latest checked-boundary rebuild. This is deliberately
/// not serialized: relocation validates byte bounds, while cache/Builtin load
/// re-establishes this stronger guarantee before consumers run.
w6b_semantically_validated: bool,

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
/// Flat pool of (source scheme var → fresh var) pairs backing
/// `scheme_uses`.
scheme_use_pairs: SchemeUsePair.SafeList,
/// Per-marker rows opened by `where_method_use` records.
where_method_marker_uses: WhereMethodMarkerUse.SafeList,
/// Guarded structural paths backing `where_method_marker_uses`.
where_method_marker_path_steps: WhereMethodMarkerPathStep.SafeList,
/// Exact flattened source-clause membership for reusable where aliases.
where_alias_expansions: WhereAliasExpansion.SafeList,
/// Exact ready/error outcome and dependency rank for every reusable alias.
where_alias_declaration_publications: WhereAliasDeclarationPublication.SafeList,
/// Source-authored method occurrences from this module's one checker run,
/// finalized in canonical source-key/signature order.
where_method_sources: WhereMethodSource.SafeList,
/// Complete producer-authored external-template lookup token stream. Tokens
/// retain raw import identity until import resolution supplies the module key.
external_lookup_tokens: ExternalLookupToken.SafeList,
/// Durable first-consumer authority for each resolved external cache key.
external_cache_seeds: ExternalCacheSeed.SafeList,
/// Canonical proof steps authenticating every imported or instantiated
/// where-method marker occurrence retained by this module.
where_marker_copy_steps: WhereMarkerCopyStep.SafeList,
/// Gapless complete source-to-destination relations owned by
/// `where_marker_copy_steps`.
where_marker_copy_pairs: WhereMarkerCopyPair.SafeList,
/// Immutable copy-time requests owned by `where_marker_copy_steps`; every row
/// names the canonical resolved pair for that exact raw occurrence.
where_marker_copy_occurrences: WhereMarkerCopyOccurrence.SafeList,
/// Gapless complete source-to-destination constraint relations owned by
/// `where_marker_copy_steps`.
where_marker_constraint_copy_pairs: WhereMarkerConstraintCopyPair.SafeList,
/// Gapless producer-authored edge/cut replay certificates owned by
/// `where_marker_copy_steps`.
where_marker_copy_witnesses: WhereMarkerCopyWitness.SafeList,
/// Receiver-level copy inventory owned gaplessly by
/// `where_marker_copy_steps` through each step's copied-group range.
copied_open_literal_groups: CopiedOpenLiteralGroup.SafeList,
/// Literal-conversion occurrences owned gaplessly by
/// `copied_open_literal_groups` through each group's event range.
copied_open_literal_events: CopiedOpenLiteralEvent.SafeList,
/// Exact source-to-destination marker-offset maps for ordinary constraint
/// merges. Every row owns one gapless slice in
/// `where_marker_constraint_move_offsets`.
where_marker_constraint_moves: WhereMarkerConstraintMove.SafeList,
where_marker_constraint_move_offsets: collections.SafeList(u32),
/// Exact platform-for-application substitution rows owned by platform copy
/// steps. This is separate from the source-var-sorted complete pair pool.
where_marker_platform_substitutions: WhereMarkerPlatformSubstitution.SafeList,
/// Exhaustive producer-authored expected-type consumption outcomes, keyed by
/// exact CIR owner, role, and slot.
expected_consumption_plans: ExpectedConsumptionPlan.SafeList,
/// Canonicalization-authored root and argument tokens for every call node.
expected_call_slot_tokens: ExpectedCallSlotToken.SafeList,
/// Immutable producer-time identities for successful call argument formals.
expected_call_formals: ExpectedCallFormal.SafeList,
/// Immutable direct Expected-producer failures referenced by plans and
/// retirement terminal authorities.
expected_failures: ExpectedFailure.SafeList,
/// Exact body-forced instantiation ambiguity verdicts which retire otherwise
/// settled branch Expected ranges.
expected_ambiguity_retirements: ExpectedAmbiguityRetirement.SafeList,
/// Immutable pre-rewrite snapshots for every final runtime-error node.
expected_consumer_retirements: ExpectedConsumerRetirement.SafeList,
/// Complete checker-authored failure cause sets owned by retirements.
expected_retirement_failures: ExpectedRetirementFailure.SafeList,
/// Exact expected-consumer sites owned by eligible checker retirements.
expected_retired_consumers: ExpectedRetiredConsumer.SafeList,
/// Exact typed namespace publications for canonicalization-authored malformed
/// expressions.
malformed_expression_publications: MalformedExpressionPublication.SafeList,
/// Exact typed namespace publications for canonicalization-authored malformed
/// type annotations.
malformed_type_annotation_publications: MalformedTypeAnnotationPublication.SafeList,
/// Exact canonicalization-owned annotation attachments for bodies which are
/// checked with that annotation.
body_annotation_attachments: BodyAnnotationAttachment.SafeList,
/// Exact canonicalization-owned joins from annotated bodies to their malformed
/// type-annotation leaves.
body_annotation_malformed_type_publications: BodyAnnotationMalformedTypePublication.SafeList,
/// Exact specialization-default selections authorizing `default_method`
/// copies. Rows are checker decisions, not derivations from finalized types.
default_decisions: DefaultDecision.SafeList,
/// Canonical, decision-owned producer tokens for `default_decisions`.
default_decision_contributors: DefaultDecisionContributor.SafeList,
/// Exhaustive completed imported selected-dispatch events whose copied method
/// schemes carry where-marker authority.
selected_method_decisions: SelectedMethodDecision.SafeList,
/// Finite selectable receiver origins referenced by type-store handle sets.
selected_receiver_anchors: SelectedReceiverAnchor.SafeList,
/// Immutable initial-source plans owned by terminal dispatch settlements.
dispatch_settlement_sources: DispatchSettlementSource.SafeList,
/// Exhaustive tagged-evidence movement across newly appended constraint
/// occurrences.
constraint_evidence_moves: ConstraintEvidenceMove.SafeList,
/// Gapless exact provenance-transition subgraphs owned by selected decisions.
selected_method_decision_moves: SelectedMethodDecisionMove.SafeList,
/// Settled positive tag rows for procedure method bindings.
method_output_publications: MethodOutputPublication.SafeList,
/// Canonical guarded paths backing `method_output_rows`. This pool is owned
/// and rebuilt with the derived publications; where-method use paths remain
/// durable exact witnesses in the checked module.
method_output_row_path_steps: WhereMethodMarkerPathStep.SafeList,
method_output_rows: MethodOutputRow.SafeList,
/// Exact selected occurrences authorized for result-row widening.
result_row_widening_uses: ResultRowWideningUse.SafeList,
/// Explicit callable-target sharing produced by generalized requirement
/// deduplication. Rows retain raw witnesses for checked-artifact construction.
generalized_dispatch_target_shares: GeneralizedDispatchTargetShare.SafeList,
/// Exact source bindings that checking generalized into rank-1 type schemes.
/// Sorted by source node for allocation-free cross-module lookup.
binding_schemes: BindingScheme.SafeList,
/// Generated-codec relations carried by those schemes. Sorted by source node;
/// multiple requirements for one binding occupy one contiguous run.
binding_scheme_codec_requirements: BindingSchemeCodecRequirement.SafeList,
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

/// One nested-safe destination-side boundary for a public cross-module type
/// copy. The type store owns its separate savepoint; this mark owns only the
/// ModuleEnv state that cross-module copying is allowed to append.
pub const CrossModuleCopyMark = struct {
    ident_interner: base.SmallStringInterner.Savepoint,
    module_identity_interner: base.SerialStringInterner.Savepoint,
    module_identity_displays_len: usize,
    where_marker_copy_steps_len: usize,
    where_marker_copy_pairs_len: usize,
    where_marker_copy_occurrences_len: usize,
    where_marker_constraint_copy_pairs_len: usize,
    where_marker_copy_witnesses_len: usize,
    copied_open_literal_groups_len: usize,
    copied_open_literal_events_len: usize,
    where_marker_platform_substitutions_len: usize,
    selected_receiver_anchors_len: usize,
    dispatch_settlement_sources_len: usize,
};

/// Open the ModuleEnv half of one cross-module-copy transaction. Opening both
/// interner savepoints is atomic: failure to snapshot the module-identity
/// interner closes the already-open identifier savepoint before returning.
pub fn beginCrossModuleCopyMark(self: *Self) std.mem.Allocator.Error!CrossModuleCopyMark {
    var ident_interner = try self.common.idents.interner.createSavepoint(self.gpa);
    errdefer self.common.idents.interner.rollbackToSavepoint(&ident_interner);
    const module_identity_interner = try self.module_identities.createSavepoint(self.gpa);

    return .{
        .ident_interner = ident_interner,
        .module_identity_interner = module_identity_interner,
        .module_identity_displays_len = self.module_identity_displays.items.items.len,
        .where_marker_copy_steps_len = self.where_marker_copy_steps.items.items.len,
        .where_marker_copy_pairs_len = self.where_marker_copy_pairs.items.items.len,
        .where_marker_copy_occurrences_len = self.where_marker_copy_occurrences.items.items.len,
        .where_marker_constraint_copy_pairs_len = self.where_marker_constraint_copy_pairs.items.items.len,
        .where_marker_copy_witnesses_len = self.where_marker_copy_witnesses.items.items.len,
        .copied_open_literal_groups_len = self.copied_open_literal_groups.items.items.len,
        .copied_open_literal_events_len = self.copied_open_literal_events.items.items.len,
        .where_marker_platform_substitutions_len = self.where_marker_platform_substitutions.items.items.len,
        .selected_receiver_anchors_len = self.selected_receiver_anchors.items.items.len,
        .dispatch_settlement_sources_len = self.dispatch_settlement_sources.items.items.len,
    };
}

/// Keep every append made since `mark` opened. The owned interner savepoints
/// enforce that marks close exactly once, on their owner, in strict LIFO order.
pub fn commitCrossModuleCopyMark(self: *Self, mark: *CrossModuleCopyMark) void {
    self.module_identities.commitSavepoint(&mark.module_identity_interner);
    self.common.idents.interner.commitSavepoint(&mark.ident_interner);
    mark.* = undefined;
}

/// Undo every ModuleEnv append made since `mark` opened. Interner rollback is
/// allocation-free and happens before list truncation so an invalid non-LIFO
/// close is rejected before any durable pool is changed.
pub fn rollbackCrossModuleCopyMark(self: *Self, mark: *CrossModuleCopyMark) void {
    self.module_identities.rollbackToSavepoint(&mark.module_identity_interner);
    self.common.idents.interner.rollbackToSavepoint(&mark.ident_interner);
    self.module_identity_displays.items.shrinkRetainingCapacity(mark.module_identity_displays_len);
    self.where_marker_copy_steps.items.shrinkRetainingCapacity(mark.where_marker_copy_steps_len);
    self.where_marker_copy_pairs.items.shrinkRetainingCapacity(mark.where_marker_copy_pairs_len);
    self.where_marker_copy_occurrences.items.shrinkRetainingCapacity(mark.where_marker_copy_occurrences_len);
    self.where_marker_constraint_copy_pairs.items.shrinkRetainingCapacity(mark.where_marker_constraint_copy_pairs_len);
    self.where_marker_copy_witnesses.items.shrinkRetainingCapacity(mark.where_marker_copy_witnesses_len);
    self.copied_open_literal_groups.items.shrinkRetainingCapacity(mark.copied_open_literal_groups_len);
    self.copied_open_literal_events.items.shrinkRetainingCapacity(mark.copied_open_literal_events_len);
    self.where_marker_platform_substitutions.items.shrinkRetainingCapacity(mark.where_marker_platform_substitutions_len);
    self.selected_receiver_anchors.items.shrinkRetainingCapacity(mark.selected_receiver_anchors_len);
    self.dispatch_settlement_sources.items.shrinkRetainingCapacity(mark.dispatch_settlement_sources_len);
    mark.* = undefined;
}

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
    self.numeral_digit_bytes.relocate(offset);
    self.numeral_literals.relocate(offset);
    self.numeric_suffix_targets.relocate(offset);
    self.scheme_uses.relocate(offset);
    self.scheme_use_pairs.relocate(offset);
    self.where_method_marker_uses.relocate(offset);
    self.where_method_marker_path_steps.relocate(offset);
    self.where_alias_expansions.relocate(offset);
    self.where_alias_declaration_publications.relocate(offset);
    self.where_method_sources.relocate(offset);
    self.external_lookup_tokens.relocate(offset);
    self.external_cache_seeds.relocate(offset);
    self.where_marker_copy_steps.relocate(offset);
    self.where_marker_copy_pairs.relocate(offset);
    self.where_marker_copy_occurrences.relocate(offset);
    self.where_marker_constraint_copy_pairs.relocate(offset);
    self.where_marker_copy_witnesses.relocate(offset);
    self.copied_open_literal_groups.relocate(offset);
    self.copied_open_literal_events.relocate(offset);
    self.where_marker_constraint_moves.relocate(offset);
    self.where_marker_constraint_move_offsets.relocate(offset);
    self.where_marker_platform_substitutions.relocate(offset);
    self.expected_consumption_plans.relocate(offset);
    self.expected_call_slot_tokens.relocate(offset);
    self.expected_call_formals.relocate(offset);
    self.expected_failures.relocate(offset);
    self.expected_ambiguity_retirements.relocate(offset);
    self.expected_consumer_retirements.relocate(offset);
    self.expected_retirement_failures.relocate(offset);
    self.expected_retired_consumers.relocate(offset);
    self.malformed_expression_publications.relocate(offset);
    self.malformed_type_annotation_publications.relocate(offset);
    self.body_annotation_attachments.relocate(offset);
    self.body_annotation_malformed_type_publications.relocate(offset);
    self.default_decisions.relocate(offset);
    self.default_decision_contributors.relocate(offset);
    self.selected_method_decisions.relocate(offset);
    self.selected_receiver_anchors.relocate(offset);
    self.dispatch_settlement_sources.relocate(offset);
    self.constraint_evidence_moves.relocate(offset);
    self.selected_method_decision_moves.relocate(offset);
    self.method_output_publications.relocate(offset);
    self.method_output_row_path_steps.relocate(offset);
    self.method_output_rows.relocate(offset);
    self.result_row_widening_uses.relocate(offset);
    self.generalized_dispatch_target_shares.relocate(offset);
    self.binding_schemes.relocate(offset);
    self.binding_scheme_codec_requirements.relocate(offset);
    self.generated_codec_derivations.relocate(offset);
    self.generated_codec_calls.relocate(offset);
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
    self.typecheck_state = .canonical_unchecked;
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
    self.w6b_semantically_validated = false;
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
        .typecheck_state = .canonical_unchecked,
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
        .w6b_semantically_validated = false,
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
        .where_method_marker_uses = try WhereMethodMarkerUse.SafeList.initCapacity(gpa, 8),
        .where_method_marker_path_steps = try WhereMethodMarkerPathStep.SafeList.initCapacity(gpa, 16),
        .where_alias_expansions = .{},
        .where_alias_declaration_publications = .{},
        .where_method_sources = .{},
        .external_lookup_tokens = .{},
        .external_cache_seeds = .{},
        .where_marker_copy_steps = .{},
        .where_marker_copy_pairs = .{},
        .where_marker_copy_occurrences = .{},
        .where_marker_constraint_copy_pairs = .{},
        .where_marker_copy_witnesses = .{},
        .copied_open_literal_groups = .{},
        .copied_open_literal_events = .{},
        .where_marker_constraint_moves = .{},
        .where_marker_constraint_move_offsets = .{},
        .where_marker_platform_substitutions = .{},
        .expected_consumption_plans = .{},
        .expected_call_slot_tokens = .{},
        .expected_call_formals = .{},
        .expected_failures = .{},
        .expected_ambiguity_retirements = .{},
        .expected_consumer_retirements = .{},
        .expected_retirement_failures = .{},
        .expected_retired_consumers = .{},
        .malformed_expression_publications = .{},
        .malformed_type_annotation_publications = .{},
        .body_annotation_attachments = .{},
        .body_annotation_malformed_type_publications = .{},
        .default_decisions = .{},
        .default_decision_contributors = .{},
        .selected_method_decisions = .{},
        .selected_receiver_anchors = .{},
        .dispatch_settlement_sources = .{},
        .constraint_evidence_moves = .{},
        .selected_method_decision_moves = .{},
        .method_output_publications = try MethodOutputPublication.SafeList.initCapacity(gpa, 8),
        .method_output_row_path_steps = try WhereMethodMarkerPathStep.SafeList.initCapacity(gpa, 16),
        .method_output_rows = try MethodOutputRow.SafeList.initCapacity(gpa, 16),
        .result_row_widening_uses = try ResultRowWideningUse.SafeList.initCapacity(gpa, 8),
        .generalized_dispatch_target_shares = try GeneralizedDispatchTargetShare.SafeList.initCapacity(gpa, 4),
        .binding_schemes = try BindingScheme.SafeList.initCapacity(gpa, 8),
        .binding_scheme_codec_requirements = try BindingSchemeCodecRequirement.SafeList.initCapacity(gpa, 4),
        .generated_codec_derivations = try GeneratedCodecDerivation.SafeList.initCapacity(gpa, 4),
        .generated_codec_calls = try GeneratedCodecCall.SafeList.initCapacity(gpa, 16),
        .rejected_static_dispatches = try RejectedStaticDispatch.SafeList.initCapacity(gpa, 4),
        .record_omitted_defaults = try RecordOmittedDefault.SafeList.initCapacity(gpa, 4),
    };
}

fn deinitMutableCopiedLists(self: *Self) void {
    self.provided_low_level_defs.deinit(self.gpa);
    self.for_loop_dispatch_plans.deinit(self.gpa);
    self.numeral_digit_bytes.deinit(self.gpa);
    self.numeral_literals.deinit(self.gpa);
    self.numeric_suffix_targets.deinit(self.gpa);
    self.scheme_uses.deinit(self.gpa);
    self.scheme_use_pairs.deinit(self.gpa);
    self.where_method_marker_uses.deinit(self.gpa);
    self.where_method_marker_path_steps.deinit(self.gpa);
    self.where_alias_expansions.deinit(self.gpa);
    self.where_alias_declaration_publications.deinit(self.gpa);
    self.where_method_sources.deinit(self.gpa);
    self.external_lookup_tokens.deinit(self.gpa);
    self.external_cache_seeds.deinit(self.gpa);
    self.where_marker_copy_steps.deinit(self.gpa);
    self.where_marker_copy_pairs.deinit(self.gpa);
    self.where_marker_copy_occurrences.deinit(self.gpa);
    self.where_marker_constraint_copy_pairs.deinit(self.gpa);
    self.where_marker_copy_witnesses.deinit(self.gpa);
    self.copied_open_literal_groups.deinit(self.gpa);
    self.copied_open_literal_events.deinit(self.gpa);
    self.where_marker_constraint_moves.deinit(self.gpa);
    self.where_marker_constraint_move_offsets.deinit(self.gpa);
    self.where_marker_platform_substitutions.deinit(self.gpa);
    self.expected_consumption_plans.deinit(self.gpa);
    self.expected_call_slot_tokens.deinit(self.gpa);
    self.expected_call_formals.deinit(self.gpa);
    self.expected_failures.deinit(self.gpa);
    self.expected_ambiguity_retirements.deinit(self.gpa);
    self.expected_consumer_retirements.deinit(self.gpa);
    self.expected_retirement_failures.deinit(self.gpa);
    self.expected_retired_consumers.deinit(self.gpa);
    self.malformed_expression_publications.deinit(self.gpa);
    self.malformed_type_annotation_publications.deinit(self.gpa);
    self.body_annotation_attachments.deinit(self.gpa);
    self.body_annotation_malformed_type_publications.deinit(self.gpa);
    self.default_decisions.deinit(self.gpa);
    self.default_decision_contributors.deinit(self.gpa);
    self.selected_method_decisions.deinit(self.gpa);
    self.selected_receiver_anchors.deinit(self.gpa);
    self.dispatch_settlement_sources.deinit(self.gpa);
    self.constraint_evidence_moves.deinit(self.gpa);
    self.selected_method_decision_moves.deinit(self.gpa);
    self.method_output_publications.deinit(self.gpa);
    self.method_output_row_path_steps.deinit(self.gpa);
    self.method_output_rows.deinit(self.gpa);
    self.result_row_widening_uses.deinit(self.gpa);
    self.generalized_dispatch_target_shares.deinit(self.gpa);
    self.binding_schemes.deinit(self.gpa);
    self.binding_scheme_codec_requirements.deinit(self.gpa);
    self.generated_codec_derivations.deinit(self.gpa);
    self.generated_codec_calls.deinit(self.gpa);
    self.rejected_static_dispatches.deinit(self.gpa);
    self.record_omitted_defaults.deinit(self.gpa);
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
    self.deinitMutableCopiedLists();
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
    self.deinitMutableCopiedLists();

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
    if (comptime RetIdx == CIR.Expr.Idx) {
        try self.malformed_expression_publications.items.ensureUnusedCapacity(self.gpa, 1);
    }
    if (comptime RetIdx == CIR.TypeAnno.Idx) {
        try self.malformed_type_annotation_publications.items.ensureUnusedCapacity(self.gpa, 1);
    }
    const diag_idx = try self.addDiagnostic(reason);
    const region = getDiagnosticRegion(reason);
    const malformed_idx = try self.addMalformed(diag_idx, region);
    if (comptime RetIdx == CIR.Expr.Idx) {
        _ = self.malformed_expression_publications.appendAssumeCapacity(.{
            .expr_node = @intFromEnum(malformed_idx),
            .diagnostic_index = @intFromEnum(diag_idx),
        });
    }
    if (comptime RetIdx == CIR.TypeAnno.Idx) {
        _ = self.malformed_type_annotation_publications.appendAssumeCapacity(.{
            .annotation_node = @intFromEnum(malformed_idx),
            .diagnostic_index = @intFromEnum(diag_idx),
        });
    }
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
    if (comptime RetIdx == CIR.Expr.Idx) {
        try self.malformed_expression_publications.items.ensureUnusedCapacity(self.gpa, 1);
    }
    const diag_idx = try self.store.addDiagnosticUnregistered(reason);
    const region = getDiagnosticRegion(reason);
    const malformed_idx = try self.addMalformed(diag_idx, region);
    if (comptime RetIdx == CIR.Expr.Idx) {
        _ = self.malformed_expression_publications.appendAssumeCapacity(.{
            .expr_node = @intFromEnum(malformed_idx),
            .diagnostic_index = @intFromEnum(diag_idx),
        });
    }
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
        .record_default_reference_cycle => |data| blk: {
            const field_name = self.getIdent(data.field_name);
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Default Value Cycle", "", .runtime_error);
            const owned_field_name = try report.addOwnedString(field_name);
            try report.headline.addReflowingText("The default value for the ");
            try report.headline.addRecordField(owned_field_name);
            try report.headline.addReflowingText(" field depends on itself.");

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
            try report.document.addReflowingText(") is materialized at every construction site that omits the field. This default reaches itself again—through values it references, or through constructions that omit the field and would materialize it—so there is no value to start from. Break the cycle by supplying the field at one of the constructions involved, or by removing the self-dependent reference from the default.");

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
        .default_not_allowed_in_structural_record => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Default Not Allowed In Structural Record", "", .runtime_error);
            try report.headline.addReflowingText("Field defaults (");
            try report.headline.addInlineCode("??");
            try report.headline.addReflowingText(") are only allowed on the fields of a nominal record type declaration's backing record, not in structural record types (type aliases, inline annotations, or nested records).");

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
            try report.document.addReflowingText(" A default belongs to one named type, so declare a nominal type (with ");
            try report.document.addInlineCode(":=");
            try report.document.addReflowingText(") whose backing record carries the default, and use that type here.");

            break :blk report;
        },
        .default_not_allowed_on_local_type_decl => |data| blk: {
            const region_info = self.calcRegionInfo(data.region);

            var report = try Report.init(allocator, "Default Not Allowed On Local Type Declaration", "", .runtime_error);
            try report.headline.addReflowingText("Field defaults (");
            try report.headline.addInlineCode("??");
            try report.headline.addReflowingText(") are only allowed on nominal type declarations at the top level of a module, not on type declarations inside a function or block.");

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
            try report.document.addReflowingText(" A default is materialized at every construction site that omits the field, so it cannot depend on the locals of one function. Move the type declaration to the module top level, or remove the default.");

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
    typecheck_state: u8,
    typecheck_state_padding: [3]u8,
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
    where_method_marker_uses: WhereMethodMarkerUse.SafeList.Serialized,
    where_method_marker_path_steps: WhereMethodMarkerPathStep.SafeList.Serialized,
    where_alias_expansions: WhereAliasExpansion.SafeList.Serialized,
    where_alias_declaration_publications: WhereAliasDeclarationPublication.SafeList.Serialized,
    where_method_sources: WhereMethodSource.SafeList.Serialized,
    external_lookup_tokens: ExternalLookupToken.SafeList.Serialized,
    external_cache_seeds: ExternalCacheSeed.SafeList.Serialized,
    where_marker_copy_steps: WhereMarkerCopyStep.SafeList.Serialized,
    where_marker_copy_pairs: WhereMarkerCopyPair.SafeList.Serialized,
    where_marker_copy_occurrences: WhereMarkerCopyOccurrence.SafeList.Serialized,
    where_marker_constraint_copy_pairs: WhereMarkerConstraintCopyPair.SafeList.Serialized,
    where_marker_copy_witnesses: WhereMarkerCopyWitness.SafeList.Serialized,
    copied_open_literal_groups: CopiedOpenLiteralGroup.SafeList.Serialized,
    copied_open_literal_events: CopiedOpenLiteralEvent.SafeList.Serialized,
    where_marker_constraint_moves: WhereMarkerConstraintMove.SafeList.Serialized,
    where_marker_constraint_move_offsets: collections.SafeList(u32).Serialized,
    where_marker_platform_substitutions: WhereMarkerPlatformSubstitution.SafeList.Serialized,
    expected_consumption_plans: ExpectedConsumptionPlan.SafeList.Serialized,
    expected_call_slot_tokens: ExpectedCallSlotToken.SafeList.Serialized,
    expected_call_formals: ExpectedCallFormal.SafeList.Serialized,
    expected_failures: ExpectedFailure.SafeList.Serialized,
    expected_ambiguity_retirements: ExpectedAmbiguityRetirement.SafeList.Serialized,
    expected_consumer_retirements: ExpectedConsumerRetirement.SafeList.Serialized,
    expected_retirement_failures: ExpectedRetirementFailure.SafeList.Serialized,
    expected_retired_consumers: ExpectedRetiredConsumer.SafeList.Serialized,
    malformed_expression_publications: MalformedExpressionPublication.SafeList.Serialized,
    malformed_type_annotation_publications: MalformedTypeAnnotationPublication.SafeList.Serialized,
    body_annotation_attachments: BodyAnnotationAttachment.SafeList.Serialized,
    body_annotation_malformed_type_publications: BodyAnnotationMalformedTypePublication.SafeList.Serialized,
    default_decisions: DefaultDecision.SafeList.Serialized,
    default_decision_contributors: DefaultDecisionContributor.SafeList.Serialized,
    selected_method_decisions: SelectedMethodDecision.SafeList.Serialized,
    selected_receiver_anchors: SelectedReceiverAnchor.SafeList.Serialized,
    dispatch_settlement_sources: DispatchSettlementSource.SafeList.Serialized,
    constraint_evidence_moves: ConstraintEvidenceMove.SafeList.Serialized,
    selected_method_decision_moves: SelectedMethodDecisionMove.SafeList.Serialized,
    method_output_publications: MethodOutputPublication.SafeList.Serialized,
    method_output_row_path_steps: WhereMethodMarkerPathStep.SafeList.Serialized,
    method_output_rows: MethodOutputRow.SafeList.Serialized,
    result_row_widening_uses: ResultRowWideningUse.SafeList.Serialized,
    generalized_dispatch_target_shares: GeneralizedDispatchTargetShare.SafeList.Serialized,
    binding_schemes: BindingScheme.SafeList.Serialized,
    binding_scheme_codec_requirements: BindingSchemeCodecRequirement.SafeList.Serialized,
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
            "typecheck_state_padding", // Fixed-width padding for the serialized lifecycle tag.
            "runtime_prepared_padding", // Fixed-width padding for the serialized bool.
            "_reserved_flags", // Format-reserved bytes for fields removed from ModuleEnv.
            "_padding", // Tail padding kept explicit and zeroed for deterministic bytes.
        };
        collections.serde_validation.assertBidirectionalFieldSet(
            Self,
            Serialized,
            &.{"w6b_semantically_validated"},
            &serialized_only_fields,
            &renamed_fields,
        );
        collections.serde_validation.assertSerializedRelocatable(Serialized);
    }

    pub fn validate(self: *const Serialized, backing_len: usize) error{CorruptArtifact}!void {
        if (backing_len < @sizeOf(Serialized)) return error.CorruptArtifact;
        if (std.enums.fromInt(TypecheckState, self.typecheck_state) == null or
            self.typecheck_state_padding[0] != 0 or
            self.typecheck_state_padding[1] != 0 or
            self.typecheck_state_padding[2] != 0)
        {
            return error.CorruptArtifact;
        }
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
        self.typecheck_state = @intFromEnum(env.typecheck_state);
        self.typecheck_state_padding = .{ 0, 0, 0 };
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
        try self.where_method_marker_uses.serialize(&env.where_method_marker_uses, allocator, writer);
        try self.where_method_marker_path_steps.serialize(&env.where_method_marker_path_steps, allocator, writer);
        try self.where_alias_expansions.serialize(&env.where_alias_expansions, allocator, writer);
        try self.where_alias_declaration_publications.serialize(&env.where_alias_declaration_publications, allocator, writer);
        try self.where_method_sources.serialize(&env.where_method_sources, allocator, writer);
        try self.external_lookup_tokens.serialize(&env.external_lookup_tokens, allocator, writer);
        try self.external_cache_seeds.serialize(&env.external_cache_seeds, allocator, writer);
        try self.where_marker_copy_steps.serialize(&env.where_marker_copy_steps, allocator, writer);
        try self.where_marker_copy_pairs.serialize(&env.where_marker_copy_pairs, allocator, writer);
        try self.where_marker_copy_occurrences.serialize(&env.where_marker_copy_occurrences, allocator, writer);
        try self.where_marker_constraint_copy_pairs.serialize(&env.where_marker_constraint_copy_pairs, allocator, writer);
        try self.where_marker_copy_witnesses.serialize(&env.where_marker_copy_witnesses, allocator, writer);
        try self.copied_open_literal_groups.serialize(&env.copied_open_literal_groups, allocator, writer);
        try self.copied_open_literal_events.serialize(&env.copied_open_literal_events, allocator, writer);
        try self.where_marker_constraint_moves.serialize(&env.where_marker_constraint_moves, allocator, writer);
        try self.where_marker_constraint_move_offsets.serialize(&env.where_marker_constraint_move_offsets, allocator, writer);
        try self.where_marker_platform_substitutions.serialize(&env.where_marker_platform_substitutions, allocator, writer);
        try self.expected_consumption_plans.serialize(&env.expected_consumption_plans, allocator, writer);
        try self.expected_call_slot_tokens.serialize(&env.expected_call_slot_tokens, allocator, writer);
        try self.expected_call_formals.serialize(&env.expected_call_formals, allocator, writer);
        try self.expected_failures.serialize(&env.expected_failures, allocator, writer);
        try self.expected_ambiguity_retirements.serialize(&env.expected_ambiguity_retirements, allocator, writer);
        try self.expected_consumer_retirements.serialize(&env.expected_consumer_retirements, allocator, writer);
        try self.expected_retirement_failures.serialize(&env.expected_retirement_failures, allocator, writer);
        try self.expected_retired_consumers.serialize(&env.expected_retired_consumers, allocator, writer);
        try self.malformed_expression_publications.serialize(&env.malformed_expression_publications, allocator, writer);
        try self.malformed_type_annotation_publications.serialize(&env.malformed_type_annotation_publications, allocator, writer);
        try self.body_annotation_attachments.serialize(&env.body_annotation_attachments, allocator, writer);
        try self.body_annotation_malformed_type_publications.serialize(&env.body_annotation_malformed_type_publications, allocator, writer);
        try self.default_decisions.serialize(&env.default_decisions, allocator, writer);
        try self.default_decision_contributors.serialize(&env.default_decision_contributors, allocator, writer);
        try self.selected_method_decisions.serialize(&env.selected_method_decisions, allocator, writer);
        try self.selected_receiver_anchors.serialize(&env.selected_receiver_anchors, allocator, writer);
        try self.dispatch_settlement_sources.serialize(&env.dispatch_settlement_sources, allocator, writer);
        try self.constraint_evidence_moves.serialize(&env.constraint_evidence_moves, allocator, writer);
        try self.selected_method_decision_moves.serialize(&env.selected_method_decision_moves, allocator, writer);
        try self.method_output_publications.serialize(&env.method_output_publications, allocator, writer);
        try self.method_output_row_path_steps.serialize(&env.method_output_row_path_steps, allocator, writer);
        try self.method_output_rows.serialize(&env.method_output_rows, allocator, writer);
        try self.result_row_widening_uses.serialize(&env.result_row_widening_uses, allocator, writer);
        try self.generalized_dispatch_target_shares.serialize(&env.generalized_dispatch_target_shares, allocator, writer);
        try self.binding_schemes.serialize(&env.binding_schemes, allocator, writer);
        try self.binding_scheme_codec_requirements.serialize(&env.binding_scheme_codec_requirements, allocator, writer);
        try self.generated_codec_derivations.serialize(&env.generated_codec_derivations, allocator, writer);
        try self.generated_codec_calls.serialize(&env.generated_codec_calls, allocator, writer);
        try self.rejected_static_dispatches.serialize(&env.rejected_static_dispatches, allocator, writer);
        try self.record_omitted_defaults.serialize(&env.record_omitted_defaults, allocator, writer);

        self._reserved_flags = .{ 0, 0 };
    }

    /// Build one non-owning ModuleEnv view over serialized storage. Import-map
    /// ownership is selected explicitly by the caller; every serialized field
    /// remains named here so schema additions fail compilation until handled.
    fn materialize(
        self: *const Serialized,
        imports: CIR.Import.Store,
        base_addr: usize,
        gpa: std.mem.Allocator,
        source: []const u8,
        module_name: []const u8,
    ) Self {
        return Self{
            .gpa = gpa,
            .common = self.common.deserializeInto(base_addr, source),
            .types = self.types.deserializeInto(base_addr, gpa),
            .module_kind = self.module_kind.decode(),
            .module_role = self.module_role,
            .typecheck_state = @enumFromInt(self.typecheck_state),
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
            .imports = imports,
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
            .w6b_semantically_validated = false,
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
            .where_method_marker_uses = self.where_method_marker_uses.deserializeInto(base_addr),
            .where_method_marker_path_steps = self.where_method_marker_path_steps.deserializeInto(base_addr),
            .where_alias_expansions = self.where_alias_expansions.deserializeInto(base_addr),
            .where_alias_declaration_publications = self.where_alias_declaration_publications.deserializeInto(base_addr),
            .where_method_sources = self.where_method_sources.deserializeInto(base_addr),
            .external_lookup_tokens = self.external_lookup_tokens.deserializeInto(base_addr),
            .external_cache_seeds = self.external_cache_seeds.deserializeInto(base_addr),
            .where_marker_copy_steps = self.where_marker_copy_steps.deserializeInto(base_addr),
            .where_marker_copy_pairs = self.where_marker_copy_pairs.deserializeInto(base_addr),
            .where_marker_copy_occurrences = self.where_marker_copy_occurrences.deserializeInto(base_addr),
            .where_marker_constraint_copy_pairs = self.where_marker_constraint_copy_pairs.deserializeInto(base_addr),
            .where_marker_copy_witnesses = self.where_marker_copy_witnesses.deserializeInto(base_addr),
            .copied_open_literal_groups = self.copied_open_literal_groups.deserializeInto(base_addr),
            .copied_open_literal_events = self.copied_open_literal_events.deserializeInto(base_addr),
            .where_marker_constraint_moves = self.where_marker_constraint_moves.deserializeInto(base_addr),
            .where_marker_constraint_move_offsets = self.where_marker_constraint_move_offsets.deserializeInto(base_addr),
            .where_marker_platform_substitutions = self.where_marker_platform_substitutions.deserializeInto(base_addr),
            .expected_consumption_plans = self.expected_consumption_plans.deserializeInto(base_addr),
            .expected_call_slot_tokens = self.expected_call_slot_tokens.deserializeInto(base_addr),
            .expected_call_formals = self.expected_call_formals.deserializeInto(base_addr),
            .expected_failures = self.expected_failures.deserializeInto(base_addr),
            .expected_ambiguity_retirements = self.expected_ambiguity_retirements.deserializeInto(base_addr),
            .expected_consumer_retirements = self.expected_consumer_retirements.deserializeInto(base_addr),
            .expected_retirement_failures = self.expected_retirement_failures.deserializeInto(base_addr),
            .expected_retired_consumers = self.expected_retired_consumers.deserializeInto(base_addr),
            .malformed_expression_publications = self.malformed_expression_publications.deserializeInto(base_addr),
            .malformed_type_annotation_publications = self.malformed_type_annotation_publications.deserializeInto(base_addr),
            .body_annotation_attachments = self.body_annotation_attachments.deserializeInto(base_addr),
            .body_annotation_malformed_type_publications = self.body_annotation_malformed_type_publications.deserializeInto(base_addr),
            .default_decisions = self.default_decisions.deserializeInto(base_addr),
            .default_decision_contributors = self.default_decision_contributors.deserializeInto(base_addr),
            .selected_method_decisions = self.selected_method_decisions.deserializeInto(base_addr),
            .selected_receiver_anchors = self.selected_receiver_anchors.deserializeInto(base_addr),
            .dispatch_settlement_sources = self.dispatch_settlement_sources.deserializeInto(base_addr),
            .constraint_evidence_moves = self.constraint_evidence_moves.deserializeInto(base_addr),
            .selected_method_decision_moves = self.selected_method_decision_moves.deserializeInto(base_addr),
            .method_output_publications = self.method_output_publications.deserializeInto(base_addr),
            .method_output_row_path_steps = self.method_output_row_path_steps.deserializeInto(base_addr),
            .method_output_rows = self.method_output_rows.deserializeInto(base_addr),
            .result_row_widening_uses = self.result_row_widening_uses.deserializeInto(base_addr),
            .generalized_dispatch_target_shares = self.generalized_dispatch_target_shares.deserializeInto(base_addr),
            .binding_schemes = self.binding_schemes.deserializeInto(base_addr),
            .binding_scheme_codec_requirements = self.binding_scheme_codec_requirements.deserializeInto(base_addr),
            .generated_codec_derivations = self.generated_codec_derivations.deserializeInto(base_addr),
            .generated_codec_calls = self.generated_codec_calls.deserializeInto(base_addr),
            .rejected_static_dispatches = self.rejected_static_dispatches.deserializeInto(base_addr),
            .record_omitted_defaults = self.record_omitted_defaults.deserializeInto(base_addr),
        };
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
        const env = try gpa.create(Self);
        errdefer gpa.destroy(env);

        const imports = try self.imports.deserializeInto(base_addr, gpa);
        env.* = self.materialize(imports, base_addr, gpa, source, module_name);
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

        return self.materialize(
            CIR.Import.Store.init(),
            base_addr,
            gpa,
            source,
            module_name,
        );
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
        // Begin with the non-owning view. It owns only the reconstructed import
        // map and its empty runtime import-mapping table; every mutable
        // component below is staged separately and installed only after all
        // allocations succeed.
        const env = try self.deserializeInto(base_addr, gpa, source, module_name);
        errdefer {
            env.imports.deinitMapOnly(gpa);
            env.import_mapping.deinit();
            gpa.destroy(env);
        }

        var mutable_types = try self.types.deserializeWithCopy(base_addr, gpa);
        errdefer mutable_types.deinit();
        var module_identity_displays = try self.module_identity_displays.deserializeWithCopy(base_addr, gpa);
        errdefer module_identity_displays.deinit(gpa);
        var mutable_store = try self.store.deserializeWithCopy(base_addr, gpa);
        errdefer mutable_store.regions.deinit(gpa);
        var provided_low_level_defs = try self.provided_low_level_defs.deserializeWithCopy(base_addr, gpa);
        errdefer provided_low_level_defs.deinit(gpa);
        var for_loop_dispatch_plans = try self.for_loop_dispatch_plans.deserializeWithCopy(base_addr, gpa);
        errdefer for_loop_dispatch_plans.deinit(gpa);
        var numeral_digit_bytes = try self.numeral_digit_bytes.deserializeWithCopy(base_addr, gpa);
        errdefer numeral_digit_bytes.deinit(gpa);
        var numeral_literals = try self.numeral_literals.deserializeWithCopy(base_addr, gpa);
        errdefer numeral_literals.deinit(gpa);
        var numeric_suffix_targets = try self.numeric_suffix_targets.deserializeWithCopy(base_addr, gpa);
        errdefer numeric_suffix_targets.deinit(gpa);
        var scheme_uses = try self.scheme_uses.deserializeWithCopy(base_addr, gpa);
        errdefer scheme_uses.deinit(gpa);
        var scheme_use_pairs = try self.scheme_use_pairs.deserializeWithCopy(base_addr, gpa);
        errdefer scheme_use_pairs.deinit(gpa);
        var where_method_marker_uses = try self.where_method_marker_uses.deserializeWithCopy(base_addr, gpa);
        errdefer where_method_marker_uses.deinit(gpa);
        var where_method_marker_path_steps = try self.where_method_marker_path_steps.deserializeWithCopy(base_addr, gpa);
        errdefer where_method_marker_path_steps.deinit(gpa);
        var where_alias_expansions = try self.where_alias_expansions.deserializeWithCopy(base_addr, gpa);
        errdefer where_alias_expansions.deinit(gpa);
        var where_alias_declaration_publications = try self.where_alias_declaration_publications.deserializeWithCopy(base_addr, gpa);
        errdefer where_alias_declaration_publications.deinit(gpa);
        var where_method_sources = try self.where_method_sources.deserializeWithCopy(base_addr, gpa);
        errdefer where_method_sources.deinit(gpa);
        var external_lookup_tokens = try self.external_lookup_tokens.deserializeWithCopy(base_addr, gpa);
        errdefer external_lookup_tokens.deinit(gpa);
        var external_cache_seeds = try self.external_cache_seeds.deserializeWithCopy(base_addr, gpa);
        errdefer external_cache_seeds.deinit(gpa);
        var where_marker_copy_steps = try self.where_marker_copy_steps.deserializeWithCopy(base_addr, gpa);
        errdefer where_marker_copy_steps.deinit(gpa);
        var where_marker_copy_pairs = try self.where_marker_copy_pairs.deserializeWithCopy(base_addr, gpa);
        errdefer where_marker_copy_pairs.deinit(gpa);
        var where_marker_copy_occurrences = try self.where_marker_copy_occurrences.deserializeWithCopy(base_addr, gpa);
        errdefer where_marker_copy_occurrences.deinit(gpa);
        var where_marker_constraint_copy_pairs = try self.where_marker_constraint_copy_pairs.deserializeWithCopy(base_addr, gpa);
        errdefer where_marker_constraint_copy_pairs.deinit(gpa);
        var where_marker_copy_witnesses = try self.where_marker_copy_witnesses.deserializeWithCopy(base_addr, gpa);
        errdefer where_marker_copy_witnesses.deinit(gpa);
        var copied_open_literal_groups = try self.copied_open_literal_groups.deserializeWithCopy(base_addr, gpa);
        errdefer copied_open_literal_groups.deinit(gpa);
        var copied_open_literal_events = try self.copied_open_literal_events.deserializeWithCopy(base_addr, gpa);
        errdefer copied_open_literal_events.deinit(gpa);
        var where_marker_constraint_moves = try self.where_marker_constraint_moves.deserializeWithCopy(base_addr, gpa);
        errdefer where_marker_constraint_moves.deinit(gpa);
        var where_marker_constraint_move_offsets = try self.where_marker_constraint_move_offsets.deserializeWithCopy(base_addr, gpa);
        errdefer where_marker_constraint_move_offsets.deinit(gpa);
        var where_marker_platform_substitutions = try self.where_marker_platform_substitutions.deserializeWithCopy(base_addr, gpa);
        errdefer where_marker_platform_substitutions.deinit(gpa);
        var expected_consumption_plans = try self.expected_consumption_plans.deserializeWithCopy(base_addr, gpa);
        errdefer expected_consumption_plans.deinit(gpa);
        var expected_call_slot_tokens = try self.expected_call_slot_tokens.deserializeWithCopy(base_addr, gpa);
        errdefer expected_call_slot_tokens.deinit(gpa);
        var expected_call_formals = try self.expected_call_formals.deserializeWithCopy(base_addr, gpa);
        errdefer expected_call_formals.deinit(gpa);
        var expected_failures = try self.expected_failures.deserializeWithCopy(base_addr, gpa);
        errdefer expected_failures.deinit(gpa);
        var expected_ambiguity_retirements = try self.expected_ambiguity_retirements.deserializeWithCopy(base_addr, gpa);
        errdefer expected_ambiguity_retirements.deinit(gpa);
        var expected_consumer_retirements = try self.expected_consumer_retirements.deserializeWithCopy(base_addr, gpa);
        errdefer expected_consumer_retirements.deinit(gpa);
        var expected_retirement_failures = try self.expected_retirement_failures.deserializeWithCopy(base_addr, gpa);
        errdefer expected_retirement_failures.deinit(gpa);
        var expected_retired_consumers = try self.expected_retired_consumers.deserializeWithCopy(base_addr, gpa);
        errdefer expected_retired_consumers.deinit(gpa);
        var malformed_expression_publications = try self.malformed_expression_publications.deserializeWithCopy(base_addr, gpa);
        errdefer malformed_expression_publications.deinit(gpa);
        var malformed_type_annotation_publications = try self.malformed_type_annotation_publications.deserializeWithCopy(base_addr, gpa);
        errdefer malformed_type_annotation_publications.deinit(gpa);
        var body_annotation_attachments = try self.body_annotation_attachments.deserializeWithCopy(base_addr, gpa);
        errdefer body_annotation_attachments.deinit(gpa);
        var body_annotation_malformed_type_publications = try self.body_annotation_malformed_type_publications.deserializeWithCopy(base_addr, gpa);
        errdefer body_annotation_malformed_type_publications.deinit(gpa);
        var default_decisions = try self.default_decisions.deserializeWithCopy(base_addr, gpa);
        errdefer default_decisions.deinit(gpa);
        var default_decision_contributors = try self.default_decision_contributors.deserializeWithCopy(base_addr, gpa);
        errdefer default_decision_contributors.deinit(gpa);
        var selected_method_decisions = try self.selected_method_decisions.deserializeWithCopy(base_addr, gpa);
        errdefer selected_method_decisions.deinit(gpa);
        var selected_receiver_anchors = try self.selected_receiver_anchors.deserializeWithCopy(base_addr, gpa);
        errdefer selected_receiver_anchors.deinit(gpa);
        var dispatch_settlement_sources = try self.dispatch_settlement_sources.deserializeWithCopy(base_addr, gpa);
        errdefer dispatch_settlement_sources.deinit(gpa);
        var constraint_evidence_moves = try self.constraint_evidence_moves.deserializeWithCopy(base_addr, gpa);
        errdefer constraint_evidence_moves.deinit(gpa);
        var selected_method_decision_moves = try self.selected_method_decision_moves.deserializeWithCopy(base_addr, gpa);
        errdefer selected_method_decision_moves.deinit(gpa);
        var method_output_publications = try self.method_output_publications.deserializeWithCopy(base_addr, gpa);
        errdefer method_output_publications.deinit(gpa);
        var method_output_row_path_steps = try self.method_output_row_path_steps.deserializeWithCopy(base_addr, gpa);
        errdefer method_output_row_path_steps.deinit(gpa);
        var method_output_rows = try self.method_output_rows.deserializeWithCopy(base_addr, gpa);
        errdefer method_output_rows.deinit(gpa);
        var result_row_widening_uses = try self.result_row_widening_uses.deserializeWithCopy(base_addr, gpa);
        errdefer result_row_widening_uses.deinit(gpa);
        var generalized_dispatch_target_shares = try self.generalized_dispatch_target_shares.deserializeWithCopy(base_addr, gpa);
        errdefer generalized_dispatch_target_shares.deinit(gpa);
        var binding_schemes = try self.binding_schemes.deserializeWithCopy(base_addr, gpa);
        errdefer binding_schemes.deinit(gpa);
        var binding_scheme_codec_requirements = try self.binding_scheme_codec_requirements.deserializeWithCopy(base_addr, gpa);
        errdefer binding_scheme_codec_requirements.deinit(gpa);
        var generated_codec_derivations = try self.generated_codec_derivations.deserializeWithCopy(base_addr, gpa);
        errdefer generated_codec_derivations.deinit(gpa);
        var generated_codec_calls = try self.generated_codec_calls.deserializeWithCopy(base_addr, gpa);
        errdefer generated_codec_calls.deinit(gpa);
        var rejected_static_dispatches = try self.rejected_static_dispatches.deserializeWithCopy(base_addr, gpa);
        errdefer rejected_static_dispatches.deinit(gpa);
        var record_omitted_defaults = try self.record_omitted_defaults.deserializeWithCopy(base_addr, gpa);
        errdefer record_omitted_defaults.deinit(gpa);

        env.types = mutable_types;
        env.module_identity_displays = module_identity_displays;
        env.store = mutable_store;
        env.provided_low_level_defs = provided_low_level_defs;
        env.for_loop_dispatch_plans = for_loop_dispatch_plans;
        env.numeral_digit_bytes = numeral_digit_bytes;
        env.numeral_literals = numeral_literals;
        env.numeric_suffix_targets = numeric_suffix_targets;
        env.scheme_uses = scheme_uses;
        env.scheme_use_pairs = scheme_use_pairs;
        env.where_method_marker_uses = where_method_marker_uses;
        env.where_method_marker_path_steps = where_method_marker_path_steps;
        env.where_alias_expansions = where_alias_expansions;
        env.where_alias_declaration_publications = where_alias_declaration_publications;
        env.where_method_sources = where_method_sources;
        env.external_lookup_tokens = external_lookup_tokens;
        env.external_cache_seeds = external_cache_seeds;
        env.where_marker_copy_steps = where_marker_copy_steps;
        env.where_marker_copy_pairs = where_marker_copy_pairs;
        env.where_marker_copy_occurrences = where_marker_copy_occurrences;
        env.where_marker_constraint_copy_pairs = where_marker_constraint_copy_pairs;
        env.where_marker_copy_witnesses = where_marker_copy_witnesses;
        env.copied_open_literal_groups = copied_open_literal_groups;
        env.copied_open_literal_events = copied_open_literal_events;
        env.where_marker_constraint_moves = where_marker_constraint_moves;
        env.where_marker_constraint_move_offsets = where_marker_constraint_move_offsets;
        env.where_marker_platform_substitutions = where_marker_platform_substitutions;
        env.expected_consumption_plans = expected_consumption_plans;
        env.expected_call_slot_tokens = expected_call_slot_tokens;
        env.expected_call_formals = expected_call_formals;
        env.expected_failures = expected_failures;
        env.expected_ambiguity_retirements = expected_ambiguity_retirements;
        env.expected_consumer_retirements = expected_consumer_retirements;
        env.expected_retirement_failures = expected_retirement_failures;
        env.expected_retired_consumers = expected_retired_consumers;
        env.malformed_expression_publications = malformed_expression_publications;
        env.malformed_type_annotation_publications = malformed_type_annotation_publications;
        env.body_annotation_attachments = body_annotation_attachments;
        env.body_annotation_malformed_type_publications = body_annotation_malformed_type_publications;
        env.default_decisions = default_decisions;
        env.default_decision_contributors = default_decision_contributors;
        env.selected_method_decisions = selected_method_decisions;
        env.selected_receiver_anchors = selected_receiver_anchors;
        env.dispatch_settlement_sources = dispatch_settlement_sources;
        env.constraint_evidence_moves = constraint_evidence_moves;
        env.selected_method_decision_moves = selected_method_decision_moves;
        env.method_output_publications = method_output_publications;
        env.method_output_row_path_steps = method_output_row_path_steps;
        env.method_output_rows = method_output_rows;
        env.result_row_widening_uses = result_row_widening_uses;
        env.generalized_dispatch_target_shares = generalized_dispatch_target_shares;
        env.binding_schemes = binding_schemes;
        env.binding_scheme_codec_requirements = binding_scheme_codec_requirements;
        env.generated_codec_derivations = generated_codec_derivations;
        env.generated_codec_calls = generated_codec_calls;
        env.rejected_static_dispatches = rejected_static_dispatches;
        env.record_omitted_defaults = record_omitted_defaults;
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
    iter_outcome: ForLoopDispatchOutcome,
    next_outcome: ForLoopDispatchOutcome,
) std.mem.Allocator.Error!u32 {
    const raw_node: u32 = @intFromEnum(node_idx);
    const raw_pattern: u32 = @intFromEnum(pattern_idx);
    const raw_iterable: u32 = @intFromEnum(iterable_idx);
    if (!iter_outcome.hasCanonicalTags() or !next_outcome.hasCanonicalTags()) {
        std.debug.panic("for-loop dispatch plan published an unresolved slot outcome", .{});
    }
    for (self.for_loop_dispatch_plans.items.items) |plan| {
        if (plan.node_idx == raw_node) {
            std.debug.panic("for-loop dispatch plan was published more than once", .{});
        }
    }
    const plan_index: u32 = @intCast(self.for_loop_dispatch_plans.items.items.len);
    _ = try self.for_loop_dispatch_plans.append(self.gpa, .{
        .node_idx = raw_node,
        .pattern_idx = raw_pattern,
        .iterable_idx = raw_iterable,
        .iterator_var = @intFromEnum(iterator_var),
        .step_var = @intFromEnum(step_var),
        .iter_fn_var = @intFromEnum(iter_fn_var),
        .next_fn_var = @intFromEnum(next_fn_var),
        .step_topology = step_topology,
        .iter_outcome = iter_outcome,
        .next_outcome = next_outcome,
    });
    return plan_index;
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

/// First index whose `node_idx` is greater than `raw_node` in a node-sorted
/// table. This is the end of the contiguous run returned for multi-entry
/// source-node metadata.
fn sortedNodeEndSlot(comptime T: type, entries: []const T, raw_node: u32) usize {
    var low: usize = 0;
    var high: usize = entries.len;
    while (low < high) {
        const mid = low + (high - low) / 2;
        if (entries[mid].node_idx <= raw_node) {
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

/// Record one exact generated-codec relation owned by a source binding scheme.
/// Duplicate aliases are harmless but duplicate relations for the same alias
/// would create redundant imported work, so exact entries are coalesced here.
pub fn recordBindingSchemeCodecRequirement(
    self: *Self,
    node_idx: Node.Idx,
    scheme_root: TypeVar,
    receiver_var: TypeVar,
    constraint_index: u32,
) std.mem.Allocator.Error!void {
    const entry = BindingSchemeCodecRequirement{
        .node_idx = @intFromEnum(node_idx),
        .scheme_root = @intFromEnum(scheme_root),
        .receiver_var = @intFromEnum(receiver_var),
        .constraint_index = constraint_index,
    };
    const entries = self.binding_scheme_codec_requirements.items.items;
    const start = sortedNodeSlot(BindingSchemeCodecRequirement, entries, entry.node_idx);
    const end = sortedNodeEndSlot(BindingSchemeCodecRequirement, entries, entry.node_idx);
    for (entries[start..end]) |existing| {
        if (existing.scheme_root == entry.scheme_root and
            existing.receiver_var == entry.receiver_var and
            existing.constraint_index == entry.constraint_index)
        {
            return;
        }
    }

    if (end == entries.len) {
        _ = try self.binding_scheme_codec_requirements.append(self.gpa, entry);
        return;
    }
    _ = try self.binding_scheme_codec_requirements.append(self.gpa, entry);
    const grown = self.binding_scheme_codec_requirements.items.items;
    std.mem.copyBackwards(
        BindingSchemeCodecRequirement,
        grown[end + 1 ..],
        grown[end .. grown.len - 1],
    );
    grown[end] = entry;
}

/// Return all generated-codec relations belonging to `node_idx`. The borrowed
/// slice is allocation-free and remains valid until the table is mutated.
pub fn bindingSchemeCodecRequirementsForNode(
    self: *const Self,
    node_idx: Node.Idx,
) []const BindingSchemeCodecRequirement {
    const entries = self.binding_scheme_codec_requirements.items.items;
    const raw_node = @intFromEnum(node_idx);
    const start = sortedNodeSlot(BindingSchemeCodecRequirement, entries, raw_node);
    const end = sortedNodeEndSlot(BindingSchemeCodecRequirement, entries, raw_node);
    return entries[start..end];
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
/// `dispatch_target` slots, the body constraint callable for
/// `where_method_use`, and 0 for value and nested-function use slots.
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
        .marker_uses_start = @intCast(self.where_method_marker_uses.items.items.len),
        .marker_uses_len = 0,
    });
}

/// Atomically record one per-body-use where-method signature copy. Every
/// allocation is reserved before any durable list grows, so an allocation
/// failure cannot leave a scheme-use row whose marker or pair range was only
/// partly published.
pub fn recordWhereMethodUse(
    self: *Self,
    node_idx: u32,
    constraint_fn_var: TypeVar,
    scheme_root: TypeVar,
    pairs: []const SchemeUsePair,
    markers: []const WhereMethodMarkerUse,
    path_steps: []const WhereMethodMarkerPathStep,
) std.mem.Allocator.Error!u32 {
    try self.scheme_use_pairs.items.ensureUnusedCapacity(self.gpa, pairs.len);
    try self.scheme_uses.items.ensureUnusedCapacity(self.gpa, 1);
    try self.where_method_marker_path_steps.items.ensureUnusedCapacity(self.gpa, path_steps.len);
    try self.where_method_marker_uses.items.ensureUnusedCapacity(self.gpa, markers.len);

    const pairs_start: u32 = @intCast(self.scheme_use_pairs.items.items.len);
    for (pairs) |pair| {
        _ = self.scheme_use_pairs.appendAssumeCapacity(pair);
    }
    const path_base: u32 = @intCast(self.where_method_marker_path_steps.items.items.len);
    for (path_steps) |step| {
        if (step.kindOrNull() == null) {
            std.debug.panic("where-method marker path had an invalid step kind", .{});
        }
        self.where_method_marker_path_steps.items.appendAssumeCapacity(.{
            .kind = step.kind,
            .index = step.index,
            .arity = step.arity,
            .name = step.name,
            .origin_module = step.origin_module,
            .source_decl = step.source_decl,
        });
    }

    const marker_base: u32 = @intCast(self.where_method_marker_uses.items.items.len);
    for (markers) |marker| {
        if (marker.path_start > path_steps.len or marker.path_len > path_steps.len - marker.path_start) {
            std.debug.panic("opened where-method marker path was out of bounds", .{});
        }
        var durable = marker;
        durable.path_start = path_base + marker.path_start;
        self.where_method_marker_uses.items.appendAssumeCapacity(durable);
    }

    const record_index: u32 = @intCast(self.scheme_uses.items.items.len);
    _ = self.scheme_uses.appendAssumeCapacity(.{
        .node_idx = node_idx,
        .slot_kind = @intFromEnum(SchemeUseRecord.Slot.where_method_use),
        .slot_data = @intFromEnum(constraint_fn_var),
        .scheme_root = @intFromEnum(scheme_root),
        .pairs_start = pairs_start,
        .pairs_len = @intCast(pairs.len),
        .marker_uses_start = marker_base,
        .marker_uses_len = @intCast(markers.len),
    });
    return record_index;
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
    if (self.numericSuffixTargetForNode(node_idx) != null) {
        std.debug.panic("numeric suffix target was published more than once", .{});
    }
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

    try self.numeric_suffix_targets.items.ensureUnusedCapacity(self.gpa, 1);
    if (target == .external) {
        try self.external_lookup_tokens.items.ensureUnusedCapacity(self.gpa, 1);
    }
    try upsertSortedByNode(NumericSuffixTarget, &self.numeric_suffix_targets, self.gpa, suffix_target);
    switch (target) {
        .external => |external| {
            self.appendExternalLookupTokenAssumeCapacity(.{
                .import_idx = external.import_idx,
                .target_node = external.target_node_idx,
                .site_kind = .external_numeric_suffix,
            }, raw_node, ExternalLookupToken.none);
        },
        .builtin, .local, .invalid => {},
    }
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

const BodyAnnotationStatementAttachment = struct {
    kind: BodyAnnotationAttachment.AttachmentKind,
    annotation: CIR.Annotation.Idx,
    body_expr: CIR.Expr.Idx,
};

fn bodyAnnotationStatementAttachment(statement: CIR.Statement) ?BodyAnnotationStatementAttachment {
    return switch (statement) {
        .s_decl => |decl| if (decl.anno) |annotation| .{
            .kind = .local_decl,
            .annotation = annotation,
            .body_expr = decl.expr,
        } else null,
        .s_var => |var_| if (var_.anno) |annotation| .{
            .kind = .local_var,
            .annotation = annotation,
            .body_expr = var_.expr,
        } else null,
        else => null,
    };
}

fn moduleEnvRawRangeFits(start: u32, len: u32, total: usize) bool {
    const raw_start: usize = start;
    const raw_len: usize = len;
    return raw_start <= total and raw_len <= total - raw_start;
}

fn isTypeAnnoSyntaxTag(tag: Node.Tag) bool {
    return switch (tag) {
        .ty_apply,
        .ty_rigid_var,
        .ty_rigid_var_lookup,
        .ty_underscore,
        .ty_lookup,
        .ty_tag_union,
        .ty_tag,
        .ty_tuple,
        .ty_record,
        .ty_fn,
        .ty_parens,
        .ty_malformed,
        .malformed,
        => true,
        else => false,
    };
}

fn countTypeAnnoStructuralChild(
    self: *const Self,
    parent_raw: u32,
    child_raw: u32,
    wanted_raw: u32,
    matches: *usize,
) bool {
    if (child_raw >= parent_raw or
        child_raw >= self.store.nodes.len() or
        !isTypeAnnoSyntaxTag(self.store.nodes.get(@enumFromInt(child_raw)).tag))
    {
        return false;
    }
    if (child_raw == wanted_raw) matches.* += 1;
    return true;
}

/// Validate one candidate TypeAnno parent without using a typed getter, and
/// count its direct structural edges to `wanted_raw`. Non-TypeAnno nodes are
/// skipped. A null result means a typed node or side-table span was malformed.
fn typeAnnoStructuralChildOccurrences(
    self: *const Self,
    parent_raw: u32,
    wanted_raw: u32,
) ?usize {
    if (parent_raw >= self.store.nodes.len()) return null;
    const node = self.store.nodes.get(@enumFromInt(parent_raw));
    const payload = node.getPayload();
    const index_data = self.store.index_data.items.items;
    var matches: usize = 0;

    switch (node.tag) {
        .ty_apply => {
            const apply = payload.ty_apply;
            if (apply.type_apply_data_idx >= self.store.type_apply_data.items.items.len) return null;
            const apply_data = self.store.type_apply_data.items.items[apply.type_apply_data_idx];
            if (std.enums.fromInt(CIR.TypeAnno.LocalOrExternal.Tag, apply_data.base_tag) == null or
                !moduleEnvRawRangeFits(apply.args_start, apply_data.args_len, index_data.len))
            {
                return null;
            }
            for (index_data[apply.args_start..][0..apply_data.args_len]) |child_raw| {
                if (!self.countTypeAnnoStructuralChild(parent_raw, child_raw, wanted_raw, &matches)) return null;
            }
        },
        .ty_rigid_var => if (!std.mem.allEqual(u8, &payload.ty_rigid_var._padding, 0)) return null,
        .ty_rigid_var_lookup => {
            const lookup = payload.ty_rigid_var_lookup;
            if (!std.mem.allEqual(u8, &lookup._padding, 0) or
                lookup.ref >= parent_raw or
                lookup.ref >= self.store.nodes.len() or
                self.store.nodes.get(@enumFromInt(lookup.ref)).tag != .ty_rigid_var)
            {
                return null;
            }
        },
        .ty_underscore => {},
        .ty_lookup => {
            const lookup = payload.ty_lookup;
            if (lookup.base_span2_idx >= self.store.span2_data.items.items.len or
                std.enums.fromInt(CIR.TypeAnno.LocalOrExternal.Tag, lookup.base) == null)
            {
                return null;
            }
        },
        .ty_tag_union => {
            const tag_union = payload.ty_tag_union;
            if (!moduleEnvRawRangeFits(tag_union.tags_start, tag_union.tags_len, index_data.len)) return null;
            for (index_data[tag_union.tags_start..][0..tag_union.tags_len]) |child_raw| {
                if (!self.countTypeAnnoStructuralChild(parent_raw, child_raw, wanted_raw, &matches)) return null;
            }
            if (tag_union.ext_plus_one != 0 and
                !self.countTypeAnnoStructuralChild(
                    parent_raw,
                    tag_union.ext_plus_one - 1,
                    wanted_raw,
                    &matches,
                ))
            {
                return null;
            }
        },
        .ty_tag => {
            const tag = payload.ty_tag;
            if (!moduleEnvRawRangeFits(tag.args_start, tag.args_len, index_data.len)) return null;
            for (index_data[tag.args_start..][0..tag.args_len]) |child_raw| {
                if (!self.countTypeAnnoStructuralChild(parent_raw, child_raw, wanted_raw, &matches)) return null;
            }
        },
        .ty_tuple => {
            const tuple = payload.ty_tuple;
            if (!std.mem.allEqual(u8, &tuple._padding, 0) or
                !moduleEnvRawRangeFits(tuple.elems_start, tuple.elems_len, index_data.len))
            {
                return null;
            }
            for (index_data[tuple.elems_start..][0..tuple.elems_len]) |child_raw| {
                if (!self.countTypeAnnoStructuralChild(parent_raw, child_raw, wanted_raw, &matches)) return null;
            }
        },
        .ty_record => {
            const record = payload.ty_record;
            if (!moduleEnvRawRangeFits(record.fields_start, record.fields_len, index_data.len)) return null;
            for (index_data[record.fields_start..][0..record.fields_len]) |field_raw| {
                if (field_raw >= parent_raw or field_raw >= self.store.nodes.len()) return null;
                const field_node = self.store.nodes.get(@enumFromInt(field_raw));
                const child_raw = switch (field_node.tag) {
                    .ty_record_field => blk: {
                        const field = field_node.getPayload().ty_record_field;
                        if (!std.mem.allEqual(u8, &field._padding, 0)) return null;
                        break :blk field.ty;
                    },
                    .ty_record_field_defaulted => field_node.getPayload().ty_record_field_defaulted.ty,
                    else => return null,
                };
                if (child_raw >= field_raw or
                    !self.countTypeAnnoStructuralChild(parent_raw, child_raw, wanted_raw, &matches))
                {
                    return null;
                }
            }
            if (record.ext_plus_one != 0 and
                !self.countTypeAnnoStructuralChild(
                    parent_raw,
                    record.ext_plus_one - 1,
                    wanted_raw,
                    &matches,
                ))
            {
                return null;
            }
        },
        .ty_fn => {
            const function = payload.ty_fn;
            if (function.fn_info_span2_idx >= self.store.span2_data.items.items.len or
                !moduleEnvRawRangeFits(function.args_start, function.args_len, index_data.len))
            {
                return null;
            }
            for (index_data[function.args_start..][0..function.args_len]) |child_raw| {
                if (!self.countTypeAnnoStructuralChild(parent_raw, child_raw, wanted_raw, &matches)) return null;
            }
            const fn_info = self.store.span2_data.items.items[function.fn_info_span2_idx];
            if (fn_info.start > 1 or
                !self.countTypeAnnoStructuralChild(parent_raw, fn_info.len, wanted_raw, &matches))
            {
                return null;
            }
        },
        .ty_parens => {
            const parens = payload.ty_parens;
            if (!std.mem.allEqual(u8, &parens._padding, 0) or
                !self.countTypeAnnoStructuralChild(parent_raw, parens.anno, wanted_raw, &matches))
            {
                return null;
            }
        },
        .ty_malformed => if (!std.mem.allEqual(u8, &payload.ty_malformed._padding, 0)) return null,
        .malformed => {},
        .ty_record_field, .ty_record_field_defaulted => {},
        .ty_apply_external, .ty_lookup_external => return null,
        else => {},
    }
    return matches;
}

/// Canon emits every structural child before its unique syntax parent. A
/// monotonic scan therefore walks the only possible parent chain without
/// recursion, allocation, or a heuristic depth bound. On corrupt DAGs the
/// walk may reject conservatively, but every accepted hop is an exact bounded
/// structural edge.
fn typeAnnoTreeContains(self: *const Self, root: CIR.TypeAnno.Idx, target: CIR.TypeAnno.Idx) bool {
    const root_raw = @intFromEnum(root);
    const target_raw = @intFromEnum(target);
    if (root_raw >= self.store.nodes.len() or
        target_raw >= self.store.nodes.len() or
        !isTypeAnnoSyntaxTag(self.store.nodes.get(@enumFromInt(root_raw)).tag) or
        !isTypeAnnoSyntaxTag(self.store.nodes.get(@enumFromInt(target_raw)).tag) or
        self.typeAnnoStructuralChildOccurrences(root_raw, std.math.maxInt(u32)) == null)
    {
        return false;
    }
    if (root_raw == target_raw) return true;
    if (target_raw > root_raw) return false;

    var current = target_raw;
    var candidate_raw = target_raw + 1;
    while (candidate_raw <= root_raw) : (candidate_raw += 1) {
        const occurrences = self.typeAnnoStructuralChildOccurrences(
            candidate_raw,
            current,
        ) orelse return false;
        if (occurrences > 1) return false;
        if (occurrences == 1) current = candidate_raw;
    }
    return current == root_raw;
}

const CheckedAnnotationWhereStorage = struct {
    written_start: u32,
    written_len: u32,
    owners_start: u32,
    owners_len: u32,
};

const CheckedAnnotationStorage = struct {
    payload: Node.Payload.Annotation,
    where: ?CheckedAnnotationWhereStorage,
};

/// Decode only after validating every auxiliary index used by Annotation.where.
fn checkedAnnotationStorage(self: *const Self, annotation_idx: CIR.Annotation.Idx) ?CheckedAnnotationStorage {
    const annotation_raw = @intFromEnum(annotation_idx);
    if (annotation_raw >= self.store.nodes.len()) return null;
    const annotation_node = self.store.nodes.get(@enumFromInt(annotation_raw));
    if (annotation_node.tag != .annotation) return null;
    const payload = annotation_node.getPayload().annotation;
    if (payload.flags.unused != 0 or
        payload.anno >= annotation_raw or
        payload.anno >= self.store.nodes.len() or
        !isTypeAnnoSyntaxTag(self.store.nodes.get(@enumFromInt(payload.anno)).tag))
    {
        return null;
    }
    if (payload.flags.has_name_region) {
        if (payload.name_region_span2_idx >= self.store.span2_data.items.items.len) return null;
    } else if (payload.name_region_span2_idx != 0) return null;
    if (!payload.flags.has_where) {
        if (payload.where_span2_idx != 0) return null;
        return .{ .payload = payload, .where = null };
    }
    if (payload.where_span2_idx >= self.store.span_with_node_data.items.items.len) return null;
    const where = self.store.span_with_node_data.items.items[payload.where_span2_idx];
    if (!moduleEnvRawRangeFits(where.start, where.len, self.store.index_data.items.items.len) or
        where.node >= self.store.span2_data.items.items.len)
    {
        return null;
    }
    const owners = self.store.span2_data.items.items[where.node];
    if (!moduleEnvRawRangeFits(
        owners.start,
        owners.len,
        self.store.where_clause_owners.items.items.len,
    )) {
        return null;
    }
    return .{
        .payload = payload,
        .where = .{
            .written_start = where.start,
            .written_len = where.len,
            .owners_start = owners.start,
            .owners_len = owners.len,
        },
    };
}

fn isWhereClauseTag(tag: Node.Tag) bool {
    return switch (tag) {
        .where_method, .where_method_effectful, .where_alias, .where_malformed => true,
        else => false,
    };
}

fn whereClauseListIsStrictlyIncreasing(clauses: []const u32) bool {
    for (clauses, 0..) |where_raw, offset| {
        if (offset != 0 and clauses[offset - 1] >= where_raw) return false;
    }
    return true;
}

fn whereClauseListIsWrittenSubsequence(
    written: []const u32,
    owned: []const u32,
) bool {
    if (!whereClauseListIsStrictlyIncreasing(owned)) return false;
    for (owned) |owned_raw| {
        var low: usize = 0;
        var high = written.len;
        while (low < high) {
            const mid = low + (high - low) / 2;
            if (written[mid] < owned_raw) {
                low = mid + 1;
            } else {
                high = mid;
            }
        }
        if (low >= written.len or written[low] != owned_raw) return false;
    }
    return true;
}

/// Decode the canonical receiver owner for one clause which appears in a
/// `WhereClauseOwner` row. The receiver occurrence is validated without
/// following a rigid-lookup identity reference as a structural child.
fn listedWhereClauseReceiverOwner(self: *const Self, where_raw: u32) ?u32 {
    if (where_raw >= self.store.nodes.len()) return null;
    const where_node = self.store.nodes.get(@enumFromInt(where_raw));
    const receiver_raw = switch (where_node.tag) {
        .where_method, .where_method_effectful => blk: {
            const method = where_node.getPayload().where_clause;
            if (method.effectful != @intFromBool(where_node.tag == .where_method_effectful)) {
                return null;
            }
            break :blk method.var_idx;
        },
        .where_alias => blk: {
            const alias = where_node.getPayload().where_alias;
            if (!std.mem.allEqual(u8, &alias._padding, 0)) return null;
            break :blk alias.var_idx;
        },
        else => return null,
    };
    if (receiver_raw >= where_raw or receiver_raw >= self.store.nodes.len()) return null;
    const receiver_idx: CIR.TypeAnno.Idx = @enumFromInt(receiver_raw);
    if (!self.typeAnnoTreeContains(receiver_idx, receiver_idx)) return null;
    const receiver_node = self.store.nodes.get(@enumFromInt(receiver_raw));
    return switch (receiver_node.tag) {
        .ty_rigid_var => receiver_raw,
        .ty_rigid_var_lookup => receiver_node.getPayload().ty_rigid_var_lookup.ref,
        else => null,
    };
}

fn annotationOwnsTypeAnnoNode(self: *const Self, annotation_idx: CIR.Annotation.Idx, target: CIR.TypeAnno.Idx) bool {
    const annotation = self.checkedAnnotationStorage(annotation_idx) orelse return false;
    var owns_target = self.typeAnnoTreeContains(@enumFromInt(annotation.payload.anno), target);
    const where = annotation.where orelse return owns_target;
    const index_data = self.store.index_data.items.items;
    const written_clauses = index_data[where.written_start..][0..where.written_len];
    if (!whereClauseListIsStrictlyIncreasing(written_clauses)) return false;
    for (written_clauses) |where_raw| {
        if (where_raw >= @intFromEnum(annotation_idx) or
            where_raw >= self.store.nodes.len() or
            !isWhereClauseTag(self.store.nodes.get(@enumFromInt(where_raw)).tag))
        {
            return false;
        }
    }

    const owners = self.store.where_clause_owners.items.items[where.owners_start..][0..where.owners_len];
    for (owners) |owner| {
        if (!std.mem.allEqual(u8, &owner._padding, 0) or
            owner.rigid_var >= @intFromEnum(annotation_idx) or
            owner.rigid_var >= self.store.nodes.len() or
            self.store.nodes.get(@enumFromInt(owner.rigid_var)).tag != .ty_rigid_var or
            !moduleEnvRawRangeFits(owner.clauses_start, owner.clauses_len, index_data.len))
        {
            return false;
        }
        const owned_clauses = index_data[owner.clauses_start..][0..owner.clauses_len];
        if (!whereClauseListIsWrittenSubsequence(written_clauses, owned_clauses)) return false;
        for (owned_clauses) |where_raw| {
            if (where_raw >= self.store.nodes.len()) return false;
            const where_node = self.store.nodes.get(@enumFromInt(where_raw));
            if (!isWhereClauseTag(where_node.tag)) return false;
            const receiver_owner = self.listedWhereClauseReceiverOwner(where_raw) orelse return false;
            if (receiver_owner != owner.rigid_var) return false;
            if (!owner.owned_by_annotation) continue;

            switch (where_node.tag) {
                .where_method, .where_method_effectful => {
                    const method = where_node.getPayload().where_clause;
                    if (method.effectful != @intFromBool(where_node.tag == .where_method_effectful) or
                        method.args_ret_idx >= self.store.span_with_node_data.items.items.len)
                    {
                        return false;
                    }
                    const args_ret = self.store.span_with_node_data.items.items[method.args_ret_idx];
                    if (!moduleEnvRawRangeFits(args_ret.start, args_ret.len, index_data.len)) return false;
                    if (method.var_idx >= where_raw or args_ret.node >= where_raw) return false;
                    const receiver_matches = self.typeAnnoTreeContains(
                        @enumFromInt(method.var_idx),
                        target,
                    );
                    const result_matches = self.typeAnnoTreeContains(
                        @enumFromInt(args_ret.node),
                        target,
                    );
                    owns_target = owns_target or receiver_matches or result_matches;
                    for (index_data[args_ret.start..][0..args_ret.len]) |arg_raw| {
                        if (arg_raw >= where_raw) return false;
                        const argument_matches = self.typeAnnoTreeContains(
                            @enumFromInt(arg_raw),
                            target,
                        );
                        owns_target = owns_target or argument_matches;
                    }
                },
                .where_alias => {
                    const alias = where_node.getPayload().where_alias;
                    if (!std.mem.allEqual(u8, &alias._padding, 0) or
                        alias.var_idx >= where_raw or alias.alias_idx >= where_raw)
                    {
                        return false;
                    }
                    const receiver_matches = self.typeAnnoTreeContains(
                        @enumFromInt(alias.var_idx),
                        target,
                    );
                    const alias_matches = self.typeAnnoTreeContains(
                        @enumFromInt(alias.alias_idx),
                        target,
                    );
                    owns_target = owns_target or receiver_matches or alias_matches;
                },
                .where_malformed => {
                    const malformed = where_node.getPayload().where_malformed;
                    if (!std.mem.allEqual(u8, &malformed._padding, 0)) return false;
                },
                else => unreachable,
            }
        }
    }
    return owns_target;
}

fn checkedMalformedWhereAliasTarget(
    self: *const Self,
    where_raw: u32,
) ?CIR.TypeAnno.Idx {
    if (where_raw >= self.store.nodes.len()) return null;
    const where_node = self.store.nodes.get(@enumFromInt(where_raw));
    if (where_node.tag != .where_alias) return null;
    const alias = where_node.getPayload().where_alias;
    if (!std.mem.allEqual(u8, &alias._padding, 0) or
        alias.var_idx >= where_raw or alias.alias_idx >= where_raw or
        alias.var_idx >= self.store.nodes.len() or alias.alias_idx >= self.store.nodes.len() or
        !isTypeAnnoSyntaxTag(self.store.nodes.get(@enumFromInt(alias.var_idx)).tag) or
        self.store.nodes.get(@enumFromInt(alias.alias_idx)).tag != .malformed or
        !self.typeAnnoTreeContains(@enumFromInt(alias.var_idx), @enumFromInt(alias.var_idx)) or
        !self.typeAnnoTreeContains(@enumFromInt(alias.alias_idx), @enumFromInt(alias.alias_idx)))
    {
        return null;
    }
    return @enumFromInt(alias.alias_idx);
}

/// Select the first malformed where-alias target in the exact written clause
/// order after validating the annotation and its auxiliary where topology.
pub fn firstMalformedWhereAliasTarget(
    self: *const Self,
    annotation_idx: CIR.Annotation.Idx,
) ?CIR.TypeAnno.Idx {
    const annotation = self.checkedAnnotationStorage(annotation_idx) orelse return null;
    const where = annotation.where orelse return null;
    if (!self.annotationOwnsTypeAnnoNode(
        annotation_idx,
        @enumFromInt(annotation.payload.anno),
    )) return null;

    const written = self.store.index_data.items.items[where.written_start..][0..where.written_len];
    if (!whereClauseListIsStrictlyIncreasing(written)) return null;
    var selected: ?CIR.TypeAnno.Idx = null;
    for (written) |where_raw| {
        const node = self.store.nodes.get(@enumFromInt(where_raw));
        switch (node.tag) {
            .where_alias => {
                const alias = node.getPayload().where_alias;
                if (!std.mem.allEqual(u8, &alias._padding, 0) or
                    alias.var_idx >= where_raw or alias.alias_idx >= where_raw or
                    alias.var_idx >= self.store.nodes.len() or alias.alias_idx >= self.store.nodes.len() or
                    !isTypeAnnoSyntaxTag(self.store.nodes.get(@enumFromInt(alias.var_idx)).tag) or
                    !isTypeAnnoSyntaxTag(self.store.nodes.get(@enumFromInt(alias.alias_idx)).tag))
                {
                    return null;
                }
                if (selected == null and self.store.nodes.get(@enumFromInt(alias.alias_idx)).tag == .malformed) {
                    selected = self.checkedMalformedWhereAliasTarget(where_raw) orelse return null;
                }
            },
            .where_method, .where_method_effectful, .where_malformed => {},
            else => return null,
        }
    }
    return selected;
}

/// Select the source-ordered first malformed alias owned by one exact
/// canonical where receiver. The `owned_by_annotation` bit is Can's explicit
/// transitive ownership publication; this routine does not reconstruct it.
pub fn firstMalformedWhereAliasTargetForOwnedRigid(
    self: *const Self,
    annotation_idx: CIR.Annotation.Idx,
    rigid_var: u32,
) ?CIR.TypeAnno.Idx {
    const annotation = self.checkedAnnotationStorage(annotation_idx) orelse return null;
    const where = annotation.where orelse return null;
    if (!self.annotationOwnsTypeAnnoNode(
        annotation_idx,
        @enumFromInt(annotation.payload.anno),
    )) return null;

    const index_data = self.store.index_data.items.items;
    const written = index_data[where.written_start..][0..where.written_len];
    if (!whereClauseListIsStrictlyIncreasing(written)) return null;
    const owners = self.store.where_clause_owners.items.items[where.owners_start..][0..where.owners_len];
    var found_owner = false;
    var selected: ?CIR.TypeAnno.Idx = null;
    for (owners) |owner| {
        if (!owner.owned_by_annotation or owner.rigid_var != rigid_var) continue;
        if (found_owner) return null;
        found_owner = true;
        const owned_clauses = index_data[owner.clauses_start..][0..owner.clauses_len];
        if (!whereClauseListIsWrittenSubsequence(written, owned_clauses)) return null;
        for (owned_clauses) |where_raw| {
            const node = self.store.nodes.get(@enumFromInt(where_raw));
            switch (node.tag) {
                .where_alias => {
                    const alias = node.getPayload().where_alias;
                    if (!std.mem.allEqual(u8, &alias._padding, 0) or
                        alias.var_idx >= where_raw or alias.alias_idx >= where_raw or
                        alias.var_idx >= self.store.nodes.len() or alias.alias_idx >= self.store.nodes.len() or
                        !isTypeAnnoSyntaxTag(self.store.nodes.get(@enumFromInt(alias.var_idx)).tag) or
                        !isTypeAnnoSyntaxTag(self.store.nodes.get(@enumFromInt(alias.alias_idx)).tag))
                    {
                        return null;
                    }
                    if (selected == null and self.store.nodes.get(@enumFromInt(alias.alias_idx)).tag == .malformed) {
                        selected = self.checkedMalformedWhereAliasTarget(where_raw) orelse return null;
                    }
                },
                .where_method, .where_method_effectful, .where_malformed => {},
                else => return null,
            }
        }
    }
    return if (found_owner) selected else null;
}

fn bodyAnnotationAttachmentRowIsLocallyValid(
    self: *const Self,
    attachment: BodyAnnotationAttachment,
) bool {
    if (!attachment.hasLegalTags() or
        attachment.attachment_node >= self.store.nodes.len() or
        attachment.annotation_root >= self.store.nodes.len() or
        attachment.body_expr >= self.store.nodes.len())
    {
        return false;
    }
    if (self.store.nodes.get(@enumFromInt(attachment.annotation_root)).tag != .annotation) {
        return false;
    }

    const annotation_idx: CIR.Annotation.Idx = @enumFromInt(attachment.annotation_root);
    _ = self.checkedAnnotationStorage(annotation_idx) orelse return false;
    const attachment_node = self.store.nodes.get(@enumFromInt(attachment.attachment_node));
    return switch (attachment.decodedAttachmentKind() orelse return false) {
        .top_level_def => blk: {
            if (attachment_node.tag != .def) break :blk false;
            const def_data_index = attachment_node.getPayload().def.def_data_idx;
            if (def_data_index >= self.store.def_data.items.items.len) break :blk false;
            const def = self.store.def_data.items.items[def_data_index];
            break :blk def.anno_idx == @intFromEnum(annotation_idx) and
                def.expr == attachment.body_expr;
        },
        .local_decl => blk: {
            if (attachment_node.tag != .statement_decl) break :blk false;
            const statement = attachment_node.getPayload().statement_decl;
            if (statement.anno_span2_idx >= self.store.span2_data.items.items.len) break :blk false;
            const annotation_data = self.store.span2_data.items.items[statement.anno_span2_idx];
            break :blk annotation_data.start == 1 and
                annotation_data.len == @intFromEnum(annotation_idx) and
                statement.expr == attachment.body_expr;
        },
        .local_var => blk: {
            if (attachment_node.tag != .statement_var) break :blk false;
            const statement = attachment_node.getPayload().statement_var;
            if (statement.anno_span2_idx >= self.store.span2_data.items.items.len) break :blk false;
            const annotation_data = self.store.span2_data.items.items[statement.anno_span2_idx];
            break :blk annotation_data.start == 1 and
                annotation_data.len == @intFromEnum(annotation_idx) and
                statement.expr == attachment.body_expr;
        },
    };
}

/// Whether the indexed canonicalization publication still names the exact
/// Def-or-statement annotation attachment and body in the current CIR.
pub fn bodyAnnotationAttachmentIsLocallyValid(self: *const Self, attachment_index: u32) bool {
    const attachments = self.body_annotation_attachments.items.items;
    if (attachment_index >= attachments.len) return false;
    return self.bodyAnnotationAttachmentRowIsLocallyValid(attachments[attachment_index]);
}

/// Resolve an exact body-annotation attachment tuple to its unique canonical
/// publication index. The table's strict key order makes this allocation-free.
pub fn bodyAnnotationAttachmentIndex(
    self: *const Self,
    attachment_kind: BodyAnnotationAttachment.AttachmentKind,
    attachment_node: u32,
    annotation_idx: CIR.Annotation.Idx,
    body_expr: CIR.Expr.Idx,
) ?u32 {
    const wanted = BodyAnnotationAttachment{
        .attachment_kind = @intFromEnum(attachment_kind),
        .attachment_node = attachment_node,
        .annotation_root = @intFromEnum(annotation_idx),
        .body_expr = @intFromEnum(body_expr),
    };
    const attachments = self.body_annotation_attachments.items.items;
    var low: usize = 0;
    var high: usize = attachments.len;
    while (low < high) {
        const mid = low + (high - low) / 2;
        if (bodyAnnotationAttachmentLessThan(attachments[mid], wanted)) {
            low = mid + 1;
        } else {
            high = mid;
        }
    }
    if (low >= attachments.len or !std.meta.eql(attachments[low], wanted)) return null;
    return @intCast(low);
}

/// Whether `target` is an exact structural TypeAnno child of the indexed
/// attachment's annotation. Identity-only rigid lookups are leaves.
pub fn bodyAnnotationAttachmentContainsTypeAnno(
    self: *const Self,
    attachment_index: u32,
    target: CIR.TypeAnno.Idx,
) bool {
    if (!self.bodyAnnotationAttachmentIsLocallyValid(attachment_index)) return false;
    const attachment = self.body_annotation_attachments.items.items[attachment_index];
    return self.annotationOwnsTypeAnnoNode(@enumFromInt(attachment.annotation_root), target);
}

/// Whether `target` is an exact member of the indexed attachment's written
/// where-clause span. This includes malformed and unowned-receiver clauses.
pub fn bodyAnnotationAttachmentContainsWhereClause(
    self: *const Self,
    attachment_index: u32,
    target: CIR.WhereClause.Idx,
) bool {
    if (!self.bodyAnnotationAttachmentIsLocallyValid(attachment_index) or
        @intFromEnum(target) >= self.store.nodes.len()) return false;

    const target_tag = self.store.nodes.get(@enumFromInt(@intFromEnum(target))).tag;
    switch (target_tag) {
        .where_method, .where_method_effectful, .where_alias, .where_malformed => {},
        else => return false,
    }

    const attachment = self.body_annotation_attachments.items.items[attachment_index];
    const annotation = self.checkedAnnotationStorage(
        @enumFromInt(attachment.annotation_root),
    ) orelse return false;
    const where = annotation.where orelse return false;
    const written = self.store.index_data.items.items[where.written_start..][0..where.written_len];
    // This helper proves only selected-site membership. Contextual replay
    // authenticates the complete written span, while the first-malformed
    // selectors separately require its canonical source order.
    var found = false;
    for (written) |where_raw| {
        if (where_raw >= self.store.nodes.len() or
            !isWhereClauseTag(self.store.nodes.get(@enumFromInt(where_raw)).tag))
        {
            return false;
        }
        if (where_raw == @intFromEnum(target)) found = true;
    }
    return found;
}

/// Whether one typed malformed-annotation publication is an exact structural
/// descendant of `annotation_idx`, including its canonically owned where
/// clauses. This follows syntax ownership edges only; a `rigid_var_lookup`
/// reference is deliberately not traversed as a child edge.
pub fn annotationOwnsMalformedTypePublication(
    self: *const Self,
    annotation_idx: CIR.Annotation.Idx,
    publication_index: u32,
) bool {
    const publications = self.malformed_type_annotation_publications.items.items;
    if (publication_index >= publications.len) return false;
    return self.annotationOwnsTypeAnnoNode(
        annotation_idx,
        @enumFromInt(publications[publication_index].annotation_node),
    );
}

fn countOwnedMalformedTypePublications(self: *const Self, annotation_idx: CIR.Annotation.Idx) usize {
    var count: usize = 0;
    for (self.malformed_type_annotation_publications.items.items, 0..) |_, publication_index| {
        if (self.annotationOwnsMalformedTypePublication(annotation_idx, @intCast(publication_index))) count += 1;
    }
    return count;
}

pub fn bodyAnnotationAttachmentLessThan(
    a: BodyAnnotationAttachment,
    b: BodyAnnotationAttachment,
) bool {
    if (a.attachment_node != b.attachment_node) return a.attachment_node < b.attachment_node;
    return a.attachment_kind < b.attachment_kind;
}

fn insertBodyAnnotationAttachmentAssumeCapacity(
    self: *Self,
    attachment: BodyAnnotationAttachment,
) void {
    const entries = self.body_annotation_attachments.items.items;
    var low: usize = 0;
    var high: usize = entries.len;
    while (low < high) {
        const mid = low + (high - low) / 2;
        if (bodyAnnotationAttachmentLessThan(entries[mid], attachment)) {
            low = mid + 1;
        } else {
            high = mid;
        }
    }
    std.debug.assert(low == entries.len or
        bodyAnnotationAttachmentLessThan(attachment, entries[low]));

    _ = self.body_annotation_attachments.appendAssumeCapacity(attachment);
    const grown = self.body_annotation_attachments.items.items;
    std.mem.copyBackwards(
        BodyAnnotationAttachment,
        grown[low + 1 ..],
        grown[low .. grown.len - 1],
    );
    grown[low] = attachment;
}

fn publishBodyAnnotationAttachmentAssumeCapacity(
    self: *Self,
    attachment_kind: BodyAnnotationAttachment.AttachmentKind,
    attachment_node: u32,
    annotation_idx: CIR.Annotation.Idx,
    body_expr: CIR.Expr.Idx,
) void {
    self.insertBodyAnnotationAttachmentAssumeCapacity(.{
        .attachment_kind = @intFromEnum(attachment_kind),
        .attachment_node = attachment_node,
        .annotation_root = @intFromEnum(annotation_idx),
        .body_expr = @intFromEnum(body_expr),
    });
}

fn bodyAnnotationPublicationLessThan(
    a: BodyAnnotationMalformedTypePublication,
    b: BodyAnnotationMalformedTypePublication,
) bool {
    if (a.attachment_node != b.attachment_node) return a.attachment_node < b.attachment_node;
    if (a.attachment_kind != b.attachment_kind) return a.attachment_kind < b.attachment_kind;
    return a.malformed_type_publication_index < b.malformed_type_publication_index;
}

fn insertBodyAnnotationMalformedTypeAssumeCapacity(
    self: *Self,
    publication: BodyAnnotationMalformedTypePublication,
) void {
    const entries = self.body_annotation_malformed_type_publications.items.items;
    var low: usize = 0;
    var high: usize = entries.len;
    while (low < high) {
        const mid = low + (high - low) / 2;
        if (bodyAnnotationPublicationLessThan(entries[mid], publication)) {
            low = mid + 1;
        } else {
            high = mid;
        }
    }
    std.debug.assert(low == entries.len or
        bodyAnnotationPublicationLessThan(publication, entries[low]));

    _ = self.body_annotation_malformed_type_publications.appendAssumeCapacity(publication);
    const grown = self.body_annotation_malformed_type_publications.items.items;
    std.mem.copyBackwards(
        BodyAnnotationMalformedTypePublication,
        grown[low + 1 ..],
        grown[low .. grown.len - 1],
    );
    grown[low] = publication;
}

fn publishBodyAnnotationMalformedTypesAssumeCapacity(
    self: *Self,
    attachment_kind: BodyAnnotationMalformedTypePublication.AttachmentKind,
    attachment_node: u32,
    annotation_idx: CIR.Annotation.Idx,
    body_expr: CIR.Expr.Idx,
) void {
    for (self.malformed_type_annotation_publications.items.items, 0..) |_, publication_index| {
        const raw_publication_index: u32 = @intCast(publication_index);
        if (!self.annotationOwnsMalformedTypePublication(annotation_idx, raw_publication_index)) continue;

        self.insertBodyAnnotationMalformedTypeAssumeCapacity(.{
            .attachment_kind = @intFromEnum(attachment_kind),
            .attachment_node = attachment_node,
            .annotation_root = @intFromEnum(annotation_idx),
            .body_expr = @intFromEnum(body_expr),
            .malformed_type_publication_index = raw_publication_index,
        });
    }
}

pub const ExternalLookupTokenSealError = error{
    ExternalCacheSeedAlreadyPublished,
    InvalidExternalLookupToken,
    UnresolvedLiveImport,
    DuplicateExternalLookupProducer,
};

/// Complete canonicalization's external-lookup producer stream after import
/// resolution and before either cache admission or checking can consume it.
///
/// The operation is idempotent: a coordinator-prepared stream is validated but
/// not mutated when ordinary checker preflight reaches it. Every fallible check
/// completes before the infallible key rewrite and sort, so rejection preserves
/// the exact pre-call stream.
pub fn ensureExternalLookupTokensSealedAfterImportResolution(
    self: *Self,
) ExternalLookupTokenSealError!void {
    if (self.external_cache_seeds.items.items.len != 0) {
        return error.ExternalCacheSeedAlreadyPublished;
    }

    const tokens = self.external_lookup_tokens.items.items;
    const import_count = self.imports.imports.items.items.len;
    for (tokens, 0..) |token, token_index| {
        if (!token.hasCanonicalTags() or @as(usize, token.import_idx) >= import_count) {
            return error.InvalidExternalLookupToken;
        }
        const import_idx: CIR.Import.Idx = @enumFromInt(token.import_idx);
        if (self.imports.getResolvedModule(import_idx) == null and
            !self.imports.importFailedBeforeChecking(import_idx))
        {
            return error.UnresolvedLiveImport;
        }

        // Producer identity excludes only the not-yet-authored resolved-module
        // coordinate. Distinct raw imports remain distinct producer sites even
        // when workspace resolution maps them to the same provider.
        for (tokens[0..token_index]) |prior| {
            if (prior.key.target_node == token.key.target_node and
                prior.import_idx == token.import_idx and
                prior.origin_node == token.origin_node and
                prior.site_kind == token.site_kind and
                prior.parameter_ordinal == token.parameter_ordinal)
            {
                return error.DuplicateExternalLookupProducer;
            }
        }
    }

    var already_sealed = self.externalLookupTokensAreCanonical();
    for (tokens) |token| {
        const import_idx: CIR.Import.Idx = @enumFromInt(token.import_idx);
        const resolved = self.imports.getResolvedModule(import_idx) orelse
            ExternalLookupKey.unresolved;
        already_sealed = already_sealed and token.key.resolved_module_idx == resolved;
    }
    if (already_sealed) return;

    for (tokens) |*token| {
        const import_idx: CIR.Import.Idx = @enumFromInt(token.import_idx);
        token.key.resolved_module_idx = self.imports.getResolvedModule(import_idx) orelse
            ExternalLookupKey.unresolved;
    }
    std.mem.sortUnstable(
        ExternalLookupToken,
        tokens,
        {},
        ExternalLookupToken.canonicalLessThan,
    );
    std.debug.assert(self.externalLookupTokensAreCanonical());
}

/// Append one exact external-template lookup occurrence. The raw import is
/// retained because this API is also called before imports are resolved.
const ExternalLookupTokenInput = struct {
    import_idx: CIR.Import.Idx,
    target_node: u32,
    site_kind: ExternalLookupSiteKind,
};

fn appendExternalLookupTokenAssumeCapacity(
    self: *Self,
    input: ExternalLookupTokenInput,
    origin_node: u32,
    parameter_ordinal: u32,
) void {
    const token = ExternalLookupToken{
        .key = .{
            .resolved_module_idx = self.imports.getResolvedModule(input.import_idx) orelse ExternalLookupKey.unresolved,
            .target_node = input.target_node,
        },
        .import_idx = @intFromEnum(input.import_idx),
        .origin_node = origin_node,
        .site_kind = @intFromEnum(input.site_kind),
        .parameter_ordinal = parameter_ordinal,
    };
    std.debug.assert(token.hasCanonicalTags());
    _ = self.external_lookup_tokens.appendAssumeCapacity(token);
}

/// Atomically publish the external where-alias receiver lookup and the
/// complete provider-declared parameter lookup group. `parameter_nodes` comes
/// directly from the resolved provider declaration header; callers may not
/// synthesize its arity from the consumer application or from checked types.
pub fn appendExternalWhereAliasLookupGroup(
    self: *Self,
    alias_reference_anno: CIR.TypeAnno.Idx,
    import_idx: CIR.Import.Idx,
    declaration_node: u32,
    parameter_nodes: []const CIR.TypeAnno.Idx,
) std.mem.Allocator.Error!void {
    const raw_reference: u32 = @intFromEnum(alias_reference_anno);
    for (self.external_lookup_tokens.items.items) |token| {
        const kind = token.decodedSiteKind() orelse
            std.debug.panic("external lookup token group encountered an invalid prior kind", .{});
        if ((kind == .external_where_alias_receiver or
            kind == .external_where_alias_parameter) and
            token.origin_node == raw_reference)
        {
            std.debug.panic("external where-alias token group was published more than once", .{});
        }
    }
    const group_len = std.math.add(usize, parameter_nodes.len, 1) catch
        return error.OutOfMemory;
    try self.external_lookup_tokens.items.ensureUnusedCapacity(self.gpa, group_len);
    self.appendExternalLookupTokenAssumeCapacity(.{
        .import_idx = import_idx,
        .target_node = declaration_node,
        .site_kind = .external_where_alias_receiver,
    }, raw_reference, ExternalLookupToken.none);
    for (parameter_nodes, 0..) |parameter, ordinal| {
        self.appendExternalLookupTokenAssumeCapacity(.{
            .import_idx = import_idx,
            .target_node = @intFromEnum(parameter),
            .site_kind = .external_where_alias_parameter,
        }, raw_reference, @intCast(ordinal));
    }
}

/// Return the canonical minimum lookup token for one exact cache key.
pub fn minimumExternalLookupToken(
    self: *const Self,
    key: ExternalLookupKey,
) ?ExternalLookupToken.SafeList.Idx {
    var minimum: ?ExternalLookupToken.SafeList.Idx = null;
    for (self.external_lookup_tokens.items.items, 0..) |token, raw_idx| {
        if (!ExternalLookupKey.eql(token.key, key)) continue;
        const idx: ExternalLookupToken.SafeList.Idx = @enumFromInt(@as(u32, @intCast(raw_idx)));
        if (minimum == null or ExternalLookupToken.canonicalLessThan(
            {},
            token,
            self.external_lookup_tokens.items.items[@intFromEnum(minimum.?)],
        )) {
            minimum = idx;
        }
    }
    return minimum;
}

/// Validate that the token pool is a complete nondecreasing canonical stream.
/// This is an explicit structural check; it does not sort or infer missing rows.
pub fn externalLookupTokensAreCanonical(self: *const Self) bool {
    var previous: ?ExternalLookupToken = null;
    for (self.external_lookup_tokens.items.items) |token| {
        if (!token.hasCanonicalTags()) return false;
        if (previous) |before| {
            if (ExternalLookupToken.canonicalLessThan({}, token, before)) return false;
        }
        previous = token;
    }
    return true;
}

/// Return whether every durable seed row is valid and canonically ordered by
/// resolved cache key and its owning minimum token.
pub fn externalCacheSeedsAreCanonical(self: *const Self) bool {
    if (!self.externalLookupTokensAreCanonical()) return false;
    var previous: ?ExternalCacheSeed = null;
    for (self.external_cache_seeds.items.items) |seed| {
        if (!seed.hasCanonicalTags()) return false;
        if (previous) |before| {
            if (ExternalCacheSeed.canonicalLessThan({}, seed, before)) return false;
            if (ExternalLookupKey.eql(seed.key, before.key)) return false;
        }
        const minimum = self.minimumExternalLookupToken(seed.key) orelse return false;
        if (@intFromEnum(minimum) != seed.seed_token) return false;
        const token = self.external_lookup_tokens.items.items[seed.seed_token];
        if (token.origin_node != seed.seed_node) return false;
        previous = seed;
    }
    return true;
}

/// Reserve the stable row named by an eager external-cache support copy. The
/// owner must either complete this row in place or roll the whole transaction
/// back before checked publication.
pub fn reserveExternalCacheSeed(
    self: *Self,
    key: ExternalLookupKey,
    seed_token: u32,
    seed_node: u32,
) std.mem.Allocator.Error!ExternalCacheSeed.SafeList.Idx {
    const row = ExternalCacheSeed{
        .key = key,
        .seed_token = seed_token,
        .seed_node = seed_node,
        .support_step = ExternalCacheSeed.none,
        .state = @intFromEnum(ExternalCacheSeed.State.reserved),
    };
    std.debug.assert(row.hasReservedTags());
    try self.external_cache_seeds.items.ensureUnusedCapacity(self.gpa, 1);
    return self.external_cache_seeds.appendAssumeCapacity(row);
}

/// Complete one owner-held external-cache seed reservation infallibly after
/// its exact support copy and detached binding-codec ingress have succeeded.
pub fn completeExternalCacheSeed(
    self: *Self,
    seed_index: ExternalCacheSeed.SafeList.Idx,
    support_step: u32,
) void {
    const row = &self.external_cache_seeds.items.items[@intFromEnum(seed_index)];
    if (!row.hasReservedTags() or support_step == ExternalCacheSeed.none) {
        std.debug.panic("only a canonical external-cache seed reservation may complete", .{});
    }
    row.support_step = support_step;
    row.state = @intFromEnum(ExternalCacheSeed.State.complete);
    std.debug.assert(row.hasCanonicalTags());
}

/// Add a new expression to the node store.
/// This function asserts that the nodes and regions are in sync.
pub fn addDef(self: *Self, expr: CIR.Def, region: Region) std.mem.Allocator.Error!CIR.Def.Idx {
    const publication_count = if (expr.annotation) |annotation_idx|
        self.countOwnedMalformedTypePublications(annotation_idx)
    else
        0;
    if (expr.annotation != null) {
        try self.body_annotation_attachments.items.ensureUnusedCapacity(self.gpa, 1);
    }
    try self.body_annotation_malformed_type_publications.items.ensureUnusedCapacity(self.gpa, publication_count);

    const expr_idx = try self.store.addDef(expr, region);
    if (expr.annotation) |annotation_idx| {
        self.publishBodyAnnotationAttachmentAssumeCapacity(
            .top_level_def,
            @intFromEnum(expr_idx),
            annotation_idx,
            expr.expr,
        );
        self.publishBodyAnnotationMalformedTypesAssumeCapacity(
            .top_level_def,
            @intFromEnum(expr_idx),
            annotation_idx,
            expr.expr,
        );
    }
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add the temporary Def representation used while a local associated value
/// is canonicalized. This Def is never checked as a Def and therefore publishes
/// no body-annotation attachment; its eventual local statement owns that row.
pub fn addStagedLocalDef(self: *Self, expr: CIR.Def, region: Region) std.mem.Allocator.Error!CIR.Def.Idx {
    const expr_idx = try self.store.addDef(expr, region);
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Retarget a canonicalization-time replacement of a definition body together
/// with the exact malformed-annotation ownership rows published for that def.
pub fn setDefExpr(self: *Self, def_idx: CIR.Def.Idx, new_expr: CIR.Expr.Idx) void {
    self.store.setDefExpr(def_idx, new_expr);

    const raw_def: u32 = @intFromEnum(def_idx);
    for (self.body_annotation_attachments.items.items) |*attachment| {
        if (attachment.attachment_kind == @intFromEnum(BodyAnnotationAttachment.AttachmentKind.top_level_def) and
            attachment.attachment_node == raw_def)
        {
            attachment.body_expr = @intFromEnum(new_expr);
        }
    }
    for (self.body_annotation_malformed_type_publications.items.items) |*publication| {
        if (publication.attachment_kind == @intFromEnum(BodyAnnotationMalformedTypePublication.AttachmentKind.top_level_def) and
            publication.attachment_node == raw_def)
        {
            publication.body_expr = @intFromEnum(new_expr);
        }
    }
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
    const attachment = bodyAnnotationStatementAttachment(expr);
    const publication_count = if (attachment) |owned|
        self.countOwnedMalformedTypePublications(owned.annotation)
    else
        0;
    if (attachment != null) {
        try self.body_annotation_attachments.items.ensureUnusedCapacity(self.gpa, 1);
    }
    try self.body_annotation_malformed_type_publications.items.ensureUnusedCapacity(self.gpa, publication_count);

    const expr_idx = try self.store.addStatement(expr, region);
    if (attachment) |owned| {
        self.publishBodyAnnotationAttachmentAssumeCapacity(
            owned.kind,
            @intFromEnum(expr_idx),
            owned.annotation,
            owned.body_expr,
        );
        self.publishBodyAnnotationMalformedTypesAssumeCapacity(
            owned.kind,
            @intFromEnum(expr_idx),
            owned.annotation,
            owned.body_expr,
        );
    }
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Fill an existing unannotated local-body placeholder and publish the exact
/// malformed annotation ownership relation at the same typed mutation edge.
pub fn setBodyAnnotationStatement(
    self: *Self,
    statement_idx: CIR.Statement.Idx,
    statement: CIR.Statement,
) std.mem.Allocator.Error!void {
    const attachment = bodyAnnotationStatementAttachment(statement) orelse unreachable;
    const previous = self.store.getStatement(statement_idx);
    switch (previous) {
        .s_decl => |decl| {
            std.debug.assert(statement == .s_decl);
            std.debug.assert(decl.anno == null);
        },
        .s_var => |var_| {
            std.debug.assert(statement == .s_var);
            std.debug.assert(var_.anno == null);
        },
        else => unreachable,
    }
    const raw_statement: u32 = @intFromEnum(statement_idx);
    for (self.body_annotation_attachments.items.items) |published_attachment| {
        std.debug.assert(published_attachment.attachment_node != raw_statement);
    }
    for (self.body_annotation_malformed_type_publications.items.items) |publication| {
        std.debug.assert(publication.attachment_node != raw_statement);
    }

    const publication_count = self.countOwnedMalformedTypePublications(attachment.annotation);
    try self.body_annotation_attachments.items.ensureUnusedCapacity(self.gpa, 1);
    try self.body_annotation_malformed_type_publications.items.ensureUnusedCapacity(self.gpa, publication_count);
    try self.store.setStatementNode(statement_idx, statement);
    self.publishBodyAnnotationAttachmentAssumeCapacity(
        attachment.kind,
        raw_statement,
        attachment.annotation,
        attachment.body_expr,
    );
    self.publishBodyAnnotationMalformedTypesAssumeCapacity(
        attachment.kind,
        raw_statement,
        attachment.annotation,
        attachment.body_expr,
    );
    self.debugAssertArraysInSync();
}

/// Add a new pattern to the node store.
/// This function asserts that the nodes and regions are in sync.
pub fn addPattern(self: *Self, expr: CIR.Pattern, region: Region) std.mem.Allocator.Error!CIR.Pattern.Idx {
    const external_lookup: ?ExternalLookupTokenInput = switch (expr) {
        .nominal_external => |nominal| .{
            .import_idx = nominal.module_idx,
            .target_node = nominal.target_node_idx,
            .site_kind = .external_nominal_pattern,
        },
        else => null,
    };
    if (external_lookup != null) {
        try self.external_lookup_tokens.items.ensureUnusedCapacity(self.gpa, 1);
    }
    const expr_idx = try self.store.addPattern(expr, region);
    if (external_lookup) |lookup| {
        self.appendExternalLookupTokenAssumeCapacity(
            lookup,
            @intFromEnum(expr_idx),
            ExternalLookupToken.none,
        );
    }
    self.debugAssertArraysInSync();
    return expr_idx;
}

/// Add a new expression to the node store.
/// This function asserts that the nodes and regions are in sync.
pub fn addExpr(self: *Self, expr: CIR.Expr, region: Region) std.mem.Allocator.Error!CIR.Expr.Idx {
    const call_token_kind: ?ExpectedCallKind = switch (expr) {
        .e_call => |call| switch (call.called_via) {
            .apply => .apply,
            .record_builder => .record_builder,
            .binop, .unary_op, .string_interpolation => std.debug.panic(
                "function-call CIR node used a non-call-shape producer kind",
                .{},
            ),
        },
        else => null,
    };
    const call_token_count: usize = switch (expr) {
        .e_call => |call| @as(usize, call.args.span.len) + 1,
        else => 0,
    };
    const external_lookup: ?ExternalLookupTokenInput = switch (expr) {
        .e_lookup_external => |lookup| .{
            .import_idx = lookup.module_idx,
            .target_node = lookup.target_node_idx,
            .site_kind = .external_lookup_expr,
        },
        .e_nominal_external => |nominal| .{
            .import_idx = nominal.module_idx,
            .target_node = nominal.target_node_idx,
            .site_kind = .external_nominal_expr,
        },
        .e_lookup_associated => |lookup| .{
            .import_idx = lookup.module_idx,
            .target_node = lookup.type_node_idx,
            .site_kind = .external_associated_lookup,
        },
        else => null,
    };
    if (call_token_count != 0) {
        try self.expected_call_slot_tokens.items.ensureUnusedCapacity(
            self.gpa,
            call_token_count,
        );
        try self.store.ensureCallExprCapacity();
    }
    // Reserve the external token before mutating NodeStore. Once the node
    // append succeeds, the token append below is infallible and the two
    // producer records are published together.
    if (external_lookup != null) {
        try self.external_lookup_tokens.items.ensureUnusedCapacity(self.gpa, 1);
    }
    const expr_idx = if (call_token_count != 0)
        self.store.addCallExprAssumeCapacity(expr, region)
    else
        try self.store.addExpr(expr, region);
    switch (expr) {
        .e_call => |call| {
            const called_via = call_token_kind orelse unreachable;
            const owner_node: u32 = @intFromEnum(expr_idx);
            const cardinality = call.args.span.len;
            _ = self.expected_call_slot_tokens.appendAssumeCapacity(.{
                .owner_node = owner_node,
                .site_node = @intFromEnum(call.func),
                .slot = ExpectedCallSlotToken.none,
                .cardinality = cardinality,
                .called_via = @intFromEnum(called_via),
                .role = @intFromEnum(ExpectedCallSlotToken.Role.root),
            });
            for (self.store.sliceExpr(call.args), 0..) |argument, slot| {
                _ = self.expected_call_slot_tokens.appendAssumeCapacity(.{
                    .owner_node = owner_node,
                    .site_node = @intFromEnum(argument),
                    .slot = @intCast(slot),
                    .cardinality = cardinality,
                    .called_via = @intFromEnum(called_via),
                    .role = @intFromEnum(ExpectedCallSlotToken.Role.argument),
                });
            }
        },
        else => {},
    }
    if (external_lookup) |lookup| {
        self.appendExternalLookupTokenAssumeCapacity(
            lookup,
            @intFromEnum(expr_idx),
            ExternalLookupToken.none,
        );
    }
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

/// Add a new unset record field (`name: _`) to the node store.
/// This function asserts that the nodes and regions are in sync.
pub fn addUnsetField(self: *Self, unset_field: CIR.UnsetField, region: Region) std.mem.Allocator.Error!CIR.UnsetField.Idx {
    const unset_idx = try self.store.addUnsetField(unset_field, region);
    self.debugAssertArraysInSync();
    return unset_idx;
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
    const external_lookup: ?ExternalLookupTokenInput = switch (expr) {
        .lookup => |lookup| switch (lookup.base) {
            .external => |external| .{
                .import_idx = external.module_idx,
                .target_node = external.target_node_idx,
                .site_kind = .external_type_annotation_lookup,
            },
            .builtin, .local, .pending => null,
        },
        .apply => |apply| switch (apply.base) {
            .external => |external| .{
                .import_idx = external.module_idx,
                .target_node = external.target_node_idx,
                .site_kind = .external_type_annotation_apply,
            },
            .builtin, .local, .pending => null,
        },
        .rigid_var,
        .rigid_var_lookup,
        .underscore,
        .tag_union,
        .tag,
        .tuple,
        .record,
        .@"fn",
        .parens,
        .malformed,
        => null,
    };
    if (external_lookup != null) {
        try self.external_lookup_tokens.items.ensureUnusedCapacity(self.gpa, 1);
    }
    const expr_idx = try self.store.addTypeAnno(expr, region);
    if (external_lookup) |lookup| {
        self.appendExternalLookupTokenAssumeCapacity(
            lookup,
            @intFromEnum(expr_idx),
            ExternalLookupToken.none,
        );
    }
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

/// The coordinator-assigned package-qualified module identifier (e.g.
/// `pf.Utils`), unique across the build's packages. Module identity
/// comparisons in diagnostics must use this rather than the bare module
/// name, which can collide between packages. Environments constructed
/// outside the coordinator (unit tests) have no qualified ident; they are
/// single-module worlds, so the bare name is their qualified name.
pub fn qualifiedModuleName(self: *const Self) []const u8 {
    if (self.qualified_module_ident.isNone()) return self.module_name;
    return self.getIdent(self.qualified_module_ident);
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
    if (self.module_identities.lookup(hash)) |existing| {
        std.debug.assert(existing < self.module_identity_displays.len());
        return @enumFromInt(existing);
    }
    const before = self.module_identities.count();
    try self.module_identity_displays.items.ensureUnusedCapacity(self.gpa, 1);
    const id = try self.module_identities.insert(self.gpa, hash);
    std.debug.assert(id == before);
    self.module_identity_displays.items.appendAssumeCapacity(display);
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
    const entry = self.lookupMethodBindingEntryForMethodOwnerConst(owner, method_ident) orelse return null;
    return entry.binding;
}

/// Looks up method metadata and its exact finalized provider-table row for an
/// explicit receiver owner declaration.
pub fn lookupMethodBindingEntryForMethodOwnerConst(self: *const Self, owner: MethodOwner, method_ident: Ident.Idx) ?MethodBindingEntry {
    const key = MethodKey.init(owner, method_ident);
    if (builtin.mode == .Debug) {
        std.debug.assert(self.method_defs.sorted);
        std.debug.assert(self.method_defs.deduplicated);
    }

    var left: usize = 0;
    var right = self.method_defs.entries.items.len;
    while (left < right) {
        const mid = left + (right - left) / 2;
        const entry = self.method_defs.entries.items[mid];
        switch (MethodKey.order(entry.key, key)) {
            .eq => return .{
                .entry_index = std.math.cast(u32, mid) orelse return null,
                .binding = entry.value,
            },
            .lt => left = mid + 1,
            .gt => right = mid,
        }
    }
    return null;
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

/// Looks up method metadata and its exact finalized provider-table row using
/// a type declaration owner and method ident from different environments.
pub fn lookupMethodBindingEntryFromTwoEnvsAndDeclConst(
    self: *const Self,
    source_decl: ?u32,
    method_source_env: *const Self,
    method_ident: Ident.Idx,
) ?MethodBindingEntry {
    return self.lookupMethodBindingEntryFromOwnerAndMethodEnvsConst(self, source_decl, method_source_env, method_ident);
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
    const entry = self.lookupMethodBindingEntryFromOwnerAndMethodEnvsConst(
        owner_source_env,
        source_decl,
        method_source_env,
        method_ident,
    ) orelse return null;
    return entry.binding;
}

/// Looks up method metadata and its exact finalized provider-table row using
/// an owner declaration and method ident which may come from different envs.
pub fn lookupMethodBindingEntryFromOwnerAndMethodEnvsConst(
    self: *const Self,
    owner_source_env: *const Self,
    source_decl: ?u32,
    method_source_env: *const Self,
    method_ident: Ident.Idx,
) ?MethodBindingEntry {
    const method_name = method_source_env.getIdent(method_ident);
    const owner_module_name = owner_source_env.getIdent(owner_source_env.qualified_module_ident);

    const local_method_ident = self.common.findIdent(method_name) orelse return null;
    const local_owner_module_ident = self.common.findIdent(owner_module_name) orelse return null;
    const owner: CIR.Statement.Idx = @enumFromInt(source_decl orelse return null);

    return self.lookupMethodBindingEntryForMethodOwnerConst(MethodOwner.init(local_owner_module_ident, owner), local_method_ident);
}

/// Returns the line start positions for source code position mapping.
/// Each element represents the byte offset where a new line begins.
pub fn getLineStarts(self: *const Self) []const u32 {
    return self.common.getLineStartsAll();
}

const CrossModuleCopyOwnedStateSnapshot = struct {
    allocator: Allocator,
    bytes: []u8,

    fn capture(allocator: Allocator, env: *const Self) Allocator.Error!@This() {
        var bytes: std.ArrayList(u8) = .empty;
        errdefer bytes.deinit(allocator);

        try appendValue(&bytes, allocator, env.common.idents.interner.entry_count);
        try appendSlice(&bytes, allocator, env.common.idents.interner.bytes.items.items);
        try appendSlice(&bytes, allocator, env.common.idents.interner.index.items.items);

        try appendValue(&bytes, allocator, env.module_identities.count());
        try appendSlice(&bytes, allocator, env.module_identities.bytes.items.items);
        try appendSlice(&bytes, allocator, env.module_identities.ranges.items.items);
        try appendSlice(&bytes, allocator, env.module_identities.index.items.items);
        try appendSlice(&bytes, allocator, env.module_identity_displays.items.items);

        try appendSlice(&bytes, allocator, env.where_marker_copy_steps.items.items);
        try appendSlice(&bytes, allocator, env.where_marker_copy_pairs.items.items);
        try appendSlice(&bytes, allocator, env.where_marker_copy_occurrences.items.items);
        try appendSlice(&bytes, allocator, env.where_marker_constraint_copy_pairs.items.items);
        try appendSlice(&bytes, allocator, env.where_marker_copy_witnesses.items.items);
        try appendSlice(&bytes, allocator, env.copied_open_literal_groups.items.items);
        try appendSlice(&bytes, allocator, env.copied_open_literal_events.items.items);
        try appendSlice(&bytes, allocator, env.where_marker_platform_substitutions.items.items);
        try appendSlice(&bytes, allocator, env.selected_receiver_anchors.items.items);
        try appendSlice(&bytes, allocator, env.dispatch_settlement_sources.items.items);

        return .{
            .allocator = allocator,
            .bytes = try bytes.toOwnedSlice(allocator),
        };
    }

    fn expectEqual(self: *const @This(), env: *const Self) !void {
        var actual = try capture(self.allocator, env);
        defer actual.deinit();
        try std.testing.expectEqualSlices(u8, self.bytes, actual.bytes);
    }

    fn deinit(self: *@This()) void {
        self.allocator.free(self.bytes);
        self.* = undefined;
    }

    fn appendValue(
        bytes: *std.ArrayList(u8),
        allocator: Allocator,
        value: anytype,
    ) Allocator.Error!void {
        try bytes.appendSlice(allocator, std.mem.asBytes(&value));
    }

    fn appendSlice(
        bytes: *std.ArrayList(u8),
        allocator: Allocator,
        slice: anytype,
    ) Allocator.Error!void {
        const len: u64 = @intCast(slice.len);
        try appendValue(bytes, allocator, len);
        try bytes.appendSlice(allocator, std.mem.sliceAsBytes(slice));
    }
};

fn appendCrossModuleCopyOwnedRows(env: *Self, tag: u32) Allocator.Error!void {
    const copy_step_index: u32 = @intCast(env.where_marker_copy_steps.items.items.len);
    const copied_group_index: u32 = @intCast(env.copied_open_literal_groups.items.items.len);
    const copied_event_index: u32 = @intCast(env.copied_open_literal_events.items.items.len);
    _ = try env.where_marker_copy_steps.append(env.gpa, .{
        .kind = @intFromEnum(WhereMarkerCopyStep.Kind.reserved),
        .copy_policy = @intFromEnum(WhereMarkerCopyStep.CopyPolicy.cross_module_import),
        .source_root_var = tag,
        .destination_root_var = tag + 1,
        .pairs_start = tag + 2,
        .pairs_len = 1,
        .copied_groups_start = copied_group_index,
        .copied_groups_len = 1,
        .origin = .{ .reserved = .{} },
    });
    _ = try env.where_marker_copy_pairs.append(env.gpa, .{
        .source_var = tag,
        .destination_var = tag + 1,
        .discovery_depth = 0,
        .predecessor_pair_offset = std.math.maxInt(u32),
        .predecessor_edge_ordinal = std.math.maxInt(u32),
    });
    _ = try env.where_marker_copy_occurrences.append(env.gpa, .{
        .raw_source_var = tag,
        .raw_destination_var = tag + 1,
        .canonical_pair_offset = 0,
    });
    _ = try env.where_marker_constraint_copy_pairs.append(env.gpa, .{
        .source_constraint_index = tag,
        .destination_constraint_index = tag + 1,
    });
    _ = try env.where_marker_copy_witnesses.append(env.gpa, .{
        .parent_occurrence_offset = 0,
        .child_occurrence_offset = 0,
        .edge_kind = @intFromEnum(WhereMarkerCopyWitness.EdgeKind.root_copy_action),
        .edge_index = tag,
        .edge_name = 0,
        .edge_origin_module = 0,
        .edge_source_decl = 0,
        .action = @intFromEnum(WhereMarkerCopyWitness.Action.traverse),
        .auxiliary_origin_kind = @intFromEnum(WhereMarkerCopyWitness.AuxiliaryOriginKind.none),
        .auxiliary_origin_index = 0,
    });
    _ = try env.copied_open_literal_groups.append(env.gpa, .{
        .copy_step_index = copy_step_index,
        .receiver_occurrence_offset = 0,
        .source_constraints_start = tag,
        .source_constraints_len = 1,
        .destination_constraints_start = tag + 1,
        .destination_constraints_len = 1,
        .component = CopiedOpenLiteralComponent.rootGraph(),
        .events_start = copied_event_index,
        .events_len = 1,
    });
    _ = try env.copied_open_literal_events.append(env.gpa, .{
        .group_index = copied_group_index,
        .constraint_offset = 0,
        .literal_kind = @intFromEnum(CopiedOpenLiteralEvent.LiteralKind.numeral),
    });
    _ = try env.where_marker_platform_substitutions.append(env.gpa, .{
        .platform_alias_statement = tag,
        .app_declaration_node = tag + 1,
        .app_instantiation_step = tag + 2,
    });
    _ = try env.selected_receiver_anchors.append(env.gpa, .{
        .constraint_index = tag,
        .receiver_var = tag + 1,
        .kind = @intFromEnum(SelectedMethodDecision.ReceiverAnchorKind.copied_constraint),
        .node = 0,
        .slot = 0,
        .copy_step = tag + 2,
        .receiver_occurrence_offset = 0,
        .constraint_pair_offset = 0,
    });
    _ = try env.dispatch_settlement_sources.append(
        env.gpa,
        DispatchSettlementSource.whereRequirement(tag),
    );
}

fn appendCrossModuleCopyOwnedMutation(
    env: *Self,
    ident_text: []const u8,
    hash: *const base.ModuleIdentity.Hash,
    tag: u32,
) Allocator.Error!void {
    const display = try env.insertIdent(Ident.for_text(ident_text));
    _ = try env.internModuleIdentity(hash, display);
    try appendCrossModuleCopyOwnedRows(env, tag);
}

test "cross-module copy mark inner commit remains owned by outer rollback" {
    var env = try Self.init(std.testing.allocator, "");
    defer env.deinit();

    const prefix_hash = [_]u8{0x71} ** @sizeOf(base.ModuleIdentity.Hash);
    const outer_hash = [_]u8{0x72} ** @sizeOf(base.ModuleIdentity.Hash);
    const inner_hash = [_]u8{0x73} ** @sizeOf(base.ModuleIdentity.Hash);
    try appendCrossModuleCopyOwnedMutation(&env, "cross-copy-mark-prefix", &prefix_hash, 10);

    var baseline = try CrossModuleCopyOwnedStateSnapshot.capture(std.testing.allocator, &env);
    defer baseline.deinit();
    try std.testing.expect(env.common.findIdent("cross-copy-mark-outer-rollback") == null);
    try std.testing.expect(env.common.findIdent("cross-copy-mark-inner-committed") == null);
    try std.testing.expect(env.lookupModuleIdentity(&outer_hash) == null);
    try std.testing.expect(env.lookupModuleIdentity(&inner_hash) == null);

    var outer = try env.beginCrossModuleCopyMark();
    var outer_open = true;
    errdefer if (outer_open) env.rollbackCrossModuleCopyMark(&outer);
    try appendCrossModuleCopyOwnedMutation(&env, "cross-copy-mark-outer-rollback", &outer_hash, 20);
    var inner = try env.beginCrossModuleCopyMark();
    var inner_open = true;
    errdefer if (inner_open) env.rollbackCrossModuleCopyMark(&inner);
    try appendCrossModuleCopyOwnedMutation(&env, "cross-copy-mark-inner-committed", &inner_hash, 30);
    env.commitCrossModuleCopyMark(&inner);
    inner_open = false;

    try std.testing.expect(env.common.findIdent("cross-copy-mark-outer-rollback") != null);
    try std.testing.expect(env.common.findIdent("cross-copy-mark-inner-committed") != null);
    try std.testing.expect(env.lookupModuleIdentity(&outer_hash) != null);
    try std.testing.expect(env.lookupModuleIdentity(&inner_hash) != null);
    try std.testing.expectEqual(@as(u16, 1), env.common.idents.interner.savepoint_depth);
    try std.testing.expectEqual(@as(u16, 1), env.module_identities.savepoint_depth);

    env.rollbackCrossModuleCopyMark(&outer);
    outer_open = false;
    try baseline.expectEqual(&env);
    try std.testing.expect(env.common.findIdent("cross-copy-mark-outer-rollback") == null);
    try std.testing.expect(env.common.findIdent("cross-copy-mark-inner-committed") == null);
    try std.testing.expect(env.lookupModuleIdentity(&outer_hash) == null);
    try std.testing.expect(env.lookupModuleIdentity(&inner_hash) == null);
    try std.testing.expectEqual(@as(u16, 0), env.common.idents.interner.savepoint_depth);
    try std.testing.expectEqual(@as(u16, 0), env.module_identities.savepoint_depth);
    try env.common.idents.interner.validateSemanticState();
    try env.module_identities.validateSemanticState(@sizeOf(base.ModuleIdentity.Hash));
}

test "cross-module copy mark inner rollback preserves outer commit exactly" {
    var env = try Self.init(std.testing.allocator, "");
    defer env.deinit();

    const prefix_hash = [_]u8{0x81} ** @sizeOf(base.ModuleIdentity.Hash);
    const outer_hash = [_]u8{0x82} ** @sizeOf(base.ModuleIdentity.Hash);
    const inner_hash = [_]u8{0x83} ** @sizeOf(base.ModuleIdentity.Hash);
    try appendCrossModuleCopyOwnedMutation(&env, "cross-copy-mark-second-prefix", &prefix_hash, 40);
    try std.testing.expect(env.common.findIdent("cross-copy-mark-outer-commit") == null);
    try std.testing.expect(env.common.findIdent("cross-copy-mark-inner-rollback") == null);

    var outer = try env.beginCrossModuleCopyMark();
    var outer_open = true;
    errdefer if (outer_open) env.rollbackCrossModuleCopyMark(&outer);
    try appendCrossModuleCopyOwnedMutation(&env, "cross-copy-mark-outer-commit", &outer_hash, 50);
    var outer_state = try CrossModuleCopyOwnedStateSnapshot.capture(std.testing.allocator, &env);
    defer outer_state.deinit();

    var inner = try env.beginCrossModuleCopyMark();
    var inner_open = true;
    errdefer if (inner_open) env.rollbackCrossModuleCopyMark(&inner);
    try appendCrossModuleCopyOwnedMutation(&env, "cross-copy-mark-inner-rollback", &inner_hash, 60);
    env.rollbackCrossModuleCopyMark(&inner);
    inner_open = false;

    try outer_state.expectEqual(&env);
    try std.testing.expect(env.common.findIdent("cross-copy-mark-outer-commit") != null);
    try std.testing.expect(env.common.findIdent("cross-copy-mark-inner-rollback") == null);
    try std.testing.expect(env.lookupModuleIdentity(&outer_hash) != null);
    try std.testing.expect(env.lookupModuleIdentity(&inner_hash) == null);
    try std.testing.expectEqual(@as(u16, 1), env.common.idents.interner.savepoint_depth);
    try std.testing.expectEqual(@as(u16, 1), env.module_identities.savepoint_depth);

    env.commitCrossModuleCopyMark(&outer);
    outer_open = false;
    try outer_state.expectEqual(&env);
    try std.testing.expect(env.common.findIdent("cross-copy-mark-outer-commit") != null);
    try std.testing.expect(env.common.findIdent("cross-copy-mark-inner-rollback") == null);
    try std.testing.expect(env.lookupModuleIdentity(&outer_hash) != null);
    try std.testing.expect(env.lookupModuleIdentity(&inner_hash) == null);
    try std.testing.expectEqual(@as(u16, 0), env.common.idents.interner.savepoint_depth);
    try std.testing.expectEqual(@as(u16, 0), env.module_identities.savepoint_depth);
    try env.common.idents.interner.validateSemanticState();
    try env.module_identities.validateSemanticState(@sizeOf(base.ModuleIdentity.Hash));
}

test "cross-module copy mark opening OOM preserves active owner at both snapshot stages" {
    var env = try Self.init(std.testing.allocator, "");
    defer env.deinit();

    const prefix_hash = [_]u8{0x91} ** @sizeOf(base.ModuleIdentity.Hash);
    const outer_hash = [_]u8{0x92} ** @sizeOf(base.ModuleIdentity.Hash);
    try appendCrossModuleCopyOwnedMutation(&env, "cross-copy-mark-oom-prefix", &prefix_hash, 70);
    var root_state = try CrossModuleCopyOwnedStateSnapshot.capture(std.testing.allocator, &env);
    defer root_state.deinit();

    var outer = try env.beginCrossModuleCopyMark();
    var outer_open = true;
    errdefer if (outer_open) env.rollbackCrossModuleCopyMark(&outer);
    try appendCrossModuleCopyOwnedMutation(&env, "cross-copy-mark-oom-outer", &outer_hash, 80);
    var active_state = try CrossModuleCopyOwnedStateSnapshot.capture(std.testing.allocator, &env);
    defer active_state.deinit();

    for (0..2) |fail_index| {
        var failing = std.testing.FailingAllocator.init(
            std.testing.allocator,
            .{ .fail_index = fail_index },
        );
        const saved_gpa = env.gpa;
        env.gpa = failing.allocator();
        const result = env.beginCrossModuleCopyMark();
        env.gpa = saved_gpa;

        if (result) |opened| {
            var unexpected = opened;
            env.rollbackCrossModuleCopyMark(&unexpected);
            return error.TestExpectedOutOfMemory;
        } else |err| {
            try std.testing.expectEqual(error.OutOfMemory, err);
            try std.testing.expect(failing.has_induced_failure);
        }

        try active_state.expectEqual(&env);
        try std.testing.expectEqual(@as(u16, 1), env.common.idents.interner.savepoint_depth);
        try std.testing.expectEqual(@as(u16, 1), env.module_identities.savepoint_depth);
        try std.testing.expect(outer.ident_interner.owner == &env.common.idents.interner);
        try std.testing.expect(outer.module_identity_interner.owner == &env.module_identities);
        try std.testing.expectEqual(@as(u16, 1), outer.ident_interner.depth);
        try std.testing.expectEqual(@as(u16, 1), outer.module_identity_interner.depth);
    }

    var inner = try env.beginCrossModuleCopyMark();
    var inner_open = true;
    errdefer if (inner_open) env.rollbackCrossModuleCopyMark(&inner);
    env.commitCrossModuleCopyMark(&inner);
    inner_open = false;
    try active_state.expectEqual(&env);
    try std.testing.expectEqual(@as(u16, 1), env.common.idents.interner.savepoint_depth);
    try std.testing.expectEqual(@as(u16, 1), env.module_identities.savepoint_depth);

    env.rollbackCrossModuleCopyMark(&outer);
    outer_open = false;
    try root_state.expectEqual(&env);
    try std.testing.expect(env.common.findIdent("cross-copy-mark-oom-outer") == null);
    try std.testing.expect(env.lookupModuleIdentity(&outer_hash) == null);
    try std.testing.expectEqual(@as(u16, 0), env.common.idents.interner.savepoint_depth);
    try std.testing.expectEqual(@as(u16, 0), env.module_identities.savepoint_depth);
}

test "where-marker copy kinds have one closed source namespace" {
    var local_count: usize = 0;
    var cross_module_count: usize = 0;
    var transient_count: usize = 0;
    for (std.enums.values(WhereMarkerCopyStep.Kind)) |kind| {
        switch (kind.sourceNamespace()) {
            .local => local_count += 1,
            .cross_module => cross_module_count += 1,
            .transient => transient_count += 1,
        }
    }

    try std.testing.expectEqual(@as(usize, 20), local_count);
    try std.testing.expectEqual(@as(usize, 14), cross_module_count);
    try std.testing.expectEqual(@as(usize, 2), transient_count);
    try std.testing.expectEqual(
        WhereMarkerCopyStep.Kind.SourceNamespace.transient,
        WhereMarkerCopyStep.Kind.reserved.sourceNamespace(),
    );
    try std.testing.expectEqual(
        WhereMarkerCopyStep.Kind.SourceNamespace.transient,
        WhereMarkerCopyStep.Kind.discarded.sourceNamespace(),
    );
}

test "expected consumption legality covers every closed tag" {
    const roles = std.enums.values(ExpectedConsumptionPlan.Role);
    const outcomes = std.enums.values(ExpectedConsumptionPlan.Outcome);
    const reasons = std.enums.values(ExpectedConsumptionPlan.Reason);
    const owner_kinds = std.enums.values(CauseOwner.Kind);

    var seen_roles = [_]bool{false} ** roles.len;
    var seen_outcomes = [_]bool{false} ** outcomes.len;
    var seen_reasons = [_]bool{false} ** reasons.len;
    var seen_owner_kinds = [_]bool{false} ** owner_kinds.len;

    for (roles) |role| {
        for (outcomes) |outcome| {
            if (ExpectedConsumptionPlan.legalCombination(role, outcome, null, null, false)) {
                seen_roles[@intFromEnum(role)] = true;
                seen_outcomes[@intFromEnum(outcome)] = true;
            }
            for (reasons) |reason| {
                for ([_]bool{ false, true }) |has_failure_cause_plan| {
                    if (ExpectedConsumptionPlan.legalCombination(role, outcome, reason, null, has_failure_cause_plan)) {
                        seen_roles[@intFromEnum(role)] = true;
                        seen_outcomes[@intFromEnum(outcome)] = true;
                        seen_reasons[@intFromEnum(reason)] = true;
                    }
                    for (owner_kinds) |owner_kind| {
                        if (!ExpectedConsumptionPlan.legalCombination(
                            role,
                            outcome,
                            reason,
                            owner_kind,
                            has_failure_cause_plan,
                        )) continue;
                        seen_roles[@intFromEnum(role)] = true;
                        seen_outcomes[@intFromEnum(outcome)] = true;
                        seen_reasons[@intFromEnum(reason)] = true;
                        seen_owner_kinds[@intFromEnum(owner_kind)] = true;
                    }
                }
            }
        }
    }

    for (seen_roles) |seen| try std.testing.expect(seen);
    for (outcomes) |outcome| {
        if (outcome == .reserved) {
            try std.testing.expect(!seen_outcomes[@intFromEnum(outcome)]);
        } else {
            try std.testing.expect(seen_outcomes[@intFromEnum(outcome)]);
        }
    }
    for (seen_reasons) |seen| try std.testing.expect(seen);
    for (seen_owner_kinds) |seen| try std.testing.expect(seen);
}

test "expected consumption owner absence is canonical" {
    var plan = ExpectedConsumptionPlan{
        .owner_node = 1,
        .site_node = 2,
        .role = @intFromEnum(ExpectedConsumptionPlan.Role.aggregate_owner),
        .slot = 0,
        .outcome = @intFromEnum(ExpectedConsumptionPlan.Outcome.anchored),
        .reason = ExpectedConsumptionPlan.none,
        .raw_consumer_var = 2,
        .parent_authority = .{
            .kind = @intFromEnum(ExpectedMarkerAuthority.Kind.copy_occurrence),
            .payload = .{ .copy_occurrence = .{
                .copy_step = 0,
                .occurrence_offset = 0,
                .side = @intFromEnum(WhereMarkerCopyOccurrenceSide.source),
            } },
        },
        .produced_copy_step = 1,
        .produced_occurrence_offset = 0,
        .produced_side = @intFromEnum(WhereMarkerCopyOccurrenceSide.destination),
        .call_root_plan_index = ExpectedConsumptionPlan.none,
        .failure_owner = CauseOwner.inactive(),
        .failure_cause_plan_index = ExpectedConsumptionPlan.none,
    };
    try std.testing.expect(plan.hasLegalTags());

    plan.failure_owner.payload.expected_failure.index = 0;
    try std.testing.expect(!plan.hasLegalTags());
    plan.failure_owner = CauseOwner.inactive();

    plan.failure_owner = CauseOwner.expectedConsumerRetirement(0);
    try std.testing.expect(!plan.hasLegalTags());

    plan.produced_copy_step = ExpectedConsumptionPlan.none;
    plan.produced_occurrence_offset = ExpectedConsumptionPlan.none;
    plan.produced_side = ExpectedConsumptionPlan.none;
    plan.outcome = @intFromEnum(ExpectedConsumptionPlan.Outcome.checked_error);
    plan.reason = @intFromEnum(ExpectedConsumptionPlan.Reason.aggregate_expected_contains_error);
    try std.testing.expect(!plan.hasLegalTags());
    plan.failure_cause_plan_index = 0;
    try std.testing.expect(plan.hasLegalTags());
    plan.failure_owner = CauseOwner.expectedFailure(0);
    try std.testing.expect(plan.hasLegalTags());

    plan.failure_owner = CauseOwner.expectedConsumerRetirement(0);
    try std.testing.expect(plan.hasLegalTags());
    plan.failure_cause_plan_index = ExpectedConsumptionPlan.none;
    try std.testing.expect(!plan.hasLegalTags());
    plan.failure_cause_plan_index = 0;

    plan.reason = @intFromEnum(ExpectedConsumptionPlan.Reason.aggregate_shape_mismatch);
    try std.testing.expect(!plan.hasLegalTags());
    plan.failure_cause_plan_index = ExpectedConsumptionPlan.none;
    try std.testing.expect(plan.hasLegalTags());
    plan.outcome = @intFromEnum(ExpectedConsumptionPlan.Outcome.not_projected);
    try std.testing.expect(!plan.hasLegalTags());

    plan.outcome = @intFromEnum(ExpectedConsumptionPlan.Outcome.checked_error);
    plan.reason = @intFromEnum(ExpectedConsumptionPlan.Reason.aggregate_expected_direct_error);
    try std.testing.expect(plan.hasLegalTags());
    plan.failure_owner = CauseOwner.expectedFailure(0);
    try std.testing.expect(plan.hasLegalTags());
    plan.failure_cause_plan_index = 1;
    try std.testing.expect(!plan.hasLegalTags());
    plan.failure_cause_plan_index = ExpectedConsumptionPlan.none;

    plan.reason = @intFromEnum(ExpectedConsumptionPlan.Reason.aggregate_child_relation_rejected);
    plan.role = @intFromEnum(ExpectedConsumptionPlan.Role.list_element);
    plan.failure_owner = CauseOwner.inactive();
    try std.testing.expect(!plan.hasLegalTags());
    plan.failure_owner = CauseOwner.expectedFailure(0);
    try std.testing.expect(plan.hasLegalTags());
    plan.failure_owner = CauseOwner.expectedConsumerRetirement(0);
    try std.testing.expect(!plan.hasLegalTags());
    plan.failure_owner = CauseOwner.expectedFailure(0);

    plan.reason = @intFromEnum(ExpectedConsumptionPlan.Reason.aggregate_child_relation_suppressed);
    plan.failure_owner = CauseOwner.inactive();
    try std.testing.expect(!plan.hasLegalTags());
    plan.failure_owner = CauseOwner.expectedFailure(0);
    try std.testing.expect(plan.hasLegalTags());
    plan.failure_owner = CauseOwner.expectedConsumerRetirement(0);
    try std.testing.expect(plan.hasLegalTags());
    plan.failure_owner = CauseOwner.cirDiagnostic(0);
    try std.testing.expect(plan.hasLegalTags());
    plan.failure_owner = CauseOwner.providerWhereAliasCheckedError(0, 0);
    try std.testing.expect(plan.hasLegalTags());
    plan.failure_cause_plan_index = 0;
    try std.testing.expect(!plan.hasLegalTags());
    plan.failure_cause_plan_index = ExpectedConsumptionPlan.none;
    plan.role = @intFromEnum(ExpectedConsumptionPlan.Role.aggregate_owner);
    try std.testing.expect(!plan.hasLegalTags());

    plan.reason = std.math.maxInt(u32) - 1;
    try std.testing.expect(!plan.hasLegalTags());
    plan.reason = @intFromEnum(ExpectedConsumptionPlan.Reason.aggregate_expected_contains_error);
    plan.role = std.math.maxInt(u32);
    try std.testing.expect(!plan.hasLegalTags());
}

test "expected marker authority arms have canonical payloads" {
    const none = ExpectedMarkerAuthority.none;
    try std.testing.expectEqual(
        @as(usize, 4),
        std.enums.values(ExpectedMarkerAuthority.Kind).len,
    );
    var authority = ExpectedMarkerAuthority.inactive();
    try std.testing.expect(authority.hasCanonicalTags(false));
    try std.testing.expect(!authority.hasCanonicalTags(true));

    authority.payload.copy_occurrence.copy_step = 0;
    try std.testing.expect(!authority.hasCanonicalTags(false));
    authority = .{
        .kind = @intFromEnum(ExpectedMarkerAuthority.Kind.copy_occurrence),
        .payload = .{ .copy_occurrence = .{
            .copy_step = 1,
            .occurrence_offset = 2,
            .side = @intFromEnum(WhereMarkerCopyOccurrenceSide.destination),
        } },
    };
    try std.testing.expect(authority.hasCanonicalTags(true));
    authority.payload.copy_occurrence.side = none;
    try std.testing.expect(!authority.hasCanonicalTags(true));

    authority = .{
        .kind = @intFromEnum(ExpectedMarkerAuthority.Kind.producer_root_plan),
        .payload = .{ .producer_root_plan = .{
            .plan_index = 3,
            .raw_var = 4,
        } },
    };
    try std.testing.expect(authority.hasCanonicalTags(true));
    authority.payload.producer_root_plan.raw_var = none;
    try std.testing.expect(!authority.hasCanonicalTags(true));
    authority.payload.producer_root_plan.raw_var = 4;
    authority.payload.producer_root_plan.reserved_0 = 1;
    try std.testing.expect(!authority.hasCanonicalTags(true));

    authority = .{
        .kind = @intFromEnum(ExpectedMarkerAuthority.Kind.relation_plan),
        .payload = .{ .relation_plan = .{
            .plan_index = 5,
            .raw_var = 6,
        } },
    };
    try std.testing.expect(authority.hasCanonicalTags(true));
    authority.payload.relation_plan.raw_var = none;
    try std.testing.expect(!authority.hasCanonicalTags(true));
    authority.payload.relation_plan.raw_var = 6;
    authority.payload.relation_plan.plan_index = none;
    try std.testing.expect(!authority.hasCanonicalTags(true));
    authority.payload.relation_plan.plan_index = 5;
    authority.payload.relation_plan.reserved_0 = 1;
    try std.testing.expect(!authority.hasCanonicalTags(true));

    authority = .{
        .kind = @intFromEnum(ExpectedMarkerAuthority.Kind.evidence_free_plan),
        .payload = .{ .evidence_free_plan = .{
            .plan_index = 7,
            .raw_var = 8,
        } },
    };
    try std.testing.expect(authority.hasCanonicalTags(true));
    authority.payload.evidence_free_plan.plan_index = none;
    try std.testing.expect(!authority.hasCanonicalTags(true));

    authority.kind = std.math.maxInt(u32) - 1;
    try std.testing.expect(!authority.hasCanonicalTags(true));
}

test "expected consumption endpoints are canonical for anchored and producer roots" {
    const none = ExpectedConsumptionPlan.none;
    var plan = ExpectedConsumptionPlan{
        .owner_node = 1,
        .site_node = 2,
        .role = @intFromEnum(ExpectedConsumptionPlan.Role.aggregate_owner),
        .slot = 0,
        .outcome = @intFromEnum(ExpectedConsumptionPlan.Outcome.anchored),
        .reason = none,
        .raw_consumer_var = 3,
        .parent_authority = .{
            .kind = @intFromEnum(ExpectedMarkerAuthority.Kind.copy_occurrence),
            .payload = .{ .copy_occurrence = .{
                .copy_step = 4,
                .occurrence_offset = 0,
                .side = @intFromEnum(WhereMarkerCopyOccurrenceSide.source),
            } },
        },
        .produced_copy_step = 5,
        .produced_occurrence_offset = 0,
        .produced_side = @intFromEnum(WhereMarkerCopyOccurrenceSide.destination),
        .call_root_plan_index = none,
        .failure_owner = CauseOwner.inactive(),
        .failure_cause_plan_index = none,
    };
    try std.testing.expect(plan.hasLegalTags());

    plan.source_retirement_index_plus_one = 1;
    try std.testing.expect(!plan.hasLegalTags());
    plan.source_retirement_index_plus_one = 0;

    plan.parent_authority.payload.copy_occurrence.occurrence_offset = none;
    try std.testing.expect(!plan.hasLegalTags());
    plan.parent_authority.payload.copy_occurrence.occurrence_offset = 0;
    plan.produced_side = none;
    try std.testing.expect(!plan.hasLegalTags());
    plan.produced_side = @intFromEnum(WhereMarkerCopyOccurrenceSide.destination);

    plan.parent_authority.payload.copy_occurrence.side = std.math.maxInt(u32) - 1;
    try std.testing.expect(!plan.hasLegalTags());
    plan.parent_authority.payload.copy_occurrence.side = @intFromEnum(WhereMarkerCopyOccurrenceSide.source);
    plan.produced_side = @intFromEnum(WhereMarkerCopyOccurrenceSide.source);
    try std.testing.expect(!plan.hasLegalTags());
    plan.produced_side = @intFromEnum(WhereMarkerCopyOccurrenceSide.destination);

    plan.produced_copy_step = plan.parent_authority.payload.copy_occurrence.copy_step;
    try std.testing.expect(!plan.hasLegalTags());
    plan.produced_copy_step = 5;

    plan.parent_authority = .{
        .kind = @intFromEnum(ExpectedMarkerAuthority.Kind.producer_root_plan),
        .payload = .{ .producer_root_plan = .{
            .plan_index = 2,
            .raw_var = 7,
        } },
    };
    try std.testing.expect(plan.hasLegalTags());
    plan.parent_authority.payload.producer_root_plan.reserved_0 = 1;
    try std.testing.expect(!plan.hasLegalTags());
    plan.parent_authority.payload.producer_root_plan.reserved_0 = 0;

    plan.parent_authority = .{
        .kind = @intFromEnum(ExpectedMarkerAuthority.Kind.relation_plan),
        .payload = .{ .relation_plan = .{
            .plan_index = 2,
            .raw_var = 7,
        } },
    };
    try std.testing.expect(plan.hasLegalTags());
    plan.parent_authority.payload.relation_plan.raw_var = none;
    try std.testing.expect(!plan.hasLegalTags());
    plan.parent_authority.payload.relation_plan.raw_var = 7;

    plan.parent_authority = .{
        .kind = @intFromEnum(ExpectedMarkerAuthority.Kind.evidence_free_plan),
        .payload = .{ .evidence_free_plan = .{
            .plan_index = 2,
            .raw_var = 7,
        } },
    };
    try std.testing.expect(plan.hasLegalTags());
    plan.parent_authority.payload.evidence_free_plan.reserved_0 = 1;
    try std.testing.expect(!plan.hasLegalTags());
    plan.parent_authority.payload.evidence_free_plan.reserved_0 = 0;

    plan.call_root_plan_index = 0;
    try std.testing.expect(!plan.hasLegalTags());
    plan.outcome = @intFromEnum(ExpectedConsumptionPlan.Outcome.producer_root);
    try std.testing.expect(!plan.hasLegalTags());
    plan.parent_authority = ExpectedMarkerAuthority.inactive();
    try std.testing.expect(!plan.hasLegalTags());
    plan.produced_copy_step = none;
    plan.produced_occurrence_offset = none;
    plan.produced_side = none;
    plan.role = @intFromEnum(ExpectedConsumptionPlan.Role.call_argument);
    try std.testing.expect(plan.hasLegalTags());

    plan.parent_authority.kind = @intFromEnum(ExpectedMarkerAuthority.Kind.copy_occurrence);
    try std.testing.expect(!plan.hasLegalTags());
    plan.parent_authority = ExpectedMarkerAuthority.inactive();
    plan.produced_copy_step = 5;
    try std.testing.expect(!plan.hasLegalTags());
    plan.produced_copy_step = none;

    plan.outcome = @intFromEnum(ExpectedConsumptionPlan.Outcome.anchored);
    plan.role = @intFromEnum(ExpectedConsumptionPlan.Role.call_root);
    plan.call_root_plan_index = none;
    plan.parent_authority = .{
        .kind = @intFromEnum(ExpectedMarkerAuthority.Kind.copy_occurrence),
        .payload = .{ .copy_occurrence = .{
            .copy_step = 4,
            .occurrence_offset = 0,
            .side = @intFromEnum(WhereMarkerCopyOccurrenceSide.destination),
        } },
    };
    plan.produced_copy_step = 5;
    plan.produced_occurrence_offset = 0;
    plan.produced_side = @intFromEnum(WhereMarkerCopyOccurrenceSide.destination);
    try std.testing.expect(!plan.hasLegalTags());

    plan.role = @intFromEnum(ExpectedConsumptionPlan.Role.branch_seed);
    try std.testing.expect(plan.hasLegalTags());
    plan.role = @intFromEnum(ExpectedConsumptionPlan.Role.branch_contribution);
    try std.testing.expect(!plan.hasLegalTags());
    plan.role = @intFromEnum(ExpectedConsumptionPlan.Role.branch_final);
    try std.testing.expect(!plan.hasLegalTags());

    plan.outcome = @intFromEnum(ExpectedConsumptionPlan.Outcome.reserved);
    plan.role = @intFromEnum(ExpectedConsumptionPlan.Role.aggregate_owner);
    try std.testing.expect(!plan.hasLegalTags());
}

fn expectExpectedPlanLegality(
    expected: bool,
    plan: ExpectedConsumptionPlan,
) !void {
    try std.testing.expectEqual(expected, plan.hasLegalTags());
}

test "evidence-free expected parent is exact through aggregate child rejection" {
    const none = ExpectedConsumptionPlan.none;
    var plan = ExpectedConsumptionPlan{
        .owner_node = 1,
        .site_node = 2,
        .role = @intFromEnum(ExpectedConsumptionPlan.Role.list_element),
        .slot = 3,
        .outcome = @intFromEnum(ExpectedConsumptionPlan.Outcome.evidence_free),
        .reason = @intFromEnum(ExpectedConsumptionPlan.Reason.parent_evidence_free),
        .raw_consumer_var = 4,
        .parent_authority = .{
            .kind = @intFromEnum(ExpectedMarkerAuthority.Kind.evidence_free_plan),
            .payload = .{ .evidence_free_plan = .{
                .plan_index = 5,
                .raw_var = 4,
            } },
        },
        .produced_copy_step = none,
        .produced_occurrence_offset = none,
        .produced_side = none,
        .call_root_plan_index = none,
        .failure_owner = CauseOwner.inactive(),
        .failure_cause_plan_index = none,
    };
    try expectExpectedPlanLegality(true, plan);

    // These arms have the same payload shape. The outcome tag, not incidental
    // bytes, selects evidence-free authority.
    plan.parent_authority.kind = @intFromEnum(ExpectedMarkerAuthority.Kind.producer_root_plan);
    try expectExpectedPlanLegality(false, plan);
    plan.parent_authority.kind = @intFromEnum(ExpectedMarkerAuthority.Kind.evidence_free_plan);
    plan.parent_authority.payload.evidence_free_plan.reserved_0 = 1;
    try expectExpectedPlanLegality(false, plan);
    plan.parent_authority.payload.evidence_free_plan.reserved_0 = 0;

    const successful_parent = plan.parent_authority;
    plan.outcome = @intFromEnum(ExpectedConsumptionPlan.Outcome.checked_error);
    plan.reason = @intFromEnum(ExpectedConsumptionPlan.Reason.aggregate_child_relation_rejected);
    plan.failure_owner = CauseOwner.expectedFailure(0);
    try expectExpectedPlanLegality(true, plan);
    try std.testing.expectEqualSlices(
        u8,
        std.mem.asBytes(&successful_parent),
        std.mem.asBytes(&plan.parent_authority),
    );

    plan.failure_owner = CauseOwner.inactive();
    try expectExpectedPlanLegality(false, plan);
    plan.failure_owner = CauseOwner.expectedConsumerRetirement(0);
    try expectExpectedPlanLegality(false, plan);
    plan.failure_owner = CauseOwner.cirDiagnostic(0);
    try expectExpectedPlanLegality(false, plan);
    plan.failure_owner = CauseOwner.providerWhereAliasCheckedError(0, 0);
    try expectExpectedPlanLegality(false, plan);
    plan.failure_owner = CauseOwner.expectedFailure(0);
    plan.failure_cause_plan_index = 0;
    try expectExpectedPlanLegality(false, plan);
    plan.failure_cause_plan_index = none;

    plan.parent_authority = ExpectedMarkerAuthority.inactive();
    try expectExpectedPlanLegality(false, plan);
    plan.parent_authority = successful_parent;
    plan.reason = @intFromEnum(ExpectedConsumptionPlan.Reason.parent_evidence_free);
    try expectExpectedPlanLegality(false, plan);

    plan.outcome = @intFromEnum(ExpectedConsumptionPlan.Outcome.evidence_free);
    plan.failure_owner = CauseOwner.inactive();
    plan.reason = @intFromEnum(ExpectedConsumptionPlan.Reason.parent_evidence_free);
    plan.parent_authority = successful_parent;
    plan.role = @intFromEnum(ExpectedConsumptionPlan.Role.lambda_return);
    try expectExpectedPlanLegality(true, plan);
    plan.role = @intFromEnum(ExpectedConsumptionPlan.Role.branch_seed);
    try expectExpectedPlanLegality(true, plan);
    plan.role = @intFromEnum(ExpectedConsumptionPlan.Role.branch_contribution);
    try expectExpectedPlanLegality(false, plan);
    plan.role = @intFromEnum(ExpectedConsumptionPlan.Role.branch_final);
    try expectExpectedPlanLegality(false, plan);
}

test "branch expected plans preserve predecessor authority and typed failure ownership" {
    const none = ExpectedConsumptionPlan.none;
    var plan = ExpectedConsumptionPlan{
        .owner_node = 1,
        .site_node = 2,
        .role = @intFromEnum(ExpectedConsumptionPlan.Role.branch_contribution),
        .slot = 3,
        .outcome = @intFromEnum(ExpectedConsumptionPlan.Outcome.related),
        .reason = none,
        .raw_consumer_var = 4,
        .parent_authority = .{
            .kind = @intFromEnum(ExpectedMarkerAuthority.Kind.copy_occurrence),
            .payload = .{ .copy_occurrence = .{
                .copy_step = 5,
                .occurrence_offset = 0,
                .side = @intFromEnum(WhereMarkerCopyOccurrenceSide.destination),
            } },
        },
        .produced_copy_step = none,
        .produced_occurrence_offset = none,
        .produced_side = none,
        .call_root_plan_index = none,
        .failure_owner = CauseOwner.inactive(),
        .failure_cause_plan_index = none,
    };
    try expectExpectedPlanLegality(true, plan);
    const predecessor = plan.parent_authority;

    plan.role = @intFromEnum(ExpectedConsumptionPlan.Role.branch_seed);
    try expectExpectedPlanLegality(false, plan);
    plan.role = @intFromEnum(ExpectedConsumptionPlan.Role.branch_final);
    try expectExpectedPlanLegality(true, plan);
    plan.role = @intFromEnum(ExpectedConsumptionPlan.Role.branch_contribution);

    plan.parent_authority = .{
        .kind = @intFromEnum(ExpectedMarkerAuthority.Kind.relation_plan),
        .payload = .{ .relation_plan = .{
            .plan_index = 4,
            .raw_var = 6,
        } },
    };
    try expectExpectedPlanLegality(true, plan);
    plan.parent_authority.payload.relation_plan.reserved_0 = 1;
    try expectExpectedPlanLegality(false, plan);
    plan.parent_authority = predecessor;

    plan.parent_authority = ExpectedMarkerAuthority.inactive();
    try expectExpectedPlanLegality(false, plan);
    plan.parent_authority = predecessor;
    plan.produced_copy_step = 6;
    plan.produced_occurrence_offset = 0;
    plan.produced_side = @intFromEnum(WhereMarkerCopyOccurrenceSide.destination);
    try expectExpectedPlanLegality(false, plan);
    plan.produced_copy_step = none;
    plan.produced_occurrence_offset = none;
    plan.produced_side = none;
    plan.failure_owner = CauseOwner.expectedConsumerRetirement(0);
    try expectExpectedPlanLegality(false, plan);
    plan.failure_owner = CauseOwner.inactive();
    plan.reason = @intFromEnum(ExpectedConsumptionPlan.Reason.branch_body_already_expected);
    try expectExpectedPlanLegality(false, plan);

    plan.outcome = @intFromEnum(ExpectedConsumptionPlan.Outcome.retained);
    try expectExpectedPlanLegality(true, plan);

    plan.parent_authority = ExpectedMarkerAuthority.inactive();
    try expectExpectedPlanLegality(false, plan);
    plan.parent_authority = predecessor;
    plan.produced_copy_step = 6;
    plan.produced_occurrence_offset = 0;
    plan.produced_side = @intFromEnum(WhereMarkerCopyOccurrenceSide.destination);
    try expectExpectedPlanLegality(false, plan);
    plan.produced_copy_step = none;
    plan.produced_occurrence_offset = none;
    plan.produced_side = none;
    plan.failure_owner = CauseOwner.expectedConsumerRetirement(0);
    try expectExpectedPlanLegality(false, plan);
    plan.failure_owner = CauseOwner.inactive();

    plan.outcome = @intFromEnum(ExpectedConsumptionPlan.Outcome.checked_error);
    plan.reason = @intFromEnum(ExpectedConsumptionPlan.Reason.branch_body_error_short_circuit);
    plan.failure_owner = CauseOwner.expectedFailure(0);
    try expectExpectedPlanLegality(true, plan);
    plan.failure_owner = CauseOwner.expectedConsumerRetirement(0);
    try expectExpectedPlanLegality(true, plan);
    plan.failure_owner = CauseOwner.cirDiagnostic(0);
    try expectExpectedPlanLegality(true, plan);
    plan.failure_owner = CauseOwner.providerWhereAliasCheckedError(0, 0);
    try expectExpectedPlanLegality(true, plan);
    plan.failure_cause_plan_index = 0;
    try expectExpectedPlanLegality(false, plan);
    plan.failure_cause_plan_index = none;
    plan.parent_authority = ExpectedMarkerAuthority.inactive();
    try expectExpectedPlanLegality(false, plan);
    plan.parent_authority = predecessor;

    plan.reason = @intFromEnum(ExpectedConsumptionPlan.Reason.branch_expected_compatibility_rejected);
    plan.failure_owner = CauseOwner.expectedConsumerRetirement(0);
    try expectExpectedPlanLegality(true, plan);
    try std.testing.expectEqual(
        ExpectedConsumptionPlan.OwnerRelation.direct_source_retirement,
        ExpectedConsumptionPlan.ownerRelationForReason(.branch_expected_compatibility_rejected),
    );
    plan.failure_owner = CauseOwner.expectedFailure(0);
    try expectExpectedPlanLegality(false, plan);
    plan.failure_owner = CauseOwner.providerWhereAliasCheckedError(0, 0);
    try expectExpectedPlanLegality(false, plan);

    plan.reason = @intFromEnum(ExpectedConsumptionPlan.Reason.branch_accumulator_fold_rejected);
    plan.failure_owner = CauseOwner.cirDiagnostic(0);
    try expectExpectedPlanLegality(true, plan);
    try std.testing.expectEqual(
        ExpectedConsumptionPlan.OwnerRelation.direct_source_retirement,
        ExpectedConsumptionPlan.ownerRelationForReason(.branch_accumulator_fold_rejected),
    );

    inline for (.{
        ExpectedConsumptionPlan.Reason.branch_retired_after_failure,
        ExpectedConsumptionPlan.Reason.branch_retired_after_ambiguity_verdict,
    }) |reason| {
        plan.reason = @intFromEnum(reason);
        plan.failure_owner = CauseOwner.expectedConsumerRetirement(0);
        plan.parent_authority = ExpectedMarkerAuthority.inactive();
        inline for (.{
            ExpectedConsumptionPlan.Role.branch_seed,
            ExpectedConsumptionPlan.Role.branch_contribution,
            ExpectedConsumptionPlan.Role.branch_final,
        }) |role| {
            plan.role = @intFromEnum(role);
            try expectExpectedPlanLegality(true, plan);
        }
        plan.failure_owner = CauseOwner.cirDiagnostic(0);
        try expectExpectedPlanLegality(false, plan);
        plan.failure_owner = CauseOwner.expectedConsumerRetirement(0);
        plan.role = @intFromEnum(ExpectedConsumptionPlan.Role.aggregate_owner);
        try expectExpectedPlanLegality(false, plan);
    }
}

test "call expected plans distinguish cardinality root from argument producer roots" {
    const none = ExpectedConsumptionPlan.none;
    var root = ExpectedConsumptionPlan{
        .owner_node = 1,
        .site_node = 2,
        .role = @intFromEnum(ExpectedConsumptionPlan.Role.call_root),
        .slot = 0,
        .outcome = @intFromEnum(ExpectedConsumptionPlan.Outcome.not_projected),
        .reason = @intFromEnum(ExpectedConsumptionPlan.Reason.call_shape_ready),
        .raw_consumer_var = 3,
        .parent_authority = ExpectedMarkerAuthority.inactive(),
        .produced_copy_step = none,
        .produced_occurrence_offset = none,
        .produced_side = none,
        .call_root_plan_index = none,
        .failure_owner = CauseOwner.inactive(),
        .failure_cause_plan_index = none,
    };
    try std.testing.expect(root.hasLegalTags());
    root.role = @intFromEnum(ExpectedConsumptionPlan.Role.call_argument);
    try std.testing.expect(!root.hasLegalTags());

    var argument = root;
    argument.outcome = @intFromEnum(ExpectedConsumptionPlan.Outcome.producer_root);
    argument.reason = none;
    argument.slot = 1;
    argument.call_root_plan_index = 0;
    try std.testing.expect(argument.hasLegalTags());
    argument.call_root_plan_index = none;
    try std.testing.expect(!argument.hasLegalTags());
    argument.call_root_plan_index = 0;
    argument.role = @intFromEnum(ExpectedConsumptionPlan.Role.call_root);
    try std.testing.expect(!argument.hasLegalTags());
}

test "external lookup tokens use closed ordering and canonical minimum" {
    var env = try Self.init(std.testing.allocator, "");
    defer env.deinit();

    const import_0 = try env.imports.getOrPut(env.gpa, &env.common, "External0");
    const import_1 = try env.imports.getOrPut(env.gpa, &env.common, "External1");
    const import_2 = try env.imports.getOrPut(env.gpa, &env.common, "External2");

    const key = ExternalLookupKey{ .resolved_module_idx = 4, .target_node = 9 };
    const none = ExternalLookupToken.none;
    try env.external_lookup_tokens.items.ensureUnusedCapacity(env.gpa, 3);
    env.appendExternalLookupTokenAssumeCapacity(.{
        .import_idx = import_2,
        .target_node = key.target_node,
        .site_kind = .external_where_alias_parameter,
    }, 8, 3);
    env.appendExternalLookupTokenAssumeCapacity(.{
        .import_idx = import_1,
        .target_node = key.target_node,
        .site_kind = .external_type_annotation_lookup,
    }, 4, none);
    env.appendExternalLookupTokenAssumeCapacity(.{
        .import_idx = import_0,
        .target_node = key.target_node,
        .site_kind = .external_numeric_suffix,
    }, 2, none);
    try std.testing.expect(!env.externalLookupTokensAreCanonical());
    for (env.external_lookup_tokens.items.items) |token| {
        try std.testing.expectEqual(ExternalLookupKey.unresolved, token.key.resolved_module_idx);
    }
    env.imports.setResolvedModule(import_0, 4);
    env.imports.setResolvedModule(import_1, 4);
    env.imports.setResolvedModule(import_2, 4);
    try env.ensureExternalLookupTokensSealedAfterImportResolution();
    try std.testing.expect(env.externalLookupTokensAreCanonical());
    for (env.external_lookup_tokens.items.items) |token| {
        try std.testing.expectEqual(@as(u32, 4), token.key.resolved_module_idx);
    }
    const sealed = try std.testing.allocator.dupe(
        ExternalLookupToken,
        env.external_lookup_tokens.items.items,
    );
    defer std.testing.allocator.free(sealed);
    try env.ensureExternalLookupTokensSealedAfterImportResolution();
    try std.testing.expectEqualSlices(
        ExternalLookupToken,
        sealed,
        env.external_lookup_tokens.items.items,
    );
    const minimum = env.minimumExternalLookupToken(key) orelse unreachable;
    try std.testing.expectEqual(@as(u32, 0), @intFromEnum(minimum));
    try std.testing.expectEqual(@as(u32, 4), env.external_lookup_tokens.items.items[@intFromEnum(minimum)].origin_node);
    try std.testing.expect(env.minimumExternalLookupToken(.{ .resolved_module_idx = 4, .target_node = 10 }) == null);

    const seed_key = ExternalLookupKey{ .resolved_module_idx = 4, .target_node = 9 };
    const first_seed = try env.reserveExternalCacheSeed(seed_key, 0, 4);
    env.completeExternalCacheSeed(first_seed, 11);
    try std.testing.expect(env.externalCacheSeedsAreCanonical());
    try std.testing.expectError(
        error.ExternalCacheSeedAlreadyPublished,
        env.ensureExternalLookupTokensSealedAfterImportResolution(),
    );
    const invalid_seed = try env.reserveExternalCacheSeed(
        .{ .resolved_module_idx = 3, .target_node = 2 },
        1,
        1,
    );
    env.completeExternalCacheSeed(invalid_seed, 10);
    try std.testing.expect(!env.externalCacheSeedsAreCanonical());
}

test "external lookup token seal rejects invalid producer state without mutation" {
    var env = try Self.init(std.testing.allocator, "");
    defer env.deinit();

    const import_idx = try env.imports.getOrPut(env.gpa, &env.common, "External");
    try env.external_lookup_tokens.items.ensureUnusedCapacity(env.gpa, 2);
    env.appendExternalLookupTokenAssumeCapacity(.{
        .import_idx = import_idx,
        .target_node = 9,
        .site_kind = .external_lookup_expr,
    }, 4, ExternalLookupToken.none);

    const unresolved = try std.testing.allocator.dupe(
        ExternalLookupToken,
        env.external_lookup_tokens.items.items,
    );
    defer std.testing.allocator.free(unresolved);
    try std.testing.expectError(
        error.UnresolvedLiveImport,
        env.ensureExternalLookupTokensSealedAfterImportResolution(),
    );
    try std.testing.expectEqualSlices(
        ExternalLookupToken,
        unresolved,
        env.external_lookup_tokens.items.items,
    );

    env.imports.setResolvedModule(import_idx, 4);
    env.appendExternalLookupTokenAssumeCapacity(.{
        .import_idx = import_idx,
        .target_node = 9,
        .site_kind = .external_lookup_expr,
    }, 4, ExternalLookupToken.none);
    const duplicated = try std.testing.allocator.dupe(
        ExternalLookupToken,
        env.external_lookup_tokens.items.items,
    );
    defer std.testing.allocator.free(duplicated);
    try std.testing.expectError(
        error.DuplicateExternalLookupProducer,
        env.ensureExternalLookupTokensSealedAfterImportResolution(),
    );
    try std.testing.expectEqualSlices(
        ExternalLookupToken,
        duplicated,
        env.external_lookup_tokens.items.items,
    );

    env.external_lookup_tokens.items.items.len = 1;
    env.external_lookup_tokens.items.items[0].reserved_0 = 1;
    const invalid = try std.testing.allocator.dupe(
        ExternalLookupToken,
        env.external_lookup_tokens.items.items,
    );
    defer std.testing.allocator.free(invalid);
    try std.testing.expectError(
        error.InvalidExternalLookupToken,
        env.ensureExternalLookupTokensSealedAfterImportResolution(),
    );
    try std.testing.expectEqualSlices(
        ExternalLookupToken,
        invalid,
        env.external_lookup_tokens.items.items,
    );
}

test "external lookup addExpr and token publication are an exhaustive OOM no-op" {
    var induced_failures: usize = 0;
    var reached_success = false;
    for (0..8) |fail_index| {
        var env = try Self.init(std.testing.allocator, "");
        defer env.deinit();

        const nodes_len = env.store.nodes.len();
        const regions_before = try std.testing.allocator.dupe(Region, env.store.regions.items.items);
        defer std.testing.allocator.free(regions_before);
        const tokens_before = try std.testing.allocator.dupe(ExternalLookupToken, env.external_lookup_tokens.items.items);
        defer std.testing.allocator.free(tokens_before);
        var failing = std.testing.FailingAllocator.init(std.testing.allocator, .{ .fail_index = fail_index });
        const failing_gpa = failing.allocator();
        const original_env_gpa = env.gpa;
        const original_store_gpa = env.store.gpa;
        env.gpa = failing_gpa;
        env.store.gpa = failing_gpa;
        const result = env.addExpr(.{ .e_lookup_external = .{
            .module_idx = @enumFromInt(0),
            .target_node_idx = 17,
            .ident_idx = Ident.Idx.NONE,
            .region = Region.zero(),
        } }, Region.zero());
        env.gpa = original_env_gpa;
        env.store.gpa = original_store_gpa;

        if (result) |expr_idx| {
            try std.testing.expect(!failing.has_induced_failure);
            try std.testing.expectEqual(nodes_len + 1, env.store.nodes.len());
            try std.testing.expectEqual(regions_before.len + 1, env.store.regions.items.items.len);
            try std.testing.expectEqual(tokens_before.len + 1, env.external_lookup_tokens.items.items.len);
            const token = env.external_lookup_tokens.items.items[tokens_before.len];
            try std.testing.expectEqual(@intFromEnum(expr_idx), token.origin_node);
            try std.testing.expectEqual(@as(u32, 17), token.key.target_node);
            try std.testing.expectEqual(ExternalLookupKey.unresolved, token.key.resolved_module_idx);
            reached_success = true;
        } else |err| {
            try std.testing.expectEqual(error.OutOfMemory, err);
            try std.testing.expect(failing.has_induced_failure);
            induced_failures += 1;
            try std.testing.expectEqual(nodes_len, env.store.nodes.len());
            try std.testing.expectEqualSlices(Region, regions_before, env.store.regions.items.items);
            try std.testing.expectEqualSlices(ExternalLookupToken, tokens_before, env.external_lookup_tokens.items.items);
        }
    }
    try std.testing.expect(reached_success);
    try std.testing.expect(induced_failures > 0);
}

test "external where alias lookup group publication is an exhaustive OOM no-op" {
    var induced_failures: usize = 0;
    var reached_success = false;
    const parameters = [_]CIR.TypeAnno.Idx{
        @enumFromInt(21),
        @enumFromInt(22),
        @enumFromInt(23),
    };
    for (0..8) |fail_index| {
        var env = try Self.init(std.testing.allocator, "");
        defer env.deinit();
        const before = try std.testing.allocator.dupe(
            ExternalLookupToken,
            env.external_lookup_tokens.items.items,
        );
        defer std.testing.allocator.free(before);

        var failing = std.testing.FailingAllocator.init(
            std.testing.allocator,
            .{ .fail_index = fail_index },
        );
        const saved_gpa = env.gpa;
        env.gpa = failing.allocator();
        const result = env.appendExternalWhereAliasLookupGroup(
            @enumFromInt(17),
            @enumFromInt(3),
            19,
            &parameters,
        );
        env.gpa = saved_gpa;

        if (result) |_| {
            try std.testing.expect(!failing.has_induced_failure);
            const rows = env.external_lookup_tokens.items.items;
            try std.testing.expectEqual(@as(usize, 4), rows.len);
            try std.testing.expectEqual(
                ExternalLookupSiteKind.external_where_alias_receiver,
                rows[0].decodedSiteKind().?,
            );
            for (rows[1..], parameters, 0..) |row, parameter, ordinal| {
                try std.testing.expectEqual(
                    ExternalLookupSiteKind.external_where_alias_parameter,
                    row.decodedSiteKind().?,
                );
                try std.testing.expectEqual(@as(u32, @intCast(ordinal)), row.parameter_ordinal);
                try std.testing.expectEqual(@intFromEnum(parameter), row.key.target_node);
            }
            reached_success = true;
            break;
        } else |err| {
            try std.testing.expectEqual(error.OutOfMemory, err);
            try std.testing.expect(failing.has_induced_failure);
            induced_failures += 1;
            try std.testing.expectEqualSlices(
                u8,
                std.mem.sliceAsBytes(before),
                std.mem.sliceAsBytes(env.external_lookup_tokens.items.items),
            );
        }
    }
    try std.testing.expect(reached_success);
    try std.testing.expect(induced_failures > 0);
}

test "external cache seed reservation is completed in place" {
    var env = try Self.init(std.testing.allocator, "");
    defer env.deinit();
    const key = ExternalLookupKey{ .resolved_module_idx = 3, .target_node = 7 };
    const seed = try env.reserveExternalCacheSeed(key, 5, 11);
    try std.testing.expectEqual(@as(usize, 1), env.external_cache_seeds.items.items.len);
    try std.testing.expect(env.external_cache_seeds.items.items[@intFromEnum(seed)].hasReservedTags());
    env.completeExternalCacheSeed(seed, 13);
    try std.testing.expect(env.external_cache_seeds.items.items[@intFromEnum(seed)].hasCanonicalTags());
    try std.testing.expectEqual(@as(u32, 13), env.external_cache_seeds.items.items[@intFromEnum(seed)].support_step);
}

test "call node and slot-token publication is an exhaustive OOM no-op" {
    var induced_failures: usize = 0;
    var reached_success = false;
    for (0..8) |fail_index| {
        var env = try Self.init(std.testing.allocator, "");
        defer env.deinit();

        const synthetic_argument: CIR.Expr.Idx = @enumFromInt(0);
        const args = try env.store.appendExprSpan(&.{synthetic_argument});
        const nodes_len = env.store.nodes.len();
        const regions_before = try std.testing.allocator.dupe(
            Region,
            env.store.regions.items.items,
        );
        defer std.testing.allocator.free(regions_before);
        const spans_before = try std.testing.allocator.dupe(
            NodeStore.Span2,
            env.store.span2_data.items.items,
        );
        defer std.testing.allocator.free(spans_before);
        const tokens_before = try std.testing.allocator.dupe(
            ExpectedCallSlotToken,
            env.expected_call_slot_tokens.items.items,
        );
        defer std.testing.allocator.free(tokens_before);
        const nodes_before = try std.testing.allocator.alloc(Node, nodes_len);
        defer std.testing.allocator.free(nodes_before);
        for (nodes_before, 0..) |*node, index| {
            node.* = env.store.nodes.get(@enumFromInt(index));
        }

        var failing = std.testing.FailingAllocator.init(std.testing.allocator, .{
            .fail_index = fail_index,
        });
        const failing_gpa = failing.allocator();
        const original_env_gpa = env.gpa;
        const original_store_gpa = env.store.gpa;
        env.gpa = failing_gpa;
        env.store.gpa = failing_gpa;
        const result = env.addExpr(.{ .e_call = .{
            .func = @enumFromInt(0),
            .args = args,
            .called_via = .apply,
        } }, Region.zero());
        env.gpa = original_env_gpa;
        env.store.gpa = original_store_gpa;

        if (result) |call_idx| {
            try std.testing.expect(!failing.has_induced_failure);
            try std.testing.expectEqual(nodes_len + 1, env.store.nodes.len());
            try std.testing.expectEqual(regions_before.len + 1, env.store.regions.items.items.len);
            try std.testing.expectEqual(spans_before.len + 1, env.store.span2_data.items.items.len);
            try std.testing.expectEqual(tokens_before.len + 2, env.expected_call_slot_tokens.items.items.len);
            const root = env.expected_call_slot_tokens.items.items[tokens_before.len];
            const argument = env.expected_call_slot_tokens.items.items[tokens_before.len + 1];
            try std.testing.expectEqual(@intFromEnum(call_idx), root.owner_node);
            try std.testing.expectEqual(ExpectedCallSlotToken.Role.root, root.decodedRole().?);
            try std.testing.expectEqual(@as(u32, 1), root.cardinality);
            try std.testing.expectEqual(@intFromEnum(call_idx), argument.owner_node);
            try std.testing.expectEqual(ExpectedCallSlotToken.Role.argument, argument.decodedRole().?);
            try std.testing.expectEqual(@as(u32, 0), argument.slot);
            reached_success = true;
            break;
        } else |err| {
            try std.testing.expectEqual(error.OutOfMemory, err);
            try std.testing.expect(failing.has_induced_failure);
            induced_failures += 1;
            try std.testing.expectEqual(nodes_len, env.store.nodes.len());
            for (nodes_before, 0..) |node, index| {
                try std.testing.expectEqualSlices(
                    u8,
                    std.mem.asBytes(&node),
                    std.mem.asBytes(&env.store.nodes.get(@enumFromInt(index))),
                );
            }
            try std.testing.expectEqualSlices(
                Region,
                regions_before,
                env.store.regions.items.items,
            );
            try std.testing.expectEqualSlices(
                NodeStore.Span2,
                spans_before,
                env.store.span2_data.items.items,
            );
            try std.testing.expectEqualSlices(
                ExpectedCallSlotToken,
                tokens_before,
                env.expected_call_slot_tokens.items.items,
            );
        }
    }
    try std.testing.expect(reached_success);
    try std.testing.expectEqual(@as(usize, 4), induced_failures);
}

test "record builder calls publish the exact closed called-via token mapping" {
    var env = try Self.init(std.testing.allocator, "");
    defer env.deinit();

    const args = try env.store.appendExprSpan(&.{@as(CIR.Expr.Idx, @enumFromInt(0))});
    const call_idx = try env.addExpr(.{ .e_call = .{
        .func = @enumFromInt(0),
        .args = args,
        .called_via = .record_builder,
    } }, Region.zero());
    const rows = env.expected_call_slot_tokens.items.items;
    try std.testing.expectEqual(@as(usize, 2), rows.len);
    try std.testing.expectEqual(@intFromEnum(call_idx), rows[0].owner_node);
    try std.testing.expectEqual(ExpectedCallSlotToken.Role.root, rows[0].decodedRole().?);
    try std.testing.expectEqual(ExpectedCallKind.record_builder, rows[0].decodedCalledVia().?);
    try std.testing.expectEqual(@intFromEnum(call_idx), rows[1].owner_node);
    try std.testing.expectEqual(ExpectedCallSlotToken.Role.argument, rows[1].decodedRole().?);
    try std.testing.expectEqual(ExpectedCallKind.record_builder, rows[1].decodedCalledVia().?);
}

test "call slot and formal ledgers serialize and relocate every closed origin" {
    const none = ExpectedCallFormal.none;
    const token_rows = [_]ExpectedCallSlotToken{
        .{ .owner_node = 10, .site_node = 20, .slot = none, .cardinality = 1, .called_via = @intFromEnum(ExpectedCallKind.apply), .role = @intFromEnum(ExpectedCallSlotToken.Role.root) },
        .{ .owner_node = 10, .site_node = 21, .slot = 0, .cardinality = 1, .called_via = @intFromEnum(ExpectedCallKind.apply), .role = @intFromEnum(ExpectedCallSlotToken.Role.argument) },
        .{ .owner_node = 30, .site_node = 40, .slot = none, .cardinality = 1, .called_via = @intFromEnum(ExpectedCallKind.record_builder), .role = @intFromEnum(ExpectedCallSlotToken.Role.root) },
        .{ .owner_node = 30, .site_node = 42, .slot = 0, .cardinality = 1, .called_via = @intFromEnum(ExpectedCallKind.record_builder), .role = @intFromEnum(ExpectedCallSlotToken.Role.argument) },
        .{ .owner_node = 60, .site_node = 70, .slot = none, .cardinality = 1, .called_via = @intFromEnum(ExpectedCallKind.apply), .role = @intFromEnum(ExpectedCallSlotToken.Role.root) },
        .{ .owner_node = 60, .site_node = 72, .slot = 0, .cardinality = 1, .called_via = @intFromEnum(ExpectedCallKind.apply), .role = @intFromEnum(ExpectedCallSlotToken.Role.argument) },
    };
    const formal_rows = [_]ExpectedCallFormal{
        .{
            .owner_node = 10,
            .call_root_plan_index = 0,
            .argument_plan_index = 1,
            .call_root_token_index = 0,
            .slot_token_index = 1,
            .raw_callee_var = 20,
            .instantiation_source_var = none,
            .raw_callable_var = 20,
            .fresh_shape_var = none,
            .slot = 0,
            .raw_formal_var = 21,
            .immutable_exposure_var = 22,
            .instantiation_copy_step = none,
            .instantiation_root_occurrence = none,
            .instantiation_formal_occurrence = none,
            .fresh_args_start = none,
            .fresh_args_len = 0,
            .fresh_ret_var = none,
            .fresh_effect_deps_start = none,
            .fresh_effect_deps_len = 0,
            .called_via = @intFromEnum(ExpectedCallKind.apply),
            .shape_kind = @intFromEnum(ExpectedCallFormal.ShapeKind.existing_callable),
            .formal_origin = @intFromEnum(ExpectedCallFormal.FormalOrigin.direct_monomorphic_or_alias),
        },
        .{
            .owner_node = 30,
            .call_root_plan_index = 2,
            .argument_plan_index = 3,
            .call_root_token_index = 2,
            .slot_token_index = 3,
            .raw_callee_var = 40,
            .instantiation_source_var = 39,
            .raw_callable_var = 41,
            .fresh_shape_var = none,
            .slot = 0,
            .raw_formal_var = 42,
            .immutable_exposure_var = none,
            .instantiation_copy_step = 50,
            .instantiation_root_occurrence = 51,
            .instantiation_formal_occurrence = 52,
            .fresh_args_start = none,
            .fresh_args_len = 0,
            .fresh_ret_var = none,
            .fresh_effect_deps_start = none,
            .fresh_effect_deps_len = 0,
            .called_via = @intFromEnum(ExpectedCallKind.record_builder),
            .shape_kind = @intFromEnum(ExpectedCallFormal.ShapeKind.existing_callable),
            .formal_origin = @intFromEnum(ExpectedCallFormal.FormalOrigin.explicitly_instantiated),
        },
        .{
            .owner_node = 60,
            .call_root_plan_index = 4,
            .argument_plan_index = 5,
            .call_root_token_index = 4,
            .slot_token_index = 5,
            .raw_callee_var = 70,
            .instantiation_source_var = none,
            .raw_callable_var = 70,
            .fresh_shape_var = 71,
            .slot = 0,
            .raw_formal_var = 72,
            .immutable_exposure_var = none,
            .instantiation_copy_step = none,
            .instantiation_root_occurrence = none,
            .instantiation_formal_occurrence = none,
            .fresh_args_start = 73,
            .fresh_args_len = 1,
            .fresh_ret_var = 74,
            .fresh_effect_deps_start = none,
            .fresh_effect_deps_len = 0,
            .called_via = @intFromEnum(ExpectedCallKind.apply),
            .shape_kind = @intFromEnum(ExpectedCallFormal.ShapeKind.fresh_arity_shape),
            .formal_origin = @intFromEnum(ExpectedCallFormal.FormalOrigin.fresh_arity_shape),
        },
    };
    for (token_rows) |row| try std.testing.expect(row.hasCanonicalTags());
    for (formal_rows) |row| try std.testing.expect(row.hasCanonicalTags());

    var tokens = ExpectedCallSlotToken.SafeList{};
    defer tokens.deinit(std.testing.allocator);
    try tokens.items.appendSlice(std.testing.allocator, &token_rows);
    var formals = ExpectedCallFormal.SafeList{};
    defer formals.deinit(std.testing.allocator);
    try formals.items.appendSlice(std.testing.allocator, &formal_rows);

    const LedgerSerialized = extern struct {
        tokens: ExpectedCallSlotToken.SafeList.Serialized,
        formals: ExpectedCallFormal.SafeList.Serialized,

        comptime {
            collections.serde_validation.assertSerializedRelocatable(@This());
        }
    };
    var writer = CompactWriter.init();
    defer writer.deinit(std.testing.allocator);
    const pending = try writer.appendAlloc(std.testing.allocator, LedgerSerialized);
    try pending.tokens.serialize(&tokens, std.testing.allocator, &writer);
    try pending.formals.serialize(&formals, std.testing.allocator, &writer);

    const buffer = try std.testing.allocator.alignedAlloc(
        u8,
        CompactWriter.SERIALIZATION_ALIGNMENT,
        writer.total_bytes,
    );
    defer std.testing.allocator.free(buffer);
    _ = try writer.writeToBuffer(buffer);
    const serialized: *LedgerSerialized = @ptrCast(@alignCast(buffer.ptr));
    try collections.validateSerializedRelocations(
        LedgerSerialized,
        serialized,
        @intCast(buffer.len),
    );
    const base_addr = @intFromPtr(buffer.ptr);
    const token_view = serialized.tokens.deserializeInto(base_addr);
    const formal_view = serialized.formals.deserializeInto(base_addr);
    try std.testing.expectEqualSlices(ExpectedCallSlotToken, &token_rows, token_view.items.items);
    try std.testing.expectEqualSlices(ExpectedCallFormal, &formal_rows, formal_view.items.items);
    try std.testing.expectEqual(
        base_addr + @as(usize, @intCast(serialized.tokens.offset)),
        @intFromPtr(token_view.items.items.ptr),
    );
    try std.testing.expectEqual(
        base_addr + @as(usize, @intCast(serialized.formals.offset)),
        @intFromPtr(formal_view.items.items.ptr),
    );

    var copied_tokens = try serialized.tokens.deserializeWithCopy(
        base_addr,
        std.testing.allocator,
    );
    defer copied_tokens.deinit(std.testing.allocator);
    var copied_formals = try serialized.formals.deserializeWithCopy(
        base_addr,
        std.testing.allocator,
    );
    defer copied_formals.deinit(std.testing.allocator);
    try std.testing.expectEqualSlices(ExpectedCallSlotToken, &token_rows, copied_tokens.items.items);
    try std.testing.expectEqualSlices(ExpectedCallFormal, &formal_rows, copied_formals.items.items);
    try std.testing.expectEqual(
        ExpectedCallFormal.FormalOrigin.direct_monomorphic_or_alias,
        copied_formals.items.items[0].decodedFormalOrigin().?,
    );
    try std.testing.expectEqual(
        ExpectedCallFormal.FormalOrigin.explicitly_instantiated,
        copied_formals.items.items[1].decodedFormalOrigin().?,
    );
    try std.testing.expectEqual(
        ExpectedCallFormal.FormalOrigin.fresh_arity_shape,
        copied_formals.items.items[2].decodedFormalOrigin().?,
    );

    const saved_token_capacity = serialized.tokens.capacity;
    serialized.tokens.capacity +%= 1;
    try std.testing.expectError(
        error.CorruptArtifact,
        collections.validateSerializedRelocations(
            LedgerSerialized,
            serialized,
            @intCast(buffer.len),
        ),
    );
    serialized.tokens.capacity = saved_token_capacity;
    const saved_formal_offset = serialized.formals.offset;
    serialized.formals.offset = @intCast(buffer.len);
    try std.testing.expectError(
        error.CorruptArtifact,
        collections.validateSerializedRelocations(
            LedgerSerialized,
            serialized,
            @intCast(buffer.len),
        ),
    );
    serialized.formals.offset = saved_formal_offset;
    try collections.validateSerializedRelocations(
        LedgerSerialized,
        serialized,
        @intCast(buffer.len),
    );
}

test "expected marker authorities preserve the fixed copy-origin payload" {
    try std.testing.expectEqual(4 * @sizeOf(u32), @sizeOf(ExpectedMarkerAuthority));
    try std.testing.expectEqual(
        @sizeOf(WhereMarkerReservedOrigin),
        @sizeOf(WhereMarkerAggregateFreshShapeChildOrigin),
    );
    try std.testing.expectEqual(
        @sizeOf(WhereMarkerCopyOrigin),
        @sizeOf(WhereMarkerAggregateFreshShapeChildOrigin),
    );
    try std.testing.expectEqual(
        @sizeOf(WhereMarkerReservedOrigin),
        @sizeOf(WhereMarkerExpectedProjectionOrigin),
    );
    try std.testing.expectEqual(
        @as(usize, 5 * @sizeOf(u32)),
        @offsetOf(WhereMarkerExpectedProjectionOrigin, "expected_plan_index"),
    );
}

test "expected failure subject authority arms and inactive bytes are canonical" {
    const none = SubjectAuthority.none;
    try std.testing.expectEqual(
        @as(usize, 6),
        std.enums.values(SubjectAuthority.Kind).len,
    );
    var authority = SubjectAuthority.inactive();
    try std.testing.expect(authority.hasCanonicalTags(false));
    try std.testing.expect(!authority.hasCanonicalTags(true));
    for (std.mem.asBytes(&authority)) |byte| try std.testing.expectEqual(@as(u8, 0xff), byte);

    authority.payload.direct.producer_node = 0;
    try std.testing.expect(!authority.hasCanonicalTags(false));

    authority = SubjectAuthority.direct(2, .annotation_malformed_type, 0);
    try std.testing.expect(authority.hasCanonicalTags(true));
    try std.testing.expectEqual(
        SubjectAuthority.DirectPhase.annotation_malformed_type,
        authority.decodedDirectPhase().?,
    );
    authority.payload.direct.producer_node = none;
    try std.testing.expect(!authority.hasCanonicalTags(true));
    authority.payload.direct.producer_node = 2;
    authority.payload.direct.phase = none - 1;
    try std.testing.expect(!authority.hasCanonicalTags(true));

    authority = SubjectAuthority.expectedCopyOccurrence(3, 4, .destination);
    try std.testing.expect(authority.hasCanonicalTags(true));
    try std.testing.expectEqual(
        WhereMarkerCopyOccurrenceSide.destination,
        authority.decodedExpectedCopySide().?,
    );
    authority.payload.expected_copy_occurrence.side = none;
    try std.testing.expect(!authority.hasCanonicalTags(true));

    authority = SubjectAuthority.expectedProducerRootPlan(5, 6);
    try std.testing.expect(authority.hasCanonicalTags(true));
    try std.testing.expectEqual(@as(u32, 5), authority.decodedExpectedProducerRootPlan().?.plan_index);
    authority.payload.expected_producer_root_plan.reserved_0 = 1;
    try std.testing.expect(!authority.hasCanonicalTags(true));

    authority = SubjectAuthority.expectedRelationPlan(7, 8);
    try std.testing.expect(authority.hasCanonicalTags(true));
    try std.testing.expectEqual(@as(u32, 7), authority.decodedExpectedRelationPlan().?.plan_index);
    authority.payload.expected_relation_plan.raw_var = none;
    try std.testing.expect(!authority.hasCanonicalTags(true));
    authority.payload.expected_relation_plan.raw_var = 8;
    authority.payload.expected_relation_plan.plan_index = none;
    try std.testing.expect(!authority.hasCanonicalTags(true));
    authority.payload.expected_relation_plan.plan_index = 7;
    authority.payload.expected_relation_plan.reserved_0 = 1;
    try std.testing.expect(!authority.hasCanonicalTags(true));

    authority = SubjectAuthority.expectedEvidenceFreePlan(9, 10);
    try std.testing.expect(authority.hasCanonicalTags(true));
    try std.testing.expectEqual(@as(u32, 10), authority.decodedExpectedEvidenceFreePlan().?.raw_var);
    authority.payload.expected_evidence_free_plan.raw_var = none;
    try std.testing.expect(!authority.hasCanonicalTags(true));

    authority = SubjectAuthority.providerWhereAliasCheckedError(9, 10);
    try std.testing.expect(authority.hasCanonicalTags(true));
    try std.testing.expectEqual(
        @as(u32, 9),
        authority.decodedProviderWhereAliasCheckedError().?.dependency_index,
    );
    authority.payload.provider_where_alias_checked_error.publication_index = none;
    try std.testing.expect(!authority.hasCanonicalTags(true));

    authority.kind = none - 1;
    try std.testing.expect(!authority.hasCanonicalTags(true));
}

test "expected failure cause owner arms and inactive bytes are canonical" {
    const none = CauseOwner.none;
    var cause = CauseOwner.inactive();
    try std.testing.expect(cause.hasCanonicalTags(false));
    try std.testing.expect(!cause.hasCanonicalTags(true));
    for (std.mem.asBytes(&cause)) |byte| try std.testing.expectEqual(@as(u8, 0xff), byte);

    cause.payload.expected_failure.index = 0;
    try std.testing.expect(!cause.hasCanonicalTags(false));

    cause = CauseOwner.expectedFailure(1);
    try std.testing.expect(cause.hasCanonicalTags(true));
    try std.testing.expectEqual(@as(u32, 1), cause.decodedExpectedFailure().?.index);
    cause.payload.expected_failure.reserved_0 = 1;
    try std.testing.expect(!cause.hasCanonicalTags(true));

    cause = CauseOwner.expectedConsumerRetirement(2);
    try std.testing.expect(cause.hasCanonicalTags(true));
    try std.testing.expectEqual(@as(u32, 2), cause.decodedExpectedConsumerRetirement().?.index);
    cause.payload.expected_consumer_retirement.index = none;
    try std.testing.expect(!cause.hasCanonicalTags(true));

    cause = CauseOwner.cirDiagnostic(3);
    try std.testing.expect(cause.hasCanonicalTags(true));
    try std.testing.expectEqual(@as(u32, 3), cause.decodedCirDiagnostic().?.index);

    cause = CauseOwner.providerWhereAliasCheckedError(4, 5);
    try std.testing.expect(cause.hasCanonicalTags(true));
    try std.testing.expectEqual(
        @as(u32, 5),
        cause.decodedProviderWhereAliasCheckedError().?.publication_index,
    );
    cause.payload.provider_where_alias_checked_error.reserved_0 = 1;
    try std.testing.expect(!cause.hasCanonicalTags(true));

    cause.kind = none - 1;
    try std.testing.expect(!cause.hasCanonicalTags(true));
}

test "expected authority inactive encodings reject every changed byte" {
    const marker_pristine = ExpectedMarkerAuthority.inactive();
    for (0..@sizeOf(ExpectedMarkerAuthority)) |byte_index| {
        var marker = marker_pristine;
        std.mem.asBytes(&marker)[byte_index] = 0;
        try std.testing.expect(!marker.hasCanonicalTags(false));
    }

    const subject_pristine = SubjectAuthority.inactive();
    for (0..@sizeOf(SubjectAuthority)) |byte_index| {
        var subject = subject_pristine;
        std.mem.asBytes(&subject)[byte_index] = 0;
        try std.testing.expect(!subject.hasCanonicalTags(false));
    }

    const cause_pristine = CauseOwner.inactive();
    for (0..@sizeOf(CauseOwner)) |byte_index| {
        var cause = cause_pristine;
        std.mem.asBytes(&cause)[byte_index] = 0;
        try std.testing.expect(!cause.hasCanonicalTags(false));
    }
}

test "expected failure legality covers every closed kind and direct phase" {
    const Fixture = struct {
        fn directPhase(kind: ExpectedFailure.Kind) ?SubjectAuthority.DirectPhase {
            return switch (kind) {
                .annotation_malformed_type => .annotation_malformed_type,
                .annotation_malformed_where => .annotation_malformed_where,
                .annotation_invalid_tag_child => .annotation_invalid_tag_child,
                .annotation_where_receiver_not_introduced => .annotation_where_receiver_not_introduced,
                .annotation_where_alias_not_alias => .annotation_where_alias_not_alias,
                .annotation_recursive_where_alias => .annotation_recursive_where_alias,
                .annotation_where_alias_publication_error => .annotation_local_where_alias_checked_error,
                .annotation_where_alias_unresolved => .annotation_where_alias_unresolved,
                .annotation_where_alias_arity => .annotation_where_alias_arity,
                .annotation_where_alias_in_type_position => .annotation_where_alias_in_type_position,
                .annotation_builtin_not_type => .annotation_builtin_not_type,
                .annotation_recursive_type_decl => .annotation_recursive_type_decl,
                .annotation_type_decl_poisoned => .annotation_type_decl_poisoned,
                .annotation_type_formal_poisoned => .annotation_type_formal_poisoned,
                .annotation_type_apply_arity => .annotation_type_apply_arity,
                .annotation_alias_row_rejected => .annotation_alias_row_rejected,
                .annotation_external_type_unresolved => .annotation_external_type_unresolved,
                .annotation_child_failure => .annotation_child_before_copy,
                .annotation_duplicate_where_signature_rejected => .annotation_duplicate_where_relation,
                .direct_binder_lookup_checked_error => .direct_binder_lookup_checked_error,
                .annotated_binding_lookup_checked_error => .annotated_binding_lookup_checked_error,
                .call_operand_checked_error => null,
                .nominal_pattern_external_unresolved => .nominal_pattern_external_unresolved,
                .nominal_pattern_decl_poisoned => .nominal_pattern_decl_poisoned,
                .nominal_pattern_opaque_inaccessible => .nominal_pattern_opaque_inaccessible,
                .nominal_pattern_backing_unavailable => .nominal_pattern_backing_unavailable,
                .nominal_pattern_backing_checked_error,
                .nominal_pattern_backing_relation_rejected,
                .aggregate_child_relation_rejected,
                => null,
            };
        }

        fn ownerKind(kind: ExpectedFailure.Kind) ExpectedFailure.OwnerKind {
            return switch (kind) {
                .nominal_pattern_external_unresolved,
                .nominal_pattern_decl_poisoned,
                .nominal_pattern_opaque_inaccessible,
                .nominal_pattern_backing_unavailable,
                .nominal_pattern_backing_checked_error,
                .nominal_pattern_backing_relation_rejected,
                => .pattern,
                .annotation_malformed_type,
                .annotation_malformed_where,
                .annotation_invalid_tag_child,
                .annotation_where_receiver_not_introduced,
                .annotation_where_alias_not_alias,
                .annotation_recursive_where_alias,
                .annotation_where_alias_publication_error,
                .annotation_where_alias_unresolved,
                .annotation_where_alias_arity,
                .annotation_where_alias_in_type_position,
                .annotation_builtin_not_type,
                .annotation_recursive_type_decl,
                .annotation_type_decl_poisoned,
                .annotation_type_formal_poisoned,
                .annotation_type_apply_arity,
                .annotation_alias_row_rejected,
                .annotation_external_type_unresolved,
                .annotation_child_failure,
                .annotation_duplicate_where_signature_rejected,
                .direct_binder_lookup_checked_error,
                .annotated_binding_lookup_checked_error,
                .call_operand_checked_error,
                .aggregate_child_relation_rejected,
                => .expression,
            };
        }

        fn subject(kind: ExpectedFailure.Kind) SubjectAuthority {
            if (directPhase(kind)) |phase| {
                return SubjectAuthority.direct(
                    2,
                    phase,
                    if (kind == .direct_binder_lookup_checked_error)
                        0
                    else if (kind == .annotation_where_alias_publication_error or
                        kind == .annotation_malformed_type or
                        kind == .annotation_malformed_where or
                        kind == .annotation_invalid_tag_child or
                        kind == .annotation_builtin_not_type or
                        kind == .annotated_binding_lookup_checked_error) 7 else null,
                );
            }
            return switch (kind) {
                .nominal_pattern_backing_checked_error,
                .nominal_pattern_backing_relation_rejected,
                => SubjectAuthority.expectedCopyOccurrence(5, 0, .destination),
                .aggregate_child_relation_rejected => SubjectAuthority.expectedEvidenceFreePlan(6, 2),
                .call_operand_checked_error => SubjectAuthority.expectedProducerRootPlan(6, 2),
                else => unreachable,
            };
        }

        fn cause(kind: ExpectedFailure.Kind) CauseOwner {
            return switch (kind) {
                .annotation_malformed_type,
                .annotation_malformed_where,
                .annotation_invalid_tag_child,
                => CauseOwner.cirDiagnostic(8),
                .annotation_child_failure => CauseOwner.expectedFailure(0),
                .direct_binder_lookup_checked_error,
                .annotated_binding_lookup_checked_error,
                .call_operand_checked_error,
                .nominal_pattern_backing_checked_error,
                => CauseOwner.expectedConsumerRetirement(9),
                else => CauseOwner.inactive(),
            };
        }

        fn subjectWithKind(
            kind: ExpectedFailure.Kind,
            subject_kind: SubjectAuthority.Kind,
        ) SubjectAuthority {
            return switch (subject_kind) {
                .direct => SubjectAuthority.direct(
                    2,
                    directPhase(kind) orelse .annotation_malformed_type,
                    if (kind == .direct_binder_lookup_checked_error)
                        0
                    else if (kind == .annotation_where_alias_publication_error or
                        kind == .annotation_malformed_type or
                        kind == .annotation_malformed_where or
                        kind == .annotation_invalid_tag_child or
                        kind == .annotation_builtin_not_type or
                        kind == .annotated_binding_lookup_checked_error) 7 else null,
                ),
                .expected_copy_occurrence => SubjectAuthority.expectedCopyOccurrence(
                    5,
                    0,
                    if (kind == .annotation_child_failure) .source else .destination,
                ),
                .expected_producer_root_plan => SubjectAuthority.expectedProducerRootPlan(6, 2),
                .expected_relation_plan => SubjectAuthority.expectedRelationPlan(6, 2),
                .expected_evidence_free_plan => SubjectAuthority.expectedEvidenceFreePlan(6, 2),
                .provider_where_alias_checked_error => SubjectAuthority.providerWhereAliasCheckedError(
                    9,
                    10,
                ),
            };
        }

        fn causeWithKind(cause_kind: ?CauseOwner.Kind) CauseOwner {
            const kind = cause_kind orelse return CauseOwner.inactive();
            return switch (kind) {
                .expected_failure => CauseOwner.expectedFailure(0),
                .expected_consumer_retirement => CauseOwner.expectedConsumerRetirement(8),
                .cir_diagnostic => CauseOwner.cirDiagnostic(8),
                .provider_where_alias_checked_error => CauseOwner.providerWhereAliasCheckedError(
                    9,
                    10,
                ),
            };
        }

        fn combinationAllowed(
            kind: ExpectedFailure.Kind,
            subject_kind: SubjectAuthority.Kind,
            cause_kind: ?CauseOwner.Kind,
        ) bool {
            return switch (kind) {
                .annotation_malformed_type,
                .annotation_malformed_where,
                .annotation_invalid_tag_child,
                => subject_kind == .direct and cause_kind == .cir_diagnostic,
                .annotation_where_receiver_not_introduced,
                .annotation_where_alias_not_alias,
                .annotation_recursive_where_alias,
                .annotation_where_alias_unresolved,
                .annotation_where_alias_arity,
                .annotation_where_alias_in_type_position,
                .annotation_builtin_not_type,
                .annotation_recursive_type_decl,
                .annotation_type_decl_poisoned,
                .annotation_type_formal_poisoned,
                .annotation_type_apply_arity,
                .annotation_alias_row_rejected,
                .annotation_external_type_unresolved,
                .annotation_duplicate_where_signature_rejected,
                .nominal_pattern_external_unresolved,
                .nominal_pattern_decl_poisoned,
                .nominal_pattern_opaque_inaccessible,
                .nominal_pattern_backing_unavailable,
                => subject_kind == .direct and cause_kind == null,
                .annotation_where_alias_publication_error => (subject_kind == .direct and cause_kind == null) or
                    (subject_kind == .provider_where_alias_checked_error and
                        cause_kind == .provider_where_alias_checked_error),
                .annotation_child_failure => (subject_kind == .direct or subject_kind == .expected_copy_occurrence) and
                    cause_kind != null,
                .direct_binder_lookup_checked_error,
                .annotated_binding_lookup_checked_error,
                => subject_kind == .direct and cause_kind == .expected_consumer_retirement,
                .call_operand_checked_error => subject_kind == .expected_producer_root_plan and
                    cause_kind == .expected_consumer_retirement,
                .nominal_pattern_backing_checked_error => subject_kind == .expected_copy_occurrence and cause_kind != null,
                .nominal_pattern_backing_relation_rejected => subject_kind == .expected_copy_occurrence and cause_kind == null,
                .aggregate_child_relation_rejected => (subject_kind == .expected_copy_occurrence or
                    subject_kind == .expected_producer_root_plan or
                    subject_kind == .expected_relation_plan or
                    subject_kind == .expected_evidence_free_plan) and
                    cause_kind == null,
            };
        }

        fn hasOrdinalSlot(kind: ExpectedFailure.Kind) bool {
            return switch (kind) {
                .annotation_malformed_where,
                .annotation_invalid_tag_child,
                .annotation_where_receiver_not_introduced,
                .annotation_type_formal_poisoned,
                .annotation_child_failure,
                .annotation_duplicate_where_signature_rejected,
                .aggregate_child_relation_rejected,
                .direct_binder_lookup_checked_error,
                .call_operand_checked_error,
                => true,
                .annotation_malformed_type,
                .annotation_where_alias_not_alias,
                .annotation_recursive_where_alias,
                .annotation_where_alias_publication_error,
                .annotation_where_alias_unresolved,
                .annotation_where_alias_arity,
                .annotation_where_alias_in_type_position,
                .annotation_builtin_not_type,
                .annotation_recursive_type_decl,
                .annotation_type_decl_poisoned,
                .annotation_type_apply_arity,
                .annotation_alias_row_rejected,
                .annotation_external_type_unresolved,
                .annotated_binding_lookup_checked_error,
                .nominal_pattern_external_unresolved,
                .nominal_pattern_decl_poisoned,
                .nominal_pattern_opaque_inaccessible,
                .nominal_pattern_backing_unavailable,
                .nominal_pattern_backing_checked_error,
                .nominal_pattern_backing_relation_rejected,
                => false,
            };
        }
    };

    const kinds = std.enums.values(ExpectedFailure.Kind);
    const owner_kinds = std.enums.values(ExpectedFailure.OwnerKind);
    const cause_kinds = std.enums.values(CauseOwner.Kind);
    const direct_phases = std.enums.values(SubjectAuthority.DirectPhase);
    var seen_owner_kinds = [_]bool{false} ** owner_kinds.len;
    var seen_direct_phases = [_]bool{false} ** direct_phases.len;
    try std.testing.expectEqual(@as(usize, 29), kinds.len);

    for (kinds) |kind| {
        const needs_peer = switch (kind) {
            .annotation_type_formal_poisoned,
            .annotation_duplicate_where_signature_rejected,
            .direct_binder_lookup_checked_error,
            .call_operand_checked_error,
            .nominal_pattern_backing_checked_error,
            .nominal_pattern_backing_relation_rejected,
            .aggregate_child_relation_rejected,
            => true,
            else => false,
        };
        const owner_kind = Fixture.ownerKind(kind);
        const subject = Fixture.subject(kind);
        var row = ExpectedFailure{
            .owner_node = 1,
            .site_node = 2,
            .owner_kind = @intFromEnum(owner_kind),
            .kind = @intFromEnum(kind),
            .raw_owner_var = 1,
            .raw_subject_var = 2,
            .raw_peer_var = if (needs_peer) 3 else ExpectedFailure.none,
            .constraint_index = if (kind == .annotation_duplicate_where_signature_rejected) 4 else ExpectedFailure.none,
            .slot = 0,
            .plan_index = if (kind == .aggregate_child_relation_rejected or
                kind == .call_operand_checked_error) 6 else ExpectedFailure.none,
            .subject_authority = subject,
            .cause_owner = Fixture.cause(kind),
        };
        try std.testing.expect(row.hasLegalTagsAt(1));
        seen_owner_kinds[@intFromEnum(owner_kind)] = true;
        if (subject.decodedDirectPhase()) |phase| seen_direct_phases[@intFromEnum(phase)] = true;

        row.slot = ExpectedFailure.none;
        try std.testing.expect(!row.hasLegalTagsAt(1));
        row.slot = 37;
        try std.testing.expectEqual(Fixture.hasOrdinalSlot(kind), row.hasLegalTagsAt(1));
        row.slot = 0;

        const subject_kinds = std.enums.values(SubjectAuthority.Kind);
        for (subject_kinds) |subject_kind| {
            for (0..cause_kinds.len + 1) |cause_offset| {
                const cause_kind: ?CauseOwner.Kind = if (cause_offset == 0)
                    null
                else
                    cause_kinds[cause_offset - 1];
                row.subject_authority = Fixture.subjectWithKind(kind, subject_kind);
                row.cause_owner = Fixture.causeWithKind(cause_kind);
                try std.testing.expectEqual(
                    Fixture.combinationAllowed(kind, subject_kind, cause_kind),
                    row.hasLegalTagsAt(1),
                );
            }
        }

        if (Fixture.directPhase(kind)) |expected_phase| {
            row.cause_owner = Fixture.cause(kind);
            for (direct_phases) |phase| {
                row.subject_authority = SubjectAuthority.direct(
                    2,
                    phase,
                    if (kind == .direct_binder_lookup_checked_error)
                        0
                    else if (kind == .annotation_where_alias_publication_error or
                        kind == .annotation_malformed_type or
                        kind == .annotation_malformed_where or
                        kind == .annotation_invalid_tag_child or
                        kind == .annotation_builtin_not_type or
                        kind == .annotated_binding_lookup_checked_error) 7 else null,
                );
                try std.testing.expectEqual(
                    phase == expected_phase,
                    row.hasLegalTagsAt(1),
                );
            }
        }
    }

    for (seen_owner_kinds) |seen| try std.testing.expect(seen);
    for (seen_direct_phases) |seen| try std.testing.expect(seen);
}

test "expected failure kind subject and cause swaps are rejected" {
    const none = ExpectedFailure.none;
    var row = ExpectedFailure{
        .owner_node = 1,
        .site_node = 2,
        .owner_kind = @intFromEnum(ExpectedFailure.OwnerKind.expression),
        .kind = @intFromEnum(ExpectedFailure.Kind.annotation_malformed_type),
        .raw_owner_var = 1,
        .raw_subject_var = 2,
        .raw_peer_var = none,
        .constraint_index = none,
        .slot = 0,
        .plan_index = none,
        .subject_authority = SubjectAuthority.direct(2, .annotation_malformed_type, 0),
        .cause_owner = CauseOwner.cirDiagnostic(4),
    };
    try std.testing.expect(row.hasLegalTagsAt(0));

    row.subject_authority.kind = @intFromEnum(SubjectAuthority.Kind.expected_copy_occurrence);
    try std.testing.expect(!row.hasLegalTagsAt(0));
    row.subject_authority.kind = @intFromEnum(SubjectAuthority.Kind.direct);
    row.cause_owner.kind = @intFromEnum(CauseOwner.Kind.expected_consumer_retirement);
    try std.testing.expect(!row.hasLegalTagsAt(0));
    row.cause_owner.kind = @intFromEnum(CauseOwner.Kind.cir_diagnostic);

    row.owner_kind = @intFromEnum(ExpectedFailure.OwnerKind.pattern);
    try std.testing.expect(!row.hasLegalTagsAt(0));
    row.owner_kind = @intFromEnum(ExpectedFailure.OwnerKind.expression);
    row.subject_authority.payload.direct.producer_node = 3;
    try std.testing.expect(!row.hasLegalTagsAt(0));
    row.subject_authority.payload.direct.producer_node = 2;
    row.subject_authority = SubjectAuthority.direct(2, .annotation_malformed_where, null);
    try std.testing.expect(!row.hasLegalTagsAt(0));
    row.subject_authority = SubjectAuthority.direct(2, .annotation_malformed_type, 0);
    row.cause_owner = CauseOwner.expectedConsumerRetirement(4);
    try std.testing.expect(!row.hasLegalTagsAt(0));
    row.cause_owner = CauseOwner.inactive();
    try std.testing.expect(!row.hasLegalTagsAt(0));

    row.kind = @intFromEnum(ExpectedFailure.Kind.annotation_child_failure);
    row.subject_authority = SubjectAuthority.direct(2, .annotation_child_before_copy, null);
    row.cause_owner = CauseOwner.expectedFailure(0);
    try std.testing.expect(!row.hasLegalTagsAt(0));
    try std.testing.expect(row.hasLegalTagsAt(1));
    row.cause_owner = CauseOwner.inactive();
    try std.testing.expect(!row.hasLegalTagsAt(1));
    row.cause_owner = CauseOwner.providerWhereAliasCheckedError(5, 6);
    try std.testing.expect(row.hasLegalTagsAt(1));
    row.subject_authority = SubjectAuthority.expectedCopyOccurrence(7, 0, .source);
    try std.testing.expect(row.hasLegalTagsAt(1));
    row.subject_authority = SubjectAuthority.expectedCopyOccurrence(7, 0, .destination);
    try std.testing.expect(!row.hasLegalTagsAt(1));
    row.subject_authority = SubjectAuthority.expectedCopyOccurrence(7, 0, .source);
    row.subject_authority = SubjectAuthority.expectedProducerRootPlan(7, 2);
    try std.testing.expect(!row.hasLegalTagsAt(1));

    row.kind = @intFromEnum(ExpectedFailure.Kind.annotation_duplicate_where_signature_rejected);
    row.raw_peer_var = 3;
    row.constraint_index = 8;
    row.subject_authority = SubjectAuthority.direct(2, .annotation_duplicate_where_relation, null);
    row.cause_owner = CauseOwner.inactive();
    try std.testing.expect(row.hasLegalTagsAt(0));
    row.cause_owner = CauseOwner.expectedFailure(0);
    try std.testing.expect(!row.hasLegalTagsAt(1));
    row.cause_owner = CauseOwner.inactive();
    row.constraint_index = none;
    try std.testing.expect(!row.hasLegalTagsAt(0));
}

test "expected failure provider nominal and aggregate arms are exact" {
    const none = ExpectedFailure.none;
    var row = ExpectedFailure{
        .owner_node = 1,
        .site_node = 2,
        .owner_kind = @intFromEnum(ExpectedFailure.OwnerKind.expression),
        .kind = @intFromEnum(ExpectedFailure.Kind.annotation_where_alias_publication_error),
        .raw_owner_var = 1,
        .raw_subject_var = 2,
        .raw_peer_var = none,
        .constraint_index = none,
        .slot = 0,
        .plan_index = none,
        .subject_authority = SubjectAuthority.providerWhereAliasCheckedError(3, 4),
        .cause_owner = CauseOwner.providerWhereAliasCheckedError(3, 4),
    };
    try std.testing.expect(row.hasLegalTagsAt(0));
    row.cause_owner = CauseOwner.providerWhereAliasCheckedError(3, 5);
    try std.testing.expect(!row.hasLegalTagsAt(0));
    row.cause_owner = CauseOwner.inactive();
    try std.testing.expect(!row.hasLegalTagsAt(0));
    row.subject_authority = SubjectAuthority.direct(
        2,
        .annotation_local_where_alias_checked_error,
        6,
    );
    try std.testing.expect(row.hasLegalTagsAt(0));
    row.subject_authority = SubjectAuthority.direct(
        2,
        .annotation_local_where_alias_checked_error,
        null,
    );
    try std.testing.expect(!row.hasLegalTagsAt(0));

    row.owner_kind = @intFromEnum(ExpectedFailure.OwnerKind.pattern);
    row.kind = @intFromEnum(ExpectedFailure.Kind.nominal_pattern_backing_checked_error);
    row.raw_peer_var = 7;
    row.subject_authority = SubjectAuthority.expectedCopyOccurrence(8, 0, .destination);
    row.cause_owner = CauseOwner.cirDiagnostic(9);
    try std.testing.expect(row.hasLegalTagsAt(0));
    row.subject_authority = SubjectAuthority.expectedCopyOccurrence(8, 0, .source);
    try std.testing.expect(!row.hasLegalTagsAt(0));
    row.subject_authority = SubjectAuthority.expectedCopyOccurrence(8, 0, .destination);
    row.cause_owner = CauseOwner.inactive();
    try std.testing.expect(!row.hasLegalTagsAt(0));
    row.cause_owner = CauseOwner.cirDiagnostic(9);
    row.subject_authority = SubjectAuthority.direct(2, .nominal_pattern_backing_unavailable, null);
    try std.testing.expect(!row.hasLegalTagsAt(0));

    row.kind = @intFromEnum(ExpectedFailure.Kind.nominal_pattern_backing_relation_rejected);
    row.subject_authority = SubjectAuthority.expectedCopyOccurrence(8, 0, .destination);
    row.cause_owner = CauseOwner.inactive();
    try std.testing.expect(row.hasLegalTagsAt(0));
    row.subject_authority = SubjectAuthority.expectedCopyOccurrence(8, 0, .source);
    try std.testing.expect(!row.hasLegalTagsAt(0));
    row.subject_authority = SubjectAuthority.expectedCopyOccurrence(8, 0, .destination);
    row.cause_owner = CauseOwner.cirDiagnostic(9);
    try std.testing.expect(!row.hasLegalTagsAt(0));

    row.owner_kind = @intFromEnum(ExpectedFailure.OwnerKind.expression);
    row.kind = @intFromEnum(ExpectedFailure.Kind.aggregate_child_relation_rejected);
    row.plan_index = 10;
    row.subject_authority = SubjectAuthority.expectedEvidenceFreePlan(10, 2);
    row.cause_owner = CauseOwner.inactive();
    try std.testing.expect(row.hasLegalTagsAt(0));
    row.subject_authority = SubjectAuthority.expectedProducerRootPlan(10, 11);
    try std.testing.expect(!row.hasLegalTagsAt(0));
    row.subject_authority = SubjectAuthority.expectedProducerRootPlan(10, 2);
    try std.testing.expect(row.hasLegalTagsAt(0));
    row.subject_authority = SubjectAuthority.expectedRelationPlan(10, 11);
    try std.testing.expect(!row.hasLegalTagsAt(0));
    row.subject_authority = SubjectAuthority.expectedRelationPlan(10, 2);
    try std.testing.expect(row.hasLegalTagsAt(0));
    row.subject_authority = SubjectAuthority.expectedCopyOccurrence(8, 0, .destination);
    try std.testing.expect(row.hasLegalTagsAt(0));
    row.subject_authority = SubjectAuthority.direct(2, .annotation_child_before_copy, null);
    try std.testing.expect(!row.hasLegalTagsAt(0));
    row.subject_authority = SubjectAuthority.expectedEvidenceFreePlan(10, 2);
    row.cause_owner = CauseOwner.expectedConsumerRetirement(12);
    try std.testing.expect(!row.hasLegalTagsAt(0));
    row.cause_owner = CauseOwner.inactive();
    row.plan_index = none;
    try std.testing.expect(!row.hasLegalTagsAt(0));

    row.kind = std.math.maxInt(u32);
    try std.testing.expect(!row.hasLegalTagsAt(0));
}

test "expected failure authority owner and row layouts are fixed" {
    try std.testing.expectEqual(4 * @sizeOf(u32), @sizeOf(SubjectAuthority));
    try std.testing.expectEqual(4 * @sizeOf(u32), @sizeOf(CauseOwner));
    try std.testing.expectEqual(21 * @sizeOf(u32), @sizeOf(ExpectedConsumptionPlan));
    try std.testing.expectEqual(19 * @sizeOf(u32), @sizeOf(ExpectedFailure));
    try std.testing.expectEqual(4 * @sizeOf(u32), @sizeOf(BodyAnnotationAttachment));
    try std.testing.expectEqual(@as(usize, 4), @offsetOf(SubjectAuthority, "payload"));
    try std.testing.expectEqual(@as(usize, 4), @offsetOf(CauseOwner, "payload"));
    try std.testing.expectEqual(@as(usize, 60), @offsetOf(ExpectedConsumptionPlan, "failure_owner"));
    try std.testing.expectEqual(@as(usize, 76), @offsetOf(ExpectedConsumptionPlan, "failure_cause_plan_index"));
    try std.testing.expectEqual(@as(usize, 40), @offsetOf(ExpectedFailure, "subject_authority"));
    try std.testing.expectEqual(@as(usize, 56), @offsetOf(ExpectedFailure, "cause_owner"));
}

test "expected retirement snapshots the complete raw node payload" {
    try std.testing.expectEqual(@sizeOf(Node.Payload), @sizeOf([4]u32));
    try std.testing.expectEqual(@sizeOf(u32), @sizeOf(ExpectedRetirementFailure));
    try std.testing.expectEqual(18 * @sizeOf(u32), @sizeOf(ExpectedConsumerRetirement));
    try std.testing.expectEqual(
        @as(usize, 44),
        @offsetOf(ExpectedConsumerRetirement, "expected_failures_start"),
    );
    try std.testing.expectEqual(
        @as(usize, 48),
        @offsetOf(ExpectedConsumerRetirement, "expected_failures_len"),
    );
}

test "expected ambiguity retirement canonical tags are exhaustive" {
    const Row = ExpectedAmbiguityRetirement;
    try std.testing.expectEqual(14 * @sizeOf(u32), @sizeOf(Row));
    try std.testing.expectEqual(2, @typeInfo(Row.Kind).@"enum".fields.len);
    try std.testing.expectEqual(2, @typeInfo(Row.Source).@"enum".fields.len);
    try std.testing.expectEqual(2, @typeInfo(Row.ConsumerGroupKind).@"enum".fields.len);
    var row = Row{
        .raw_receiver_var = 1,
        .retired_expr = 2,
        .selected_constraint_index = 3,
        .copy_step = 4,
        .occurrence_offset = 5,
        .pair_offset = 6,
        .diagnostic_index = 7,
        .consumer_root_plan = 8,
        .consumer_plan_count = 4,
        .kind = @intFromEnum(Row.Kind.body_forced_instantiation_branch),
        .source = @intFromEnum(Row.Source.instantiation),
        .consumer_group_kind = @intFromEnum(Row.ConsumerGroupKind.branch),
        .selection_flags = Row.encodeSelectionFlags(true, false, true, false),
    };

    const required_flags = Row.instantiated_where_clause_flag | Row.body_forced_flag;
    for (0..16) |raw_flags| {
        const flags: u32 = @intCast(raw_flags);
        row.selection_flags = flags;
        const expected = flags == required_flags or
            flags == (required_flags | Row.only_where_clause_contracts_flag);
        try std.testing.expectEqual(expected, row.hasCanonicalTags());
    }

    row.selection_flags = Row.encodeSelectionFlags(true, false, true, false);
    try std.testing.expect(row.hasCanonicalTags());
    try std.testing.expect(row.isInstantiatedWhereClause());
    try std.testing.expect(!row.onlyWhereClauseContracts());
    try std.testing.expect(row.isBodyForced());
    try std.testing.expect(!row.hasWhereDispatchUse());

    row.selection_flags |= 1 << 8;
    try std.testing.expect(!row.hasCanonicalTags());
    row.selection_flags = Row.encodeSelectionFlags(true, true, true, false);
    try std.testing.expect(row.hasCanonicalTags());
    try std.testing.expect(row.onlyWhereClauseContracts());

    inline for (.{
        "raw_receiver_var",
        "retired_expr",
        "selected_constraint_index",
        "diagnostic_index",
        "consumer_root_plan",
    }) |field| {
        var missing = row;
        @field(missing, field) = Row.none;
        try std.testing.expect(!missing.hasCanonicalTags());
    }
    var missing_count = row;
    missing_count.consumer_plan_count = 0;
    try std.testing.expect(!missing_count.hasCanonicalTags());
    inline for (.{ "copy_step", "occurrence_offset", "pair_offset" }) |field| {
        var missing = row;
        @field(missing, field) = Row.none;
        try std.testing.expect(!missing.hasCanonicalTags());
    }

    row.kind = 2;
    try std.testing.expect(!row.hasCanonicalTags());
    row.kind = std.math.maxInt(u32);
    try std.testing.expect(!row.hasCanonicalTags());
    row.kind = @intFromEnum(Row.Kind.body_forced_instantiation_branch);
    row.source = 2;
    try std.testing.expect(!row.hasCanonicalTags());
    row.source = std.math.maxInt(u32);
    try std.testing.expect(!row.hasCanonicalTags());
    row.source = @intFromEnum(Row.Source.instantiation);
    row.consumer_group_kind = @intFromEnum(Row.ConsumerGroupKind.call);
    try std.testing.expect(!row.hasCanonicalTags());
    row.consumer_group_kind = @intFromEnum(Row.ConsumerGroupKind.branch);
    row.reserved_0 = 1;
    try std.testing.expect(!row.hasCanonicalTags());
    row.reserved_0 = 0;

    row.kind = @intFromEnum(Row.Kind.creation_dispatch_call);
    row.source = @intFromEnum(Row.Source.creation);
    row.consumer_group_kind = @intFromEnum(Row.ConsumerGroupKind.call);
    row.copy_step = Row.none;
    row.occurrence_offset = Row.none;
    row.pair_offset = Row.none;
    row.selection_flags = Row.encodeSelectionFlags(false, false, false, false);
    try std.testing.expect(row.hasCanonicalTags());
    row.copy_step = 4;
    try std.testing.expect(!row.hasCanonicalTags());
    row.copy_step = Row.none;
    row.source = @intFromEnum(Row.Source.instantiation);
    try std.testing.expect(!row.hasCanonicalTags());
    row.source = @intFromEnum(Row.Source.creation);
    row.consumer_group_kind = @intFromEnum(Row.ConsumerGroupKind.branch);
    try std.testing.expect(!row.hasCanonicalTags());
    row.consumer_group_kind = @intFromEnum(Row.ConsumerGroupKind.call);
    row.selection_flags = Row.encodeSelectionFlags(false, false, true, false);
    try std.testing.expect(!row.hasCanonicalTags());
}

test "expected ambiguity retirement order uses the complete semantic key" {
    const Row = ExpectedAmbiguityRetirement;
    const row = Row{
        .raw_receiver_var = 2,
        .retired_expr = 1,
        .selected_constraint_index = 3,
        .copy_step = 4,
        .occurrence_offset = 5,
        .pair_offset = 6,
        .diagnostic_index = 7,
        .consumer_root_plan = 8,
        .consumer_plan_count = 4,
        .kind = @intFromEnum(Row.Kind.body_forced_instantiation_branch),
        .source = @intFromEnum(Row.Source.instantiation),
        .consumer_group_kind = @intFromEnum(Row.ConsumerGroupKind.branch),
        .selection_flags = Row.encodeSelectionFlags(true, false, true, false),
    };
    try std.testing.expect(!Row.canonicalLessThan(row, row));

    inline for (.{
        "retired_expr",
        "consumer_group_kind",
        "consumer_root_plan",
        "consumer_plan_count",
        "raw_receiver_var",
        "selected_constraint_index",
        "copy_step",
        "occurrence_offset",
        "pair_offset",
        "diagnostic_index",
    }) |field| {
        var later = row;
        @field(later, field) += 1;
        try std.testing.expect(Row.canonicalLessThan(row, later));
        try std.testing.expect(!Row.canonicalLessThan(later, row));
    }

    var later_flags = row;
    later_flags.selection_flags |= Row.only_where_clause_contracts_flag;
    try std.testing.expect(Row.canonicalLessThan(row, later_flags));
    try std.testing.expect(!Row.canonicalLessThan(later_flags, row));
}

test "malformed expression publications preserve the typed creation namespace" {
    var env = try Self.init(std.testing.allocator, "");
    defer env.deinit();

    const expr = try env.pushMalformed(CIR.Expr.Idx, .{ .range_op_chained = .{
        .region = Region.zero(),
    } });
    _ = try env.pushMalformed(CIR.Statement.Idx, .{ .range_op_chained = .{
        .region = Region.zero(),
    } });
    const annotation = try env.pushMalformed(CIR.TypeAnno.Idx, .{ .malformed_type_annotation = .{
        .region = Region.zero(),
    } });

    try std.testing.expectEqual(
        @as(usize, 1),
        env.malformed_expression_publications.items.items.len,
    );
    const publication = env.malformed_expression_publications.items.items[0];
    try std.testing.expect(publication.hasLegalTags());
    try std.testing.expectEqual(@intFromEnum(expr), publication.expr_node);
    try std.testing.expectEqual(
        @intFromEnum(env.store.getExpr(expr).e_runtime_error.diagnostic),
        publication.diagnostic_index,
    );
    try std.testing.expectEqual(
        @as(usize, 1),
        env.malformed_type_annotation_publications.items.items.len,
    );
    const annotation_publication = env.malformed_type_annotation_publications.items.items[0];
    try std.testing.expect(annotation_publication.hasLegalTags());
    try std.testing.expectEqual(@intFromEnum(annotation), annotation_publication.annotation_node);
    try std.testing.expectEqual(
        @intFromEnum(env.store.getTypeAnno(annotation).malformed.diagnostic),
        annotation_publication.diagnostic_index,
    );

    const deferred = try env.pushRuntimeErrorExpr(CIR.Expr.Idx, .{ .range_op_chained = .{
        .region = Region.zero(),
    } });
    try std.testing.expectEqual(
        @as(usize, 2),
        env.malformed_expression_publications.items.items.len,
    );
    try std.testing.expectEqual(
        @intFromEnum(deferred),
        env.malformed_expression_publications.items.items[1].expr_node,
    );
}

test "where clause owner lists are strict written subsequences" {
    var written: [128]u32 = undefined;
    for (&written, 0..) |*where_raw, index| {
        where_raw.* = @intCast(index * 3 + 1);
    }
    try std.testing.expect(whereClauseListIsStrictlyIncreasing(&written));

    const ordered = [_]u32{ written[0], written[63], written[127] };
    try std.testing.expect(whereClauseListIsWrittenSubsequence(&written, &ordered));
    const missing = [_]u32{ written[0], written[63] + 1, written[127] };
    try std.testing.expect(!whereClauseListIsWrittenSubsequence(&written, &missing));
    const permuted = [_]u32{ written[127], written[0] };
    try std.testing.expect(!whereClauseListIsWrittenSubsequence(&written, &permuted));
    const duplicate = [_]u32{ written[63], written[63] };
    try std.testing.expect(!whereClauseListIsWrittenSubsequence(&written, &duplicate));

    for (written) |where_raw| {
        const singleton = [_]u32{where_raw};
        try std.testing.expect(whereClauseListIsWrittenSubsequence(&written, &singleton));
    }
}

test "where clause owner rows preserve exact normalized receivers" {
    var env = try Self.init(std.testing.allocator, "");
    defer env.deinit();

    const region = Region.zero();
    const rigid_name: Ident.Idx = @bitCast(@as(u32, 1));
    const method_name: Ident.Idx = @bitCast(@as(u32, 2));
    const rigid = try env.addTypeAnno(.{ .rigid_var = .{ .name = rigid_name } }, region);
    const detached_rigid = try env.addTypeAnno(.{ .rigid_var = .{ .name = rigid_name } }, region);
    const alias_argument_rigid = try env.addTypeAnno(.{ .rigid_var = .{ .name = rigid_name } }, region);
    const rigid_ref = try env.addTypeAnno(.{ .rigid_var_lookup = .{ .ref = rigid } }, region);
    const detached_ref = try env.addTypeAnno(.{ .rigid_var_lookup = .{ .ref = detached_rigid } }, region);

    const alias_args_start = env.store.scratchTypeAnnoTop();
    try env.store.addScratchTypeAnno(alias_argument_rigid);
    const alias_args = try env.store.typeAnnoSpanFrom(alias_args_start);
    const alias_apply = try env.addTypeAnno(.{ .apply = .{
        .name = rigid_name,
        .base = .{ .builtin = .list },
        .args = alias_args,
    } }, region);

    const where_start = env.store.scratchWhereClauseTop();
    const direct_method = try env.addWhereClause(.{ .w_method = .{
        .var_ = rigid,
        .method_name = method_name,
        .args = .{ .span = base.DataSpan.empty() },
        .ret = rigid_ref,
        .effectful = false,
    } }, region);
    try env.store.addScratchWhereClause(direct_method);
    const lookup_effectful_method = try env.addWhereClause(.{ .w_method = .{
        .var_ = rigid_ref,
        .method_name = method_name,
        .args = .{ .span = base.DataSpan.empty() },
        .ret = rigid_ref,
        .effectful = true,
    } }, region);
    try env.store.addScratchWhereClause(lookup_effectful_method);
    const direct_alias = try env.addWhereClause(.{ .w_alias = .{
        .var_ = rigid,
        .alias = alias_apply,
    } }, region);
    try env.store.addScratchWhereClause(direct_alias);
    const lookup_alias = try env.addWhereClause(.{ .w_alias = .{
        .var_ = rigid_ref,
        .alias = alias_apply,
    } }, region);
    try env.store.addScratchWhereClause(lookup_alias);
    const unowned_method = try env.addWhereClause(.{ .w_method = .{
        .var_ = detached_ref,
        .method_name = method_name,
        .args = .{ .span = base.DataSpan.empty() },
        .ret = detached_ref,
        .effectful = false,
    } }, region);
    try env.store.addScratchWhereClause(unowned_method);

    const where = try env.store.whereClauseSpanFrom(where_start, &.{rigid});
    const annotation = try env.addAnnotation(.{ .anno = rigid, .where = where }, region);
    const expected_clauses = [_]struct { clause: CIR.WhereClause.Idx, owner: CIR.TypeAnno.Idx }{
        .{ .clause = direct_method, .owner = rigid },
        .{ .clause = lookup_effectful_method, .owner = rigid },
        .{ .clause = direct_alias, .owner = rigid },
        .{ .clause = lookup_alias, .owner = rigid },
        .{ .clause = unowned_method, .owner = detached_rigid },
    };
    for (expected_clauses) |expected| {
        try std.testing.expectEqual(
            @intFromEnum(expected.owner),
            env.listedWhereClauseReceiverOwner(@intFromEnum(expected.clause)) orelse
                return error.TestUnexpectedResult,
        );
    }

    var saw_owned = false;
    var saw_unowned = false;
    for (env.store.sliceWhereClauseOwners(where)) |owner| {
        if (owner.rigid_var == @intFromEnum(rigid)) {
            try std.testing.expect(owner.owned_by_annotation);
            try std.testing.expectEqual(@as(usize, 4), env.store.sliceWhereClausesForOwner(owner).len);
            saw_owned = true;
        } else if (owner.rigid_var == @intFromEnum(detached_rigid)) {
            try std.testing.expect(!owner.owned_by_annotation);
            try std.testing.expectEqual(@as(usize, 1), env.store.sliceWhereClausesForOwner(owner).len);
            saw_unowned = true;
        } else return error.TestUnexpectedResult;
    }
    try std.testing.expect(saw_owned and saw_unowned);
    try std.testing.expect(env.annotationOwnsTypeAnnoNode(annotation, rigid));
    try std.testing.expect(env.annotationOwnsTypeAnnoNode(annotation, rigid_ref));
    try std.testing.expect(env.annotationOwnsTypeAnnoNode(annotation, alias_apply));
    try std.testing.expect(env.annotationOwnsTypeAnnoNode(annotation, alias_argument_rigid));
    try std.testing.expect(!env.annotationOwnsTypeAnnoNode(annotation, detached_ref));

    // The lookup occurrence is a valid syntax node, but its `ref` remains an
    // identity coordinate rather than a structural path to either rigid.
    try std.testing.expect(env.typeAnnoTreeContains(rigid_ref, rigid_ref));
    try std.testing.expect(!env.typeAnnoTreeContains(rigid_ref, rigid));
    try std.testing.expect(!env.typeAnnoTreeContains(rigid_ref, detached_rigid));

    // Retargeting a lookup receiver to another real rigid keeps the occurrence
    // locally well-formed but contradicts the already-authored owner row.
    const rigid_ref_node_idx = nodeIdxFrom(rigid_ref);
    const saved_rigid_ref_node = env.store.nodes.get(rigid_ref_node_idx);
    var changed_rigid_ref_node = saved_rigid_ref_node;
    var changed_rigid_ref_payload = changed_rigid_ref_node.getPayload();
    changed_rigid_ref_payload.ty_rigid_var_lookup.ref = @intFromEnum(detached_rigid);
    changed_rigid_ref_node.setPayload(changed_rigid_ref_payload);
    env.store.nodes.set(rigid_ref_node_idx, changed_rigid_ref_node);
    const changed_rigid_ref_is_valid = env.typeAnnoTreeContains(rigid_ref, rigid_ref);
    const changed_rigid_ref_is_owned = env.annotationOwnsTypeAnnoNode(annotation, rigid);
    env.store.nodes.set(rigid_ref_node_idx, saved_rigid_ref_node);
    try std.testing.expect(changed_rigid_ref_is_valid);
    try std.testing.expect(!changed_rigid_ref_is_owned);
    try std.testing.expect(env.annotationOwnsTypeAnnoNode(annotation, rigid));

    // The inverse is checked for unowned rows as well; the bit cannot hide a
    // receiver which was retargeted after canonical publication.
    const detached_ref_node_idx = nodeIdxFrom(detached_ref);
    const saved_detached_ref_node = env.store.nodes.get(detached_ref_node_idx);
    var changed_detached_ref_node = saved_detached_ref_node;
    var changed_detached_ref_payload = changed_detached_ref_node.getPayload();
    changed_detached_ref_payload.ty_rigid_var_lookup.ref = @intFromEnum(rigid);
    changed_detached_ref_node.setPayload(changed_detached_ref_payload);
    env.store.nodes.set(detached_ref_node_idx, changed_detached_ref_node);
    const changed_detached_ref_is_valid = env.typeAnnoTreeContains(detached_ref, detached_ref);
    const changed_detached_ref_is_owned = env.annotationOwnsTypeAnnoNode(annotation, rigid);
    env.store.nodes.set(detached_ref_node_idx, saved_detached_ref_node);
    try std.testing.expect(changed_detached_ref_is_valid);
    try std.testing.expect(!changed_detached_ref_is_owned);
    try std.testing.expect(env.annotationOwnsTypeAnnoNode(annotation, rigid));
}

test "body annotation malformed type publications preserve exact owned attachments" {
    var env = try Self.init(std.testing.allocator, "");
    defer env.deinit();

    const region = Region.zero();
    const name: Ident.Idx = @bitCast(@as(u32, 1));
    const method_name: Ident.Idx = @bitCast(@as(u32, 2));

    const rigid = try env.addTypeAnno(.{ .rigid_var = .{ .name = name } }, region);
    const main_malformed = try env.pushMalformed(CIR.TypeAnno.Idx, .{ .malformed_type_annotation = .{
        .region = region,
    } });
    const main_start = env.store.scratchTypeAnnoTop();
    try env.store.addScratchTypeAnno(rigid);
    try env.store.addScratchTypeAnno(main_malformed);
    const main_elems = try env.store.typeAnnoSpanFrom(main_start);
    const main_root = try env.addTypeAnno(.{ .tuple = .{ .elems = main_elems } }, region);
    const rigid_ref = try env.addTypeAnno(.{ .rigid_var_lookup = .{ .ref = rigid } }, region);

    const method_malformed = try env.pushMalformed(CIR.TypeAnno.Idx, .{ .malformed_type_annotation = .{
        .region = region,
    } });
    const method_nested = try env.addTypeAnno(.{ .parens = .{ .anno = method_malformed } }, region);
    const method_args_start = env.store.scratchTypeAnnoTop();
    try env.store.addScratchTypeAnno(method_nested);
    const method_args = try env.store.typeAnnoSpanFrom(method_args_start);

    const alias_malformed = try env.pushMalformed(CIR.TypeAnno.Idx, .{ .malformed_type_annotation = .{
        .region = region,
    } });
    const alias_args_start = env.store.scratchTypeAnnoTop();
    try env.store.addScratchTypeAnno(alias_malformed);
    const alias_args = try env.store.typeAnnoSpanFrom(alias_args_start);
    const alias_apply = try env.addTypeAnno(.{ .apply = .{
        .name = name,
        .base = .{ .builtin = .list },
        .args = alias_args,
    } }, region);

    const detached_rigid = try env.addTypeAnno(.{ .rigid_var = .{ .name = name } }, region);
    const detached_ref = try env.addTypeAnno(.{ .rigid_var_lookup = .{ .ref = detached_rigid } }, region);
    const detached_malformed = try env.pushMalformed(CIR.TypeAnno.Idx, .{ .malformed_type_annotation = .{
        .region = region,
    } });
    const detached_nested = try env.addTypeAnno(.{ .parens = .{ .anno = detached_malformed } }, region);

    const where_start = env.store.scratchWhereClauseTop();
    const method_clause = try env.addWhereClause(.{ .w_method = .{
        .var_ = rigid_ref,
        .method_name = method_name,
        .args = method_args,
        .ret = rigid_ref,
        .effectful = false,
    } }, region);
    try env.store.addScratchWhereClause(method_clause);
    const alias_clause = try env.addWhereClause(.{ .w_alias = .{
        .var_ = rigid_ref,
        .alias = alias_apply,
    } }, region);
    try env.store.addScratchWhereClause(alias_clause);
    const detached_clause = try env.addWhereClause(.{ .w_method = .{
        .var_ = detached_ref,
        .method_name = method_name,
        .args = .{ .span = base.DataSpan.empty() },
        .ret = detached_nested,
        .effectful = false,
    } }, region);
    try env.store.addScratchWhereClause(detached_clause);
    const malformed_where_diagnostic = try env.addDiagnostic(.{ .malformed_where_clause = .{
        .region = region,
    } });
    const malformed_clause = try env.addWhereClause(.{ .w_malformed = .{
        .diagnostic = malformed_where_diagnostic,
    } }, region);
    try env.store.addScratchWhereClause(malformed_clause);
    const where = try env.store.whereClauseSpanFrom(where_start, &.{main_root});
    const foreign_clause = try env.addWhereClause(.{ .w_alias = .{
        .var_ = detached_ref,
        .alias = detached_nested,
    } }, region);

    const annotation = try env.addAnnotation(.{
        .anno = main_root,
        .where = where,
    }, region);
    try std.testing.expect(env.annotationOwnsMalformedTypePublication(annotation, 0));
    try std.testing.expect(env.annotationOwnsMalformedTypePublication(annotation, 1));
    try std.testing.expect(env.annotationOwnsMalformedTypePublication(annotation, 2));
    try std.testing.expect(!env.annotationOwnsMalformedTypePublication(annotation, 3));
    try std.testing.expect(!env.annotationOwnsMalformedTypePublication(annotation, 4));

    const pattern = try env.addPattern(.{ .assign = .{ .ident = name } }, region);
    const def_body = try env.addExpr(.{ .e_empty_record = .{} }, region);
    const decl_body = try env.addExpr(.{ .e_empty_record = .{} }, region);
    const var_body = try env.addExpr(.{ .e_empty_record = .{} }, region);
    const placeholder_body = try env.addExpr(.{ .e_empty_record = .{} }, region);
    const associated_body = try env.addExpr(.{ .e_empty_record = .{} }, region);
    const replacement_body = try env.addExpr(.{ .e_empty_record = .{} }, region);

    _ = try env.addStagedLocalDef(.{
        .pattern = pattern,
        .expr = associated_body,
        .annotation = annotation,
        .kind = .let,
    }, region);
    try std.testing.expectEqual(
        @as(usize, 0),
        env.body_annotation_attachments.items.items.len,
    );
    try std.testing.expectEqual(
        @as(usize, 0),
        env.body_annotation_malformed_type_publications.items.items.len,
    );

    const placeholder_idx = try env.addStatement(.{ .s_decl = .{
        .pattern = pattern,
        .expr = placeholder_body,
        .anno = null,
    } }, region);
    const def_idx = try env.addDef(.{
        .pattern = pattern,
        .expr = def_body,
        .annotation = annotation,
        .kind = .let,
    }, region);
    const decl_idx = try env.addStatement(.{ .s_decl = .{
        .pattern = pattern,
        .expr = decl_body,
        .anno = annotation,
    } }, region);
    const var_idx = try env.addStatement(.{ .s_var = .{
        .pattern_idx = pattern,
        .expr = var_body,
        .anno = annotation,
    } }, region);
    _ = try env.addStatement(.{ .s_var_uninitialized = .{
        .pattern_idx = pattern,
        .anno = annotation,
    } }, region);
    try env.setBodyAnnotationStatement(placeholder_idx, .{ .s_decl = .{
        .pattern = pattern,
        .expr = associated_body,
        .anno = annotation,
    } });

    const ExpectedAttachment = struct {
        kind: BodyAnnotationAttachment.AttachmentKind,
        node: u32,
        body: CIR.Expr.Idx,
    };
    const expected_attachments = [_]ExpectedAttachment{
        .{ .kind = .local_decl, .node = @intFromEnum(placeholder_idx), .body = associated_body },
        .{ .kind = .top_level_def, .node = @intFromEnum(def_idx), .body = def_body },
        .{ .kind = .local_decl, .node = @intFromEnum(decl_idx), .body = decl_body },
        .{ .kind = .local_var, .node = @intFromEnum(var_idx), .body = var_body },
    };
    const attachments = env.body_annotation_attachments.items.items;
    try std.testing.expectEqual(@as(usize, expected_attachments.len), attachments.len);
    for (expected_attachments, 0..) |expected, attachment_index| {
        const attachment = attachments[attachment_index];
        try std.testing.expect(attachment.hasLegalTags());
        try std.testing.expectEqual(@intFromEnum(expected.kind), attachment.attachment_kind);
        try std.testing.expectEqual(expected.node, attachment.attachment_node);
        try std.testing.expectEqual(@intFromEnum(annotation), attachment.annotation_root);
        try std.testing.expectEqual(@intFromEnum(expected.body), attachment.body_expr);
        try std.testing.expect(env.bodyAnnotationAttachmentIsLocallyValid(@intCast(attachment_index)));
        try std.testing.expectEqual(
            @as(?u32, @intCast(attachment_index)),
            env.bodyAnnotationAttachmentIndex(
                expected.kind,
                expected.node,
                annotation,
                expected.body,
            ),
        );
    }
    try std.testing.expectEqual(
        @as(?u32, null),
        env.bodyAnnotationAttachmentIndex(.local_decl, @intFromEnum(def_idx), annotation, def_body),
    );

    const def_attachment_index: u32 = 1;
    try std.testing.expect(env.bodyAnnotationAttachmentContainsTypeAnno(
        def_attachment_index,
        main_malformed,
    ));
    try std.testing.expect(env.bodyAnnotationAttachmentContainsTypeAnno(
        def_attachment_index,
        method_malformed,
    ));
    try std.testing.expect(env.bodyAnnotationAttachmentContainsTypeAnno(
        def_attachment_index,
        alias_malformed,
    ));
    try std.testing.expect(!env.bodyAnnotationAttachmentContainsTypeAnno(
        def_attachment_index,
        detached_malformed,
    ));
    try std.testing.expect(env.bodyAnnotationAttachmentContainsWhereClause(
        def_attachment_index,
        method_clause,
    ));
    try std.testing.expect(env.bodyAnnotationAttachmentContainsWhereClause(
        def_attachment_index,
        alias_clause,
    ));
    try std.testing.expect(env.bodyAnnotationAttachmentContainsWhereClause(
        def_attachment_index,
        detached_clause,
    ));
    try std.testing.expect(env.bodyAnnotationAttachmentContainsWhereClause(
        def_attachment_index,
        malformed_clause,
    ));
    try std.testing.expect(!env.bodyAnnotationAttachmentContainsWhereClause(
        def_attachment_index,
        foreign_clause,
    ));

    const publications = env.body_annotation_malformed_type_publications.items.items;
    try std.testing.expectEqual(@as(usize, expected_attachments.len * 3), publications.len);
    for (expected_attachments, 0..) |expected, attachment_index| {
        for (0..3) |malformed_index| {
            const publication = publications[attachment_index * 3 + malformed_index];
            try std.testing.expect(publication.hasLegalTags());
            try std.testing.expectEqual(@intFromEnum(expected.kind), publication.attachment_kind);
            try std.testing.expectEqual(expected.node, publication.attachment_node);
            try std.testing.expectEqual(@intFromEnum(annotation), publication.annotation_root);
            try std.testing.expectEqual(@intFromEnum(expected.body), publication.body_expr);
            try std.testing.expectEqual(@as(u32, @intCast(malformed_index)), publication.malformed_type_publication_index);
        }
    }

    env.setDefExpr(def_idx, replacement_body);
    try std.testing.expectEqual(replacement_body, env.store.getDef(def_idx).expr);
    try std.testing.expectEqual(
        @intFromEnum(replacement_body),
        env.body_annotation_attachments.items.items[def_attachment_index].body_expr,
    );
    try std.testing.expect(env.bodyAnnotationAttachmentIsLocallyValid(def_attachment_index));
    try std.testing.expectEqual(
        def_attachment_index,
        env.bodyAnnotationAttachmentIndex(
            .top_level_def,
            @intFromEnum(def_idx),
            annotation,
            replacement_body,
        ).?,
    );
    for (publications[3..6]) |publication| {
        try std.testing.expectEqual(@intFromEnum(replacement_body), publication.body_expr);
    }
    for (publications[0..3]) |publication| {
        try std.testing.expect(publication.body_expr != @intFromEnum(replacement_body));
    }
    for (publications[6..]) |publication| {
        try std.testing.expect(publication.body_expr != @intFromEnum(replacement_body));
    }
}

test "expected retirement failure ranges have exact empty and nonempty forms" {
    var failure_ref = ExpectedRetirementFailure{ .failure_index = 3 };
    try std.testing.expect(failure_ref.hasLegalTags());
    failure_ref.failure_index = ExpectedRetirementFailure.none;
    try std.testing.expect(!failure_ref.hasLegalTags());

    var retirement = ExpectedConsumerRetirement{
        .retired_node = 1,
        .owner_kind = @intFromEnum(ExpectedConsumerRetirement.OwnerKind.expression),
        .original_node_tag = @intFromEnum(Node.Tag.expr_call),
        .original_payload = .{ 0, 0, 0, 0 },
        .kind = @intFromEnum(ExpectedConsumerRetirement.Kind.checker_rewrite_ineligible),
        .retired_consumers_start = 0,
        .retired_consumers_len = 0,
        .diagnostic_index = 2,
        .expected_failures_start = 0,
        .expected_failures_len = 0,
        .rejection_owner_kind = ExpectedConsumerRetirement.none,
        .rejection_owner_index = ExpectedConsumerRetirement.none,
        .rejection_subject_var = ExpectedConsumerRetirement.none,
    };
    try std.testing.expect(retirement.hasCanonicalExpectedFailureRange());
    try std.testing.expect(retirement.hasLegalTags());

    retirement.expected_failures_start = 1;
    try std.testing.expect(!retirement.hasCanonicalExpectedFailureRange());
    try std.testing.expect(!retirement.hasLegalTags());
    retirement.expected_failures_len = 2;
    try std.testing.expect(retirement.hasCanonicalExpectedFailureRange());
    try std.testing.expect(retirement.hasLegalTags());

    retirement.diagnostic_index = ExpectedConsumerRetirement.none;
    try std.testing.expect(!retirement.hasLegalTags());
    retirement.diagnostic_index = 2;
    retirement.reserved_0 = 1;
    try std.testing.expect(!retirement.hasLegalTags());
    retirement.reserved_0 = 0;

    retirement.kind = @intFromEnum(ExpectedConsumerRetirement.Kind.checker_rewrite_expected);
    retirement.retired_consumers_len = 1;
    retirement.expected_failures_start = 0;
    retirement.expected_failures_len = 0;
    try std.testing.expect(retirement.hasLegalTags());
    retirement.expected_failures_start = 4;
    retirement.expected_failures_len = 2;
    try std.testing.expect(retirement.hasLegalTags());

    retirement.kind = @intFromEnum(ExpectedConsumerRetirement.Kind.preexisting_runtime_error);
    retirement.original_node_tag = @intFromEnum(Node.Tag.malformed);
    retirement.retired_consumers_len = 0;
    try std.testing.expect(!retirement.hasLegalTags());
    retirement.expected_failures_start = 0;
    retirement.expected_failures_len = 0;
    try std.testing.expect(retirement.hasLegalTags());

    retirement.owner_kind = @intFromEnum(ExpectedConsumerRetirement.OwnerKind.pattern);
    retirement.kind = @intFromEnum(ExpectedConsumerRetirement.Kind.checker_poison_expected_pattern);
    retirement.original_node_tag = @intFromEnum(Node.Tag.pattern_nominal);
    retirement.retired_consumers_len = 1;
    retirement.diagnostic_index = ExpectedConsumerRetirement.none;
    try std.testing.expect(!retirement.hasLegalTags());
    retirement.expected_failures_start = 7;
    retirement.expected_failures_len = 2;
    try std.testing.expect(retirement.hasLegalTags());
    retirement.diagnostic_index = 2;
    try std.testing.expect(!retirement.hasLegalTags());
}

test "expected retirement rejection owner absence is canonical" {
    var retirement = ExpectedConsumerRetirement{
        .retired_node = 1,
        .owner_kind = @intFromEnum(ExpectedConsumerRetirement.OwnerKind.expression),
        .original_node_tag = @intFromEnum(Node.Tag.expr_call),
        .original_payload = .{ 0, 0, 0, 0 },
        .kind = @intFromEnum(ExpectedConsumerRetirement.Kind.checker_rewrite_ineligible),
        .retired_consumers_start = 0,
        .retired_consumers_len = 0,
        .diagnostic_index = 2,
        .expected_failures_start = 0,
        .expected_failures_len = 0,
        .rejection_owner_kind = ExpectedConsumerRetirement.none,
        .rejection_owner_index = ExpectedConsumerRetirement.none,
        .rejection_subject_var = ExpectedConsumerRetirement.none,
    };
    try std.testing.expect(retirement.hasCanonicalRejectionOwner());
    try std.testing.expect(retirement.hasLegalTags());

    retirement.rejection_owner_index = 0;
    try std.testing.expect(!retirement.hasCanonicalRejectionOwner());
    retirement.rejection_owner_index = ExpectedConsumerRetirement.none;
    retirement.rejection_subject_var = 3;
    try std.testing.expect(!retirement.hasCanonicalRejectionOwner());

    retirement.rejection_owner_kind = @intFromEnum(ExpectedConsumerRetirement.RejectionOwnerKind.rejected_static_dispatch);
    retirement.rejection_owner_index = 0;
    try std.testing.expect(retirement.hasCanonicalRejectionOwner());

    retirement.rejection_owner_kind = @intFromEnum(ExpectedConsumerRetirement.RejectionOwnerKind.expected_ambiguity_retirement);
    try std.testing.expect(retirement.hasCanonicalRejectionOwner());

    retirement.rejection_owner_kind = std.math.maxInt(u32) - 1;
    try std.testing.expect(!retirement.hasCanonicalRejectionOwner());
}

test "expected retirement owner namespace and kind pairing is closed" {
    var retirement = ExpectedConsumerRetirement{
        .retired_node = 1,
        .owner_kind = @intFromEnum(ExpectedConsumerRetirement.OwnerKind.expression),
        .original_node_tag = @intFromEnum(Node.Tag.expr_call),
        .original_payload = .{ 0, 0, 0, 0 },
        .kind = @intFromEnum(ExpectedConsumerRetirement.Kind.checker_rewrite_expected),
        .retired_consumers_start = 0,
        .retired_consumers_len = 1,
        .diagnostic_index = 2,
        .expected_failures_start = 0,
        .expected_failures_len = 0,
        .rejection_owner_kind = ExpectedConsumerRetirement.none,
        .rejection_owner_index = ExpectedConsumerRetirement.none,
        .rejection_subject_var = ExpectedConsumerRetirement.none,
    };
    try std.testing.expect(retirement.hasLegalTags());

    retirement.owner_kind = @intFromEnum(ExpectedConsumerRetirement.OwnerKind.pattern);
    try std.testing.expect(!retirement.hasLegalTags());
    retirement.kind = @intFromEnum(ExpectedConsumerRetirement.Kind.checker_poison_expected_pattern);
    retirement.original_node_tag = @intFromEnum(Node.Tag.pattern_nominal);
    retirement.diagnostic_index = ExpectedConsumerRetirement.none;
    retirement.expected_failures_start = 0;
    retirement.expected_failures_len = 1;
    try std.testing.expect(retirement.hasLegalTags());

    retirement.owner_kind = std.math.maxInt(u32);
    try std.testing.expect(!retirement.hasLegalTags());
    retirement.owner_kind = @intFromEnum(ExpectedConsumerRetirement.OwnerKind.pattern);
    retirement.kind = std.math.maxInt(u32);
    try std.testing.expect(!retirement.hasLegalTags());
    retirement.kind = @intFromEnum(ExpectedConsumerRetirement.Kind.checker_poison_expected_pattern);
    retirement.original_node_tag = std.math.maxInt(u32);
    try std.testing.expect(!retirement.hasLegalTags());

    retirement.original_node_tag = @intFromEnum(Node.Tag.pattern_nominal);
    retirement.retired_consumers_len = 0;
    try std.testing.expect(!retirement.hasLegalTags());
    retirement.retired_consumers_len = 1;
    retirement.expected_failures_start = 0;
    retirement.expected_failures_len = 0;
    try std.testing.expect(!retirement.hasLegalTags());
}

fn dispatchSettlementSourceTestRows() [8]DispatchSettlementSource {
    return .{
        DispatchSettlementSource.dispatchExpr(10, 20, .dispatch_call, 30, 40, 50),
        DispatchSettlementSource.literalConversion(11, 21, 31, 41, .numeral),
        DispatchSettlementSource.interpolation(12, 22, 32, 42),
        DispatchSettlementSource.patternLiteralEquality(13, 23, .numeral, 33, 43),
        DispatchSettlementSource.negatedEqualityNot(14, 24, 34, 44, 54),
        DispatchSettlementSource.forLoopDispatch(15, 25, .iter),
        DispatchSettlementSource.whereRequirement(26),
        DispatchSettlementSource.copiedConstraint(
            17,
            CopiedConstraintComponentRef.rootGraphReceiver(27, 37),
            CopiedConstraintComponentRef.rootGraphFunction(27, 47, 57),
        ),
    };
}

test "copied open literal inventory has closed canonical encodings and order" {
    try std.testing.expectEqual(
        @as(usize, 3 * @sizeOf(u32)),
        @sizeOf(CopiedOpenLiteralComponent),
    );
    try std.testing.expectEqual(
        @as(usize, 11 * @sizeOf(u32)),
        @sizeOf(CopiedOpenLiteralGroup),
    );
    try std.testing.expectEqual(
        @as(usize, 3 * @sizeOf(u32)),
        @sizeOf(CopiedOpenLiteralEvent),
    );
    try std.testing.expectEqual(
        @as(usize, 4),
        std.enums.values(CopiedOpenLiteralComponent.Kind).len,
    );
    try std.testing.expectEqual(
        @as(usize, 3),
        std.enums.values(CopiedOpenLiteralEvent.LiteralKind).len,
    );

    const components = [_]CopiedOpenLiteralComponent{
        CopiedOpenLiteralComponent.rootGraph(),
        CopiedOpenLiteralComponent.schemeRequirement(7),
        CopiedOpenLiteralComponent.bindingCodecReceiver(11, 7),
        CopiedOpenLiteralComponent.bindingCodecFunction(11, 7),
    };
    const component_kinds = [_]CopiedOpenLiteralComponent.Kind{
        .root_graph,
        .scheme_requirement,
        .binding_codec_receiver,
        .binding_codec_function,
    };
    for (components, component_kinds, 0..) |component, expected_kind, index| {
        try std.testing.expectEqual(@as(u32, @intCast(index)), component.kind);
        try std.testing.expectEqual(expected_kind, component.decodedKind().?);
        try std.testing.expect(component.hasCanonicalTags());
        if (index + 1 < components.len) {
            try std.testing.expect(CopiedOpenLiteralComponent.canonicalLessThan(
                component,
                components[index + 1],
            ));
            try std.testing.expect(!CopiedOpenLiteralComponent.canonicalLessThan(
                components[index + 1],
                component,
            ));
        }
    }
    try std.testing.expectEqual(
        @as(u32, 7),
        components[1].decodedSchemeRequirement().?.requirement_ordinal,
    );
    try std.testing.expectEqual(
        @as(u32, 11),
        components[2].decodedBindingCodecReceiver().?.binding_root_step,
    );
    try std.testing.expectEqual(
        @as(u32, 7),
        components[3].decodedBindingCodecFunction().?.requirement_ordinal,
    );
    try std.testing.expectEqual(
        @as(?CopiedOpenLiteralComponent.SchemeRequirement, null),
        components[0].decodedSchemeRequirement(),
    );

    var invalid_component = components[0];
    invalid_component.requirement_ordinal = 1;
    try std.testing.expect(!invalid_component.hasCanonicalTags());
    invalid_component = components[1];
    invalid_component.binding_root_step = 1;
    try std.testing.expect(!invalid_component.hasCanonicalTags());
    invalid_component = components[2];
    invalid_component.binding_root_step = CopiedOpenLiteralComponent.none;
    try std.testing.expect(!invalid_component.hasCanonicalTags());
    invalid_component = components[3];
    invalid_component.kind = @intCast(components.len);
    try std.testing.expectEqual(
        @as(?CopiedOpenLiteralComponent.Kind, null),
        invalid_component.decodedKind(),
    );
    try std.testing.expect(!invalid_component.hasCanonicalTags());

    const groups = [_]CopiedOpenLiteralGroup{
        .{
            .copy_step_index = 3,
            .receiver_occurrence_offset = 4,
            .source_constraints_start = 5,
            .source_constraints_len = 2,
            .destination_constraints_start = 7,
            .destination_constraints_len = 2,
            .component = components[0],
            .events_start = 0,
            .events_len = 1,
        },
        .{
            .copy_step_index = 3,
            .receiver_occurrence_offset = 4,
            .source_constraints_start = 5,
            .source_constraints_len = 2,
            .destination_constraints_start = 7,
            .destination_constraints_len = 2,
            .component = components[1],
            .events_start = 1,
            .events_len = 1,
        },
    };
    for (groups) |group| try std.testing.expect(group.hasCanonicalTags());
    try std.testing.expect(CopiedOpenLiteralGroup.canonicalLessThan(groups[0], groups[1]));
    try std.testing.expect(!CopiedOpenLiteralGroup.canonicalLessThan(groups[1], groups[0]));
    var invalid_group = groups[0];
    invalid_group.destination_constraints_len += 1;
    try std.testing.expect(!invalid_group.hasCanonicalTags());
    invalid_group = groups[0];
    invalid_group.events_len = 0;
    try std.testing.expect(!invalid_group.hasCanonicalTags());

    const events = [_]CopiedOpenLiteralEvent{
        .{ .group_index = 3, .constraint_offset = 0, .literal_kind = @intFromEnum(CopiedOpenLiteralEvent.LiteralKind.numeral) },
        .{ .group_index = 3, .constraint_offset = 1, .literal_kind = @intFromEnum(CopiedOpenLiteralEvent.LiteralKind.quote) },
        .{ .group_index = 4, .constraint_offset = 0, .literal_kind = @intFromEnum(CopiedOpenLiteralEvent.LiteralKind.interpolation) },
    };
    for (events, 0..) |event, index| {
        try std.testing.expect(event.hasCanonicalTags());
        try std.testing.expectEqual(
            std.enums.values(CopiedOpenLiteralEvent.LiteralKind)[index],
            event.decodedLiteralKind().?,
        );
        if (index + 1 < events.len) {
            try std.testing.expect(CopiedOpenLiteralEvent.canonicalLessThan(
                event,
                events[index + 1],
            ));
        }
    }
    var invalid_event = events[0];
    invalid_event.literal_kind = @intCast(events.len);
    try std.testing.expectEqual(
        @as(?CopiedOpenLiteralEvent.LiteralKind, null),
        invalid_event.decodedLiteralKind(),
    );
    try std.testing.expect(!invalid_event.hasCanonicalTags());
}

test "copied open literal pools serialize relocate and mutable-copy" {
    const gpa = std.testing.allocator;
    const groups = [_]CopiedOpenLiteralGroup{
        .{
            .copy_step_index = 0,
            .receiver_occurrence_offset = 0,
            .source_constraints_start = 10,
            .source_constraints_len = 3,
            .destination_constraints_start = 20,
            .destination_constraints_len = 3,
            .component = CopiedOpenLiteralComponent.rootGraph(),
            .events_start = 0,
            .events_len = 2,
        },
        .{
            .copy_step_index = 0,
            .receiver_occurrence_offset = 1,
            .source_constraints_start = 30,
            .source_constraints_len = 1,
            .destination_constraints_start = 40,
            .destination_constraints_len = 1,
            .component = CopiedOpenLiteralComponent.schemeRequirement(2),
            .events_start = 2,
            .events_len = 1,
        },
    };
    const events = [_]CopiedOpenLiteralEvent{
        .{ .group_index = 0, .constraint_offset = 0, .literal_kind = @intFromEnum(CopiedOpenLiteralEvent.LiteralKind.numeral) },
        .{ .group_index = 0, .constraint_offset = 2, .literal_kind = @intFromEnum(CopiedOpenLiteralEvent.LiteralKind.interpolation) },
        .{ .group_index = 1, .constraint_offset = 0, .literal_kind = @intFromEnum(CopiedOpenLiteralEvent.LiteralKind.quote) },
    };

    var original = try Self.init(gpa, "");
    defer original.deinit();
    _ = try original.where_marker_copy_steps.append(gpa, .{
        .kind = @intFromEnum(WhereMarkerCopyStep.Kind.reserved),
        .copy_policy = @intFromEnum(WhereMarkerCopyStep.CopyPolicy.cross_module_import),
        .source_root_var = 1,
        .destination_root_var = 2,
        .pairs_start = 0,
        .pairs_len = 1,
        .copied_groups_start = 0,
        .copied_groups_len = groups.len,
        .origin = .{ .reserved = .{} },
    });
    _ = try original.copied_open_literal_groups.appendSlice(gpa, &groups);
    _ = try original.copied_open_literal_events.appendSlice(gpa, &events);

    var writer = CompactWriter.init();
    defer writer.deinit(gpa);
    const pending = try writer.appendAlloc(gpa, Serialized);
    try pending.serialize(&original, gpa, &writer);

    const buffer = try gpa.alignedAlloc(
        u8,
        CompactWriter.SERIALIZATION_ALIGNMENT,
        writer.total_bytes,
    );
    defer gpa.free(buffer);
    _ = try writer.writeToBuffer(buffer);
    const base_addr = @intFromPtr(buffer.ptr);
    const serialized: *const Serialized = @ptrCast(@alignCast(buffer.ptr));
    try serialized.validate(buffer.len);

    const static_env = try serialized.viewStatic(base_addr, gpa, "", "");
    try std.testing.expectEqual(@as(u32, 0), static_env.where_marker_copy_steps.items.items[0].copied_groups_start);
    try std.testing.expectEqual(@as(u32, groups.len), static_env.where_marker_copy_steps.items.items[0].copied_groups_len);
    try std.testing.expectEqual(
        base_addr + @as(usize, @intCast(serialized.copied_open_literal_groups.offset)),
        @intFromPtr(static_env.copied_open_literal_groups.items.items.ptr),
    );
    try std.testing.expectEqual(
        base_addr + @as(usize, @intCast(serialized.copied_open_literal_events.offset)),
        @intFromPtr(static_env.copied_open_literal_events.items.items.ptr),
    );
    try std.testing.expectEqualSlices(
        u8,
        std.mem.sliceAsBytes(groups[0..]),
        std.mem.sliceAsBytes(static_env.copied_open_literal_groups.items.items),
    );
    try std.testing.expectEqualSlices(
        u8,
        std.mem.sliceAsBytes(events[0..]),
        std.mem.sliceAsBytes(static_env.copied_open_literal_events.items.items),
    );

    const mutable_env = try serialized.deserializeWithMutableTypes(base_addr, gpa, "", "");
    defer {
        mutable_env.deinitCachedModule();
        gpa.destroy(mutable_env);
    }
    try std.testing.expectEqualSlices(
        u8,
        std.mem.sliceAsBytes(groups[0..]),
        std.mem.sliceAsBytes(mutable_env.copied_open_literal_groups.items.items),
    );
    try std.testing.expectEqualSlices(
        u8,
        std.mem.sliceAsBytes(events[0..]),
        std.mem.sliceAsBytes(mutable_env.copied_open_literal_events.items.items),
    );
    _ = try mutable_env.copied_open_literal_groups.append(gpa, groups[0]);
    _ = try mutable_env.copied_open_literal_events.append(gpa, events[0]);
    try std.testing.expectEqual(@as(u64, groups.len + 1), mutable_env.copied_open_literal_groups.len());
    try std.testing.expectEqual(@as(u64, events.len + 1), mutable_env.copied_open_literal_events.len());
    try std.testing.expectEqual(@as(u64, groups.len), static_env.copied_open_literal_groups.len());
    try std.testing.expectEqual(@as(u64, events.len), static_env.copied_open_literal_events.len());

    const relocated_buffer = try gpa.alignedAlloc(
        u8,
        CompactWriter.SERIALIZATION_ALIGNMENT,
        buffer.len,
    );
    defer gpa.free(relocated_buffer);
    @memcpy(relocated_buffer, buffer);
    const relocated_base = @intFromPtr(relocated_buffer.ptr);
    var relocated_env = static_env;
    relocated_env.relocate(@as(isize, @intCast(relocated_base)) - @as(isize, @intCast(base_addr)));
    const relocated_serialized: *const Serialized = @ptrCast(@alignCast(relocated_buffer.ptr));
    try std.testing.expectEqual(
        relocated_base + @as(usize, @intCast(relocated_serialized.copied_open_literal_groups.offset)),
        @intFromPtr(relocated_env.copied_open_literal_groups.items.items.ptr),
    );
    try std.testing.expectEqual(
        relocated_base + @as(usize, @intCast(relocated_serialized.copied_open_literal_events.offset)),
        @intFromPtr(relocated_env.copied_open_literal_events.items.items.ptr),
    );
    try std.testing.expectEqualSlices(
        u8,
        std.mem.sliceAsBytes(groups[0..]),
        std.mem.sliceAsBytes(relocated_env.copied_open_literal_groups.items.items),
    );
    try std.testing.expectEqualSlices(
        u8,
        std.mem.sliceAsBytes(events[0..]),
        std.mem.sliceAsBytes(relocated_env.copied_open_literal_events.items.items),
    );

    var corrupt = serialized.*;
    corrupt.copied_open_literal_groups.len = std.math.maxInt(u64);
    try std.testing.expectError(error.CorruptArtifact, corrupt.validate(buffer.len));
    corrupt = serialized.*;
    corrupt.copied_open_literal_events.offset = @intCast(buffer.len);
    try std.testing.expectError(error.CorruptArtifact, corrupt.validate(buffer.len));
}

test "where-method source and evidence-handle pools serialize relocate and mutable-copy" {
    const gpa = std.testing.allocator;
    try std.testing.expectEqual(
        @as(usize, 7 * @sizeOf(u32)),
        @sizeOf(WhereMethodSource),
    );
    try std.testing.expectEqual(
        @as(usize, 2 * @sizeOf(u32)),
        @sizeOf(types_mod.ConstraintEvidenceHandle),
    );
    try std.testing.expectEqual(
        @as(usize, 4 * @sizeOf(u32)),
        @sizeOf(ConstraintEvidenceMove),
    );

    const source_rows = [_]WhereMethodSource{
        .{
            .owner_node = 10,
            .where_node = 20,
            .method_ident = 30,
            .source_ordinal = 0,
            .retained_constraint_index = 40,
            .source_contracts_start = 0,
            .source_contracts_len = 0,
        },
        .{
            .owner_node = 10,
            .where_node = 21,
            .method_ident = 31,
            .source_ordinal = 1,
            .retained_constraint_index = 41,
            .source_contracts_start = 50,
            .source_contracts_len = 2,
        },
    };
    const handle_rows = [_]types_mod.ConstraintEvidenceHandle{
        .{ .kind = @intFromEnum(types_mod.ConstraintEvidenceHandle.Kind.selected_receiver_anchor), .index = 60 },
        .{ .kind = @intFromEnum(types_mod.ConstraintEvidenceHandle.Kind.copied_literal_event), .index = 61 },
        .{ .kind = @intFromEnum(types_mod.ConstraintEvidenceHandle.Kind.where_requirement_source), .index = 62 },
    };
    const move_rows = [_]ConstraintEvidenceMove{
        .{ .handle = handle_rows[0], .source_constraint_index = 70, .destination_constraint_index = 71 },
        .{ .handle = handle_rows[1], .source_constraint_index = 72, .destination_constraint_index = 73 },
        .{ .handle = handle_rows[2], .source_constraint_index = 74, .destination_constraint_index = 75 },
    };

    try std.testing.expectEqual(@as(u32, 0), source_rows[0].source_ordinal);
    try std.testing.expectEqual(@as(u32, 0), source_rows[0].source_contracts_start);
    try std.testing.expectEqual(@as(u32, 0), source_rows[0].source_contracts_len);
    // SafeRange's start word is explicitly inactive when count is zero; a
    // checker must branch on count before reading it. The serialized source
    // row is a different raw fixed-width schema and canonicalizes both words.
    const inactive_retained_range = types_mod.WhereMethodMarkerContract.SafeList.Range{
        .start = @enumFromInt(0xAAAAAAAA),
        .count = 0,
    };
    try std.testing.expectEqual(@as(u32, 0), inactive_retained_range.len());
    try std.testing.expectEqual(@as(u32, 0), source_rows[0].source_contracts_start);
    try std.testing.expectEqual(@as(u32, 0), source_rows[0].source_contracts_len);
    for (handle_rows, 0..) |handle, expected_tag| {
        try std.testing.expectEqual(@as(u32, @intCast(expected_tag)), handle.kind);
        try std.testing.expectEqual(
            std.enums.values(types_mod.ConstraintEvidenceHandle.Kind)[expected_tag],
            handle.decodedKind().?,
        );
    }

    var original = try Self.init(gpa, "");
    defer original.deinit();
    _ = try original.where_method_sources.appendSlice(gpa, &source_rows);
    _ = try original.types.constraint_evidence_handles.appendSlice(gpa, &handle_rows);
    _ = try original.constraint_evidence_moves.appendSlice(gpa, &move_rows);

    var writer = CompactWriter.init();
    defer writer.deinit(gpa);
    const pending = try writer.appendAlloc(gpa, Serialized);
    try pending.serialize(&original, gpa, &writer);

    const buffer = try gpa.alignedAlloc(
        u8,
        CompactWriter.SERIALIZATION_ALIGNMENT,
        writer.total_bytes,
    );
    defer gpa.free(buffer);
    _ = try writer.writeToBuffer(buffer);
    const base_addr = @intFromPtr(buffer.ptr);
    const serialized: *const Serialized = @ptrCast(@alignCast(buffer.ptr));
    try serialized.validate(buffer.len);

    const static_env = try serialized.viewStatic(base_addr, gpa, "", "");
    try std.testing.expectEqualSlices(
        u8,
        std.mem.sliceAsBytes(source_rows[0..]),
        std.mem.sliceAsBytes(static_env.where_method_sources.items.items),
    );
    try std.testing.expectEqualSlices(
        u8,
        std.mem.sliceAsBytes(handle_rows[0..]),
        std.mem.sliceAsBytes(static_env.types.constraint_evidence_handles.items.items),
    );
    try std.testing.expectEqualSlices(
        u8,
        std.mem.sliceAsBytes(move_rows[0..]),
        std.mem.sliceAsBytes(static_env.constraint_evidence_moves.items.items),
    );

    const mutable_env = try serialized.deserializeWithMutableTypes(base_addr, gpa, "", "");
    defer {
        mutable_env.deinitCachedModule();
        gpa.destroy(mutable_env);
    }
    try std.testing.expectEqualSlices(
        u8,
        std.mem.sliceAsBytes(source_rows[0..]),
        std.mem.sliceAsBytes(mutable_env.where_method_sources.items.items),
    );
    try std.testing.expectEqualSlices(
        u8,
        std.mem.sliceAsBytes(handle_rows[0..]),
        std.mem.sliceAsBytes(mutable_env.types.constraint_evidence_handles.items.items),
    );
    try std.testing.expectEqualSlices(
        u8,
        std.mem.sliceAsBytes(move_rows[0..]),
        std.mem.sliceAsBytes(mutable_env.constraint_evidence_moves.items.items),
    );
    _ = try mutable_env.where_method_sources.append(gpa, source_rows[0]);
    _ = try mutable_env.types.constraint_evidence_handles.append(gpa, handle_rows[0]);
    _ = try mutable_env.constraint_evidence_moves.append(gpa, move_rows[0]);
    try std.testing.expectEqual(@as(u64, source_rows.len + 1), mutable_env.where_method_sources.len());
    try std.testing.expectEqual(@as(u64, handle_rows.len + 1), mutable_env.types.constraint_evidence_handles.len());
    try std.testing.expectEqual(@as(u64, move_rows.len + 1), mutable_env.constraint_evidence_moves.len());
    try std.testing.expectEqual(@as(u64, source_rows.len), static_env.where_method_sources.len());
    try std.testing.expectEqual(@as(u64, handle_rows.len), static_env.types.constraint_evidence_handles.len());
    try std.testing.expectEqual(@as(u64, move_rows.len), static_env.constraint_evidence_moves.len());

    const relocated_buffer = try gpa.alignedAlloc(
        u8,
        CompactWriter.SERIALIZATION_ALIGNMENT,
        buffer.len,
    );
    defer gpa.free(relocated_buffer);
    @memcpy(relocated_buffer, buffer);
    const relocated_base = @intFromPtr(relocated_buffer.ptr);
    var relocated_env = static_env;
    relocated_env.relocate(@as(isize, @intCast(relocated_base)) - @as(isize, @intCast(base_addr)));
    const relocated_serialized: *const Serialized = @ptrCast(@alignCast(relocated_buffer.ptr));
    try std.testing.expectEqual(
        relocated_base + @as(usize, @intCast(relocated_serialized.where_method_sources.offset)),
        @intFromPtr(relocated_env.where_method_sources.items.items.ptr),
    );
    try std.testing.expectEqual(
        relocated_base + @as(usize, @intCast(relocated_serialized.types.constraint_evidence_handles.offset)),
        @intFromPtr(relocated_env.types.constraint_evidence_handles.items.items.ptr),
    );
    try std.testing.expectEqual(
        relocated_base + @as(usize, @intCast(relocated_serialized.constraint_evidence_moves.offset)),
        @intFromPtr(relocated_env.constraint_evidence_moves.items.items.ptr),
    );
    try std.testing.expectEqualSlices(
        u8,
        std.mem.sliceAsBytes(source_rows[0..]),
        std.mem.sliceAsBytes(relocated_env.where_method_sources.items.items),
    );
    try std.testing.expectEqualSlices(
        u8,
        std.mem.sliceAsBytes(handle_rows[0..]),
        std.mem.sliceAsBytes(relocated_env.types.constraint_evidence_handles.items.items),
    );
    try std.testing.expectEqualSlices(
        u8,
        std.mem.sliceAsBytes(move_rows[0..]),
        std.mem.sliceAsBytes(relocated_env.constraint_evidence_moves.items.items),
    );
}

test "dispatch settlement sources have closed fixed-width canonical encodings" {
    try std.testing.expectEqual(
        @as(usize, 6 * @sizeOf(u32)),
        @sizeOf(CopiedConstraintComponentRef),
    );
    try std.testing.expectEqual(
        @as(usize, 13 * @sizeOf(u32)),
        @sizeOf(DispatchSettlementSource.Payload),
    );
    try std.testing.expectEqual(
        @as(usize, 14 * @sizeOf(u32)),
        @sizeOf(DispatchSettlementSource),
    );
    try std.testing.expectEqual(
        @as(usize, 8),
        std.enums.values(DispatchSettlementSource.Kind).len,
    );
    try std.testing.expectEqual(
        @as(usize, 4),
        std.enums.values(CopiedConstraintComponentRef.Kind).len,
    );

    const expected_source_tags = [_]DispatchSettlementSource.Kind{
        .dispatch_expr,
        .literal_conversion,
        .interpolation,
        .pattern_literal_equality,
        .negated_equality_not,
        .for_loop_dispatch,
        .where_requirement,
        .copied_constraint,
    };
    const rows = dispatchSettlementSourceTestRows();
    for (rows, expected_source_tags, 0..) |row, expected_kind, index| {
        try std.testing.expectEqual(@as(u32, @intCast(index)), row.kind);
        try std.testing.expectEqual(expected_kind, row.decodedKind().?);
        try std.testing.expect(row.hasCanonicalTags());
        if (index + 1 < rows.len) {
            try std.testing.expect(DispatchSettlementSource.canonicalLessThan(
                row,
                rows[index + 1],
            ));
        }
    }
    try std.testing.expectEqual(DispatchSettlementSource.OriginalNodeKind.dispatch_call, rows[0].decodedOriginalNodeKind().?);
    try std.testing.expectEqual(DispatchSettlementSource.LiteralKind.numeral, rows[1].decodedLiteralKind().?);
    try std.testing.expectEqual(DispatchSettlementSource.LiteralKind.numeral, rows[3].decodedPatternLiteralKind().?);
    try std.testing.expectEqual(DispatchSettlementSource.ForLoopSlot.iter, rows[5].decodedForLoopSlot().?);
    try std.testing.expectEqual(@as(?u32, null), rows[6].primaryAnchorIndex());
    try std.testing.expectEqual(@as(?u32, 26), rows[6].whereSourceIndex());
    try std.testing.expectEqual(@as(?u32, 17), rows[7].primaryAnchorIndex());

    // The last word is reserved in every non-copied payload arm.
    for (rows[0..7]) |row| {
        var invalid = row;
        std.mem.asBytes(&invalid.payload)[@sizeOf(DispatchSettlementSource.Payload) - 1] = 1;
        try std.testing.expect(!invalid.hasCanonicalTags());
    }

    var invalid_source_tag = rows[0];
    invalid_source_tag.kind = @intCast(expected_source_tags.len);
    try std.testing.expectEqual(@as(?DispatchSettlementSource.Kind, null), invalid_source_tag.decodedKind());
    try std.testing.expect(!invalid_source_tag.hasCanonicalTags());

    var invalid_original_kind = rows[0];
    invalid_original_kind.payload.dispatch_expr.original_node_kind = std.math.maxInt(u32);
    try std.testing.expectEqual(
        @as(?DispatchSettlementSource.OriginalNodeKind, null),
        invalid_original_kind.decodedOriginalNodeKind(),
    );
    try std.testing.expect(!invalid_original_kind.hasCanonicalTags());

    const component_rows = [_]CopiedConstraintComponentRef{
        CopiedConstraintComponentRef.rootGraphReceiver(1, 2),
        CopiedConstraintComponentRef.rootGraphFunction(1, 3, 4),
        CopiedConstraintComponentRef.schemeRequirementReceiver(5, 6, 7),
        CopiedConstraintComponentRef.schemeRequirementFunction(5, 8, 9, 7),
        CopiedConstraintComponentRef.bindingCodecReceiver(10, 11, 12, 13),
        CopiedConstraintComponentRef.bindingCodecFunction(14, 15, 16, 12, 13),
    };
    const component_roles = [_]CopiedConstraintComponentRef.Role{
        .receiver,
        .function,
        .receiver,
        .function,
        .receiver,
        .function,
    };
    for (component_rows, component_roles) |component, role| {
        try std.testing.expect(component.hasCanonicalTags(role));
    }

    var invalid_component = component_rows[0];
    invalid_component.constraint_pair_offset = 1;
    try std.testing.expect(!invalid_component.hasCanonicalTags(.receiver));
    invalid_component = component_rows[1];
    invalid_component.requirement_ordinal = 1;
    try std.testing.expect(!invalid_component.hasCanonicalTags(.function));
    invalid_component = component_rows[4];
    try std.testing.expect(!invalid_component.hasCanonicalTags(.function));

    var invalid_pair = DispatchSettlementSource.copiedConstraint(
        1,
        CopiedConstraintComponentRef.rootGraphReceiver(2, 3),
        CopiedConstraintComponentRef.rootGraphFunction(4, 5, 6),
    );
    try std.testing.expect(!invalid_pair.hasCanonicalTags());
    invalid_pair = DispatchSettlementSource.copiedConstraint(
        1,
        CopiedConstraintComponentRef.schemeRequirementReceiver(2, 3, 4),
        CopiedConstraintComponentRef.schemeRequirementFunction(2, 5, 6, 7),
    );
    try std.testing.expect(!invalid_pair.hasCanonicalTags());
    invalid_pair = DispatchSettlementSource.copiedConstraint(
        1,
        CopiedConstraintComponentRef.bindingCodecReceiver(2, 3, 4, 5),
        CopiedConstraintComponentRef.bindingCodecFunction(6, 7, 8, 4, 9),
    );
    try std.testing.expect(!invalid_pair.hasCanonicalTags());

    var later = rows;
    later[0].payload.dispatch_expr.method_ident += 1;
    later[1].payload.literal_conversion.literal_kind = @intFromEnum(DispatchSettlementSource.LiteralKind.quote);
    later[2].payload.interpolation.constraint_fn_var += 1;
    later[3].payload.pattern_literal_equality.literal_kind = @intFromEnum(DispatchSettlementSource.LiteralKind.quote);
    later[4].payload.negated_equality_not.not_fn_var += 1;
    later[5].payload.for_loop_dispatch.slot = @intFromEnum(DispatchSettlementSource.ForLoopSlot.next);
    later[6].payload.where_requirement.source_index += 1;
    later[7].payload.copied_constraint.function_component_ref.constraint_pair_offset += 1;
    for (rows, later) |before, after| {
        try std.testing.expect(after.hasCanonicalTags());
        try std.testing.expect(DispatchSettlementSource.canonicalLessThan(before, after));
        try std.testing.expect(!DispatchSettlementSource.canonicalLessThan(after, before));
    }
}

test "dispatch settlement source pool serializes relocates and mutable-copies" {
    const gpa = std.testing.allocator;
    const rows = dispatchSettlementSourceTestRows();

    var original = try Self.init(gpa, "");
    defer original.deinit();
    try std.testing.expectEqual(@as(u64, 0), original.dispatch_settlement_sources.len());
    _ = try original.dispatch_settlement_sources.appendSlice(gpa, &rows);

    var writer = CompactWriter.init();
    defer writer.deinit(gpa);
    const pending = try writer.appendAlloc(gpa, Serialized);
    try pending.serialize(&original, gpa, &writer);

    const buffer = try gpa.alignedAlloc(
        u8,
        CompactWriter.SERIALIZATION_ALIGNMENT,
        writer.total_bytes,
    );
    defer gpa.free(buffer);
    _ = try writer.writeToBuffer(buffer);
    const base_addr = @intFromPtr(buffer.ptr);
    const serialized: *const Serialized = @ptrCast(@alignCast(buffer.ptr));
    try serialized.validate(buffer.len);

    const static_env = try serialized.viewStatic(base_addr, gpa, "", "");
    try std.testing.expectEqual(
        base_addr + @as(usize, @intCast(serialized.dispatch_settlement_sources.offset)),
        @intFromPtr(static_env.dispatch_settlement_sources.items.items.ptr),
    );
    try std.testing.expectEqualSlices(
        u8,
        std.mem.sliceAsBytes(rows[0..]),
        std.mem.sliceAsBytes(static_env.dispatch_settlement_sources.items.items),
    );

    const mutable_env = try serialized.deserializeWithMutableTypes(base_addr, gpa, "", "");
    defer {
        mutable_env.deinitCachedModule();
        gpa.destroy(mutable_env);
    }
    try std.testing.expectEqualSlices(
        u8,
        std.mem.sliceAsBytes(rows[0..]),
        std.mem.sliceAsBytes(mutable_env.dispatch_settlement_sources.items.items),
    );
    _ = try mutable_env.dispatch_settlement_sources.append(gpa, rows[0]);
    try std.testing.expectEqual(@as(u64, rows.len + 1), mutable_env.dispatch_settlement_sources.len());
    try std.testing.expectEqual(@as(u64, rows.len), static_env.dispatch_settlement_sources.len());

    const relocated_buffer = try gpa.alignedAlloc(
        u8,
        CompactWriter.SERIALIZATION_ALIGNMENT,
        buffer.len,
    );
    defer gpa.free(relocated_buffer);
    @memcpy(relocated_buffer, buffer);
    const relocated_base = @intFromPtr(relocated_buffer.ptr);
    var relocated_env = static_env;
    relocated_env.relocate(@as(isize, @intCast(relocated_base)) - @as(isize, @intCast(base_addr)));
    const relocated_serialized: *const Serialized = @ptrCast(@alignCast(relocated_buffer.ptr));
    try std.testing.expectEqual(
        relocated_base + @as(usize, @intCast(relocated_serialized.dispatch_settlement_sources.offset)),
        @intFromPtr(relocated_env.dispatch_settlement_sources.items.items.ptr),
    );
    try std.testing.expectEqualSlices(
        u8,
        std.mem.sliceAsBytes(rows[0..]),
        std.mem.sliceAsBytes(relocated_env.dispatch_settlement_sources.items.items),
    );

    var corrupt = serialized.*;
    corrupt.dispatch_settlement_sources.len = std.math.maxInt(u64);
    try std.testing.expectError(error.CorruptArtifact, corrupt.validate(buffer.len));
    corrupt = serialized.*;
    corrupt.dispatch_settlement_sources.offset = @intCast(buffer.len);
    try std.testing.expectError(error.CorruptArtifact, corrupt.validate(buffer.len));
}

test "for-loop dispatch outcomes have closed canonical encodings" {
    try std.testing.expectEqual(
        @as(usize, 3 * @sizeOf(u32)),
        @sizeOf(ForLoopDispatchOutcome),
    );
    try std.testing.expectEqual(
        @as(usize, 2),
        std.enums.values(ForLoopDispatchOutcome.Kind).len,
    );

    const live = ForLoopDispatchOutcome.liveConstraint(11, 12);
    try std.testing.expectEqual(@as(u32, 0), live.kind);
    try std.testing.expectEqual(ForLoopDispatchOutcome.Kind.live_constraint, live.decodedKind().?);
    try std.testing.expectEqual(@as(?u32, 11), live.constraintIndex());
    try std.testing.expectEqual(@as(?u32, 12), live.anchorIndex());
    try std.testing.expectEqual(@as(?u32, null), live.rejectionIndex());
    try std.testing.expect(live.hasCanonicalTags());

    const rejected = ForLoopDispatchOutcome.rejectedBeforeConstraint(13);
    try std.testing.expectEqual(@as(u32, 1), rejected.kind);
    try std.testing.expectEqual(ForLoopDispatchOutcome.Kind.rejected_before_constraint, rejected.decodedKind().?);
    try std.testing.expectEqual(@as(?u32, null), rejected.constraintIndex());
    try std.testing.expectEqual(@as(?u32, null), rejected.anchorIndex());
    try std.testing.expectEqual(@as(?u32, 13), rejected.rejectionIndex());
    try std.testing.expect(rejected.hasCanonicalTags());

    var invalid = live;
    invalid.kind = @intCast(std.enums.values(ForLoopDispatchOutcome.Kind).len);
    try std.testing.expectEqual(@as(?ForLoopDispatchOutcome.Kind, null), invalid.decodedKind());
    try std.testing.expect(!invalid.hasCanonicalTags());

    invalid = live;
    invalid.first = ForLoopDispatchOutcome.none;
    try std.testing.expect(!invalid.hasCanonicalTags());
    invalid = live;
    invalid.second = ForLoopDispatchOutcome.none;
    try std.testing.expect(!invalid.hasCanonicalTags());

    invalid = rejected;
    invalid.first = ForLoopDispatchOutcome.none;
    try std.testing.expect(!invalid.hasCanonicalTags());
    invalid = rejected;
    invalid.second = 1;
    try std.testing.expect(!invalid.hasCanonicalTags());
}

fn forLoopDispatchPlanTestRows() [2]ForLoopDispatchPlan {
    return .{
        .{
            .node_idx = 10,
            .pattern_idx = 11,
            .iterable_idx = 12,
            .iterator_var = 13,
            .step_var = 14,
            .iter_fn_var = 15,
            .next_fn_var = 16,
            .step_topology = .{
                .done_tag_ident = 17,
                .one_tag_ident = 18,
                .skip_tag_ident = 19,
                .item_field_ident = 20,
                .rest_field_ident = 21,
                .one_payload_var = 22,
                .skip_payload_var = 23,
            },
            .iter_outcome = ForLoopDispatchOutcome.liveConstraint(24, 25),
            .next_outcome = ForLoopDispatchOutcome.rejectedBeforeConstraint(26),
        },
        .{
            .node_idx = 30,
            .pattern_idx = 31,
            .iterable_idx = 32,
            .iterator_var = 33,
            .step_var = 34,
            .iter_fn_var = 35,
            .next_fn_var = 36,
            .step_topology = .{
                .done_tag_ident = 37,
                .one_tag_ident = 38,
                .skip_tag_ident = 39,
                .item_field_ident = 40,
                .rest_field_ident = 41,
                .one_payload_var = 42,
                .skip_payload_var = 43,
            },
            .iter_outcome = ForLoopDispatchOutcome.rejectedBeforeConstraint(44),
            .next_outcome = ForLoopDispatchOutcome.liveConstraint(45, 46),
        },
    };
}

test "for-loop dispatch plan pool serializes relocates and mutable-copies" {
    const gpa = std.testing.allocator;
    const rows = forLoopDispatchPlanTestRows();

    try std.testing.expectEqual(
        @as(usize, 7 * @sizeOf(u32)),
        @sizeOf(IteratorStepTopology),
    );
    try std.testing.expectEqual(
        @as(usize, 20 * @sizeOf(u32)),
        @sizeOf(ForLoopDispatchPlan),
    );

    var original = try Self.init(gpa, "");
    defer original.deinit();
    try std.testing.expectEqual(@as(u64, 0), original.for_loop_dispatch_plans.len());
    _ = try original.for_loop_dispatch_plans.appendSlice(gpa, &rows);

    var writer = CompactWriter.init();
    defer writer.deinit(gpa);
    const pending = try writer.appendAlloc(gpa, Serialized);
    try pending.serialize(&original, gpa, &writer);

    const buffer = try gpa.alignedAlloc(
        u8,
        CompactWriter.SERIALIZATION_ALIGNMENT,
        writer.total_bytes,
    );
    defer gpa.free(buffer);
    _ = try writer.writeToBuffer(buffer);
    const base_addr = @intFromPtr(buffer.ptr);
    const serialized: *const Serialized = @ptrCast(@alignCast(buffer.ptr));
    try serialized.validate(buffer.len);

    const static_env = try serialized.viewStatic(base_addr, gpa, "", "");
    try std.testing.expectEqual(
        base_addr + @as(usize, @intCast(serialized.for_loop_dispatch_plans.offset)),
        @intFromPtr(static_env.for_loop_dispatch_plans.items.items.ptr),
    );
    try std.testing.expectEqualSlices(
        u8,
        std.mem.sliceAsBytes(rows[0..]),
        std.mem.sliceAsBytes(static_env.for_loop_dispatch_plans.items.items),
    );

    const mutable_env = try serialized.deserializeWithMutableTypes(base_addr, gpa, "", "");
    defer {
        mutable_env.deinitCachedModule();
        gpa.destroy(mutable_env);
    }
    try std.testing.expectEqualSlices(
        u8,
        std.mem.sliceAsBytes(rows[0..]),
        std.mem.sliceAsBytes(mutable_env.for_loop_dispatch_plans.items.items),
    );
    _ = try mutable_env.for_loop_dispatch_plans.append(gpa, rows[0]);
    try std.testing.expectEqual(@as(u64, rows.len + 1), mutable_env.for_loop_dispatch_plans.len());
    try std.testing.expectEqual(@as(u64, rows.len), static_env.for_loop_dispatch_plans.len());

    const relocated_buffer = try gpa.alignedAlloc(
        u8,
        CompactWriter.SERIALIZATION_ALIGNMENT,
        buffer.len,
    );
    defer gpa.free(relocated_buffer);
    @memcpy(relocated_buffer, buffer);
    const relocated_base = @intFromPtr(relocated_buffer.ptr);
    var relocated_env = static_env;
    relocated_env.relocate(@as(isize, @intCast(relocated_base)) - @as(isize, @intCast(base_addr)));
    const relocated_serialized: *const Serialized = @ptrCast(@alignCast(relocated_buffer.ptr));
    try std.testing.expectEqual(
        relocated_base + @as(usize, @intCast(relocated_serialized.for_loop_dispatch_plans.offset)),
        @intFromPtr(relocated_env.for_loop_dispatch_plans.items.items.ptr),
    );
    try std.testing.expectEqualSlices(
        u8,
        std.mem.sliceAsBytes(rows[0..]),
        std.mem.sliceAsBytes(relocated_env.for_loop_dispatch_plans.items.items),
    );

    var corrupt = serialized.*;
    corrupt.for_loop_dispatch_plans.len = std.math.maxInt(u64);
    try std.testing.expectError(error.CorruptArtifact, corrupt.validate(buffer.len));
    corrupt = serialized.*;
    corrupt.for_loop_dispatch_plans.offset = @intCast(buffer.len);
    try std.testing.expectError(error.CorruptArtifact, corrupt.validate(buffer.len));
}
