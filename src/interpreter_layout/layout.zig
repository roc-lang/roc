//! Memory layout representations for values in running Roc programs.
//!
//! The interpreter shares one canonical `Layout` representation with the
//! compiler: every type here is re-exported from the `layout` module so the
//! interpreter's layout store and the compiler's layout store can never disagree
//! on how a value is represented in memory. Only the interpreter's *driver* (the
//! `fromTypeVar`-based store in `store.zig`) is separate; the types are shared.

const base = @import("base");
const Ident = base.Ident;

const canonical = @import("layout");

pub const Layout = canonical.Layout;
pub const LayoutTag = canonical.LayoutTag;
pub const LayoutData = canonical.LayoutData;
pub const Idx = canonical.Idx;
pub const Scalar = canonical.Scalar;
pub const ScalarTag = canonical.ScalarTag;
pub const ScalarData = canonical.ScalarData;
pub const Closure = canonical.Closure;

// Unified struct types (records and tuples are both structs at the layout level)
pub const StructField = canonical.StructField;
pub const StructLayout = canonical.StructLayout;
pub const StructIdx = canonical.StructIdx;
pub const StructData = canonical.StructData;
// Backwards-compat aliases
pub const RecordField = canonical.RecordField;
pub const RecordLayout = canonical.RecordLayout;
pub const RecordIdx = canonical.RecordIdx;
pub const RecordData = canonical.RecordData;
pub const TupleField = canonical.TupleField;
pub const TupleFieldLayout = canonical.TupleFieldLayout;
pub const TupleLayout = canonical.TupleLayout;
pub const TupleIdx = canonical.TupleIdx;
pub const TupleData = canonical.TupleData;
pub const TagUnionLayout = canonical.TagUnionLayout;
pub const TagUnionIdx = canonical.TagUnionIdx;
pub const TagUnionData = canonical.TagUnionData;
pub const TagUnionVariant = canonical.TagUnionVariant;
pub const ClosureLayout = canonical.ClosureLayout;
pub const RocAlignment = canonical.RocAlignment;
pub const SortKey = canonical.SortKey;
pub const WidthValues = canonical.WidthValues;
pub const SizeAlign = canonical.SizeAlign;

// Re-export Info types
pub const ListInfo = canonical.ListInfo;
pub const BoxInfo = canonical.BoxInfo;
pub const StructInfo = canonical.StructInfo;
// Backwards-compat aliases
pub const RecordInfo = canonical.RecordInfo;
pub const TupleInfo = canonical.TupleInfo;
pub const TagUnionInfo = canonical.TagUnionInfo;
pub const ScalarInfo = canonical.ScalarInfo;

/// Field name plus the module that owns the Ident.Idx.
///
/// Interpreter-only: used solely to resolve the alphabetical tiebreak when the
/// layout store sorts a record's fields by alignment. It is never stored on a
/// committed `StructField` (the canonical shape has no name), only carried in the
/// store's transient sort entries.
pub const FieldName = struct {
    module_idx: u32,
    ident: Ident.Idx,
};
