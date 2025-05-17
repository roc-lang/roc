const store = @import("./types/store.zig");
const types = @import("./types/types.zig");

/// Type Store
pub const Store = store.Store;

/// Type Slot
pub const Slot = store.Slot;

/// Type Var
pub const Var = types.Var;

/// Type Desc
pub const Desc = types.Descriptor;

/// Type Rank
pub const Rank = types.Rank;

/// Type Mark
pub const Mark = types.Mark;

/// Type Content
pub const Content = types.Content;

/// Type Alias
pub const Alias = types.Alias;

/// Type FlatType
pub const FlatType = types.FlatType;

/// Type TypeApply
pub const TypeApply = types.Builtin;

/// Type Tuple
pub const Tuple = types.Tuple;

/// Type Num
pub const Num = types.Num;

/// Type Func
pub const Func = types.Func;

/// Type Record
pub const Record = types.Record;

/// Type RecordField
pub const RecordField = types.RecordField;

/// Type TagUnion
pub const TagUnion = types.TagUnion;

/// Type Tag
pub const Tag = types.Tag;
