const types = @import("./types/types.zig");
// TODO: refactor this to just be types.zig
// Get rid of all of the reexporting. types.zig and types/types.zig should not exist separately.
pub const Alias = types.Alias;
pub const Content = types.Content;
pub const FlatType = types.FlatType;
pub const Func = types.Func;
pub const NominalType = types.NominalType;
pub const Num = types.Num;
pub const Record = types.Record;
pub const RecordField = types.RecordField;
pub const Tag = types.Tag;
pub const TagUnion = types.TagUnion;
pub const Tuple = types.Tuple;
pub const Var = types.Var;

pub const store = @import("./types/store.zig");
pub const writers = @import("./types/writers.zig");

pub const Store = store.Store;
