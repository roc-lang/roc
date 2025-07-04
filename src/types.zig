const types = @import("./types/types.zig");
// TODO: refactor this to just be types.zig
// Get rid of all of the reexporting. types/types should not exist.
pub const Content = types.Content;
pub const FlatType = types.FlatType;
pub const Num = types.Num;
pub const RecordField = types.RecordField;
pub const Tag = types.Tag;
pub const Tuple = types.Tuple;
pub const Var = types.Var;

pub const store = @import("./types/store.zig");
pub const writers = @import("./types/writers.zig");

pub const Store = store.Store;
