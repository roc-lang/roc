//! Lifted monotype program uses the same type graph as the monotype stage.

const mono_type = @import("../mono/mod.zig").Type;

pub const TypeId = mono_type.TypeId;
pub const TypeIds = mono_type.TypeIds;
pub const Prim = mono_type.Prim;
pub const Tag = mono_type.Tag;
pub const Tags = mono_type.Tags;
pub const Field = mono_type.Field;
pub const Fields = mono_type.Fields;
pub const Content = mono_type.Content;
pub const Store = mono_type.Store;
