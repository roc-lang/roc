//! Compatibility wrapper exposing the staged monotype system through `mir`.

const staged = @import("corecir").Monotype;

pub const Idx = staged.Idx;
pub const Span = staged.Span;
pub const Monotype = staged.Monotype;
pub const Prim = staged.Prim;
pub const Tag = staged.Tag;
pub const TagSpan = staged.TagSpan;
pub const Field = staged.Field;
pub const Name = staged.Name;
pub const FieldSpan = staged.FieldSpan;
pub const Store = staged.Store;
