// This is a glue package that just re-exports other libs useful for zig hosts.
// Long term, a slimmed down version of these libraries without all of the roc builtins should be create via `roc glue`.
// We also should make RocList use comptime types in order to make it nice to use in zig.

pub const dec = @import("dec.zig");
pub const list = @import("list.zig");
pub const str = @import("str.zig");
