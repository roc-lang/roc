//! `.boxy` runtime lowering: the layout-polymorphic strategy described in
//! design.md's "`.boxy` Runtime Lowering" section.

pub const Lower = @import("lower.zig");
pub const Types = @import("types.zig");
pub const RowJoins = @import("row_joins.zig");
pub const Layouts = @import("layouts.zig");
pub const StaticDescs = @import("static_descs.zig");
pub const Body = @import("body.zig");
pub const Match = @import("match.zig");
pub const LiteralValues = @import("literal_values.zig").LiteralValues;

pub const run = Lower.run;
pub const Input = Lower.Input;
pub const Output = Lower.Output;

test "boxy declarations are referenced" {
    @import("std").testing.refAllDecls(@This());
}
