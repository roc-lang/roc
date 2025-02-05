const std = @import("std");
const interner = @import("collections/string_interner.zig");
const name = @import("collections/name_interner.zig");
const safe_list = @import("collections/safe_list.zig");

pub const SmallStringInterner = interner.SmallStringInterner;
pub const SmallStringId = interner.SmallStringId;
pub const LargeStringInterner = interner.LargeStringInterner;
pub const LargeStringId = interner.LargeStringId;

pub const TagNameId = name.TagNameId;
pub const TagNameInterner = name.TagNameInterner;
pub const FieldNameId = name.FieldNameId;
pub const FieldNameInterner = name.FieldNameInterner;

pub const SafeList = safe_list.SafeList;
pub const SafeMultiList = safe_list.SafeMultiList;

pub fn exit_on_oom() noreturn {
    const oom_message =
        \\I ran out of memory! I can't do anything to recover, so I'm exiting.
        \\Try reducing memory usage on your machine and then running again.
    ;

    std.debug.print(oom_message, .{});
    std.process.exit(1);
}
