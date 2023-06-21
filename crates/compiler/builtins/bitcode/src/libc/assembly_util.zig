const builtin = @import("builtin");
const os = builtin.os;

pub const function_prefix = switch (os.tag) {
    .macos => "_",
    else => "",
};
