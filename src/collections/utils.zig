const std = @import("std");

/// A simple string hash.
///
/// http://isthe.com/chongo/tech/comp/fnv/#FNV-1
pub fn fnvStringHash(string: []const u8) u32 {
    const FNV_PRIME_32_BIT: u32 = 16777619;
    const OFFSET_BASIS_32_BIT: u32 = 2166136261;

    var hash = OFFSET_BASIS_32_BIT;

    for (string) |byte| {
        hash *= FNV_PRIME_32_BIT;
        hash ^= @as(u32, byte);
    }

    return hash;
}

pub fn exit_on_oom() noreturn {
    const oom_message =
        \\I ran out of memory! I can't do anything to recover, so I'm exiting.
        \\Try reducing memory usage on your machine and then running again.
    ;

    std.debug.print(oom_message, .{});
    std.process.exit(1);
}
