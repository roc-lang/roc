const std = @import("std");

/// A simple, linear-time string hash.
///
/// http://isthe.com/chongo/tech/comp/fnv/#FNV-1
pub fn fnvStringHash(string: []const u8) u32 {
    const fnv_prime_32_bit: u32 = 16777619;
    const offset_basis_32_bit: u32 = 2166136261;

    var hash = offset_basis_32_bit;

    for (string) |byte| {
        hash *= fnv_prime_32_bit;
        hash ^= @as(u32, byte);
    }

    return hash;
}

/// Exit the current process when we hit an out-of-memory error.
///
/// Since there's nothing we can do to recover from such an issue,
/// it's best to always exit the process with a nice message than to
/// propagate unrecoverable errors all over the compiler.
pub fn exitOnOom() noreturn {
    const oom_message =
        \\I ran out of memory! I can't do anything to recover, so I'm exiting.
        \\Try reducing memory usage on your machine and then running again.
    ;

    std.debug.print(oom_message, .{});
    std.process.exit(1);
}
