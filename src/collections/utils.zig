const std = @import("std");

/// Exit the current process when we hit an out-of-memory error.
///
/// Since there's nothing we can do to recover from such an issue,
/// it's best to always exit the process with a nice message than to
/// propagate unrecoverable errors all over the compiler.
pub fn exitOnOom(err: std.mem.Allocator.Error) noreturn {
    switch (err) {
        error.OutOfMemory => {
            const oom_message =
                \\I ran out of memory! I can't do anything to recover, so I'm exiting.
                \\Try reducing memory usage on your machine and then running again.
            ;

            std.debug.print(oom_message, .{});
            std.process.exit(1);
        },
    }
}
