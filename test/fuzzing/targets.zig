//! Every fuzz target, one declaration per `fuzz-<name>.zig` file, named as
//! build.zig's `fuzz_target_names` names it. `fuzz-afl.zig` builds one of them
//! for AFL++, and `fuzz-repro.zig` dispatches to any of them by name.

pub const tokenize = @import("fuzz-tokenize.zig");
pub const parse = @import("fuzz-parse.zig");
pub const format = @import("fuzz-format.zig");
pub const canonicalize = @import("fuzz-canonicalize.zig");
pub const typecheck = @import("fuzz-typecheck.zig");
pub const build = @import("fuzz-build.zig");
pub const @"build-errors" = @import("fuzz-build-errors.zig");
