//! Minimal module surface needed by the standalone Boxy runtime artifact.
//!
//! Keeping this separate from the full eval module lets build-time target
//! runtime artifacts depend on the Boxy implementation without depending on
//! evaluator-only embedded artifacts in the opposite direction.

pub const boxy_abi = @import("boxy_abi.zig");
pub const boxy_runtime = @import("boxy_runtime.zig");
