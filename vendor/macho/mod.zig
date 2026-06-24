//! Vendored Mach-O code-signing helpers (see CodeSignature.zig).
pub const CodeSignature = @import("CodeSignature.zig");
pub const ParallelHasher = @import("hasher.zig").ParallelHasher;
