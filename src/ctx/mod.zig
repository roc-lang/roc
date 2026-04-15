//! Unified context for the Roc compiler.
//!
//! This module provides allocators, I/O, and other cross-cutting concerns
//! in a single context object, allowing easy testing and alternative
//! implementations (e.g. WASM playground, in-memory test mocks).

pub const CoreCtx = @import("CoreCtx.zig");
