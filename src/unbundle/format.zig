//! Shared on-disk format constants for bundled Roc packages (`.tar.zst`
//! archives). `unbundle` is the lower, WebAssembly-compatible layer that `bundle`
//! depends on, so it owns the single definition of these constants; the `bundle`
//! writer references them through the `unbundle` module.

/// File extension for a bundled Roc package: a zstd-compressed tar archive.
pub const TAR_EXTENSION = ".tar.zst";

/// Size of the buffer used for streaming bundle/unbundle operations, in bytes.
pub const STREAM_BUFFER_SIZE: usize = 64 * 1024;
