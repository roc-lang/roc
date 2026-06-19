//! Vendored LLVM IR builder/bitcode library (see the individual files).
pub const Builder = @import("Builder.zig");
pub const ir = @import("ir.zig");
pub const bindings = @import("bindings.zig");
pub const bitcode_writer = @import("bitcode_writer.zig");
pub const BitcodeReader = @import("BitcodeReader.zig");
