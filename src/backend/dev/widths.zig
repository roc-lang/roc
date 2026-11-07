//! The register widths the dev backend's shared driver uses, named by the
//! kind of value they hold rather than by their size.
//!
//! The driver serves every word size. On an 8-byte-word ABI every name
//! below is `.w64`; on a 4-byte-word ABI only `word_width` has a value.
//! The names are declarations of the struct that `Widths` returns, and Zig
//! resolves a container's declarations only when they are used, so
//! `wide64_width` and `scalar_width` are compile errors exactly where a
//! function that uses them is analyzed for a 4-byte-word ABI. A user
//! writes `const widths = Widths(machine_abi);` and reads `widths.word_width`
//! directly; copying a name into a local `const` would resolve it eagerly.
//!
//! Rules:
//! - A width follows the kind of value, never an immediate's numeric value:
//!   a small U64 is still 64 bits.
//! - A 32-bit instruction set's `RegisterWidth` has no `w64` member, so
//!   `wide64_width`, `scalar_width` and a stray `.w64` are compile errors on
//!   it until the driver routes those values through register pairs.

const std = @import("std");
const isa = @import("isa.zig");
const Abi = isa.Abi;
const x86_64 = @import("x86_64/mod.zig");
const aarch64 = @import("aarch64/mod.zig");

/// The register widths of `machine_abi`'s instruction set, named by kind of
/// value.
pub fn Widths(comptime machine_abi: Abi) type {
    return struct {
        /// The instruction set's own register width type.
        pub const RegisterWidth = switch (machine_abi.isa()) {
            .x86_64 => x86_64.RegisterWidth,
            .aarch64 => aarch64.RegisterWidth,
        };

        /// The number of bits in one target word.
        pub const word_bits: comptime_int = machine_abi.ptrBitWidth();

        /// One target word: a pointer, a length, a size, an address.
        pub const word_width: RegisterWidth = switch (word_bits) {
            64 => .w64,
            32 => .w32,
            else => unreachable, // `Abi.ptrBitWidth` returns only 32 or 64
        };

        /// A value that is 64 bits on every target: a U64, an I64, the bits
        /// of an F64, a half of an i128 or a Dec.
        pub const wide64_width: RegisterWidth = if (word_bits == 64)
            .w64
        else
            @compileError("a 64-bit value needs a register pair on a 4-byte-word ABI");

        /// An integer or bit pattern of up to 8 bytes held in one register,
        /// whose width follows its layout.
        pub const scalar_width: RegisterWidth = if (word_bits == 64)
            .w64
        else
            @compileError("a scalar of up to 8 bytes in one register; route 8-byte values to a pair first, then use `word_width`");

        /// The number of bits in the register that holds a scalar.
        pub const scalar_bits: comptime_int = if (word_bits == 64)
            64
        else
            @compileError("a scalar of up to 8 bytes in one register; route 8-byte values to a pair first, then use `word_bits`");
    };
}

test "every width is w64 and every bit count 64 on the x86_64 and aarch64 ABIs" {
    inline for (@typeInfo(Abi).@"enum".field_names) |name| {
        const widths = Widths(@field(Abi, name));
        try std.testing.expectEqual(widths.RegisterWidth.w64, widths.word_width);
        try std.testing.expectEqual(widths.RegisterWidth.w64, widths.wide64_width);
        try std.testing.expectEqual(widths.RegisterWidth.w64, widths.scalar_width);
        try std.testing.expectEqual(64, widths.word_bits);
        try std.testing.expectEqual(64, widths.scalar_bits);
    }
}
