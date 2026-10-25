//! Producer-declared compatibility of relocatable native dev procedures.
//! This is neither final-link identity nor authority to elide source bodies:
//! consumers must additionally own the same complete lookup/splice provider.

const std = @import("std");
const base = @import("base");
const targets = @import("roc_target");
const build_options = @import("build_options");
const PackFile = @import("PackFile.zig");
const Self = @This();

/// Exact targets encode their architecture, pointer width, endianness and ABI.
/// The sole cross-target contract is explicitly audited, never inferred from
/// matching architecture/OS. It has 64-bit LE data and System V calls.
pub const TargetContract = union(enum) {
    exact: targets.RocTarget,
    x64_linux_sysv,
};

compiler: [32]u8 = build_options.compiler_compatibility_hash,
format: u32 = PackFile.format_version,
target: TargetContract,
cpu: targets.CpuLevel,
strategy: base.SpecializationStrategy,
/// Dev lowering, builtin/runtime ABI, capture and relocation representation.
/// Change this revision when changing this contract independently of the pack.
representation: u32 = 1,
capability: enum { relocatable_native, linked_only } = .relocatable_native,

pub fn init(target: targets.RocTarget, strategy: base.SpecializationStrategy) Self {
    return .{
        .target = if (strategy == .lss) switch (target) {
            .x64musl, .x64glibc, .x64v1musl, .x64v1glibc => .x64_linux_sysv,
            .x64mac, .x64win, .x64mingw, .x64freebsd, .x64openbsd, .x64netbsd, .x64linux, .x64elf, .x64v1mac, .x64v1win, .x64v1mingw, .x64v1freebsd, .x64v1openbsd, .x64v1netbsd, .x64v1linux, .x64v1elf, .arm64mac, .arm64win, .arm64mingw, .arm64linux, .arm64musl, .arm64glibc, .arm64v1win, .arm64v1mingw, .arm64v1linux, .arm64v1musl, .arm64v1glibc, .arm32linux, .arm32musl, .wasm32, .wasm32v1 => .{ .exact = target.defaultCpuTarget() },
        } else .{ .exact = target.defaultCpuTarget() },
        .cpu = target.cpuLevel(),
        .strategy = strategy,
    };
}

/// Whether procedures produced under `other` may be loaded under `self`:
/// every field of the two domains must match.
pub fn compatible(self: Self, other: Self) bool {
    return std.meta.eql(self, other);
}

/// Stable explicit encoding, without struct padding or host byte order.
pub fn key(self: Self) [32]u8 {
    var hash = std.crypto.hash.sha2.Sha256.init(.{});
    hash.update("roc-native-dev-procedure-domain-v1\x00");
    hash.update(&self.compiler);
    var word: [4]u8 = undefined;
    std.mem.writeInt(u32, &word, self.format, .little);
    hash.update(&word);
    hash.update(@tagName(self.target));
    if (self.target == .exact) hash.update(@tagName(self.target.exact));
    hash.update("\x00");
    hash.update(@tagName(self.cpu));
    hash.update("\x00");
    hash.update(@tagName(self.strategy));
    std.mem.writeInt(u32, &word, self.representation, .little);
    hash.update(&word);
    hash.update(@tagName(self.capability));
    return hash.finalResult();
}

test "native procedure domain shares only the declared libc pair and matching floors" {
    for ([_][2]targets.RocTarget{ .{ .x64musl, .x64glibc }, .{ .x64v1musl, .x64v1glibc } }) |pair| {
        const left = init(pair[0], .lss);
        const right = init(pair[1], .lss);
        try std.testing.expect(left.compatible(right));
        try std.testing.expectEqual(left.key(), right.key());
    }
    const source = init(.x64musl, .lss);
    for ([_]targets.RocTarget{ .x64v1glibc, .arm64musl, .x64win, .x64linux, .x64mac }) |target| {
        try std.testing.expect(!source.compatible(init(target, .lss)));
        try std.testing.expect(!std.meta.eql(source.key(), init(target, .lss).key()));
    }
    try std.testing.expect(!init(.x64musl, .boxy).compatible(init(.x64glibc, .boxy)));
    var variants: [5]Self = @splat(source);
    variants[0].compiler[0] ^= 1;
    variants[1].format += 1;
    variants[2].strategy = .boxy;
    variants[3].representation += 1;
    variants[4].capability = .linked_only;
    for (variants) |variant| {
        try std.testing.expect(!source.compatible(variant));
        try std.testing.expect(!std.meta.eql(source.key(), variant.key()));
    }
}
