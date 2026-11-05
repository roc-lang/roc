//! The jumps a patch link writes into code it did not generate: a provide
//! window's jump to its entry wrapper, and an arm64 veneer that extends a
//! `B` or `BL` whose target lies beyond the instruction's 128 MiB reach.
//!
//! A veneer is `ADRP x16, target; ADD x16, x16, :lo12:target; BR x16`,
//! which reaches 4 GiB, the limit a link keeps the app region within. An
//! arm64 image with imports gets one veneer per import after its block; a
//! branch to an import the branch cannot reach goes through the import's
//! veneer instead. Only the Zig-compiled support image branches to imports
//! with `BL`; the dev backend reaches other images and the host through
//! page-relative stubs of its own.
const std = @import("std");
const base = @import("base");
const backend = @import("backend");

const UnitImage = backend.dev.UnitImage;

/// The bytes of one veneer.
pub const veneer_size: u64 = 12;

/// Whether a `B` or `BL` at `site` reaches `target`.
pub fn branchReaches(site: u64, target: u64) bool {
    const delta = @as(i64, @intCast(target)) - @as(i64, @intCast(site));
    return delta & 3 == 0 and std.math.cast(i26, delta >> 2) != null;
}

/// Whether `import` needs a veneer: some reached atom of the image branches
/// to it. An import only unreached atoms name may resolve to an image the
/// link does not place.
pub fn importNeedsVeneer(view: UnitImage.View, import: UnitImage.Import, reached_atoms: []const bool) bool {
    for (view.importSites(import)) |site| {
        if (@as(UnitImage.SiteKind, @fromBackingInt(site.kind)) != .branch26) continue;
        if (site.atom == UnitImage.no_atom or reached_atoms[site.atom]) return true;
    }
    return false;
}

/// Write the veneer at `veneer_address` jumping to `target`. Returns false
/// when the target is beyond the veneer's 4 GiB reach.
pub fn writeVeneer(window: *[veneer_size]u8, veneer_address: u64, target: u64) bool {
    std.mem.writeInt(u32, window[0..4], adrp_x16, .little);
    std.mem.writeInt(u32, window[4..8], add_x16_x16, .little);
    std.mem.writeInt(u32, window[8..12], br_x16, .little);
    if (!UnitImage.applySite(window[0..4], .page21, @intCast(target), veneer_address)) return false;
    if (!UnitImage.applySite(window[4..8], .pageoff12, @intCast(target), veneer_address + 4)) unreachable; // an ADD takes any low bits
    return true;
}

/// Write a procedure window: a jump from `window_address` to `target`, then
/// traps. On x86_64 the window is `jmp rel32` and `int3`s; on arm64 it is
/// `ADRP`, `ADD`, `BR x16` and `BRK`.
pub fn writeJump(arm64: bool, window: *[16]u8, window_address: u64, target: u64) void {
    if (!arm64) {
        window[0] = 0xe9;
        std.mem.writeInt(i32, window[1..5], @intCast(@as(i64, @intCast(target)) - @as(i64, @intCast(window_address + 5))), .little);
        @memset(window[5..], 0xcc);
        return;
    }
    std.mem.writeInt(u32, window[12..16], brk, .little);
    // The app region is within 4 GiB of the host.
    if (!writeVeneer(window[0..12], window_address, target)) base.invariant("fast dev link invariant violated: a provide window cannot reach its procedure", .{});
}

const adrp_x16: u32 = 0x9000_0010;
const add_x16_x16: u32 = 0x9100_0210;
const br_x16: u32 = 0xd61f_0200;
const brk: u32 = 0xd420_0000;

test "a branch reaches 128 MiB either way, word aligned" {
    try std.testing.expect(branchReaches(0x1000, 0x1000 + (1 << 27) - 4));
    try std.testing.expect(!branchReaches(0x1000, 0x1000 + (1 << 27)));
    try std.testing.expect(branchReaches(0x1000_0000, 0x1000_0000 - (1 << 27)));
    try std.testing.expect(!branchReaches(0x1000, 0x1002));
}

test "a veneer and a procedure window jump to their targets" {
    var veneer: [veneer_size]u8 = undefined;
    try std.testing.expect(writeVeneer(&veneer, 0x1_4000_2000, 0x1_4001_0010));
    try std.testing.expect(!writeVeneer(&veneer, 0x1_4000_2000, 0x2_4001_0010));
    try std.testing.expect(writeVeneer(&veneer, 0x1_4000_2000, 0x1_4001_0010));
    // ADRP x16 to page 0x10000 from page 0x2000: 14 pages, immlo 2, immhi 3.
    try std.testing.expectEqual(adrp_x16 | (2 << 29) | (3 << 5), std.mem.readInt(u32, veneer[0..4], .little));
    try std.testing.expectEqual(add_x16_x16 | (0x10 << 10), std.mem.readInt(u32, veneer[4..8], .little));
    try std.testing.expectEqual(br_x16, std.mem.readInt(u32, veneer[8..12], .little));
    var window: [16]u8 = undefined;
    writeJump(true, &window, 0x1_4000_2000, 0x1_4001_0010);
    try std.testing.expectEqualSlices(u8, &veneer, window[0..12]);
    try std.testing.expectEqual(brk, std.mem.readInt(u32, window[12..16], .little));
    writeJump(false, &window, 0x1_4000_2000, 0x1_4001_0000);
    try std.testing.expectEqual(@as(u8, 0xe9), window[0]);
    try std.testing.expectEqual(@as(i32, 0x1_0000 - 0x2000 - 5), std.mem.readInt(i32, window[1..5], .little));
    try std.testing.expectEqual(@as(u8, 0xcc), window[5]);
}
