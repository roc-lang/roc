//! Roc compiler version strings.
//!
//! These are the strings `roc version` prints, and that an app, package or
//! platform header may pin with the reserved `roc` entry in its dependency
//! record:
//!
//! ```roc
//! app [main!] {
//!     pf: platform "https://example.com/basic-cli.tar.zst",
//!     roc: "nightly-2026-July-31-123c5d7",
//! }
//! ```
//!
//! Two forms are recognized:
//!
//!   - nightly tags, e.g. `nightly-2026-08-05-24f0b47`, as produced by the
//!     nightly release workflow and passed to `-Dcompiler-version`. The month
//!     may also be spelled by name, as in `nightly-2026-July-31-123c5d7`.
//!   - releases, e.g. `0.1.0` or `1.0.0-rc1`
//!
//! Local development builds report `<build mode>-<git short sha>` (e.g.
//! `debug-c6dfe61b`), which is deliberately *not* a recognized form: a version
//! only one machine can reproduce is not something a header should pin.
//!
//! This is the compiler's own version, which is unrelated to the package
//! versions embedded in package URLs—see `base.url.Version` for those.

const std = @import("std");

/// Prefix every nightly release tag starts with.
pub const nightly_prefix = "nightly-";

/// Calendar month of a nightly release. Release tags spell it as a number
/// (`08`), and older ones spell it by name (`August`); see `parseMonth`.
pub const Month = enum(u8) {
    January = 1,
    February,
    March,
    April,
    May,
    June,
    July,
    August,
    September,
    October,
    November,
    December,
};

/// A nightly release tag, e.g. `nightly-2026-08-05-24f0b47`.
pub const Nightly = struct {
    year: u16,
    month: Month,
    day: u8,
    /// Short git commit hash the nightly was built from. Borrowed from the
    /// text this was parsed out of.
    commit: []const u8,

    /// Order two nightlies by release date.
    ///
    /// Nightlies from the same day compare `.eq` even when they name different
    /// commits: the tag records no time of day, so nothing in it says which of
    /// two same-day commits came first.
    pub fn dateOrder(self: Nightly, other: Nightly) std.math.Order {
        const year_order = std.math.order(self.year, other.year);
        if (year_order != .eq) return year_order;
        const month_order = std.math.order(@intFromEnum(self.month), @intFromEnum(other.month));
        if (month_order != .eq) return month_order;
        return std.math.order(self.day, other.day);
    }
};

/// A released compiler version, e.g. `0.1.0` or `1.0.0-rc1`.
pub const Release = struct {
    major: u32,
    minor: u32,
    patch: u32,
    /// Pre-release suffix without its leading `-`, e.g. `rc1`. Empty when the
    /// version has none. Borrowed from the text this was parsed out of.
    prerelease: []const u8 = "",
};

/// A compiler version in one of the forms a header is allowed to pin.
pub const Version = union(enum) {
    nightly: Nightly,
    release: Release,
};

/// Parse a compiler version string, or return null if it is not in a form a
/// header may pin.
pub fn parse(text: []const u8) ?Version {
    if (std.mem.startsWith(u8, text, nightly_prefix)) {
        return .{ .nightly = parseNightly(text[nightly_prefix.len..]) orelse return null };
    }
    return .{ .release = parseRelease(text) orelse return null };
}

/// Whether `roc fmt` should rewrite a header pinned to `pinned` so that it
/// names `current`—the version of the compiler doing the formatting.
///
/// Only nightly-to-nightly upgrades happen automatically. A pin on a released
/// version is a deliberate choice that a nightly compiler must not overwrite,
/// and a nightly pin is never rolled back to an older nightly just because an
/// older compiler happened to format the file.
///
/// Same-day nightlies do upgrade: `Nightly.dateOrder` cannot tell two commits
/// from one day apart, and the compiler that is actually running is the better
/// guess of the two.
pub fn shouldUpgrade(pinned: []const u8, current: []const u8) bool {
    if (std.mem.eql(u8, pinned, current)) return false;

    const current_nightly = switch (parse(current) orelse return false) {
        .nightly => |nightly| nightly,
        .release => return false,
    };
    const pinned_nightly = switch (parse(pinned) orelse return false) {
        .nightly => |nightly| nightly,
        .release => return false,
    };

    return current_nightly.dateOrder(pinned_nightly) != .lt;
}

/// Whether a header pinned to `pinned` disagrees with `current`, the version
/// of the compiler that is running, in a way worth reporting.
///
/// A pin the compiler cannot read is not a mismatch: parsing already rejected
/// it, and one mistake should not produce two diagnostics.
///
/// Neither is anything a mismatch when the running compiler reports a local
/// development version (e.g. `debug-c6dfe61b`): a build from source is not a
/// version any header could pin, so no pin can name it, and `roc fmt` will not
/// write it either. Reporting every pinned file the developer touches would
/// only be noise.
pub fn isMismatch(pinned: []const u8, current: []const u8) bool {
    if (parse(pinned) == null) return false;
    if (parse(current) == null) return false;
    return !std.mem.eql(u8, pinned, current);
}

/// Parse the `<year>-<month>-<day>-<commit>` body of a nightly tag.
fn parseNightly(body: []const u8) ?Nightly {
    var parts = std.mem.splitScalar(u8, body, '-');

    const year = parseDigits(u16, parts.next() orelse return null, 4, 4) orelse return null;
    const month = parseMonth(parts.next() orelse return null) orelse return null;
    const day = parseDigits(u8, parts.next() orelse return null, 1, 2) orelse return null;
    if (day < 1 or day > 31) return null;

    // The commit is the last field, so it must not itself contain a `-`;
    // otherwise `nightly-2026-July-31-a-b` would have no single reading.
    const commit = parts.next() orelse return null;
    if (parts.next() != null) return null;
    if (commit.len == 0) return null;
    for (commit) |char| {
        if (!std.ascii.isHex(char)) return null;
    }

    return .{ .year = year, .month = month, .day = day, .commit = commit };
}

/// Parse the month field of a nightly tag, written either as a number (`08`)
/// or as the month's English name (`August`).
///
/// Both spellings are accepted because the release workflow has produced both:
/// tags name the month by number, and headers pinned by an earlier compiler
/// that spelled it out should keep working rather than become unreadable.
fn parseMonth(text: []const u8) ?Month {
    if (std.meta.stringToEnum(Month, text)) |named| return named;

    const number = parseDigits(u8, text, 1, 2) orelse return null;
    if (number < 1 or number > 12) return null;
    return @enumFromInt(number);
}

/// Parse `MAJOR.MINOR.PATCH` with an optional `-PRERELEASE` suffix.
fn parseRelease(text: []const u8) ?Release {
    const dash = std.mem.findScalar(u8, text, '-');
    const numbers = if (dash) |index| text[0..index] else text;
    const prerelease = if (dash) |index| text[index + 1 ..] else "";

    if (dash != null and prerelease.len == 0) return null;
    for (prerelease) |char| {
        if (!std.ascii.isAlphanumeric(char) and char != '.' and char != '-') return null;
    }

    var parts = std.mem.splitScalar(u8, numbers, '.');
    const major = parseDigits(u32, parts.next() orelse return null, 1, 10) orelse return null;
    const minor = parseDigits(u32, parts.next() orelse return null, 1, 10) orelse return null;
    const patch = parseDigits(u32, parts.next() orelse return null, 1, 10) orelse return null;
    if (parts.next() != null) return null;

    return .{ .major = major, .minor = minor, .patch = patch, .prerelease = prerelease };
}

/// Parse `text` as a decimal number of between `min_len` and `max_len` digits.
fn parseDigits(comptime T: type, text: []const u8, min_len: usize, max_len: usize) ?T {
    if (text.len < min_len or text.len > max_len) return null;
    for (text) |char| {
        if (!std.ascii.isDigit(char)) return null;
    }
    return std.fmt.parseInt(T, text, 10) catch null;
}

test "parse nightly tag" {
    const version = parse("nightly-2026-08-05-24f0b47") orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(u16, 2026), version.nightly.year);
    try std.testing.expectEqual(Month.August, version.nightly.month);
    try std.testing.expectEqual(@as(u8, 5), version.nightly.day);
    try std.testing.expectEqualStrings("24f0b47", version.nightly.commit);
}

test "parse nightly tag with a named month" {
    const version = parse("nightly-2026-July-31-123c5d7") orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(u16, 2026), version.nightly.year);
    try std.testing.expectEqual(Month.July, version.nightly.month);
    try std.testing.expectEqual(@as(u8, 31), version.nightly.day);
    try std.testing.expectEqualStrings("123c5d7", version.nightly.commit);
}

test "parse nightly tag with an unpadded numeric month" {
    const version = parse("nightly-2026-8-5-24f0b47") orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(Month.August, version.nightly.month);
    try std.testing.expectEqual(@as(u8, 5), version.nightly.day);
}

test "parse nightly tag with a single-digit day" {
    const version = parse("nightly-2026-January-1-abc1234") orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(Month.January, version.nightly.month);
    try std.testing.expectEqual(@as(u8, 1), version.nightly.day);
}

test "reject malformed nightly tags" {
    try std.testing.expectEqual(@as(?Version, null), parse("nightly-2026-Jul-31-123c5d7"));
    try std.testing.expectEqual(@as(?Version, null), parse("nightly-2026-july-31-123c5d7"));
    try std.testing.expectEqual(@as(?Version, null), parse("nightly-2026-00-05-24f0b47"));
    try std.testing.expectEqual(@as(?Version, null), parse("nightly-2026-13-05-24f0b47"));
    try std.testing.expectEqual(@as(?Version, null), parse("nightly-2026-008-05-24f0b47"));
    try std.testing.expectEqual(@as(?Version, null), parse("nightly-26-July-31-123c5d7"));
    try std.testing.expectEqual(@as(?Version, null), parse("nightly-2026-July-32-123c5d7"));
    try std.testing.expectEqual(@as(?Version, null), parse("nightly-2026-July-31"));
    try std.testing.expectEqual(@as(?Version, null), parse("nightly-2026-July-31-123c5d7-extra"));
    try std.testing.expectEqual(@as(?Version, null), parse("nightly-2026-July-31-zzz"));
    try std.testing.expectEqual(@as(?Version, null), parse("nightly-"));
}

test "reject local development version strings" {
    try std.testing.expectEqual(@as(?Version, null), parse("debug-c6dfe61b"));
    try std.testing.expectEqual(@as(?Version, null), parse("release-fast-abc12345"));
    try std.testing.expectEqual(@as(?Version, null), parse("no-git"));
    try std.testing.expectEqual(@as(?Version, null), parse(""));
}

test "parse release versions" {
    const plain = parse("0.1.0") orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(u32, 0), plain.release.major);
    try std.testing.expectEqual(@as(u32, 1), plain.release.minor);
    try std.testing.expectEqual(@as(u32, 0), plain.release.patch);
    try std.testing.expectEqualStrings("", plain.release.prerelease);

    const prerelease = parse("1.0.0-rc1") orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(u32, 1), prerelease.release.major);
    try std.testing.expectEqualStrings("rc1", prerelease.release.prerelease);
}

test "reject malformed release versions" {
    try std.testing.expectEqual(@as(?Version, null), parse("1.0"));
    try std.testing.expectEqual(@as(?Version, null), parse("1.0.0.0"));
    try std.testing.expectEqual(@as(?Version, null), parse("1.0.x"));
    try std.testing.expectEqual(@as(?Version, null), parse("1.0.0-"));
    try std.testing.expectEqual(@as(?Version, null), parse("v1.0.0"));
}

test "nightly date ordering" {
    const older = (parse("nightly-2026-July-31-aaaaaaa") orelse return error.TestUnexpectedResult).nightly;
    const newer = (parse("nightly-2026-August-1-bbbbbbb") orelse return error.TestUnexpectedResult).nightly;
    try std.testing.expectEqual(std.math.Order.lt, older.dateOrder(newer));
    try std.testing.expectEqual(std.math.Order.gt, newer.dateOrder(older));
    try std.testing.expectEqual(std.math.Order.eq, older.dateOrder(older));
}

test "report a pin that does not name the running compiler" {
    try std.testing.expect(isMismatch("nightly-2026-July-30-aaaaaaa", "nightly-2026-July-31-bbbbbbb"));
    try std.testing.expect(isMismatch("0.1.0", "nightly-2026-July-31-bbbbbbb"));
}

test "stay quiet when there is nothing to report about a pin" {
    try std.testing.expect(!isMismatch("nightly-2026-July-31-aaaaaaa", "nightly-2026-July-31-aaaaaaa"));
    // Already reported as an invalid version by the parser.
    try std.testing.expect(!isMismatch("nonsense", "nightly-2026-July-31-bbbbbbb"));
    // No pin can name a build from source, so none disagrees with one.
    try std.testing.expect(!isMismatch("nightly-2026-July-31-aaaaaaa", "debug-c6dfe61b"));
    try std.testing.expect(!isMismatch("0.1.0", "release-fast-7fdb318d"));
}

test "upgrade a pin to a newer nightly" {
    try std.testing.expect(shouldUpgrade("nightly-2026-July-30-aaaaaaa", "nightly-2026-July-31-bbbbbbb"));
    // A pin written when months were spelled out is still comparable, and
    // formatting rewrites it in the spelling the running compiler reports.
    try std.testing.expect(shouldUpgrade("nightly-2026-July-30-aaaaaaa", "nightly-2026-08-05-24f0b47"));
    // Same day, different commit: the running compiler wins.
    try std.testing.expect(shouldUpgrade("nightly-2026-July-31-aaaaaaa", "nightly-2026-July-31-bbbbbbb"));
}

test "leave a pin alone when there is nothing to upgrade to" {
    // Already current.
    try std.testing.expect(!shouldUpgrade("nightly-2026-July-31-aaaaaaa", "nightly-2026-July-31-aaaaaaa"));
    // Running an older nightly than the pin.
    try std.testing.expect(!shouldUpgrade("nightly-2026-July-31-aaaaaaa", "nightly-2026-July-30-bbbbbbb"));
    // Released pins are deliberate and are never overwritten by a nightly.
    try std.testing.expect(!shouldUpgrade("0.1.0", "nightly-2026-July-31-bbbbbbb"));
    // A local development build is not something a header may pin.
    try std.testing.expect(!shouldUpgrade("nightly-2026-July-31-aaaaaaa", "debug-c6dfe61b"));
    // A pin the compiler cannot read is left for the programmer to fix.
    try std.testing.expect(!shouldUpgrade("nonsense", "nightly-2026-July-31-bbbbbbb"));
}
