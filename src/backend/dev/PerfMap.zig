//! A perf map of the procedures the compiler places in its own process: the
//! unit images `roc test` runs its roots from and compile-time evaluation
//! calls into (`UnitImageLoader`). `perf` names a sample in such code by the
//! lines of `/tmp/perf-<pid>.map`, one `<start hex> <size hex> <name>` per
//! function, which this appends when the developer diagnostic
//! `ROC_PERF_MAP` is set (`base.dev_diagnostics`). A procedure is named by
//! the readable name a lowering of this process recorded for it
//! (`dev_diagnostics.notePerfName`), and otherwise by its symbol.

const std = @import("std");
const builtin = @import("builtin");
const dev_diagnostics = @import("base").dev_diagnostics;
const UnitImage = @import("UnitImage.zig");

/// Whether this host writes perf maps: `perf` reads them on Linux.
pub const supported = builtin.os.tag == .linux;

/// The perf map line of the function of `size` bytes at `start` named
/// `name`, written to `out`.
pub fn writeLine(out: *std.ArrayList(u8), gpa: std.mem.Allocator, start: usize, size: usize, name: []const u8) std.mem.Allocator.Error!void {
    var numbers: [2 * 16 + 2]u8 = undefined;
    const prefix = std.fmt.bufPrint(&numbers, "{x} {x} ", .{ start, size }) catch unreachable; // two 64-bit hex numbers and two spaces fit the buffer
    try out.ensureUnusedCapacity(gpa, prefix.len + name.len + 1);
    out.appendSliceAssumeCapacity(prefix);
    out.appendSliceAssumeCapacity(name);
    out.appendAssumeCapacity('\n');
}

/// Append a line for every named function of `view`, whose symbol addresses
/// `address_of` gives, to this process's perf map. A failure to allocate or
/// write leaves the map without them: a profile then names fewer functions.
pub fn appendImage(gpa: std.mem.Allocator, view: UnitImage.View, context: anytype, comptime address_of: fn (@TypeOf(context), UnitImage.Symbol) usize) void {
    if (comptime !supported) return;
    var lines: std.ArrayList(u8) = .empty;
    defer lines.deinit(gpa);
    for (view.symbols()) |symbol| {
        if (symbol.type != @backingInt(UnitImage.SymbolType.func) or symbol.size == 0) continue;
        const name = view.name(symbol.name);
        if (name.len == 0) continue;
        // A procedure a lowering of this process named is written by its
        // readable name, any other function by its symbol.
        writeLine(&lines, gpa, address_of(context, symbol), symbol.size, dev_diagnostics.perfName(name) orelse name) catch return;
    }
    if (lines.items.len == 0) return;
    var path_buffer: [64]u8 = undefined;
    const path = std.fmt.bufPrintSentinel(&path_buffer, "/tmp/perf-{d}.map", .{std.c.getpid()}, 0) catch unreachable; // a pid fits the buffer
    const fd = std.c.open(path, .{ .ACCMODE = .WRONLY, .CREAT = true, .APPEND = true, .CLOEXEC = true }, @as(std.c.mode_t, 0o644));
    if (fd < 0) return;
    defer _ = std.c.close(fd);
    var written: usize = 0;
    while (written < lines.items.len) {
        const result = std.c.write(fd, lines.items[written..].ptr, lines.items.len - written);
        if (result <= 0) return;
        written += @intCast(result);
    }
}

test "a perf map line is the start and size in hex, then the name" {
    const gpa = std.testing.allocator;
    var lines: std.ArrayList(u8) = .empty;
    defer lines.deinit(gpa);
    try writeLine(&lines, gpa, 0x7f12_3400_0010, 0x2c, "roc__p0123456789abcdef0123456789abcdef");
    try std.testing.expectEqualStrings("7f1234000010 2c roc__p0123456789abcdef0123456789abcdef\n", lines.items);
}
