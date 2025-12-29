//! Test file for OrderedSink functionality.
//!
//! These tests verify the internal logic of OrderedSink which manages
//! ordered emission of reports across modules. This is unit-tested here
//! because it involves internal state management that can't be exercised
//! through end-to-end snapshot tests.
//!
//! BuildEnv integration tests (multi-module builds, cycle detection,
//! error ordering, etc.) are covered by snapshot tests in test/snapshots/.

const std = @import("std");
const reporting = @import("reporting");

const Report = reporting.Report;
const OrderedSink = @import("../compile_build.zig").OrderedSink;

// OrderedSink should gate emission to a contiguous ready prefix and drain in order.
test "OrderedSink: prefix gating and drain order" {
    const gpa = std.testing.allocator;
    var sink = OrderedSink.init(gpa);
    defer sink.deinit();

    const pkg_names = [_][]const u8{ "pkg", "pkg" };
    const module_names = [_][]const u8{ "First", "Second" };
    const depths = [_]u32{ 1, 1 };
    try sink.buildOrder(&pkg_names, &module_names, &depths);

    // Emit Second first; should not emit anything yet due to prefix gating (First not ready).
    const r2 = Report.init(gpa, "Second report", .runtime_error);
    sink.emitReport("pkg", "Second", r2);

    const drained0 = try sink.drainEmitted(gpa);
    defer gpa.free(drained0);
    try std.testing.expectEqual(@as(usize, 0), drained0.len);

    // Emit First; now both First and Second should emit (First unlocks the prefix, then Second).
    const r1 = Report.init(gpa, "First report", .runtime_error);
    sink.emitReport("pkg", "First", r1);

    const drained1 = try sink.drainEmitted(gpa);
    defer {
        var i: usize = 0;
        while (i < drained1.len) : (i += 1) {
            for (drained1[i].reports) |*rep| rep.deinit();
            gpa.free(drained1[i].reports);
        }
        gpa.free(drained1);
    }
    try std.testing.expectEqual(@as(usize, 2), drained1.len);
    try std.testing.expect(std.mem.eql(u8, drained1[0].pkg_name, "pkg") and std.mem.eql(u8, drained1[0].module_name, "First"));
    try std.testing.expectEqual(@as(usize, 1), drained1[0].reports.len);
    try std.testing.expect(std.mem.eql(u8, drained1[1].pkg_name, "pkg") and std.mem.eql(u8, drained1[1].module_name, "Second"));
    try std.testing.expectEqual(@as(usize, 1), drained1[1].reports.len);
}

test "OrderedSink: early emit before order still drains after order build" {
    const gpa = std.testing.allocator;
    var sink = OrderedSink.init(gpa);
    defer sink.deinit();

    // Emit before any order is built; should not drain yet
    const early = Report.init(gpa, "Early", .runtime_error);
    sink.emitReport("pkg", "M", early);

    const drained0 = try sink.drainEmitted(gpa);
    defer gpa.free(drained0);
    try std.testing.expectEqual(@as(usize, 0), drained0.len);

    // Build order with the module
    const pkg_names = [_][]const u8{"pkg"};
    const module_names = [_][]const u8{"M"};
    const depths = [_]u32{1};
    try sink.buildOrder(&pkg_names, &module_names, &depths);

    // Emit again to trigger tryEmitLocked; now drain should return the module
    const later = Report.init(gpa, "Later", .runtime_error);
    sink.emitReport("pkg", "M", later);

    const drained1 = try sink.drainEmitted(gpa);
    defer {
        if (drained1.len > 0) {
            var i: usize = 0;
            while (i < drained1[0].reports.len) : (i += 1) {
                var rep = drained1[0].reports[i];
                rep.deinit();
            }
            gpa.free(drained1[0].reports);
        }
        gpa.free(drained1);
    }
    try std.testing.expectEqual(@as(usize, 1), drained1.len);
    try std.testing.expect(std.mem.eql(u8, drained1[0].pkg_name, "pkg") and std.mem.eql(u8, drained1[0].module_name, "M"));
    try std.testing.expectEqual(@as(usize, 2), drained1[0].reports.len);
}

test "OrderedSink: case-insensitive sort of fq names at same depth" {
    const gpa = std.testing.allocator;
    var sink = OrderedSink.init(gpa);
    defer sink.deinit();

    const pkg_names = [_][]const u8{ "pkg", "pkg" };
    const module_names = [_][]const u8{ "B", "a" };
    const depths = [_]u32{ 0, 0 };
    try sink.buildOrder(&pkg_names, &module_names, &depths);

    const r_b = Report.init(gpa, "B", .runtime_error);
    sink.emitReport("pkg", "B", r_b);
    const r_a = Report.init(gpa, "a", .runtime_error);
    sink.emitReport("pkg", "a", r_a);

    const drained = try sink.drainEmitted(gpa);
    defer {
        var i: usize = 0;
        while (i < drained.len) : (i += 1) {
            var j: usize = 0;
            while (j < drained[i].reports.len) : (j += 1) {
                var rep = drained[i].reports[j];
                rep.deinit();
            }
            gpa.free(drained[i].reports);
        }
        gpa.free(drained);
    }

    try std.testing.expectEqual(@as(usize, 2), drained.len);
    // Case-insensitive compare should place "a" before "B"
    try std.testing.expect(std.mem.eql(u8, drained[0].pkg_name, "pkg") and std.mem.eql(u8, drained[0].module_name, "a"));
    try std.testing.expect(std.mem.eql(u8, drained[1].pkg_name, "pkg") and std.mem.eql(u8, drained[1].module_name, "B"));
}
