const std = @import("std");
const always_inline = std.builtin.CallOptions.Modifier.always_inline;
const math = std.math;
const RocList = @import("list.zig").RocList;

pub fn exportPow(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(base: T, exp: T) callconv(.C) T {
            return std.math.pow(T, base, exp);
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportIsFinite(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(input: T) callconv(.C) bool {
            return std.math.isFinite(input);
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportAsin(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(input: T) callconv(.C) T {
            return std.math.asin(input);
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportAcos(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(input: T) callconv(.C) T {
            return std.math.acos(input);
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportAtan(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(input: T) callconv(.C) T {
            return std.math.atan(input);
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportRound(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(input: T) callconv(.C) i64 {
            return @floatToInt(i64, (@round(input)));
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn exportDivCeil(comptime T: type, comptime name: []const u8) void {
    comptime var f = struct {
        fn func(a: T, b: T) callconv(.C) T {
            return math.divCeil(T, a, b) catch unreachable;
        }
    }.func;
    @export(f, .{ .name = name ++ @typeName(T), .linkage = .Strong });
}

pub fn bytesToU16C(arg: RocList, position: usize) callconv(.C) u16 {
    return @call(.{ .modifier = always_inline }, bytesToU16, .{ arg, position });
}

fn bytesToU16(arg: RocList, position: usize) u16 {
    const bytes = @ptrCast([*]const u8, arg.bytes);
    return @bitCast(u16, [_]u8{ bytes[position], bytes[position + 1] });
}

pub fn bytesToU32C(arg: RocList, position: usize) callconv(.C) u32 {
    return @call(.{ .modifier = always_inline }, bytesToU32, .{ arg, position });
}

fn bytesToU32(arg: RocList, position: usize) u32 {
    const bytes = @ptrCast([*]const u8, arg.bytes);
    return @bitCast(u32, [_]u8{ bytes[position], bytes[position + 1], bytes[position + 2], bytes[position + 3] });
}
