const std = @import("std");

fn typeNameContains(comptime T: type, comptime needle: []const u8) bool {
    const haystack = @typeName(T);
    if (needle.len == 0 or needle.len > haystack.len) return false;
    var i: usize = 0;
    while (i + needle.len <= haystack.len) : (i += 1) {
        if (std.mem.eql(u8, haystack[i .. i + needle.len], needle)) {
            return true;
        }
    }
    return false;
}

fn hasAny(comptime T: type) bool {
    return @hasDecl(T, "any");
}

pub fn toAnyReader(reader: anytype) std.io.AnyReader {
    return toAnyReaderImpl(reader);
}

fn toAnyReaderImpl(reader: anytype) std.io.AnyReader {
    const T = @TypeOf(reader);
    if (T == std.io.AnyReader) {
        return reader;
    }

    switch (@typeInfo(T)) {
        .pointer => |ptr_info| {
            if (ptr_info.child == std.io.AnyReader) {
                return reader.*;
            }
            if (ptr_info.child != void) {
                if (ptr_info.child == std.io.Reader) {
                    return reader.adaptToOldInterface();
                }
                if (hasAny(ptr_info.child)) {
                    return reader.*.any();
                }
                if (ptr_info.size == .One) {
                    return toAnyReaderImpl(reader.*);
                }
            }
        },
        else => {
            const has_method = comptime hasAny(T);
            const matches_generic = comptime typeNameContains(T, "GenericReader");
            if (has_method or matches_generic) {
                return reader.any();
            }
        },
    }

    @compileError("cannot convert type '" ++ @typeName(T) ++ "' to std.io.AnyReader");
}

pub fn toAnyWriter(writer: anytype) std.io.AnyWriter {
    return toAnyWriterImpl(writer);
}

fn newWriterToAny(writer: *std.Io.Writer) std.io.AnyWriter {
    return .{ .context = writer, .writeFn = writeFromIoWriter };
}

fn writeFromIoWriter(context: *const anyopaque, bytes: []const u8) anyerror!usize {
    const writer: *std.Io.Writer = @ptrCast(@alignCast(@constCast(context)));
    return writer.write(bytes);
}

fn toAnyWriterImpl(writer: anytype) std.io.AnyWriter {
    const T = @TypeOf(writer);
    if (T == std.io.AnyWriter) {
        return writer;
    }

    switch (@typeInfo(T)) {
        .pointer => |ptr_info| {
            if (ptr_info.child == std.io.AnyWriter) {
                return writer.*;
            }
            if (ptr_info.child != void) {
                if (ptr_info.child == std.io.Writer) {
                    return newWriterToAny(writer);
                }
                if (hasAny(ptr_info.child)) {
                    return writer.*.any();
                }
                if (ptr_info.size == .One) {
                    return toAnyWriterImpl(writer.*);
                }
            }
        },
        else => {
            const has_method = comptime hasAny(T);
            const matches_generic = comptime typeNameContains(T, "GenericWriter");
            if (has_method or matches_generic) {
                return writer.any();
            }
        },
    }

    @compileError("cannot convert type '" ++ @typeName(T) ++ "' to std.io.AnyWriter");
}
