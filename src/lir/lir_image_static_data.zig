//! Explicit offset-based frozen graph transport for mapped interpreter images.
const std = @import("std");
const core = @import("lir_core");
const layout = @import("layout");
const Program = core.Program;
const missing = std.math.maxInt(u32);

/// Offset-based frozen export schema parameterized by the containing image API.
pub fn Schema(comptime Image: type) type {
    return struct {
        const ArrayRef = Image.ArrayRef;
        pub const Export = extern struct {
            name: ArrayRef,
            bytes: ArrayRef,
            relocations: ArrayRef,
            value_id: u32,
            symbol_offset: u32,
            alignment: u32,
            is_global: u8,
            is_exported: u8,
        };
        pub const Relocation = extern struct {
            name: ArrayRef,
            offset: u64,
            addend: i64,
            data_symbol: u32,
            capture_offset: u32,
            procedure: u32,
            rc_layout: u32,
            rc_op: u32,
            function_pointer: u8,
        };

        pub fn copy(allocator: std.mem.Allocator, base: [*]align(1) const u8, capacity: usize, exports: []const Program.StaticDataExport) Image.CopyError!ArrayRef {
            const rows = try allocator.alloc(Export, exports.len);
            for (exports, rows) |item, *row| {
                const relocations = try allocator.alloc(Relocation, item.relocations.len);
                for (item.relocations, relocations) |reloc, *out| out.* = .{
                    .name = try Image.copyArrayRef(allocator, base, capacity, reloc.target_symbol_name),
                    .offset = reloc.offset,
                    .addend = reloc.addend,
                    .data_symbol = switch (reloc.target) {
                        .named => missing,
                        .data_symbol => |id| @intFromEnum(id),
                    },
                    .capture_offset = reloc.callable_capture_offset orelse missing,
                    .procedure = if (reloc.procedure) |id| @intFromEnum(id) else missing,
                    .rc_layout = if (reloc.rc_helper) |helper| @intCast(@intFromEnum(helper.layout_idx)) else missing,
                    .rc_op = if (reloc.rc_helper) |helper| @intFromEnum(helper.op) else missing,
                    .function_pointer = @intFromBool(reloc.kind == .function_pointer),
                };
                row.* = .{
                    .name = try Image.copyArrayRef(allocator, base, capacity, item.symbol_name),
                    .bytes = try Image.copyArrayRef(allocator, base, capacity, item.bytes),
                    .relocations = try Image.arrayRef(base, capacity, relocations),
                    .value_id = if (item.value_id) |id| @intFromEnum(id) else missing,
                    .symbol_offset = item.symbol_offset,
                    .alignment = item.alignment,
                    .is_global = @intFromBool(item.is_global),
                    .is_exported = @intFromBool(item.is_exported),
                };
            }
            return Image.arrayRef(base, capacity, rows);
        }

        pub fn view(allocator: std.mem.Allocator, base: [*]align(1) u8, size: usize, rows_ref: ArrayRef, value_count: u32, proc_count: usize, layout_count: usize, pointer_bytes: usize) Image.ViewError![]Program.StaticDataExport {
            const rows = try Image.sliceFromRef(Export, base, size, rows_ref);
            const exports = try allocator.alloc(Program.StaticDataExport, rows.len);
            var initialized: usize = 0;
            errdefer {
                for (exports[0..initialized]) |item| allocator.free(item.relocations);
                allocator.free(exports);
            }
            const values = try allocator.alloc(bool, value_count);
            defer allocator.free(values);
            @memset(values, false);
            for (rows, exports) |row, *item| {
                const bytes = try Image.sliceFromRef(u8, base, size, row.bytes);
                if (row.symbol_offset > bytes.len or row.alignment == 0 or !std.math.isPowerOfTwo(row.alignment) or row.is_global > 1 or row.is_exported > 1) return error.InvalidLirImage;
                if (row.value_id != missing) {
                    if (row.value_id >= value_count or values[row.value_id]) return error.InvalidLirImage;
                    values[row.value_id] = true;
                }
                const relocs = try Image.sliceFromRef(Relocation, base, size, row.relocations);
                const name = try Image.sliceFromRef(u8, base, size, row.name);
                const decoded = try allocator.alloc(Program.StaticDataRelocation, relocs.len);
                item.* = .{ .symbol_name = name, .bytes = bytes, .value_id = if (row.value_id == missing) null else @enumFromInt(row.value_id), .symbol_offset = row.symbol_offset, .alignment = row.alignment, .is_global = row.is_global != 0, .is_exported = row.is_exported != 0, .relocations = decoded };
                initialized += 1;
                for (relocs, decoded) |reloc, *out| {
                    if (reloc.offset > bytes.len or pointer_bytes > bytes.len - reloc.offset or reloc.function_pointer > 1 or (reloc.data_symbol != missing and reloc.data_symbol >= rows.len) or (reloc.procedure != missing and reloc.procedure >= proc_count) or ((reloc.rc_layout == missing) != (reloc.rc_op == missing))) return error.InvalidLirImage;
                    if (reloc.rc_layout != missing and (reloc.rc_layout >= layout_count or reloc.rc_layout > std.math.maxInt(@typeInfo(layout.Idx).@"enum".tag_type))) return error.InvalidLirImage;
                    if (reloc.capture_offset != missing and (reloc.capture_offset > bytes.len - reloc.offset or reloc.procedure == missing)) return error.InvalidLirImage;
                    const rc_op: layout.RcOp = if (reloc.rc_op == missing or reloc.rc_op == @intFromEnum(layout.RcOp.incref))
                        .incref
                    else if (reloc.rc_op == @intFromEnum(layout.RcOp.decref))
                        .decref
                    else if (reloc.rc_op == @intFromEnum(layout.RcOp.free))
                        .free
                    else
                        return error.InvalidLirImage;

                    out.* = .{
                        .offset = reloc.offset,
                        .addend = reloc.addend,
                        .target_symbol_name = try Image.sliceFromRef(u8, base, size, reloc.name),
                        .target = if (reloc.data_symbol == missing) .named else .{ .data_symbol = @enumFromInt(reloc.data_symbol) },
                        .kind = if (reloc.function_pointer != 0) .function_pointer else .address,
                        .callable_capture_offset = if (reloc.capture_offset == missing) null else reloc.capture_offset,
                        .procedure = if (reloc.procedure == missing) null else @enumFromInt(reloc.procedure),
                        .rc_helper = if (reloc.rc_layout == missing) null else .{ .op = rc_op, .layout_idx = @enumFromInt(reloc.rc_layout) },
                    };
                }
            }
            for (values) |present| if (!present) return error.InvalidLirImage;
            return exports;
        }

        pub fn deinit(allocator: std.mem.Allocator, exports: []Program.StaticDataExport) void {
            for (exports) |item| allocator.free(item.relocations);
            allocator.free(exports);
        }
    };
}
