//! Explicit offset-based frozen graph transport for mapped interpreter images.
const std = @import("std");
const core = @import("lir_core");
const layout = @import("layout");
const collections = @import("collections");
const serde_validation = collections.serde_validation;
const Program = core.Program;
/// An absent persisted id or offset is zero; a present one stores its value
/// plus one.
const OptionalU32 = collections.OptionalU32;

/// Offset-based frozen export schema parameterized by the containing image API.
pub fn Schema(comptime Image: type) type {
    return struct {
        const ArrayRef = Image.ArrayRef;
        pub const Export = extern struct {
            name: ArrayRef,
            bytes: ArrayRef,
            relocations: ArrayRef,
            value_id: OptionalU32,
            symbol_offset: u32,
            alignment: u32,
            is_global: u8,
            is_exported: u8,
            /// Explicit trailing alignment bytes.
            _padding: [2]u8 = .{ 0, 0 },
        };
        pub const Relocation = extern struct {
            name: ArrayRef,
            offset: u64,
            addend: i64,
            data_symbol: OptionalU32,
            capture_offset: OptionalU32,
            procedure: OptionalU32,
            rc_layout: OptionalU32,
            rc_op: OptionalU32,
            function_pointer: u8,
            /// The bytes alignment adds after `function_pointer`, declared so
            /// that every byte of a row is defined.
            _padding: [3]u8 = @as([3]u8, @splat(0)),
        };

        comptime {
            // `copy` authors these rows field by field into image storage, so
            // their bytes are a function of the frozen graph under either
            // `Image.ByteContract` only while every byte belongs to a field.
            serde_validation.assertFullyDefined(Export, "StaticDataImage.Export");
            serde_validation.assertFullyDefined(Relocation, "StaticDataImage.Relocation");
        }

        /// Copy a frozen graph into image storage. Every byte this writes is a
        /// function of `exports` alone: the rows declare all of their bytes and
        /// the names and value bytes are plain byte arrays, so the copy
        /// satisfies `Image.ByteContract.persisted` at no cost to a mapped
        /// image.
        pub fn copy(allocator: std.mem.Allocator, base: [*]align(1) const u8, capacity: usize, exports: []const Program.StaticDataExport) Image.CopyError!ArrayRef {
            const rows = try allocator.alloc(Export, exports.len);
            for (exports, rows) |item, *row| {
                const relocations = try allocator.alloc(Relocation, item.relocations.len);
                for (item.relocations, relocations) |reloc, *out| out.* = .{
                    .name = try Image.copyArrayRef(allocator, base, capacity, .persisted, reloc.target_symbol_name),
                    .offset = reloc.offset,
                    .addend = reloc.addend,
                    .data_symbol = switch (reloc.target) {
                        .named => .none,
                        .data_symbol => |id| .some(@backingInt(id)),
                    },
                    .capture_offset = reloc.callable_capture_offset,
                    .procedure = if (reloc.procedure.get()) |id| .some(@backingInt(id)) else .none,
                    .rc_layout = if (reloc.rc_helper) |helper| .some(@backingInt(helper.layout_idx)) else .none,
                    .rc_op = if (reloc.rc_helper) |helper| .some(@backingInt(helper.op)) else .none,
                    .function_pointer = @intFromBool(reloc.kind == .function_pointer),
                };
                row.* = .{
                    .name = try Image.copyArrayRef(allocator, base, capacity, .persisted, item.symbol_name),
                    .bytes = try Image.copyArrayRef(allocator, base, capacity, .persisted, item.bytes),
                    .relocations = try Image.arrayRef(base, capacity, relocations),
                    .value_id = if (item.value_id) |id| .some(@backingInt(id)) else .none,
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
                const value_id = row.value_id.get();
                if (value_id) |id| {
                    if (id >= value_count or values[id]) return error.InvalidLirImage;
                    values[id] = true;
                }
                const relocs = try Image.sliceFromRef(Relocation, base, size, row.relocations);
                const name = try Image.sliceFromRef(u8, base, size, row.name);
                const decoded = try allocator.alloc(Program.StaticDataRelocation, relocs.len);
                item.* = .{ .symbol_name = name, .bytes = bytes, .value_id = if (value_id) |id| @fromBackingInt(@intCast(id)) else null, .symbol_offset = row.symbol_offset, .alignment = row.alignment, .is_global = row.is_global != 0, .is_exported = row.is_exported != 0, .relocations = decoded };
                initialized += 1;
                for (relocs, decoded) |reloc, *out| {
                    const data_symbol = reloc.data_symbol.get();
                    const capture_offset = reloc.capture_offset.get();
                    const procedure = reloc.procedure.get();
                    const rc_layout = reloc.rc_layout.get();
                    const rc_op_raw = reloc.rc_op.get();
                    if (reloc.offset > bytes.len or pointer_bytes > bytes.len - reloc.offset or reloc.function_pointer > 1 or (data_symbol != null and data_symbol.? >= rows.len) or (procedure != null and procedure.? >= proc_count) or ((rc_layout == null) != (rc_op_raw == null))) return error.InvalidLirImage;
                    const rc_layout_idx: ?layout.Idx = if (rc_layout) |raw| idx: {
                        if (raw >= layout_count) return error.InvalidLirImage;
                        const tag = std.math.cast(@typeInfo(layout.Idx).@"enum".tag_type, raw) orelse return error.InvalidLirImage;
                        break :idx @fromBackingInt(tag);
                    } else null;
                    if (capture_offset != null and (capture_offset.? > bytes.len - reloc.offset or procedure == null)) return error.InvalidLirImage;
                    const rc_op: ?layout.RcOp = if (rc_op_raw) |raw| (if (raw == @backingInt(layout.RcOp.incref))
                        .incref
                    else if (raw == @backingInt(layout.RcOp.decref))
                        .decref
                    else if (raw == @backingInt(layout.RcOp.free))
                        .free
                    else if (raw == @backingInt(layout.RcOp.host_drop))
                        .host_drop
                    else
                        return error.InvalidLirImage) else null;

                    out.* = .{
                        .offset = reloc.offset,
                        .addend = reloc.addend,
                        .target_symbol_name = try Image.sliceFromRef(u8, base, size, reloc.name),
                        .target = if (data_symbol) |id| .{ .data_symbol = @fromBackingInt(@intCast(id)) } else .named,
                        .kind = if (reloc.function_pointer != 0) .function_pointer else .address,
                        .callable_capture_offset = reloc.capture_offset,
                        .procedure = if (procedure) |id| .some(@fromBackingInt(@intCast(id))) else .none,
                        .rc_helper = if (rc_layout_idx) |layout_idx| .{ .op = rc_op.?, .layout_idx = layout_idx } else null,
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
