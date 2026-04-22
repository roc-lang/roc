//! Authoritative compile-time value graph.
//!
//! Semantic compile-time evaluation now retains:
//! - explicit schemas produced from checked source types plus chosen layouts
//! - explicit constant nodes reified from runtime bytes using those schemas
//! - explicit top-level binding entries pointing at `(schema_id, value_id)`
//!
//! No compile-time path is allowed to treat raw runtime addresses as the
//! authoritative fact, and no compile-time path is allowed to rebuild CIR
//! syntax from runtime results.

const std = @import("std");
const base = @import("base");
const types = @import("types");
const layout_mod = @import("layout");
const builtins = @import("builtins");
const runtime_value = @import("value.zig");

const Ident = base.Ident;
const Var = types.Var;
const TypeStore = types.Store;
const Layout = layout_mod.Layout;
const LayoutStore = layout_mod.Store;
const LayoutIdx = layout_mod.Idx;
const RocOps = builtins.host_abi.RocOps;
const RocStr = builtins.str.RocStr;
const RocList = builtins.list.RocList;
const Value = runtime_value.Value;

pub const Error = error{
    OutOfMemory,
    UnsupportedLayout,
    UnsupportedSourceType,
    AbstractSchemaType,
    SchemaLayoutMismatch,
};

pub const SerializationError = Error || error{
    InvalidFormat,
    UnsupportedVersion,
};

pub const SchemaId = enum(u32) {
    _,
};

pub const ValueId = enum(u32) {
    _,
};

pub const Binding = struct {
    schema_id: SchemaId,
    value_id: ValueId,
};

pub const WrappedSchema = struct {
    name: []u8,
    origin_module: []u8,
    backing: SchemaId,
    is_opaque: bool = false,
};

pub const FieldSchema = struct {
    name: []u8,
    schema_id: SchemaId,
};

pub const VariantSchema = struct {
    name: []u8,
    payload_schemas: []SchemaId,
};

pub const Schema = union(enum) {
    pending,
    zst,
    int: types.Int.Precision,
    frac: types.Frac.Precision,
    str,
    list: SchemaId,
    box: SchemaId,
    tuple: []SchemaId,
    record: []FieldSchema,
    tag_union: []VariantSchema,
    alias: WrappedSchema,
    nominal: WrappedSchema,
};

pub const VariantValue = struct {
    tag_index: u32,
    payloads: []ValueId,
};

pub const Constant = union(enum) {
    zst,
    int_bytes: [16]u8,
    f32: f32,
    f64: f64,
    dec: [16]u8,
    str: []u8,
    list: []ValueId,
    box: ValueId,
    tuple: []ValueId,
    record: []ValueId,
    tag_union: VariantValue,
    alias: ValueId,
    nominal: ValueId,
};

pub const SchemaStore = struct {
    allocator: std.mem.Allocator,
    schemas: std.ArrayListUnmanaged(Schema) = .{},

    pub fn init(allocator: std.mem.Allocator) SchemaStore {
        return .{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *SchemaStore) void {
        for (self.schemas.items) |*schema| {
            self.deinitSchema(schema);
        }
        self.schemas.deinit(self.allocator);
    }

    fn deinitSchema(self: *SchemaStore, schema: *Schema) void {
        switch (schema.*) {
            .pending, .zst, .int, .frac, .str, .list, .box => {},
            .tuple => |items| self.allocator.free(items),
            .record => |fields| {
                for (fields) |field| self.allocator.free(field.name);
                self.allocator.free(fields);
            },
            .tag_union => |variants| {
                for (variants) |variant| {
                    self.allocator.free(variant.name);
                    self.allocator.free(variant.payload_schemas);
                }
                self.allocator.free(variants);
            },
            .alias => |wrapped| {
                self.allocator.free(wrapped.name);
                self.allocator.free(wrapped.origin_module);
            },
            .nominal => |wrapped| {
                self.allocator.free(wrapped.name);
                self.allocator.free(wrapped.origin_module);
            },
        }
    }

    pub fn addPending(self: *SchemaStore) Error!SchemaId {
        const idx: u32 = @intCast(self.schemas.items.len);
        try self.schemas.append(self.allocator, .pending);
        return @enumFromInt(idx);
    }

    pub fn add(self: *SchemaStore, schema: Schema) Error!SchemaId {
        const idx: u32 = @intCast(self.schemas.items.len);
        try self.schemas.append(self.allocator, schema);
        return @enumFromInt(idx);
    }

    pub fn overwrite(self: *SchemaStore, id: SchemaId, schema: Schema) void {
        self.deinitSchema(&self.schemas.items[@intFromEnum(id)]);
        self.schemas.items[@intFromEnum(id)] = schema;
    }

    pub fn get(self: *const SchemaStore, id: SchemaId) Schema {
        return self.schemas.items[@intFromEnum(id)];
    }
};

pub const ConstantStore = struct {
    allocator: std.mem.Allocator,
    values: std.ArrayListUnmanaged(Constant) = .{},

    pub fn init(allocator: std.mem.Allocator) ConstantStore {
        return .{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *ConstantStore) void {
        for (self.values.items) |*value| {
            self.deinitConstant(value);
        }
        self.values.deinit(self.allocator);
    }

    fn deinitConstant(self: *ConstantStore, constant: *Constant) void {
        switch (constant.*) {
            .zst, .int_bytes, .f32, .f64, .dec, .box, .alias, .nominal => {},
            .str => |bytes| self.allocator.free(bytes),
            .list => |items| self.allocator.free(items),
            .tuple => |items| self.allocator.free(items),
            .record => |items| self.allocator.free(items),
            .tag_union => |variant| self.allocator.free(variant.payloads),
        }
    }

    pub fn add(self: *ConstantStore, constant: Constant) Error!ValueId {
        const idx: u32 = @intCast(self.values.items.len);
        try self.values.append(self.allocator, constant);
        return @enumFromInt(idx);
    }

    pub fn get(self: *const ConstantStore, id: ValueId) Constant {
        return self.values.items[@intFromEnum(id)];
    }
};

pub const TopLevelBindings = struct {
    bindings: std.AutoHashMap(u32, Binding),

    pub fn init(allocator: std.mem.Allocator) TopLevelBindings {
        return .{
            .bindings = std.AutoHashMap(u32, Binding).init(allocator),
        };
    }

    pub fn deinit(self: *TopLevelBindings) void {
        self.bindings.deinit();
    }

    pub fn bind(self: *TopLevelBindings, pattern_idx: u32, binding: Binding) Error!void {
        try self.bindings.put(pattern_idx, binding);
    }

    pub fn lookup(self: *const TopLevelBindings, pattern_idx: u32) ?Binding {
        return self.bindings.get(pattern_idx);
    }
};

pub const Store = struct {
    schemas: SchemaStore,
    constants: ConstantStore,
    bindings: TopLevelBindings,

    pub fn init(allocator: std.mem.Allocator) Store {
        return .{
            .schemas = SchemaStore.init(allocator),
            .constants = ConstantStore.init(allocator),
            .bindings = TopLevelBindings.init(allocator),
        };
    }

    pub fn deinit(self: *Store) void {
        self.bindings.deinit();
        self.constants.deinit();
        self.schemas.deinit();
    }

    pub fn bind(self: *Store, pattern_idx: u32, binding: Binding) Error!void {
        try self.bindings.bind(pattern_idx, binding);
    }

    pub fn lookupBinding(self: *const Store, pattern_idx: u32) ?Binding {
        return self.bindings.lookup(pattern_idx);
    }

    pub fn isEmpty(self: *const Store) bool {
        return self.schemas.schemas.items.len == 0 and
            self.constants.values.items.len == 0 and
            self.bindings.bindings.count() == 0;
    }

    pub fn serialize(self: *const Store, allocator: std.mem.Allocator) SerializationError![]u8 {
        var writer = BinaryWriter.init(allocator);
        errdefer writer.deinit();

        try writer.writeU32(serialization_magic);
        try writer.writeU32(serialization_version);

        try writer.writeU32(@intCast(self.schemas.schemas.items.len));
        for (self.schemas.schemas.items) |schema| {
            try serializeSchema(&writer, schema);
        }

        try writer.writeU32(@intCast(self.constants.values.items.len));
        for (self.constants.values.items) |constant| {
            try serializeConstant(&writer, constant);
        }

        const binding_count = self.bindings.bindings.count();
        try writer.writeU32(@intCast(binding_count));
        const sorted_keys = try allocator.alloc(u32, binding_count);
        defer allocator.free(sorted_keys);
        var binding_it = self.bindings.bindings.iterator();
        var binding_index: usize = 0;
        while (binding_it.next()) |entry| : (binding_index += 1) {
            sorted_keys[binding_index] = entry.key_ptr.*;
        }
        std.mem.sort(u32, sorted_keys, {}, struct {
            fn lessThan(_: void, a: u32, b: u32) bool {
                return a < b;
            }
        }.lessThan);
        for (sorted_keys) |pattern_idx| {
            const binding = self.bindings.bindings.get(pattern_idx) orelse
                return error.InvalidFormat;
            try writer.writeU32(pattern_idx);
            try writer.writeSchemaId(binding.schema_id);
            try writer.writeValueId(binding.value_id);
        }

        return writer.takeOwnedSlice();
    }

    pub fn deserialize(allocator: std.mem.Allocator, bytes: []const u8) SerializationError!Store {
        var reader = BinaryReader.init(bytes);
        const magic = try reader.readU32();
        if (magic != serialization_magic) return error.InvalidFormat;
        const version = try reader.readU32();
        if (version != serialization_version) return error.UnsupportedVersion;

        var store = Store.init(allocator);
        errdefer store.deinit();

        const schema_count = try reader.readU32();
        var schema_index: u32 = 0;
        while (schema_index < schema_count) : (schema_index += 1) {
            try store.schemas.schemas.append(allocator, try deserializeSchema(&reader, allocator));
        }

        const constant_count = try reader.readU32();
        var constant_index: u32 = 0;
        while (constant_index < constant_count) : (constant_index += 1) {
            try store.constants.values.append(allocator, try deserializeConstant(&reader, allocator));
        }

        const binding_count = try reader.readU32();
        var binding_index: u32 = 0;
        while (binding_index < binding_count) : (binding_index += 1) {
            const pattern_idx = try reader.readU32();
            try store.bindings.bindings.put(allocator, pattern_idx, .{
                .schema_id = try reader.readSchemaId(),
                .value_id = try reader.readValueId(),
            });
        }

        if (!reader.isDone()) return error.InvalidFormat;
        return store;
    }

    pub fn tupleElementBinding(
        self: *const Store,
        schema_id: SchemaId,
        value_id: ValueId,
        index: usize,
    ) ?Binding {
        const unwrapped = self.unwrapTransparent(schema_id, value_id);
        const schema = self.schemas.get(unwrapped.schema_id);
        const constant = self.constants.get(unwrapped.value_id);

        const elem_schemas = switch (schema) {
            .tuple => |items| items,
            else => return null,
        };
        const elem_values = switch (constant) {
            .tuple => |items| items,
            else => return null,
        };
        if (index >= elem_schemas.len or index >= elem_values.len) return null;

        return .{
            .schema_id = elem_schemas[index],
            .value_id = elem_values[index],
        };
    }

    pub fn buildSchema(
        self: *Store,
        type_store: *const TypeStore,
        ident_store: *const Ident.Store,
        root_var: Var,
        layout_store: *const LayoutStore,
        layout_idx: LayoutIdx,
    ) Error!SchemaId {
        var builder = SchemaBuilder.init(
            self,
            type_store,
            ident_store,
            layout_store,
        );
        defer builder.deinit();
        return builder.build(root_var, layout_idx);
    }

    pub fn reify(
        self: *Store,
        schema_id: SchemaId,
        value: Value,
        layout_store: *const LayoutStore,
        layout_idx: LayoutIdx,
        roc_ops: *RocOps,
    ) Error!ValueId {
        return self.reifyWithSchema(
            self.schemas.get(schema_id),
            value,
            layout_store,
            layout_idx,
            roc_ops,
        );
    }

    pub fn unwrapTransparent(self: *const Store, schema_id: SchemaId, value_id: ValueId) struct {
        schema_id: SchemaId,
        value_id: ValueId,
    } {
        var current_schema_id = schema_id;
        var current_value_id = value_id;

        while (true) {
            const schema = self.schemas.get(current_schema_id);
            const constant = self.constants.get(current_value_id);
            switch (schema) {
                .alias => |wrapped| {
                    current_schema_id = wrapped.backing;
                    current_value_id = switch (constant) {
                        .alias => |inner| inner,
                        else => break,
                    };
                },
                .nominal => |wrapped| {
                    current_schema_id = wrapped.backing;
                    current_value_id = switch (constant) {
                        .nominal => |inner| inner,
                        else => break,
                    };
                },
                else => break,
            }
        }

        return .{
            .schema_id = current_schema_id,
            .value_id = current_value_id,
        };
    }

    pub fn intAsI128(self: *const Store, schema_id: SchemaId, value_id: ValueId) ?i128 {
        const unwrapped = self.unwrapTransparent(schema_id, value_id);
        const schema = self.schemas.get(unwrapped.schema_id);
        const constant = self.constants.get(unwrapped.value_id);
        const precision = switch (schema) {
            .int => |p| p,
            else => return null,
        };
        const bytes = switch (constant) {
            .int_bytes => |b| b,
            else => return null,
        };
        return switch (precision) {
            .u8 => @as(i128, bytes[0]),
            .i8 => @as(i128, @intCast(@as(i8, @bitCast(bytes[0])))),
            .u16 => @as(i128, std.mem.readInt(u16, bytes[0..2], .little)),
            .i16 => @as(i128, std.mem.readInt(i16, bytes[0..2], .little)),
            .u32 => @as(i128, std.mem.readInt(u32, bytes[0..4], .little)),
            .i32 => @as(i128, std.mem.readInt(i32, bytes[0..4], .little)),
            .u64 => @as(i128, @intCast(std.mem.readInt(u64, bytes[0..8], .little))),
            .i64 => @as(i128, std.mem.readInt(i64, bytes[0..8], .little)),
            .u128 => @as(i128, @intCast(std.mem.readInt(u128, bytes[0..16], .little))),
            .i128 => std.mem.readInt(i128, bytes[0..16], .little),
        };
    }

    fn reifyWithSchema(
        self: *Store,
        schema: Schema,
        value: Value,
        layout_store: *const LayoutStore,
        layout_idx: LayoutIdx,
        roc_ops: *RocOps,
    ) Error!ValueId {
        const layout = layout_store.getLayout(layout_idx);
        return switch (schema) {
            .pending => std.debug.panic(
                "eval.comptime_value.reifyWithSchema reached pending schema during reification",
                .{},
            ),
            .zst => try self.constants.add(.zst),
            .int => |precision| try self.reifyInt(layout, value, precision),
            .frac => |precision| try self.reifyFrac(layout, value, precision),
            .str => blk: {
                if (!(layout.tag == .scalar and layout.data.scalar.tag == .str)) return error.SchemaLayoutMismatch;
                const roc_str = value.read(RocStr);
                break :blk try self.constants.add(.{ .str = try self.constants.allocator.dupe(u8, roc_str.asSlice()) });
            },
            .list => |elem_schema_id| try self.reifyList(elem_schema_id, value, layout_store, layout_idx, roc_ops),
            .box => |inner_schema_id| try self.reifyBox(inner_schema_id, value, layout_store, layout_idx, roc_ops),
            .tuple => |elem_schemas| try self.reifyTupleLike(elem_schemas, value, layout_store, layout_idx, false, roc_ops),
            .record => |field_schemas| try self.reifyRecord(field_schemas, value, layout_store, layout_idx, roc_ops),
            .tag_union => |variants| try self.reifyTagUnion(variants, value, layout_store, layout_idx, roc_ops),
            .alias => |wrapped| blk: {
                const inner = try self.reify(wrapped.backing, value, layout_store, layout_idx, roc_ops);
                break :blk try self.constants.add(.{ .alias = inner });
            },
            .nominal => |wrapped| blk: {
                const inner = try self.reify(wrapped.backing, value, layout_store, layout_idx, roc_ops);
                break :blk try self.constants.add(.{ .nominal = inner });
            },
        };
    }

    fn reifyInt(
        self: *Store,
        layout: Layout,
        value: Value,
        precision: types.Int.Precision,
    ) Error!ValueId {
        if (!(layout.tag == .scalar and layout.data.scalar.tag == .int)) return error.SchemaLayoutMismatch;
        var bytes = std.mem.zeroes([16]u8);
        switch (precision) {
            .u8 => bytes[0] = value.read(u8),
            .i8 => bytes[0] = @bitCast(value.read(i8)),
            .u16 => std.mem.writeInt(u16, bytes[0..2], value.read(u16), .little),
            .i16 => std.mem.writeInt(i16, bytes[0..2], value.read(i16), .little),
            .u32 => std.mem.writeInt(u32, bytes[0..4], value.read(u32), .little),
            .i32 => std.mem.writeInt(i32, bytes[0..4], value.read(i32), .little),
            .u64 => std.mem.writeInt(u64, bytes[0..8], value.read(u64), .little),
            .i64 => std.mem.writeInt(i64, bytes[0..8], value.read(i64), .little),
            .u128 => std.mem.writeInt(u128, bytes[0..16], value.read(u128), .little),
            .i128 => std.mem.writeInt(i128, bytes[0..16], value.read(i128), .little),
        }
        return try self.constants.add(.{ .int_bytes = bytes });
    }

    fn reifyFrac(
        self: *Store,
        layout: Layout,
        value: Value,
        precision: types.Frac.Precision,
    ) Error!ValueId {
        if (!(layout.tag == .scalar and layout.data.scalar.tag == .frac)) return error.SchemaLayoutMismatch;
        return switch (precision) {
            .f32 => try self.constants.add(.{ .f32 = value.read(f32) }),
            .f64 => try self.constants.add(.{ .f64 = value.read(f64) }),
            .dec => blk: {
                var bytes = std.mem.zeroes([16]u8);
                @memcpy(bytes[0..16], value.readBytes(16));
                break :blk try self.constants.add(.{ .dec = bytes });
            },
        };
    }

    fn reifyList(
        self: *Store,
        elem_schema_id: SchemaId,
        value: Value,
        layout_store: *const LayoutStore,
        layout_idx: LayoutIdx,
        roc_ops: *RocOps,
    ) Error!ValueId {
        const layout = layout_store.getLayout(layout_idx);
        if (!(layout.tag == .list or layout.tag == .list_of_zst)) return error.SchemaLayoutMismatch;
        const info = layout_store.getListInfo(layout);
        const roc_list = value.read(RocList);
        const count = roc_list.len();
        var items = try self.constants.allocator.alloc(ValueId, count);
        errdefer self.constants.allocator.free(items);

        if (count == 0) {
            return try self.constants.add(.{ .list = items });
        }

        if (layout.tag == .list_of_zst) {
            const zst_constant = try self.reify(elem_schema_id, Value.zst, layout_store, .zst, roc_ops);
            for (items) |*slot| slot.* = zst_constant;
            return try self.constants.add(.{ .list = items });
        }

        const base_ptr = roc_list.getAllocationDataPtr(roc_ops) orelse return error.SchemaLayoutMismatch;
        var iter = info.iterateElements(base_ptr, count);
        var i: usize = 0;
        while (iter.next()) |elem_ptr| : (i += 1) {
            items[i] = try self.reify(
                elem_schema_id,
                .{ .ptr = elem_ptr },
                layout_store,
                info.elem_layout_idx,
                roc_ops,
            );
        }
        return try self.constants.add(.{ .list = items });
    }

    fn reifyBox(
        self: *Store,
        inner_schema_id: SchemaId,
        value: Value,
        layout_store: *const LayoutStore,
        layout_idx: LayoutIdx,
        roc_ops: *RocOps,
    ) Error!ValueId {
        const layout = layout_store.getLayout(layout_idx);
        if (!(layout.tag == .box or layout.tag == .box_of_zst)) return error.SchemaLayoutMismatch;
        if (layout.tag == .box_of_zst) {
            const inner = try self.reify(inner_schema_id, Value.zst, layout_store, .zst, roc_ops);
            return try self.constants.add(.{ .box = inner });
        }

        const info = layout_store.getBoxInfo(layout);
        const boxed_addr = value.read(usize);
        if (boxed_addr == 0) return error.SchemaLayoutMismatch;
        const inner = try self.reify(
            inner_schema_id,
            .{ .ptr = @ptrFromInt(boxed_addr) },
            layout_store,
            info.elem_layout_idx,
            roc_ops,
        );
        return try self.constants.add(.{ .box = inner });
    }

    fn reifyTupleLike(
        self: *Store,
        elem_schemas: []const SchemaId,
        value: Value,
        layout_store: *const LayoutStore,
        layout_idx: LayoutIdx,
        comptime is_record: bool,
        roc_ops: *RocOps,
    ) Error!ValueId {
        const layout = layout_store.getLayout(layout_idx);
        if (layout.tag == .zst and elem_schemas.len == 0) {
            return if (is_record)
                try self.constants.add(.{ .record = try self.constants.allocator.alloc(ValueId, 0) })
            else
                try self.constants.add(.{ .tuple = try self.constants.allocator.alloc(ValueId, 0) });
        }
        if (layout.tag != .struct_) return error.SchemaLayoutMismatch;

        const info = layout_store.getStructInfo(layout);
        var items = try self.constants.allocator.alloc(ValueId, elem_schemas.len);
        errdefer self.constants.allocator.free(items);

        const struct_idx = layout.data.struct_.idx;
        for (elem_schemas, 0..) |elem_schema_id, i| {
            const field_offset = layout_store.getStructFieldOffsetByOriginalIndex(struct_idx, @intCast(i));
            const field_layout_idx = layout_store.getStructFieldLayoutByOriginalIndex(struct_idx, @intCast(i));
            items[i] = try self.reify(
                elem_schema_id,
                value.offset(field_offset),
                layout_store,
                field_layout_idx,
                roc_ops,
            );
        }
        _ = info;
        return if (is_record)
            try self.constants.add(.{ .record = items })
        else
            try self.constants.add(.{ .tuple = items });
    }

    fn reifyRecord(
        self: *Store,
        field_schemas: []const FieldSchema,
        value: Value,
        layout_store: *const LayoutStore,
        layout_idx: LayoutIdx,
        roc_ops: *RocOps,
    ) Error!ValueId {
        const schema_ids = try self.constants.allocator.alloc(SchemaId, field_schemas.len);
        defer self.constants.allocator.free(schema_ids);
        for (field_schemas, 0..) |field, i| schema_ids[i] = field.schema_id;
        return try self.reifyTupleLike(schema_ids, value, layout_store, layout_idx, true, roc_ops);
    }

    fn reifyTagUnion(
        self: *Store,
        variants: []const VariantSchema,
        value: Value,
        layout_store: *const LayoutStore,
        layout_idx: LayoutIdx,
        roc_ops: *RocOps,
    ) Error!ValueId {
        const layout = layout_store.getLayout(layout_idx);
        if (layout.tag != .tag_union) return error.SchemaLayoutMismatch;

        const info = layout_store.getTagUnionInfo(layout);
        const disc: usize = info.readDiscriminant(value.ptr);
        if (disc >= variants.len) return error.SchemaLayoutMismatch;

        const variant_schema = variants[disc];
        const payload_layout_idx = info.variants[disc].payload_layout;
        var payloads = try self.constants.allocator.alloc(ValueId, variant_schema.payload_schemas.len);
        errdefer self.constants.allocator.free(payloads);

        if (variant_schema.payload_schemas.len == 0) {
            return try self.constants.add(.{ .tag_union = .{
                .tag_index = @intCast(disc),
                .payloads = payloads,
            } });
        }

        const payload_layout = layout_store.getLayout(payload_layout_idx);
        if (variant_schema.payload_schemas.len == 1 and payload_layout.tag != .struct_) {
            payloads[0] = try self.reify(
                variant_schema.payload_schemas[0],
                value,
                layout_store,
                payload_layout_idx,
                roc_ops,
            );
        } else {
            if (payload_layout.tag != .struct_) return error.SchemaLayoutMismatch;
            const struct_idx = payload_layout.data.struct_.idx;
            for (variant_schema.payload_schemas, 0..) |payload_schema_id, i| {
                const field_offset = layout_store.getStructFieldOffsetByOriginalIndex(struct_idx, @intCast(i));
                const field_layout_idx = layout_store.getStructFieldLayoutByOriginalIndex(struct_idx, @intCast(i));
                payloads[i] = try self.reify(
                    payload_schema_id,
                    value.offset(field_offset),
                    layout_store,
                    field_layout_idx,
                    roc_ops,
                );
            }
        }

        return try self.constants.add(.{ .tag_union = .{
            .tag_index = @intCast(disc),
            .payloads = payloads,
        } });
    }
};

const serialization_magic: u32 = 0x43545631; // CTV1
const serialization_version: u32 = 1;

const SerializedSchemaTag = enum(u8) {
    pending,
    zst,
    int,
    frac,
    str,
    list,
    box,
    tuple,
    record,
    tag_union,
    alias,
    nominal,
};

const SerializedConstantTag = enum(u8) {
    zst,
    int_bytes,
    f32,
    f64,
    dec,
    str,
    list,
    box,
    tuple,
    record,
    tag_union,
    alias,
    nominal,
};

const BinaryWriter = struct {
    bytes: std.ArrayList(u8),

    fn init(allocator: std.mem.Allocator) BinaryWriter {
        return .{ .bytes = std.ArrayList(u8).init(allocator) };
    }

    fn deinit(self: *BinaryWriter) void {
        self.bytes.deinit();
    }

    fn takeOwnedSlice(self: *BinaryWriter) SerializationError![]u8 {
        return try self.bytes.toOwnedSlice();
    }

    fn writeU8(self: *BinaryWriter, value: u8) SerializationError!void {
        try self.bytes.append(value);
    }

    fn writeU32(self: *BinaryWriter, value: u32) SerializationError!void {
        var buf: [4]u8 = undefined;
        std.mem.writeInt(u32, buf[0..4], value, .little);
        try self.bytes.appendSlice(buf[0..4]);
    }

    fn writeBytes(self: *BinaryWriter, bytes: []const u8) SerializationError!void {
        try self.bytes.appendSlice(bytes);
    }

    fn writeString(self: *BinaryWriter, bytes: []const u8) SerializationError!void {
        try self.writeU32(@intCast(bytes.len));
        try self.writeBytes(bytes);
    }

    fn writeSchemaId(self: *BinaryWriter, id: SchemaId) SerializationError!void {
        try self.writeU32(@intFromEnum(id));
    }

    fn writeValueId(self: *BinaryWriter, id: ValueId) SerializationError!void {
        try self.writeU32(@intFromEnum(id));
    }
};

const BinaryReader = struct {
    bytes: []const u8,
    index: usize = 0,

    fn init(bytes: []const u8) BinaryReader {
        return .{ .bytes = bytes };
    }

    fn isDone(self: *const BinaryReader) bool {
        return self.index == self.bytes.len;
    }

    fn readBytes(self: *BinaryReader, count: usize) SerializationError![]const u8 {
        const end = self.index + count;
        if (end > self.bytes.len) return error.InvalidFormat;
        const out = self.bytes[self.index..end];
        self.index = end;
        return out;
    }

    fn readU8(self: *BinaryReader) SerializationError!u8 {
        return (try self.readBytes(1))[0];
    }

    fn readU32(self: *BinaryReader) SerializationError!u32 {
        const bytes = try self.readBytes(4);
        return std.mem.readInt(u32, bytes[0..4], .little);
    }

    fn readString(self: *BinaryReader, allocator: std.mem.Allocator) SerializationError![]u8 {
        const len = try self.readU32();
        return try allocator.dupe(u8, try self.readBytes(len));
    }

    fn readSchemaId(self: *BinaryReader) SerializationError!SchemaId {
        return @enumFromInt(try self.readU32());
    }

    fn readValueId(self: *BinaryReader) SerializationError!ValueId {
        return @enumFromInt(try self.readU32());
    }
};

fn serializeSchema(writer: *BinaryWriter, schema: Schema) SerializationError!void {
    switch (schema) {
        .pending => try writer.writeU8(@intFromEnum(SerializedSchemaTag.pending)),
        .zst => try writer.writeU8(@intFromEnum(SerializedSchemaTag.zst)),
        .int => |precision| {
            try writer.writeU8(@intFromEnum(SerializedSchemaTag.int));
            try writer.writeU8(@intFromEnum(precision));
        },
        .frac => |precision| {
            try writer.writeU8(@intFromEnum(SerializedSchemaTag.frac));
            try writer.writeU8(@intFromEnum(precision));
        },
        .str => try writer.writeU8(@intFromEnum(SerializedSchemaTag.str)),
        .list => |elem_schema_id| {
            try writer.writeU8(@intFromEnum(SerializedSchemaTag.list));
            try writer.writeSchemaId(elem_schema_id);
        },
        .box => |inner_schema_id| {
            try writer.writeU8(@intFromEnum(SerializedSchemaTag.box));
            try writer.writeSchemaId(inner_schema_id);
        },
        .tuple => |items| {
            try writer.writeU8(@intFromEnum(SerializedSchemaTag.tuple));
            try writer.writeU32(@intCast(items.len));
            for (items) |item| try writer.writeSchemaId(item);
        },
        .record => |fields| {
            try writer.writeU8(@intFromEnum(SerializedSchemaTag.record));
            try writer.writeU32(@intCast(fields.len));
            for (fields) |field| {
                try writer.writeString(field.name);
                try writer.writeSchemaId(field.schema_id);
            }
        },
        .tag_union => |variants| {
            try writer.writeU8(@intFromEnum(SerializedSchemaTag.tag_union));
            try writer.writeU32(@intCast(variants.len));
            for (variants) |variant| {
                try writer.writeString(variant.name);
                try writer.writeU32(@intCast(variant.payload_schemas.len));
                for (variant.payload_schemas) |payload_schema| {
                    try writer.writeSchemaId(payload_schema);
                }
            }
        },
        .alias => |wrapped| {
            try writer.writeU8(@intFromEnum(SerializedSchemaTag.alias));
            try serializeWrappedSchema(writer, wrapped);
        },
        .nominal => |wrapped| {
            try writer.writeU8(@intFromEnum(SerializedSchemaTag.nominal));
            try serializeWrappedSchema(writer, wrapped);
        },
    }
}

fn serializeWrappedSchema(writer: *BinaryWriter, wrapped: WrappedSchema) SerializationError!void {
    try writer.writeString(wrapped.name);
    try writer.writeString(wrapped.origin_module);
    try writer.writeSchemaId(wrapped.backing);
    try writer.writeU8(if (wrapped.is_opaque) 1 else 0);
}

fn deserializeSchema(reader: *BinaryReader, allocator: std.mem.Allocator) SerializationError!Schema {
    const tag = try std.meta.intToEnum(SerializedSchemaTag, try reader.readU8());
    return switch (tag) {
        .pending => .pending,
        .zst => .zst,
        .int => .{ .int = try std.meta.intToEnum(types.Int.Precision, try reader.readU8()) },
        .frac => .{ .frac = try std.meta.intToEnum(types.Frac.Precision, try reader.readU8()) },
        .str => .str,
        .list => .{ .list = try reader.readSchemaId() },
        .box => .{ .box = try reader.readSchemaId() },
        .tuple => blk: {
            const len = try reader.readU32();
            const items = try allocator.alloc(SchemaId, len);
            errdefer allocator.free(items);
            for (items) |*item| item.* = try reader.readSchemaId();
            break :blk .{ .tuple = items };
        },
        .record => blk: {
            const len = try reader.readU32();
            const fields = try allocator.alloc(FieldSchema, len);
            errdefer allocator.free(fields);
            var i: usize = 0;
            errdefer while (i > 0) : (i -= 1) allocator.free(fields[i - 1].name);
            while (i < fields.len) : (i += 1) {
                fields[i] = .{
                    .name = try reader.readString(allocator),
                    .schema_id = try reader.readSchemaId(),
                };
            }
            break :blk .{ .record = fields };
        },
        .tag_union => blk: {
            const len = try reader.readU32();
            const variants = try allocator.alloc(VariantSchema, len);
            errdefer allocator.free(variants);
            var i: usize = 0;
            errdefer while (i > 0) : (i -= 1) {
                allocator.free(variants[i - 1].name);
                allocator.free(variants[i - 1].payload_schemas);
            };
            while (i < variants.len) : (i += 1) {
                const name = try reader.readString(allocator);
                errdefer allocator.free(name);
                const payload_len = try reader.readU32();
                const payload_schemas = try allocator.alloc(SchemaId, payload_len);
                errdefer allocator.free(payload_schemas);
                for (payload_schemas) |*payload_schema| payload_schema.* = try reader.readSchemaId();
                variants[i] = .{
                    .name = name,
                    .payload_schemas = payload_schemas,
                };
            }
            break :blk .{ .tag_union = variants };
        },
        .alias => .{ .alias = try deserializeWrappedSchema(reader, allocator) },
        .nominal => .{ .nominal = try deserializeWrappedSchema(reader, allocator) },
    };
}

fn deserializeWrappedSchema(reader: *BinaryReader, allocator: std.mem.Allocator) SerializationError!WrappedSchema {
    const name = try reader.readString(allocator);
    errdefer allocator.free(name);
    const origin_module = try reader.readString(allocator);
    errdefer allocator.free(origin_module);
    return .{
        .name = name,
        .origin_module = origin_module,
        .backing = try reader.readSchemaId(),
        .is_opaque = switch (try reader.readU8()) {
            0 => false,
            1 => true,
            else => return error.InvalidFormat,
        },
    };
}

fn serializeConstant(writer: *BinaryWriter, constant: Constant) SerializationError!void {
    switch (constant) {
        .zst => try writer.writeU8(@intFromEnum(SerializedConstantTag.zst)),
        .int_bytes => |bytes| {
            try writer.writeU8(@intFromEnum(SerializedConstantTag.int_bytes));
            try writer.writeBytes(&bytes);
        },
        .f32 => |value| {
            try writer.writeU8(@intFromEnum(SerializedConstantTag.f32));
            try writer.writeU32(@bitCast(value));
        },
        .f64 => |value| {
            try writer.writeU8(@intFromEnum(SerializedConstantTag.f64));
            var buf: [8]u8 = undefined;
            std.mem.writeInt(u64, buf[0..8], @bitCast(value), .little);
            try writer.writeBytes(buf[0..8]);
        },
        .dec => |bytes| {
            try writer.writeU8(@intFromEnum(SerializedConstantTag.dec));
            try writer.writeBytes(&bytes);
        },
        .str => |bytes| {
            try writer.writeU8(@intFromEnum(SerializedConstantTag.str));
            try writer.writeString(bytes);
        },
        .list => |items| {
            try writer.writeU8(@intFromEnum(SerializedConstantTag.list));
            try writer.writeU32(@intCast(items.len));
            for (items) |item| try writer.writeValueId(item);
        },
        .box => |inner| {
            try writer.writeU8(@intFromEnum(SerializedConstantTag.box));
            try writer.writeValueId(inner);
        },
        .tuple => |items| {
            try writer.writeU8(@intFromEnum(SerializedConstantTag.tuple));
            try writer.writeU32(@intCast(items.len));
            for (items) |item| try writer.writeValueId(item);
        },
        .record => |items| {
            try writer.writeU8(@intFromEnum(SerializedConstantTag.record));
            try writer.writeU32(@intCast(items.len));
            for (items) |item| try writer.writeValueId(item);
        },
        .tag_union => |variant| {
            try writer.writeU8(@intFromEnum(SerializedConstantTag.tag_union));
            try writer.writeU32(variant.tag_index);
            try writer.writeU32(@intCast(variant.payloads.len));
            for (variant.payloads) |payload| try writer.writeValueId(payload);
        },
        .alias => |inner| {
            try writer.writeU8(@intFromEnum(SerializedConstantTag.alias));
            try writer.writeValueId(inner);
        },
        .nominal => |inner| {
            try writer.writeU8(@intFromEnum(SerializedConstantTag.nominal));
            try writer.writeValueId(inner);
        },
    }
}

fn deserializeConstant(reader: *BinaryReader, allocator: std.mem.Allocator) SerializationError!Constant {
    const tag = try std.meta.intToEnum(SerializedConstantTag, try reader.readU8());
    return switch (tag) {
        .zst => .zst,
        .int_bytes => blk: {
            var bytes: [16]u8 = undefined;
            @memcpy(bytes[0..], try reader.readBytes(bytes.len));
            break :blk .{ .int_bytes = bytes };
        },
        .f32 => .{ .f32 = @bitCast(try reader.readU32()) },
        .f64 => blk: {
            const bytes = try reader.readBytes(8);
            break :blk .{ .f64 = @bitCast(std.mem.readInt(u64, bytes[0..8], .little)) };
        },
        .dec => blk: {
            var bytes: [16]u8 = undefined;
            @memcpy(bytes[0..], try reader.readBytes(bytes.len));
            break :blk .{ .dec = bytes };
        },
        .str => .{ .str = try reader.readString(allocator) },
        .list => .{ .list = try deserializeValueIdSlice(reader, allocator) },
        .box => .{ .box = try reader.readValueId() },
        .tuple => .{ .tuple = try deserializeValueIdSlice(reader, allocator) },
        .record => .{ .record = try deserializeValueIdSlice(reader, allocator) },
        .tag_union => blk: {
            const tag_index = try reader.readU32();
            const payloads = try deserializeValueIdSlice(reader, allocator);
            break :blk .{ .tag_union = .{
                .tag_index = tag_index,
                .payloads = payloads,
            } };
        },
        .alias => .{ .alias = try reader.readValueId() },
        .nominal => .{ .nominal = try reader.readValueId() },
    };
}

fn deserializeValueIdSlice(reader: *BinaryReader, allocator: std.mem.Allocator) SerializationError![]ValueId {
    const len = try reader.readU32();
    const values = try allocator.alloc(ValueId, len);
    errdefer allocator.free(values);
    for (values) |*value_id| value_id.* = try reader.readValueId();
    return values;
}

const SchemaBuilder = struct {
    store: *Store,
    type_store: *const TypeStore,
    ident_store: *const Ident.Store,
    layout_store: *const LayoutStore,
    cache: std.AutoHashMap(Var, SchemaId),

    fn init(
        store: *Store,
        type_store: *const TypeStore,
        ident_store: *const Ident.Store,
        layout_store: *const LayoutStore,
    ) SchemaBuilder {
        return .{
            .store = store,
            .type_store = type_store,
            .ident_store = ident_store,
            .layout_store = layout_store,
            .cache = std.AutoHashMap(Var, SchemaId).init(store.schemas.allocator),
        };
    }

    fn deinit(self: *SchemaBuilder) void {
        self.cache.deinit();
    }

    fn build(self: *SchemaBuilder, var_: Var, layout_idx: LayoutIdx) Error!SchemaId {
        const resolved = self.type_store.resolveVar(var_);
        switch (resolved.desc.content) {
            .flex, .rigid => return error.AbstractSchemaType,
            .err => return error.UnsupportedSourceType,
            .alias => |alias| if (self.isBuiltinOrigin(alias.origin_module)) {
                return self.buildBuiltinAlias(alias, layout_idx);
            },
            .structure => |flat| switch (flat) {
                .nominal_type => |nominal| if (self.isBuiltinOrigin(nominal.origin_module)) {
                    return self.buildBuiltinNominal(nominal, layout_idx);
                },
                else => {},
            },
        }

        if (self.cache.get(resolved.var_)) |existing| return existing;

        const placeholder = try self.store.schemas.addPending();
        try self.cache.put(resolved.var_, placeholder);

        const schema = try self.buildNonBuiltinResolved(resolved.desc.content, layout_idx);
        self.store.schemas.schemas.items[@intFromEnum(placeholder)] = schema;
        return placeholder;
    }

    fn buildBuiltinAlias(self: *SchemaBuilder, alias: types.Alias, layout_idx: LayoutIdx) Error!SchemaId {
        return self.build(self.type_store.getAliasBackingVar(alias), layout_idx);
    }

    fn buildBuiltinNominal(self: *SchemaBuilder, nominal: types.NominalType, layout_idx: LayoutIdx) Error!SchemaId {
        const layout = self.layout_store.getLayout(layout_idx);
        return switch (layout.tag) {
            .list, .list_of_zst => blk: {
                const args = self.type_store.sliceNominalArgs(nominal);
                if (args.len == 0) return error.UnsupportedSourceType;
                const child_layout_idx = self.layout_store.getListInfo(layout).elem_layout_idx;
                break :blk try self.store.schemas.add(.{
                    .list = try self.build(args[0], child_layout_idx),
                });
            },
            .box, .box_of_zst => blk: {
                const args = self.type_store.sliceNominalArgs(nominal);
                if (args.len == 0) return error.UnsupportedSourceType;
                const child_layout_idx = self.layout_store.getBoxInfo(layout).elem_layout_idx;
                break :blk try self.store.schemas.add(.{
                    .box = try self.build(args[0], child_layout_idx),
                });
            },
            else => self.build(self.type_store.getNominalBackingVar(nominal), layout_idx),
        };
    }

    fn buildNonBuiltinResolved(self: *SchemaBuilder, content: types.Content, layout_idx: LayoutIdx) Error!Schema {
        const layout = self.layout_store.getLayout(layout_idx);
        return switch (content) {
            .flex, .rigid, .err => error.UnsupportedSourceType,
            .alias => |alias| .{ .alias = .{
                .name = try self.dupeIdent(alias.ident.ident_idx),
                .origin_module = try self.dupeIdent(alias.origin_module),
                .backing = try self.build(self.type_store.getAliasBackingVar(alias), layout_idx),
            } },
            .structure => |flat| switch (flat) {
                .nominal_type => |nominal| .{ .nominal = .{
                    .name = try self.dupeIdent(nominal.ident.ident_idx),
                    .origin_module = try self.dupeIdent(nominal.origin_module),
                    .backing = try self.build(self.type_store.getNominalBackingVar(nominal), layout_idx),
                    .is_opaque = nominal.is_opaque,
                } },
                .tuple => |tuple| try self.buildTuple(tuple, layout),
                .record => |record| try self.buildRecord(record.fields, layout),
                .record_unbound => |fields| try self.buildRecord(fields, layout),
                .empty_record => .{ .record = try self.store.schemas.allocator.alloc(FieldSchema, 0) },
                .tag_union => |tag_union| try self.buildTagUnion(tag_union, layout),
                .empty_tag_union => .{ .tag_union = try self.store.schemas.allocator.alloc(VariantSchema, 0) },
                .fn_pure, .fn_effectful, .fn_unbound => error.UnsupportedSourceType,
            },
        };
    }

    fn buildTuple(self: *SchemaBuilder, tuple: types.Tuple, layout: Layout) Error!Schema {
        if (layout.tag == .zst) {
            const elems = self.type_store.sliceVars(tuple.elems);
            if (elems.len != 0) return error.SchemaLayoutMismatch;
            return .{ .tuple = try self.store.schemas.allocator.alloc(SchemaId, 0) };
        }
        if (layout.tag != .struct_) return error.SchemaLayoutMismatch;
        const elems = self.type_store.sliceVars(tuple.elems);
        const items = try self.store.schemas.allocator.alloc(SchemaId, elems.len);
        for (elems, 0..) |elem_var, i| {
            const field_layout_idx = layout.data.struct_.idx;
            items[i] = try self.build(
                elem_var,
                self.layout_store.getStructFieldLayoutByOriginalIndex(field_layout_idx, @intCast(i)),
            );
        }
        return .{ .tuple = items };
    }

    fn buildRecord(self: *SchemaBuilder, fields_range: anytype, layout: Layout) Error!Schema {
        if (layout.tag == .zst) {
            const fields = self.type_store.getRecordFieldsSlice(fields_range);
            if (fields.len != 0) return error.SchemaLayoutMismatch;
            return .{ .record = try self.store.schemas.allocator.alloc(FieldSchema, 0) };
        }
        if (layout.tag != .struct_) return error.SchemaLayoutMismatch;
        const struct_idx = layout.data.struct_.idx;
        const fields = self.type_store.getRecordFieldsSlice(fields_range);
        const items = try self.store.schemas.allocator.alloc(FieldSchema, fields.len);
        for (fields, 0..) |field, i| {
            items[i] = .{
                .name = try self.dupeIdent(field.name),
                .schema_id = try self.build(
                    field.var_,
                    self.layout_store.getStructFieldLayoutByOriginalIndex(struct_idx, @intCast(i)),
                ),
            };
        }
        return .{ .record = items };
    }

    fn buildTagUnion(self: *SchemaBuilder, tag_union: types.TagUnion, layout: Layout) Error!Schema {
        if (layout.tag != .tag_union) return error.SchemaLayoutMismatch;
        const tags = self.type_store.getTagsSlice(tag_union.tags);
        const info = self.layout_store.getTagUnionInfo(layout);
        if (tags.len != info.variants.len) return error.SchemaLayoutMismatch;

        const items = try self.store.schemas.allocator.alloc(VariantSchema, tags.len);
        for (tags, 0..) |tag, i| {
            const payload_schemas = try self.store.schemas.allocator.alloc(SchemaId, tag.args.count);
            if (tag.args.count == 1 and info.variants[i].payload_layout != .zst and self.layout_store.getLayout(info.variants[i].payload_layout).tag != .struct_) {
                const only_arg = self.type_store.getVarAt(tag.args, 0);
                payload_schemas[0] = try self.build(only_arg, info.variants[i].payload_layout);
            } else {
                const payload_layout = self.layout_store.getLayout(info.variants[i].payload_layout);
                if (tag.args.count == 0) {
                    // nothing
                } else if (payload_layout.tag != .struct_) {
                    return error.SchemaLayoutMismatch;
                } else {
                    const payload_struct_idx = payload_layout.data.struct_.idx;
                    for (0..tag.args.count) |arg_index| {
                        const arg_var = self.type_store.getVarAt(tag.args, @intCast(arg_index));
                        payload_schemas[arg_index] = try self.build(
                            arg_var,
                            self.layout_store.getStructFieldLayoutByOriginalIndex(payload_struct_idx, @intCast(arg_index)),
                        );
                    }
                }
            }
            items[i] = .{
                .name = try self.dupeIdent(tag.name),
                .payload_schemas = payload_schemas,
            };
        }
        return .{ .tag_union = items };
    }

    fn isBuiltinOrigin(self: *const SchemaBuilder, origin_module: Ident.Idx) bool {
        return std.mem.eql(u8, self.ident_store.getText(origin_module), "Builtin");
    }

    fn dupeIdent(self: *SchemaBuilder, ident: Ident.Idx) Error![]u8 {
        return try self.store.schemas.allocator.dupe(u8, self.ident_store.getText(ident));
    }
};

test "top-level bindings bind and lookup" {
    var bindings = TopLevelBindings.init(std.testing.allocator);
    defer bindings.deinit();

    try bindings.bind(42, .{
        .schema_id = @enumFromInt(1),
        .value_id = @enumFromInt(2),
    });

    const looked_up = bindings.lookup(42);
    try std.testing.expect(looked_up != null);
    try std.testing.expectEqual(@as(u32, 1), @intFromEnum(looked_up.?.schema_id));
    try std.testing.expectEqual(@as(u32, 2), @intFromEnum(looked_up.?.value_id));
}

test "store unwraps transparent wrappers" {
    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const leaf_schema = try store.schemas.add(.{ .int = .i64 });
    const alias_schema = try store.schemas.add(.{ .alias = .{
        .name = try std.testing.allocator.dupe(u8, "Age"),
        .origin_module = try std.testing.allocator.dupe(u8, "Test"),
        .backing = leaf_schema,
    } });
    const leaf_value = try store.constants.add(.{ .int_bytes = std.mem.zeroes([16]u8) });
    const alias_value = try store.constants.add(.{ .alias = leaf_value });

    const unwrapped = store.unwrapTransparent(alias_schema, alias_value);
    try std.testing.expectEqual(@intFromEnum(leaf_schema), @intFromEnum(unwrapped.schema_id));
    try std.testing.expectEqual(@intFromEnum(leaf_value), @intFromEnum(unwrapped.value_id));
}
