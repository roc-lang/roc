//! Type extraction module for generating glue code.
//!
//! This module extracts type information from compiled Roc modules
//! and converts it into a format suitable for glue code generation.
//! The output matches the Types.roc structure expected by glue specs.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const types_mod = @import("types");
const layout_mod = @import("layout");

const ModuleEnv = can.ModuleEnv;
const TypeStore = types_mod.store.Store;
const Var = types_mod.Var;
const Content = types_mod.Content;
const FlatType = types_mod.FlatType;
const Ident = base.Ident;
const RocTarget = @import("roc_target").RocTarget;
const LayoutStore = layout_mod.Store;
const Layout = layout_mod.Layout;
const SizeAlign = layout_mod.SizeAlign;
const RocAlignment = layout_mod.RocAlignment;

const Allocator = std.mem.Allocator;

/// TypeId is an index into the types array
pub const TypeId = u64;

/// Numeric type variants
pub const RocNum = enum(u8) {
    I8 = 0,
    U8 = 1,
    I16 = 2,
    U16 = 3,
    I32 = 4,
    U32 = 5,
    I64 = 6,
    U64 = 7,
    I128 = 8,
    U128 = 9,
    F32 = 10,
    F64 = 11,
    Dec = 12,
};

/// Shape tag - must match Roc's alphabetical tag ordering
pub const ShapeTag = enum(u8) {
    Bool = 0,
    EmptyTagUnion = 1,
    Function = 2,
    Num = 3,
    RecursivePointer = 4,
    RocBox = 5,
    RocDict = 6,
    RocList = 7,
    RocResult = 8,
    RocSet = 9,
    RocStr = 10,
    Struct = 11,
    TagUnion = 12,
    TagUnionPayload = 13,
    Unit = 14,
    Unsized = 15,
};

/// Shape represents the structure of a type for glue generation
pub const Shape = union(ShapeTag) {
    Bool: void,
    EmptyTagUnion: void,
    Function: RocFn,
    Num: RocNum,
    RecursivePointer: TypeId,
    RocBox: TypeId,
    RocDict: struct { key: TypeId, value: TypeId },
    RocList: TypeId,
    RocResult: struct { ok: TypeId, err: TypeId },
    RocSet: TypeId,
    RocStr: void,
    Struct: StructInfo,
    TagUnion: RocTagUnion,
    TagUnionPayload: StructInfo,
    Unit: void,
    Unsized: void,
};

/// Information about a struct type
pub const StructInfo = struct {
    name: []const u8,
    fields: []const StructField,
    has_closure: bool,
};

/// A field in a struct
pub const StructField = struct {
    name: []const u8,
    type_id: TypeId,
    getter: ?[]const u8, // For closure fields
};

/// Tag union representation
pub const RocTagUnion = union(enum) {
    Enumeration: struct {
        name: []const u8,
        tags: []const []const u8,
        size: u32,
    },
    NonRecursive: TagUnionInfo,
    Recursive: TagUnionInfo,
    NullableWrapped: NullableWrappedInfo,
    NonNullableUnwrapped: struct {
        name: []const u8,
        tag_name: []const u8,
        payload: TypeId,
    },
    SingleTagStruct: struct {
        name: []const u8,
        tag_name: []const u8,
        payload_fields: []const SingleTagField,
        has_closure: bool,
    },
    NullableUnwrapped: struct {
        name: []const u8,
        null_tag: []const u8,
        non_null_tag: []const u8,
        non_null_payload: TypeId,
        first_tag_is_null: bool,
    },
};

/// Common tag union info
pub const TagUnionInfo = struct {
    name: []const u8,
    tags: []const TagInfo,
    discriminant_size: u32,
    discriminant_offset: u32,
};

/// Nullable wrapped tag union info
pub const NullableWrappedInfo = struct {
    name: []const u8,
    index_of_null_tag: u16,
    tags: []const TagInfo,
    discriminant_size: u32,
    discriminant_offset: u32,
};

/// Information about a single tag
pub const TagInfo = struct {
    name: []const u8,
    payload: ?TypeId,
};

/// Single tag field (for SingleTagStruct)
pub const SingleTagField = struct {
    name: ?[]const u8, // null for HasNoClosure variant
    type_id: TypeId,
};

/// Function type information
pub const RocFn = struct {
    function_name: []const u8,
    extern_name: []const u8,
    args: []const TypeId,
    lambda_set: TypeId,
    ret: TypeId,
    is_toplevel: bool,
};

/// Entry point information
pub const EntryPoint = struct {
    name: []const u8,
    type_id: TypeId,
};

/// Type dependency (for declaration ordering)
pub const TypeDependency = struct {
    type_id: TypeId,
    depends_on: []const TypeId,
};

/// Named type mapping
pub const NamedType = struct {
    name: []const u8,
    type_id: TypeId,
};

/// Extracted type information from compiled modules
pub const ExtractedTypes = struct {
    allocator: Allocator,
    types: []Shape,
    sizes: []u32,
    aligns: []u32,
    types_by_name: []NamedType,
    deps: []TypeDependency,
    entrypoints: []EntryPoint,
    target: RocTarget,

    pub fn deinit(self: *ExtractedTypes) void {
        self.allocator.free(self.types);
        self.allocator.free(self.sizes);
        self.allocator.free(self.aligns);
        for (self.types_by_name) |named| {
            self.allocator.free(named.name);
        }
        self.allocator.free(self.types_by_name);
        for (self.deps) |dep| {
            self.allocator.free(dep.depends_on);
        }
        self.allocator.free(self.deps);
        for (self.entrypoints) |ep| {
            self.allocator.free(ep.name);
        }
        self.allocator.free(self.entrypoints);
    }
};

/// Context for type extraction
const ExtractionContext = struct {
    allocator: Allocator,
    env: *ModuleEnv,
    layout_store: *LayoutStore,

    // Maps Var to TypeId for deduplication
    var_to_type_id: std.AutoHashMap(Var, TypeId),

    // Collected types
    types: std.ArrayList(Shape),
    sizes: std.ArrayList(u32),
    aligns: std.ArrayList(u32),
    names: std.ArrayList(NamedType),
    deps: std.ArrayList(TypeDependency),

    fn init(allocator: Allocator, env: *ModuleEnv, layout_store: *LayoutStore) ExtractionContext {
        return .{
            .allocator = allocator,
            .env = env,
            .layout_store = layout_store,
            .var_to_type_id = std.AutoHashMap(Var, TypeId).init(allocator),
            .types = std.ArrayList(Shape).init(allocator),
            .sizes = std.ArrayList(u32).init(allocator),
            .aligns = std.ArrayList(u32).init(allocator),
            .names = std.ArrayList(NamedType).init(allocator),
            .deps = std.ArrayList(TypeDependency).init(allocator),
        };
    }

    fn deinit(self: *ExtractionContext) void {
        self.var_to_type_id.deinit();
        self.types.deinit();
        self.sizes.deinit();
        self.aligns.deinit();
        self.names.deinit();
        self.deps.deinit();
    }

    /// Get or create a TypeId for a type variable
    fn getOrCreateTypeId(self: *ExtractionContext, var_: Var) !TypeId {
        if (self.var_to_type_id.get(var_)) |existing| {
            return existing;
        }

        // Reserve a slot for this type
        const type_id: TypeId = @intCast(self.types.items.len);
        try self.var_to_type_id.put(var_, type_id);

        // Extract the shape for this type
        const shape = try self.extractShape(var_);
        try self.types.append(shape);

        // Get size and alignment from layout store
        const size_align = self.getSizeAlign(var_);
        try self.sizes.append(size_align.size);
        try self.aligns.append(@intCast(size_align.alignment.toByteUnits()));

        return type_id;
    }

    fn getSizeAlign(self: *ExtractionContext, var_: Var) SizeAlign {
        // Try to get layout for this var
        if (self.layout_store.layouts_by_var.get(var_)) |layout_idx| {
            const layout = self.layout_store.getLayout(layout_idx);
            return self.layout_store.layoutSizeAlign(layout);
        }
        // Default to pointer size if layout not found
        const ptr_size = self.layout_store.targetUsize().size();
        return .{ .size = ptr_size, .alignment = RocAlignment.fromByteUnits(@intCast(ptr_size)) };
    }

    fn extractShape(self: *ExtractionContext, var_: Var) !Shape {
        const resolved = self.env.types.resolveVar(var_);
        const content = resolved.desc.content;

        return switch (content) {
            .flex => .{ .Unsized = {} },
            .rigid => .{ .Unsized = {} },
            .err => .{ .Unsized = {} },
            .alias => |alias| try self.extractAliasShape(alias),
            .structure => |flat_type| try self.extractFlatTypeShape(flat_type),
        };
    }

    fn extractAliasShape(self: *ExtractionContext, alias: types_mod.Alias) !Shape {
        // For aliases, we need to look at what they alias to
        // Get the aliased type variable from the vars range
        const vars = self.env.types.sliceVars(alias.vars.toRange());
        if (vars.len > 0) {
            const aliased_var = vars[0];
            return try self.extractShape(aliased_var);
        }
        return .{ .Unsized = {} };
    }

    fn extractFlatTypeShape(self: *ExtractionContext, flat_type: FlatType) !Shape {
        return switch (flat_type) {
            .empty_record => .{ .Unit = {} },
            .empty_tag_union => .{ .EmptyTagUnion = {} },
            .record => |record| try self.extractRecordShape(record),
            .record_unbound => .{ .Unsized = {} },
            .tuple => |tuple| try self.extractTupleShape(tuple),
            .nominal_type => |nominal| try self.extractNominalShape(nominal),
            .fn_pure, .fn_effectful, .fn_unbound => |func| try self.extractFunctionShape(func),
            .tag_union => |tag_union| try self.extractTagUnionShape(tag_union),
        };
    }

    fn extractRecordShape(self: *ExtractionContext, record: types_mod.Record) !Shape {
        const fields_slice = self.env.types.sliceRecordFields(record.fields);
        var fields = std.ArrayList(StructField).init(self.allocator);
        defer fields.deinit();

        for (fields_slice) |field| {
            const field_name = self.env.getIdent(field.name);
            const field_type_id = try self.getOrCreateTypeId(field.var_);
            try fields.append(.{
                .name = try self.allocator.dupe(u8, field_name),
                .type_id = field_type_id,
                .getter = null,
            });
        }

        return .{
            .Struct = .{
                .name = "", // Records don't have names
                .fields = try fields.toOwnedSlice(),
                .has_closure = false,
            },
        };
    }

    fn extractTupleShape(self: *ExtractionContext, tuple: types_mod.Tuple) !Shape {
        const elems = self.env.types.sliceVars(tuple.elems);
        var fields = std.ArrayList(StructField).init(self.allocator);
        defer fields.deinit();

        for (elems, 0..) |elem_var, i| {
            const field_type_id = try self.getOrCreateTypeId(elem_var);
            var name_buf: [16]u8 = undefined;
            const name = std.fmt.bufPrint(&name_buf, "{d}", .{i}) catch "?";
            try fields.append(.{
                .name = try self.allocator.dupe(u8, name),
                .type_id = field_type_id,
                .getter = null,
            });
        }

        return .{
            .Struct = .{
                .name = "",
                .fields = try fields.toOwnedSlice(),
                .has_closure = false,
            },
        };
    }

    fn extractNominalShape(self: *ExtractionContext, nominal: types_mod.NominalType) !Shape {
        const type_name = self.env.getIdent(nominal.symbol.ident_idx);

        // Check for builtin types
        if (std.mem.eql(u8, type_name, "Str") or std.mem.eql(u8, type_name, "Builtin.Str")) {
            return .{ .RocStr = {} };
        }
        if (std.mem.eql(u8, type_name, "Bool") or std.mem.eql(u8, type_name, "Builtin.Bool")) {
            return .{ .Bool = {} };
        }

        // Check for numeric types
        if (getNumType(type_name)) |num| {
            return .{ .Num = num };
        }

        // Check for List
        if (std.mem.startsWith(u8, type_name, "List")) {
            const args = self.env.types.sliceVars(nominal.args.toRange());
            if (args.len > 0) {
                const elem_type_id = try self.getOrCreateTypeId(args[0]);
                return .{ .RocList = elem_type_id };
            }
        }

        // Check for Box
        if (std.mem.startsWith(u8, type_name, "Box")) {
            const args = self.env.types.sliceVars(nominal.args.toRange());
            if (args.len > 0) {
                const elem_type_id = try self.getOrCreateTypeId(args[0]);
                return .{ .RocBox = elem_type_id };
            }
        }

        // Check for Result
        if (std.mem.startsWith(u8, type_name, "Result")) {
            const args = self.env.types.sliceVars(nominal.args.toRange());
            if (args.len >= 2) {
                const ok_type_id = try self.getOrCreateTypeId(args[0]);
                const err_type_id = try self.getOrCreateTypeId(args[1]);
                return .{ .RocResult = .{ .ok = ok_type_id, .err = err_type_id } };
            }
        }

        // Default: treat as a struct with the type name
        return .{
            .Struct = .{
                .name = try self.allocator.dupe(u8, type_name),
                .fields = &[_]StructField{},
                .has_closure = false,
            },
        };
    }

    fn getNumType(type_name: []const u8) ?RocNum {
        const mappings = [_]struct { name: []const u8, num: RocNum }{
            .{ .name = "I8", .num = .I8 },
            .{ .name = "U8", .num = .U8 },
            .{ .name = "I16", .num = .I16 },
            .{ .name = "U16", .num = .U16 },
            .{ .name = "I32", .num = .I32 },
            .{ .name = "U32", .num = .U32 },
            .{ .name = "I64", .num = .I64 },
            .{ .name = "U64", .num = .U64 },
            .{ .name = "I128", .num = .I128 },
            .{ .name = "U128", .num = .U128 },
            .{ .name = "F32", .num = .F32 },
            .{ .name = "F64", .num = .F64 },
            .{ .name = "Dec", .num = .Dec },
        };

        for (mappings) |m| {
            if (std.mem.eql(u8, type_name, m.name) or
                std.mem.endsWith(u8, type_name, m.name))
            {
                return m.num;
            }
        }
        return null;
    }

    fn extractFunctionShape(self: *ExtractionContext, func: types_mod.Func) !Shape {
        var args = std.ArrayList(TypeId).init(self.allocator);
        defer args.deinit();

        const arg_vars = self.env.types.sliceVars(func.args);
        for (arg_vars) |arg_var| {
            const arg_type_id = try self.getOrCreateTypeId(arg_var);
            try args.append(arg_type_id);
        }

        const ret_type_id = try self.getOrCreateTypeId(func.ret);

        return .{
            .Function = .{
                .function_name = "",
                .extern_name = "",
                .args = try args.toOwnedSlice(),
                .lambda_set = 0, // TODO: extract lambda set
                .ret = ret_type_id,
                .is_toplevel = false,
            },
        };
    }

    fn extractTagUnionShape(self: *ExtractionContext, tag_union: types_mod.TagUnion) !Shape {
        const tags = self.env.types.sliceTags(tag_union.tags);

        // Simple enumeration check: all tags have no payload
        var is_enumeration = true;
        for (tags) |tag| {
            if (tag.args.len() > 0) {
                is_enumeration = false;
                break;
            }
        }

        if (is_enumeration) {
            var tag_names = std.ArrayList([]const u8).init(self.allocator);
            defer tag_names.deinit();

            for (tags) |tag| {
                const tag_name = self.env.getIdent(tag.name);
                try tag_names.append(try self.allocator.dupe(u8, tag_name));
            }

            return .{
                .TagUnion = .{
                    .Enumeration = .{
                        .name = "",
                        .tags = try tag_names.toOwnedSlice(),
                        .size = @intCast(tags.len),
                    },
                },
            };
        }

        // Non-enumeration tag union
        var tag_infos = std.ArrayList(TagInfo).init(self.allocator);
        defer tag_infos.deinit();

        for (tags) |tag| {
            const tag_name = self.env.getIdent(tag.name);
            var payload: ?TypeId = null;

            if (tag.args.len() > 0) {
                const tag_args = self.env.types.sliceVars(tag.args);
                if (tag_args.len > 0) {
                    payload = try self.getOrCreateTypeId(tag_args[0]);
                }
            }

            try tag_infos.append(.{
                .name = try self.allocator.dupe(u8, tag_name),
                .payload = payload,
            });
        }

        return .{
            .TagUnion = .{
                .NonRecursive = .{
                    .name = "",
                    .tags = try tag_infos.toOwnedSlice(),
                    .discriminant_size = 1, // Default, would need layout info for accurate value
                    .discriminant_offset = 0,
                },
            },
        };
    }
};

/// Extract types from compiled module environments
pub fn extractTypes(
    allocator: Allocator,
    module_env: *ModuleEnv,
    layout_store: *LayoutStore,
    entry_point_names: []const []const u8,
    roc_target: RocTarget,
) !ExtractedTypes {
    var ctx = ExtractionContext.init(allocator, module_env, layout_store);
    defer ctx.deinit();

    // Build entry points
    var entrypoints = std.ArrayList(EntryPoint).init(allocator);

    for (entry_point_names) |name| {
        // Look up the entry point in the module's exports
        // For now, create placeholder entries
        try entrypoints.append(.{
            .name = try allocator.dupe(u8, name),
            .type_id = 0, // Placeholder - would need to look up actual type
        });
    }

    return ExtractedTypes{
        .allocator = allocator,
        .types = try ctx.types.toOwnedSlice(),
        .sizes = try ctx.sizes.toOwnedSlice(),
        .aligns = try ctx.aligns.toOwnedSlice(),
        .types_by_name = try ctx.names.toOwnedSlice(),
        .deps = try ctx.deps.toOwnedSlice(),
        .entrypoints = try entrypoints.toOwnedSlice(),
        .target = roc_target,
    };
}
