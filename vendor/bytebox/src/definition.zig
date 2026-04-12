// This file contains types and code shared between both the ModuleDefinition and VMs
const builtin = @import("builtin");
const std = @import("std");
const AllocError = std.mem.Allocator.Error;

const common = @import("common.zig");
const Logger = common.Logger;
const StableArray = common.StableArray;

const opcodes = @import("opcode.zig");
const Opcode = opcodes.Opcode;
const WasmOpcode = opcodes.WasmOpcode;

pub const MalformedError = error{
    MalformedBytecode,
    MalformedCustomSection,
    MalformedDataCountMismatch,
    MalformedDataType,
    MalformedElementType,
    MalformedFunctionCodeSectionMismatch,
    MalformedIllegalOpcode,
    MalformedInvalidImport,
    MalformedLEB128,
    MalformedLimits,
    MalformedMagicSignature,
    MalformedMissingDataCountSection,
    MalformedMissingZeroByte,
    MalformedMultipleStartSections,
    MalformedMutability,
    MalformedReferenceType,
    MalformedSectionId,
    MalformedSectionSizeMismatch,
    MalformedTableType,
    MalformedTooManyLocals,
    MalformedTypeSentinel,
    MalformedUnexpectedEnd,
    MalformedUnsupportedWasmVersion,
    MalformedUTF8Encoding,
    MalformedValType,
};

pub const ValidationError = error{
    ValidationBadAlignment,
    ValidationBadConstantExpression,
    ValidationConstantExpressionGlobalMustBeImmutable,
    ValidationConstantExpressionGlobalMustBeImport,
    ValidationConstantExpressionTypeMismatch,
    ValidationDuplicateExportName,
    ValidationFuncRefUndeclared,
    ValidationGlobalReferencingMutableGlobal,
    ValidationIfElseMismatch,
    ValidationImmutableGlobal,
    ValidationInvalidLaneIndex,
    ValidationLimitsMinMustNotBeLargerThanMax,
    ValidationMemoryInvalidMaxLimit,
    ValidationMemoryMaxPagesExceeded,
    ValidationMultipleMemories,
    ValidationOutOfBounds,
    ValidationSelectArity,
    ValidationStartFunctionType,
    ValidationTooManyFunctionImportParams,
    ValidationTooManyFunctionImportReturns,
    ValidationTypeMismatch,
    ValidationTypeMustBeNumeric,
    ValidationTypeStackHeightMismatch,
    ValidationUnknownBlockTypeIndex,
    ValidationUnknownData,
    ValidationUnknownElement,
    ValidationUnknownFunction,
    ValidationUnknownGlobal,
    ValidationUnknownLabel,
    ValidationUnknownLocal,
    ValidationUnknownMemory,
    ValidationUnknownTable,
    ValidationUnknownType,
};

const DecodeError = AllocError || MalformedError || ValidationError;

pub const i8x16 = @Vector(16, i8);
pub const u8x16 = @Vector(16, u8);
pub const i16x8 = @Vector(8, i16);
pub const u16x8 = @Vector(8, u16);
pub const i32x4 = @Vector(4, i32);
pub const u32x4 = @Vector(4, u32);
pub const i64x2 = @Vector(2, i64);
pub const u64x2 = @Vector(2, u64);
pub const f32x4 = @Vector(4, f32);
pub const f64x2 = @Vector(2, f64);
pub const v128 = f32x4;

const Section = enum(u8) { Custom, FunctionType, Import, Function, Table, Memory, Global, Export, Start, Element, Code, Data, DataCount };

const k_function_type_sentinel_byte: u8 = 0x60;
const k_block_type_void_sentinel_byte: u8 = 0x40;

fn eosError(e: anyerror) MalformedError {
    if (e == error.EndOfStream) {
        return MalformedError.MalformedUnexpectedEnd;
    } else if (e == error.EndOfBuffer) {
        return MalformedError.MalformedUnexpectedEnd;
    } else {
        unreachable;
    }
}

fn readByte(reader: anytype) MalformedError!u8 {
    return reader.readByte() catch |e| return eosError(e);
}

fn readBytes(reader: anytype, bytes: []u8) MalformedError!usize {
    return reader.read(bytes) catch |e| return eosError(e);
}

fn decodeLEB128(comptime T: type, reader: anytype) MalformedError!T {
    if (@typeInfo(T).int.signedness == .signed) {
        return std.leb.readIleb128(T, reader) catch |e| {
            if (e == error.Overflow) {
                return error.MalformedLEB128;
            } else {
                return eosError(e);
            }
        };
    } else {
        return std.leb.readUleb128(T, reader) catch |e| {
            if (e == error.Overflow) {
                return error.MalformedLEB128;
            } else {
                return eosError(e);
            }
        };
    }
}

fn decodeWasmOpcode(reader: anytype) MalformedError!WasmOpcode {
    const byte = try readByte(&reader);
    var wasm_op: WasmOpcode = undefined;
    if (byte == 0xFC or byte == 0xFD) {
        const type_opcode = try decodeLEB128(u32, reader);
        if (type_opcode > std.math.maxInt(u8)) {
            return error.MalformedIllegalOpcode;
        }
        const byte2 = @as(u8, @intCast(type_opcode));
        var extended: u16 = byte;
        extended = extended << 8;
        extended |= byte2;

        wasm_op = std.meta.intToEnum(WasmOpcode, extended) catch {
            return error.MalformedIllegalOpcode;
        };
    } else {
        wasm_op = std.meta.intToEnum(WasmOpcode, byte) catch {
            return error.MalformedIllegalOpcode;
        };
    }
    return wasm_op;
}

fn decodeFloat(comptime T: type, reader: anytype) MalformedError!T {
    return switch (T) {
        f32 => @as(f32, @bitCast(reader.readInt(u32, .little) catch |e| return eosError(e))),
        f64 => @as(f64, @bitCast(reader.readInt(u64, .little) catch |e| return eosError(e))),
        else => unreachable,
    };
}

fn decodeVec(reader: anytype) MalformedError!v128 {
    var bytes: [16]u8 = undefined;
    _ = reader.read(&bytes) catch |e| eosError(e);
    return std.mem.bytesToValue(v128, &bytes);
}

pub const ValType = enum(c_int) {
    I32,
    I64,
    F32,
    F64,
    V128,
    FuncRef,
    ExternRef,

    fn bytecodeToValtype(byte: u8) MalformedError!ValType {
        return switch (byte) {
            0x7B => .V128,
            0x7F => .I32,
            0x7E => .I64,
            0x7D => .F32,
            0x7C => .F64,
            0x70 => .FuncRef,
            0x6F => .ExternRef,
            else => {
                return error.MalformedValType;
            },
        };
    }

    fn decode(reader: anytype) MalformedError!ValType {
        return try bytecodeToValtype(try readByte(&reader));
    }

    fn decodeReftype(reader: anytype) MalformedError!ValType {
        const valtype = try decode(reader);
        if (isRefType(valtype) == false) {
            return error.MalformedReferenceType;
        }
        return valtype;
    }

    pub fn isRefType(valtype: ValType) bool {
        return switch (valtype) {
            .FuncRef => true,
            .ExternRef => true,
            else => false,
        };
    }

    pub fn count() comptime_int {
        return @typeInfo(ValType).Enum.fields.len;
    }
};

// FuncRefs that are in the "index" state without a valid pointer are unresolved. They will
// all be resolved by instantiation time in the VM, which has the actual function instance
// data to which func points.
pub const FuncRef = extern union {
    func: *const anyopaque,
    index: usize,

    const k_null_sentinel: usize = std.math.maxInt(usize);

    pub fn nullRef() FuncRef {
        return .{ .index = k_null_sentinel };
    }

    pub fn isNull(funcref: FuncRef) bool {
        return funcref.index == k_null_sentinel;
    }

    comptime {
        std.debug.assert(@sizeOf(?*const anyopaque) == @sizeOf(usize));
        std.debug.assert(@sizeOf(FuncRef) == @sizeOf(usize));
        std.debug.assert(@sizeOf(FuncRef) == @sizeOf(ExternRef));
    }
};

pub const ExternRef = usize;

pub const Val = extern union {
    I32: i32,
    I64: i64,
    F32: f32,
    F64: f64,
    V128: v128,
    FuncRef: FuncRef,
    ExternRef: ExternRef,

    pub fn default(valtype: ValType) Val {
        return switch (valtype) {
            .I32 => Val{ .I32 = 0 },
            .I64 => Val{ .I64 = 0 },
            .F32 => Val{ .F32 = 0.0 },
            .F64 => Val{ .F64 = 0.0 },
            .V128 => Val{ .V128 = f32x4{ 0, 0, 0, 0 } },
            .FuncRef => nullRef(.FuncRef) catch unreachable,
            .ExternRef => nullRef(.ExternRef) catch unreachable,
        };
    }

    pub fn funcrefFromIndex(index: usize) Val {
        return Val{ .FuncRef = .{ .index = index } };
    }

    pub fn nullRef(valtype: ValType) ?Val {
        return switch (valtype) {
            .FuncRef => Val{ .FuncRef = FuncRef.nullRef() },
            .ExternRef => Val{ .ExternRef = FuncRef.k_null_sentinel },
            else => null,
        };
    }

    pub fn isNull(v: Val) bool {
        // Because FuncRef.index is located at the same memory location as ExternRef, this passes for both types
        return v.FuncRef.index == FuncRef.k_null_sentinel;
    }

    pub fn eql(valtype: ValType, v1: Val, v2: Val) bool {
        return switch (valtype) {
            .I32 => v1.I32 == v2.I32,
            .I64 => v1.I64 == v2.I64,
            .F32 => v1.F32 == v2.F32,
            .F64 => v1.F64 == v2.F64,
            .V128 => @reduce(.And, v1.V128 == v2.V128),
            .FuncRef => v1.FuncRef.index == v2.FuncRef.index,
            .ExternRef => v1.ExternRef == v2.ExternRef,
        };
    }
};

test "Val.isNull" {
    const v1: Val = Val.nullRef(.FuncRef).?;
    const v2: Val = Val.nullRef(.ExternRef).?;

    try std.testing.expect(v1.isNull() == true);
    try std.testing.expect(v2.isNull() == true);

    const v3 = Val.funcrefFromIndex(12);
    const v4 = Val{ .ExternRef = 234 };

    try std.testing.expect(v3.isNull() == false);
    try std.testing.expect(v4.isNull() == false);
}

pub const TaggedVal = struct {
    val: Val,
    type: ValType,

    pub fn nullRef(valtype: ValType) ?TaggedVal {
        if (Val.nullRef(valtype)) |val| {
            return TaggedVal{
                .val = val,
                .type = valtype,
            };
        }
        return null;
    }

    pub fn funcrefFromIndex(index: u32) TaggedVal {
        return TaggedVal{
            .val = Val.funcrefFromIndex(index),
            .type = .FuncRef,
        };
    }

    pub const HashMapContext = struct {
        pub fn hash(self: @This(), v: TaggedVal) u64 {
            _ = self;

            var hasher = std.hash.Wyhash.init(0);
            hasher.update(std.mem.asBytes(&v.val));
            hasher.update(std.mem.asBytes(&v.type));
            return hasher.final();
        }

        pub fn eql(self: @This(), a: TaggedVal, b: TaggedVal) bool {
            _ = self;

            return a.type == b.type and Val.eql(a.type, a.val, b.val);
        }
    };
};

pub const Limits = struct {
    // Note that 32bit architectures should be able to decode and validate wasm modules that
    // were compiled with 64-bit limits. However, they will be unable to instantiate them.
    min: u64,
    max: ?u64,
    limit_type: u8,

    // limit_type table:
    // 0x00 n:u32        ⇒ i32, {min n, max ?}, 0
    // 0x01 n:u32 m:u32  ⇒ i32, {min n, max m}, 0
    // 0x02 n:u32        ⇒ i32, {min n, max ?}, 1  ;; from threads proposal
    // 0x03 n:u32 m:u32  ⇒ i32, {min n, max m}, 1  ;; from threads proposal
    // 0x04 n:u64        ⇒ i64, {min n, max ?}, 0
    // 0x05 n:u64 m:u64  ⇒ i64, {min n, max m}, 0
    // 0x06 n:u64        ⇒ i64, {min n, max ?}, 1  ;; from threads proposal
    // 0x07 n:u64 m:u64  ⇒ i64, {min n, max m}, 1  ;; from threads proposal

    pub const k_max_bytes_i32 = (1024 * 1024 * 1024 * 4);
    pub const k_max_pages_i32 = k_max_bytes_i32 / MemoryDefinition.k_page_size;

    // Technically the max bytes should be maxInt(u64), but that is wayyy more memory than PCs have available and
    // is just a waste of virtual address space in the implementation. Instead we'll set the upper limit to 128GB.
    pub const k_max_bytes_i64 = (1024 * 1024 * 1024 * 128);
    pub const k_max_pages_i64 = k_max_bytes_i64 / MemoryDefinition.k_page_size;

    fn decode(reader: anytype) !Limits {
        const limit_type: u8 = try readByte(&reader);

        if (limit_type > 7) {
            return error.MalformedLimits;
        }

        const is_u32 = limit_type < 4;

        const min = decodeLEB128(u64, reader) catch return error.MalformedLimits;
        if (is_u32 and min > std.math.maxInt(u32)) {
            return error.MalformedLimits;
        }

        var max: ?u64 = null;

        switch (std.math.rem(u8, limit_type, 2) catch unreachable) {
            0 => {},
            1 => {
                max = decodeLEB128(u64, reader) catch return error.MalformedLimits;
                if (is_u32 and max.? > std.math.maxInt(u32)) {
                    return error.MalformedLimits;
                }
                if (max.? < min) {
                    return error.ValidationLimitsMinMustNotBeLargerThanMax;
                }
            },
            else => unreachable,
        }

        return Limits{
            .min = min,
            .max = max,
            .limit_type = limit_type,
        };
    }

    pub fn isIndex32(self: Limits) bool {
        return self.limit_type < 4;
    }

    pub fn indexType(self: Limits) ValType {
        return if (self.limit_type < 4) .I32 else .I64;
    }

    pub fn maxPages(self: Limits) u64 {
        if (self.max) |max| {
            return @max(1, max);
        }

        return self.indexTypeMaxPages();
    }

    pub fn indexTypeMaxPages(self: Limits) u64 {
        return if (self.limit_type < 4) k_max_pages_i32 else k_max_pages_i64;
    }
};

const BlockType = enum(u8) {
    Void,
    ValType,
    TypeIndex,
};

pub const BlockTypeValue = extern union {
    ValType: ValType,
    TypeIndex: u32,

    fn getBlocktypeParamTypes(value: BlockTypeValue, block_type: BlockType, module_def: *const ModuleDefinition) []const ValType {
        switch (block_type) {
            else => return &.{},
            .TypeIndex => return module_def.types.items[value.TypeIndex].getParams(),
        }
    }

    pub fn getBlocktypeReturnTypes(value: BlockTypeValue, block_type: BlockType, module_def: *const ModuleDefinition) []const ValType {
        switch (block_type) {
            .Void => return &.{},
            .ValType => return switch (value.ValType) {
                .I32 => &.{.I32},
                .I64 => &.{.I64},
                .F32 => &.{.F32},
                .F64 => &.{.F64},
                .V128 => &.{.V128},
                .FuncRef => &.{.FuncRef},
                .ExternRef => &.{.ExternRef},
            },
            .TypeIndex => return module_def.types.items[value.TypeIndex].getReturns(),
        }
    }
};

const ConstantExpressionType = enum {
    Value,
    Global,
};

pub const ConstantExpression = union(ConstantExpressionType) {
    Value: TaggedVal,
    Global: u32, // global index

    const ExpectedGlobalMut = enum {
        Any,
        Immutable,
    };

    fn decode(reader: anytype, module_def: *const ModuleDefinition, comptime expected_global_mut: ExpectedGlobalMut, expected_valtype: ValType) !ConstantExpression {
        const opcode = try decodeWasmOpcode(reader);

        const expr = switch (opcode) {
            .I32_Const => ConstantExpression{ .Value = TaggedVal{ .type = .I32, .val = .{ .I32 = try decodeLEB128(i32, reader) } } },
            .I64_Const => ConstantExpression{ .Value = TaggedVal{ .type = .I64, .val = .{ .I64 = try decodeLEB128(i64, reader) } } },
            .F32_Const => ConstantExpression{ .Value = TaggedVal{ .type = .F32, .val = .{ .F32 = try decodeFloat(f32, reader) } } },
            .F64_Const => ConstantExpression{ .Value = TaggedVal{ .type = .F64, .val = .{ .F64 = try decodeFloat(f64, reader) } } },
            .V128_Const => ConstantExpression{ .Value = TaggedVal{ .type = .V128, .val = .{ .V128 = try decodeVec(reader) } } },
            .Ref_Null => ConstantExpression{ .Value = TaggedVal.nullRef(try ValType.decode(reader)) orelse return error.MalformedBytecode },
            .Ref_Func => ConstantExpression{ .Value = TaggedVal.funcrefFromIndex(try decodeLEB128(u32, reader)) },
            .Global_Get => ConstantExpression{ .Global = try decodeLEB128(u32, reader) },
            else => return error.ValidationBadConstantExpression,
        };

        if (opcode == .Global_Get) {
            try ModuleValidator.validateGlobalIndex(expr.Global, module_def);

            if (module_def.imports.globals.items.len <= expr.Global) {
                return error.ValidationConstantExpressionGlobalMustBeImport;
            }

            if (expected_global_mut == .Immutable) {
                if (expr.Global < module_def.imports.globals.items.len) {
                    if (module_def.imports.globals.items[expr.Global].mut != .Immutable) {
                        return error.ValidationConstantExpressionGlobalMustBeImmutable;
                    }
                } else {
                    const local_index: usize = module_def.imports.globals.items.len - expr.Global;
                    if (module_def.globals.items[local_index].mut != .Immutable) {
                        return error.ValidationConstantExpressionGlobalMustBeImmutable;
                    }
                }
            }

            var global_valtype: ValType = undefined;
            if (expr.Global < module_def.imports.globals.items.len) {
                const global_import_def: *const GlobalImportDefinition = &module_def.imports.globals.items[expr.Global];
                global_valtype = global_import_def.valtype;
            } else {
                const local_index: usize = module_def.imports.globals.items.len - expr.Global;
                const global_def: *const GlobalDefinition = &module_def.globals.items[local_index];
                global_valtype = global_def.valtype;
            }

            if (global_valtype != expected_valtype) {
                return error.ValidationConstantExpressionTypeMismatch;
            }
        } else {
            if (expr.Value.type != expected_valtype) {
                return error.ValidationConstantExpressionTypeMismatch;
            }
        }

        const end = @as(WasmOpcode, @enumFromInt(try readByte(&reader)));
        if (end != .End) {
            return error.ValidationBadConstantExpression;
        }

        return expr;
    }
};

pub const FunctionTypeDefinition = struct {
    types: std.array_list.Managed(ValType), // TODO replace this with offsets into a single array in the ModuleDefinition
    num_params: u32,

    pub fn getParams(self: *const FunctionTypeDefinition) []const ValType {
        return self.types.items[0..self.num_params];
    }

    pub fn getReturns(self: *const FunctionTypeDefinition) []const ValType {
        return self.types.items[self.num_params..];
    }

    pub const SortContext = struct {
        const Self = @This();

        pub fn hash(_: Self, f: *FunctionTypeDefinition) u64 {
            var seed: u64 = 0;
            if (f.types.items.len > 0) {
                seed = std.hash.Murmur2_64.hash(std.mem.sliceAsBytes(f.types.items));
            }
            return std.hash.Murmur2_64.hashWithSeed(std.mem.asBytes(&f.num_params), seed);
        }

        pub fn eql(_: Self, a: *const FunctionTypeDefinition, b: *const FunctionTypeDefinition) bool {
            if (a.num_params != b.num_params or a.types.items.len != b.types.items.len) {
                return false;
            }

            for (a.types.items, 0..) |typeA, i| {
                const typeB = b.types.items[i];
                if (typeA != typeB) {
                    return false;
                }
            }

            return true;
        }

        fn less(context: Self, a: *FunctionTypeDefinition, b: *FunctionTypeDefinition) bool {
            const ord = Self.order(context, a, b);
            return ord == std.math.Order.lt;
        }

        fn order(context: Self, a: *FunctionTypeDefinition, b: *FunctionTypeDefinition) std.math.Order {
            const hashA = Self.hash(context, a);
            const hashB = Self.hash(context, b);

            if (hashA < hashB) {
                return std.math.Order.lt;
            } else if (hashA > hashB) {
                return std.math.Order.gt;
            } else {
                return std.math.Order.eq;
            }
        }
    };
};

pub const FunctionDefinition = struct {
    type_index: usize,
    instructions_begin: usize,
    instructions_end: usize,
    continuation: usize,
    locals_begin: usize,
    locals_end: usize,
    stack_stats: FunctionStackStats = .{},

    pub fn locals(func: *const FunctionDefinition, module_def: *const ModuleDefinition) []const ValType {
        return module_def.code.locals.items[func.locals_begin..func.locals_end];
    }

    pub fn instructions(func: *const FunctionDefinition, module_def: *const ModuleDefinition) []Instruction {
        return module_def.code.instructions.items[func.instructions_begin..func.instructions_end];
    }

    pub fn numParamsAndLocals(func: *const FunctionDefinition, module_def: *const ModuleDefinition) usize {
        const func_type: *const FunctionTypeDefinition = func.typeDefinition(module_def);
        const param_types: []const ValType = func_type.getParams();
        return param_types.len + func.locals.items.len;
    }

    pub fn typeDefinition(func: *const FunctionDefinition, module_def: *const ModuleDefinition) *const FunctionTypeDefinition {
        return &module_def.types.items[func.type_index];
    }
};

const ExportType = enum(u8) {
    Function = 0x00,
    Table = 0x01,
    Memory = 0x02,
    Global = 0x03,
};

pub const ExportDefinition = struct {
    name: []const u8,
    index: u32,
};

pub const FunctionExport = struct {
    params: []const ValType,
    returns: []const ValType,
};

pub const FunctionHandle = extern struct {
    index: u32,
};

pub const GlobalMut = enum(u8) {
    Immutable = 0,
    Mutable = 1,

    fn decode(reader: anytype) !GlobalMut {
        const byte = try readByte(&reader);
        const value = std.meta.intToEnum(GlobalMut, byte) catch {
            return error.MalformedMutability;
        };
        return value;
    }
};

pub const GlobalDefinition = struct {
    valtype: ValType,
    mut: GlobalMut,
    expr: ConstantExpression,
};

pub const GlobalExport = struct {
    val: *Val,
    valtype: ValType,
    mut: GlobalMut,
};

pub const TableDefinition = struct {
    reftype: ValType,
    limits: Limits,
};

pub const MemoryDefinition = struct {
    limits: Limits,

    pub const k_page_size: u64 = 64 * 1024;
};

pub const ElementMode = enum {
    Active,
    Passive,
    Declarative,
};

pub const ElementDefinition = struct {
    table_index: u32,
    mode: ElementMode,
    reftype: ValType,
    offset: ?ConstantExpression,
    elems_value: std.array_list.Managed(Val),
    elems_expr: std.array_list.Managed(ConstantExpression),
};

pub const DataMode = enum {
    Active,
    Passive,
};

pub const DataDefinition = struct {
    bytes: std.array_list.Managed(u8),
    memory_index: ?u32,
    offset: ?ConstantExpression,
    mode: DataMode,

    fn decode(reader: anytype, module_def: *const ModuleDefinition, allocator: std.mem.Allocator) DecodeError!DataDefinition {
        const data_type: u32 = try decodeLEB128(u32, reader);
        if (data_type > 2) {
            return error.MalformedDataType;
        }

        var memory_index: ?u32 = null;
        var index_type: ValType = .I32;
        if (data_type == 0x00) {
            memory_index = 0;
        } else if (data_type == 0x02) {
            memory_index = try decodeLEB128(u32, reader);
        }

        if (memory_index) |index| {
            if (module_def.imports.memories.items.len + module_def.memories.items.len <= index) {
                return error.ValidationUnknownMemory;
            }
            const limits = module_def.getMemoryLimits();
            index_type = limits.indexType();
        }

        var mode = DataMode.Passive;
        var offset: ?ConstantExpression = null;
        if (data_type == 0x00 or data_type == 0x02) {
            mode = DataMode.Active;
            offset = try ConstantExpression.decode(reader, module_def, .Immutable, index_type);
        }

        const num_bytes = try decodeLEB128(u32, reader);
        var bytes = std.array_list.Managed(u8).init(allocator);
        try bytes.resize(num_bytes);
        const num_read = try readBytes(reader, bytes.items);
        if (num_read != num_bytes) {
            return error.MalformedUnexpectedEnd;
        }

        return DataDefinition{
            .bytes = bytes,
            .memory_index = memory_index,
            .offset = offset,
            .mode = mode,
        };
    }
};

pub const MAX_FUNCTION_IMPORT_PARAMS = 256;
pub const MAX_FUNCTION_IMPORT_RETURNS = 256;

pub const ImportNames = struct {
    module_name: []const u8,
    import_name: []const u8,
};

const FunctionImportDefinition = struct {
    names: ImportNames,
    type_index: u32,
};

const TableImportDefinition = struct {
    names: ImportNames,
    reftype: ValType,
    limits: Limits,
};

const MemoryImportDefinition = struct {
    names: ImportNames,
    limits: Limits,
};

const GlobalImportDefinition = struct {
    names: ImportNames,
    valtype: ValType,
    mut: GlobalMut,
};

const MemArg = struct {
    alignment: u32,
    offset: u64,

    fn decode(reader: anytype, comptime bitwidth: u32) !MemArg {
        std.debug.assert(bitwidth % 8 == 0);
        const memarg = MemArg{
            .alignment = try decodeLEB128(u32, reader),
            .offset = try decodeLEB128(u64, reader),
        };
        const bit_alignment = std.math.powi(u32, 2, memarg.alignment) catch return error.ValidationBadAlignment;
        if (bit_alignment > bitwidth / 8) {
            return error.ValidationBadAlignment;
        }
        return memarg;
    }
};

pub const MemoryOffsetAndLaneImmediates = struct {
    offset: u64,
    laneidx: u8,
};

pub const BranchTableImmediates = struct {
    label_ids_begin: u32,
    label_ids_end: u32,
    fallback_id: u32,

    pub fn getLabelIds(self: BranchTableImmediates, module: ModuleDefinition) []const u32 {
        return module.code.branch_table_ids_immediates.items[self.label_ids_begin..self.label_ids_end];
    }
};

pub const CallIndirectImmediates = struct {
    type_index: u32,
    table_index: u32,
};

pub const TablePairImmediates = struct {
    index_x: u32,
    index_y: u32,
};

pub const BlockImmediates = struct {
    continuation: u32,
    num_returns: u16,
};

pub const IfImmediates = struct {
    num_returns: u16,
    else_continuation_relative: u32,
    end_continuation_relative: u32,
};

pub const InstructionImmediates = union {
    Void: void,
    ValType: ValType,
    ValueI32: i32,
    ValueF32: f32,
    ValueI64: i64,
    ValueF64: f64,
    Index: u32,
    LabelId: u32,
    MemoryOffset: u64,
    Block: BlockImmediates,
    CallIndirect: CallIndirectImmediates,
    TablePair: TablePairImmediates,
    If: IfImmediates,

    comptime {
        if (builtin.mode == .ReleaseFast) {
            std.debug.assert(@sizeOf(BlockImmediates) == 8);
            std.debug.assert(@sizeOf(CallIndirectImmediates) == 8);
            std.debug.assert(@sizeOf(TablePairImmediates) == 8);
            std.debug.assert(@sizeOf(IfImmediates) == 12);
            std.debug.assert(@sizeOf(InstructionImmediates) == 8);
        }
    }
};

pub const ValidationImmediates = union {
    Void: void,
    BlockOrIf: struct {
        block_type: BlockType,
        block_value: BlockTypeValue,
    },
};

pub const DecodedInstruction = struct {
    instruction: Instruction,
    validation_immediates: ValidationImmediates,
};

pub const Instruction = struct {
    opcode: Opcode,
    immediate: InstructionImmediates,

    comptime {
        if (builtin.mode == .ReleaseFast) {
            std.debug.assert(@sizeOf(Instruction) == 16);
        }
    }

    fn decode(reader: anytype, module: *ModuleDefinition, func: *FunctionDefinition) !DecodedInstruction {
        const Helpers = struct {
            fn decodeBlockType(
                _reader: anytype,
                _module: *ModuleDefinition,
                out_immediates: *InstructionImmediates,
                out_validation_immediates: *ValidationImmediates,
            ) !void {
                var block_type: BlockType = undefined;
                var block_value: BlockTypeValue = undefined;

                const blocktype_raw = try readByte(&_reader);
                const valtype_or_err = ValType.bytecodeToValtype(blocktype_raw);
                if (std.meta.isError(valtype_or_err)) {
                    if (blocktype_raw == k_block_type_void_sentinel_byte) {
                        block_type = .Void;
                        block_value = BlockTypeValue{ .TypeIndex = 0 };
                    } else {
                        _reader.context.pos -= 1; // move the stream backwards 1 byte to reconstruct the integer
                        const index_33bit = try decodeLEB128(i33, _reader);
                        if (index_33bit < 0) {
                            return error.MalformedBytecode;
                        }
                        const index: u32 = @as(u32, @intCast(index_33bit));
                        if (index < _module.types.items.len) {
                            block_type = .TypeIndex;
                            block_value = BlockTypeValue{ .TypeIndex = index };
                        } else {
                            return error.ValidationUnknownBlockTypeIndex;
                        }
                    }
                } else {
                    const valtype: ValType = valtype_or_err catch unreachable;
                    block_type = .ValType;
                    block_value = BlockTypeValue{ .ValType = valtype };
                }

                const num_returns: u16 = @intCast(block_value.getBlocktypeReturnTypes(block_type, _module).len);

                out_immediates.* = InstructionImmediates{
                    .Block = BlockImmediates{
                        .num_returns = num_returns,
                        .continuation = std.math.maxInt(u32), // will be set later in the code section decode
                    },
                };
                out_validation_immediates.* = ValidationImmediates{
                    .BlockOrIf = .{
                        .block_type = block_type,
                        .block_value = block_value,
                    },
                };
            }

            fn decodeTablePair(_reader: anytype) !InstructionImmediates {
                const elem_index = try decodeLEB128(u32, _reader);
                const table_index = try decodeLEB128(u32, _reader);

                return InstructionImmediates{
                    .TablePair = TablePairImmediates{
                        .index_x = elem_index,
                        .index_y = table_index,
                    },
                };
            }

            fn decodeMemoryOffsetAndLane(_reader: anytype, comptime bitwidth: u32, _module: *ModuleDefinition) DecodeError!InstructionImmediates {
                const memarg = try MemArg.decode(_reader, bitwidth);
                const laneidx = try readByte(&_reader);
                const immediates = MemoryOffsetAndLaneImmediates{
                    .offset = memarg.offset,
                    .laneidx = laneidx,
                };
                const index = _module.code.memory_offset_and_lane_immediates.items.len;
                try _module.code.memory_offset_and_lane_immediates.append(immediates);
                return InstructionImmediates{ .Index = @intCast(index) };
            }
        };

        const wasm_op: WasmOpcode = try decodeWasmOpcode(reader);

        // note that this opcode can be remapped as we get more information about the instruction
        var opcode: Opcode = wasm_op.toOpcode();
        var immediate = InstructionImmediates{ .Void = {} };
        var validation_immediates = ValidationImmediates{ .Void = {} };

        switch (opcode) {
            .Select_T => {
                const num_types = try decodeLEB128(u32, reader);
                if (num_types != 1) {
                    return error.ValidationSelectArity;
                }
                immediate = InstructionImmediates{ .ValType = try ValType.decode(reader) };
            },
            .Local_Get, .Local_Set, .Local_Tee => {
                const index = try decodeLEB128(u32, reader);
                immediate = InstructionImmediates{ .Index = index };

                const type_def: *const FunctionTypeDefinition = func.typeDefinition(module);
                const params = type_def.getParams();

                // note we don't do validation here, we'll do that after decode
                var is_local_v128: bool = false;
                if (index < params.len) {
                    is_local_v128 = params[index] == .V128;
                } else {
                    const locals = func.locals(module);
                    const func_locals_index = index - params.len;
                    if (func_locals_index < locals.len) {
                        is_local_v128 = locals[func_locals_index] == .V128;
                    }
                }

                if (is_local_v128) {
                    opcode = switch (opcode) {
                        .Local_Get => .Local_Get_V128,
                        .Local_Set => .Local_Set_V128,
                        .Local_Tee => .Local_Tee_V128,
                        else => unreachable,
                    };
                }
            },
            .Global_Get, .Global_Set => {
                immediate = InstructionImmediates{ .Index = try decodeLEB128(u32, reader) };
                if (immediate.Index < module.globals.items.len) {
                    if (module.globals.items[immediate.Index].valtype == .V128) {
                        opcode = switch (opcode) {
                            .Global_Get => .Global_Get_V128,
                            .Global_Set => .Global_Set_V128,
                            else => unreachable,
                        };
                    }
                }
            },
            .Table_Get => {
                immediate = InstructionImmediates{ .Index = try decodeLEB128(u32, reader) };
            },
            .Table_Set => {
                immediate = InstructionImmediates{ .Index = try decodeLEB128(u32, reader) };
            },
            .I32_Const => {
                immediate = InstructionImmediates{ .ValueI32 = try decodeLEB128(i32, reader) };
            },
            .I64_Const => {
                immediate = InstructionImmediates{ .ValueI64 = try decodeLEB128(i64, reader) };
            },
            .F32_Const => {
                immediate = InstructionImmediates{ .ValueF32 = try decodeFloat(f32, reader) };
            },
            .F64_Const => {
                immediate = InstructionImmediates{ .ValueF64 = try decodeFloat(f64, reader) };
            },
            .Block => {
                try Helpers.decodeBlockType(reader, module, &immediate, &validation_immediates);
            },
            .Loop => {
                try Helpers.decodeBlockType(reader, module, &immediate, &validation_immediates);
            },
            .If => {
                var block_immediates: InstructionImmediates = undefined;
                try Helpers.decodeBlockType(reader, module, &block_immediates, &validation_immediates);

                // continuation values will be set later in the code section decode
                immediate = InstructionImmediates{
                    .If = IfImmediates{
                        .num_returns = block_immediates.Block.num_returns,
                        .else_continuation_relative = std.math.maxInt(u32),
                        .end_continuation_relative = std.math.maxInt(u32),
                    },
                };
            },
            .Branch => {
                immediate = InstructionImmediates{ .LabelId = try decodeLEB128(u32, reader) };
            },
            .Branch_If => {
                immediate = InstructionImmediates{ .LabelId = try decodeLEB128(u32, reader) };
            },
            .Branch_Table => {
                const table_length = try decodeLEB128(u32, reader);

                var label_ids = std.array_list.Managed(u32).init(module.allocator);
                defer label_ids.deinit();
                try label_ids.ensureTotalCapacity(table_length);

                var index: u32 = 0;
                while (index < table_length) : (index += 1) {
                    const id = try decodeLEB128(u32, reader);
                    label_ids.addOneAssumeCapacity().* = id;
                }
                const fallback_id = try decodeLEB128(u32, reader);

                // check to see if there are any existing tables we can reuse
                var needs_immediate: bool = true;
                for (module.code.branch_table_immediates.items, 0..) |*item, i| {
                    if (item.fallback_id == fallback_id) {
                        const item_label_ids: []const u32 = item.getLabelIds(module.*);
                        if (std.mem.eql(u32, item_label_ids, label_ids.items)) {
                            immediate = InstructionImmediates{ .Index = @as(u32, @intCast(i)) };
                            needs_immediate = false;
                            break;
                        }
                    }
                }

                if (needs_immediate) {
                    immediate = InstructionImmediates{ .Index = @as(u32, @intCast(module.code.branch_table_immediates.items.len)) };

                    const label_ids_begin: u32 = @intCast(module.code.branch_table_ids_immediates.items.len);
                    try module.code.branch_table_ids_immediates.appendSlice(label_ids.items);
                    const label_ids_end: u32 = @intCast(module.code.branch_table_ids_immediates.items.len);

                    const branch_table = BranchTableImmediates{
                        .label_ids_begin = label_ids_begin,
                        .label_ids_end = label_ids_end,
                        .fallback_id = fallback_id,
                    };

                    try module.code.branch_table_immediates.append(branch_table);
                }
            },
            .Call_Local => {
                const index = try decodeLEB128(u32, reader);
                immediate = InstructionImmediates{ .Index = index }; // function index
            },
            .Call_Indirect => {
                immediate = InstructionImmediates{ .CallIndirect = .{
                    .type_index = try decodeLEB128(u32, reader),
                    .table_index = try decodeLEB128(u32, reader),
                } };
            },
            .I32_Load => {
                const memarg = try MemArg.decode(reader, 32);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .I64_Load => {
                const memarg = try MemArg.decode(reader, 64);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .F32_Load => {
                const memarg = try MemArg.decode(reader, 32);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .F64_Load => {
                const memarg = try MemArg.decode(reader, 64);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .I32_Load8_S => {
                const memarg = try MemArg.decode(reader, 8);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .I32_Load8_U => {
                const memarg = try MemArg.decode(reader, 8);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .I32_Load16_S => {
                const memarg = try MemArg.decode(reader, 16);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .I32_Load16_U => {
                const memarg = try MemArg.decode(reader, 16);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .I64_Load8_S => {
                const memarg = try MemArg.decode(reader, 8);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .I64_Load8_U => {
                const memarg = try MemArg.decode(reader, 8);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .I64_Load16_S => {
                const memarg = try MemArg.decode(reader, 16);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .I64_Load16_U => {
                const memarg = try MemArg.decode(reader, 16);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .I64_Load32_S => {
                const memarg = try MemArg.decode(reader, 32);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .I64_Load32_U => {
                const memarg = try MemArg.decode(reader, 32);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .I32_Store => {
                const memarg = try MemArg.decode(reader, 32);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .I64_Store => {
                const memarg = try MemArg.decode(reader, 64);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .F32_Store => {
                const memarg = try MemArg.decode(reader, 32);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .F64_Store => {
                const memarg = try MemArg.decode(reader, 64);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .I32_Store8 => {
                const memarg = try MemArg.decode(reader, 8);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .I32_Store16 => {
                const memarg = try MemArg.decode(reader, 16);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .I64_Store8 => {
                const memarg = try MemArg.decode(reader, 8);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .I64_Store16 => {
                const memarg = try MemArg.decode(reader, 16);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .I64_Store32 => {
                const memarg = try MemArg.decode(reader, 32);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .Memory_Size => {
                const reserved = try readByte(&reader);
                if (reserved != 0x00) {
                    return error.MalformedMissingZeroByte;
                }
            },
            .Memory_Grow => {
                const reserved = try readByte(&reader);
                if (reserved != 0x00) {
                    return error.MalformedMissingZeroByte;
                }
            },
            .Memory_Init => {
                try ModuleValidator.validateMemoryIndex(module);

                if (module.data_count == null) {
                    return error.MalformedMissingDataCountSection;
                }

                immediate = InstructionImmediates{ .Index = try decodeLEB128(u32, reader) }; // dataidx

                const reserved = try readByte(&reader);
                if (reserved != 0x00) {
                    return error.MalformedMissingZeroByte;
                }
            },
            .Ref_Null => {
                const valtype = try ValType.decode(reader);
                if (valtype.isRefType() == false) {
                    return error.MalformedBytecode;
                }

                immediate = InstructionImmediates{ .ValType = valtype };
            },
            .Ref_Func => {
                immediate = InstructionImmediates{ .Index = try decodeLEB128(u32, reader) }; // funcidx
            },
            .Data_Drop => {
                immediate = InstructionImmediates{ .Index = try decodeLEB128(u32, reader) }; // dataidx
            },
            .Memory_Copy => {
                var reserved = try readByte(&reader);
                if (reserved != 0x00) {
                    return error.MalformedMissingZeroByte;
                }
                reserved = try readByte(&reader);
                if (reserved != 0x00) {
                    return error.MalformedMissingZeroByte;
                }
            },
            .Memory_Fill => {
                const reserved = try readByte(&reader);
                if (reserved != 0x00) {
                    return error.MalformedMissingZeroByte;
                }
            },
            .Table_Init => {
                immediate = try Helpers.decodeTablePair(reader);
            },
            .Elem_Drop => {
                immediate = InstructionImmediates{ .Index = try decodeLEB128(u32, reader) }; // elemidx
            },
            .Table_Copy => {
                immediate = try Helpers.decodeTablePair(reader);
            },
            .Table_Grow => {
                immediate = InstructionImmediates{ .Index = try decodeLEB128(u32, reader) }; // elemidx
            },
            .Table_Size => {
                immediate = InstructionImmediates{ .Index = try decodeLEB128(u32, reader) }; // elemidx
            },
            .Table_Fill => {
                immediate = InstructionImmediates{ .Index = try decodeLEB128(u32, reader) }; // elemidx
            },
            .V128_Load => {
                const memarg = try MemArg.decode(reader, 128);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .V128_Load8x8_S, .V128_Load8x8_U => {
                const memarg = try MemArg.decode(reader, 8 * 8);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .V128_Load16x4_S, .V128_Load16x4_U => {
                const memarg = try MemArg.decode(reader, 16 * 4);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .V128_Load32x2_S, .V128_Load32x2_U => {
                const memarg = try MemArg.decode(reader, 32 * 2);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .V128_Load8_Splat => {
                const memarg = try MemArg.decode(reader, 8);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .V128_Load16_Splat => {
                const memarg = try MemArg.decode(reader, 16);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .V128_Load32_Splat => {
                const memarg = try MemArg.decode(reader, 32);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .V128_Load64_Splat => {
                const memarg = try MemArg.decode(reader, 64);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .I8x16_Extract_Lane_S,
            .I8x16_Extract_Lane_U,
            .I8x16_Replace_Lane,
            .I16x8_Extract_Lane_S,
            .I16x8_Extract_Lane_U,
            .I16x8_Replace_Lane,
            .I32x4_Extract_Lane,
            .I32x4_Replace_Lane,
            .I64x2_Extract_Lane,
            .I64x2_Replace_Lane,
            .F32x4_Extract_Lane,
            .F32x4_Replace_Lane,
            .F64x2_Extract_Lane,
            .F64x2_Replace_Lane,
            => {
                immediate = InstructionImmediates{ .Index = try readByte(&reader) }; // laneidx
            },
            .V128_Store => {
                const memarg = try MemArg.decode(reader, 128);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .V128_Const => {
                const vec: v128 = try decodeVec(reader);
                immediate = InstructionImmediates{ .Index = @intCast(module.code.v128_immediates.items.len) };
                try module.code.v128_immediates.append(vec);
            },
            .I8x16_Shuffle => {
                var lane_indices: [16]u8 = undefined;
                for (&lane_indices) |*v| {
                    const laneidx: u8 = try readByte(&reader);
                    v.* = laneidx;
                }

                immediate = InstructionImmediates{ .Index = @intCast(module.code.vec_shuffle_16_immediates.items.len) };
                try module.code.vec_shuffle_16_immediates.append(lane_indices);
            },
            .V128_Load8_Lane => {
                immediate = try Helpers.decodeMemoryOffsetAndLane(reader, 8, module);
            },
            .V128_Load16_Lane => {
                immediate = try Helpers.decodeMemoryOffsetAndLane(reader, 16, module);
            },
            .V128_Load32_Lane => {
                immediate = try Helpers.decodeMemoryOffsetAndLane(reader, 32, module);
            },
            .V128_Load64_Lane => {
                immediate = try Helpers.decodeMemoryOffsetAndLane(reader, 64, module);
            },
            .V128_Store8_Lane => {
                immediate = try Helpers.decodeMemoryOffsetAndLane(reader, 8, module);
            },
            .V128_Store16_Lane => {
                immediate = try Helpers.decodeMemoryOffsetAndLane(reader, 16, module);
            },
            .V128_Store32_Lane => {
                immediate = try Helpers.decodeMemoryOffsetAndLane(reader, 32, module);
            },
            .V128_Store64_Lane => {
                immediate = try Helpers.decodeMemoryOffsetAndLane(reader, 64, module);
            },
            .V128_Load32_Zero => {
                const memarg = try MemArg.decode(reader, 128);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            .V128_Load64_Zero => {
                const memarg = try MemArg.decode(reader, 128);
                immediate = InstructionImmediates{ .MemoryOffset = memarg.offset };
            },
            else => {},
        }

        return DecodedInstruction{
            .instruction = .{
                .opcode = opcode,
                .immediate = immediate,
            },
            .validation_immediates = validation_immediates,
        };
    }
};

const CustomSection = struct {
    name: []const u8,
    data: std.array_list.Managed(u8),
};

pub const NameCustomSection = struct {
    // all string slices here are static strings or point into CustomSection.data - no need to free
    module_name: []const u8,
    function_names: std.AutoHashMap(u32, []const u8),

    fn init(allocator: std.mem.Allocator) NameCustomSection {
        return NameCustomSection{
            .module_name = "",
            .function_names = std.AutoHashMap(u32, []const u8).init(allocator),
        };
    }

    fn deinit(self: *NameCustomSection) void {
        self.function_names.deinit();
    }

    fn decode(self: *NameCustomSection, module_definition: *const ModuleDefinition, bytes: []const u8) MalformedError!void {
        self.decodeInternal(module_definition, bytes) catch |err| {
            std.debug.print("NameCustomSection.decode: caught error from internal: {}", .{err});
            return MalformedError.MalformedCustomSection;
        };
    }

    fn decodeInternal(self: *NameCustomSection, module_definition: *const ModuleDefinition, bytes: []const u8) !void {
        const DecodeHelpers = struct {
            fn readName(stream: anytype) ![]const u8 {
                const reader = stream.reader();
                const name_length = try decodeLEB128(u32, reader);
                const name: []const u8 = stream.buffer[stream.pos .. stream.pos + name_length];
                try stream.seekBy(name_length);
                return name;
            }
        };

        var fixed_buffer_stream = std.io.fixedBufferStream(bytes);
        var reader = fixed_buffer_stream.reader();

        while (try fixed_buffer_stream.getPos() != try fixed_buffer_stream.getEndPos()) {
            const section_code = try readByte(&reader);
            const section_size = try decodeLEB128(u32, reader);

            switch (section_code) {
                0 => {
                    self.module_name = try DecodeHelpers.readName(&fixed_buffer_stream);
                },
                1 => {
                    const num_func_names = try decodeLEB128(u32, reader);
                    try self.function_names.ensureTotalCapacity(num_func_names);

                    var index: u32 = 0;
                    while (index < num_func_names) : (index += 1) {
                        const func_index = try decodeLEB128(u32, reader);
                        const func_name: []const u8 = try DecodeHelpers.readName(&fixed_buffer_stream);
                        try self.function_names.putNoClobber(func_index, func_name);
                    }
                },
                2 => { // TODO locals
                    try fixed_buffer_stream.seekBy(section_size);
                },
                else => {
                    try fixed_buffer_stream.seekBy(section_size);
                },
            }
        }

        if (self.module_name.len == 0) {
            if (module_definition.debug_name.len > 0) {
                self.module_name = module_definition.debug_name;
            } else {
                self.module_name = "<unknown_module>";
            }
        }
    }

    pub fn getModuleName(self: *const NameCustomSection) []const u8 {
        return self.module_name;
    }

    pub fn findFunctionName(self: *const NameCustomSection, func_index: usize) []const u8 {
        if (self.function_names.get(@intCast(func_index))) |name| {
            return name;
        }
        return "<unknown_function>";
    }

    // fn findFunctionLocalName(self: *NameCustomSection, func_index: u32, local_index: u32) []const u8 {
    //     return "<unknown_local>";
    // }
};

pub const FunctionStackStats = struct {
    values: usize = 0,
    labels: usize = 0,
};

const ModuleValidator = struct {
    const ControlFrame = struct {
        opcode: Opcode,
        start_types: []const ValType,
        end_types: []const ValType,
        types_stack_height: usize,
        is_unreachable: bool,
    };

    // Note that we use a nullable ValType here to map to the "Unknown" value type as described in the wasm spec
    // validation algorithm: https://webassembly.github.io/spec/core/appendix/algorithm.html
    type_stack: std.array_list.Managed(?ValType),
    control_stack: std.array_list.Managed(ControlFrame),
    control_types: StableArray(ValType),
    log: Logger,

    // tracks stack usage per-function
    stack_stats: FunctionStackStats = .{},

    fn init(allocator: std.mem.Allocator, log: Logger) ModuleValidator {
        return ModuleValidator{
            .type_stack = std.array_list.Managed(?ValType).init(allocator),
            .control_stack = std.array_list.Managed(ControlFrame).init(allocator),
            .control_types = StableArray(ValType).init(1 * 1024 * 1024),
            .log = log,
        };
    }

    fn deinit(self: *ModuleValidator) void {
        self.type_stack.deinit();
        self.control_stack.deinit();
        self.control_types.deinit();
    }

    fn validateTypeIndex(index: u64, module: *const ModuleDefinition) !void {
        if (module.types.items.len <= index) {
            return error.ValidationUnknownType;
        }
    }

    fn validateGlobalIndex(index: u64, module: *const ModuleDefinition) !void {
        if (module.imports.globals.items.len + module.globals.items.len <= index) {
            return error.ValidationUnknownGlobal;
        }
    }

    fn validateTableIndex(index: u64, module: *const ModuleDefinition) !void {
        if (module.imports.tables.items.len + module.tables.items.len <= index) {
            return error.ValidationUnknownTable;
        }
    }

    fn getTableReftype(module: *const ModuleDefinition, index: u64) !ValType {
        if (index < module.imports.tables.items.len) {
            return module.imports.tables.items[@intCast(index)].reftype;
        }

        const local_index = index - module.imports.tables.items.len;
        if (local_index < module.tables.items.len) {
            return module.tables.items[@intCast(local_index)].reftype;
        }

        return error.ValidationUnknownTable;
    }

    fn validateFunctionIndex(index: u64, module: *const ModuleDefinition) !void {
        // function imports are setup with 1:1 local function trampolines, so we can check the index against
        // the local function array
        if (module.functions.items.len <= index) {
            return error.ValidationUnknownFunction;
        }
    }

    fn validateMemoryIndex(module: *const ModuleDefinition) !void {
        if (module.imports.memories.items.len + module.memories.items.len < 1) {
            return error.ValidationUnknownMemory;
        }
    }

    fn validateElementIndex(index: u64, module: *const ModuleDefinition) !void {
        if (module.elements.items.len <= index) {
            return error.ValidationUnknownElement;
        }
    }

    fn validateDataIndex(index: u64, module: *const ModuleDefinition) !void {
        if (module.data_count.? <= index) {
            return error.ValidationUnknownData;
        }
    }

    fn beginValidateCode(self: *ModuleValidator, module: *const ModuleDefinition, func: *const FunctionDefinition) !void {
        try validateTypeIndex(func.type_index, module);

        self.stack_stats = .{};
        const func_type_def: *const FunctionTypeDefinition = &module.types.items[func.type_index];

        try self.pushControl(Opcode.Call_Local, func_type_def.getParams(), func_type_def.getReturns());
    }

    // Note that validateCode() can modify the instruction depending on if the type information causes a change.
    // For example, see Drop which is converted into Drop_V128 if the dropped type is a V128.
    fn validateCode(
        self: *ModuleValidator,
        module: *const ModuleDefinition,
        func: *const FunctionDefinition,
        instruction: *Instruction,
        validation_immediates: ValidationImmediates,
    ) !void {
        const Helpers = struct {
            fn popReturnTypes(validator: *ModuleValidator, types: []const ValType) !void {
                var i = types.len;
                while (i > 0) {
                    i -= 1;
                    try validator.popType(types[i]);
                }
            }

            fn enterBlock(validator: *ModuleValidator, module_: *const ModuleDefinition, opcode: Opcode, immediate: ValidationImmediates) !void {
                var block_type: BlockType = undefined;
                var block_value: BlockTypeValue = undefined;

                switch (opcode) {
                    .Block, .Loop, .If => {
                        block_type = immediate.BlockOrIf.block_type;
                        block_value = immediate.BlockOrIf.block_value;
                    },
                    else => unreachable,
                }

                const start_types: []const ValType = block_value.getBlocktypeParamTypes(block_type, module_);
                const end_types: []const ValType = block_value.getBlocktypeReturnTypes(block_type, module_);
                try popReturnTypes(validator, start_types);

                try validator.pushControl(opcode, start_types, end_types);
            }

            fn getLocalValtype(validator: *const ModuleValidator, module_: *const ModuleDefinition, func_: *const FunctionDefinition, locals_index: u64) !ValType {
                var i = validator.control_stack.items.len - 1;
                while (i >= 0) : (i -= 1) {
                    const func_type: *const FunctionTypeDefinition = &module_.types.items[func_.type_index];
                    if (locals_index < func_type.num_params) {
                        return func_type.getParams()[@intCast(locals_index)];
                    } else {
                        const locals: []const ValType = func_.locals(module_);
                        if (locals.len <= locals_index - func_type.num_params) {
                            return error.ValidationUnknownLocal;
                        }
                        return locals[@as(usize, @intCast(locals_index)) - func_type.num_params];
                    }
                }
                unreachable;
            }

            const GlobalMutablilityRequirement = enum {
                None,
                Mutable,
            };

            fn getGlobalValtype(module_: *const ModuleDefinition, global_index: u64, required_mutability: GlobalMutablilityRequirement) !ValType {
                if (global_index < module_.imports.globals.items.len) {
                    const global: *const GlobalImportDefinition = &module_.imports.globals.items[@intCast(global_index)];
                    if (required_mutability == .Mutable and global.mut == .Immutable) {
                        return error.ValidationImmutableGlobal;
                    }
                    return global.valtype;
                }

                const module_global_index = global_index - module_.imports.globals.items.len;
                if (module_global_index < module_.globals.items.len) {
                    const global: *const GlobalDefinition = &module_.globals.items[@intCast(module_global_index)];
                    if (required_mutability == .Mutable and global.mut == .Immutable) {
                        return error.ValidationImmutableGlobal;
                    }
                    return global.valtype;
                }

                return error.ValidationUnknownGlobal;
            }

            fn vecLaneTypeToValtype(comptime T: type) ValType {
                return switch (T) {
                    i8 => .I32,
                    u8 => .I32,
                    i16 => .I32,
                    u16 => .I32,
                    i32 => .I32,
                    i64 => .I64,
                    f32 => .F32,
                    f64 => .F64,
                    else => @compileError("unsupported lane type"),
                };
            }

            fn validateNumericUnaryOp(validator: *ModuleValidator, pop_type: ValType, push_type: ValType) !void {
                try validator.popType(pop_type);
                try validator.pushType(push_type);
            }

            fn validateNumericBinaryOp(validator: *ModuleValidator, pop_type: ValType, push_type: ValType) !void {
                try validator.popType(pop_type);
                try validator.popType(pop_type);
                try validator.pushType(push_type);
            }

            fn validateLoadOp(validator: *ModuleValidator, module_: *const ModuleDefinition, load_type: ValType) !void {
                try validateMemoryIndex(module_);
                const index_type: ValType = module_.getMemoryLimits().indexType();
                try validator.popType(index_type);
                try validator.pushType(load_type);
            }

            fn validateStoreOp(validator: *ModuleValidator, module_: *const ModuleDefinition, store_type: ValType) !void {
                try validateMemoryIndex(module_);
                const index_type: ValType = module_.getMemoryLimits().indexType();
                try validator.popType(store_type);
                try validator.popType(index_type);
            }

            fn validateVectorLane(comptime T: type, laneidx: u32) !void {
                const vec_type_info = @typeInfo(T).vector;
                if (vec_type_info.len <= laneidx) {
                    return error.ValidationInvalidLaneIndex;
                }
            }

            fn validateLoadLaneOp(validator: *ModuleValidator, module_: *const ModuleDefinition, instruction_: *Instruction, comptime T: type) !void {
                const immediate_index = instruction_.immediate.Index;
                const immediates: MemoryOffsetAndLaneImmediates = module_.code.memory_offset_and_lane_immediates.items[immediate_index];
                try validateVectorLane(T, immediates.laneidx);
                try validator.popType(.V128);
                try validator.popType(.I32);
                try validateMemoryIndex(module_);
                try validator.pushType(.V128);
            }

            fn validateStoreLaneOp(validator: *ModuleValidator, module_: *const ModuleDefinition, instruction_: *Instruction, comptime T: type) !void {
                const immediate_index = instruction_.immediate.Index;
                const immediates: MemoryOffsetAndLaneImmediates = module_.code.memory_offset_and_lane_immediates.items[immediate_index];
                try validateVectorLane(T, immediates.laneidx);
                try validator.popType(.V128);
                try validator.popType(.I32);
                try validateMemoryIndex(module_);
            }

            fn validateVecExtractLane(comptime T: type, validator: *ModuleValidator, instruction_: *Instruction) !void {
                try validateVectorLane(T, instruction_.immediate.Index);
                const lane_valtype = vecLaneTypeToValtype(@typeInfo(T).vector.child);
                try validator.popType(.V128);
                try validator.pushType(lane_valtype);
            }

            fn validateVecReplaceLane(comptime T: type, validator: *ModuleValidator, instruction_: *Instruction) !void {
                try validateVectorLane(T, instruction_.immediate.Index);
                const lane_valtype = vecLaneTypeToValtype(@typeInfo(T).vector.child);
                try validator.popType(lane_valtype);
                try validator.popType(.V128);
                try validator.pushType(.V128);
            }

            fn getControlTypes(validator: *ModuleValidator, control_index: usize) ![]const ValType {
                if (validator.control_stack.items.len <= control_index) {
                    return error.ValidationUnknownLabel;
                }
                const stack_index = validator.control_stack.items.len - control_index - 1;
                const frame: *ControlFrame = &validator.control_stack.items[stack_index];
                return if (frame.opcode != .Loop) frame.end_types else frame.start_types;
            }

            fn markFrameInstructionsUnreachable(validator: *ModuleValidator) !void {
                var frame: *ControlFrame = &validator.control_stack.items[validator.control_stack.items.len - 1];
                try validator.type_stack.resize(frame.types_stack_height);
                frame.is_unreachable = true;
            }

            fn popPushFuncTypes(validator: *ModuleValidator, type_index: usize, module_: *const ModuleDefinition) !void {
                const func_type: *const FunctionTypeDefinition = &module_.types.items[type_index];

                try popReturnTypes(validator, func_type.getParams());
                for (func_type.getReturns()) |valtype| {
                    try validator.pushType(valtype);
                }
            }
        };
        switch (instruction.opcode) {
            .Invalid => unreachable,
            .Unreachable => {
                try Helpers.markFrameInstructionsUnreachable(self);
            },
            .DebugTrap, .Noop => {},
            .Drop => {
                if (try self.popAnyType()) |valtype| {
                    switch (valtype) {
                        .V128 => {
                            instruction.opcode = .Drop_V128;
                        },
                        else => {},
                    }
                }
            },
            .Drop_V128 => unreachable, // validation generates this instruction, it shouldn't be generated externally
            .Block => {
                try Helpers.enterBlock(self, module, instruction.opcode, validation_immediates);
            },
            .Loop => {
                try Helpers.enterBlock(self, module, instruction.opcode, validation_immediates);
            },
            .If, .IfNoElse => {
                try self.popType(.I32);
                try Helpers.enterBlock(self, module, instruction.opcode, validation_immediates);
            },
            .Else => {
                const frame: ControlFrame = try self.popControl();
                if (frame.opcode.isIf() == false) {
                    return error.ValidationIfElseMismatch;
                }
                try self.pushControl(.Else, frame.start_types, frame.end_types);
            },
            .End => {
                const frame: ControlFrame = try self.popControl();

                // if must have matching else block when returns are expected and the params don't match
                if (frame.opcode.isIf() and !std.mem.eql(ValType, frame.start_types, frame.end_types)) {
                    return error.ValidationTypeMismatch;
                }

                if (self.control_stack.items.len > 0) {
                    for (frame.end_types) |valtype| {
                        try self.pushType(valtype);
                    }
                }
                try self.freeControlTypes(&frame);
            },
            .Branch => {
                const control_index: u32 = instruction.immediate.LabelId;
                const block_return_types: []const ValType = try Helpers.getControlTypes(self, control_index);

                try Helpers.popReturnTypes(self, block_return_types);
                try Helpers.markFrameInstructionsUnreachable(self);
            },
            .Branch_If => {
                const control_index: u32 = instruction.immediate.LabelId;
                const block_return_types: []const ValType = try Helpers.getControlTypes(self, control_index);
                try self.popType(.I32);

                try Helpers.popReturnTypes(self, block_return_types);
                for (block_return_types) |valtype| {
                    try self.pushType(valtype);
                }
            },
            .Branch_Table => {
                const immediates: *const BranchTableImmediates = &module.code.branch_table_immediates.items[instruction.immediate.Index];
                const label_ids: []const u32 = immediates.getLabelIds(module.*);

                const fallback_block_return_types: []const ValType = try Helpers.getControlTypes(self, immediates.fallback_id);

                try self.popType(.I32);

                for (label_ids) |control_index| {
                    const block_return_types: []const ValType = try Helpers.getControlTypes(self, control_index);

                    if (fallback_block_return_types.len != block_return_types.len) {
                        return error.ValidationTypeMismatch;
                    }

                    // Seems like the wabt validation code for br_table is implemented by "peeking" at types on the stack
                    // instead of actually popping/pushing them. This allows certain block type mismatches to be considered
                    // valid when the current block is marked unreachable.
                    const frame: *const ControlFrame = &self.control_stack.items[control_index];
                    const type_stack: []const ?ValType = self.type_stack.items[frame.types_stack_height..];

                    var i: usize = block_return_types.len;
                    while (i > 0) : (i -= 1) {
                        if (!frame.is_unreachable and frame.types_stack_height < type_stack.len) {
                            if (type_stack[type_stack.len - i] != block_return_types[block_return_types.len - i]) {
                                return error.ValidationTypeMismatch;
                            }
                        }
                    }
                }

                try Helpers.popReturnTypes(self, fallback_block_return_types);
                try Helpers.markFrameInstructionsUnreachable(self);
            },
            .Return => {
                const block_return_types: []const ValType = try Helpers.getControlTypes(self, self.control_stack.items.len - 1);
                try Helpers.popReturnTypes(self, block_return_types);
                try Helpers.markFrameInstructionsUnreachable(self);
            },
            .Call_Local => {
                // The wasm spec has local and import functions in different "index spaces", but because we create
                // a local trampoline to each import function in the same index space, we can simply always check
                // if a function index is valid against the local function array
                const func_index: usize = instruction.immediate.Index;
                if (module.functions.items.len <= func_index) {
                    return error.ValidationUnknownFunction;
                }

                const type_index: usize = module.getFuncTypeIndex(@intCast(func_index));
                try Helpers.popPushFuncTypes(self, type_index, module);
            },
            .Call_Import => {
                const func_index: u64 = instruction.immediate.Index;
                if (module.imports.functions.items.len <= func_index) {
                    return error.ValidationUnknownFunction;
                }

                std.debug.assert(func_index < std.math.maxInt(usize));

                const type_index: usize = module.getFuncTypeIndex(@intCast(func_index));
                try Helpers.popPushFuncTypes(self, type_index, module);
            },
            .Call_Indirect => {
                const immediates: CallIndirectImmediates = instruction.immediate.CallIndirect;

                try validateTypeIndex(immediates.type_index, module);
                try validateTableIndex(immediates.table_index, module);

                try self.popType(.I32);

                try Helpers.popPushFuncTypes(self, immediates.type_index, module);
            },
            .Select => {
                try self.popType(.I32);
                const valtype1_or_null: ?ValType = try self.popAnyType();
                const valtype2_or_null: ?ValType = try self.popAnyType();
                if (valtype1_or_null == null) {
                    try self.pushType(valtype2_or_null);
                } else if (valtype2_or_null == null) {
                    try self.pushType(valtype1_or_null);
                } else {
                    const valtype1 = valtype1_or_null.?;
                    const valtype2 = valtype2_or_null.?;
                    if (valtype1 != valtype2) {
                        return error.ValidationTypeMismatch;
                    }
                    if (valtype1.isRefType()) {
                        return error.ValidationTypeMustBeNumeric;
                    }
                    try self.pushType(valtype1);
                }

                const valtype = self.type_stack.items[self.type_stack.items.len - 1];
                instruction.opcode = if (valtype == .V128) .Select_V128 else .Select;
            },
            .Select_T => {
                const valtype: ValType = instruction.immediate.ValType;
                try self.popType(.I32);
                try self.popType(valtype);
                try self.popType(valtype);
                try self.pushType(valtype);

                instruction.opcode = if (valtype == .V128) .Select_V128 else .Select;
            },
            .Select_V128 => unreachable, // this opcode is generated by validation only
            .Local_Get, .Local_Get_V128 => {
                const valtype = try Helpers.getLocalValtype(self, module, func, instruction.immediate.Index);
                try self.pushType(valtype);
            },
            .Local_Set, .Local_Set_V128 => {
                const valtype = try Helpers.getLocalValtype(self, module, func, instruction.immediate.Index);
                try self.popType(valtype);
            },
            .Local_Tee, .Local_Tee_V128 => {
                const valtype = try Helpers.getLocalValtype(self, module, func, instruction.immediate.Index);
                try self.popType(valtype);
                try self.pushType(valtype);
            },
            .Global_Get, .Global_Get_V128 => {
                const valtype = try Helpers.getGlobalValtype(module, instruction.immediate.Index, .None);
                try self.pushType(valtype);
            },
            .Global_Set, .Global_Set_V128 => {
                const valtype = try Helpers.getGlobalValtype(module, instruction.immediate.Index, .Mutable);
                try self.popType(valtype);
            },
            .Table_Get => {
                const reftype = try getTableReftype(module, instruction.immediate.Index);
                try self.popType(.I32);
                try self.pushType(reftype);
            },
            .Table_Set => {
                const reftype = try getTableReftype(module, instruction.immediate.Index);
                try self.popType(reftype);
                try self.popType(.I32);
            },
            .I32_Load, .I32_Load8_S, .I32_Load8_U, .I32_Load16_S, .I32_Load16_U => {
                try Helpers.validateLoadOp(self, module, .I32);
            },
            .I64_Load, .I64_Load8_S, .I64_Load8_U, .I64_Load16_S, .I64_Load16_U, .I64_Load32_S, .I64_Load32_U => {
                try Helpers.validateLoadOp(self, module, .I64);
            },
            .F32_Load => {
                try Helpers.validateLoadOp(self, module, .F32);
            },
            .F64_Load => {
                try Helpers.validateLoadOp(self, module, .F64);
            },
            .I32_Store, .I32_Store8, .I32_Store16 => {
                try Helpers.validateStoreOp(self, module, .I32);
            },
            .I64_Store, .I64_Store8, .I64_Store16, .I64_Store32 => {
                try Helpers.validateStoreOp(self, module, .I64);
            },
            .F32_Store => {
                try Helpers.validateStoreOp(self, module, .F32);
            },
            .F64_Store => {
                try Helpers.validateStoreOp(self, module, .F64);
            },
            .Memory_Size => {
                try validateMemoryIndex(module);
                const index_type: ValType = module.getMemoryLimits().indexType();
                try self.pushType(index_type);
            },
            .Memory_Grow => {
                try validateMemoryIndex(module);
                const index_type: ValType = module.getMemoryLimits().indexType();
                try self.popType(index_type);
                try self.pushType(index_type);
            },
            .I32_Const => {
                try self.pushType(.I32);
            },
            .I64_Const => {
                try self.pushType(.I64);
            },
            .F32_Const => {
                try self.pushType(.F32);
            },
            .F64_Const => {
                try self.pushType(.F64);
            },
            .I32_Eqz, .I32_Clz, .I32_Ctz, .I32_Popcnt => {
                try Helpers.validateNumericUnaryOp(self, .I32, .I32);
            },
            .I32_Eq,
            .I32_NE,
            .I32_LT_S,
            .I32_LT_U,
            .I32_GT_S,
            .I32_GT_U,
            .I32_LE_S,
            .I32_LE_U,
            .I32_GE_S,
            .I32_GE_U,
            .I32_Add,
            .I32_Sub,
            .I32_Mul,
            .I32_Div_S,
            .I32_Div_U,
            .I32_Rem_S,
            .I32_Rem_U,
            .I32_And,
            .I32_Or,
            .I32_Xor,
            .I32_Shl,
            .I32_Shr_S,
            .I32_Shr_U,
            .I32_Rotl,
            .I32_Rotr,
            => {
                try Helpers.validateNumericBinaryOp(self, .I32, .I32);
            },
            .I64_Clz, .I64_Ctz, .I64_Popcnt => {
                try Helpers.validateNumericUnaryOp(self, .I64, .I64);
            },
            .I64_Eqz => {
                try Helpers.validateNumericUnaryOp(self, .I64, .I32);
            },
            .I64_Eq, .I64_NE, .I64_LT_S, .I64_LT_U, .I64_GT_S, .I64_GT_U, .I64_LE_S, .I64_LE_U, .I64_GE_S, .I64_GE_U => {
                try Helpers.validateNumericBinaryOp(self, .I64, .I32);
            },
            .I64_Add,
            .I64_Sub,
            .I64_Mul,
            .I64_Div_S,
            .I64_Div_U,
            .I64_Rem_S,
            .I64_Rem_U,
            .I64_And,
            .I64_Or,
            .I64_Xor,
            .I64_Shl,
            .I64_Shr_S,
            .I64_Shr_U,
            .I64_Rotl,
            .I64_Rotr,
            => {
                try Helpers.validateNumericBinaryOp(self, .I64, .I64);
            },
            .F32_EQ, .F32_NE, .F32_LT, .F32_GT, .F32_LE, .F32_GE => {
                try Helpers.validateNumericBinaryOp(self, .F32, .I32);
            },
            .F32_Add, .F32_Sub, .F32_Mul, .F32_Div, .F32_Min, .F32_Max, .F32_Copysign => {
                try Helpers.validateNumericBinaryOp(self, .F32, .F32);
            },
            .F32_Abs, .F32_Neg, .F32_Ceil, .F32_Floor, .F32_Trunc, .F32_Nearest, .F32_Sqrt => {
                try Helpers.validateNumericUnaryOp(self, .F32, .F32);
            },
            .F64_Abs, .F64_Neg, .F64_Ceil, .F64_Floor, .F64_Trunc, .F64_Nearest, .F64_Sqrt => {
                try Helpers.validateNumericUnaryOp(self, .F64, .F64);
            },
            .F64_EQ, .F64_NE, .F64_LT, .F64_GT, .F64_LE, .F64_GE => {
                try Helpers.validateNumericBinaryOp(self, .F64, .I32);
            },
            .F64_Add, .F64_Sub, .F64_Mul, .F64_Div, .F64_Min, .F64_Max, .F64_Copysign => {
                try Helpers.validateNumericBinaryOp(self, .F64, .F64);
            },
            .I32_Wrap_I64 => {
                try Helpers.validateNumericUnaryOp(self, .I64, .I32);
            },
            .I32_Trunc_F32_S, .I32_Trunc_F32_U => {
                try Helpers.validateNumericUnaryOp(self, .F32, .I32);
            },
            .I32_Trunc_F64_S, .I32_Trunc_F64_U => {
                try Helpers.validateNumericUnaryOp(self, .F64, .I32);
            },
            .I64_Extend_I32_S, .I64_Extend_I32_U => {
                try Helpers.validateNumericUnaryOp(self, .I32, .I64);
            },
            .I64_Trunc_F32_S, .I64_Trunc_F32_U => {
                try Helpers.validateNumericUnaryOp(self, .F32, .I64);
            },
            .I64_Trunc_F64_S, .I64_Trunc_F64_U => {
                try Helpers.validateNumericUnaryOp(self, .F64, .I64);
            },
            .F32_Convert_I32_S, .F32_Convert_I32_U => {
                try Helpers.validateNumericUnaryOp(self, .I32, .F32);
            },
            .F32_Convert_I64_S, .F32_Convert_I64_U => {
                try Helpers.validateNumericUnaryOp(self, .I64, .F32);
            },
            .F32_Demote_F64 => {
                try Helpers.validateNumericUnaryOp(self, .F64, .F32);
            },
            .F64_Convert_I32_S, .F64_Convert_I32_U => {
                try Helpers.validateNumericUnaryOp(self, .I32, .F64);
            },
            .F64_Convert_I64_S, .F64_Convert_I64_U => {
                try Helpers.validateNumericUnaryOp(self, .I64, .F64);
            },
            .F64_Promote_F32 => {
                try Helpers.validateNumericUnaryOp(self, .F32, .F64);
            },
            .I32_Reinterpret_F32 => {
                try Helpers.validateNumericUnaryOp(self, .F32, .I32);
            },
            .I64_Reinterpret_F64 => {
                try Helpers.validateNumericUnaryOp(self, .F64, .I64);
            },
            .F32_Reinterpret_I32 => {
                try Helpers.validateNumericUnaryOp(self, .I32, .F32);
            },
            .F64_Reinterpret_I64 => {
                try Helpers.validateNumericUnaryOp(self, .I64, .F64);
            },
            .I32_Extend8_S, .I32_Extend16_S => {
                try Helpers.validateNumericUnaryOp(self, .I32, .I32);
            },
            .I64_Extend8_S, .I64_Extend16_S, .I64_Extend32_S => {
                try Helpers.validateNumericUnaryOp(self, .I64, .I64);
            },
            .Ref_Null => {
                try self.pushType(instruction.immediate.ValType);
            },
            .Ref_Is_Null => {
                const valtype_or_null: ?ValType = try self.popAnyType();
                if (valtype_or_null) |valtype| {
                    if (valtype.isRefType() == false) {
                        return error.ValidationTypeMismatch;
                    }
                }
                try self.pushType(.I32);
            },
            .Ref_Func => {
                const func_index: u32 = instruction.immediate.Index;
                try validateFunctionIndex(func_index, module);

                const is_referencing_current_function: bool = &module.functions.items[func_index] == func;

                // references to the current function must be declared in element segments
                if (is_referencing_current_function) {
                    var needs_declaration: bool = true;
                    skip_outer: for (module.elements.items) |elem_def| {
                        if (elem_def.mode == .Declarative and elem_def.reftype == .FuncRef) {
                            if (elem_def.elems_value.items.len > 0) {
                                for (elem_def.elems_value.items) |val| {
                                    if (val.FuncRef.index == func_index) {
                                        needs_declaration = false;
                                        break :skip_outer;
                                    }
                                }
                            } else {
                                for (elem_def.elems_expr.items) |expr| {
                                    if (std.meta.activeTag(expr) == .Value) {
                                        if (expr.Value.val.FuncRef.index == func_index) {
                                            needs_declaration = false;
                                            break :skip_outer;
                                        }
                                    }
                                }
                            }
                        }
                    }

                    if (needs_declaration) {
                        return error.ValidationFuncRefUndeclared;
                    }
                }

                try self.pushType(.FuncRef);
            },
            .I32_Trunc_Sat_F32_S, .I32_Trunc_Sat_F32_U => {
                try Helpers.validateNumericUnaryOp(self, .F32, .I32);
            },
            .I32_Trunc_Sat_F64_S, .I32_Trunc_Sat_F64_U => {
                try Helpers.validateNumericUnaryOp(self, .F64, .I32);
            },
            .I64_Trunc_Sat_F32_S, .I64_Trunc_Sat_F32_U => {
                try Helpers.validateNumericUnaryOp(self, .F32, .I64);
            },
            .I64_Trunc_Sat_F64_S, .I64_Trunc_Sat_F64_U => {
                try Helpers.validateNumericUnaryOp(self, .F64, .I64);
            },
            .Memory_Init => {
                try validateMemoryIndex(module);
                try validateDataIndex(instruction.immediate.Index, module);
                const index_type: ValType = module.getMemoryLimits().indexType();
                try self.popType(index_type);
                try self.popType(index_type);
                try self.popType(index_type);
            },
            .Data_Drop => {
                if (module.data_count != null) {
                    try validateDataIndex(instruction.immediate.Index, module);
                } else {
                    return error.MalformedMissingDataCountSection;
                }
            },
            .Memory_Fill => {
                try validateMemoryIndex(module);
                const index_type: ValType = module.getMemoryLimits().indexType();
                try self.popType(index_type);
                try self.popType(.I32);
                try self.popType(index_type);
            },
            .Memory_Copy => {
                try validateMemoryIndex(module);
                const index_type: ValType = module.getMemoryLimits().indexType();
                try self.popType(index_type);
                try self.popType(index_type);
                try self.popType(index_type);
            },
            .Table_Init => {
                const pair: TablePairImmediates = instruction.immediate.TablePair;
                const elem_index = pair.index_x;
                const table_index = pair.index_y;
                try validateTableIndex(table_index, module);
                try validateElementIndex(elem_index, module);

                const elem_reftype: ValType = module.elements.items[elem_index].reftype;
                const table_reftype: ValType = module.tables.items[table_index].reftype;

                if (elem_reftype != table_reftype) {
                    return error.ValidationTypeMismatch;
                }

                try self.popType(.I32);
                try self.popType(.I32);
                try self.popType(.I32);
            },
            .Elem_Drop => {
                try validateElementIndex(instruction.immediate.Index, module);
            },
            .Table_Copy => {
                const pair: TablePairImmediates = instruction.immediate.TablePair;
                const dest_table_index = pair.index_x;
                const src_table_index = pair.index_y;
                try validateTableIndex(dest_table_index, module);
                try validateTableIndex(src_table_index, module);

                const dest_reftype: ValType = module.tables.items[dest_table_index].reftype;
                const src_reftype: ValType = module.tables.items[src_table_index].reftype;

                if (dest_reftype != src_reftype) {
                    return error.ValidationTypeMismatch;
                }

                try self.popType(.I32);
                try self.popType(.I32);
                try self.popType(.I32);
            },
            .Table_Grow => {
                try validateTableIndex(instruction.immediate.Index, module);

                try self.popType(.I32);
                if (try self.popAnyType()) |init_type| {
                    const table_reftype: ValType = try getTableReftype(module, instruction.immediate.Index);
                    if (init_type != table_reftype) {
                        return error.ValidationTypeMismatch;
                    }
                }

                try self.pushType(.I32);
            },
            .Table_Size => {
                try validateTableIndex(instruction.immediate.Index, module);
                try self.pushType(.I32);
            },
            .Table_Fill => {
                try validateTableIndex(instruction.immediate.Index, module);
                try self.popType(.I32);
                if (try self.popAnyType()) |valtype| {
                    const table_reftype: ValType = try getTableReftype(module, instruction.immediate.Index);
                    if (valtype != table_reftype) {
                        return error.ValidationTypeMismatch;
                    }
                }
                try self.popType(.I32);
            },
            .V128_Load,
            .V128_Load8x8_S,
            .V128_Load8x8_U,
            .V128_Load16x4_S,
            .V128_Load16x4_U,
            .V128_Load32x2_S,
            .V128_Load32x2_U,
            .V128_Load8_Splat,
            .V128_Load16_Splat,
            .V128_Load32_Splat,
            .V128_Load64_Splat,
            => {
                try Helpers.validateLoadOp(self, module, .V128);
            },
            .I8x16_Splat, .I16x8_Splat, .I32x4_Splat => {
                try Helpers.validateNumericUnaryOp(self, .I32, .V128);
            },
            .I64x2_Splat => {
                try Helpers.validateNumericUnaryOp(self, .I64, .V128);
            },
            .F32x4_Splat => {
                try Helpers.validateNumericUnaryOp(self, .F32, .V128);
            },
            .F64x2_Splat => {
                try Helpers.validateNumericUnaryOp(self, .F64, .V128);
            },
            .I8x16_Extract_Lane_S, .I8x16_Extract_Lane_U => {
                try Helpers.validateVecExtractLane(i8x16, self, instruction);
            },
            .I8x16_Replace_Lane => {
                try Helpers.validateVecReplaceLane(i8x16, self, instruction);
            },
            .I16x8_Extract_Lane_S, .I16x8_Extract_Lane_U => {
                try Helpers.validateVecExtractLane(i16x8, self, instruction);
            },
            .I16x8_Replace_Lane => {
                try Helpers.validateVecReplaceLane(i16x8, self, instruction);
            },
            .I32x4_Extract_Lane => {
                try Helpers.validateVecExtractLane(i32x4, self, instruction);
            },
            .I32x4_Replace_Lane => {
                try Helpers.validateVecReplaceLane(i32x4, self, instruction);
            },
            .I64x2_Extract_Lane => {
                try Helpers.validateVecExtractLane(i64x2, self, instruction);
            },
            .I64x2_Replace_Lane => {
                try Helpers.validateVecReplaceLane(i64x2, self, instruction);
            },
            .F32x4_Extract_Lane => {
                try Helpers.validateVecExtractLane(f32x4, self, instruction);
            },
            .F32x4_Replace_Lane => {
                try Helpers.validateVecReplaceLane(f32x4, self, instruction);
            },
            .F64x2_Extract_Lane => {
                try Helpers.validateVecExtractLane(f64x2, self, instruction);
            },
            .F64x2_Replace_Lane => {
                try Helpers.validateVecReplaceLane(f64x2, self, instruction);
            },
            .V128_Store => {
                try Helpers.validateStoreOp(self, module, .V128);
            },
            .V128_Const => {
                try self.pushType(.V128);
            },
            .I8x16_Shuffle => {
                const indices = module.code.vec_shuffle_16_immediates.items[instruction.immediate.Index];
                for (indices) |v| {
                    if (v >= 32) {
                        return ValidationError.ValidationInvalidLaneIndex;
                    }
                }
                try Helpers.validateNumericBinaryOp(self, .V128, .V128);
            },
            .I8x16_Swizzle => {
                try Helpers.validateNumericBinaryOp(self, .V128, .V128);
            },
            .V128_Not,
            .F32x4_Demote_F64x2_Zero,
            .F64x2_Promote_Low_F32x4,
            .I8x16_Abs,
            .I8x16_Neg,
            .I8x16_Popcnt,
            .F32x4_Ceil,
            .F32x4_Floor,
            .F32x4_Trunc,
            .F32x4_Nearest,
            .F64x2_Ceil,
            .F64x2_Floor,
            .F64x2_Trunc,
            .F64x2_Nearest,
            .I16x8_Extadd_Pairwise_I8x16_S,
            .I16x8_Extadd_Pairwise_I8x16_U,
            .I32x4_Extadd_Pairwise_I16x8_S,
            .I32x4_Extadd_Pairwise_I16x8_U,
            .I16x8_Abs,
            .I16x8_Neg,
            .I16x8_Extend_Low_I8x16_S,
            .I16x8_Extend_High_I8x16_S,
            .I16x8_Extend_Low_I8x16_U,
            .I16x8_Extend_High_I8x16_U,
            .I32x4_Abs,
            .I32x4_Neg,
            .I32x4_Extend_Low_I16x8_S,
            .I32x4_Extend_High_I16x8_S,
            .I32x4_Extend_Low_I16x8_U,
            .I32x4_Extend_High_I16x8_U,
            .I64x2_Abs,
            .I64x2_Neg,
            .I64x2_Extend_Low_I32x4_S,
            .I64x2_Extend_High_I32x4_S,
            .I64x2_Extend_Low_I32x4_U,
            .I64x2_Extend_High_I32x4_U,
            .F32x4_Abs,
            .F32x4_Neg,
            .F32x4_Sqrt,
            .F64x2_Abs,
            .F64x2_Neg,
            .F64x2_Sqrt,
            .F32x4_Trunc_Sat_F32x4_S,
            .F32x4_Trunc_Sat_F32x4_U,
            .F32x4_Convert_I32x4_S,
            .F32x4_Convert_I32x4_U,
            .I32x4_Trunc_Sat_F64x2_S_Zero,
            .I32x4_Trunc_Sat_F64x2_U_Zero,
            .F64x2_Convert_Low_I32x4_S,
            .F64x2_Convert_Low_I32x4_U,
            => {
                try Helpers.validateNumericUnaryOp(self, .V128, .V128);
            },
            .I8x16_EQ,
            .I8x16_NE,
            .I8x16_LT_S,
            .I8x16_LT_U,
            .I8x16_GT_S,
            .I8x16_GT_U,
            .I8x16_LE_S,
            .I8x16_LE_U,
            .I8x16_GE_S,
            .I8x16_GE_U,
            .I16x8_EQ,
            .I16x8_NE,
            .I16x8_LT_S,
            .I16x8_LT_U,
            .I16x8_GT_S,
            .I16x8_GT_U,
            .I16x8_LE_S,
            .I16x8_LE_U,
            .I16x8_GE_S,
            .I16x8_GE_U,
            .I32x4_EQ,
            .I32x4_NE,
            .I32x4_LT_S,
            .I32x4_LT_U,
            .I32x4_GT_S,
            .I32x4_GT_U,
            .I32x4_LE_S,
            .I32x4_LE_U,
            .I32x4_GE_S,
            .I32x4_GE_U,
            .F32x4_EQ,
            .F32x4_NE,
            .F32x4_LT,
            .F32x4_GT,
            .F32x4_LE,
            .F32x4_GE,
            .F64x2_EQ,
            .F64x2_NE,
            .F64x2_LT,
            .F64x2_GT,
            .F64x2_LE,
            .F64x2_GE,
            .I64x2_EQ,
            .I64x2_NE,
            .I64x2_LT_S,
            .I64x2_GT_S,
            .I64x2_LE_S,
            .I64x2_GE_S,
            .I64x2_Extmul_Low_I32x4_S,
            .I64x2_Extmul_High_I32x4_S,
            .I64x2_Extmul_Low_I32x4_U,
            .I64x2_Extmul_High_I32x4_U,
            => {
                try Helpers.validateNumericBinaryOp(self, .V128, .V128);
            },
            .V128_AnyTrue,
            .I8x16_AllTrue,
            .I8x16_Bitmask,
            .I16x8_AllTrue,
            .I16x8_Bitmask,
            .I32x4_AllTrue,
            .I32x4_Bitmask,
            .I64x2_AllTrue,
            .I64x2_Bitmask,
            => {
                try Helpers.validateNumericUnaryOp(self, .V128, .I32);
            },
            .V128_Load8_Lane => {
                try Helpers.validateLoadLaneOp(self, module, instruction, i8x16);
            },
            .V128_Load16_Lane => {
                try Helpers.validateLoadLaneOp(self, module, instruction, i16x8);
            },
            .V128_Load32_Lane => {
                try Helpers.validateLoadLaneOp(self, module, instruction, i32x4);
            },
            .V128_Load64_Lane => {
                try Helpers.validateLoadLaneOp(self, module, instruction, i64x2);
            },
            .V128_Store8_Lane => {
                try Helpers.validateStoreLaneOp(self, module, instruction, i8x16);
            },
            .V128_Store16_Lane => {
                try Helpers.validateStoreLaneOp(self, module, instruction, i16x8);
            },
            .V128_Store32_Lane => {
                try Helpers.validateStoreLaneOp(self, module, instruction, i32x4);
            },
            .V128_Store64_Lane => {
                try Helpers.validateStoreLaneOp(self, module, instruction, i64x2);
            },
            .V128_Load32_Zero => {
                try Helpers.validateLoadOp(self, module, .V128);
            },
            .V128_Load64_Zero => {
                try Helpers.validateLoadOp(self, module, .V128);
            },
            .V128_And,
            .V128_AndNot,
            .V128_Or,
            .V128_Xor,
            .I8x16_Narrow_I16x8_S,
            .I8x16_Narrow_I16x8_U,
            .I8x16_Add,
            .I8x16_Add_Sat_S,
            .I8x16_Add_Sat_U,
            .I8x16_Sub,
            .I8x16_Sub_Sat_S,
            .I8x16_Sub_Sat_U,
            .I8x16_Min_S,
            .I8x16_Min_U,
            .I8x16_Max_S,
            .I8x16_Max_U,
            .I8x16_Avgr_U,
            .I16x8_Narrow_I32x4_S,
            .I16x8_Narrow_I32x4_U,
            .I16x8_Add,
            .I16x8_Add_Sat_S,
            .I16x8_Add_Sat_U,
            .I16x8_Sub,
            .I16x8_Sub_Sat_S,
            .I16x8_Sub_Sat_U,
            .I16x8_Mul,
            .I16x8_Min_S,
            .I16x8_Min_U,
            .I16x8_Max_S,
            .I16x8_Max_U,
            .I16x8_Avgr_U,
            .I16x8_Q15mulr_Sat_S,
            .I16x8_Extmul_Low_I8x16_S,
            .I16x8_Extmul_High_I8x16_S,
            .I16x8_Extmul_Low_I8x16_U,
            .I16x8_Extmul_High_I8x16_U,
            .I32x4_Add,
            .I32x4_Sub,
            .I32x4_Mul,
            .I32x4_Min_S,
            .I32x4_Min_U,
            .I32x4_Max_S,
            .I32x4_Max_U,
            .I32x4_Dot_I16x8_S,
            .I32x4_Extmul_Low_I16x8_S,
            .I32x4_Extmul_High_I16x8_S,
            .I32x4_Extmul_Low_I16x8_U,
            .I32x4_Extmul_High_I16x8_U,
            .I64x2_Add,
            .I64x2_Sub,
            .I64x2_Mul,
            .F32x4_Add,
            .F32x4_Sub,
            .F32x4_Mul,
            .F32x4_Div,
            .F32x4_Min,
            .F32x4_Max,
            .F32x4_PMin,
            .F32x4_PMax,
            .F64x2_Add,
            .F64x2_Sub,
            .F64x2_Mul,
            .F64x2_Div,
            .F64x2_Min,
            .F64x2_Max,
            .F64x2_PMin,
            .F64x2_PMax,
            => {
                try Helpers.validateNumericBinaryOp(self, .V128, .V128);
            },
            .I8x16_Shl,
            .I8x16_Shr_S,
            .I8x16_Shr_U,
            .I16x8_Shl,
            .I16x8_Shr_S,
            .I16x8_Shr_U,
            .I32x4_Shl,
            .I32x4_Shr_S,
            .I32x4_Shr_U,
            .I64x2_Shl,
            .I64x2_Shr_S,
            .I64x2_Shr_U,
            => {
                try self.popType(.I32);
                try self.popType(.V128);
                try self.pushType(.V128);
            },
            .V128_Bitselect => {
                try self.popType(.V128);
                try self.popType(.V128);
                try self.popType(.V128);
                try self.pushType(.V128);
            },
        }
    }

    fn endValidateCode(self: *ModuleValidator) !FunctionStackStats {
        try self.type_stack.resize(0);
        try self.control_stack.resize(0);
        try self.control_types.resize(0);
        return self.stack_stats;
    }

    fn pushType(self: *ModuleValidator, valtype: ?ValType) !void {
        try self.type_stack.append(valtype);

        self.stack_stats.values = @max(self.stack_stats.values, self.type_stack.items.len);
    }

    fn popAnyType(self: *ModuleValidator) !?ValType {
        const top_frame: *const ControlFrame = &self.control_stack.items[self.control_stack.items.len - 1];
        const types: []?ValType = self.type_stack.items;

        if (top_frame.is_unreachable and types.len == top_frame.types_stack_height) {
            return null;
        }

        if (self.type_stack.items.len <= top_frame.types_stack_height) {
            return error.ValidationTypeMismatch;
        }

        std.debug.assert(self.type_stack.items.len > 0);
        return self.type_stack.pop().?;
    }

    fn popType(self: *ModuleValidator, expected_or_null: ?ValType) !void {
        const valtype_or_null = try self.popAnyType();
        if (valtype_or_null != expected_or_null and valtype_or_null != null and expected_or_null != null) {
            self.log.err("Validation failed: Expected type {?} but got {?}", .{ expected_or_null, valtype_or_null });
            return error.ValidationTypeMismatch;
        }
    }

    fn pushControl(self: *ModuleValidator, opcode: Opcode, start_types: []const ValType, end_types: []const ValType) !void {
        const control_types_start_index: usize = self.control_types.items.len;
        try self.control_types.appendSlice(start_types);
        const control_start_types: []const ValType = self.control_types.items[control_types_start_index..self.control_types.items.len];

        const control_types_end_index: usize = self.control_types.items.len;
        try self.control_types.appendSlice(end_types);
        const control_end_types: []const ValType = self.control_types.items[control_types_end_index..self.control_types.items.len];

        try self.control_stack.append(ControlFrame{
            .opcode = opcode,
            .start_types = control_start_types,
            .end_types = control_end_types,
            .types_stack_height = self.type_stack.items.len,
            .is_unreachable = false,
        });

        if (opcode != .Call_Local) {
            for (start_types) |valtype| {
                try self.pushType(valtype);
            }
            // -1 because the first control frame is always a .Call, which is not a label
            self.stack_stats.labels = @max(self.stack_stats.labels, self.control_stack.items.len - 1);
        }
    }

    fn popControl(self: *ModuleValidator) !ControlFrame {
        const frame: *const ControlFrame = &self.control_stack.items[self.control_stack.items.len - 1];

        var i = frame.end_types.len;
        while (i > 0) : (i -= 1) {
            if (frame.is_unreachable and self.type_stack.items.len == frame.types_stack_height) {
                break;
            }
            try self.popType(frame.end_types[i - 1]);
        }

        if (self.type_stack.items.len != frame.types_stack_height) {
            return error.ValidationTypeStackHeightMismatch;
        }

        _ = self.control_stack.pop();

        return frame.*;
    }

    fn freeControlTypes(self: *ModuleValidator, frame: *const ControlFrame) !void {
        const num_used_types: usize = frame.start_types.len + frame.end_types.len;
        try self.control_types.resize(self.control_types.items.len - num_used_types);
    }
};

pub const ModuleDefinitionOpts = struct {
    debug_name: []const u8 = "",
    log: ?Logger = null, // if null, uses default logger
};

pub const ModuleDefinition = struct {
    const Code = struct {
        locals: std.array_list.Managed(ValType),
        instructions: std.array_list.Managed(Instruction),
        validation_immediates: std.array_list.Managed(ValidationImmediates),

        wasm_address_to_instruction_index: std.AutoHashMap(u32, u32),

        // Instruction.immediate indexes these arrays depending on the opcode
        branch_table_immediates: std.array_list.Managed(BranchTableImmediates),
        branch_table_ids_immediates: std.array_list.Managed(u32),
        v128_immediates: std.array_list.Managed(v128),
        memory_offset_and_lane_immediates: std.array_list.Managed(MemoryOffsetAndLaneImmediates),
        vec_shuffle_16_immediates: std.array_list.Managed([16]u8),
    };

    const Imports = struct {
        functions: std.array_list.Managed(FunctionImportDefinition),
        tables: std.array_list.Managed(TableImportDefinition),
        memories: std.array_list.Managed(MemoryImportDefinition),
        globals: std.array_list.Managed(GlobalImportDefinition),
    };

    const Exports = struct {
        functions: std.array_list.Managed(ExportDefinition),
        tables: std.array_list.Managed(ExportDefinition),
        memories: std.array_list.Managed(ExportDefinition),
        globals: std.array_list.Managed(ExportDefinition),
    };

    allocator: std.mem.Allocator,

    code: Code,

    types: std.array_list.Managed(FunctionTypeDefinition),
    imports: Imports,
    functions: std.array_list.Managed(FunctionDefinition),
    globals: std.array_list.Managed(GlobalDefinition),
    tables: std.array_list.Managed(TableDefinition),
    memories: std.array_list.Managed(MemoryDefinition),
    elements: std.array_list.Managed(ElementDefinition),
    exports: Exports,
    datas: std.array_list.Managed(DataDefinition),
    custom_sections: std.array_list.Managed(CustomSection),

    name_section: NameCustomSection,

    log: Logger,
    debug_name: []const u8,
    start_func_index: ?u32 = null,
    data_count: ?u32 = null,

    is_decoded: bool = false,

    pub fn create(allocator: std.mem.Allocator, opts: ModuleDefinitionOpts) AllocError!*ModuleDefinition {
        const def = try allocator.create(ModuleDefinition);
        def.* = ModuleDefinition{
            .allocator = allocator,
            .code = Code{
                .instructions = std.array_list.Managed(Instruction).init(allocator),
                .validation_immediates = std.array_list.Managed(ValidationImmediates).init(allocator),
                .locals = std.array_list.Managed(ValType).init(allocator),
                .wasm_address_to_instruction_index = std.AutoHashMap(u32, u32).init(allocator),

                .branch_table_immediates = std.array_list.Managed(BranchTableImmediates).init(allocator),
                .branch_table_ids_immediates = std.array_list.Managed(u32).init(allocator),
                .v128_immediates = std.array_list.Managed(v128).init(allocator),
                .memory_offset_and_lane_immediates = std.array_list.Managed(MemoryOffsetAndLaneImmediates).init(allocator),
                .vec_shuffle_16_immediates = std.array_list.Managed([16]u8).init(allocator),
            },
            .types = std.array_list.Managed(FunctionTypeDefinition).init(allocator),
            .imports = Imports{
                .functions = std.array_list.Managed(FunctionImportDefinition).init(allocator),
                .tables = std.array_list.Managed(TableImportDefinition).init(allocator),
                .memories = std.array_list.Managed(MemoryImportDefinition).init(allocator),
                .globals = std.array_list.Managed(GlobalImportDefinition).init(allocator),
            },
            .functions = std.array_list.Managed(FunctionDefinition).init(allocator),
            .globals = std.array_list.Managed(GlobalDefinition).init(allocator),
            .tables = std.array_list.Managed(TableDefinition).init(allocator),
            .memories = std.array_list.Managed(MemoryDefinition).init(allocator),
            .elements = std.array_list.Managed(ElementDefinition).init(allocator),
            .exports = Exports{
                .functions = std.array_list.Managed(ExportDefinition).init(allocator),
                .tables = std.array_list.Managed(ExportDefinition).init(allocator),
                .memories = std.array_list.Managed(ExportDefinition).init(allocator),
                .globals = std.array_list.Managed(ExportDefinition).init(allocator),
            },
            .datas = std.array_list.Managed(DataDefinition).init(allocator),
            .custom_sections = std.array_list.Managed(CustomSection).init(allocator),
            .name_section = NameCustomSection.init(allocator),
            .log = if (opts.log) |log| log else Logger.empty(),
            .debug_name = try allocator.dupe(u8, opts.debug_name),
        };
        return def;
    }

    pub fn decode(self: *ModuleDefinition, wasm: []const u8) DecodeError!void {
        std.debug.assert(self.is_decoded == false);

        const DecodeHelpers = struct {
            fn readRefValue(valtype: ValType, reader: anytype) MalformedError!Val {
                switch (valtype) {
                    .FuncRef => {
                        const func_index = try decodeLEB128(u32, reader);
                        return Val.funcrefFromIndex(func_index);
                    },
                    .ExternRef => {
                        const ref = try decodeLEB128(usize, reader);
                        return Val{ .ExternRef = ref };
                    },
                    else => unreachable,
                }
            }

            // TODO move these names into a string pool
            fn readName(reader: anytype, _allocator: std.mem.Allocator) DecodeError![]const u8 {
                const name_length = try decodeLEB128(u32, reader);

                const name: []u8 = try _allocator.alloc(u8, name_length);
                errdefer _allocator.free(name);
                const read_length = try reader.read(name);
                if (read_length != name_length) {
                    return error.MalformedUnexpectedEnd;
                }

                if (std.unicode.utf8ValidateSlice(name) == false) {
                    return error.MalformedUTF8Encoding;
                }

                return name;
            }
        };

        var allocator = self.allocator;
        var validator = ModuleValidator.init(allocator, self.log);
        defer validator.deinit();

        var stream = std.io.fixedBufferStream(wasm);
        var reader = stream.reader();

        // wasm header
        {
            const magic = reader.readInt(u32, .big) catch |e| return eosError(e);
            if (magic != 0x0061736D) {
                return error.MalformedMagicSignature;
            }
            const version = reader.readInt(u32, .little) catch |e| return eosError(e);
            if (version != 1) {
                return error.MalformedUnsupportedWasmVersion;
            }
        }

        var num_functions_parsed: u32 = 0;

        while (stream.pos < stream.buffer.len) {
            const section_id: Section = std.meta.intToEnum(Section, try readByte(&reader)) catch {
                return error.MalformedSectionId;
            };
            const section_size_bytes: usize = try decodeLEB128(u32, reader);
            const section_start_pos = stream.pos;

            switch (section_id) {
                .Custom => {
                    if (section_size_bytes == 0) {
                        return error.MalformedUnexpectedEnd;
                    }

                    const name = try DecodeHelpers.readName(reader, allocator);
                    errdefer allocator.free(name);

                    var section = CustomSection{
                        .name = name,
                        .data = std.array_list.Managed(u8).init(allocator),
                    };

                    const name_length: usize = stream.pos - section_start_pos;
                    const data_length: usize = section_size_bytes - name_length;
                    try section.data.resize(data_length);
                    const data_length_read = try reader.read(section.data.items);
                    if (data_length != data_length_read) {
                        return error.MalformedUnexpectedEnd;
                    }

                    try self.custom_sections.append(section);

                    if (std.mem.eql(u8, section.name, "name")) {
                        try self.name_section.decode(self, section.data.items);
                    }
                },
                .FunctionType => {
                    const num_types = try decodeLEB128(u32, reader);

                    try self.types.ensureTotalCapacity(num_types);

                    var types_index: u32 = 0;
                    while (types_index < num_types) : (types_index += 1) {
                        const sentinel = try readByte(&reader);
                        if (sentinel != k_function_type_sentinel_byte) {
                            return error.MalformedTypeSentinel;
                        }

                        const num_params = try decodeLEB128(u32, reader);

                        var func = FunctionTypeDefinition{ .num_params = num_params, .types = std.array_list.Managed(ValType).init(allocator) };
                        errdefer func.types.deinit();

                        var params_left = num_params;
                        while (params_left > 0) {
                            params_left -= 1;

                            const param_type = try ValType.decode(reader);
                            try func.types.append(param_type);
                        }

                        const num_returns = try decodeLEB128(u32, reader);
                        var returns_left = num_returns;
                        while (returns_left > 0) {
                            returns_left -= 1;

                            const return_type = try ValType.decode(reader);
                            try func.types.append(return_type);
                        }

                        try self.types.append(func);
                    }
                },
                .Import => {
                    const num_imports = try decodeLEB128(u32, reader);

                    var import_index: u32 = 0;
                    while (import_index < num_imports) : (import_index += 1) {
                        const module_name: []const u8 = try DecodeHelpers.readName(reader, allocator);
                        errdefer allocator.free(module_name);

                        const import_name: []const u8 = try DecodeHelpers.readName(reader, allocator);
                        errdefer allocator.free(import_name);

                        const names = ImportNames{
                            .module_name = module_name,
                            .import_name = import_name,
                        };

                        const desc = try readByte(&reader);
                        switch (desc) {
                            0x00 => {
                                const type_index = try decodeLEB128(u32, reader);
                                try ModuleValidator.validateTypeIndex(type_index, self);
                                const func_type: *const FunctionTypeDefinition = &self.types.items[type_index];
                                if (func_type.num_params >= MAX_FUNCTION_IMPORT_PARAMS) {
                                    return ValidationError.ValidationTooManyFunctionImportParams;
                                }
                                if (func_type.getReturns().len >= MAX_FUNCTION_IMPORT_RETURNS) {
                                    return ValidationError.ValidationTooManyFunctionImportReturns;
                                }
                                try self.imports.functions.append(FunctionImportDefinition{
                                    .names = names,
                                    .type_index = type_index,
                                });
                            },
                            0x01 => {
                                const valtype = try ValType.decode(reader);
                                if (valtype.isRefType() == false) {
                                    return error.MalformedInvalidImport;
                                }
                                const limits = try Limits.decode(reader);
                                try self.imports.tables.append(TableImportDefinition{
                                    .names = names,
                                    .reftype = valtype,
                                    .limits = limits,
                                });
                            },
                            0x02 => {
                                const limits = try Limits.decode(reader);
                                try self.imports.memories.append(MemoryImportDefinition{
                                    .names = names,
                                    .limits = limits,
                                });
                            },
                            0x03 => {
                                const valtype = try ValType.decode(reader);
                                const mut = try GlobalMut.decode(reader);

                                try self.imports.globals.append(GlobalImportDefinition{
                                    .names = names,
                                    .valtype = valtype,
                                    .mut = mut,
                                });
                            },
                            else => return error.MalformedInvalidImport,
                        }
                    }

                    // to avoid special casing local vs import functions, we'll make a bunch of local functions
                    // that simply trampoline to their corresponding import. Import trampolines come first since
                    // that's the index space they occupy in vanilla wasm.
                    try self.functions.ensureUnusedCapacity(self.imports.functions.items.len);
                    for (0..self.imports.functions.items.len) |index| {
                        const type_index: u32 = self.imports.functions.items[index].type_index;

                        // trampoline function
                        try self.code.instructions.ensureUnusedCapacity(2);

                        const instructions_begin = self.code.instructions.items.len;
                        self.code.instructions.addOneAssumeCapacity().* = Instruction{
                            .opcode = .Call_Import,
                            .immediate = InstructionImmediates{ .Index = @intCast(index) },
                        };
                        self.code.instructions.addOneAssumeCapacity().* = Instruction{
                            .opcode = .End,
                            .immediate = InstructionImmediates{ .Index = 0 },
                        };
                        const instructions_end = self.code.instructions.items.len;

                        try self.code.validation_immediates.ensureUnusedCapacity(2);
                        self.code.validation_immediates.appendNTimesAssumeCapacity(.{ .Void = {} }, 2);

                        const func = FunctionDefinition{
                            .type_index = type_index,
                            .instructions_begin = instructions_begin,
                            .instructions_end = instructions_end,
                            .locals_begin = 0,
                            .locals_end = 0,
                            .continuation = 0,
                        };

                        self.functions.addOneAssumeCapacity().* = func;
                    }
                },
                .Function => {
                    const num_funcs = try decodeLEB128(u32, reader);

                    // the array could have already been populated with import trampolines
                    try self.functions.ensureUnusedCapacity(num_funcs);

                    for (0..num_funcs) |_| {
                        const func = FunctionDefinition{
                            .type_index = try decodeLEB128(u32, reader),

                            // we'll fix these up later when we find them in the Code section
                            .instructions_begin = 0,
                            .instructions_end = 0,
                            .locals_begin = 0,
                            .locals_end = 0,
                            .continuation = 0,
                        };

                        self.functions.addOneAssumeCapacity().* = func;
                    }
                },
                .Table => {
                    const num_tables = try decodeLEB128(u32, reader);

                    try self.tables.ensureTotalCapacity(num_tables);

                    var table_index: u32 = 0;
                    while (table_index < num_tables) : (table_index += 1) {
                        const valtype = try ValType.decode(reader);
                        if (valtype.isRefType() == false) {
                            return error.MalformedTableType;
                        }

                        const limits = try Limits.decode(reader);

                        try self.tables.append(TableDefinition{
                            .reftype = valtype,
                            .limits = limits,
                        });
                    }
                },
                .Memory => {
                    const num_memories = try decodeLEB128(u32, reader);

                    if (num_memories > 1) {
                        return error.ValidationMultipleMemories;
                    }

                    try self.memories.ensureTotalCapacity(num_memories);

                    var memory_index: u32 = 0;
                    while (memory_index < num_memories) : (memory_index += 1) {
                        const limits = try Limits.decode(reader);

                        const max_pages = limits.maxPages();
                        if (limits.min > max_pages) {
                            self.log.err(
                                "Validation error: max memory pages exceeded. Got {} but max is {}",
                                .{ limits.min, max_pages },
                            );
                            return error.ValidationMemoryMaxPagesExceeded;
                        }

                        if (limits.max) |max| {
                            if (max < limits.min) {
                                return error.ValidationMemoryInvalidMaxLimit;
                            }

                            const index_max_pages = limits.indexTypeMaxPages();
                            if (max > index_max_pages) {
                                self.log.err(
                                    "Validation error: max memory pages exceeded. Got {} but max is {}",
                                    .{ max, index_max_pages },
                                );
                                return error.ValidationMemoryMaxPagesExceeded;
                            }
                        }

                        const def = MemoryDefinition{
                            .limits = limits,
                        };
                        try self.memories.append(def);
                    }
                },
                .Global => {
                    const num_globals = try decodeLEB128(u32, reader);

                    try self.globals.ensureTotalCapacity(num_globals);

                    var global_index: u32 = 0;
                    while (global_index < num_globals) : (global_index += 1) {
                        const valtype = try ValType.decode(reader);
                        const mut = try GlobalMut.decode(reader);

                        const expr = try ConstantExpression.decode(reader, self, .Immutable, valtype);

                        if (std.meta.activeTag(expr) == .Value) {
                            if (expr.Value.type == .FuncRef) {
                                if (expr.Value.val.isNull() == false) {
                                    const index: u32 = @intCast(expr.Value.val.FuncRef.index);
                                    try ModuleValidator.validateFunctionIndex(index, self);
                                }
                            }
                        }

                        try self.globals.append(GlobalDefinition{
                            .valtype = valtype,
                            .expr = expr,
                            .mut = mut,
                        });
                    }
                },
                .Export => {
                    const num_exports = try decodeLEB128(u32, reader);

                    var export_names = std.StringHashMap(void).init(allocator);
                    defer export_names.deinit();

                    var export_index: u32 = 0;
                    while (export_index < num_exports) : (export_index += 1) {
                        const name: []const u8 = try DecodeHelpers.readName(reader, allocator);
                        errdefer allocator.free(name);

                        {
                            const getOrPutResult = try export_names.getOrPut(name);
                            if (getOrPutResult.found_existing == true) {
                                return error.ValidationDuplicateExportName;
                            }
                        }

                        const exportType = @as(ExportType, @enumFromInt(try readByte(&reader)));
                        const item_index = try decodeLEB128(u32, reader);
                        const def = ExportDefinition{ .name = name, .index = item_index };

                        switch (exportType) {
                            .Function => {
                                try ModuleValidator.validateFunctionIndex(item_index, self);
                                try self.exports.functions.append(def);
                            },
                            .Table => {
                                try ModuleValidator.validateTableIndex(item_index, self);
                                try self.exports.tables.append(def);
                            },
                            .Memory => {
                                if (self.imports.memories.items.len + self.memories.items.len <= item_index) {
                                    return error.ValidationUnknownMemory;
                                }
                                try self.exports.memories.append(def);
                            },
                            .Global => {
                                try ModuleValidator.validateGlobalIndex(item_index, self);
                                try self.exports.globals.append(def);
                            },
                        }
                    }
                },
                .Start => {
                    if (self.start_func_index != null) {
                        return error.MalformedMultipleStartSections;
                    }

                    self.start_func_index = try decodeLEB128(u32, reader);

                    if (self.functions.items.len <= self.start_func_index.?) {
                        return error.ValidationUnknownFunction;
                    }

                    const func_type_index: usize = self.functions.items[self.start_func_index.?].type_index;
                    const func_type: *const FunctionTypeDefinition = &self.types.items[func_type_index];
                    if (func_type.types.items.len > 0) {
                        return error.ValidationStartFunctionType;
                    }
                },
                .Element => {
                    const ElementHelpers = struct {
                        fn readOffsetExpr(_reader: anytype, _module: *const ModuleDefinition) !ConstantExpression {
                            const expr = try ConstantExpression.decode(_reader, _module, .Immutable, .I32);
                            return expr;
                        }

                        fn readElemsVal(elems: *std.array_list.Managed(Val), valtype: ValType, _reader: anytype, _module: *const ModuleDefinition) !void {
                            const num_elems = try decodeLEB128(u32, _reader);
                            try elems.ensureTotalCapacity(num_elems);

                            var elem_index: u32 = 0;
                            while (elem_index < num_elems) : (elem_index += 1) {
                                const ref: Val = try DecodeHelpers.readRefValue(valtype, _reader);
                                if (valtype == .FuncRef) {
                                    try ModuleValidator.validateFunctionIndex(ref.FuncRef.index, _module);
                                }
                                try elems.append(ref);
                            }
                        }

                        fn readElemsExpr(elems: *std.array_list.Managed(ConstantExpression), _reader: anytype, _module: *const ModuleDefinition, expected_reftype: ValType) !void {
                            const num_elems = try decodeLEB128(u32, _reader);
                            try elems.ensureTotalCapacity(num_elems);

                            var elem_index: u32 = 0;
                            while (elem_index < num_elems) : (elem_index += 1) {
                                const expr = try ConstantExpression.decode(_reader, _module, .Any, expected_reftype);
                                try elems.append(expr);
                            }
                        }

                        fn readNullElemkind(_reader: anytype) !void {
                            const null_elemkind = try readByte(&_reader);
                            if (null_elemkind != 0x00) {
                                return error.MalformedBytecode;
                            }
                        }
                    };

                    const num_segments = try decodeLEB128(u32, reader);

                    try self.elements.ensureTotalCapacity(num_segments);

                    var segment_index: u32 = 0;
                    while (segment_index < num_segments) : (segment_index += 1) {
                        const flags = try decodeLEB128(u32, reader);

                        var def = ElementDefinition{
                            .mode = ElementMode.Active,
                            .reftype = ValType.FuncRef,
                            .table_index = 0,
                            .offset = null,
                            .elems_value = std.array_list.Managed(Val).init(allocator),
                            .elems_expr = std.array_list.Managed(ConstantExpression).init(allocator),
                        };
                        errdefer def.elems_value.deinit();
                        errdefer def.elems_expr.deinit();

                        switch (flags) {
                            0x00 => {
                                def.offset = try ElementHelpers.readOffsetExpr(reader, self);
                                try ElementHelpers.readElemsVal(&def.elems_value, def.reftype, reader, self);
                            },
                            0x01 => {
                                def.mode = .Passive;
                                try ElementHelpers.readNullElemkind(reader);
                                try ElementHelpers.readElemsVal(&def.elems_value, def.reftype, reader, self);
                            },
                            0x02 => {
                                def.table_index = try decodeLEB128(u32, reader);
                                def.offset = try ElementHelpers.readOffsetExpr(reader, self);
                                try ElementHelpers.readNullElemkind(reader);
                                try ElementHelpers.readElemsVal(&def.elems_value, def.reftype, reader, self);
                            },
                            0x03 => {
                                def.mode = .Declarative;
                                try ElementHelpers.readNullElemkind(reader);
                                try ElementHelpers.readElemsVal(&def.elems_value, def.reftype, reader, self);
                            },
                            0x04 => {
                                def.offset = try ElementHelpers.readOffsetExpr(reader, self);
                                try ElementHelpers.readElemsExpr(&def.elems_expr, reader, self, def.reftype);
                            },
                            0x05 => {
                                def.mode = .Passive;
                                def.reftype = try ValType.decodeReftype(reader);
                                try ElementHelpers.readElemsExpr(&def.elems_expr, reader, self, def.reftype);
                            },
                            0x06 => {
                                def.table_index = try decodeLEB128(u32, reader);
                                def.offset = try ElementHelpers.readOffsetExpr(reader, self);
                                def.reftype = try ValType.decodeReftype(reader);
                                try ElementHelpers.readElemsExpr(&def.elems_expr, reader, self, def.reftype);
                            },
                            0x07 => {
                                def.mode = .Declarative;
                                def.reftype = try ValType.decodeReftype(reader);
                                try ElementHelpers.readElemsExpr(&def.elems_expr, reader, self, def.reftype);
                            },
                            else => {
                                return error.MalformedElementType;
                            },
                        }

                        try self.elements.append(def);
                    }
                },
                .Code => {
                    const BlockData = struct {
                        begin_index: u32,
                        opcode: Opcode,
                    };
                    var block_stack = std.array_list.Managed(BlockData).init(allocator);
                    defer block_stack.deinit();

                    var if_to_else_offsets = std.AutoHashMap(u32, u32).init(allocator);
                    defer if_to_else_offsets.deinit();

                    var instructions = &self.code.instructions;
                    var instruction_validation_immediates = &self.code.validation_immediates;
                    std.debug.assert(instructions.items.len == instruction_validation_immediates.items.len);

                    const num_codes = try decodeLEB128(u32, reader);

                    // codes refer to local functions not including the trampoline funcs we've generated for calling imports
                    if (num_codes != self.functions.items.len - self.imports.functions.items.len) {
                        return error.MalformedFunctionCodeSectionMismatch;
                    }

                    const wasm_code_address_begin: usize = stream.pos;

                    const TypeCount = struct {
                        valtype: ValType,
                        count: u32,
                    };
                    var local_types_scratch = std.array_list.Managed(TypeCount).init(allocator);
                    defer local_types_scratch.deinit();

                    var code_index: u32 = 0;
                    while (code_index < num_codes) {
                        const code_size = try decodeLEB128(u32, reader);
                        const code_begin_pos = stream.pos;

                        var func_def: *FunctionDefinition = &self.functions.items[code_index + self.imports.functions.items.len];

                        // parse locals
                        {
                            func_def.locals_begin = self.code.locals.items.len;

                            const num_locals = try decodeLEB128(u32, reader);
                            try local_types_scratch.resize(num_locals);

                            var locals_total: usize = 0;
                            for (local_types_scratch.items) |*item| {
                                const n = try decodeLEB128(u32, reader);
                                const local_type = try ValType.decode(reader);

                                locals_total += n;
                                if (locals_total >= std.math.maxInt(u32)) {
                                    return error.MalformedTooManyLocals;
                                }
                                item.* = TypeCount{ .valtype = local_type, .count = n };
                            }

                            try self.code.locals.ensureUnusedCapacity(locals_total);

                            for (local_types_scratch.items) |type_count| {
                                self.code.locals.appendNTimesAssumeCapacity(type_count.valtype, type_count.count);
                            }

                            func_def.locals_end = self.code.locals.items.len;
                        }

                        func_def.instructions_begin = @intCast(instructions.items.len);
                        try block_stack.append(BlockData{
                            .begin_index = @intCast(func_def.instructions_begin),
                            .opcode = .Block,
                        });

                        try validator.beginValidateCode(self, func_def);

                        var parsing_code = true;
                        while (parsing_code) {
                            const instruction_index = @as(u32, @intCast(instructions.items.len));

                            const wasm_instruction_address = stream.pos - wasm_code_address_begin;

                            const decoded_instruction: DecodedInstruction = try Instruction.decode(reader, self, func_def);
                            const validation_immediates: ValidationImmediates = decoded_instruction.validation_immediates;
                            var instruction: Instruction = decoded_instruction.instruction;

                            if (instruction.opcode.beginsBlock()) {
                                try block_stack.append(BlockData{
                                    .begin_index = instruction_index,
                                    .opcode = instruction.opcode,
                                });
                            } else if (instruction.opcode == .Else) {
                                const block: *const BlockData = &block_stack.items[block_stack.items.len - 1];
                                try if_to_else_offsets.putNoClobber(block.begin_index, instruction_index);
                                // the else gets the matching if's immediates
                                instruction.immediate = instructions.items[block.begin_index].immediate;
                                // and the if will have its else_continuation updated when .End is parsed
                            } else if (instruction.opcode == .End) {
                                const block: BlockData = block_stack.orderedRemove(block_stack.items.len - 1);
                                if (block_stack.items.len == 0) {
                                    parsing_code = false;

                                    func_def.continuation = instruction_index;

                                    block_stack.clearRetainingCapacity();

                                    num_functions_parsed += 1;
                                } else {
                                    var block_instruction: *Instruction = &instructions.items[block.begin_index];

                                    // fixup the block continuations in previously-emitted Instructions
                                    if (block.opcode == .Loop) {
                                        block_instruction.immediate.Block.continuation = block.begin_index;
                                    } else {
                                        switch (block_instruction.opcode) {
                                            .Block => block_instruction.immediate.Block.continuation = instruction_index,
                                            .If => {
                                                block_instruction.immediate.If.end_continuation_relative = @intCast(instruction_index - block.begin_index);
                                                block_instruction.immediate.If.else_continuation_relative = @intCast(instruction_index - block.begin_index);
                                            },
                                            else => unreachable,
                                        }

                                        const else_index_or_null = if_to_else_offsets.get(block.begin_index);
                                        if (else_index_or_null) |else_instruction_index| {
                                            var else_instruction: *Instruction = &instructions.items[else_instruction_index];
                                            std.debug.assert(else_instruction.opcode == .Else);

                                            block_instruction.immediate.If.else_continuation_relative = @intCast(else_instruction_index - block.begin_index);
                                            else_instruction.immediate = InstructionImmediates{ .Index = instruction_index };
                                        } else if (block_instruction.opcode == .If) {
                                            block_instruction.opcode = .IfNoElse;
                                        }
                                    }
                                }
                            }

                            try validator.validateCode(self, func_def, &instruction, validation_immediates);

                            try self.code.wasm_address_to_instruction_index.put(@as(u32, @intCast(wasm_instruction_address)), instruction_index);

                            switch (instruction.opcode) {
                                .Noop => {}, // no need to emit noops since they don't do anything
                                else => {
                                    try instructions.append(instruction);
                                    try instruction_validation_immediates.append(validation_immediates);
                                    std.debug.assert(instructions.items.len == instruction_validation_immediates.items.len);
                                },
                            }
                        }

                        func_def.stack_stats = try validator.endValidateCode();

                        func_def.instructions_end = @intCast(instructions.items.len);

                        const code_actual_size = stream.pos - code_begin_pos;
                        if (code_actual_size != code_size) {
                            return error.MalformedSectionSizeMismatch;
                        }

                        code_index += 1;
                    }
                },
                .Data => {
                    const num_datas = try decodeLEB128(u32, reader);

                    if (self.data_count != null and num_datas != self.data_count.?) {
                        return error.MalformedDataCountMismatch;
                    }

                    var data_index: u32 = 0;
                    while (data_index < num_datas) : (data_index += 1) {
                        const data = try DataDefinition.decode(reader, self, allocator);
                        try self.datas.append(data);
                    }
                },
                .DataCount => {
                    self.data_count = try decodeLEB128(u32, reader);
                    try self.datas.ensureTotalCapacity(self.data_count.?);
                },
            }

            const consumed_bytes = stream.pos - section_start_pos;
            if (section_size_bytes != consumed_bytes) {
                return error.MalformedSectionSizeMismatch;
            }
        }

        for (self.elements.items) |elem_def| {
            if (elem_def.mode == .Active) {
                const valtype = try ModuleValidator.getTableReftype(self, elem_def.table_index);
                if (elem_def.reftype != valtype) {
                    return error.ValidationTypeMismatch;
                }
            }
        }

        if (self.imports.memories.items.len + self.memories.items.len > 1) {
            return error.ValidationMultipleMemories;
        }

        if (num_functions_parsed != self.functions.items.len - self.imports.functions.items.len) {
            return error.MalformedFunctionCodeSectionMismatch;
        }
    }

    pub fn destroy(self: *ModuleDefinition) void {
        self.code.instructions.deinit();
        self.code.validation_immediates.deinit();
        self.code.locals.deinit();
        self.code.wasm_address_to_instruction_index.deinit();
        self.code.branch_table_immediates.deinit();
        self.code.branch_table_ids_immediates.deinit();
        self.code.v128_immediates.deinit();
        self.code.memory_offset_and_lane_immediates.deinit();
        self.code.vec_shuffle_16_immediates.deinit();

        for (self.imports.functions.items) |*item| {
            self.allocator.free(item.names.module_name);
            self.allocator.free(item.names.import_name);
        }
        for (self.imports.tables.items) |*item| {
            self.allocator.free(item.names.module_name);
            self.allocator.free(item.names.import_name);
        }
        for (self.imports.memories.items) |*item| {
            self.allocator.free(item.names.module_name);
            self.allocator.free(item.names.import_name);
        }
        for (self.imports.globals.items) |*item| {
            self.allocator.free(item.names.module_name);
            self.allocator.free(item.names.import_name);
        }

        for (self.exports.functions.items) |*item| {
            self.allocator.free(item.name);
        }
        for (self.exports.tables.items) |*item| {
            self.allocator.free(item.name);
        }
        for (self.exports.memories.items) |*item| {
            self.allocator.free(item.name);
        }
        for (self.exports.globals.items) |*item| {
            self.allocator.free(item.name);
        }

        for (self.types.items) |*item| {
            item.types.deinit();
        }

        for (self.elements.items) |*item| {
            item.elems_value.deinit();
            item.elems_expr.deinit();
        }

        for (self.datas.items) |*data| {
            data.bytes.deinit();
        }

        self.types.deinit();
        self.imports.functions.deinit();
        self.imports.tables.deinit();
        self.imports.memories.deinit();
        self.imports.globals.deinit();
        self.functions.deinit();
        self.globals.deinit();
        self.tables.deinit();
        self.memories.deinit();
        self.elements.deinit();
        self.exports.functions.deinit();
        self.exports.tables.deinit();
        self.exports.memories.deinit();
        self.exports.globals.deinit();
        self.datas.deinit();
        self.name_section.deinit();

        for (self.custom_sections.items) |*item| {
            self.allocator.free(item.name);
            item.data.deinit();
        }
        self.custom_sections.deinit();

        self.allocator.free(self.debug_name);

        var allocator = self.allocator;
        allocator.destroy(self);
    }

    pub fn getCustomSection(self: *const ModuleDefinition, name: []const u8) ?[]u8 {
        for (self.custom_sections.items) |section| {
            if (std.mem.eql(u8, section.name, name)) {
                return section.data.items;
            }
        }

        return null;
    }

    pub fn getFunctionExport(self: *const ModuleDefinition, func_handle: FunctionHandle) ?FunctionExport {
        if (func_handle.index < self.functions.items.len) {
            const type_index = self.functions.items[func_handle.index].type_index;
            const type_def: *const FunctionTypeDefinition = &self.types.items[type_index];
            const params: []const ValType = type_def.getParams();
            const returns: []const ValType = type_def.getReturns();

            return FunctionExport{
                .params = params,
                .returns = returns,
            };
        }
        return null;
    }

    pub fn dump(self: *const ModuleDefinition, writer: anytype) anyerror!void {
        const Helpers = struct {
            fn function(_writer: anytype, functype: *const FunctionTypeDefinition) !void {
                const params: []const ValType = functype.getParams();
                const returns: []const ValType = functype.getReturns();

                try _writer.print("(", .{});
                for (params, 0..) |v, i| {
                    try _writer.print("{s}", .{valtype(v)});
                    if (i != params.len - 1) {
                        try _writer.print(", ", .{});
                    }
                }
                try _writer.print(") -> ", .{});

                if (returns.len == 0) {
                    try _writer.print("void", .{});
                } else {
                    for (returns, 0..) |v, i| {
                        try _writer.print("{s}", .{valtype(v)});
                        if (i != returns.len - 1) {
                            try _writer.print(", ", .{});
                        }
                    }
                }

                try _writer.print("\n", .{});
            }

            fn limits(_writer: anytype, l: *const Limits) !void {
                try _writer.print("limits (min {}, max {?})\n", .{ l.min, l.max });
            }

            fn valtype(v: ValType) []const u8 {
                return switch (v) {
                    .I32 => "i32",
                    .I64 => "i64",
                    .F32 => "f32",
                    .F64 => "f64",
                    .V128 => "v128",
                    .FuncRef => "funcref",
                    .ExternRef => "externref",
                };
            }

            fn mut(m: GlobalMut) []const u8 {
                return switch (m) {
                    .Immutable => "immutable",
                    .Mutable => "mutable",
                };
            }
        };

        try writer.print("Imports:\n", .{});

        try writer.print("\tFunctions: {}\n", .{self.imports.functions.items.len});
        for (self.imports.functions.items) |*import| {
            try writer.print("\t\t{s}.{s}", .{ import.names.module_name, import.names.import_name });
            try Helpers.function(writer, &self.types.items[import.type_index]);
        }

        try writer.print("\tGlobals: {}\n", .{self.imports.globals.items.len});
        for (self.imports.globals.items) |import| {
            try writer.print("\t\t{s}.{s}: type {s}, mut: {s}\n", .{
                import.names.module_name,
                import.names.import_name,
                Helpers.valtype(import.valtype),
                Helpers.mut(import.mut),
            });
        }

        try writer.print("\tMemories: {}\n", .{self.imports.memories.items.len});
        for (self.imports.memories.items) |import| {
            try writer.print("\t\t{s}.{s}: ", .{ import.names.module_name, import.names.import_name });
            try Helpers.limits(writer, &import.limits);
        }

        try writer.print("\tTables: {}\n", .{self.imports.tables.items.len});
        for (self.imports.tables.items) |import| {
            try writer.print("\t\t{s}.{s}: type {s}, ", .{
                import.names.module_name,
                import.names.import_name,
                Helpers.valtype(import.reftype),
            });
            try Helpers.limits(writer, &import.limits);
        }

        try writer.print("Exports:\n", .{});

        try writer.print("\tFunctions: {}\n", .{self.exports.functions.items.len});
        for (self.exports.functions.items) |*ex| {
            try writer.print("\t\t{s}", .{ex.name});
            const func_type: *const FunctionTypeDefinition = &self.types.items[self.getFuncTypeIndex(ex.index)];
            try Helpers.function(writer, func_type);
        }

        try writer.print("\tGlobal: {}\n", .{self.exports.globals.items.len});
        for (self.exports.globals.items) |*ex| {
            var valtype: ValType = undefined;
            var mut: GlobalMut = undefined;
            if (ex.index < self.imports.globals.items.len) {
                valtype = self.imports.globals.items[ex.index].valtype;
                mut = self.imports.globals.items[ex.index].mut;
            } else {
                const instance_index: usize = ex.index - self.imports.globals.items.len;
                valtype = self.globals.items[instance_index].valtype;
                mut = self.globals.items[instance_index].mut;
            }
            try writer.print("\t\t{s}: type {s}, mut: {s}\n", .{ ex.name, Helpers.valtype(valtype), Helpers.mut(mut) });
        }

        try writer.print("\tMemories: {}\n", .{self.exports.memories.items.len});
        for (self.exports.memories.items) |*ex| {
            var limits: *const Limits = undefined;
            if (ex.index < self.imports.memories.items.len) {
                limits = &self.imports.memories.items[ex.index].limits;
            } else {
                const instance_index: usize = ex.index - self.imports.memories.items.len;
                limits = &self.memories.items[instance_index].limits;
            }
            try writer.print("\t\t{s}: ", .{ex.name});
            try Helpers.limits(writer, limits);
        }

        try writer.print("\tTables: {}\n", .{self.exports.tables.items.len});
        for (self.exports.tables.items) |*ex| {
            var reftype: ValType = undefined;
            var limits: *const Limits = undefined;
            if (ex.index < self.imports.tables.items.len) {
                reftype = self.imports.tables.items[ex.index].reftype;
                limits = &self.imports.tables.items[ex.index].limits;
            } else {
                const instance_index: usize = ex.index - self.imports.tables.items.len;
                reftype = self.tables.items[instance_index].reftype;
                limits = &self.tables.items[instance_index].limits;
            }
            try writer.print("\t\t{s}: type {s}, ", .{ ex.name, Helpers.valtype(reftype) });
            try Helpers.limits(writer, limits);
        }
    }

    fn getFuncTypeIndex(self: *const ModuleDefinition, func_index: usize) usize {
        const func_def: *const FunctionDefinition = &self.functions.items[func_index];
        return func_def.type_index;
    }

    fn getMemoryLimits(module: *const ModuleDefinition) Limits {
        if (module.imports.memories.items.len > 0) {
            return module.imports.memories.items[0].limits;
        }

        if (module.memories.items.len > 0) {
            return module.memories.items[0].limits;
        }

        unreachable;
    }
};
